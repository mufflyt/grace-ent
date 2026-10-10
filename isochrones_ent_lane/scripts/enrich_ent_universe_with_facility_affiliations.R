#!/usr/bin/env Rscript
# =============================================================================
# Add secondary-practice affiliation columns to the ENT board-cert universe CSV
# =============================================================================
#
# For every NPI in board_cert_ent_universe_n11333_zip_aboto.csv, query the
# same three secondary-location sources used by R/extract_secondary_practice_-
# locations.R (NPPES pl_pfile, CMS DAC, Physician Compare), enrich missing
# facility names via R/enrich_facility_name.R, then aggregate per NPI into
# the columns:
#
#   primary_facility_name           — resolved from NPPES primary practice
#                                     address via enrich_facility_name (or NA)
#   primary_facility_name_source    — which source resolved it
#   n_secondary_locations_from_sources — count of unique (addr_norm, zip5, state)
#                                     secondary keys across all sources
#   n_secondary_facility_names      — count of distinct resolved facility names
#   secondary_facility_names        — semicolon-joined unique names (priority
#                                     ordered by location_rank)
#   secondary_facility_name_sources — semicolon-joined provenance labels
#                                     aligned to secondary_facility_names
#
# Writes the enriched CSV side-by-side (NOT in place) with suffix
# _with_facility_affiliations.csv. Original file is untouched.
#
# Usage:  Rscript scripts/enrich_ent_universe_with_facility_affiliations.R
#
# Author: Tyler Muffly, MD + Claude Code
# Date:   2026-05-18
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(stringr)
  library(DBI);   library(duckdb)
})

source(here::here("R", "duckdb_safe.R"))

source(here::here("R", "enrich_facility_name.R"))
# Brings NPPES_PL_PFILE_CANDIDATES, DAC_TABLES, PC_TABLES, DUCKDB_PATH,
# read_nppes_pl_pfile().
source(here::here("R", "extract_secondary_practice_locations.R"))

HAVERSINE_THRESHOLD_KM <- 5L

# ── Paths ────────────────────────────────────────────────────────────────────
ENT_CSV <- here::here("publication_materials/figures/ent_data_integrity",
                      "board_cert_ent_universe_n11333_zip_aboto.csv")
OUT_CSV <- sub("\\.csv$", "_with_facility_affiliations.csv", ENT_CSV)

stopifnot(file.exists(ENT_CSV))

cat("Loading ENT universe: ", ENT_CSV, "\n")
ent <- readr::read_csv(ENT_CSV, show_col_types = FALSE,
                       col_types = readr::cols(npi = readr::col_character()))
cohort_npis <- unique(ent$npi)
cat(sprintf("  %s ENT physicians (unique NPIs)\n",
            format(length(cohort_npis), big.mark = ",")))

# ── 1. Pull secondary locations from all three sources ──────────────────────
cat("\n1. Querying secondary-location sources...\n")
con <- safe_duckdb(DUCKDB_PATH, read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

# Register cohort NPIs as a temp table (avoids 11k-element IN clause).
duckdb::duckdb_register(con, ".ent_npis",
                        data.frame(npi = cohort_npis), overwrite = TRUE)

# DAC — most recent.
# 2026-05-21 (Facility-enrichment sweep 5 F5-#9): `DAC_TABLES[1]` reaches
# into the static constant declared in extract_secondary_practice_locations.R
# at L179-184. When a newer DAC vintage (e.g., `..._06_2025_..._csv_8`)
# arrives in DuckDB without a source-code update, the static constant
# silently lags and this script picks the stale vintage. Use the
# dynamic resolver `.resolve_dac_tables(con)` (already sourced from
# the extractor) which discovers tables at runtime and returns them
# sorted newest-first with the F3-#2 import-suffix tiebreaker.
dac_table <- .resolve_dac_tables(con)[1]
cat("  DAC: ", dac_table, "\n")
dac_locs <- DBI::dbGetQuery(con, sprintf(r"--(
  SELECT
    CAST(d.NPI AS VARCHAR) AS npi,
    CAST(d.adr_ln_1 AS VARCHAR) AS address_line1,
    CAST(d.adr_ln_2 AS VARCHAR) AS address_line2,
    CAST(d."City/Town" AS VARCHAR) AS city,
    CAST(d.State AS VARCHAR) AS state,
    CAST(d."ZIP Code" AS VARCHAR) AS zip,
    CAST(d."Facility Name" AS VARCHAR) AS facility_name,
    CAST(d.org_pac_id AS VARCHAR) AS org_pac_id,
    CAST(d.adrs_id AS VARCHAR) AS adrs_id,
    'cms_dac' AS source
  FROM %s d
  INNER JOIN ".ent_npis" e ON CAST(d.NPI AS VARCHAR) = e.npi
)--", sprintf('"%s"', dac_table)))
cat(sprintf("    %s rows from DAC\n", format(nrow(dac_locs), big.mark = ",")))

# Physician Compare — most recent.
# 2026-05-21 (Facility-enrichment sweep 5 F5-#9): same rationale as the
# DAC site above — use the dynamic resolver which now sorts on
# (year DESC, quarter DESC, name DESC) per F3-#3, so newer Q4 tables
# are not lost to lex order.
pc_table <- .resolve_pc_tables(con)[1]
pc_locs <- tryCatch(DBI::dbGetQuery(con, sprintf(r"--(
  SELECT
    CAST(p.NPI AS VARCHAR) AS npi,
    CAST(p.adr_ln_1 AS VARCHAR) AS address_line1,
    CAST(p.adr_ln_2 AS VARCHAR) AS address_line2,
    CAST(p."City/Town" AS VARCHAR) AS city,
    CAST(p.State AS VARCHAR) AS state,
    CAST(p."ZIP Code" AS VARCHAR) AS zip,
    CAST(p."Facility Name" AS VARCHAR) AS facility_name,
    CAST(p.org_pac_id AS VARCHAR) AS org_pac_id,
    CAST(p.adrs_id AS VARCHAR) AS adrs_id,
    'physician_compare' AS source
  FROM %s p
  INNER JOIN ".ent_npis" e ON CAST(p.NPI AS VARCHAR) = e.npi
)--", sprintf('"%s"', pc_table))),
                    error = function(e) {
                      warning("Physician Compare query failed: ", e$message,
                              call. = FALSE)
                      dac_locs[0, ]
                    })
cat(sprintf("    %s rows from Physician Compare\n",
            format(nrow(pc_locs), big.mark = ",")))

duckdb::duckdb_unregister(con, ".ent_npis")

# NPPES pl_pfile (read once across all NPIs at once)
cat("\n2. Reading NPPES pl_pfile...\n")
pl_locs <- read_nppes_pl_pfile(cohort_npis) %>%
  dplyr::transmute(
    npi,
    address_line1, address_line2, city, state, zip,
    facility_name,  # all NA by construction
    org_pac_id,
    adrs_id,
    source = "nppes_pl_pfile"
  )

# ── 3. Union + dedup with source hierarchy ──────────────────────────────────
cat("\n3. Unioning and deduplicating...\n")
# 2026-05-21 (Facility-enrichment sweep 6 F6-#4): the previous form
# `stringr::str_pad(substr(zip, 1, 5), 5, "left", "0")` produced
# false-positive ZIP keys: a blank or all-letters input would substr
# to "" then pad to "00000", and a ZIP+4 fragment with punctuation
# (e.g., "02115-") would keep the hyphen as part of the first 5
# characters ("02115" → ok, but "02115-" → "02115" → ok, but
# "MA 02115" → "MA 02" → padded to "MA 02" — clearly wrong key).
# Use the hardened ZIP-normalizer pattern from the extractor: strip
# all non-digits, take first 5, reject empty/all-zero, then pad.
.normalize_zip5_to_join_key <- function(z) {
  raw    <- ifelse(is.na(z), "", as.character(z))
  digits <- substr(gsub("[^0-9]", "", raw), 1, 5)
  ifelse(
    digits == "" | grepl("^0+$", digits),
    NA_character_,
    formatC(as.integer(digits), width = 5L, flag = "0", format = "d")
  )
}

all_sec <- dplyr::bind_rows(pl_locs, dac_locs, pc_locs) %>%
  dplyr::filter(!is.na(.data$address_line1), .data$address_line1 != "") %>%
  dplyr::mutate(
    zip5 = .normalize_zip5_to_join_key(.data$zip),
    # 2026-05-21 (Facility-enrichment sweep 7 F7-msg2-#1): the previous
    # .addr_key omitted city and address_line2. Two distinct locations
    # in the same ZIP — e.g., "123 Main St" in two different cities
    # that share a ZIP boundary, or two suites of the same building
    # at different street2 — collapsed into one dedup key. Include
    # both for a proper composite address key.
    .addr_key = paste(toupper(trimws(.data$address_line1)),
                      toupper(trimws(dplyr::coalesce(.data$address_line2, ""))),
                      toupper(trimws(dplyr::coalesce(.data$city, ""))),
                      toupper(trimws(.data$state)), .data$zip5,
                      sep = "|"),
    .src_rank = dplyr::case_when(
      source == "nppes_pl_pfile"    ~ 1L,
      source == "cms_dac"           ~ 2L,
      source == "physician_compare" ~ 3L,
      TRUE                          ~ 9L
    )
  ) %>%
  dplyr::arrange(.data$npi, .data$.addr_key, .data$.src_rank) %>%
  dplyr::group_by(.data$npi, .data$.addr_key) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::select(-.addr_key, -.src_rank)
cat(sprintf("  %s unique secondary location rows after dedup\n",
            format(nrow(all_sec), big.mark = ",")))

# ── 3c. Geocode primary + secondary addresses, then haversine 5 km filter ───
# Use the canonical Census Bureau batch geocoder (chunked to <=10k per POST,
# the Census /addressbatch limit), then haversine_km on real lat/lon to
# drop same-campus rows.
cat("\n3c. Geocoding primary + secondary addresses (canonical Census batch)...\n")
source(here::here("R", "census_geocode_enhanced.R"))

# Helper: chunked census_batch_geocode (10k Census limit per POST).
.chunked_census <- function(addr_df, chunk_size = 9500, label = "") {
  if (nrow(addr_df) == 0L) return(addr_df %>%
    dplyr::mutate(latitude = numeric(), longitude = numeric()))
  addr_df <- addr_df %>% dplyr::mutate(.cb_id = dplyr::row_number())
  n_chunks <- ceiling(nrow(addr_df) / chunk_size)
  cat(sprintf("    %s: %s addresses in %d chunk(s) of <= %d\n",
              label, format(nrow(addr_df), big.mark = ","),
              n_chunks, chunk_size))
  out <- vector("list", n_chunks)
  for (i in seq_len(n_chunks)) {
    chunk <- addr_df[((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(addr_df)), ]
    geo_in <- chunk %>%
      dplyr::transmute(id = .cb_id,
                       street = trimws(as.character(street)),
                       city   = trimws(as.character(city)),
                       state  = trimws(as.character(state)),
                       zip    = trimws(as.character(zip)))
    res <- tryCatch(census_batch_geocode(geo_in),
                    error = function(e) {
                      message(sprintf("[%s chunk %d] %s", label, i, e$message))
                      NULL
                    })
    if (is.null(res) || nrow(res) == 0) {
      out[[i]] <- tibble::tibble(.cb_id = chunk$.cb_id,
                                 latitude = NA_real_, longitude = NA_real_)
      next
    }
    # BUG FIX (round-2): same tolower mismatch as filter_ent_secondary_by_-
    # haversine.R — the lookup key needs to match the actual column names of
    # `res`, not a tolower'd view of them. Otherwise res[[lat_c]] returns
    # NULL when the geocoder emits a non-lowercase column name.
    lat_c <- intersect(c("latitude","lat","y"), names(res))[1]
    lon_c <- intersect(c("longitude","lon","lng","x"), names(res))[1]
    id_c  <- intersect(c("id","row_id","UniqueID"), names(res))[1]
    out[[i]] <- tibble::tibble(
      .cb_id    = as.integer(res[[id_c]]),
      latitude  = as.numeric(res[[lat_c]]),
      longitude = as.numeric(res[[lon_c]])
    )
  }
  dplyr::left_join(addr_df, dplyr::bind_rows(out), by = ".cb_id") %>%
    dplyr::select(-.cb_id)
}

# Get primary addresses for the ENT cohort from NPPES npidata.
# BUG FIX 2026-05-18 (round-2 #2): auto-pick the latest NPPES table via the
# helper in enrich_facility_name.R rather than hardcoding the Feb 2026
# vintage. Hardcoding breaks every time a new monthly snapshot lands.
nppes_main_tbl <- .pick_latest_nppes(con)
# BUG FIX (round-2): if .pick_latest_nppes returns NA/NULL, the warning was
# only logged — the code below then tried to interpolate the missing table
# name into SQL, producing a syntax error or worse, silently quoting the
# string "NA" as if it were a real table. Skip the primary-address query and
# substitute an empty frame so the haversine path still runs.
duckdb::duckdb_register(con, ".ent_npis",
                        data.frame(npi = cohort_npis), overwrite = TRUE)
if (is.na(nppes_main_tbl) || is.null(nppes_main_tbl)) {
  warning("[ENT enrichment] No NPPES npidata table in DuckDB; primary ",
          "addresses will be skipped.", call. = FALSE)
  primary_addrs <- tibble::tibble(
    npi    = character(0), street = character(0),
    city   = character(0), state  = character(0),
    zip    = character(0)
  )
} else {
  primary_addrs <- DBI::dbGetQuery(con, sprintf(r"--(
    SELECT
      CAST(n.NPI AS VARCHAR) AS npi,
      CAST(n."Provider First Line Business Practice Location Address" AS VARCHAR) AS street,
      CAST(n."Provider Business Practice Location Address City Name" AS VARCHAR) AS city,
      CAST(n."Provider Business Practice Location Address State Name" AS VARCHAR) AS state,
      CAST(n."Provider Business Practice Location Address Postal Code" AS VARCHAR) AS zip
    FROM %s n
    INNER JOIN ".ent_npis" e ON CAST(n.NPI AS VARCHAR) = e.npi
  )--", sprintf('"%s"', nppes_main_tbl)))
}
duckdb::duckdb_unregister(con, ".ent_npis")
primary_geo <- .chunked_census(primary_addrs, label = "primary")
n_pri_hit <- sum(!is.na(primary_geo$latitude))
cat(sprintf("    primary geocode hit: %s / %s (%.1f%%)\n",
            format(n_pri_hit, big.mark = ","),
            format(nrow(primary_geo), big.mark = ","),
            100 * n_pri_hit / max(nrow(primary_geo), 1L)))

# Geocode secondary addresses too.
sec_input <- all_sec %>%
  dplyr::transmute(
    .sec_rid = dplyr::row_number(),
    street   = .data$address_line1,
    city     = .data$city,
    state    = .data$state,
    zip      = .data$zip5
  )
secondary_geo <- .chunked_census(sec_input, label = "secondary") %>%
  dplyr::transmute(.sec_rid, sec_latitude = .data$latitude,
                   sec_longitude = .data$longitude)
n_sec_hit <- sum(!is.na(secondary_geo$sec_latitude))
cat(sprintf("    secondary geocode hit: %s / %s (%.1f%%)\n",
            format(n_sec_hit, big.mark = ","),
            format(nrow(secondary_geo), big.mark = ","),
            100 * n_sec_hit / max(nrow(secondary_geo), 1L)))

# Haversine on real coords.
# BUG FIX (round-2): distinct() picked whichever row happened to come first
# in source order, which is non-deterministic when NPPES carries multiple
# rows per NPI (Type 1 vs Type 2 enumeration). Prefer the row with a
# non-missing geocode, then arrange deterministically by NPI before dedup.
primary_join <- primary_geo %>%
  # 2026-05-21 (Facility-enrichment sweep 7 F7-msg2-#2): carry the
  # primary address fields through so the same-campus-by-key fallback
  # at the haversine step has something to compare against when
  # geocoding misses on either side. primary_geo columns are
  # (npi, street, city, state, zip, latitude, longitude) per the
  # .chunked_census transmute above; the SUBSTR is the same ZIP5
  # normalizer used for secondary-side keys.
  dplyr::transmute(npi = .data$npi,
                   pri_latitude      = .data$latitude,
                   pri_longitude     = .data$longitude,
                   pri_address_line1 = .data$street,
                   pri_state         = .data$state,
                   pri_zip5          = substr(
                     gsub("[^0-9]", "", dplyr::coalesce(.data$zip, "")),
                     1, 5)) %>%
  dplyr::arrange(.data$npi, is.na(.data$pri_latitude), is.na(.data$pri_longitude)) %>%
  dplyr::distinct(npi, .keep_all = TRUE)

.n_before <- nrow(all_sec)
all_sec <- all_sec %>%
  dplyr::mutate(.sec_rid = dplyr::row_number()) %>%
  dplyr::left_join(secondary_geo, by = ".sec_rid") %>%
  dplyr::left_join(primary_join, by = "npi") %>%
  dplyr::mutate(distance_km_from_primary = haversine_km(
    .data$pri_latitude, .data$pri_longitude,
    .data$sec_latitude, .data$sec_longitude
  )) %>%
  # 2026-05-21 (Facility-enrichment sweep 7 F7-msg2-#2): the previous
  # filter kept every row with NA `distance_km_from_primary`, which
  # meant any same-campus duplicate survived whenever EITHER primary
  # or secondary geocoding missed (a common case). Now: if the
  # secondary address EXACTLY matches the primary (same .addr_key),
  # always drop it regardless of NA-distance. If geocoding gave a
  # real distance, apply the haversine threshold. NA-distance rows
  # whose address keys DO NOT match the primary are kept (we'd be
  # discarding real distinct secondaries otherwise), but flagged so
  # downstream consumers can count them separately.
  dplyr::mutate(
    .sec_addr_key_norm = paste(
      toupper(trimws(dplyr::coalesce(.data$address_line1, ""))),
      toupper(trimws(dplyr::coalesce(.data$state, ""))),
      dplyr::coalesce(.data$zip5, ""), sep = "|"),
    .pri_addr_key_norm = paste(
      toupper(trimws(dplyr::coalesce(.data$pri_address_line1, ""))),
      toupper(trimws(dplyr::coalesce(.data$pri_state, ""))),
      dplyr::coalesce(.data$pri_zip5, ""), sep = "|"),
    .same_campus_by_key = nzchar(.data$.pri_addr_key_norm) &
                           .data$.sec_addr_key_norm == .data$.pri_addr_key_norm,
    distance_unknown = is.na(.data$distance_km_from_primary)
  ) %>%
  dplyr::filter(!.data$.same_campus_by_key &
                (.data$distance_unknown |
                  .data$distance_km_from_primary >= HAVERSINE_THRESHOLD_KM)) %>%
  dplyr::select(-".sec_rid", -".sec_addr_key_norm", -".pri_addr_key_norm",
                -".same_campus_by_key")
.n_after <- nrow(all_sec)
cat(sprintf("    Haversine 5 km filter: dropped %s / %s (kept %s)\n",
            format(.n_before - .n_after, big.mark = ","),
            format(.n_before, big.mark = ","),
            format(.n_after, big.mark = ",")))

# ── 4. Enrich rows whose source doesn't carry a facility name ────────────────
cat("\n4. Backfilling facility names via cascade (DAC -> NPPES Type 2 -> hospital)...\n")
needs <- is.na(all_sec$facility_name) | all_sec$facility_name == ""
all_sec$facility_name_source <- ifelse(needs, NA_character_, all_sec$source)
if (any(needs)) {
  enrich_in <- all_sec[needs, c("address_line1","city","state","zip5"), drop = FALSE]
  enriched <- enrich_facility_name(enrich_in, con = con, verbose = TRUE)
  all_sec$facility_name[needs]        <- enriched$facility_name
  all_sec$facility_name_source[needs] <- enriched$facility_name_source
}
cat(sprintf("  Facility name resolved for %s / %s rows (%.1f%%)\n",
            format(sum(!is.na(all_sec$facility_name)), big.mark = ","),
            format(nrow(all_sec), big.mark = ","),
            100 * sum(!is.na(all_sec$facility_name)) / max(nrow(all_sec), 1L)))

# ── 5. Per-NPI aggregation ──────────────────────────────────────────────────
cat("\n5. Aggregating per NPI...\n")
# 2026-05-21 (Facility-enrichment sweep 5 F5-extra-B): the previous form
# aggregated `secondary_facility_names` and
# `secondary_facility_name_sources` INDEPENDENTLY with `unique(...)` —
# two separate dedup passes whose results could lose positional
# alignment. For rows [(A, dac), (B, dac), (A, hospital)] the names
# unique returns [A, B] (2 elements) and the sources unique returns
# [dac, hospital] (2 elements); the semicolon-joined output would then
# wrongly attribute "B" to source "hospital" when in fact "B" only
# came from "dac". Pre-aggregate distinct (facility_name,
# facility_name_source) PAIRS first via a separate per-NPI pair-table,
# then join the serialized pair columns onto the main per_npi summary.
pair_per_npi <- all_sec %>%
  dplyr::filter(!is.na(.data$facility_name)        & .data$facility_name != "" &
                !is.na(.data$facility_name_source) & .data$facility_name_source != "none") %>%
  dplyr::distinct(.data$npi, .data$facility_name, .data$facility_name_source) %>%
  dplyr::arrange(.data$npi, .data$facility_name, .data$facility_name_source) %>%
  dplyr::group_by(.data$npi) %>%
  dplyr::summarise(
    secondary_facility_names        = paste(.data$facility_name,        collapse = "; "),
    secondary_facility_name_sources = paste(.data$facility_name_source, collapse = "; "),
    .groups = "drop"
  )

per_npi <- all_sec %>%
  dplyr::group_by(.data$npi) %>%
  dplyr::summarise(
    n_secondary_locations_from_sources = dplyr::n(),
    # 2026-05-21 (Facility-enrichment sweep 7 F7-msg2-#4): the previous
    # form `sum(!is.na(facility_name) & facility_name != "")` counted
    # ROWS, not DISTINCT facility names. Repeated rows for the same
    # facility (one per source: pl_pfile + dac + pc) inflated the
    # count. Switch to n_distinct() over the filtered non-blank
    # facility names so each facility is counted once per NPI.
    n_secondary_facility_names = dplyr::n_distinct(
      .data$facility_name[!is.na(.data$facility_name) &
                          .data$facility_name != ""]),
    secondary_sources_used = paste(sort(unique(.data$source)), collapse = "; "),
    .groups = "drop"
  ) %>%
  dplyr::left_join(pair_per_npi, by = "npi") %>%
  dplyr::mutate(
    secondary_facility_names = dplyr::na_if(.data$secondary_facility_names, ""),
    secondary_facility_name_sources = dplyr::na_if(.data$secondary_facility_name_sources, "")
  )

cat(sprintf("  %s ENT NPIs with at least one secondary location\n",
            format(nrow(per_npi), big.mark = ",")))

# ── 6. Primary-facility lookup (one row per NPI from NPPES primary practice) ─
cat("\n6. Resolving primary facility names from NPPES enumeration address...\n")
nppes_main_tbl <- .pick_latest_nppes(con)
if (is.na(nppes_main_tbl) || is.null(nppes_main_tbl)) {
  warning("NPPES npidata table not found; primary_facility_name will be NA.",
          call. = FALSE)
  primary_addr <- tibble::tibble(
    npi = cohort_npis,
    primary_facility_name = NA_character_,
    primary_facility_name_source = NA_character_
  )
} else {
  duckdb::duckdb_register(con, ".ent_npis",
                          data.frame(npi = cohort_npis), overwrite = TRUE)
  primary_addrs <- DBI::dbGetQuery(con, sprintf(r"--(
    SELECT
      CAST(n.NPI AS VARCHAR) AS npi,
      CAST(n."Provider First Line Business Practice Location Address" AS VARCHAR) AS address_line1,
      CAST(n."Provider Business Practice Location Address City Name" AS VARCHAR) AS city,
      CAST(n."Provider Business Practice Location Address State Name" AS VARCHAR) AS state,
      CAST(n."Provider Business Practice Location Address Postal Code" AS VARCHAR) AS zip
    FROM %s n
    INNER JOIN ".ent_npis" e ON CAST(n.NPI AS VARCHAR) = e.npi
  )--", sprintf('"%s"', nppes_main_tbl))) %>%
    # F6-#4: same hardened ZIP normalizer as the secondary-side bind.
    dplyr::mutate(zip5 = .normalize_zip5_to_join_key(.data$zip)) %>%
    dplyr::select(-zip)
  duckdb::duckdb_unregister(con, ".ent_npis")
  primary_enriched <- enrich_facility_name(primary_addrs, con = con, verbose = TRUE)
  # 2026-05-21 (Facility-enrichment sweep 7 F7-msg2-#5): NPPES can carry
  # multiple practice-location rows per NPI (Type 1 + organization rows;
  # multiple locations after the practiceLocations expansion). Without
  # an explicit dedup, the left_join at L419 will fan out the ENT
  # universe — one ENT physician → N rows in the output. Dedupe to
  # one row per NPI, preferring rows where a facility_name actually
  # resolved (non-NA), then deterministic tiebreak.
  primary_addr <- primary_enriched %>%
    dplyr::transmute(npi = .data$npi,
                     primary_facility_name        = .data$facility_name,
                     primary_facility_name_source = .data$facility_name_source) %>%
    dplyr::arrange(.data$npi,
                   is.na(.data$primary_facility_name),
                   dplyr::coalesce(.data$primary_facility_name_source, "")) %>%
    dplyr::distinct(.data$npi, .keep_all = TRUE)
}

# ── 7. Merge onto ENT universe and write ────────────────────────────────────
cat("\n7. Merging and writing output CSV...\n")
ent_out <- ent %>%
  dplyr::left_join(primary_addr, by = "npi") %>%
  dplyr::left_join(per_npi,     by = "npi") %>%
  # Fill counts with 0 for NPIs that had no secondary locations at all
  dplyr::mutate(
    n_secondary_locations_from_sources = dplyr::coalesce(
      .data$n_secondary_locations_from_sources, 0L),
    n_secondary_facility_names = dplyr::coalesce(
      .data$n_secondary_facility_names, 0L)
  )

dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
tmp_out_csv <- paste0(OUT_CSV, ".tmp")
readr::write_csv(ent_out, tmp_out_csv)
# 2026-05-21 (Facility-enrichment sweep 6 F6-#6): the bare file.rename()
# return value was ignored. A FALSE return (cross-device, permission,
# missing target dir) would leave the tmp file in place AND still
# print the "Wrote:" success message — operator wouldn't know the
# move failed. Capture the boolean and stop loudly on failure.
if (!isTRUE(file.rename(tmp_out_csv, OUT_CSV))) {
  stop(sprintf("[write] file.rename failed: %s -> %s (tmp preserved)",
               tmp_out_csv, OUT_CSV), call. = FALSE)
}
cat(sprintf("\nWrote: %s (%s rows, %s cols)\n", OUT_CSV,
            format(nrow(ent_out), big.mark = ","), ncol(ent_out)))

# Quick coverage report
cat("\n=== Coverage report ===\n")
cat(sprintf("Total ENT NPIs                  : %s\n",
            format(nrow(ent_out), big.mark = ",")))
cat(sprintf("  with >=1 secondary location  : %s (%.1f%%)\n",
            format(sum(ent_out$n_secondary_locations_from_sources > 0),
                   big.mark = ","),
            100 * mean(ent_out$n_secondary_locations_from_sources > 0)))
cat(sprintf("  with >=1 secondary facility  : %s (%.1f%%)\n",
            format(sum(ent_out$n_secondary_facility_names > 0),
                   big.mark = ","),
            100 * mean(ent_out$n_secondary_facility_names > 0)))
cat(sprintf("  with primary facility name   : %s (%.1f%%)\n",
            format(sum(!is.na(ent_out$primary_facility_name)),
                   big.mark = ","),
            100 * mean(!is.na(ent_out$primary_facility_name))))
