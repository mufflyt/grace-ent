#!/usr/bin/env Rscript
# =============================================================================
# ENT secondary-location geocode + 5km haversine filter
# =============================================================================
# Mirrors scripts/filter_secondary_by_haversine_geocoded.R (canonical OB-GYN
# pipeline, committed in bd2bfd519) but targets the ENT cohort.
#
# Pipeline:
#   1. Extract ENT secondaries from NPPES pl_pfile (already done)
#   2. Geocode the secondary addresses via chunked Census batch
#   3. Geocode the ENT primary addresses (from NPPES parquet)
#   4. Compute haversine distance, drop rows < 5 km from primary
#   5. Write artifacts/secondary_locations/ent_secondary_locations_filtered.csv
#
# Output schema matches the OB-GYN filtered CSV so downstream consumers
# (leaflet, static map, cohort enrichment) can swap data files freely.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(arrow)
  library(tibble); library(jsonlite)
})
source(here("R", "extract_secondary_practice_locations.R"))
source(here("R", "ent_registry_counts.R"))
source(here("R", "census_geocode_enhanced.R"))

OUT_DIR        <- here("artifacts", "secondary_locations")
SECONDARY_CSV  <- file.path(OUT_DIR, "ent_secondary_locations_extracted.csv")
GEOCODED_CSV   <- file.path(OUT_DIR, "ent_secondary_locations_geocoded.csv")
PRIMARY_CSV    <- file.path(OUT_DIR, "ent_primary_geocoded.csv")
FILTERED_CSV   <- file.path(OUT_DIR, "ent_secondary_locations_filtered.csv")
SUMMARY_JSON   <- file.path(OUT_DIR, "ent_haversine_filter_summary.json")
THRESHOLD_KM   <- 5
CHUNK_SIZE     <- 9500

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── 1. Cohort ───────────────────────────────────────────────────────────────
cohort <- read_csv(here("publication_materials/figures/ent_data_integrity",
                         "board_cert_ent_universe_n11333_zip_aboto.csv"),
                   col_types = cols(.default = "c"))
cohort_npis <- unique(cohort$npi)
cat(sprintf("ENT cohort: %s NPIs\n", format(length(cohort_npis), big.mark = ",")))

# Chunked census batch helper (mirrors enrich_ent_universe_with_facility_affiliations.R)
chunked_census <- function(addr_df, chunk_size = CHUNK_SIZE, label = "") {
  if (nrow(addr_df) == 0L) {
    return(addr_df %>% mutate(latitude = numeric(0), longitude = numeric(0),
                               geocode_method = character(0)))
  }
  addr_df <- addr_df %>% mutate(.cb_id = row_number())
  n_chunks <- ceiling(nrow(addr_df) / chunk_size)
  cat(sprintf("  [%s] %s addresses in %d chunk(s) of <= %s\n",
              label, format(nrow(addr_df), big.mark = ","),
              n_chunks, format(chunk_size, big.mark = ",")))
  out <- vector("list", n_chunks)
  for (i in seq_len(n_chunks)) {
    rng <- ((i - 1) * chunk_size + 1):min(i * chunk_size, nrow(addr_df))
    chunk <- addr_df[rng, , drop = FALSE]
    geo_in <- chunk %>%
      transmute(id     = .cb_id,
                street = trimws(as.character(street)),
                city   = trimws(as.character(city)),
                state  = trimws(as.character(state)),
                zip    = trimws(as.character(zip)))
    res <- tryCatch(census_batch_geocode(geo_in),
                    error = function(e) {
                      message(sprintf("[%s chunk %d/%d] %s",
                                      label, i, n_chunks, e$message))
                      NULL
                    })
    if (is.null(res) || nrow(res) == 0) {
      out[[i]] <- tibble(.cb_id = chunk$.cb_id,
                          latitude = NA_real_, longitude = NA_real_,
                          geocode_method = NA_character_)
      next
    }
    # BUG FIX (round-2): tolower was being applied to one side of the
    # intersect only — `res[[lat_c]]` then used a lowercase key against the
    # original-case names of `res`, returning NULL when `res` happens to
    # carry a non-lowercase column name. Match against the original names so
    # the key we picked actually exists in `res`.
    lat_c <- intersect(c("latitude","lat","y"), names(res))[1]
    lon_c <- intersect(c("longitude","lon","lng","x"), names(res))[1]
    id_c  <- intersect(c("id","row_id","UniqueID"), names(res))[1]
    out[[i]] <- tibble(
      .cb_id         = as.integer(res[[id_c]]),
      latitude       = as.numeric(res[[lat_c]]),
      longitude      = as.numeric(res[[lon_c]]),
      geocode_method = "census_batch"
    )
    cat(sprintf("    chunk %d/%d: %s/%s geocoded\n",
                i, n_chunks,
                format(sum(!is.na(out[[i]]$latitude)), big.mark = ","),
                format(nrow(chunk), big.mark = ",")))
  }
  left_join(addr_df, bind_rows(out), by = ".cb_id") %>%
    select(-.cb_id)
}

# ── 2. Extract ENT secondaries from NPPES pl_pfile ─────────────────────────
if (file.exists(SECONDARY_CSV) &&
    file.mtime(SECONDARY_CSV) > Sys.time() - 24 * 3600) {
  cat("\nUsing cached ENT secondaries: ", SECONDARY_CSV, "\n", sep = "")
  secondary <- read_csv(SECONDARY_CSV, show_col_types = FALSE,
                        col_types = cols(npi = col_character()))
} else {
  cat("\nExtracting ENT secondaries from NPPES pl_pfile...\n")
  # BUG FIX 2026-05-18 (round-2 #1): location_rank must be assigned PER NPI
  # starting at 2 (rank 1 is reserved for the primary), matching the OB-GYN
  # extractor in R/extract_secondary_practice_locations.R. The previous
  # `row_number()` produced a global 1..N rank that didn't match the
  # downstream "rank >= 2 == secondary" semantics.
  secondary <- read_nppes_pl_pfile(cohort_npis = cohort_npis,
                                    candidates = NPPES_PL_PFILE_CANDIDATES) %>%
    group_by(npi) %>%
    mutate(location_rank = row_number() + 1L) %>%
    ungroup() %>%
    select(npi, location_rank, address_line1, address_line2,
           city, state, zip, facility_name, org_pac_id,
           source, adrs_id, vintage_date)
  tmp_secondary_csv <- paste0(SECONDARY_CSV, ".tmp")
  write_csv(secondary, tmp_secondary_csv)
  file.rename(tmp_secondary_csv, SECONDARY_CSV)
  cat(sprintf("  Wrote: %s (%s rows)\n", SECONDARY_CSV,
              format(nrow(secondary), big.mark = ",")))
}

# ── 3. Geocode secondaries ──────────────────────────────────────────────────
if (file.exists(GEOCODED_CSV) &&
    file.mtime(GEOCODED_CSV) > file.mtime(SECONDARY_CSV)) {
  cat("\nUsing cached geocoded secondaries: ", GEOCODED_CSV, "\n", sep = "")
  secondary_geo <- read_csv(GEOCODED_CSV, show_col_types = FALSE,
                             col_types = cols(npi = col_character()))
} else {
  cat("\nGeocoding ENT secondaries via Census batch (chunked)...\n")
  geo_in <- secondary %>%
    transmute(.rid   = row_number(),
              street = address_line1,
              city   = city,
              state  = state,
              zip    = substr(as.character(zip), 1, 5))
  res <- chunked_census(geo_in, label = "secondary") %>%
    select(.rid, latitude, longitude, geocode_method)
  secondary_geo <- secondary %>%
    mutate(.rid = row_number()) %>%
    left_join(res, by = ".rid") %>%
    select(-.rid)
  write_csv(secondary_geo, GEOCODED_CSV)
  cat(sprintf("  Geocoded: %s / %s (%.1f%%)\n",
              format(sum(!is.na(secondary_geo$latitude)), big.mark = ","),
              format(nrow(secondary_geo), big.mark = ","),
              100 * mean(!is.na(secondary_geo$latitude))))
}

# ── 4. Geocode ENT primary addresses ───────────────────────────────────────
if (file.exists(PRIMARY_CSV) &&
    file.mtime(PRIMARY_CSV) > file.mtime(SECONDARY_CSV)) {
  cat("\nUsing cached ENT primaries: ", PRIMARY_CSV, "\n", sep = "")
  primary_geo <- read_csv(PRIMARY_CSV, show_col_types = FALSE,
                           col_types = cols(npi = col_character()))
  # BUG FIX 2026-05-18 (round-2 #4): a cached PRIMARY_CSV from a different
  # cohort or an older schema would silently mismatch. Require the cache
  # to cover every cohort NPI AND carry the `primary_geocode_method`
  # column; otherwise rebuild.
  required_cols <- c("npi", "primary_lat", "primary_lon", "primary_geocode_method")
  cache_stale <- !all(required_cols %in% names(primary_geo)) ||
    !all(cohort_npis %in% primary_geo$npi)
  if (cache_stale) {
    cat("  ↻ Cache stale (cohort or schema mismatch); regenerating...\n")
    file.remove(PRIMARY_CSV)
  }
}
if (!file.exists(PRIMARY_CSV)) {
  cat("\nGeocoding ENT primary addresses (NPPES parquet)...\n")
  ct <- compute_ent_registry_counts()
  nppes <- read_parquet(ct$input_paths$nppes) %>%
    mutate(npi = as.character(npi))
  # BUG FIX 2026-05-18 (round-2 #3): dedup NPPES on npi BEFORE the join so
  # an NPI present in multiple npidata rows doesn't fan out the cohort
  # frame (which would in turn fan out the haversine join). One primary
  # address per NPI is the documented schema.
  pri_in <- cohort %>%
    select(npi) %>%
    left_join(nppes %>%
                select(npi, address_line1, city, state, zip) %>%
                distinct(npi, .keep_all = TRUE),
              by = "npi") %>%
    transmute(.pid   = row_number(),
              npi    = npi,
              street = address_line1,
              city   = city,
              state  = state,
              zip    = substr(as.character(zip), 1, 5))
  res <- chunked_census(pri_in, label = "primary") %>%
    select(.pid, latitude, longitude, geocode_method)
  primary_geo <- pri_in %>%
    left_join(res, by = ".pid") %>%
    select(npi, primary_lat = latitude, primary_lon = longitude,
           primary_geocode_method = geocode_method)
  write_csv(primary_geo, PRIMARY_CSV)
  cat(sprintf("  Geocoded: %s / %s (%.1f%%)\n",
              format(sum(!is.na(primary_geo$primary_lat)), big.mark = ","),
              format(nrow(primary_geo), big.mark = ","),
              100 * mean(!is.na(primary_geo$primary_lat))))
}

# ── 5. Haversine filter ────────────────────────────────────────────────────
cat("\nComputing haversine distances...\n")
joined <- secondary_geo %>%
  left_join(primary_geo %>% select(npi, primary_lat, primary_lon),
            by = "npi") %>%
  mutate(distance_km_from_primary = haversine_km(
    primary_lat, primary_lon,
    latitude, longitude))

n_total  <- nrow(joined)
n_no_pri <- sum(is.na(joined$primary_lat) | is.na(joined$primary_lon))
n_no_sec <- sum(is.na(joined$latitude)    | is.na(joined$longitude))
n_kept   <- sum(is.na(joined$distance_km_from_primary) |
                  joined$distance_km_from_primary >= THRESHOLD_KM)
n_drop   <- n_total - n_kept

filtered <- joined %>%
  filter(is.na(distance_km_from_primary) |
         distance_km_from_primary >= THRESHOLD_KM)

write_csv(filtered, FILTERED_CSV)

cat(sprintf("\nHaversine filter results:\n"))
cat(sprintf("  Total secondary rows         : %s\n", format(n_total,  big.mark = ",")))
cat(sprintf("  Missing primary coord        : %s\n", format(n_no_pri, big.mark = ",")))
cat(sprintf("  Missing secondary coord      : %s\n", format(n_no_sec, big.mark = ",")))
cat(sprintf("  Dropped (< %.1f km)             : %s (%.1f%%)\n",
            THRESHOLD_KM, format(n_drop, big.mark = ","),
            # BUG FIX 2026-05-18 (round-2 #8): avoid 0/0 = NaN when the
            # cohort has no secondary rows (e.g., pl_pfile read failed).
            100 * n_drop / max(n_total, 1L)))
cat(sprintf("  Kept                         : %s\n", format(n_kept, big.mark = ",")))
cat(sprintf("\nWrote: %s\n", FILTERED_CSV))

# ── 6. Summary JSON ────────────────────────────────────────────────────────
write_json(list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  cohort_size  = length(cohort_npis),
  inputs       = list(secondary_csv = SECONDARY_CSV,
                       primary_csv   = PRIMARY_CSV,
                       threshold_km  = THRESHOLD_KM),
  counts       = list(total_secondary_rows         = n_total,
                       rows_missing_primary_coord   = n_no_pri,
                       rows_missing_secondary_coord = n_no_sec,
                       rows_dropped_under_threshold = n_drop,
                       rows_kept                    = n_kept,
                       pct_dropped                  = round(100 * n_drop / max(n_total, 1L), 2)),
  geocode_provenance = list(
    primary_method   = as.list(table(primary_geo$primary_geocode_method, useNA = "ifany")),
    secondary_method = as.list(table(filtered$geocode_method,           useNA = "ifany"))
  )
), SUMMARY_JSON, auto_unbox = TRUE, pretty = TRUE, na = "null")
cat(sprintf("Wrote summary: %s\n", SUMMARY_JSON))
