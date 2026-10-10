#!/usr/bin/env Rscript
# =============================================================================
# Enrich Board+NPPES cohort with secondary-location flags + state-level rollup
# =============================================================================
# Outputs:
#   board_cert_ent_universe_n11333_zip_aboto.csv   (NPI-level, +6 new columns)
#   rural_by_state_primary_vs_secondary.csv        (state-level rollup)
#   rural_ent_physicians_map_v2.png                (primary + secondary dots)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(ggplot2)
  library(sf); library(tigris); library(tibble); library(jsonlite)
})
options(tigris_use_cache = TRUE)
source(here("R", "extract_secondary_practice_locations.R"))

OUT_DIR <- here("publication_materials", "figures", "ent_data_integrity")
OUT_CSV_NPI    <- file.path(OUT_DIR, "board_cert_ent_universe_n11333_zip_aboto.csv")
OUT_CSV_STATE  <- file.path(OUT_DIR, "rural_by_state_primary_vs_secondary.csv")
OUT_PNG_MAP    <- file.path(OUT_DIR, "rural_ent_physicians_map_v2.png")

# 2026-05-21 (Facility-enrichment sweep 2 F2-#4): the script wrote CSV /
# PNG / receipt outputs into OUT_DIR without ever creating it. On a fresh
# checkout (or a freshly cleaned figures tree) the first write would
# fail with "cannot open file ..." and abort partway through. Create
# the directory at startup, recursive, and stop loudly if creation
# fails (also fails loudly if a non-directory file already occupies
# that path).
if (!dir.exists(OUT_DIR)) {
  if (!dir.create(OUT_DIR, recursive = TRUE, showWarnings = TRUE)) {
    stop(sprintf("[startup] cannot create OUT_DIR: %s", OUT_DIR),
         call. = FALSE)
  }
}
if (!file.info(OUT_DIR)$isdir) {
  stop(sprintf("[startup] OUT_DIR exists but is not a directory: %s", OUT_DIR),
       call. = FALSE)
}

# Use the canonical-geocoded + 5km-haversine-filtered ENT secondaries
# (produced by scripts/filter_ent_secondary_by_haversine.R). Falls back
# to live NPPES pl_pfile extraction if the filtered file is missing.
FILTERED_SEC <- here("artifacts", "secondary_locations",
                     "ent_secondary_locations_filtered.csv")

# ── 1. Load primary cohort ──────────────────────────────────────────────────
cohort <- read_csv(OUT_CSV_NPI, col_types = cols(.default = "c"))
cat("Loaded primary cohort:", nrow(cohort), "rows\n")

# 2026-05-21 (Facility-enrichment sweep 3 F3-#9): the rest of the script
# assumes one row per NPI in `cohort` — `left_join(sec_per_npi, by = "npi")`
# at L166 fans out if NPIs repeat, and `count(state)` at L203 / L208
# double-counts physicians under the same condition. Assert uniqueness
# loudly so a malformed cohort fails fast with diagnostic detail
# instead of silently inflating downstream numbers.
.dup_npi <- cohort$npi[duplicated(cohort$npi) & !is.na(cohort$npi)]
if (length(.dup_npi) > 0L) {
  stop(sprintf(
    "[startup] cohort has %d duplicate NPI(s) — first 5: %s. Dedupe upstream before running this script.",
    length(unique(.dup_npi)), paste(head(unique(.dup_npi), 5), collapse = ", ")
  ), call. = FALSE)
}

# ── 2. Load filtered ENT secondaries (real geocoded + 5km haversine) ───────
if (file.exists(FILTERED_SEC)) {
  cat("Using filtered ENT secondaries:", FILTERED_SEC, "\n")
  sec <- read_csv(FILTERED_SEC, show_col_types = FALSE,
                  col_types = cols(npi = col_character()))
} else {
  cat("[WARN] Filtered ENT secondaries not found; falling back to live ",
      "NPPES pl_pfile pull (no 5km filter applied).\n")
  sec <- read_nppes_pl_pfile(cohort_npis = unique(cohort$npi),
                              candidates = NPPES_PL_PFILE_CANDIDATES)
}
cat("Secondary rows:", nrow(sec),
    "  Unique NPIs:", length(unique(sec$npi)), "\n")

# ── 3. Join RUCA to secondary ZIPs ──────────────────────────────────────────
ruca_raw <- read_csv(here("data/external/ruca_2020_zip.csv"),
                     show_col_types = FALSE) %>%
  # BUG FIX 2026-05-18 (round-1): sprintf("%05d", as.integer(NA)) returns
  # the literal "   NA" string, so every non-numeric ZIPCode row gets the
  # same key. Build zip5 explicitly NA-safe and drop the empties BEFORE
  # distinct() so we don't dedup a real ZIP under a junk NA-stand-in.
  mutate(.zips_int = suppressWarnings(as.integer(ZIPCode)),
         zip5      = ifelse(is.na(.zips_int), NA_character_,
                            sprintf("%05d", .zips_int))) %>%
  filter(!is.na(zip5)) %>%
  select(-.zips_int) %>%
  distinct(zip5, .keep_all = TRUE)

ruca <- ruca_raw %>%
  select(zip5, ruca_primary_sec = PrimaryRUCA)

# Ensure cohort has ruca_classification + ruca_primary (may be absent on first run).
if (!"ruca_classification" %in% names(cohort) || !"ruca_primary" %in% names(cohort)) {
  cat("[INFO] ruca_classification/ruca_primary absent from cohort — computing from RUCA crosswalk.\n")
  ruca_primary <- ruca_raw %>%
    select(zip5, ruca_primary = PrimaryRUCA)
  cohort <- cohort %>%
    # 2026-05-21 (Facility-enrichment sweep 2 F2-#2): the previous form
    # `as.integer(as.character(zip5))` silently dropped valid ZIP+4
    # strings like "02115-1234" — as.integer("02115-1234") returns NA,
    # so the RUCA join then missed all ZIP+4 rows. Normalize by
    # stripping non-digits first, taking the leading 5 digits, then
    # left-padding. Handles "02115-1234", "02115", "2115" (leading-zero
    # loss), and " 02115 " (trim) identically.
    mutate(.zip5_raw    = ifelse(is.na(zip5), "", as.character(zip5)),
           .zip5_digits = substr(gsub("[^0-9]", "", .zip5_raw), 1, 5),
           zip5         = ifelse(.zip5_digits == "" | grepl("^0+$", .zip5_digits),
                                 NA_character_,
                                 formatC(as.integer(.zip5_digits),
                                         width = 5, flag = "0", format = "d"))) %>%
    select(-.zip5_raw, -.zip5_digits) %>%
    left_join(ruca_primary, by = "zip5") %>%
    mutate(ruca_classification = case_when(
      is.na(ruca_primary)                               ~ "unknown",
      as.integer(ruca_primary) %in% 1:3                ~ "urban",
      TRUE                                              ~ "rural"
    ))
}

NON_CONUS <- c("AK","HI","PR","VI","GU","MP","AS","AA","AE","AP")
sec_flagged <- sec %>%
  # 2026-05-21 (Facility-enrichment sweep 2 F2-#2): same ZIP+4 issue as
  # the cohort path above — as.integer("02115-1234") returns NA and the
  # RUCA join silently misses real rows. Same NA-aware digit-strip
  # pipeline.
  mutate(.zip_raw    = ifelse(is.na(zip), "", as.character(zip)),
         .zip_digits = substr(gsub("[^0-9]", "", .zip_raw), 1, 5),
         zip5        = ifelse(.zip_digits == "" | grepl("^0+$", .zip_digits),
                              NA_character_,
                              formatC(as.integer(.zip_digits),
                                      width = 5, flag = "0", format = "d"))) %>%
  select(-.zip_raw, -.zip_digits) %>%
  left_join(ruca, by = "zip5") %>%
  # 2026-05-21 (Facility-enrichment sweep 1 F1-R2): the filter clauses
  # `state %in% NON_CONUS` and `state %in% c(state.abb, "DC")` compare to
  # uppercase abbreviations, but `sec$state` was not guaranteed to be
  # uppercase — any row with a lowercase state ("ca", "ny") would silently
  # FAIL the second clause (`state %in% c("CA","NY",...)` → FALSE) and be
  # dropped from sec_flagged. That row then never reaches sec_per_npi or
  # state_secondary, suppressing rural-secondary counts at the state
  # level. Normalize via toupper() before the filter so the comparison
  # matches regardless of upstream case-handling.
  # 2026-05-21 (Facility-enrichment sweep 2 F2-#3 follow-up): also trim
  # whitespace — a padded " NY " still fails the membership check.
  mutate(state = toupper(trimws(state))) %>%
  filter(!state %in% NON_CONUS, state %in% c(state.abb, "DC"), !is.na(state)) %>%
  # BUG FIX 2026-05-18 (round-1): suppress integer-parse warning for
  # non-numeric RUCA codes (e.g., "" or "99.x") that otherwise spam the
  # console with "NAs introduced by coercion" once per row.
  mutate(is_rural_sec = !is.na(ruca_primary_sec) &
                         !(suppressWarnings(as.integer(ruca_primary_sec)) %in% 1:3))

# ── 4. Per-NPI rollup of secondary location counts ──────────────────────────
sec_per_npi <- sec_flagged %>%
  group_by(npi) %>%
  summarise(
    n_secondary_locations       = dplyr::n(),
    n_rural_secondary_locations = sum(is_rural_sec),
    has_rural_secondary         = any(is_rural_sec),
    # 2026-05-21 (Facility-enrichment sweep 3 F3-#10): paste(unique(...))
    # depends on row order — across runs the same physician's rural-
    # secondary states could appear as "CO|NM" one run and "NM|CO" the
    # next. Sort before concatenation for deterministic output that
    # diffs cleanly between runs.
    secondary_rural_states      = paste(sort(unique(state[is_rural_sec])),
                                         collapse = "|"),
    .groups = "drop"
  )

# ── 5. Merge into cohort + compute combined classification ──────────────────
# Drop stale columns from a prior run so the left_join doesn't create
# .x/.y duplicates with the newly computed sec_per_npi.
stale_cols <- intersect(
  names(cohort),
  c("n_secondary_locations", "n_rural_secondary_locations",
    "has_rural_secondary", "secondary_rural_states",
    "is_rural_primary", "rural_status_combined")
)
cohort_enriched <- cohort %>%
  select(-any_of(stale_cols)) %>%
  left_join(sec_per_npi, by = "npi") %>%
  mutate(
    n_secondary_locations       = tidyr::replace_na(n_secondary_locations, 0L),
    n_rural_secondary_locations = tidyr::replace_na(n_rural_secondary_locations, 0L),
    has_rural_secondary         = tidyr::replace_na(has_rural_secondary, FALSE),
    secondary_rural_states      = tidyr::replace_na(secondary_rural_states, ""),
    is_rural_primary            = ruca_classification == "rural",
    rural_status_combined = case_when(
      is_rural_primary &  has_rural_secondary ~ "rural_both",
      is_rural_primary & !has_rural_secondary ~ "rural_primary_only",
     !is_rural_primary &  has_rural_secondary ~ "rural_secondary_only",
      TRUE                                     ~ "not_rural_touching"
    )
  ) %>%
  arrange(desc(suppressWarnings(as.integer(ruca_primary))), state, npi)

cat("\n=== rural_status_combined breakdown ===\n")
print(cohort_enriched %>% count(rural_status_combined))

tmp_npi <- paste0(OUT_CSV_NPI, ".tmp")
write_csv(cohort_enriched, tmp_npi)
# 2026-05-21 (Facility-enrichment sweep 2 F2-#5): capture the
# file.rename() return value. A FALSE return means the atomic move
# failed (cross-device, permission, missing target dir) and the tmp
# file remains while the success message would have been printed
# anyway. Stop loudly so the operator sees the failure.
if (!isTRUE(file.rename(tmp_npi, OUT_CSV_NPI))) {
  stop(sprintf("[write] file.rename failed: %s -> %s (tmp preserved)",
               tmp_npi, OUT_CSV_NPI), call. = FALSE)
}
cat(sprintf("\nWrote enriched cohort: %s (%d rows, %d cols)\n",
            OUT_CSV_NPI, nrow(cohort_enriched), ncol(cohort_enriched)))

# ── 6. State-level rollup ──────────────────────────────────────────────────
# Primary side: each rural-primary physician counts in their primary state
state_primary <- cohort_enriched %>%
  filter(ruca_classification == "rural") %>%
  count(state, name = "n_rural_primary")

# Secondary side: each rural secondary practice counts in its address state
state_secondary <- sec_flagged %>%
  filter(is_rural_sec) %>%
  count(state, name = "n_rural_secondary_practices")

# Distinct NPI-state pairs touching rural (combined)
prim_state <- cohort_enriched %>% filter(ruca_classification == "rural") %>%
  transmute(npi, state)
sec_state  <- sec_flagged %>% filter(is_rural_sec) %>%
  transmute(npi, state)
combined_pairs <- bind_rows(prim_state, sec_state) %>%
  distinct(npi, state)
state_combined <- combined_pairs %>%
  count(state, name = "n_rural_combined_npi_state_pairs")

state_rollup <- tibble(state = setdiff(c(state.abb, "DC"), c("AK","HI"))) %>%
  left_join(state_primary,     by = "state") %>%
  left_join(state_secondary,   by = "state") %>%
  left_join(state_combined,    by = "state") %>%
  mutate(across(starts_with("n_"), ~ tidyr::replace_na(.x, 0L)),
         delta_secondary = n_rural_combined_npi_state_pairs - n_rural_primary) %>%
  arrange(desc(n_rural_combined_npi_state_pairs))

tmp_state <- paste0(OUT_CSV_STATE, ".tmp")
write_csv(state_rollup, tmp_state)
# 2026-05-21 (Facility-enrichment sweep 2 F2-#5): see comment above L188.
if (!isTRUE(file.rename(tmp_state, OUT_CSV_STATE))) {
  stop(sprintf("[write] file.rename failed: %s -> %s (tmp preserved)",
               tmp_state, OUT_CSV_STATE), call. = FALSE)
}
cat(sprintf("Wrote state rollup: %s (%d states + DC)\n",
            OUT_CSV_STATE, nrow(state_rollup)))
cat("\n=== State rollup (top 15) ===\n")
print(state_rollup, n = 15)

# ── 7. Build the v2 map: primary + secondary rural points ───────────────────
# Use REAL geocoded lat/lon from the canonical filter output (rooftop
# precision via Census batch geocoder).
#
# - Primary lat/lon comes from artifacts/secondary_locations/ent_primary_geocoded.csv
# - Secondary lat/lon comes from the filtered CSV (already loaded above)
#
# Fall back to ZCTA centroid for the small set of primaries whose Census
# batch geocode missed.
prim_geo_csv <- here("artifacts", "secondary_locations",
                     "ent_primary_geocoded.csv")
if (file.exists(prim_geo_csv)) {
  prim_geo <- read_csv(prim_geo_csv, show_col_types = FALSE,
                        col_types = cols(npi = col_character())) %>%
    select(npi, primary_lat, primary_lon)
} else {
  prim_geo <- tibble(npi = character(0),
                      primary_lat = numeric(0),
                      primary_lon = numeric(0))
}

cat("\nLoading ZCTA centroids as fallback for primaries the Census batch missed...\n")
zctas <- tigris::zctas(year = 2020, progress_bar = FALSE)
zcta_col <- intersect(c("ZCTA5CE20","ZCTA5CE10","GEOID20","GEOID10","ZCTA5CE"),
                       names(zctas))[1]
zctas_geom <- sf::st_make_valid(zctas)
coords <- sf::st_coordinates(sf::st_point_on_surface(zctas_geom))
zcta_centroids <- tibble::tibble(
  zip5     = as.character(zctas[[zcta_col]]),
  lon_zcta = coords[, 1],
  lat_zcta = coords[, 2]
)

# Primary rural points — real geocode first, ZCTA centroid as backup
prim_pts <- cohort_enriched %>%
  filter(ruca_classification == "rural") %>%
  left_join(prim_geo,        by = "npi") %>%
  left_join(zcta_centroids,  by = "zip5") %>%
  mutate(
    lon = coalesce(primary_lon, lon_zcta),
    lat = coalesce(primary_lat, lat_zcta),
    point_kind = "Primary practice (rural)"
  ) %>%
  filter(!is.na(lat), !is.na(lon))

# Secondary rural points — already carries real geocoded lat/lon
# 2026-05-21 (Facility-enrichment sweep 2 F2-#1): the FILTERED_SEC CSV
# at L26-27 supplies `latitude`/`longitude` (haversine-geocoded), but
# the fallback path at L41-42 (`read_nppes_pl_pfile(...)`) returns
# `.empty_pl_pfile_df()` whose schema (R/extract_secondary_practice_locations.R:599-614)
# is `npi, address_line1, address_line2, city, state, zip, facility_name,
# org_pac_id, dac_specialty, adrs_id, source, vintage_date` — NO lat/lon.
# Accessing `latitude`/`longitude` then errors with "object not found"
# and the whole map branch crashes. Guard: if the columns are absent,
# fall back to ZCTA centroid for the secondary points (same treatment
# as primary at L226-236).
.sec_has_geocode <- all(c("latitude", "longitude") %in% names(sec_flagged))
sec_pts <- if (.sec_has_geocode) {
  sec_flagged %>%
    filter(is_rural_sec) %>%
    mutate(
      lat = suppressWarnings(as.numeric(latitude)),
      lon = suppressWarnings(as.numeric(longitude)),
      point_kind = "Secondary practice (rural)"
    ) %>%
    filter(!is.na(lat), !is.na(lon))
} else {
  cat("[WARN] sec_flagged is missing latitude/longitude columns ",
      "(likely the read_nppes_pl_pfile fallback path); using ZCTA-centroid ",
      "fallback for secondary rural points.\n")
  sec_flagged %>%
    filter(is_rural_sec) %>%
    left_join(zcta_centroids, by = "zip5") %>%
    mutate(
      lat = lat_zcta,
      lon = lon_zcta,
      point_kind = "Secondary practice (rural, ZCTA centroid)"
    ) %>%
    filter(!is.na(lat), !is.na(lon))
}

cat(sprintf("Primary rural points (real geocode + ZCTA fallback): %d\n",
            nrow(prim_pts)))
# 2026-05-21 (Facility-enrichment sweep 3 F3-lower-severity): branch
# the cat message on .sec_has_geocode. The previous form claimed "real
# geocode only" even when the script had fallen back to ZCTA centroids
# (when read_nppes_pl_pfile() returned the empty-pl-pfile schema with
# no lat/lon). Make the labelling match reality.
cat(sprintf("Secondary rural points (%s): %d\n",
            if (.sec_has_geocode) "real geocode only" else "ZCTA centroid fallback",
            nrow(sec_pts)))

# CONUS state outlines
states_sf <- tigris::states(cb = TRUE, year = 2020, progress_bar = FALSE) %>%
  filter(!STUSPS %in% c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP"))

prim_sf <- sf::st_as_sf(prim_pts, coords = c("lon","lat"), crs = 4326,
                        remove = FALSE)
sec_sf  <- sf::st_as_sf(sec_pts,  coords = c("lon","lat"), crs = 4326,
                        remove = FALSE)

p_map <- ggplot() +
  geom_sf(data = states_sf, fill = "#f5f5f0", color = "#888888",
          linewidth = 0.25) +
  # Primary rural — filled orange dots
  geom_sf(data = prim_sf, aes(color = "Primary practice (rural)"),
          size = 1.6, alpha = 0.8, stroke = 0) +
  # Secondary rural — hollow blue rings (visually distinct)
  geom_sf(data = sec_sf, aes(color = "Secondary practice (rural)"),
          size = 2.1, alpha = 0.85, shape = 21,
          fill = NA, stroke = 0.9) +
  scale_color_manual(
    name = NULL,
    values = c("Primary practice (rural)"   = "#E64A19",
               "Secondary practice (rural)" = "#0F4C81")
  ) +
  coord_sf(crs = 5070, datum = NA) +
  labs(
    title = sprintf(
      "Rural ENT physicians: primary + secondary practice locations  (n = %s primary, %s secondary)",
      format(nrow(prim_pts), big.mark = ","),
      format(nrow(sec_pts),  big.mark = ",")
    ),
    # 2026-05-21 (Facility-enrichment sweep 3 F3-lower-severity): branch
    # subtitle + caption text on .sec_has_geocode so the figure metadata
    # actually describes the data shown when the script falls back to
    # ZCTA centroids.
    subtitle = paste0(
      "RUCA 4-10 ZIP codes, plotted at ",
      if (.sec_has_geocode) "real geocode" else "ZCTA centroid",
      ".  Filled orange = NPPES primary practice address.  ",
      "Hollow blue ring = NPPES secondary practice location",
      if (.sec_has_geocode) "" else " (ZCTA-centroid fallback)",
      ".  CONUS only."
    ),
    caption = paste0(
      "Sources: ABOto board certifications + NPPES practice addresses ",
      "(primary + pl_pfile secondary, Feb 2026 vintage) + USDA 2020 RUCA crosswalk.  ",
      if (.sec_has_geocode) ""
      else paste0("Secondary locations plotted at ZCTA centroid because the ",
                  "real-geocode filtered file was missing on this run.  "),
      "Some physicians appear as both colors when they have a rural primary AND a (separate) rural secondary."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 15,
                                       color = "#1a2b3c", margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 11, color = "#4a6880",
                                       margin = margin(b = 14), lineheight = 1.25),
    plot.caption       = element_text(size = 8.5, color = "#4a6880",
                                       hjust = 0, margin = margin(t = 12),
                                       lineheight = 1.35),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(14, 14, 14, 14)
  ) +
  guides(color = guide_legend(override.aes = list(
    size = c(3.5, 3.5),
    shape = c(16, 21),
    fill  = c(NA, NA),
    stroke = c(0, 1.1),
    alpha = 1
  )))

tmp_png_map <- paste0(OUT_PNG_MAP, ".tmp.png")
ggsave(tmp_png_map, p_map, width = 13, height = 8.5, dpi = 300, bg = "white")
# 2026-05-21 (Facility-enrichment sweep 2 F2-#5): see comment above L188.
if (!isTRUE(file.rename(tmp_png_map, OUT_PNG_MAP))) {
  stop(sprintf("[write] file.rename failed: %s -> %s (tmp preserved)",
               tmp_png_map, OUT_PNG_MAP), call. = FALSE)
}
cat(sprintf("\nWrote map v2: %s\n", OUT_PNG_MAP))

# ── 8. Receipt ──────────────────────────────────────────────────────────────
receipt <- list(
  figure_path = OUT_PNG_MAP,
  csv_npi     = OUT_CSV_NPI,
  csv_state   = OUT_CSV_STATE,
  script_path = "scripts/add_secondary_locations_to_cohort.R",
  rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version   = paste0(R.version$major, ".", R.version$minor),
  live_counts = list(
    n_cohort                          = nrow(cohort_enriched),
    n_npis_with_any_secondary         = sum(cohort_enriched$n_secondary_locations > 0),
    n_npis_with_rural_secondary       = sum(cohort_enriched$has_rural_secondary),
    n_rural_primary                   = sum(cohort_enriched$is_rural_primary),
    n_rural_combined_pairs            = nrow(combined_pairs),
    n_primary_rural_with_centroid     = nrow(prim_pts),
    n_secondary_rural_with_centroid   = nrow(sec_pts),
    rural_status_breakdown            = as.list(table(cohort_enriched$rural_status_combined))
  )
)
receipt_path <- paste0(OUT_PNG_MAP, ".receipt.json")
tmp_receipt  <- paste0(receipt_path, ".tmp")
write_json(receipt, tmp_receipt, auto_unbox = TRUE, pretty = TRUE)
# 2026-05-21 (Facility-enrichment sweep 2 F2-#5): see comment above L188.
if (!isTRUE(file.rename(tmp_receipt, receipt_path))) {
  stop(sprintf("[write] file.rename failed: %s -> %s (tmp preserved)",
               tmp_receipt, receipt_path), call. = FALSE)
}
cat("Wrote receipt:", receipt_path, "\n")
