#!/usr/bin/env Rscript
# =============================================================================
# Interactive Leaflet version of rural_ent_physicians_map_v2.png
# =============================================================================
# Mirrors the static map's color scheme and legend so the static / interactive
# pair reads as the same map in two formats.
#
# Basemap: CartoDB.Positron (project convention — see
# manuscript/R/build_interactive_maps.R:277 and
# manuscript/R/03b_spatial_visualizations.R:737).
#
# Colors / shapes (matching rural_ent_physicians_map_v2.png):
#   Primary practice (rural)   = filled orange  (#E64A19), solid circle
#   Secondary practice (rural) = hollow blue    (#0F4C81), ring
#
# Output: publication_materials/figures/ent_data_integrity/
#         rural_ent_physicians_leaflet_v1.html
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(leaflet)
  library(htmlwidgets); library(sf); library(tigris); library(tibble)
  library(arrow); library(jsonlite)
})
options(tigris_use_cache = TRUE)

source(here("R", "extract_secondary_practice_locations.R"))
source(here("R", "ent_registry_counts.R"))

OUT_DIR  <- here("publication_materials", "figures", "ent_data_integrity")
OUT_HTML <- file.path(OUT_DIR, "rural_ent_physicians_leaflet_v1.html")
# Bug Round LL50 (2026-05-22): OUT_DIR was not created before the downstream
# saveWidget()/write_json() writes — the first run on a fresh checkout
# crashed with a misleading "cannot open file" deep inside saveWidget.
# Idempotent dir.create with recursive=TRUE.
if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(OUT_DIR)) {
    stop(sprintf(
      "[build_rural_ent_leaflet] Failed to create OUT_DIR: %s\n  Check parent directory permissions / disk space.",
      OUT_DIR
    ), call. = FALSE)
  }
}

PRIM_COL <- "#E64A19"   # matches static map orange
SEC_COL  <- "#0F4C81"   # matches static map blue

# Format an address line as: "City, ST ZIP - RUCA-N"  (matches user spec)
fmt_loc <- function(city, state, zip, ruca) {
  # Bug Round LL43 (2026-05-22): the prior code compared numeric inputs to
  # "" via `ruca == ""` which silently coerces.  For numeric ruca this is
  # always FALSE (no `""` numeric); for character ruca with whitespace
  # like "  " the empty-string compare misses.  Normalise to character +
  # trimws before the empty/NA check so all 4 callers (character/numeric/
  # whitespace/NA) get the same "?" or "" treatment.
  .blank <- function(x) {
    .v <- as.character(x)
    is.na(.v) | !nzchar(trimws(.v))
  }
  city_s <- ifelse(.blank(city),  "", as.character(city))
  st_s   <- ifelse(.blank(state), "", as.character(state))
  zip_s  <- ifelse(.blank(zip),   "", as.character(zip))
  ruca_s <- ifelse(.blank(ruca),  "?", as.character(ruca))
  paste0(city_s, ", ", st_s, " ", zip_s, " &mdash; RUCA-", ruca_s)
}

# ── 1. Load enriched cohort + secondary locations ───────────────────────────
# Bug Round LL41 (2026-05-22): read_csv was unwrapped — a missing or
# unreadable cohort CSV produced a cryptic `vroom` error.  Wrap with
# tryCatch + a path-named diagnostic so the operator sees what failed.
.cohort_csv <- file.path(OUT_DIR, "board_cert_ent_universe_n11333_zip_aboto.csv")
cohort <- tryCatch(
  read_csv(.cohort_csv, col_types = cols(.default = "c")),
  error = function(e) {
    stop(sprintf(
      "[build_rural_ent_leaflet] read_csv failed for cohort '%s': %s\n  Check the file exists and the upstream cohort-build ran.",
      .cohort_csv, conditionMessage(e)
    ), call. = FALSE)
  }
)

# Pull primary CITY from NPPES parquet (cohort CSV doesn't carry it)
ct <- compute_ent_registry_counts()
# Bug Round LL42 (2026-05-22): same unwrapped-read issue for arrow::read_parquet.
.nppes_path <- ct$input_paths$nppes
nppes_primary <- tryCatch(
  read_parquet(.nppes_path),
  error = function(e) {
    stop(sprintf(
      "[build_rural_ent_leaflet] read_parquet failed for NPPES '%s': %s",
      .nppes_path, conditionMessage(e)
    ), call. = FALSE)
  }
) %>%
  mutate(npi = as.character(npi),
         zip5_full = as.character(zip),
         primary_city = as.character(city)) %>%
  select(npi, primary_city)

cohort <- cohort %>% left_join(nppes_primary, by = "npi")

# Load filtered ENT secondaries (real geocoded + 5km haversine).
# Falls back to live pl_pfile extraction if not yet produced.
FILTERED_SEC <- here("artifacts", "secondary_locations",
                      "ent_secondary_locations_filtered.csv")
if (file.exists(FILTERED_SEC)) {
  cat("Using filtered ENT secondaries: ", FILTERED_SEC, "\n", sep = "")
  sec <- read_csv(FILTERED_SEC, show_col_types = FALSE,
                   col_types = cols(npi = col_character()))
} else {
  cat("[WARN] Filtered file missing; falling back to NPPES pl_pfile.\n")
  sec <- read_nppes_pl_pfile(cohort_npis = unique(cohort$npi),
                              candidates = NPPES_PL_PFILE_CANDIDATES)
}

ruca_raw <- read_csv(here("data/external/ruca_2020_zip.csv"),
                     show_col_types = FALSE) %>%
  # BUG FIX (round-2): as.integer() loses leading zeros AND returns NA for
  # any ZIPCode that isn't a clean number (e.g., ZIP+4 hyphens, alpha-numeric
  # PR ZIPs). Use the str_pad / substr idiom used elsewhere in the project.
  mutate(zip5 = stringr::str_pad(substr(as.character(ZIPCode), 1, 5),
                                  5, "left", "0")) %>%
  distinct(zip5, .keep_all = TRUE)

ruca <- ruca_raw %>%
  select(zip5, ruca_primary_sec = PrimaryRUCA,
                ruca_secondary_sec = SecondaryRUCA)

# Ensure cohort has ruca_classification + ruca_primary (may be absent on first run).
if (!"ruca_classification" %in% names(cohort) || !"ruca_primary" %in% names(cohort)) {
  cat("[INFO] ruca_classification/ruca_primary absent from cohort — computing from RUCA crosswalk.\n")
  ruca_primary <- ruca_raw %>%
    select(zip5, ruca_primary = PrimaryRUCA)
  # BUG FIX (round-2): same ZIP+4 / leading-zero issue as the secondary path.
  cohort <- cohort %>%
    mutate(zip5 = stringr::str_pad(substr(as.character(zip5), 1, 5),
                                    5, "left", "0")) %>%
    left_join(ruca_primary, by = "zip5") %>%
    mutate(ruca_classification = case_when(
      is.na(ruca_primary)                ~ "unknown",
      as.integer(ruca_primary) %in% 1:3  ~ "urban",
      TRUE                               ~ "rural"
    ))
}

NON_CONUS <- c("AK","HI","PR","VI","GU","MP","AS","AA","AE","AP")
# BUG FIX (round-2): as.integer() on ZIP+4 strings like "12345-6789" yields
# NA with a warning, then sprintf("%05d", NA) produces the literal "NA",
# breaking the join. Strip to the first 5 chars then left-pad.
sec_flagged <- sec %>%
  mutate(zip5 = stringr::str_pad(substr(as.character(zip), 1, 5),
                                  5, "left", "0")) %>%
  left_join(ruca, by = "zip5") %>%
  filter(!state %in% NON_CONUS, state %in% c(state.abb, "DC"),
         !is.na(state)) %>%
  mutate(is_rural_sec = !is.na(ruca_primary_sec) &
                         !(as.integer(ruca_primary_sec) %in% 1:3),
         # Secondary lat/lon ALREADY in the filtered CSV (Census batch).
         sec_lon = suppressWarnings(as.numeric(longitude)),
         sec_lat = suppressWarnings(as.numeric(latitude)))

# Primary lat/lon from the canonical primary-geocode artifact (rooftop).
prim_geo_csv <- here("artifacts", "secondary_locations",
                      "ent_primary_geocoded.csv")
prim_geo <- if (file.exists(prim_geo_csv)) {
  # BUG FIX (round-2): dedup prim_geo on npi before downstream joins. The
  # canonical primary-geocode CSV occasionally carries a duplicate NPI row
  # (e.g., when the cohort was re-geocoded mid-run). Without dedup, the
  # left_join below fans out the primary frame and draws duplicate markers.
  read_csv(prim_geo_csv, show_col_types = FALSE,
            col_types = cols(npi = col_character())) %>%
    select(npi, primary_lat, primary_lon) %>%
    distinct(npi, .keep_all = TRUE)
} else {
  tibble(npi = character(0), primary_lat = numeric(0), primary_lon = numeric(0))
}

# ── 2. ZCTA centroids (fallback for primaries the Census batch missed) ─────
# Bug Round LL44 (2026-05-22): tigris::zctas() hits the Census Bureau API
# on cache-miss; on a network outage it threw a raw "could not resolve
# host" and aborted the script — there was no remediation hint pointing
# at the tigris cache.  Wrap with tryCatch + a remediation message.
cat("Loading ZCTA centroids via tigris (cached, fallback only)...\n")
zctas <- tryCatch(
  tigris::zctas(year = 2020, progress_bar = FALSE),
  error = function(e) {
    stop(sprintf(
      "[build_rural_ent_leaflet] tigris::zctas(2020) failed: %s\n  Check network or set options(tigris_use_cache = TRUE) with a pre-cached dataset.",
      conditionMessage(e)
    ), call. = FALSE)
  }
)
zcta_col <- intersect(c("ZCTA5CE20","ZCTA5CE10","GEOID20","GEOID10","ZCTA5CE"),
                       names(zctas))[1]
coords <- sf::st_coordinates(sf::st_point_on_surface(sf::st_make_valid(zctas)))
zcta_centroids <- tibble(
  zip5     = as.character(zctas[[zcta_col]]),
  lon_zcta = coords[, 1],
  lat_zcta = coords[, 2]
)

# ── 3. Build the two point layers ──────────────────────────────────────────
# Per-NPI rural-secondary summary, so primary-marker popups can list
# every rural secondary site this physician has.
sec_summary_for_prim <- sec_flagged %>%
  filter(is_rural_sec) %>%
  group_by(npi) %>%
  summarise(
    sec_rural_block = paste0(
      "<li>",
      fmt_loc(city, state, zip, ruca_primary_sec),
      "</li>",
      collapse = ""
    ),
    n_rural_sec = dplyr::n(),
    .groups = "drop"
  )

# CONUS bounding box for sanity-clipping bad geocoder hits.
in_conus <- function(lat, lon) {
  # Bug Round LL49 (2026-05-22): the prior comparisons (lat >= 24 etc.)
  # silently coerced character input to numeric and returned NA-filled
  # logicals — non-numeric lat/lon vectors were treated as "outside
  # CONUS" with no diagnostic.  Coerce explicitly via as.numeric() with
  # suppressWarnings so the contract is "any coercible-to-numeric value
  # works; non-numeric strings become NA and are correctly filtered out
  # by the !is.na guard".
  lat_n <- suppressWarnings(as.numeric(lat))
  lon_n <- suppressWarnings(as.numeric(lon))
  !is.na(lat_n) & !is.na(lon_n) &
    lat_n >=  24 & lat_n <= 50 &
    lon_n >= -125 & lon_n <= -66
}

prim <- cohort %>%
  filter(ruca_classification == "rural") %>%
  left_join(prim_geo,        by = "npi") %>%
  left_join(zcta_centroids,  by = "zip5") %>%
  # Real geocode wins; ZCTA centroid only when Census batch missed.
  mutate(lon = coalesce(primary_lon, lon_zcta),
         lat = coalesce(primary_lat, lat_zcta)) %>%
  filter(in_conus(lat, lon)) %>%
  mutate(
    popup = sprintf(paste0(
      "<div style='font-family:-apple-system,sans-serif;font-size:13px;",
      "min-width:260px;line-height:1.5'>",
      "<b style='color:%s'>Primary practice (rural)</b><br/>",
      "<b>%s, %s</b> &mdash; NPI: %s<br/>",
      "<u>Primary practice location</u><br/>",
      "&nbsp;&nbsp;%s<br/>",
      "Subspecialty (ABOto): %s",
      "</div>"
    ),
    PRIM_COL,
    ifelse(is.na(last_name), "", last_name),
    ifelse(is.na(first_name), "", first_name),
    npi,
    fmt_loc(primary_city, state, zip5, ruca_primary),
    ifelse(is.na(aboto_subspecialty), "-", aboto_subspecialty)
    )
  )

sec_pts <- sec_flagged %>%
  filter(is_rural_sec) %>%
  # Real-geocoded lat/lon only — drop the 42 secondaries the Census batch
  # missed (no ZCTA-centroid fallback) so leaflet count matches static map.
  mutate(lon = sec_lon, lat = sec_lat) %>%
  filter(in_conus(lat, lon)) %>%
  # BUG FIX (round-2): dedup cohort on NPI before the left_join. The cohort
  # CSV (board_cert_ent_universe_*) is single-row-per-NPI by design, but a
  # CSV regenerated from a multi-vintage source can carry duplicates. Without
  # dedup, the join fans out sec_pts and we draw the same secondary marker
  # multiple times.
  left_join(cohort %>% select(
    npi,
    last_name_cohort  = last_name,
    first_name_cohort = first_name,
    aboto_subspecialty,
    primary_state     = state,
    primary_zip       = zip5,
    primary_ruca      = ruca_primary,
    primary_city
  ) %>% distinct(npi, .keep_all = TRUE),
  by = "npi") %>%
  mutate(
    popup = sprintf(paste0(
      "<div style='font-family:-apple-system,sans-serif;font-size:13px;",
      "min-width:280px;line-height:1.5'>",
      "<b style='color:%s'>Secondary practice (rural)</b><br/>",
      "<b>%s, %s</b> &mdash; NPI: %s<br/>",
      "<u>Secondary practice location</u> (this point)<br/>",
      "&nbsp;&nbsp;%s<br/>",
      "<u>Primary practice location</u> (for reference)<br/>",
      "&nbsp;&nbsp;%s<br/>",
      "Subspecialty (ABOto): %s",
      "</div>"
    ),
    SEC_COL,
    ifelse(is.na(last_name_cohort),  "", last_name_cohort),
    ifelse(is.na(first_name_cohort), "", first_name_cohort),
    npi,
    fmt_loc(city, state, zip, ruca_primary_sec),
    fmt_loc(primary_city, primary_state, primary_zip, primary_ruca),
    ifelse(is.na(aboto_subspecialty), "-", aboto_subspecialty)
    )
  )

cat(sprintf("Primary rural points: %d\nSecondary rural points: %d\n",
            nrow(prim), nrow(sec_pts)))

# ── 4. Build leaflet map ───────────────────────────────────────────────────
legend_html <- sprintf(paste0(
  "<div style='background:white;padding:10px 14px;border-radius:6px;",
  "box-shadow:0 1px 4px rgba(0,0,0,0.2);font-family:-apple-system,sans-serif;",
  "font-size:12px;line-height:1.6'>",
  "<div style='font-weight:bold;font-size:13px;color:#1a2b3c;margin-bottom:6px'>",
  "Rural ENT practice locations</div>",
  "<div><span style='display:inline-block;width:12px;height:12px;",
  "background:%s;border-radius:50%%;margin-right:6px;vertical-align:middle'></span>",
  "Primary practice (rural)  -  n = %s</div>",
  "<div><span style='display:inline-block;width:12px;height:12px;",
  "border:2px solid %s;border-radius:50%%;background:transparent;",
  "margin-right:6px;vertical-align:middle'></span>",
  "Secondary practice (rural)  -  n = %s</div>",
  "<div style='margin-top:6px;color:#4a6880;font-size:11px;line-height:1.3'>",
  "RUCA 4-10 ZIP codes plotted at ZIP centroid.<br/>",
  "Click any point for physician details.</div>",
  "</div>"
),
PRIM_COL, format(nrow(prim), big.mark = ","),
SEC_COL,  format(nrow(sec_pts), big.mark = ","))

title_html <- paste0(
  "<div style='background:white;padding:8px 14px;border-radius:6px;",
  "box-shadow:0 1px 4px rgba(0,0,0,0.2);font-family:-apple-system,sans-serif;",
  "font-size:14px;font-weight:bold;color:#1a2b3c'>",
  "Rural ENT physicians  -  primary + secondary practice locations</div>"
)

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  # CartoDB.Positron — project-canonical basemap
  addProviderTiles(providers$CartoDB.Positron,
                    options = providerTileOptions(noWrap = TRUE)) %>%
  # Primary rural — filled orange circle (matches static map's geom_sf shape 16)
  addCircleMarkers(
    data = prim,
    lng  = ~lon, lat = ~lat,
    radius      = 5,
    color       = PRIM_COL,
    fillColor   = PRIM_COL,
    fillOpacity = 0.78,
    weight      = 0,
    opacity     = 1,
    popup       = ~popup,
    group       = "Primary practice (rural)"
  ) %>%
  # Secondary rural — hollow blue ring (matches static map's shape 21)
  addCircleMarkers(
    data = sec_pts,
    lng  = ~lon, lat = ~lat,
    radius      = 6,
    color       = SEC_COL,
    fillColor   = "transparent",
    fillOpacity = 0,
    weight      = 1.6,
    opacity     = 0.9,
    popup       = ~popup,
    group       = "Secondary practice (rural)"
  ) %>%
  # Layer toggle (matches the two legend rows)
  addLayersControl(
    overlayGroups = c("Primary practice (rural)",
                       "Secondary practice (rural)"),
    options       = layersControlOptions(collapsed = FALSE),
    position      = "topright"
  ) %>%
  addControl(html = title_html, position = "topleft") %>%
  addControl(html = legend_html, position = "bottomright") %>%
  setView(lng = -96, lat = 38, zoom = 4)

# Save self-contained HTML (no external dependencies)
# Bug Round LL45 (2026-05-22): tmp_html was `.tmp.html` with no PID/host
# entropy — two concurrent runs on the same OUT_DIR would race.  Use
# host+PID+nanosecond entropy in the suffix.
# Bug Round LL45 (cont): saveWidget unwrapped — pandoc-missing produced
# a cryptic message.  Wrap with a pandoc-aware hint.
tmp_html <- paste0(
  OUT_HTML, ".tmp.",
  Sys.info()[["nodename"]], ".", Sys.getpid(), ".",
  sub("\\.", "", sprintf("%.6f", as.numeric(Sys.time()))),
  ".html"
)
.sw_ok <- tryCatch({
  htmlwidgets::saveWidget(m, tmp_html, selfcontained = TRUE,
                           title = "Rural ENT practice locations - leaflet")
  TRUE
}, error = function(e) {
  .hint <- if (grepl("pandoc", conditionMessage(e), ignore.case = TRUE))
             " — selfcontained=TRUE requires pandoc on PATH"
           else ""
  stop(sprintf(
    "[build_rural_ent_leaflet] saveWidget failed: %s%s",
    conditionMessage(e), .hint
  ), call. = FALSE)
})
# Bug Round LL46 (2026-05-22): file.rename() return value was dropped.
# A cross-device rename would silently leave tmp_html behind and OUT_HTML
# would never appear — the next file.info(OUT_HTML)$size read NA and the
# operator only noticed when downstream consumers complained.  Capture
# the return and fall back to file.copy(); raise an error if both fail.
.rn_ok <- isTRUE(file.rename(tmp_html, OUT_HTML))
if (!.rn_ok) {
  .cp_ok <- isTRUE(file.copy(tmp_html, OUT_HTML, overwrite = TRUE))
  if (file.exists(tmp_html)) try(file.remove(tmp_html), silent = TRUE)
  if (!.cp_ok) {
    stop(sprintf(
      "[build_rural_ent_leaflet] Both file.rename and file.copy failed for %s -> %s",
      tmp_html, OUT_HTML
    ), call. = FALSE)
  }
}
cat(sprintf("\nWrote leaflet: %s\n", OUT_HTML))
# Bug Round LL47 (2026-05-22): file.info()$size returned NA when the
# rename silently failed (pre-LL46) — sprintf("%.1f", NA) printed "NA MB"
# without an error.  After LL46 above guards the rename path explicitly,
# also guard the size read so a follow-up stat() failure surfaces a
# clear message instead of a misleading log.
.out_sz <- file.info(OUT_HTML)$size
if (is.na(.out_sz)) {
  warning(sprintf(
    "[build_rural_ent_leaflet] file.info()$size=NA for %s (stat failed after rename)",
    OUT_HTML
  ), call. = FALSE)
  cat("File size: unknown (stat failed)\n")
} else {
  cat(sprintf("File size: %.1f MB\n", .out_sz / 1024^2))
}

# Receipt.
# Bug Round LL48 (2026-05-22): tmp_receipt was `.tmp` with no PID/host
# entropy — concurrent runs of this script (or another writer to the
# same receipt path) would race.  Use the same host+PID+nanosecond
# pattern as LL45 above.
receipt_path <- paste0(OUT_HTML, ".receipt.json")
tmp_receipt  <- paste0(
  receipt_path, ".tmp.",
  Sys.info()[["nodename"]], ".", Sys.getpid(), ".",
  sub("\\.", "", sprintf("%.6f", as.numeric(Sys.time())))
)
write_json(list(
  figure_path     = OUT_HTML,
  static_pair     = file.path(OUT_DIR, "rural_ent_physicians_map_v2.png"),
  script_path     = "scripts/build_rural_ent_leaflet.R",
  basemap         = "CartoDB.Positron (project convention)",
  colors          = list(primary = PRIM_COL, secondary = SEC_COL),
  rendered_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version       = paste0(R.version$major, ".", R.version$minor),
  live_counts     = list(
    n_primary_rural   = nrow(prim),
    n_secondary_rural = nrow(sec_pts)
  )
), tmp_receipt,
auto_unbox = TRUE, pretty = TRUE)
file.rename(tmp_receipt, receipt_path)
cat("Receipt:", receipt_path, "\n")
