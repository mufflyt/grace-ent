#!/usr/bin/env Rscript
# =============================================================================
# Interactive Leaflet equivalent of rural_ent_physicians_map_v1.png
# =============================================================================
# This is the PRIMARY-ONLY companion to the existing combined leaflet
# (rural_ent_physicians_leaflet_v1.html which shows primary + secondary).
#
# Visual encoding matches rural_ent_physicians_map_v1.png exactly:
#   - One filled dot per physician's PRIMARY NPPES practice address
#   - Color graded by RUCA primary code (4 warm yellow -> 10 deep red)
#   - CartoDB.Positron basemap (project convention)
#
# Popup format mirrors the cleaned combined leaflet (cleaned-popups version):
#   - Primary practice (rural) header
#   - Name + NPI
#   - "City, ST ZIP - RUCA-N" primary location
#   - Subspecialty (ABOto)
# No secondary-location code text, no Triangulation snake_case bucket.
#
# Output:
#   publication_materials/figures/ent_data_integrity/
#     rural_ent_physicians_primary_leaflet_v1.html
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(leaflet)
  library(htmlwidgets); library(tibble); library(arrow)
  library(sf); library(tigris); library(jsonlite)
})
options(tigris_use_cache = TRUE)

source(here("R", "ent_registry_counts.R"))

OUT_DIR  <- here("publication_materials", "figures", "ent_data_integrity")
OUT_HTML <- file.path(OUT_DIR, "rural_ent_physicians_primary_leaflet_v1.html")

# RUCA palette matches rural_ent_physicians_map_v1.png
RUCA_PALETTE <- c(
  "4"  = "#FFC400",   # warm yellow  - micropolitan urban core
  "5"  = "#FF9100",
  "6"  = "#FF6F00",
  "7"  = "#E64A19",   # orange-red   - small town urban core
  "8"  = "#D32F2F",
  "9"  = "#B71C1C",
  "10" = "#7F1D1D"    # deep red     - rural / completely isolated
)
RUCA_LABEL <- c(
  "4"  = "4 - Micropolitan (urban core)",
  "5"  = "5 - Micropolitan (high commute)",
  "6"  = "6 - Micropolitan (low commute)",
  "7"  = "7 - Small town (urban core)",
  "8"  = "8 - Small town (high commute)",
  "9"  = "9 - Small town (low commute)",
  "10" = "10 - Rural / isolated"
)

#' HTML-escape user-supplied text for inclusion in a leaflet popup.
#'
#' BUG FIX 2026-05-18 (round-4 #10): physician last_names / facility names
#' can contain `&` (e.g., "Smith & Jones"), `<`, `>`, or quotes that would
#' break the popup HTML or be silently dropped. Escape the standard 5 to
#' keep the popup valid.
.html_esc <- function(x) {
  s <- ifelse(is.na(x), "", as.character(x))
  s <- gsub("&", "&amp;",  s, fixed = TRUE)
  s <- gsub("<", "&lt;",   s, fixed = TRUE)
  s <- gsub(">", "&gt;",   s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s <- gsub("'", "&#39;",  s, fixed = TRUE)
  s
}

# Address format helper (matches combined leaflet). HTML-safe: escapes
# city/state/zip values before inserting them into the popup template.
fmt_loc <- function(city, state, zip, ruca) {
  city_s <- .html_esc(ifelse(is.na(city) | city == "", "", city))
  st_s   <- .html_esc(ifelse(is.na(state) | state == "", "", state))
  zip_s  <- .html_esc(ifelse(is.na(zip) | zip == "", "", zip))
  ruca_s <- ifelse(is.na(ruca) | ruca == "", "?", as.character(ruca))
  paste0(city_s, ", ", st_s, " ", zip_s, " &mdash; RUCA-", ruca_s)
}

# ── 1. Cohort + primary city ────────────────────────────────────────────────
cohort <- read_csv(
  file.path(OUT_DIR, "board_cert_ent_universe_n11333_zip_aboto.csv"),
  col_types = cols(.default = "c")
)

ct <- compute_ent_registry_counts()
# BUG FIX 2026-05-18 (round-3 #7): dedup nppes on npi before the join.
# NPPES parquet can have duplicate NPIs (e.g., when the file aggregates
# multiple vintages or both Type 1 and Type 2 records for an individual).
# Without dedup, the left_join fans out the cohort frame, then the popup
# loop draws multiple markers per physician at the same coords.
nppes_primary <- read_parquet(ct$input_paths$nppes) %>%
  mutate(npi = as.character(npi),
         primary_city = as.character(city)) %>%
  select(npi, primary_city) %>%
  distinct(npi, .keep_all = TRUE)
cohort <- cohort %>% left_join(nppes_primary, by = "npi")

# ── 2. Real geocoded primary lat/lon (from canonical Census batch run) ──────
prim_geo_csv <- here("artifacts", "secondary_locations",
                      "ent_primary_geocoded.csv")
prim_geo <- if (file.exists(prim_geo_csv)) {
  # BUG FIX (round-2): dedup prim_geo on NPI to prevent fan-out in the
  # downstream left_join (same fix as build_rural_ent_leaflet.R).
  read_csv(prim_geo_csv, show_col_types = FALSE,
            col_types = cols(npi = col_character())) %>%
    select(npi, primary_lat, primary_lon) %>%
    distinct(npi, .keep_all = TRUE)
} else {
  stop("Primary geocoded CSV missing: ", prim_geo_csv,
       "\n  Run scripts/filter_ent_secondary_by_haversine.R first.")
}

# CONUS bounding-box sanity clip (matches combined leaflet)
in_conus <- function(lat, lon) {
  !is.na(lat) & !is.na(lon) &
    lat >=  24 & lat <= 50 &
    lon >= -125 & lon <= -66
}

# ── 2b. ZCTA centroids (fallback for the small set Census batch missed) ───
# BUG FIX 2026-05-18 (round-3 #6 + #9): prefer the cached
# data/reference/zcta_centroids_2020.csv (produced by
# R/extract_secondary_practice_locations.R::load_zcta_centroids()). It's a
# ~1.5 MB CSV vs ~80 MB tigris download. When the cache is missing, build
# via tigris with cb=TRUE (cartographic boundary file, ~5x smaller than
# the default TIGER/Line download) and write the cache for next time.
cached_centroids <- here("data", "reference", "zcta_centroids_2020.csv")
if (file.exists(cached_centroids)) {
  cat("Loading cached ZCTA centroids: ", cached_centroids, "\n", sep = "")
  zcta_centroids <- readr::read_csv(cached_centroids, show_col_types = FALSE) %>%
    transmute(zip5     = stringr::str_pad(as.character(.data$zcta5),
                                          5, "left", "0"),
              lon_zcta = .data$centroid_lon,
              lat_zcta = .data$centroid_lat)
} else {
  cat("Building ZCTA centroids via tigris (cb=TRUE, smaller file)...\n")
  zctas <- tigris::zctas(year = 2020, cb = TRUE, progress_bar = FALSE)
  zcta_col <- intersect(c("ZCTA5CE20","ZCTA5CE10","GEOID20","GEOID10","ZCTA5CE"),
                         names(zctas))[1]
  coords <- sf::st_coordinates(sf::st_point_on_surface(sf::st_make_valid(zctas)))
  zcta_centroids <- tibble(
    zip5     = stringr::str_pad(as.character(zctas[[zcta_col]]),
                                 5, "left", "0"),
    lon_zcta = coords[, 1],
    lat_zcta = coords[, 2]
  )
  dir.create(dirname(cached_centroids), recursive = TRUE, showWarnings = FALSE)
  tmp_centroids <- paste0(cached_centroids, ".tmp")
  readr::write_csv(zcta_centroids %>%
                     transmute(zcta5 = .data$zip5,
                               centroid_lat = .data$lat_zcta,
                               centroid_lon = .data$lon_zcta),
                   tmp_centroids)
  file.rename(tmp_centroids, cached_centroids)
}

# BUG FIX 2026-05-18 (round-2): cohort CSV may lack ruca_classification /
# ruca_primary if add_secondary_locations_to_cohort.R hasn't been run yet.
# Compute them here defensively so the filter below never errors.
if (!"ruca_classification" %in% names(cohort) || !"ruca_primary" %in% names(cohort)) {
  cat("[INFO] ruca_classification/ruca_primary absent from cohort — computing from RUCA crosswalk.\n")
  # BUG FIX (round-2): as.integer() on ZIP+4 / non-numeric ZIPs yields NA
  # then sprintf("%05d", NA) emits the literal "NA". Use the str_pad/substr
  # idiom that's standard across this project.
  ruca_raw_tmp <- readr::read_csv(here("data/external/ruca_2020_zip.csv"),
                                  show_col_types = FALSE) %>%
    dplyr::mutate(zip5 = stringr::str_pad(substr(as.character(ZIPCode), 1, 5),
                                           5, "left", "0")) %>%
    dplyr::distinct(zip5, .keep_all = TRUE) %>%
    dplyr::select(zip5, ruca_primary = PrimaryRUCA)
  cohort <- cohort %>%
    dplyr::mutate(zip5 = stringr::str_pad(substr(as.character(zip5), 1, 5),
                                           5, "left", "0")) %>%
    dplyr::left_join(ruca_raw_tmp, by = "zip5") %>%
    dplyr::mutate(ruca_classification = dplyr::case_when(
      is.na(ruca_primary)               ~ "unknown",
      as.integer(ruca_primary) %in% 1:3 ~ "urban",
      TRUE                              ~ "rural"
    ))
}

# ── 3. Build rural-primary point layer ─────────────────────────────────────
prim <- cohort %>%
  filter(ruca_classification == "rural") %>%
  left_join(prim_geo,       by = "npi") %>%
  left_join(zcta_centroids, by = "zip5") %>%
  # Real geocode wins; ZCTA centroid only when Census batch missed.
  mutate(plot_lon = coalesce(primary_lon, lon_zcta),
         plot_lat = coalesce(primary_lat, lat_zcta)) %>%
  filter(in_conus(plot_lat, plot_lon)) %>%
  mutate(
    ruca_str   = as.character(ruca_primary),
    ruca_label = unname(RUCA_LABEL[ruca_str]),
    ruca_color = unname(RUCA_PALETTE[ruca_str]),
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
    ruca_color,
    .html_esc(ifelse(is.na(last_name),  "", last_name)),
    .html_esc(ifelse(is.na(first_name), "", first_name)),
    npi,
    fmt_loc(primary_city, state, zip5, ruca_primary),
    .html_esc(ifelse(is.na(aboto_subspecialty), "-", aboto_subspecialty))
    )
  )

cat(sprintf("Rural primary points (real geocode, CONUS): %d\n", nrow(prim)))

# ── 4. Build legend HTML (matches static v1 figure legend) ─────────────────
legend_rows <- paste0(
  vapply(c("4","5","6","7","8","9","10"), function(k) {
    sprintf(paste0(
      "<div><span style='display:inline-block;width:12px;height:12px;",
      "background:%s;border-radius:50%%;margin-right:6px;",
      "vertical-align:middle'></span>%s</div>"
    ), RUCA_PALETTE[k], RUCA_LABEL[k])
  }, character(1)),
  collapse = ""
)

legend_html <- sprintf(paste0(
  "<div style='background:white;padding:10px 14px;border-radius:6px;",
  "box-shadow:0 1px 4px rgba(0,0,0,0.2);font-family:-apple-system,sans-serif;",
  "font-size:12px;line-height:1.6'>",
  "<div style='font-weight:bold;font-size:13px;color:#1a2b3c;margin-bottom:6px'>",
  "RUCA primary code  &mdash;  n = %s</div>",
  "%s",
  "<div style='margin-top:6px;color:#4a6880;font-size:11px;line-height:1.3'>",
  "Each point = ZIP centroid of one physician's<br/>",
  "PRIMARY NPPES practice address (RUCA 4-10).<br/>",
  "Click any point for physician details.</div>",
  "</div>"
),
format(nrow(prim), big.mark = ","),
legend_rows)

title_html <- paste0(
  "<div style='background:white;padding:8px 14px;border-radius:6px;",
  "box-shadow:0 1px 4px rgba(0,0,0,0.2);font-family:-apple-system,sans-serif;",
  "font-size:14px;font-weight:bold;color:#1a2b3c'>",
  "Rural board-certified ENT physicians  &mdash;  PRIMARY practice address</div>"
)

# ── 5. Leaflet ─────────────────────────────────────────────────────────────
m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron,
                    options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    data = prim,
    lng         = ~plot_lon,
    lat         = ~plot_lat,
    radius      = 5,
    color       = ~ruca_color,
    fillColor   = ~ruca_color,
    fillOpacity = 0.78,
    weight      = 0,
    opacity     = 1,
    popup       = ~popup
  ) %>%
  addControl(html = title_html,  position = "topleft") %>%
  addControl(html = legend_html, position = "bottomright") %>%
  setView(lng = -96, lat = 38, zoom = 4)

tmp_html <- paste0(OUT_HTML, ".tmp.html")
htmlwidgets::saveWidget(m, tmp_html, selfcontained = TRUE,
                         title = "Rural ENT primary practice locations - leaflet")
file.rename(tmp_html, OUT_HTML)
cat(sprintf("\nWrote leaflet: %s\n", OUT_HTML))
cat(sprintf("File size: %.1f MB\n", file.info(OUT_HTML)$size / 1024^2))

# ── 6. Receipt ──────────────────────────────────────────────────────────────
receipt_path <- paste0(OUT_HTML, ".receipt.json")
tmp_receipt  <- paste0(receipt_path, ".tmp")
write_json(list(
  figure_path     = OUT_HTML,
  static_pair     = file.path(OUT_DIR, "rural_ent_physicians_map_v1.png"),
  script_path     = "scripts/build_rural_ent_primary_leaflet.R",
  basemap         = "CartoDB.Positron (project convention)",
  palette         = as.list(RUCA_PALETTE),
  rendered_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version       = paste0(R.version$major, ".", R.version$minor),
  live_counts     = list(
    n_primary_rural = nrow(prim),
    n_by_ruca       = as.list(table(prim$ruca_str))
  )
), tmp_receipt,
auto_unbox = TRUE, pretty = TRUE)
file.rename(tmp_receipt, receipt_path)
cat("Receipt:", receipt_path, "\n")
