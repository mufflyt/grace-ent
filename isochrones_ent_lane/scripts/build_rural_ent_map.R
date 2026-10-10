#!/usr/bin/env Rscript
# =============================================================================
# Rural ENT physician map (static, publication-quality)
# =============================================================================
# Builds two artifacts:
#   1. Corrected board_cert_ent_universe CSV (drops 12 non-CONUS NPPES rows,
#      sorts by ruca_primary descending so rural rows surface to the top)
#   2. Static PNG map of all rural board-certified active ENTs (RUCA 4-10),
#      plotted as points over CONUS state boundaries, colored by RUCA.
#
# Run:  Rscript scripts/build_rural_ent_map.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(here); library(ggplot2)
  library(sf); library(tigris); library(stringr); library(jsonlite)
})
options(tigris_use_cache = TRUE)

OUT_CSV <- here("publication_materials/figures/ent_data_integrity",
                "board_cert_ent_universe_n11338_zip_aboto.csv")
OUT_PNG <- here("publication_materials/figures/ent_data_integrity",
                "rural_ent_physicians_map_v1.png")
dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)

# ── 1. Rebuild the Board universe from source (idempotent) ─────────────────
source(here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()

tri <- ct$qa$subspecialty_triangulation

nppes_raw <- arrow::read_parquet(ct$input_paths$nppes) %>%
  mutate(
    npi_chr = as.character(npi),
    zip5    = stringr::str_pad(substr(as.character(zip), 1, 5), 5, "left", "0")
  ) %>%
  select(npi_chr, zip5_nppes = zip5, state_nppes = state,
         last_name_nppes = last_name, first_name_nppes = first_name)

# Bug Round LL161 (2026-05-22): read_csv unwrapped — corrupt or missing
# input produced a cryptic vroom error.  Wrap with path-named diagnostic.
abohns_raw <- tryCatch(
  read_csv(ct$input_paths$abohns_matched, show_col_types = FALSE),
  error = function(e) stop(sprintf(
    "[build_rural_ent_map] read_csv failed for abohns_matched '%s': %s",
    ct$input_paths$abohns_matched, conditionMessage(e)
  ), call. = FALSE)
) %>%
  mutate(npi_chr = as.character(npi)) %>%
  group_by(npi_chr) %>% slice(1) %>% ungroup() %>%
  select(npi_chr, last_name_aboto = last_name, first_name_aboto = first_name,
         state_aboto = state_full)

board_full <- tri %>%
  left_join(abohns_raw, by = "npi_chr") %>%
  left_join(nppes_raw,  by = "npi_chr") %>%
  transmute(
    npi              = npi_chr,
    last_name        = coalesce(last_name_aboto, last_name_nppes),
    first_name       = coalesce(first_name_aboto, first_name_nppes),
    state            = coalesce(state_nppes, state_aboto),
    zip5             = zip5_nppes,
    aboto_subspecialty,
    nppes_subspec_aboto,
    ent_subspec_aboto,
    subspecialty_agreement
  )

# RUCA join
# BUG FIX (round-2): as.integer() drops leading zeros AND emits NA for any
# ZIPCode that isn't a clean number (alphanumeric PR ZIPs, ZIP+4 hyphens).
# Use the str_pad / substr idiom consistent with the rest of the project.
# Bug Round LL162 (2026-05-22): read_csv unwrapped for ruca_2020_zip.csv.
ruca_x <- tryCatch(
  read_csv(here("data/external/ruca_2020_zip.csv"), show_col_types = FALSE),
  error = function(e) stop(sprintf(
    "[build_rural_ent_map] read_csv failed for RUCA crosswalk: %s\n  Expected at data/external/ruca_2020_zip.csv",
    conditionMessage(e)
  ), call. = FALSE)
) %>%
  mutate(zip5 = stringr::str_pad(substr(as.character(ZIPCode), 1, 5),
                                  5, "left", "0")) %>%
  select(zip5, ruca_primary = PrimaryRUCA, ruca_secondary = SecondaryRUCA) %>%
  distinct(zip5, .keep_all = TRUE)

board_full <- board_full %>%
  left_join(ruca_x, by = "zip5") %>%
  mutate(
    ruca_classification = case_when(
      is.na(ruca_primary)       ~ "ruca_missing",
      as.integer(ruca_primary) %in% 1:3 ~ "urban",
      TRUE                              ~ "rural"
    )
  )

before <- nrow(board_full)

# CONUS-only filter: ABOto CONUS state required; drop NPPES non-CONUS addresses
NON_CONUS <- c("AK","HI","PR","VI","GU","MP","AS","AA","AE","AP")
board_conus <- board_full %>%
  filter(!state %in% NON_CONUS,
         state %in% c(setdiff(state.abb, c("AK","HI")), "DC"),
         !is.na(state))
dropped <- before - nrow(board_conus)
cat(sprintf("Started with %d (Board cohort with NPPES address)\n", before))
cat(sprintf("Dropped %d non-CONUS rows (military / territories / foreign)\n", dropped))
cat(sprintf("Remaining: %d Board+NPPES CONUS-only ENT cohort\n",
            nrow(board_conus)))

# Sort: rural first (descending ruca_primary), then state, then NPI
board_sorted <- board_conus %>%
  mutate(ruca_primary_num = suppressWarnings(as.integer(ruca_primary))) %>%
  arrange(desc(ruca_primary_num), state, npi) %>%
  select(npi, last_name, first_name, state, zip5,
         ruca_primary, ruca_secondary, ruca_classification,
         aboto_subspecialty, nppes_subspec_aboto, ent_subspec_aboto,
         subspecialty_agreement)

# Bug Round LL164 (2026-05-22): tmp_out_csv had no host+PID+nanosecond
# entropy — two concurrent runs on the same OUT_CSV path could race.
tmp_out_csv <- paste0(
  OUT_CSV, ".tmp.",
  Sys.info()[["nodename"]], ".", Sys.getpid(), ".",
  sub("\\.", "", sprintf("%.6f", as.numeric(Sys.time())))
)
write_csv(board_sorted, tmp_out_csv)
# Bug Round LL163 (2026-05-22): file.rename return value dropped —
# cross-device rename silently failed leaving tmp_out_csv behind.
.rn_ok_csv <- isTRUE(file.rename(tmp_out_csv, OUT_CSV))
if (!.rn_ok_csv) {
  .cp_ok_csv <- isTRUE(file.copy(tmp_out_csv, OUT_CSV, overwrite = TRUE))
  if (file.exists(tmp_out_csv)) try(file.remove(tmp_out_csv), silent = TRUE)
  if (!.cp_ok_csv) {
    stop(sprintf("[build_rural_ent_map] file.rename + file.copy both failed for %s -> %s",
                 tmp_out_csv, OUT_CSV), call. = FALSE)
  }
}
cat(sprintf("Wrote: %s (%d rows, %d cols)\n", OUT_CSV,
            nrow(board_sorted), ncol(board_sorted)))

# ── 2. RUCA breakdown ───────────────────────────────────────────────────────
ruca_counts <- board_sorted %>% count(ruca_classification)
cat("\n=== Final RUCA breakdown ===\n")
print(ruca_counts)

rural <- board_sorted %>% filter(ruca_classification == "rural")
cat(sprintf("\nRural physicians for map: %d\n", nrow(rural)))

# ── 3. Pull ZCTA centroids for the rural ZIPs ───────────────────────────────
# BUG FIX 2026-05-18 (round-4 #1): prefer the cached
# data/reference/zcta_centroids_2020.csv (~1.5 MB) over the
# ~80 MB tigris TIGER/Line download. Fall back to tigris with cb=TRUE
# (cartographic boundary file, ~5x smaller than the default) when the
# cache is missing, and write the cache for next time.
cached_centroids <- here("data", "reference", "zcta_centroids_2020.csv")
if (file.exists(cached_centroids)) {
  cat("\nLoading cached ZCTA centroids: ", cached_centroids, "\n", sep = "")
  # Bug Round LL165 (2026-05-22): read_csv unwrapped for cached_centroids.
  zcta_centroids <- tryCatch(
    readr::read_csv(cached_centroids, show_col_types = FALSE),
    error = function(e) stop(sprintf(
      "[build_rural_ent_map] read_csv failed for cached centroids '%s': %s",
      cached_centroids, conditionMessage(e)
    ), call. = FALSE)
  ) %>%
    transmute(zip5 = stringr::str_pad(as.character(.data$zcta5),
                                       5, "left", "0"),
              lon  = .data$centroid_lon,
              lat  = .data$centroid_lat)
} else {
  cat("\nBuilding ZCTA centroids via tigris (cb=TRUE, smaller file)...\n")
  # Bug Round LL166 (2026-05-22): tigris::zctas unwrapped — network
  # outages produced cryptic errors with no remediation hint.
  zctas <- tryCatch(
    tigris::zctas(year = 2020, cb = TRUE, progress_bar = FALSE),
    error = function(e) stop(sprintf(
      "[build_rural_ent_map] tigris::zctas(2020) failed: %s\n  Check network or set options(tigris_use_cache = TRUE) with a pre-cached dataset.",
      conditionMessage(e)
    ), call. = FALSE)
  )
  zcta_col <- intersect(c("ZCTA5CE20","ZCTA5CE10","GEOID20","GEOID10","ZCTA5CE"),
                         names(zctas))[1]
  cat(sprintf("ZCTA column: %s\n", zcta_col))
  zctas_geom   <- sf::st_make_valid(zctas)
  centroid_pts <- sf::st_point_on_surface(zctas_geom)
  coords       <- sf::st_coordinates(centroid_pts)
  zcta_centroids <- tibble::tibble(
    zip5 = stringr::str_pad(as.character(zctas[[zcta_col]]),
                             5, "left", "0"),
    lon  = coords[, 1],
    lat  = coords[, 2]
  )
  dir.create(dirname(cached_centroids), recursive = TRUE, showWarnings = FALSE)
  # Bug Round LL167 (2026-05-22): tmp_centroids had no host+PID entropy
  # — concurrent runs racing the same cache path collided.
  tmp_centroids <- paste0(
    cached_centroids, ".tmp.",
    Sys.info()[["nodename"]], ".", Sys.getpid(), ".",
    sub("\\.", "", sprintf("%.6f", as.numeric(Sys.time())))
  )
  readr::write_csv(zcta_centroids %>%
                     transmute(zcta5 = .data$zip5,
                               centroid_lat = .data$lat,
                               centroid_lon = .data$lon),
                   tmp_centroids)
  # Bug Round LL168 (2026-05-22): file.rename return value not checked
  # — cross-device rename silently left the tmp behind.
  .rn_ok_cen <- isTRUE(file.rename(tmp_centroids, cached_centroids))
  if (!.rn_ok_cen) {
    .cp_ok_cen <- isTRUE(file.copy(tmp_centroids, cached_centroids, overwrite = TRUE))
    if (file.exists(tmp_centroids)) try(file.remove(tmp_centroids), silent = TRUE)
    if (!.cp_ok_cen) {
      warning(sprintf(
        "[build_rural_ent_map] file.rename + file.copy both failed for centroids cache (%s)",
        cached_centroids
      ), call. = FALSE)
    }
  }
}

rural_geo <- rural %>%
  left_join(zcta_centroids, by = "zip5") %>%
  filter(!is.na(lat), !is.na(lon))

# BUG FIX 2026-05-18 (round-4 #2): avoid 0/0 = NaN when rural cohort empty.
cat(sprintf("Rural with ZIP centroid: %d / %d (%.1f%%)\n",
            nrow(rural_geo), nrow(rural),
            100 * nrow(rural_geo) / max(nrow(rural), 1L)))

# ── 4. Pull CONUS state outlines ────────────────────────────────────────────
cat("\nLoading CONUS state boundaries via tigris...\n")
# Bug Round LL169 (2026-05-22): tigris::states unwrapped — network
# outage crashes the whole script with cryptic error.
states <- tryCatch(
  tigris::states(cb = TRUE, year = 2020, progress_bar = FALSE),
  error = function(e) stop(sprintf(
    "[build_rural_ent_map] tigris::states() failed: %s\n  Check network or restore tigris cache.",
    conditionMessage(e)
  ), call. = FALSE)
) %>%
  filter(!STUSPS %in% c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP"))

# ── 5. Build the map ────────────────────────────────────────────────────────
ruca_palette <- c(
  "4"  = "#FFC400",  # warm yellow — micropolitan, urban core
  "5"  = "#FF9100",
  "6"  = "#FF6F00",
  "7"  = "#E64A19",  # orange-red — small town, urban core
  "8"  = "#D32F2F",
  "9"  = "#B71C1C",
  "10" = "#7F1D1D"   # deep red — rural, completely isolated
)

rural_geo$ruca_factor <- factor(rural_geo$ruca_primary,
                                 levels = as.character(4:10))

# Convert points to sf so coord_sf can project them into Albers
rural_sf <- sf::st_as_sf(rural_geo,
                         coords = c("lon", "lat"),
                         crs = 4326,
                         remove = FALSE)

p <- ggplot() +
  geom_sf(data = states, fill = "#f5f5f0", color = "#888888",
          linewidth = 0.25) +
  geom_sf(data = rural_sf,
          aes(color = ruca_factor),
          size = 1.6, alpha = 0.78,
          stroke = 0) +
  scale_color_manual(values = ruca_palette,
                     name = "RUCA primary code",
                     drop = FALSE,
                     labels = c(
                       "4"  = "4  Micropolitan (urban core)",
                       "5"  = "5  Micropolitan (high commute)",
                       "6"  = "6  Micropolitan (low commute)",
                       "7"  = "7  Small town (urban core)",
                       "8"  = "8  Small town (high commute)",
                       "9"  = "9  Small town (low commute)",
                       "10" = "10 Rural / isolated"
                     )) +
  coord_sf(crs = 5070, datum = NA) +  # Albers Equal Area for CONUS
  labs(
    title = sprintf(
      "Rural board-certified ENT physicians \u2014 PRIMARY practice address  (n = %s)",
      format(nrow(rural_geo), big.mark = ",")
    ),
    subtitle = paste0(
      "Each point = the ZIP centroid of one physician's PRIMARY NPPES practice ",
      "address (RUCA 4-10).\n",
      "Board + NPPES intersection, n = ",
      format(nrow(board_sorted), big.mark = ","),
      ", CONUS only.  Secondary practice locations are NOT shown here\n",
      "(see rural_ent_physicians_map_v2.png for primary + secondary)."
    ),
    caption = paste0(
      "Sources: ABOto board certifications, NPPES primary practice address, ",
      "USDA 2020 RUCA crosswalk.  Color = RUCA primary (4 = least rural, ",
      "10 = most rural)."
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16,
                                 color = "#1a2b3c", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "#4a6880",
                                 margin = margin(b = 14), lineheight = 1.2),
    plot.caption  = element_text(size = 9, color = "#4a6880",
                                 hjust = 0, margin = margin(t = 12),
                                 lineheight = 1.3),
    legend.position    = "right",
    legend.title       = element_text(face = "bold", size = 10),
    legend.text        = element_text(size = 9),
    plot.margin        = margin(12, 12, 12, 12),
    plot.background    = element_rect(fill = "white", color = NA)
  ) +
  guides(color = guide_legend(override.aes = list(size = 3.5, alpha = 1)))

# Bug Round LL170 (2026-05-22): tmp_out_png had no host+PID entropy
# and ggsave was unwrapped; file.rename return value was dropped.
tmp_out_png <- paste0(
  OUT_PNG, ".tmp.",
  Sys.info()[["nodename"]], ".", Sys.getpid(), ".",
  sub("\\.", "", sprintf("%.6f", as.numeric(Sys.time()))),
  ".png"
)
.ggs_ok <- tryCatch({
  ggsave(tmp_out_png, p, width = 14, height = 8.5, dpi = 300, bg = "white")
  TRUE
}, error = function(e) {
  stop(sprintf("[build_rural_ent_map] ggsave failed: %s", conditionMessage(e)),
       call. = FALSE)
})
.rn_ok_png <- isTRUE(file.rename(tmp_out_png, OUT_PNG))
if (!.rn_ok_png) {
  .cp_ok_png <- isTRUE(file.copy(tmp_out_png, OUT_PNG, overwrite = TRUE))
  if (file.exists(tmp_out_png)) try(file.remove(tmp_out_png), silent = TRUE)
  if (!.cp_ok_png) {
    stop(sprintf("[build_rural_ent_map] PNG rename+copy both failed for %s -> %s",
                 tmp_out_png, OUT_PNG), call. = FALSE)
  }
}
cat(sprintf("\nWrote map: %s\n", OUT_PNG))

# ── 6. Receipt ──────────────────────────────────────────────────────────────
receipt <- list(
  figure_path = OUT_PNG,
  script_path = "scripts/build_rural_ent_map.R",
  rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version   = paste0(R.version$major, ".", R.version$minor),
  live_counts = list(
    n_board_universe_before_conus_fix = before,
    n_dropped_non_conus               = dropped,
    n_board_universe_corrected        = nrow(board_sorted),
    n_rural                           = nrow(rural),
    n_rural_with_zip_centroid         = nrow(rural_geo),
    n_urban                           = sum(board_sorted$ruca_classification == "urban"),
    n_ruca_missing                    = sum(board_sorted$ruca_classification == "ruca_missing"),
    rural_by_ruca = as.list(table(rural$ruca_primary))
  )
)
receipt_path <- paste0(OUT_PNG, ".receipt.json")
tmp_receipt  <- paste0(receipt_path, ".tmp")
write_json(receipt, tmp_receipt, auto_unbox = TRUE, pretty = TRUE)
file.rename(tmp_receipt, receipt_path)
cat(sprintf("Wrote receipt: %s\n", receipt_path))
