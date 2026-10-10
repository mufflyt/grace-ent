#!/usr/bin/env Rscript
# Generate 30-minute drive-time isochrones for every unique ENT physician
# location from enthealth.org.
#
# Output: data/external/ent_isochrones_30min.rds  (sf, EPSG:4326)
#
# Uses the EC2 Valhalla server at localhost:8002 (on the EC2 host). Valhalla is
# never run locally — this script refuses on a non-EC2 host (see guard below).

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(sf); library(httr)
  library(jsonlite)
})
.old_scipen <- getOption("scipen")
on.exit(options(scipen = .old_scipen), add = TRUE)
options(scipen = 999)
sf::sf_use_s2(FALSE)

VALHALLA <- "http://localhost:8002"
OUT_RDS  <- here("data", "external", "ent_isochrones_30min.rds")

# EC2-only policy (2026-06-17): Valhalla is never run locally — all routing
# bands run on EC2. Refuse on a non-EC2 host (override tests/manual only:
# VALHALLA_BIG_BAND_OVERRIDE=1).
if (!exists("require_ec2_for_bands", mode = "function")) {
  source(here::here("R", "utils", "host_environment.R"))
}
require_ec2_for_bands(30L)

# ── Check Valhalla ─────────────────────────────────────────────────────────────
status <- tryCatch(
  content(GET(paste0(VALHALLA, "/status")), "parsed"),
  error = function(e) NULL
)
if (is.null(status)) stop("Valhalla not responding at ", VALHALLA)
cat(sprintf("Valhalla %s ready\n", status$version))

# ── Load ENT physician locations ───────────────────────────────────────────────
DROPBOX <- file.path(path.expand("~"), "Dropbox (Personal)",
                     "mystery_shopper", "Corbi study", "ENT", "Data")
geo <- read_csv(file.path(DROPBOX, "results_geocoded.csv"),
                na = c("","NA"), show_col_types = FALSE) |>
  filter(!is.na(latitude), !is.na(longitude),
         latitude > 24, latitude < 50,
         longitude > -125, longitude < -66) |>
  distinct(latitude, longitude, .keep_all = TRUE) |>
  mutate(location_key = paste(round(longitude, 4), round(latitude, 4), sep = "_"))

cat(sprintf("Unique CONUS ENT locations: %d\n", nrow(geo)))

# ── Resume from existing output ────────────────────────────────────────────────
done_keys <- character(0)
iso_list  <- list()

if (file.exists(OUT_RDS)) {
  existing <- readRDS(OUT_RDS)
  done_keys <- existing$location_key
  iso_list  <- list(existing)
  cat(sprintf("Resuming: %d already done\n", length(done_keys)))
}

todo <- geo |> filter(!location_key %in% done_keys)
cat(sprintf("Remaining: %d locations\n", nrow(todo)))

# ── Isochrone request helper ───────────────────────────────────────────────────

#' Request a single drive-time isochrone from a local Valhalla server
#'
#' @description
#' Posts a single-location isochrone request to the Valhalla HTTP API and
#' returns the result as an \code{sf} polygon. Returns \code{NULL} silently on
#' any network error or non-200 response so the generation loop can skip failed
#' locations without crashing.
#'
#' @details
#' Uses Valhalla's \code{auto} costing profile (passenger car routing).
#' \code{denoise = 0.5} removes small disconnected polygon fragments.
#' \code{generalize = 150} simplifies geometry to ~150 m resolution, which
#' reduces file size without meaningful loss of geographic accuracy for
#' accessibility analysis.
#'
#' The request times out after 60 seconds — sufficient for local Valhalla on
#' OrbStack. For public servers (osm.de) increase the timeout or use the
#' standalone generator script which handles rate limiting automatically.
#'
#' @param lat Numeric scalar. Latitude in decimal degrees (WGS-84). Must be in
#'   the range \code{[-90, 90]}.
#' @param lng Numeric scalar. Longitude in decimal degrees (WGS-84). Must be in
#'   the range \code{[-180, 180]}.
#' @param minutes Integer or numeric. Drive-time contour in minutes. Default
#'   \code{30}. Public Valhalla servers cap at ~100 min; local servers are
#'   unrestricted.
#'
#' @return An \code{sf} data.frame with a single polygon row and CRS EPSG:4326,
#'   or \code{NULL} if the request failed, timed out, or returned a non-200
#'   status code.
#'
#' @seealso \code{\link[httr]{POST}}, \code{\link[sf]{st_read}}
get_iso <- function(lat, lng, minutes = 30) {
  body <- list(
    locations   = list(list(lat = lat, lon = lng)),
    costing     = "auto",
    contours    = list(list(time = minutes)),
    polygons    = TRUE,
    denoise     = 0.5,
    generalize  = 150
  )
  resp <- tryCatch(
    POST(paste0(VALHALLA, "/isochrone"),
         body = toJSON(body, auto_unbox = TRUE),
         content_type_json(),
         timeout(60)),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  geojson <- content(resp, "text", encoding = "UTF-8")
  tryCatch(st_read(geojson, quiet = TRUE), error = function(e) NULL)
}

# ── Generate isochrones ────────────────────────────────────────────────────────
n_todo <- nrow(todo)
start  <- Sys.time()

for (i in seq_len(n_todo)) {
  row <- todo[i, ]
  iso <- get_iso(row$latitude, row$longitude, minutes = 30)

  if (!is.null(iso) && nrow(iso) > 0) {
    iso <- iso |>
      mutate(
        location_key     = row$location_key,
        center_lat       = row$latitude,
        center_lng       = row$longitude,
        drive_time_minutes = 30L,
        full_name        = coalesce(row$full_name, NA_character_),
        specialty        = coalesce(row$specialty_primary, NA_character_)
      ) |>
      select(location_key, center_lat, center_lng, drive_time_minutes,
             full_name, specialty, geometry)
    iso_list[[length(iso_list) + 1]] <- iso
  }

  if (i %% 100 == 0 || i == n_todo) {
    elapsed <- as.numeric(Sys.time() - start, units = "secs")
    rate    <- i / elapsed
    eta_min <- (n_todo - i) / rate / 60
    cat(sprintf("  [%d/%d] %.0f/min | ETA %.0f min\n",
                i, n_todo, rate * 60, eta_min))

    # Save checkpoint every 200 locations (atomic write)
    if (length(iso_list) > 0) {
      combined <- do.call(rbind, iso_list)
      tmp_ck <- paste0(OUT_RDS, ".tmp")
      saveRDS(combined, tmp_ck)
      file.rename(tmp_ck, OUT_RDS)
    }
  }
}

# ── Final save (atomic write) ──────────────────────────────────────────────────
if (length(iso_list) > 0) {
  final <- do.call(rbind, iso_list)
  tmp_final <- paste0(OUT_RDS, ".tmp")
  saveRDS(final, tmp_final)
  file.rename(tmp_final, OUT_RDS)
  cat(sprintf("\nDone: %d isochrones saved to %s (%.1f MB)\n",
              nrow(final), OUT_RDS, file.size(OUT_RDS) / 1e6))
} else {
  cat("No isochrones generated.\n")
}
