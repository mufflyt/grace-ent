#!/usr/bin/env Rscript
# =============================================================================
# Drive-time isochrones for the grace-ent ENT practices, using the Valhalla-on-
# EC2 routing engine (NOT the HERE API). Calls the project's own generator:
#   /Users/tylermuffly/isochrones/R/valhalla_isochrone_generator.R
#
# Reaches Valhalla at VALHALLA_URL (default http://localhost:8002). The EC2
# instance must be running AND an SSH tunnel forwarding local 8002 -> EC2 8002
# must be active, e.g.:
#   ssh -N -L 8002:localhost:8002 -i ~/.ssh/<key>.pem ec2-user@<public_ip>
#
# We call generate_valhalla_isochrone() per band directly (it honors valhalla_url
# unconditionally), avoiding generate_valhalla_isochrones_all_bands(), which
# reroutes bands <=100 min to the public OSM.de server.
#
# Usage:
#   Rscript scripts/valhalla_ent_isochrones.R            # test: connection + 1 point
#   Rscript scripts/valhalla_ent_isochrones.R full [N]   # full batch (optional N cap)
#
# Output: model_output/isochrones/ent_isochrones.gpkg + .rds
# =============================================================================

suppressPackageStartupMessages({library(sf); library(dplyr)})
ISO_PROJ <- "/Users/tylermuffly/isochrones"
GRACE    <- "/Users/tylermuffly/grace-ent"
VALHALLA_URL <- Sys.getenv("VALHALLA_URL", "http://localhost:8002")
BANDS  <- c(30, 60, 120, 180)          # minutes (project canonical bands)
args   <- commandArgs(trailingOnly = TRUE)
FULL   <- length(args) >= 1 && args[1] == "full"
NCAP   <- if (FULL && length(args) >= 2) as.integer(args[2]) else NA_integer_

outdir <- file.path(GRACE, "model_output", "isochrones")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# source the generator from the isochrones project (its here::here sourcing needs
# the project as working directory)
owd <- setwd(ISO_PROJ); on.exit(setwd(owd), add = TRUE)
suppressMessages(source(file.path(ISO_PROJ, "R", "valhalla_isochrone_generator.R")))

# ---- connectivity gate ------------------------------------------------------
cat("Valhalla URL:", VALHALLA_URL, "\n")
ok <- tryCatch(test_valhalla_connection(VALHALLA_URL), error = function(e) FALSE)
if (!isTRUE(ok)) {
  stop("Cannot reach Valhalla at ", VALHALLA_URL,
       ".\n  -> Confirm the EC2 instance is up AND the SSH tunnel is active:\n",
       "     ssh -N -L 8002:localhost:8002 -i ~/.ssh/<key>.pem ec2-user@<public_ip>\n",
       "  or set VALHALLA_URL to a reachable endpoint.", call. = FALSE)
}
cat("Connection OK.\n")

# ---- coordinates ------------------------------------------------------------
loc <- read.csv(file.path(GRACE, "data/processed/ent_unique_locations.csv"))
if (!FULL) loc <- loc[1:1, , drop = FALSE]              # test: single point
if (FULL && !is.na(NCAP)) loc <- loc[seq_len(min(NCAP, nrow(loc))), , drop = FALSE]
cat(sprintf("Generating isochrones for %d location(s) x %d bands = %d calls\n",
            nrow(loc), length(BANDS), nrow(loc) * length(BANDS)))

# ---- generate ---------------------------------------------------------------
results <- list(); k <- 0L; fail <- 0L
ckpt <- file.path(outdir, "ent_isochrones_checkpoint.rds")
for (i in seq_len(nrow(loc))) {
  for (tb in BANDS) {
    iso <- tryCatch(generate_valhalla_isochrone(
      lat = loc$lat[i], lon = loc$long[i], time_minutes = tb,
      costing = "auto", valhalla_url = VALHALLA_URL),
      error = function(e) NULL)
    if (!is.null(iso) && inherits(iso, "sf") && nrow(iso) > 0) {
      iso$loc_id <- loc$id[i]; iso$zip5 <- loc$zip5[i]; iso$band_min <- tb
      k <- k + 1L; results[[k]] <- iso
    } else fail <- fail + 1L
  }
  if (i %% 25 == 0) { saveRDS(results, ckpt); cat("  ", i, "/", nrow(loc), " locations\n") }
}
cat(sprintf("Done: %d isochrones (%d failures).\n", k, fail))

if (k > 0) {
  all_iso <- dplyr::bind_rows(results)
  saveRDS(all_iso, file.path(outdir, "ent_isochrones.rds"))
  sf::st_write(all_iso, file.path(outdir, "ent_isochrones.gpkg"),
               delete_dsn = TRUE, quiet = TRUE)
  cat("Wrote ent_isochrones.gpkg + .rds (", nrow(all_iso), "polygons)\n")
}
