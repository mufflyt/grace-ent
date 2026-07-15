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
MODE   <- if (length(args) >= 1) args[1] else "test"   # test | full | resume
FULL   <- MODE %in% c("full", "resume")
NCAP   <- if (MODE == "full" && length(args) >= 2) as.integer(args[2]) else NA_integer_

outdir <- file.path(GRACE, "model_output", "isochrones")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
final_rds  <- file.path(outdir, "ent_isochrones.rds")
final_gpkg <- file.path(outdir, "ent_isochrones.gpkg")

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

# RESUME: keep already-completed isochrones, run only the locations still missing
# (a location is "done" only if all BANDS are present, so partial locations are
# retried). Existing results are merged back in at the end and at each checkpoint.
existing <- NULL
if (MODE == "resume" && file.exists(final_rds)) {
  existing <- readRDS(final_rds)
  done_by_loc <- tapply(existing$band_min, existing$loc_id,
                        function(b) all(BANDS %in% b))
  done_ids <- as.integer(names(done_by_loc)[which(done_by_loc)])
  loc <- loc[!(loc$id %in% done_ids), , drop = FALSE]
  cat(sprintf("RESUME: %d locations complete; %d remaining to generate.\n",
              length(done_ids), nrow(loc)))
}
if (MODE == "test") loc <- loc[1:1, , drop = FALSE]     # test: single point
if (MODE == "full" && !is.na(NCAP)) loc <- loc[seq_len(min(NCAP, nrow(loc))), , drop = FALSE]
cat(sprintf("Generating isochrones for %d location(s) x %d bands = %d calls\n",
            nrow(loc), length(BANDS), nrow(loc) * length(BANDS)))

# merge helper: existing (resume) + reused-within-5km + newly built, de-duped by
# loc+band. `reused` is a global set later by the REUSE block (NULL if disabled).
merge_all <- function(new_list) {
  new_iso <- if (length(new_list)) dplyr::bind_rows(new_list) else NULL
  m <- dplyr::bind_rows(existing, if (exists("reused")) reused else NULL, new_iso)
  if (!is.null(m) && nrow(m))
    m <- m[!duplicated(paste(m$loc_id, m$band_min)), , drop = FALSE]
  m
}

# ---- REUSE: pull existing isochrones within 5 km before building ------------
# Never rebuild an isochrone that already exists within 5 km (great-circle) in
# the project store or our own cache. Disable with ISO_REUSE=FALSE.
reused <- NULL; reused_keys <- character(0)
if (Sys.getenv("ISO_REUSE", "TRUE") != "FALSE" && MODE != "test" && nrow(loc) > 0) {
  suppressMessages(source(file.path(GRACE, "scripts", "isochrone_reuse.R")))
  reused <- tryCatch(iso_materialize_reused(loc, BANDS), error = function(e) NULL)
  if (!is.null(reused) && nrow(reused)) {
    reused_keys <- paste(reused$loc_id, reused$band_min)
    cat(sprintf("REUSE: %d isochrones reused within 5 km (median %.0f m); building the rest.\n",
                nrow(reused), stats::median(reused$dist_m)))
  }
}

# ---- generate ---------------------------------------------------------------
results <- list(); k <- 0L; fail <- 0L    # results holds newly BUILT isochrones
ckpt <- file.path(outdir, "ent_isochrones_checkpoint.rds")
for (i in seq_len(nrow(loc))) {
  for (tb in BANDS) {
    if (paste(loc$id[i], tb) %in% reused_keys) next   # already have it within 5 km
    iso <- tryCatch(generate_valhalla_isochrone(
      lat = loc$lat[i], lon = loc$long[i], time_minutes = tb,
      costing = "auto", valhalla_url = VALHALLA_URL),
      error = function(e) NULL)
    if (!is.null(iso) && inherits(iso, "sf") && nrow(iso) > 0) {
      iso$loc_id <- loc$id[i]; iso$zip5 <- loc$zip5[i]; iso$band_min <- tb
      k <- k + 1L; results[[k]] <- iso
    } else fail <- fail + 1L
  }
  # checkpoint: write the MERGED (existing + new-so-far) final rds so the run is
  # re-resumable if the tunnel/instance drops again mid-batch.
  if (i %% 25 == 0) {
    saveRDS(results, ckpt)
    saveRDS(merge_all(results), final_rds)
    cat("  ", i, "/", nrow(loc), " locations (checkpointed)\n")
  }
}
cat(sprintf("Done: %d new isochrones (%d failures).\n", k, fail))

all_iso <- merge_all(results)
if (!is.null(all_iso) && nrow(all_iso)) {
  saveRDS(all_iso, final_rds)
  sf::st_write(all_iso, final_gpkg, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("Wrote ent_isochrones.{gpkg,rds}: %d polygons, %d/%d locations complete\n",
              nrow(all_iso), length(unique(all_iso$loc_id)),
              nrow(read.csv(file.path(GRACE, "data/processed/ent_unique_locations.csv")))))
}
