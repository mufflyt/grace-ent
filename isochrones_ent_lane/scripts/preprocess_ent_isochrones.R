#!/usr/bin/env Rscript
# ============================================================================
# preprocess_ent_isochrones.R
# Preprocessing + Isochrone Generation for ENT Physicians
# ============================================================================
#
# WHAT THIS DOES:
#   1. Loads 14K+ geocoded ENT physicians
#   2. Deduplicates by exact (lat, lon)
#   3. Runs 5km Haversine DBSCAN clustering (state-blocked)
#   4. Saves clustered representatives as RDS
#   5. Invokes generate_isochrones_standalone.R for specified bands
#
# USAGE:
#   Rscript scripts/preprocess_ent_isochrones.R                  # 30,60 min (osm.de)
#   Rscript scripts/preprocess_ent_isochrones.R --bands 120      # 120 min (localhost)
#   Rscript scripts/preprocess_ent_isochrones.R --bands 30,60,120
#   Rscript scripts/preprocess_ent_isochrones.R --cluster-only   # stop after clustering
#   Rscript scripts/preprocess_ent_isochrones.R --skip-cluster   # reuse existing reps
#   Rscript scripts/preprocess_ent_isochrones.R --threshold 10   # 10km clustering
#   Rscript scripts/preprocess_ent_isochrones.R --dry-run        # preprocess only, no generation
#
# OUTPUTS:
#   artifacts/ent/ent_clustered_representatives.rds
#   artifacts/isochrones/isochrones_{30,60}min_consolidated.rds (via standalone generator)
#
# ============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
})

cat("============================================================\n")
cat("  ENT Isochrone Preprocessing Pipeline\n")
cat("============================================================\n\n")

# ---------------------------------------------------------------------------
# Parse arguments
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

bands_arg      <- "30,60"
dry_run        <- FALSE
skip_cluster   <- FALSE
cluster_only   <- FALSE
upload_dropbox <- TRUE
threshold_km   <- 5

i <- 1L
while (i <= length(args)) {
  if (args[i] == "--bands" && i < length(args)) {
    bands_arg <- args[i + 1]
    i <- i + 2L
  } else if (args[i] == "--threshold" && i < length(args)) {
    threshold_km <- as.numeric(args[i + 1])
    if (is.na(threshold_km) || threshold_km <= 0) stop("Invalid --threshold: ", args[i + 1])
    i <- i + 2L
  } else if (args[i] == "--dry-run") {
    dry_run <- TRUE
    i <- i + 1L
  } else if (args[i] == "--skip-cluster") {
    skip_cluster <- TRUE
    i <- i + 1L
  } else if (args[i] == "--cluster-only") {
    cluster_only <- TRUE
    i <- i + 1L
  } else if (args[i] == "--no-upload") {
    upload_dropbox <- FALSE
    i <- i + 1L
  } else if (args[i] %in% c("--help", "-h")) {
    cat("
ENT Isochrone Preprocessing Pipeline
=====================================
Preprocesses ENT physician locations (dedup, cluster) then generates isochrones.

USAGE:
  Rscript scripts/preprocess_ent_isochrones.R [OPTIONS]

OPTIONS:
  --bands 30,60        Bands to generate (default: 30,60)
  --threshold 5        Clustering threshold in km (default: 5)
  --cluster-only       Stop after clustering (no isochrone generation)
  --skip-cluster       Reuse existing ent_clustered_representatives.rds
  --no-upload          Skip Dropbox uploads
  --dry-run            Show cohort stats and exit
  --help, -h           Show this help
")
    quit(status = 0)
  } else {
    cat("Unknown argument:", args[i], "\n")
    i <- i + 1L
  }
}

bands <- as.integer(strsplit(bands_arg, ",")[[1]])
cat(sprintf("Bands: %s min\n", paste(bands, collapse = ", ")))
cat(sprintf("Clustering threshold: %s km\n", threshold_km))
cat(sprintf("Dry run: %s\n", dry_run))
cat(sprintf("Cluster only: %s\n", cluster_only))
cat(sprintf("Skip clustering: %s\n\n", skip_cluster))

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
input_csv    <- here("artifacts", "ent", "ent_physicians_geocoded.csv")
cluster_rds  <- here("artifacts", "ent", "ent_clustered_representatives.rds")
iso_dir      <- here("artifacts", "isochrones")

stopifnot(file.exists(input_csv))

# ---------------------------------------------------------------------------
# Step 1: Load & Deduplicate
# ---------------------------------------------------------------------------
cat("== Step 1: Load & Deduplicate ==\n")
physicians <- read.csv(input_csv, stringsAsFactors = FALSE)
cat(sprintf("  Loaded: %s rows\n", format(nrow(physicians), big.mark = ",")))

# Drop rows with missing/invalid coordinates
physicians <- physicians %>%
  filter(
    !is.na(latitude), !is.na(longitude),
    is.finite(latitude), is.finite(longitude),
    abs(latitude) > 0.1 | abs(longitude) > 0.1  # Null Island guard
  )
cat(sprintf("  After coordinate filter: %s rows\n", format(nrow(physicians), big.mark = ",")))

# Deduplicate by exact (lat, lon) — keep first occurrence
n_before <- nrow(physicians)
physicians_dedup <- physicians %>%
  distinct(latitude, longitude, .keep_all = TRUE)
cat(sprintf("  After lat/lon dedup: %s unique locations (from %s)\n",
            format(nrow(physicians_dedup), big.mark = ","),
            format(n_before, big.mark = ",")))

# ---------------------------------------------------------------------------
# Step 2: 5km Haversine DBSCAN Clustering
# ---------------------------------------------------------------------------
if (skip_cluster && file.exists(cluster_rds)) {
  cat("\n== Step 2: Loading existing clustered representatives ==\n")
  reps <- readRDS(cluster_rds)
  cat(sprintf("  Loaded: %s representatives\n", format(nrow(reps), big.mark = ",")))
} else {
  cat("\n== Step 2: 5km Haversine DBSCAN Clustering ==\n")

  source(here("R", "haversine_clustering_implementation.R"))

  clustered <- haversine_clustering(
    physician_data       = physicians_dedup,
    lat_col              = "latitude",
    lon_col              = "longitude",
    threshold_km         = threshold_km,
    year_col             = NULL,
    enable_state_blocking = TRUE,
    verbose              = TRUE
  )

  reps <- extract_cluster_representatives(clustered, include_metadata = TRUE)

  cat(sprintf("\n  Input locations: %s\n", format(nrow(physicians_dedup), big.mark = ",")))
  cat(sprintf("  Cluster representatives: %s\n", format(nrow(reps), big.mark = ",")))
  cat(sprintf("  Reduction: %.1f%%\n",
              100 * (1 - nrow(reps) / nrow(physicians_dedup))))

  # Ensure required columns for standalone generator
  if (!"npi" %in% names(reps) && "npi" %in% names(physicians_dedup)) {
    # npi should already be carried through from input
    cat("  [NOTE] npi column present\n")
  }

  # Save
  cluster_rds_tmp <- paste0(cluster_rds, ".tmp")
  saveRDS(reps, cluster_rds_tmp)
  file.rename(cluster_rds_tmp, cluster_rds)
  cat(sprintf("  Saved: %s (%s)\n", cluster_rds,
              format(file.size(cluster_rds), big.mark = ",")))
}

# ---------------------------------------------------------------------------
# Step 3: Upload clustered representatives to Dropbox
# ---------------------------------------------------------------------------
if (upload_dropbox) {
  cat("\n== Step 3: Dropbox Upload (clustered representatives) ==\n")
  tryCatch({
    source(here("R", "utils", "dropbox_utils.R"))
    dbxcli_put(
      local_path  = cluster_rds,
      remote_path = "/isochrones/ent/ent_clustered_representatives.rds",
      verbose     = TRUE
    )
    cat("  Uploaded to Dropbox\n")
  }, error = function(e) {
    cat(sprintf("  [WARN] Dropbox upload failed: %s\n", conditionMessage(e)))
  })
}

# ---------------------------------------------------------------------------
# Step 3.5: Coverage Report
# ---------------------------------------------------------------------------
cat("\n== Coverage Report ==\n")
rep_keys <- sprintf("%.5f_%.5f", reps$latitude, reps$longitude)
ent_iso_dir <- here("artifacts", "ent", "isochrones")
if (!dir.exists(ent_iso_dir)) dir.create(ent_iso_dir, recursive = TRUE)

for (b in bands) {
  iso_path <- file.path(ent_iso_dir, sprintf("isochrones_%dmin_consolidated.rds", b))
  if (file.exists(iso_path)) {
    iso <- readRDS(iso_path)
    matched <- sum(rep_keys %in% iso$location_key)
    pct <- round(matched / length(rep_keys) * 100, 1)
    status <- if (matched == length(rep_keys)) "COMPLETE" else paste(length(rep_keys) - matched, "missing")
    cat(sprintf("  %3d-min: %d/%d (%.1f%%) -- %s\n", b, matched, length(rep_keys), pct, status))
  } else {
    cat(sprintf("  %3d-min: NOT FOUND -- needs full generation\n", b))
  }
}

if (cluster_only) {
  cat("\n== --cluster-only: stopping after clustering ==\n")
  cat(sprintf("To generate isochrones, run:\n"))
  cat(sprintf("  Rscript scripts/generate_isochrones_standalone.R --cohort_path %s --bands %s\n",
              cluster_rds, bands_arg))
  quit(status = 0)
}

# ---------------------------------------------------------------------------
# Step 4: Generate Isochrones (invoke standalone generator)
# ---------------------------------------------------------------------------
if (dry_run) {
  cat("\n== DRY RUN: Would generate isochrones ==\n")
  cat(sprintf("  Cohort: %s (%s representatives)\n",
              basename(cluster_rds), format(nrow(reps), big.mark = ",")))
  cat(sprintf("  Bands: %s min\n", paste(bands, collapse = ", ")))

  # Estimate time
  for (b in bands) {
    is_local <- b > 60
    rate_s <- if (is_local) 0.3 else 1.5
    est_hours <- (nrow(reps) * rate_s) / 3600
    server <- if (is_local) "localhost:8002" else "osm.de (public)"
    cat(sprintf("  %3d min -> %s (~%.1f hours)\n", b, server, est_hours))
  }
  cat("\nRemove --dry-run to start generation.\n")
} else {
  cat("\n== Step 4: Generate Isochrones ==\n")

  # Smart skip: only generate bands that are incomplete
  bands_needed <- c()
  for (b in bands) {
    iso_path <- file.path(ent_iso_dir, sprintf("isochrones_%dmin_consolidated.rds", b))
    if (file.exists(iso_path)) {
      iso <- readRDS(iso_path)
      matched <- sum(rep_keys %in% iso$location_key)
      if (matched == length(rep_keys)) {
        cat(sprintf("  %d-min: already complete (%d/%d), skipping\n", b, matched, length(rep_keys)))
        next
      } else {
        cat(sprintf("  %d-min: %d missing, will regenerate\n", b, length(rep_keys) - matched))
        bands_needed <- c(bands_needed, b)
      }
    } else {
      cat(sprintf("  %d-min: not found, will generate\n", b))
      bands_needed <- c(bands_needed, b)
    }
  }

  if (length(bands_needed) == 0) {
    cat("\n  All requested bands are already complete!\n")
    cat("  Pass --force to generate_isochrones_standalone.R to regenerate.\n")
    exit_code <- 0
  } else {
    bands_needed_str <- paste(bands_needed, collapse = ",")
    cat(sprintf("\n  Generating bands: %s min\n", bands_needed_str))

    # Notify start
    tryCatch({
      source(here("R", "utils", "notify.R"))
      pipeline_notify(
        "ENT Isochrones Starting",
        sprintf("%s reps, bands: %s min", format(nrow(reps), big.mark = ","), bands_needed_str)
      )
    }, error = function(e) {
      cat(sprintf("  [WARN] Notification failed: %s\n", conditionMessage(e)))
    })

    standalone_script <- here("scripts", "generate_isochrones_standalone.R")
    cmd <- sprintf(
      "Rscript %s --cohort_path %s --bands %s --force",
      shQuote(standalone_script),
      shQuote(cluster_rds),
      bands_needed_str
    )

    cat(sprintf("  Command: %s\n\n", cmd))
    exit_code <- system(cmd)
  }

  if (exit_code == 0) {
    cat("\n== Isochrone generation completed successfully ==\n")

    # Copy outputs to ENT-specific directory
    ent_iso_dir <- here("artifacts", "ent", "isochrones")
    if (!dir.exists(ent_iso_dir)) dir.create(ent_iso_dir, recursive = TRUE)

    for (b in bands) {
      src <- file.path(iso_dir, sprintf("isochrones_%dmin_consolidated.rds", b))
      if (file.exists(src)) {
        dst <- file.path(ent_iso_dir, basename(src))
        file.copy(src, dst, overwrite = TRUE)
        cat(sprintf("  Copied: %s (%s MB)\n", basename(dst),
                    round(file.size(dst) / 1e6, 1)))
      }
    }

    # Upload to Dropbox
    if (upload_dropbox) {
      cat("\n== Step 5: Dropbox Upload (isochrones) ==\n")
      tryCatch({
        if (!exists("dbxcli_put")) source(here("R", "utils", "dropbox_utils.R"))
        for (b in bands) {
          local_file <- file.path(ent_iso_dir,
                                  sprintf("isochrones_%dmin_consolidated.rds", b))
          if (file.exists(local_file)) {
            dbxcli_put(
              local_path  = local_file,
              remote_path = sprintf("/isochrones/ent/isochrones_%dmin_consolidated.rds", b),
              verbose     = TRUE
            )
          }
        }
        cat("  Uploaded to Dropbox\n")
      }, error = function(e) {
        cat(sprintf("  [WARN] Dropbox upload failed: %s\n", conditionMessage(e)))
      })
    }

    # Notify completion
    tryCatch({
      if (!exists("pipeline_notify")) source(here("R", "utils", "notify.R"))
      pipeline_notify(
        "ENT Isochrones Complete",
        sprintf("Bands: %s min. Check artifacts/ent/isochrones/", bands_arg)
      )
    }, error = function(e) NULL)

  } else {
    cat(sprintf("\n== Isochrone generation FAILED (exit code %d) ==\n", exit_code))
    tryCatch({
      if (!exists("pipeline_notify")) source(here("R", "utils", "notify.R"))
      pipeline_notify(
        "ENT Isochrones FAILED",
        sprintf("Exit code %d. Bands: %s min.", exit_code, bands_arg),
        priority = "critical"
      )
    }, error = function(e) NULL)
    quit(status = exit_code)
  }
}

cat("\nDone.\n")
