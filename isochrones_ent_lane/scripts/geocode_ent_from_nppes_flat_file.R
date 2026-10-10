#!/usr/bin/env Rscript
# =============================================================================
# GEOCODE ENT PHYSICIANS FROM NPPES FEBRUARY 2026 FLAT FILE
# =============================================================================
# Reads primary practice locations (npidata_pfile) and secondary practice
# locations (pl_pfile) from the NPPES February 2026 flat file.
# Filters to ENT taxonomy codes (207Y family).
# Geocodes both sets using the project's existing cascade.
#
# Usage:
#   caffeinate -idms Rscript scripts/geocode_ent_from_nppes_flat_file.R \
#     2>&1 | tee logs/geocode_ent_flat_$(date +%Y%m%d_%H%M%S).log
#
# Expected runtime: 45-90 minutes
# Expected output:
#   artifacts/ent/ent_primary_geocoded.parquet   (~15,000 rows)
#   artifacts/ent/ent_secondary_geocoded.parquet (~45,000-60,000 rows)
#
# Author: Tyler Muffly, MD + Claude Code
# Date:   2026-05-03
# =============================================================================

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(here)
})

source(here("R", "core_utils.R"))          # geocode_batch_first_with_fallbacks
source(here("R", "utils", "notify.R"))     # pipeline_notify
source(here("R", "utils", "io_helpers.R")) # write_parquet_atomic, write_csv_atomic
source(here("R", "utils", "dropbox_utils.R")) # dbxcli_put

# -----------------------------------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------------------------------
NPPES_DIR    <- "/Volumes/MufflySamsung 1/nppes_historical_downloads/NPPES_Data_Dissemination_February_2026"
NPIDATA_FILE <- file.path(NPPES_DIR, "npidata_pfile_20050523-20260208.csv")
PL_FILE      <- file.path(NPPES_DIR, "pl_pfile_20050523-20260208.csv")

ENT_PATTERN <- "207Y%"
DUCKDB_PATH  <- Sys.getenv(
  "NBER_DB_PATH",
  "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb"
)

OUTPUT_DIR     <- here("artifacts", "ent")
CHECKPOINT_DIR <- here("artifacts", "ent", "checkpoints")

# =============================================================================
# PHASE 1: EXTRACT ENT PRIMARY PRACTICE ADDRESSES FROM NPIDATA FLAT FILE
# =============================================================================
extract_ent_primary <- function() {
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("PHASE 1: Extracting ENT primary practice addresses from flat file\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  if (!file.exists(NPIDATA_FILE))
    stop(sprintf("NPPES flat file not found: %s", NPIDATA_FILE))

  cat(sprintf("  Source: %s\n\n", NPIDATA_FILE))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit({try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

  tax_conditions <- paste(
    sprintf("\"Healthcare Provider Taxonomy Code_%d\" LIKE '%s'", seq_len(15), ENT_PATTERN),
    collapse = "\n     OR "
  )

  sql <- sprintf("
    SELECT
      \"NPI\"                                                       AS npi,
      \"Provider Last Name (Legal Name)\"                           AS last_name,
      \"Provider First Name\"                                       AS first_name,
      \"Provider Middle Name\"                                      AS middle_name,
      \"Provider Credential Text\"                                  AS credential,
      \"Provider Sex Code\"                                         AS gender,
      \"Provider Enumeration Date\"                                 AS enumeration_date,
      \"Last Update Date\"                                          AS last_updated,
      \"Provider First Line Business Practice Location Address\"    AS address_line1,
      \"Provider Second Line Business Practice Location Address\"   AS address_line2,
      \"Provider Business Practice Location Address City Name\"     AS city,
      \"Provider Business Practice Location Address State Name\"    AS state,
      \"Provider Business Practice Location Address Postal Code\"   AS zip,
      \"Provider Business Practice Location Address Telephone Number\" AS phone,
      \"Healthcare Provider Taxonomy Code_1\"  AS taxonomy_1,
      \"Healthcare Provider Taxonomy Code_2\"  AS taxonomy_2,
      \"Healthcare Provider Taxonomy Code_3\"  AS taxonomy_3,
      \"Healthcare Provider Taxonomy Code_4\"  AS taxonomy_4,
      \"Healthcare Provider Taxonomy Code_5\"  AS taxonomy_5,
      \"Healthcare Provider Taxonomy Code_6\"  AS taxonomy_6,
      \"Healthcare Provider Taxonomy Code_7\"  AS taxonomy_7,
      \"Healthcare Provider Taxonomy Code_8\"  AS taxonomy_8,
      \"Healthcare Provider Taxonomy Code_9\"  AS taxonomy_9,
      \"Healthcare Provider Taxonomy Code_10\" AS taxonomy_10,
      \"Healthcare Provider Taxonomy Code_11\" AS taxonomy_11,
      \"Healthcare Provider Taxonomy Code_12\" AS taxonomy_12,
      \"Healthcare Provider Taxonomy Code_13\" AS taxonomy_13,
      \"Healthcare Provider Taxonomy Code_14\" AS taxonomy_14,
      \"Healthcare Provider Taxonomy Code_15\" AS taxonomy_15
    FROM read_csv('%s', all_varchar = true, sample_size = -1)
    WHERE \"Entity Type Code\" = '1'
      AND (\"NPI Deactivation Date\" IS NULL OR \"NPI Deactivation Date\" = '')
      AND (%s)
  ", NPIDATA_FILE, tax_conditions)

  cat("  Querying DuckDB (scanning 10 GB flat file — ~11 minutes)...\n")
  t0 <- Sys.time()
  df <- DBI::dbGetQuery(con, sql)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  Done in %.0f s: %s records\n\n", elapsed, format(nrow(df), big.mark = ",")))

  df <- df[!duplicated(df$npi), ]

  all_codes <- unlist(df[, grep("^taxonomy_", names(df))])
  ent_codes <- all_codes[grepl("^207Y", all_codes) & !is.na(all_codes) & nzchar(all_codes)]
  cat("  ENT taxonomy code distribution:\n")
  tbl <- sort(table(ent_codes), decreasing = TRUE)
  for (i in seq_along(tbl))
    cat(sprintf("    %-15s  %s\n", names(tbl)[i], format(tbl[i], big.mark = ",")))
  cat("\n")

  df
}

# =============================================================================
# MAIN
# =============================================================================
main <- function() {
  t0 <- Sys.time()
  cat("\n")
  cat("=======================================================================\n")
  cat("  GEOCODE ENT PHYSICIANS FROM NPPES FEBRUARY 2026 FLAT FILE\n")
  cat(sprintf("  Started: %s\n", format(t0, "%Y-%m-%d %H:%M:%S")))
  cat("=======================================================================\n\n")

  pipeline_notify("ENT flat-file geocoding started",
                  sprintf("Launched at %s", format(t0, "%H:%M:%S")))

  dir.create(OUTPUT_DIR,     recursive = TRUE, showWarnings = FALSE)
  dir.create(CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)

  # ─── Phase 1: Extract primary addresses ──────────────────────────────────────
  ck_primary_raw <- file.path(CHECKPOINT_DIR, "ent_primary_raw.rds")
  if (file.exists(ck_primary_raw)) {
    cat("Restoring Phase 1 checkpoint...\n")
    primary_raw <- readRDS(ck_primary_raw)
    cat(sprintf("  Loaded %s rows.\n\n", format(nrow(primary_raw), big.mark = ",")))
  } else {
    primary_raw <- extract_ent_primary()
    saveRDS(primary_raw, paste0(ck_primary_raw, ".tmp"))
    file.rename(paste0(ck_primary_raw, ".tmp"), ck_primary_raw)
  }

  # ─── Phase 2: Geocode primary (geocode_batch_first_with_fallbacks) ────────────
  ck_primary_geo <- file.path(CHECKPOINT_DIR, "ent_primary_geocoded.rds")
  if (file.exists(ck_primary_geo)) {
    cat("Restoring Phase 2 checkpoint (primary geocoded)...\n")
    primary_geo <- readRDS(ck_primary_geo)
    cat(sprintf("  Loaded %s rows.\n\n", format(nrow(primary_geo), big.mark = ",")))
  } else {
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("PHASE 2: Geocoding primary practice addresses\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n")

    geo_input <- primary_raw %>%
      dplyr::mutate(
        full_address = trimws(paste(
          dplyr::coalesce(address_line1, ""),
          dplyr::coalesce(address_line2, "")
        )),
        full_address = dplyr::na_if(trimws(full_address), ""),
        city  = dplyr::na_if(trimws(dplyr::coalesce(city, "")),  ""),
        state = dplyr::na_if(toupper(trimws(dplyr::coalesce(state, ""))), ""),
        zip   = dplyr::na_if(substr(gsub("[^0-9]", "", dplyr::coalesce(zip, "")), 1, 5), "")
      )

    primary_geo <- geocode_batch_first_with_fallbacks(
      addresses        = geo_input,
      address_col      = "full_address",
      city_col         = "city",
      state_col        = "state",
      zip_col          = "zip",
      batch_size       = 10000,
      enable_fallbacks = TRUE,
      fast_mode        = FALSE
    )
    saveRDS(primary_geo, paste0(ck_primary_geo, ".tmp"))
    file.rename(paste0(ck_primary_geo, ".tmp"), ck_primary_geo)
    cat(sprintf("  Checkpoint saved: %s\n\n", ck_primary_geo))
  }

  # ─── Phase 3: Extract + geocode secondary (extract_secondary_locations via pl_pfile) ──
  ck_secondary_geo <- file.path(CHECKPOINT_DIR, "ent_secondary_geocoded.rds")
  if (file.exists(ck_secondary_geo)) {
    cat("Restoring Phase 3 checkpoint (secondary geocoded)...\n")
    secondary_geo <- readRDS(ck_secondary_geo)
    cat(sprintf("  Loaded %s rows.\n\n", format(nrow(secondary_geo), big.mark = ",")))
  } else {
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat("PHASE 3: Extracting + geocoding secondary practice addresses\n")
    cat(paste(rep("=", 70), collapse = ""), "\n\n")

    # extract_secondary_locations now supports nppes_pl_file as a 3rd source
    source(here("R", "extract_secondary_practice_locations.R"))

    secondary_raw <- extract_secondary_locations(
      cohort_path    = ck_primary_geo,   # geocoded primary file acts as cohort
      duckdb_path    = DUCKDB_PATH,
      output_dir     = CHECKPOINT_DIR,
      nppes_pl_file  = PL_FILE
    )

    secondary_geo <- geocode_batch_first_with_fallbacks(
      addresses        = secondary_raw,
      address_col      = "address_line1",
      city_col         = "city",
      state_col        = "state",
      zip_col          = "zip",
      batch_size       = 10000,
      enable_fallbacks = TRUE,
      fast_mode        = FALSE
    )
    saveRDS(secondary_geo, paste0(ck_secondary_geo, ".tmp"))
    file.rename(paste0(ck_secondary_geo, ".tmp"), ck_secondary_geo)
    cat(sprintf("  Checkpoint saved: %s\n\n", ck_secondary_geo))
  }

  # ─── Phase 4: Save (write_parquet_atomic + write_csv_atomic + dbxcli_put) ────
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("PHASE 4: Saving results\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")

  primary_parquet   <- file.path(OUTPUT_DIR, "ent_primary_geocoded.parquet")
  secondary_parquet <- file.path(OUTPUT_DIR, "ent_secondary_geocoded.parquet")
  primary_csv       <- file.path(OUTPUT_DIR, "ent_primary_geocoded.csv")
  secondary_csv     <- file.path(OUTPUT_DIR, "ent_secondary_geocoded.csv")

  write_parquet_atomic(primary_geo,   primary_parquet)
  write_csv_atomic(primary_geo,       primary_csv)
  write_parquet_atomic(secondary_geo, secondary_parquet)
  write_csv_atomic(secondary_geo,     secondary_csv)

  cat(sprintf("  Primary:   %s (%s rows)\n", basename(primary_parquet),
              format(nrow(primary_geo), big.mark = ",")))
  cat(sprintf("  Secondary: %s (%s rows)\n", basename(secondary_parquet),
              format(nrow(secondary_geo), big.mark = ",")))

  if (exists("dbxcli_available", mode = "function") && dbxcli_available()) {
    dbxcli_put(primary_parquet,   "/isochrones/ent/ent_primary_geocoded.parquet",   max_retries = 3)
    dbxcli_put(primary_csv,       "/isochrones/ent/ent_primary_geocoded.csv",       max_retries = 3)
    dbxcli_put(secondary_parquet, "/isochrones/ent/ent_secondary_geocoded.parquet", max_retries = 3)
    dbxcli_put(secondary_csv,     "/isochrones/ent/ent_secondary_geocoded.csv",     max_retries = 3)
  }

  # ─── Summary ─────────────────────────────────────────────────────────────────
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  n_prim_geo <- sum(!is.na(primary_geo$latitude))
  n_sec_geo  <- sum(!is.na(secondary_geo$latitude))

  cat("\n=======================================================================\n")
  cat("                             COMPLETE\n")
  cat("=======================================================================\n")
  cat(sprintf("  Primary  : %s total, %s geocoded (%.1f%%)\n",
              format(nrow(primary_geo), big.mark = ","),
              format(n_prim_geo, big.mark = ","),
              100 * n_prim_geo / max(nrow(primary_geo), 1)))
  cat(sprintf("  Secondary: %s total, %s geocoded (%.1f%%)\n",
              format(nrow(secondary_geo), big.mark = ","),
              format(n_sec_geo, big.mark = ","),
              100 * n_sec_geo / max(nrow(secondary_geo), 1)))
  cat(sprintf("  Elapsed  : %.1f minutes\n", elapsed))
  cat("=======================================================================\n\n")

  pipeline_notify(
    sprintf("ENT geocoding DONE: %s primary, %s secondary",
            format(nrow(primary_geo), big.mark = ","),
            format(nrow(secondary_geo), big.mark = ",")),
    sprintf("Elapsed: %.1f min", elapsed),
    priority = "critical"
  )

  invisible(NULL)
}

if (sys.nframe() == 0L) {
  tryCatch(
    main(),
    error = function(e) {
      pipeline_notify("ENT GEOCODING FAILED", e$message, priority = "critical")
      cat(sprintf("\nFATAL ERROR: %s\n", e$message))
      cat(sprintf("Traceback:\n%s\n", paste(capture.output(traceback()), collapse = "\n")))
      quit(status = 1)
    }
  )
}
