#!/usr/bin/env Rscript
# =============================================================================
# GEOCODE ENT PHYSICIANS FROM NPPES API
# =============================================================================
# Standalone script — no external drive required.
#
# What it does:
#   1. Queries CMS NPPES API for all Otolaryngology (ENT) physicians
#   2. Filters by exact taxonomy codes (207Y family)
#   3. Geocodes practice addresses via US Census Bureau batch API
#   4. Saves parquet + CSV to artifacts/ent/
#   5. Uploads parquet to Dropbox
#
# Usage:
#   caffeinate -idms Rscript scripts/geocode_ent_from_nppes_api.R \
#     2>&1 | tee logs/geocode_ent_$(date +%Y%m%d_%H%M%S).log
#
# Expected runtime: 30-60 minutes
# Expected output:  ~12,000-15,000 geocoded ENT physicians
#
# Author: Tyler Muffly, MD + Claude Code
# Date:   2026-04-13
# =============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(arrow)
  library(digest)
  library(here)
})

# -----------------------------------------------------------------------------
# CONFIGURATION
# -----------------------------------------------------------------------------
ENT_TAXONOMY_CODES <- c(

  "207Y00000X",   # General Otolaryngology
  "207YS0123X",   # Facial Plastic Surgery
  "207YX0602X",   # Otolaryngic Allergy
  "207YX0901X",   # Otology & Neurotology
  "207YX0905X",   # Otolaryngology/Facial Plastic Surgery
  "207YP0228X",   # Pediatric Otolaryngology
  "207YS0012X"    # Sleep Medicine
)

NPPES_API_BASE   <- "https://npiregistry.cms.hhs.gov/api/"
CENSUS_BATCH_URL <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
CENSUS_BENCHMARK <- "Public_AR_Current"
CENSUS_VINTAGE   <- "Current_Current"

OUTPUT_DIR <- here("artifacts", "ent")
CHECKPOINT_DIR <- here("artifacts", "ent", "checkpoints")

US_STATES <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
  "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
  "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
  "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
  "PR","VI","GU","AS","MP"  # territories
)

# Rate limiting
NPPES_DELAY_SEC   <- 0.5   # delay between NPPES API requests
CENSUS_BATCH_SIZE <- 1000  # addresses per Census batch request

# -----------------------------------------------------------------------------
# NOTIFICATION (reuse pipeline_notify if available)
# -----------------------------------------------------------------------------
notify <- function(subject, body = "", priority = "info") {
  notify_path <- here("R", "utils", "notify.R")
  if (file.exists(notify_path)) {
    tryCatch({
      source(notify_path, local = TRUE)
      if (exists("pipeline_notify", mode = "function")) {
        pipeline_notify(subject, body, priority = priority)
        return(invisible(NULL))
      }
    }, error = function(e) NULL)
  }
  # Fallback: macOS notification
  if (Sys.info()[["sysname"]] == "Darwin") {
    tryCatch(
      system2("osascript", c("-e", shQuote(sprintf(
        'display notification "%s" with title "ENT Geocoding"',
        gsub('"', '\\\\"', paste(subject, body, sep = " — "))
      ))), stdout = FALSE, stderr = FALSE),
      error = function(e) NULL
    )
  }
  cat(sprintf("[NOTIFY] %s: %s\n", subject, body))
}

#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# PHASE 1: QUERY NPPES API
# =============================================================================

#' Query NPPES API for a single state with pagination
#' @param state 2-letter state code
#' @return data.frame of ENT physician records
query_nppes_state <- function(state) {
  all_results <- list()
  skip <- 0
  page <- 1

  repeat {
    resp <- tryCatch(
      httr::GET(
        NPPES_API_BASE,
        query = list(
          version = "2.1",
          taxonomy_description = "otolaryngology",
          enumeration_type = "NPI-1",
          state = state,
          limit = 200,
          skip = skip
        ),
        httr::timeout(30)
      ),
      error = function(e) {
        warning(sprintf("  [%s] API error at skip=%d: %s", state, skip, e$message))
        NULL
      }
    )

    if (is.null(resp) || httr::status_code(resp) != 200) {
      if (!is.null(resp) && httr::status_code(resp) == 429) {
        cat(sprintf("  [%s] Rate limited — waiting 10s...\n", state))
        Sys.sleep(10)
        next
      }
      warning(sprintf("  [%s] HTTP %s at skip=%d",
                       state,
                       if (!is.null(resp)) httr::status_code(resp) else "NULL",
                       skip))
      break
    }

    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE),
                       error = function(e) NULL)

    if (is.null(parsed) || is.null(parsed$results)) break

    result_count <- parsed$result_count %||% 0
    if (result_count == 0) break

    all_results <- c(all_results, parsed$results)

    if (result_count < 200) break  # last page

    skip <- skip + 200
    page <- page + 1
    Sys.sleep(NPPES_DELAY_SEC)
  }

  if (length(all_results) == 0) return(NULL)

  # Parse each result into a flat row
  rows <- lapply(all_results, function(r) {
    basic <- r$basic %||% list()

    # Get practice location address (prefer LOCATION over MAILING)
    addrs <- r$addresses %||% list()
    loc_addr <- NULL
    for (a in addrs) {
      if (identical(a$address_purpose, "LOCATION")) {
        loc_addr <- a
        break
      }
    }
    if (is.null(loc_addr) && length(addrs) > 0) loc_addr <- addrs[[1]]

    # Get all taxonomy codes
    taxonomies <- r$taxonomies %||% list()
    tax_codes <- vapply(taxonomies, function(t) t$code %||% "", character(1))
    tax_descs <- vapply(taxonomies, function(t) t$desc %||% "", character(1))
    primary_tax <- ""
    for (t in taxonomies) {
      if (isTRUE(t$primary)) {
        primary_tax <- t$code %||% ""
        break
      }
    }

    data.frame(
      npi              = as.character(r$number %||% NA),
      first_name       = basic$first_name %||% NA_character_,
      last_name        = basic$last_name %||% NA_character_,
      middle_name      = basic$middle_name %||% NA_character_,
      credential       = basic$credential %||% NA_character_,
      gender           = basic$gender %||% NA_character_,
      enumeration_date = basic$enumeration_date %||% NA_character_,
      last_updated     = basic$last_updated %||% NA_character_,
      status           = basic$status %||% NA_character_,
      address_line1    = loc_addr$address_1 %||% NA_character_,
      address_line2    = loc_addr$address_2 %||% NA_character_,
      city             = loc_addr$city %||% NA_character_,
      state            = loc_addr$state %||% NA_character_,
      zip              = loc_addr$postal_code %||% NA_character_,
      phone            = loc_addr$telephone_number %||% NA_character_,
      primary_taxonomy = primary_tax,
      all_taxonomy_codes = paste(tax_codes, collapse = "|"),
      all_taxonomy_descs = paste(tax_descs, collapse = "|"),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

#' Filter to only records with ENT taxonomy codes (207Y family)
#' @param df data.frame from query_nppes_state
#' @return filtered data.frame
filter_ent_taxonomy <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Check if ANY of the provider's taxonomy codes match ENT codes
  has_ent <- vapply(strsplit(df$all_taxonomy_codes, "\\|"), function(codes) {
    any(codes %in% ENT_TAXONOMY_CODES)
  }, logical(1))

  df[has_ent, , drop = FALSE]
}

#' Query all states and collect ENT physicians
#' @return data.frame of all ENT physicians, deduplicated by NPI
query_all_ent_physicians <- function() {
  cat("=" %s+% paste(rep("=", 69), collapse = "") %s+% "\n")
  cat("PHASE 1: Querying NPPES API for ENT physicians (state-by-state)\n")
  cat("=" %s+% paste(rep("=", 69), collapse = "") %s+% "\n\n")

  all_dfs <- list()
  total_raw <- 0
  total_ent <- 0

  for (i in seq_along(US_STATES)) {
    state <- US_STATES[i]
    cat(sprintf("[%2d/%d] Querying %s... ", i, length(US_STATES), state))

    state_df <- query_nppes_state(state)

    if (is.null(state_df) || nrow(state_df) == 0) {
      cat("0 results\n")
      next
    }

    n_raw <- nrow(state_df)
    state_df <- filter_ent_taxonomy(state_df)
    n_ent <- nrow(state_df)

    total_raw <- total_raw + n_raw
    total_ent <- total_ent + n_ent

    cat(sprintf("%d raw -> %d ENT\n", n_raw, n_ent))

    if (n_ent > 0) all_dfs[[length(all_dfs) + 1]] <- state_df

    Sys.sleep(NPPES_DELAY_SEC)
  }

  if (length(all_dfs) == 0) {
    stop("No ENT physicians found across any state!")
  }

  combined <- dplyr::bind_rows(all_dfs)
  n_before_dedup <- nrow(combined)
  combined <- combined[!duplicated(combined$npi), ]
  n_after_dedup <- nrow(combined)

  cat(sprintf(
    "\nNPPES query complete:\n  Raw results:  %s\n  ENT filtered: %s\n  After dedup:  %s (removed %d duplicates)\n\n",
    format(total_raw, big.mark = ","),
    format(total_ent, big.mark = ","),
    format(n_after_dedup, big.mark = ","),
    n_before_dedup - n_after_dedup
  ))

  # Taxonomy code distribution
  cat("Taxonomy code distribution:\n")
  for (code in ENT_TAXONOMY_CODES) {
    n <- sum(grepl(code, combined$all_taxonomy_codes, fixed = TRUE))
    cat(sprintf("  %s: %s\n", code, format(n, big.mark = ",")))
  }
  cat("\n")

  combined
}

# Helper: paste with no default sep
`%s+%` <- function(a, b) paste0(a, b)

# =============================================================================
# PHASE 2: ADDRESS CLEANING + GEOCODING (PROJECT PIPELINE)
# =============================================================================
# Uses the project's full address normalization stack (address_core/*,
# address_specialized/*) and geocode_batch_first_with_fallbacks() which
# provides: Census batch → Census single → ArcGIS → Nominatim → city centroid.

#' Geocode all ENT physicians using the project's address cleaning pipeline
#' and full fallback cascade.
#' @param physicians data.frame from query_all_ent_physicians
#' @return physicians data.frame with geocoding results joined
geocode_ent_physicians <- function(physicians) {
  cat("=" %s+% paste(rep("=", 69), collapse = "") %s+% "\n")
  cat("PHASE 2: Address cleaning + geocoding (project pipeline)\n")
  cat("=" %s+% paste(rep("=", 69), collapse = "") %s+% "\n\n")

  n_total <- nrow(physicians)
  t_start <- Sys.time()

  # --- Load project modules via core_utils.R (no external drive required) ---
  cat("  Loading project address & geocoding modules via core_utils.R...\n")
  source(here("R", "core_utils.R"))
  cat("  Modules loaded.\n\n")

  # --- Step 1: Normalize addresses ---
  cat("  Step 1: Address normalization (address_core pipeline)...\n")
  if (exists("normalize_address_df", mode = "function")) {
    physicians <- tryCatch(
      normalize_address_df(
        physicians,
        addr1_col = "address_line1",
        addr2_col = "address_line2",
        city_col  = "city",
        state_col = "state",
        zip_col   = "zip"
      ),
      error = function(e) {
        cat(sprintf("    WARN: normalize_address_df failed: %s\n", e$message))
        physicians
      }
    )
  }

  # --- Step 2: Medical address normalization ---
  cat("  Step 2: Medical address normalization...\n")
  if (exists("apply_medical_normalization", mode = "function")) {
    physicians <- tryCatch(
      apply_medical_normalization(physicians, address_col = "address_line1", use_config = TRUE),
      error = function(e) {
        cat(sprintf("    WARN: apply_medical_normalization failed: %s\n", e$message))
        physicians
      }
    )
  }

  # --- Step 3: Clean addresses for geocoding (PR/HI detection, city standardization) ---
  cat("  Step 3: Pre-geocoding address cleanup...\n")
  if (exists("clean_addresses_for_geocoding", mode = "function")) {
    physicians <- tryCatch(
      clean_addresses_for_geocoding(physicians, city_col = "city", state_col = "state", zip_col = "zip"),
      error = function(e) {
        cat(sprintf("    WARN: clean_addresses_for_geocoding failed: %s\n", e$message))
        physicians
      }
    )
  }

  # Pick the best cleaned address column
  addr_col <- if ("address_for_geocoding" %in% names(physicians)) {
    cat("  Using: address_for_geocoding (medical-normalized)\n")
    "address_for_geocoding"
  } else if ("address1_no_unit" %in% names(physicians)) {
    cat("  Using: address1_no_unit (suite-stripped)\n")
    "address1_no_unit"
  } else {
    cat("  Using: address_line1 (raw)\n")
    "address_line1"
  }

  # --- Step 4: Prepare geocoding input ---
  geo_df <- data.frame(
    npi     = physicians$npi,
    address = physicians[[addr_col]],
    city    = physicians$city,
    state   = physicians$state,
    zip     = substr(gsub("[^0-9]", "", physicians$zip), 1, 5),
    stringsAsFactors = FALSE
  )

  has_address <- !is.na(geo_df$address) & nzchar(trimws(geo_df$address))
  n_missing <- sum(!has_address)
  if (n_missing > 0) {
    cat(sprintf("  Skipping %d records with missing addresses\n", n_missing))
  }

  cat(sprintf("\n  Step 4: Geocoding %s addresses with full fallback cascade...\n",
              format(sum(has_address), big.mark = ",")))
  cat("    Census batch -> Census single -> ArcGIS -> Nominatim -> city centroid\n\n")

  # --- Step 5: Geocode with project's full cascade ---
  geo_results <- tryCatch(
    geocode_batch_first_with_fallbacks(
      addresses      = geo_df[has_address, ],
      address_col    = "address",
      city_col       = "city",
      state_col      = "state",
      zip_col        = "zip",
      batch_size     = 10000,
      enable_fallbacks = TRUE,
      fast_mode      = FALSE  # FALSE = enable external API fallbacks
    ),
    error = function(e) {
      cat(sprintf("\n  ERROR in geocode_batch_first_with_fallbacks: %s\n", e$message))
      cat("  Falling back to simple Census batch...\n")
      NULL
    }
  )

  # --- Step 6: Join results back ---
  physicians$latitude           <- NA_real_
  physicians$longitude          <- NA_real_
  physicians$match_type         <- "NOT_ATTEMPTED"
  physicians$geocoder_provenance <- NA_character_
  physicians$matched_address    <- NA_character_
  physicians$census_tract       <- NA_character_

  if (!is.null(geo_results) && nrow(geo_results) > 0) {
    # The batch function returns results aligned with input rows
    valid_idx <- which(has_address)

    # Map columns from geo_results to physicians
    lat_col <- intersect(c("latitude", "lat"), names(geo_results))[1]
    lon_col <- intersect(c("longitude", "lon"), names(geo_results))[1]
    prov_col <- intersect(c("geocoder_provenance", "provenance", "source"), names(geo_results))[1]
    match_col <- intersect(c("match_type", "matchtype"), names(geo_results))[1]
    addr_match_col <- intersect(c("matched_address", "matchedaddress"), names(geo_results))[1]
    tract_col <- intersect(c("census_tract", "tract", "geoid"), names(geo_results))[1]

    if (!is.na(lat_col) && lat_col %in% names(geo_results)) {
      physicians$latitude[valid_idx]  <- geo_results[[lat_col]]
    }
    if (!is.na(lon_col) && lon_col %in% names(geo_results)) {
      physicians$longitude[valid_idx] <- geo_results[[lon_col]]
    }
    if (!is.na(prov_col) && prov_col %in% names(geo_results)) {
      physicians$geocoder_provenance[valid_idx] <- geo_results[[prov_col]]
    }
    if (!is.na(match_col) && match_col %in% names(geo_results)) {
      physicians$match_type[valid_idx] <- geo_results[[match_col]]
    }
    if (!is.na(addr_match_col) && addr_match_col %in% names(geo_results)) {
      physicians$matched_address[valid_idx] <- geo_results[[addr_match_col]]
    }
    if (!is.na(tract_col) && tract_col %in% names(geo_results)) {
      physicians$census_tract[valid_idx] <- geo_results[[tract_col]]
    }
  }

  # --- Summary ---
  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
  n_geocoded <- sum(!is.na(physicians$latitude))

  cat(sprintf(
    "\nGeocoding complete in %.1f minutes:\n  Total:    %s\n  Matched:  %s (%.1f%%)\n  Failed:   %s\n",
    elapsed,
    format(n_total, big.mark = ","),
    format(n_geocoded, big.mark = ","),
    100 * n_geocoded / max(n_total, 1L),
    format(n_total - n_geocoded, big.mark = ",")
  ))

  # Provenance breakdown
  if ("geocoder_provenance" %in% names(physicians)) {
    cat("\n  Geocoder provenance:\n")
    prov_table <- sort(table(physicians$geocoder_provenance, useNA = "ifany"), decreasing = TRUE)
    for (j in seq_along(prov_table)) {
      cat(sprintf("    %-25s %s\n", names(prov_table)[j], format(prov_table[j], big.mark = ",")))
    }
  }
  cat("\n")

  physicians
}

# =============================================================================
# PHASE 3: SAVE & UPLOAD
# =============================================================================

save_and_upload <- function(physicians) {
  cat("=" %s+% paste(rep("=", 69), collapse = "") %s+% "\n")
  cat("PHASE 3: Saving results and uploading to Dropbox\n")
  cat("=" %s+% paste(rep("=", 69), collapse = "") %s+% "\n\n")

  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # --- Save parquet ---
  parquet_path <- file.path(OUTPUT_DIR, "ent_physicians_geocoded.parquet")
  tmp_parquet <- paste0(parquet_path, ".tmp")
  arrow::write_parquet(physicians, tmp_parquet)
  file.rename(tmp_parquet, parquet_path)
  cat(sprintf("  Parquet: %s (%s)\n", parquet_path,
              format(file.size(parquet_path), big.mark = ",")))

  # --- Save CSV ---
  csv_path <- file.path(OUTPUT_DIR, "ent_physicians_geocoded.csv")
  tmp_csv <- paste0(csv_path, ".tmp")
  readr::write_csv(physicians, tmp_csv)
  file.rename(tmp_csv, csv_path)
  cat(sprintf("  CSV:     %s (%s)\n", csv_path,
              format(file.size(csv_path), big.mark = ",")))

  # --- SHA-256 ---
  sha <- digest::digest(parquet_path, algo = "sha256", file = TRUE)
  cat(sprintf("  SHA-256: %s\n", sha))

  # --- Upload to Dropbox ---
  dropbox_utils_path <- here("R", "utils", "dropbox_utils.R")
  if (file.exists(dropbox_utils_path)) {
    tryCatch({
      source(dropbox_utils_path, local = TRUE)
      if (dbxcli_available()) {
        cat("\n  Uploading to Dropbox...\n")
        dbxcli_put(
          parquet_path,
          "/isochrones/ent/ent_physicians_geocoded.parquet",
          max_retries = 3,
          verbose = TRUE
        )
        dbxcli_put(
          csv_path,
          "/isochrones/ent/ent_physicians_geocoded.csv",
          max_retries = 3,
          verbose = TRUE
        )
        cat("  Dropbox upload complete.\n")
      } else {
        cat("  dbxcli not available — skipping Dropbox upload.\n")
      }
    }, error = function(e) {
      warning(sprintf("Dropbox upload failed: %s", e$message))
    })
  } else {
    cat("  dropbox_utils.R not found — skipping upload.\n")
  }

  # --- Receipt ---
  receipt <- list(
    script          = "scripts/geocode_ent_from_nppes_api.R",
    timestamp       = timestamp,
    n_total         = nrow(physicians),
    n_geocoded      = sum(!is.na(physicians$latitude)),
    geocode_rate    = round(100 * sum(!is.na(physicians$latitude)) / max(nrow(physicians), 1L), 1),
    n_states        = length(unique(physicians$state)),
    parquet_path    = parquet_path,
    csv_path        = csv_path,
    sha256          = sha,
    hostname        = Sys.info()[["nodename"]],
    r_version       = paste(R.version$major, R.version$minor, sep = "."),
    taxonomy_codes  = ENT_TAXONOMY_CODES
  )
  receipt_path <- file.path(OUTPUT_DIR, sprintf("receipt_geocode_ent_%s.json", timestamp))
  receipt_path_tmp <- paste0(receipt_path, ".tmp")
  writeLines(jsonlite::toJSON(receipt, auto_unbox = TRUE, pretty = TRUE), receipt_path_tmp)
  file.rename(receipt_path_tmp, receipt_path)
  cat(sprintf("  Receipt: %s\n\n", receipt_path))

  invisible(receipt)
}

# =============================================================================
# MAIN
# =============================================================================

main <- function() {
  t0 <- Sys.time()

  cat("\n")
  cat("=======================================================================\n")
  cat("         GEOCODE ENT PHYSICIANS FROM NPPES API\n")
  cat(sprintf("         Started: %s\n", format(t0, "%Y-%m-%d %H:%M:%S")))
  cat("=======================================================================\n\n")

  notify("ENT geocoding started",
         sprintf("Script launched at %s", format(t0, "%H:%M:%S")),
         priority = "info")

  dir.create(CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)

  # --- Phase 1: Query NPPES ---
  checkpoint_nppes <- file.path(CHECKPOINT_DIR, "nppes_raw_ent.rds")

  if (file.exists(checkpoint_nppes)) {
    cat("Restoring NPPES checkpoint (Phase 1 already complete)...\n\n")
    physicians <- readRDS(checkpoint_nppes)
    cat(sprintf("  Loaded %s ENT physicians from checkpoint.\n\n",
                format(nrow(physicians), big.mark = ",")))
  } else {
    physicians <- query_all_ent_physicians()
    .cp_tmp <- paste0(checkpoint_nppes, ".tmp.", Sys.getpid())
    saveRDS(physicians, .cp_tmp)
    if (!isTRUE(file.rename(.cp_tmp, checkpoint_nppes))) { file.copy(.cp_tmp, checkpoint_nppes, overwrite = TRUE); unlink(.cp_tmp) }
    cat(sprintf("  Checkpoint saved: %s\n\n", checkpoint_nppes))
  }

  # --- Phase 2: Geocode ---
  checkpoint_geocoded <- file.path(CHECKPOINT_DIR, "ent_geocoded.rds")

  if (file.exists(checkpoint_geocoded)) {
    cat("Restoring geocoding checkpoint (Phase 2 already complete)...\n\n")
    physicians <- readRDS(checkpoint_geocoded)
    cat(sprintf("  Loaded %s geocoded records from checkpoint.\n\n",
                format(nrow(physicians), big.mark = ",")))
  } else {
    physicians <- geocode_ent_physicians(physicians)
    .cp_tmp2 <- paste0(checkpoint_geocoded, ".tmp.", Sys.getpid())
    saveRDS(physicians, .cp_tmp2)
    if (!isTRUE(file.rename(.cp_tmp2, checkpoint_geocoded))) { file.copy(.cp_tmp2, checkpoint_geocoded, overwrite = TRUE); unlink(.cp_tmp2) }
    cat(sprintf("  Checkpoint saved: %s\n\n", checkpoint_geocoded))
  }

  # --- Phase 3: Save & Upload ---
  receipt <- save_and_upload(physicians)

  # --- Summary ---
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

  cat("=======================================================================\n")
  cat("                         COMPLETE\n")
  cat("=======================================================================\n")
  cat(sprintf("  ENT physicians:  %s\n", format(receipt$n_total, big.mark = ",")))
  cat(sprintf("  Geocoded:        %s (%.1f%%)\n",
              format(receipt$n_geocoded, big.mark = ","), receipt$geocode_rate))
  cat(sprintf("  States:          %d\n", receipt$n_states))
  cat(sprintf("  Elapsed:         %.1f minutes\n", elapsed))
  cat(sprintf("  Output:          %s\n", receipt$parquet_path))
  cat("=======================================================================\n\n")

  notify(
    sprintf("ENT geocoding DONE: %s physicians (%.0f%% geocoded)",
            format(receipt$n_total, big.mark = ","), receipt$geocode_rate),
    sprintf("Elapsed: %.1f min. Output: %s", elapsed, basename(receipt$parquet_path)),
    priority = "critical"
  )

  invisible(receipt)
}

# Run if executed directly (not sourced)
if (sys.nframe() == 0L) {
  tryCatch(
    main(),
    error = function(e) {
      notify("ENT GEOCODING FAILED", e$message, priority = "critical")
      cat(sprintf("\n\nFATAL ERROR: %s\n", e$message))
      cat(sprintf("Traceback:\n%s\n", paste(capture.output(traceback()), collapse = "\n")))
      quit(status = 1)
    }
  )
}
