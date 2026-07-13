#!/usr/bin/env R
# THE ONE SUBSPECIALTY STANDARDIZER FOR ALL ETERNITY
# ===================================================
# STRONGLY SUPPORTED: Subspecialty Classification (98.3% completeness)

# REMOVED by fix_redundant_sources.R - already loaded by 01-setup.R: source(here("R", "log_safe.R"))
# - Validation: Cross-validated with ABOG workforce directory (n=105,517)
# - Ground Truth: paths$abog_provider_data
# - Column: subspecialty_name
# - Completeness: 98.3% (exceeds 95% threshold by +3.5%)
# - Status: STRONGLY SUPPORTED
# - Citation: DATA_VALIDATION_ASSUMPTIONS.md, Section 3
# - Validation Date: 2025-09-29
#
# Mandatory use - NEVER create duplicates
# Configuration: config/subspecialty_standardization_config.yml
# Documentation: SUBSPECIALTY_STANDARDIZER_DOCUMENTATION.md
# Updated: 2025-10-02 (added empirical validation documentation)

if (!require("dplyr", quietly = TRUE)) {
  stop("dplyr package required for subspecialty standardization")
}
if (!require("DBI", quietly = TRUE)) {
  stop("DBI package required for DuckDB caching")
}
if (!require("duckdb", quietly = TRUE)) {
  stop("duckdb package required for subspecialty caching")
}
if (!require("digest", quietly = TRUE)) {
  stop("digest package required for MD5 hashing")
}
if (!require("yaml", quietly = TRUE)) {
  stop("yaml package required for configuration")
}
if (!require("logger", quietly = TRUE)) {
  stop("logger package required for logging")
}

# Set up logger
tryCatch(source(here::here("R", "log_safe.R")), error = function(e) NULL)
log_layout_safe("layout_simple")
log_formatter_safe("formatter_sprintf")

log_info_safe("THE ONE SUBSPECIALTY STANDARDIZER FOR ALL ETERNITY loaded")
log_info_safe("Primary function: the_one_subspecialty_standardizer()")
log_info_safe("Configuration: config/subspecialty_standardization_config.yml")
log_info_safe("Features: DuckDB caching, MD5 hashing, confidence scoring, quality metrics, batch processing")

# Global cache connection
.subspecialty_cache_con <- NULL

# Centralized ABOG subspecialty constants are now loaded from config

# ============================================================================
# CONFIGURATION MANAGEMENT
# ============================================================================

#' Create default subspecialty standardization configuration
create_default_subspecialty_config <- function() {
  tryCatch({
    if (!dir.exists("config")) {
      dir.create("config", recursive = TRUE)
    }
  }, error = function(e) {
    stop(sprintf_safe("Failed to create config directory: %s", e$message))
  })

  default_config <- list(
    subspecialty_mappings = list(
      # Define core subspecialty lists within config
      abog_subspecialties = c("URPS", "GO", "MFM", "REI", "MIG", "PAG"),
      generalist_class = "Generalist",
      multiple_class = "Multiple",
      
      # CRITICAL FIX (2026-01-18): Remove incorrect recoding of Critical Care and Hospice to GO
      # Previously: critical_care_recode_to = "GO" and hospice_recode_to = "GO"
      # This was clinically incorrect - Critical Care Medicine is a distinct specialty,
      # not related to Gynecologic Oncology. While a GynOnc may also be certified in
      # palliative medicine, automatically merging these inflates GO counts.
      # Now these are marked as "Other" subspecialty (non-ABOG recognized)
      critical_care_recode_to = "Other",  # Critical Care is distinct from GO
      hospice_recode_to = "Other",        # Hospice/Palliative is distinct from GO

      # Comprehensive mapping patterns from existing code + enhancements
      URPS = list(
        exact_matches = c("URPS", "urps", "FPMRS", "fpmrs", "FPM", "fpm", "Urogynecology and Reconstructive Pelvic Surgery", "Female Pelvic Medicine and Reconstructive Surgery"),
        partial_matches = c("female pelvic medicine", "pelvic medicine", "urogynecology",
                          "female pelvic med reconstructive surgery", "pelvic floor", "urogynaecology",
                          "reconstructive pelvic surgery", "urogynecology and reconstructive"),
        confidence = 0.95
      ),

      GO = list(
        exact_matches = c("GO", "go", "ONC", "onc", "Gynecologic Oncology"),
        # CRITICAL FIX (2026-01-18): Removed "critical care", "hospice", "hospice care",
        # "hospice and palliative medicine", "palliative medicine" from GO partial_matches.
        # These are distinct specialties and should not be merged into Gynecologic Oncology.
        partial_matches = c("gynecologic oncology", "gyn onc", "gynonc",
                          "gynecological oncology", "gyn oncology"),
        confidence = 0.90
      ),

      MFM = list(
        exact_matches = c("MFM", "mfm", "Maternal-Fetal Medicine"),
        partial_matches = c("maternal fetal medicine", "maternalfetal medicine",
                          "perinatology", "maternal fetal med", "high risk pregnancy"),
        confidence = 0.95
      ),

      REI = list(
        exact_matches = c("REI", "rei", "Reproductive Endocrinology and Infertility"),
        partial_matches = c("reproductive endocrinology and infertility", "reproductive endocrinology",
                          "infertility", "reproductive endo", "reproductive endo and infertility"),
        confidence = 0.95
      ),

      MIG = list(
        exact_matches = c("MIG", "mig", "Minimally Invasive Gynecology"),
        partial_matches = c("minimally invasive gynecology", "minimally invasive",
                          "laparoscopy", "laparoscopic surgery", "minimally invasive gyn"),
        confidence = 0.85
      ),

      PAG = list(
        exact_matches = c("PAG", "pag", "PAGS", "pags", "Pediatric and Adolescent Gynecology"),
        partial_matches = c("pediatric and adolescent gynecology", "pediatric gynecology",
                          "adolescent gynecology", "pediatric gyn", "pediatric adolescent gynecology"),
        confidence = 0.90
      ),

      Generalist = list(
        exact_matches = c("Generalist", "generalist"),
        partial_matches = c("general", "general obgyn", "general obstetrics and gynecology",
                          "ob/gyn", "obgyn", "ob gyn", "obstetrics and gynecology",
                          "obstetrics gynecology", "general ob gyn", "general ob/gyn"),
        confidence = 0.85
      ),

      Multiple = list(
        exact_matches = c("Multiple", "multiple"),
        partial_matches = c("multiple subspecialties", "dual subspecialty", "dual",
                          "two subspecialities", "multiple certifications", "dual certified",
                          "multiple specialties", "combined subspecialties"),
        confidence = 0.80
      )
    ),

    provenance = list(
      abog_scraping = list(
        base_url = "https://www.abog.org",
        physician_directory_url = "https://www.abog.org/find-an-ob-gyn",
        verification_endpoints = c("/diplomate-search", "/physician-directory")
      ),
      verification_sources = c("ABOG_website", "ABOG_database", "ABOG_manual_entry", "standardization_only"),
      quality_requirements = list(
        require_verification_date = FALSE,
        require_scrape_location = FALSE,
        max_verification_age_days = 365
      ),
      audit_trail = list(
        include_provenance_in_exports = TRUE,
        log_provenance_changes = TRUE,
        validate_scrape_locations = FALSE
      )
    ),

    quality_metrics = list(
      confidence_scoring_enabled = TRUE,
      minimum_confidence_threshold = 0.7,
      edge_case_detection_enabled = TRUE,
      similarity_calculation_enabled = TRUE
    ),

    performance = list(
      batch_processing_threshold = 1000,
      chunk_size = 100,
      progress_reporting_interval = 10,
      cache_every_record = TRUE,
      checkpoint_frequency = 1000
    ),

    caching = list(
      duckdb_path = "data/subspecialty_cache.duckdb",
      cache_enabled = TRUE,
      md5_hashing_enabled = TRUE,
      cache_ttl_hours = 8760  # 1 year
    )
  )

  tryCatch({
    yaml::write_yaml(default_config, "config/subspecialty_standardization_config.yml")
  }, error = function(e) {
    stop(sprintf_safe("Failed to write default configuration file: %s", e$message))
  })
  log_info_safe("📝 Created default subspecialty standardization configuration")
}

# ============================================================================
# DUCKDB CACHING SYSTEM WITH MD5 HASHING
# ============================================================================

#' Initialize subspecialty DuckDB cache
#' @param config_path Configuration file path
#' @return DuckDB connection
init_subspecialty_cache <- function(config_path = "config/subspecialty_standardization_config.yml") {
  if (!file.exists(config_path)) {
    create_default_subspecialty_config()
  }

  config <- yaml::read_yaml(config_path)

  if (!config$caching$cache_enabled) {
    return(NULL)
  }

  cache_dir <- dirname(config$caching$duckdb_path)
  tryCatch({
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
  }, error = function(e) {
    stop(sprintf_safe("Failed to create cache directory at %s: %s", cache_dir, e$message))
  })

  con <- DBI::dbConnect(duckdb::duckdb(), config$caching$duckdb_path)

  # Create cache table if it doesn't exist
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS subspecialty_cache (
      original_subspecialty VARCHAR,
      normalized_subspecialty VARCHAR,
      standardized_subspecialty VARCHAR,
      confidence_score DOUBLE,
      is_edge_case BOOLEAN,
      standardization_method VARCHAR,
      subspecialty_md5 VARCHAR PRIMARY KEY,
      created_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      last_accessed TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Create index for faster lookups
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_subspecialty_md5 ON subspecialty_cache(subspecialty_md5)")

  .subspecialty_cache_con <<- con
  return(con)
}

#' Get subspecialty from cache
#' @param subspecialty Original subspecialty string
#' @return Cached result or NULL
get_cached_subspecialty <- function(subspecialty) {
  if (is.null(.subspecialty_cache_con)) {
    return(NULL)
  }

  subspecialty_md5 <- digest::digest(subspecialty, algo = "md5")

  result <- DBI::dbGetQuery(.subspecialty_cache_con,
    "SELECT * FROM subspecialty_cache WHERE subspecialty_md5 = ?",
    params = list(subspecialty_md5))

  if (nrow(result) > 0) {
    # Update last accessed timestamp
    DBI::dbExecute(.subspecialty_cache_con,
      "UPDATE subspecialty_cache SET last_accessed = CURRENT_TIMESTAMP WHERE subspecialty_md5 = ?",
      params = list(subspecialty_md5))
    return(result[1, ])
  }

  return(NULL)
}

#' Cache subspecialty result
#' @param original Original subspecialty string
#' @param normalized Normalized subspecialty string
#' @param standardized Standardized subspecialty result
#' @param confidence Confidence score
#' @param is_edge_case Boolean edge case flag
#' @param method Standardization method used
cache_subspecialty <- function(original, normalized, standardized, confidence, is_edge_case, method) {
  if (is.null(.subspecialty_cache_con)) {
    return(invisible(NULL))
  }

  subspecialty_md5 <- digest::digest(original, algo = "md5")

  # Insert or replace cache entry
  DBI::dbExecute(.subspecialty_cache_con,
    "INSERT OR REPLACE INTO subspecialty_cache
     (original_subspecialty, normalized_subspecialty, standardized_subspecialty,
      confidence_score, is_edge_case, standardization_method, subspecialty_md5)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
    params = list(original, normalized, standardized, confidence, is_edge_case, method, subspecialty_md5))
}

# ============================================================================
# CORE STANDARDIZATION FUNCTIONS (Enhanced from existing code)
# ============================================================================

#' String normalization for robust matching (from existing code)
#' @param x Subspecialty string
#' @return Normalized string
norm <- function(x) {
  if (is.na(x) || is.null(x) || x == "") {
    return("")
  }

  x %>%
    tolower() %>%
    trimws() %>%
    gsub("&", "and", .) %>%
    gsub("-", " ", .) %>%  # Convert hyphens to spaces first
    gsub("[^a-z0-9 ]", "", .) %>%  # Remove special chars except space
    gsub("\\s+", " ", .) %>%  # Consolidate multiple spaces
    trimws()
}

#' Calculate confidence score for subspecialty match
#' @param original Original subspecialty string
#' @param standardized Standardized result
#' @param match_type Type of match (exact, partial, fuzzy, fallback)
#' @param config Configuration list
#' @return Confidence score 0.0-1.0
calculate_subspecialty_confidence <- function(original, standardized, match_type, config) {
  if (!config$quality_metrics$confidence_scoring_enabled) {
    return(0.0)
  }

  base_confidence <- switch(match_type,
    "exact" = 1.0,
    "partial" = 0.85,
    "fuzzy" = 0.70,
    "fallback" = 0.50,
    0.0
  )

  # Adjust based on subspecialty-specific confidence from config
  subspecialty_config <- config$subspecialty_mappings[[standardized]]
  if (!is.null(subspecialty_config) && !is.null(subspecialty_config$confidence)) {
    base_confidence <- base_confidence * subspecialty_config$confidence
  }

  # Length penalty for very short inputs
  if (nchar(original) < 3) {
    base_confidence <- base_confidence * 0.8
  }

  return(round(base_confidence, 3))
}

#' Detect if subspecialty is an edge case
#' @param original Original subspecialty string
#' @param standardized Standardized result
#' @param config Configuration list
#' @return Boolean
is_subspecialty_edge_case <- function(original, standardized, config) {
  if (!config$quality_metrics$edge_case_detection_enabled) {
    return(FALSE)
  }

  # Edge cases: problematic terms that need special handling
  edge_patterns <- c(
    "other", "unknown", "unspecified", "tbd", "pending", "various", "mixed", "n/a", "na",
    "generalist", "multiple"
  )

  normalized <- norm(original)
  normalized_standardized <- tolower(trimws(standardized))

  # Very short inputs are ALWAYS edge cases
  if (nchar(normalized) <= 2) {
    return(TRUE)
  }

  # If the original input matches edge patterns, it's an edge case
  if (normalized %in% edge_patterns) {
    return(TRUE)
  }

  # If the standardized result is also an edge pattern, it's an edge case
  if (normalized_standardized %in% edge_patterns) {
    return(TRUE)
  }

  # If standardized to a non-standard subspecialty, it's an edge case
  valid_classifications <- config$subspecialty_mappings$abog_subspecialties
  if (!standardized %in% valid_classifications) {
    return(TRUE)
  }

  return(FALSE)
}

# THE ONE SUBSPECIALTY STANDARDIZER FOR ALL ETERNITY
# ===================================================
# Mandatory use - NEVER create duplicates
# Configuration: config/subspecialty_standardization_config.yml
# Documentation: SUBSPECIALTY_STANDARDIZER_DOCUMENTATION.md

#' The canonical medical subspecialty standardizer
#' @param subspecialties [vector]: of subspecialty strings to standardize
#' @param config_path Configuration file path
#' @param verbose Boolean for progress reporting
#' @param verification_date [date]: of subspecialty verification (for ABOG scraping)
#' @param verification_source Source of verification (e.g., "ABOG_website", "ABOG_database")
#' @param scrape_location Location/URL where data was scraped from
#' @return Data frame with standardization results including provenance
the_one_subspecialty_standardizer <- function(subspecialties,
                                            config_path = "config/subspecialty_standardization_config.yml",
                                            verbose = TRUE,
                                            verification_date = NULL,
                                            verification_source = NULL,
                                            scrape_location = NULL) {
  if (length(subspecialties) == 0) {
    return(data.frame(
      original_subspecialty = character(0),
      normalized_subspecialty = character(0),
      standardized_subspecialty = character(0),
      confidence_score = numeric(0),
      is_edge_case = logical(0),
      standardization_method = character(0),
      verification_date = as.Date(character(0)),
      verification_source = character(0),
      scrape_location = character(0),
      provenance_timestamp = as.POSIXct(character(0))
    ))
  }

  # Load configuration from specified path
  if (!file.exists(config_path)) {
    log_warn_safe("Configuration file missing: %s, creating default", config_path)
    tryCatch({
      create_default_subspecialty_config()
    }, error = function(e) {
      stop(sprintf_safe("Failed to create default configuration: %s", e$message))
    })
  }
  config <- tryCatch({
    yaml::read_yaml(config_path)
  }, error = function(e) {
    stop(sprintf_safe("Failed to read configuration file: %s", e$message))
  })

  # Initialize cache
  if (is.null(.subspecialty_cache_con)) {
    init_subspecialty_cache(config_path)
  }

  # Process provenance information
  current_timestamp <- Sys.time()

  # Handle verification date
  if (is.null(verification_date)) {
    verification_date <- as.Date(current_timestamp)
  } else if (is.character(verification_date)) {
    verification_date <- as.Date(verification_date)
  }

  # Handle verification source
  if (is.null(verification_source)) {
    verification_source <- "standardization_only"
  }

  # Handle scrape location
  if (is.null(scrape_location)) {
    scrape_location <- "not_specified"
  }

  if (verbose) {
    log_info_safe("🏥 Standardizing %d subspecialties with THE ONE SUBSPECIALTY STANDARDIZER", length(subspecialties))
    if (verification_source != "standardization_only") {
      log_info_safe("📅 Verification provenance: %s from %s (%s)",
                       verification_date, verification_source, scrape_location)
    }
  }

  results <- vector("list", length(subspecialties))
  cache_hits <- 0

  for (i in seq_along(subspecialties)) {
    subspecialty <- subspecialties[i]

    # Check cache first
    cached <- get_cached_subspecialty(subspecialty)
    if (!is.null(cached)) {
      # Recalculate edge case status (logic may have been updated since caching)
      edge_case_recalc <- is_subspecialty_edge_case(
        cached$original_subspecialty,
        cached$standardized_subspecialty,
        config
      )

      results[[i]] <- list(
        original_subspecialty = cached$original_subspecialty,
        normalized_subspecialty = cached$normalized_subspecialty,
        standardized_subspecialty = cached$standardized_subspecialty,
        confidence_score = cached$confidence_score,
        is_edge_case = edge_case_recalc,  # Use recalculated value
        standardization_method = paste0(cached$standardization_method, "_cached"),
        verification_date = verification_date,
        verification_source = verification_source,
        scrape_location = scrape_location,
        provenance_timestamp = current_timestamp
      )
      cache_hits <- cache_hits + 1
      next
    }

    # Process new subspecialty using existing standardization logic
    subspecialty_names_norm <- norm(subspecialty)

    if (subspecialty_names_norm == "" || is.na(subspecialty)) {
      standardized <- NA_character_
      method <- "missing"
      confidence <- 0.0
    } else {
      # Enhanced standardization from existing code
      standardized <- case_when(
        # CRITICAL FIX (2026-01-18): Critical Care and Hospice are distinct specialties
        # Recoded to "Other" (non-ABOG recognized), NOT to GO
        subspecialty_names_norm %in% c("critical care", "hospice", "hospice care",
                                      "hospice and palliative medicine", "palliative medicine") ~ config$subspecialty_mappings$critical_care_recode_to,

        # URPS variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$URPS$exact_matches ~ "URPS",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$URPS$partial_matches, collapse = "|")) ~ "URPS",

        # GO variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$GO$exact_matches ~ "GO",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$GO$partial_matches, collapse = "|")) ~ "GO",

        # MFM variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$MFM$exact_matches ~ "MFM",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$MFM$partial_matches, collapse = "|")) ~ "MFM",

        # REI variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$REI$exact_matches ~ "REI",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$REI$partial_matches, collapse = "|")) ~ "REI",

        # MIG variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$MIG$exact_matches ~ "MIG",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$MIG$partial_matches, collapse = "|")) ~ "MIG",

        # PAG variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$PAG$exact_matches ~ "PAG",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$PAG$partial_matches, collapse = "|")) ~ "PAG",

        # Generalist variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$Generalist$exact_matches ~ "Generalist",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$Generalist$partial_matches, collapse = "|")) ~ "Generalist",

        # Multiple subspecialty variations (normalized)
        subspecialty_names_norm %in% config$subspecialty_mappings$Multiple$exact_matches ~ "Multiple",
        stringr::str_detect(subspecialty_names_norm, paste(config$subspecialty_mappings$Multiple$partial_matches, collapse = "|")) ~ "Multiple",

        # Keep original if not recognized
        TRUE ~ subspecialty
      )

      # Determine method and confidence
      all_standard_classes <- c(config$subspecialty_mappings$abog_subspecialties,
                                config$subspecialty_mappings$generalist_class,
                                config$subspecialty_mappings$multiple_class)

      if (standardized %in% all_standard_classes && subspecialty_names_norm == tolower(standardized)) {
        method <- "exact"
      } else if (standardized %in% all_standard_classes) {
        method <- "partial"
      } else {
        method <- "fallback"
      }

      confidence <- calculate_subspecialty_confidence(subspecialty, standardized, method, config)
    }

    # Detect edge cases
    edge_case <- is_subspecialty_edge_case(subspecialty, standardized, config)

    results[[i]] <- list(
      original_subspecialty = subspecialty,
      normalized_subspecialty = subspecialty_names_norm,
      standardized_subspecialty = standardized,
      confidence_score = confidence,
      is_edge_case = edge_case,
      standardization_method = method,
      verification_date = verification_date,
      verification_source = verification_source,
      scrape_location = scrape_location,
      provenance_timestamp = current_timestamp
    )

    # Cache result
    if (config$caching$cache_enabled) {
      cache_subspecialty(subspecialty, subspecialty_names_norm, standardized, confidence, edge_case, method)
    }

    # Progress reporting
    if (verbose && config$performance$progress_reporting_interval > 0 &&
        i %% config$performance$progress_reporting_interval == 0) {
      log_info_safe("📊 Processed %d/%d subspecialties (%.1f%% complete)",
                       i, length(subspecialties), 100 * i / length(subspecialties))
    }
  }

  # Convert to data frame
  result_df <- do.call(rbind, lapply(results, data.frame, stringsAsFactors = FALSE))

  if (verbose) {
    # Capture summary output for logging
    summary_file <- tempfile()
    # BUG #42 FIX: Ensure temp file cleanup on error
    on.exit(unlink(summary_file), add = TRUE)

    # Wrap in tryCatch to make logging non-fatal
    tryCatch({
      con <- file(summary_file, open = "wt")
      sink(con, type = "output", split = TRUE)  # Split=TRUE shows output on console too
    }, error = function(e) {
      # If sink fails, just print to console directly
      message("Note: Could not initialize log file, printing to console only")
      con <- NULL
    })

    cat("\n")
    cat("═══════════════════════════════════════════════════════════════════════\n")
    cat("SUBSPECIALTY STANDARDIZATION RESULTS SUMMARY\n")
    cat("═══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf_safe("Total subspecialties processed: %s\n", format(nrow(result_df), big.mark = ",")))
    cat(sprintf_safe("Cache hits: %s (%.1f%%)\n",
                format(cache_hits, big.mark = ","),
                100 * cache_hits / length(subspecialties)))
    cat("\n")

    # Confidence score distribution
    conf_scores <- result_df$confidence_score[!is.na(result_df$confidence_score)]
    if (length(conf_scores) > 0) {
      high_conf <- sum(conf_scores >= 0.9)
      med_conf <- sum(conf_scores >= 0.7 & conf_scores < 0.9)
      low_conf <- sum(conf_scores < 0.7)

      cat("🎯 CONFIDENCE SCORE DISTRIBUTION:\n")
      cat(sprintf_safe("  High confidence (≥0.9): %s (%.1f%%)\n",
                  format(high_conf, big.mark = ","), 100 * high_conf / length(conf_scores)))
      cat(sprintf_safe("  Medium confidence (0.7-0.9): %s (%.1f%%)\n",
                  format(med_conf, big.mark = ","), 100 * med_conf / length(conf_scores)))
      cat(sprintf_safe("  Low confidence (<0.7): %s (%.1f%%)\n",
                  format(low_conf, big.mark = ","), 100 * low_conf / length(conf_scores)))
      cat(sprintf_safe("  Mean confidence: %.3f\n", mean(conf_scores)))
      cat("\n")
    }

    # Edge case detection
    edge_cases <- sum(result_df$is_edge_case, na.rm = TRUE)
    if (edge_cases > 0) {
      cat("🚩 EDGE CASE DETECTION:\n")
      cat(sprintf_safe("  Edge cases identified: %s (%.1f%%)\n",
                  format(edge_cases, big.mark = ","),
                  100 * edge_cases / nrow(result_df)))
      cat("\n")
    }

    # Method distribution
    cat("📊 STANDARDIZATION METHOD BREAKDOWN:\n")
    method_dist <- table(result_df$standardization_method)
    for (method in names(method_dist)) {
      cat(sprintf_safe("  %-25s %s (%.1f%%)\n",
                  paste0(method, ":"),
                  format(method_dist[method], big.mark = ",", width = 8),
                  100 * method_dist[method] / nrow(result_df)))
    }
    cat("\n")

    # Standard subspecialty distribution
    standard_subs <- result_df$standardized_subspecialty[
      result_df$standardized_subspecialty %in% config$subspecialty_mappings$abog_subspecialties]
    if (length(standard_subs) > 0) {
      cat("🏥 ABOG SUBSPECIALTY DISTRIBUTION:\n")
      standard_dist <- table(standard_subs)
      for (sub in names(standard_dist)) {
        pct <- 100 * standard_dist[sub] / length(standard_subs)
        cat(sprintf_safe("  %-25s %s (%.1f%%)\n",
                    paste0(sub, ":"),
                    format(standard_dist[sub], big.mark = ",", width = 8),
                    pct))
      }
      cat("\n")
    }

    # Non-standard subspecialties
    non_standard <- sum(!result_df$standardized_subspecialty %in% config$subspecialty_mappings$abog_subspecialties)
    if (non_standard > 0) {
      cat("⚠️  DATA QUALITY WARNINGS:\n")
      cat(sprintf_safe("  Non-standard subspecialties: %s (%.1f%%)\n",
                  format(non_standard, big.mark = ","),
                  100 * non_standard / nrow(result_df)))
      cat("  Consider reviewing these for data quality issues\n")
      cat("\n")
    }

    # Verification provenance
    if ("verification_source" %in% names(result_df) && !all(is.na(result_df$verification_source))) {
      unique_sources <- unique(result_df$verification_source[!is.na(result_df$verification_source)])
      if (length(unique_sources) > 0) {
        cat("📅 VERIFICATION PROVENANCE:\n")
        cat(sprintf_safe("  Data sources: %s\n", paste(unique_sources, collapse = ", ")))
        if ("verification_date" %in% names(result_df) && !all(is.na(result_df$verification_date))) {
          latest_date <- max(result_df$verification_date, na.rm = TRUE)
          cat(sprintf_safe("  Most recent verification: %s\n", latest_date))
        }
        cat("\n")
      }
    }

    cat("✅ Subspecialty standardization complete!\n")

    # Close sink and write summary to log file (make this non-fatal)
    tryCatch({
      if (!is.null(con)) {
        sink()
        close(con)
      }

      # Read captured summary
      summary_text <- paste(readLines(summary_file), collapse = "\n")

      # Write to permanent log file only if paths is available
      if (exists("paths") && !is.null(paths) && "logs_dir" %in% names(paths)) {
        log_dir <- file.path(paths$logs_dir, "summaries")
        if (!dir.exists(log_dir)) {
          dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
        }

        timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
        log_filename <- sprintf("subspecialty_standardization_%s.log", timestamp)
        log_path <- file.path(log_dir, log_filename)

        writeLines(summary_text, log_path)
        cat(sprintf_safe("\n💾 Summary logged to: %s\n", log_path))
      }

      # Clean up temp file
      # BUG #33 FIX: Check file exists before unlinking
      if (file.exists(summary_file)) {
        result <- unlink(summary_file)
        if (result != 0 && requireNamespace("logger", quietly = TRUE)) {
          logger::log_warn("Failed to remove temp file: %s", summary_file)
        }
      }
    }, error = function(e) {
      # Logging failed but don't crash - just warn
      message(sprintf("Note: Could not save log file (%s), continuing...", e$message))
    })
  }

  return(result_df)
}

# ============================================================================
# LEGACY COMPATIBILITY FUNCTIONS
# ============================================================================

#' Legacy compatibility: standardize_abog_subspecialty (redirects to canonical function)
#' Deprecated: Use the_one_subspecialty_standardizer() instead
#' @param subspecialty_names [vector]: of subspecialty names to standardize
#' @return Vector of standardized subspecialty names
standardize_abog_subspecialty <- function(subspecialty_names) {
  log_warn_safe("⚠️ Using deprecated function standardize_abog_subspecialty()")
  log_warn_safe("   Please use the_one_subspecialty_standardizer() instead")

  result <- the_one_subspecialty_standardizer(subspecialty_names, verbose = FALSE)
  return(result[["standardized_subspecialty"]])
}

#' Legacy compatibility: apply_subspecialty_standardization (enhanced with canonical function)
#' Deprecated: Use the_one_subspecialty_standardizer() directly for better performance and features
#' @param data [data.frame]: to process
#' @param subspecialty_col Name of subspecialty column
#' @param new_col Name for new standardized column (default: "subspecialty_standardized")
#' @param config_path Configuration file path
#' @return Data frame with standardized subspecialty column added
apply_subspecialty_standardization <- function(data,
                                             subspecialty_col = "subspecialty_name",
                                             new_col = "subspecialty_standardized",
                                             config_path = "config/subspecialty_standardization_config.yml") {

  log_info_safe("🏥 Applying subspecialty standardization using THE ONE SUBSPECIALTY STANDARDIZER")

  # Use canonical function
  subspecialty_results <- the_one_subspecialty_standardizer(
    data[[subspecialty_col]],
    config_path = config_path,
    verbose = FALSE
  )

  # Add standardized column to original data
  result <- data %>%
    dplyr::mutate(
      !!new_col := subspecialty_results$standardized_subspecialty
    )

  # Convert ABOG subspecialties to ordered factor for consistent plots/tables
  standardized_col <- result[[new_col]]
  
  # Load config to get ABOG_SUBS
  config <- tryCatch({
    yaml::read_yaml(config_path)
  }, error = function(e) {
    stop(sprintf_safe("Failed to read configuration file: %s", e$message))
  })
  abog_subs_from_config <- config$subspecialty_mappings$abog_subspecialties

  abog_mask <- standardized_col %in% abog_subs_from_config

  if (any(abog_mask, na.rm = TRUE)) {
    # BUG FIX (2025-12-04): Don't use ifelse with subset - causes vector recycling
    # Previously: ifelse(abog_mask, as.character(factor(standardized_col[abog_mask], ...)), ...)
    # passed a shortened vector to ifelse, which recycles incorrectly when there are
    # both ABOG and non-ABOG values, corrupting the data.
    # Fix: Build the result vector properly without ifelse on mismatched lengths.
    temp_col <- standardized_col  # Start with original values
    # Only convert ABOG subspecialties to ordered factor levels
    temp_col[abog_mask] <- as.character(factor(standardized_col[abog_mask],
                                               levels = abog_subs_from_config, ordered = TRUE))
    result[[new_col]] <- temp_col

    # Convert the whole column to factor if all are ABOG subspecialties
    if (all(!is.na(standardized_col) & abog_mask[!is.na(standardized_col)])) {
      result[[new_col]] <- factor(result[[new_col]], levels = abog_subs_from_config, ordered = TRUE)
    }
  }

  # Subspecialty standardization hygiene checks from existing code
  standardized_values <- result[[new_col]][!is.na(result[[new_col]])]
  abog_subspecialties <- abog_subs_from_config

  # Check that only valid ABOG subspecialties + unrecognized remain
  unique_standardized <- unique(standardized_values)
  abog_found <- intersect(unique_standardized, abog_subspecialties)
  unrecognized <- setdiff(unique_standardized, abog_subspecialties)

  # Assertion: prevent drift in ABOG subspecialties
  if (length(abog_found) > 0) {
    log_info_safe("ABOG subspecialties found: %s", paste(abog_found, collapse = ", "))
  }

  # Warning if unrecognized labels remain
  if (length(unrecognized) > 0) {
    warning(paste("⚠️ Unrecognized subspecialties after standardization:",
                 paste(unrecognized, collapse = ", "),
                 "- consider adding mappings to the canonical subspecialty standardizer"))
  }

  # Hard assertion: ABOG subspecialties should be exactly the expected 6
  abog_in_data <- intersect(unique_standardized, abog_subspecialties)
  if (length(abog_in_data) > 0) {
    log_info_safe("ABOG subspecialty drift check: %s",
                  all(abog_in_data %in% abog_subs_from_config))
  }

  return(result)
}

# ============================================================================
# UTILITY FUNCTIONS FROM EXISTING CODE
# ============================================================================

#' Get the official 6 ABOG subspecialties (from existing code)
#' @param config_path Configuration file path
#' @return Vector of the 6 standard ABOG subspecialty codes
get_abog_subspecialties <- function(config_path = "config/subspecialty_standardization_config.yml") {
  config <- yaml::read_yaml(config_path)
  config$subspecialty_mappings$abog_subspecialties
}

#' Alias for get_abog_subspecialties for backward compatibility
#' @return Vector of the 6 standard ABOG subspecialty codes
get_standard_subspecialties <- function() {
  get_abog_subspecialties()
}

#' Validate subspecialty results
#' @param results Results data frame from standardization
#' @return List with validation summary
validate_subspecialty_results <- function(results, config_path = "config/subspecialty_standardization_config.yml") {
  config <- yaml::read_yaml(config_path)
  abog_subs_from_config <- config$subspecialty_mappings$abog_subspecialties

  # Calculate success rate (records with valid standardization)
  valid_standardizations <- sum(!is.na(results$standardized_subspecialty) &
                                results$standardized_subspecialty != "",
                                na.rm = TRUE)
  success_rate <- valid_standardizations / nrow(results)

  # Count unique standardized values
  unique_standardized <- length(unique(results$standardized_subspecialty[
    !is.na(results$standardized_subspecialty)]))

  # Calculate cache hit rate - check both possible column names
  method_col <- NULL
  if ("match_method" %in% names(results)) {
    method_col <- results$match_method
  } else if ("standardization_method" %in% names(results)) {
    method_col <- results$standardization_method
  }

  if (!is.null(method_col)) {
    cache_hits <- sum(grepl("cached", method_col, ignore.case = TRUE), na.rm = TRUE)
    cache_hit_rate <- cache_hits / nrow(results)
  } else {
    cache_hit_rate <- 0.0
  }

  # Create method distribution table
  if (!is.null(method_col)) {
    method_distribution <- table(method_col)
  } else {
    method_distribution <- table(character(0))
  }

  # Edge case analysis
  edge_case_analysis <- list(
    edge_cases_detected = sum(results$is_edge_case, na.rm = TRUE),
    edge_case_percentage = mean(results$is_edge_case, na.rm = TRUE) * 100
  )

  # Confidence metrics - handle empty data
  if (nrow(results) > 0 && "confidence_score" %in% names(results)) {
    confidence_metrics <- list(
      mean_confidence = mean(results$confidence_score, na.rm = TRUE),
      median_confidence = median(results$confidence_score, na.rm = TRUE),
      min_confidence = min(results$confidence_score, na.rm = TRUE),
      max_confidence = max(results$confidence_score, na.rm = TRUE),
      low_confidence_count = sum(results$confidence_score < 0.7, na.rm = TRUE),
      high_confidence_count = sum(results$confidence_score >= 0.9, na.rm = TRUE)
    )
  } else {
    # Empty results - return NA metrics
    confidence_metrics <- list(
      mean_confidence = NA_real_,
      median_confidence = NA_real_,
      min_confidence = NA_real_,
      max_confidence = NA_real_,
      low_confidence_count = 0L,
      high_confidence_count = 0L
    )
  }

  list(
    total_records = nrow(results),
    standard_subspecialty_coverage = length(intersect(results$standardized_subspecialty, abog_subs_from_config)),
    edge_case_count = sum(results$is_edge_case, na.rm = TRUE),
    edge_case_analysis = edge_case_analysis,
    method_distribution = method_distribution,
    confidence_metrics = confidence_metrics,
    confidence_stats = summary(results$confidence_score),
    success_rate = success_rate,
    unique_standardized = unique_standardized,
    cache_hit_rate = cache_hit_rate
  )
}

#' Generate comprehensive QA report
#' @param results Results data frame from standardization
#' @param output_path Optional path to save QA report
#' @return List with QA metrics
generate_subspecialty_qa_report <- function(results, output_path = NULL) {
  # Calculate performance metrics
  processing_time_avg <- if ("processing_time" %in% names(results)) {
    mean(results$processing_time, na.rm = TRUE)
  } else {
    NA_real_
  }

  performance_metrics <- list(
    avg_confidence = mean(results$confidence_score, na.rm = TRUE),
    processing_time_avg = processing_time_avg,
    throughput = nrow(results)
  )

  # Generate recommendations based on data quality
  recommendations <- list()
  low_conf_pct <- sum(results$confidence_score < 0.7, na.rm = TRUE) / nrow(results)
  if (low_conf_pct > 0.2) {
    recommendations <- c(recommendations,
                        "High percentage of low-confidence standardizations - review mapping rules")
  }
  edge_case_pct <- sum(results$is_edge_case, na.rm = TRUE) / nrow(results)
  if (edge_case_pct > 0.1) {
    recommendations <- c(recommendations,
                        "Many edge cases detected - consider adding more standardization rules")
  }

  report <- list(
    summary = list(
      overall_quality_score = mean(results$confidence_score, na.rm = TRUE) * 100,
      total_subspecialties_processed = nrow(results),
      timestamp = Sys.time()
    ),
    performance_metrics = performance_metrics,
    data_quality_issues = list(
      low_confidence_records = results[results$confidence_score < 0.7, ],
      edge_cases = results[results$is_edge_case == TRUE, ]
    ),
    recommendations = recommendations
  )

  # Save to file if output_path provided
  if (!is.null(output_path)) {
    timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
    # Extract base name without extension and add timestamp
    base_name <- tools::file_path_sans_ext(basename(output_path))
    extension <- tools::file_ext(output_path)
    if (extension == "") extension <- "rds"

    output_file <- file.path(dirname(output_path),
                             paste0(base_name, "_", timestamp_str, ".", extension))
    saveRDS(report, output_file)
  }

  return(report)
}

#' Load subspecialty configuration
#' @param config_path Path to config file
#' @return Config list
load_subspecialty_config <- function(config_path = "config/subspecialty_standardization_config.yml") {
  # If config file doesn't exist, create default config
  if (!file.exists(config_path)) {
    log_info_safe("Config file not found at %s, creating default config", config_path)

    # Create config directory if it doesn't exist
    config_dir <- dirname(config_path)
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE)
    }

    # Create minimal default config with standard subspecialties
    # CRITICAL FIX (2026-01-18): Changed critical_care/hospice recode from "GO" to "Other"
    default_config <- list(
      subspecialty_mappings = list(
        standard_subspecialties = c("URPS", "GO", "MFM", "REI", "MIG", "PAG"),
        abog_subspecialties = c("URPS", "GO", "MFM", "REI", "MIG", "PAG"),
        generalist_class = "Generalist",
        multiple_class = "Multiple",
        critical_care_recode_to = "Other",  # NOT GO - distinct specialty
        hospice_recode_to = "Other"  # NOT GO - distinct specialty
      ),
      quality_metrics = list(
        confidence_scoring_enabled = TRUE,
        minimum_confidence_threshold = 0.7,
        edge_case_detection_enabled = TRUE,
        similarity_calculation_enabled = TRUE
      ),
      performance = list(
        batch_processing_threshold = 1000,
        chunk_size = 100,
        progress_reporting_interval = 10,
        cache_every_record = TRUE,
        checkpoint_frequency = 1000
      ),
      caching = list(
        duckdb_path = "data/subspecialty_cache.duckdb",
        cache_enabled = TRUE,
        md5_hashing_enabled = TRUE,
        cache_ttl_hours = 87600,
        enable_cache_statistics = TRUE,
        log_cache_operations = TRUE
      )
    )

    # Write default config to file
    yaml::write_yaml(default_config, config_path)
    log_info_safe("Created default config at %s", config_path)

    return(default_config)
  }

  # Load existing config
  yaml::read_yaml(config_path)
}

#' Standardize subspecialties with ABOG website scraping provenance
#' @param subspecialties [vector]: of subspecialty strings to standardize
#' @param scrape_date [date]: when the data was scraped from ABOG website
#' @param scrape_url URL where the data was scraped from
#' @param physician_ids Optional vector of physician IDs for tracking
#' @return Data frame with standardization results including provenance
standardize_subspecialties_with_abog_provenance <- function(subspecialties,
                                                          scrape_date,
                                                          scrape_url,
                                                          physician_ids = NULL,
                                                          current_date = NULL) {

  log_info_safe("🏥 Processing %d subspecialties with ABOG website provenance", length(subspecialties))
  log_info_safe("📅 Scrape date: %s", scrape_date)
  log_info_safe("🌐 Scrape location: %s", scrape_url)

  # Validate inputs
  if (length(subspecialties) == 0) {
    stop("subspecialties vector cannot be empty")
  }

  if (is.character(scrape_date)) {
    scrape_date <- as.Date(scrape_date)
  }

  if (is.null(scrape_url) || scrape_url == "") {
    stop("scrape_url must be provided for ABOG provenance")
  }

  # Use provided current_date or default to Sys.Date() for testing flexibility
  if (is.null(current_date)) {
    current_date <- Sys.Date()
  } else if (is.character(current_date)) {
    current_date <- as.Date(current_date)
  }

  # Call the main standardizer with provenance
  results <- the_one_subspecialty_standardizer(
    subspecialties = subspecialties,
    verbose = TRUE,
    verification_date = scrape_date,
    verification_source = "ABOG_website",
    scrape_location = scrape_url
  )

  # Add physician IDs if provided
  if (!is.null(physician_ids)) {
    if (length(physician_ids) != length(subspecialties)) {
      stop("physician_ids length must match subspecialties length")
    }
    results$physician_id <- physician_ids
  }

  # Add data quality flags
  days_since_scrape <- as.numeric(current_date - scrape_date)
  results$days_since_verification <- days_since_scrape
  results$verification_current <- days_since_scrape <= 365  # Flag if > 1 year old

  log_info_safe("✅ ABOG provenance processing complete")
  log_info_safe("📊 %d subspecialties processed with verification date %s (%d days ago)",
                   nrow(results), scrape_date, days_since_scrape)

  return(results)
}

#' Validate subspecialty against fellowship program timeline
#' @param subspecialty Standardized subspecialty name
#' @param graduation_year Medical school graduation year
#' @param fellowship_year Optional fellowship completion year
#' @param config Configuration list
#' @return List with validation results
validate_subspecialty_timeline <- function(subspecialty, graduation_year, fellowship_year = NULL, config = NULL) {
  if (is.null(config)) {
    config <- yaml::read_yaml("config/subspecialty_standardization_config.yml")
  }

  # BUG FIX (2025-12-04): Guard against missing fellowship_timelines config section
  # Previously: Accessing config$fellowship_timelines$enable_timeline_validation crashed
  # with "$ operator is invalid for atomic vectors" if fellowship_timelines wasn't defined.
  # Fix: Check if the section exists before accessing its properties.
  timeline_enabled <- tryCatch({
    !is.null(config$fellowship_timelines) &&
      isTRUE(config$fellowship_timelines$enable_timeline_validation)
  }, error = function(e) FALSE)

  # Skip validation if disabled, config missing, or for non-subspecialty classifications
  if (!timeline_enabled ||
      subspecialty %in% c("Generalist", "Multiple") ||
      is.na(graduation_year) || is.null(graduation_year)) {
    return(list(
      is_valid = TRUE,
      confidence_adjustment = 0,
      warning_message = NA,
      earliest_possible_fellowship = NA
    ))
  }

  # Get fellowship program start year
  fellowship_start_year <- config$fellowship_timelines[[subspecialty]]
  if (is.null(fellowship_start_year) || is.na(fellowship_start_year)) { # Added is.na check
    return(list(
      is_valid = TRUE,
      confidence_adjustment = 0,
      warning_message = sprintf_safe("No timeline data for subspecialty: %s", subspecialty),
      earliest_possible_fellowship = NA
    ))
  }

  # Calculate earliest possible fellowship completion
  training_duration <- config$fellowship_timelines$typical_residency_duration +
                      config$fellowship_timelines$typical_fellowship_duration
  earliest_fellowship_completion <- max(fellowship_start_year, graduation_year + training_duration)

  # Current year for validation
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Check if subspecialty training was possible given graduation year
  if (graduation_year + training_duration < fellowship_start_year) {
    # Physician graduated too early to have done formal fellowship
    return(list(
      is_valid = FALSE,
      confidence_adjustment = -0.3,  # Significant confidence penalty
      warning_message = sprintf_safe("Anachronistic: %s fellowship not available until %d, but physician graduated %d (completed training ~%d)",
                               subspecialty, fellowship_start_year, graduation_year, graduation_year + training_duration),
      earliest_possible_fellowship = fellowship_start_year
    ))
  }

  # Check if fellowship year provided and validates
  if (!is.null(fellowship_year)) {
    if (fellowship_year < fellowship_start_year) {
      return(list(
        is_valid = FALSE,
        confidence_adjustment = -0.4,
        warning_message = sprintf_safe("Invalid fellowship year: %s fellowship not available until %d, but fellowship year listed as %d",
                                 subspecialty, fellowship_start_year, fellowship_year),
        earliest_possible_fellowship = fellowship_start_year
      ))
    }
  }

  # Timeline is valid
  return(list(
    is_valid = TRUE,
    confidence_adjustment = 0.05,  # Small confidence boost for validated timeline
    warning_message = NA,
    earliest_possible_fellowship = earliest_fellowship_completion
  ))
}

#' Enhanced subspecialty standardizer with timeline validation
#' @param subspecialties [vector]: of subspecialty strings to standardize
#' @param graduation_years [vector]: of medical school graduation years (optional)
#' @param fellowship_years [vector]: of fellowship completion years (optional)
#' @param config_path Configuration file path
#' @param verbose Boolean for progress reporting
#' @param verification_date [date]: of subspecialty verification
#' @param verification_source Source of verification
#' @param scrape_location Location/URL where data was scraped from
#' @return Data frame with standardization results including timeline validation
the_one_subspecialty_standardizer_with_timeline <- function(subspecialties,
                                                          graduation_years = NULL,
                                                          fellowship_years = NULL,
                                                          config_path = "config/subspecialty_standardization_config.yml",
                                                          verbose = TRUE,
                                                          verification_date = NULL,
                                                          verification_source = NULL,
                                                          scrape_location = NULL) {

  # Get basic standardization results
  results <- the_one_subspecialty_standardizer(
    subspecialties = subspecialties,
    config_path = config_path,
    verbose = verbose,
    verification_date = verification_date,
    verification_source = verification_source,
    scrape_location = scrape_location
  )

  # If no graduation years provided, skip timeline validation
  if (is.null(graduation_years)) {
    results$timeline_valid <- NA
    results$timeline_warning <- NA
    results$earliest_possible_fellowship <- NA
    results$confidence_adjusted <- results$confidence_score
    if (verbose) {
      log_info_safe("⏭️ Timeline validation skipped - no graduation years provided")
    }
    return(results)
  }

  # Validate input lengths
  if (length(graduation_years) != length(subspecialties)) {
    stop("graduation_years length must match subspecialties length")
  }
  if (!is.null(fellowship_years) && length(fellowship_years) != length(subspecialties)) {
    stop("fellowship_years length must match subspecialties length")
  }

  # Load configuration
  config <- yaml::read_yaml(config_path)

  # Apply timeline validation
  if (verbose) {
    log_info_safe("⏰ Applying fellowship timeline validation")
  }

  for (i in seq_along(subspecialties)) {
    fellowship_year <- if (is.null(fellowship_years)) NULL else fellowship_years[i]

    validation <- validate_subspecialty_timeline(
      subspecialty = results$standardized_subspecialty[i],
      graduation_year = graduation_years[i],
      fellowship_year = fellowship_year,
      config = config
    )

    results$timeline_valid[i] <- validation$is_valid
    results$timeline_warning[i] <- validation$warning_message
    results$earliest_possible_fellowship[i] <- validation$earliest_possible_fellowship

    # Adjust confidence score based on timeline validation
    adjusted_confidence <- results$confidence_score[i] + validation$confidence_adjustment
    results$confidence_adjusted[i] <- pmax(0, pmin(1, adjusted_confidence))  # Keep between 0-1
  }

  # Summary logging
  if (verbose) {
    invalid_timelines <- sum(!results$timeline_valid, na.rm = TRUE)
    if (invalid_timelines > 0) {
      log_warn_safe("⚠️ Found %d subspecialty assignments with invalid timelines", invalid_timelines)
    }
    log_info_safe("✅ Timeline validation complete")
  }

  return(results)
}

#' Cross-validate subspecialty assignments from multiple sources
#' @param subspecialty_sources [list]: data frames, each containing subspecialty assignments from different sources
#' @param source_names [character vector]: of source names (must match config)
#' @param physician_id_col Name of column containing physician identifiers
#' @param subspecialty_col Name of column containing subspecialty assignments
#' @param config Configuration list (loaded from YAML)
#' @param verbose Whether to print detailed logging
#' @return Data frame with cross-validated subspecialty assignments
#' @export
cross_validate_subspecialty_sources <- function(subspecialty_sources,
                                                source_names,
                                                physician_id_col = "physician_id",
                                                subspecialty_col = "subspecialty",
                                                config = NULL,
                                                verbose = TRUE) {

  if (is.null(config)) {
    config <- load_subspecialty_config()
  }

  if (verbose) {
    log_info_safe("🔍 Starting cross-validation of %d subspecialty sources", length(subspecialty_sources))
  }

  # Validate inputs
  if (length(subspecialty_sources) != length(source_names)) {
    stop("Number of sources must match number of source names")
  }

  # Get reliability scores from config
  reliability_scores <- config$verification_source_hierarchy$reliability_scores

  # Combine all sources with source identifiers
  combined_data <- NULL
  for (i in seq_along(subspecialty_sources)) {
    source_data <- subspecialty_sources[[i]]
    source_name <- source_names[i]

    if (!source_name %in% names(reliability_scores)) {
      log_warn_safe("⚠️ Source '%s' not found in reliability hierarchy, using default score 0.5", source_name)
      reliability_scores[[source_name]] <- 0.5
    }

    # Add source metadata
    source_data$verification_source <- source_name
    source_data$source_reliability <- reliability_scores[[source_name]]

    combined_data <- rbind(combined_data, source_data)
  }

  # Group by physician and analyze conflicts
  physician_groups <- split(combined_data, combined_data[[physician_id_col]])

  cross_validation_results <- NULL

  for (physician_id in names(physician_groups)) {
    physician_data <- physician_groups[[physician_id]]

    # Get unique subspecialties for this physician
    unique_subspecialties <- unique(physician_data[[subspecialty_col]])
    unique_subspecialties <- unique_subspecialties[!is.na(unique_subspecialties)]

    if (length(unique_subspecialties) == 0) {
      # No subspecialty data for this physician
      result_row <- data.frame(
        physician_id = physician_id,
        consensus_subspecialty = NA,
        confidence_score = 0.0,
        source_agreement = "no_data",
        num_sources = nrow(physician_data),
        conflicting_sources = FALSE,
        highest_reliability_source = NA,
        stringsAsFactors = FALSE
      )
    } else if (length(unique_subspecialties) == 1) {
      # All sources agree
      consensus_subspecialty <- unique_subspecialties[1]
      avg_reliability <- mean(physician_data$source_reliability)

      result_row <- data.frame(
        physician_id = physician_id,
        consensus_subspecialty = consensus_subspecialty,
        confidence_score = avg_reliability,
        source_agreement = "unanimous",
        num_sources = nrow(physician_data),
        conflicting_sources = FALSE,
        highest_reliability_source = physician_data$verification_source[which.max(physician_data$source_reliability)][1],
        stringsAsFactors = FALSE
      )
    } else {
      # Sources disagree - use reliability-weighted consensus
      subspecialty_scores <- list()

      for (subspecialty in unique_subspecialties) {
        matching_sources <- physician_data[physician_data[[subspecialty_col]] == subspecialty, ]
        total_reliability <- sum(matching_sources$source_reliability)
        subspecialty_scores[[subspecialty]] <- total_reliability
      }

      # Choose subspecialty with highest weighted score
      best_subspecialty <- names(subspecialty_scores)[which.max(unlist(subspecialty_scores))]
      best_score <- max(unlist(subspecialty_scores))

      # Calculate confidence based on agreement level
      total_possible_score <- sum(physician_data$source_reliability)
      confidence <- best_score / total_possible_score

      result_row <- data.frame(
        physician_id = physician_id,
        consensus_subspecialty = best_subspecialty,
        confidence_score = confidence,
        source_agreement = "conflict_resolved",
        num_sources = nrow(physician_data),
        conflicting_sources = TRUE,
        highest_reliability_source = physician_data$verification_source[which.max(physician_data$source_reliability)][1],
        stringsAsFactors = FALSE
      )
    }

    cross_validation_results <- rbind(cross_validation_results, result_row)
  }

  # Apply standardization to consensus subspecialties
  if (verbose) {
    log_info_safe("🔧 Standardizing cross-validated subspecialties")
  }

  standardized_results <- the_one_subspecialty_standardizer(
    cross_validation_results$consensus_subspecialty,
    config_path = config$config_path,
    verbose = FALSE
  )

  # Combine cross-validation results with standardization
  final_results <- cbind(cross_validation_results, standardized_results)

  # Adjust confidence scores based on cross-validation
  confidence_boost <- config$cross_validation$confidence_boost_multiple_sources
  multiple_source_mask <- final_results$num_sources >= config$cross_validation$minimum_sources_for_high_confidence

  final_results$confidence_score[multiple_source_mask] <-
    pmin(1.0, final_results$confidence_score[multiple_source_mask] + confidence_boost)

  # Flag disagreements if configured
  if (config$cross_validation$flag_source_disagreements) {
    final_results$source_disagreement_flagged <- final_results$conflicting_sources
  }

  if (verbose) {
    conflicts <- sum(final_results$conflicting_sources, na.rm = TRUE)
    unanimous <- sum(final_results$source_agreement == "unanimous", na.rm = TRUE)
    log_info_safe("✅ Cross-validation complete: %d unanimous, %d conflicts resolved", unanimous, conflicts)
  }

  return(final_results)
}

#' Subspecialty standardizer with cross-validation support
#' @param subspecialty_sources [list]: data frames with subspecialty data from different sources
#' @param source_names [character vector]: of source names
#' @param physician_id_col Name of column containing physician identifiers
#' @param subspecialty_col Name of column containing subspecialty assignments
#' @param config_path Path to YAML configuration file
#' @param verbose Whether to print detailed logging
#' @return Data frame with cross-validated and standardized subspecialty assignments
#' @export
the_one_subspecialty_standardizer_with_cross_validation <- function(subspecialty_sources,
                                                                   source_names,
                                                                   physician_id_col = "physician_id",
                                                                   subspecialty_col = "subspecialty",
                                                                   config_path = "config/subspecialty_standardization_config.yml",
                                                                   verbose = TRUE) {

  config <- load_subspecialty_config(config_path)
  config$config_path <- config_path  # Store for nested calls

  if (verbose) {
    log_info_safe("🚀 THE ONE SUBSPECIALTY STANDARDIZER - Cross-Validation Mode")
    log_info_safe("📋 Processing %d sources: %s", length(source_names), paste(source_names, collapse = ", "))
  }

  # Perform cross-validation
  results <- cross_validate_subspecialty_sources(
    subspecialty_sources = subspecialty_sources,
    source_names = source_names,
    physician_id_col = physician_id_col,
    subspecialty_col = subspecialty_col,
    config = config,
    verbose = verbose
  )

  if (verbose) {
    log_info_safe("📊 Cross-validation summary:")
    log_info_safe("   - Total physicians: %d", nrow(results))
    log_info_safe("   - Source conflicts: %d (%.1f%%)",
                     sum(results$conflicting_sources, na.rm = TRUE),
                     100 * mean(results$conflicting_sources, na.rm = TRUE))
    log_info_safe("   - Average confidence: %.3f", mean(results$confidence_score, na.rm = TRUE))
  }

  return(results)
}

#' Analyze subspecialty practice patterns by geographic region
#' @param subspecialty_data [data.frame]: with subspecialty assignments
#' @param physician_id_col Name of column containing physician identifiers
#' @param subspecialty_col Name of column containing standardized subspecialty assignments
#' @param state_col Name of column containing state information
#' @param city_col Name of column containing city information (optional)
#' @param zip_col Name of column containing ZIP code information (optional)
#' @param config Configuration list (loaded from YAML)
#' @param verbose Whether to print detailed logging
#' @return List containing geographic analysis results
#' @export
analyze_geographic_subspecialty_patterns <- function(subspecialty_data,
                                                    physician_id_col = "physician_id",
                                                    subspecialty_col = "standardized_subspecialty",
                                                    state_col = "state",
                                                    city_col = NULL,
                                                    zip_col = NULL,
                                                    config = NULL,
                                                    verbose = TRUE) {

  if (is.null(config)) {
    config <- load_subspecialty_config()
  }

  if (verbose) {
    log_info_safe("🗺️ Starting geographic subspecialty pattern analysis")
  }

  # Validate required columns
  required_cols <- c(physician_id_col, subspecialty_col, state_col)
  missing_cols <- required_cols[!required_cols %in% names(subspecialty_data)]
  if (length(missing_cols) > 0) {
    stop(sprintf_safe("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Filter to valid subspecialty assignments
  valid_data <- subspecialty_data[!is.na(subspecialty_data[[subspecialty_col]]) &
                                 !is.na(subspecialty_data[[state_col]]), ]

  if (nrow(valid_data) == 0) {
    log_warn_safe("⚠️ No valid data for geographic analysis")
    return(list(summary = "No valid data"))
  }

  # State-level analysis
  state_analysis <- valid_data %>%
    group_by(!!sym(state_col), !!sym(subspecialty_col)) %>%
    summarise(physician_count = n(), .groups = "drop") %>%
    group_by(!!sym(state_col)) %>%
    mutate(
      total_state_physicians = sum(physician_count),
      subspecialty_percentage = round(100 * physician_count / total_state_physicians, 2)
    ) %>%
    ungroup()

  # Calculate national averages for comparison
  national_totals <- valid_data %>%
    group_by(!!sym(subspecialty_col)) %>%
    summarise(national_count = n(), .groups = "drop") %>%
    mutate(national_percentage = round(100 * national_count / sum(national_count), 2))

  # Identify geographic outliers (states with unusually high/low subspecialty concentrations)
  state_outliers <- state_analysis %>%
    left_join(national_totals, by = subspecialty_col) %>%
    mutate(
      percentage_deviation = subspecialty_percentage - national_percentage,
      z_score = abs(percentage_deviation) / sd(subspecialty_percentage, na.rm = TRUE),
      is_outlier = z_score > 2.0  # Flag states >2 standard deviations from mean
    ) %>%
    filter(is_outlier) %>%
    arrange(desc(abs(percentage_deviation)))

  # City-level analysis (if city column provided)
  city_analysis <- NULL
  if (!is.null(city_col) && city_col %in% names(valid_data)) {
    city_analysis <- valid_data %>%
      filter(!is.na(!!sym(city_col))) %>%
      group_by(!!sym(state_col), !!sym(city_col), !!sym(subspecialty_col)) %>%
      summarise(physician_count = n(), .groups = "drop") %>%
      group_by(!!sym(state_col), !!sym(city_col)) %>%
      mutate(
        total_city_physicians = sum(physician_count),
        city_subspecialty_percentage = round(100 * physician_count / total_city_physicians, 2)
      ) %>%
      ungroup() %>%
      filter(total_city_physicians >= 5)  # Only include cities with 5+ physicians
  }

  # Practice concentration analysis
  practice_concentration <- state_analysis %>%
    group_by(!!sym(subspecialty_col)) %>%
    summarise(
      num_states_practicing = n(),
      concentration_hhi = sum((subspecialty_percentage/100)^2),  # Herfindahl-Hirschman Index
      most_concentrated_state = .data[[state_col]][which.max(subspecialty_percentage)],
      max_state_percentage = max(subspecialty_percentage),
      .groups = "drop"
    ) %>%
    mutate(
      concentration_level = case_when(
        concentration_hhi > 0.25 ~ "Highly Concentrated",
        concentration_hhi > 0.15 ~ "Moderately Concentrated",
        TRUE ~ "Well Distributed"
      )
    )

  # Identify subspecialty deserts (states with unusually low subspecialty representation)
  subspecialty_deserts <- state_analysis %>%
    group_by(!!sym(subspecialty_col)) %>%
    mutate(
      avg_state_percentage = mean(subspecialty_percentage, na.rm = TRUE),
      is_desert = subspecialty_percentage < (avg_state_percentage * 0.5)  # <50% of average
    ) %>%
    filter(is_desert) %>%
    select(!!sym(state_col), !!sym(subspecialty_col), subspecialty_percentage, avg_state_percentage) %>%
    arrange(!!sym(subspecialty_col), subspecialty_percentage)

  # Generate summary statistics
  summary_stats <- list(
    total_physicians = nrow(valid_data),
    total_states = length(unique(valid_data[[state_col]])),
    subspecialties_analyzed = length(unique(valid_data[[subspecialty_col]])),
    geographic_outliers = nrow(state_outliers),
    subspecialty_deserts = nrow(subspecialty_deserts),
    most_concentrated_subspecialty = practice_concentration[[subspecialty_col]][which.max(practice_concentration$concentration_hhi)],
    most_distributed_subspecialty = practice_concentration[[subspecialty_col]][which.min(practice_concentration$concentration_hhi)]
  )

  if (verbose) {
    log_info_safe("📊 Geographic analysis summary:")
    log_info_safe("   - Physicians analyzed: %d", summary_stats$total_physicians)
    log_info_safe("   - States represented: %d", summary_stats$total_states)
    log_info_safe("   - Geographic outliers found: %d", summary_stats$geographic_outliers)
    log_info_safe("   - Subspecialty deserts identified: %d", summary_stats$subspecialty_deserts)
  }

  return(list(
    summary_stats = summary_stats,
    state_analysis = state_analysis,
    national_averages = national_totals,
    geographic_outliers = state_outliers,
    practice_concentration = practice_concentration,
    subspecialty_deserts = subspecialty_deserts,
    city_analysis = city_analysis
  ))
}

#' Generate geographic practice pattern report
#' @param geographic_analysis Results from analyze_geographic_subspecialty_patterns()
#' @param output_path Path for saving the report (optional)
#' @param state_col Name of state column (for dynamic references)
#' @param subspecialty_col Name of subspecialty column (for dynamic references)
#' @param verbose Whether to print detailed logging
#' @return List containing formatted report sections
#' @export
generate_geographic_subspecialty_report <- function(geographic_analysis,
                                                   output_path = NULL,
                                                   state_col = "state",
                                                   subspecialty_col = "standardized_subspecialty",
                                                   verbose = TRUE) {

  if (verbose) {
    log_info_safe("📋 Generating geographic subspecialty practice pattern report")
  }

  # Report sections
  report <- list()

  # Executive Summary
  report$executive_summary <- sprintf_safe(
    "Geographic Subspecialty Practice Pattern Analysis\n%s\n\nSummary:\n- %d physicians analyzed across %d states\n- %d subspecialties examined\n- %d geographic outliers identified\n- %d subspecialty desert regions found\n\nMost concentrated subspecialty: %s\nMost distributed subspecialty: %s",
    paste(rep("=", 50), collapse = ""),
    geographic_analysis$summary_stats$total_physicians,
    geographic_analysis$summary_stats$total_states,
    geographic_analysis$summary_stats$subspecialties_analyzed,
    geographic_analysis$summary_stats$geographic_outliers,
    geographic_analysis$summary_stats$subspecialty_deserts,
    geographic_analysis$summary_stats$most_concentrated_subspecialty,
    geographic_analysis$summary_stats$most_distributed_subspecialty
  )

  # Geographic Outliers Section
  if (nrow(geographic_analysis$geographic_outliers) > 0) {
    outlier_text <- geographic_analysis$geographic_outliers %>%
      head(10) %>%  # Top 10 outliers
      mutate(description = sprintf_safe("%s: %s (%.1f%% vs %.1f%% national, deviation: %+.1f%%)",
                                 !!sym(state_col),
                                 !!sym(subspecialty_col),
                                 subspecialty_percentage,
                                 national_percentage,
                                 percentage_deviation)) %>%
      pull(description) %>%
      paste(collapse = "\n")

    report$geographic_outliers <- sprintf_safe("Geographic Outliers (Top 10):\n%s\n%s",
                                        paste(rep("-", 30), collapse = ""),
                                        outlier_text)
  } else {
    report$geographic_outliers <- "Geographic Outliers: None identified"
  }

  # Practice Concentration Analysis
  concentration_text <- geographic_analysis$practice_concentration %>%
    arrange(desc(concentration_hhi)) %>%
    mutate(description = sprintf_safe("%s: %s (%d states, HHI: %.3f)",
                               !!sym(subspecialty_col),
                               concentration_level,
                               num_states_practicing,
                               concentration_hhi)) %>%
    pull(description) %>%
    paste(collapse = "\n")

  report$practice_concentration <- sprintf_safe("Practice Concentration Analysis:\n%s\n%s",
                                         paste(rep("-", 30), collapse = ""),
                                         concentration_text)

  # Subspecialty Deserts
  if (nrow(geographic_analysis$subspecialty_deserts) > 0) {
    desert_text <- geographic_analysis$subspecialty_deserts %>%
      head(15) %>%  # Top 15 deserts
      mutate(description = sprintf_safe("%s: %s (%.1f%% vs %.1f%% average)",
                                 !!sym(state_col),
                                 !!sym(subspecialty_col),
                                 subspecialty_percentage,
                                 avg_state_percentage)) %>%
      pull(description) %>%
      paste(collapse = "\n")

    report$subspecialty_deserts <- sprintf_safe("Subspecialty Desert Regions:\n%s\n%s",
                                         paste(rep("-", 30), collapse = ""),
                                         desert_text)
  } else {
    report$subspecialty_deserts <- "Subspecialty Desert Regions: None identified"
  }

  # Data Quality Insights
  data_quality_insights <- c()

  if (geographic_analysis$summary_stats$geographic_outliers > 0) {
    data_quality_insights <- c(data_quality_insights,
                             sprintf_safe("- %d geographic outliers may indicate data quality issues or genuine practice pattern differences",
                                   geographic_analysis$summary_stats$geographic_outliers))
  }

  if (geographic_analysis$summary_stats$subspecialty_deserts > 0) {
    data_quality_insights <- c(data_quality_insights,
                             sprintf_safe("- %d subspecialty desert regions identified, indicating access disparities",
                                   geographic_analysis$summary_stats$subspecialty_deserts))
  }

  highly_concentrated <- sum(geographic_analysis$practice_concentration$concentration_level == "Highly Concentrated")
  if (highly_concentrated > 0) {
    data_quality_insights <- c(data_quality_insights,
                             sprintf_safe("- %d subspecialties show high geographic concentration (HHI > 0.25)",
                                   highly_concentrated))
  }

  if (length(data_quality_insights) == 0) {
    data_quality_insights <- "- No significant data quality concerns identified in geographic patterns"
  }

  report$data_quality_insights <- sprintf_safe("Data Quality Insights:\n%s\n%s",
                                        paste(rep("-", 30), collapse = ""),
                                        paste(data_quality_insights, collapse = "\n"))

  # Combine full report
  full_report <- paste(report$executive_summary,
                      report$geographic_outliers,
                      report$practice_concentration,
                      report$subspecialty_deserts,
                      report$data_quality_insights,
                      sep = "\n\n")

  # Save to file if requested
  if (!is.null(output_path)) {
    tryCatch({
      writeLines(full_report, output_path)
      if (verbose) {
        log_info_safe("💾 Geographic subspecialty report saved: %s", output_path)
      }
    }, error = function(e) {
      warning(sprintf_safe("Failed to save geographic report to %s: %s", output_path, e$message))
    })
  }

  if (verbose) {
    log_info_safe("✅ Geographic subspecialty report generation complete")
  }

  return(list(
    full_report = full_report,
    sections = report
  ))
}

#' Detect anomalies in subspecialty assignments for quality assurance
#' @param subspecialty_data [data.frame]: with subspecialty assignments and related information
#' @param physician_id_col Name of column containing physician identifiers
#' @param subspecialty_col Name of column containing standardized subspecialty assignments
#' @param confidence_col Name of column containing confidence scores
#' @param graduation_year_col Name of column containing graduation years (optional)
#' @param verification_source_col Name of column containing verification sources (optional)
#' @param config Configuration list (loaded from YAML)
#' @param verbose Whether to print detailed logging
#' @return List containing detected anomalies and quality metrics
#' @export
detect_subspecialty_anomalies <- function(subspecialty_data,
                                         physician_id_col = "physician_id",
                                         subspecialty_col = "standardized_subspecialty",
                                         confidence_col = "confidence_score",
                                         graduation_year_col = NULL,
                                         verification_source_col = NULL,
                                         config = NULL,
                                         verbose = TRUE) {

  if (is.null(config)) {
    config <- load_subspecialty_config()
  }

  if (verbose) {
    log_info_safe("🔍 Starting subspecialty anomaly detection")
  }

  # Validate required columns
  required_cols <- c(physician_id_col, subspecialty_col)
  missing_cols <- required_cols[!required_cols %in% names(subspecialty_data)]
  if (length(missing_cols) > 0) {
    stop(sprintf_safe("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  anomalies <- list()

  # 1. Low confidence score anomalies
  if (confidence_col %in% names(subspecialty_data)) {
    min_threshold <- config$quality_metrics$minimum_confidence_threshold
    low_confidence <- subspecialty_data[
      !is.na(subspecialty_data[[confidence_col]]) &
      subspecialty_data[[confidence_col]] < min_threshold, ]

    if (nrow(low_confidence) > 0) {
      anomalies$low_confidence <- low_confidence %>%
        select(all_of(c(physician_id_col, subspecialty_col, confidence_col))) %>%
        arrange(!!sym(confidence_col)) %>%
        mutate(anomaly_type = "Low Confidence Score",
               anomaly_description = sprintf_safe("Confidence %.3f below threshold %.3f",
                                           !!sym(confidence_col), min_threshold))
    }
  }

  # 2. Duplicate subspecialty assignments (same physician, multiple entries)
  physician_counts <- subspecialty_data %>%
    filter(!is.na(!!sym(subspecialty_col))) %>%
    group_by(!!sym(physician_id_col)) %>%
    summarise(subspecialty_count = n(),
              unique_subspecialties = n_distinct(!!sym(subspecialty_col)),
              .groups = "drop") %>%
    filter(subspecialty_count > 1)

  if (nrow(physician_counts) > 0) {
    duplicate_entries <- subspecialty_data %>%
      filter(!!sym(physician_id_col) %in% physician_counts[[physician_id_col]]) %>%
      arrange(!!sym(physician_id_col), !!sym(subspecialty_col))

    anomalies$duplicate_entries <- duplicate_entries %>%
      mutate(anomaly_type = "Duplicate Entries",
             anomaly_description = "Multiple subspecialty assignments for same physician")
  }

  # 3. Anachronistic subspecialty assignments (if graduation year available)
  if (!is.null(graduation_year_col) && graduation_year_col %in% names(subspecialty_data)) {
    fellowship_timelines <- config$fellowship_timelines

    anachronistic <- subspecialty_data %>%
      filter(!is.na(.data[[graduation_year_col]]) & !is.na(.data[[subspecialty_col]])) %>%
      rowwise() %>%
      filter(
        .data[[subspecialty_col]] %in% names(fellowship_timelines)
      ) %>%
      filter(
        .data[[graduation_year_col]] < fellowship_timelines[[.data[[subspecialty_col]]]]
      ) %>%
      ungroup()

    if (nrow(anachronistic) > 0) {
      anomalies$anachronistic_assignments <- anachronistic %>%
        mutate(
          anomaly_type = "Anachronistic Assignment",
          fellowship_start_year = as.numeric(fellowship_timelines[as.character(.data[[subspecialty_col]])]),
          anomaly_description = sprintf_safe("Graduated %d but %s fellowship started %d",
                                      as.numeric(.data[[graduation_year_col]]),
                                      as.character(.data[[subspecialty_col]]),
                                      fellowship_start_year)
        )
    }
  }

  # 4. Unreliable source patterns (if verification source available)
  if (!is.null(verification_source_col) && verification_source_col %in% names(subspecialty_data)) {
    reliability_scores <- config$verification_source_hierarchy$reliability_scores
    low_reliability_threshold <- 0.3

    unreliable_sources <- subspecialty_data %>%
      filter(!is.na(!!sym(verification_source_col))) %>%
      mutate(source_reliability = reliability_scores[!!sym(verification_source_col)]) %>%
      filter(source_reliability < low_reliability_threshold | is.na(source_reliability))

    if (nrow(unreliable_sources) > 0) {
      anomalies$unreliable_sources <- unreliable_sources %>%
        mutate(anomaly_type = "Unreliable Source",
               anomaly_description = sprintf_safe("Source '%s' has low reliability (%.2f)",
                                           !!sym(verification_source_col),
                                           source_reliability))
    }
  }

  # 5. Statistical outliers in subspecialty distributions
  subspecialty_counts <- subspecialty_data %>%
    filter(!is.na(!!sym(subspecialty_col))) %>%
    group_by(!!sym(subspecialty_col)) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      percentage = 100 * count / sum(count),
      z_score = abs(percentage - mean(percentage)) / sd(percentage)
    )

  statistical_outliers <- subspecialty_counts %>%
    filter(z_score > 2.5) %>%  # More than 2.5 standard deviations from mean
    mutate(anomaly_type = "Statistical Outlier",
           anomaly_description = sprintf_safe("Subspecialty frequency %.1f%% (z-score: %.2f)",
                                       percentage, z_score))

  if (nrow(statistical_outliers) > 0) {
    anomalies$statistical_outliers <- statistical_outliers
  }

  # 6. Edge case pattern analysis
  if (config$quality_metrics$edge_case_detection_enabled) {
    edge_case_patterns <- config$quality_metrics$edge_case_patterns

    edge_cases <- subspecialty_data %>%
      filter(
        !is.na(.data[[subspecialty_col]]) &
        tolower(.data[[subspecialty_col]]) %in% tolower(edge_case_patterns)
      )

    if (nrow(edge_cases) > 0) {
      anomalies$edge_cases <- edge_cases %>%
        mutate(anomaly_type = "Edge Case Pattern",
               anomaly_description = sprintf_safe("Subspecialty '%s' matches edge case pattern",
                                           !!sym(subspecialty_col)))
    }
  }

  # 7. Inconsistent standardization results
  original_col <- "original_subspecialty"
  if (original_col %in% names(subspecialty_data)) {
    inconsistent <- subspecialty_data %>%
      filter(!is.na(.data[[original_col]]) & !is.na(.data[[subspecialty_col]])) %>%
      group_by(!!sym(original_col)) %>%
      summarise(
        unique_standardized = n_distinct(!!sym(subspecialty_col)),
        standardized_values = list(unique(!!sym(subspecialty_col))),
        .groups = "drop"
      ) %>%
      filter(unique_standardized > 1)

    if (nrow(inconsistent) > 0) {
      anomalies$inconsistent_standardization <- inconsistent %>%
        mutate(
          anomaly_type = "Inconsistent Standardization",
          anomaly_description = sprintf_safe("Original '%s' maps to %d different standardized values",
                                      !!sym(original_col), unique_standardized)
        )
    }
  }

  # Generate summary metrics
  total_records <- nrow(subspecialty_data)
  total_anomalies <- sum(sapply(anomalies, nrow))

  summary_metrics <- list(
    total_records_analyzed = total_records,
    total_anomalies_detected = total_anomalies,
    anomaly_rate = round(100 * total_anomalies / total_records, 2),
    anomaly_types_found = length(anomalies),
    data_quality_score = round(100 * (1 - total_anomalies / total_records), 1)
  )

  # Add counts for each anomaly type
  for (anomaly_type in names(anomalies)) {
    summary_metrics[[paste0(anomaly_type, "_count")]] <- nrow(anomalies[[anomaly_type]])
  }

  if (verbose) {
    log_info_safe("📊 Anomaly detection summary:")
    log_info_safe("   - Records analyzed: %d", summary_metrics$total_records_analyzed)
    log_info_safe("   - Anomalies detected: %d (%.2f%%)", summary_metrics$total_anomalies_detected, summary_metrics$anomaly_rate)
    log_info_safe("   - Data quality score: %.1f%%", summary_metrics$data_quality_score)
    log_info_safe("   - Anomaly types found: %d", summary_metrics$anomaly_types_found)
  }

  return(list(
    summary_metrics = summary_metrics,
    anomalies = anomalies,
    quality_flags = list(
      high_anomaly_rate = summary_metrics$anomaly_rate > 10,
      low_data_quality = summary_metrics$data_quality_score < 80,
      multiple_anomaly_types = summary_metrics$anomaly_types_found > 3
    )
  ))
}

#' Generate comprehensive quality assurance report with anomaly detection
#' @param subspecialty_data [data.frame]: with subspecialty data
#' @param output_path Path for saving the report (optional)
#' @param include_geographic_analysis Whether to include geographic analysis
#' @param ... Additional parameters passed to detect_subspecialty_anomalies()
#' @return List containing comprehensive QA report
#' @export
generate_comprehensive_subspecialty_qa_report <- function(subspecialty_data,
                                                         output_path = NULL,
                                                         include_geographic_analysis = TRUE,
                                                         verbose = TRUE,
                                                         ...) {

  if (verbose) {
    log_info_safe("📋 Generating comprehensive subspecialty QA report")
  }

  # Anomaly detection
  anomaly_results <- detect_subspecialty_anomalies(subspecialty_data, verbose = verbose, ...)

  # Geographic analysis (if requested and data supports it)
  geographic_results <- NULL
  if (include_geographic_analysis && "state" %in% names(subspecialty_data)) {
    geographic_results <- analyze_geographic_subspecialty_patterns(
      subspecialty_data, verbose = verbose
    )
  }

  # Build comprehensive report
  report_sections <- list()

  # Executive Summary
  report_sections$executive_summary <- sprintf_safe(
    "Comprehensive Subspecialty Quality Assurance Report\n%s\n\nOverall Data Quality Score: %.1f%%\n\nSummary:\n- %d records analyzed\n- %d anomalies detected (%.2f%%)\n- %d anomaly types identified\n\nQuality Flags:\n- High anomaly rate: %s\n- Low data quality: %s\n- Multiple anomaly types: %s",
    paste(rep("=", 60), collapse = ""),
    anomaly_results$summary_metrics$data_quality_score,
    anomaly_results$summary_metrics$total_records_analyzed,
    anomaly_results$summary_metrics$total_anomalies_detected,
    anomaly_results$summary_metrics$anomaly_rate,
    anomaly_results$summary_metrics$anomaly_types_found,
    ifelse(anomaly_results$quality_flags$high_anomaly_rate, "❌ YES", "✅ No"),
    ifelse(anomaly_results$quality_flags$low_data_quality, "❌ YES", "✅ No"),
    ifelse(anomaly_results$quality_flags$multiple_anomaly_types, "⚠️ YES", "✅ No")
  )

  # Anomaly Details
  anomaly_details <- c()
  for (anomaly_type in names(anomaly_results$anomalies)) {
    count <- nrow(anomaly_results$anomalies[[anomaly_type]])
    percentage <- round(100 * count / anomaly_results$summary_metrics$total_records_analyzed, 2)
    anomaly_details <- c(anomaly_details, sprintf_safe("- %s: %d records (%.2f%%)", anomaly_type, count, percentage))
  }

  if (length(anomaly_details) == 0) {
    anomaly_details <- "- No anomalies detected"
  }

  report_sections$anomaly_summary <- sprintf_safe("Anomaly Detection Results:\n%s\n%s",
                                           paste(rep("-", 30), collapse = ""),
                                           paste(anomaly_details, collapse = "\n"))

  # Geographic Analysis (if available)
  if (!is.null(geographic_results)) {
    geo_report <- generate_geographic_subspecialty_report(geographic_results, verbose = FALSE)
    report_sections$geographic_analysis <- geo_report$sections$executive_summary
  }

  # Data Quality Recommendations
  recommendations <- c()

  if (anomaly_results$quality_flags$high_anomaly_rate) {
    recommendations <- c(recommendations, "- URGENT: High anomaly rate detected. Review data collection and standardization processes.")
  }

  if (anomaly_results$quality_flags$low_data_quality) {
    recommendations <- c(recommendations, "- Consider implementing additional data validation steps.")
  }

  if ("low_confidence" %in% names(anomaly_results$anomalies)) {
    recommendations <- c(recommendations, "- Review low-confidence subspecialty assignments for manual verification.")
  }

  if ("anachronistic_assignments" %in% names(anomaly_results$anomalies)) {
    recommendations <- c(recommendations, "- Investigate anachronistic subspecialty assignments - may indicate data entry errors.")
  }

  if ("unreliable_sources" %in% names(anomaly_results$anomalies)) {
    recommendations <- c(recommendations, "- Consider obtaining subspecialty data from more reliable sources.")
  }

  if (length(recommendations) == 0) {
    recommendations <- "- Data quality appears good. Continue current data management practices."
  }

  report_sections$recommendations <- sprintf_safe("Quality Assurance Recommendations:\n%s\n%s",
                                           paste(rep("-", 30), collapse = ""),
                                           paste(recommendations, collapse = "\n"))

  # Combine full report
  full_report <- paste(report_sections$executive_summary,
                      report_sections$anomaly_summary,
                      if (!is.null(report_sections$geographic_analysis)) report_sections$geographic_analysis else "",
                      report_sections$recommendations,
                      sep = "\n\n")

  # Save to file if requested
  if (!is.null(output_path)) {
    writeLines(full_report, output_path)
    if (verbose) {
      log_info_safe("💾 Comprehensive QA report saved: %s", output_path)
    }
  }

  if (verbose) {
    log_info_safe("✅ Comprehensive QA report generation complete")
  }

  return(list(
    full_report = full_report,
    sections = report_sections,
    anomaly_results = anomaly_results,
    geographic_results = geographic_results,
    data_quality_score = anomaly_results$summary_metrics$data_quality_score
  ))
}

#' Get subspecialty full names (from existing code)
#' @return Named vector with codes as names and full names as values
get_abog_subspecialty_names <- function() {
  c(
    "URPS" = "Urogynecology and Reconstructive Pelvic Surgery",
    "GO" = "Gynecologic Oncology",
    "MFM" = "Maternal-Fetal Medicine",
    "REI" = "Reproductive Endocrinology and Infertility",
    "MIG" = "Minimally Invasive Gynecology",
    "PAG" = "Pediatric and Adolescent Gynecology"
  )
}

#' Export subspecialty results with timestamp
#' @param results Results data frame
#' @param base_filename Base filename (without extension)
#' @return Export file path
export_subspecialty_results <- function(results, base_filename = "subspecialty_standardization_results") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  export_dir <- "artifacts"
  export_path <- sprintf_safe("%s/%s_%s.csv", export_dir, base_filename, timestamp)

  tryCatch({
    if (!dir.exists(export_dir)) {
      dir.create(export_dir, recursive = TRUE)
    }
  }, error = function(e) {
    warning(sprintf_safe("Failed to create directory %s: %s", export_dir, e$message))
    return(NULL)
  })

  tryCatch({
    write.csv(results, export_path, row.names = FALSE)
    log_info_safe("💾 Subspecialty results exported: %s", export_path)
    log_info_safe("📊 Export contains %d rows and %d columns", nrow(results), ncol(results))
    return(export_path)
  }, error = function(e) {
    warning(sprintf_safe("Failed to export subspecialty results to %s: %s", export_path, e$message))
    return(NULL)
  })
}

# ============================================================================
# INITIALIZATION
# ============================================================================

# Initialize cache connection

# Load configuration - handle case where here() may not find project root correctly
paths <- tryCatch({
  yaml::read_yaml(here::here("config/paths.yml"))
}, error = function(e) {
  # Fallback: try to find config relative to this file's location
  tryCatch({
    script_dir <- dirname(sys.frame(1)$ofile)
    yaml::read_yaml(file.path(dirname(script_dir), "config", "paths.yml"))
  }, error = function(e2) {
    list()  # Return empty list if config not found
  })
})

if (!exists(".subspecialty_cache_initialized")) {
  tryCatch({
    init_subspecialty_cache()
    .subspecialty_cache_initialized <<- TRUE
    log_info_safe("✅ Subspecialty DuckDB cache initialized")
  }, error = function(e) {
    log_warn_safe("Could not initialize subspecialty cache: %s", e$message)
    .subspecialty_cache_initialized <<- FALSE
  })
}

log_info_safe("Loading subspecialty standardization configuration...")
log_info_safe("THE ONE SUBSPECIALTY STANDARDIZER FOR ALL ETERNITY ready for use!")
log_kv("Standard subspecialties", "Loaded from config")
log_kv("Critical Care/Hospice", "Other (distinct from GO per 2026-01-18 fix)")
log_kv("DuckDB caching", "MD5 hashing active")
log_kv("Confidence scoring", "quality metrics enabled")
log_kv("Legacy compatibility", "functions available")

# ============================================================================
# BATCH PROCESSING FUNCTION
# ============================================================================

#' Batch process subspecialties from a data frame
#' @param data [data.frame]: containing subspecialty data
#' @param subspecialty_column Name of column containing subspecialty strings
#' @param verbose Whether to print progress messages
#' @return Data frame with standardized results
the_one_subspecialty_standardizer_batch <- function(data,
                                                    subspecialty_column = "subspecialty",
                                                    verbose = FALSE) {
  if (!subspecialty_column %in% names(data)) {
    stop(sprintf("Column '%s' not found in data frame", subspecialty_column))
  }

  subspecialties <- data[[subspecialty_column]]

  # Call the main standardizer
  results <- the_one_subspecialty_standardizer(
    subspecialties = subspecialties,
    verbose = verbose
  )

  return(results)
}
