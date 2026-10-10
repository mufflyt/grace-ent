#!/usr/bin/env Rscript

# =============================================================================
# TABLE 1 ENRICHMENT FALLBACKS — Non-DuckDB Coverage Improvement
# =============================================================================
# Purpose: Provide enrichment functions that work WITHOUT DuckDB (external drive).
#   Each fallback uses:
#     - Columns already in the canonical CSV (ABOG + NPI match fields)
#     - Flat-file crosswalks shipped with the repo
#     - Cached enrichment artifacts from previous runs
#     - NPPES API enrichment RDS (data/derived/api_enrichment.rds)
#
# Usage:
#   source(here("manuscript", "R", "table1_enrichment_fallbacks.R"))
#   physicians <- enrich_table1_fallbacks(physicians)
#
# Author: Tyler Muffly + Claude
# =============================================================================

library(dplyr)
library(here)
source(here("R", "classify_medical_school_aamc.R"))
source(here("R", "physician_demographic_utils.R"))  # extract_advanced_degrees()

# =============================================================================
# CACHE HELPERS
# =============================================================================

#' Get cache directory path
#' @return Character path to cache directory
#' @keywords internal
.table1_cache_dir <- function() {
  here("manuscript", "data", "table1_enrichment_cache")
}

#' Write a Table 1 enrichment cache artifact
#'
#' Saves an RDS snapshot of the enriched physicians data keyed by a run_id.
#' This allows subsequent runs to reuse expensive enrichment results.
#'
#' @param data [data.frame]: to cache.
#' @param run_id [character]: identifier for this run. Default: current date.
#' @param label Optional label for the cache (e.g., "post_fallback").
#' @return Invisible path to the cache file.
#' @export
write_table1_cache <- function(data,
                                run_id = format(Sys.Date(), "%Y%m%d"),
                                label = "enriched") {
  cache_dir <- .table1_cache_dir()
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  filename <- sprintf("table1_%s_%s.rds", label, run_id)
  path <- file.path(cache_dir, filename)
  saveRDS(data, path)
  message(sprintf("  Cache written: %s (%s rows, %s cols)",
                  basename(path), nrow(data), ncol(data)))
  invisible(path)
}

#' Read the most recent Table 1 enrichment cache
#'
#' Looks for the latest cached RDS in the cache directory matching the label.
#'
#' @param label [character]: label to match. Default "enriched".
#' @return Data frame, or NULL if no cache found.
#' @export
read_table1_cache <- function(label = "enriched") {
  cache_dir <- .table1_cache_dir()
  if (!dir.exists(cache_dir)) return(NULL)

  pattern <- sprintf("table1_%s_.*\\.rds$", label)
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)

  # Most recent by filename (contains date)
  latest <- sort(files, decreasing = TRUE)[1]
  data <- readRDS(latest)
  message(sprintf("  Cache loaded: %s (%s rows, %s cols)",
                  basename(latest), nrow(data), ncol(data)))
  data
}


# =============================================================================
# FALLBACK ENRICHERS
# =============================================================================

#' Fallback: State from Existing Columns
#'
#' Derives two-letter state abbreviation by coalescing available state columns
#' in priority order: \code{state_std} > \code{state} > \code{api_nppes_state}.
#' Normalizes to uppercase 2-character codes. Used when DuckDB NPPES tables
#' are unavailable for direct state lookup.
#'
#' @param physicians [data.frame]: with physician data. Must contain at least one
#'   of: \code{state_std}, \code{state}, \code{api_nppes_state}.
#' @return Data frame with \code{state_std} column populated where possible.
#'   Values are 2-letter uppercase US state/territory abbreviations.
#' @keywords internal
.fallback_state <- function(physicians) {
  before <- sum(!is.na(physicians$state_std) & physicians$state_std != "", na.rm = TRUE)

  # Coalesce from available state columns
  state_cols <- intersect(
    c("state_std", "state", "api_nppes_state"),
    names(physicians)
  )

  if (length(state_cols) > 0) {
    physicians <- physicians %>%
      mutate(
        state_std = coalesce(!!!syms(state_cols))
      )
  }

  # Ensure state_std is a valid 2-letter code

  physicians <- physicians %>%
    mutate(
      state_std = case_when(
        nchar(trimws(state_std)) == 2 ~ toupper(trimws(state_std)),
        TRUE ~ state_std
      )
    )

  after <- sum(!is.na(physicians$state_std) & physicians$state_std != "", na.rm = TRUE)
  if (after > before) {
    message(sprintf("    state: %d -> %d (+%d from fallback sources)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: RUCA Rurality from ZIP Crosswalk (No DuckDB Required)
#'
#' Assigns rurality classification using USDA Rural-Urban Commuting Area (RUCA)
#' codes via flat-file crosswalks. Does NOT require DuckDB or the external drive.
#'
#' Data source priority:
#' \enumerate{
#'   \item \code{data/external/ruca_tract_mapping.csv} — Pre-built ZIP-to-RUCA crosswalk
#'   \item Medicare Part B provider file — Extracts ZIP-RUCA pairs from billing data
#' }
#'
#' RUCA codes are grouped into 3 categories:
#' \itemize{
#'   \item \strong{Urban} (RUCA 1-3): Metropolitan areas
#'   \item \strong{Large Rural} (RUCA 4-6): Micropolitan and large towns
#'   \item \strong{Small/Isolated Rural} (RUCA 7-10): Small towns and isolated areas
#' }
#'
#' @param physicians [data.frame]: with \code{zip_code}, \code{zip5}, or
#'   \code{api_nppes_zip} column. ZIP codes are truncated to 5 digits.
#' @return Data frame with \code{ruca_category} column as an ordered factor
#'   with levels: Urban, Large Rural, Small/Isolated Rural, Unknown.
#' @keywords internal
.fallback_ruca <- function(physicians) {
  # Already has RUCA? count baseline
  if (!"ruca_category" %in% names(physicians)) {
    physicians$ruca_category <- factor("Unknown",
      levels = c("Urban", "Large Rural", "Small/Isolated Rural", "Unknown"))
  }

  before <- sum(physicians$ruca_category != "Unknown", na.rm = TRUE)

  # Try loading flat-file RUCA crosswalk
  ruca_file <- here("data", "external", "ruca_tract_mapping.csv")
  if (!file.exists(ruca_file)) {
    # Try Medicare-derived ZIP crosswalk as alternative
    med_file <- here("data", "medicare_part_b_physician", "raw",
                     "MUP_PHY_R24_P07_V10_D22_Prov.csv")
    if (file.exists(med_file)) {
      message("    Loading Medicare Part B for RUCA crosswalk...")
      med <- readr::read_csv(med_file,
        col_select = c("Rndrng_Prvdr_Zip5", "Rndrng_Prvdr_RUCA"),
        show_col_types = FALSE
      )
      zip_ruca <- med %>%
        filter(!is.na(Rndrng_Prvdr_Zip5), !is.na(Rndrng_Prvdr_RUCA)) %>%
        mutate(zip5 = sprintf("%05d", as.integer(Rndrng_Prvdr_Zip5))) %>%
        group_by(zip5) %>%
        summarise(ruca_code = {
          tab <- table(Rndrng_Prvdr_RUCA)
          if (length(tab) == 0) NA_real_ else as.numeric(names(tab)[which.max(tab)])
        }, .groups = "drop") %>%
        mutate(
          ruca_cat_fb = case_when(
            ruca_code >= 1 & ruca_code < 4 ~ "Urban",
            ruca_code >= 4 & ruca_code < 7 ~ "Large Rural",
            ruca_code >= 7 & ruca_code <= 10 ~ "Small/Isolated Rural",
            TRUE ~ "Unknown"
          )
        )
    } else {
      message("    No RUCA crosswalk or Medicare file found for fallback")
      return(physicians)
    }
  } else {
    ruca_raw <- readr::read_csv(ruca_file, show_col_types = FALSE)
    if ("zip5" %in% names(ruca_raw) && "ruca_category" %in% names(ruca_raw)) {
      zip_ruca <- ruca_raw %>%
        rename(ruca_cat_fb = ruca_category) %>%
        mutate(zip5 = sprintf("%05d", as.integer(gsub("[^0-9]", "", zip5)))) %>%
        distinct(zip5, .keep_all = TRUE)
    } else {
      message("    RUCA crosswalk missing zip5/ruca_category columns")
      return(physicians)
    }
  }

  # Ensure physicians have zip5
  if (!"zip5" %in% names(physicians)) {
    zip_sources <- intersect(c("zip_code", "api_nppes_zip"), names(physicians))
    if (length(zip_sources) > 0) {
      physicians <- physicians %>%
        mutate(zip5 = substr(gsub("[^0-9]", "", coalesce(!!!syms(zip_sources))), 1, 5))
    } else {
      message("    No ZIP column available for RUCA fallback")
      return(physicians)
    }
  }

  # Join
  physicians <- physicians %>%
    left_join(zip_ruca %>% select(zip5, ruca_cat_fb), by = "zip5") %>%
    mutate(
      ruca_category = case_when(
        as.character(ruca_category) != "Unknown" ~ as.character(ruca_category),
        !is.na(ruca_cat_fb) & ruca_cat_fb != "Unknown" ~ ruca_cat_fb,
        TRUE ~ "Unknown"
      ),
      ruca_category = factor(ruca_category,
        levels = c("Urban", "Large Rural", "Small/Isolated Rural", "Unknown"))
    ) %>%
    select(-any_of("ruca_cat_fb"))

  after <- sum(physicians$ruca_category != "Unknown", na.rm = TRUE)
  if (after > before) {
    message(sprintf("    rurality: %d -> %d (+%d from ZIP crosswalk)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: Medical Degree Type (MD/DO) from Credential Parsing
#'
#' Extracts MD or DO designation by regex-matching across all available
#' credential and suffix columns. Supports common abbreviation variants:
#' \code{M.D.}, \code{MD}, \code{D.O.}, \code{DO}, \code{MBBS} (→ MD).
#'
#' Checks columns in order: \code{nppes_credentials}, \code{nppes_suffix},
#' \code{physician_name}, \code{honorific}, \code{suffix}, \code{api_nppes_credential}.
#' Only fills \code{degree_type} where it is currently \code{"Unknown"}.
#'
#' @param physicians [data.frame]: with at least one credential-bearing column.
#' @return Data frame with \code{degree_type} column ("MD", "DO", or "Unknown").
#' @keywords internal
.fallback_degree_type <- function(physicians) {
  if (!"degree_type" %in% names(physicians)) {
    physicians$degree_type <- "Unknown"
  }

  before <- sum(physicians$degree_type != "Unknown" & !is.na(physicians$degree_type), na.rm = TRUE)

  cred_cols <- intersect(
    c("nppes_credentials", "nppes_suffix", "physician_name",
      "honorific", "suffix", "api_nppes_credential"),
    names(physicians)
  )

  if (length(cred_cols) > 0) {
    physicians <- physicians %>%
      mutate(
        .cred_combined = paste(!!!syms(cred_cols), sep = " "),
        .degree_inferred = case_when(
          grepl("\\bD\\.?O\\.?\\b", .cred_combined, ignore.case = TRUE) ~ "DO",
          grepl("\\bM\\.?D\\.?\\b", .cred_combined, ignore.case = TRUE) ~ "MD",
          grepl("\\bMBBS\\b", .cred_combined, ignore.case = TRUE) ~ "MD",
          TRUE ~ NA_character_
        ),
        degree_type = coalesce(
          if_else(degree_type != "Unknown", degree_type, NA_character_),
          .degree_inferred,
          degree_type
        )
      ) %>%
      select(-starts_with(".cred_"), -starts_with(".degree_"))
  }

  after <- sum(physicians$degree_type != "Unknown" & !is.na(physicians$degree_type), na.rm = TRUE)
  if (after > before) {
    message(sprintf("    degree_type: %d -> %d (+%d from credential parsing)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: IMG status from medical school name via AAMC classifier
#'
#' Uses the comprehensive AAMC-based classifier (200+ patterns) from
#' R/classify_medical_school_aamc.R. This properly handles:
#'   - All 40 COCA-accredited DO schools (US-trained, NOT IMG)
#'   - All 17 LCME-accredited Canadian schools (US-trained)
#'   - Puerto Rico schools (US territory = US-trained)
#'   - Caribbean offshore schools (IMG)
#'   - International schools by region (IMG)
#'
#' Degree type (MD/DO) and IMG status are independent:
#'   A DO is US-trained if they attended a US osteopathic school.
#'
#' Maps AAMC categories to img_status values used by the pipeline:
#'   US_MD, US_DO, CAN_MD -> "US-trained"
#'   IMG                  -> "IMG"
#'   UNKNOWN, NA          -> left as-is (no change)
#'
#' @param physicians [data.frame]
#' @return Data frame with img_status populated.
#' @keywords internal
.fallback_img_status <- function(physicians) {
  if (!"img_status" %in% names(physicians)) {
    physicians$img_status <- "Unknown"
  }

  before <- sum(physicians$img_status != "Unknown" & !is.na(physicians$img_status), na.rm = TRUE)

  if ("medical_school_name" %in% names(physicians)) {
    # Use the comprehensive AAMC classifier (200+ patterns, proper priority order)
    physicians <- physicians %>%
      mutate(
        .aamc_cat = classify_medical_school(medical_school_name),
        .img_inferred = case_when(
          .aamc_cat %in% c("US_MD", "US_DO", "CAN_MD") ~ "US-trained",
          .aamc_cat == "IMG" ~ "IMG",
          TRUE ~ NA_character_
        ),
        img_status = coalesce(
          if_else(img_status != "Unknown", img_status, NA_character_),
          .img_inferred,
          img_status
        )
      ) %>%
      select(-starts_with(".aamc_"), -starts_with(".img_"))
  }

  after <- sum(physicians$img_status != "Unknown" & !is.na(physicians$img_status), na.rm = TRUE)
  if (after > before) {
    message(sprintf("    img_status: %d -> %d (+%d from AAMC school classifier)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: Multi-Location Practice from Available Proxies
#'
#' Determines whether a physician practices at multiple locations using
#' whichever data source is available:
#' \enumerate{
#'   \item \code{api_multi_location} — Direct flag from NPPES API enrichment
#'   \item \code{n_practice_locations} — Count of practice sites (>1 = multi)
#'   \item \code{api_nppes_n_addresses} — Number of NPPES-registered addresses
#' }
#'
#' Only fills values where \code{multi_location_practice} is currently
#' \code{"Unknown"} or \code{NA}. Does not overwrite existing classifications.
#'
#' @param physicians [data.frame]: with physician data.
#' @return Data frame with \code{multi_location_practice} column
#'   ("Multi-location", "Single location", or "Unknown").
#' @keywords internal
.fallback_multi_location <- function(physicians) {
  if (!"multi_location_practice" %in% names(physicians)) {
    physicians$multi_location_practice <- "Unknown"
  }

  before <- sum(physicians$multi_location_practice != "Unknown" &
                !is.na(physicians$multi_location_practice), na.rm = TRUE)

  # Apply each fallback source individually (case_when can't reference missing columns)
  # NOTE: api_multi_location from old cache is bogus (nppes_n_addresses is always 2,
  # meaning MAILING + LOCATION types, not practice sites). Only use if data has
  # meaningful variation (not all one value), indicating it was derived from
  # practiceLocations (new cache).
  if ("api_multi_location" %in% names(physicians)) {
    api_vals <- physicians$api_multi_location[!is.na(physicians$api_multi_location)]
    if (length(unique(api_vals)) >= 2) {
      physicians <- physicians %>%
        mutate(
          multi_location_practice = if_else(
            multi_location_practice == "Unknown" | is.na(multi_location_practice),
            coalesce(api_multi_location, "Unknown"),
            multi_location_practice
          )
        )
    }
  }

  if ("n_practice_locations" %in% names(physicians)) {
    physicians <- physicians %>%
      mutate(
        multi_location_practice = if_else(
          (multi_location_practice == "Unknown" | is.na(multi_location_practice)) &
            !is.na(n_practice_locations),
          if_else(n_practice_locations > 1, "Multi-location", "Single location"),
          multi_location_practice
        )
      )
  }

  # Use api_nppes_n_practice_locations (real practice sites) NOT api_nppes_n_addresses
  # (which is always 2: MAILING + LOCATION address types)
  if ("api_nppes_n_practice_locations" %in% names(physicians)) {
    physicians <- physicians %>%
      mutate(
        multi_location_practice = if_else(
          (multi_location_practice == "Unknown" | is.na(multi_location_practice)) &
            !is.na(api_nppes_n_practice_locations),
          if_else(as.integer(api_nppes_n_practice_locations) > 1, "Multi-location", "Single location"),
          multi_location_practice
        )
      )
  }

  after <- sum(physicians$multi_location_practice != "Unknown" &
               !is.na(physicians$multi_location_practice), na.rm = TRUE)
  if (after > before) {
    message(sprintf("    multi_location: %d -> %d (+%d from fallback)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: Advanced Degree Category from Credential Fields
#'
#' Extracts and categorizes non-MD/DO advanced degrees (PhD, MPH, MBA, etc.)
#' from credential columns using \code{extract_advanced_degrees()} from
#' \code{R/physician_demographic_utils.R}.
#'
#' Categories:
#' \itemize{
#'   \item \strong{PhD/Research} — PhD, DSc, DrPH
#'   \item \strong{Public Health/Masters} — MPH, MSPH, MSc
#'   \item \strong{Management/Admin} — MBA, MHA, MMM
#'   \item \strong{Other Advanced Degree} — JD, LLM, MS, MA, etc.
#'   \item \strong{None} — No advanced degrees detected
#' }
#'
#' @param physicians [data.frame]: with credential columns (\code{advanced_degrees},
#'   \code{nppes_credentials}, \code{nppes_suffix}, or \code{physician_name}).
#' @return Data frame with \code{advanced_degree_category} column.
#' @keywords internal
.fallback_advanced_degree_category <- function(physicians) {
  if (!"advanced_degree_category" %in% names(physicians)) {
    physicians$advanced_degree_category <- "None"
  }

  before <- sum(!is.na(physicians$advanced_degree_category) &
                physicians$advanced_degree_category != "None", na.rm = TRUE)

  # Column sources for advanced degrees
  raw_cols <- intersect(
    c("advanced_degrees", "nppes_credentials", "nppes_suffix", "physician_name"),
    names(physicians)
  )

  if (length(raw_cols) > 0) {
    # Combine and extract
    physicians <- physicians %>%
      mutate(
        .combined_creds = paste(!!!syms(raw_cols), sep = " "),
        .raw_degrees = sapply(.combined_creds, extract_advanced_degrees),
        advanced_degree_category = case_when(
          grepl("PHD", .raw_degrees, ignore.case = TRUE) ~ "PhD/Research",
          grepl("MPH|MSPH|MSC", .raw_degrees, ignore.case = TRUE) ~ "Public Health/Masters",
          grepl("MBA|MHA|MMM", .raw_degrees, ignore.case = TRUE) ~ "Management/Admin",
          !is.na(.raw_degrees) & .raw_degrees != "" ~ "Other Advanced Degree",
          TRUE ~ "None"
        )
      ) %>%
      select(-any_of(c(".combined_creds", ".raw_degrees")))
  }

  after <- sum(!is.na(physicians$advanced_degree_category) &
                physicians$advanced_degree_category != "None", na.rm = TRUE)
  if (after > before) {
    message(sprintf("    advanced_degree_category: %d -> %d (+%d categorized)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: Detailed Medical School Tier from AAMC Classifier
#'
#' Classifies each physician's medical school into a granular tier using the
#' AAMC-based classifier (\code{classify_medical_school()} from
#' \code{R/classify_medical_school_aamc.R}), then further splits IMGs into
#' Caribbean offshore vs. other international graduates.
#'
#' Tier assignments:
#' \itemize{
#'   \item \strong{US Allopathic (MD)} — LCME-accredited US MD programs
#'   \item \strong{US Osteopathic (DO)} — COCA-accredited DO programs (all 40)
#'   \item \strong{Canadian} — LCME-accredited Canadian schools (17 programs)
#'   \item \strong{Offshore IMG (Caribbean)} — Ross, St. George's, Saba, AUC, etc.
#'   \item \strong{International IMG} — All other foreign medical graduates
#'   \item \strong{Unknown} — School name missing or not classifiable
#' }
#'
#' Caribbean detection uses a dedicated regex for the major offshore schools:
#' Ross, St. George's (SGU), Saba, AUC, Grenada, Dominica.
#'
#' Only fills \code{med_school_tier} where the current value is \code{"Unknown"}.
#' Existing classifications are preserved.
#'
#' @param physicians [data.frame]: with \code{medical_school_name} column.
#' @return Data frame with \code{med_school_tier} column populated. Values are
#'   one of: "US Allopathic (MD)", "US Osteopathic (DO)", "Canadian",
#'   "Offshore IMG (Caribbean)", "International IMG", "Unknown".
#' @keywords internal
.fallback_med_school_tier <- function(physicians) {
  if (!"med_school_tier" %in% names(physicians)) {
    physicians$med_school_tier <- "Unknown"
  }

  before <- sum(physicians$med_school_tier != "Unknown", na.rm = TRUE)

  if ("medical_school_name" %in% names(physicians)) {
    # Define Caribbean patterns locally to avoid redundant calls if needed
    carib_regex <- "(ROSS|ST\\.? GEORGE|SGU|SABA|AMERICAN UNIVERSITY.*CARIB|AUC.*CARIB|GRENADA|DOMINICA.*MED)"

    physicians <- physicians %>%
      mutate(
        .aamc_cat = classify_medical_school(medical_school_name),
        .is_carib = grepl(carib_regex, medical_school_name, ignore.case = TRUE),
        .tier_inferred = case_when(
          .aamc_cat == "US_MD" ~ "US Allopathic (MD)",
          .aamc_cat == "US_DO" ~ "US Osteopathic (DO)",
          .aamc_cat == "CAN_MD" ~ "Canadian",
          .aamc_cat == "IMG" & .is_carib ~ "Offshore IMG (Caribbean)",
          .aamc_cat == "IMG" ~ "International IMG",
          TRUE ~ "Unknown"
        ),
        med_school_tier = coalesce(
          if_else(med_school_tier != "Unknown", med_school_tier, NA_character_),
          .tier_inferred,
          med_school_tier
        )
      ) %>%
      select(-starts_with(".aamc_"), -starts_with(".is_"), -starts_with(".tier_"))
  }

  after <- sum(physicians$med_school_tier != "Unknown", na.rm = TRUE)
  if (after > before) {
    message(sprintf("    med_school_tier: %d -> %d (+%d classified)",
                    before, after, after - before))
  }
  physicians
}


#' Fallback: Years-to-Retirement Category from Age or Graduation Year
#'
#' Estimates how many years remain until retirement (assumed age 65) and bins
#' physicians into workforce planning categories. Uses direct age when available,
#' otherwise infers age from graduation year (assuming graduation at age 26).
#'
#' Age source priority:
#' \enumerate{
#'   \item \code{age} or \code{final_age} — Direct age columns
#'   \item \code{graduation_year}, \code{grad_year}, or \code{api_enum_year} —
#'     Inferred age = 2024 - grad_year + 26
#' }
#'
#' Retirement categories (years to age 65):
#' \itemize{
#'   \item \strong{Imminent (<0y)} — Already past 65
#'   \item \strong{Near-term (1-5y)} — 1 to 5 years until 65
#'   \item \strong{Mid-term (6-15y)} — 6 to 15 years until 65
#'   \item \strong{Long-term (>15y)} — More than 15 years until 65
#'   \item \strong{Unknown} — Neither age nor graduation year available
#' }
#'
#' Only fills \code{years_to_retirement_cat} where the current value is
#' \code{"Unknown"}. Existing classifications are preserved.
#'
#' @param physicians [data.frame]: with at least one of: \code{age},
#'   \code{final_age}, \code{graduation_year}, \code{grad_year}, or
#'   \code{api_enum_year}.
#' @return Data frame with \code{years_to_retirement_cat} column populated.
#' @keywords internal
.fallback_retirement_cat <- function(physicians) {
  if (!"years_to_retirement_cat" %in% names(physicians)) {
    physicians$years_to_retirement_cat <- "Unknown"
  }

  before <- sum(physicians$years_to_retirement_cat != "Unknown", na.rm = TRUE)

  # Check available age/grad year columns
  age_src <- intersect(c("age", "final_age"), names(physicians))
  grad_src <- intersect(c("graduation_year", "grad_year", "api_enum_year"), names(physicians))

  physicians <- physicians %>%
    mutate(
      .age_val = if (length(age_src) > 0) coalesce(!!!syms(age_src)) else NA_real_,
      .grad_val = if (length(grad_src) > 0) as.integer(coalesce(!!!syms(grad_src))) else NA_integer_,
      # Inferred age from grad year if age missing (grad at 26)
      # Use ANALYSIS_YEAR from config so result is stable across run dates.
      .inferred_age = coalesce(.age_val, getOption("isochrones.analysis_year", 2024L) - .grad_val + 26L),
      .years_to_65 = 65 - .inferred_age,
      .retire_cat = case_when(
        .years_to_65 <= 0 ~ "Imminent (>=65y)",
        .years_to_65 <= 5 ~ "Near-term (1-5y)",
        .years_to_65 <= 15 ~ "Mid-term (6-15y)",
        .years_to_65 > 15 ~ "Long-term (>15y)",
        TRUE ~ "Unknown"
      ),
      years_to_retirement_cat = coalesce(
        if_else(years_to_retirement_cat != "Unknown", years_to_retirement_cat, NA_character_),
        .retire_cat,
        years_to_retirement_cat
      )
    ) %>%
    select(-starts_with("."), -any_of(c(".age_val", ".grad_val", ".inferred_age", ".years_to_65", ".retire_cat")))

  after <- sum(physicians$years_to_retirement_cat != "Unknown", na.rm = TRUE)
  if (after > before) {
    message(sprintf("    retirement_cat: %d -> %d (+%d estimated)",
                    before, after, after - before))
  }
  physicians
}


# =============================================================================
# SCHEMA GUARANTEE
# =============================================================================

#' Ensure All Table 1 Expected Columns Exist (Schema Guarantee)
#'
#' Creates \code{NA}-valued placeholder columns for any Table 1 variable that
#' is missing from the data frame. This prevents downstream \code{gtsummary}
#' and \code{flextable} errors when DuckDB enrichment is unavailable and some
#' variables were never populated.
#'
#' When \code{registry} is \code{NULL}, uses a hard-coded list of 33 expected
#' column names covering all standard Table 1 variables (demographics,
#' practice characteristics, training, certification, geographic). When a
#' registry tibble is provided (from \code{table1_variable_registry()}), uses
#' its \code{var_name} column instead.
#'
#' This function is idempotent: calling it multiple times has no additional
#' effect. Existing columns and their values are never modified.
#'
#' @param physicians [data.frame]: with physician data.
#' @param registry Optional tibble from \code{table1_variable_registry()}
#'   containing a \code{var_name} column. If \code{NULL} (default), uses an
#'   inline list of 33 expected columns.
#' @return Data frame with all expected columns present. Columns that were
#'   already present retain their original values; newly added columns are
#'   \code{NA}.
#' @export
ensure_table1_schema <- function(physicians,
                                  registry = NULL) {
  if (is.null(registry)) {
    # Inline minimal list to avoid circular dependency
    expected_cols <- c(
      "age", "gender", "acog_district", "rurality", "state",
      "years_active", "practice_type", "hospital_affiliated", "hospital_type",
      "practice_size_category", "practice_size_count",
      "years_since_cert", "cert_decade", "moc_active",
      "clinical_intensity", "subspecialty_agreement",
      "img_status", "degree_type", "advanced_degrees", "years_since_grad",
      "practice_stability", "practice_arrangement", "language_category",
      "medicare_volume_quartile", "ownership_status",
      "multi_state_licensed", "n_states_licensed",
      "multi_location_practice", "n_practice_locations",
      "underserved_area", "pct_minority", "pct_dual_eligible", "avg_risk_score"
    )
  } else {
    expected_cols <- registry$var_name
  }

  for (col in expected_cols) {
    if (!col %in% names(physicians)) {
      physicians[[col]] <- NA
    }
  }
  physicians
}


# =============================================================================
# MASTER FALLBACK ENTRY POINT
# =============================================================================

#' Apply All Non-DuckDB Enrichment Fallbacks
#'
#' Master entry point that runs all fallback enrichment functions in sequence.
#' These enrichers work entirely WITHOUT the external DuckDB drive, using only:
#' \itemize{
#'   \item Columns already present in the canonical CSV
#'   \item Flat-file crosswalks shipped with the repository
#'   \item Cached enrichment artifacts from previous pipeline runs
#'   \item NPPES API enrichment RDS (\code{data/derived/api_enrichment.rds})
#' }
#'
#' Execution order (each only fills \code{"Unknown"}/\code{NA} values):
#' \enumerate{
#'   \item \code{.fallback_state} — Coalesce state from \code{state_std}/\code{state}/\code{api_nppes_state}
#'   \item \code{.fallback_ruca} — ZIP-to-RUCA crosswalk for rurality
#'   \item \code{.fallback_degree_type} — MD/DO from credential parsing
#'   \item \code{.fallback_img_status} — IMG classification via AAMC school matcher
#'   \item \code{.fallback_multi_location} — Multi-site practice from NPPES proxies
#'   \item \code{.fallback_advanced_degree_category} — PhD/MPH/MBA from credentials
#'   \item \code{.fallback_med_school_tier} — Granular school tier (US MD, DO, Caribbean, etc.)
#'   \item \code{.fallback_retirement_cat} — Years-to-retirement bins from age/grad year
#' }
#'
#' Before running fallbacks, calls \code{ensure_table1_schema()} to guarantee
#' all expected columns exist. After enrichment, prints a coverage summary
#' and optionally writes a cache RDS for future runs.
#'
#' @param physicians [data.frame]: with physician data (typically from the
#'   canonical ABOG-NPI pipeline output).
#' @param use_cache If \code{TRUE}, attempts to load a previous fallback cache
#'   from \code{manuscript/data/table1_enrichment_cache/} and merge any columns
#'   that are missing from the current data. Default \code{FALSE}.
#' @param write_cache If \code{TRUE}, saves an RDS snapshot after enrichment
#'   to the cache directory for future runs. Default \code{TRUE}.
#' @param verbose If \code{TRUE}, prints progress messages and a final
#'   coverage summary showing count and percentage for each key variable.
#'   Default \code{TRUE}.
#' @return Enriched data frame with improved coverage across all fallback
#'   variables. Original values are never overwritten; only \code{"Unknown"},
#'   \code{NA}, or \code{"None"} values are filled.
#' @export
enrich_table1_fallbacks <- function(physicians,
                                    use_cache = FALSE,
                                    write_cache = TRUE,
                                    verbose = TRUE) {
  if (verbose) {
    message("\n  Table 1 Fallback Enrichment (no DuckDB required)")
    message("  ", strrep("-", 50))
  }

  # Optionally load cached enrichment

  if (use_cache) {
    cached <- read_table1_cache("fallback")
    if (!is.null(cached) && "npi" %in% names(cached)) {
      # Merge cached columns that are missing
      cache_cols <- setdiff(names(cached), names(physicians))
      if (length(cache_cols) > 0) {
        physicians <- physicians %>%
          left_join(cached %>% select(npi, all_of(cache_cols)), by = "npi")
        if (verbose) message(sprintf("    Loaded %d columns from cache", length(cache_cols)))
      }
    }
  }

  # Ensure schema
  physicians <- ensure_table1_schema(physicians)

  # Run each fallback
  physicians <- .fallback_state(physicians)
  physicians <- .fallback_ruca(physicians)
  physicians <- .fallback_degree_type(physicians)
  physicians <- .fallback_img_status(physicians)
  physicians <- .fallback_multi_location(physicians)
  physicians <- .fallback_advanced_degree_category(physicians)
  physicians <- .fallback_med_school_tier(physicians)
  physicians <- .fallback_retirement_cat(physicians)

  # Coverage summary
  if (verbose) {
    n <- nrow(physicians)
    message(sprintf("\n  Coverage Summary (%s physicians):", format(n, big.mark = ",")))
    key_vars <- c("state_std", "ruca_category", "degree_type", "img_status",
                  "multi_location_practice", "gender", "advanced_degree_category",
                  "med_school_tier", "years_to_retirement_cat")
    for (v in key_vars) {
      if (v %in% names(physicians)) {
        col <- physicians[[v]]
        n_info <- if (is.factor(col) || is.character(col)) {
          sum(!is.na(col) & !col %in% c("Unknown", ""), na.rm = TRUE)
        } else {
          sum(!is.na(col))
        }
        message(sprintf("    %-30s: %s / %s (%.1f%%)", v,
                        format(n_info, big.mark = ","),
                        format(n, big.mark = ","),
                        100 * n_info / n))
      }
    }
  }

  # Cache the result
  if (write_cache) {
    tryCatch(
      write_table1_cache(physicians, label = "fallback"),
      error = function(e) message(sprintf("    Cache write failed: %s", e$message))
    )
  }

  physicians
}
