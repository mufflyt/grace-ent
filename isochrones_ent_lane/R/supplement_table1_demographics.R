#!/usr/bin/env Rscript
#' @title Data Supplementation Functions for Table 1 Demographics
#'
#' @description
#' Supplements low-coverage demographics (<95%) with alternative data sources.
#' Implements a three-tier supplementation strategy:
#' \enumerate{
#'   \item Primary source: Medicare, CMS Physician Compare
#'   \item Secondary source: Doximity, Healthgrades, State Medicaid
#'   \item Tertiary source: County/state-level aggregate data
#' }
#'
#' @details
#' Functions supplemented:
#' \itemize{
#'   \item Medicare Volume Quartiles (supplement with State Medicaid volume)
#'   \item Language Fields (supplement with Doximity + Healthgrades)
#'   \item Underserved Areas (supplement with county-level MUA if tract-level
#' fails)
#' }
#'
#' @seealso
#' \code{\link{enrich_table1_demographics}} in
#'   \code{R/enrich_table1_demographics.R} for the primary DuckDB-based
#'   enrichment functions that run before supplementation,
#' \code{\link{create_table1_physician_characteristics}} in
#'   \code{manuscript/R/create_table1_physician_characteristics.R} for the
#'   main Table 1 generation script,
#' \code{\link{table1_enrichment_fallbacks}} in
#'   \code{manuscript/R/table1_enrichment_fallbacks.R} for coverage backfill.
#'
#' @author Tyler Muffly & Claude Code
#' @name supplement_table1_demographics-module
NULL

# NOTE (2026-05-19 audit): library(dplyr) is retained here because the
# function bodies use unqualified `filter()`, `mutate()`, `left_join()`,
# `%>%`. Project convention for callr-subprocess files is requireNamespace
# + `pkg::` prefixing, but converting this file would require qualifying
# ~50 call sites. Filed as a follow-up cleanup; for now, library() keeps
# the existing behavior intact. DBI + duckdb are mostly namespaced and
# can be switched to requireNamespace.
suppressPackageStartupMessages({
  library(dplyr)
})
if (!requireNamespace("DBI", quietly = TRUE))
  stop("[supplement_table1_demographics] Required package not installed: DBI", call. = FALSE)
if (!requireNamespace("duckdb", quietly = TRUE))
  stop("[supplement_table1_demographics] Required package not installed: duckdb", call. = FALSE)

if (!exists("safe_divide", mode = "function"))
  source(here::here("R", "safe_divide.R"), local = FALSE)
if (!exists("default_reference_year", mode = "function"))
  source(here::here("R", "utils", "default_reference_year.R"), local = FALSE)

#' Supplement Medicare Volume with State Medicaid Data (Specialty-Agnostic)
#'
#' @description
#' Enriches physician cohort data by supplementing missing Medicare volume
#' information with state Medicaid claims data. Works for any medical specialty
#' where payer mix analysis or patient volume metrics are needed for Table 1.
#'
#' @details
#' This supplementation strategy applies to **all medical specialties**:
#' \itemize{
#'   \item **Surgical specialties**: OB/GYN, ENT, Orthopedics, General Surgery
#'   \item **Medical specialties**: Cardiology, Oncology, Nephrology
#'   \item **Primary care**: Family Medicine, Pediatrics, Internal Medicine
#'   \item **Women's health**: OB/GYN, Maternal-Fetal Medicine, Urogynecology
#'   \item **Pediatric specialties**: All pediatric subspecialties (high
#' Medicaid volume)
#' }
#'
#' **Why Supplement with Medicaid?**
#'
#' Physicians may have low/missing Medicare volume but high Medicaid volume if
#' they:
#' \itemize{
#'   \item Practice in areas with younger populations (e.g., pediatrics, OB/GYN)
#'   \item Serve low-income communities (Medicaid expansion states)
#'   \item Work in community health centers (CHCs) or FQHCs
#'   \item Focus on prenatal care, family planning, or pediatric subspecialties
#'   \item Practice in urban safety-net hospitals
#' }
#'
#' **Supplementation Strategy:**
#' \enumerate{
#'   \item Identify physicians with "No Medicare" or "Unknown" volume
#'   \item Query state Medicaid tables for these NPIs
#'   \item Calculate Medicaid volume quartiles (Q1-Q4) within cohort
#'   \item Replace "Unknown" with Medicaid quartile where available
#'   \item Mark data source (Medicare vs Medicaid) for transparency
#' }
#'
#' **Volume Quartile Categories (Applied to Both Payers):**
#' \itemize{
#'   \item **Q1 (Lowest)** - Bottom 25\% of volume (part-time or limited
#' practice)
#'   \item **Q2** - 25th-50th percentile (moderate volume)
#'   \item **Q3** - 50th-75th percentile (above-average volume)
#'   \item **Q4 (Highest)** - Top 25\% of volume (high-volume practice)
#'   \item **No Medicare/Medicaid** - No claims in either system
#'   \item **Unknown** - Data not available from either source
#' }
#'
#' **Specialty-Specific Supplementation Rates (Expected):**
#' \itemize{
#'   \item Pediatrics: 60-80\% (Medicaid is primary payer for children)
#'   \item OB/GYN: 40-60\% (Medicaid covers ~40\% of births nationally)
#'   \item Family Medicine: 30-50\% (serves diverse payer mix)
#'   \item Cardiology: 10-20\% (primarily Medicare-aged patients)
#'   \item Orthopedics: 5-15\% (primarily commercial/Medicare)
#' }
#'
#' @inheritParams shared_params_cohort
#' @inheritParams shared_params_db_ext
#' @param reference_year `integer`: Year for Medicaid claims query (default:
#'   2022).
#'   Adjust based on data availability - some states lag 1-2 years in reporting.
#'
#' @return The input data frame with supplemented/modified columns:
#'   \describe{
#'     \item{medicare_volume_quartile}{Factor. Updated to include Medicaid
#'       quartiles for physicians with no Medicare data. Original Medicare
#'       quartiles preserved for physicians with Medicare claims.}
#'     \item{volume_data_source}{Character. Indicates data source: "Medicare"
#'       (original), "Medicaid" (supplemented), or "Unknown" (neither
#' available).
#'       Critical for transparent reporting and sensitivity analyses.}
#'   }
#'   Original columns \code{total_medicare_services} and \code{npi} are
#' preserved.
#'
#' @examples
#' \dontrun{
#' # Example 1: OB/GYN cohort (high Medicaid supplementation expected)
#' library(dplyr)
#' otolaryngology_cohort <- read.csv("abog_physicians.csv") %>%
#'   mutate(medicare_volume_quartile = case_when(
#'     medicare_services == 0 ~ "No Medicare",
#'     is.na(medicare_services) ~ "Unknown",
#'     TRUE ~ ntile(medicare_services, 4)
#'   ))
#' otolaryngology_supplemented <- supplement_medicare_volume_with_medicaid(
#'   cohort = otolaryngology_cohort,
#'   duckdb_path = "data/state_medicaid.duckdb",
#'   reference_year = 2022
#' )
#' table(otolaryngology_supplemented$volume_data_source)  # Expect ~40-60%
#' Medicaid
#'
#' # Example 2: Pediatrics (very high Medicaid reliance)
#' peds_cohort <- read.csv("pediatricians.csv")
#' peds_supplemented <- supplement_medicare_volume_with_medicaid(
#'   peds_cohort,
#'   "data/state_medicaid.duckdb"
#' )
#' peds_supplemented %>%
#'   count(volume_data_source) %>%
#'   mutate(pct = 100 * n / sum(n))  # Expect 60-80% Medicaid
#'
#' # Example 3: Multi-specialty payer mix comparison
#' specialties <- bind_rows(
#'   read.csv("otolaryngology.csv") %>% mutate(specialty = "OB/GYN"),
#'   read.csv("peds.csv") %>% mutate(specialty = "Pediatrics"),
#'   read.csv("fammed.csv") %>% mutate(specialty = "Family Medicine"),
#'   read.csv("cardio.csv") %>% mutate(specialty = "Cardiology")
#' )
#' all_supplemented <- supplement_medicare_volume_with_medicaid(
#'   specialties,
#'   "data/state_medicaid.duckdb"
#' )
#' payer_mix <- all_supplemented %>%
#'   count(specialty, volume_data_source) %>%
#'   pivot_wider(names_from = volume_data_source, values_from = n)
#'
#' # Example 4: Safety-net practice identification
#' safety_net <- supplement_medicare_volume_with_medicaid(cohort, db_path) %>%
#'   filter(volume_data_source == "Medicaid",
#'          medicare_volume_quartile %in% c("Q3", "Q4 (Highest)")) %>%
#'   # High-volume Medicaid practitioners = likely safety-net providers
#'   mutate(safety_net_provider = TRUE)
#' }
#'
#' @seealso
#' \code{\link{add_practice_stability}} for practice retention demographics
#' \code{\link{supplement_language_with_doximity_healthgrades}} for language data
#' \code{\link{create_table1_with_age}} for complete Table 1 generation
#' \code{\link{add_medicare_volume_quartiles}} in
#'   \code{R/enrich_table1_demographics.R} for the primary Medicare volume
#'   enrichment, \code{\link{apply_all_supplementation}} for the wrapper
#'   that calls all supplementation functions.
#'
#' @family table1_supplementation
#' @family payer_mix_analysis
#'
#' @note
#' \itemize{
#'   \item Medicaid data availability varies by state (some states don't report
#' NPI-level)
#'   \item Quartiles calculated within cohort, not nationally
#' (specialty-specific ranking)
#'   \item Physicians with BOTH Medicare and Medicaid retain Medicare
#' classification
#'   \item Supplementation particularly valuable for: pediatrics, OB/GYN, family
#' medicine
#'   \item Safety-net identification: high Medicaid volume + low Medicare =
#' CHC/FQHC
#'   \item For Table 1: report \% by data source in footnote for transparency
#'   \item Returns original cohort if database unavailable (pipeline continues)
#'   \item Compatible with HRSA medically underserved area (MUA) analyses
#'   \item Useful for health equity research (Medicaid = proxy for low-income
#' care)
#' }
#'
#' @export
supplement_medicare_volume_with_medicaid <- function(cohort,
                                                      duckdb_path,
                                                      reference_year = default_reference_year()) {

  cat("\n  Supplementing Medicare volume with State Medicaid data...\n")

  # Identify physicians with Unknown/No Medicare coverage
  needs_supplement <- cohort %>%
    filter(medicare_volume_quartile %in% c("No Medicare", "Unknown"))

  if (nrow(needs_supplement) == 0) {
    cat("  No supplementation needed - all physicians have Medicare data\n")
    return(cohort)
  }

  cat(sprintf("  %d physicians need Medicaid supplementation (%.1f%%)\n",
              nrow(needs_supplement),
              100 * nrow(needs_supplement) / max(nrow(cohort), 1L)))

  # Check database exists
  if (is.null(duckdb_path) || !file.exists(duckdb_path)) {
    cat("  ⚠️  Medicaid database not available for supplementation\n")
    return(cohort)
  }

  con <- dbConnect(duckdb(), duckdb_path, read_only = TRUE)
  on.exit(tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL), add = TRUE)

  # Get available state Medicaid tables
  medicaid_tables <- dbGetQuery(con, "
    SELECT table_name
    FROM information_schema.tables
    WHERE table_schema = 'main'
      AND table_name LIKE '%medicaid%'
  ")

  if (nrow(medicaid_tables) == 0) {
    cat("  ⚠️  No Medicaid tables found in database\n")
    return(cohort)
  }

  cat(sprintf("  Found %d state Medicaid tables\n", nrow(medicaid_tables)))

  # S10 fix: select the Medicaid table matching reference_year (or nearest year <= reference_year)
  # rather than always picking [1] (alphabetical first = oldest data).
  # 2026-05-21 (Facility-enrichment sweep 8 F8-batch2-#7): when MULTIPLE
  # snapshots exist for the same year (e.g., 2024_q1, 2024_q4,
  # 2024_q4_import_3), `exact[1L]` picked the alphabetically-first
  # table — usually the oldest within the year. Parse quarter and
  # trailing import suffix so the latest in-year snapshot wins.
  .pick_medicaid_table <- function(tbl_names, ref_yr) {
    .extract_quarter <- function(s) {
      m <- regmatches(s, regexpr("_[Qq]([1-4])\\b", s, perl = TRUE))
      if (length(m) > 0L && nzchar(m)) {
        as.integer(sub("_[Qq]", "", m, perl = TRUE))
      } else 0L
    }
    .extract_suffix <- function(s) {
      m <- regmatches(s, regexpr("_(?:import|imp|v)_?([0-9]+)$", s, perl = TRUE))
      if (length(m) > 0L && nzchar(m)) {
        suppressWarnings(as.integer(regmatches(m, regexpr("[0-9]+$", m))))
      } else 0L
    }
    exact <- grep(as.character(ref_yr), tbl_names, value = TRUE)
    if (length(exact) > 0L) {
      qs <- vapply(exact, .extract_quarter, integer(1L))
      ss <- vapply(exact, .extract_suffix,  integer(1L))
      ss[is.na(ss)] <- 0L
      return(exact[order(-qs, -ss, exact,
                         method = "radix",
                         decreasing = c(FALSE, FALSE, TRUE))][1L])
    }
    .extract_yr <- function(s) {
      d <- regmatches(s, regexpr("[0-9]{4}", s))
      if (length(d) > 0L) as.integer(d) else 0L
    }
    yrs   <- vapply(tbl_names, .extract_yr, integer(1L))
    valid <- yrs <= ref_yr & yrs > 0L
    if (!any(valid)) return(tbl_names[1L])  # last resort: first alphabetical
    tbl_names[which.max(yrs * valid)]
  }
  selected_medicaid_table <- .pick_medicaid_table(medicaid_tables$table_name, reference_year)
  cat(sprintf("  Selected Medicaid table: %s (reference_year=%d)\n",
              selected_medicaid_table, reference_year))

  # Query Medicaid volume for physicians needing supplementation
  npi_list <- paste0("'", needs_supplement$npi, "'", collapse = ", ")

  # Try to get Medicaid claims from state tables
  # FIX 2026-05-21: CAST(npi AS VARCHAR) so the result npi type is stable
  # regardless of how the source Medicaid table stores it (some are
  # BIGINT, some VARCHAR). The cohort $npi is character; without the cast
  # the left_join at L~315 could 0-match on type mismatch.
  medicaid_volume <- tryCatch({
    query <- sprintf("
      SELECT
        CAST(npi AS VARCHAR) AS npi,
        SUM(claim_count) as total_medicaid_claims,
        SUM(beneficiary_count) as total_medicaid_beneficiaries
      FROM %s
      WHERE CAST(npi AS VARCHAR) IN (%s)
      GROUP BY CAST(npi AS VARCHAR)
    ", selected_medicaid_table, npi_list)

    dbGetQuery(con, query)
  }, error = function(e) {
    cat(sprintf("  ⚠️  Error querying Medicaid data: %s\n", e$message))
    data.frame(npi = character(0), total_medicaid_claims = numeric(0))
  })

  if (nrow(medicaid_volume) == 0) {
    cat("  No Medicaid data available for supplementation\n")
    return(cohort)
  }

  # Calculate Medicaid quartiles within cohort
  medicaid_volume <- medicaid_volume %>%
    filter(total_medicaid_claims > 0) %>%
    mutate(
      medicaid_quartile = ntile(total_medicaid_claims, 4),
      medicaid_volume_category = case_when(
        medicaid_quartile == 1 ~ "Q1 (Lowest)",
        medicaid_quartile == 2 ~ "Q2",
        medicaid_quartile == 3 ~ "Q3",
        medicaid_quartile == 4 ~ "Q4 (Highest)",
        TRUE ~ "Unknown"
      )
    )

  # Supplement cohort with Medicaid data
  # FIX 2026-05-21: coerce both sides to character + na_matches='never'
  # so the join is robust to upstream type drift and NA-npi phantom matches.
  cohort_supplemented <- cohort %>%
    mutate(npi = as.character(npi)) %>%
    left_join(
      medicaid_volume %>% mutate(npi = as.character(npi)) %>%
        select(npi, medicaid_volume_category, total_medicaid_claims),
      by = "npi", na_matches = "never"
    ) %>%
    mutate(
      # Use Medicaid quartile for physicians with no Medicare
      medicare_volume_quartile = if_else(
        medicare_volume_quartile %in% c("No Medicare", "Unknown") &
          !is.na(medicaid_volume_category),
        medicaid_volume_category,
        as.character(medicare_volume_quartile)
      ),
      medicare_volume_quartile = factor(
        medicare_volume_quartile,
        levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
      ),
      # Mark as supplemented
      volume_data_source = case_when(
        !is.na(total_medicare_services) & total_medicare_services > 0 ~ "Medicare",
        !is.na(total_medicaid_claims) & total_medicaid_claims > 0 ~ "Medicaid",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(-medicaid_volume_category, -total_medicaid_claims)

  # Report supplementation results
  n_supplemented <- sum(cohort_supplemented$volume_data_source == "Medicaid", na.rm = TRUE)
  pct_supplemented <- round(100 * n_supplemented / max(nrow(needs_supplement), 1L), 1)

  cat(sprintf("  Successfully supplemented %d physicians with Medicaid data (%.1f%%)\n",
              n_supplemented, pct_supplemented))

  # Calculate new coverage rate
  n_with_data <- sum(cohort_supplemented$medicare_volume_quartile != "Unknown", na.rm = TRUE)
  new_coverage <- round(100 * n_with_data / max(nrow(cohort), 1L), 1)

  cat(sprintf("  New coverage: %d physicians (%.1f%%)\n", n_with_data, new_coverage))

  return(cohort_supplemented)
}

#' Supplement Language Fields with Doximity and Healthgrades
#' (Specialty-Agnostic)
#'
#' @description
#' Supplements missing CMS language data using Doximity and Healthgrades
#' physician
#' profiles. Works for all patient-facing specialties where language concordance
#' affects access for limited English proficiency (LEP) populations.
#'
#' @details
#' Applicable to all specialties. Particularly valuable for Primary Care,
#' OB/GYN,
#' Pediatrics, Mental Health where ongoing patient communication is critical.
#'
#' @inheritParams shared_params_cohort
#' @param doximity_path Path to Doximity physician data file.
#' @param healthgrades_path Path to Healthgrades physician data file.
#' @return Data frame with supplemented language_category and language_source
#'   columns.
#'
#' @seealso \code{\link{supplement_medicare_volume_with_medicaid}} for volume
#'   supplementation, \code{\link{supplement_hpsa_with_county_mua}} for
#'   underserved area supplementation, \code{\link{apply_all_supplementation}}
#'   for the wrapper that calls all supplementation functions.
#'
#' @export
supplement_language_with_doximity_healthgrades <- function(cohort,
                                                            doximity_path = NULL,
                                                            healthgrades_path = NULL) {

  cat("\n  Supplementing language fields with Doximity + Healthgrades...\n")

  # Identify physicians with Unknown language
  needs_supplement <- cohort %>%
    filter(language_category == "Unknown")

  if (nrow(needs_supplement) == 0) {
    cat("  No supplementation needed - all physicians have language data\n")
    return(cohort)
  }

  cat(sprintf("  %d physicians need language supplementation (%.1f%%)\n",
              nrow(needs_supplement),
              100 * nrow(needs_supplement) / max(nrow(cohort), 1L)))

  # Try Doximity first
  if (!is.null(doximity_path) && file.exists(doximity_path)) {
    cat("  Reading Doximity data...\n")

    doximity_data <- tryCatch({
      # 2026-05-21 (Facility-enrichment sweep 8 F8-batch2-#6): coerce
      # NPI to character on BOTH sides before %in%. Doximity CSV
      # often returns numeric, but needs_supplement$npi is character;
      # without coercion, `numeric %in% character` returns FALSE for
      # every valid row.
      read.csv(doximity_path, stringsAsFactors = FALSE) %>%
        mutate(npi = as.character(npi)) %>%
        filter(npi %in% as.character(needs_supplement$npi)) %>%
        filter(!is.na(languages_spoken) & languages_spoken != "") %>%
        mutate(
          speaks_spanish = grepl("Spanish|Español", languages_spoken, ignore.case = TRUE),
          n_languages = sapply(strsplit(languages_spoken, ","), length),
          language_category = case_when(
            languages_spoken == "English" ~ "English Only",
            speaks_spanish ~ "Spanish",
            n_languages > 1 ~ "Other Non-English",
            TRUE ~ "Unknown"
          ),
          data_source = "Doximity"
        ) %>%
        select(npi, language_category, speaks_spanish, n_languages, data_source)
    }, error = function(e) {
      cat(sprintf("  ⚠️  Error reading Doximity data: %s\n", e$message))
      data.frame(npi = character(0))
    })

    if (nrow(doximity_data) > 0) {
      cat(sprintf("  Found language data for %d physicians in Doximity\n", nrow(doximity_data)))
    }
  } else {
    doximity_data <- data.frame(npi = character(0))
  }

  # Try Healthgrades if still need more
  still_need_supplement <- needs_supplement %>%
    filter(!npi %in% doximity_data$npi)

  if (nrow(still_need_supplement) > 0 &&
      !is.null(healthgrades_path) &&
      file.exists(healthgrades_path)) {

    cat("  Reading Healthgrades data...\n")

    healthgrades_data <- tryCatch({
      # 2026-05-21 (Facility-enrichment sweep 8 F8-batch2-#6): same
      # type-coercion fix as the Doximity branch above.
      read.csv(healthgrades_path, stringsAsFactors = FALSE) %>%
        mutate(npi = as.character(npi)) %>%
        filter(npi %in% as.character(still_need_supplement$npi)) %>%
        filter(!is.na(languages) & languages != "") %>%
        mutate(
          speaks_spanish = grepl("Spanish|Español", languages, ignore.case = TRUE),
          n_languages = sapply(strsplit(languages, ","), length),
          language_category = case_when(
            languages == "English" ~ "English Only",
            speaks_spanish ~ "Spanish",
            n_languages > 1 ~ "Other Non-English",
            TRUE ~ "Unknown"
          ),
          data_source = "Healthgrades"
        ) %>%
        select(npi, language_category, speaks_spanish, n_languages, data_source)
    }, error = function(e) {
      cat(sprintf("  ⚠️  Error reading Healthgrades data: %s\n", e$message))
      data.frame(npi = character(0))
    })

    if (nrow(healthgrades_data) > 0) {
      cat(sprintf("  Found language data for %d physicians in Healthgrades\n", nrow(healthgrades_data)))
    }
  } else {
    healthgrades_data <- data.frame(npi = character(0))
  }

  # Combine supplemental sources
  # FIX 2026-05-21: bind_rows can produce duplicate NPIs (a physician in
  # BOTH doximity_data and healthgrades_data, or repeated rows in either
  # source). A non-unique right-side NPI in a left_join fans out the
  # cohort and corrupts downstream counts + percentages. Dedup
  # deterministically: prefer Doximity (richer language taxonomy) over
  # Healthgrades, then npi as tiebreaker. Also add na_matches="never" so
  # NA-npi cohort rows can't phantom-match.
  # FIX 2026-05-21 (Round 6): preferring Doximity over Healthgrades
  # unconditionally throws away the useful Healthgrades row when Doximity's
  # language_category is "Unknown". Sort by informativeness FIRST (real
  # category > "Unknown" > NA), then apply source precedence as a
  # secondary tiebreaker.
  supplemental_languages <- bind_rows(doximity_data, healthgrades_data) %>%
    mutate(
      .is_informative = !is.na(language_category) &
                        nzchar(trimws(as.character(language_category))) &
                        toupper(trimws(as.character(language_category))) != "UNKNOWN"
    ) %>%
    arrange(npi,
            dplyr::desc(.is_informative),
            factor(data_source, levels = c("Doximity", "Healthgrades"))) %>%
    distinct(npi, .keep_all = TRUE) %>%
    select(-.is_informative)

  if (nrow(supplemental_languages) == 0) {
    cat("  No supplemental language data available\n")
    return(cohort)
  }

  # Supplement cohort
  cohort_supplemented <- cohort %>%
    left_join(
      supplemental_languages %>% select(npi, supplemental_lang = language_category,
                                        supplemental_spanish = speaks_spanish,
                                        supplemental_n_langs = n_languages,
                                        lang_data_source = data_source),
      by = "npi", na_matches = "never"
    ) %>%
    mutate(
      # Use supplemental data for Unknown entries
      language_category = if_else(
        language_category == "Unknown" & !is.na(supplemental_lang),
        supplemental_lang,
        as.character(language_category)
      ),
      language_category = factor(
        language_category,
        levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
      ),
      speaks_spanish = coalesce(speaks_spanish, supplemental_spanish),
      n_languages = coalesce(n_languages, supplemental_n_langs),
      # Track data source
      language_data_source = case_when(
        !is.na(lang_data_source) ~ lang_data_source,
        language_category != "Unknown" ~ "CMS",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(-supplemental_lang, -supplemental_spanish, -supplemental_n_langs, -lang_data_source)

  # Report results
  n_supplemented <- sum(cohort_supplemented$language_data_source %in% c("Doximity", "Healthgrades"),
                        na.rm = TRUE)
  pct_supplemented <- round(100 * n_supplemented / max(nrow(needs_supplement), 1L), 1)

  cat(sprintf("  Successfully supplemented %d physicians (%.1f%%)\n",
              n_supplemented, pct_supplemented))

  # Calculate new coverage
  n_with_data <- sum(cohort_supplemented$language_category != "Unknown", na.rm = TRUE)
  new_coverage <- round(100 * n_with_data / max(nrow(cohort), 1L), 1)

  cat(sprintf("  New coverage: %d physicians (%.1f%%)\n", n_with_data, new_coverage))

  return(cohort_supplemented)
}

#' Supplement HPSA with County-Level MUA Data (Specialty-Agnostic)
#'
#' @description
#' Supplements missing tract-level HPSA designations with county-level Medically
#' Underserved Area (MUA) data. Works for all specialties serving underserved
#' populations when fine-grained HPSA data unavailable.
#'
#' @details
#' Applicable to all specialties. Provides broader geographic coverage than
#' tract-level HPSA, useful for rural areas and safety-net practice
#' identification.
#'
#' @inheritParams shared_params_cohort
#' @return Data frame with supplemented underserved_area and mua_score columns.
#'
#' @seealso \code{\link{supplement_medicare_volume_with_medicaid}} for volume
#'   supplementation,
#' \code{\link{supplement_language_with_doximity_healthgrades}}
#'   for language supplementation, \code{\link{apply_all_supplementation}} for
#'   the wrapper that calls all supplementation functions.
#'
#' @export
supplement_hpsa_with_county_mua <- function(cohort, duckdb_path) {

  cat("\n  Supplementing tract-level HPSA with county-level MUA...\n")

  # Identify physicians with Unknown underserved status
  needs_supplement <- cohort %>%
    filter(underserved_area == "Unknown")

  if (nrow(needs_supplement) == 0) {
    cat("  No supplementation needed - all physicians have HPSA data\n")
    return(cohort)
  }

  cat(sprintf("  %d physicians need county MUA supplementation (%.1f%%)\n",
              nrow(needs_supplement),
              100 * nrow(needs_supplement) / max(nrow(cohort), 1L)))

  # Check database exists
  if (is.null(duckdb_path) || !file.exists(duckdb_path)) {
    cat("  ⚠️  Database not available for MUA supplementation\n")
    return(cohort)
  }

  con <- dbConnect(duckdb(), duckdb_path, read_only = TRUE)
  on.exit(tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL), add = TRUE)

  # Try to get county-level MUA data
  # Assumption: cohort has county_fips or can be derived from census_tract
  county_mua <- tryCatch({
    # Extract county FIPS from census tract (first 5 digits)
    # This assumes census_tract is in format SSCCCTTTTTT (state-county-tract)
    # FIX 2026-05-21: substr() on a numeric column silently drops leading
    # zeros — county 08001 (CO Denver) becomes "80010" and physicians get
    # routed to the wrong (or nonexistent) county. Normalize to a
    # zero-padded 11-char character GEOID first, then slice the first 5
    # for the county FIPS.
    # FIX 2026-05-21: the prior fix added an nchar() == 11 length check
    # but didn't validate that the chars are DIGIT-ONLY. A value like
    # "ABCDEFG" would be padded to "0000ABCDEFG" (11 chars), pass the
    # length check, and slice to "0000A" — a bogus county_fips that
    # silently joins to nothing. Now also require digit-only after pad.
    # FIX 2026-05-21 (Round 7): the docstring says cohort may already
    # have county_fips OR it can be derived from census_tract, but the
    # prior implementation ignored any existing county_fips entirely.
    # Rows with valid county_fips but missing census_tract failed
    # supplementation unnecessarily. Now: prefer the existing
    # county_fips when present and valid (digit-only, length 5);
    # derive from census_tract only as fallback.
    # FIX 2026-05-21 (Round 3): enrich_geocode_tracts.R now emits two
    # related fields: county_fips (3-char county-within-state) AND
    # county_geoid_5char (5-char state+county GEOID). The downstream MUA
    # table uses 5-char GEOIDs. Prefer county_geoid_5char if present;
    # fall back to county_fips ONLY if it's already 5 chars (legacy
    # cohorts pre-Bug-4.7 fix); otherwise derive from census_tract.
    .has_existing_county <- "county_fips" %in% names(needs_supplement)
    .has_existing_5char  <- "county_geoid_5char" %in% names(needs_supplement)
    npi_counties <- needs_supplement %>%
      mutate(
        # Pre-existing 5-char GEOID (preferred): zero-pad to 5 + digit check
        # STEP49_R10F1_sprintf-pad: sprintf("%05s") uses the %0 flag with %s
        # which is undefined behavior. Use stringr::str_pad() instead.
        .existing_5char_char = if (.has_existing_5char) {
          stringr::str_pad(gsub(" ", "0", as.character(county_geoid_5char), fixed = TRUE),
                           width = 5L, side = "left", pad = "0")
        } else {
          NA_character_
        },
        .existing_5char_valid = if (.has_existing_5char) {
          !is.na(county_geoid_5char) &
            nchar(.existing_5char_char) == 5L &
            grepl("^[0-9]{5}$", .existing_5char_char)
        } else {
          FALSE
        },
        # Pre-existing county_fips (if any): zero-pad to 5 char + digit check
        # Only useful if it ALREADY has the 5-char form (legacy cohorts).
        .existing_county_char = if (.has_existing_county) {
          stringr::str_pad(gsub(" ", "0", as.character(county_fips), fixed = TRUE),
                           width = 5L, side = "left", pad = "0")
        } else {
          NA_character_
        },
        .existing_county_valid = if (.has_existing_county) {
          !is.na(county_fips) &
            nchar(.existing_county_char) == 5L &
            grepl("^[0-9]{5}$", .existing_county_char)
        } else {
          FALSE
        },
        # Derive from census_tract as fallback
        .tract_char = stringr::str_pad(
          gsub(" ", "0", as.character(census_tract), fixed = TRUE),
          width = 11L, side = "left", pad = "0"
        ),
        .tract_valid = !is.na(census_tract) &
                       nchar(.tract_char) == 11L &
                       grepl("^[0-9]{11}$", .tract_char),
        county_fips = dplyr::case_when(
          # FIX 2026-05-21 (Round 3): prefer the 5-char county_geoid_5char
          # emitted by enrich_geocode_tracts.R if present and valid;
          # otherwise fall back to existing county_fips IF it's already
          # 5-char (legacy cohorts); finally derive from census_tract.
          .existing_5char_valid                        ~ .existing_5char_char,
          .existing_county_valid                       ~ .existing_county_char,
          .tract_valid                                 ~ substr(.tract_char, 1, 5),
          TRUE                                         ~ NA_character_
        )
      ) %>%
      filter(!is.na(county_fips)) %>%
      select(npi, county_fips) %>%
      distinct()

    # FIX 2026-05-21: a physician with multiple practice tracts could carry
    # MORE THAN ONE county_fips. `distinct(npi, county_fips)` keeps both
    # rows, and the downstream left_join on npi (line ~629) then fans out
    # the supplemented cohort. Collapse to one county per npi here:
    # prefer the alphabetically-first county_fips as the deterministic
    # tiebreaker. Multi-county physicians are rare; this avoids row
    # inflation while keeping the join 1:1.
    # FIX 2026-05-21 (Round 8): keeping the alphabetically-first
    # county_fips per multi-county NPI was deterministic but arbitrary
    # — a physician with practice locations in 2 counties would be
    # assigned the alphabetically smaller county's MUA status, which
    # is wrong in roughly half of cases. Treat multi-county NPIs as
    # ambiguous and SKIP supplementation for them; log the count so
    # the analyst can manually resolve if needed. Single-county NPIs
    # are unaffected.
    .multi_county_npis <- npi_counties |>
      dplyr::count(npi) |>
      dplyr::filter(n > 1L) |>
      dplyr::pull(npi)
    if (length(.multi_county_npis) > 0L) {
      cat(sprintf(
        "  ⚠ %d physicians have multiple county_fips; SKIPPING county-MUA supplementation for them (ambiguous primary location)\n",
        length(.multi_county_npis)
      ))
    }
    npi_counties <- npi_counties |>
      dplyr::filter(!npi %in% .multi_county_npis)

    if (nrow(npi_counties) == 0) {
      # 2026-05-21 (Facility-enrichment sweep 8 F8-batch2-#5): returning
      # `data.frame(npi = character(0))` drops the caller's cohort
      # entirely when downstream join-back uses inner_join, instead
      # of leaving the cohort unchanged with NA supplements. Return
      # the original cohort so the caller sees explicit "no
      # county supplement" rather than empty output.
      return(cohort)
    }

    county_list <- paste0("'", unique(npi_counties$county_fips), "'", collapse = ", ")

    # Query county MUA designations
    query <- sprintf("
      SELECT
        county_fips,
        mua_status,
        mua_score
      FROM county_mua
      WHERE county_fips IN (%s)
    ", county_list)

    mua_data <- dbGetQuery(con, query)

    # FIX 2026-05-21 (Round 9): normalize + validate mua_data$county_fips
    # to the same 5-char digit-only format used by npi_counties before
    # the join. If the county_mua table stores county_fips as integer
    # (leading zeros lost) or with embedded whitespace, the join
    # silently 0-matches and supplementation looks like it just had
    # no MUA hits, when really the join format was incompatible.
    mua_data <- mua_data %>%
      mutate(
        .mua_cf_char = stringr::str_pad(
          gsub(" ", "0", as.character(county_fips), fixed = TRUE),
          width = 5L, side = "left", pad = "0"
        ),
        county_fips = dplyr::if_else(
          !is.na(county_fips) &
            nchar(.mua_cf_char) == 5L &
            grepl("^[0-9]{5}$", .mua_cf_char),
          .mua_cf_char,
          NA_character_
        )
      ) %>%
      filter(!is.na(county_fips)) %>%
      select(-.mua_cf_char)

    # Join to NPIs
    # FIX 2026-05-22 (Round 13 R3): mua_data join missing both contracts.
    # npi_counties.county_fips is digit-validated upstream (D9-F3 fix)
    # but mua_data.county_fips came straight from the SQL read and may
    # contain NA (counties with no MUA designation). na_matches="never"
    # ensures NA-county_fips physicians can't phantom-match a stray NA
    # row. relationship="many-to-one" because mua_data is keyed by
    # county; a duplicate county would silently fan out the cohort.
    npi_counties %>%
      left_join(mua_data, by = "county_fips",
                na_matches = "never",
                relationship = "many-to-one") %>%
      filter(!is.na(mua_status))

  }, error = function(e) {
    cat(sprintf("  ⚠️  Error querying county MUA data: %s\n", e$message))
    data.frame(npi = character(0))
  })

  if (nrow(county_mua) == 0) {
    cat("  No county MUA data available for supplementation\n")
    return(cohort)
  }

  # Supplement cohort with county MUA data
  # FIX 2026-05-21: provenance was determined by raw column PRESENCE
  # (`!is.na(hpsa_score)` always wins as "Tract-HPSA") rather than by which
  # update actually fired. A row that had hpsa_score populated but
  # underserved_area = "Unknown" (rare but real — hpsa_score can leak in
  # from a stale prior pass) would be labeled "Tract-HPSA" even when the
  # County-MUA branch was what actually replaced "Unknown". Track the
  # update path explicitly via .updated_by_county_mua so provenance
  # reflects the actual winning source.
  cohort_supplemented <- cohort %>%
    left_join(
      county_mua %>% select(npi, county_mua_status = mua_status),
      by = "npi", na_matches = "never"
    ) %>%
    mutate(
      .updated_by_county_mua = underserved_area == "Unknown" & !is.na(county_mua_status),
      # Use county MUA for Unknown tract-level entries
      underserved_area = if_else(
        .updated_by_county_mua,
        county_mua_status,
        as.character(underserved_area)
      ),
      underserved_area = factor(
        underserved_area,
        levels = c("HPSA", "MUA", "Non-Underserved", "Unknown")
      ),
      # Track data source from the update path that actually won
      underserved_data_source = case_when(
        .updated_by_county_mua ~ "County-MUA",
        !is.na(hpsa_score) ~ "Tract-HPSA",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(-county_mua_status, -.updated_by_county_mua)

  # Report results
  n_supplemented <- sum(cohort_supplemented$underserved_data_source == "County-MUA",
                        na.rm = TRUE)
  pct_supplemented <- round(100 * n_supplemented / max(nrow(needs_supplement), 1L), 1)

  cat(sprintf("  Successfully supplemented %d physicians with county MUA (%.1f%%)\n",
              n_supplemented, pct_supplemented))

  # Calculate new coverage
  n_with_data <- sum(cohort_supplemented$underserved_area != "Unknown", na.rm = TRUE)
  new_coverage <- round(100 * n_with_data / max(nrow(cohort), 1L), 1)

  cat(sprintf("  New coverage: %d physicians (%.1f%%)\n", n_with_data, new_coverage))

  return(cohort_supplemented)
}

#' Comprehensive Supplementation Pipeline (Specialty-Agnostic)
#'
#' @description
#' Applies all supplementation functions sequentially to maximize demographic
#' data coverage. Works for all medical specialties by cascading through
#' multiple data sources (Medicare, Medicaid, Doximity, Healthgrades, MUA).
#'
#' @details
#' Applicable to all specialties. Optimizes Table 1 completeness by filling gaps
#' in volume, language, underserved area status across primary/secondary
#' sources.
#'
#' @inheritParams shared_params_cohort
#' @param nber_duckdb_path Path to NBER DuckDB with supplementation tables.
#' @param doximity_path Path to Doximity data (optional)
#' @param healthgrades_path Path to Healthgrades data (optional)
#' @return Data frame with all supplementation applied
#'
#' @seealso \code{\link{supplement_medicare_volume_with_medicaid}} for volume
#'   supplementation,
#' \code{\link{supplement_language_with_doximity_healthgrades}}
#'   for language supplementation, \code{\link{supplement_hpsa_with_county_mua}}
#'   for underserved area supplementation,
#'   \code{\link{enrich_table1_demographics}} in
#'   \code{R/enrich_table1_demographics.R} for the primary enrichment that
#'   runs before supplementation.
#'
#' @export
apply_all_supplementation <- function(cohort,
                                      nber_duckdb_path,
                                      doximity_path = NULL,
                                      healthgrades_path = NULL) {

  cat("\n")
  cat(strrep("=", 80))
  cat("\n  APPLYING DATA SUPPLEMENTATION TO IMPROVE COVERAGE\n")
  cat(strrep("=", 80))
  cat("\n")

  # Calculate baseline coverage
  baseline_coverage <- cohort %>%
    summarise(
      medicare_coverage = sum(medicare_volume_quartile != "Unknown", na.rm = TRUE) / n() * 100,
      language_coverage = sum(language_category != "Unknown", na.rm = TRUE) / n() * 100,
      underserved_coverage = sum(underserved_area != "Unknown", na.rm = TRUE) / n() * 100
    )

  cat("  Baseline Coverage:\n")
  cat(sprintf("    Medicare Volume: %.1f%%\n", baseline_coverage$medicare_coverage))
  cat(sprintf("    Language Fields: %.1f%%\n", baseline_coverage$language_coverage))
  cat(sprintf("    Underserved Area: %.1f%%\n", baseline_coverage$underserved_coverage))

  # Apply supplementation functions
  cohort_supplemented <- cohort

  # 1. Supplement Medicare with Medicaid
  if ("medicare_volume_quartile" %in% names(cohort)) {
    cohort_supplemented <- supplement_medicare_volume_with_medicaid(
      cohort_supplemented,
      nber_duckdb_path
    )
  }

  # 2. Supplement language with Doximity + Healthgrades
  if ("language_category" %in% names(cohort)) {
    cohort_supplemented <- supplement_language_with_doximity_healthgrades(
      cohort_supplemented,
      doximity_path,
      healthgrades_path
    )
  }

  # 3. Supplement HPSA with county MUA
  if ("underserved_area" %in% names(cohort)) {
    cohort_supplemented <- supplement_hpsa_with_county_mua(
      cohort_supplemented,
      nber_duckdb_path
    )
  }

  # Calculate final coverage
  final_coverage <- cohort_supplemented %>%
    summarise(
      medicare_coverage = sum(medicare_volume_quartile != "Unknown", na.rm = TRUE) / n() * 100,
      language_coverage = sum(language_category != "Unknown", na.rm = TRUE) / n() * 100,
      underserved_coverage = sum(underserved_area != "Unknown", na.rm = TRUE) / n() * 100
    )

  cat("\n")
  cat(strrep("=", 80))
  cat("\n  SUPPLEMENTATION COMPLETE\n")
  cat(strrep("=", 80))
  cat("\n")
  cat("  Final Coverage:\n")
  cat(sprintf("    Medicare Volume: %.1f%% (Δ +%.1f%%)\n",
              final_coverage$medicare_coverage,
              final_coverage$medicare_coverage - baseline_coverage$medicare_coverage))
  cat(sprintf("    Language Fields: %.1f%% (Δ +%.1f%%)\n",
              final_coverage$language_coverage,
              final_coverage$language_coverage - baseline_coverage$language_coverage))
  cat(sprintf("    Underserved Area: %.1f%% (Δ +%.1f%%)\n",
              final_coverage$underserved_coverage,
              final_coverage$underserved_coverage - baseline_coverage$underserved_coverage))
  cat("\n")

  return(cohort_supplemented)
}
