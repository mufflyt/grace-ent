#' @title Augment Accessibility Data with Covariates
#'
#' @description
#' Adds geographic (RUCA) and socioeconomic (ACS) covariates to accessibility
#' data to support disparity analysis and statistical modeling.
#'
#' @family reporting-stats
#' @name augment_accessibility_with_covariates
NULL

#!/usr/bin/env Rscript
# =============================================================================
# AUGMENT ACCESSIBILITY DATA WITH COVARIATES
# =============================================================================
# Purpose: Add geographic and socioeconomic covariates to accessibility data
#
# Author: Tyler Muffly + Claude Code
# Created: 2026-01-29
#
# Augments Walker accessibility data with:
# - RUCA codes (urban/suburban/rural classification)
# - ACS socioeconomic variables (income, education, poverty)
# - Provider density (providers per 100,000 population)
#
# Data Sources:
# - RUCA: USDA Economic Research Service (loaded in DuckDB via load_ruca_to_duckdb.R)
# - ACS: Census Bureau American Community Survey via tidycensus
# - Provider counts: From isochrone data (physician locations by year)
# =============================================================================

# Source dependencies
# --- AUTO-INSERTED 2026-05-22 (yaml::read_yaml -> load_paths sweep) ---
# Ensure load_paths() is in scope. Idempotent: only sources
# R/utils/load_paths.R if load_paths is not already defined upstream
# (e.g. by the orchestrator). load_paths() reads config/paths.yml AND
# applies the MufflySamsung volume-name corrector at
# R/utils/load_paths.R:101. See test-regression-integrate-retirement
# -load-paths-2026-05-22.R for the original bug that motivated this.
if (!base::exists("load_paths", mode = "function")) {
  source(here::here("R", "utils", "load_paths.R"))
}

source(here::here("R", "ruca_classifier.R"))
source(here::here("R", "moe_propagation.R"))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Classify RUCA Codes to Simplified Categories
#'
#' Converts RUCA codes (1.0-10.6) to simplified urban/suburban/rural categories.
#'
#' @inheritParams shared_params_db_ext
#' @inheritParams shared_params_paths
#' @inheritParams shared_params_addr_line1
#' @inheritParams shared_params_cfg
#' @inheritParams shared_params_col_names
#' @param ruca_code `numeric vector`: of RUCA codes
#' @return Factor with levels: c("urban", "suburban", "rural")
#'
#' @details
#' RUCA Classification:
#' - Urban (1.0-3.0): Metropolitan areas
#' - Suburban (4.0-6.0): Micropolitan areas and small towns
#' - Rural (7.0-10.6): Rural areas
#'
#' @examples
#' classify_ruca(c(1.0, 4.0, 7.0, 10.0))
#' # Returns: factor("urban", "suburban", "rural", "rural")
#'
#' @export
classify_ruca <- function(ruca_code) {
  # slice/ruca-classifier-consolidation-2026-06-15: delegate the boundary
  # logic to the canonical classify_ruca_3level() in R/utils/ruca_levels.R.
  # This function's contract is preserved:
  #   - Empty / NULL input returns an empty 3-level factor (no "unknown" level).
  #   - NA / out-of-range codes return factor(NA) (not the "unknown" string).
  #   - Returned levels are c("urban", "suburban", "rural") only.
  # Centralizing the boundary logic eliminates the four-way drift between
  # the classify_ruca* functions. See slice/env-var-truthy-checks-2026-06-15
  # for the prior fractional-code bug in the sibling clustering classifier.
  if (is.null(ruca_code) || length(ruca_code) == 0) {
    return(factor(character(0), levels = c("urban", "suburban", "rural")))
  }
  if (!exists("classify_ruca_3level", mode = "function", inherits = TRUE)) {
    source(here::here("R", "utils", "ruca_levels.R"))
  }
  # Use lowercase + character so we can re-map "unknown" -> NA to preserve
  # this function's original contract (NA factor level instead of an
  # explicit "unknown" level).
  raw <- classify_ruca_3level(ruca_code, case = "lower", as_factor = FALSE)
  raw[raw == "unknown"] <- NA_character_
  factor(raw, levels = c("urban", "suburban", "rural"))
}


#' Load RUCA Codes for Census Tracts
#'
#' Retrieves RUCA codes from DuckDB and returns a mapping from tract GEOID to
#' RUCA category.
#'
#' @return Data frame with columns: tract_geoid (character), ruca_code
#'   (numeric), ruca_category (factor)
#'
#' @export
load_ruca_codes <- function(duckdb_path) {

  cat("Loading RUCA codes from DuckDB...\n")

  if (!file.exists(duckdb_path)) {
    stop(sprintf("DuckDB database not found: %s\n\nRUCA data must be loaded first.\nRun: Rscript R/load_ruca_to_duckdb.R", duckdb_path))
  }

  # Connect to DuckDB
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Check if RUCA tract table exists
  table_exists <- DBI::dbExistsTable(con, "ruca_2020_tract")

  # STEP49_R7F1_double-disconnect: on.exit(DBI::dbDisconnect(...), add = TRUE) at
  # line 88 already guarantees disconnect when the function exits (normally or via
  # stop()). The manual DBI::dbDisconnect() here fires BEFORE on.exit(), causing
  # DuckDB to see two disconnect calls on the same connection handle — the second
  # silently errors and can leave the .duckdb_lock file stale. Remove the manual
  # disconnect; on.exit() handles all exit paths including this stop().
  if (!table_exists) {
    stop("RUCA tract table not found in DuckDB.\n\nRun: Rscript R/load_ruca_to_duckdb.R")
  }

  # Query RUCA tract data
  ruca_data <- DBI::dbGetQuery(con, "
    SELECT
      tract_fips as tract_geoid,
      ruca_primary as ruca_code
    FROM ruca_2020_tract
    WHERE tract_fips IS NOT NULL
      AND ruca_primary IS NOT NULL
  ")

  # Standardize GEOID to 11 characters (state + county + tract)
  # STEP49_R7F2_sprintf-pad: sprintf("%011s", ...) is NOT a zero-padding format
  # for character strings — the "0" flag is only valid for numeric conversions.
  # For strings, sprintf("%011s", "37059123456") produces spaces on the left:
  # "     37059123456" instead of "37059123456". Use stringr::str_pad() with
  # pad="0" to enforce the 11-digit GEOID invariant.
  ruca_data <- ruca_data %>%
    dplyr::mutate(
      tract_geoid = stringr::str_pad(as.character(tract_geoid),
                                     width = 11L, side = "left", pad = "0"),
      ruca_category = classify_ruca(ruca_code)
    )

  cat(sprintf("  Loaded %s RUCA codes for census tracts\n", format(nrow(ruca_data), big.mark = ",")))

  return(ruca_data)
}


#' Fetch Socioeconomic Variables from ACS
#'
#' Retrieves median income, education, and poverty data from the American
#' Community Survey.
#'
#' @inheritParams shared_params_temporal
#' @param geography `character`: Census geography level (default: "tract")
#'
#' @return Data frame with columns:
#'   - GEOID: Census tract identifier
#'   - median_income: Median household income (estimate)
#'   - median_income_moe: Margin of error for median income
#'   - pct_bachelors_plus: Percentage with bachelor's degree or higher
#'   - pct_bachelors_plus_moe: Margin of error for education percentage
#'   - pct_poverty: Percentage below poverty line
#'   - pct_poverty_moe: Margin of error for poverty percentage
#'
#' @details
#' Variables retrieved:
#' - B19013_001: Median household income in the past 12 months
#' - B15003_022 through B15003_025: Bachelor's degree or higher (ages 25+)
#' - B17001_002: Population below poverty line
#'
#' @export
fetch_socioeconomic_variables <- function(year, state = NULL, geography = "tract", cache_dir = NULL) {

  cat(sprintf("Fetching ACS socioeconomic data for year %d...\n", year))

  # Check for Census API key
  census_key <- Sys.getenv("CENSUS_API_KEY")
  if (census_key == "") {
    stop("Census API key not found.\nSet CENSUS_API_KEY environment variable.\nGet a free key at: https://api.census.gov/data/key_signup.html")
  }

  # Define variables to fetch
  acs_vars <- c(
    median_income = "B19013_001",       # Median household income
    total_25plus = "B15003_001",        # Total population 25 years and over
    bachelors = "B15003_022",           # Bachelor's degree
    masters = "B15003_023",             # Master's degree
    professional = "B15003_024",        # Professional degree
    doctorate = "B15003_025",           # Doctorate degree
    poverty_denominator = "B17001_001", # Total population for poverty determination
    poverty_numerator = "B17001_002"    # Population below poverty line
  )

  # Fetch data
  if (is.null(state)) {
    # Fetch all states (slower but comprehensive)
    cat("  Fetching data for all states (this may take several minutes)...\n")
    acs_data <- tidycensus::get_acs(
      geography = geography,
      variables = acs_vars,
      year = year,
      survey = "acs5",
      cache_table = !is.null(cache_dir),
      output = "wide"
    )
  } else {
    # Fetch specific state(s)
    cat(sprintf("  Fetching data for state(s): %s\n", paste(state, collapse = ", ")))
    acs_data <- tidycensus::get_acs(
      geography = geography,
      state = state,
      variables = acs_vars,
      year = year,
      survey = "acs5",
      cache_table = !is.null(cache_dir),
      output = "wide"
    )
  }

  # Calculate derived variables
  socioeconomic_data <- acs_data %>%
    dplyr::mutate(
      # Median income (already a direct variable)
      median_income = median_incomeE,
      median_income_moe = median_incomeM,

      # Education: % with bachelor's degree or higher
      bachelors_plus = bachelorsE + mastersE + professionalE + doctorateE,
      bachelors_plus_moe = tidycensus::moe_sum(
        moe = c(bachelorsM, mastersM, professionalM, doctorateM),
        estimate = c(bachelorsE, mastersE, professionalE, doctorateE)
      ),
      pct_bachelors_plus = ifelse(total_25plusE > 0, 100 * bachelors_plus / total_25plusE, NA_real_),
      pct_bachelors_plus_moe = ifelse(total_25plusE > 0,
        tidycensus::moe_prop(bachelors_plus, total_25plusE, bachelors_plus_moe, total_25plusM) * 100,
        NA_real_
      ),

      # BUG FIX BUG_REGR_009 (2026-01-30): Explicit NA check before division
      # Poverty: % below poverty line
      pct_poverty = dplyr::if_else(
        !is.na(poverty_denominatorE) & poverty_denominatorE > 0,
        100 * poverty_numeratorE / poverty_denominatorE,
        NA_real_
      ),
      pct_poverty_moe = dplyr::if_else(
        !is.na(poverty_denominatorE) & poverty_denominatorE > 0,
        tidycensus::moe_prop(poverty_numeratorE, poverty_denominatorE,
                             poverty_numeratorM, poverty_denominatorM) * 100,
        NA_real_
      )
    ) %>%
    dplyr::select(
      GEOID, NAME,
      median_income, median_income_moe,
      pct_bachelors_plus, pct_bachelors_plus_moe,
      pct_poverty, pct_poverty_moe
    )

  cat(sprintf("  Fetched socioeconomic data for %s tracts\n", format(nrow(socioeconomic_data), big.mark = ",")))

  return(socioeconomic_data)
}


#' Calculate Provider Density per 100,000 Population
#'
#' Calculates providers per 100,000 population based on provider counts and
#' population totals.
#'
#' @param provider_counts `numeric vector`: Number of providers in each area
#' @param population_totals `numeric vector`: Total population in each area
#' @param scale `numeric`: Population scale for density calculation (default:
#'   100000)
#'
#' @return Numeric vector of provider densities (providers per 100K population)
#'
#' @export
calculate_provider_density <- function(provider_counts, population_totals, scale = 100000) {
  ifelse(population_totals > 0,
         (provider_counts / population_totals) * scale,
         NA_real_)
}


#' Aggregate Covariates by Group
#'
#' Aggregates tract-level covariates to category-year level using population
#' weighting.
#'
#' @param tract_data `data.frame`: with tract-level data including:
#'   - category_col: Grouping variable (race/ethnicity category)
#'   - year_col: Year variable
#'   - weight_col: Population weight for aggregation
#'   - ruca_category: Urban/suburban/rural classification
#'   - median_income: Median household income
#'   - pct_bachelors_plus: Percentage with bachelor's degree or higher
#'   - pct_poverty: Percentage below poverty line
#'
#' @param category_col `character`: Name of category column (default:
#'   "category")
#' @param weight_col `character`: Name of weight column for aggregation
#'   (default: "population")
#'
#' @return Data frame with one row per (category, year) containing:
#'   - category: Demographic category
#'   - year: Analysis year
#'   - pct_urban: Percentage of population in urban areas
#'   - pct_suburban: Percentage of population in suburban areas
#'   - pct_rural: Percentage of population in rural areas
#'   - median_income: Population-weighted mean of tract median incomes
#'   - pct_bachelors_plus: Population-weighted mean of education percentages
#'   - pct_poverty: Population-weighted mean of poverty percentages
#'   - total_population: Total population in category-year
#'
#' @export
aggregate_covariates_by_group <- function(tract_data, category_col = "category", year_col = "year", weight_col = "population") {

  cat("Aggregating covariates by demographic group and year...\n")

  # Calculate RUCA percentages
  ruca_pcts <- tract_data %>%
    dplyr::group_by(!!rlang::sym(category_col), !!rlang::sym(year_col), ruca_category) %>%
    dplyr::summarise(
      pop_in_ruca = sum(!!rlang::sym(weight_col), na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    dplyr::mutate(
      total_pop = sum(pop_in_ruca),
      # BUG FIX BUG_REGR_001 (2026-01-29): Guard against division by zero
      # If category-year has zero population, return NA instead of Inf/NaN
      pct = dplyr::if_else(total_pop > 0, 100 * pop_in_ruca / total_pop, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c(!!rlang::sym(category_col), !!rlang::sym(year_col)),
      names_from = ruca_category,
      values_from = pct,
      names_prefix = "pct_",
      values_fill = 0
    )

  # Calculate weighted means for continuous variables
  weighted_means <- tract_data %>%
    dplyr::group_by(!!rlang::sym(category_col), !!rlang::sym(year_col)) %>%
    dplyr::summarise(
      median_income = stats::weighted.mean(median_income, !!rlang::sym(weight_col), na.rm = TRUE),
      pct_bachelors_plus = stats::weighted.mean(pct_bachelors_plus, !!rlang::sym(weight_col), na.rm = TRUE),
      pct_poverty = stats::weighted.mean(pct_poverty, !!rlang::sym(weight_col), na.rm = TRUE),
      total_population = sum(!!rlang::sym(weight_col), na.rm = TRUE),
      .groups = "drop"
    )

  # Join RUCA percentages with weighted means
  # BUG FIX BUG_REGR_004 (2026-01-29): Verify no row duplication from join
  pre_join_n <- nrow(weighted_means)
  aggregated_data <- dplyr::left_join(
    weighted_means,
    ruca_pcts,
    by = c(category_col, year_col),
    relationship = "one-to-one"  # Error if many-to-many
  )

  # Verify join didn't create duplicates
  if (nrow(aggregated_data) != pre_join_n) {
    stop(sprintf("CRITICAL: Join created duplicates (%d rows before, %d after)",
                 pre_join_n, nrow(aggregated_data)))
  }

  cat(sprintf("  Aggregated %d category-year combinations\n", nrow(aggregated_data)))

  return(aggregated_data)
}


# =============================================================================
# DATA VALIDATION
# =============================================================================

#' Validate Augmented Accessibility Data
#'
#' Performs quality checks on augmented accessibility data to ensure data
#' integrity.
#'
#' @param augmented_data `data.frame`: Augmented accessibility data with
#'   covariates
#'
#' @return Logical. TRUE if validation passes, stops with error if validation
#'   fails
#'
#' @details
#' Validation checks:
#' 1. No NAs in covariate columns (except where expected)
#' 2. Urban + Suburban + Rural percentages sum to approximately 100%
#' 3. Median income in reasonable range ($10K - $250K)
#' 4. Education percentage in range (0-100%)
#' 5. Poverty percentage in range (0-100%)
#' 6. Provider density is non-negative
#'
#' @export
validate_augmented_data <- function(augmented_data) {

  cat("Validating augmented accessibility data...\n")

  # Check 1: No unexpected NAs in covariate columns
  covariate_cols <- c("pct_urban", "pct_suburban", "pct_rural",
                      "median_income", "pct_bachelors_plus", "pct_poverty")

  for (col in covariate_cols) {
    if (col %in% names(augmented_data)) {
      na_count <- sum(is.na(augmented_data[[col]]))
      if (na_count > 0) {
        warning(sprintf("  Warning: %d NA values found in %s column", na_count, col))
      }
    }
  }

  # BUG FIX BUG_REGR_010 (2026-01-30): Stricter RUCA validation with range reporting
  # Check 2: RUCA percentages sum to approximately 100%
  if (all(c("pct_urban", "pct_suburban", "pct_rural") %in% names(augmented_data))) {
    ruca_check <- augmented_data %>%
      dplyr::mutate(ruca_total = pct_urban + pct_suburban + pct_rural) %>%
      dplyr::summarise(
        min_sum = min(ruca_total, na.rm = TRUE),
        max_sum = max(ruca_total, na.rm = TRUE),
        n_outside_range = sum(abs(ruca_total - 100) > 0.1, na.rm = TRUE)
      )

    cat(sprintf("  RUCA percentage sums: [%.2f%%, %.2f%%]\n",
                ruca_check$min_sum, ruca_check$max_sum))

    if (ruca_check$n_outside_range > 0) {
      warning(sprintf("  ⚠ %d rows have RUCA sums outside [99.9%%, 100.1%%]",
                      ruca_check$n_outside_range))
    }

    if (ruca_check$min_sum < 99.5 || ruca_check$max_sum > 100.5) {
      stop(sprintf("CRITICAL: RUCA percentages out of acceptable range: [%.2f%%, %.2f%%]",
                   ruca_check$min_sum, ruca_check$max_sum))
    }
  }

  # Check 3: Median income in reasonable range
  if ("median_income" %in% names(augmented_data)) {
    income_out_of_range <- augmented_data %>%
      dplyr::filter(!is.na(median_income), median_income < 10000 | median_income > 250000) %>%
      nrow()

    if (income_out_of_range > 0) {
      warning(sprintf("  Warning: %d rows have median income outside $10K-$250K range", income_out_of_range))
    }
  }

  # Check 4: Education percentage in range (0-100%)
  if ("pct_bachelors_plus" %in% names(augmented_data)) {
    edu_out_of_range <- augmented_data %>%
      dplyr::filter(!is.na(pct_bachelors_plus), pct_bachelors_plus < 0 | pct_bachelors_plus > 100) %>%
      nrow()

    if (edu_out_of_range > 0) {
      stop(sprintf("ERROR: %d rows have education percentage outside 0-100%% range", edu_out_of_range))
    }
  }

  # Check 5: Poverty percentage in range (0-100%)
  if ("pct_poverty" %in% names(augmented_data)) {
    pov_out_of_range <- augmented_data %>%
      dplyr::filter(!is.na(pct_poverty), pct_poverty < 0 | pct_poverty > 100) %>%
      nrow()

    if (pov_out_of_range > 0) {
      stop(sprintf("ERROR: %d rows have poverty percentage outside 0-100%% range", pov_out_of_range))
    }
  }

  # Check 6: Provider density is non-negative
  if ("providers_per_100k" %in% names(augmented_data)) {
    negative_density <- augmented_data %>%
      dplyr::filter(!is.na(providers_per_100k), providers_per_100k < 0) %>%
      nrow()

    if (negative_density > 0) {
      stop(sprintf("ERROR: %d rows have negative provider density", negative_density))
    }
  }

  cat("  All validation checks passed\n")
  return(TRUE)
}


# =============================================================================
# MAIN AUGMENTATION FUNCTION
# =============================================================================

#' Augment Accessibility Data with Covariates
#'
#' Main function to add geographic and socioeconomic covariates to accessibility
#' data.
#'
#' @param access_data `data.frame`: National accessibility data (Walker format)
#'   with columns:
#'   - category: Demographic category (race/ethnicity)
#'   - year: Analysis year
#'   - range: Drive time range (minutes)
#'   - percent_with_access: Percentage with access (estimate)
#'   - moe: Margin of error
#'   Additional columns will be preserved
#'
#' @param tract_access_path `character`: Path to tract-level accessibility data
#'   (RDS or CSV)
#'   Expected columns:
#'   - tract_geoid: Census tract identifier (11-character FIPS)
#'   - year: Analysis year
#'   - category: Demographic category
#'   - population: Population count
#'   - has_access: Logical indicating access within drive time
#'
#' @inheritParams shared_params_temporal
#'
#'
#'
#' @return Data frame. Original access_data augmented with:
#'   - pct_urban: Percentage of population in urban areas
#'   - pct_suburban: Percentage of population in suburban areas
#'   - pct_rural: Percentage of population in rural areas
#'   - median_income: Population-weighted median household income
#'   - pct_bachelors_plus: Population-weighted % with bachelor's degree or
#' higher
#'   - pct_poverty: Population-weighted % below poverty line
#'   - providers_per_100k: Providers per 100,000 population (if provider data
#' available)
#'
#' @examples
#' \dontrun{
#' # Load Walker accessibility data
#' access_data <-
#' read_csv("manuscript/data/Walker_data/access_by_group_with_moe_latest.csv")
#'
#' # Augment with covariates
#' augmented <- augment_accessibility_with_covariates(
#'   access_data = access_data,
#'   tract_access_path = "data/outputs/accessibility/tract_access_by_group.rds",
#'   years = 2013:2022,
#'   paths = paths
#' )
#' }
#'
#' @export
augment_accessibility_with_covariates <- function(
    access_data,
    tract_access_path,
    years,
    paths,
    cache_dir = here::here("data", "cache", "acs_covariates")
) {

  cat("\n")
  cat("================================================================================\n")
  cat("AUGMENTING ACCESSIBILITY DATA WITH COVARIATES\n")
  cat("================================================================================\n\n")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 1: Load tract-level accessibility data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("Step 1: Loading tract-level accessibility data...\n")

  if (!file.exists(tract_access_path)) {
    stop(sprintf("Tract accessibility data not found: %s", tract_access_path))
  }

  # Load based on file extension
  if (grepl("\\.rds$", tract_access_path)) {
    tract_data <- readRDS(tract_access_path)
  } else if (grepl("\\.csv$", tract_access_path)) {
    tract_data <- readr::read_csv(tract_access_path, show_col_types = FALSE)
  } else {
    stop("Tract accessibility data must be .rds or .csv format")
  }

  cat(sprintf("  Loaded %s rows of tract-level data\n", format(nrow(tract_data), big.mark = ",")))

  # BUG FIX BUG_REGR_006 (2026-01-29): Check for empty data
  if (nrow(tract_data) == 0) {
    stop(sprintf("CRITICAL: Tract accessibility data is empty: %s", tract_access_path))
  }

  # Validate required columns
  required_cols <- c("tract_geoid", "year", "category", "population")
  missing_cols <- setdiff(required_cols, names(tract_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in tract data: %s", paste(missing_cols, collapse = ", ")))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 2: Load RUCA codes
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 2: Loading RUCA codes...\n")

  ruca_data <- load_ruca_codes(duckdb_path = paths$nber_duckdb_main)

  # Join RUCA codes to tract data
  # BUG FIX BUG_REGR_004 (2026-01-29): Verify no row duplication from join
  pre_join_n <- nrow(tract_data)
  tract_data <- tract_data %>%
    dplyr::left_join(
      ruca_data %>% dplyr::select(tract_geoid, ruca_category),
      by = "tract_geoid",
      relationship = "many-to-one"  # Each tract should match one RUCA code
    )

  # Verify join didn't create duplicates
  if (nrow(tract_data) != pre_join_n) {
    stop(sprintf("CRITICAL: RUCA join created duplicates (%d rows before, %d after)",
                 pre_join_n, nrow(tract_data)))
  }

  # Report RUCA coverage
  ruca_coverage <- sum(!is.na(tract_data$ruca_category)) / nrow(tract_data) * 100
  cat(sprintf("  RUCA codes matched for %.1f%% of tracts\n", ruca_coverage))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 3: Fetch ACS socioeconomic variables
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 3: Fetching ACS socioeconomic variables...\n")

  # Create cache directory if needed
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # Fetch ACS data for each unique year in the analysis
  acs_data_list <- list()
  unique_years <- sort(unique(years))

  for (yr in unique_years) {
    cat(sprintf("\n  Fetching ACS data for year %d...\n", yr))

    # Check cache first
    cache_file <- NULL
    if (!is.null(cache_dir)) {
      cache_file <- file.path(cache_dir, sprintf("acs_socioeconomic_%d.rds", yr))
      if (file.exists(cache_file)) {
        cat(sprintf("    Loading from cache: %s\n", basename(cache_file)))
        acs_data_list[[as.character(yr)]] <- readRDS(cache_file)
        next
      }
    }

    # Fetch from API
    tryCatch({
      acs_yr <- fetch_socioeconomic_variables(
        year = yr,
        state = NULL,  # All states
        geography = "tract",
        cache_dir = cache_dir
      )

      acs_data_list[[as.character(yr)]] <- acs_yr

      # Save to cache atomically via .tmp + rename
      if (!is.null(cache_file)) {
        tmp_cache_file <- paste0(cache_file, ".tmp")
        saveRDS(acs_yr, tmp_cache_file)
        file.rename(tmp_cache_file, cache_file)
        cat(sprintf("    Cached to: %s\n", basename(cache_file)))
      }

    }, error = function(e) {
      warning(sprintf("Failed to fetch ACS data for year %d: %s", yr, e$message))
      acs_data_list[[as.character(yr)]] <- NULL
    })
  }

  # Combine ACS data across years
  acs_combined <- dplyr::bind_rows(acs_data_list, .id = "year") %>%
    dplyr::mutate(year = as.numeric(year))

  cat(sprintf("\n  Combined ACS data: %s tract-years\n", format(nrow(acs_combined), big.mark = ",")))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 4: Join ACS data to tract data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 4: Joining ACS socioeconomic data to tract data...\n")

  # RETRACTION GUARD: Validate ACS join does not create duplicate rows
  pre_acs_join_n <- nrow(tract_data)
  tract_data <- tract_data %>%
    dplyr::left_join(
      acs_combined %>% dplyr::select(GEOID, year, median_income, median_income_moe,
                                     pct_bachelors_plus, pct_bachelors_plus_moe,
                                     pct_poverty, pct_poverty_moe),
      by = c("tract_geoid" = "GEOID", "year"),
      relationship = "many-to-one"  # Each tract-year should match one ACS record
    )

  # Verify ACS join didn't create duplicates
  if (nrow(tract_data) != pre_acs_join_n) {
    stop(sprintf("RETRACTION GUARD: ACS join created duplicates (%d rows before, %d after). Check for duplicate GEOID-year combinations in ACS data.",
                 pre_acs_join_n, nrow(tract_data)),
         call. = FALSE)
  }

  # Report ACS coverage
  acs_coverage <- sum(!is.na(tract_data$median_income)) / nrow(tract_data) * 100
  cat(sprintf("  ACS data matched for %.1f%% of tract-years\n", acs_coverage))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 5: Aggregate covariates by category and year
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 5: Aggregating covariates by demographic group and year...\n")

  aggregated_covariates <- aggregate_covariates_by_group(
    tract_data = tract_data,
    category_col = "category",
    year_col = "year",
    weight_col = "population"
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 6: Calculate provider density (if provider data available)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 6: Calculating provider density...\n")

  # Attempt to calculate provider counts from tract data
  if ("has_access" %in% names(tract_data)) {
    # This is a simplification - actual provider counts would come from isochrone data
    # For now, we'll skip this and note it should be added based on actual provider locations
    cat("  Skipping provider density calculation (requires provider location data)\n")
    cat("  Note: Provider density should be calculated from isochrone/physician data\n")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 7: Join aggregated covariates to national access data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 7: Joining aggregated covariates to national accessibility data...\n")

  # BUG FIX BUG_REGR_004 (2026-01-29): Verify no row duplication from join
  pre_join_n <- nrow(access_data)
  augmented_data <- access_data %>%
    dplyr::left_join(
      aggregated_covariates,
      by = c("category", "year"),
      relationship = "one-to-one"  # Each category-year should have one covariate row
    )

  # Verify join didn't create duplicates
  if (nrow(augmented_data) != pre_join_n) {
    stop(sprintf("CRITICAL: Final join created duplicates (%d rows before, %d after)",
                 pre_join_n, nrow(augmented_data)))
  }

  # Check for within-group duplication on join keys
  augment_key_cols <- intersect(names(augmented_data), c("category", "year"))
  if (length(augment_key_cols) > 0L) {
    n_distinct_augment_keys <- nrow(dplyr::distinct(augmented_data[, augment_key_cols, drop = FALSE]))
    if (n_distinct_augment_keys < nrow(augmented_data)) {
      stop(sprintf(
        "augment join created within-group duplicates: %d rows but only %d distinct key combinations (%s)",
        nrow(augmented_data), n_distinct_augment_keys, paste(augment_key_cols, collapse = ", ")
      ), call. = FALSE)
    }
    message(sprintf("[DEBUG] Step 7: %d rows after final join, %d distinct category-year keys",
                    nrow(augmented_data), n_distinct_augment_keys))
  }

  cat(sprintf("  Augmented %s rows of national accessibility data\n", format(nrow(augmented_data), big.mark = ",")))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 8: Validate augmented data
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\nStep 8: Validating augmented data...\n")
  validate_augmented_data(augmented_data)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Summary
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cat("\n")
  cat("================================================================================\n")
  cat("AUGMENTATION COMPLETE\n")
  cat("================================================================================\n\n")

  cat("Summary:\n")
  cat(sprintf("  Input rows: %s\n", format(nrow(access_data), big.mark = ",")))
  cat(sprintf("  Output rows: %s\n", format(nrow(augmented_data), big.mark = ",")))
  cat(sprintf("  Categories: %d\n", length(unique(augmented_data$category))))
  cat(sprintf("  Years: %d\n", length(unique(augmented_data$year))))
  cat("\n")

  cat("Covariates added:\n")
  cat("  - pct_urban, pct_suburban, pct_rural (RUCA classification)\n")
  cat("  - median_income (population-weighted)\n")
  cat("  - pct_bachelors_plus (population-weighted)\n")
  cat("  - pct_poverty (population-weighted)\n")
  cat("\n")

  return(augmented_data)
}


# =============================================================================
# EXECUTION SECTION
# =============================================================================

# BUG FIX (2026-08-31): !interactive() is FALSE both when run directly via
# Rscript AND when merely source()d non-interactively (e.g.
# pkgload::load_all(), which sources every file in R/ during
# roxygen2::roxygenise()) -- it does not distinguish "run directly" from
# "sourced." sys.nframe() == 0L (0 only at the true top level of an Rscript
# invocation) is the correct check. Callers that source()/sys.source() this
# file (confirmed via grep) only need the function definitions above, not
# this usage-text tail.
if (sys.nframe() == 0L) {
  cat("Loading configuration...\n")
  paths <- load_paths(verbose = FALSE)

  cat("\nTo use this script, call augment_accessibility_with_covariates() with:\n")
  cat("  - access_data: Walker accessibility data\n")
  cat("  - tract_access_path: Path to tract-level accessibility data\n")
  cat("  - years: Vector of analysis years\n")
  cat("  - paths: Configuration paths\n")
  cat("\nSee function documentation for details.\n")
}
