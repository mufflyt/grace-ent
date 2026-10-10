#' @title Augment Medical School Data with AHRQ 3P-RD
#'
#' @description
#' Uses 3P-RD to fill missing medical schools, add graduation years,
#' and classify IMG (International Medical Graduate) status.
#'
#' @family data-enrichment
#' @name augment_medical_schools_3prd
NULL

#!/usr/bin/env Rscript
# ==============================================================================
# Augment Medical School Data with AHRQ 3P-RD
# ==============================================================================
# Purpose: Use 3P-RD to fill missing medical schools, add graduation years,
#          and classify IMG (International Medical Graduate) status
# Author: Tyler Muffly & Claude
# Date: 2025-10-26
# Data Source: AHRQ 3P-RD Physician Directory
# Coverage: 22K OB/GYN physicians, 71% with medical school, 69% with grad year
# ==============================================================================

# No library() calls — use pkg::fn() throughout

# Load paths
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

paths <- load_paths(verbose = FALSE)

cat("\n")
cat(strrep("=", 80), "\n")
cat("MEDICAL SCHOOL AUGMENTATION WITH AHRQ 3P-RD\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# Step 1: Load ABOG Physicians with Current Medical School Data
# ==============================================================================

cat("Step 1: Loading current ABOG physician data...\n")

con <- DBI::dbConnect(duckdb::duckdb(), paths$nber_duckdb_main, read_only = TRUE)
on.exit(
  if (exists("con") && DBI::dbIsValid(con)) {
    tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)
  },
  add = TRUE
)

# Get current ABOG data
abog_current <- DBI::dbGetQuery(con, "
  SELECT
    npi,
    medical_school_name,
    medical_school_country,
    graduation_year,
    subspecialty
  FROM credentials.abog_enhanced_production
  WHERE npi IS NOT NULL
")

message(sprintf("[DEBUG] Step 1: %d rows loaded from abog_enhanced_production",
                nrow(abog_current)))
cat(sprintf("  Total ABOG physicians: %s\n", scales::comma(nrow(abog_current))))
cat(sprintf("  With medical school: %s (%.1f%%)\n",
            scales::comma(sum(!is.na(abog_current$medical_school_name))),
            100 * mean(!is.na(abog_current$medical_school_name), na.rm = TRUE)))
cat(sprintf("  With graduation year: %s (%.1f%%)\n",
            scales::comma(sum(!is.na(abog_current$graduation_year))),
            100 * mean(!is.na(abog_current$graduation_year), na.rm = TRUE)))
cat(sprintf("  Missing both: %s (%.1f%%)\n\n",
            scales::comma(sum(is.na(abog_current$medical_school_name) &
                      is.na(abog_current$graduation_year))),
            100 * mean(is.na(abog_current$medical_school_name) &
                       is.na(abog_current$graduation_year), na.rm = TRUE)))

# ==============================================================================
# Step 2: Load 3P-RD Medical School Data
# ==============================================================================

cat("Step 2: Loading AHRQ 3P-RD medical school data...\n")

# Check if 3P-RD table exists
if (!"ahrq_3prd_physicians" %in% DBI::dbListTables(con)) {
  stop("3P-RD table not found. Run R/B-ahrq_3prd_load_duckdb.R first.")
}

prd_medical_schools <- DBI::dbGetQuery(con, "
  SELECT
    PHYS_NPI as npi,
    PHYS_MED_SCH as medical_school_3prd,
    PHYS_MED_SCH_LOC as med_school_location,
    PHYS_GRAD_YR as graduation_year_3prd,
    PHYS_PRIM_SPC_DESC as primary_specialty_3prd
  FROM main.ahrq_3prd_physicians
  WHERE PHYS_NPI IS NOT NULL
    AND (PHYS_MED_SCH IS NOT NULL OR PHYS_GRAD_YR IS NOT NULL)
    AND PHYS_PRIM_SPC_DESC IN ('Obstetrics/gynecology', 'Gynecologist/oncologist')
")

message(sprintf("[DEBUG] Step 2: %d rows loaded from ahrq_3prd_physicians",
                nrow(prd_medical_schools)))
cat(sprintf("  3P-RD OB/GYN physicians: %s\n", scales::comma(nrow(prd_medical_schools))))
cat(sprintf("  With medical school: %s (%.1f%%)\n",
            scales::comma(sum(!is.na(prd_medical_schools$medical_school_3prd))),
            100 * mean(!is.na(prd_medical_schools$medical_school_3prd), na.rm = TRUE)))
cat(sprintf("  With graduation year: %s (%.1f%%)\n",
            scales::comma(sum(!is.na(prd_medical_schools$graduation_year_3prd))),
            100 * mean(!is.na(prd_medical_schools$graduation_year_3prd), na.rm = TRUE)))
cat(sprintf("  With IMG classification: %s (%.1f%%)\n\n",
            scales::comma(sum(!is.na(prd_medical_schools$med_school_location))),
            100 * mean(!is.na(prd_medical_schools$med_school_location), na.rm = TRUE)))

# ==============================================================================
# Step 3: Merge and Augment
# ==============================================================================

cat("Step 3: Merging ABOG with 3P-RD data...\n")

abog_augmented <- abog_current |>
  dplyr::left_join(prd_medical_schools, by = "npi", na_matches = "never") |>
  dplyr::mutate(
    # Fill missing medical schools from 3P-RD
    medical_school_final = dplyr::coalesce(medical_school_name, medical_school_3prd),

    # Fill missing graduation years from 3P-RD
    # ABOG graduation year takes precedence over 3P-RD because:
    # - ABOG data comes directly from board certification records
    # - 3P-RD aggregates from multiple sources with varying reliability
    graduation_year_final = dplyr::coalesce(
      graduation_year,
      as.integer(graduation_year_3prd)
    ),

    # Classify IMG status
    # Priority: 1) ABOG medical_school_country, 2) 3P-RD med_school_location, 3) Name-based (conservative)
    img_status = dplyr::case_when(
      # Existing country data takes precedence (most reliable)
      !is.na(medical_school_country) ~
        dplyr::if_else(medical_school_country == "United States", "US Grad", "IMG"),

      # Use 3P-RD classification (second most reliable)
      !is.na(med_school_location) ~
        dplyr::case_when(
          med_school_location == "US" ~ "US Grad",
          med_school_location %in% c("Territory", "Other") ~ "IMG",
          TRUE ~ "Unknown"
        ),

      # Only classify as IMG if school name explicitly indicates international origin.
      !is.na(medical_school_final) &
        stringr::str_detect(medical_school_final,
                   stringr::regex("International|Caribbean|Mexico|Philippines|India|Pakistan|Caribbean|Ross|St.? George|Grenada|Dominica|Saba|AUC|SGU",
                         ignore_case = TRUE)) ~ "IMG",

      # Default to "Unknown" rather than guessing
      TRUE ~ "Unknown"
    ),

    # Data source tracking
    medical_school_source = dplyr::case_when(
      !is.na(medical_school_name) & !is.na(medical_school_3prd) ~ "Both (present)",
      !is.na(medical_school_name) ~ "ABOG only",
      !is.na(medical_school_3prd) ~ "3P-RD only (NEW)",
      TRUE ~ "Missing"
    ),

    graduation_year_source = dplyr::case_when(
      !is.na(graduation_year) & !is.na(graduation_year_3prd) ~ "Both (present)",
      !is.na(graduation_year) ~ "ABOG only",
      !is.na(graduation_year_3prd) ~ "3P-RD only (NEW)",
      TRUE ~ "Missing"
    ),

    # Calculate years in practice using dynamic current year
    years_in_practice = dplyr::if_else(
      !is.na(graduation_year_final),
      as.integer(format(Sys.Date(), "%Y")) - graduation_year_final,
      NA_real_
    ),

    # Career stage classification
    career_stage = dplyr::case_when(
      is.na(years_in_practice) ~ "Unknown",
      years_in_practice < 5 ~ "Early Career (0-4 years)",
      years_in_practice < 15 ~ "Established (5-14 years)",
      years_in_practice < 25 ~ "Mid-Late Career (15-24 years)",
      years_in_practice >= 25 ~ "Late Career (25+ years)",
      TRUE ~ "Unknown"
    )
  )

message(sprintf("[DEBUG] Step 3: %d rows after merge", nrow(abog_augmented)))

# ==============================================================================
# Step 4: Generate Summary Statistics
# ==============================================================================

cat("\nStep 4: Generating summary statistics...\n\n")

cat(strrep("-", 80), "\n")
cat("AUGMENTATION RESULTS\n")
cat(strrep("-", 80), "\n\n")

# Medical School Coverage
cat("Medical School Coverage:\n")
cat(sprintf("  Before (ABOG only): %s (%.1f%%)\n",
            scales::comma(sum(!is.na(abog_current$medical_school_name))),
            100 * mean(!is.na(abog_current$medical_school_name), na.rm = TRUE)))
cat(sprintf("  After (ABOG + 3P-RD): %s (%.1f%%)\n",
            scales::comma(sum(!is.na(abog_augmented$medical_school_final))),
            100 * mean(!is.na(abog_augmented$medical_school_final), na.rm = TRUE)))
cat(sprintf("  New records from 3P-RD: %s\n\n",
            scales::comma(sum(abog_augmented$medical_school_source == "3P-RD only (NEW)"))))

# Graduation Year Coverage
cat("Graduation Year Coverage:\n")
cat(sprintf("  Before (ABOG only): %s (%.1f%%)\n",
            scales::comma(sum(!is.na(abog_current$graduation_year))),
            100 * mean(!is.na(abog_current$graduation_year), na.rm = TRUE)))
cat(sprintf("  After (ABOG + 3P-RD): %s (%.1f%%)\n",
            scales::comma(sum(!is.na(abog_augmented$graduation_year_final))),
            100 * mean(!is.na(abog_augmented$graduation_year_final), na.rm = TRUE)))
cat(sprintf("  New records from 3P-RD: %s\n\n",
            scales::comma(sum(abog_augmented$graduation_year_source == "3P-RD only (NEW)"))))

# IMG Classification
cat("IMG Status Distribution:\n")
img_summary <- abog_augmented |>
  dplyr::count(img_status) |>
  dplyr::mutate(pct = 100 * n / sum(n)) |>
  dplyr::arrange(dplyr::desc(n))

for (i in seq_len(nrow(img_summary))) {
  cat(sprintf("  %s: %s (%.1f%%)\n",
              img_summary$img_status[i],
              scales::comma(img_summary$n[i]),
              img_summary$pct[i]))
}
cat("\n")

# Career Stage Distribution
cat("Career Stage Distribution:\n")
career_summary <- abog_augmented |>
  dplyr::filter(!is.na(years_in_practice)) |>
  dplyr::count(career_stage) |>
  dplyr::mutate(pct = 100 * n / sum(n)) |>
  dplyr::arrange(dplyr::desc(n))

for (i in seq_len(nrow(career_summary))) {
  cat(sprintf("  %s: %s (%.1f%%)\n",
              career_summary$career_stage[i],
              scales::comma(career_summary$n[i]),
              career_summary$pct[i]))
}
cat("\n")

# Years in Practice Statistics
years_stats <- abog_augmented |>
  dplyr::filter(!is.na(years_in_practice)) |>
  dplyr::summarize(
    mean   = mean(years_in_practice, na.rm = TRUE),
    sd     = stats::sd(years_in_practice, na.rm = TRUE),
    median = stats::median(years_in_practice, na.rm = TRUE),
    q25    = stats::quantile(years_in_practice, 0.25, na.rm = TRUE),
    q75    = stats::quantile(years_in_practice, 0.75, na.rm = TRUE),
    min    = min(years_in_practice, na.rm = TRUE),
    max    = max(years_in_practice, na.rm = TRUE)
  )

cat("Years in Practice (among those with graduation year):\n")
cat(sprintf("  Mean (SD): %.1f (%.1f) years\n", years_stats$mean, years_stats$sd))
cat(sprintf("  Median (IQR): %.1f (%.1f-%.1f) years\n",
            years_stats$median, years_stats$q25, years_stats$q75))
cat(sprintf("  Range: %.0f-%.0f years\n\n", years_stats$min, years_stats$max))

# ==============================================================================
# Step 5: Top Medical Schools Analysis
# ==============================================================================

cat(strrep("-", 80), "\n")
cat("TOP 10 MEDICAL SCHOOLS (OB/GYN PHYSICIANS)\n")
cat(strrep("-", 80), "\n\n")

top_schools <- abog_augmented |>
  dplyr::filter(!is.na(medical_school_final)) |>
  dplyr::count(medical_school_final, img_status, sort = TRUE) |>
  utils::head(10)

cat("Rank | Medical School | IMG | Count\n")
cat(strrep("-", 80), "\n")
for (i in seq_len(nrow(top_schools))) {
  cat(sprintf("%2d   | %-50s | %-8s | %s\n",
              i,
              stringr::str_trunc(top_schools$medical_school_final[i], 50),
              top_schools$img_status[i],
              scales::comma(top_schools$n[i])))
}
cat("\n")

# ==============================================================================
# Step 6: IMG vs US Grad by Subspecialty
# ==============================================================================

cat(strrep("-", 80), "\n")
cat("IMG STATUS BY SUBSPECIALTY\n")
cat(strrep("-", 80), "\n\n")

subspecialty_img <- abog_augmented |>
  dplyr::filter(!is.na(subspecialty) & img_status != "Unknown") |>
  dplyr::count(subspecialty, img_status) |>
  dplyr::group_by(subspecialty) |>
  dplyr::mutate(
    total = sum(n),
    pct = if_else(total > 0L, 100 * n / total, NA_real_)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(img_status == "IMG") |>
  dplyr::arrange(dplyr::desc(pct))

cat("Subspecialty | IMG Count | Total | IMG %\n")
cat(strrep("-", 80), "\n")
for (i in seq_len(nrow(subspecialty_img))) {
  cat(sprintf("%-12s | %9s | %5s | %5.1f%%\n",
              subspecialty_img$subspecialty[i],
              scales::comma(subspecialty_img$n[i]),
              scales::comma(subspecialty_img$total[i]),
              subspecialty_img$pct[i]))
}
cat("\n")

# ==============================================================================
# Step 7: Save Augmented Dataset (atomic write)
# ==============================================================================

cat("Step 7: Saving augmented dataset...\n")

# Validate external_drive path before attempting to write;
# fallback to project data directory if external drive unavailable.
output_dir <- paths$external_drive
if (is.null(output_dir) || !dir.exists(output_dir)) {
  cat(sprintf("  WARNING: External drive not available: %s\n",
              if (is.null(output_dir)) "NULL" else output_dir))
  output_dir <- here::here("data", "derived")
  cat(sprintf("  Using fallback directory: %s\n", output_dir))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Atomic write: write to .tmp then rename
output_file <- file.path(output_dir, "abog_with_medical_schools_3prd.parquet")
output_tmp  <- paste0(output_file, ".tmp.", Sys.getpid())
tryCatch({
  arrow::write_parquet(abog_augmented, output_tmp)
  file.rename(output_tmp, output_file)
}, error = function(e) {
  if (file.exists(output_tmp)) unlink(output_tmp)
  stop("Failed to write augmented dataset: ", conditionMessage(e))
})

cat(sprintf("  Saved to %s\n", output_file))
cat(sprintf("  Records: %s\n", scales::comma(nrow(abog_augmented))))
cat("  Note: Load to DuckDB later when database is free\n\n")

# ==============================================================================
# Step 8: Generate Manuscript Text
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("MANUSCRIPT TABLE 1 TEXT (Copy-Paste Ready)\n")
cat(strrep("=", 80), "\n\n")

us_grad_pct <- abog_augmented |>
  dplyr::filter(img_status != "Unknown") |>
  dplyr::summarize(us_pct = 100 * mean(img_status == "US Grad", na.rm = TRUE)) |>
  dplyr::pull(us_pct)

img_pct <- 100 - us_grad_pct

mean_years <- round(years_stats$mean, 1)
sd_years   <- round(years_stats$sd, 1)

cat("Medical Training:\n")
cat(sprintf("  US Medical Graduate, n (%%): %s (%.1f%%)\n",
            scales::comma(sum(abog_augmented$img_status == "US Grad")),
            us_grad_pct))
cat(sprintf("  International Medical Graduate, n (%%): %s (%.1f%%)\n",
            scales::comma(sum(abog_augmented$img_status == "IMG")),
            img_pct))
cat(sprintf("  Years in Practice, mean (SD): %.1f (%.1f)\n\n",
            mean_years, sd_years))

cat("METHODS TEXT:\n")
cat(strrep("-", 80), "\n")
cat(sprintf(
  paste0(
    "Medical school and graduation year data were obtained from the American\n",
    "Board of Obstetrics and Gynecology records and augmented with physician\n",
    "directory data from the AHRQ 3P-RD database (n=%s physicians matched,\n",
    "%s with medical school information, %s with graduation years). Physicians\n",
    "were classified as US medical graduates or international medical graduates\n",
    "(IMGs) based on medical school location. Years in practice were calculated\n",
    "from graduation year to the current analysis year.\n\n"
  ),
  scales::comma(sum(!is.na(prd_medical_schools$npi))),
  scales::comma(sum(!is.na(prd_medical_schools$medical_school_3prd))),
  scales::comma(sum(!is.na(prd_medical_schools$graduation_year_3prd)))
))

# ==============================================================================
# Step 9: Create Validation Report
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("DATA QUALITY VALIDATION\n")
cat(strrep("=", 80), "\n\n")

# Check for conflicting data (ABOG vs 3P-RD)
conflicts <- abog_augmented |>
  dplyr::filter(
    !is.na(graduation_year) &
    !is.na(graduation_year_3prd) &
    abs(graduation_year - as.integer(graduation_year_3prd)) > 2
  )

n_augmented <- nrow(abog_augmented)
conflict_pct <- if (n_augmented > 0L) {
  100 * nrow(conflicts) / n_augmented
} else {
  NA_real_
}

cat(sprintf("Graduation year conflicts (>2 year difference): %s (%.2f%%)\n",
            scales::comma(nrow(conflicts)),
            conflict_pct))

if (nrow(conflicts) > 0) {
  cat("\nExample conflicts:\n")
  conflicts_sample <- conflicts |>
    dplyr::select(npi, graduation_year, graduation_year_3prd) |>
    dplyr::mutate(difference = abs(graduation_year - as.integer(graduation_year_3prd))) |>
    dplyr::arrange(dplyr::desc(difference)) |>
    utils::head(5)

  print(conflicts_sample)
  cat("\nNote: Using ABOG graduation year when conflicts exist (higher confidence)\n")
}

cat("\n")

# ==============================================================================
# Cleanup — on.exit handler above disconnects con
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("MEDICAL SCHOOL AUGMENTATION COMPLETE\n")
cat(strrep("=", 80), "\n\n")

cat("Next Steps:\n")
cat("  1. Review credentials.abog_with_medical_schools table in DuckDB\n")
cat("  2. Copy manuscript text above into Table 1\n")
cat("  3. Run IMG geographic analysis: analyze_img_location_patterns.R\n")
cat("  4. Generate career stage figures for manuscript\n\n")
