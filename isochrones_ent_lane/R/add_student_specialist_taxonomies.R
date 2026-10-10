#' @title Add Student and Specialist Taxonomies to Temporal Tables
#'
#' @description
#' Adds OB/GYN-related student and specialist records to the
#' \code{temporal_all_years_fixed} table based on secondary taxonomy codes.
#'
#' @family database-utils
#' @name add_student_specialist_taxonomies
NULL

#!/usr/bin/env Rscript
#===============================================================================
# ADD STUDENT AND SPECIALIST TAXONOMIES TO temporal_all_years_fixed
#===============================================================================
#
# Purpose: Add OB/GYN-related students and specialists to the temporal table
#
# Date: 2025-12-02
#
# Taxonomy Codes Added:
#   - 390200000X = Student in Organized Health Care Education/Training Program
#   - 174400000X = Specialist
#
# Criteria: Primary taxonomy is Student or Specialist AND secondary taxonomy
#           contains 207V% (OB/GYN)
#
#===============================================================================

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

library(DBI)
library(duckdb)
library(yaml)

# Execution section ----
# BUG FIX (2026-08-31): this whole driver body ran unconditionally at file
# scope, so roxygen2::roxygenise() sourcing this file during the pkgdown
# build crashed on file() inside duckdb:::path_normalize() when the
# nber_duckdb_main path's parent directory doesn't exist on the CI
# runner. No reusable functions and no other source()-based callers
# (confirmed via grep; the one reference does readLines()-based text
# inspection only).
if (sys.nframe() == 0L) {

# Load paths from config
paths <- yaml::read_yaml(here::here("config/paths.yml"))

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ADD STUDENT AND SPECIALIST TAXONOMIES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Load paths
paths <- load_paths(verbose = FALSE)
db_path <- paths$nber_duckdb_main

cat(sprintf("Database: %s\n\n", basename(db_path)))

# Connect
con <- dbConnect(duckdb(), db_path, read_only = FALSE)
on.exit(tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL), add = TRUE)

# Check current counts
cat("Current temporal_all_years_fixed counts:\n")
current <- dbGetQuery(con, "
  SELECT analysis_year, COUNT(*) as n
  FROM temporal_all_years_fixed
  GROUP BY analysis_year
  ORDER BY analysis_year
")
print(current)

#===============================================================================
# Define source tables for each year
#===============================================================================

year_tables <- list(
  "2012" = "npidata_2012",
  "2020" = "NPPES_Data_Dissemination_July_2020_npidata_pfile_20050523_20200712_csv",
  "2021" = "NPPES_Data_Disseminat_April_2021_npidata_pfile_20050523_20210411_csv",
  "2022" = "NPPES_Data_Disseminat_April_2022_npidata_pfile_20050523_20220410_csv",
  "2023" = "npi_2023",
  "2024" = "npi_2024",
  "2025" = "npi_2025"
)

# Check which tables exist
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("CHECKING AVAILABLE SOURCE TABLES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

existing_tables <- dbGetQuery(con, "
  SELECT table_name FROM information_schema.tables
  WHERE table_schema = 'main'
")$table_name

for (year in names(year_tables)) {
  tbl <- year_tables[[year]]
  exists <- tbl %in% existing_tables
  cat(sprintf("  %s: %s [%s]\n", year, tbl, if(exists) "OK" else "MISSING"))
}

#===============================================================================
# Add Student and Specialist records for each year
#===============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ADDING STUDENT AND SPECIALIST TAXONOMIES\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

for (year in names(year_tables)) {
  tbl <- year_tables[[year]]

  if (!tbl %in% existing_tables) {
    cat(sprintf("Skipping %s - table not found\n", year))
    next
  }

  cat(sprintf("\n--- Year %s ---\n", year))

  # Determine the gender column name (>= 2025 uses "Provider Sex Code")
  gender_col <- if (year >= "2025") "Provider Sex Code" else "Provider Gender Code"

  # Check if we need different date handling (>= 2023 uses MM/DD/YYYY format)
  date_handling <- if (year >= "2023") {
    "TRY_STRPTIME(\"Provider Enumeration Date\", '%m/%d/%Y')::DATE"
  } else {
    "\"Provider Enumeration Date\""
  }

  deact_handling <- if (year >= "2023") {
    "TRY_STRPTIME(\"NPI Deactivation Date\", '%m/%d/%Y')::DATE"
  } else {
    "\"NPI Deactivation Date\""
  }

  # Build INSERT SQL for Students (390200000X primary + 207V% secondary)
  # REQUIRES secondary OB/GYN taxonomy to avoid noise from unrelated students
  # 2025-12-06 LESSON LEARNED: Adding ALL students degraded precision from 95.2% to 90.5%
  # See docs/HALL_OF_SHAME_ABOG_NPI_2025-12-06.md for details
  sql_student <- sprintf('
    INSERT INTO temporal_all_years_fixed
    SELECT
      CAST(NPI AS VARCHAR) as npi,
      CAST("Entity Type Code" AS BIGINT) as entity_type,
      CAST("Provider Last Name (Legal Name)" AS VARCHAR) as last_name,
      CAST("Provider First Name" AS VARCHAR) as first_name,
      CAST("Provider Middle Name" AS VARCHAR) as middle_name,
      CAST("Provider Name Prefix Text" AS VARCHAR) as prefix,
      CAST("Provider Name Suffix Text" AS VARCHAR) as suffix,
      CAST("Provider Credential Text" AS VARCHAR) as credentials,
      CAST("%s" AS VARCHAR) as gender,
      CAST("Provider First Line Business Practice Location Address" AS VARCHAR) as practice_address_street,
      CAST("Provider Second Line Business Practice Location Address" AS VARCHAR) as practice_address_street2,
      CAST("Provider Business Practice Location Address City Name" AS VARCHAR) as practice_address_city,
      CAST("Provider Business Practice Location Address State Name" AS VARCHAR) as practice_address_state,
      CAST("Provider Business Practice Location Address Postal Code" AS VARCHAR) as practice_address_zip,
      CAST("Provider Business Practice Location Address Telephone Number" AS VARCHAR) as practice_phone,
      CAST("Healthcare Provider Taxonomy Code_1" AS VARCHAR) as taxonomy_1,
      CAST("Healthcare Provider Taxonomy Code_2" AS VARCHAR) as taxonomy_2,
      %s as npi_enumeration_date,
      %s as npi_deactivation_date,
      %s as analysis_year
    FROM "%s"
    WHERE "Entity Type Code" = \'1\'
      AND "Healthcare Provider Taxonomy Code_1" = \'390200000X\'
      AND ("Healthcare Provider Taxonomy Code_2" LIKE \'207V%%\'
           OR "Healthcare Provider Taxonomy Code_3" LIKE \'207V%%\'
           OR "Healthcare Provider Taxonomy Code_4" LIKE \'207V%%\')
      AND "NPI Deactivation Date" IS NULL
      AND CAST(NPI AS VARCHAR) NOT IN (SELECT npi FROM temporal_all_years_fixed WHERE analysis_year = %s)
  ', gender_col, date_handling, deact_handling, year, tbl, year)

  tryCatch({
    student_added <- dbExecute(con, sql_student)
    cat(sprintf("  Students (390200000X): added %d records\n", student_added))
  }, error = function(e) {
    cat(sprintf("  Students ERROR: %s\n", e$message))
  })

  # Build INSERT SQL for Specialists (174400000X primary + 207V% secondary)
  sql_specialist <- sprintf('
    INSERT INTO temporal_all_years_fixed
    SELECT
      CAST(NPI AS VARCHAR) as npi,
      CAST("Entity Type Code" AS BIGINT) as entity_type,
      CAST("Provider Last Name (Legal Name)" AS VARCHAR) as last_name,
      CAST("Provider First Name" AS VARCHAR) as first_name,
      CAST("Provider Middle Name" AS VARCHAR) as middle_name,
      CAST("Provider Name Prefix Text" AS VARCHAR) as prefix,
      CAST("Provider Name Suffix Text" AS VARCHAR) as suffix,
      CAST("Provider Credential Text" AS VARCHAR) as credentials,
      CAST("%s" AS VARCHAR) as gender,
      CAST("Provider First Line Business Practice Location Address" AS VARCHAR) as practice_address_street,
      CAST("Provider Second Line Business Practice Location Address" AS VARCHAR) as practice_address_street2,
      CAST("Provider Business Practice Location Address City Name" AS VARCHAR) as practice_address_city,
      CAST("Provider Business Practice Location Address State Name" AS VARCHAR) as practice_address_state,
      CAST("Provider Business Practice Location Address Postal Code" AS VARCHAR) as practice_address_zip,
      CAST("Provider Business Practice Location Address Telephone Number" AS VARCHAR) as practice_phone,
      CAST("Healthcare Provider Taxonomy Code_1" AS VARCHAR) as taxonomy_1,
      CAST("Healthcare Provider Taxonomy Code_2" AS VARCHAR) as taxonomy_2,
      %s as npi_enumeration_date,
      %s as npi_deactivation_date,
      %s as analysis_year
    FROM "%s"
    WHERE "Entity Type Code" = \'1\'
      AND "Healthcare Provider Taxonomy Code_1" = \'174400000X\'
      AND ("Healthcare Provider Taxonomy Code_2" LIKE \'207V%%\'
           OR "Healthcare Provider Taxonomy Code_3" LIKE \'207V%%\'
           OR "Healthcare Provider Taxonomy Code_4" LIKE \'207V%%\')
      AND "NPI Deactivation Date" IS NULL
      AND CAST(NPI AS VARCHAR) NOT IN (SELECT npi FROM temporal_all_years_fixed WHERE analysis_year = %s)
  ', gender_col, date_handling, deact_handling, year, tbl, year)

  tryCatch({
    specialist_added <- dbExecute(con, sql_specialist)
    cat(sprintf("  Specialists (174400000X): added %d records\n", specialist_added))
  }, error = function(e) {
    cat(sprintf("  Specialists ERROR: %s\n", e$message))
  })
}

#===============================================================================
# Rebuild physicians-only table
#===============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("REBUILDING PHYSICIANS-ONLY TABLE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

dbExecute(con, "DROP TABLE IF EXISTS temporal_all_years_fixed_physicians_only")
dbExecute(con, "
  CREATE TABLE temporal_all_years_fixed_physicians_only AS
  SELECT * FROM temporal_all_years_fixed
  WHERE (credentials LIKE '%MD%' OR credentials LIKE '%DO%'
         OR credentials LIKE '%M.D.%' OR credentials LIKE '%D.O.%'
         OR credentials LIKE '%MBBS%' OR credentials LIKE '%MBChB%'
         OR credentials IS NULL OR credentials = '')
    AND credentials NOT LIKE '%NP%'
    AND credentials NOT LIKE '%PA-C%'
    AND credentials NOT LIKE '%APRN%'
    AND credentials NOT LIKE '%CNM%'
")

#===============================================================================
# SUMMARY
#===============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("FINAL SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Get final counts
total <- dbGetQuery(con, "SELECT COUNT(*) as n FROM temporal_all_years_fixed")$n
phys <- dbGetQuery(con, "SELECT COUNT(*) as n FROM temporal_all_years_fixed_physicians_only")$n

cat(sprintf("Total records in temporal_all_years_fixed: %s\n", format(total, big.mark = ",")))
cat(sprintf("Physicians-only records: %s\n\n", format(phys, big.mark = ",")))

cat("Year distribution (with students and specialists):\n")
years <- dbGetQuery(con, "
  SELECT analysis_year, COUNT(*) as all_providers
  FROM temporal_all_years_fixed
  GROUP BY analysis_year
  ORDER BY analysis_year
")
print(years)

cat("\nPhysicians-only by year:\n")
phys_years <- dbGetQuery(con, "
  SELECT analysis_year, COUNT(*) as physicians
  FROM temporal_all_years_fixed_physicians_only
  GROUP BY analysis_year
  ORDER BY analysis_year
")
print(phys_years)

# Show taxonomy breakdown
cat("\nTaxonomy breakdown (primary taxonomy):\n")
taxonomy_dist <- dbGetQuery(con, "
  SELECT
    CASE
      WHEN taxonomy_1 LIKE '207V%' THEN 'OB/GYN (207V%)'
      WHEN taxonomy_1 = '390200000X' THEN 'Student (390200000X)'
      WHEN taxonomy_1 = '174400000X' THEN 'Specialist (174400000X)'
      ELSE 'Other'
    END as taxonomy_category,
    COUNT(*) as n
  FROM temporal_all_years_fixed
  GROUP BY taxonomy_category
  ORDER BY n DESC
")
print(taxonomy_dist)

dbDisconnect(con, shutdown = TRUE)

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("TAXONOMY UPDATE COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

}  # end if (sys.nframe() == 0L)
