#!/usr/bin/env Rscript
# =============================================================================
# Create PPEF Enrollment Status Consolidated Table
# =============================================================================
#
# PURPOSE: Build credentials.ppef_enrollment_status from individual year tables
#
# COVERAGE: 2016, 2017, 2018, 2019, 2023Q3, 2024Q3, 2025 (7 years)
#          Provides continuous 2016-2019 coverage after 2018 integration
#
# RATIONALE: Converted from VIEW to TABLE to resolve schema qualification bug
#            when accessing through database aliases (main_db.credentials.ppef_enrollment_status)
#
# DATA SOURCE: NBER PPEF repository (https://data.nber.org/ppef/)
#
# DATE: 2026-01-10
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(here)
})
source(here::here("R", "duckdb_safe.R"))

cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("Creating PPEF Enrollment Status Consolidated Table\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Connect to Database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13)
db_path <- load_paths(verbose = FALSE)$nber_duckdb_main

if (!file.exists(db_path)) {
  stop(sprintf("Database not found: %s", db_path))
}

cat(sprintf("[1/4] Connecting to database: %s\n", basename(db_path)))
con <- safe_duckdb(db_path, read_only = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Verify Source Tables Exist
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat("[2/4] Verifying source tables exist...\n")

required_tables <- c(
  "ppef_enrol_2016",
  "ppef_enrol_2017",
  "ppef_enrol_2018",
  "ppef_enrol_2019",
  "ppef_enrol_2023Q3",
  "ppef_enrol_2024Q3",
  "ppef_enrol_2025"
)

for (tbl in required_tables) {
  query <- sprintf("SELECT COUNT(*) as n FROM main.%s LIMIT 1", tbl)
  tryCatch({
    result <- dbGetQuery(con, query)
    cat(sprintf("      ✓ %s exists (%s rows)\n", tbl,
               format(result$n, big.mark = ",")))
  }, error = function(e) {
    dbDisconnect(con, shutdown = TRUE)
    stop(sprintf("Required table missing: main.%s\n%s", tbl, e$message))
  })
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Create credentials Schema (if needed)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat("[3/4] Ensuring credentials schema exists...\n")
dbExecute(con, "CREATE SCHEMA IF NOT EXISTS credentials")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Build PPEF Enrollment Status Table
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat("[4/4] Building credentials.ppef_enrollment_status table...\n")

# Drop existing table if present
dbExecute(con, "DROP TABLE IF EXISTS credentials.ppef_enrollment_status")

create_query <- "
CREATE TABLE credentials.ppef_enrollment_status AS
WITH ppef_snapshots AS (
  -- Annual snapshots (integer years)
  SELECT CAST(NPI AS VARCHAR) AS npi, 2016.0 AS snapshot_year, 'annual' AS snapshot_type
  FROM main.ppef_enrol_2016

  UNION ALL

  SELECT CAST(NPI AS VARCHAR) AS npi, 2017.0 AS snapshot_year, 'annual' AS snapshot_type
  FROM main.ppef_enrol_2017

  UNION ALL

  SELECT CAST(NPI AS VARCHAR) AS npi, 2018.0 AS snapshot_year, 'annual' AS snapshot_type
  FROM main.ppef_enrol_2018

  UNION ALL

  SELECT CAST(NPI AS VARCHAR) AS npi, 2019.0 AS snapshot_year, 'annual' AS snapshot_type
  FROM main.ppef_enrol_2019

  UNION ALL

  -- Phase 1.2: Quarterly snapshots with fractional years (Q3 = .75)
  SELECT CAST(NPI AS VARCHAR) AS npi, 2023.75 AS snapshot_year, 'Q3' AS snapshot_type
  FROM main.ppef_enrol_2023Q3

  UNION ALL

  SELECT CAST(NPI AS VARCHAR) AS npi, 2024.75 AS snapshot_year, 'Q3' AS snapshot_type
  FROM main.ppef_enrol_2024Q3

  UNION ALL

  SELECT CAST(NPI AS VARCHAR) AS npi, 2025.0 AS snapshot_year, 'annual' AS snapshot_type
  FROM main.ppef_enrol_2025
),
ppef_with_max AS (
  SELECT
    npi,
    snapshot_year,
    snapshot_type,
    MAX(snapshot_year) OVER (PARTITION BY npi) AS max_snapshot_year
  FROM ppef_snapshots
),
npi_enrollment_summary AS (
  SELECT
    npi,
    -- Keep integer year for backward compatibility
    CAST(FLOOR(MAX(snapshot_year)) AS INTEGER) AS last_enrollment_year,
    CAST(FLOOR(MIN(snapshot_year)) AS INTEGER) AS first_enrollment_year,
    COUNT(DISTINCT snapshot_year) AS years_enrolled,
    BOOL_OR(snapshot_year >= 2024) AS enrolled_recently,
    (MAX(snapshot_year) < 2025 AND MIN(snapshot_year) <= 2023) AS likely_disenrolled,

    -- Phase 1.2: NEW - Fractional year for quarterly resolution
    MAX(snapshot_year) AS last_enrollment_year_refined,

    -- Phase 1.2: NEW - Get snapshot type of the last enrollment year
    MAX(CASE WHEN snapshot_year = max_snapshot_year THEN snapshot_type END) AS last_snapshot_type
  FROM ppef_with_max
  GROUP BY npi
)
SELECT * FROM npi_enrollment_summary
"

dbExecute(con, create_query)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4b. Create PPEF Snapshot By-Year Sibling (ponytail finding #10, 2026-10-09)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# credentials.ppef_enrollment_status above collapses straight to one row per
# NPI, with enrolled_recently/likely_disenrolled computed against HARDCODED
# calendar years (2024, 2025) baked in at build time. A historical Step 1.8
# run asking "was this physician enrolled as of 2017?" has no way to recover
# that answer from the collapsed table -- the per-snapshot grain is gone.
# This sibling keeps (npi, snapshot_year, snapshot_type) so
# R/retirement_sql_builder.R::build_base_cte() can re-derive
# enrolled_recently/likely_disenrolled at query time, bounded by
# current_year, instead of reading the pre-collapsed all-time table. Purely
# additive -- ppef_enrollment_status itself is untouched, so every existing
# consumer of its schema is unaffected.

cat("\n[4b] Creating ppef_snapshot_by_year (per-NPI-per-snapshot grain)...\n")
dbExecute(con, "DROP TABLE IF EXISTS credentials.ppef_snapshot_by_year")

by_year_query <- "
CREATE TABLE credentials.ppef_snapshot_by_year AS
SELECT CAST(NPI AS VARCHAR) AS npi, 2016.0 AS snapshot_year, 'annual' AS snapshot_type
FROM main.ppef_enrol_2016

UNION ALL

SELECT CAST(NPI AS VARCHAR) AS npi, 2017.0 AS snapshot_year, 'annual' AS snapshot_type
FROM main.ppef_enrol_2017

UNION ALL

SELECT CAST(NPI AS VARCHAR) AS npi, 2018.0 AS snapshot_year, 'annual' AS snapshot_type
FROM main.ppef_enrol_2018

UNION ALL

SELECT CAST(NPI AS VARCHAR) AS npi, 2019.0 AS snapshot_year, 'annual' AS snapshot_type
FROM main.ppef_enrol_2019

UNION ALL

SELECT CAST(NPI AS VARCHAR) AS npi, 2023.75 AS snapshot_year, 'Q3' AS snapshot_type
FROM main.ppef_enrol_2023Q3

UNION ALL

SELECT CAST(NPI AS VARCHAR) AS npi, 2024.75 AS snapshot_year, 'Q3' AS snapshot_type
FROM main.ppef_enrol_2024Q3

UNION ALL

SELECT CAST(NPI AS VARCHAR) AS npi, 2025.0 AS snapshot_year, 'annual' AS snapshot_type
FROM main.ppef_enrol_2025
"

dbExecute(con, by_year_query)
cat("  ppef_snapshot_by_year created successfully!\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Verify Table Created Successfully
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stats <- dbGetQuery(con, "
  SELECT
    COUNT(*) as total_npis,
    MIN(first_enrollment_year) as min_year,
    MAX(last_enrollment_year) as max_year,
    AVG(years_enrolled) as avg_years_enrolled,
    COUNT(*) FILTER (WHERE enrolled_recently) as enrolled_recently_count,
    COUNT(*) FILTER (WHERE likely_disenrolled) as likely_disenrolled_count,
    -- Phase 1.2: NEW - Quarterly resolution stats
    COUNT(*) FILTER (WHERE last_snapshot_type = 'Q3') as quarterly_snapshot_count,
    MIN(last_enrollment_year_refined) as min_refined_year,
    MAX(last_enrollment_year_refined) as max_refined_year,
    COUNT(*) FILTER (WHERE last_enrollment_year_refined <> FLOOR(last_enrollment_year_refined)) as fractional_year_count
  FROM credentials.ppef_enrollment_status
")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")
cat("PPEF ENROLLMENT STATUS TABLE CREATED\n")
cat("═══════════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("Total unique NPIs:           %s\n", format(stats$total_npis, big.mark = ",")))
cat(sprintf("Coverage span:               %d-%d\n", stats$min_year, stats$max_year))
cat(sprintf("Average years enrolled:      %.1f\n", stats$avg_years_enrolled))
cat(sprintf("Enrolled recently (2024+):   %s (%.1f%%)\n",
           format(stats$enrolled_recently_count, big.mark = ","),
           100 * stats$enrolled_recently_count / stats$total_npis))
cat(sprintf("Likely disenrolled:          %s (%.1f%%)\n",
           format(stats$likely_disenrolled_count, big.mark = ","),
           100 * stats$likely_disenrolled_count / stats$total_npis))
cat(sprintf("\nPhase 1.2: Quarterly Resolution:\n"))
cat(sprintf("  Q3 snapshots (latest):     %s (%.1f%%)\n",
           format(stats$quarterly_snapshot_count, big.mark = ","),
           100 * stats$quarterly_snapshot_count / stats$total_npis))
cat(sprintf("  Fractional year precision: %s (%.1f%%)\n",
           format(stats$fractional_year_count, big.mark = ","),
           100 * stats$fractional_year_count / stats$total_npis))
cat(sprintf("  Refined year range:        %.2f-%.2f\n",
           stats$min_refined_year, stats$max_refined_year))

cat("\n")
cat("✅ Table credentials.ppef_enrollment_status is ready for retirement detection\n")
cat("═══════════════════════════════════════════════════════════════════════════\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Disconnect
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dbDisconnect(con, shutdown = TRUE)
