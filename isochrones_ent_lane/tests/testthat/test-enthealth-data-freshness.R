# tests/testthat/test-enthealth-data-freshness.R
#
# Verifies that:
#   1.  The source enthealth XLSX exists and was scraped recently
#       (≤ 120 days old — covers "1-2 months ago" plus a generous buffer)
#   2.  The XLSX has the expected number of rows and required columns
#   3.  The pipeline output (enthealth_npi_LATEST.csv) exists, is recent,
#       and contains known matched physicians with valid NPIs
#   4.  No duplicate enthealth_id values appear in the pipeline output
#
# Skip all tests gracefully when the Dropbox source is absent (CI, other
# machines without external drive).  The pipeline itself will catch missing
# files at run-time via load_enthealth_data().

library(testthat)
library(readr)
library(dplyr)

# ── Paths ─────────────────────────────────────────────────────────────────────

XLSX_PATH <- file.path(
  path.expand("~"), "Dropbox (Personal)",
  "mystery_shopper", "Corbi study", "ENT", "Data", "results.xlsx"
)
LATEST_CSV <- here::here("data", "ent", "enthealth_npi_LATEST.csv")

# Maximum acceptable age of the source scrape in days
MAX_SCRAPE_AGE_DAYS <- 120L

# Expected national row count — enthealth.org listed 7,911 ENT physicians as of
# the Feb 2026 scrape.  Allow ±5% for future scrape drift.
EXPECTED_NATIONAL_ROWS_MIN <- 7500L
EXPECTED_NATIONAL_ROWS_MAX <- 8400L

# Required columns in the raw XLSX
REQUIRED_XLSX_COLS <- c(
  "url", "full_name", "address", "city", "state_code", "post_code",
  "phone_number", "specialty_primary"
)

# Known physicians verified in enthealth_npi_LATEST.csv (NJ test-mode run,
# stable across pipeline re-runs because they have unique names + NPI 10-digit).
# Format: list(enthealth_id, npi, first_clean, last_clean, state)
KNOWN_PHYSICIANS <- list(
  list(id = "EH_8317c82213a5", npi = "1003088675",  first = "CHRISTOPHER", last = "SHAARI",    state = "NJ"),
  list(id = "EH_bffd8d7970b7", npi = "1013940717",  first = "SCOTT",       last = "SCHAFFER",  state = "NJ"),
  list(id = "EH_224fb3859fac", npi = "1023129509",  first = "BASIL",       last = "PAPARONE",  state = "NJ"),
  list(id = "EH_0207fee994ca", npi = "1023438199",  first = "SUHYLA",      last = "ALAM",      state = "NJ"),
  list(id = "EH_4d5702268f68", npi = "1033156286",  first = "DAVID",       last = "BORTNIKER", state = "NJ"),
  list(id = "EH_03b4f3b07121", npi = "1033387766",  first = "MICHAEL",     last = "CHEE",      state = "NJ")
)

# ── Helper ────────────────────────────────────────────────────────────────────

.file_age_days <- function(path) {
  as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))
}

# ── Tests: source XLSX ────────────────────────────────────────────────────────

test_that("enthealth source XLSX exists at expected Dropbox path", {
  skip_if(!file.exists(XLSX_PATH), "Dropbox source not mounted — skipping")
  expect_true(file.exists(XLSX_PATH),
    info = paste("Expected:", XLSX_PATH))
})

test_that("enthealth XLSX was scraped within the last 120 days", {
  skip_if(!file.exists(XLSX_PATH), "Dropbox source not mounted — skipping")
  age <- .file_age_days(XLSX_PATH)
  expect_lte(age, MAX_SCRAPE_AGE_DAYS,
    label = sprintf("XLSX age (%.0f days) must be ≤ %d days", age, MAX_SCRAPE_AGE_DAYS))
})

test_that("enthealth XLSX has expected national row count (~7,911)", {
  skip_if(!file.exists(XLSX_PATH), "Dropbox source not mounted — skipping")
  suppressPackageStartupMessages(library(readxl))
  df <- readxl::read_excel(XLSX_PATH, col_types = "text")
  n <- nrow(df)
  expect_gte(n, EXPECTED_NATIONAL_ROWS_MIN,
    label = sprintf("Row count %d must be ≥ %d", n, EXPECTED_NATIONAL_ROWS_MIN))
  expect_lte(n, EXPECTED_NATIONAL_ROWS_MAX,
    label = sprintf("Row count %d must be ≤ %d", n, EXPECTED_NATIONAL_ROWS_MAX))
})

test_that("enthealth XLSX contains required columns", {
  skip_if(!file.exists(XLSX_PATH), "Dropbox source not mounted — skipping")
  suppressPackageStartupMessages(library(readxl))
  df <- readxl::read_excel(XLSX_PATH, col_types = "text", n_max = 1L)
  missing_cols <- setdiff(REQUIRED_XLSX_COLS, names(df))
  expect_length(missing_cols, 0L)
  if (length(missing_cols) > 0) fail(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
})

test_that("enthealth XLSX has no blank full_name values", {
  skip_if(!file.exists(XLSX_PATH), "Dropbox source not mounted — skipping")
  suppressPackageStartupMessages(library(readxl))
  df <- readxl::read_excel(XLSX_PATH, col_types = "text")
  n_blank <- sum(is.na(df$full_name) | trimws(df$full_name) == "")
  expect_lte(n_blank, floor(nrow(df) * 0.01),
    label = sprintf("%d rows (%.1f%%) have blank full_name — threshold is 1%%",
                    n_blank, 100 * n_blank / nrow(df)))
})

test_that("enthealth XLSX has no blank state_code values", {
  skip_if(!file.exists(XLSX_PATH), "Dropbox source not mounted — skipping")
  suppressPackageStartupMessages(library(readxl))
  df <- readxl::read_excel(XLSX_PATH, col_types = "text")
  n_blank <- sum(is.na(df$state_code) | trimws(df$state_code) == "")
  expect_lte(n_blank, floor(nrow(df) * 0.02),
    label = sprintf("%d rows (%.1f%%) have blank state_code — threshold is 2%%",
                    n_blank, 100 * n_blank / nrow(df)))
})

# ── Tests: pipeline output CSV ────────────────────────────────────────────────

test_that("enthealth_npi_LATEST.csv exists", {
  skip_if(!file.exists(LATEST_CSV),
    "enthealth_npi_LATEST.csv not present — run: SPECIALTY_ID=ENT Rscript R/canonical_specialty_npi_pipeline.R")
  succeed()
})

test_that("enthealth_npi_LATEST.csv was produced recently (≤ 120 days)", {
  skip_if(!file.exists(LATEST_CSV), "LATEST CSV missing — skipping freshness check")
  age <- .file_age_days(LATEST_CSV)
  expect_lte(age, MAX_SCRAPE_AGE_DAYS,
    label = sprintf("LATEST CSV age (%.0f days) must be ≤ %d days", age, MAX_SCRAPE_AGE_DAYS))
})

test_that("enthealth_npi_LATEST.csv has required output columns", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE, n_max = 1L)
  required <- c("enthealth_id", "npi", "first_clean", "last_clean",
                "state", "confidence_score", "match_strategy", "ent_subspecialty")
  missing <- setdiff(required, names(df))
  expect_length(missing, 0L)
  if (length(missing) > 0) fail(paste("Missing output columns:", paste(missing, collapse = ", ")))
})

test_that("enthealth_npi_LATEST.csv has no duplicate enthealth_id values", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
  matched <- df[!is.na(df$npi), ]
  dupes <- matched |>
    dplyr::count(enthealth_id) |>
    dplyr::filter(n > 1L)
  expect_equal(nrow(dupes), 0L,
    label = sprintf("%d enthealth_id values appear more than once in matched rows",
                    nrow(dupes)))
})

test_that("enthealth_npi_LATEST.csv has no duplicate NPI values in matched rows", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
  matched <- df[!is.na(df$npi), ]
  dupes <- matched |>
    dplyr::count(npi) |>
    dplyr::filter(n > 1L)
  expect_equal(nrow(dupes), 0L,
    label = sprintf("%d NPI values appear more than once in matched rows",
                    nrow(dupes)))
})

test_that("all matched NPI values are valid 10-digit numbers", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
  matched_npi <- df$npi[!is.na(df$npi)]
  invalid <- sum(!grepl("^[0-9]{10}$", as.character(matched_npi)))
  expect_equal(invalid, 0L,
    label = sprintf("%d matched NPIs fail the 10-digit format check", invalid))
})

test_that("confidence_score values are all in [0, 1] for matched rows", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
  matched <- df[!is.na(df$npi), ]
  out_of_range <- sum(
    is.na(matched$confidence_score) |
    matched$confidence_score < 0 |
    matched$confidence_score > 1,
    na.rm = FALSE
  )
  expect_equal(out_of_range, 0L,
    label = sprintf("%d rows have confidence_score outside [0,1]", out_of_range))
})

test_that("known NJ physicians from Feb 2026 scrape appear with correct NPIs", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE,
                        col_types = readr::cols(npi = readr::col_character()))

  for (p in KNOWN_PHYSICIANS) {
    id_rows <- df[df$enthealth_id == p$id, ]
    expect_gt(nrow(id_rows), 0L,
      label = sprintf("enthealth_id %s (%s %s) not found in LATEST CSV", p$id, p$first, p$last))

    if (nrow(id_rows) == 0L) next

    row <- id_rows[1L, ]

    expect_equal(as.character(row$npi[[1L]]), p$npi,
      label = sprintf("%s %s (id=%s): expected NPI %s, got %s",
                      p$first, p$last, p$id, p$npi,
                      as.character(row$npi[[1L]])))

    expect_equal(row$first_clean[[1L]], p$first,
      label = sprintf("first_clean mismatch for id=%s", p$id))

    expect_equal(row$last_clean[[1L]], p$last,
      label = sprintf("last_clean mismatch for id=%s", p$id))

    expect_equal(row$state[[1L]], p$state,
      label = sprintf("state mismatch for id=%s", p$id))
  }
})

test_that("ent_subspecialty column contains only known ENT subspecialty codes or NA", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
  known_codes <- c(
    "GEN_ENT", "FACIAL_PLAST", "H_N_ONCO", "PEDS_ENT",
    "OTOL", "LARYN", "RHINO", "SLEEP", "OTHER_ENT", NA_character_
  )
  unexpected <- setdiff(unique(df$ent_subspecialty), known_codes)
  expect_length(unexpected, 0L)
  if (length(unexpected) > 0) fail(paste("Unknown ent_subspecialty codes:", paste(unexpected, collapse = ", ")))
})

test_that("state column contains only valid 2-letter US abbreviations", {
  skip_if(!file.exists(LATEST_CSV))
  df <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
  valid_states <- c(
    "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
    "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
    "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
    "VT","VA","WA","WV","WI","WY", NA_character_
  )
  bad_states <- setdiff(unique(df$state), valid_states)
  expect_length(bad_states, 0L)
  if (length(bad_states) > 0) fail(paste("Unexpected state values:", paste(bad_states, collapse = ", ")))
})
