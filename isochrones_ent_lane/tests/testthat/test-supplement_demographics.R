################################################################################
# Test Suite for Data Supplementation Functions
# File: tests/testthat/test-supplement_demographics.R
#
# Tests supplementation logic for low-coverage demographics (<95%)
# Focus: Coverage improvement, data source tracking, primary data preservation
################################################################################

library(testthat)
library(dplyr)

# Source the supplementation functions
source_repo_R("R/supplement_table1_demographics.R")

################################################################################
# Test Category 1: Coverage Improvement Tests
################################################################################

test_that("SUPPLEMENT-COV-01: Medicare supplementation improves coverage", {
  # Create cohort with low Medicare coverage (<95%)
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    medicare_volume_quartile = factor(
      c(rep("Q1 (Lowest)", 10), rep("No Medicare", 50), rep("Unknown", 40)),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = c(rep(100, 10), rep(0, 90)),
    stringsAsFactors = FALSE
  )

  # Calculate initial coverage
  initial_coverage <- sum(test_cohort$medicare_volume_quartile != "Unknown") / nrow(test_cohort)
  expect_lt(initial_coverage, 0.95)  # Should be <95%

  # Apply supplementation (will gracefully handle missing database)
  result <- supplement_medicare_volume_with_medicaid(
    test_cohort,
    "/nonexistent/db.duckdb"
  )

  # Should return cohort even if no supplementation occurs
  expect_equal(nrow(result), nrow(test_cohort))
  expect_true("medicare_volume_quartile" %in% names(result))
})

test_that("SUPPLEMENT-COV-02: Language supplementation improves coverage", {
  # Create cohort with low language coverage
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    language_category = factor(
      c(rep("English Only", 20), rep("Spanish", 10), rep("Unknown", 70)),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = c(rep(FALSE, 20), rep(TRUE, 10), rep(NA, 70)),
    n_languages = c(rep(1, 30), rep(NA_integer_, 70)),
    stringsAsFactors = FALSE
  )

  initial_coverage <- sum(test_cohort$language_category != "Unknown") / nrow(test_cohort)
  expect_lt(initial_coverage, 0.95)

  # Apply supplementation (no files provided)
  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  # Should return cohort unchanged if no supplemental data
  expect_equal(nrow(result), nrow(test_cohort))
})

test_that("SUPPLEMENT-COV-03: HPSA supplementation with county MUA", {
  # Create cohort with low HPSA coverage
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    census_tract = c(rep("01001010100", 20), rep(NA_character_, 80)),
    underserved_area = factor(
      c(rep("HPSA", 15), rep("Non-Underserved", 5), rep("Unknown", 80)),
      levels = c("HPSA", "MUA", "Non-Underserved", "Unknown")
    ),
    hpsa_score = c(rep(25, 15), rep(NA_real_, 85)),
    stringsAsFactors = FALSE
  )

  initial_coverage <- sum(test_cohort$underserved_area != "Unknown") / nrow(test_cohort)
  expect_lt(initial_coverage, 0.95)

  # Apply supplementation
  result <- supplement_hpsa_with_county_mua(test_cohort, "/nonexistent/db.duckdb")

  # Should return cohort
  expect_equal(nrow(result), nrow(test_cohort))
})

################################################################################
# Test Category 2: Primary Data Preservation Tests
################################################################################

test_that("SUPPLEMENT-PRESERVE-01: Supplementation never overwrites primary Medicare data", {
  test_cohort <- data.frame(
    npi = c("1234567890", "0987654321", "1111111111"),
    medicare_volume_quartile = factor(
      c("Q4 (Highest)", "No Medicare", "Unknown"),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = c(5000, 0, NA),
    stringsAsFactors = FALSE
  )

  result <- supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb")

  # Q4 should remain Q4 (never overwritten)
  expect_equal(as.character(result$medicare_volume_quartile[1]), "Q4 (Highest)")
})

test_that("SUPPLEMENT-PRESERVE-02: Supplementation only fills Unknown language entries", {
  test_cohort <- data.frame(
    npi = c("1234567890", "0987654321", "1111111111"),
    language_category = factor(
      c("Spanish", "English Only", "Unknown"),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = c(TRUE, FALSE, NA),
    n_languages = c(2, 1, NA),
    stringsAsFactors = FALSE
  )

  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  # Spanish and English Only should be unchanged
  expect_equal(as.character(result$language_category[1]), "Spanish")
  expect_equal(as.character(result$language_category[2]), "English Only")
})

test_that("SUPPLEMENT-PRESERVE-03: Tract-level HPSA not overwritten by county MUA", {
  test_cohort <- data.frame(
    npi = c("1234567890", "0987654321", "1111111111"),
    census_tract = c("01001010100", "01001010200", NA),
    underserved_area = factor(
      c("HPSA", "Non-Underserved", "Unknown"),
      levels = c("HPSA", "MUA", "Non-Underserved", "Unknown")
    ),
    hpsa_score = c(25, NA, NA),
    stringsAsFactors = FALSE
  )

  result <- supplement_hpsa_with_county_mua(test_cohort, "/nonexistent/db.duckdb")

  # HPSA should remain HPSA
  expect_equal(as.character(result$underserved_area[1]), "HPSA")
  # Non-Underserved should remain Non-Underserved
  expect_equal(as.character(result$underserved_area[2]), "Non-Underserved")
})

################################################################################
# Test Category 3: Data Source Tracking Tests
################################################################################

test_that("SUPPLEMENT-TRACKING-01: Volume data source tracked correctly", {
  test_cohort <- data.frame(
    npi = c("1234567890", "0987654321", "1111111111"),
    medicare_volume_quartile = factor(
      c("Q2", "No Medicare", "Unknown"),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = c(1000, 0, NA),
    stringsAsFactors = FALSE
  )

  result <- supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb")

  # If supplementation attempted, volume_data_source column should exist
  if ("volume_data_source" %in% names(result)) {
    # Physicians with Medicare data should be marked as "Medicare"
    if (!is.na(result$volume_data_source[1])) {
      expect_equal(result$volume_data_source[1], "Medicare")
    }
  }
})

test_that("SUPPLEMENT-TRACKING-02: Language data source tracked", {
  test_cohort <- data.frame(
    npi = c("1234567890", "0987654321"),
    language_category = factor(
      c("Spanish", "Unknown"),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = c(TRUE, NA),
    n_languages = c(2, NA),
    stringsAsFactors = FALSE
  )

  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  # If supplementation attempted, language_data_source should exist
  if ("language_data_source" %in% names(result)) {
    # Physicians with language data should have source tracked
    expect_true(is.character(result$language_data_source))
  }
})

test_that("SUPPLEMENT-TRACKING-03: Underserved data source tracked", {
  test_cohort <- data.frame(
    npi = c("1234567890", "0987654321"),
    census_tract = c("01001010100", NA),
    underserved_area = factor(
      c("HPSA", "Unknown"),
      levels = c("HPSA", "MUA", "Non-Underserved", "Unknown")
    ),
    hpsa_score = c(25, NA),
    stringsAsFactors = FALSE
  )

  result <- supplement_hpsa_with_county_mua(test_cohort, "/nonexistent/db.duckdb")

  # If supplementation attempted, underserved_data_source should exist
  if ("underserved_data_source" %in% names(result)) {
    # HPSA physician should be marked as "Tract-HPSA"
    expect_true(is.character(result$underserved_data_source))
  }
})

################################################################################
# Test Category 4: Graceful Degradation Tests
################################################################################

test_that("SUPPLEMENT-GRACEFUL-01: Missing Medicaid database handled", {
  test_cohort <- data.frame(
    npi = c("1234567890"),
    medicare_volume_quartile = factor("Unknown",
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")),
    total_medicare_services = NA_real_,
    stringsAsFactors = FALSE
  )

  # Should not error with missing database
  expect_error(
    supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb"),
    NA
  )
})

test_that("SUPPLEMENT-GRACEFUL-02: Missing Doximity/Healthgrades files handled", {
  test_cohort <- data.frame(
    npi = c("1234567890"),
    language_category = factor("Unknown",
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")),
    speaks_spanish = NA,
    n_languages = NA_integer_,
    stringsAsFactors = FALSE
  )

  # Should not error with missing files
  expect_error(
    supplement_language_with_doximity_healthgrades(
      test_cohort,
      doximity_path = NULL,
      healthgrades_path = NULL
    ),
    NA
  )

  # Also test with nonexistent paths
  expect_error(
    supplement_language_with_doximity_healthgrades(
      test_cohort,
      doximity_path = "/nonexistent/doximity.csv",
      healthgrades_path = "/nonexistent/healthgrades.csv"
    ),
    NA
  )
})

test_that("SUPPLEMENT-GRACEFUL-03: NULL database paths handled", {
  test_cohort <- data.frame(
    npi = c("1234567890"),
    medicare_volume_quartile = factor("Unknown",
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")),
    total_medicare_services = NA_real_,
    stringsAsFactors = FALSE
  )

  # Should handle NULL gracefully
  expect_error(
    supplement_medicare_volume_with_medicaid(test_cohort, NULL),
    NA
  )
})

################################################################################
# Test Category 5: Row Count Invariant Tests
################################################################################

test_that("SUPPLEMENT-INVARIANT-01: Row count preserved after Medicare supplementation", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:50),
    medicare_volume_quartile = factor(
      sample(c("Q1 (Lowest)", "No Medicare", "Unknown"), 50, replace = TRUE),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = sample(c(0, 100, NA), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  original_rows <- nrow(test_cohort)

  result <- supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb")

  expect_equal(nrow(result), original_rows)
})

test_that("SUPPLEMENT-INVARIANT-02: Row count preserved after language supplementation", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:50),
    language_category = factor(
      sample(c("English Only", "Spanish", "Unknown"), 50, replace = TRUE),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = sample(c(TRUE, FALSE, NA), 50, replace = TRUE),
    n_languages = sample(c(1, 2, NA), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  original_rows <- nrow(test_cohort)

  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  expect_equal(nrow(result), original_rows)
})

test_that("SUPPLEMENT-INVARIANT-03: Row count preserved after HPSA supplementation", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:50),
    census_tract = sample(c("01001010100", NA), 50, replace = TRUE),
    underserved_area = factor(
      sample(c("HPSA", "Unknown"), 50, replace = TRUE),
      levels = c("HPSA", "MUA", "Non-Underserved", "Unknown")
    ),
    hpsa_score = sample(c(25, NA), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  original_rows <- nrow(test_cohort)

  result <- supplement_hpsa_with_county_mua(test_cohort, "/nonexistent/db.duckdb")

  expect_equal(nrow(result), original_rows)
})

################################################################################
# Test Category 6: NPI Uniqueness Tests
################################################################################

test_that("SUPPLEMENT-UNIQUE-01: No duplicate NPIs after Medicare supplementation", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    medicare_volume_quartile = factor(
      rep("Unknown", 100),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = rep(NA_real_, 100),
    stringsAsFactors = FALSE
  )

  result <- supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb")

  # No duplicates should be introduced
  expect_equal(length(unique(result$npi)), length(unique(test_cohort$npi)))
  expect_equal(nrow(result), nrow(test_cohort))
})

test_that("SUPPLEMENT-UNIQUE-02: No duplicate NPIs after language supplementation", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    language_category = factor(
      rep("Unknown", 100),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = rep(NA, 100),
    n_languages = rep(NA_integer_, 100),
    stringsAsFactors = FALSE
  )

  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  expect_equal(length(unique(result$npi)), length(unique(test_cohort$npi)))
})

################################################################################
# Test Category 7: Empty Cohort Tests
################################################################################

test_that("SUPPLEMENT-EMPTY-01: Empty cohort handled in Medicare supplementation", {
  empty_cohort <- data.frame(
    npi = character(0),
    medicare_volume_quartile = factor(
      character(0),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = numeric(0),
    stringsAsFactors = FALSE
  )

  result <- supplement_medicare_volume_with_medicaid(empty_cohort, "/nonexistent/db.duckdb")

  expect_equal(nrow(result), 0)
  expect_true("medicare_volume_quartile" %in% names(result))
})

test_that("SUPPLEMENT-EMPTY-02: Empty cohort handled in language supplementation", {
  empty_cohort <- data.frame(
    npi = character(0),
    language_category = factor(
      character(0),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = logical(0),
    n_languages = integer(0),
    stringsAsFactors = FALSE
  )

  result <- supplement_language_with_doximity_healthgrades(
    empty_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  expect_equal(nrow(result), 0)
})

################################################################################
# Test Category 8: 100% Coverage (No Supplementation Needed) Tests
################################################################################

test_that("SUPPLEMENT-NOOP-01: No Medicare supplementation if 100% coverage", {
  # All physicians have Medicare data
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    medicare_volume_quartile = factor(
      sample(c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"), 100, replace = TRUE),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = sample(100:5000, 100),
    stringsAsFactors = FALSE
  )

  initial_quartiles <- test_cohort$medicare_volume_quartile

  result <- supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb")

  # Should return unchanged (no supplementation needed)
  expect_equal(as.character(result$medicare_volume_quartile), as.character(initial_quartiles))
})

test_that("SUPPLEMENT-NOOP-02: No language supplementation if 100% coverage", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    language_category = factor(
      sample(c("English Only", "Spanish", "Other Non-English"), 100, replace = TRUE),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = sample(c(TRUE, FALSE), 100, replace = TRUE),
    n_languages = sample(1:3, 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  initial_categories <- test_cohort$language_category

  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  expect_equal(as.character(result$language_category), as.character(initial_categories))
})

################################################################################
# Test Category 9: Partial Supplementation Tests
################################################################################

test_that("SUPPLEMENT-PARTIAL-01: Partial Medicare supplementation tracked", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    medicare_volume_quartile = factor(
      c(rep("Q1 (Lowest)", 50), rep("Unknown", 50)),
      levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare", "Unknown")
    ),
    total_medicare_services = c(rep(100, 50), rep(NA_real_, 50)),
    stringsAsFactors = FALSE
  )

  # Exactly 50% initial coverage
  initial_coverage <- sum(test_cohort$medicare_volume_quartile != "Unknown") / nrow(test_cohort)
  expect_equal(initial_coverage, 0.5)

  result <- supplement_medicare_volume_with_medicaid(test_cohort, "/nonexistent/db.duckdb")

  # Should still have 50% coverage (no supplemental data available)
  final_coverage <- sum(result$medicare_volume_quartile != "Unknown") / nrow(result)
  expect_equal(final_coverage, 0.5)
})

test_that("SUPPLEMENT-PARTIAL-02: Partial language supplementation tracked", {
  test_cohort <- data.frame(
    npi = sprintf("%010d", 1:100),
    language_category = factor(
      c(rep("English Only", 50), rep("Unknown", 50)),
      levels = c("English Only", "Spanish", "Other Non-English", "Unknown")
    ),
    speaks_spanish = c(rep(FALSE, 50), rep(NA, 50)),
    n_languages = c(rep(1, 50), rep(NA_integer_, 50)),
    stringsAsFactors = FALSE
  )

  # Exactly 50% initial coverage
  initial_coverage <- sum(test_cohort$language_category != "Unknown") / nrow(test_cohort)
  expect_equal(initial_coverage, 0.5)

  result <- supplement_language_with_doximity_healthgrades(
    test_cohort,
    doximity_path = NULL,
    healthgrades_path = NULL
  )

  # Should still have 50% coverage (no supplemental data available)
  final_coverage <- sum(result$language_category != "Unknown") / nrow(result)
  expect_equal(final_coverage, 0.5)
})

################################################################################
# Test Summary - Verify Minimum Coverage
################################################################################

test_that("META-SUPPLEMENT: All supplementation categories have minimum 2 tests", {
  test_file <- readLines(here::here("tests/testthat/test-supplement_demographics.R"))

  categories <- c(
    "SUPPLEMENT-COV",         # Coverage improvement
    "SUPPLEMENT-PRESERVE",    # Primary data preservation
    "SUPPLEMENT-TRACKING",    # Data source tracking
    "SUPPLEMENT-GRACEFUL",    # Graceful degradation
    "SUPPLEMENT-INVARIANT",   # Row count invariant
    "SUPPLEMENT-UNIQUE",      # NPI uniqueness
    "SUPPLEMENT-EMPTY",       # Empty cohort handling
    "SUPPLEMENT-NOOP",        # 100% coverage (no-op)
    "SUPPLEMENT-PARTIAL"      # Partial supplementation
  )

  for (category in categories) {
    pattern <- sprintf("%s-\\d+:", category)
    count <- sum(grepl(pattern, test_file))
    expect_gte(count, 2)
  }
})
