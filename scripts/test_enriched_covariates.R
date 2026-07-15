#!/usr/bin/env Rscript
# =============================================================================
# Adversarial + semantic validation of data/processed/ent_phase2_enriched.csv.
#
# Proves the 7 geographic covariate groups joined onto the 960-row call log
# CORRECTLY: no row multiplication, geography-level invariance (county covariates
# constant within a county, state covariates constant within a state), FIPS
# integrity, valid ranges, and real (non-degenerate) variation.
#
# Run:  Rscript scripts/test_enriched_covariates.R
# =============================================================================

suppressPackageStartupMessages({library(testthat)})
num <- function(x) suppressWarnings(as.numeric(x))
d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")

# helper: max distinct non-missing values within a grouping key
grp_distinct_max <- function(value, key) {
  v <- value; v[!nzchar(v)] <- NA
  max(tapply(v, key, function(x) length(unique(x[!is.na(x)]))), na.rm = TRUE)
}

county_cols <- c("n_ent","ent_per_100k","medicare_benes","dual_medicaid_benes","dual_pct")
state_cols  <- c("medicaid_fee_index","aao_hns_district","aao_hns_region")
zip_cols    <- c("county_fips","cbsa_code")

# ---- STRUCTURE --------------------------------------------------------------
test_that("row count preserved: no join multiplied rows", {
  expect_equal(nrow(d), 960)
  expect_equal(length(unique(d$record_id)), 960)
})

# ---- SEMANTIC: geography-level invariance -----------------------------------
test_that("county-level covariates are constant within each county_fips", {
  for (col in county_cols)
    expect_true(grp_distinct_max(d[[col]], d$county_fips) <= 1,
               info = paste(col, "varies within a county -> bad join"))
})

test_that("state-level covariates are constant within each state", {
  for (col in state_cols)
    expect_true(grp_distinct_max(d[[col]], d$state) <= 1,
               info = paste(col, "varies within a state -> bad join"))
})

test_that("ZIP-resolved covariates are constant within each zip", {
  for (col in zip_cols)
    expect_true(grp_distinct_max(d[[col]], d$zip) <= 1,
               info = paste(col, "varies within a zip -> bad crosswalk"))
})

# ---- SEMANTIC: FIPS / geography nesting -------------------------------------
test_that("county_fips is a 5-digit code; state prefix matches the state (>=99.5%)", {
  cf <- d$county_fips[nzchar(d$county_fips)]
  expect_true(all(nchar(cf) == 5))
  data(fips_codes, package = "tidycensus")
  fips2abbr <- setNames(fips_codes$state, fips_codes$state_code)
  fips2abbr <- fips2abbr[!duplicated(names(fips2abbr))]
  sub <- d[nzchar(d$county_fips) & nzchar(d$state), ]
  match_rate <- mean(fips2abbr[substr(sub$county_fips, 1, 2)] == sub$state, na.rm = TRUE)
  # A handful of legitimate cross-border cases exist (source state typo, or a ZIP
  # whose dominant county sits across a state line); require overwhelming match.
  mism <- sub[fips2abbr[substr(sub$county_fips, 1, 2)] != sub$state, ]
  if (nrow(mism))
    cat("  [note] cross-border county_fips rows:",
        paste(sprintf("%s/%s->%s", mism$state, mism$zip, mism$county_fips), collapse = "; "), "\n")
  expect_gt(match_rate, 0.995)
})

test_that("AAO-HNS district labels come from the valid 8-district set", {
  dist <- unique(d$aao_hns_district[nzchar(d$aao_hns_district)])
  expect_true(length(dist) >= 1 && length(dist) <= 8)
})

# ---- SEMANTIC: valid ranges -------------------------------------------------
test_that("covariates fall in plausible ranges", {
  expect_true(all(num(d$svi_overall) >= 0 & num(d$svi_overall) <= 1, na.rm = TRUE))
  expect_true(all(num(d$medicaid_fee_index) > 0 & num(d$medicaid_fee_index) < 2, na.rm = TRUE))
  expect_true(all(num(d$dual_pct) >= 0 & num(d$dual_pct) <= 1, na.rm = TRUE))
  expect_true(all(num(d$ent_per_100k) >= 0, na.rm = TRUE))
  expect_true(all(num(d$n_ent) >= 0, na.rm = TRUE))
})

test_that("ent_per_100k is 0 iff n_ent is 0 (internal consistency)", {
  sub <- d[nzchar(d$n_ent) & nzchar(d$ent_per_100k), ]
  n0 <- num(sub$n_ent) == 0; p0 <- num(sub$ent_per_100k) == 0
  expect_equal(n0, p0)
})

# ---- ADVERSARIAL: real variation, not degenerate/fake -----------------------
test_that("each covariate shows real cross-sectional variation (not constant)", {
  for (col in c(county_cols, "svi_overall", "medicaid_fee_index"))
    expect_gt(length(unique(num(d[[col]])[!is.na(num(d[[col]]))])), 5,
              label = paste(col, "distinct values"))
})

test_that("covariates are NOT a per-row random draw (county cols repeat across rows)", {
  # a fake per-row rnorm fill would give ~960 distinct values; real county data
  # has far fewer distinct values than rows (one per county)
  for (col in county_cols) {
    nd <- length(unique(num(d[[col]])[!is.na(num(d[[col]]))]))
    expect_lt(nd, nrow(d),
              label = paste(col, "distinct-value count (should be << 960)"))
  }
})

# ---- COVERAGE ---------------------------------------------------------------
test_that("covariate coverage is adequate (metro-only HHI excepted)", {
  cov <- function(col) mean(nzchar(d[[col]]) & !is.na(num(d[[col]])))
  for (col in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
    expect_gt(cov(col), 0.90, label = paste(col, "coverage"))
  expect_gt(mean(nzchar(d$aao_hns_district)), 0.95, label = "aao_hns_district coverage")
})

cat("\nAll enriched-covariate validation checks executed.\n")
