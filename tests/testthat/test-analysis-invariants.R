# Regression tests for the raw-vs-derived column confusion fixed in
# 03_analysis.R: appointment_with_md was being overwritten by a comparison of
# the RAW REDCap column (values "Yes"/"No-but...") against the DERIVED factor
# label "With sampled physician", which is never TRUE -- silently zeroing all
# 370 positive outcomes to FALSE. The derivation lives in 01 and nowhere else.
library(testthat)

complete_path <- tail(sort(list.files(here::here("data", "processed"),
                                      pattern = "^ent_phase2_complete_.*\\.csv$", full.names = TRUE)), 1)
df <- readr::read_csv(complete_path, show_col_types = FALSE)

test_that("appointment_with_md is exactly the appointment_outcome derivation", {
  non_missing <- !is.na(df$appointment_with_md)
  expect_equal(df$appointment_with_md[non_missing],
               df$appointment_outcome[non_missing] == "With sampled physician")
  # NA in appointment_with_md only where the outcome itself is missing
  expect_true(all(is.na(df$appointment_outcome[is.na(df$appointment_with_md)])))
})

test_that("raw and derived appointment columns never share a vocabulary", {
  # The trap itself: the raw REDCap values must never include the derived
  # labels, or a raw == derived-label comparison would silently go stale
  # rather than obviously returning all-FALSE.
  raw_vals <- unique(df$appointment_with_physician[!is.na(df$appointment_with_physician)])
  derived_vals <- unique(df$appointment_outcome[!is.na(df$appointment_outcome)])
  expect_length(intersect(raw_vals, derived_vals), 0)
  expect_true("Yes" %in% raw_vals)   # the raw positive outcome is "Yes", not the derived label
})

test_that("the analysis sample has positive appointment_with_md outcomes to analyse", {
  # If the overwrite regression ever returns, this count collapses to zero.
  expect_gte(sum(df$appointment_with_md %in% TRUE), 300L)
})
