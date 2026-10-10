# The mysteryroster rewire must reproduce the legacy 05_database_accuracy_paper.R
# tables committed on 2026-07-20 exactly. Byte-level parity on counts; percents
# to the two decimals the legacy script wrote.
library(testthat)
source(here::here("scripts", "05b_validity_mysteryroster.R"))
res <- validity_from_call_log()
legacy <- function(f) readr::read_csv(here::here("output", "tables", f), show_col_types = FALSE)

test_that("listing_status counts and percents match table3a_listing_status.csv", {
  l <- legacy("table3a_listing_status.csv")
  got <- res$status_table |> dplyr::mutate(listing_status = as.character(listing_status))
  expect_equal(got$listing_status, l$listing_status)
  expect_equal(got$n, l$n)                       # 445 / 138 / 169 / 208
  expect_equal(got$percent, l$percent, tolerance = 0.011)   # 14.375 rounds to .37 or .38 by path; counts are exact
})

test_that("sensitivity bounds match sensitivity_bounds.csv", {
  l <- legacy("sensitivity_bounds.csv")
  expect_equal(res$bounds$denominator, l$denominator)      # 583 / 752 / 752
  expect_equal(res$bounds$n_valid, l$n_valid_cnt)          # 445 / 614 / 445
  expect_equal(round(res$bounds$pct_valid, 1), l$pct_valid) # 76.3 / 81.6 / 59.2
})

test_that("per-zone cascade counts match table2_cascade.csv", {
  l <- legacy("table2_cascade.csv") |> dplyr::arrange(overlap_zone)
  got <- res$cascade_by_zone |> dplyr::arrange(overlap_zone)
  expect_equal(got$overlap_zone, l$overlap_zone)
  for (v in c("n_total", "n_reached", "n_at_location", "n_active", "n_new_patients")) expect_equal(got[[v]], l[[v]], info = v)
})

test_that("design weights are N / n per sampled stratum (6748/498, 4573/462)", {
  w <- unique(res$calls[!is.na(res$calls$stratum), c("stratum", "n_sampled", "weight")])
  expect_equal(w$n_sampled[w$stratum == "NPPES+ABOto"], 498L)
  expect_equal(w$n_sampled[w$stratum == "NPPES+ABOto+ENTHealth"], 462L)
  expect_equal(w$weight[w$stratum == "NPPES+ABOto"], 6748 / 498)
  expect_equal(w$weight[w$stratum == "NPPES+ABOto+ENTHealth"], 4573 / 462)
})

test_that("invalid reasons match table3b_invalid_reasons.csv where the legacy table reports them", {
  l <- legacy("table3b_invalid_reasons.csv")
  got <- res$calls |> dplyr::filter(listing_status == "confirmed_invalid") |> dplyr::count(invalid_reason, name = "n")
  for (i in seq_len(nrow(l))) {
    expect_equal(got$n[got$invalid_reason == l[[1]][i]], l$n[i], info = as.character(l[[1]][i]))
  }
})
