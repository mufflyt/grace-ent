# Regression tests for LL41-LL50 in scripts/build_rural_ent_leaflet.R
suppressPackageStartupMessages({ library(testthat); library(here) })

.src <- paste(readLines(here("scripts", "build_rural_ent_leaflet.R"), warn = FALSE),
              collapse = "\n")

test_that("[LL-41] read_csv wrapped in tryCatch", {
  expect_true(grepl("read_csv failed for cohort", .src, fixed = TRUE),
              info = "LL41: read_csv must surface path-named error")
})

test_that("[LL-42] read_parquet wrapped in tryCatch", {
  expect_true(grepl("read_parquet failed for NPPES", .src, fixed = TRUE),
              info = "LL42: read_parquet must surface path-named error")
})

test_that("[LL-43] fmt_loc uses .blank helper for empty/NA/whitespace handling", {
  expect_true(grepl("\\.blank <- function", .src, perl = TRUE),
              info = "LL43: must use trimws-based blank check")
  expect_true(grepl("!nzchar\\(trimws\\(\\.v\\)\\)", .src, perl = TRUE),
              info = "LL43: must check non-whitespace via nzchar(trimws())")
})

test_that("[LL-44] tigris::zctas wrapped in tryCatch", {
  expect_true(grepl("tigris::zctas\\(2020\\) failed", .src, perl = TRUE),
              info = "LL44: tigris::zctas must surface remediation")
})

test_that("[LL-45] HTML tmp path uses host+PID+nanosecond entropy", {
  expect_true(grepl('Sys\\.info\\(\\)\\[\\["nodename"\\]\\]', .src, perl = TRUE),
              info = "LL45: tmp_html must include nodename entropy")
  expect_true(grepl("saveWidget failed", .src, fixed = TRUE),
              info = "LL45: saveWidget must be wrapped")
})

test_that("[LL-46] file.rename return value checked + file.copy fallback", {
  expect_true(grepl("\\.rn_ok <- isTRUE\\(file\\.rename", .src, perl = TRUE),
              info = "LL46: file.rename return value must be captured")
  expect_true(grepl("file\\.rename and file\\.copy failed", .src, perl = TRUE),
              info = "LL46: must error if both rename and copy fail")
})

test_that("[LL-47] file.info()$size NA guard added", {
  expect_true(grepl("file\\.info\\(\\)\\$size=NA for", .src, perl = TRUE),
              info = "LL47: must warn explicitly on NA file.info()$size")
})

test_that("[LL-48] receipt tmp path uses host+PID+nanosecond entropy", {
  # The pattern appears twice in this file (LL45 HTML + LL48 receipt) —
  # require at least 2 occurrences.
  .n_occur <- sum(grepl('Sys\\.info\\(\\)\\[\\["nodename"\\]\\]',
                         strsplit(.src, "\n")[[1]], perl = TRUE))
  expect_true(.n_occur >= 2L,
              info = "LL48: at least two host-namespaced tmp paths must exist (HTML + receipt)")
})

test_that("[LL-49] in_conus coerces lat/lon via as.numeric", {
  expect_true(grepl("lat_n <- suppressWarnings\\(as\\.numeric\\(lat\\)\\)", .src, perl = TRUE),
              info = "LL49: must coerce lat to numeric")
  expect_true(grepl("lon_n <- suppressWarnings\\(as\\.numeric\\(lon\\)\\)", .src, perl = TRUE),
              info = "LL49: must coerce lon to numeric")
})

test_that("[LL-50] OUT_DIR is created if missing", {
  expect_true(grepl("Failed to create OUT_DIR", .src, fixed = TRUE),
              info = "LL50: must dir.create OUT_DIR with error path")
})
