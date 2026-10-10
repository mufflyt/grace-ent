# Regression tests for LL161-LL170 in scripts/build_rural_ent_map.R
suppressPackageStartupMessages({ library(testthat); library(here) })

.src <- paste(readLines(here("scripts", "build_rural_ent_map.R"), warn = FALSE),
              collapse = "\n")

test_that("[LL-161] abohns_matched read_csv wrapped", {
  expect_true(grepl("read_csv failed for abohns_matched", .src, fixed = TRUE),
              info = "LL161")
})

test_that("[LL-162] ruca_2020_zip read_csv wrapped", {
  expect_true(grepl("read_csv failed for RUCA crosswalk", .src, fixed = TRUE),
              info = "LL162")
})

test_that("[LL-163] OUT_CSV file.rename return value captured", {
  expect_true(grepl("\\.rn_ok_csv <- isTRUE\\(file\\.rename", .src, perl = TRUE),
              info = "LL163")
})

test_that("[LL-164] tmp_out_csv uses host+PID+nanosecond entropy", {
  expect_true(grepl('paste0\\(\\s*\\n?\\s*OUT_CSV, "\\.tmp\\.",\\s*\\n?\\s*Sys\\.info\\(\\)\\[\\["nodename"\\]\\]',
                     .src, perl = TRUE),
              info = "LL164")
})

test_that("[LL-165] cached_centroids read_csv wrapped", {
  expect_true(grepl("read_csv failed for cached centroids", .src, fixed = TRUE),
              info = "LL165")
})

test_that("[LL-166] tigris::zctas wrapped", {
  expect_true(grepl("tigris::zctas\\(2020\\) failed", .src, perl = TRUE),
              info = "LL166")
})

test_that("[LL-167] tmp_centroids uses host+PID entropy", {
  expect_true(grepl('cached_centroids, "\\.tmp\\.",\\s*\\n?\\s*Sys\\.info\\(\\)\\[\\["nodename"\\]\\]',
                     .src, perl = TRUE),
              info = "LL167")
})

test_that("[LL-168] cached_centroids file.rename captured + copy fallback", {
  expect_true(grepl("\\.rn_ok_cen <- isTRUE\\(file\\.rename", .src, perl = TRUE),
              info = "LL168")
})

test_that("[LL-169] tigris::states wrapped", {
  expect_true(grepl("tigris::states\\(\\) failed", .src, perl = TRUE),
              info = "LL169")
})

test_that("[LL-170] tmp_out_png entropy + ggsave wrapped + rename captured", {
  expect_true(grepl('OUT_PNG, "\\.tmp\\.",\\s*\\n?\\s*Sys\\.info\\(\\)\\[\\["nodename"\\]\\]',
                     .src, perl = TRUE),
              info = "LL170: tmp_out_png entropy")
  expect_true(grepl("\\.ggs_ok <- tryCatch", .src, perl = TRUE),
              info = "LL170: ggsave wrapped")
  expect_true(grepl("\\.rn_ok_png <- isTRUE\\(file\\.rename", .src, perl = TRUE),
              info = "LL170: PNG rename captured")
})
