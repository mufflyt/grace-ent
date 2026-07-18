# Boundary-value analysis, part 2: covariate subset/derivation edges, categorical
# domain sizes, concentration-index bounds, pinned regression boundaries, and the
# strict interiors of confidence intervals and p-values.

num <- function(x) suppressWarnings(as.numeric(x))

test_that("dual-eligibles are a subset of Medicare beneficiaries (nesting bound)", {
  d  <- rd_chr("data/processed/ent_phase2_enriched.csv")
  mb <- num(d$medicare_benes); dm <- num(d$dual_medicaid_benes)
  expect_true(all(dm <= mb, na.rm = TRUE))                 # dual count never exceeds Medicare count
  dp <- num(d$dual_pct)
  expect_true(all(dp >= 0 & dp <= 1, na.rm = TRUE))        # derived share stays a proportion
})

test_that("ENT counts are non-negative integers (lower count boundary)", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  n <- num(d$n_ent)
  expect_gte(min(n, na.rm = TRUE), 0)
  expect_true(all(n == round(n), na.rm = TRUE))
})

test_that("Herfindahl-Hirschman index stays within its 0-10000 scale", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  h <- num(d$hhi_2024)
  hv <- h[!is.na(h)]
  expect_gt(min(hv), 0)
  expect_lte(max(hv), 10000)          # 10000 = single-provider (monopoly) upper bound
})

test_that("the subspecialty domain has exactly eight non-degenerate categories", {
  comp <- analytic()
  tab <- table(comp$ent_type)
  expect_equal(length(tab), 8L)
  expect_gte(min(tab), 1L)            # no empty category
  expect_gt(min(tab), 5L)             # smallest cell still supports estimation
})

test_that("maximum wait is pinned at 160 days (record-470 outlier stays removed)", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  w <- num(d$wait_days_business)
  expect_equal(max(w, na.rm = TRUE), 160)   # regression boundary: not the 774-day typo
})

test_that("appointments in the timeliness sample fall in a plausible future window", {
  comp <- analytic()
  w  <- num(comp$wait_days_business)
  ok <- !is.na(w)
  ad <- as.Date(comp$appointment_date[ok])
  expect_true(all(ad >= as.Date("2026-05-29")))   # not before the study opened
  expect_true(all(ad <= as.Date("2027-12-31")))   # not a far-future (e.g. 2029) outlier
})

test_that("p-values are strictly positive and confidence intervals have positive width", {
  for (rc in list(c("model_output/part1_access_OR.csv", "or"),
                  c("model_output/part2_wait_IRR.csv", "irr"))) {
    x <- rd_num(rc[1]); rcol <- rc[2]
    expect_true(all(x$p_value > 0))                 # open lower boundary; p == 0 is degenerate
    expect_true(all(x$p_value <= 1))
    nb <- x[x$term != "(Intercept)", ]
    expect_true(all(nb$ci_upper > nb$ci_lower))     # non-zero width
    expect_true(all(nb$ci_lower < nb[[rcol]] & nb[[rcol]] < nb$ci_upper))  # strict interior
  }
})

test_that("complete-case sensitivity samples are strict subsets of the primary", {
  s8 <- rd_num("model_output/supp/S8_sensitivity_analyses.csv")
  na_primary <- unique(s8[["N (access)"]][grepl("Primary", s8[["Specification"]])])
  na_cc      <- unique(s8[["N (access)"]][grepl("Complete-case", s8[["Specification"]])])
  nt_primary <- unique(s8[["N (timeliness)"]][grepl("Primary", s8[["Specification"]])])
  nt_cc      <- unique(s8[["N (timeliness)"]][grepl("Complete-case", s8[["Specification"]])])
  expect_lt(na_cc, na_primary)        # dropping missing-covariate rows shrinks n
  expect_lt(nt_cc, nt_primary)
  expect_equal(na_primary, 731)
})

test_that("per-subspecialty wait quartiles are correctly ordered", {
  s3 <- rd_num("model_output/supp/S3_wait_by_subspecialty.csv")
  expect_true(all(s3$q1 <= s3$median_days))
  expect_true(all(s3$median_days <= s3$q3))
  expect_true(all(s3$n >= 1))
})
