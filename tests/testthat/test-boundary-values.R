# Boundary-value analysis (BVA): probe the edges of every input/output domain —
# cutpoints, min/max, zero, the null ratio (1.0), 0%/100%, and subset endpoints.

num <- function(x) suppressWarnings(as.numeric(x))

test_that("RUCA binary recode is exact at the 3/4 cutpoint", {
  d  <- rd_chr("data/processed/ent_phase2_enriched.csv")
  rc <- num(d$ruca_code)
  expect_gte(min(rc), 1)                 # lower domain bound
  expect_lte(max(rc), 10)                # upper domain bound
  expect_true(all(rc[rc <= 3] > 0))
  # just below the cutpoint -> Urban; at/above -> Rural
  expect_true(all(d$ruca_category[rc <= 3] == "Urban"))
  expect_true(all(d$ruca_category[rc >= 4] == "Rural"))
})

test_that("business-day wait respects its lower boundary of zero", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  w <- num(d$wait_days_business)
  expect_equal(min(w, na.rm = TRUE), 0)                 # 0 is the valid minimum
  expect_false(any(w < 0, na.rm = TRUE))                # nothing below the boundary
  expect_true(all(d$appointment_offered[which(w == 0)] == "TRUE"))  # wait 0 => offered
  expect_lt(max(w, na.rm = TRUE), 400)                  # upper plausibility bound
})

test_that("area covariates stay within their defined ranges (inclusive bounds)", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  svi <- num(d$svi_overall); dual <- num(d$dual_pct)
  ent <- num(d$ent_per_100k); mfi <- num(d$medicaid_fee_index)
  expect_gte(min(svi, na.rm = TRUE), 0);  expect_lte(max(svi, na.rm = TRUE), 1)   # SVI percentile in [0,1]
  expect_gte(min(dual, na.rm = TRUE), 0); expect_lte(max(dual, na.rm = TRUE), 1)  # a proportion
  expect_gt(min(ent, na.rm = TRUE), 0)                                            # density strictly positive
  expect_gt(min(mfi, na.rm = TRUE), 0)                                            # fee index strictly positive
})

test_that("data-collection window endpoints are exact", {
  d  <- rd_chr("data/processed/ent_phase2_enriched.csv")
  cd <- as.Date(d$call_date[nzchar(d$call_date)])
  expect_equal(min(cd), as.Date("2026-05-29"))
  expect_equal(max(cd), as.Date("2026-07-06"))
})

test_that("the analytic waterfall is monotonically non-increasing", {
  d    <- rd_chr("data/processed/ent_phase2_enriched.csv")
  comp <- analytic()
  n_total    <- nrow(d)
  n_complete <- sum(d$complete == "Complete")
  n_analytic <- nrow(comp)
  n_offered  <- sum(comp$appointment_offered == "TRUE")
  n_wait     <- sum(comp$appointment_offered == "TRUE" & !is.na(num(comp$wait_days_business)))
  # 960 >= 749 >= 731 >= 436 >= 426 >= 0  (each stage a subset of the prior)
  expect_true(all(diff(c(n_total, n_complete, n_analytic, n_offered, n_wait)) <= 0))
  expect_equal(n_total, 960L)
  expect_equal(n_analytic, 731L)
  expect_equal(n_offered, 436L)
})

test_that("cascade proportions sit within [0, 100] and counts within [0, denom]", {
  s <- rd_num("model_output/supp/S10_access_cascade.csv")
  expect_gte(min(s[["%"]]), 0);  expect_lte(max(s[["%"]]), 100)   # percentage bounds
  expect_true(all(s$n >= 0));    expect_true(all(s$n <= s$Denominator))  # count endpoints
  # NB: the "cascade" steps are parallel access measures, not strictly nested
  # subsets (e.g. an offer can occur where sees_chief_complaint != "Yes"), so no
  # monotonicity is asserted across steps. The genuinely nested containment
  # (offered >= appointment-with-sampled-physician) is checked in test-access-cascade.
})

test_that("the offer rate is strictly interior (non-degenerate)", {
  comp <- analytic()
  rate <- mean(comp$appointment_offered == "TRUE")
  expect_gt(rate, 0)     # not the empty boundary
  expect_lt(rate, 1)     # not the saturated boundary
})

test_that("IQR of the wait distribution is correctly ordered", {
  comp <- analytic()
  w <- num(comp$wait_days_business)[comp$appointment_offered == "TRUE"]
  q <- quantile(w, c(.25, .5, .75), na.rm = TRUE)
  expect_lte(q[[1]], q[[2]])
  expect_lte(q[[2]], q[[3]])
})

test_that("significance is decided consistently at the null ratio of 1.0", {
  for (rc in list(c("model_output/part1_access_OR.csv", "or"),
                  c("model_output/part2_wait_IRR.csv", "irr"))) {
    x <- rd_num(rc[1]); rcol <- rc[2]
    x <- x[x$term != "(Intercept)", ]
    sig    <- x$p_value < 0.05
    incl_1 <- x$ci_lower <= 1 & x$ci_upper >= 1
    # a term is significant iff its 95% CI excludes the null (1.0)
    expect_equal(sig, !incl_1)
  }
})
