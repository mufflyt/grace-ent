# Adversarial internal-consistency checks on the model-output tables.
# These verify the reported ratios, CIs, z- and p-values are mutually derivable
# from one another — catching column mislabeling, wrong CI formulas, or stale
# copy-paste that structural checks would miss.

check_internal_consistency <- function(rel, ratio_col) {
  x <- rd_num(rel)
  # ratio == exp(log-scale estimate)
  expect_lt(max(abs(x[[ratio_col]] - exp(x$estimate))), 1e-6)
  # standard errors are strictly positive
  expect_true(all(x$se > 0))
  # z == estimate / se
  expect_lt(max(abs(x$z_value - x$estimate / x$se)), 1e-6)
  # two-sided p == 2 * Phi(-|z|)
  expect_lt(max(abs(x$p_value - 2 * pnorm(-abs(x$z_value)))), 1e-6)
  # 95% Wald CI on the ratio scale
  expect_lt(max(abs(x$ci_lower - exp(x$estimate - 1.96 * x$se))), 1e-3)
  expect_lt(max(abs(x$ci_upper - exp(x$estimate + 1.96 * x$se))), 1e-3)
  # CI strictly brackets the point estimate
  expect_true(all(x$ci_lower <= x[[ratio_col]] & x[[ratio_col]] <= x$ci_upper))
  # formatted p-value is faithful: "<..." when tiny, else the rounded value
  tiny <- x$p_value < 0.001
  expect_true(all(grepl("<", x$p_value_fmt[tiny])))
  big  <- !tiny
  expect_lt(max(abs(suppressWarnings(as.numeric(x$p_value_fmt[big])) -
                    round(x$p_value[big], 3))), 1e-9)
}

test_that("access odds-ratio table is internally consistent", {
  check_internal_consistency("model_output/part1_access_OR.csv", "or")
})

test_that("timeliness incidence-rate-ratio table is internally consistent", {
  check_internal_consistency("model_output/part2_wait_IRR.csv", "irr")
})

test_that("significance and confidence intervals agree (a p<0.05 term excludes 1)", {
  for (rc in list(c("model_output/part1_access_OR.csv", "or"),
                  c("model_output/part2_wait_IRR.csv", "irr"))) {
    x <- rd_num(rc[1]); rcol <- rc[2]
    sig <- x[x$term != "(Intercept)" & x$p_value < 0.05, ]
    if (nrow(sig)) {
      excludes_one <- (sig$ci_lower > 1 & sig$ci_upper > 1) |
                      (sig$ci_lower < 1 & sig$ci_upper < 1)
      expect_true(all(excludes_one))
    }
  }
})
