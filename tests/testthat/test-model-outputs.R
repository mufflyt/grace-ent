# Well-formedness and characterization of the two-part model outputs.

expect_effect_table <- function(df, est_col) {
  expect_true(all(c("term", est_col, "ci_lower", "ci_upper", "p_value") %in% names(df)))
  fin <- df[df$term != "(Intercept)", ]
  est <- fin[[est_col]]
  expect_true(all(is.finite(est) & est > 0))                       # ratios are positive
  expect_true(all(fin$ci_lower <= est & est <= fin$ci_upper))      # CI brackets the point
  expect_true(all(fin$ci_lower > 0))
  expect_true(all(fin$p_value >= 0 & fin$p_value <= 1))
}

test_that("access odds-ratio table is well-formed", {
  expect_effect_table(rd_num("model_output/part1_access_OR.csv"), "or")
})

test_that("timeliness incidence-rate-ratio table is well-formed", {
  expect_effect_table(rd_num("model_output/part2_wait_IRR.csv"), "irr")
})

test_that("the reference subspecialty (general) is not a coefficient row", {
  a <- rd_num("model_output/part1_access_OR.csv")
  b <- rd_num("model_output/part2_wait_IRR.csv")
  expect_false(any(grepl("ent_typeGeneral", a$term)))
  expect_false(any(grepl("ent_typeGeneral", b$term)))
})

test_that("subspecialty is the dominant, directionally-correct determinant", {
  a <- rd_num("model_output/part1_access_OR.csv")
  b <- rd_num("model_output/part2_wait_IRR.csv")
  # pediatric otolaryngology: higher odds of an offer
  ped_acc <- a[a$term == "ent_typePediatrics", ]
  expect_gt(ped_acc$or, 1)
  expect_lt(ped_acc$p_value, 0.05)
  # laryngology & pediatric: longer waits
  for (tm in c("ent_typeLaryngology", "ent_typePediatrics")) {
    r <- b[b$term == tm, ]
    expect_gt(r$irr, 1)
    expect_lt(r$p_value, 0.05)
  }
})

test_that("no geographic covariate is significantly associated with either outcome", {
  geo <- c("ent_per_100k_z", "medicaid_fee_index_z", "svi_overall_z", "dual_pct_z", "ruralRural")
  for (csv in c("model_output/part1_access_OR.csv", "model_output/part2_wait_IRR.csv")) {
    x <- rd_num(csv)
    p <- x$p_value[x$term %in% geo]
    expect_true(all(p > 0.05))
  }
})
