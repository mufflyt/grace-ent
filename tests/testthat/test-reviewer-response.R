# Reviewer-response analyses (Tables S11-S14): guard the claims added in revision.

test_that("global subspecialty test uses the correct df and is a valid p", {
  g <- rd_num("model_output/supp/S14_global_subspecialty_test.csv")
  expect_true(any(grepl("Access", g[["Model part"]])))
  expect_true(any(grepl("Timeliness", g[["Model part"]])))
  expect_true(all(g[["p-value"]] >= 0 & g[["p-value"]] <= 1))
  # 8 subspecialty levels -> the joint LRT has exactly 7 df (guards the df bug)
  expect_true(all(g[["df"]] == 7))
  # the honest headline: neither part is jointly significant at 0.05
  expect_true(all(g[["p-value"]] > 0.05))
})

test_that("caller factor is jointly significant for offers but not for wait time", {
  f <- rd_num("model_output/supp/S14b_factor_joint_tests.csv")
  cal_a <- f[grepl("Caller", f$Factor) & f[["Model part"]] == "Access (any offer)", ]
  sub_a <- f[grepl("subspecialty", f$Factor) & f[["Model part"]] == "Access (any offer)", ]
  cal_w <- f[grepl("Caller", f$Factor) & f[["Model part"]] == "Timeliness (wait)", ]
  expect_lt(cal_a[["p-value"]], 0.05)        # caller jointly significant for offers
  expect_gt(sub_a[["p-value"]], 0.05)        # subspecialty not
  expect_gt(cal_a[["Chi-square"]], sub_a[["Chi-square"]])
  expect_gt(cal_w[["p-value"]], 0.05)        # caller NOT associated with wait time (guards the abstract claim)
})

test_that("Table S10 provider categories sum to the offer count (436, not 437)", {
  s10 <- rd_num("model_output/supp/S10_access_cascade.csv")
  who <- s10[s10$Group == "Whom the appointment was with (% of offers)", ]
  expect_equal(sum(who$n), unique(who$Denominator))   # 370+54+9+3 == 436
  expect_false(any(who$n == 58))                      # the stale count is gone
})

test_that("design-weighting barely moves the offer rate (strata are similar)", {
  w <- rd_num("model_output/supp/S15_design_weighted.csv")
  gp <- function(col) as.numeric(sub("%.*", "", w[[col]][grepl("offer", w$Estimate)]))
  expect_lt(abs(gp("Unweighted (analytic sample)") - gp("Design-weighted to frame (rural 7%)")), 3)
})

test_that("dual access outcomes cover the seven non-reference subspecialties with positive ORs", {
  d <- rd_num("model_output/supp/S13_dual_access_outcomes.csv")
  expect_equal(nrow(d), 7)
  any_or  <- as.numeric(sub(" .*", "", d[["Any offer: OR (95% CI)"]]))
  samp_or <- as.numeric(sub(" .*", "", d[["Sampled physician: OR (95% CI)"]]))
  expect_true(all(any_or > 0) && all(samp_or > 0))
  # the pediatric any-offer signal does not survive the stricter sampled outcome
  ped <- grepl("Pediatric", d[[1]])
  expect_lt(d[["Any offer: p"]][ped], 0.05)
  expect_gt(d[["Sampled physician: p"]][ped], 0.05)
})

test_that("caller effects are large and mostly significant (an implementation signal)", {
  c <- rd_num("model_output/supp/S12_caller_effects.csv")
  named <- c[c[["Caller"]] != "Other", ]
  expect_true(sum(named[["p-value"]] < 0.05) >= 3)
  or <- as.numeric(sub(" .*", "", named[["Access OR (95% CI)"]]))
  expect_true(max(or) >= 1.8)          # at least one caller ~2x the reference
})

test_that("practice-telephone clustering leaves the headline estimate stable", {
  cl <- rd_num("model_output/supp/S11_clustering_sensitivity.csv")
  expect_equal(nrow(cl), 3)
  or <- as.numeric(sub(" .*", "", cl[["Pediatrics access OR (95% CI)"]]))
  expect_lt(max(or) - min(or), 0.25)   # estimates barely move across clustering choices
})
