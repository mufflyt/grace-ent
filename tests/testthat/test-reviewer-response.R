# Reviewer-response analyses (Tables S11-S14): guard the claims added in revision.

test_that("global subspecialty test is reported for both model parts and is a valid p", {
  g <- rd_num("model_output/supp/S14_global_subspecialty_test.csv")
  expect_true(any(grepl("Access", g[["Model part"]])))
  expect_true(any(grepl("Timeliness", g[["Model part"]])))
  expect_true(all(g[["p-value"]] >= 0 & g[["p-value"]] <= 1))
  expect_true(all(g[["df"]] >= 1))
  # the honest headline: neither part is jointly significant at 0.05
  expect_true(all(g[["p-value"]] > 0.05))
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
