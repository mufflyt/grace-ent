# Sensitivity analyses (Table S8): three specifications, stable conclusions.

test_that("all three specifications are present", {
  s <- rd_num("model_output/supp/S8_sensitivity_analyses.csv")
  specs <- unique(s$spec)
  expect_true(any(grepl("Primary", specs)))
  expect_true(any(grepl("3-level RUCA", specs)))
  expect_true(any(grepl("Complete-case", specs)))
})

test_that("subspecialty wait effects stay elevated across specifications", {
  s <- rd_num("model_output/supp/S8_sensitivity_analyses.csv")
  tim <- s[s$part == "Timeliness (IRR)" &
             s$term %in% c("Laryngology", "Pediatric otolaryngology"), ]
  expect_true(nrow(tim) >= 4)          # 2 subspecialties x >= 2 specs reporting them
  expect_true(all(tim$estimate > 1))
})

test_that("no urban-to-rural gradient in the three-level specification", {
  s <- rd_num("model_output/supp/S8_sensitivity_analyses.csv")
  g <- s[grepl("3-level", s$spec) & s$part == "Timeliness (IRR)" &
           grepl("Suburban|Rural", s$term), ]
  expect_true(nrow(g) >= 1)
  expect_true(all(g$p > 0.05))         # neither suburban nor rural differs from urban
})
