# Integrity of the analytic dataset and the documented data corrections.

test_that("call log has 960 unique records", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  expect_equal(nrow(d), 960L)
  expect_equal(length(unique(d$record_id)), 960L)
})

test_that("analytic sample and offer counts match the manuscript", {
  comp <- analytic()
  expect_equal(nrow(comp), 731L)
  expect_equal(sum(comp$appointment_offered == "TRUE"), 433L)
})

test_that("record 470 wrong-year appointment wait is corrected to missing", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  r <- d[d$record_id == "470", ]
  expect_equal(nrow(r), 1L)
  # kept as an offered appointment in the access model...
  expect_equal(r$appointment_offered, "TRUE")
  # ...but excluded from the timeliness model (wait set to NA/blank)
  expect_true(is.na(r$wait_days_business) || r$wait_days_business == "")
})

test_that("rurality is the binary RUCA split, 480 urban / 480 rural", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  tb <- table(d$ruca_category)
  expect_equal(as.integer(tb[["Urban"]]), 480L)
  expect_equal(as.integer(tb[["Rural"]]), 480L)
})

test_that("business-day wait is observed only when an appointment was offered", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  w <- suppressWarnings(as.numeric(d$wait_days_business))
  expect_equal(sum(!is.na(w) & d$appointment_offered != "TRUE"), 0L)
})

test_that("no wait exceeds the plausible horizon after the record-470 fix", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  w <- suppressWarnings(as.numeric(d$wait_days_business))
  expect_lt(max(w, na.rm = TRUE), 400)   # 774-day outlier removed; realized max ~160
})
