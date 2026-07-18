# The access-cascade secondary analysis (Table S10) recomputes from the data.

test_that("access-cascade table is well-formed", {
  s <- rd_num("model_output/supp/S10_access_cascade.csv")
  expect_true(all(c("Group", "Measure", "n", "Denominator", "%") %in% names(s)))
  expect_true(all(s$n >= 0 & s$n <= s$Denominator))
  expect_true(all(s[["%"]] >= 0 & s[["%"]] <= 100))
})

test_that("cascade counts recompute exactly from the enriched call log", {
  comp <- analytic()
  s <- rd_num("model_output/supp/S10_access_cascade.csv")
  get <- function(measure) s$n[s$Measure == measure]
  expect_equal(get("Reached a live office (office answered)"),
               sum(comp$office_answered == "TRUE", na.rm = TRUE))
  expect_equal(get("Practice accepting new patients"),
               sum(comp$new_patient_status == "Accepting", na.rm = TRUE))
  expect_equal(get("New-patient appointment offered"),
               sum(comp$appointment_offered == "TRUE", na.rm = TRUE))
  expect_equal(get("Appointment with the sampled physician"),
               sum(comp$appointment_outcome == "With sampled physician", na.rm = TRUE))
})

test_that("appointment with the sampled physician is a subset of all offers", {
  s <- rd_num("model_output/supp/S10_access_cascade.csv")
  offered <- s$n[s$Measure == "New-patient appointment offered"]
  sampled <- s$n[s$Measure == "Appointment with the sampled physician"]
  expect_lte(sampled, offered)
  expect_equal(s$Denominator[s$Measure == "Reached a live office (office answered)"], 731L)
})
