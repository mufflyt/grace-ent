# Adversarial invariants for the timeliness data path.
#
# Study decision (fix_data_errors.R): record 470's appointment date
# (2029-07-28 against a 2026-06-23 call) is a year typo whose true value is
# UNVERIFIED. No corrected year may be fabricated, because that would put
# plausible-looking invented data into the timeliness analysis. The record is
# retained for access (an appointment genuinely was offered) but excluded
# from timeliness everywhere. The rule generalises: appointments more than a
# year after the call are implausible for new-patient ENT care (longest
# legitimate wait in the data: 160 business days) and must never yield a
# wait time.

library(testthat)

enriched <- read.csv(test_path("../../data/processed/ent_phase2_enriched.csv"),
                     colClasses = "character")

test_that("no row with a >1yr appointment gap carries a wait time", {
  appt <- as.Date(enriched$appointment_date)
  call <- as.Date(enriched$call_date)
  gap_days <- as.numeric(appt - call)
  implausible <- !is.na(gap_days) & gap_days > 366
  expect_true(any(implausible)) # guard: record 470 must still be present
  expect_true(all(is.na(enriched$wait_days_business[implausible]) |
                  enriched$wait_days_business[implausible] == ""))
})

test_that("every recorded wait is consistent with the underlying dates", {
  has_wait <- !is.na(enriched$wait_days_business) & enriched$wait_days_business != ""
  appt <- as.Date(enriched$appointment_date[has_wait])
  call <- as.Date(enriched$call_date[has_wait])
  gap_days <- as.numeric(appt - call)
  # Adversarial: a fabricated or drifted date must surface here as a gap
  # inconsistent with a plausible wait - either negative or beyond a year.
  expect_true(all(gap_days >= 0 & gap_days <= 366))
})

test_that("no business-day wait exceeds one year of business days", {
  waits <- suppressWarnings(as.numeric(enriched$wait_days_business))
  waits <- waits[!is.na(waits)]
  expect_true(all(waits <= 260))
  expect_gt(max(waits), 30) # guard: real long waits exist, column not empty
})

test_that("record 470 pins both halves of the study decision", {
  r470 <- enriched[enriched$record_id == "470", ]
  expect_equal(nrow(r470), 1)
  # Retained for access: the appointment was genuinely offered.
  expect_equal(r470$appointment_offered, "TRUE")
  # Excluded from timeliness: no fabricated wait may appear.
  expect_true(is.na(r470$wait_days_business) || r470$wait_days_business == "")
  # The wrong date is preserved as recorded - transparently wrong rather than
  # silently replaced with a plausible-looking fabrication.
  expect_equal(r470$appointment_date, "2029-07-28")
})

test_that("timeliness models consume the enriched file, never the raw dates", {
  # model_wait_days.R and model_robustness.R must keep reading the corrected
  # enriched file; regressing to the uncorrected complete file would
  # re-include fabricated waits.
  for (f in c("../../scripts/model_wait_days.R", "../../scripts/model_robustness.R")) {
    src <- readLines(test_path(f), warn = FALSE)
    expect_true(any(grepl('ent_phase2_enriched', src)),
                info = paste(f, "must read the enriched file"))
    expect_false(any(grepl('ent_phase2_complete', src)),
                 info = paste(f, "must not read the complete file"))
  }
})
