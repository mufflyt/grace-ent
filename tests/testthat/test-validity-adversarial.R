# Adversarial tests for the registry-validity pipeline in
# scripts/05b_validity_mysteryroster.R. The threats are the ones a REDCap
# export or a package update actually produces: new phone-validity flags the
# study has never seen, new spellings of the datasets label, whitespace drift
# in dispositions, and rows whose evidence points in two directions at once.
# Every one of these must fail closed (an error) or follow the documented
# precedence -- never silently change a listing's status.
library(testthat)
source(here::here("scripts", "05b_validity_mysteryroster.R"))

write_log <- function(rows) {
  path <- tempfile(fileext = ".csv")
  readr::write_csv(rows, path)
  path
}

# A minimal one-row-per-listing call log; each test overrides the fields it attacks.
base_log <- function() {
  data.frame(
    npi = c("1111111111", "2222222222", "3333333333", "4444444444", "5555555555"),
    datasets = c("ABO, NPI but no ENTHealth", "ABO, NPI but not ENT Health",
                 "ABO, NPI, ENT Health", "ABO, NPI, ENT Health", "ABO, NPI but no ENTHealth"),
    phone_validity_flag = c("valid", "valid", "valid", "valid", "valid"),
    taking_new_patients = c("Yes", "No-changed practices", "Yes", NA, "Yes"),
    office_answered = c(TRUE, TRUE, TRUE, NA, TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("a phone-validity flag the study has never seen stops the pipeline (fail closed)", {
  # The pre-fix construction `flag != "invalid_format"` would have counted any
  # future invalid-type flag as a VALID number. A mysterycall update adding
  # e.g. "disconnected" must never silently flip that decision.
  log <- base_log()
  log$phone_validity_flag[1] <- "disconnected"
  expect_error(validity_from_call_log(write_log(log)), "unrecognised phone_validity_flag.*disconnected")
})

test_that("a datasets spelling the study has never seen stops the pipeline (fail closed)", {
  log <- base_log()
  log$datasets[1] <- "ABO, NPI, ENTHealth"   # plausible next drift variant: no comma-space
  expect_error(validity_from_call_log(write_log(log)), "unrecognised `datasets` label")
})

test_that("an area-code/state mismatch can never invalidate a listing", {
  # Area codes are portable; the mismatch is a QC flag, not evidence of a wrong number.
  log <- base_log()
  log$phone_validity_flag <- "area_code_state_mismatch"
  res <- validity_from_call_log(write_log(log))
  expect_false(any(res$calls$invalid_reason == "wrong_disconnected_number", na.rm = TRUE))
  expect_equal(as.character(res$calls$listing_status[1]), "confirmed_valid")
})

test_that("a missing phone can never fabricate an invalid listing", {
  log <- base_log()
  log$phone_validity_flag <- "missing"
  log$office_answered <- NA
  log$taking_new_patients <- NA
  res <- validity_from_call_log(write_log(log))
  expect_true(all(res$calls$listing_status == "not_attempted"))
  expect_true(all(is.na(res$calls$invalid_reason)))
})

test_that("a malformed number invalidates even when the office was never reached", {
  # The invalid number is itself the evidence; no call needs to have happened.
  log <- base_log()
  log$phone_validity_flag[5] <- "invalid_format"
  log$office_answered[5] <- NA
  log$taking_new_patients[5] <- NA
  res <- validity_from_call_log(write_log(log))
  expect_equal(as.character(res$calls$listing_status[5]), "confirmed_invalid")
  expect_equal(res$calls$invalid_reason[5], "wrong_disconnected_number")
})

test_that("staff confirmation outranks a bad-number flag when both exist", {
  # Contradictory row: the number is malformed yet staff confirmed the physician.
  # The human evidence must win; dropping it would overstate directory error rates.
  log <- base_log()
  log$phone_validity_flag[1] <- "invalid_format"
  log$taking_new_patients[1] <- "Yes"
  log$office_answered[1] <- TRUE
  res <- validity_from_call_log(write_log(log))
  expect_equal(as.character(res$calls$listing_status[1]), "confirmed_valid")
  expect_true(is.na(res$calls$invalid_reason[1]))
})

test_that("disposition whitespace drift cannot silently misclassify at either layer", {
  # Three independent defences, all pinned here:
  #   1. the CSV roundtrip trims whitespace (readr's trim_ws default);
  #   2. mysteryroster >= 0.4.1 trims both data and map before matching, so
  #      even a direct (non-CSV) ingestion path classifies drifted values
  #      exactly like the clean ones;
  #   3. drift beyond whitespace still stops the call rather than guessing.
  log <- base_log()
  log$taking_new_patients[2] <- "No-changed practices "   # trailing space from a REDCap edit
  res <- validity_from_call_log(write_log(log))
  expect_equal(as.character(res$calls$listing_status[2]), "confirmed_invalid")   # same as the clean string
  direct <- data.frame(disposition = c("No-changed practices ", "No-changed practices!"), reached = TRUE)
  expect_equal(as.character(mysteryroster::classify_listing_status(direct[1, , drop = FALSE])$listing_status),
               "confirmed_invalid")
  expect_error(mysteryroster::classify_listing_status(direct[2, , drop = FALSE]), "not in the map")
})

test_that("the four statuses still partition every row under mixed adversarial input", {
  log <- base_log()
  log$phone_validity_flag <- c("valid", "invalid_format", "missing", "area_code_state_mismatch", "valid")
  log$office_answered[2] <- NA; log$taking_new_patients[2] <- NA
  res <- validity_from_call_log(write_log(log))
  expect_false(anyNA(res$calls$listing_status))
  expect_equal(sum(table(res$calls$listing_status)), nrow(log))
  expect_true(all(is.na(res$calls$invalid_reason[res$calls$listing_status != "confirmed_invalid"])))
})
