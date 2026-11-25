# Regression tests for the yes/no parsing fixes in 01_clean_phase2_call_log.R.
# The hand-rolled tolower() == "yes" parsing silently mapped whitespace/case
# drift to NA, which downstream reads as "not attempted" -- a silent
# misclassification. The fix trims REDCap's categorical strings and parses
# the three boolean fields with mysteryroster::coerce_logical_flag(), which
# normalises formatting noise and fails closed on anything else. These tests
# pin the invariants on the committed processed data and the behaviour of
# the parsing path itself.
library(testthat)

phase2_path <- tail(sort(list.files(here::here("data", "processed"),
                                    pattern = "^ent_phase2_all_.*\\.csv$", full.names = TRUE)), 1)
phase2 <- readr::read_csv(phase2_path, show_col_types = FALSE)

test_that("the three boolean fields are pure logical with no character residue", {
  for (col in c("office_answered", "website_found", "rural_satellite_office")) {
    expect_type(phase2[[col]], "logical")
  }
})

test_that("no categorical column carries leading or trailing whitespace", {
  # If drift exists in the raw export, the pipeline must have normalised it --
  # or failed closed. Whitespace surviving into the committed data means neither happened.
  cat_cols <- c("taking_new_patients", "appointment_with_physician", "sees_chief_complaint",
                "ent_type", "complete", "new_patient_status", "appointment_outcome")
  for (col in intersect(cat_cols, names(phase2))) {
    vals <- as.character(phase2[[col]])
    vals <- vals[!is.na(vals)]
    expect_false(any(vals != trimws(vals)), info = paste("whitespace in", col))
  }
})

test_that("every taking_new_patients value is in the ENT disposition codebook or missing", {
  # 05b fails closed on drift, but this pins the invariant at the source:
  # a value outside the codebook in the committed data means the drift
  # entered upstream of the classifier.
  codes <- unlist(mysteryroster::ent_disposition_map(), use.names = FALSE)
  vals <- unique(phase2$taking_new_patients[!is.na(phase2$taking_new_patients)])
  expect_true(all(vals %in% codes),
              info = paste("outside codebook:", paste(setdiff(vals, codes), collapse = "; ")))
})

test_that("the parsing path normalises formatting noise and fails closed on real drift", {
  # The exact behaviour 01 now relies on: formatting variants parse to their
  # intended values rather than silently becoming NA...
  expect_equal(mysteryroster::coerce_logical_flag(c("Yes", "yes ", " YES", "No", NA, "")),
               c(TRUE, TRUE, TRUE, FALSE, NA, NA))
  # ...and anything that is genuinely not a yes/no encoding stops the pipeline
  # instead of silently turning into a missing value.
  expect_error(mysteryroster::coerce_logical_flag(c("Yes", "chk")), "unrecognised")
})

test_that("contact-dependent dispositions never lack contact evidence", {
  # The silent-drift failure mode turned answered calls into NA office_answered.
  # The signature that would prove it: a disposition nobody could know without
  # speaking to the office (accepting patients, practice full, on leave,
  # unspecified refusal) paired with no contact evidence. Retired/relocated
  # dispositions are exempt -- the protocol's website verification can establish
  # those without a call, and indeed 28 such rows exist legitimately.
  contact_dependent <- c(
    "Yes",
    "No-practice is full but physician does still see established patients",
    "No-on leave of absence (like parental leave)",
    "No-not specified why")
  bad <- !is.na(phase2$taking_new_patients) &
         phase2$taking_new_patients %in% contact_dependent &
         is.na(phase2$office_answered)
  expect_equal(sum(bad), 0L,
               info = "contact-dependent dispositions without contact evidence -- the silent-drift signature")
})

test_that("web-verifiable dispositions without contact evidence still classify correctly", {
  # The 28 retired/relocated rows with NA office_answered must still reach
  # confirmed_invalid: the disposition evidence dominates the missing reached
  # field in classify_listing_status(). If that precedence ever inverted,
  # these rows would silently degrade to not_attempted.
  web_rows <- !is.na(phase2$taking_new_patients) &
              phase2$taking_new_patients %in% c("No-retired or no longer practicing medicine",
                                                "No-changed practices") &
              is.na(phase2$office_answered)
  expect_gte(sum(web_rows), 20L)   # the pattern exists in the data; pin that it classifies right
  d <- data.frame(disposition = c("No-retired or no longer practicing medicine", "No-changed practices"),
                  reached = c(NA, NA))
  out <- mysteryroster::classify_listing_status(d)
  expect_equal(as.character(out$listing_status), c("confirmed_invalid", "confirmed_invalid"))
})
