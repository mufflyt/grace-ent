# Adversarial invariants for the REDCap correction report and the random-effect
# cluster construction.
#
# Two bug classes pinned here:
#  1. Cleaning blanks the wait for implausible appointment gaps (the true date
#     is unverified). Any correction-report detection keyed on the wait would
#     then silently miss exactly the rows most in need of correction - the
#     blanking itself hides the evidence. Detection must use the date gap.
#  2. CBSA codes and county FIPS are both 5-digit numerics from DIFFERENT
#     codebooks. Merging them unprefixed into one cluster column means a county
#     FIPS equal to a CBSA code silently collapses two unrelated geographies
#     into one random-effect group.

library(testthat)

test_that("impossible-wait detection fires on date evidence when the wait is blanked", {
  # Replicates the detection rule of 02_redcap_correction_report.R issue 1
  # against a synthetic row whose wait has been blanked by cleaning - the
  # exact state record 470 is in downstream of 01.
  df <- data.frame(
    record_id = "470",
    call_date = as.Date("2026-06-23"),
    appointment_date = as.Date("2029-07-28"),
    wait_days_business = NA_real_
  )
  flagged <- !is.na(df$appointment_date) & !is.na(df$call_date) &
    as.numeric(df$appointment_date - df$call_date) > 366
  expect_true(flagged[1])
  # And the inverse: a legitimate 20-day wait must never be flagged.
  df2 <- data.frame(
    call_date = as.Date("2026-06-23"),
    appointment_date = as.Date("2026-07-13")
  )
  expect_false(any(!is.na(df2$appointment_date) &
    as.numeric(df2$appointment_date - df2$call_date) > 366))
})

test_that("correction report source keys issue 1 on dates, not the blankable wait", {
  src <- readLines(test_path("../../scripts/02_redcap_correction_report.R"), warn = FALSE)
  i1_region <- src[seq(grep("Issue 1", src)[1], grep("Issue 2", src)[1])]
  expect_true(any(grepl("appointment_date - call_date", i1_region)))
  expect_false(any(grepl("wait_days_business > ", i1_region)),
               info = "filtering on the blanked wait would hide corrected rows")
})

test_that("prefixed cluster construction can never merge a CBSA with a county", {
  # Adversarial input: county FIPS "19740" equals CBSA code "19740" (Denver).
  # Unprefixed merging would collapse these two rows into one cluster.
  cbsa <- c("19740", "")
  fips <- c("08031", "19740")
  clust <- ifelse(is.na(cbsa) | !nzchar(cbsa), "", paste0("cbsa_", cbsa))
  nf <- is.na(fips) | !nzchar(fips)
  clust[clust == "" & !nf] <- paste0("cty_", fips[clust == "" & !nf])
  expect_length(unique(clust), 2)
  expect_true(all(grepl("^(cbsa|cty|solo)_", clust)))
})

test_that("unprefixed construction really would have collided (red test for the trap)", {
  # Pins that the adversarial input above is genuinely dangerous - if someone
  # reverts to unprefixed merging, the collision is silent.
  cbsa <- c("19740", "")
  fips <- c("08031", "19740")
  clust <- cbsa
  clust[!nzchar(clust)] <- fips[!nzchar(clust)]
  expect_length(unique(clust), 1) # both rows collapse into one group
})

test_that("model scripts prefix cluster ids in source", {
  for (f in c("../../scripts/model_wait_days.R", "../../scripts/model_robustness.R")) {
    src <- readLines(test_path(f), warn = FALSE)
    expect_true(any(grepl("cbsa_", src)), info = paste(f, "must prefix cbsa cluster ids"))
    expect_true(any(grepl("cty_", src)), info = paste(f, "must prefix county cluster ids"))
  }
})

test_that("no cluster id in the enriched data is ambiguous today", {
  d <- read.csv(test_path("../../data/processed/ent_phase2_enriched.csv"),
                colClasses = "character")
  cbsas <- unique(d$cbsa_code[nzchar(d$cbsa_code)])
  fips_nonmetro <- unique(d$county_fips[nzchar(d$county_fips) & !nzchar(d$cbsa_code)])
  # If this ever fails the data itself has a collision - the prefixes are then
  # load-bearing, not just defensive.
  expect_length(intersect(cbsas, fips_nonmetro), 0)
})
