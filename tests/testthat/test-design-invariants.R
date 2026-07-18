# Adversarial invariants on the analytic dataset — properties that must hold by
# the study's design and semantics, chosen to trip on subtle corruption.

test_that("one call per physician: NPIs are unique across all 960 records", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  npi <- d$npi[nzchar(d$npi)]
  expect_equal(length(npi), 960L)          # every record has an NPI
  expect_equal(length(unique(npi)), 960L)  # no physician contacted twice
})

test_that("all calls fall within the stated data-collection window", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  cd <- as.Date(d$call_date[nzchar(d$call_date)])
  expect_gte(min(cd), as.Date("2026-05-29"))
  expect_lte(max(cd), as.Date("2026-07-06"))
})

test_that("business-day waits are non-negative and within a plausible horizon", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  w <- suppressWarnings(as.numeric(d$wait_days_business))
  expect_false(any(w < 0, na.rm = TRUE))
  expect_lt(max(w, na.rm = TRUE), 400)
})

test_that("no appointment in the timeliness sample predates its call", {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  w  <- suppressWarnings(as.numeric(d$wait_days_business))
  ok <- !is.na(w)
  expect_true(all(as.Date(d$appointment_date[ok]) >= as.Date(d$call_date[ok])))
})

test_that("binary rurality is a faithful recode of the RUCA primary code", {
  d  <- rd_chr("data/processed/ent_phase2_enriched.csv")
  rc <- suppressWarnings(as.numeric(d$ruca_code))
  # RUCA 1-3 -> Urban, 4-10 -> Rural, with no exceptions
  expect_true(all((rc <= 3) == (d$ruca_category == "Urban")))
  expect_true(all((rc >= 4) == (d$ruca_category == "Rural")))
})

test_that("key modeling columns are non-degenerate (no silently blanked field)", {
  comp <- analytic()
  expect_gt(length(unique(comp$ent_type)), 1L)                       # multiple subspecialties
  expect_setequal(unique(comp$appointment_offered[nzchar(comp$appointment_offered)]),
                  c("TRUE", "FALSE"))                                 # both offer states present
  w <- suppressWarnings(as.numeric(comp$wait_days_business))
  expect_gt(stats::sd(w, na.rm = TRUE), 0)                            # real wait variation
})

test_that("reasons for non-acceptance partition the non-accepting practices", {
  s <- rd_num("model_output/supp/S10_access_cascade.csv")
  r <- s[s$Group == "Reasons not accepting new patients (% of non-accepting)", ]
  expect_gt(nrow(r), 0L)
  expect_equal(sum(r$n), unique(r$Denominator))
})

test_that("offer flag and appointment outcome agree (bounded known discordance)", {
  # ADVERSARIAL DATA-QUALITY GUARDRAIL.
  # A handful of records disagree between appointment_offered and
  # appointment_outcome (surfaced 2026-07: 6 outcomes recorded as "With sampled
  # physician" but offered=FALSE; 3 offered=TRUE but "No appointment offered").
  # These should be reconciled toward zero in REDCap. This test passes at the
  # current level and FAILS if the discordance grows.
  comp <- analytic()
  made_with_sampled_but_not_offered <-
    sum(comp$appointment_outcome == "With sampled physician" &
        comp$appointment_offered != "TRUE")
  offered_but_no_appointment <-
    sum(comp$appointment_offered == "TRUE" &
        comp$appointment_outcome == "No appointment offered")
  expect_lte(made_with_sampled_but_not_offered, 6L)
  expect_lte(offered_but_no_appointment, 3L)
})
