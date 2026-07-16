#!/usr/bin/env Rscript
# =============================================================================
# Correct known data-entry errors in the enriched call log.
#
# Record 470: appointment_date = 2029-07-28 (call 2026-06-23) -> 774 business
# days. The year is a clear typo (appointment 3 years out). The true date is
# unverified, so the wait time is set to missing (the record is retained for the
# ACCESS model, where it was correctly coded as offered, but excluded from the
# TIMELINESS model). If REDCap yields the true appointment date, restore it here.
#
# This one record inflated the wait-time SD from ~24 to ~43 days.
# Idempotent.
# =============================================================================

f <- "data/processed/ent_phase2_enriched.csv"
d <- read.csv(f, colClasses = "character")

bad <- d$record_id == "470"
if (any(bad) && d$wait_days_business[bad] != "") {
  cat(sprintf("Record 470: appt %s, wait %s business days -> flagged as wrong-year typo; wait set to NA.\n",
              d$appointment_date[bad], d$wait_days_business[bad]))
  d$wait_days_business[bad] <- ""     # exclude from timeliness (kept as offered for access)
  write.csv(d, f, row.names = FALSE, na = "")
} else {
  cat("Record 470 already corrected (or not found).\n")
}
