#!/usr/bin/env Rscript
# =============================================================================
# Reconcile the binary appointment_offered flag with the authoritative
# appointment_outcome field (surfaced by the adversarial test suite, 2026-07).
#
# Nine records disagreed between the summary flag and the specific outcome:
#
#   GROUP A (6: 257, 722, 805, 842, 846, 849)
#     appointment_outcome = "With sampled physician" (with appointment_with_md =
#     TRUE, taking_new_patients = Accepting, sees_chief_complaint = Yes) but
#     appointment_offered = FALSE. Every field except the flag indicates an
#     appointment WAS offered -> set appointment_offered = TRUE.
#
#   GROUP B (3: 203, 616, 790)
#     appointment_offered = TRUE but appointment_outcome = "No appointment
#     offered" (with appointment_with_physician = "...no appointment offered with
#     anyone in practice", appointment_with_md = FALSE) -> set appointment_offered
#     = FALSE and clear the stray appointment_date / wait_days_business so the
#     "wait observed only when offered" invariant holds.
#
# Rule: trust appointment_outcome (the specific, considered determination) over
# the binary flag. NOTE for the study team: record 790 carried a non-trivial
# wait (14 business days) alongside "no appointment offered"; verify it against
# the source call note — if an appointment was in fact offered, set its outcome
# to the appropriate category and re-run instead.
#
# Idempotent. Run before modeling (part of run_all.R data/analysis prep).
# =============================================================================

f <- "data/processed/ent_phase2_enriched.csv"
d <- read.csv(f, colClasses = "character", check.names = FALSE)

# Surgical scope: only the nine records where the flag is unambiguously wrong
# (every other field agrees). We deliberately do NOT touch the four "With
# different physician" rows coded offered=FALSE (records 21, 96, 245, 291): those
# turn on how "offered" treats an offer from a non-sampled provider and are a
# separate definitional question left for the study team.
grpA <- d$appointment_outcome == "With sampled physician" & d$appointment_offered != "TRUE"
grpB <- d$appointment_outcome == "No appointment offered" & d$appointment_offered == "TRUE"

if (any(grpA) || any(grpB)) {
  cat(sprintf("Group A (offered FALSE -> TRUE, appointment made): %s\n",
              paste(d$record_id[grpA], collapse = ", ")))
  cat(sprintf("Group B (offered TRUE -> FALSE, no appointment): %s\n",
              paste(d$record_id[grpB], collapse = ", ")))

  d$appointment_offered[grpA] <- "TRUE"

  d$appointment_offered[grpB]   <- "FALSE"
  d$wait_days_business[grpB]    <- ""     # no appointment -> no wait
  d$appointment_date[grpB]      <- ""     # no appointment -> no date

  write.csv(d, f, row.names = FALSE, na = "")
  cat("Reconciled 9 discordant records; wrote", f, "\n")
} else {
  cat("No offered/outcome discordance found (already reconciled).\n")
}
