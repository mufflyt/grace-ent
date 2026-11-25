#!/usr/bin/env Rscript
# =============================================================================
# Reconcile the binary appointment_offered flag with the authoritative
# appointment_outcome field (surfaced by the adversarial test suite, 2026-07).
#
# Records can disagree between the summary flag and the specific outcome:
#
#   GROUP A
#     appointment_outcome = "With sampled physician" (with appointment_with_md =
#     TRUE, taking_new_patients = Accepting, sees_chief_complaint = Yes) but
#     appointment_offered = FALSE. Every field except the flag indicates an
#     appointment WAS offered -> set appointment_offered = TRUE.
#
#   GROUP B
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

# Surgical scope: only records where the flag is unambiguously wrong
# (every other field agrees). We deliberately do NOT touch the four "With
# different physician" rows coded offered=FALSE (records 21, 96, 245, 291): those
# turn on how "offered" treats an offer from a non-sampled provider and are a
# separate definitional question left for the study team.
reconciliation <- mysterycall::mysterycall_reconcile_offer_outcome(
  d,
  flag_col           = "appointment_offered",
  outcome_col        = "appointment_outcome",
  offered_values     = "With sampled physician",
  not_offered_values = "No appointment offered",
  dependent_cols     = c("wait_days_business", "appointment_date"),
  action             = "fix",
  id_col             = "record_id"
)

if (nrow(reconciliation$discordant)) {
  grpA <- reconciliation$discordant$direction == "flag_understates"
  grpB <- reconciliation$discordant$direction == "flag_overstates"
  cat(sprintf("Group A (offered FALSE -> TRUE, appointment made): %s\n",
              paste(reconciliation$discordant$record_id[grpA], collapse = ", ")))
  cat(sprintf("Group B (offered TRUE -> FALSE, no appointment): %s\n",
              paste(reconciliation$discordant$record_id[grpB], collapse = ", ")))

  write.csv(reconciliation$data, f, row.names = FALSE, na = "")
  cat(sprintf("Reconciled %d discordant records; wrote %s\n",
              nrow(reconciliation$discordant), f))
} else {
  cat("No offered/outcome discordance found (already reconciled).\n")
}
