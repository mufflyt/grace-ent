#!/usr/bin/env Rscript
# =============================================================================
# STROBE participant-flow figure for the ENT access study, generated with the
# study package: mysterycall_strobe_flow() (returns a ggplot -> PNG/TIFF, no
# Graphviz required). Counts come from the analytic waterfall.
#
#   960 calls placed
#   -> 749 complete data collection      (-211 incomplete)
#   -> 731 analytic, subspecialty known  (-18 subspecialty undetermined)
#   -> 731 access model (offer yes/no)
#   -> 430 timeliness model (business-day wait)   (-301 no appointment / no wait)
#
# Output: model_output/strobe_flow_mysterycall.png (+ .tiff)
# =============================================================================

suppressMessages(devtools::load_all("~/mysterycall", quiet = TRUE))
dir.create("model_output", showWarnings = FALSE)

mysterycall_strobe_flow(
  n_total    = 960,   # calls placed
  n_calldate = 749,   # complete data collection
  n_included = 731,   # analytic sample (subspecialty determinable)
  n_logistic = 731,   # access model (appointment offered vs not)
  n_waittime = 430,   # timeliness model (business-day wait recorded)
  label_total    = "Calls placed",
  label_calldate = "Complete data collection",
  label_included = "Analytic sample (subspecialty known)",
  label_logistic = "Access model (appointment offered vs not)",
  label_waittime = "Timeliness model (business-day wait)",
  label_excl_calldate = "Incomplete data collection",
  label_excl_screen   = "Subspecialty undetermined",
  label_excl_waittime = "No appointment offered / no wait recorded",
  title       = "Participant flow (STROBE)",
  output_path = "model_output/strobe_flow_mysterycall.png"
)
cat("Wrote model_output/strobe_flow_mysterycall.png\n")
