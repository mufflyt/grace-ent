#!/usr/bin/env Rscript

#' @title Cross-Reference Mystery Caller Study with Medicaid Roster
#'
#' @description
#' Loads mystery caller study data and identifies physicians who were called
#' that appear in the Medicaid acceptance roster. Extracts waiting times,
#' insurance acceptance, and validates matching algorithm.

library(readr)
library(dplyr)
library(here)

cat("\n")
cat(strrep("=", 80), "\n")
cat("MYSTERY CALLER + MEDICAID ROSTER OVERLAP ANALYSIS\n")
cat(strrep("=", 80), "\n\n")

# ============================================================================
# Step 1: Load mystery caller study data
# ============================================================================

cat("Step 1: Loading mystery caller study data...\n\n")

mystery_file <- "~/mystery_shopper_data/Mystery caller revised 7.25.2019.csv"

mystery_data <- read_csv(mystery_file, col_types = cols(.default = col_character())) %>%
  mutate(
    across(everything(), trimws),
    # Standardize physician name for matching
    physician_name_clean = tolower(gsub("[^a-z0-9\\s]", "", physician_name)),
    # Numeric waiting time
    wait_days = as.numeric(`Business_Days_Until_Appt`),
    # Accept Medicare (direct data)
    accepts_medicare = `Accept Medicare` %in% c("Yes", "yes", "Y", "y"),
    # Accepting new patients
    accepting_new = `Accepting new patients` %in% c("Yes", "yes", "Y", "y")
  ) %>%
  filter(!is.na(physician_name), nchar(physician_name) > 0)

cat(sprintf("✓ Loaded %s mystery caller records\n", format(nrow(mystery_data), big.mark = ",")))
cat(sprintf("  - With valid waiting times: %s\n", format(sum(!is.na(mystery_data$wait_days)), big.mark = ",")))
cat(sprintf("  - Accept Medicare (direct): %s\n", format(sum(mystery_data$accepts_medicare, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  - Accepting new patients: %s\n\n", format(sum(mystery_data$accepting_new, na.rm = TRUE), big.mark = ",")))

# ============================================================================
# Step 2: Load Medicaid roster
# ============================================================================

cat("Step 2: Loading Medicaid roster...\n\n")

medicaid_file <- here("artifacts", "medicaid_matching", "obgyn_medicaid_matches_by_state.csv")

medicaid_matches <- read_csv(medicaid_file, col_types = cols(.default = col_character())) %>%
  select(npi, first_name_lower, last_name_lower, practice_address_state, medicaid_state, match_type) %>%
  distinct(npi, .keep_all = TRUE) %>%
  mutate(
    full_name_medicaid = paste(first_name_lower, last_name_lower),
    full_name_medicaid_clean = tolower(gsub("[^a-z0-9\\s]", "", full_name_medicaid))
  )

cat(sprintf("✓ Loaded %s Medicaid-accepting physicians\n\n", format(nrow(medicaid_matches), big.mark = ",")))

# ============================================================================
# Step 3: Match mystery caller physicians to Medicaid roster
# ============================================================================

cat("Step 3: Matching mystery caller physicians to Medicaid roster...\n\n")

# Join on name (fuzzy matching not available, so exact name match only)
overlap <- mystery_data %>%
  left_join(
    medicaid_matches %>% select(npi, first_name_lower, last_name_lower, full_name_medicaid_clean),
    by = c("physician_name_clean" = "full_name_medicaid_clean")
  ) %>%
  filter(!is.na(npi))  # Only those with Medicaid match

cat(sprintf("✓ Matched %s mystery caller physicians to Medicaid roster\n\n", format(nrow(overlap), big.mark = ",")))

# HARDENING (adversarial audit cycle 23): the join key above is a cleaned
# NAME STRING, not npi. `distinct(npi, .keep_all = TRUE)` in Step 2 only
# guarantees no duplicate NPI rows in medicaid_matches -- it does NOT
# guarantee no duplicate NAMES across different NPIs (two different
# Medicaid-accepting physicians can share a common name). Such a collision
# would silently fan a single mystery-caller record out into >1 row, both
# inflating the "physicians" counts reported below AND misattributing one
# physician's wait-time/insurance data to another. Not silently resolved
# (which NPI is "correct" for a shared name is not decidable from this
# data), but surfaced so it is never silently invisible.
ambiguous_medicaid_names <- medicaid_matches %>%
  dplyr::count(full_name_medicaid_clean) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(full_name_medicaid_clean)
n_ambiguous_overlap_rows <- sum(overlap$physician_name_clean %in% ambiguous_medicaid_names)
if (n_ambiguous_overlap_rows > 0) {
  warning(sprintf(
    paste(
      "[NAME COLLISION] %d overlap row(s) matched a name shared by >1 distinct",
      "Medicaid NPI -- downstream physician counts/wait-time stats may be",
      "inflated or misattributed. Exact-name matching cannot disambiguate;",
      "consider adding a state/NPI cross-check before trusting these rows."
    ),
    n_ambiguous_overlap_rows
  ), call. = FALSE)
}

# ============================================================================
# Step 4: Analysis
# ============================================================================

cat("Step 4: Analysis of mystery caller physicians accepting Medicaid\n\n")

if (nrow(overlap) > 0) {
  cat("Waiting time statistics (days):\n")
  wait_stats <- overlap %>%
    filter(!is.na(wait_days)) %>%
    summarize(
      n = n(),
      mean = mean(wait_days, na.rm = TRUE),
      median = median(wait_days, na.rm = TRUE),
      min = min(wait_days, na.rm = TRUE),
      max = max(wait_days, na.rm = TRUE),
      sd = sd(wait_days, na.rm = TRUE)
    )
  print(wait_stats)
  cat("\n")

  cat("Insurance acceptance breakdown (mystery caller direct report):\n")
  insurance <- overlap %>%
    group_by(accepts_medicare) %>%
    summarize(n = n(), .groups = "drop")
  print(insurance)
  cat("\n")

  cat("Accepting new patients breakdown:\n")
  new_patient <- overlap %>%
    group_by(accepting_new) %>%
    summarize(n = n(), .groups = "drop")
  print(new_patient)
  cat("\n")

} else {
  cat("⚠️  No matches found with exact name matching.\n")
  cat("Recommendation: Use fuzzy name matching or NPI from another source.\n\n")
}

# ============================================================================
# Step 5: Save results
# ============================================================================

cat("Step 5: Saving overlap results...\n\n")

output_dir <- here("artifacts", "medicaid_validation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "mystery_caller_medicaid_overlap.csv")
write_csv(overlap %>% select(
  physician_name,
  city,
  state,
  npi,
  first_name_lower,
  last_name_lower,
  wait_days,
  accepts_medicare,
  accepting_new,
  `Accept Medicare`,
  Notes
), output_file)

cat(sprintf("✓ Saved to: %s\n\n", basename(output_file)))

cat(strrep("=", 80), "\n")
cat("SUMMARY\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("Mystery caller study size: %s physicians\n", format(nrow(mystery_data), big.mark = ",")))
cat(sprintf("Medicaid roster size: %s physicians\n", format(nrow(medicaid_matches), big.mark = ",")))
cat(sprintf("Overlap (called & on Medicaid): %s physicians\n\n", format(nrow(overlap), big.mark = ",")))

if (nrow(overlap) > 0) {
  cat("This validates your Medicaid matching algorithm against real-world call data.\n")
  cat("Waiting times and insurance acceptance can now inform Phase 3 validation.\n")
}

