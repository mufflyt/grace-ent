#!/usr/bin/env Rscript
# =============================================================================
# 05b_validity_mysteryroster.R
# The registry-validity analysis of 05_database_accuracy_paper.R, sections 2-4
# and 8, expressed through the mysteryroster package. Produces the same objects
# the legacy script produced (listing_status, weights, bounds) from the same
# inputs, so tests/testthat/test-mysteryroster-parity.R can diff them against
# the committed output/tables/*.csv the legacy script wrote on 2026-07-20.
#
# Inputs:  data/processed/ent_phase2_all_<latest>.csv   (960 call-log records)
# Stratum populations: the two sampled Euler zones' N, as stated in README.md
#   and the manuscript (NPPES+ABOto 6,748; triple-match 4,573). The legacy
#   script read them from the Euler zone CSV under ~/isochrones, which is not
#   committed anywhere (recovery item: regenerate with
#   isochrones scripts/build_ent_euler_table1.R).
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(here); library(mysteryroster) })

validity_from_call_log <- function(path = tail(sort(list.files(here("data", "processed"),
                                                              pattern = "^ent_phase2_all_.*\\.csv$", full.names = TRUE)), 1),
                                   pop_N = c("NPPES+ABOto" = 6748L, "NPPES+ABOto+ENTHealth" = 4573L)) {
  calls <- read_csv(path, show_col_types = FALSE) |> mutate(npi = as.character(npi))
  # Section 2 of the legacy script: the REDCap `datasets` label -> overlap stratum.
  # Fail closed on label drift: both historical spellings are mapped, and any
  # new variant stops the pipeline instead of silently dropping the row's weight.
  known_datasets <- c("ABO, NPI but not ENT Health", "ABO, NPI but no ENTHealth", "ABO, NPI, ENT Health")
  unmapped_datasets <- setdiff(unique(calls$datasets[!is.na(calls$datasets)]), known_datasets)
  if (length(unmapped_datasets)) {
    stop("05b: unrecognised `datasets` label(s), update the stratum mapping: ",
         paste(unmapped_datasets, collapse = "; "), call. = FALSE)
  }
  # Phone-validity decisions, recorded explicitly:
  #   invalid_format          -> FALSE: a malformed number is evidence against the listing.
  #   missing                 -> NA: no number was transcribed, so validity is unknown;
  #                            an untranscribed field is not evidence against the listing.
  #   valid                   -> TRUE.
  #   area_code_state_mismatch -> TRUE: area codes are portable (mobile numbers move
  #                            across state lines), so a mismatch is a QC flag,
  #                            not evidence of a wrong number.
  # The legacy `phone_validity_flag != "invalid_format"` construction was fail-open:
  # any new flag mysterycall adds (a future "disconnected", say) would silently
  # count as valid. Unrecognised flags now stop the pipeline instead.
  known_flags <- c("valid", "invalid_format", "missing", "area_code_state_mismatch")
  unmapped_flags <- setdiff(unique(calls$phone_validity_flag[!is.na(calls$phone_validity_flag)]), known_flags)
  if (length(unmapped_flags)) {
    stop("05b: unrecognised phone_validity_flag value(s), update the mapping: ",
         paste(unmapped_flags, collapse = "; "), call. = FALSE)
  }
  calls <- calls |> mutate(
    stratum = case_when(
      datasets %in% c("ABO, NPI but not ENT Health", "ABO, NPI but no ENTHealth") ~ "NPPES+ABOto",
      datasets == "ABO, NPI, ENT Health"                                         ~ "NPPES+ABOto+ENTHealth",
      TRUE                                                                        ~ NA_character_),
    phone_valid = case_when(
      phone_validity_flag == "invalid_format"                            ~ FALSE,
      phone_validity_flag %in% "missing" | is.na(phone_validity_flag)    ~ NA,
      TRUE                                                               ~ TRUE))
  # Section 3: statuses from dispositions, through the package's ENT codebook.
  # phone_valid_na = "valid" is the decision recorded above: unknown validity
  # is not evidence against the listing, so missing values are treated as valid
  # numbers -- explicitly, rather than by the package's warning default.
  calls <- classify_listing_status(calls, disposition = "taking_new_patients", reached = "office_answered",
                                   phone_valid = "phone_valid", map = ent_disposition_map(),
                                   phone_valid_na = "valid")
  # Section 4: design weights = N / n per stratum (legacy: pop_N / n_sampled).
  calls <- listing_weights(calls, pop_N = pop_N, stratum = "stratum")
  # Section 8: bounds.
  bounds <- validity_sensitivity_bounds(calls)
  list(calls = calls, bounds = bounds,
       status_table = calls |> count(listing_status, name = "n") |> mutate(percent = round(100 * n / sum(n), 2)),
       cascade_by_zone = calls |> filter(!is.na(stratum)) |> group_by(overlap_zone = stratum) |> summarise(
         n_total = n(), n_reached = sum(office_answered %in% TRUE),
         n_at_location = sum(physician_at_location == "confirmed_yes"),
         # legacy cascade stage: active AND at the listed location (05 section 6, active_lgl)
         n_active = sum(physician_active == "confirmed_yes" & physician_at_location == "confirmed_yes"),
         n_new_patients = sum(taking_new_patients %in% "Yes"), .groups = "drop"))
}

if (sys.nframe() == 0L) {
  res <- validity_from_call_log()
  print(res$status_table); print(res$bounds); print(res$cascade_by_zone)
  print(unique(res$calls[, c("stratum", "pop_N", "n_sampled", "weight")]) |> filter(!is.na(stratum)))
}
