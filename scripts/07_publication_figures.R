# =============================================================================
# 07_publication_figures.R
# Generates all seven publication figures for the registry validity manuscript
# by calling the create_all_registry_figures() wrapper from the external
# helper script.
#
# Run after 01_clean_phase2_call_log.R (produces ent_phase2_all_*.csv).
# Output: output/figures/  (PNG 600 DPI + PDF, timestamped)
# =============================================================================

library(here)
library(readr)
library(dplyr)

# Source the figure-function library (contains all seven create_* functions)
source(here("scripts", "ent_registry_7_figures.R"))

# =============================================================================
# 1. LOAD RAW DATA
# =============================================================================
f_all <- tail(sort(list.files(here("data", "processed"),
                               pattern = "ent_phase2_all", full.names = TRUE)), 1)
calls <- read_csv(f_all, show_col_types = FALSE) |>
  mutate(npi = as.character(npi))

euler <- read_csv(
  path.expand("~/isochrones/publication_materials/figures/ent_data_integrity/ent_euler_zone_npi_assignments_with_demographics.csv"),
  show_col_types = FALSE
) |> rename(npi = npi_chr) |> mutate(npi = as.character(npi))

# =============================================================================
# 2. BUILD LISTING TABLE
# =============================================================================
# Derive validity outcomes needed by the helper script

calls <- calls |>
  mutate(
    # Normalise REDCap datasets field → overlap_zone
    overlap_zone = case_when(
      datasets %in% c("ABO, NPI but not ENT Health",
                      "ABO, NPI but no ENTHealth") ~ "NPPES+ABOto",
      datasets == "ABO, NPI, ENT Health"            ~ "Triple match",
      TRUE                                          ~ NA_character_
    ),

    # physician_at_location: confirmed at the NPPES-listed address
    physician_at_location = case_when(
      taking_new_patients %in% c(
        "Yes",
        "No-practice is full but physician does still see established patients",
        "No-on leave of absence (like parental leave)"
      ) ~ TRUE,
      taking_new_patients == "No-changed practices" ~ FALSE,
      TRUE ~ NA
    ),

    # physician_active: confirmed still practicing medicine anywhere
    physician_active = case_when(
      taking_new_patients %in% c(
        "Yes",
        "No-practice is full but physician does still see established patients",
        "No-on leave of absence (like parental leave)",
        "No-changed practices"
      ) ~ TRUE,
      taking_new_patients == "No-retired or no longer practicing medicine" ~ FALSE,
      TRUE ~ NA
    ),

    # invalid_reason using labels recognized by collapse_invalid_reason()
    invalid_reason = case_when(
      phone_validity_flag == "invalid_format"                              ~ "wrong number",
      taking_new_patients == "No-changed practices"                        ~ "relocated",
      taking_new_patients == "No-retired or no longer practicing medicine" ~ "retired inactive",
      TRUE                                                                 ~ NA_character_
    ),

    # listing_status labels recognized by prepare_registry_listings()
    listing_status = case_when(
      physician_at_location == TRUE                                        ~ "confirmed_valid",
      physician_at_location == FALSE                                       ~ "confirmed_invalid",
      physician_active      == FALSE                                       ~ "confirmed_invalid",
      phone_validity_flag   == "invalid_format"                            ~ "confirmed_invalid",
      is.na(office_answered) & is.na(taking_new_patients)                 ~ "not_attempted",
      TRUE                                                                 ~ "unresolved"
    ),

    # Boolean flags passed directly
    location_valid  = listing_status == "confirmed_valid",
    workforce_valid = physician_active == TRUE,
    attempted       = listing_status != "not_attempted"
  )

# Fail closed on dataset-label drift: unmapped rows must never silently lose
# their stratum and fall through to the weight-1 fallback in the weights below.
unmapped_zone <- calls |>
  filter(is.na(overlap_zone) & !is.na(datasets) & nzchar(trimws(datasets)))
if (nrow(unmapped_zone) > 0) {
  stop(sprintf(
    "%d record(s) have unrecognised datasets values: %s. Extend the overlap_zone mapping deliberately rather than letting rows silently lose their stratum.",
    nrow(unmapped_zone),
    paste(unique(unmapped_zone$datasets), collapse = "; ")
  ), call. = FALSE)
}

# Join Euler demographics: years_since_enum is our best proxy for record age
# Drop blank-keyed rows (ENTHealth-only listings without an NPI): they can
# never match a call and would multiply any blank-keyed call row.
euler <- euler |> filter(!is.na(npi) & nzchar(trimws(npi)))
listing_tbl <- calls |>
  mysterycall::mysterycall_safe_left_join(
    euler |> select(npi, zone, years_since_enum, bog_region,
                    billed_any_part_b, credential_class),
    by = "npi"
  ) |>
  # Rename to match what the helper script expects for Figure 5
  rename(years_since_update = years_since_enum,
         recent_medicare_billing = billed_any_part_b,
         census_region = bog_region) |>
  # Sampling weight: population N / sampled n per stratum (weight = 1 for unmatched)
  mutate(
    sampling_weight = case_when(
      overlap_zone == "NPPES+ABOto"  ~ 6748 / sum(overlap_zone == "NPPES+ABOto",  na.rm = TRUE),
      overlap_zone == "Triple match" ~ 4573 / sum(overlap_zone == "Triple match",  na.rm = TRUE),
      TRUE ~ 1
    )
  )

message(sprintf("Listing table: %d rows, %d columns", nrow(listing_tbl), ncol(listing_tbl)))
message("Overlap zone counts:")
print(table(listing_tbl$overlap_zone, useNA = "ifany"))

# =============================================================================
# 3. BUILD POPULATION TABLE
# =============================================================================
population_tbl <- tibble::tibble(
  overlap_zone = c("NPPES only",
                   "NPPES + ABOto",
                   "NPPES + ENTHealth",
                   "Triple match"),
  population_n = c(2756L, 6748L, 711L, 4573L)
)

# The population counts above are hardcoded from the Euler zone file. Fail
# loud if the file ever regenerates with different counts - silently stale
# population denominators would corrupt every weighted figure.
euler_zone_counts <- euler |> count(zone)
expected_zones <- c("1_nppes_only" = 2756L, "2_nppes_board" = 6748L,
                    "3_nppes_ent" = 711L, "8_triple_match" = 4573L)
for (z in names(expected_zones)) {
  actual <- euler_zone_counts$n[euler_zone_counts$zone == z]
  if (length(actual) == 0 || actual != expected_zones[[z]]) {
    stop(sprintf(
      "Euler zone '%s' count is %s but population_tbl hardcodes %d. Update population_tbl and the sampling_weight numerators deliberately.",
      z, ifelse(length(actual) == 0, "missing", as.character(actual)), expected_zones[[z]]
    ), call. = FALSE)
  }
}

# =============================================================================
# 4. GENERATE ALL SEVEN FIGURES
# =============================================================================
figure_artifacts <- create_all_registry_figures(
  listing_tbl    = listing_tbl,
  population_tbl = population_tbl,
  save_dir       = here("output", "figures"),
  minimum_state_n = 5L
)

message("\nAll figures saved to: ", here("output", "figures"))
message("Artifact names: ", paste(names(figure_artifacts), collapse = ", "))
