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

# Join Euler demographics: years_since_enum is our best proxy for record age
listing_tbl <- calls |>
  left_join(
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
