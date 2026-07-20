# =============================================================================
# 04_nppes_only_ruca.R
# ENT Mystery Caller Study — NPPES-Only Physician File with RUCA
#
# Extracts ENT physicians in the "NPPES only" Euler zone:
#   - In NPPES 207Y* taxonomy extract
#   - NOT matched in ABOto board-certification file
#   - NOT matched in ENTHealth directory
#
# Source: isochrones publication materials (ent_data_integrity figures),
#         generated from the Euler diagram analysis of the three ENT registries.
#
# Input:  isochrones/publication_materials/.../ent_npi_only_cohort_demographics.csv
#         data/raw/ruca_2020_zip_crosswalk_usda.csv
# Output: data/processed/nppes_only_ent_ruca_<timestamp>.csv
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(stringr)

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
NPPES_ONLY_FILE <- file.path(
  path.expand("~"),
  "isochrones/publication_materials/figures/ent_data_integrity",
  "ent_npi_only_cohort_demographics.csv"
)
RUCA_FILE <- here("data", "raw", "ruca_2020_zip_crosswalk_usda.csv")
OUT_DIR   <- here("data", "processed")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(NPPES_ONLY_FILE))
  stop("NPPES-only demographics file not found: ", NPPES_ONLY_FILE)

# -----------------------------------------------------------------------------
# 1. Load NPPES-only cohort
# -----------------------------------------------------------------------------
message("Loading NPPES-only ENT cohort ...")
nppes_only <- read_csv(NPPES_ONLY_FILE, show_col_types = FALSE)
message(sprintf("  %d physicians loaded", nrow(nppes_only)))

# -----------------------------------------------------------------------------
# 2. Load RUCA 2020 zip-code crosswalk
# -----------------------------------------------------------------------------
message("Loading RUCA 2020 zip-code crosswalk ...")
ruca <- read_csv(RUCA_FILE, show_col_types = FALSE) |>
  rename_with(tolower) |>
  mutate(
    zip5      = str_pad(as.character(zipcode), 5, "left", "0"),
    ruca_code = as.numeric(primaryruca)
  ) |>
  select(zip5, ruca_code) |>
  distinct(zip5, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 3. Join RUCA and classify
# -----------------------------------------------------------------------------
message("Joining RUCA codes and classifying rurality ...")
nppes_only <- nppes_only |>
  mutate(zip5 = str_pad(substr(as.character(zip), 1, 5), 5, "left", "0")) |>
  left_join(ruca, by = "zip5") |>
  mutate(
    ruca_category = case_when(
      is.na(ruca_code) ~ NA_character_,
      ruca_code <= 3   ~ "Urban",
      ruca_code <= 6   ~ "Suburban",
      TRUE             ~ "Rural"
    ),
    ruca_binary = factor(
      case_when(
        is.na(ruca_code) ~ NA_character_,
        ruca_code >= 7   ~ "Rural",
        TRUE             ~ "Non-Rural"
      ),
      levels = c("Non-Rural", "Rural")
    )
  )

# -----------------------------------------------------------------------------
# 4. Restrict to US addresses (exclude APO/FPO/DPO and non-US state codes)
# -----------------------------------------------------------------------------
us_states <- c(
  state.abb, "DC", "PR", "VI", "GU", "AS", "MP"
)

nppes_only_us <- nppes_only |>
  filter(state %in% us_states)

n_excluded <- nrow(nppes_only) - nrow(nppes_only_us)
message(sprintf("  Excluded %d non-US addresses (APO/FPO/foreign)", n_excluded))

# -----------------------------------------------------------------------------
# 5. Select output columns
# -----------------------------------------------------------------------------
out <- nppes_only_us |>
  select(
    npi,
    last_name,
    first_name,
    middle_name,
    credential,
    gender,
    address_line1,
    city,
    state,
    zip5,
    phone,
    practice_phone,
    primary_taxonomy,
    enumeration_year,
    years_since_enumeration,
    is_deactivated,
    ruca_code,
    ruca_category,
    ruca_binary
  )

# -----------------------------------------------------------------------------
# 6. Summary
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 60), "\n")
cat("NPPES-only ENT Cohort (US addresses)\n")
cat(strrep("=", 60), "\n")
cat(sprintf("  Total: %d\n", nrow(out)))

cat("\n  By RUCA binary (Rural vs Non-Rural):\n")
print(count(out, ruca_binary))

cat("\n  By RUCA category:\n")
print(count(out, ruca_category))

cat("\n  Credential breakdown:\n")
cred_tbl <- out |>
  mutate(credential_clean = case_when(
    str_detect(toupper(credential), "^M\\.?D\\.?$|^MD$") ~ "MD",
    str_detect(toupper(credential), "^D\\.?O\\.?$|^DO$") ~ "DO",
    is.na(credential) | credential == ""                 ~ "Missing",
    TRUE                                                 ~ "Other"
  ))
print(count(cred_tbl, credential_clean, sort = TRUE))

cat("\n  Enumeration era:\n")
era_tbl <- out |>
  mutate(era = case_when(
    years_since_enumeration <= 5  ~ "<=5 yrs (resident/fellow plausible)",
    years_since_enumeration <= 10 ~ "6-10 yrs (pre-cert window)",
    years_since_enumeration <= 20 ~ "11-20 yrs (mid-career)",
    TRUE                          ~ ">20 yrs (senior)"
  ))
print(count(era_tbl, era))

cat("\n  Rural physicians by state:\n")
print(filter(out, ruca_binary == "Rural") |> count(state, sort = TRUE))

# -----------------------------------------------------------------------------
# 7. Save
# -----------------------------------------------------------------------------
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
out_path  <- file.path(OUT_DIR, paste0("nppes_only_ent_ruca_", timestamp, ".csv"))

write_csv(out, out_path)
message(sprintf("\nSaved %d records to: %s", nrow(out), out_path))
message("Done.")
