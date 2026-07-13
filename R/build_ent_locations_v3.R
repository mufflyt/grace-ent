# build_ent_locations_v3.R
# Build ENT practice locations using PRIMARY sources with full addresses
#
# Primary Sources (full addresses):
#   1. Physician Compare - Medicare practice addresses (multiple per NPI)
#   2. NPPES - Primary practice address
#   3. Hospital Affiliations - Full address via CCN crosswalk (CMS Hospital Info)
#
# EXCLUDED from location analysis:
#   - Open Payments: Too noisy (conference locations, misspellings, historical)
#   - PECOS: Very low coverage (162 NPIs)

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)

# ============================================================================
# Configuration
# ============================================================================

input_file <- "ent_final_with_all_npi.xlsx"
output_file <- "ent_practice_locations_v3.xlsx"
nber_duckdb_path <- "/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb"

# ============================================================================
# Helper Functions
# ============================================================================

# Use canonical safe_duckdb() from duckdb_safe.R
source(here::here("R", "duckdb_safe.R"))

#' Extract 5-digit ZIP code from ZIP+4 or other formats
#'
#' @description
#' Standardizes ZIP code data by extracting only the first 5 digits, removing
#' ZIP+4 extensions and handling various input formats.
#'
#' @details
#' - **Pipeline step:** Utility (address normalization)
#' - **Inputs:** ZIP code strings (5-digit, ZIP+4, or malformed)
#' - **Algorithm:** 1. Coerce to character, 2. Extract first 5 digits via
#'   regex pattern "^[0-9]{5}"
#' - **Performance:** O(n) where n = string length; instant for single ZIP
#' - **Reproducibility:** Deterministic
#'
#' @param zip [character|numeric]: ZIP code in any format (12345, 12345-6789,
#'   etc.).
#' @return Character. 5-digit ZIP code, or NA if no 5-digit prefix found.
#' @examples
#' \dontrun{
#'   clean_zip("12345-6789")  # Returns "12345"
#'   clean_zip(12345)         # Returns "12345"
#' }
#' @keywords internal
clean_zip <- function(zip) {
  zip <- as.character(zip)
  stringr::str_extract(zip, "^[0-9]{5}")
}

# ============================================================================
# Main Script
# ============================================================================

cat("\n========================================\n")
cat("Building ENT Location Database (v3)\n")
cat("Primary Sources: Physician Compare, NPPES, Hospital Affiliations\n")
cat("========================================\n\n")

# Check external drive
if (!dir.exists("/Volumes/MufflySamsung")) {
  stop("External drive not mounted. Please connect MufflySamsung drive.")
}

# Load ENT data
ent_data <- read_excel(input_file)
valid_npis <- ent_data %>%
  filter(!is.na(npi_final) & npi_final != "") %>%
  pull(npi_final) %>%
  unique()

cat("ENTs with valid NPI:", length(valid_npis), "\n\n")

# Connect to DuckDB
con <- safe_duckdb(nber_duckdb_path)

# Process in chunks
chunk_size <- 500
n_chunks <- ceiling(length(valid_npis) / chunk_size)

# Initialize results
all_locations <- tibble()

# ============================================================================
# Source 1: Physician Compare (PRIMARY)
# ============================================================================

cat("Source 1: Physician Compare...\n")

pc_locations <- tibble()

for (i in seq_len(n_chunks)) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(valid_npis))
  chunk_npis <- valid_npis[start_idx:end_idx]

  npi_list <- paste0("'", chunk_npis, "'", collapse = ", ")

  chunk_result <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT DISTINCT
        CAST(NPI AS VARCHAR) as npi,
        adr_ln_1 as address_line1,
        adr_ln_2 as address_line2,
        \"City/Town\" as city,
        State as state,
        \"ZIP Code\" as zip,
        'Physician Compare' as source
      FROM physician_compare_2024_Q2
      WHERE CAST(NPI AS VARCHAR) IN (%s)
    ", npi_list))
  }, error = function(e) {
    cat("  [WARN] Chunk", i, "failed:", e$message, "\n")
    tibble()
  })

  pc_locations <- bind_rows(pc_locations, chunk_result)
}

cat("  Found", nrow(pc_locations), "location records for",
    n_distinct(pc_locations$npi), "NPIs\n")

all_locations <- bind_rows(all_locations, pc_locations)

# ============================================================================
# Source 2: NPPES (PRIMARY)
# ============================================================================

cat("\nSource 2: NPPES...\n")

nppes_locations <- tibble()

for (i in seq_len(n_chunks)) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(valid_npis))
  chunk_npis <- valid_npis[start_idx:end_idx]

  npi_list <- paste0("'", chunk_npis, "'", collapse = ", ")

  chunk_result <- tryCatch({
    DBI::dbGetQuery(con, sprintf('
      SELECT DISTINCT
        CAST(NPI AS VARCHAR) as npi,
        "Provider First Line Business Practice Location Address" as address_line1,
        "Provider Second Line Business Practice Location Address" as address_line2,
        "Provider Business Practice Location Address City Name" as city,
        "Provider Business Practice Location Address State Name" as state,
        "Provider Business Practice Location Address Postal Code" as zip,
        \'NPPES\' as source
      FROM npidata
      WHERE CAST(NPI AS VARCHAR) IN (%s)
    ', npi_list))
  }, error = function(e) {
    cat("  [WARN] Chunk", i, "failed:", e$message, "\n")
    tibble()
  })

  nppes_locations <- bind_rows(nppes_locations, chunk_result)
}

cat("  Found", nrow(nppes_locations), "location records for",
    n_distinct(nppes_locations$npi), "NPIs\n")

all_locations <- bind_rows(all_locations, nppes_locations)

# ============================================================================
# Source 3: Hospital Affiliations with CCN Crosswalk (PRIMARY)
# ============================================================================

cat("\nSource 3: Hospital Affiliations (via CCN crosswalk)...\n")

hospital_locations <- tibble()

for (i in seq_len(n_chunks)) {
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, length(valid_npis))
  chunk_npis <- valid_npis[start_idx:end_idx]

  npi_list <- paste0("'", chunk_npis, "'", collapse = ", ")

  chunk_result <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT DISTINCT
        CAST(fa.npi AS VARCHAR) as npi,
        h.address as address_line1,
        CAST(NULL AS VARCHAR) as address_line2,
        h.citytown as city,
        h.state as state,
        h.zip_code as zip,
        'Hospital Affiliation' as source
      FROM facility_affiliation fa
      JOIN cms_hospital_info h ON fa.facility_cert_number = h.facility_id
      WHERE fa.facility_type = 'Hospital'
        AND CAST(fa.npi AS VARCHAR) IN (%s)
    ", npi_list))
  }, error = function(e) {
    cat("  [WARN] Chunk", i, "failed:", e$message, "\n")
    tibble()
  })

  hospital_locations <- bind_rows(hospital_locations, chunk_result)
}

cat("  Found", nrow(hospital_locations), "location records for",
    n_distinct(hospital_locations$npi), "NPIs\n")

all_locations <- bind_rows(all_locations, hospital_locations)

DBI::dbDisconnect(con, shutdown = TRUE)

# ============================================================================
# Clean and Process
# ============================================================================

cat("\n========================================\n")
cat("Processing All Locations\n")
cat("========================================\n\n")

# Clean and standardize
all_locations <- all_locations %>%
  mutate(
    zip_5 = clean_zip(zip),
    city_clean = toupper(trimws(city)),
    state_clean = toupper(trimws(state))
  ) %>%
  filter(!is.na(zip_5) | !is.na(city_clean))

cat("Total location records:", nrow(all_locations), "\n")

# Deduplicate by NPI + ZIP
unique_locations <- all_locations %>%
  filter(!is.na(zip_5)) %>%
  group_by(npi, zip_5) %>%
  summarise(
    address_line1 = first(address_line1[!is.na(address_line1)]),
    city = first(city_clean[!is.na(city_clean)]),
    state = first(state_clean[!is.na(state_clean)]),
    sources = paste(unique(source), collapse = ", "),
    n_sources = n_distinct(source),
    .groups = "drop"
  )

cat("Unique NPI + ZIP combinations:", nrow(unique_locations), "\n")

# ============================================================================
# Add RUCA Codes
# ============================================================================

cat("\n========================================\n")
cat("Adding RUCA Codes\n")
cat("========================================\n\n")

con <- safe_duckdb(nber_duckdb_path)

unique_zips <- unique(unique_locations$zip_5[!is.na(unique_locations$zip_5)])
zip_list <- paste0("'", unique_zips, "'", collapse = ", ")

ruca_lookup <- DBI::dbGetQuery(con, sprintf("
  SELECT zip, ruca_code, ruca_category
  FROM ruca_zip_unified
  WHERE zip IN (%s)
  AND ruca_year = 2020
", zip_list))

DBI::dbDisconnect(con, shutdown = TRUE)

# Join RUCA codes
unique_locations <- unique_locations %>%
  left_join(ruca_lookup, by = c("zip_5" = "zip")) %>%
  mutate(
    # HRSA/FORHP definition: RUCA 4-10 = Rural
    is_rural = case_when(
      is.na(ruca_code) ~ NA,
      ruca_code >= 4 ~ TRUE,
      TRUE ~ FALSE
    ),
    rurality = case_when(
      is.na(ruca_code) ~ "Unknown",
      ruca_code <= 3 ~ "Urban",
      TRUE ~ "Rural"
    )
  )

# ============================================================================
# Build Summary Statistics
# ============================================================================

cat("========================================\n")
cat("Building Summary\n")
cat("========================================\n\n")

# Location summary per NPI
location_summary <- unique_locations %>%
  group_by(npi) %>%
  summarise(
    n_locations = n(),
    n_rural_locations = sum(is_rural, na.rm = TRUE),
    n_urban_locations = sum(!is_rural, na.rm = TRUE),
    any_rural = any(is_rural, na.rm = TRUE),
    all_rural = all(is_rural, na.rm = TRUE) & !all(is.na(is_rural)),
    n_states = n_distinct(state, na.rm = TRUE),
    states = paste(sort(unique(state[!is.na(state)])), collapse = ", "),
    zips = paste(sort(unique(zip_5)), collapse = ", "),
    all_sources = paste(unique(unlist(strsplit(sources, ", "))), collapse = ", "),
    .groups = "drop"
  )

# ============================================================================
# Statistics
# ============================================================================

cat("=== COVERAGE STATISTICS ===\n\n")

cat("Source contributions:\n")
source_summary <- all_locations %>%
  count(source) %>%
  arrange(desc(n))
print(source_summary)

cat("\nCoverage:\n")
cat("  ENTs with location data:", nrow(location_summary),
    sprintf("(%.1f%% of %d)\n", 100 * nrow(location_summary) / length(valid_npis), length(valid_npis)))

cat("\nMultiple Locations:\n")
cat("  ENTs with 2+ locations:", sum(location_summary$n_locations >= 2),
    sprintf("(%.1f%%)\n", 100 * mean(location_summary$n_locations >= 2)))
cat("  ENTs with 3+ locations:", sum(location_summary$n_locations >= 3),
    sprintf("(%.1f%%)\n", 100 * mean(location_summary$n_locations >= 3)))

cat("\nMulti-State Practice:\n")
cat("  ENTs practicing in 2+ states:", sum(location_summary$n_states >= 2, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(location_summary$n_states >= 2, na.rm = TRUE)))

cat("\nRural Coverage (RUCA 4-10):\n")
cat("  ENTs with ANY rural location:", sum(location_summary$any_rural, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(location_summary$any_rural, na.rm = TRUE)))
cat("  ENTs with ALL rural locations:", sum(location_summary$all_rural, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(location_summary$all_rural, na.rm = TRUE)))

cat("\n=== LOCATION DISTRIBUTION ===\n\n")
loc_dist <- location_summary %>%
  mutate(loc_group = case_when(
    n_locations == 1 ~ "1",
    n_locations == 2 ~ "2",
    n_locations == 3 ~ "3",
    n_locations <= 5 ~ "4-5",
    n_locations <= 10 ~ "6-10",
    TRUE ~ "11+"
  )) %>%
  count(loc_group) %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(loc_dist)

# ============================================================================
# Save Results
# ============================================================================

cat("\n========================================\n")
cat("Saving Results\n")
cat("========================================\n\n")

# Join summary back to original ENT data
ent_enriched <- ent_data %>%
  left_join(location_summary, by = c("npi_final" = "npi"))

# Source summary table
source_table <- tibble(
  source = c("Physician Compare", "NPPES", "Hospital Affiliations"),
  description = c(
    "Medicare practice addresses (multiple per NPI)",
    "Primary practice location from NPI registry",
    "Hospital privileges with full address (via CCN crosswalk)"
  ),
  records = c(nrow(pc_locations), nrow(nppes_locations), nrow(hospital_locations)),
  unique_npis = c(
    n_distinct(pc_locations$npi),
    n_distinct(nppes_locations$npi),
    n_distinct(hospital_locations$npi)
  )
)

write_xlsx(
  list(
    "ENT Summary" = ent_enriched,
    "All Locations" = unique_locations,
    "Location Distribution" = loc_dist,
    "Source Summary" = source_table
  ),
  output_file
)

cat("Saved to:", output_file, "\n")
cat("Sheets: ENT Summary, All Locations, Location Distribution, Source Summary\n")

cat("\n========================================\n")
cat("COMPLETE\n")
cat("========================================\n")
