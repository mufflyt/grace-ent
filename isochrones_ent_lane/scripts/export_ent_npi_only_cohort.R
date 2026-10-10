#!/usr/bin/env Rscript
# =============================================================================
# Export the ENT Euler "NPI Only" cohort with demographics.
#
# Cohort definition: physicians in the NPPES 207Y* universe (CONUS, individual)
# who are NOT in the active ABOto board-certified set and NOT in ENThealth.
# This is the 2,954-physician zone in publication_materials/figures/
# ent_data_integrity/ent_registry_overlap_euler_v2.png.
#
# Demographics joined from the February 2026 NPPES dissemination file
# (nber_my_duckdb.duckdb).
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(arrow)
  library(DBI)
  library(duckdb)
})

source(here::here("R", "ent_registry_counts.R"))

ct <- compute_ent_registry_counts()

# Reproduce the set-membership logic from compute_ent_registry_counts() so we
# can isolate the NPI-only NPIs (no helper exposes them directly).
abohns_matched <- read_csv(ct$input_paths$abohns_matched, show_col_types = FALSE)
nppes_all      <- read_parquet(ct$input_paths$nppes)
ent_all        <- read_csv(ct$input_paths$ent, show_col_types = FALSE)

if (!exists("convert_state_name_to_abbrev", mode = "function")) {
  source(here::here("R", "utils", "state_normalization.R"))
}
abohns_matched <- abohns_matched %>%
  mutate(state_code = unname(convert_state_name_to_abbrev(state_full)))

ent_all <- ent_all %>% filter(!state %in% NON_CONTIGUOUS_CODES | is.na(state))
abohns_conus <- abohns_matched %>%
  filter(!is.na(state_code) & !state_code %in% NON_CONTIGUOUS_CODES)
abohns_active <- abohns_conus %>%
  filter(practicing_status == "Practicing", !is.na(npi))
nppes <- nppes_all %>% filter(!state %in% NON_CONTIGUOUS_CODES, !is.na(npi))

npi_nppes <- unique(as.character(nppes$npi))
npi_board <- unique(as.character(abohns_active$npi))
npi_ent   <- unique(as.character(ent_all$npi[!is.na(ent_all$npi)]))

npi_only <- setdiff(setdiff(npi_nppes, npi_board), npi_ent)

stopifnot(
  "NPI-only zone count must match Euler diagram (2,954)" =
    length(npi_only) == ct$zones$nppes_only
)

cat(sprintf("NPI-only cohort size: %d (matches Euler zone %d)\n",
            length(npi_only), ct$zones$nppes_only))

# ── Base record from the ENT-NPPES parquet (already filtered to 207Y*) ──────
base <- nppes %>%
  mutate(npi_chr = as.character(npi)) %>%
  filter(npi_chr %in% npi_only) %>%
  distinct(npi_chr, .keep_all = TRUE) %>%
  select(
    npi               = npi_chr,
    last_name, first_name, middle_name, credential,
    address_line1, city, state, zip, phone,
    primary_taxonomy, all_taxonomy_codes
  )

# ── Join richer demographics from Feb 2026 NPPES dissemination ──────────────
if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13); fixes broken un-suffixed mount
duckdb_path <- load_paths(verbose = FALSE)$nber_duckdb_main
con <- dbConnect(duckdb::duckdb(), duckdb_path, read_only = TRUE)

npi_list_sql <- paste(npi_only, collapse = ",")
demo_q <- sprintf('
  SELECT
    "NPI"::VARCHAR                                                       AS npi,
    "Provider Sex Code"                                                  AS gender,
    "Provider Enumeration Date"                                          AS enumeration_date,
    "Last Update Date"                                                   AS last_update_date,
    "NPI Deactivation Date"                                              AS deactivation_date,
    "NPI Deactivation Reason Code"                                       AS deactivation_reason,
    "NPI Reactivation Date"                                              AS reactivation_date,
    "Provider Credential Text"                                           AS nppes_credentials,
    "Provider Business Practice Location Address Telephone Number"      AS practice_phone,
    "Healthcare Provider Taxonomy Code_1"                                AS taxonomy_1,
    "Healthcare Provider Taxonomy Code_2"                                AS taxonomy_2,
    "Healthcare Provider Taxonomy Code_3"                                AS taxonomy_3,
    "Provider License Number_1"                                          AS license_1,
    "Provider License Number State Code_1"                               AS license_state_1
  FROM "NPPES_Data_Dissemination_February_2026_npidata_pfile_20050523_20260208_csv"
  WHERE "NPI" IN (%s)
', npi_list_sql)

demo <- dbGetQuery(con, demo_q)
dbDisconnect(con, shutdown = TRUE)

cat(sprintf("Pulled NPPES demographics for %d / %d NPIs\n",
            nrow(demo), length(npi_only)))

# ── Compose output ──────────────────────────────────────────────────────────
out <- base %>%
  left_join(demo, by = "npi") %>%
  mutate(
    enumeration_year = suppressWarnings(as.integer(substr(enumeration_date, 7, 10))),
    years_since_enumeration = 2026L - enumeration_year,
    is_deactivated   = !is.na(deactivation_date) & nchar(deactivation_date) > 0,
    cohort           = "NPI_only (NPPES 207Y* but not ABOto board-cert, not ENThealth)"
  ) %>%
  select(
    npi, cohort,
    last_name, first_name, middle_name, credential, nppes_credentials,
    gender,
    enumeration_date, enumeration_year, years_since_enumeration,
    last_update_date,
    is_deactivated, deactivation_date, deactivation_reason, reactivation_date,
    address_line1, city, state, zip,
    phone, practice_phone,
    primary_taxonomy, taxonomy_1, taxonomy_2, taxonomy_3,
    all_taxonomy_codes,
    license_1, license_state_1
  ) %>%
  arrange(state, last_name, first_name)

out_dir  <- here::here("publication_materials", "figures", "ent_data_integrity")
out_path <- file.path(out_dir, "ent_npi_only_cohort_demographics.csv")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write_csv(out, out_path, na = "")

cat(sprintf("Wrote %d rows x %d cols\n  -> %s\n",
            nrow(out), ncol(out), out_path))

# ── Quick demographic summary printed to console ────────────────────────────
cat("\n--- Gender ---\n");        print(table(out$gender, useNA = "ifany"))
cat("\n--- Top 10 states ---\n"); print(sort(table(out$state), decreasing = TRUE)[1:10])
cat("\n--- Enumeration year quartiles ---\n")
print(quantile(out$enumeration_year, c(0, .25, .5, .75, 1), na.rm = TRUE))
cat("\n--- Primary taxonomy ---\n")
print(sort(table(out$primary_taxonomy), decreasing = TRUE))
cat(sprintf("\nNPIs with deactivation record: %d\n", sum(out$is_deactivated)))
