#!/usr/bin/env Rscript
# =============================================================================
# Mystery Caller Cohort Export — Triple Match (n = 4,578) with RUCA
# =============================================================================
# Outputs a single CSV for Project 1 (the mystery caller study) containing
# every physician in the Triple Match cohort (NPPES CONUS ∩ active ABOto
# board-certified ∩ ENThealth public directory), with the contact and
# rural/urban classification columns needed for sample selection and
# power analysis.
#
# Columns produced:
#   npi, last_name, first_name, credentials,
#   specialty_primary, ent_subspecialty (NPPES self-designation),
#   aboto_subspecialty (priority-resolved from ABOto certs),
#   address, suite, city, state, zip5,
#   latitude, longitude,
#   phone, phone_source,
#   ruca_primary, ruca_secondary,
#   rurality        (RUCA 1-3 = "urban", 4-10 = "rural" — workforce-research definition)
#
# Power-analysis summary printed to stdout: rural counts under both
# RUCA cutoffs, broken down by state and subspecialty.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(arrow)
  library(dplyr)
  library(here)
})

source(here::here("R", "ent_registry_counts.R"))
source(here::here("R", "phone_validation.R"))

# ── Compute the Triple Match NPIs (live, from the shared function) ────────────
ct <- compute_ent_registry_counts()

# Triple Match = NPIs present in all three CONUS-filtered sets
state_lookup <- c(setNames(state.abb, state.name),
                  "District of Columbia" = "DC",
                  "Puerto Rico"          = "PR")
abohns_active <- read_csv(here("data/ent/abohns_with_npi_master.csv"),
                          show_col_types = FALSE) %>%
  mutate(state_code = state_lookup[state_full]) %>%
  filter(!state_code %in% c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP"),
         practicing_status == "Practicing",
         !is.na(npi)) %>%
  mutate(npi_chr = as.character(npi))

nppes <- read_parquet(here("artifacts/ent/ent_from_nppes_flat_file.parquet")) %>%
  filter(!state %in% c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP"),
         !is.na(npi)) %>%
  mutate(npi_chr = as.character(npi))

ent <- read_csv(here("data/ent/ent_master_roster_final_20260513.csv"),
                show_col_types = FALSE) %>%
  filter(!state %in% c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP") | is.na(state)) %>%
  mutate(npi_chr = as.character(npi))

triple_npis <- intersect(
  intersect(unique(abohns_active$npi_chr), unique(nppes$npi_chr)),
  unique(ent$npi_chr[!is.na(ent$npi_chr)])
)
stopifnot("Triple match n must equal ct$sets$n_triple" =
            length(triple_npis) == ct$sets$n_triple)

cat(sprintf("Triple Match cohort: n = %d\n\n", length(triple_npis)))

# ── ABOto subspecialty (priority-resolved at NPI level, matches Figure 4) ─────
abohns_raw <- read_csv(here("data/ent/abohns_certifications_clean.csv"),
                       show_col_types = FALSE)
abohns_subspec_per_id <- abohns_raw %>%
  group_by(abohns_id) %>%
  summarise(
    has_pediatric   = any(certification == "Complex Pediatric Otolaryngology"),
    has_neurotology = any(certification == "Neurotology"),
    has_sleep       = any(certification == "Sleep Medicine"),
    .groups         = "drop"
  )
aboto_subspec_by_npi <- abohns_active %>%
  left_join(abohns_subspec_per_id, by = "abohns_id") %>%
  group_by(npi_chr) %>%
  summarise(
    has_pediatric   = any(has_pediatric,   na.rm = TRUE),
    has_neurotology = any(has_neurotology, na.rm = TRUE),
    has_sleep       = any(has_sleep,       na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(aboto_subspecialty = case_when(
    has_pediatric   ~ ABOTO_SUBSPECIALTY_PRIORITY[1],
    has_neurotology ~ ABOTO_SUBSPECIALTY_PRIORITY[2],
    has_sleep       ~ ABOTO_SUBSPECIALTY_PRIORITY[3],
    TRUE            ~ ABOTO_SUBSPECIALTY_PRIORITY[4]
  )) %>%
  select(npi_chr, aboto_subspecialty)

# ── ENThealth contact + address columns (the registry with the phone numbers)─
ent_contact <- ent %>%
  filter(npi_chr %in% triple_npis) %>%
  distinct(npi_chr, .keep_all = TRUE) %>%
  select(npi_chr,
         last_name, first_name, credentials,
         specialty_primary,
         ent_subspecialty_nppes = ent_subspecialty,
         address, suite, city, state, zip5,
         latitude, longitude,
         geocoder_provenance,
         phone = phone_norm,
         phone_source)

# ── Data-quality flags from shared function (state-mismatch + entity-type) ───
qa_state    <- ct$qa$state_mismatch
qa_entity   <- ct$qa$nppes_entity %>% select(npi_chr, nppes_entity_type)
qa_tri      <- ct$qa$subspecialty_triangulation %>%
  select(npi_chr,
         nppes_subspec_raw, nppes_subspec_aboto,
         ent_subspec_raw,   ent_subspec_aboto,
         subspecialty_agreement)

# ── Activity recency (proxy for address staleness) ───────────────────────────
# We don't have NPPES `last_updated_date` in the ENT parquet, so we use
# Medicare Part B last-billed year as a proxy for "is this physician still
# practicing under this NPI?".  See DATA_INTEGRITY_NOTES.md for the
# limitation discussion (this measures activity recency, not address
# staleness per se).
suppressPackageStartupMessages({library(DBI); library(duckdb)})
if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13)
.db_path <- load_paths(verbose = FALSE)$nber_duckdb_main
if (file.exists(.db_path)) {
  .con <- dbConnect(duckdb(), .db_path, read_only = TRUE)
  on.exit({try(dbDisconnect(.con, shutdown = TRUE), silent = TRUE)}, add = TRUE)
  DBI::dbWriteTable(.con, "tmp_cohort_npis",
                    data.frame(npi = triple_npis),
                    temporary = TRUE, overwrite = TRUE)
  activity <- DBI::dbGetQuery(.con, "
    SELECT CAST(b.Rndrng_NPI AS VARCHAR) AS npi_chr,
           MAX(CAST(b.data_year AS INTEGER)) AS last_active_year_medicare,
           MIN(CAST(b.data_year AS INTEGER)) AS first_active_year_medicare,
           SUM(b.Tot_Srvcs)                  AS total_services_all_years
    FROM medicare_part_b_by_service_all_years b
    JOIN tmp_cohort_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
    GROUP BY 1
  ") %>% as_tibble()
  cat(sprintf("Medicare activity pulled for %d of %d NPIs\n\n",
              nrow(activity), length(triple_npis)))
} else {
  warning("DuckDB not mounted; skipping activity recency join")
  activity <- tibble::tibble(npi_chr = character(0),
                             last_active_year_medicare = integer(0),
                             first_active_year_medicare = integer(0),
                             total_services_all_years = double(0))
}

# Bin recency relative to the snapshot cutoff (last full year of Medicare data)
SNAPSHOT_LAST_YEAR <- 2023L
activity <- activity %>%
  mutate(years_since_last_active = SNAPSHOT_LAST_YEAR - last_active_year_medicare)

# ── RUCA join (by ZIP5) ───────────────────────────────────────────────────────
ruca <- read_csv(here("data/external/ruca_2020_zip.csv"), show_col_types = FALSE) %>%
  mutate(.z_int = suppressWarnings(as.integer(ZIPCode)),
         zip5   = ifelse(is.na(.z_int), NA_character_, sprintf("%05d", .z_int))) %>%
  select(-.z_int) %>%
  filter(!is.na(zip5)) %>%
  select(zip5, ruca_primary = PrimaryRUCA, ruca_secondary = SecondaryRUCA) %>%
  distinct(zip5, .keep_all = TRUE)

cohort <- ent_contact %>%
  left_join(aboto_subspec_by_npi, by = "npi_chr") %>%
  left_join(qa_state %>% select(npi_chr, aboto_state, nppes_state, state_mismatch_flag),
            by = "npi_chr") %>%
  left_join(qa_entity, by = "npi_chr") %>%
  left_join(qa_tri,    by = "npi_chr") %>%
  left_join(activity %>% select(npi_chr, last_active_year_medicare,
                                 first_active_year_medicare,
                                 years_since_last_active),
            by = "npi_chr") %>%
  mutate(
    # Apply activity_recency bin AFTER the join so the 567 NPIs without any
    # Medicare row (NA after left_join) get classified as NO_MEDICARE_ACTIVITY
    # rather than NA. SNAPSHOT_LAST_YEAR = 2023.
    activity_recency = dplyr::case_when(
      is.na(last_active_year_medicare) ~ "NO_MEDICARE_ACTIVITY",
      years_since_last_active <= 1L    ~ "CURRENT",      # 2022-2023
      years_since_last_active <= 3L    ~ "RECENT",       # 2020-2021
      years_since_last_active <= 6L    ~ "STALE",        # 2017-2019
      TRUE                             ~ "VERY_STALE"    # pre-2017
    )
  ) %>%
  mutate(.z_int = suppressWarnings(as.integer(zip5)),
         zip5   = ifelse(is.na(.z_int), NA_character_, sprintf("%05d", .z_int))) %>%
  select(-.z_int) %>%
  left_join(ruca, by = "zip5") %>%
  bind_cols(validate_phone(phone_str = .$phone, practice_state = .$state)) %>%
  rename(npi = npi_chr) %>%
  mutate(
    # Workforce-research convention (locked in 2026-05-15):
    # RUCA 1-3 = "urban" (metropolitan), RUCA 4-10 = "rural"
    # (micropolitan + small town + rural). This is the canonical
    # rurality classification for this project.
    rurality = case_when(
      is.na(ruca_primary)     ~ NA_character_,
      ruca_primary %in% 1:3   ~ "urban",
      TRUE                    ~ "rural"
    )
  ) %>%
  arrange(state, city, last_name, first_name)

stopifnot("Cohort row count must equal Triple Match" =
            nrow(cohort) == length(triple_npis))

# ── Write CSV ─────────────────────────────────────────────────────────────────
out_dir <- here("publication_materials", "figures", "ent_data_integrity")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_csv <- file.path(out_dir, "mystery_caller_cohort_triple_match_4578.csv")
write_csv(cohort, out_csv)
cat(sprintf("Wrote: %s\n  (%d rows, %d columns)\n\n",
            out_csv, nrow(cohort), ncol(cohort)))

# ── Data-quality summary ──────────────────────────────────────────────────────
cat("=== DATA-QUALITY FLAGS ===\n\n")

cat("Geocoder provenance (ENThealth address → lat/lon):\n")
print(cohort %>% count(geocoder_provenance, sort = TRUE))

cat("\nNPPES entity type (1 = individual; 2 = organization):\n")
print(cohort %>% count(nppes_entity_type, sort = TRUE))

cat("\nSubspecialty triangulation (ABOto vs NPPES vs ENThealth):\n")
print(cohort %>% count(subspecialty_agreement, sort = TRUE) %>%
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))))

cat("\nPhone-number validity (NANP rules + area-code state match):\n")
print(cohort %>% count(phone_validity_flag, sort = TRUE) %>%
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))))

cat("\nActivity recency (Medicare Part B last-billed year, as address-staleness proxy):\n")
print(cohort %>%
  count(activity_recency = factor(activity_recency,
                                  levels = c("CURRENT","RECENT","STALE","VERY_STALE","NO_MEDICARE_ACTIVITY")),
        .drop = FALSE) %>%
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))))
cat("(CURRENT: last active 2022-2023; RECENT: 2020-2021; STALE: 2017-2019; VERY_STALE: pre-2017)\n")

cat("\nState mismatch (ABOto state ≠ NPPES state):\n")
sm <- cohort %>% count(state_mismatch_flag, sort = TRUE)
print(sm)
n_mismatch <- sum(cohort$state_mismatch_flag, na.rm = TRUE)
cat(sprintf("  %d of %d (%.1f%%) physicians have mismatched ABOto and NPPES states.\n",
            n_mismatch, nrow(cohort), 100 * n_mismatch / nrow(cohort)))
cat("  Top mismatch pairs:\n")
print(cohort %>% filter(state_mismatch_flag) %>%
        count(aboto_state, nppes_state, sort = TRUE) %>% head(10))

cat("\n=== RURALITY BREAKDOWN (RUCA 4-10 = rural) ===\n\n")
print(cohort %>% count(rurality))

cat("\n=== POWER CHECK (target: n >= 415 rural) ===\n")
n_rural <- sum(cohort$rurality == "rural", na.rm = TRUE)
cat(sprintf("  n = %4d rural  %s\n",
            n_rural,
            if (n_rural >= 415) "[OK — exceeds 415]" else sprintf("[SHORT by %d]", 415 - n_rural)))

cat("\n=== TOP STATES BY RURAL COUNT ===\n")
print(cohort %>% filter(rurality == "rural") %>%
        count(state, sort = TRUE) %>% head(10))

cat("\n=== RURAL BY ABOto SUBSPECIALTY ===\n")
print(cohort %>%
        group_by(aboto_subspecialty) %>%
        summarise(
          n_total        = dplyr::n(),
          n_rural        = sum(rurality == "rural", na.rm = TRUE),
          n_urban        = sum(rurality == "urban", na.rm = TRUE),
          n_missing_ruca = sum(is.na(rurality))
        ))
