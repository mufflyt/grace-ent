#!/usr/bin/env Rscript
# =============================================================================
# Discover what Medicare Part B codes the Triple Match cohort ACTUALLY bills
# =============================================================================
# Empirical approach: take all 4,578 Triple Match NPIs, pull their full
# Medicare Part B billing history, and rank HCPCS codes by:
#   - n_unique_npis (how many distinct ENT physicians bill this code)
#   - tot_srvcs    (total services billed)
#
# Output 1: every code billed by >= 1% of the cohort (>= 46 ENTs)
# Output 2: top codes NOT currently in config/ent_procedure_categories.yml
#           — these are candidate additions for better ENT characterization
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(here)
})

source(here::here("R", "duckdb_safe.R"))
source(here::here("R", "ent_procedure_categories.R"))
current_map <- load_ent_procedure_categories()
current_cpts <- unique(current_map$cpt_code)

cohort_csv <- here::here("publication_materials/figures/ent_data_integrity",
                         "mystery_caller_cohort_triple_match_4578.csv")
cohort <- read_csv(cohort_csv, show_col_types = FALSE)
all_npis <- as.character(cohort$npi)

db <- "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb"
con <- safe_duckdb(db, read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

DBI::dbWriteTable(con, "tm_npis", data.frame(npi = all_npis),
                  temporary = TRUE, overwrite = TRUE)

cat("Pulling all Medicare Part B claims for the 4,578 Triple Match NPIs...\n")
all_codes <- DBI::dbGetQuery(con, "
SELECT
  b.HCPCS_Cd                          AS cpt_code,
  ANY_VALUE(b.HCPCS_Desc)             AS hcpcs_desc,
  COUNT(DISTINCT b.Rndrng_NPI)        AS n_npis,
  CAST(SUM(b.Tot_Srvcs) AS BIGINT)    AS tot_srvcs,
  CAST(SUM(b.Tot_Benes) AS BIGINT)    AS tot_benes
FROM medicare_part_b_by_service_all_years b
JOIN tm_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
GROUP BY b.HCPCS_Cd
ORDER BY n_npis DESC
")
cat(sprintf("  %d distinct HCPCS codes billed by the cohort\n", nrow(all_codes)))

# Top codes billed by >= 1% of the cohort
threshold <- ceiling(0.01 * nrow(cohort))
common <- all_codes %>% filter(n_npis >= threshold)
cat(sprintf("\n=== %d codes billed by >= %d NPIs (>= 1%% of cohort) ===\n",
            nrow(common), threshold))

cat("\n--- TOP OVERALL (by n_npis) ---\n")
print(common %>%
        mutate(in_yaml = ifelse(cpt_code %in% current_cpts, "[YAML]", "")) %>%
        select(in_yaml, cpt_code, n_npis, tot_srvcs, hcpcs_desc))

# Candidate additions: common codes NOT in YAML
candidates <- common %>% filter(!cpt_code %in% current_cpts)
cat(sprintf("\n=== %d CANDIDATE ADDITIONS (NOT in current YAML) ===\n",
            nrow(candidates)))
print(candidates %>% select(cpt_code, n_npis, tot_srvcs, hcpcs_desc))

# How many additional Triple Match physicians would each candidate "rescue"?
miss_npis <- DBI::dbGetQuery(con, sprintf("
SELECT DISTINCT CAST(b.Rndrng_NPI AS VARCHAR) AS npi
FROM medicare_part_b_by_service_all_years b
JOIN tm_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
WHERE b.HCPCS_Cd IN ('%s')
", paste(current_cpts, collapse = "','")))
cat(sprintf("\n=== COHORT COVERAGE GROWTH FROM CANDIDATES ===\n"))
cat(sprintf("  Current YAML covers           : %d of %d Triple Match NPIs (%.0f%%)\n",
            nrow(miss_npis), nrow(cohort),
            100 * nrow(miss_npis) / max(nrow(cohort), 1L)))

# What happens if we add top-N candidate codes?
out_csv <- here::here("publication_materials/figures/ent_data_integrity",
                      "candidate_ent_codes_for_yaml.csv")
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
tmp_out_csv <- paste0(out_csv, ".tmp")
write_csv(common %>%
            mutate(in_yaml = cpt_code %in% current_cpts,
                   pct_npis = round(100 * n_npis / max(nrow(cohort), 1L), 1)),
          tmp_out_csv)
file.rename(tmp_out_csv, out_csv)
cat(sprintf("\nFull >=1%% codes list saved to:\n  %s\n", out_csv))
