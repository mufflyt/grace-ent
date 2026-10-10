#!/usr/bin/env Rscript
# =============================================================================
# Smoke test: ENT procedure-category YAML against 100 Triple Match NPIs
# =============================================================================
# Pull a reproducible random sample of 100 NPIs from the Triple Match cohort
# (4,578), join them against `medicare_part_b_by_service_all_years` filtered
# to the 46 CPTs in config/ent_procedure_categories.yml, and report:
#   - how many of the 100 have >= 1 billing record for any ENT CPT (hit rate)
#   - per-physician total claim count and categories billed
#   - per-category hit count across the sample
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

source(here::here("R", "ent_procedure_categories.R"))

# ── 1. Load CPT list from YAML ───────────────────────────────────────────────
cpt_map <- load_ent_procedure_categories()
cat(sprintf("Loaded YAML: %d CPTs across %d categories\n",
            nrow(cpt_map),
            dplyr::n_distinct(cpt_map$procedure_category)))

# ── 2. Sample 100 NPIs from the Triple Match cohort ──────────────────────────
cohort_csv <- here::here("publication_materials/figures/ent_data_integrity",
                         "mystery_caller_cohort_triple_match_4578.csv")
cohort <- read_csv(cohort_csv, show_col_types = FALSE)
stopifnot(nrow(cohort) == 4578L)

sample_npis <- as.character(cohort$npi)
cat(sprintf("Using all %d NPIs from Triple Match cohort\n\n",
            length(sample_npis)))

# ── 3. Query Medicare Part B ─────────────────────────────────────────────────
if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13)
db_path <- load_paths(verbose = FALSE)$nber_duckdb_main
stopifnot("DuckDB file not found — check external drive" = file.exists(db_path))
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

# Inject sample as a temp table for an efficient join
DBI::dbWriteTable(con, "sample_npis",
                  data.frame(npi = as.character(sample_npis)),
                  temporary = TRUE, overwrite = TRUE)
DBI::dbWriteTable(con, "ent_cpts",
                  data.frame(cpt_code = cpt_map$cpt_code,
                             procedure_category = cpt_map$procedure_category,
                             representative_code = cpt_map$inclusive_representative_code),
                  temporary = TRUE, overwrite = TRUE)

# Pull all matched claims (CONUS Triple Match × ENT CPTs, all years)
sql <- "
SELECT
  CAST(b.Rndrng_NPI AS VARCHAR) AS npi,
  b.HCPCS_Cd                    AS cpt_code,
  b.HCPCS_Desc                  AS hcpcs_desc,
  c.procedure_category,
  c.representative_code,
  CAST(b.Tot_Srvcs AS DOUBLE)   AS tot_srvcs,
  CAST(b.Tot_Benes AS DOUBLE)   AS tot_benes,
  CAST(b.Avg_Mdcr_Pymt_Amt AS DOUBLE) AS avg_pymt
FROM medicare_part_b_by_service_all_years b
JOIN sample_npis s ON CAST(b.Rndrng_NPI AS VARCHAR) = s.npi
JOIN ent_cpts c    ON b.HCPCS_Cd = c.cpt_code
"
cat("Running Medicare Part B query...\n")
hits <- DBI::dbGetQuery(con, sql)
cat(sprintf("  Matched %d Medicare claim rows\n\n", nrow(hits)))

# ── 4. Per-NPI rollup ─────────────────────────────────────────────────────────
per_npi <- hits %>%
  group_by(npi) %>%
  summarise(
    n_claim_rows = dplyr::n(),
    n_categories = dplyr::n_distinct(procedure_category),
    categories   = paste(sort(unique(procedure_category)), collapse = "; "),
    tot_srvcs    = sum(tot_srvcs, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  arrange(desc(tot_srvcs))

hit_npis <- unique(per_npi$npi)
miss_npis <- setdiff(sample_npis, hit_npis)

cat("=== HIT RATE ===\n")
cat(sprintf("  Sampled NPIs               : %d\n", length(sample_npis)))
cat(sprintf("  NPIs with >= 1 ENT claim   : %d  (%.0f%%)\n",
            length(hit_npis), 100 * length(hit_npis) / length(sample_npis)))
cat(sprintf("  NPIs with NO ENT claim     : %d  (%.0f%%)\n",
            length(miss_npis), 100 * length(miss_npis) / length(sample_npis)))

# ── 5. Per-category hit distribution ─────────────────────────────────────────
cat("\n=== PER-CATEGORY HIT COUNT (out of 100 NPIs) ===\n")
per_cat <- hits %>%
  distinct(npi, procedure_category) %>%
  count(procedure_category, name = "n_npis_with_any_claim") %>%
  arrange(desc(n_npis_with_any_claim))
print(per_cat)

# ── 6. Distribution of unique categories per NPI ─────────────────────────────
cat("\n=== DISTRIBUTION OF DISTINCT CATEGORIES PER NPI ===\n")
print(per_npi %>% count(n_categories))

# ── 7. Top 10 NPIs by ENT service volume ─────────────────────────────────────
cat("\n=== TOP 10 NPIs BY TOTAL ENT SERVICE COUNT ===\n")
top10 <- per_npi %>% head(10)
print(top10)

# ── 8. Save the per-NPI hit report ───────────────────────────────────────────
out_csv <- here::here(
  "publication_materials/figures/ent_data_integrity",
  "test_ent_procedure_match_100_npis.csv"
)
report <- tibble::tibble(npi = sample_npis) %>%
  left_join(per_npi, by = "npi") %>%
  mutate(hit = !is.na(n_claim_rows))
out_csv_tmp <- paste0(out_csv, ".tmp")
write_csv(report, out_csv_tmp)
file.rename(out_csv_tmp, out_csv)
cat(sprintf("\nPer-NPI hit report saved to:\n  %s\n", out_csv))
