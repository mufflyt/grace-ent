#!/usr/bin/env Rscript
# =============================================================================
# Full ENT classifier evaluation (sensitivity + specificity + continuous score
# + temporal recency)
# =============================================================================
# Three improvements over the binary smoke test:
#
#   (1) Comparison cohort  -> measure FALSE-POSITIVE rate against 100 random
#       Otolaryngologys (Medicare-active, clinically distinct from ENT).
#   (2) Continuous score   -> pct_ent = ENT services / all services per NPI.
#       Bins: HIGHLY_LIKELY (>=50%), LIKELY (10-49%), INCIDENTAL (1-9%),
#       NEGLIGIBLE (<1%), NONE (no ENT codes billed).
#   (3) Temporal recency   -> flag whether the physician billed any ENT
#       code in the last 3 years (2021-2023) vs. only earlier.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(here)
})

source(here::here("R", "ent_procedure_categories.R"))
cpt_map      <- load_ent_procedure_categories()
ent_cpts     <- unique(cpt_map$cpt_code)
recent_years <- 2021:2023

# ── Two cohorts ──────────────────────────────────────────────────────────────
cohort_csv <- here::here("publication_materials/figures/ent_data_integrity",
                         "mystery_caller_cohort_triple_match_4578.csv")
ent_full <- read_csv(cohort_csv, show_col_types = FALSE)
ent_sample <- as.character(ent_full$npi)

con <- dbConnect(duckdb(), "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb",
                 read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

# Comparison cohort: all Obstetrics & Gynecology NPIs with 2023 activity
otolaryngology_sample <- DBI::dbGetQuery(con, "
  SELECT CAST(Rndrng_NPI AS VARCHAR) AS npi
  FROM medicare_part_b_2023
  WHERE Rndrng_Prvdr_Type = 'Obstetrics & Gynecology'
  ORDER BY Rndrng_NPI
") %>% pull(npi)

# ── Per-cohort evaluation ────────────────────────────────────────────────────
score_cohort <- function(npis, label) {
  DBI::dbWriteTable(con, "tmp_npis",
                    data.frame(npi = as.character(npis)),
                    temporary = TRUE, overwrite = TRUE)
  DBI::dbWriteTable(con, "tmp_ent",
                    data.frame(cpt_code = ent_cpts),
                    temporary = TRUE, overwrite = TRUE)

  # All Medicare services by NPI (denominator for pct_ent)
  total_by_npi <- DBI::dbGetQuery(con, "
    SELECT CAST(b.Rndrng_NPI AS VARCHAR) AS npi,
           CAST(SUM(b.Tot_Srvcs) AS DOUBLE) AS total_services_all_years
    FROM medicare_part_b_by_service_all_years b
    JOIN tmp_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
    GROUP BY 1
  ")

  # ENT-only services + per-year recency
  ent_by_npi <- DBI::dbGetQuery(con, paste0("
    SELECT CAST(b.Rndrng_NPI AS VARCHAR) AS npi,
           CAST(SUM(b.Tot_Srvcs) AS DOUBLE) AS ent_services_all_years,
           SUM(CASE WHEN b.data_year IN (", paste(recent_years, collapse=","), ")
                    THEN b.Tot_Srvcs ELSE 0 END) AS ent_services_recent,
           COUNT(DISTINCT b.HCPCS_Cd) AS ent_distinct_codes
    FROM medicare_part_b_by_service_all_years b
    JOIN tmp_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
    JOIN tmp_ent  e ON b.HCPCS_Cd = e.cpt_code
    GROUP BY 1
  "))

  scored <- tibble::tibble(npi = as.character(npis)) %>%
    left_join(total_by_npi, by = "npi") %>%
    left_join(ent_by_npi,   by = "npi") %>%
    mutate(
      total_services_all_years = dplyr::coalesce(total_services_all_years, 0),
      ent_services_all_years   = dplyr::coalesce(ent_services_all_years,   0),
      ent_services_recent      = dplyr::coalesce(ent_services_recent,      0),
      ent_distinct_codes       = dplyr::coalesce(ent_distinct_codes,       0L),
      pct_ent = dplyr::if_else(
        total_services_all_years > 0,
        100 * ent_services_all_years / total_services_all_years,
        NA_real_
      ),
      ent_score = case_when(
        is.na(pct_ent)        ~ "NO_MEDICARE_ACTIVITY",
        pct_ent >= 50         ~ "HIGHLY_LIKELY",
        pct_ent >= 10         ~ "LIKELY",
        pct_ent >= 1          ~ "INCIDENTAL",
        pct_ent >  0          ~ "NEGLIGIBLE",
        TRUE                  ~ "NONE"
      ),
      recently_active = ent_services_recent > 0
    )
  scored$cohort <- label
  scored
}

cat(sprintf("Scoring %d ENT (Triple Match) NPIs...\n", length(ent_sample)))
ent_scored <- score_cohort(ent_sample, "ENT_TripleMatch")
cat(sprintf("Scoring %d Otolaryngology comparison NPIs...\n", length(otolaryngology_sample)))
otolaryngology_scored <- score_cohort(otolaryngology_sample, "Otolaryngology_comparison")

# ── Comparison table ─────────────────────────────────────────────────────────
score_levels <- c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE",
                  "NONE","NO_MEDICARE_ACTIVITY")

summary_tbl <- bind_rows(ent_scored, otolaryngology_scored) %>%
  mutate(ent_score = factor(ent_score, levels = score_levels)) %>%
  count(cohort, ent_score, .drop = FALSE) %>%
  tidyr::pivot_wider(names_from = cohort, values_from = n, values_fill = 0)

cat(sprintf("\n=== ENT SPECIFICITY SCORE DISTRIBUTION (n_ent=%d, n_comparison=%d) ===\n",
            length(ent_sample), length(otolaryngology_sample)))
print(summary_tbl)

# Headline metrics
sens_loose  <- mean(ent_scored$ent_score %in%
                    c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE"))
sens_strict <- mean(ent_scored$ent_score %in% c("HIGHLY_LIKELY","LIKELY"))
fpr_loose   <- mean(otolaryngology_scored$ent_score %in%
                    c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE"))
fpr_strict  <- mean(otolaryngology_scored$ent_score %in% c("HIGHLY_LIKELY","LIKELY"))

cat("\n=== HEADLINE METRICS ===\n")
cat(sprintf("  Sensitivity (any ENT code)       : %5.1f%%   |  FPR vs Otolaryngology: %5.1f%%\n",
            100*sens_loose, 100*fpr_loose))
cat(sprintf("  Sensitivity (>=10%% pct_ent)      : %5.1f%%   |  FPR vs Otolaryngology: %5.1f%%\n",
            100*sens_strict, 100*fpr_strict))
cat(sprintf("  Specificity (>=10%% threshold)    : %5.1f%%\n",
            100*(1-fpr_strict)))

# ── Temporal recency among ENT cohort ────────────────────────────────────────
cat("\n=== TEMPORAL RECENCY (ENT cohort) ===\n")
print(ent_scored %>%
        filter(ent_services_all_years > 0) %>%
        count(recently_active, name = "n_NPIs") %>%
        mutate(meaning = ifelse(recently_active,
                                "billed >=1 ENT code in 2021-2023",
                                "ENT billing ONLY before 2021 (likely retired/inactive)")))

# Stratify recency × score
cat("\nRecency x score (ENT cohort):\n")
print(ent_scored %>%
        filter(ent_services_all_years > 0) %>%
        mutate(ent_score = factor(ent_score, levels = score_levels)) %>%
        count(ent_score, recently_active, .drop = FALSE))

# ── Save per-NPI report ──────────────────────────────────────────────────────
out_csv <- here::here("publication_materials/figures/ent_data_integrity",
                      "ent_classifier_evaluation_full.csv")
tmp_out_csv <- paste0(out_csv, ".tmp")
write_csv(bind_rows(ent_scored, otolaryngology_scored), tmp_out_csv)
file.rename(tmp_out_csv, out_csv)
cat(sprintf("\nPer-NPI report saved: %s\n", out_csv))
