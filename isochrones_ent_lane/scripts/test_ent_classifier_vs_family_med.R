#!/usr/bin/env Rscript
# =============================================================================
# Harder specificity test: ENT classifier vs Family Practice
# =============================================================================
# Family Practice is a stricter test than Otolaryngology because FPs DO bill some
# overlapping codes — particularly cerumen removal (69210) and basic
# audiometry — that ENTs also bill. If the YAML's specificity holds against
# FPs, the classifier is robust.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(here)
})

source(here::here("R", "ent_procedure_categories.R"))
cpt_map      <- load_ent_procedure_categories()
ent_cpts     <- unique(cpt_map$cpt_code)
recent_years <- 2021:2023

cohort_csv <- here::here("publication_materials/figures/ent_data_integrity",
                         "mystery_caller_cohort_triple_match_4578.csv")
ent_full <- read_csv(cohort_csv, show_col_types = FALSE)
ent_sample <- as.character(ent_full$npi)

if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13)
con <- dbConnect(duckdb(),
                 load_paths(verbose = FALSE)$nber_duckdb_main,
                 read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

# Family Practice comparison sample
fp_sample <- DBI::dbGetQuery(con, "
  SELECT CAST(Rndrng_NPI AS VARCHAR) AS npi
  FROM medicare_part_b_2023
  WHERE Rndrng_Prvdr_Type = 'Family Practice'
  ORDER BY Rndrng_NPI
") %>% pull(npi)

score_cohort <- function(npis, label) {
  DBI::dbWriteTable(con, "tmp_npis",
                    data.frame(npi = as.character(npis)),
                    temporary = TRUE, overwrite = TRUE)
  DBI::dbWriteTable(con, "tmp_ent",
                    data.frame(cpt_code = ent_cpts),
                    temporary = TRUE, overwrite = TRUE)

  total_by_npi <- DBI::dbGetQuery(con, "
    SELECT CAST(b.Rndrng_NPI AS VARCHAR) AS npi,
           CAST(SUM(b.Tot_Srvcs) AS DOUBLE) AS total_services_all_years
    FROM medicare_part_b_by_service_all_years b
    JOIN tmp_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
    GROUP BY 1
  ")

  ent_by_npi <- DBI::dbGetQuery(con, paste0("
    SELECT CAST(b.Rndrng_NPI AS VARCHAR) AS npi,
           CAST(SUM(b.Tot_Srvcs) AS DOUBLE) AS ent_services_all_years,
           SUM(CASE WHEN b.data_year IN (", paste(recent_years, collapse=","), ")
                    THEN b.Tot_Srvcs ELSE 0 END) AS ent_services_recent,
           COUNT(DISTINCT b.HCPCS_Cd) AS ent_distinct_codes,
           STRING_AGG(DISTINCT b.HCPCS_Cd, ',' ORDER BY b.HCPCS_Cd) AS ent_codes_billed
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
        is.na(pct_ent) ~ "NO_MEDICARE_ACTIVITY",
        pct_ent >= 50  ~ "HIGHLY_LIKELY",
        pct_ent >= 10  ~ "LIKELY",
        pct_ent >= 1   ~ "INCIDENTAL",
        pct_ent >  0   ~ "NEGLIGIBLE",
        TRUE           ~ "NONE"
      )
    )
  scored$cohort <- label
  scored
}

cat(sprintf("Scoring %d ENT (Triple Match) NPIs...\n", length(ent_sample)))
ent_scored <- score_cohort(ent_sample, "ENT_TripleMatch")
cat(sprintf("Scoring %d Family Practice NPIs...\n", length(fp_sample)))
fp_scored  <- score_cohort(fp_sample,  "FamilyPractice")

score_levels <- c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE",
                  "NONE","NO_MEDICARE_ACTIVITY")
summary_tbl <- bind_rows(ent_scored, fp_scored) %>%
  mutate(ent_score = factor(ent_score, levels = score_levels)) %>%
  count(cohort, ent_score, .drop = FALSE) %>%
  tidyr::pivot_wider(names_from = cohort, values_from = n, values_fill = 0)
cat(sprintf("\n=== ENT SCORE DISTRIBUTION (n_ent=%d, n_fp=%d) ===\n",
            length(ent_sample), length(fp_sample)))
print(summary_tbl)

# Headline
sens_loose <- mean(ent_scored$ent_score %in%
                   c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE"))
sens_strict <- mean(ent_scored$ent_score %in% c("HIGHLY_LIKELY","LIKELY"))
fpr_loose  <- mean(fp_scored$ent_score %in%
                   c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE"))
fpr_strict <- mean(fp_scored$ent_score %in% c("HIGHLY_LIKELY","LIKELY"))
cat("\n=== HEADLINE METRICS ===\n")
cat(sprintf("  Sensitivity (any ENT code)     : %5.1f%%   |  FPR vs Family Practice: %5.1f%%\n",
            100*sens_loose, 100*fpr_loose))
cat(sprintf("  Sensitivity (>=10%% pct_ent)    : %5.1f%%   |  FPR vs Family Practice: %5.1f%%\n",
            100*sens_strict, 100*fpr_strict))
cat(sprintf("  Specificity (>=10%% threshold)  : %5.1f%%\n",
            100*(1-fpr_strict)))

# Which codes are FPs billing? (diagnostic for false positives)
cat("\n=== WHICH ENT CODES DO FAMILY PRACTICE PHYSICIANS BILL? ===\n")
print(fp_scored %>%
        filter(ent_services_all_years > 0) %>%
        select(npi, pct_ent, ent_distinct_codes, ent_codes_billed) %>%
        arrange(desc(pct_ent)))

cat("\n=== TOP CODES BILLED BY THE FP COMPARISON COHORT ===\n")
fp_codes <- DBI::dbGetQuery(con, "
  SELECT b.HCPCS_Cd, ANY_VALUE(b.HCPCS_Desc) AS desc,
         COUNT(DISTINCT b.Rndrng_NPI) AS n_fp_npis
  FROM medicare_part_b_by_service_all_years b
  JOIN tmp_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
  JOIN tmp_ent  e ON b.HCPCS_Cd = e.cpt_code
  GROUP BY 1
  ORDER BY n_fp_npis DESC
")
if (nrow(fp_codes) == 0) cat("  (no FPs bill any of the 71 codes)\n") else print(fp_codes)
