#!/usr/bin/env Rscript
# =============================================================================
# ENT classifier validation at scale: n = 1,000 Triple Match physicians
# =============================================================================
# Final sensitivity validation. Same classifier as scripts/test_ent_classifier_full.R
# (71 CPT codes, ≥10% pct_ent threshold, 2021-2023 recency) but evaluated on
# 1,000 of the 4,578 Triple Match NPIs for tight confidence intervals.
#
# Reports:
#   - Sensitivity + 95% CI at both thresholds
#   - Score distribution (HIGHLY_LIKELY / LIKELY / INCIDENTAL / ...)
#   - pct_ent distribution (median, IQR, histogram bins)
#   - Sensitivity stratified by ABOto subspecialty
#   - Recency breakdown
#   - Top NPIs by ENT service volume
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(readr); library(here)
})

source(here::here("R", "ent_procedure_categories.R"))
source(here::here("R", "ent_registry_counts.R"))

cpt_map      <- load_ent_procedure_categories()
ent_cpts     <- unique(cpt_map$cpt_code)
recent_years <- 2021:2023

cohort_csv <- here::here("publication_materials/figures/ent_data_integrity",
                         "mystery_caller_cohort_triple_match_4578.csv")
ent_full <- read_csv(cohort_csv, show_col_types = FALSE)
stopifnot(nrow(ent_full) == 4578L)

set.seed(20260515)
sample_n <- 1000
sample_npis <- as.character(sample(ent_full$npi, sample_n, replace = FALSE))
cat(sprintf("Sampling %d of %d Triple Match NPIs (seed = 20260515)\n",
            sample_n, nrow(ent_full)))

# ── Pull ENT subspecialty for stratified analysis ─────────────────────────────
ct <- compute_ent_registry_counts()
subspec_lookup <- ct$by_subspecialty  # not at NPI level — recompute at NPI
# Get per-NPI subspecialty by replaying compute_ent_registry_counts' logic
state_lookup <- c(setNames(state.abb, state.name),
                  "District of Columbia" = "DC", "Puerto Rico" = "PR")
abohns_raw <- read_csv(here("data/ent/abohns_certifications_clean.csv"),
                       show_col_types = FALSE)
abohns_matched <- read_csv(here("data/ent/abohns_with_npi_master.csv"),
                           show_col_types = FALSE) %>%
  mutate(state_code = state_lookup[state_full])
abohns_active <- abohns_matched %>%
  filter(!state_code %in% c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP"),
         practicing_status == "Practicing", !is.na(npi)) %>%
  mutate(npi_chr = as.character(npi))
abohns_subspec_per_id <- abohns_raw %>%
  group_by(abohns_id) %>%
  summarise(
    has_pediatric   = any(certification == "Complex Pediatric Otolaryngology"),
    has_neurotology = any(certification == "Neurotology"),
    has_sleep       = any(certification == "Sleep Medicine"),
    .groups = "drop"
  )
subspec_by_npi <- abohns_active %>%
  left_join(abohns_subspec_per_id, by = "abohns_id") %>%
  group_by(npi_chr) %>%
  summarise(
    has_pediatric   = any(has_pediatric,   na.rm = TRUE),
    has_neurotology = any(has_neurotology, na.rm = TRUE),
    has_sleep       = any(has_sleep,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(subspecialty = case_when(
    has_pediatric   ~ ABOTO_SUBSPECIALTY_PRIORITY[1],
    has_neurotology ~ ABOTO_SUBSPECIALTY_PRIORITY[2],
    has_sleep       ~ ABOTO_SUBSPECIALTY_PRIORITY[3],
    TRUE            ~ ABOTO_SUBSPECIALTY_PRIORITY[4]
  )) %>%
  select(npi = npi_chr, subspecialty)

# ── Score the cohort ──────────────────────────────────────────────────────────
con <- dbConnect(duckdb(),
                 "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb",
                 read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

DBI::dbWriteTable(con, "tmp_npis",
                  data.frame(npi = sample_npis),
                  temporary = TRUE, overwrite = TRUE)
DBI::dbWriteTable(con, "tmp_ent",
                  data.frame(cpt_code = ent_cpts),
                  temporary = TRUE, overwrite = TRUE)

cat("Querying Medicare Part B (this may take ~30s)...\n")
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
         COUNT(DISTINCT b.HCPCS_Cd) AS ent_distinct_codes
  FROM medicare_part_b_by_service_all_years b
  JOIN tmp_npis t ON CAST(b.Rndrng_NPI AS VARCHAR) = t.npi
  JOIN tmp_ent  e ON b.HCPCS_Cd = e.cpt_code
  GROUP BY 1
"))

scored <- tibble::tibble(npi = sample_npis) %>%
  left_join(total_by_npi,   by = "npi") %>%
  left_join(ent_by_npi,     by = "npi") %>%
  left_join(subspec_by_npi, by = "npi") %>%
  mutate(
    total_services_all_years = dplyr::coalesce(total_services_all_years, 0),
    ent_services_all_years   = dplyr::coalesce(ent_services_all_years,   0),
    ent_services_recent      = dplyr::coalesce(ent_services_recent,      0),
    ent_distinct_codes       = dplyr::coalesce(ent_distinct_codes,       0L),
    pct_ent = if_else(total_services_all_years > 0,
                      100 * ent_services_all_years / total_services_all_years,
                      NA_real_),
    ent_score = case_when(
      is.na(pct_ent) ~ "NO_MEDICARE_ACTIVITY",
      pct_ent >= 50  ~ "HIGHLY_LIKELY",
      pct_ent >= 10  ~ "LIKELY",
      pct_ent >= 1   ~ "INCIDENTAL",
      pct_ent >  0   ~ "NEGLIGIBLE",
      TRUE           ~ "NONE"
    ),
    recently_active = ent_services_recent > 0,
    final_classification = case_when(
      ent_score %in% c("HIGHLY_LIKELY","LIKELY") & recently_active ~ "ACTIVE_ENT",
      ent_score %in% c("HIGHLY_LIKELY","LIKELY") & !recently_active ~ "PAST_ENT",
      ent_score %in% c("INCIDENTAL","NEGLIGIBLE","NONE")            ~ "NOT_ENT_BY_BILLING",
      ent_score == "NO_MEDICARE_ACTIVITY"                          ~ "NO_MEDICARE_DATA"
    )
  )

# ── Headline metrics with 95% CI ──────────────────────────────────────────────
n <- nrow(scored)
ci <- function(k, n) {
  est <- k / n
  se  <- sqrt(est * (1 - est) / n)
  sprintf("%.1f%%  (95%% CI %.1f - %.1f)",
          100*est, 100*pmax(0, est - 1.96*se), 100*pmin(1, est + 1.96*se))
}
n_loose  <- sum(scored$ent_score %in%
                c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE"))
n_strict <- sum(scored$ent_score %in% c("HIGHLY_LIKELY","LIKELY"))
n_active <- sum(scored$final_classification == "ACTIVE_ENT")

cat("\n=== HEADLINE SENSITIVITY (n = 1,000 Triple Match NPIs) ===\n")
cat(sprintf("  Any ENT code billed             : %s\n", ci(n_loose,  n)))
cat(sprintf("  pct_ent >= 10%% (recommended)    : %s\n", ci(n_strict, n)))
cat(sprintf("  Final ACTIVE_ENT (>=10%% AND     : %s\n", ci(n_active, n)))
cat( "                  recent activity)\n")

# ── Score distribution ────────────────────────────────────────────────────────
score_levels <- c("HIGHLY_LIKELY","LIKELY","INCIDENTAL","NEGLIGIBLE",
                  "NONE","NO_MEDICARE_ACTIVITY")
cat("\n=== SCORE DISTRIBUTION ===\n")
print(scored %>% count(ent_score = factor(ent_score, levels = score_levels),
                       .drop = FALSE) %>%
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))))

# ── Final classification ──────────────────────────────────────────────────────
cat("\n=== FINAL CLASSIFICATION ===\n")
print(scored %>% count(final_classification) %>%
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) %>%
        arrange(desc(n)))

# ── pct_ent distribution ──────────────────────────────────────────────────────
cat("\n=== pct_ent DISTRIBUTION (among NPIs with any ENT billing) ===\n")
nonzero <- scored$pct_ent[!is.na(scored$pct_ent) & scored$pct_ent > 0]
cat(sprintf("  n with any ENT code  : %d\n", length(nonzero)))
cat(sprintf("  Median pct_ent       : %.1f%%\n", median(nonzero)))
cat(sprintf("  IQR                  : %.1f%% - %.1f%%\n",
            quantile(nonzero, 0.25), quantile(nonzero, 0.75)))
cat(sprintf("  Min - Max            : %.2f%% - %.1f%%\n",
            min(nonzero), max(nonzero)))

# ── Subspecialty stratified ───────────────────────────────────────────────────
cat("\n=== SENSITIVITY BY ABOto SUBSPECIALTY ===\n")
print(scored %>%
        group_by(subspecialty) %>%
        summarise(
          n_total       = dplyr::n(),
          n_active_ent  = sum(final_classification == "ACTIVE_ENT"),
          pct_active    = sprintf("%.1f%%", 100 * n_active_ent / n_total),
          n_no_medicare = sum(final_classification == "NO_MEDICARE_DATA"),
          .groups = "drop"
        ) %>% arrange(desc(n_total)))

# ── Recency ───────────────────────────────────────────────────────────────────
cat("\n=== TEMPORAL RECENCY (NPIs with any ENT billing) ===\n")
print(scored %>%
        filter(ent_services_all_years > 0) %>%
        count(recently_active) %>%
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n)),
               meaning = ifelse(recently_active,
                                "ENT code billed in 2021-2023",
                                "ENT only before 2021 (likely retired)")))

# ── Save per-NPI report ───────────────────────────────────────────────────────
out_csv <- here::here("publication_materials/figures/ent_data_integrity",
                      "ent_classifier_n1000_validation.csv")
write_csv(scored, out_csv)
cat(sprintf("\nPer-NPI report (1,000 rows): %s\n", out_csv))
