#!/usr/bin/env Rscript
# =============================================================================
# Who are the 2,954 "NPI Only" ENT physicians?
#
# Cohort: NPPES 207Y* universe, NOT ABOto-active, NOT in ENThealth (Euler
# zone "NPI Only"). Defined upstream by scripts/build_ent_euler_table1.R.
#
# Discriminating signals:
#   - Credential text (MD vs DO vs other) — DOs are board-certified by
#     AOBOHNS (not ABOto), so they look "never boarded" in our analysis.
#   - Years since NPPES enumeration in buckets — recent enumeration =
#     resident/fellow plausibility (still in pre-cert window).
#   - Medicare Part B billing in 2022 OR 2023 — trainees typically don't
#     bill under their own NPI. Active billing = practicing, not a resident.
#
# Output:
#   publication_materials/figures/ent_data_integrity/
#     who_are_npi_only_summary.csv      (one-row overall summary)
#     who_are_npi_only_crosstabs.csv    (cross-tabs for the report)
#     who_are_npi_only_per_npi.csv      (per-physician enrichment)
# =============================================================================

suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(stringr)
  library(DBI); library(duckdb)
})

dir_out <- here::here("publication_materials", "figures", "ent_data_integrity")

# ── Pull the 2,954 NPI-only NPIs from the per-NPI assignments CSV ───────────
assignments <- read_csv(file.path(dir_out, "ent_euler_zone_npi_assignments.csv"),
                        show_col_types = FALSE)
npi_only <- assignments %>%
  filter(zone == "1_nppes_only") %>%
  mutate(npi = as.character(npi_chr)) %>%
  select(npi, nppes_gender, nppes_enum_year, state_final, nppes_primary_taxonomy)
stopifnot("NPI-only N != 2,954" = nrow(npi_only) == 2954L)

# ── Pull credentials + enum + deactivation from NPPES Feb 2026 ──────────────
duckdb_path <- "/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb"
con <- dbConnect(duckdb::duckdb(), duckdb_path, read_only = TRUE)

npi_sql <- paste(npi_only$npi, collapse = ",")
nppes_extra <- dbGetQuery(con, sprintf('
  SELECT
    "NPI"::VARCHAR                  AS npi,
    "Provider Credential Text"      AS credential_text,
    "NPI Deactivation Date"         AS deact_date
  FROM "NPPES_Data_Dissemination_February_2026_npidata_pfile_20050523_20260208_csv"
  WHERE "NPI" IN (%s)
', npi_sql))

# Medicare Part B 2022 + 2023 — whether NPI appears at all (any HCPCS)
medicare_npis <- dbGetQuery(con, sprintf('
  SELECT DISTINCT CAST("Rndrng_NPI" AS VARCHAR) AS npi
  FROM medicare_part_b_2023
  WHERE CAST("Rndrng_NPI" AS VARCHAR) IN (%s)
  UNION
  SELECT DISTINCT CAST("Rndrng_NPI" AS VARCHAR) AS npi
  FROM medicare_part_b_2022
  WHERE CAST("Rndrng_NPI" AS VARCHAR) IN (%s)
', npi_sql, npi_sql)) %>%
  mutate(billed_medicare_2022_or_2023 = TRUE)

dbDisconnect(con, shutdown = TRUE)

# ── Compose per-NPI table with classifications ──────────────────────────────
classify_credential <- function(txt) {
  if (is.na(txt) || !nzchar(trimws(txt))) return("Missing")
  u <- toupper(txt)
  has_md  <- grepl("(^|[^A-Z])M\\.?D\\.?([^A-Z]|$)|MBBS", u)
  has_do  <- grepl("(^|[^A-Z])D\\.?O\\.?([^A-Z]|$)",       u)
  dplyr::case_when(
    has_md & has_do ~ "MD + DO",
    has_do          ~ "DO",
    has_md          ~ "MD",
    TRUE            ~ "Other (non-MD/DO)"
  )
}
classify_credential_v <- Vectorize(classify_credential, USE.NAMES = FALSE)

enum_bucket <- function(yr) {
  if (is.na(yr)) return("Missing")
  yrs <- 2026L - as.integer(yr)
  dplyr::case_when(
    yrs <= 5             ~ "<=5 yrs (likely resident/fellow)",
    yrs >= 6 & yrs <= 10 ~ "6-10 yrs (pre-cert window)",
    yrs >= 11 & yrs <= 20 ~ "11-20 yrs (mid-career)",
    yrs > 20             ~ ">20 yrs (senior)",
    TRUE                  ~ "Missing"
  )
}
enum_bucket_v <- Vectorize(enum_bucket, USE.NAMES = FALSE)

per_npi <- npi_only %>%
  left_join(nppes_extra,   by = "npi") %>%
  left_join(medicare_npis, by = "npi") %>%
  mutate(
    billed_medicare_2022_or_2023 = dplyr::coalesce(billed_medicare_2022_or_2023, FALSE),
    credential_class             = classify_credential_v(credential_text),
    enum_bucket                  = enum_bucket_v(nppes_enum_year),
    is_deactivated               = !is.na(deact_date) & nchar(deact_date) > 0
  ) %>%
  select(npi, credential_text, credential_class,
         nppes_enum_year, enum_bucket,
         billed_medicare_2022_or_2023,
         is_deactivated, deact_date,
         nppes_gender, state_final, nppes_primary_taxonomy)

write_csv(per_npi, file.path(dir_out, "who_are_npi_only_per_npi.csv"), na = "")

# ── Overall one-row summary ─────────────────────────────────────────────────
n <- nrow(per_npi)
pct <- function(x) sprintf("%d (%.1f%%)", x, 100 * x / n)
overall <- tibble::tibble(
  Statistic = c(
    "N",
    "Credential: MD",
    "Credential: DO",
    "Credential: MD + DO",
    "Credential: Other / non-physician",
    "Credential: Missing",
    "Enumeration: <=5 yrs (resident/fellow plausible)",
    "Enumeration: 6-10 yrs (pre-cert window)",
    "Enumeration: 11-20 yrs (mid-career)",
    "Enumeration: >20 yrs (senior)",
    "Billed Medicare Part B in 2022 or 2023",
    "Did NOT bill Medicare in 2022 or 2023",
    "NPI deactivated (in NPPES Feb 2026)"
  ),
  Value = c(
    format(n, big.mark = ","),
    pct(sum(per_npi$credential_class == "MD")),
    pct(sum(per_npi$credential_class == "DO")),
    pct(sum(per_npi$credential_class == "MD + DO")),
    pct(sum(per_npi$credential_class == "Other (non-MD/DO)")),
    pct(sum(per_npi$credential_class == "Missing")),
    pct(sum(per_npi$enum_bucket == "<=5 yrs (likely resident/fellow)")),
    pct(sum(per_npi$enum_bucket == "6-10 yrs (pre-cert window)")),
    pct(sum(per_npi$enum_bucket == "11-20 yrs (mid-career)")),
    pct(sum(per_npi$enum_bucket == ">20 yrs (senior)")),
    pct(sum(per_npi$billed_medicare_2022_or_2023)),
    pct(sum(!per_npi$billed_medicare_2022_or_2023)),
    pct(sum(per_npi$is_deactivated))
  )
)
write_csv(overall, file.path(dir_out, "who_are_npi_only_summary.csv"), na = "")
cat("--- Overall summary ---\n"); print(overall, n = 30)

# ── Cross-tabs for the report ───────────────────────────────────────────────
ct1 <- per_npi %>%
  count(enum_bucket, credential_class, name = "n") %>%
  tidyr::pivot_wider(names_from = credential_class, values_from = n, values_fill = 0)
cat("\n--- Enumeration bucket  x  credential class ---\n"); print(ct1)

ct2 <- per_npi %>%
  count(enum_bucket, billed_medicare_2022_or_2023, name = "n") %>%
  tidyr::pivot_wider(names_from = billed_medicare_2022_or_2023,
                     names_prefix = "billed_", values_from = n, values_fill = 0)
cat("\n--- Enumeration bucket  x  Medicare 2022-2023 billing ---\n"); print(ct2)

ct3 <- per_npi %>%
  count(credential_class, billed_medicare_2022_or_2023, name = "n") %>%
  tidyr::pivot_wider(names_from = billed_medicare_2022_or_2023,
                     names_prefix = "billed_", values_from = n, values_fill = 0)
cat("\n--- Credential class  x  Medicare 2022-2023 billing ---\n"); print(ct3)

# Combined: a single "who-are-they" matrix
ct_all <- per_npi %>%
  mutate(
    profile = dplyr::case_when(
      enum_bucket == "<=5 yrs (likely resident/fellow)" & !billed_medicare_2022_or_2023 &
        credential_class %in% c("MD","MD + DO")
                                                ~ "A. Likely resident/fellow (recent NPI, no Medicare, MD)",
      credential_class == "DO"                  ~ "B. DO (likely AOBOHNS-certified, not ABOto)",
      enum_bucket %in% c("11-20 yrs (mid-career)",">20 yrs (senior)") &
        billed_medicare_2022_or_2023            ~ "C. Practicing physician, never ABOto-boarded / lapsed",
      enum_bucket == "6-10 yrs (pre-cert window)" &
        !billed_medicare_2022_or_2023           ~ "D. Pre-cert window, low/no Medicare visibility",
      enum_bucket == "6-10 yrs (pre-cert window)" &
        billed_medicare_2022_or_2023            ~ "E. Pre-cert window but billing (likely fellow/post-grad)",
      enum_bucket == "<=5 yrs (likely resident/fellow)" &
        billed_medicare_2022_or_2023            ~ "F. Recent NPI + Medicare billing (post-grad early-career)",
      credential_class == "Other (non-MD/DO)"   ~ "G. Non-physician (PA/NP/etc. with 207Y taxonomy)",
      credential_class == "Missing"             ~ "H. Missing credential text",
      TRUE                                       ~ "I. Other / does not fit a clean bucket"
    )
  ) %>%
  count(profile, sort = TRUE, name = "n") %>%
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n)))

write_csv(ct_all, file.path(dir_out, "who_are_npi_only_crosstabs.csv"), na = "")
cat("\n--- WHO ARE THEY?  (single-best-fit profile) ---\n")
print(ct_all, n = 20)

cat(sprintf("\nWrote:\n  %s\n  %s\n  %s\n",
            file.path(dir_out, "who_are_npi_only_summary.csv"),
            file.path(dir_out, "who_are_npi_only_crosstabs.csv"),
            file.path(dir_out, "who_are_npi_only_per_npi.csv")))
