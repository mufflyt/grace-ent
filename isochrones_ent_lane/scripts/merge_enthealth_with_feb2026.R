#!/usr/bin/env Rscript
# =============================================================================
# MERGE FRESH ENTHEALTH SCRAPE WITH FEB 2026 SUPPLEMENTARY DATA
# =============================================================================
# Left-joins the fresh enthealth.org directory scrape (results.xlsx) with the
# Feb 2026 enriched file (ent_final_with_all_npi.xlsx) and the ABOHNS
# certification file on `url`.
#
# Fresh scrape supplies:    full_name, address, phone, specialty, education
#                           fields, board_cert, medical_school_year (age proxy)
# Feb 2026 supplies:        npi_final (86%), npi_taxonomy, npi_credential,
#                           age/gender for mystery shopper subsample (6%)
# ABOHNS file supplies:     initial_certification date (age proxy, 61%)
#
# Age priority (first non-NA wins):
#   1. mystery shopper age, adjusted forward from call date to today
#   2. estimated from medical_school_year (scrape):  2026 - (med_yr - 26)
#   3. estimated from residency_year (scrape):       2026 - (res_yr - 30)
#   4. estimated from initial_certification year:    2026 - (cert_yr - 32)
#
# Usage:
#   Rscript scripts/merge_enthealth_with_feb2026.R
#
# Output:
#   ~/Dropbox (Personal)/mystery_shopper/ent_merged_YYYYMMDD.csv
#   ~/Dropbox (Personal)/mystery_shopper/ent_merged_YYYYMMDD.xlsx
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(openxlsx)
  library(lubridate)
})

TODAY <- as.Date("2026-05-03")

# ── Paths ─────────────────────────────────────────────────────────────────────
DROPBOX <- file.path(path.expand("~"), "Dropbox (Personal)", "mystery_shopper")
if (!dir.exists(DROPBOX))
  DROPBOX <- file.path(path.expand("~"), "Dropbox", "mystery_shopper")

SCRAPE_XLSX  <- file.path(DROPBOX, "Corbi study", "ENT", "Data", "results.xlsx")
FEB_XLSX     <- file.path(DROPBOX, "ent_final_with_all_npi.xlsx")
ABOHNS_XLSX  <- file.path(DROPBOX, "abohns_all_ent_certification.xlsx")
DATESTAMP    <- format(TODAY, "%Y%m%d")
OUT_CSV      <- file.path(DROPBOX, sprintf("ent_merged_%s.csv",  DATESTAMP))
OUT_XLSX     <- file.path(DROPBOX, sprintf("ent_merged_%s.xlsx", DATESTAMP))

cat("\n=======================================================================\n")
cat("  MERGE ENTHEALTH SCRAPE + FEB 2026 + ABOHNS SUPPLEMENTARY DATA\n")
cat(sprintf("  Run  : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  Today: %s\n", TODAY))
cat("=======================================================================\n\n")

# ── Load fresh scrape ─────────────────────────────────────────────────────────
if (!file.exists(SCRAPE_XLSX))
  stop(sprintf("Fresh scrape not found: %s\nRun scrape_enthealth_directory.R first.", SCRAPE_XLSX))

scrape <- read_excel(SCRAPE_XLSX) |> rename_with(trimws)
cat(sprintf("Fresh scrape    : %s rows, %d cols\n", format(nrow(scrape), big.mark=","), ncol(scrape)))

# ── Load Feb 2026 supplementary file ─────────────────────────────────────────
feb_want <- c("url", "npi_final", "npi_taxonomy", "npi_credential",
              "age", "Age", "age_category", "age_adjusted",
              "gender", "AAO_regions", "cbsatype10",
              "business_days_until_appointment", "Able_to_make_appointment",
              "contacted", "call_date", "Appointment_Date")

feb_raw <- read_excel(FEB_XLSX) |>
  rename_with(trimws) |>
  mutate(across(any_of("call_date"), as.Date))   # POSIXct → Date before any math

feb <- feb_raw |>
  select(all_of(feb_want[feb_want %in% names(feb_raw)])) |>
  distinct(url, .keep_all = TRUE)
cat(sprintf("Feb 2026 file   : %s rows, %d supplementary cols\n",
            format(nrow(feb), big.mark=","), ncol(feb) - 1L))

# ── Load ABOHNS certification file ────────────────────────────────────────────
abohns <- read_excel(ABOHNS_XLSX) |>
  rename_with(trimws) |>
  select(url, initial_certification) |>
  mutate(
    cert_year = as.integer(format(as.Date(initial_certification, "%m/%d/%Y"), "%Y"))
  ) |>
  distinct(url, .keep_all = TRUE)
cat(sprintf("ABOHNS file     : %s rows, cert_year coverage: %s (%.0f%%)\n",
            format(nrow(abohns), big.mark=","),
            format(sum(!is.na(abohns$cert_year)), big.mark=","),
            100 * mean(!is.na(abohns$cert_year))))

# ── URL overlap ───────────────────────────────────────────────────────────────
cat(sprintf("\nURL overlap (Feb)   : %s / %s (%.0f%%)\n",
            format(sum(scrape$url %in% feb$url),   big.mark=","),
            format(nrow(scrape), big.mark=","),
            100 * mean(scrape$url %in% feb$url)))
cat(sprintf("URL overlap (ABOHNS): %s / %s (%.0f%%)\n",
            format(sum(scrape$url %in% abohns$url), big.mark=","),
            format(nrow(scrape), big.mark=","),
            100 * mean(scrape$url %in% abohns$url)))

# ── Rename clashing Feb columns ───────────────────────────────────────────────
clash <- setdiff(intersect(names(feb), names(scrape)), "url")
if (length(clash) > 0)
  feb <- feb |> rename_with(\(x) paste0(x, "_feb2026"), all_of(clash))

# ── Join all three sources ────────────────────────────────────────────────────
merged <- scrape |>
  left_join(feb,    by = "url", relationship = "many-to-one") |>
  left_join(abohns, by = "url", relationship = "many-to-one")

cat(sprintf("\nMerged dataset  : %s rows, %d cols\n",
            format(nrow(merged), big.mark=","), ncol(merged)))

# ── Age adjustment and estimation ─────────────────────────────────────────────
# Average ages at key career milestones (US norms):
#   Medical school graduation: 26
#   End of ENT residency (5 yr):  31
#   Board certification:          ~32 (1 yr after residency completion)
MS_GRAD_AGE   <- 26L
RES_DONE_AGE  <- 31L
CERT_AGE      <- 32L
CURRENT_YEAR  <- as.integer(format(TODAY, "%Y"))

merged <- merged |>
  mutate(
    # Adjust mystery-shopper age forward from call date to today
    # call_date is already a Date (normalised after read_excel above)
    years_since_call  = as.numeric(TODAY - call_date) / 365.25,
    age_ms_adjusted   = suppressWarnings(
      round(as.numeric(coalesce(age, Age)) + years_since_call, 1)
    ),

    # Estimate age from medical_school_year (scrape field)
    age_from_med_school = if_else(
      !is.na(medical_school_year) & between(medical_school_year, 1950L, CURRENT_YEAR),
      as.numeric(CURRENT_YEAR - (medical_school_year - MS_GRAD_AGE)),
      NA_real_
    ),

    # Estimate age from residency_year (scrape field)
    age_from_residency = if_else(
      !is.na(residency_year) & between(residency_year, 1950L, CURRENT_YEAR),
      as.numeric(CURRENT_YEAR - (residency_year - RES_DONE_AGE)),
      NA_real_
    ),

    # Estimate age from ABOHNS initial_certification year
    age_from_cert = if_else(
      !is.na(cert_year) & between(cert_year, 1950L, CURRENT_YEAR),
      as.numeric(CURRENT_YEAR - (cert_year - CERT_AGE)),
      NA_real_
    ),

    # Final age: priority cascade
    age_final = coalesce(
      age_ms_adjusted,      # 1. mystery shopper (most accurate, adjusted)
      age_from_med_school,  # 2. medical school graduation year
      age_from_residency,   # 3. residency completion year
      age_from_cert         # 4. board certification year
    ),

    graduation_year_source = case_when(
      !is.na(age_ms_adjusted)    ~ "mystery_shopper_adjusted",
      !is.na(age_from_med_school) ~ "medical_school_year",
      !is.na(age_from_residency) ~ "residency_year",
      !is.na(age_from_cert)      ~ "board_certification_year",
      TRUE                       ~ NA_character_
    )
  ) |>
  select(-years_since_call)

# ── Age coverage report ───────────────────────────────────────────────────────
age_src_tbl <- merged |>
  count(graduation_year_source, name = "n") |>
  mutate(pct = round(100 * n / nrow(merged), 1)) |>
  arrange(desc(n))

cat("\nAge source breakdown:\n")
for (i in seq_len(nrow(age_src_tbl))) {
  cat(sprintf("  %-35s %s (%.1f%%)\n",
              coalesce(age_src_tbl$graduation_year_source[i], "(none)"),
              format(age_src_tbl$n[i], big.mark=","),
              age_src_tbl$pct[i]))
}
cat(sprintf("\n  age_final coverage: %s / %s (%.0f%%)\n",
            format(sum(!is.na(merged$age_final)), big.mark=","),
            format(nrow(merged), big.mark=","),
            100 * mean(!is.na(merged$age_final))))

if (!all(is.na(merged$age_final))) {
  cat(sprintf("  age_final range   : %.0f – %.0f (median %.0f)\n",
              min(merged$age_final, na.rm=TRUE),
              max(merged$age_final, na.rm=TRUE),
              median(merged$age_final, na.rm=TRUE)))
}

# ── Validation: formula age vs. mystery-shopper known age ────────────────────
# Corbi calls supplied ages for ~178 physicians. Compare age_from_med_school
# (formula estimate) against age_ms_adjusted (ground truth, call date adjusted).
validation_rows <- merged |>
  dplyr::filter(!is.na(age_ms_adjusted) & !is.na(age_from_med_school))

if (nrow(validation_rows) > 0) {
  errs   <- validation_rows$age_from_med_school - validation_rows$age_ms_adjusted
  mae    <- mean(abs(errs), na.rm = TRUE)
  rmse   <- sqrt(mean(errs^2, na.rm = TRUE))
  bias   <- mean(errs, na.rm = TRUE)
  within5 <- mean(abs(errs) <= 5, na.rm = TRUE)

  cat(sprintf("\nAge formula validation (n = %d with known Corbi age):\n",
              nrow(validation_rows)))
  cat(sprintf("  MAE               : %.1f years\n", mae))
  cat(sprintf("  RMSE              : %.1f years\n", rmse))
  cat(sprintf("  Bias (formula−true): %+.1f years\n", bias))
  cat(sprintf("  Within ±5 years   : %.0f%%\n", 100 * within5))
  cat(sprintf("  Error range       : %.0f to %+.0f years\n",
              min(errs, na.rm=TRUE), max(errs, na.rm=TRUE)))
} else {
  cat("\nAge formula validation: no overlap between Corbi ages and med-school-year estimates.\n")
}

# ── Full coverage report ──────────────────────────────────────────────────────
key_cols <- c("npi_final", "npi_taxonomy", "npi_credential",
              "age_final", "gender", "AAO_regions",
              "residency_name", "medical_school_name", "fellowship_name",
              "board_cert", "phone_number", "address")

cat("\nKey column coverage:\n")
for (col in key_cols[key_cols %in% names(merged)]) {
  x <- merged[[col]]
  n_ok <- sum(!is.na(x) & nzchar(as.character(x), keepNA = FALSE))
  cat(sprintf("  %-38s %s / %s (%.0f%%)\n",
              col,
              format(n_ok, big.mark=","),
              format(nrow(merged), big.mark=","),
              100 * n_ok / nrow(merged)))
}

# ── Save ──────────────────────────────────────────────────────────────────────
cat("\nSaving...\n")

tmp <- paste0(OUT_CSV, ".tmp")
write.csv(merged, tmp, row.names = FALSE, na = "")
file.rename(tmp, OUT_CSV)
cat(sprintf("  CSV : %s\n", OUT_CSV))

tmp <- paste0(OUT_XLSX, ".tmp")
write.xlsx(merged, tmp, overwrite = TRUE)
file.rename(tmp, OUT_XLSX)
cat(sprintf("  XLSX: %s\n", OUT_XLSX))

cat("\n=======================================================================\n")
cat("  DONE\n")
cat(sprintf("  %s rows  |  NPI known: %.0f%%  |  Age estimated: %.0f%%\n",
            format(nrow(merged), big.mark=","),
            100 * mean(!is.na(merged$npi_final)),
            100 * mean(!is.na(merged$age_final))))
cat("=======================================================================\n\n")
