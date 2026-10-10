#!/usr/bin/env Rscript
# =============================================================================
# SUPPLEMENT ENT PHONE NUMBERS FROM NPPES
# =============================================================================
# For physicians missing a phone in results_cleaned.csv / results_geocoded.csv,
# look up the NPPES practice-location telephone number by matching:
#   UPPER(last_name) + state_code + first_initial  (among taxonomy 207Y*)
#
# Also recovers the 19 garbled raw phone values where ≥10 digits exist.
#
# Updates both:
#   results_cleaned.csv  / results_cleaned.xlsx
#   results_geocoded.csv / results_geocoded.xlsx
#
# Usage:
#   Rscript scripts/supplement_ent_phone_nppes.R
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(stringdist)
  library(openxlsx)
  library(DBI)
  library(duckdb)
})

DROPBOX_DATA <- if (dir.exists(file.path(path.expand("~"), "Dropbox (Personal)",
                                         "mystery_shopper", "Corbi study", "ENT", "Data"))) {
  file.path(path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data")
} else {
  file.path(path.expand("~"), "Dropbox", "mystery_shopper", "Corbi study", "ENT", "Data")
}
DB_PATH      <- "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb"
CLEANED_CSV  <- file.path(DROPBOX_DATA, "results_cleaned.csv")
GEOCODED_CSV <- file.path(DROPBOX_DATA, "results_geocoded.csv")

cat("=======================================================================\n")
cat("  SUPPLEMENT ENT PHONE NUMBERS FROM NPPES\n")
cat(sprintf("  Started : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=======================================================================\n\n")

# ── Load cleaned data ─────────────────────────────────────────────────────────
df <- read_csv(CLEANED_CSV, na = c("", "NA"), show_col_types = FALSE)
cat(sprintf("Loaded %s physicians\n", format(nrow(df), big.mark = ",")))

# ── Step 1: recover digits from the 19 garbled raw phone_number values ────────
cat("\nStep 1: recovering garbled phone values...\n")

garbled_mask <- !is.na(df$phone_number) &
                (is.na(df$phone_digits) | nchar(as.character(df$phone_digits)) < 10)

recover_digits <- function(raw) {
  digits <- gsub("[^0-9]", "", as.character(raw))
  if (nchar(digits) == 10) return(as.numeric(digits))
  if (nchar(digits) == 11 && substr(digits, 1, 1) == "1")
    return(as.numeric(substr(digits, 2, 11)))
  NA_real_
}

recovered <- vapply(df$phone_number[garbled_mask], recover_digits, numeric(1))
n_recovered_garbled <- sum(!is.na(recovered))
df$phone_digits[garbled_mask] <- recovered
cat(sprintf("  Recovered %d of %d garbled values\n",
            n_recovered_garbled, sum(garbled_mask)))

# ── Step 2: identify still-missing phones ─────────────────────────────────────
still_missing <- is.na(df$phone_digits) | nchar(as.character(df$phone_digits)) < 10
cat(sprintf("\nStep 2: %s physicians still lack a valid phone\n",
            format(sum(still_missing), big.mark = ",")))

# ── Step 3: pull NPPES ENT telephone numbers ──────────────────────────────────
cat("\nStep 3: querying NPPES for ENT practice telephones...\n")

if (!file.exists(DB_PATH)) stop("DuckDB not found: ", DB_PATH)

con <- dbConnect(duckdb(), DB_PATH, read_only = TRUE)
on.exit(dbDisconnect(con, shutdown = FALSE), add = TRUE)

nppes_q <- paste0(
  'SELECT ',
  '  "NPI"                                                                AS npi, ',
  '  UPPER(TRIM("Provider Last Name (Legal Name)"))                       AS last_nppes, ',
  '  UPPER(TRIM("Provider First Name"))                                   AS first_nppes, ',
  '  UPPER(TRIM("Provider Business Practice Location Address State Name")) AS state_nppes, ',
  '  REGEXP_REPLACE("Provider Business Practice Location Address Telephone Number",',
  "    '[^0-9]', '', 'g')                                                AS tel_raw ",
  'FROM npi_2026 ',
  "WHERE \"Entity Type Code\" = '1' ",
  "  AND (\"NPI Deactivation Date\" IS NULL OR \"NPI Deactivation Date\" = '') ",
  "  AND (\"Healthcare Provider Taxonomy Code_1\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_2\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_3\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_4\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_5\" LIKE '207Y%') ",
  "  AND \"Provider Business Practice Location Address Telephone Number\" IS NOT NULL ",
  "  AND LENGTH(REGEXP_REPLACE(\"Provider Business Practice Location Address Telephone Number\",",
  "    '[^0-9]', '', 'g')) >= 10"
)

t0 <- Sys.time()
nppes_ent <- dbGetQuery(con, nppes_q) |>
  mutate(
    first_initial_nppes = substr(first_nppes, 1L, 1L),
    phone_digits_nppes  = as.numeric(case_when(
      nchar(tel_raw) == 10                              ~ tel_raw,
      nchar(tel_raw) == 11 & substr(tel_raw,1,1) == "1" ~ substr(tel_raw, 2, 11),
      TRUE                                              ~ NA_character_
    ))
  ) |>
  filter(!is.na(phone_digits_nppes))

cat(sprintf("  NPPES ENTs with valid phone: %s (%d sec)\n",
            format(nrow(nppes_ent), big.mark = ","),
            round(as.numeric(difftime(Sys.time(), t0, units = "secs")))))

# ── Step 4: match missing-phone physicians to NPPES ───────────────────────────
cat("\nStep 4: matching missing-phone physicians to NPPES...\n")

missing_df <- df |>
  filter(still_missing) |>
  mutate(
    last_clean    = toupper(trimws(last_name)),
    first_clean   = toupper(trimws(first_name)),
    first_initial = substr(first_clean, 1L, 1L),
    state_upper   = toupper(trimws(state_code))
  ) |>
  select(url, last_clean, first_clean, first_initial, state_upper)

nppes_by_state <- split(nppes_ent, nppes_ent$state_nppes)

n_matched   <- 0L
phone_lookup <- rep(NA_real_, nrow(missing_df))

for (i in seq_len(nrow(missing_df))) {
  pool <- nppes_by_state[[missing_df$state_upper[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next

  # Pass 1: exact last + first_initial
  cands <- pool[pool$last_nppes == missing_df$last_clean[i] &
                pool$first_initial_nppes == missing_df$first_initial[i], ]

  # Pass 2: exact last, relax first_initial
  if (nrow(cands) == 0L)
    cands <- pool[pool$last_nppes == missing_df$last_clean[i], ]

  if (nrow(cands) == 0L) next

  # Tiebreaker: Jaro-Winkler on first name
  if (nrow(cands) > 1L) {
    jw <- stringsim(missing_df$first_clean[i], cands$first_nppes,
                    method = "jw", p = 0.1)
    cands <- cands[which.max(jw), , drop = FALSE]
  }

  phone_lookup[i] <- cands$phone_digits_nppes[1L]
  n_matched <- n_matched + 1L
}

cat(sprintf("  Matched: %d / %d missing-phone physicians (%.0f%%)\n",
            n_matched, nrow(missing_df),
            100 * n_matched / nrow(missing_df)))

# ── Step 5: apply phone supplements back to main data ─────────────────────────
cat("\nStep 5: applying supplements...\n")

missing_idx <- which(still_missing)
df$phone_digits[missing_idx] <- phone_lookup
df$phone_source <- "enthealth"
df$phone_source[missing_idx[!is.na(phone_lookup)]] <- "nppes_supplement"

# Reformat phone_number display for newly-supplemented rows
format_phone <- function(digits) {
  d <- sprintf("%.0f", digits)
  if (nchar(d) != 10) return(NA_character_)
  paste0("(", substr(d,1,3), ") ", substr(d,4,6), "-", substr(d,7,10))
}
supplemented_mask <- df$phone_source == "nppes_supplement"
df$phone_number[supplemented_mask] <- vapply(
  df$phone_digits[supplemented_mask], format_phone, character(1)
)

# ── Final coverage report ─────────────────────────────────────────────────────
n_valid_final <- sum(!is.na(df$phone_digits) & nchar(as.character(df$phone_digits)) == 10)
cat(sprintf("\n  Phone coverage BEFORE: %d / %d (%.0f%%)\n",
            sum(!still_missing), nrow(df), 100 * sum(!still_missing) / nrow(df)))
cat(sprintf("  Phone coverage AFTER : %d / %d (%.0f%%)\n",
            n_valid_final, nrow(df), 100 * n_valid_final / nrow(df)))
cat(sprintf("  Sources: enthealth=%d  nppes_supplement=%d  garbled_recovered=%d\n",
            sum(df$phone_source == "enthealth", na.rm = TRUE),
            sum(df$phone_source == "nppes_supplement", na.rm = TRUE),
            n_recovered_garbled))

# ── Save cleaned CSV + XLSX ───────────────────────────────────────────────────
cat("\nSaving results_cleaned...\n")
tmp <- paste0(CLEANED_CSV, ".tmp")
write_csv(df, tmp, na = "")
file.rename(tmp, CLEANED_CSV)

wb <- createWorkbook()
addWorksheet(wb, "ENT_Cleaned")
writeData(wb, "ENT_Cleaned", df)
hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E79",
                  halign = "center", textDecoration = "Bold", border = "Bottom")
addStyle(wb, "ENT_Cleaned", hs, rows = 1,
         cols = seq_len(ncol(df)), gridExpand = TRUE)
freezePane(wb, "ENT_Cleaned", firstRow = TRUE)
setColWidths(wb, "ENT_Cleaned", cols = seq_len(ncol(df)), widths = "auto")
xlsx_path <- sub("\\.csv$", ".xlsx", CLEANED_CSV)
tmp_x <- paste0(xlsx_path, ".tmp")
saveWorkbook(wb, tmp_x, overwrite = TRUE)
file.rename(tmp_x, xlsx_path)
cat(sprintf("  CSV : %s\n", CLEANED_CSV))
cat(sprintf("  XLSX: %s\n", xlsx_path))

# ── Update geocoded file if it exists ─────────────────────────────────────────
if (file.exists(GEOCODED_CSV)) {
  cat("\nUpdating results_geocoded...\n")

  geo <- read_csv(GEOCODED_CSV, na = c("", "NA"), show_col_types = FALSE)

  phone_update <- df |>
    select(url, phone_number, phone_digits, phone_source)

  geo <- geo |>
    select(-any_of(c("phone_number", "phone_digits", "phone_source"))) |>
    left_join(phone_update, by = "url")

  tmp_g <- paste0(GEOCODED_CSV, ".tmp")
  write_csv(geo, tmp_g, na = "")
  file.rename(tmp_g, GEOCODED_CSV)

  wb2 <- createWorkbook()
  addWorksheet(wb2, "Physicians_Geocoded")
  writeData(wb2, "Physicians_Geocoded", geo)
  addStyle(wb2, "Physicians_Geocoded", hs, rows = 1,
           cols = seq_len(ncol(geo)), gridExpand = TRUE)
  freezePane(wb2, "Physicians_Geocoded", firstRow = TRUE)
  setColWidths(wb2, "Physicians_Geocoded", cols = seq_len(ncol(geo)), widths = "auto")
  geo_xlsx <- sub("\\.csv$", ".xlsx", GEOCODED_CSV)
  tmp_gx <- paste0(geo_xlsx, ".tmp")
  saveWorkbook(wb2, tmp_gx, overwrite = TRUE)
  file.rename(tmp_gx, geo_xlsx)
  cat(sprintf("  CSV : %s\n", GEOCODED_CSV))
  cat(sprintf("  XLSX: %s\n", geo_xlsx))
}

cat("\n=======================================================================\n")
cat("  PHONE SUPPLEMENTATION COMPLETE\n")
cat(sprintf("  Garbled values recovered : %d\n", n_recovered_garbled))
cat(sprintf("  NPPES supplements added  : %d\n", n_matched))
cat(sprintf("  Still missing            : %d\n",
            sum(is.na(df$phone_digits) | nchar(as.character(df$phone_digits)) < 10)))
cat(sprintf("  Final phone coverage     : %.1f%%\n",
            100 * n_valid_final / nrow(df)))
cat("=======================================================================\n\n")
