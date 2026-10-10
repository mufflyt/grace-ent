#!/usr/bin/env Rscript
# =============================================================================
# SUPPLEMENT ENT SPECIALTY FROM NPPES TAXONOMY
# =============================================================================
# For physicians with specialty_primary = NA in results_cleaned.csv,
# match to NPPES by last_name + state + first_initial (207Y* taxonomy)
# and map taxonomy code → specialty label.
#
# Unknown specialty physicians stay NA per protocol — they do NOT default
# to "General Otolaryngology."
#
# Usage:
#   Rscript scripts/supplement_ent_specialty_nppes.R
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

source(here::here("R", "duckdb_safe.R"))

DROPBOX_DATA <- if (dir.exists(file.path(path.expand("~"), "Dropbox (Personal)",
                                         "mystery_shopper", "Corbi study", "ENT", "Data"))) {
  file.path(path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data")
} else {
  file.path(path.expand("~"), "Dropbox", "mystery_shopper", "Corbi study", "ENT", "Data")
}
DB_PATH     <- "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb"
CLEANED_CSV <- file.path(DROPBOX_DATA, "results_cleaned.csv")
GEOCODED_CSV <- file.path(DROPBOX_DATA, "results_geocoded.csv")

# ── Taxonomy → specialty label ─────────────────────────────────────────────────
TAX_MAP <- c(
  "207Y00000X" = "General Otolaryngology",
  "207YX0007X" = "Head and Neck Surgery",
  "207YX0905X" = "Neurotology",
  "207YP0228X" = "Pediatric Otolaryngology",
  "207YX0901X" = "Allergy",
  "207YX0602X" = "Facial Plastic and Reconstructive Surgery",
  "207YS0012X" = "Sleep Medicine",
  "207YX0400X" = "Rhinology"
  # 207YS0123X left unmapped → NA (unclear NUCC code)
)

cat("=======================================================================\n")
cat("  SUPPLEMENT ENT SPECIALTY FROM NPPES TAXONOMY\n")
cat(sprintf("  Started : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=======================================================================\n\n")

# ── Load data ─────────────────────────────────────────────────────────────────
df <- read_csv(CLEANED_CSV, na = c("", "NA"), show_col_types = FALSE)
cat(sprintf("Loaded %s physicians\n", format(nrow(df), big.mark = ",")))
cat(sprintf("  specialty_primary NA : %s (%.0f%%)\n",
            format(sum(is.na(df$specialty_primary)), big.mark = ","),
            100 * mean(is.na(df$specialty_primary))))

# ── Pull NPPES ENT taxonomy ────────────────────────────────────────────────────
cat("\nQuerying NPPES for ENT taxonomy codes...\n")

if (!file.exists(DB_PATH)) stop("DuckDB not found: ", DB_PATH)
con <- safe_duckdb(DB_PATH, read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

nppes_q <- paste0(
  'SELECT ',
  '  UPPER(TRIM("Provider Last Name (Legal Name)"))                       AS last_nppes, ',
  '  UPPER(TRIM("Provider First Name"))                                   AS first_nppes, ',
  '  UPPER(TRIM("Provider Business Practice Location Address State Name")) AS state_nppes, ',
  "  COALESCE(\"Healthcare Provider Taxonomy Code_1\",'') AS taxonomy_1, ",
  "  COALESCE(\"Healthcare Provider Taxonomy Code_2\",'') AS taxonomy_2 ",
  'FROM npi_2026 ',
  "WHERE \"Entity Type Code\" = '1' ",
  "  AND (\"NPI Deactivation Date\" IS NULL OR \"NPI Deactivation Date\" = '') ",
  "  AND (\"Healthcare Provider Taxonomy Code_1\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_2\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_3\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_4\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_5\" LIKE '207Y%')"
)

t0 <- Sys.time()
nppes_ent <- dbGetQuery(con, nppes_q) |>
  mutate(
    first_initial_nppes = substr(first_nppes, 1L, 1L),
    # Primary taxonomy = the 207Y* code; prefer code_1 if it's 207Y, else code_2
    primary_taxonomy = case_when(
      grepl("^207Y", taxonomy_1) ~ taxonomy_1,
      grepl("^207Y", taxonomy_2) ~ taxonomy_2,
      TRUE                       ~ taxonomy_1
    )
  )

cat(sprintf("  NPPES ENTs loaded: %s (%d sec)\n",
            format(nrow(nppes_ent), big.mark = ","),
            round(as.numeric(difftime(Sys.time(), t0, units = "secs")))))

# Pre-group by state for fast lookup
nppes_by_state <- split(nppes_ent, nppes_ent$state_nppes)

# ── Match missing-specialty physicians ────────────────────────────────────────
cat("\nMatching NA-specialty physicians to NPPES...\n")

na_specialty_mask <- is.na(df$specialty_primary)
missing_df <- df |>
  filter(na_specialty_mask) |>
  mutate(
    last_clean    = toupper(trimws(last_name)),
    first_clean   = toupper(trimws(first_name)),
    first_initial = substr(first_clean, 1L, 1L),
    state_upper   = toupper(trimws(state_code))
  ) |>
  select(url, last_clean, first_clean, first_initial, state_upper)

taxonomy_lookup <- rep(NA_character_, nrow(missing_df))
match_pass_log  <- rep(NA_integer_,   nrow(missing_df))

for (i in seq_len(nrow(missing_df))) {
  pool <- nppes_by_state[[missing_df$state_upper[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next

  # Pass 1: exact last + first_initial
  cands <- pool[pool$last_nppes        == missing_df$last_clean[i] &
                pool$first_initial_nppes == missing_df$first_initial[i], ]
  if (nrow(cands) > 0L) { match_pass_log[i] <- 1L } else {
    # Pass 2: exact last, any first
    cands <- pool[pool$last_nppes == missing_df$last_clean[i], ]
    if (nrow(cands) > 0L) match_pass_log[i] <- 2L
  }

  if (nrow(cands) == 0L) next

  # Tiebreaker: best Jaro-Winkler first name
  if (nrow(cands) > 1L) {
    jw    <- stringsim(missing_df$first_clean[i], cands$first_nppes,
                       method = "jw", p = 0.1)
    cands <- cands[which.max(jw), , drop = FALSE]
  }

  taxonomy_lookup[i] <- cands$primary_taxonomy[1L]
}

n_tax_matched <- sum(!is.na(taxonomy_lookup))
cat(sprintf("  Taxonomy matched: %d / %d (%.0f%%)\n",
            n_tax_matched, nrow(missing_df),
            100 * n_tax_matched / max(nrow(missing_df), 1L)))

# ── Map taxonomy codes → specialty labels ─────────────────────────────────────
specialty_from_nppes <- TAX_MAP[taxonomy_lookup]
specialty_from_nppes[is.na(taxonomy_lookup)] <- NA_character_  # unmatched
# Unmapped codes (e.g. 207YS0123X) → NA (unknown, not defaulted to General)
specialty_from_nppes[!is.na(taxonomy_lookup) & is.na(specialty_from_nppes)] <- NA_character_

# ── Apply to main data ────────────────────────────────────────────────────────
missing_idx <- which(na_specialty_mask)
df$specialty_primary[missing_idx]        <- specialty_from_nppes
df$specialty_inferred_source[missing_idx][!is.na(specialty_from_nppes)] <- "nppes_taxonomy"

# ── Report ────────────────────────────────────────────────────────────────────
n_supplemented <- sum(!is.na(specialty_from_nppes))
n_still_na     <- sum(is.na(df$specialty_primary))

cat(sprintf("\n--- SPECIALTY SUPPLEMENTATION RESULTS ---\n"))
cat(sprintf("  Was NA specialty      : %d\n", sum(na_specialty_mask)))
cat(sprintf("  Supplemented from NPPES: %d (%.0f%% of NA group)\n",
            n_supplemented, 100 * n_supplemented / max(sum(na_specialty_mask), 1L)))
cat(sprintf("  Still NA after supplement: %d (%.0f%% of all)\n",
            n_still_na, 100 * n_still_na / max(nrow(df), 1L)))

cat(sprintf("\n--- BREAKDOWN OF NPPES-SUPPLEMENTED SPECIALTIES ---\n"))
supplemented_labels <- specialty_from_nppes[!is.na(specialty_from_nppes)]
print(sort(table(supplemented_labels), decreasing = TRUE))

cat("\n--- WHAT % WOULD NOT HAVE BEEN GENERAL ENT ---\n")
not_general <- sum(!is.na(supplemented_labels) &
                   supplemented_labels != "General Otolaryngology")
cat(sprintf("  Among %d newly-supplemented physicians:\n", n_supplemented))
cat(sprintf("    General Otolaryngology : %d (%.0f%%)\n",
            n_supplemented - not_general,
            100 * (n_supplemented - not_general) / n_supplemented))
cat(sprintf("    Non-General subspecialty: %d (%.0f%%)\n",
            not_general,
            100 * not_general / n_supplemented))

cat(sprintf("\n--- FULL SPECIALTY DISTRIBUTION (all %d physicians) ---\n", nrow(df)))
print(sort(table(df$specialty_primary, useNA = "ifany"), decreasing = TRUE))

# ── Save ──────────────────────────────────────────────────────────────────────
cat("\nSaving results_cleaned...\n")
tmp <- paste0(CLEANED_CSV, ".tmp")
write_csv(df, tmp, na = "")
file.rename(tmp, CLEANED_CSV)

wb <- createWorkbook()
addWorksheet(wb, "ENT_Cleaned")
writeData(wb, "ENT_Cleaned", df)
hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F4E79",
                  halign = "center", textDecoration = "Bold", border = "Bottom")
addStyle(wb, "ENT_Cleaned", hs, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
freezePane(wb, "ENT_Cleaned", firstRow = TRUE)
setColWidths(wb, "ENT_Cleaned", cols = seq_len(ncol(df)), widths = "auto")
xlsx_out <- sub("\\.csv$", ".xlsx", CLEANED_CSV)
tmp_x    <- paste0(xlsx_out, ".tmp")
saveWorkbook(wb, tmp_x, overwrite = TRUE)
file.rename(tmp_x, xlsx_out)
cat(sprintf("  CSV : %s\n", CLEANED_CSV))
cat(sprintf("  XLSX: %s\n", xlsx_out))

if (file.exists(GEOCODED_CSV)) {
  cat("\nUpdating results_geocoded...\n")
  geo <- read_csv(GEOCODED_CSV, na = c("", "NA"), show_col_types = FALSE)
  spec_update <- df |> select(url, specialty_primary, specialty_inferred_source)
  geo <- geo |>
    select(-any_of(c("specialty_primary", "specialty_inferred_source"))) |>
    left_join(spec_update, by = "url")
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
  tmp_gx   <- paste0(geo_xlsx, ".tmp")
  saveWorkbook(wb2, tmp_gx, overwrite = TRUE)
  file.rename(tmp_gx, geo_xlsx)
  cat(sprintf("  CSV : %s\n", GEOCODED_CSV))
  cat(sprintf("  XLSX: %s\n", geo_xlsx))
}

cat("\n=======================================================================\n")
cat("  SPECIALTY SUPPLEMENTATION COMPLETE\n")
cat(sprintf("  Supplemented from NPPES: %d physicians\n", n_supplemented))
cat(sprintf("  Still unknown specialty : %d (%.1f%%) — stay NA per protocol\n",
            n_still_na, 100 * n_still_na / nrow(df)))
cat("=======================================================================\n\n")
