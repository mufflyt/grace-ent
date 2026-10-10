library(DBI); library(duckdb); library(dplyr); library(readr); library(stringdist); library(openxlsx)
source(here::here("R", "duckdb_safe.R"))

DROPBOX_DATA <- file.path(path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data")
df <- read_csv(file.path(DROPBOX_DATA, "results_cleaned.csv"), na=c("","NA"), show_col_types=FALSE)

cat("=== STEP 0: Fix remaining garbled phones ===\n")

# Vanity number decoder: (212) 744-4ENT -> (212) 744-4368
KEYPAD <- c(A=2,B=2,C=2,D=3,E=3,F=3,G=4,H=4,I=4,J=5,K=5,L=5,
            M=6,N=6,O=6,P=7,Q=7,R=7,S=7,T=8,U=8,V=8,W=9,X=9,Y=9,Z=9)
decode_vanity <- function(x) {
  if (is.na(x)) return(NA_character_)
  chars <- strsplit(toupper(x), "")[[1]]
  decoded <- vapply(chars, function(ch) {
    if (ch %in% names(KEYPAD)) as.character(KEYPAD[[ch]]) else ch
  }, character(1))
  paste(decoded, collapse = "")
}

# Fix "(212) 744-4ENT" → decode → strip non-digits → 10-digit check
garbled_mask <- !is.na(df$phone_number) &
                (is.na(df$phone_digits) | nchar(as.character(df$phone_digits)) < 10)

for (i in which(garbled_mask)) {
  raw   <- df$phone_number[i]
  dec   <- decode_vanity(raw)          # replace letters with digits
  digs  <- gsub("[^0-9]", "", dec)     # strip punctuation
  # Trim leading 1 if 11-digit
  if (nchar(digs) == 11 && substr(digs,1,1) == "1") digs <- substr(digs, 2, 11)
  if (nchar(digs) == 10) {
    cat(sprintf("  FIXED: %s -> %s\n", raw, digs))
    df$phone_digits[i]  <- as.numeric(digs)
    df$phone_source[i]  <- "vanity_decoded"
    d <- digs
    df$phone_number[i]  <- paste0("(", substr(d,1,3), ") ", substr(d,4,6), "-", substr(d,7,10))
  }
}

still_garbled <- sum(!is.na(df$phone_number) &
                     (is.na(df$phone_digits) | nchar(as.character(df$phone_digits)) < 10))
cat(sprintf("  Still unresolvable: %d\n\n", still_garbled))

# Save fixed version
tmp <- paste0(file.path(DROPBOX_DATA, "results_cleaned.csv"), ".tmp")
write_csv(df, tmp, na=""); file.rename(tmp, file.path(DROPBOX_DATA, "results_cleaned.csv"))

# ── Connect DuckDB ──────────────────────────────────────────────────────────
con <- safe_duckdb("/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb", read_only = TRUE)
on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

cat("=== STEP 1: Pull full NPPES record for matched ENT physicians ===\n")

nppes_q <- '
SELECT
  NPI                                                                   AS npi,
  UPPER(TRIM("Provider Last Name (Legal Name)"))                        AS last_nppes,
  UPPER(TRIM("Provider First Name"))                                    AS first_nppes,
  UPPER(TRIM("Provider Business Practice Location Address State Name")) AS state_nppes,
  TRIM("Provider Business Practice Location Address City Name")         AS city_nppes,
  TRIM("Provider Business Practice Location Address Postal Code")       AS zip_nppes,
  TRIM("Provider First Line Business Practice Location Address")        AS addr_nppes,
  TRIM("Provider Organization Name (Legal Business Name)")              AS org_nppes,
  TRIM("Provider Credential Text")                                      AS credential_nppes,
  "Provider Sex Code"                                                   AS sex_nppes,
  REGEXP_REPLACE("Provider Business Practice Location Address Telephone Number",
    \'[^0-9]\', \'\', \'g\')                                          AS tel_raw,
  COALESCE("Healthcare Provider Taxonomy Code_1", \'\')                AS taxonomy_1,
  COALESCE("Healthcare Provider Taxonomy Code_2", \'\')                AS taxonomy_2,
  COALESCE("Healthcare Provider Taxonomy Code_3", \'\')                AS taxonomy_3,
  COALESCE("Healthcare Provider Taxonomy Code_4", \'\')                AS taxonomy_4,
  COALESCE("Healthcare Provider Taxonomy Code_5", \'\')                AS taxonomy_5
FROM npi_2026
WHERE "Entity Type Code" = \'1\'
  AND ("NPI Deactivation Date" IS NULL OR "NPI Deactivation Date" = \'\')
  AND ("Healthcare Provider Taxonomy Code_1" LIKE \'207Y%\'
    OR "Healthcare Provider Taxonomy Code_2" LIKE \'207Y%\'
    OR "Healthcare Provider Taxonomy Code_3" LIKE \'207Y%\'
    OR "Healthcare Provider Taxonomy Code_4" LIKE \'207Y%\'
    OR "Healthcare Provider Taxonomy Code_5" LIKE \'207Y%\')
'

t0 <- Sys.time()
nppes_ent <- dbGetQuery(con, nppes_q) |>
  mutate(
    first_initial_nppes = substr(first_nppes, 1, 1),
    last_stripped       = gsub("[^A-Za-z]", "", last_nppes),
    phone_nppes = case_when(
      nchar(tel_raw) == 10                               ~ tel_raw,
      nchar(tel_raw) == 11 & substr(tel_raw,1,1) == "1" ~ substr(tel_raw, 2, 11),
      TRUE                                               ~ NA_character_
    ),
    # Primary ENT taxonomy across all 5 fields
    primary_207y = case_when(
      grepl("^207Y", taxonomy_1) ~ taxonomy_1,
      grepl("^207Y", taxonomy_2) ~ taxonomy_2,
      grepl("^207Y", taxonomy_3) ~ taxonomy_3,
      grepl("^207Y", taxonomy_4) ~ taxonomy_4,
      grepl("^207Y", taxonomy_5) ~ taxonomy_5,
      TRUE ~ NA_character_
    ),
    # Does ANY secondary taxonomy suggest a subspecialty beyond the primary?
    any_subspecialty_207y = case_when(
      grepl("^207Y[^0]", taxonomy_1) ~ taxonomy_1,
      grepl("^207Y[^0]", taxonomy_2) ~ taxonomy_2,
      grepl("^207Y[^0]", taxonomy_3) ~ taxonomy_3,
      grepl("^207Y[^0]", taxonomy_4) ~ taxonomy_4,
      grepl("^207Y[^0]", taxonomy_5) ~ taxonomy_5,
      TRUE ~ NA_character_
    )
  )

cat(sprintf("  NPPES ENTs loaded: %s (%d sec)\n\n",
            format(nrow(nppes_ent), big.mark=","),
            round(as.numeric(difftime(Sys.time(), t0, units="secs")))))

# ── Taxonomy → specialty label map ─────────────────────────────────────────
TAX_MAP <- c(
  "207Y00000X" = "General Otolaryngology",
  "207YX0007X" = "Head and Neck Surgery",
  "207YX0905X" = "Neurotology",
  "207YP0228X" = "Pediatric Otolaryngology",
  "207YX0901X" = "Allergy",
  "207YX0602X" = "Facial Plastic and Reconstructive Surgery",
  "207YS0012X" = "Sleep Medicine",
  "207YS0123X" = "Sleep Medicine",   # alternate NUCC code
  "207YX0400X" = "Rhinology"
)

# ── Match enthealth physicians to NPPES (6-pass) ───────────────────────────
cat("=== STEP 2: Match all 7,399 enthealth physicians to NPPES ===\n")

df <- df |>
  mutate(
    last_clean    = toupper(trimws(last_name)),
    first_clean   = toupper(trimws(first_name)),
    first_initial = substr(first_clean, 1, 1),
    state_upper   = toupper(trimws(state_code)),
    last_stripped_enth = gsub("[^A-Za-z]", "", last_clean)
  )

nppes_by_state <- split(nppes_ent, nppes_ent$state_nppes)

n_rows <- nrow(df)
match_npi      <- rep(NA_character_, n_rows)
match_pass     <- rep(NA_integer_,   n_rows)
match_phone    <- rep(NA_character_, n_rows)
match_tax1     <- rep(NA_character_, n_rows)
match_tax2     <- rep(NA_character_, n_rows)
match_tax3     <- rep(NA_character_, n_rows)
match_tax4     <- rep(NA_character_, n_rows)
match_tax5     <- rep(NA_character_, n_rows)
match_primary207y     <- rep(NA_character_, n_rows)
match_any_subspec207y <- rep(NA_character_, n_rows)
match_org      <- rep(NA_character_, n_rows)
match_addr     <- rep(NA_character_, n_rows)
match_city     <- rep(NA_character_, n_rows)
match_sex      <- rep(NA_character_, n_rows)
match_cred     <- rep(NA_character_, n_rows)

.best <- function(cands, first) {
  if (nrow(cands) == 0L) return(cands)
  if (nrow(cands) == 1) return(cands)
  jw <- stringsim(first, cands$first_nppes, method="jw", p=0.1)
  if (length(jw) == 0L || all(is.na(jw))) return(cands[1L, , drop = FALSE])
  cands[which.max(jw), , drop=FALSE]
}

.record <- function(i, row, pass) {
  match_npi[i]           <<- row$npi[1]
  match_pass[i]          <<- pass
  match_phone[i]         <<- row$phone_nppes[1]
  match_tax1[i]          <<- row$taxonomy_1[1]
  match_tax2[i]          <<- row$taxonomy_2[1]
  match_tax3[i]          <<- row$taxonomy_3[1]
  match_tax4[i]          <<- row$taxonomy_4[1]
  match_tax5[i]          <<- row$taxonomy_5[1]
  match_primary207y[i]   <<- row$primary_207y[1]
  match_any_subspec207y[i] <<- row$any_subspecialty_207y[1]
  match_org[i]           <<- row$org_nppes[1]
  match_addr[i]          <<- row$addr_nppes[1]
  match_city[i]          <<- row$city_nppes[1]
  match_sex[i]           <<- row$sex_nppes[1]
  match_cred[i]          <<- row$credential_nppes[1]
}

for (i in seq_len(n_rows)) {
  pool <- nppes_by_state[[df$state_upper[i]]]
  if (is.null(pool) || nrow(pool) == 0) next

  # Pass 1: exact last + first_initial
  c1 <- pool[pool$last_nppes == df$last_clean[i] &
              pool$first_initial_nppes == df$first_initial[i], ]
  if (nrow(c1) > 0) { .record(i, .best(c1, df$first_clean[i]), 1L); next }

  # Pass 2: exact last, relax first initial
  c2 <- pool[pool$last_nppes == df$last_clean[i], ]
  if (nrow(c2) > 0) { .record(i, .best(c2, df$first_clean[i]), 2L); next }

  # Pass 3: strip hyphens/apostrophes/spaces, retry
  c3 <- pool[pool$last_stripped == df$last_stripped_enth[i] &
              pool$first_initial_nppes == df$first_initial[i], ]
  if (nrow(c3) > 0) { .record(i, .best(c3, df$first_clean[i]), 3L); next }

  # Pass 4: Levenshtein ≤ 1 on last + state + first initial
  dists <- stringdist(df$last_clean[i], pool$last_nppes, method="lv")
  c4 <- pool[dists <= 1 & pool$first_initial_nppes == df$first_initial[i], ]
  if (nrow(c4) > 0) { .record(i, .best(c4, df$first_clean[i]), 4L); next }
}

n_matched <- sum(!is.na(match_npi))
cat(sprintf("  Matched: %s / %s (%.1f%%)\n\n",
            format(n_matched, big.mark=","),
            format(n_rows, big.mark=","),
            100*n_matched/max(n_rows, 1L)))

# ── Assemble comparison table ───────────────────────────────────────────────
cat("=== STEP 3: Build comparison table ===\n")

comparison <- df |>
  mutate(
    npi_matched         = match_npi,
    match_pass          = match_pass,
    # Phone comparison
    phone_enthealth     = as.character(phone_digits),
    phone_nppes         = match_phone,
    phone_match         = case_when(
      is.na(phone_enthealth) | is.na(phone_nppes) ~ "one_missing",
      phone_enthealth == phone_nppes               ~ "exact_match",
      substr(phone_enthealth,1,6) == substr(phone_nppes,1,6) ~ "same_area_exchange",
      substr(phone_enthealth,1,3) == substr(phone_nppes,1,3) ~ "same_area_code",
      TRUE                                          ~ "different"
    ),
    # NPPES taxonomy (all 5 fields)
    nppes_taxonomy_1    = match_tax1,
    nppes_taxonomy_2    = match_tax2,
    nppes_taxonomy_3    = match_tax3,
    nppes_taxonomy_4    = match_tax4,
    nppes_taxonomy_5    = match_tax5,
    nppes_primary_207y  = match_primary207y,
    nppes_specialty     = TAX_MAP[match_primary207y],
    # Does any taxonomy field show a subspecialty?
    nppes_any_subspecialty_207y = match_any_subspec207y,
    nppes_subspecialty  = TAX_MAP[match_any_subspec207y],
    # Specialty agreement
    specialty_agreement = case_when(
      is.na(specialty_primary) | is.na(nppes_specialty)          ~ "one_missing",
      specialty_primary == nppes_specialty                        ~ "exact_match",
      # Check if any NPPES taxonomy matches the enthealth specialty
      specialty_primary == TAX_MAP[match_tax2] |
        specialty_primary == TAX_MAP[match_tax3] |
        specialty_primary == TAX_MAP[match_tax4] |
        specialty_primary == TAX_MAP[match_tax5]                 ~ "matched_secondary_taxonomy",
      # Both just "General"
      specialty_primary == "General Otolaryngology" &
        nppes_specialty == "General Otolaryngology"              ~ "exact_match",
      TRUE                                                        ~ "disagree"
    ),
    # NPPES demographics
    nppes_sex           = match_sex,
    nppes_credential    = match_cred,
    nppes_org           = match_org,
    nppes_addr          = match_addr,
    nppes_city          = match_city
  ) |>
  select(url, full_name, last_name, first_name, state_code, city, company_name, address,
         phone_enthealth, phone_nppes, phone_match, phone_source,
         specialty_primary, nppes_specialty, nppes_subspecialty, specialty_agreement,
         nppes_taxonomy_1, nppes_taxonomy_2, nppes_taxonomy_3, nppes_taxonomy_4, nppes_taxonomy_5,
         nppes_primary_207y, nppes_any_subspecialty_207y,
         degree_type, nppes_credential, nppes_sex,
         nppes_org, nppes_addr, nppes_city,
         npi_matched, match_pass)

cat("  Rows in comparison table:", nrow(comparison), "\n")
cat("  Matched to NPPES:        ", sum(!is.na(comparison$npi_matched)), "\n\n")

# ── Summary statistics ──────────────────────────────────────────────────────
matched <- comparison |> filter(!is.na(npi_matched))

cat("=== PHONE COMPARISON (among", nrow(matched), "matched) ===\n")
phone_both <- matched |> filter(!is.na(phone_enthealth) & !is.na(phone_nppes))
cat(sprintf("  Both phones available: %d (%.0f%% of matched)\n",
            nrow(phone_both), 100*nrow(phone_both)/max(nrow(matched), 1L)))
print(table(phone_both$phone_match))

cat("\n=== SPECIALTY AGREEMENT (among matched with both specialties) ===\n")
spec_both <- matched |> filter(!is.na(specialty_primary) & !is.na(nppes_specialty))
cat(sprintf("  Both specialties available: %d (%.0f%% of matched)\n",
            nrow(spec_both), 100*nrow(spec_both)/max(nrow(matched), 1L)))
print(table(spec_both$specialty_agreement))

cat("\n=== SPECIALTY CROSSTAB: enthealth (rows) vs NPPES primary (cols) ===\n")
print(table(enthealth=spec_both$specialty_primary,
            nppes=spec_both$nppes_specialty,
            useNA="ifany"))

cat("\n=== WHEN ENTHEALTH IS SUBSPECIALTY, WHAT DOES NPPES PRIMARY SHOW? ===\n")
spec_subspec <- spec_both |>
  filter(specialty_primary != "General Otolaryngology")
cat(sprintf("  Enthealth subspecialists: %d\n", nrow(spec_subspec)))
cat("  NPPES says 'General' for these:\n")
cat(sprintf("    %d (%.0f%%)\n",
            sum(spec_subspec$nppes_specialty == "General Otolaryngology", na.rm=TRUE),
            100*mean(spec_subspec$nppes_specialty == "General Otolaryngology", na.rm=TRUE)))
cat("  NPPES subspecialty (any field) agrees with enthealth:\n")
agree_any <- spec_subspec |>
  filter(!is.na(nppes_subspecialty) & specialty_primary == nppes_subspecialty)
cat(sprintf("    %d (%.0f%%)\n", nrow(agree_any),
            100*nrow(agree_any)/max(nrow(spec_subspec), 1L)))

cat("\n=== MATCH PASS DISTRIBUTION ===\n")
print(table(comparison$match_pass, useNA="ifany"))

# ── Save comparison xlsx ───────────────────────────────────────────────────
cat("\nSaving comparison table...\n")
out_path <- file.path(DROPBOX_DATA, "enthealth_nppes_comparison.xlsx")
wb <- createWorkbook()

# Sheet 1: Full comparison
addWorksheet(wb, "Comparison")
writeData(wb, "Comparison", comparison)
hs <- createStyle(fontColour="#FFFFFF", fgFill="#1F4E79",
                  halign="center", textDecoration="Bold", border="Bottom")
addStyle(wb, "Comparison", hs, rows=1, cols=seq_len(ncol(comparison)), gridExpand=TRUE)
freezePane(wb, "Comparison", firstRow=TRUE)
setColWidths(wb, "Comparison", cols=seq_len(ncol(comparison)), widths="auto")

# Sheet 2: Specialty disagreements
spec_disagree <- comparison |>
  filter(!is.na(npi_matched), specialty_agreement == "disagree") |>
  select(full_name, state_code, specialty_primary, nppes_specialty,
         nppes_subspecialty, nppes_taxonomy_1:nppes_taxonomy_5)
addWorksheet(wb, "Specialty_Disagree")
writeData(wb, "Specialty_Disagree", spec_disagree)
addStyle(wb, "Specialty_Disagree", hs, rows=1, cols=seq_len(ncol(spec_disagree)), gridExpand=TRUE)
freezePane(wb, "Specialty_Disagree", firstRow=TRUE)

# Sheet 3: Phone mismatches
phone_diff <- comparison |>
  filter(!is.na(npi_matched), phone_match %in% c("different","same_area_code")) |>
  select(full_name, state_code, phone_enthealth, phone_nppes, phone_match,
         company_name, nppes_org)
addWorksheet(wb, "Phone_Mismatch")
writeData(wb, "Phone_Mismatch", phone_diff)
addStyle(wb, "Phone_Mismatch", hs, rows=1, cols=seq_len(ncol(phone_diff)), gridExpand=TRUE)
freezePane(wb, "Phone_Mismatch", firstRow=TRUE)

tmp <- paste0(out_path, ".tmp")
saveWorkbook(wb, tmp, overwrite=TRUE)
file.rename(tmp, out_path)
cat(sprintf("  Saved: %s\n", out_path))

cat("\n=======================================================================\n")
cat("  COMPARISON COMPLETE\n")
cat(sprintf("  Matched to NPPES  : %d / %d (%.1f%%)\n",
            sum(!is.na(comparison$npi_matched)), nrow(comparison),
            100*mean(!is.na(comparison$npi_matched))))
cat(sprintf("  Vanity fixed      : (212) 744-4ENT -> (212) 744-4368\n"))
cat(sprintf("  Comparison file   : %s\n", out_path))
cat("=======================================================================\n")
