#!/usr/bin/env Rscript
# =============================================================================
# ent_table_1.R
# ENT Physician Characteristics — Table 1
#
# Produces publication-ready Table 1 for Otolaryngology (ENT) physicians
# from the enthealth.org matched NPI dataset.
#
# Outputs:
#   artifacts/ent/table1_otolaryngology__ent_.docx   (Word, gtsummary)
#   artifacts/ent/table1_otolaryngology__ent_.csv    (machine-readable)
#
# Variables:
#   Demographics:      Subspecialty, Gender
#   Training:          Degree (MD/DO), IMG status, Fellowship trained,
#                      Years since medical graduation, Trained in practice state
#   Geography:         AAO-HNS BOG Region, Practice location rurality (RUCA 2020)
#   Practice:          Practice type, Practice stability (5-yr), Underserved area,
#                      Practice size, Multiple practice locations, Telehealth participation
#   Clinical activity: Medicare volume quartile, Medicare assignment,
#                      Industry payments (Open Payments), Years since NPI enumeration
#
# Invocation:
#   Rscript scripts/ent_table_1.R
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(DBI)
  library(duckdb)
  library(gtsummary)
  library(flextable)
  library(officer)
})

source(here::here("R", "duckdb_safe.R"))

# =============================================================================
# SECTION 1: CONFIGURATION
# =============================================================================

NPI_CSV      <- here::here("data", "ent", "enthealth_npi_LATEST.csv")
PROFILE_CSV  <- here::here("data", "ent", "enthealth_profiles_LATEST.csv")
ENRICHED_CSV <- here::here("data", "ent", "enthealth_npi_enriched.csv")
OUT_DIR      <- here::here("artifacts", "ent")

DB_PATH <- Sys.getenv(
  "NBER_DB_PATH",
  "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb"
)
OP_DB_PATH <- Sys.getenv(
  "OP_DB_PATH",
  "/Volumes/MufflySamsung 1/open_payments_data/open_payments_merged_2013_2024.duckdb"
)

DAC_TABLES <- c(
  "doctors_and_clinicians_05_2024_DAC_NationalDownloadableFile_csv_4",
  "doctors_and_clinicians_12_2023_DAC_NationalDownloadableFile_csv_7",
  "doctors_and_clinicians_12_2022_DAC_NationalDownloadableFile_csv_6",
  "doctors_and_clinicians_12_2021_DAC_NationalDownloadableFile_csv_5"
)
# NPPES_TABLE is resolved dynamically after connection opens (see .pick_latest_nppes below)

GENERAL_TAXONOMY_CODE <- "207Y00000X"   # General Otolaryngology
SPECIALTY_LABEL       <- "Otolaryngology (ENT)"
SOURCE_LABEL          <- "enthealth.org (scraped Feb 2026)"
MEDICARE_REF_YEAR     <- 2022L

# =============================================================================
# SECTION 2: ENT SUBSPECIALTY LABELS
# =============================================================================

ENT_SUBSPEC_LABELS <- c(
  GEN_ENT      = "General Otolaryngology",
  FACIAL_PLAST = "Facial Plastic & Reconstructive Surgery",
  H_N_ONCO     = "Head & Neck Surgery / Oncology",
  PEDS_ENT     = "Pediatric Otolaryngology",
  OTOL         = "Otology / Neurotology",
  LARYN        = "Laryngology",
  RHINO        = "Rhinology",
  SLEEP        = "Sleep Medicine",
  OTHER_ENT    = "Other ENT"
)

# =============================================================================
# SECTION 3: AAO-HNS BOARD OF GOVERNORS REGION LOOKUP
# =============================================================================

BOG_LOOKUP <- tibble::tribble(
  ~state, ~bog_region,
  # Region 1 — New England
  "ME", "Region 1 – New England",
  "NH", "Region 1 – New England",
  "VT", "Region 1 – New England",
  "MA", "Region 1 – New England",
  "RI", "Region 1 – New England",
  "CT", "Region 1 – New England",
  # Region 2 — Mid-Atlantic
  "NY", "Region 2 – Mid-Atlantic",
  "NJ", "Region 2 – Mid-Atlantic",
  "PA", "Region 2 – Mid-Atlantic",
  # Region 3 — South Atlantic
  "DE", "Region 3 – South Atlantic",
  "MD", "Region 3 – South Atlantic",
  "DC", "Region 3 – South Atlantic",
  "VA", "Region 3 – South Atlantic",
  "WV", "Region 3 – South Atlantic",
  "NC", "Region 3 – South Atlantic",
  "SC", "Region 3 – South Atlantic",
  "GA", "Region 3 – South Atlantic",
  "FL", "Region 3 – South Atlantic",
  # Region 4 — Great Lakes
  "OH", "Region 4 – Great Lakes",
  "MI", "Region 4 – Great Lakes",
  "IN", "Region 4 – Great Lakes",
  "IL", "Region 4 – Great Lakes",
  "WI", "Region 4 – Great Lakes",
  # Region 5 — South Central
  "KY", "Region 5 – South Central",
  "TN", "Region 5 – South Central",
  "AL", "Region 5 – South Central",
  "MS", "Region 5 – South Central",
  "LA", "Region 5 – South Central",
  "AR", "Region 5 – South Central",
  # Region 6 — North Central
  "MN", "Region 6 – North Central",
  "IA", "Region 6 – North Central",
  "MO", "Region 6 – North Central",
  "ND", "Region 6 – North Central",
  "SD", "Region 6 – North Central",
  "NE", "Region 6 – North Central",
  "KS", "Region 6 – North Central",
  # Region 7 — Southwest
  "TX", "Region 7 – Southwest",
  "OK", "Region 7 – Southwest",
  "NM", "Region 7 – Southwest",
  # Region 8 — Pacific Coast
  "CA", "Region 8 – Pacific Coast",
  "OR", "Region 8 – Pacific Coast",
  "WA", "Region 8 – Pacific Coast",
  "AK", "Region 8 – Pacific Coast",
  "HI", "Region 8 – Pacific Coast",
  # Region 9 — Mountain West
  "CO", "Region 9 – Mountain West",
  "AZ", "Region 9 – Mountain West",
  "UT", "Region 9 – Mountain West",
  "NV", "Region 9 – Mountain West",
  "ID", "Region 9 – Mountain West",
  "MT", "Region 9 – Mountain West",
  "WY", "Region 9 – Mountain West"
)

BOG_LEVELS <- paste0("Region ", 1:9, " – ",
  c("New England", "Mid-Atlantic", "South Atlantic", "Great Lakes",
    "South Central", "North Central", "Southwest", "Pacific Coast",
    "Mountain West"))

# =============================================================================
# SECTION 4: HELPER FUNCTIONS
# =============================================================================

# ── 4a. IMG from NPPES credential text ────────────────────────────────────────
# Detects foreign primary-degree abbreviations (MBBS, ECFMG, FRCS, etc.)
# that appear in NPPES "Provider Credential Text" before we have a school name.
# Returns "IMG", "US Graduate", or NA (= no signal).

.FOREIGN_CRED_RE <- paste0(
  "\\bMBBS\\b|\\bMB\\b(?!\\s*S)|\\bBCh\\b|\\bBCH\\b|\\bMBBCh\\b|",
  "\\bMB\\s*BCh\\b|\\bMB\\s*BCH\\b|\\bMB\\s*BAO\\b|",
  "\\bECFMG\\b|",
  "\\bFRCS\\b|\\bFRCOG\\b|\\bFRACOG\\b|\\bFRACS\\b|",
  "\\bFRCSC\\b|\\bFRCSI\\b|\\bMRCS\\b|",
  "\\bLMCC\\b|\\bMDCM\\b"
)

classify_nppes_credential <- function(credential_text) {
  cred <- ifelse(is.na(credential_text), "", trimws(credential_text))
  dplyr::case_when(
    grepl(.FOREIGN_CRED_RE, cred, perl = TRUE, ignore.case = TRUE) ~ "IMG",
    nzchar(cred)                                                    ~ "US Graduate",
    TRUE                                                            ~ NA_character_
  )
}

# ── 4b. IMG from medical school name ─────────────────────────────────────────
# Uses the production-grade school classifier if available;
# falls back to credential-only classification if the file is missing.

.classify_school_img <- function(school_names) {
  aamc_path <- here::here("R", "classify_medical_school_aamc.R")
  if (file.exists(aamc_path) &&
      !exists("classify_medical_school", mode = "function")) {
    suppressMessages(suppressWarnings(source(aamc_path, local = FALSE)))
  }
  if (!exists("classify_medical_school", mode = "function")) {
    return(rep(NA_character_, length(school_names)))
  }
  cat_code <- classify_medical_school(school_names)
  dplyr::case_when(
    cat_code %in% c("US_MD", "US_DO", "CAN_MD") ~ "US Graduate",
    cat_code == "IMG"                             ~ "IMG",
    TRUE                                          ~ NA_character_
  )
}

# ── 4c. Residency state extractor (for trained_in_practice_state) ─────────────
# Foreign-country strings map to the sentinel "Foreign", which maps to "No".
# US institution names are matched to 2-letter state abbreviations.

.FOREIGN_RESIDENCY_RE <- paste0(
  "\\bisrael\\b|\\bindia\\b|\\bpakistan\\b|\\bnigeria\\b|",
  "\\bphilippines\\b|\\bmexico\\b|\\bchina\\b|\\begypt\\b|",
  "\\beireland\\b|\\bcanada\\b|\\baustralia\\b|",
  "\\bgermany\\b|\\bfrance\\b|\\bitalian\\b|\\bbrazil\\b|",
  "\\bcaribbean\\b|\\bgrenada\\b|\\bdominica\\b|\\bsaint lucia\\b|",
  "ross university|american university.*caribbean|saba university|",
  "hebrew university|tel aviv|weizmann|rabin medical|",
  "aga khan|manipal|armed forces medical"
)

.residency_state_single <- function(name) {
  if (is.na(name) || !nzchar(trimws(name))) return(NA_character_)
  n <- tolower(name)

  if (grepl(.FOREIGN_RESIDENCY_RE, n, perl = TRUE, ignore.case = TRUE))
    return("Foreign")

  multi <- c(
    "new york" = "NY", "new jersey" = "NJ", "new mexico" = "NM",
    "new hampshire" = "NH", "north carolina" = "NC", "north dakota" = "ND",
    "south carolina" = "SC", "south dakota" = "SD", "west virginia" = "WV",
    "rhode island" = "RI", "district of columbia" = "DC"
  )
  for (pat in names(multi))
    if (grepl(pat, n, fixed = TRUE)) return(unname(multi[pat]))

  city_map <- c(
    "albany" = "NY", "manhattan" = "NY", "brooklyn" = "NY", "bronx" = "NY",
    "mount sinai" = "NY", "montefiore" = "NY", "icahn" = "NY",
    "suny" = "NY", "nyu" = "NY", "cornell" = "NY", "stony brook" = "NY",
    "newark" = "NJ", "rutgers" = "NJ", "hackensack" = "NJ",
    "boston" = "MA", "harvard" = "MA", "tufts" = "MA", "brigham" = "MA",
    "johns hopkins" = "MD", "baltimore" = "MD",
    "george washington" = "DC", "georgetown" = "DC", "howard university" = "DC",
    "philadelphia" = "PA", "drexel" = "PA", "jefferson" = "PA",
    "pittsburgh" = "PA", "penn state" = "PA",
    "henry ford" = "MI", "wayne state" = "MI", "detroit" = "MI",
    "cleveland" = "OH", "case western" = "OH", "ohio state" = "OH",
    "vanderbilt" = "TN", "nashville" = "TN",
    "emory" = "GA", "atlanta" = "GA",
    "duke" = "NC", "wake forest" = "NC",
    "tulane" = "LA", "lsu" = "LA",
    "northwestern" = "IL", "rush" = "IL", "chicago" = "IL",
    "stanford" = "CA", "ucsf" = "CA", "ucla" = "CA",
    "mayo" = "MN", "minneapolis" = "MN",
    "baylor" = "TX", "houston" = "TX", "dallas" = "TX",
    "university of washington" = "WA", "seattle" = "WA",
    "ohsu" = "OR", "portland" = "OR",
    "university of utah" = "UT", "salt lake" = "UT",
    "university of colorado" = "CO", "denver" = "CO",
    "university of miami" = "FL", "miami" = "FL",
    "university of arizona" = "AZ", "phoenix" = "AZ"
  )
  for (pat in names(city_map))
    if (grepl(pat, n, fixed = TRUE)) return(unname(city_map[pat]))

  states <- c(
    "alabama"="AL","alaska"="AK","arizona"="AZ","arkansas"="AR",
    "california"="CA","colorado"="CO","connecticut"="CT","delaware"="DE",
    "florida"="FL","georgia"="GA","hawaii"="HI","idaho"="ID",
    "illinois"="IL","indiana"="IN","iowa"="IA","kansas"="KS",
    "kentucky"="KY","louisiana"="LA","maine"="ME","maryland"="MD",
    "massachusetts"="MA","michigan"="MI","minnesota"="MN","mississippi"="MS",
    "missouri"="MO","montana"="MT","nebraska"="NE","nevada"="NV",
    "ohio"="OH","oklahoma"="OK","oregon"="OR","pennsylvania"="PA",
    "tennessee"="TN","texas"="TX","utah"="UT","vermont"="VT",
    "virginia"="VA","washington"="WA","wisconsin"="WI","wyoming"="WY"
  )
  for (st in names(states))
    if (grepl(paste0("\\b", st, "\\b"), n, perl = TRUE))
      return(unname(states[st]))

  NA_character_
}

.residency_state <- function(names)
  vapply(names, .residency_state_single, character(1), USE.NAMES = FALSE)

# ── 4d. Practice type heuristic ──────────────────────────────────────────────
.ACADEMIC_RE <- paste0(
  "university|medical school|college|academic|",
  "\\bhospital\\b|health system|medical center|\\bclinic\\b|",
  "children|pediatric|\\bVA\\b|veterans|kaiser|mayo|",
  "johns hopkins|mount sinai|NYU|harvard|stanford|yale"
)

# ── 4e. Coverage gate ─────────────────────────────────────────────────────────
.pct_informative <- function(x) {
  if (is.factor(x)) {
    100 * mean(!is.na(x) &
               as.character(x) != "Unknown" &
               as.character(x) != "No Medicare",
               na.rm = FALSE)
  } else {
    100 * mean(!is.na(x))
  }
}

# =============================================================================
# SECTION 5: LOAD ENT COHORT
# =============================================================================

if (!file.exists(NPI_CSV))
  stop("[ENT Table 1] NPI CSV not found: ", NPI_CSV)

cohort <- readr::read_csv(NPI_CSV, show_col_types = FALSE) |>
  dplyr::mutate(npi = as.character(npi))

cat(sprintf("[ENT Table 1] %d rows loaded, %d with matched NPI (%.1f%%)\n",
            nrow(cohort),
            sum(!is.na(cohort$npi)),
            100 * mean(!is.na(cohort$npi))))

cohort <- dplyr::filter(cohort, !is.na(npi))

# =============================================================================
# SECTION 6: SUBSPECIALTY FACTOR
# =============================================================================

if (!"ent_subspecialty" %in% names(cohort))
  stop("[ENT Table 1] Column 'ent_subspecialty' not found in CSV")

cohort <- cohort |>
  dplyr::mutate(
    subspecialty = dplyr::recode(
      ent_subspecialty,
      !!!ENT_SUBSPEC_LABELS,
      .default = "Other"
    ),
    subspecialty = factor(
      subspecialty,
      levels = c(unname(ENT_SUBSPEC_LABELS),
                 setdiff("Other", unname(ENT_SUBSPEC_LABELS)))
    )
  )

cat(sprintf("[ENT Table 1] Subspecialty distribution:\n"))
print(table(cohort$subspecialty, useNA = "ifany"))

# =============================================================================
# SECTION 7: GEOGRAPHIC ENRICHMENT (BOG REGION, PRACTICE TYPE)
# =============================================================================

cohort <- cohort |>
  dplyr::left_join(BOG_LOOKUP, by = "state") |>
  dplyr::mutate(
    bog_region = factor(bog_region, levels = BOG_LEVELS),
    practice_type = dplyr::case_when(
      grepl(.ACADEMIC_RE, company_name, ignore.case = TRUE) ~ "Academic / Hospital",
      !is.na(company_name) & nchar(trimws(company_name)) > 0 ~ "Private Practice",
      TRUE ~ NA_character_
    ),
    practice_type = factor(
      practice_type,
      levels = c("Academic / Hospital", "Private Practice")
    )
  )

# =============================================================================
# SECTION 8: PROFILE DATA JOIN
# (residency, medical school, fellowship from enthealth scrape)
# =============================================================================

profile_path <- if (file.exists(ENRICHED_CSV)) ENRICHED_CSV else
                if (file.exists(PROFILE_CSV))  PROFILE_CSV  else NULL

if (!is.null(profile_path)) {
  prof_cols <- c("enthealth_id", "residency_name", "residency_year",
                 "medical_school_name", "medical_school_year",
                 "fellowship_name", "fellowship_year")
  prof <- tryCatch(
    readr::read_csv(profile_path, show_col_types = FALSE,
                    col_select = dplyr::any_of(prof_cols)),
    error = function(e) { warning("[ENT Table 1] Profile read failed: ", e$message); NULL }
  )
  if (!is.null(prof) && "enthealth_id" %in% names(cohort) &&
      "enthealth_id" %in% names(prof)) {
    cohort <- dplyr::left_join(cohort,
                               dplyr::select(prof, dplyr::any_of(prof_cols)),
                               by = "enthealth_id")
    cat(sprintf("[ENT Table 1] Profile join: %d/%d records enriched\n",
                sum(!is.na(cohort$residency_name)), nrow(cohort)))
  }
} else {
  cat("[ENT Table 1] No profile CSV found — residency/school fields will be NA\n")
  cohort <- cohort |>
    dplyr::mutate(
      residency_name      = NA_character_,
      residency_year      = NA_integer_,
      medical_school_name = NA_character_,
      medical_school_year = NA_integer_,
      fellowship_name     = NA_character_,
      fellowship_year     = NA_integer_
    )
}

# ── Derived training variables from profile ──────────────────────────────────

current_year <- as.integer(format(Sys.Date(), "%Y"))

cohort <- cohort |>
  dplyr::mutate(
    years_since_grad = dplyr::if_else(
      !is.na(medical_school_year) &
        (current_year - as.integer(medical_school_year)) >= 0 &
        (current_year - as.integer(medical_school_year)) <= 60,
      as.numeric(current_year - as.integer(medical_school_year)),
      NA_real_
    ),
    residency_state = .residency_state(residency_name),
    trained_in_practice_state = dplyr::case_when(
      is.na(residency_state) | is.na(state) ~ NA_character_,
      residency_state == "Foreign"           ~ "No",
      residency_state == state               ~ "Yes",
      TRUE                                   ~ "No"
    ),
    trained_in_practice_state = factor(
      trained_in_practice_state, levels = c("Yes", "No")
    )
  )

# =============================================================================
# SECTION 9: DUCKDB ENRICHMENT
# (NPPES gender/degree/fellowship/IMG, RUCA rurality,
#  practice stability, Medicare volume)
# =============================================================================

npi_str <- paste(sprintf("'%s'", cohort$npi), collapse = ",")
zip_str <- paste(sprintf("'%s'", unique(cohort$zip5[!is.na(cohort$zip5)])),
                 collapse = ",")

if (!file.exists(DB_PATH)) {
  warning("[ENT Table 1] DuckDB not found at ", DB_PATH,
          " — NPPES/RUCA enrichments skipped")
  cohort <- cohort |>
    dplyr::mutate(
      gender                   = factor(NA_character_, levels = c("Male","Female","Unknown")),
      degree_type              = factor(NA_character_, levels = c("MD","DO","Unknown")),
      years_since_enum         = NA_real_,
      fellowship_trained       = factor(NA_character_, levels = c("Yes","No")),
      img_status               = factor(NA_character_, levels = c("US Graduate","IMG")),
      ruca_category            = factor(NA_character_,
                                   levels = c("Metropolitan","Micropolitan","Small Town","Rural")),
      practice_stability       = factor(NA_character_,
                                   levels = c("Stable (≥5y)","1 move","Multiple moves","Unknown")),
      years_at_current_site    = NA_real_,
      medicare_volume_quartile = factor(NA_character_,
                                   levels = c("Q1 (Lowest)","Q2","Q3","Q4 (Highest)","No Medicare")),
      underserved_area         = factor(NA_character_,
                                   levels = c("HPSA / MUA","Not Underserved","Unknown")),
      telehealth_participation = factor(NA_character_, levels = c("Yes","No")),
      practice_size_category   = factor(NA_character_,
                                   levels = c("Solo","Small (2–5)","Medium (6–25)","Large (>25)")),
      multi_location_practice  = factor(NA_character_, levels = c("Yes","No")),
      medicare_assignment      = factor(NA_character_,
                                   levels = c("Participates","Opt-out / Unknown")),
      industry_payments        = factor(NA_character_, levels = c("Yes","No"))
    )
} else {
  con <- safe_duckdb(DB_PATH, read_only = TRUE)
  on.exit({try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

  NPPES_TABLE <- {
    tbls <- DBI::dbListTables(con)
    nppes_candidates <- sort(grep("^NPPES_Data_Dissemination.*npidata_pfile", tbls, value = TRUE), decreasing = TRUE)
    if (length(nppes_candidates) == 0L) stop("[ent_table_1] No NPPES table found in DuckDB")
    nppes_candidates[[1L]]
  }

  # ── 9a. NPPES: gender, degree, fellowship, IMG, years since enumeration ──────

  tax_cols <- paste(
    sprintf('"Healthcare Provider Taxonomy Code_%d" AS taxonomy_%d', 1:5, 1:5),
    collapse = ",\n       "
  )
  nppes_q <- sprintf(
    'SELECT
       NPI,
       "Provider Sex Code"              AS sex_code,
       "Provider Credential Text"       AS credential,
       "Provider Other Credential Text" AS other_credential,
       "Provider Enumeration Date"      AS enum_date,
       %s
     FROM "%s"
     WHERE NPI IN (%s)',
    tax_cols, NPPES_TABLE, npi_str
  )
  nppes_raw <- tryCatch(
    DBI::dbGetQuery(con, nppes_q),
    error = function(e) {
      warning("[ENT Table 1] NPPES query failed: ", e$message)
      data.frame(
        NPI = character(0), sex_code = character(0),
        credential = character(0), other_credential = character(0),
        enum_date = character(0),
        taxonomy_1 = character(0), taxonomy_2 = character(0),
        taxonomy_3 = character(0), taxonomy_4 = character(0),
        taxonomy_5 = character(0)
      )
    }
  )
  cat(sprintf("[ENT Table 1] NPPES: %d / %d NPIs matched\n",
              nrow(nppes_raw), nrow(cohort)))

  specialty_pattern <- sub("00000X$", "", GENERAL_TAXONOMY_CODE)  # "207Y"

  is_fellowship_tax <- function(tax) {
    !is.na(tax) & nzchar(tax) &
      grepl(specialty_pattern, tax, fixed = FALSE) &
      tax != GENERAL_TAXONOMY_CODE
  }

  nppes_demo <- nppes_raw |>
    dplyr::rename(npi = NPI) |>
    dplyr::mutate(
      npi = as.character(npi),

      gender = factor(
        dplyr::case_when(
          sex_code == "M" ~ "Male",
          sex_code == "F" ~ "Female",
          TRUE            ~ "Unknown"
        ),
        levels = c("Male", "Female", "Unknown")
      ),

      degree_type = factor(
        dplyr::case_when(
          grepl("\\bDO\\b",       credential, ignore.case = TRUE) ~ "DO",
          grepl("\\bMD\\b|M\\.D\\.", credential, ignore.case = TRUE) ~ "MD",
          !is.na(credential) & nchar(trimws(credential)) > 0     ~ "MD",
          TRUE ~ "Unknown"
        ),
        levels = c("MD", "DO", "Unknown")
      ),

      enum_year = suppressWarnings(
        as.integer(format(as.Date(enum_date, "%m/%d/%Y"), "%Y"))
      ),
      years_since_enum = dplyr::if_else(
        (current_year - enum_year) >= 0 & (current_year - enum_year) <= 50,
        as.numeric(current_year - enum_year),
        NA_real_
      ),

      fellowship_trained = factor(
        tidyr::replace_na(
          dplyr::if_else(
            is_fellowship_tax(taxonomy_1) | is_fellowship_tax(taxonomy_2) |
              is_fellowship_tax(taxonomy_3) | is_fellowship_tax(taxonomy_4) |
              is_fellowship_tax(taxonomy_5),
            "Yes", "No"
          ),
          "No"
        ),
        levels = c("Yes", "No")
      ),

      # IMG from NPPES credential text (MBBS, ECFMG, FRCS, etc.)
      all_cred = trimws(paste(
        dplyr::coalesce(credential, ""),
        dplyr::coalesce(other_credential, "")
      )),
      img_status = factor(
        classify_nppes_credential(all_cred),
        levels = c("US Graduate", "IMG")
      )
    ) |>
    dplyr::select(npi, gender, degree_type, years_since_enum,
                  fellowship_trained, img_status)

  # ── 9b. RUCA 2020 zip-level rurality ─────────────────────────────────────────

  ruca_raw <- if (nchar(zip_str) == 0L) {
    data.frame(zip = character(0), ruca_primary = integer(0))
  } else {
    tryCatch(
      DBI::dbGetQuery(con, sprintf(
        "SELECT zip, ruca_primary FROM ruca_2020_zip WHERE zip IN (%s)", zip_str
      )),
      error = function(e) {
        warning("[ENT Table 1] RUCA query failed: ", e$message)
        data.frame(zip = character(0), ruca_primary = integer(0))
      }
    )
  }

  ruca_tbl <- ruca_raw |>
    dplyr::distinct(zip, .keep_all = TRUE) |>
    dplyr::mutate(
      ruca_category = factor(
        dplyr::case_when(
          ruca_primary %in%  1:3  ~ "Metropolitan",
          ruca_primary %in%  4:6  ~ "Micropolitan",
          ruca_primary %in%  7:9  ~ "Small Town",
          ruca_primary %in% 10:99 ~ "Rural",
          TRUE                    ~ NA_character_
        ),
        levels = c("Metropolitan", "Micropolitan", "Small Town", "Rural")
      )
    ) |>
    dplyr::rename(zip5 = zip)

  # ── 9c. Join NPPES + RUCA ────────────────────────────────────────────────────

  cohort <- cohort |>
    dplyr::left_join(nppes_demo,
                     by = "npi") |>
    dplyr::left_join(dplyr::select(ruca_tbl, zip5, ruca_category),
                     by = "zip5")

  # Upgrade img_status with school-name classifier once profile data are joined
  if ("medical_school_name" %in% names(cohort)) {
    school_img <- .classify_school_img(cohort$medical_school_name)
    cohort <- cohort |>
      dplyr::mutate(
        img_status = factor(
          dplyr::coalesce(school_img, as.character(img_status)),
          levels = c("US Graduate", "IMG")
        )
      )
    cat(sprintf("[ENT Table 1] IMG classification: %d IMG, %d US Graduate, %d unknown\n",
                sum(as.character(cohort$img_status) == "IMG",    na.rm = TRUE),
                sum(as.character(cohort$img_status) == "US Graduate", na.rm = TRUE),
                sum(is.na(cohort$img_status))))
  }

  # ── 9d. Practice stability (temporal NPPES 2019–2024) ─────────────────────────

  tables <- tryCatch(DBI::dbListTables(con), error = function(e) character(0))

  if ("temporal_all_years_fixed" %in% tables) {
    stab_q <- sprintf("
      WITH addr AS (
        SELECT CAST(npi AS VARCHAR) AS npi,
               year,
               CONCAT(
                 COALESCE(practice_address_street,''), '|',
                 COALESCE(practice_address_state,''),  '|',
                 LEFT(COALESCE(practice_address_zip,''), 5)
               ) AS addr_key
        FROM temporal_all_years_fixed
        WHERE npi IN (%s)
          AND year >= 2019
          AND practice_address_street IS NOT NULL
      )
      SELECT npi,
             COUNT(DISTINCT addr_key) AS n_locs,
             COUNT(DISTINCT year)     AS n_yrs
      FROM addr GROUP BY npi", npi_str)

    stab_raw <- tryCatch(DBI::dbGetQuery(con, stab_q),
                         error = function(e) NULL)
    if (!is.null(stab_raw) && nrow(stab_raw) > 0) {
      stab <- stab_raw |>
        dplyr::mutate(
          npi = as.character(npi),
          practice_stability = factor(
            dplyr::case_when(
              n_locs == 1 & n_yrs >= 5 ~ "Stable (≥5y)",
              n_locs == 2               ~ "1 move",
              n_locs >= 3               ~ "Multiple moves",
              TRUE                      ~ "Unknown"
            ),
            levels = c("Stable (≥5y)", "1 move", "Multiple moves", "Unknown")
          ),
          years_at_current_site = dplyr::if_else(
            n_locs == 1, as.numeric(n_yrs), NA_real_
          )
        ) |>
        dplyr::select(npi, practice_stability, years_at_current_site)
      cohort <- dplyr::left_join(cohort, stab, by = "npi")
    } else {
      cohort <- dplyr::mutate(cohort,
        practice_stability    = factor(NA_character_,
          levels = c("Stable (≥5y)", "1 move", "Multiple moves", "Unknown")),
        years_at_current_site = NA_real_)
    }
  } else {
    cat("[ENT Table 1] temporal_all_years_fixed not found — practice_stability skipped\n")
    cohort <- dplyr::mutate(cohort,
      practice_stability    = factor(NA_character_,
        levels = c("Stable (≥5y)", "1 move", "Multiple moves", "Unknown")),
      years_at_current_site = NA_real_)
  }

  # ── 9e. Medicare Part B volume quartile (2022) ────────────────────────────────

  medicare_tbl <- if ("temporal_medicare_part_b_all_years" %in% tables) {
    "temporal_medicare_part_b_all_years"
  } else {
    yr_match <- grep(paste0("medicare.*", MEDICARE_REF_YEAR),
                     tables, ignore.case = TRUE, value = TRUE)
    if (length(yr_match)) yr_match[1] else NULL
  }

  if (!is.null(medicare_tbl)) {
    yr_filter <- if (grepl("all_years", medicare_tbl))
                   sprintf("AND billing_year = %d", MEDICARE_REF_YEAR) else ""
    med_q <- sprintf("
      SELECT CAST(npi AS VARCHAR) AS npi,
             SUM(n_services) AS total_services
      FROM \"%s\"
      WHERE CAST(npi AS VARCHAR) IN (%s) %s
      GROUP BY npi", medicare_tbl, npi_str, yr_filter)

    med_raw <- tryCatch(DBI::dbGetQuery(con, med_q),
                        error = function(e) NULL)
    if (!is.null(med_raw) && nrow(med_raw) > 0) {
      cuts <- quantile(med_raw$total_services, c(0.25, 0.5, 0.75), na.rm = TRUE)
      vol <- med_raw |>
        dplyr::mutate(
          npi = as.character(npi),
          medicare_volume_quartile = factor(
            dplyr::case_when(
              total_services <= cuts[1] ~ "Q1 (Lowest)",
              total_services <= cuts[2] ~ "Q2",
              total_services <= cuts[3] ~ "Q3",
              TRUE                      ~ "Q4 (Highest)"
            ),
            levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare")
          )
        ) |>
        dplyr::select(npi, medicare_volume_quartile)
      cohort <- dplyr::left_join(cohort, vol, by = "npi") |>
        dplyr::mutate(
          medicare_volume_quartile = dplyr::if_else(
            is.na(medicare_volume_quartile),
            factor("No Medicare",
                   levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare")),
            medicare_volume_quartile
          )
        )
    } else {
      cohort <- dplyr::mutate(cohort,
        medicare_volume_quartile = factor(NA_character_,
          levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare")))
    }
  } else {
    cat("[ENT Table 1] Medicare Part B table not found — quartile skipped\n")
    cohort <- dplyr::mutate(cohort,
      medicare_volume_quartile = factor(NA_character_,
        levels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)", "No Medicare")))
  }

  # ── 9f. DAC: telehealth, practice size, multi-location, Medicare assignment ──

  dac_tbl_name <- NULL
  for (tbl in DAC_TABLES) {
    if (tbl %in% tables) { dac_tbl_name <- tbl; break }
  }

  if (!is.null(dac_tbl_name)) {
    dac_q <- sprintf(
      'SELECT CAST(NPI AS VARCHAR) AS npi,
              MAX(CASE WHEN "Telehlth" = \'Y\' THEN 1 ELSE 0 END)          AS any_telehealth,
              MAX(COALESCE(CAST("num_org_mem" AS INTEGER), 1))              AS max_org_size,
              COUNT(DISTINCT CONCAT(
                COALESCE(adr_ln_1, \'\'), \'|\',
                COALESCE("City/Town", \'\'), \'|\',
                LEFT(COALESCE("ZIP Code", \'\'), 5)
              ))                                                             AS n_locations,
              MAX(CASE WHEN ind_assgn = \'Y\' OR grp_assgn = \'Y\'
                       THEN 1 ELSE 0 END)                                  AS accepts_assignment
       FROM "%s"
       WHERE CAST(NPI AS VARCHAR) IN (%s)
       GROUP BY NPI',
      dac_tbl_name, npi_str
    )
    dac_raw <- tryCatch(DBI::dbGetQuery(con, dac_q),
                        error = function(e) {
                          warning("[ENT Table 1] DAC query failed: ", e$message)
                          NULL
                        })
    if (!is.null(dac_raw) && nrow(dac_raw) > 0) {
      dac_enrich <- dac_raw |>
        dplyr::mutate(
          npi = as.character(npi),
          telehealth_participation = factor(
            dplyr::if_else(any_telehealth == 1L, "Yes", "No"),
            levels = c("Yes", "No")
          ),
          practice_size_category = factor(
            dplyr::case_when(
              max_org_size == 1  ~ "Solo",
              max_org_size <= 5  ~ "Small (2–5)",
              max_org_size <= 25 ~ "Medium (6–25)",
              TRUE               ~ "Large (>25)"
            ),
            levels = c("Solo", "Small (2–5)", "Medium (6–25)", "Large (>25)")
          ),
          multi_location_practice = factor(
            dplyr::if_else(n_locations > 1L, "Yes", "No"),
            levels = c("Yes", "No")
          ),
          medicare_assignment = factor(
            dplyr::if_else(accepts_assignment == 1L, "Participates", "Opt-out / Unknown"),
            levels = c("Participates", "Opt-out / Unknown")
          )
        ) |>
        dplyr::select(npi, telehealth_participation, practice_size_category,
                      multi_location_practice, medicare_assignment)
      cohort <- dplyr::left_join(cohort, dac_enrich, by = "npi")
      cat(sprintf(
        "[ENT Table 1] DAC (%s): telehealth=%.0f%%, size=%.0f%%, multi-loc=%.0f%%, assignment=%.0f%%\n",
        dac_tbl_name,
        100 * mean(!is.na(cohort$telehealth_participation)),
        100 * mean(!is.na(cohort$practice_size_category)),
        100 * mean(!is.na(cohort$multi_location_practice)),
        100 * mean(!is.na(cohort$medicare_assignment))
      ))
    } else {
      cohort <- cohort |> dplyr::mutate(
        telehealth_participation = factor(NA_character_, levels = c("Yes", "No")),
        practice_size_category   = factor(NA_character_,
          levels = c("Solo", "Small (2–5)", "Medium (6–25)", "Large (>25)")),
        multi_location_practice  = factor(NA_character_, levels = c("Yes", "No")),
        medicare_assignment      = factor(NA_character_,
          levels = c("Participates", "Opt-out / Unknown"))
      )
    }
  } else {
    cat("[ENT Table 1] No DAC table found — telehealth/size/multi-loc/assignment skipped\n")
    cohort <- cohort |> dplyr::mutate(
      telehealth_participation = factor(NA_character_, levels = c("Yes", "No")),
      practice_size_category   = factor(NA_character_,
        levels = c("Solo", "Small (2–5)", "Medium (6–25)", "Large (>25)")),
      multi_location_practice  = factor(NA_character_, levels = c("Yes", "No")),
      medicare_assignment      = factor(NA_character_,
        levels = c("Participates", "Opt-out / Unknown"))
    )
  }
}

# ── 9g. Open Payments: industry payments (separate DuckDB, non-fatal) ─────────

if (file.exists(OP_DB_PATH)) {
  op_con <- tryCatch(
    safe_duckdb(OP_DB_PATH, read_only = TRUE),
    error = function(e) {
      warning("[ENT Table 1] Open Payments DB connect failed: ", e$message)
      NULL
    }
  )
  if (!is.null(op_con)) {
    on.exit(try(DBI::dbDisconnect(op_con, shutdown = TRUE), silent = TRUE), add = TRUE)
    op_q <- sprintf(
      'SELECT CAST("Covered_Recipient_NPI" AS VARCHAR) AS npi,
              COUNT(*)                                  AS n_payments,
              SUM("Total_Amount_of_Payment_USDollars")  AS total_usd
       FROM "open_payments_merged"
       WHERE "Covered_Recipient_NPI" IS NOT NULL
         AND CAST("Covered_Recipient_NPI" AS VARCHAR) IN (%s)
       GROUP BY "Covered_Recipient_NPI"',
      npi_str
    )
    op_raw <- tryCatch(DBI::dbGetQuery(op_con, op_q),
                       error = function(e) {
                         warning("[ENT Table 1] Open Payments query failed: ", e$message)
                         NULL
                       })
    if (!is.null(op_raw) && nrow(op_raw) > 0) {
      op_enrich <- op_raw |>
        dplyr::mutate(
          npi = as.character(npi),
          industry_payments = factor("Yes", levels = c("Yes", "No"))
        ) |>
        dplyr::select(npi, industry_payments)
      cohort <- dplyr::left_join(cohort, op_enrich, by = "npi") |>
        dplyr::mutate(
          industry_payments = dplyr::if_else(
            is.na(industry_payments),
            factor("No", levels = c("Yes", "No")),
            industry_payments
          )
        )
      cat(sprintf("[ENT Table 1] Open Payments: %d/%d physicians received industry payments\n",
                  sum(as.character(cohort$industry_payments) == "Yes", na.rm = TRUE),
                  nrow(cohort)))
    } else {
      cohort <- dplyr::mutate(cohort,
        industry_payments = factor(NA_character_, levels = c("Yes", "No")))
    }
  } else {
    cohort <- dplyr::mutate(cohort,
      industry_payments = factor(NA_character_, levels = c("Yes", "No")))
  }
} else {
  cat("[ENT Table 1] Open Payments DB not found — industry_payments skipped\n")
  cohort <- dplyr::mutate(cohort,
    industry_payments = factor(NA_character_, levels = c("Yes", "No")))
}

# ── 9h. Underserved area (HPSA/MUA spatial join — outside main connection) ───

underserved_path <- here::here("R", "enrich_table1_demographics.R")
if (file.exists(underserved_path) &&
    !exists("add_underserved_area", mode = "function")) {
  tryCatch(source(underserved_path, local = FALSE), error = function(e) NULL)
}

if (exists("add_underserved_area", mode = "function")) {
  cohort <- tryCatch(
    add_underserved_area(cohort, DB_PATH),
    error = function(e) {
      warning("[ENT Table 1] underserved_area failed (non-fatal): ", e$message)
      dplyr::mutate(cohort,
        underserved_area = factor(NA_character_,
          levels = c("HPSA / MUA", "Not Underserved", "Unknown")))
    }
  )
} else {
  cohort <- dplyr::mutate(cohort,
    underserved_area = factor(NA_character_,
      levels = c("HPSA / MUA", "Not Underserved", "Unknown")))
}

# =============================================================================
# SECTION 10: COVERAGE-GATED VARIABLE REGISTRY
# =============================================================================
# Variables only enter the table when they (a) exist as a column and
# (b) meet the minimum % informative threshold below.

var_registry <- list(
  # list(display_label, column_name, min_pct_informative, section)
  list("Subspecialty",                        "subspecialty",               0,  "Demographics"),
  list("Gender",                              "gender",                    80,  "Demographics"),
  list("Degree (MD/DO)",                      "degree_type",               50,  "Training"),
  list("International Medical Graduate",      "img_status",                20,  "Training"),
  list("Fellowship Trained",                  "fellowship_trained",        20,  "Training"),
  list("Years Since Medical Graduation",      "years_since_grad",          40,  "Training"),
  list("Trained in Practice State",           "trained_in_practice_state", 20,  "Training"),
  list("AAO-HNS BOG Region",                  "bog_region",                80,  "Geography"),
  list("Practice Location Rurality (RUCA)",   "ruca_category",             80,  "Geography"),
  list("Practice Type",                       "practice_type",             20,  "Practice"),
  list("Practice Stability (5-Year)",         "practice_stability",        20,  "Practice"),
  list("Medicare Volume Quartile (2022)",     "medicare_volume_quartile",  20,  "Clinical Activity"),
  list("Underserved Area Practice",           "underserved_area",          20,  "Practice"),
  list("Years Since NPI Enumeration",         "years_since_enum",          50,  "Clinical Activity"),
  list("Telehealth Participation",            "telehealth_participation",  20,  "Practice"),
  list("Practice Size",                       "practice_size_category",    20,  "Practice"),
  list("Multiple Practice Locations",         "multi_location_practice",   20,  "Practice"),
  list("Medicare Assignment",                 "medicare_assignment",        20,  "Clinical Activity"),
  list("Received Industry Payments",          "industry_payments",          20,  "Clinical Activity")
)

continuous_vars <- c("years_since_enum", "years_at_current_site", "years_since_grad")

included_vars   <- character(0)
included_labels <- list()

for (vr in var_registry) {
  lbl <- vr[[1]]; col <- vr[[2]]; min_pct <- vr[[3]]
  if (!col %in% names(cohort)) next
  pct <- .pct_informative(cohort[[col]])
  if (pct < min_pct) {
    cat(sprintf("[ENT Table 1] Skipping '%s': %.1f%% informative (need ≥ %d%%)\n",
                col, pct, min_pct))
    next
  }
  included_vars          <- c(included_vars, col)
  included_labels[[col]] <- lbl
}

if (length(included_vars) == 0)
  stop("[ENT Table 1] No variables passed coverage gates — check data")

cat(sprintf("[ENT Table 1] Including %d / %d variables in table\n",
            length(included_vars), length(var_registry)))

# =============================================================================
# SECTION 11: BUILD gtsummary TABLE
# =============================================================================

tbl_data <- cohort |> dplyr::select(dplyr::all_of(included_vars))

type_list <- stats::setNames(
  lapply(included_vars, function(v) if (v %in% continuous_vars) "continuous" else NULL),
  included_vars
)
type_list <- type_list[!vapply(type_list, is.null, logical(1))]

tbl <- tbl_data |>
  gtsummary::tbl_summary(
    by           = NULL,
    label        = included_labels,
    type         = type_list,
    statistic    = list(
      gtsummary::all_continuous()  ~ "{median} ({p25}, {p75})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    digits       = list(
      gtsummary::all_continuous()  ~ 1,
      gtsummary::all_categorical() ~ c(0, 1)
    ),
    missing      = "ifany",
    missing_text = "Missing"
  ) |>
  gtsummary::add_n() |>
  gtsummary::bold_labels() |>
  gtsummary::modify_header(
    label  = "**Characteristic**",
    stat_0 = sprintf("**%s**  \n(N = %s)",
                     SPECIALTY_LABEL, format(nrow(cohort), big.mark = ","))
  ) |>
  gtsummary::modify_caption(
    sprintf("**Table 1.** Physician Characteristics — %s", SPECIALTY_LABEL)
  ) |>
  gtsummary::modify_footnote(
    stat_0 ~ paste0(
      "Median (IQR) for continuous variables; N (%) for categorical. ",
      "Gender and degree from NPPES Feb 2026. ",
      "Fellowship trained: any NPPES subspecialty taxonomy code beyond ",
      "the general otolaryngology code (207Y00000X). ",
      "IMG: classified from enthealth.org scraped medical school names ",
      "and NPPES credential text (MBBS/ECFMG/FRCS). ",
      "Rurality from USDA RUCA 2020 (zip-level). ",
      "BOG region: AAO-HNS Board of Governors geographic regions. ",
      "Practice stability from temporal NPPES (2019–2024). ",
      "Medicare volume quartile from Medicare Part B ", MEDICARE_REF_YEAR, ". ",
      "Telehealth participation, practice size, multiple locations, and ",
      "Medicare assignment from CMS Doctors and Clinicians (DAC) May 2024; ",
      "practice size = number of group practice members (solo = 1). ",
      "Industry payments: any CMS Open Payments general payment 2013–2024. ",
      "Underserved area from HRSA HPSA/MUA boundaries. ",
      "Years since NPI enumeration is a career-experience proxy. ",
      "Physician data source: ", SOURCE_LABEL, "."
    )
  )

cat("\n")
print(tbl)
cat("\n")

# =============================================================================
# SECTION 12: SAVE OUTPUTS
# =============================================================================

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

slug     <- tolower(gsub("[^A-Za-z0-9]", "_", SPECIALTY_LABEL))
out_csv  <- file.path(OUT_DIR, sprintf("table1_%s.csv",  slug))
out_docx <- file.path(OUT_DIR, sprintf("table1_%s.docx", slug))

tmp_cohort_csv <- paste0(out_csv, ".tmp")
readr::write_csv(cohort, tmp_cohort_csv)
file.rename(tmp_cohort_csv, out_csv)
cat(sprintf("[ENT Table 1] CSV saved:  %s (%d rows, %d cols)\n",
            out_csv, nrow(cohort), ncol(cohort)))

tbl_ft <- gtsummary::as_flex_table(tbl)
tbl_ft <- flextable::fontsize(tbl_ft, size = 10, part = "all")
tbl_ft <- flextable::set_table_properties(tbl_ft, width = 1, layout = "autofit")

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, tbl_ft)
tmp_out_docx <- paste0(out_docx, ".tmp.docx")
print(doc, target = tmp_out_docx)
file.rename(tmp_out_docx, out_docx)
cat(sprintf("[ENT Table 1] DOCX saved: %s\n", out_docx))
cat(sprintf("[ENT Table 1] Complete. N = %d physicians in table.\n", nrow(cohort)))
