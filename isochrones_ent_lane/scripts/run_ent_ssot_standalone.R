#!/usr/bin/env Rscript
# run_ent_ssot_standalone.R
#
# Applies the ENT SSOT subspecialty hierarchy to the full ENT cohort and
# writes a timestamped output CSV ready for sharing with collaborators.
#
# Usage:
#   Rscript scripts/run_ent_ssot_standalone.R
#
# Output:
#   data/ent/ent_master_roster_ssot_<timestamp>.csv  (full dataset)
#   artifacts/ent/ent_data_dictionary_<timestamp>.csv (data dictionary)
#
# Geocoding cascade applied at [2.5/5] for any ungeocoded rows:
#   Tier 1  US Census Bureau  (primary; free; US addresses)
#   Tier 2  ArcGIS World      (handles campus abbreviations, POI names)
#   Tier 3  Nominatim / OSM   (open; rate-limited to 1 req/sec)
#   Tier 4  City centroid     (offline fallback; city-level precision)
#   Tier 5  ZIP centroid      (last resort; ZIP-level precision)
#
# Data-quality fixes applied after SSOT (see [3.5/5] block):
#   C/D  board_cert_lapsed corrected when is_board_certified=TRUE
#   E    AK / HI physicians excluded (non-CONUS, no drive-time network)
#   F    Double spaces collapsed in full_name / clean_name
#   I    career_stage inferred from medical_school_year for rows missing
#        residency_year (proxy: med_school_year + 5 yrs ENT residency)
#   J    board_cert_lapsed=NA → FALSE when no board cert data
#   K    taxonomy_code backfilled from AAO match for 1 known gap
#   P    medical_school_name / medical_school_year cascade: 15 CMS sources
#        (PC 2025→2018 Q2, DAC 2024→2021, PC supplemental tables) via
#        R/enrich_medical_school_cascade.R; existing scrape data never overwritten
#   Q    has_advanced_degree + advanced_degrees extracted from credential field
#        (PhD, MPH, MBA, MS, JD, etc.) via extract_advanced_degrees()
#   R    credential_img_hint flagged for MBBS / MBChB / ECFMG credentials
#   S    age_final + years_since_grad estimated from medical_school_year
#        (assumption: medical school graduation at age 26)

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(yaml)
  library(arrow)
})

# Load geocoding cascade (Census → ArcGIS → Nominatim → city centroid)
# Required for [2.5/5] fallback geocoding step.
source(here("R", "census_geocode_enhanced.R"), local = FALSE)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

cat("══════════════════════════════════════════════════════\n")
cat("  ENT SSOT Subspecialty Assignment — Standalone Run  \n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat("══════════════════════════════════════════════════════\n\n")

# ── Load config ────────────────────────────────────────────────────────────────
config_path <- here("config", "specialty_configs", "ent.yml")
stopifnot(file.exists(config_path))
specialty_config <- yaml::read_yaml(config_path)
cat(sprintf("[1/5] Loaded config: %s\n", config_path))

# ── Load cohort ────────────────────────────────────────────────────────────────
cohort_path <- here("data", "ent", "enthealth_npi_full_cohort.csv")
stopifnot(file.exists(cohort_path))
export_data <- readr::read_csv(cohort_path, show_col_types = FALSE)
cat(sprintf("[2/5] Loaded cohort: %s (%s physicians)\n",
            cohort_path, format(nrow(export_data), big.mark = ",")))

# Drop old ent_subspecialty / subspecialty / ssot_source if present
# so the function builds them fresh
drop_old <- intersect(
  c("ent_subspecialty", "subspecialty", "ssot_source", "taxonomy_subspec"),
  names(export_data)
)
if (length(drop_old) > 0) {
  export_data <- export_data |> dplyr::select(-dplyr::all_of(drop_old))
  cat(sprintf("  Dropped stale columns: %s\n", paste(drop_old, collapse = ", ")))
}

#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── [2.5/5] Geocoding fallback cascade for ungeocoded rows ────────────────────
# Runs automatically whenever any row has geocoded == FALSE.  Uses the full
# 5-tier cascade: Census → ArcGIS → Nominatim → city centroid → ZIP centroid.
# Addresses from NPPES API often contain campus abbreviations (e.g.
# "MEDICAL CENTER BLVD", "EMILE 42ND ST") or have no street at all; these
# always fail Census but succeed in ArcGIS or Nominatim via POI search.
n_ungeocoded <- sum(!export_data$geocoded, na.rm = TRUE)
if (n_ungeocoded > 0L) {
  cat(sprintf("[2.5/5] Geocoding fallback cascade for %d ungeocoded rows...\n",
              n_ungeocoded))

  # Strip the literal string " NA" appended when NPPES address_line2 is NA
  .clean_addr <- function(x) {
    if (is.na(x)) return(NA_character_)
    trimws(gsub("\\s+NA$", "", trimws(x), perl = TRUE))
  }

  ungeocoded_idx <- which(!export_data$geocoded)
  n_resolved     <- 0L

  for (i in ungeocoded_idx) {
    addr  <- .clean_addr(export_data$address[i])
    city  <- trimws(as.character(export_data$city[i]))
    state <- trimws(as.character(export_data$state[i]))
    zip   <- trimws(as.character(export_data$zip5[i]))

    # Tiers 1–4 via geocode_with_fallbacks()
    res <- tryCatch(
      geocode_with_fallbacks(
        addr  = if (!is.na(addr) && nzchar(addr)) addr else NA_character_,
        city  = city,
        state = state,
        zip   = zip
      ),
      error = function(e) NULL
    )

    if (!is.null(res) && !is.na(res$latitude) && !is.na(res$longitude)) {
      export_data$latitude[i]            <- as.numeric(res$latitude)
      export_data$longitude[i]           <- as.numeric(res$longitude)
      export_data$geocoded[i]            <- TRUE
      export_data$geocoder_provenance[i] <- as.character(
        res$geocoder_provenance %||% "fallback_cascade"
      )
      export_data$matched_address[i]     <- as.character(
        res$matched_address %||% NA_character_
      )
      export_data$match_type[i]          <- as.character(
        res$match_type %||% NA_character_
      )
      n_resolved <- n_resolved + 1L
      next
    }

    # Tier 5: ZIP centroid (offline last resort)
    if (!is.na(zip) && nzchar(zip)) {
      zc <- tryCatch(geocode_zip_centroid(zip), error = function(e) NULL)
      if (!is.null(zc) && !is.na(zc$latitude) && !is.na(zc$longitude)) {
        export_data$latitude[i]            <- as.numeric(zc$latitude)
        export_data$longitude[i]           <- as.numeric(zc$longitude)
        export_data$geocoded[i]            <- TRUE
        export_data$geocoder_provenance[i] <- "zip_centroid"
        export_data$matched_address[i]     <- paste0("ZIP ", zip)
        export_data$match_type[i]          <- "zip_centroid"
        n_resolved <- n_resolved + 1L
      }
    }
  }

  n_still <- sum(!export_data$geocoded, na.rm = TRUE)
  cat(sprintf("  Resolved %d / %d  (still ungeocoded: %d)\n",
              n_resolved, n_ungeocoded, n_still))
} else {
  cat("[2.5/5] All rows already geocoded — skipping cascade.\n")
}

# ── Apply SSOT function ────────────────────────────────────────────────────────
cat("[3/5] Applying SSOT hierarchy...\n")
ssot_script <- here("R", "apply_ent_ssot_subspecialty.R")
source(ssot_script, local = TRUE)
result <- apply_ent_ssot_subspecialty(export_data, specialty_config)

# ── [3.5/5] Data-quality cleanup ──────────────────────────────────────────────
cat("[3.5/5] Applying data-quality fixes...\n")
current_year <- as.integer(format(Sys.Date(), "%Y"))

# ── C + D: board_cert_lapsed must be FALSE when is_board_certified = TRUE ──────
# board_cert_year reflects INITIAL certification date; is_board_certified
# reflects current ABMS status from the scraper (authoritative).
# A physician who has maintained MOC may have board_cert_year from the 1980s
# but is_board_certified = TRUE — their cert is NOT lapsed.
n_cd <- sum(result$is_board_certified == TRUE & result$board_cert_lapsed != FALSE,
            na.rm = TRUE)
cat(sprintf("  C/D  board_cert_lapsed fixed for %d certified physicians\n", n_cd))

# ── J: board_cert_lapsed = NA → FALSE when no cert data at all ─────────────────
n_j <- sum(is.na(result$board_cert_lapsed))

result <- result |>
  dplyr::mutate(
    board_cert_lapsed = dplyr::case_when(
      is_board_certified == TRUE  ~ FALSE,          # C + D: certified → not lapsed
      !is.na(board_cert_lapsed)   ~ board_cert_lapsed,  # preserve existing TRUE/FALSE
      TRUE                        ~ FALSE            # J: no cert data → not lapsed
    )
  )
cat(sprintf("  J    board_cert_lapsed=NA → FALSE for %d rows with no cert data\n", n_j))

# ── E: Exclude AK / HI (non-CONUS — no road-network drive-time coverage) ───────
n_nonconus <- sum(result$state %in% c("AK", "HI"), na.rm = TRUE)
result <- result |> dplyr::filter(!state %in% c("AK", "HI"))
cat(sprintf("  E    Excluded %d non-CONUS physicians (AK/HI)\n", n_nonconus))

# ── F: Collapse consecutive spaces in full_name / clean_name ────────────────────
n_doublespace <- sum(grepl("  ", result$full_name, fixed = TRUE), na.rm = TRUE)
result <- result |>
  dplyr::mutate(
    full_name  = gsub("  +", " ", trimws(full_name)),
    clean_name = gsub("  +", " ", trimws(clean_name))
  )
cat(sprintf("  F    Collapsed double spaces in %d full_name values\n", n_doublespace))

# ── I: Infer career_stage for rows missing residency_year ────────────────────────
# ENT residency = 5 years after medical school graduation.
# Fallback chain: medical_school_year + 5  →  board_cert_year - 1  →  NA
n_missing_cs_before <- sum(is.na(result$career_stage))
result <- result |>
  dplyr::mutate(
    .proxy_res_yr = dplyr::coalesce(
      dplyr::if_else(!is.na(residency_year),           residency_year,          NA_real_),
      dplyr::if_else(!is.na(medical_school_year),      medical_school_year + 5, NA_real_),
      dplyr::if_else(!is.na(board_cert_year),          board_cert_year - 1L,    NA_real_)
    ),
    .proxy_yrs = dplyr::if_else(
      is.na(years_since_residency) & !is.na(.proxy_res_yr),
      as.numeric(current_year - .proxy_res_yr),
      years_since_residency
    ),
    career_stage = dplyr::case_when(
      !is.na(career_stage)                    ~ career_stage,
      !is.na(.proxy_yrs) & .proxy_yrs <= 10  ~ "Early (<=10 yrs)",
      !is.na(.proxy_yrs) & .proxy_yrs <= 25  ~ "Mid (11-25 yrs)",
      !is.na(.proxy_yrs)                     ~ "Senior (>25 yrs)",
      TRUE                                    ~ NA_character_
    )
  ) |>
  dplyr::select(-.proxy_res_yr, -.proxy_yrs)
n_missing_cs_after <- sum(is.na(result$career_stage))
cat(sprintf("  I    career_stage filled for %d rows (still NA: %d)\n",
            n_missing_cs_before - n_missing_cs_after, n_missing_cs_after))

# ── K: Backfill taxonomy_code for 1 known gap (NPI 1215100706) ─────────────────
# NPPES ENT extract does not include this physician (taxonomy 174400000X is
# non-ENT); AAO match provides the best available code.
aao_path <- here("data", "ent", "aao_npi_matched.csv")
if (file.exists(aao_path) && "taxonomy_code" %in% names(result)) {
  aao_tax <- readr::read_csv(aao_path, show_col_types = FALSE) |>
    dplyr::filter(!is.na(npi), !is.na(ent_taxonomy)) |>
    dplyr::mutate(npi = as.character(npi)) |>
    dplyr::select(npi, ent_taxonomy) |>
    dplyr::distinct(npi, .keep_all = TRUE)

  n_tax_before <- sum(is.na(result$taxonomy_code) & !is.na(result$npi))
  result <- result |>
    dplyr::mutate(npi = as.character(npi)) |>
    dplyr::left_join(aao_tax, by = "npi", suffix = c("", ".aao")) |>
    dplyr::mutate(taxonomy_code = dplyr::coalesce(taxonomy_code, ent_taxonomy)) |>
    dplyr::select(-ent_taxonomy)
  n_tax_after <- sum(is.na(result$taxonomy_code) & !is.na(result$npi))
  cat(sprintf("  K    taxonomy_code backfilled for %d NPI(s) from AAO match\n",
              n_tax_before - n_tax_after))
}

# ── L: Fix NPI-keyed year typos ────────────────────────────────────────────────
# Identified by manual review of future/impossible year values.
result <- result |>
  dplyr::mutate(
    npi = as.character(npi),
    residency_year    = dplyr::case_when(
      npi == "1861426223" ~ 1994,   # Alan G. Micco: 2994 → 1994
      TRUE ~ residency_year
    ),
    medical_school_year = dplyr::case_when(
      npi == "1700271376" ~ 2015,   # Jennifer R. Miles: 2105 → 2015
      TRUE ~ medical_school_year
    )
  )
cat("  L    Fixed year typos: residency_year 2994→1994 (NPI 1861426223), med_school_year 2105→2015 (NPI 1700271376)\n")

# ── M: Exclude current/future residents (residency_year > current_year) ────────
# Physicians whose ENT residency has not yet completed are not practicing
# clinicians and should not appear in an access-to-care workforce analysis.
# Note: some 2028 entries with 1980s med-school years are data-entry errors;
# they are excluded here because the correct year cannot be recovered reliably.
n_residents <- sum(result$residency_year > current_year, na.rm = TRUE)
result <- result |> dplyr::filter(is.na(residency_year) | residency_year <= current_year)
cat(sprintf("  M    Excluded %d current/future residents (residency_year > %d)\n",
            n_residents, current_year))

# ── N: Type fixes + drop permanently-empty columns ─────────────────────────────
# npi and phone_digits are identifiers — store as character, not numeric.
# confidence_score, fellowship_year: never populated by any upstream source.
# hpsa_score: requires HPSA shapefile not yet loaded in this pipeline.
# has_neurotology: ABOto-HNS Neurotology subspecialty cert requires a full
#   ABMS scrape; the scrape covers only 3 test rows, so the field is all FALSE
#   and carries no information until the scrape is complete.
drop_cols <- intersect(
  c("confidence_score", "fellowship_year", "hpsa_score", "has_neurotology"),
  names(result)
)
result <- result |>
  dplyr::mutate(
    npi          = as.character(npi),
    phone_digits = as.character(phone_digits)
  ) |>
  dplyr::select(-dplyr::any_of(drop_cols))
cat(sprintf("  N    npi + phone_digits cast to character; dropped %d empty cols: %s\n",
            length(drop_cols), paste(drop_cols, collapse = ", ")))

# ── O: Wire in ABMS checkpoint subspecialty certs ──────────────────────────────
# Joins the partial ABMS Playwright scrape (~1,160 physicians scraped so far)
# to the SSOT via last_name + first_initial + state.  Populates:
#   has_neurotology  — TRUE when abms_subspecialty/2 = "Neurotology"
#   abms_subspecialty_cert — raw cert label from ABMS (for reference)
# Ambiguous matches (same last+first+state → 2 NPIs) are resolved by
# taking the first NPI alphabetically; only 2 such collisions exist.
# Unmatched rows (22.7%) retain has_neurotology = FALSE.
ckpt_path <- here("data", "ent", "abms_scrape", "abms_ent_certifications_checkpoint.csv")
if (file.exists(ckpt_path)) {
  ckpt <- readr::read_csv(ckpt_path, show_col_types = FALSE) |>
    dplyr::filter(abms_found == TRUE) |>
    dplyr::mutate(
      lc = toupper(trimws(search_last)),
      fc = toupper(trimws(gsub("\\s.*", "", search_first))),
      st = toupper(trimws(search_state)),
      has_neurotology_abms = grepl("neurotol", abms_subspecialty,   ignore.case = TRUE) |
                             grepl("neurotol", abms_subspecialty_2, ignore.case = TRUE),
      abms_subspecialty_cert = dplyr::case_when(
        nzchar(trimws(abms_subspecialty))   ~ trimws(abms_subspecialty),
        nzchar(trimws(abms_subspecialty_2)) ~ trimws(abms_subspecialty_2),
        TRUE                                ~ NA_character_
      )
    ) |>
    dplyr::select(lc, fc, st, has_neurotology_abms, abms_subspecialty_cert) |>
    dplyr::distinct(lc, fc, st, .keep_all = TRUE)   # resolve ambiguous: keep first

  result2 <- result |>
    dplyr::mutate(
      lc = toupper(trimws(last_name)),
      fc = toupper(trimws(gsub("\\s.*", "", first_name))),
      st = toupper(trimws(state))
    ) |>
    dplyr::left_join(ckpt, by = c("lc", "fc", "st")) |>
    dplyr::mutate(
      has_neurotology        = dplyr::coalesce(has_neurotology_abms, FALSE),
      abms_subspecialty_cert = abms_subspecialty_cert
    ) |>
    dplyr::select(-lc, -fc, -st, -has_neurotology_abms)

  n_neuro  <- sum(result2$has_neurotology, na.rm = TRUE)
  n_joined <- sum(!is.na(result2$abms_subspecialty_cert) | result2$has_neurotology == TRUE |
                  result2$has_neurotology == FALSE & !is.na(
                    result2$abms_subspecialty_cert))
  n_matched <- sum(result2$has_neurotology == TRUE |
                   !is.na(result2$abms_subspecialty_cert))
  cat(sprintf("  O    ABMS checkpoint joined: %d physicians matched | %d Neurotology\n",
              nrow(ckpt), n_neuro))
  result <- result2
} else {
  result <- result |> dplyr::mutate(has_neurotology = FALSE,
                                    abms_subspecialty_cert = NA_character_)
  cat("  O    ABMS checkpoint not found — has_neurotology = FALSE for all\n")
}

# ── P: Multi-source medical school enrichment cascade ─────────────────────────
# Checks 15 CMS sources in priority order (PC 2025→2018, DAC 2024→2021,
# PC supplemental) to fill NA medical_school_name / medical_school_year.
# Existing directory-scrape values are never overwritten.
# Adds medical_school_source provenance column.
source(here("R", "enrich_medical_school_cascade.R"), local = FALSE)

duckdb_path <- tryCatch(
  yaml::read_yaml(here("config", "paths.yml"))[["nber_duckdb_main"]],
  error = function(e) NULL
)
result <- enrich_medical_school_cascade(
  result,
  db_path         = duckdb_path,
  taxonomy_codes  = c("207Y00000X"),
  concordance_out = here("artifacts", "ent"),
  cache_dir       = here("data", "ent", ".cache"),
  verbose         = TRUE
)
cat("  P    (see [enrich_medical_school] lines above)\n")

# ── Q: Advanced degrees from scraped credential string ────────────────────────
# Detects PhD, MPH, MBA, MS, JD, MSN, EdD, MHA, MHS from the enthealth.org
# credential field (e.g., "MD, PhD, FACS" → advanced_degrees = "PhD").
# Adds: has_advanced_degree (logical),  advanced_degrees (character)
source(here("R", "physician_demographic_utils.R"), local = FALSE)

result <- result |>
  dplyr::mutate(
    advanced_degrees    = vapply(credentials, extract_advanced_degrees, character(1)),
    has_advanced_degree = !is.na(advanced_degrees) & advanced_degrees != "None/MD/DO Only"
  )

n_adv <- sum(result$has_advanced_degree, na.rm = TRUE)
cat(sprintf("  Q    Advanced degrees: %d (%.1f%%) physicians hold a second degree (PhD/MPH/MBA/etc.)\n",
            n_adv, 100 * n_adv / max(nrow(result), 1L)))

# ── R: Credential-based IMG hint (MBBS / MBChB / ECFMG in scraped field) ──────
# The enthealth.org credentials column carries IMG markers (MBBS, ECFMG, FRCSC)
# not present in the NPPES credential field for some physicians.
# Adds: credential_img_hint (logical) — used by create_specialty_table1.R when
# school-name classification produces NA.
result <- result |>
  dplyr::mutate(
    credential_img_hint = dplyr::if_else(
      grepl("\\bMBBS\\b|\\bMBChB\\b|\\bBMBS\\b|\\bBMBCh\\b|\\bECFMG\\b",
            dplyr::coalesce(credentials, ""), ignore.case = TRUE, perl = TRUE),
      TRUE, FALSE
    )
  )

n_img_hint <- sum(result$credential_img_hint, na.rm = TRUE)
cat(sprintf("  R    Credential IMG hint: %d physicians with MBBS/MBChB/ECFMG marker\n", n_img_hint))

# ── S: Estimated age and years-since-graduation from medical_school_year ──────
# Assumption: physicians graduate medical school at approximately age 26.
# age_final = current_year - (med_school_year - 26)
# years_since_grad = current_year - med_school_year
# These are used by build_specialty_table1() for the age row in Table 1.
current_year_s <- as.integer(format(Sys.Date(), "%Y"))
result <- result |>
  dplyr::mutate(
    age_final = dplyr::if_else(
      !is.na(medical_school_year) &
        medical_school_year >= 1950L &
        medical_school_year <= current_year_s,
      as.numeric(current_year_s - (medical_school_year - 26L)),
      NA_real_
    ),
    years_since_grad = dplyr::if_else(
      !is.na(medical_school_year) &
        medical_school_year >= 1950L &
        medical_school_year <= current_year_s,
      as.numeric(current_year_s - medical_school_year),
      NA_real_
    )
  )
n_age <- sum(!is.na(result$age_final))
cat(sprintf("  S    age_final: %d / %d (%.1f%%) physicians have an age estimate\n",
            n_age, nrow(result), 100 * n_age / nrow(result)))

# ── Summary ────────────────────────────────────────────────────────────────────
cat(sprintf("  Rows after cleanup: %s (removed %d non-CONUS)\n",
            format(nrow(result), big.mark = ","), n_nonconus))
cat(sprintf("  Missing NPI       : %d\n", sum(is.na(result$npi))))
cat(sprintf("  Missing geocoords : %d\n", sum(!result$geocoded, na.rm = TRUE)))

# ── Write full dataset ────────────────────────────────────────────────────────
out_csv  <- here("data", "ent", sprintf("ent_master_roster_ssot_%s.csv", ts))
latest   <- here("data", "ent", "ent_master_roster_ssot.csv")
tmp_out <- paste0(out_csv, ".tmp")
readr::write_csv(result, tmp_out)
file.rename(tmp_out, out_csv)
tmp_latest <- paste0(latest, ".tmp")
readr::write_csv(result, tmp_latest)
file.rename(tmp_latest, latest)
cat(sprintf("[4/5] Wrote %s rows → %s\n", format(nrow(result), big.mark = ","), out_csv))
cat(sprintf("      Also updated LATEST: %s\n", latest))

# ── Write data dictionary ─────────────────────────────────────────────────────
cat("[5/5] Building data dictionary...\n")
source(here("scripts", "build_ent_data_dictionary.R"), local = TRUE)
dict_path <- here("artifacts", "ent", sprintf("ent_data_dictionary_%s.csv", ts))
dict_latest <- here("artifacts", "ent", "ent_data_dictionary_LATEST.csv")
tmp_dict <- paste0(dict_path, ".tmp")
readr::write_csv(ent_data_dictionary, tmp_dict)
file.rename(tmp_dict, dict_path)
tmp_dict_latest <- paste0(dict_latest, ".tmp")
readr::write_csv(ent_data_dictionary, tmp_dict_latest)
file.rename(tmp_dict_latest, dict_latest)
cat(sprintf("      Data dictionary: %s (%d fields)\n",
            dict_path, nrow(ent_data_dictionary)))

cat("\n══════════════════════════════════════════════════════\n")
cat(sprintf("  Complete. Output: %s\n", out_csv))
cat(sprintf("  Dict:    %s\n", dict_path))
cat("══════════════════════════════════════════════════════\n")
