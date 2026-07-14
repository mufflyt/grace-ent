#!/usr/bin/env Rscript
# =============================================================================
# Assemble the model-ready ENT mystery-caller dataset.
#
# Starts from the phase-2 call log and left-joins every geographic covariate
# crosswalk built by the sibling scripts. Original columns are preserved; the
# covariates below are appended. Join keys:
#   state  ->  Medicaid Fee Index, AAO-HNS district/region
#   ZIP    ->  CDC SVI, county FIPS + CBSA
#   county ->  ENT supply, CMS Medicare/dual-Medicaid enrollment
#   CBSA   ->  KFF hospital-market HHI (matched by principal city + state)
#
# Prereqs (run once; all write to data/raw/):
#   scripts/build_geo_crosswalk.R      -> zip_to_county_cbsa.csv
#   scripts/build_zip_svi.R            -> zip_svi_2022.csv
#   scripts/build_county_ent_count.R   -> county_ent_count.csv
#   scripts/build_cms_enrollment.R     -> county_cms_enrollment.csv
#   (medicaid_fee_index_state.csv, state_to_aao_hns_district.csv,
#    kff_hhi_msa_2024.xlsx are committed reference files)
#
# Output: data/processed/ent_phase2_enriched.csv
#
# Run: Rscript scripts/enrich_call_log.R [path/to/call_log.csv]
# =============================================================================

suppressPackageStartupMessages({library(readxl)})
options(stringsAsFactors = FALSE)
raw <- function(...) file.path("data", "raw", ...)

# ---- 0. Locate the input call log ------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
pick_latest <- function(pat) {
  hits <- list.files("data/processed", pattern = pat, full.names = TRUE)
  if (!length(hits)) NA_character_ else rev(sort(hits))[1]
}
in_csv <- if (length(args) >= 1) args[1] else {
  f <- pick_latest("^ent_phase2_all_.*\\.csv$")
  if (is.na(f)) f <- pick_latest("^cleaned_phase_2_data_.*\\.csv$")
  f
}
if (is.na(in_csv) || !file.exists(in_csv)) stop("No input call log found.")
cat("Input:", in_csv, "\n")
cl <- read.csv(in_csv, colClasses = "character", check.names = FALSE)
n0 <- nrow(cl)

# ---- 1. Normalized join keys -----------------------------------------------
pad5 <- function(x) {
  d <- gsub("[^0-9]", "", x)
  ifelse(!nzchar(d), NA_character_, sprintf("%05d", as.integer(substr(d, 1, min(5, nchar(d))))))
}
cl$.zip5  <- vapply(cl$zip, pad5, character(1))
cl$.state <- toupper(trimws(cl$state))
cl$.row   <- seq_len(nrow(cl))

cat("Covariate coverage:\n")

# ---- 2. State-level joins ---------------------------------------------------
mfi <- read.csv(raw("medicaid_fee_index_state.csv"), colClasses = "character")
mfi$medicaid_fee_index <- suppressWarnings(as.numeric(mfi$Medicaid_Fee_Index_2024))
cl$State_Abbr <- cl$.state
cl <- merge(cl, mfi[!duplicated(mfi$State_Abbr), c("State_Abbr","medicaid_fee_index")],
            by = "State_Abbr", all.x = TRUE, sort = FALSE); cl$State_Abbr <- NULL
cat(sprintf("  %-26s %5.1f%%\n", "Medicaid Fee Index", 100*mean(!is.na(cl$medicaid_fee_index))))

aao <- read.csv(raw("state_to_aao_hns_district.csv"), colClasses = "character")
aao <- aao[!duplicated(aao$State_Abbr), c("State_Abbr","AAO_HNS_District","Region")]
names(aao) <- c("State_Abbr","aao_hns_district","aao_hns_region")
cl$State_Abbr <- cl$.state
cl <- merge(cl, aao, by = "State_Abbr", all.x = TRUE, sort = FALSE); cl$State_Abbr <- NULL
cat(sprintf("  %-26s %5.1f%%\n", "AAO-HNS district/region", 100*mean(!is.na(cl$aao_hns_district))))

# ---- 3. ZIP-level joins -----------------------------------------------------
svi <- read.csv(raw("zip_svi_2022.csv"), colClasses = c(zip = "character"))
svi_cols <- c("svi_overall","svi_socioeconomic","svi_household",
              "svi_minority_language","svi_housing_transport")
cl <- merge(cl, svi[!duplicated(svi$zip), c("zip", svi_cols)],
            by.x = ".zip5", by.y = "zip", all.x = TRUE, sort = FALSE)
cat(sprintf("  %-26s %5.1f%%\n", "CDC SVI", 100*mean(!is.na(cl$svi_overall))))

xw <- read.csv(raw("zip_to_county_cbsa.csv"), colClasses = "character")
cl <- merge(cl, xw[!duplicated(xw$zip), c("zip","county_fips","cbsa_code","cbsa_title","cbsa_type")],
            by.x = ".zip5", by.y = "zip", all.x = TRUE, sort = FALSE)
cat(sprintf("  %-26s %5.1f%%\n", "county FIPS + CBSA", 100*mean(!is.na(cl$county_fips))))

# ---- 4. County-level joins --------------------------------------------------
ent <- read.csv(raw("county_ent_count.csv"), colClasses = c(county_fips = "character"))
cl <- merge(cl, ent[!duplicated(ent$county_fips), c("county_fips","n_ent","ent_per_100k")],
            by = "county_fips", all.x = TRUE, sort = FALSE)
cat(sprintf("  %-26s %5.1f%%\n", "county ENT supply", 100*mean(!is.na(cl$n_ent))))

cms <- read.csv(raw("county_cms_enrollment.csv"), colClasses = c(county_fips = "character"))
cms_cols <- c("medicare_benes","dual_medicaid_benes","dual_pct")
cl <- merge(cl, cms[!duplicated(cms$county_fips), c("county_fips", cms_cols)],
            by = "county_fips", all.x = TRUE, sort = FALSE)
cat(sprintf("  %-26s %5.1f%%\n", "CMS enrollment", 100*mean(!is.na(cl$medicare_benes))))

# ---- 5. KFF HHI via principal-city + state key ------------------------------
kff <- as.data.frame(readxl::read_excel(raw("kff_hhi_msa_2024.xlsx"), sheet = "appendix"))
kff$hhi_2024 <- suppressWarnings(as.numeric(kff[["HHI in 2024"]]))
msa_key <- function(title) {
  t  <- sub(" (Metropolitan|Micropolitan) Statistical Area$", "", title)
  st <- sub(".*,\\s*([A-Z]{2}).*", "\\1", t)
  city <- sub("\\s*,.*", "", t); city <- sub("[-/].*", "", city)
  tolower(paste0(trimws(city), "|", st))
}
kff$.k <- msa_key(kff$MSA)
kff <- kff[!duplicated(kff$.k), c(".k","hhi_2024")]
cl$.k <- ifelse(is.na(cl$cbsa_title), NA, msa_key(cl$cbsa_title))
cl <- merge(cl, kff, by = ".k", all.x = TRUE, sort = FALSE); cl$.k <- NULL
cat(sprintf("  %-26s %5.1f%%\n", "KFF HHI (metro only)", 100*mean(!is.na(cl$hhi_2024))))

# ---- 6. Restore original order, drop temp keys, write -----------------------
cl <- cl[order(cl$.row), ]
cl$.row <- NULL; cl$.zip5 <- NULL; cl$.state <- NULL
stopifnot(nrow(cl) == n0)
out <- file.path("data", "processed", "ent_phase2_enriched.csv")
utils::write.csv(cl, out, row.names = FALSE, na = "")
cat(sprintf("\nWrote %s: %d rows x %d cols\n", out, nrow(cl), ncol(cl)))
