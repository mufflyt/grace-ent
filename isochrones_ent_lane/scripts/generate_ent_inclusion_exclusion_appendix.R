#!/usr/bin/env Rscript
# =============================================================================
# ENT MYSTERY-CALLER STUDY — INCLUSION / EXCLUSION CRITERIA APPENDIX
# =============================================================================
# All counts are computed LIVE from the source files via the shared helper
# R/ent_cohort_counts.R.  Rerun this script after any data update and the
# appendix will automatically reflect the new cohort numbers.
#
# Outputs:
#   artifacts/ent/inclusion_exclusion_appendix.txt   (human-readable)
#   artifacts/ent/inclusion_exclusion_appendix.csv   (machine-readable)
#
# Usage:
#   Rscript scripts/generate_ent_inclusion_exclusion_appendix.R
#
# Author: Tyler Muffly MD + Claude Code
# Date:   2026-05-04
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(tibble)
})

source(here::here("R", "ent_cohort_counts.R"))

# ── Paths ─────────────────────────────────────────────────────────────────────
DROPBOX_DATA <- if (dir.exists(file.path(
    path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data"
  ))) {
  file.path(path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data")
} else {
  file.path(path.expand("~"), "Dropbox", "mystery_shopper", "Corbi study", "ENT", "Data")
}
if (!dir.exists(DROPBOX_DATA)) stop("Dropbox ENT/Data directory not found")

ENTHEALTH_CSV <- file.path(DROPBOX_DATA, "results_geocoded.csv")
AAO_XLSX      <- Sys.getenv("AAO_XLSX_PATH",
  unset = file.path(path.expand("~"), "Documents", "ResultsGrid_ExportData.xlsx"))
NPI_MATCH_CSV <- here::here("data", "ent", "aao_npi_matched.csv")

OUT_DIR <- here::here("artifacts", "ent")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
OUT_TXT <- file.path(OUT_DIR, "inclusion_exclusion_appendix.txt")
OUT_CSV <- file.path(OUT_DIR, "inclusion_exclusion_appendix.csv")

cat("\n=======================================================================\n")
cat("  ENT INCLUSION/EXCLUSION APPENDIX\n")
cat(sprintf("  Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=======================================================================\n\n")

# ── Compute all counts live from source files ─────────────────────────────────
ct <- compute_ent_cohort_counts(ENTHEALTH_CSV, AAO_XLSX, NPI_MATCH_CSV, verbose = TRUE)

# ── Build the criteria table ──────────────────────────────────────────────────
criteria <- tribble(
  ~criterion_no, ~type,                            ~criterion,
  ~rationale,                                                                                     ~n_affected, ~stage,

  1L, "Inclusion",
  "Listed on ENThealth.org physician directory (February 2026)",
  paste0("ENThealth.org is the AAO-HNS public-facing patient portal used to locate ",
         "otolaryngologists. All physicians listed were eligible for initial identification."),
  ct$n_enthealth_raw, "Identification",

  2L, "Inclusion",
  "Active AAO-HNS member as of March 5, 2026",
  paste0("Membership list provided by Tina Maggio (Chief Communications Officer, AAO-HNS). ",
         "Includes only US-based, actively practicing otolaryngologists; residents and retired ",
         "members are explicitly excluded by AAO-HNS."),
  ct$n_aao_total, "Confirmation",

  3L, "Exclusion",
  "Physician listed on ENThealth.org only — not confirmed as active AAO-HNS member",
  paste0("Physicians appearing on the ENThealth directory but absent from the AAO-HNS member ",
         "roster cannot be confirmed as board-eligible practicing otolaryngologists and were ",
         "excluded to ensure specialty accuracy."),
  ct$n_enth_only, "Confirmation",

  4L, "Exclusion",
  "No phone number listed on ENThealth.org",
  paste0("The mystery-caller study design requires a callable office telephone number to ",
         "simulate how a patient would contact the physician's practice. Physicians without a ",
         "listed phone number on ENThealth.org at the time of data collection (February 2026) ",
         "are not reachable by patients using this directory and were therefore excluded."),
  ct$n_no_phone, "Study eligibility",

  5L, "Exclusion",
  "Practice address located in Puerto Rico",
  paste0("The mystery-caller protocol uses standardized English-language callers. No native-",
         "Spanish-speaking callers were available for this study. Puerto Rico offices were ",
         "excluded because the call script could not be faithfully administered and results ",
         "would not be comparable to continental US practices."),
  ct$n_puerto_rico, "Study eligibility",

  6L, "Non-exclusion (quality note)",
  "Physician not geocoded (no latitude/longitude coordinates)",
  paste0("Geocoding failure does not affect the mystery-caller protocol, which uses the listed ",
         "phone number rather than a geographic coordinate. Non-geocoded physicians are retained ",
         "in the analytic cohort and flagged with geocoded = FALSE."),
  ct$n_not_geocoded, "Quality",

  7L, "Non-exclusion (enrichment)",
  "No NPI found in NPPES February 2026",
  paste0("NPI assignment is used for data enrichment (linking Medicare claims, specialty ",
         "taxonomy) and is not required for call sampling. Physicians without a matched NPI ",
         "are retained and flagged with npi = NA."),
  ct$n_no_npi_total, "Quality"
)

# ── Write plain-text appendix ─────────────────────────────────────────────────
n  <- function(x) format(x, big.mark = ",")   # local formatter shorthand
pct <- function(x, d) sprintf("%.1f%%", 100 * x / max(d, 1L))

lines <- c(
  "========================================================================",
  "  APPENDIX — INCLUSION AND EXCLUSION CRITERIA",
  "  ENT Otolaryngologist Mystery-Caller Study",
  sprintf("  Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "========================================================================",
  "",
  "STUDY OVERVIEW",
  "--------------",
  "Design     : Cross-sectional mystery-caller audit of access to care.",
  "Population : US attending otolaryngologists actively listed on the",
  "             AAO-HNS ENThealth.org physician directory.",
  "",
  "Data sources:",
  "  1. ENThealth.org/find-ent/ — AAO-HNS public directory (February 2026)",
  sprintf("     N = %s physicians identified", n(ct$n_enthealth_raw)),
  "  2. AAO-HNS member roster — provided by Tina Maggio,",
  "     Chief Communications Officer, AAO-HNS (received March 5, 2026)",
  sprintf("     N = %s US actively practicing otolaryngologists", n(ct$n_aao_total)),
  "  3. NPPES NPI Registry — February 2026 flat file (CMS)",
  "     Used for enrichment only; not an exclusion criterion.",
  "",
  "COHORT ASSEMBLY SEQUENCE",
  "------------------------",
  sprintf("  Step 1  ENThealth scrape                     : N = %s",
          n(ct$n_enthealth_raw)),
  sprintf("  Step 2  Inner join with AAO-HNS roster       : N = %s",
          n(ct$n_inner_join)),
  sprintf("          ENThealth-only (excluded)             :  −%s",
          n(ct$n_enth_only)),
  sprintf("  Step 3  Exclude — no phone number            :  −%s",
          n(ct$n_no_phone)),
  sprintf("  Step 4  Exclude — Puerto Rico address        :  −%s",
          n(ct$n_puerto_rico)),
  sprintf("  ─────────────────────────────────────────────────────"),
  sprintf("  FINAL ANALYTIC COHORT                        : N = %s",
          n(ct$n_final)),
  "",
  sprintf("  Quality notes (retained, not excluded):"),
  sprintf("    Not geocoded      : n = %s (%s of final cohort)",
          n(ct$n_not_geocoded), pct(ct$n_not_geocoded, ct$n_final)),
  sprintf("    No NPI in NPPES   : n = %s (%s of final cohort)",
          n(ct$n_no_npi_total), pct(ct$n_no_npi_total, ct$n_final)),
  "",
  "DETAILED CRITERIA",
  "-----------------"
)

for (i in seq_len(nrow(criteria))) {
  cr <- criteria[i, ]
  lines <- c(lines,
    sprintf("Criterion %d — %s", cr$criterion_no, toupper(cr$type)),
    sprintf("  Stage      : %s", cr$stage),
    sprintf("  Rule       : %s", cr$criterion),
    strwrap(paste0("  Rationale  : ", cr$rationale), width = 72,
            exdent = 15),
    sprintf("  N affected : %s", n(cr$n_affected)),
    ""
  )
}

lines <- c(lines,
  "METHODS NOTES",
  "-------------",
  "• Name matching between ENThealth.org and AAO-HNS roster used",
  "  humaniformat::parse_names() after credential strip, joined on",
  "  normalized last name + first initial + state.",
  "• NPI matching used a 6-pass algorithm (exact → fuzzy last name →",
  "  special-char strip → nickname expansion → Jaro-Winkler first name",
  "  within last+state+city) against NPPES February 2026.",
  "• All counts are point-in-time as of February–March 2026.",
  paste0("• Counts recomputed live from source data on ",
         format(Sys.time(), "%Y-%m-%d"), "."),
  "",
  "LIVE-CODE ARCHITECTURE",
  "----------------------",
  "All counts in this appendix and in the STROBE flow diagram are computed",
  "live from source files — no intermediate CSV is read.  The shared helper",
  "R/ent_cohort_counts.R is the single source of truth:",
  "",
  "  R/ent_cohort_counts.R         <-- shared live computation",
  "          |                                  |",
  "  generate_ent_strobe_diagram.R    generate_ent_inclusion_exclusion_appendix.R",
  "",
  "compute_ent_cohort_counts() loads all three data files and derives every",
  "stage count in one pass:",
  "  1. results_geocoded.csv        (ENThealth.org scrape, Dropbox)",
  "  2. ResultsGrid_ExportData.xlsx (AAO-HNS member roster, March 5 2026)",
  "  3. data/ent/aao_npi_matched.csv (6-pass NPI matching vs NPPES Feb 2026)",
  "",
  "To update: re-run either script after refreshing any source file.",
  "Every number in the diagram and this appendix will update automatically.",
  "========================================================================"
)

writeLines(lines, OUT_TXT)
cat(sprintf("\nAppendix TXT : %s\n", OUT_TXT))

# ── Write CSV ─────────────────────────────────────────────────────────────────
tmp_csv <- paste0(OUT_CSV, ".tmp")
write_csv(criteria, tmp_csv)
file.rename(tmp_csv, OUT_CSV)
cat(sprintf("Appendix CSV : %s\n", OUT_CSV))

cat("\n=======================================================================\n")
cat("  INCLUSION/EXCLUSION APPENDIX — COMPLETE\n")
cat(sprintf("  Final analytic cohort : N = %s\n", n(ct$n_final)))
cat("=======================================================================\n\n")
