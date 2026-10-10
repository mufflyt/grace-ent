#!/usr/bin/env Rscript
#' @title Table 1: Otolaryngology Physician Characteristics (ENT)
#'
#' @description
#' Produces the publication-quality Table 1 for the ENT manuscript by
#' delegating to \code{\link{build_specialty_table1}} in
#' \code{R/create_specialty_table1.R}.  Reads the most recent SSOT master
#' roster from \code{data/ent/} and emits an enriched physician CSV plus a
#' Word document formatted per Green Journal style.
#'
#' @section Input:
#' \preformatted{
#' data/ent/ent_master_roster_ssot_<timestamp>.csv   (most recent by mtime)
#'   Falls back to: data/ent/enthealth_npi_LATEST.csv
#' }
#' The SSOT roster is preferred because it includes pre-computed
#' \code{age_final}, \code{years_since_grad}, \code{degree_type},
#' \code{medical_school_aamc_category}, board-cert columns, and
#' \code{is_aao_member}, minimising DuckDB re-derivation inside
#' \code{build_specialty_table1()}.
#'
#' @section Output:
#' \preformatted{
#' artifacts/ent/table1_otolaryngology__ent_.csv        (5,311 rows x 129 cols,
#' enriched)
#' artifacts/ent/table1_otolaryngology__ent_.docx       (Word, gtsummary, Green
#' Journal)
#' artifacts/ent/table1_otolaryngology__ent__coverage.csv  (per-variable
#' coverage audit)
#' }
#' The enriched CSV is the authoritative input for the slide version
#' (\code{scripts/create_ent_table1_slides.R}).
#'
#' @section Invariants:
#' \itemize{
#'   \item N = 5,311 after NPI-match filter (95.6\% of 5,555 SSOT rows)
#'   \item Gender enriched from NPPES Feb 2026 flat file (100\% coverage)
#'   \item RUCA codes applied at ZIP level (USDA 2020) — 100\% coverage
#'   \item AAO-HNS BOG region assigned via specialty crosswalk — 100\% coverage
#'   \item Subspecialty column: \code{ent_subspecialty} with 9 codes (see map
#' below)
#' }
#'
#' @section Subspecialty Code Map:
#' \preformatted{
#' Code        Display label
#' GEN_ENT     General Otolaryngology
#' H_N_ONCO    Head & Neck Oncology
#' FPRS        Facial Plastic & Reconstructive Surgery
#' PED_ENT     Pediatric Otolaryngology
#' OTOL_NEURO  Otology / Neurotology
#' RHINOL      Rhinology
#' LARYNGOL    Laryngology
#' SLEEP       Sleep Medicine
#' ALLERGY     Otolaryngic Allergy
#' }
#'
#' @section Failure Modes:
#' \describe{
#'   \item{No SSOT file found}{Falls back to \code{enthealth_npi_LATEST.csv}
#'     automatically with a message; that file lacks pre-computed demographics
#'     so enrichment takes longer and coverage may differ.}
#'   \item{DuckDB unavailable}{Practice stability and Medicare volume require
#'     \code{/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb}.  If the
#'     drive is absent, those variables are excluded from the table (coverage
#'     CSV flags them as \code{"excluded"}).}
#'   \item{NPPES flat file absent}{Gender enrichment falls back to SSA name
#'     inference (Tier 1 in the cascade); coverage stays high (>94\%)
#'     but source tier downgrades from \code{"high"} to \code{"medium"}.}
#' }
#'
#' @section Mental Model:
#' \preformatted{
#' SSOT roster  -->  build_specialty_table1()  -->  enriched CSV + docx
#'                         |
#'                    Enrichment tiers
#'                    1. Gender   <- NPPES flat file -> SSA name fallback
#'                    2. Age      <- medical_school_year (SSOT) -> PC Grd_yr ->
#' ABMS
#'                    3. IMG      <- medical_school_aamc_category (SSOT) -> DAC
#'                    4. RUCA     <- USDA 2020 ZIP crosswalk
#'                    5. Region   <- AAO-HNS BOG specialty crosswalk
#'                    6. Medicare <- Part B claims 2022 (DuckDB)
#'                    7. Practice <- CMS DAC group practice file
#' }
#'
#' @section Usage:
#' \preformatted{
#' Rscript manuscript/R/create_table1_ent_characteristics.R
#'
#' # Then produce slide version:
#' Rscript scripts/create_ent_table1_slides.R
#' }
#'
#' @family ent-reporting
#' @family manuscript-tables
#' @concept table1
#' @concept otolaryngology
#' @seealso \code{R/create_specialty_table1.R} for \code{build_specialty_table1()}
#' @seealso \code{scripts/create_ent_table1_slides.R} for the slide-ready compact version
#' @seealso \code{scripts/ent_table_1.R} for the standalone DuckDB-heavy version

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
})

source(here::here("R", "create_specialty_table1.R"), local = FALSE)

# ── ENT subspecialty label map ────────────────────────────────────────────────
# Ordered for Table 1 display: general first, then fellowship subspecialties
# alphabetically. Mirrors the AAO-HNS subspecialty certification structure.

ENT_SUBSPECIALTY_LABELS <- c(
  GEN_ENT    = "General Otolaryngology",
  ALLERGY    = "Otolaryngic Allergy",
  FPRS       = "Facial Plastic & Reconstructive Surgery",
  H_N_ONCO  = "Head & Neck Oncology",
  LARYNGOL   = "Laryngology",
  OTOL_NEURO = "Otology / Neurotology",
  PED_ENT    = "Pediatric Otolaryngology",
  RHINOL     = "Rhinology"
)

# ── Resolve input: most recent SSOT master roster ─────────────────────────────
# The SSOT master roster is richer than enthealth_npi_LATEST.csv — it includes
# pre-computed age_final, years_since_grad, degree_type, medical_school_aamc_category,
# board cert columns, and is_aao_member. Use it to minimise DuckDB re-derivation.

ent_roster_candidates <- sort(
  list.files(here::here("data", "ent"),
             pattern = "^ent_master_roster_ssot_.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)

if (length(ent_roster_candidates) == 0) {
  # Fallback to basic NPI-matched file if SSOT roster absent
  ent_input_path <- here::here("data", "ent", "enthealth_npi_LATEST.csv")
  message("[ENT Table1] SSOT master roster not found — using enthealth_npi_LATEST.csv")
} else {
  ent_input_path <- ent_roster_candidates[[1]]
  message("[ENT Table1] Using SSOT roster: ", basename(ent_input_path))
}

if (!file.exists(ent_input_path))
  stop("[ENT Table1] Input file not found: ", ent_input_path)

# ── Run ───────────────────────────────────────────────────────────────────────
out_dir <- here::here("artifacts", "ent")

build_specialty_table1(
  npi_csv_path          = ent_input_path,
  specialty_label       = "Otolaryngology (ENT)",
  subspecialty_col      = "ent_subspecialty",
  subspecialty_labels   = ENT_SUBSPECIALTY_LABELS,
  source_label          = paste0(
    "enthealth.org roster (scraped Feb 2026, n = 7,911); ",
    "NPI-matched via NPPES Feb 2026 flat file; ",
    "board-cert validated via ABOto/ABOHNS certification records; ",
    "AAO-HNS member list (n = 7,217 NPI-matched)"
  ),
  out_dir               = out_dir,
  general_taxonomy_code = "207Y00000X",
  region_specialty      = "ent",
  verbose               = TRUE
)
