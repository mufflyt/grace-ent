#!/usr/bin/env Rscript
#' @title ENT Physician Characteristics — Table 1 (Convenience Wrapper)
#'
#' @description
#' Thin wrapper around \code{manuscript/R/create_table1_ent_characteristics.R}
#' that delegates to \code{\link{build_specialty_table1}}.  Prefer running the
#' manuscript script directly; this script exists for historical invocation
#' compatibility.
#'
#' Reads the most recent SSOT master roster from \code{data/ent/} and emits
#' a publication-quality Word table and enriched machine-readable CSV.
#'
#' @section Input:
#' \preformatted{
#' data/ent/ent_master_roster_ssot.csv           (no-timestamp symlink, if
#' present)
#'   OR most recent: data/ent/ent_master_roster_ssot_<timestamp>.csv
#' data/ent/enthealth_profiles_LATEST.csv        (optional — ENTHealth profile
#' metadata)
#' }
#'
#' @section Output:
#' \preformatted{
#' artifacts/ent/table1_otolaryngology__ent_.docx      (Word, gtsummary)
#' artifacts/ent/table1_otolaryngology__ent_.csv       (enriched, 129 cols)
#' artifacts/ent/table1_otolaryngology__ent__coverage.csv
#' }
#'
#' @section Invariants:
#' \itemize{
#'   \item Subspecialty column read from \code{ent_subspecialty}; must be
#'     present in SSOT (populated by \code{R/apply_ent_ssot_subspecialty.R})
#'   \item \code{general_taxonomy_code = "207Y00000X"} selects General ENT
#'     as the reference subspecialty for gtsummary stratification
#'   \item \code{region_specialty = "ent"} routes AAO-HNS BOG region
#'     assignment in the specialty crosswalk
#' }
#'
#' @section Failure Modes:
#' \describe{
#'   \item{Hard-coded no-timestamp path absent}{The original path
#'     \code{data/ent/ent_master_roster_ssot.csv} (no timestamp) no longer
#'     exists after the 2026-05-07 SSOT migration to timestamped files.
#'     Use \code{manuscript/R/create_table1_ent_characteristics.R} which
#'     auto-resolves the most recent timestamped file via \code{Sys.glob()}.}
#'   \item{ent_subspecialty column absent}{If the SSOT predates
#'     \code{R/apply_ent_ssot_subspecialty.R}, subspecialty rows will all
#'     appear as NA.  Re-run the SSOT build step first.}
#' }
#'
#' @section Mental Model:
#' \preformatted{
#' This script  ──►  build_specialty_table1()  ──►  enriched CSV + docx
#'
#' Prefer: manuscript/R/create_table1_ent_characteristics.R
#'         (auto-resolves timestamped SSOT, richer source_label,
#' region_specialty)
#' }
#'
#' @section Usage:
#' \preformatted{
#' Rscript scripts/create_ent_table1.R
#'
#' # Preferred alternative:
#' Rscript manuscript/R/create_table1_ent_characteristics.R
#' }
#'
#' @family ent-reporting
#' @concept table1
#' @seealso \code{manuscript/R/create_table1_ent_characteristics.R} preferred entry point
#' @seealso \code{R/create_specialty_table1.R} for \code{build_specialty_table1()}
#' @seealso \code{scripts/create_ent_table1_slides.R} for slide-ready compact output

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
})

source(here::here("R", "create_specialty_table1.R"))

# ── Paths ─────────────────────────────────────────────────────────────────────
SSOT_CSV    <- here::here("data", "ent", "ent_master_roster_ssot.csv")
PROFILE_CSV <- here::here("data", "ent", "enthealth_profiles_LATEST.csv")
OUT_DIR     <- here::here("artifacts", "ent")

if (!file.exists(SSOT_CSV))
  stop("SSOT not found — run scripts/run_ent_ssot_standalone.R first:\n  ",
       SSOT_CSV, call. = FALSE)

cat("\n=======================================================================\n")
cat("  ENT TABLE 1\n")
cat(sprintf("  Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=======================================================================\n\n")

ssot <- readr::read_csv(SSOT_CSV, show_col_types = FALSE)
cat(sprintf("SSOT loaded: %s rows, %d with NPI (%.1f%%)\n",
            format(nrow(ssot), big.mark = ","),
            sum(!is.na(ssot$npi)),
            100 * mean(!is.na(ssot$npi))))

# ── Subspecialty factor labels ─────────────────────────────────────────────────
ENT_SUBSPEC_LABELS <- c(
  GEN_ENT    = "General Otolaryngology",
  FPRS       = "Facial Plastic & Reconstructive Surgery",
  H_N_ONCO   = "Head & Neck Surgery / Oncology",
  PED_ENT    = "Pediatric Otolaryngology",
  OTOL_NEURO = "Otology / Neurotology",
  LARYNGOL   = "Laryngology",
  RHINOL     = "Rhinology",
  SLEEP      = "Sleep Medicine",
  ALLERGY    = "Otolaryngic Allergy"
)

# ── Generate Table 1 ───────────────────────────────────────────────────────────
build_specialty_table1(
  npi_csv_path          = SSOT_CSV,
  specialty_label       = "Otolaryngology (ENT)",
  subspecialty_col      = "ent_subspecialty",
  subspecialty_labels   = ENT_SUBSPEC_LABELS,
  source_label          = "enthealth.org (scraped Feb 2026)",
  out_dir               = OUT_DIR,
  general_taxonomy_code = "207Y00000X",
  profile_csv_path      = if (file.exists(PROFILE_CSV)) PROFILE_CSV else NULL
)

cat("\n=======================================================================\n")
cat("  ENT TABLE 1 — COMPLETE\n")
cat(sprintf("  Output: %s\n", file.path(OUT_DIR, "table1_otolaryngology__ent_.docx")))
cat("=======================================================================\n\n")
