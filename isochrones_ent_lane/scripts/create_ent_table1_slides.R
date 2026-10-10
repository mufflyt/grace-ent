#!/usr/bin/env Rscript
#' @title ENT Table 1 — Slide-Ready Compact Version
#'
#' @description
#' Produces a condensed, PowerPoint-ready Table 1 of otolaryngologist
#' characteristics from the enriched physician CSV written by
#' \code{manuscript/R/create_table1_ent_characteristics.R}.
#'
#' Outputs a single \code{.docx} with a \code{flextable} (45 rows × 2 columns,
#' Calibri 10 pt) sized to paste into two PowerPoint slides without
#' reformatting.  Suggested split: rows 1–26 (Demographics → Subspecialty)
#' on slide 1; rows 27–45 (Geography + Medicare Volume) on slide 2.
#'
#' @section Input:
#' \preformatted{
#' artifacts/ent/table1_otolaryngology__ent_.csv   (5,311 rows × 129 cols)
#' }
#' This is the \strong{enriched} output of \code{build_specialty_table1()},
#' NOT the raw SSOT.  The raw SSOT lacks \code{gender}, \code{ruca_category},
#' \code{professional_region}, \code{medicare_volume_quartile}, and
#' \code{img_status} — using it produces all-zero rows for those variables.
#'
#' @section Output:
#' \preformatted{
#' artifacts/ent/table1_ent_slides.docx   (≈ 15 KB, flextable in Word)
#' }
#'
#' @section Invariants:
#' \itemize{
#'   \item N = 5,311 (NPI-matched physicians, source: ENTHealth.org Feb 2026)
#'   \item \code{gender} values are \code{"Female"} / \code{"Male"}
#' (NPPES-enriched)
#'   \item \code{img_status} uses long labels: \code{"US Allopathic (MD)"},
#'     \code{"US Osteopathic (DO)"}, \code{"Canadian Graduate"}, \code{"Other
#' IMG"}
#'   \item \code{professional_region} uses en-dashes (\code{–}), not hyphens;
#'     \code{grepl("Region N", ...)} matches correctly regardless
#'   \item \code{ruca_category} = \code{"Urban"} / \code{"Large Rural"} /
#'     \code{"Small Rural/Isolated"} (forward slash, not en-dash)
#'   \item Five \code{stopifnot()} guards prevent silent 0-row columns if the
#'     enriched CSV is replaced with the raw SSOT by mistake
#' }
#'
#' @section Failure Modes:
#' \describe{
#'   \item{Wrong input file}{If \code{table1_otolaryngology__ent_.csv} is
#' absent,
#'     \code{stop()} fires with a remediation message pointing to the upstream
#'     script.  Do NOT substitute the raw SSOT — it will produce 0\% Female,
#'     0\% Urban, and 0\% for all Medicare rows without any warning.}
#'   \item{Upstream enrichment not run}{Re-run
#'     \code{manuscript/R/create_table1_ent_characteristics.R} first, which
#'     sources \code{R/create_specialty_table1.R} and writes the enriched CSV.}
#'   \item{Column-name drift}{If \code{build_specialty_table1()} renames an
#'     enrichment column, the \code{stopifnot()} guards will catch it
#' immediately
#'     rather than silently emitting zeros.}
#'   \item{Flextable / officer version mismatch}{\code{fp_border()} lives in
#'     \code{officer}; if it moves, replace with \code{officer::fp_border()}.}
#' }
#'
#' @section Mental Model:
#' The pipeline has two distinct outputs that look similar but are very
#' different:
#' \preformatted{
#' Raw SSOT (73 cols)             Enriched CSV (129 cols)
#' ─────────────────────────────  ──────────────────────────────────────
#' ent_master_roster_ssot_*.csv   table1_otolaryngology__ent_.csv
#' No gender, no RUCA, no region  Has gender (NPPES), RUCA, AAO-HNS region,
#' No Medicare volume             Medicare volume, img_status, practice type
#' Used for cohort construction   Used for Table 1 reporting
#' }
#' This script always reads the enriched CSV.  All pct() helpers divide by
#' \code{N = nrow(d)} so rows sum to 100\% within each section.
#'
#' @section Usage:
#' \preformatted{
#' # Prerequisite
#' Rscript manuscript/R/create_table1_ent_characteristics.R
#'
#' # Generate slide table
#' Rscript scripts/create_ent_table1_slides.R
#' }
#'
#' @section Variables Included vs. Excluded:
#' \preformatted{
#' INCLUDED                        EXCLUDED (too sparse or non-informative)
#' ──────────────────────────────  ────────────────────────────────────────
#' Gender                          Practice stability (7.5% coverage)
#' Age (median IQR)                Underserved area (0% informative)
#' Degree (MD/DO)                  Advanced degree (minor variable)
#' IMG status                      Years since residency (redundant)
#' Board certification             Residency trained in practice state
#' Fellowship training             Group practice size detail
#' Career stage                    Multiple practice locations
#' Subspecialty (9 groups)
#' RUCA rurality
#' AAO-HNS region (9)
#' Medicare volume quartile
#' }
#'
#' @family ent-reporting
#' @concept table1
#' @concept slides
#' @seealso \code{manuscript/R/create_table1_ent_characteristics.R} upstream enrichment
#' @seealso \code{R/create_specialty_table1.R} for \code{build_specialty_table1()}
#' @seealso \code{scripts/ent_table_1.R} for the full gtsummary version (DuckDB-enriched)

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(flextable)
  library(officer)
})

# ── Load enriched physician data ──────────────────────────────────────────────
# Must use the enriched CSV — raw SSOT lacks gender, ruca_category,
# professional_region, medicare_volume_quartile, img_status, etc.
enriched_path <- here("artifacts", "ent", "table1_otolaryngology__ent_.csv")
if (!file.exists(enriched_path))
  stop("Run manuscript/R/create_table1_ent_characteristics.R first:\n  ", enriched_path)

d <- read_csv(enriched_path, show_col_types = FALSE)
N <- nrow(d)
if (N == 0L) stop("Enriched CSV is empty — re-run manuscript/R/create_table1_ent_characteristics.R")
cat(sprintf("N = %s physicians\n", format(N, big.mark = ",")))

# ── Sanity check key columns ──────────────────────────────────────────────────
stopifnot("gender"                  %in% names(d))
stopifnot("ruca_category"           %in% names(d))
stopifnot("professional_region"     %in% names(d))
stopifnot("medicare_volume_quartile" %in% names(d))
stopifnot("img_status"              %in% names(d))

# ── Helpers ───────────────────────────────────────────────────────────────────
pct     <- function(n) sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / N)
med_iqr <- function(x) {
  x <- x[!is.na(x)]
  sprintf("%.0f (%.0f–%.0f)", median(x), quantile(x, .25), quantile(x, .75))
}
med_iqr2 <- function(x) {
  x <- x[!is.na(x)]
  sprintf("%.0f (%.0f–%.0f)", median(x), quantile(x, .25), quantile(x, .75))
}

# ── Build summary rows ────────────────────────────────────────────────────────
rows <- tibble::tribble(
  ~Characteristic,                              ~`N (%)`,

  "Otolaryngologists, N",                        format(N, big.mark = ","),

  "DEMOGRAPHICS",                                "",
  "  Female",                                    pct(sum(d$gender == "Female", na.rm = TRUE)),
  "  Age, median (IQR), years",                  med_iqr2(d$age_final),

  "TRAINING",                                    "",
  "  MD degree",                                 pct(sum(d$is_md,  na.rm = TRUE)),
  "  DO degree",                                 pct(sum(d$is_do,  na.rm = TRUE)),
  "  US Medical Graduate",                       pct(sum(d$img_status %in% c("US Allopathic (MD)", "US Osteopathic (DO)"), na.rm = TRUE)),
  "  International Medical Graduate",            pct(sum(d$img_status %in% c("Canadian Graduate", "Other IMG"), na.rm = TRUE)),
  "  Board certified (ABOto/ABOHNS)",            pct(sum(d$is_board_certified == TRUE, na.rm = TRUE)),
  "  ABOHNS certification",                      pct(sum(d$has_abohns == TRUE, na.rm = TRUE)),
  "  Fellowship trained",                        pct(sum(d$has_fellowship == TRUE, na.rm = TRUE)),

  "CAREER STAGE",                                "",
  "  Early career (≤10 years)",             pct(sum(d$career_stage == "Early (<=10 yrs)",  na.rm = TRUE)),
  "  Mid career (11–25 years)",             pct(sum(d$career_stage == "Mid (11-25 yrs)",   na.rm = TRUE)),
  "  Senior (>25 years)",                        pct(sum(d$career_stage == "Senior (>25 yrs)",  na.rm = TRUE)),

  "SUBSPECIALTY",                                "",
  "  General Otolaryngology",                    pct(sum(d$ent_subspecialty == "GEN_ENT",    na.rm = TRUE)),
  "  Head & Neck Oncology",                      pct(sum(d$ent_subspecialty == "H_N_ONCO",   na.rm = TRUE)),
  "  Facial Plastic & Reconstructive",           pct(sum(d$ent_subspecialty == "FPRS",       na.rm = TRUE)),
  "  Pediatric Otolaryngology",                  pct(sum(d$ent_subspecialty == "PED_ENT",    na.rm = TRUE)),
  "  Otology / Neurotology",                     pct(sum(d$ent_subspecialty == "OTOL_NEURO", na.rm = TRUE)),
  "  Rhinology",                                 pct(sum(d$ent_subspecialty == "RHINOL",     na.rm = TRUE)),
  "  Laryngology",                               pct(sum(d$ent_subspecialty == "LARYNGOL",   na.rm = TRUE)),
  "  Sleep Medicine",                            pct(sum(d$ent_subspecialty == "SLEEP",      na.rm = TRUE)),
  "  Otolaryngic Allergy",                       pct(sum(d$ent_subspecialty == "ALLERGY",    na.rm = TRUE)),

  "GEOGRAPHY",                                   "",
  "  Urban",                                     pct(sum(d$ruca_category == "Urban",               na.rm = TRUE)),
  "  Large Rural",                               pct(sum(d$ruca_category == "Large Rural",          na.rm = TRUE)),
  "  Small Rural / Isolated",                    pct(sum(d$ruca_category == "Small Rural/Isolated", na.rm = TRUE)),
  "  Region 1 – New England",              pct(sum(grepl("Region 1", d$professional_region), na.rm = TRUE)),
  "  Region 2 – Mid-Atlantic",             pct(sum(grepl("Region 2", d$professional_region), na.rm = TRUE)),
  "  Region 3 – South Atlantic",           pct(sum(grepl("Region 3", d$professional_region), na.rm = TRUE)),
  "  Region 4 – Great Lakes",              pct(sum(grepl("Region 4", d$professional_region), na.rm = TRUE)),
  "  Region 5 – South Central",            pct(sum(grepl("Region 5", d$professional_region), na.rm = TRUE)),
  "  Region 6 – North Central",            pct(sum(grepl("Region 6", d$professional_region), na.rm = TRUE)),
  "  Region 7 – Southwest",               pct(sum(grepl("Region 7", d$professional_region), na.rm = TRUE)),
  "  Region 8 – Pacific Coast",            pct(sum(grepl("Region 8", d$professional_region), na.rm = TRUE)),
  "  Region 9 – Mountain West",            pct(sum(grepl("Region 9", d$professional_region), na.rm = TRUE)),

  "MEDICARE VOLUME",                             "",
  "  No Medicare billing",                       pct(sum(d$medicare_volume_quartile == "No Medicare Billing",  na.rm = TRUE)),
  "  Lowest volume (Q1)",                        pct(sum(d$medicare_volume_quartile == "Lowest Volume (Q1)",   na.rm = TRUE)),
  "  Q2",                                        pct(sum(d$medicare_volume_quartile == "Q2",                   na.rm = TRUE)),
  "  Q3",                                        pct(sum(d$medicare_volume_quartile == "Q3",                   na.rm = TRUE)),
  "  Highest volume (Q4)",                       pct(sum(d$medicare_volume_quartile == "Highest Volume (Q4)",  na.rm = TRUE))
)

# Quick sanity: print every row so we can spot any remaining 0s
cat("\n--- Row preview ---\n")
print(as.data.frame(rows), right = FALSE)
cat("---\n\n")

# ── Section header row indices ────────────────────────────────────────────────
hdr_rows <- which(rows$`N (%)` == "" & rows$Characteristic != "Otolaryngologists, N")

# ── Flextable ─────────────────────────────────────────────────────────────────
ft <- flextable(rows) |>
  set_header_labels(Characteristic = "Characteristic", `N (%)` = "N (%)") |>
  font(fontname = "Calibri", part = "all") |>
  fontsize(size = 10, part = "body") |>
  fontsize(size = 11, part = "header") |>
  bold(part = "header") |>
  bold(i = hdr_rows) |>
  bold(i = 1) |>
  bg(i = hdr_rows, bg = "#D6E4F0") |>
  width(j = 1, width = 3.8) |>
  width(j = 2, width = 1.6) |>
  align(j = 2, align = "right", part = "all") |>
  align(j = 1, align = "left",  part = "all") |>
  hline_top(border    = fp_border(color = "#2C5F8A", width = 2), part = "header") |>
  hline_bottom(border = fp_border(color = "#2C5F8A", width = 2), part = "header") |>
  hline_bottom(border = fp_border(color = "#2C5F8A", width = 1.5), part = "body") |>
  border_inner_h(border = fp_border(color = "#CCCCCC", width = 0.5), part = "body") |>
  padding(padding.top = 3, padding.bottom = 3,
          padding.left = 6, padding.right = 6, part = "all")

# ── Write docx ────────────────────────────────────────────────────────────────
out_path <- here("artifacts", "ent", "table1_ent_slides.docx")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
doc <- read_docx() |>
  body_add_par(
    sprintf("Table 1. Characteristics of U.S. Otolaryngologists (N = %s)",
            format(N, big.mark = ",")),
    style = "heading 1"
  ) |>
  body_add_par(
    paste0("Source: ENTHealth.org physician directory (Feb 2026), ",
           "NPI-matched via NPPES Feb 2026. ",
           "Suggested split: Slide 1 = Demographics through Subspecialty (rows 1–26); ",
           "Slide 2 = Geography + Medicare Volume (rows 27–45)."),
    style = "Normal"
  ) |>
  body_add_par("", style = "Normal") |>
  body_add_flextable(ft) |>
  body_add_par("", style = "Normal") |>
  body_add_par(
    paste0("ABOto = American Board of Otolaryngology; ",
           "ABOHNS = American Board of Otolaryngology–Head & Neck Surgery; ",
           "IQR = interquartile range; IMG = international medical graduate; ",
           "AAO-HNS = American Academy of Otolaryngology–Head and Neck Surgery; ",
           "RUCA = Rural-Urban Commuting Area (USDA 2020); Q = Medicare volume quartile (Part B claims 2022)."),
    style = "Normal"
  )

tmp_out_path <- paste0(out_path, ".tmp.docx")
print(doc, target = tmp_out_path)
file.rename(tmp_out_path, out_path)
cat(sprintf("Saved: %s\n", out_path))
