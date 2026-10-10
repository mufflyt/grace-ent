#!/usr/bin/env Rscript
# =============================================================================
# ENT Data-Integrity Supplementary Tables — Anomaly Roster CSVs
# =============================================================================
# Exports record-level rosters for every data-quality anomaly that the
# figure suite counts but does not display.  A reviewer can spot-check the
# numbers in the figures against the CSVs here.
#
# Counts come from R/ent_registry_counts.R$anomalies (single source of
# truth) so these tables match the figures by construction.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(here)
})

source(here::here("R", "ent_registry_counts.R"))
source(here::here("R", "figure_receipt.R"))
ct <- compute_ent_registry_counts()

supp_dir <- here::here("publication_materials", "figures", "ent_data_integrity",
                       "supplementary")
dir.create(supp_dir, recursive = TRUE, showWarnings = FALSE)

# ── Write each anomaly roster ─────────────────────────────────────────────────
tables <- list(
  abohns_dup_npis_117 = list(
    df    = ct$anomalies$abohns_dup_npis,
    file  = "abohns_duplicate_npis_117.csv",
    desc  = "NPIs appearing in 2+ ABOto rows (different abohns_id)"
  ),
  board_only_no_nppes_12 = list(
    df    = ct$anomalies$board_no_nppes,
    file  = "abohns_active_nppes_non_conus_only_12.csv",
    desc  = "Active board-cert (CONUS per ABOto) whose NPPES practice address is non-CONUS (state-mismatch across registries; physicians ARE in NPPES but outside the CONUS-filtered subset)"
  ),
  enthealth_no_npi_247 = list(
    df    = ct$anomalies$ent_no_npi,
    file  = "enthealth_no_npi_listed_247.csv",
    desc  = "ENThealth directory rows with no NPI listed"
  ),
  enthealth_npi_not_in_nppes = list(
    df    = ct$anomalies$ent_npi_not_in_nppes,
    file  = "enthealth_npi_not_in_nppes.csv",
    desc  = "ENThealth physicians whose NPI is absent from NPPES 207Y*"
  ),
  zone_712 = list(
    df    = ct$anomalies$zone_712,
    file  = "nppes_enthealth_not_in_active_cohort_712.csv",
    desc  = "NPIs in NPPES + ENThealth but not in our active board-cert cohort (split into ABOto-listed-inactive vs absent-from-ABOto)"
  )
)

for (key in names(tables)) {
  tbl <- tables[[key]]
  out_csv <- file.path(supp_dir, tbl$file)
  tmp_csv <- paste0(out_csv, ".tmp")
  write_csv(tbl$df, tmp_csv)
  file.rename(tmp_csv, out_csv)
  cat(sprintf("  %-50s %5d rows  %s\n",
              tbl$file, nrow(tbl$df), tbl$desc))
}

# ── README in the supplementary folder ────────────────────────────────────────
readme_path <- file.path(supp_dir, "README.md")
tmp_readme <- paste0(readme_path, ".tmp")
writeLines(c(
  "# ENT Data-Integrity Supplementary Tables",
  "",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("Git commit: %s", .git_sha()),
  "",
  "Record-level rosters for every data-quality anomaly counted by the",
  "ENT figure suite. Use these to spot-check the figures or audit our",
  "exclusion choices.",
  "",
  "## Files",
  "",
  paste0("- `", sapply(tables, `[[`, "file"), "` — ",
         sapply(tables, `[[`, "desc"),
         " (n = ", sapply(tables, function(t) nrow(t$df)), ")"),
  "",
  "## Reproduce",
  "",
  "```bash",
  "Rscript scripts/export_ent_supplementary_anomalies.R",
  "```",
  "",
  "Counts come from `R/ent_registry_counts.R::compute_ent_registry_counts()`",
  "(single source of truth shared with all four figure scripts).",
  "",
  "## How to verify against the figures",
  "",
  "- `ent_cohort_strobe_v2.png` exclusion box \"Duplicate NPI rows collapsed",
  "  (n = 127)\" — the file `abohns_duplicate_npis_117.csv` contains 244 rows,",
  "  which is **every row** for each of the 117 duplicated NPIs (a single",
  "  NPI can appear 2 or 3 times in ABOto). Of those 244 rows, 127 are the",
  "  *extra* rows that get collapsed by the dedup step (244 raw - 117",
  "  retained = 127 collapsed). The filename refers to the 117 distinct NPIs,",
  "  not the row count.",
  "- `ent_registry_overlap_euler_v2.png` zone \"Board (CONUS), NPPES non-CONUS only\" (12) =",
  "  `abohns_active_nppes_non_conus_only_12.csv`. Important: all 12 physicians",
  "  ARE in the full NPPES file — they're just outside our CONUS-filtered subset",
  "  because their NPPES practice address is in HI/AK/PR/territories while",
  "  their ABOto state is CONUS. State-mismatch between registries.",
  "- `ent_registry_overlap_euler_v2.png` zone \"ENThealth Only (no NPI) 248\" =",
  "  `enthealth_no_npi_listed_247.csv` (247) + `enthealth_npi_not_in_nppes.csv` (1).",
  "- `ent_registry_overlap_euler_v2.png` zone \"ENThealth & NPI 712\" =",
  "  `nppes_enthealth_not_in_active_cohort_712.csv`."
), tmp_readme)
file.rename(tmp_readme, readme_path)
cat(sprintf("  %-50s README\n", basename(readme_path)))

cat("\nSupplementary tables written to:\n  ", supp_dir, "\n")
