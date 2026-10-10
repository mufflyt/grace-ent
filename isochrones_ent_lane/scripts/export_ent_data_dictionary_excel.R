#!/usr/bin/env Rscript
# export_ent_data_dictionary_excel.R
#
# Produces a formatted Excel workbook of the ENT master roster data dictionary,
# modeled on the CADR/Lowder AUGS data dictionary format (January 2023).
#
# Output:
#   artifacts/ent/ent_data_dictionary_LATEST.xlsx
#   artifacts/ent/ent_data_dictionary_<timestamp>.xlsx
#
# Usage:
#   Rscript scripts/export_ent_data_dictionary_excel.R

suppressPackageStartupMessages({
  library(here)
  library(openxlsx)
  library(readr)
  library(dplyr)
})

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ── Build data dictionary ─────────────────────────────────────────────────────
source(here("scripts", "build_ent_data_dictionary.R"), local = TRUE)

# ── Workbook setup ────────────────────────────────────────────────────────────
wb <- createWorkbook()
addWorksheet(wb, "Data Dictionary")
addWorksheet(wb, "File Code Legend")

# ── Shared styles ─────────────────────────────────────────────────────────────
title_style <- createStyle(
  fontSize = 14, fontColour = "#FFFFFF", fontName = "Calibri",
  fgFill = "#1F4E79", halign = "LEFT", valign = "center",
  textDecoration = "bold"
)
label_style <- createStyle(
  fontSize = 10, fontColour = "#1F4E79", fontName = "Calibri",
  fgFill = "#D6E4F0", halign = "LEFT", valign = "center",
  textDecoration = "bold"
)
value_style <- createStyle(
  fontSize = 10, fontColour = "#1F2D40", fontName = "Calibri",
  fgFill = "#FFFFFF", halign = "LEFT", valign = "top",
  wrapText = TRUE
)
note_style <- createStyle(
  fontSize = 9, fontColour = "#595959", fontName = "Calibri",
  fgFill = "#F2F2F2", halign = "LEFT", valign = "center",
  textDecoration = "italic"
)
header_style <- createStyle(
  fontSize = 10, fontColour = "#FFFFFF", fontName = "Calibri",
  fgFill = "#2E75B6", halign = "CENTER", valign = "center",
  textDecoration = "bold", border = "Bottom", borderColour = "#FFFFFF"
)
odd_row_style <- createStyle(
  fontSize = 9, fontColour = "#1F2D40", fontName = "Calibri",
  fgFill = "#EBF3FB", halign = "LEFT", valign = "top",
  wrapText = TRUE
)
even_row_style <- createStyle(
  fontSize = 9, fontColour = "#1F2D40", fontName = "Calibri",
  fgFill = "#FFFFFF", halign = "LEFT", valign = "top",
  wrapText = TRUE
)
code_style <- createStyle(
  fontSize = 9, fontName = "Courier New",
  fgFill = "#F4F4F4", halign = "LEFT", valign = "top",
  wrapText = TRUE
)
section_style <- createStyle(
  fontSize = 9, fontColour = "#FFFFFF", fontName = "Calibri",
  fgFill = "#5B9BD5", halign = "LEFT", valign = "center",
  textDecoration = "bold"
)
legend_header_style <- createStyle(
  fontSize = 10, fontColour = "#FFFFFF", fontName = "Calibri",
  fgFill = "#1F4E79", halign = "CENTER", valign = "center",
  textDecoration = "bold"
)
legend_row_style <- createStyle(
  fontSize = 10, fontColour = "#1F2D40", fontName = "Calibri",
  fgFill = "#FFFFFF", halign = "LEFT", valign = "top",
  wrapText = TRUE
)
legend_alt_style <- createStyle(
  fontSize = 10, fontColour = "#1F2D40", fontName = "Calibri",
  fgFill = "#EBF3FB", halign = "LEFT", valign = "top",
  wrapText = TRUE
)

# ══════════════════════════════════════════════════════════════════════════════
# Sheet 1: Data Dictionary
# ══════════════════════════════════════════════════════════════════════════════
ws <- "Data Dictionary"

# ── Header block (rows 1–13), mirroring CADR format ──────────────────────────
header_data <- list(
  c("DATA DICTIONARY", "", "", "", "", "", ""),
  c("Project:",
    "ENT Physician Workforce and Access Study — Mystery-Caller Protocol",
    "", "", "", "", ""),
  c("Investigator:", "Tyler Muffly, MD  |  tyler.muffly@dhha.org", "", "", "", "", ""),
  c("Data Version:", sprintf("Master roster run: May 7, 2026  |  Dictionary exported: %s",
                             format(Sys.Date(), "%B %d, %Y")), "", "", "", "", ""),
  c("Cohort:", "5,599 US otolaryngologists sourced from ENThealth.org (February 2026)", "", "", "", "", ""),
  c("Roster File:", "data/ent/ent_master_roster_ssot.csv", "", "", "", "", ""),
  c("Dictionary File:", "artifacts/ent/ent_data_dictionary_LATEST.xlsx", "", "", "", "", ""),
  c("",             "",                       "", "", "", "", ""),
  c("Data Sources:", "", "", "", "", "", ""),
  c("  ENT",   "ENThealth.org physician directory + profile pages (accessed February–March 2026)", "", "", "", "", ""),
  c("  NPPES", "CMS NPI Registry — February 2026 flat file (npidata_pfile_20050523-20260208.csv)", "", "", "", "", ""),
  c("  ABOto", "ABOto verification portal + ABMS Certification Matters portal + bulk ABOto download; hand search finished May 5, 2026", "", "", "", "", ""),
  c("  AAO",   "AAO-HNS active member roster provided by Tina Maggio, Chief Communications Officer (roster date: March 5, 2026)", "", "", "", "", ""),
  c("  Geo",   "US Census Bureau Geocoding Service (primary) + ArcGIS REST API (fallback); accessed February–March 2026; Vintage 2020 benchmark", "", "", "", "", ""),
  c("  Derived", "Computed from other fields in this dataset", "", "", "", "", ""),
  c("",             "",                       "", "", "", "", ""),
  c("Missing values:", "NA = missing for all fields", "", "", "", "", ""),
  c("",             "",                       "", "", "", "", "")
)

for (i in seq_along(header_data)) {
  writeData(wb, ws, as.data.frame(t(header_data[[i]])),
            startRow = i, startCol = 1, colNames = FALSE)
}

n_header <- length(header_data)

# Style the header block
mergeCells(wb, ws, cols = 1:7, rows = 1)
addStyle(wb, ws, title_style, rows = 1, cols = 1:7, gridExpand = TRUE)

for (r in 2:7) {
  mergeCells(wb, ws, cols = 2:7, rows = r)
  addStyle(wb, ws, label_style, rows = r, cols = 1, gridExpand = TRUE)
  addStyle(wb, ws, value_style, rows = r, cols = 2:7, gridExpand = TRUE)
}

# "Data Sources:" row
addStyle(wb, ws, label_style, rows = 9, cols = 1, gridExpand = TRUE)
mergeCells(wb, ws, cols = 2:7, rows = 9)

# Source legend rows (ENT, NPPES, ABOto, AAO, Geo, Derived)
for (r in 10:15) {
  addStyle(wb, ws, label_style,  rows = r, cols = 1, gridExpand = TRUE)
  mergeCells(wb, ws, cols = 2:7, rows = r)
  addStyle(wb, ws, value_style,  rows = r, cols = 2:7, gridExpand = TRUE)
}

# Missing values row
addStyle(wb, ws, note_style, rows = 17, cols = 1:7, gridExpand = TRUE)
mergeCells(wb, ws, cols = 2:7, rows = 17)

setRowHeights(wb, ws, rows = 1,       heights = 28)
setRowHeights(wb, ws, rows = 2:17,    heights = 18)

# ── Column header row ─────────────────────────────────────────────────────────
col_headers <- c("Variable", "Type", "File\nCode", "Valid Values",
                 "Value Labels / Notes", "Source", "Description")
col_row <- n_header + 1
writeData(wb, ws, as.data.frame(t(col_headers)),
          startRow = col_row, startCol = 1, colNames = FALSE)
addStyle(wb, ws, header_style, rows = col_row, cols = 1:7, gridExpand = TRUE)
setRowHeights(wb, ws, rows = col_row, heights = 30)

# ── Detect section boundaries ─────────────────────────────────────────────────
sections <- list(
  list(label = "IDENTITY",              vars = c("url","full_name","clean_name","first_name","middle_name","last_name","credentials","npi")),
  list(label = "DEGREE / CREDENTIALS",  vars = c("degree_type","is_md","is_do","is_phd","is_facs")),
  list(label = "BOARD CERTIFICATION",   vars = c("is_board_certified","n_board_certs","board_cert_year","years_since_board_cert","board_cert_lapsed","board_names","primary_board","has_abohns","has_facial_plastic","has_neurotology","board_cert")),
  list(label = "PRACTICE LOCATION",     vars = c("company_name","address","address_had_org_prefix","suite","city","state","zip5","zip4","zip5_geo","latitude","longitude","geocoded","geocoder_provenance","matched_address","match_type","confidence_score","country","us_region","acog_district")),
  list(label = "TRAINING",              vars = c("medical_school_name","medical_school_year","residency_name","residency_year","fellowship_name","has_med_school","has_residency","has_fellowship","years_since_residency","career_stage","data_completeness")),
  list(label = "CONTACT",               vars = c("phone_norm","phone_digits","phone_source")),
  list(label = "SUBSPECIALTY ASSIGNMENT", vars = c("specialty_primary","specialty_secondary","specialty_inferred_source","ent_subspecialty","subspecialty","ssot_source","taxonomy_code")),
  list(label = "AAO MEMBERSHIP",        vars = c("is_aao_member","aao_taxonomy"))
)

# Build a lookup: variable name → section label
var_section <- setNames(
  unlist(lapply(sections, function(s) rep(s$label, length(s$vars)))),
  unlist(lapply(sections, `[[`, "vars"))
)

# ── Write data rows ───────────────────────────────────────────────────────────
current_row <- col_row + 1
prev_section <- ""
data_row_idx <- 0   # alternating row counter (reset per section)

for (i in seq_len(nrow(ent_data_dictionary))) {
  var_name <- ent_data_dictionary$column[i]
  sec <- var_section[var_name]
  if (is.na(sec)) sec <- ""

  # Insert section header when section changes
  if (!is.na(sec) && sec != prev_section) {
    writeData(wb, ws, data.frame(V1 = sec, V2="",V3="",V4="",V5="",V6="",V7=""),
              startRow = current_row, startCol = 1, colNames = FALSE)
    mergeCells(wb, ws, cols = 1:7, rows = current_row)
    addStyle(wb, ws, section_style, rows = current_row, cols = 1:7, gridExpand = TRUE)
    setRowHeights(wb, ws, rows = current_row, heights = 16)
    current_row <- current_row + 1
    prev_section <- sec
    data_row_idx <- 0
  }

  data_row_idx <- data_row_idx + 1
  row_style <- if (data_row_idx %% 2 == 1) odd_row_style else even_row_style

  row_vals <- data.frame(
    V1 = ent_data_dictionary$column[i],
    V2 = ent_data_dictionary$type[i],
    V3 = ent_data_dictionary$file_code[i],
    V4 = ent_data_dictionary$values[i],
    V5 = ifelse(is.na(ent_data_dictionary$value_labels[i]), "",
                ent_data_dictionary$value_labels[i]),
    V6 = ent_data_dictionary$source[i],
    V7 = ent_data_dictionary$description[i],
    stringsAsFactors = FALSE
  )
  writeData(wb, ws, row_vals, startRow = current_row, startCol = 1, colNames = FALSE)
  addStyle(wb, ws, code_style,  rows = current_row, cols = 1, gridExpand = TRUE)
  addStyle(wb, ws, row_style,   rows = current_row, cols = 2:7, gridExpand = TRUE)
  setRowHeights(wb, ws, rows = current_row, heights = 52)
  current_row <- current_row + 1
}

# ── Column widths ─────────────────────────────────────────────────────────────
setColWidths(wb, ws, cols = 1, widths = 26)   # Variable
setColWidths(wb, ws, cols = 2, widths = 12)   # Type
setColWidths(wb, ws, cols = 3, widths = 10)   # File Code
setColWidths(wb, ws, cols = 4, widths = 30)   # Valid Values
setColWidths(wb, ws, cols = 5, widths = 40)   # Value Labels
setColWidths(wb, ws, cols = 6, widths = 38)   # Source
setColWidths(wb, ws, cols = 7, widths = 58)   # Description

# Freeze panes below header block + column header
freezePane(wb, ws, firstActiveRow = col_row + 1, firstActiveCol = 2)

# ══════════════════════════════════════════════════════════════════════════════
# Sheet 2: File Code Legend
# ══════════════════════════════════════════════════════════════════════════════
ws2 <- "File Code Legend"

writeData(wb, ws2, data.frame(V1 = "FILE CODE LEGEND", V2 = "", V3 = ""),
          startRow = 1, startCol = 1, colNames = FALSE)
mergeCells(wb, ws2, cols = 1:3, rows = 1)
addStyle(wb, ws2, title_style, rows = 1, cols = 1:3, gridExpand = TRUE)
setRowHeights(wb, ws2, rows = 1, heights = 28)

writeData(wb, ws2,
  data.frame(V1 = "Project: ENT Physician Workforce and Access Study",
             V2 = "", V3 = ""),
  startRow = 2, startCol = 1, colNames = FALSE)
mergeCells(wb, ws2, cols = 1:3, rows = 2)
addStyle(wb, ws2, note_style, rows = 2, cols = 1:3, gridExpand = TRUE)

writeData(wb, ws2,
  data.frame(V1 = sprintf("Exported: %s", format(Sys.Date(), "%B %d, %Y")),
             V2 = "", V3 = ""),
  startRow = 3, startCol = 1, colNames = FALSE)
mergeCells(wb, ws2, cols = 1:3, rows = 3)
addStyle(wb, ws2, note_style, rows = 3, cols = 1:3, gridExpand = TRUE)

# Column headers
writeData(wb, ws2,
  data.frame(Code = "File Code", Full_Name = "Full Name", Description = "Description"),
  startRow = 5, startCol = 1, colNames = FALSE)
addStyle(wb, ws2, legend_header_style, rows = 5, cols = 1:3, gridExpand = TRUE)
setRowHeights(wb, ws2, rows = 5, heights = 22)

legend_rows <- data.frame(
  Code = c("ENT", "NPPES", "ABOto", "AAO", "Geo", "Derived"),
  Full_Name = c(
    "ENThealth.org",
    "NPPES NPI Registry",
    "ABOto / ABMS Certification",
    "AAO-HNS Member Roster",
    "Geocoding Services",
    "Derived / Computed"
  ),
  Description = c(
    "American Academy of Otolaryngology – Head and Neck Surgery (AAO-HNS) public patient portal ENThealth.org. Physician directory accessed February 2026; individual profile pages accessed March 2026 (hand search).",
    "CMS National Plan and Provider Enumeration System (NPPES) NPI Registry. February 2026 public data dissemination. File: npidata_pfile_20050523-20260208.csv. NUCC Health Care Provider Taxonomy version 24.1.",
    "American Board of Otolaryngology – Head and Neck Surgery (ABOto) verification portal (https://abohns.org/verify-certification) + ABMS Certification Matters portal (https://www.certificationmatters.org/) + bulk ABOto download (abohns_certifications.csv). ABOto is a member board of ABMS; both portals reflect the same underlying certification authority. Hand search finished May 5, 2026.",
    "AAO-HNS active member roster provided directly by Tina Maggio, Chief Communications Officer, AAO-HNS. Roster date: March 5, 2026. Includes only US-based, actively practicing otolaryngologists; residents and retired members excluded by AAO-HNS.",
    "US Census Bureau Geocoding Service (https://geocoding.geo.census.gov/geocoder/; Vintage 2020 benchmark) as primary geocoder, with ArcGIS REST API as fallback. Accessed February–March 2026.",
    "Variable computed or derived from other fields in this dataset. No external data source; derivation logic is documented in the Description column and in R/apply_ent_ssot_subspecialty.R."
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(legend_rows))) {
  r <- i + 5
  s <- if (i %% 2 == 1) legend_row_style else legend_alt_style
  writeData(wb, ws2,
    data.frame(V1 = legend_rows$Code[i],
               V2 = legend_rows$Full_Name[i],
               V3 = legend_rows$Description[i]),
    startRow = r, startCol = 1, colNames = FALSE)
  addStyle(wb, ws2, s, rows = r, cols = 1:3, gridExpand = TRUE)
  setRowHeights(wb, ws2, rows = r, heights = 70)
}

setColWidths(wb, ws2, cols = 1, widths = 10)
setColWidths(wb, ws2, cols = 2, widths = 28)
setColWidths(wb, ws2, cols = 3, widths = 80)

# ── Wrap text on legend descriptions ─────────────────────────────────────────
wrap_desc <- createStyle(wrapText = TRUE, valign = "top", fontSize = 10,
                          fontName = "Calibri")
addStyle(wb, ws2, wrap_desc, rows = 6:11, cols = 3, gridExpand = TRUE)

# ── Save ──────────────────────────────────────────────────────────────────────
out_latest    <- here("artifacts", "ent", "ent_data_dictionary_LATEST.xlsx")
out_timestamped <- here("artifacts", "ent",
                        sprintf("ent_data_dictionary_%s.xlsx", ts))
dir.create(dirname(out_latest), recursive = TRUE, showWarnings = FALSE)

tmp_latest      <- paste0(out_latest,      ".tmp")
tmp_timestamped <- paste0(out_timestamped, ".tmp")
saveWorkbook(wb, tmp_latest,      overwrite = TRUE)
saveWorkbook(wb, tmp_timestamped, overwrite = TRUE)
file.rename(tmp_latest,      out_latest)
file.rename(tmp_timestamped, out_timestamped)

cat(sprintf("\nExported data dictionary Excel workbook:\n  %s\n  %s\n",
            out_latest, out_timestamped))
cat(sprintf("  %d variables across 2 sheets (Data Dictionary + File Code Legend)\n",
            nrow(ent_data_dictionary)))
