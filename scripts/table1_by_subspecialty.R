#!/usr/bin/env Rscript
# =============================================================================
# Table 1 stratified BY SUBSPECIALTY (subspecialties as column headers), the
# standard epidemiology "Table 1 by group" layout. Characteristics run down the
# rows; each requested ENT subspecialty is a column, plus an Overall column.
#
# Output: manuscript/table1_by_subspecialty.html
# =============================================================================

suppressPackageStartupMessages({library(gtsummary); library(gt); library(dplyr)})
options(stringsAsFactors = FALSE)
dir.create("manuscript", showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]

# column order: General (reference) first, then remaining by frequency
lv <- c("General", setdiff(names(sort(table(d$ent_type), decreasing = TRUE)), "General"))
tb <- tibble::tibble(
  Subspecialty = factor(d$ent_type, levels = lv),
  offered = factor(ifelse(d$appointment_offered == "TRUE", "Offered", "Not offered"),
                   levels = c("Offered", "Not offered")),
  wait = ifelse(d$appointment_offered == "TRUE", num(d$wait_days_business), NA_real_),
  Rurality = factor(d$ruca_category, levels = c("Urban", "Rural")),
  ent_per_100k = num(d$ent_per_100k),
  medicaid_fee_index = num(d$medicaid_fee_index),
  svi_overall = num(d$svi_overall),
  dual_pct = num(d$dual_pct))

tbl <- tb |>
  tbl_summary(
    by = Subspecialty,
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(ent_per_100k ~ 1, medicaid_fee_index ~ 2, svi_overall ~ 2, dual_pct ~ 2),
    label = list(
      offered ~ "Appointment offered",
      wait ~ "Business-day wait, if offered",
      Rurality ~ "Rurality",
      ent_per_100k ~ "ENT per 100,000",
      medicaid_fee_index ~ "Medicaid fee index",
      svi_overall ~ "Social Vulnerability Index",
      dual_pct ~ "Dual-eligible fraction"),
    missing = "no") |>
  add_overall(last = FALSE) |>
  modify_header(label ~ "**Characteristic**") |>
  modify_spanning_header(all_stat_cols() ~ "**Requested subspecialty**") |>
  modify_caption("**Table 1. Sample characteristics by requested otolaryngology subspecialty.**")

gt_tbl <- tbl |> as_gt() |>
  gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(3),
                  column_labels.font.weight = "bold") |>
  gt::tab_source_note(gt::md(
    "Continuous variables: median (Q1, Q3). Categorical: n (%). Analytic sample of complete calls with a determinable subspecialty."))

gt::gtsave(gt_tbl, "manuscript/table1_by_subspecialty.html")
cat("Wrote manuscript/table1_by_subspecialty.html (", nrow(tb), "calls, ",
    length(lv), "subspecialty columns + Overall)\n")
