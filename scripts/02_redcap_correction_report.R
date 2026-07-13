# =============================================================================
# 02_redcap_correction_report.R
# ENT Mystery Caller Study — REDCap Data Correction Report
#
# Input:  data/processed/ent_phase2_all_*.csv (latest)
# Output: data/processed/redcap_correction_report_<timestamp>.xlsx
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(lubridate)
library(openxlsx)

# -----------------------------------------------------------------------------
# Load latest cleaned file
# -----------------------------------------------------------------------------
f <- tail(sort(list.files(here("data", "processed"),
                          pattern = "ent_phase2_all", full.names = TRUE)), 1)
message("Loading: ", f)
df <- read_csv(f, show_col_types = FALSE)
message(sprintf("  %d rows x %d columns", nrow(df), ncol(df)))

# -----------------------------------------------------------------------------
# Helper: build an issue tibble
# -----------------------------------------------------------------------------
issue <- function(data, flag, priority, description, action) {
  if (nrow(data) == 0) return(NULL)
  data |>
    mutate(
      flag        = flag,
      priority    = priority,
      description = description,
      action      = action
    )
}

# -----------------------------------------------------------------------------
# Issue 1 — Impossible wait time (year typo)
# -----------------------------------------------------------------------------
i1 <- df |>
  filter(!is.na(wait_days_business) & wait_days_business > 365) |>
  select(record_id, caller, last_name, state, call_date, appointment_date, wait_days_business) |>
  issue(
    flag        = "IMPOSSIBLE_WAIT_TIME",
    priority    = "CRITICAL",
    description = "Appointment date >1 year after call — likely year typo",
    action      = "Correct appointment_date in REDCap (probable year error: 2029 -> 2026)"
  )

# -----------------------------------------------------------------------------
# Issue 2 — Office answered "No" but appointment date entered
# -----------------------------------------------------------------------------
i2 <- df |>
  filter(office_answered == FALSE & !is.na(appointment_date)) |>
  select(record_id, caller, last_name, state, office_answered,
         taking_new_patients, appointment_date) |>
  issue(
    flag        = "ANSWER_CONFLICT",
    priority    = "HIGH",
    description = "office_answered = No but an appointment date was recorded",
    action      = "Clarify with caller: should office_answered be Yes?"
  )

# -----------------------------------------------------------------------------
# Issue 3 — Taking new patients = Yes but no appointment date or outcome
# -----------------------------------------------------------------------------
i3 <- df |>
  filter(
    taking_new_patients == "Yes" &
    is.na(appointment_date) &
    (is.na(appointment_with_physician) |
     appointment_with_physician == "Not applicable, no appointment offered with anyone in practice")
  ) |>
  select(record_id, caller, last_name, state, taking_new_patients,
         appointment_with_physician, appointment_date) |>
  issue(
    flag        = "MISSING_APPT_DATE",
    priority    = "HIGH",
    description = "taking_new_patients = Yes but no appointment date or outcome recorded",
    action      = "Back-fill appointment_date and appointment_with_physician in REDCap"
  )

# -----------------------------------------------------------------------------
# Issue 4 — Complete records missing call_date
# -----------------------------------------------------------------------------
i4 <- df |>
  filter(complete == "Complete" & is.na(call_date)) |>
  select(record_id, caller, last_name, state, complete, call_date, office_answered) |>
  issue(
    flag        = "MISSING_CALL_DATE",
    priority    = "HIGH",
    description = "Record marked Complete but call_date is blank",
    action      = "Caller must back-fill date_of_phone_call in REDCap"
  )

# -----------------------------------------------------------------------------
# Issue 5 — Chief complaint "No" but appointment with physician = "Yes"
# -----------------------------------------------------------------------------
i5 <- df |>
  filter(sees_chief_complaint == "No" & appointment_with_physician == "Yes") |>
  select(record_id, caller, last_name, state, ent_type,
         sees_chief_complaint, appointment_with_physician) |>
  issue(
    flag        = "COMPLAINT_APPT_CONFLICT",
    priority    = "MEDIUM",
    description = "Physician does not see chief complaint but appointment offered with them",
    action      = "Review: was a different complaint used? Update sees_chief_complaint or appointment_with_physician"
  )

# -----------------------------------------------------------------------------
# Issue 6 — No caller assigned
# -----------------------------------------------------------------------------
i6 <- df |>
  filter(is.na(caller) | tolower(trimws(caller)) == "na") |>
  select(record_id, caller, last_name, state, complete, call_date) |>
  issue(
    flag        = "NO_CALLER_ASSIGNED",
    priority    = "MEDIUM",
    description = "No student assigned to this record",
    action      = "Assign caller in REDCap field 'Student assigned to this record'"
  )

# -----------------------------------------------------------------------------
# Issue 7 — Unverified records
# -----------------------------------------------------------------------------
i7 <- df |>
  filter(complete == "Unverified") |>
  select(record_id, caller, last_name, state, complete,
         office_answered, taking_new_patients) |>
  issue(
    flag        = "UNVERIFIED",
    priority    = "MEDIUM",
    description = "Record status is Unverified — needs review",
    action      = "Caller or PI to review and set status to Complete or Incomplete"
  )

# -----------------------------------------------------------------------------
# Issue 8 — Phone needs manual lookup
# -----------------------------------------------------------------------------
i8 <- df |>
  filter(record_id %in% c(605, 651)) |>
  select(record_id, caller, last_name, state, phone) |>
  issue(
    flag        = "PHONE_INCOMPLETE",
    priority    = "LOW",
    description = "Only area code entered — full phone number needed",
    action      = "Look up practice phone number and update in REDCap"
  )

# -----------------------------------------------------------------------------
# Combine all issues
# -----------------------------------------------------------------------------
priority_order <- c("CRITICAL", "HIGH", "MEDIUM", "LOW")

report <- bind_rows(i1, i2, i3, i4, i5, i6, i7, i8) |>
  mutate(priority = factor(priority, levels = priority_order)) |>
  arrange(priority, record_id)

message(sprintf("\nTotal records flagged: %d across %d issue types\n",
                nrow(report), n_distinct(report$flag)))

print(count(report, priority, flag))

# -----------------------------------------------------------------------------
# Write Excel with one tab per issue type + summary tab
# -----------------------------------------------------------------------------
timestamp  <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
out_path   <- here("data", "processed",
                   paste0("redcap_correction_report_", timestamp, ".xlsx"))

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")

# Color palette by priority
fill_colors <- c(
  CRITICAL = "#FF4444",
  HIGH     = "#FF8C00",
  MEDIUM   = "#FFD700",
  LOW      = "#90EE90"
)

header_style <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#2F4F4F",
  halign = "LEFT", textDecoration = "Bold", wrapText = TRUE
)

add_issue_sheet <- function(wb, sheet_name, data, fill_hex) {
  if (is.null(data) || nrow(data) == 0) return(invisible(NULL))
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
  addStyle(wb, sheet_name, header_style, rows = 1, cols = seq_len(ncol(data)),
           gridExpand = TRUE)
  row_style <- createStyle(fgFill = fill_hex, wrapText = TRUE)
  addStyle(wb, sheet_name, row_style, rows = seq(2, nrow(data) + 1),
           cols = seq_len(ncol(data)), gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = "auto")
}

# Summary sheet
summary_tbl <- report |>
  count(priority, flag, description, action) |>
  rename(n_records = n) |>
  arrange(priority)

addWorksheet(wb, "SUMMARY")
writeData(wb, "SUMMARY", summary_tbl)
addStyle(wb, "SUMMARY", header_style, rows = 1,
         cols = seq_len(ncol(summary_tbl)), gridExpand = TRUE)
setColWidths(wb, "SUMMARY", cols = seq_len(ncol(summary_tbl)), widths = "auto")

# One sheet per issue
issue_sheets <- list(
  list(name = "1_IMPOSSIBLE_WAIT",      data = i1, color = fill_colors["CRITICAL"]),
  list(name = "2_ANSWER_CONFLICT",      data = i2, color = fill_colors["HIGH"]),
  list(name = "3_MISSING_APPT_DATE",    data = i3, color = fill_colors["HIGH"]),
  list(name = "4_MISSING_CALL_DATE",    data = i4, color = fill_colors["HIGH"]),
  list(name = "5_COMPLAINT_CONFLICT",   data = i5, color = fill_colors["MEDIUM"]),
  list(name = "6_NO_CALLER",            data = i6, color = fill_colors["MEDIUM"]),
  list(name = "7_UNVERIFIED",           data = i7, color = fill_colors["MEDIUM"]),
  list(name = "8_PHONE_INCOMPLETE",     data = i8, color = fill_colors["LOW"])
)

for (s in issue_sheets) {
  add_issue_sheet(wb, s$name, s$data, s$color)
}

saveWorkbook(wb, out_path, overwrite = TRUE)
message(sprintf("Correction report saved to: %s", out_path))
message(sprintf("  %d total records flagged across 8 issue types", nrow(report)))
