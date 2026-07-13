# =============================================================================
# 01_clean_phase2_call_log.R
# ENT Mystery Caller Study — Phase 2 Call Log Cleaning
#
# Input:  data/raw/call log data pull 7-6-26 for muffly.xlsb.xlsx
#         Sheet: "all data export 7-6"  (all records are Phase 2)
# Output: data/processed/ent_phase2_clean_<timestamp>.csv
# =============================================================================

library(here)
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(mysterycall)

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
RAW_XLSX   <- here("call log data pull 7-6-26 for muffly.xlsb.xlsx")
RUCA_CSV   <- here("data", "raw", "ruca_2020_zip_crosswalk_usda.csv")
OUT_DIR    <- here("data", "processed")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Load raw data
# -----------------------------------------------------------------------------
message("Loading raw Phase 2 call log ...")
raw <- read_excel(RAW_XLSX, sheet = "all data export 7-6")
message(sprintf("  Loaded %d rows x %d columns.", nrow(raw), ncol(raw)))

# -----------------------------------------------------------------------------
# 2. Standardise column names via mysterycall_clean_phase2()
#    required_strings: substrings matching janitor::clean_names() output
#    standard_names:   the short names we will use throughout the project
# -----------------------------------------------------------------------------
required_strings <- c(
  "student_assigned",
  "physician_last_name",
  "physician_first_name",
  "primary_practice_state",
  "practice_city",
  "practice_zip_code",
  "primary_practice_ruca_code",
  "rural_satellite_office",
  "which_datasets",
  "dataset_subspecialty_classification",
  "was_a_website",
  "practice_phone_number",
  "type_of_otolaryngologist",
  "date_of_phone_call",
  "did_the_sampled_physicians_office_answer",
  "is_the_sampled_physician_taking_new_patients",
  "does_the_sampled_physician_see_patients_for_the_chief_complaint",
  "if_an_appointment_was_offered",
  "what_date_was_the_appointment_offered_for",
  "complete"
)

standard_names <- c(
  "caller",
  "last_name",
  "first_name",
  "state",
  "city",
  "zip",
  "ruca_code",
  "rural_satellite_office",
  "datasets",
  "subspecialty_agreement",
  "website_found",
  "phone",
  "ent_type",
  "call_date",
  "office_answered",
  "taking_new_patients",
  "sees_chief_complaint",
  "appointment_with_physician",
  "appointment_date",
  "complete"
)

phase2 <- mysterycall_clean_phase2(
  data_or_path   = raw,
  required_strings = required_strings,
  standard_names   = standard_names,
  output_directory = OUT_DIR,
  output_format    = "csv"
)

# -----------------------------------------------------------------------------
# 3. Parse column types
# -----------------------------------------------------------------------------
message("Parsing column types ...")

phase2 <- phase2 |>
  mutate(
    record_id        = as.integer(record_id),
    npi              = as.character(npi),
    ruca_code        = as.numeric(ruca_code),
    call_date        = as_date(call_date),        # POSIXct -> Date
    appointment_date = as_date(appointment_date), # POSIXct -> Date
    zip              = stringr::str_pad(as.character(zip), width = 5, side = "left", pad = "0")
  )

# -----------------------------------------------------------------------------
# 4. Validate NPIs
# -----------------------------------------------------------------------------
message("Validating NPIs ...")
npi_validated <- mysterycall_validate_npi(phase2)

# mysterycall_validate_npi() drops rows with blank/NA NPIs and adds npi_is_valid
# join result back so we keep all 960 rows
phase2 <- phase2 |>
  left_join(
    npi_validated |> select(npi, npi_is_valid) |> distinct(npi, .keep_all = TRUE),
    by = "npi"
  )

n_invalid_npi <- sum(!phase2$npi_is_valid, na.rm = TRUE)
if (n_invalid_npi > 0) {
  message(sprintf("  WARNING: %d record(s) have invalid NPIs.", n_invalid_npi))
} else {
  message("  All NPIs passed validation.")
}

# -----------------------------------------------------------------------------
# 5. Validate and normalise phone numbers
# -----------------------------------------------------------------------------
message("Normalising phone numbers ...")
phase2 <- phase2 |>
  mutate(
    phone = case_when(
      # Strip extensions: "352-548-6000 ext 1153" -> "352-548-6000"
      grepl("\\bext\\b", phone, ignore.case = TRUE) ~
        trimws(sub("\\s*ext[^0-9]*.*$", "", phone, ignore.case = TRUE)),
      # Multiple numbers separated by "/": take everything before the first "/"
      grepl("/", phone, fixed = TRUE) ~
        trimws(sub("/.*$", "", phone)),
      TRUE ~ phone
    )
  )

fixed_phones <- c("352-548-6000", "800-789-7366", "337-239-2234")
message(sprintf("  Auto-fixed 3 phone entries (ext stripped / first number taken): %s",
                paste(fixed_phones, collapse = ", ")))
message("  Records 605 (Nesemeier, IL) and 651 (Gibbons, TX) have only area codes — require manual lookup.")

message("Validating phone numbers ...")
phone_result <- mysterycall_validate_phone(phase2$phone)
phase2 <- bind_cols(phase2, phone_result)
message(sprintf("  Valid: %d | Invalid format: %d | Missing: %d",
                sum(phase2$phone_validity_flag == "valid",          na.rm = TRUE),
                sum(phase2$phone_validity_flag == "invalid_format", na.rm = TRUE),
                sum(phase2$phone_validity_flag == "missing",        na.rm = TRUE)))

# -----------------------------------------------------------------------------
# 6. Classify RUCA codes → Urban / Suburban / Rural
#    RUCA 1-3 = Urban, 4-6 = Suburban, 7-10 = Rural  (USDA 2020 standard)
# -----------------------------------------------------------------------------
message("Classifying RUCA codes ...")
phase2 <- phase2 |>
  mutate(
    ruca_category = mysterycall_classify_ruca(
      ruca_code,
      urban_max    = 3,
      suburban_max = 6,
      labels       = c("Urban", "Suburban", "Rural"),
      as_factor    = TRUE
    )
  )

message("  RUCA category distribution:")
print(table(phase2$ruca_category, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 7. Standardise factor / categorical variables
# -----------------------------------------------------------------------------
message("Standardising categorical variables ...")

phase2 <- phase2 |>
  mutate(
    # Binary: office answered
    office_answered = case_when(
      tolower(office_answered) == "yes" ~ TRUE,
      tolower(office_answered) == "no"  ~ FALSE,
      TRUE ~ NA
    ),

    # New patient acceptance — consolidate into a clean factor
    new_patient_status = case_when(
      taking_new_patients == "Yes"                                                             ~ "Accepting",
      taking_new_patients == "No-not specified why"                                            ~ "Not accepting: unspecified",
      taking_new_patients == "No-retired or no longer practicing medicine"                     ~ "Not accepting: retired/left",
      taking_new_patients == "No-changed practices"                                            ~ "Not accepting: changed practice",
      taking_new_patients == "No-practice is full but physician does still see established patients" ~ "Not accepting: practice full",
      taking_new_patients == "No-on leave of absence (like parental leave)"                   ~ "Not accepting: on leave",
      TRUE ~ NA_character_
    ) |> factor(),

    # Appointment outcome — clean factor
    appointment_outcome = case_when(
      appointment_with_physician == "Yes"                                                      ~ "With sampled physician",
      appointment_with_physician == "No-but appointment offered with different physician"      ~ "With different physician",
      appointment_with_physician == "No-but appointment offered with APP"                      ~ "With APP only",
      appointment_with_physician == "Not applicable, no appointment offered with anyone in practice" ~ "No appointment offered",
      TRUE ~ NA_character_
    ) |> factor(),

    # ENT type — clean factor
    ent_type = factor(ent_type, levels = c(
      "General", "Facial Plastics", "Head and Neck Cancer",
      "Laryngology", "Otology/neurotology", "Pediatrics",
      "Rhinology", "Sleep"
    )),

    # Completion status
    complete = factor(complete, levels = c("Complete", "Incomplete", "Unverified")),

    # Boolean: was a website found?
    website_found = case_when(
      tolower(website_found) == "yes" ~ TRUE,
      tolower(website_found) == "no"  ~ FALSE,
      TRUE ~ NA
    ),

    # Boolean: rural satellite office
    rural_satellite_office = case_when(
      tolower(rural_satellite_office) == "yes" ~ TRUE,
      tolower(rural_satellite_office) == "no"  ~ FALSE,
      TRUE ~ NA
    )
  )

# -----------------------------------------------------------------------------
# 8. Calculate business days from call date to appointment date
# -----------------------------------------------------------------------------
message("Calculating wait times in business days ...")

cal <- mysterycall_us_federal_calendar()

phase2 <- phase2 |>
  mutate(
    wait_days_business = if_else(
      !is.na(call_date) & !is.na(appointment_date) & appointment_date >= call_date,
      mysterycall_count_business_days(call_date, appointment_date, calendar = cal),
      NA_real_
    )
  )

message(sprintf(
  "  Wait time (business days): median = %.0f, range = %.0f-%.0f (n = %d with dates)",
  median(phase2$wait_days_business, na.rm = TRUE),
  min(phase2$wait_days_business,    na.rm = TRUE),
  max(phase2$wait_days_business,    na.rm = TRUE),
  sum(!is.na(phase2$wait_days_business))
))

# -----------------------------------------------------------------------------
# 9. Flag appointment offered with sampled physician (primary outcome)
# -----------------------------------------------------------------------------
phase2 <- phase2 |>
  mutate(
    appointment_offered = !is.na(appointment_date),
    appointment_with_md = appointment_outcome == "With sampled physician"
  )

# -----------------------------------------------------------------------------
# 10. Check for duplicate NPIs
# -----------------------------------------------------------------------------
message("Checking for duplicate records ...")
dup_check <- mysterycall_check_duplicates(phase2, id_col = "npi", max_calls = 1L)
# returns rows where n_calls > max_calls
message(sprintf("  NPIs appearing more than once: %d", nrow(dup_check)))

# -----------------------------------------------------------------------------
# 11. Data completeness quality check
# -----------------------------------------------------------------------------
message("Running data completeness check ...")

key_cols <- c("npi", "state", "ruca_code", "ent_type",
              "call_date", "office_answered", "taking_new_patients",
              "appointment_with_physician", "complete")

completeness_tbl <- tibble::tibble(
  column       = key_cols,
  n_complete   = sapply(key_cols, \(col) sum(!is.na(phase2[[col]]))),
  pct_complete = sapply(key_cols, \(col) mean(!is.na(phase2[[col]]))) * 100
)
overall_pct <- mean(completeness_tbl$pct_complete)
message(sprintf("  Overall completeness: %.1f%%", overall_pct))
print(completeness_tbl)

# -----------------------------------------------------------------------------
# 12. Restrict to complete records for analysis dataset
# -----------------------------------------------------------------------------
phase2_complete <- phase2 |>
  filter(complete == "Complete")

message(sprintf(
  "  Complete records: %d / %d (%.1f%%)",
  nrow(phase2_complete), nrow(phase2),
  100 * nrow(phase2_complete) / nrow(phase2)
))

# -----------------------------------------------------------------------------
# 13. Save outputs
# -----------------------------------------------------------------------------
timestamp  <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

path_all   <- file.path(OUT_DIR, paste0("ent_phase2_all_",      timestamp, ".csv"))
path_clean <- file.path(OUT_DIR, paste0("ent_phase2_complete_", timestamp, ".csv"))

# Flatten any list columns created by validation helpers before writing
flatten_lists <- function(df) {
  df |> mutate(across(where(is.list), \(col) sapply(col, \(x) paste(unlist(x), collapse = "|"))))
}

readr::write_csv(flatten_lists(phase2),          path_all)
readr::write_csv(flatten_lists(phase2_complete), path_clean)

message(sprintf("Saved all records (%d rows) to:      %s", nrow(phase2),          path_all))
message(sprintf("Saved complete records (%d rows) to: %s", nrow(phase2_complete), path_clean))
message("Done.")
