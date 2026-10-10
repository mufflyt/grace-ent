#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(here)
})

dropbox_root <- file.path(path.expand("~"), "Dropbox (Personal)")
if (!dir.exists(dropbox_root)) {
  dropbox_root <- file.path(path.expand("~"), "Dropbox")
}

source_csv <- file.path(
  dropbox_root, "mystery_shopper", "Corbi study", "ENT", "Data", "results_geocoded.csv"
)

if (!file.exists(source_csv)) {
  stop("Could not find current ENT source file: ", source_csv, call. = FALSE)
}

out_csv <- here::here("data", "ent", "abms_scrape_input.csv")

ent <- readr::read_csv(source_csv, show_col_types = FALSE)

npi_col <- if ("npi" %in% names(ent)) "npi" else NA_character_
phone_col <- if ("phone_number" %in% names(ent)) "phone_number" else NA_character_
spec_col <- if ("specialty_primary" %in% names(ent)) "specialty_primary" else NA_character_

required_cols <- c("first_name", "last_name", "state_code", "full_name")
missing_cols <- setdiff(required_cols, names(ent))
if (length(missing_cols) > 0) {
  stop("Missing required columns in source CSV: ", paste(missing_cols, collapse = ", "),
       call. = FALSE)
}

input <- ent |>
  transmute(
    first = trimws(first_name),
    last = trimws(last_name),
    state = trimws(state_code),
    npi = if (!is.na(npi_col)) dplyr::coalesce(as.character(.data[[npi_col]]), "") else "",
    phone = if (!is.na(phone_col)) dplyr::coalesce(as.character(.data[[phone_col]]), "") else "",
    ent_subspecialty = if (!is.na(spec_col)) dplyr::coalesce(as.character(.data[[spec_col]]), "") else "",
    full_name = trimws(full_name)
  ) |>
  filter(
    !is.na(first), first != "",
    !is.na(last), last != "",
    !is.na(state), state != ""
  ) |>
  distinct(first, last, state, .keep_all = TRUE) |>
  arrange(last, first, state)

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
tmp_out_csv <- paste0(out_csv, ".tmp")
readr::write_csv(input, tmp_out_csv, na = "")
file.rename(tmp_out_csv, out_csv)

cat("Built ABOHNS scrape input\n")
cat("  Source :", source_csv, "\n")
cat("  Output :", out_csv, "\n")
cat("  Rows   :", nrow(input), "\n")
