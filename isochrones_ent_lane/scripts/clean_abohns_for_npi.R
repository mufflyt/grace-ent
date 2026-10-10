library(dplyr)
library(readr)
library(stringr)
library(lubridate)
# humaniformat is great, but regex is more portable if the package isn't installed.
# I'll check for humaniformat and use it if available, otherwise use regex.
if (!requireNamespace("humaniformat", quietly = TRUE)) {
  cat("humaniformat not found, using regex for name parsing.\n")
}

# Define US States for filtering
us_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
  "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
  "Wisconsin", "Wyoming", "District of Columbia", "Puerto Rico"
)

cat("Loading data...\n")
raw_data <- read_csv(here::here("data", "ent", "abohns_certifications.csv"), show_col_types = FALSE)

cat("Filtering for United States...\n")
clean_data <- raw_data %>%
  # Extract state from "City, State"
  mutate(state_full = str_match(location, ",\\s*(.*)$")[,2]) %>%
  filter(state_full %in% us_states)

cat("Filtering for Active Certifications...\n")
# Current date for comparison
today <- Sys.Date()

clean_data <- clean_data %>%
  mutate(
    # Handle dates like 04/01/2032 or "Non-Time Limited"
    is_date = str_detect(expiration, "\\d{2}/\\d{2}/\\d{4}"),
    exp_date = mdy(ifelse(is_date, expiration, NA)),
    is_active = case_when(
      expiration == "Non-Time Limited" ~ TRUE,
      is_date & exp_date >= today ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(is_active)

cat("Parsing names...\n")
if (requireNamespace("humaniformat", quietly = TRUE)) {
  names_parsed <- humaniformat::parse_names(clean_data$name)
  clean_data <- clean_data %>%
    mutate(
      first_name = names_parsed$first_name,
      middle_name = names_parsed$middle_name,
      last_name = names_parsed$last_name,
      suffix = names_parsed$suffix
    )
} else {
  # Robust Regex Fallback
  clean_data <- clean_data %>%
    mutate(
      # Remove common suffixes
      clean_name = str_remove_all(name, ",\\s*(MD|DO|PhD|MPH|MS|MBBS|FACS).*$"),
      # Split by space
      name_parts = str_split(clean_name, "\\s+"),
      first_name = sapply(name_parts, function(x) x[1]),
      last_name = sapply(name_parts, function(x) x[length(x)]),
      middle_name = sapply(name_parts, function(x) {
        if (length(x) > 2) paste(x[2:(length(x)-1)], collapse = " ") else NA_character_
      })
    )
}

# Final Clean and Export
output_data <- clean_data %>%
  select(
    abohns_id, 
    first_name, 
    middle_name, 
    last_name, 
    location, 
    state_full,
    certification, 
    initial_certification, 
    expiration
  ) %>%
  distinct()

output_path <- here::here("data", "ent", "abohns_certifications_clean.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
tmp_output_path <- paste0(output_path, ".tmp")
write_csv(output_data, tmp_output_path)
file.rename(tmp_output_path, output_path)

cat("\nCleaning complete!\n")
cat("Original records: ", nrow(raw_data), "\n")
cat("Cleaned records:  ", nrow(output_data), "\n")
cat("Saved to: ", output_path, "\n")
