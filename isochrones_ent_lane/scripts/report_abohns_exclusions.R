library(dplyr)
library(readr)
library(stringr)
library(lubridate)

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

today <- as.Date("2026-05-12")

raw_data <- read_csv("data/ent/abohns_certifications.csv", show_col_types = FALSE)

# Generate report with reasons
exclusions_report <- raw_data %>%
  mutate(
    state_full = str_match(location, ",\\s*(.*)$")[,2],
    is_us = state_full %in% us_states,
    
    is_date = str_detect(expiration, "\\d{2}/\\d{2}/\\d{4}"),
    exp_date = mdy(ifelse(is_date, expiration, NA)),
    is_active = case_when(
      expiration == "Non-Time Limited" ~ TRUE,
      is_date & exp_date >= today ~ TRUE,
      TRUE ~ FALSE
    ),
    
    reason = case_when(
      !is_us ~ "Non-US Location",
      !is_active ~ "Expired Certification",
      TRUE ~ "KEEP"
    )
  ) %>%
  filter(reason != "KEEP")

# Summary Table
cat("\n--- Exclusion Summary ---\n")
print(table(exclusions_report$reason))

# Show samples
cat("\n--- Sample Exclusions (Non-US Location) ---\n")
exclusions_report %>%
  filter(reason == "Non-US Location") %>%
  select(name, location) %>%
  head(10) %>%
  print()

cat("\n--- Sample Exclusions (Expired Certification) ---\n")
exclusions_report %>%
  filter(reason == "Expired Certification") %>%
  select(name, initial_certification, expiration) %>%
  head(10) %>%
  print()

# Write full report
write_csv(exclusions_report, "data/ent/abohns_certifications_exclusions.csv")
cat("\nFull report saved to data/ent/abohns_certifications_exclusions.csv\n")
