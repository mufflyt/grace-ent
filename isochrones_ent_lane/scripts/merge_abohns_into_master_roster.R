library(dplyr)
library(readr)
library(stringr)
library(here)

cat("Starting Master SSOT Merge...\n")

# 1. Load Reference Mapping
mapping <- read_csv(here::here("data/reference/subspecialty_master_mapping.csv"), show_col_types = FALSE) %>%
  filter(source_specialty == "ENT" & source_type == "BOARD_CERT") %>%
  select(certification = source_value, subspecialty_id = internal_id)

# 2. Load New Board Data (The superior source)
abohns <- read_csv(here::here("data/ent/abohns_with_npi_master.csv"), show_col_types = FALSE) %>%
  filter(practicing_status == "Practicing") %>%
  left_join(mapping, by = "certification") %>%
  mutate(
    subspecialty = coalesce(subspecialty_id, "GEN_ENT"),
    gender = toupper(substr(inferred_gender, 1, 1)) # M/F
  ) %>%
  select(npi, abohns_id, first_name, last_name, state = state_full, subspecialty, gender)

# 3. Load Old Master Roster
master_old <- read_csv(here::here("data/ent/ent_master_roster_ssot.csv"), show_col_types = FALSE)

cat("Original Master Size: ", nrow(master_old), "\n")
cat("ABOHNS Practicing Size: ", nrow(abohns), "\n")

# 4. Reconcile and Merge
# We use NPI as the primary key. 
# We want to keep all records from both, but where they overlap, ABOHNS data WINS.

master_merged <- master_old %>%
  # Remove the NPIs we are about to update from the old master to avoid conflicts
  filter(!(npi %in% abohns$npi)) %>%
  # Bind with the new board data
  bind_rows(abohns) %>%
  # Final Cleanup
  mutate(
    npi = as.character(npi),
    gender = case_when(
      gender %in% c("M", "Male") ~ "M",
      gender %in% c("F", "Female") ~ "F",
      TRUE ~ NA_character_
    )
  ) %>%
  # Deduplicate just in case
  distinct(npi, .keep_all = TRUE)

cat("New Merged Master Size: ", nrow(master_merged), "\n")
cat("Net increase: ", nrow(master_merged) - nrow(master_old), "\n")

# 5. Save Results
output_path <- here::here("data/ent/ent_master_roster_ssot_v2.csv")
write_csv(master_merged, output_path)

cat("\nMaster SSOT v2 created at: ", output_path, "\n")
