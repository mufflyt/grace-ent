library(dplyr)
library(readr)
library(here)
library(yaml)

# Load config
config <- yaml::read_yaml(here::here("config/specialty_configs/ent.yml"))

# Load base roster (Full Cohort with NPIs)
cat("Loading base roster...\n")
base_path <- here::here("data/ent/enthealth_npi_full_cohort.csv")
if (!file.exists(base_path)) {
  stop("Base roster file not found: ", base_path)
}
export_data <- read_csv(base_path, show_col_types = FALSE)

# Source the SSOT helper
cat("Sourcing SSOT helper...\n")
source(here::here("R/apply_ent_ssot_subspecialty.R"))

# Apply SSOT
# This will use the Tiers: ABMS > Board Targeted > Board Bulk > Patient Website > NPPES
cat("Applying SSOT Hierarchy...\n")
final_data <- apply_ent_ssot_subspecialty(export_data, config)

# Save result (atomic write to prevent corruption on interruption)
output_path <- here::here("data/ent/ent_master_roster_ssot.csv")
tmp_path <- paste0(output_path, ".tmp")
write_csv(final_data, tmp_path)
file.rename(tmp_path, output_path)

cat("\nMaster Roster generated: ", output_path, "\n")
cat("Total Physicians: ", nrow(final_data), "\n")
cat("Summary of Sources:\n")
print(table(final_data$ssot_source, useNA = "always"))
