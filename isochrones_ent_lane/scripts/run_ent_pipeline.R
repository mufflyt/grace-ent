#!/usr/bin/env Rscript

# Run ENT pipeline with logging, validation, provenance, and transparency report

library(here)

# Initialise logging utilities
source(here::here("R", "log_utils.R"))
log_init()
log_info("=== ENT pipeline started ===")

# Load canonical contour bands (created earlier)
source(here::here("R", "contour_bands.R"))

# Validate contour band configuration
source(here::here("R", "validate_contour_bands.R"))
validate_contour_bands()

# Run stakeholder visualization script (which now records provenance)
source(here::here("scripts", "generate_ent_stakeholder_viz.R"))
log_info("Stakeholder visualisation completed")

# Render transparency report
rmarkdown::render(
  input = here::here("reports", "ent_transparency_report.Rmd"),
  output_file = here::here("reports", "ent_transparency_report.html")
)
log_info("Transparency report generated")

log_info("=== ENT pipeline completed successfully ===")
