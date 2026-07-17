#!/usr/bin/env Rscript
# =============================================================================
# run_all.R — one-command reproduction of the ENT mystery-caller analysis.
#
# Usage:
#   Rscript run_all.R                # analysis + manuscript (default)
#   Rscript run_all.R analysis       # regenerate model_output/ only
#   Rscript run_all.R render         # re-render the manuscript only
#   Rscript run_all.R data           # rebuild the enriched dataset from raw
#   Rscript run_all.R all            # data + analysis + render
#
# Stages
#   data     Rebuilds data/processed/ent_phase2_enriched.csv from raw inputs.
#            Requires CENSUS_API_KEY (and network); OFF by default because the
#            committed enriched CSV already reflects these steps.
#   analysis Fits the two-part model and regenerates every table/figure in
#            model_output/ from the committed enriched CSV.
#   render   Knits manuscript_mystery_caller.Rmd to HTML + DOCX.
#
# NOT run here (external services / long compute; run manually if needed):
#   Valhalla isochrones .... scripts/valhalla_ent_isochrones.R (EC2 tunnel)
#   isochrone coverage map .. scripts/isochrone_map.R
#   simr power simulations .. scripts/mystery_caller_power_simr_*.R (slow)
# =============================================================================

stage <- (function() {
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) == 0) "default" else tolower(a[[1]])
})()

run <- function(path) {
  cat(sprintf("\n>>> %s\n", path))
  t0 <- Sys.time()
  source(path, local = new.env())
  cat(sprintf("    done (%.1fs)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

# ---- stage: data (from raw; needs CENSUS_API_KEY) ---------------------------
data_stage <- function() {
  if (!nzchar(Sys.getenv("CENSUS_API_KEY")))
    stop("CENSUS_API_KEY is not set; the 'data' stage needs it. ",
         "Skip it and use the committed enriched CSV instead.", call. = FALSE)
  for (s in c(
    "scripts/build_geo_crosswalk.R",
    "scripts/build_cms_enrollment.R",
    "scripts/build_county_ent_count.R",
    "scripts/build_zip_svi.R",
    "scripts/01_clean_phase2_call_log.R",
    "scripts/enrich_call_log.R",
    "scripts/fix_data_errors.R",       # record-470 wrong-year correction
    "scripts/apply_binary_ruca.R"      # RUCA 1-3 = urban, 4-10 = rural
  )) run(s)
}

# ---- stage: analysis (from committed enriched CSV) --------------------------
analysis_stage <- function() {
  for (s in c(
    "scripts/model_wait_days.R",        # two-part model -> part1/part2 CSVs
    "scripts/model_robustness.R",       # ICC, HHI sensitivity, pairwise
    "scripts/missingness_access.R",     # attrition vs covariates
    "scripts/manuscript_outputs.R",     # Table 1, STROBE dot, forest plot
    "scripts/table1_by_subspecialty.R", # stratified Table 1
    "scripts/strobe_flow.R",            # mysterycall STROBE figure
    "scripts/supplementary_tables.R",   # Tables S1-S7
    "scripts/supplementary_figures.R"   # Figures S1-S6 (KM, dist, DHARMa, maps)
  )) run(s)
}

# ---- stage: render ----------------------------------------------------------
render_stage <- function() {
  for (fmt in c("html_document", "word_document"))
    rmarkdown::render("manuscript_mystery_caller.Rmd",
                      output_format = fmt, output_dir = "manuscript_output",
                      quiet = TRUE)
  cat(">>> rendered manuscript_output/manuscript_mystery_caller.{html,docx}\n")
}

# ---- reproducibility snapshot ----------------------------------------------
snapshot <- function() {
  dir.create("repro", showWarnings = FALSE)
  writeLines(capture.output(sessionInfo()), "repro/sessionInfo.txt")
  ip <- as.data.frame(installed.packages()[, c("Package", "Version")],
                      stringsAsFactors = FALSE)
  key <- c("glmmTMB", "emmeans", "gtsummary", "gt", "flextable", "rmarkdown",
           "knitr", "ggplot2", "dplyr", "survival", "patchwork", "sf",
           "DHARMa", "broom.mixed")
  write.csv(ip[ip$Package %in% key, ], "repro/package_versions.csv",
            row.names = FALSE)
  cat(">>> wrote repro/sessionInfo.txt and repro/package_versions.csv\n")
}

cat(sprintf("ENT mystery-caller pipeline — stage: %s\n", stage))
switch(stage,
  data     = data_stage(),
  analysis = analysis_stage(),
  render   = render_stage(),
  all      = { data_stage(); analysis_stage(); render_stage() },
  default  = { analysis_stage(); render_stage() },
  stop(sprintf("unknown stage '%s' (use data|analysis|render|all)", stage))
)
snapshot()
cat("\nAll done.\n")
