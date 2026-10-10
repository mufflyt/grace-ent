# =============================================================================
# tests/testthat/test-no-otolaryngology-in-active-maps-2026-05-22.R
#
# Pass 10 regression guard: ensure no stale "Otolaryngology" / "otolaryngology"
# strings sneak back into the *active* map-regeneration scripts. The project
# pivoted from ENT to OB/GYN and a published figure with the wrong specialty
# label is a retraction-level error.
#
# Files in scope (sourced or invoked by the canonical map regen path):
#   - manuscript/R/12_generate_all_maps.R
#   - manuscript/R/13_regenerate_enhanced_maps.R
#   - manuscript/R/14_generate_percapita_map.R
#   - manuscript/R/create_figure1_faceted_subspecialty_map.R
#   - scripts/facet_map_by_obgyn_subspecialty.R (already covered in pass 5)
#
# Comment lines that EXPLAIN the prior bug ("…stale Otolaryngology-era file…")
# are allowed via a documented "historical context" marker (the word
# 'Otolaryngology' appears INSIDE quoted prose inside a comment).
# =============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(here)
})

active_map_scripts <- c(
  # Map orchestrators
  "manuscript/R/12_generate_all_maps.R",
  "manuscript/R/13_regenerate_enhanced_maps.R",
  "manuscript/R/14_generate_percapita_map.R",
  "scripts/facet_map_by_obgyn_subspecialty.R",
  # Individual figure scripts
  "manuscript/R/create_figure1_faceted_subspecialty_map.R",
  "manuscript/R/create_figure1b_example_isochrone.R",
  "manuscript/R/create_figure2_temporal_trends.R",
  "manuscript/R/create_figure2_temporal_trends_poster.R",
  "manuscript/R/create_figure5_dose_response.R",
  "manuscript/R/create_figure5_dose_response_poster.R",
  "manuscript/R/create_temporal_faceted_subspecialty_maps.R",
  # Pass 12 additions
  "manuscript/R/create_causal_dag.R",
  "manuscript/R/create_supplementary_threshold_sensitivity.R",
  "manuscript/R/create_supplementary_figure_statistical_details.R",
  "manuscript/R/create_supplementary_table_s2_drive_time.R",
  "manuscript/R/create_table1_sample_characteristics.R"
)

# Lines that mention Otolaryngology purely as historical context inside a
# comment are allowed if they reference the prior bug-fix narrative. The
# fingerprint is the literal string "Otolaryngology"-era OR a leading `#`
# describing past behavior. We're strict about plot titles and string
# literals being clean.

test_that("[ADV-OTO-1] active map scripts have no live Otolaryngology references", {
  for (rel in active_map_scripts) {
    path <- here::here(rel)
    if (!file.exists(path)) {
      skip(sprintf("not present: %s", rel))
    }
    txt <- readLines(path, warn = FALSE)
    # Find offending lines
    hit_idx <- grep("[Oo]tolaryngology", txt)
    # Filter out comment-only lines (historical context is OK in comments;
    # what we care about is `title = "...Otolaryngology..."` and similar
    # string-literal occurrences that would actually render in a figure).
    if (length(hit_idx) > 0) {
      offenders <- vapply(hit_idx, function(i) {
        line <- txt[i]
        is_comment_only <- grepl("^\\s*#", line)
        if (is_comment_only) "" else line
      }, character(1))
      offenders <- offenders[nzchar(offenders)]
      expect_length(offenders, 0L)
    }
  }
})
