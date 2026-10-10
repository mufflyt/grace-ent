#!/usr/bin/env Rscript
# Compute pct_inside_120 for EVERY flagged NPI in the most recent Step 3.85
# run, plus generate a 9-panel collage of the 9 worst cases so the full
# geometry can be inspected at a glance.
#
# This is a direct response to the claim that any NPI has "100% protrusion"
# or "60-min polygon outside 120-min polygon." If the claim is true, this
# script will print rows with pct_inside < 50%. If not, the distribution
# will show the actual worst case and settle the question.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(here)
})
sf::sf_use_s2(FALSE)
source(here::here("R", "contour_bands.R"))

run_dir <- "artifacts/20260411_193443_e716c98e"
iso <- readRDS(file.path(run_dir, "step_3_isochrones.rds"))
viol <- read.csv(file.path(run_dir, "step_3.85_flagged_violations.csv"),
                 stringsAsFactors = FALSE)
cohort <- readRDS(file.path(run_dir, "step_2.5_final_cohort.rds"))

flagged_npis <- unique(viol$npi)
cat(sprintf("n flagged NPIs in the current run: %d\n", length(flagged_npis)))

# Keep only NPIs with BOTH 60 and 120 bands present
has_both <- intersect(
  iso$npi[iso$drive_time_minutes == 60],
  iso$npi[iso$drive_time_minutes == 120]
)
targets <- intersect(flagged_npis, has_both)
cat(sprintf("n NPIs with both 60-min AND 120-min polygons on disk: %d\n",
            length(targets)))

# Project to EPSG:5070 for accurate area
iso_ea <- sf::st_transform(iso, 5070)

results <- data.frame(
  npi = character(0),
  area60_km2 = numeric(0),
  area120_km2 = numeric(0),
  sliver_km2 = numeric(0),
  pct_outside_120 = numeric(0),
  pct_inside_120 = numeric(0),
  stringsAsFactors = FALSE
)

for (n in targets) {
  r60  <- iso_ea[iso_ea$npi == n & iso_ea$drive_time_minutes == 60, ]
  r120 <- iso_ea[iso_ea$npi == n & iso_ea$drive_time_minutes == 120, ]
  if (nrow(r60) == 0L || nrow(r120) == 0L) next

  a60  <- as.numeric(sf::st_area(r60))[1]
  a120 <- as.numeric(sf::st_area(r120))[1]
  diff_sliver <- tryCatch(
    suppressWarnings(sf::st_difference(sf::st_geometry(r60), sf::st_geometry(r120))),
    error = function(e) NULL
  )
  sliver_km2 <- if (!is.null(diff_sliver) && length(diff_sliver) > 0L) {
    sum(as.numeric(sf::st_area(diff_sliver))) / 1e6
  } else {
    0
  }
  pct_out <- 100 * (sliver_km2 * 1e6) / a60
  pct_in <- 100 - pct_out

  results <- rbind(results, data.frame(
    npi = n,
    area60_km2  = round(a60 / 1e6, 2),
    area120_km2 = round(a120 / 1e6, 2),
    sliver_km2  = round(sliver_km2, 3),
    pct_outside_120 = round(pct_out, 3),
    pct_inside_120  = round(pct_in, 3),
    stringsAsFactors = FALSE
  ))
}

results <- results[order(results$pct_outside_120, decreasing = TRUE), ]

cat(sprintf("\nComputed containment for %d NPIs.\n", nrow(results)))
cat("\n=== DISTRIBUTION OF pct_outside_120 (directly recomputed via st_difference) ===\n")
print(summary(results$pct_outside_120))
cat("\n=== DISTRIBUTION OF pct_inside_120 ===\n")
print(summary(results$pct_inside_120))

cat("\n=== QUANTILES of pct_outside_120 ===\n")
qs <- quantile(results$pct_outside_120,
               probs = c(0, .10, .25, .50, .75, .90, .95, .99, 1))
for (i in seq_along(qs)) {
  cat(sprintf("  %4s : %.3f%%\n", names(qs)[i], qs[[i]]))
}

cat("\n=== ANY NPI WITH pct_outside_120 > 10%?  ===\n")
over_10 <- results[results$pct_outside_120 > 10, ]
if (nrow(over_10) == 0L) {
  cat("NONE. Maximum is", max(results$pct_outside_120), "%.\n")
} else {
  print(over_10)
}

cat("\n=== ANY NPI WITH pct_outside_120 > 50%?  ===\n")
over_50 <- results[results$pct_outside_120 > 50, ]
if (nrow(over_50) == 0L) {
  cat("NONE.\n")
} else {
  print(over_50)
}

cat("\n=== ANY NPI WITH pct_outside_120 = 100%?  ===\n")
at_100 <- results[abs(results$pct_outside_120 - 100) < 0.01, ]
if (nrow(at_100) == 0L) {
  cat("NONE. Nothing is 'fully outside' the 120-min polygon.\n")
} else {
  print(at_100)
}

# -- Top 9 worst: produce one PNG grid for visual inspection --
cat("\n\n=== TOP 9 WORST: collage ===\n")
top9 <- results
print(top9)

out_dir <- file.path(run_dir, "step_3.85_polygon_plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
csv_out <- file.path(out_dir, "full_cohort_containment_audit.csv")
tmp_csv <- paste0(csv_out, ".tmp")
write.csv(results, tmp_csv, row.names = FALSE)
file.rename(tmp_csv, csv_out)
cat(sprintf("\n[write] full audit CSV: %s\n", csv_out))

make_panel <- function(npi, pct_out) {
  rows <- iso[iso$npi == npi & iso$drive_time_minutes %in% CANONICAL_BANDS, ]
  if (nrow(rows) == 0L) return(ggplot() + ggtitle(paste(npi, "(no polygons)")))
  meta <- as.data.frame(cohort[cohort$npi == npi,
    c("practice_city","practice_state","latitude","longitude")])
  rows_ea <- sf::st_transform(rows, 5070)
  pt_ea <- sf::st_transform(
    sf::st_as_sf(meta, coords = c("longitude","latitude"), crs = 4326), 5070)
  city <- if (nrow(meta) > 0L) sprintf("%s, %s", meta$practice_city[1], meta$practice_state[1]) else ""

  ggplot() +
    geom_sf(data = rows_ea[rows_ea$drive_time_minutes == 120, ],
            fill = "#1f77b4", color = "#1f77b4", alpha = 0.25, linewidth = 0.3) +
    geom_sf(data = rows_ea[rows_ea$drive_time_minutes == 60, ],
            fill = "#d62728", color = "#d62728", alpha = 0.45, linewidth = 0.4) +
    geom_sf(data = rows_ea[rows_ea$drive_time_minutes == 30, ],
            fill = "#2ca02c", color = "#2ca02c", alpha = 0.55, linewidth = 0.4) +
    geom_sf(data = pt_ea, color = "black", size = 1) +
    labs(title = sprintf("%s — %s", npi, city),
         subtitle = sprintf("pct_outside=%.2f%%", pct_out),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 8) +
    theme(plot.title = element_text(size = 7),
          plot.subtitle = element_text(size = 7),
          axis.text = element_text(size = 6))
}

panels <- lapply(seq_len(nrow(top9)), function(i) {
  make_panel(top9$npi[i], top9$pct_outside_120[i])
})
grid_plot <- patchwork::wrap_plots(panels, ncol = 3) +
  patchwork::plot_annotation(
    title = "Top 9 worst flagged NPIs — 30/60/120-min polygons",
    subtitle = "green=30min  red=60min  blue=120min  black=origin",
    theme = theme(plot.title = element_text(size = 12))
  )

collage_out     <- file.path(out_dir, "top9_worst_collage.png")
tmp_collage_out <- paste0(collage_out, ".tmp.png")
ggsave(tmp_collage_out, grid_plot, width = 12, height = 10, dpi = 130)
file.rename(tmp_collage_out, collage_out)
cat(sprintf("[write] collage: %s\n", collage_out))

cat("\nDone.\n")
