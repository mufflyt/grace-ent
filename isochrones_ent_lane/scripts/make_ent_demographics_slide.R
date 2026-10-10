#!/usr/bin/env Rscript
# =============================================================================
# make_ent_demographics_slide.R -- Single Source of Truth for ENT Demographics
# =============================================================================
# Generates a presentation slide showing the data linkage architecture for
# building a verified ENT physician cohort from 6 administrative databases
# (ABMS, NPPES, Medicare Part B, ACGME, State Licensure, ENThealth.org).
# Illustrates the 7-step linkage engine and the attributes in the final
# single source of truth (NPI, subspecialty, geocoded address, active status,
# demographics, licensure, experience, practice type).
#
# Output: artifacts/reports/nppes_slides/12_ent_single_source_of_truth.png
# Format: 13.33 x 7.5 in (16:9 PPT), 300 DPI, white background
# Usage:  Rscript scripts/make_ent_demographics_slide.R
# =============================================================================

library(grid)

out <- "artifacts/reports/nppes_slides/12_ent_single_source_of_truth.png"
png(out, width = 13.33, height = 7.5, units = "in", res = 300, bg = "white")
grid.newpage()

# Title bar
grid.rect(x = 0.5, y = 0.94, width = 1, height = 0.12,
          gp = gpar(fill = "#0D47A1", col = NA))
grid.text("Building a Single Source of Truth for ENT Demographics",
          x = 0.5, y = 0.96, gp = gpar(col = "white", fontsize = 22, fontface = "bold"))
grid.text("Linking 6+ administrative databases to create a verified, geocoded physician cohort",
          x = 0.5, y = 0.91, gp = gpar(col = "#90CAF9", fontsize = 12))

# ======== LEFT SIDE: RAW DATA SOURCES (stacked) ========
src_y <- c(0.76, 0.66, 0.56, 0.46, 0.36, 0.26)
src_labels <- c("ABMS / ABOto", "NPPES", "Medicare Part B", "ACGME", "State Licensure", "ENThealth.org")
src_desc <- c(
  "Board certification\nSubspecialty verified",
  "NPI, address\nTaxonomy codes",
  "Billing claims\nActive practice evidence",
  "Residency programs\nGraduation year",
  "Active license\nState of practice",
  "Member profiles\nSelf-reported subspecialty"
)
src_colors <- c("#1B5E20", "#1565C0", "#4527A0", "#00695C", "#E65100", "#C62828")
src_bg <- c("#E8F5E9", "#E3F2FD", "#EDE7F6", "#E0F2F1", "#FFF3E0", "#FFEBEE")
src_icons <- c("VERIFIED", "LOCATION", "ACTIVITY", "TRAINING", "LICENSE", "DIRECTORY")

for (i in seq_along(src_y)) {
  # Source box
  grid.roundrect(x = 0.13, y = src_y[i], width = 0.20, height = 0.08,
                 r = unit(0.008, "npc"),
                 gp = gpar(fill = src_bg[i], col = src_colors[i], lwd = 2))
  # Icon badge
  grid.roundrect(x = 0.055, y = src_y[i] + 0.02, width = 0.06, height = 0.025,
                 r = unit(0.005, "npc"),
                 gp = gpar(fill = src_colors[i], col = NA))
  grid.text(src_icons[i], x = 0.055, y = src_y[i] + 0.02,
            gp = gpar(col = "white", fontsize = 5, fontface = "bold"))
  # Source name
  grid.text(src_labels[i], x = 0.15, y = src_y[i] + 0.02,
            gp = gpar(fontsize = 10, fontface = "bold", col = src_colors[i]))
  # Description
  grid.text(src_desc[i], x = 0.15, y = src_y[i] - 0.015,
            gp = gpar(fontsize = 7, col = "gray30", lineheight = 1.1))

  # Arrow to center
  grid.lines(x = c(0.235, 0.32), y = c(src_y[i], 0.51),
             arrow = arrow(length = unit(0.008, "npc"), type = "closed"),
             gp = gpar(col = src_colors[i], lwd = 1.5, alpha = 0.6))
}

# ======== CENTER: LINKAGE ENGINE ========
# Large central process box
grid.roundrect(x = 0.42, y = 0.51, width = 0.18, height = 0.40,
               r = unit(0.015, "npc"),
               gp = gpar(fill = "#F5F5F5", col = "#37474F", lwd = 3))

grid.text("LINKAGE\nENGINE", x = 0.42, y = 0.67,
          gp = gpar(fontsize = 14, fontface = "bold", col = "#37474F", lineheight = 1.1))

# Process steps inside
steps <- c(
  "1. Match by NPI",
  "2. Name fuzzy match",
  "3. Deduplicate",
  "4. Geocode addresses",
  "5. Validate subspecialty",
  "6. Detect retirement",
  "7. Apply exclusions"
)
for (i in seq_along(steps)) {
  y_pos <- 0.62 - (i - 1) * 0.04
  grid.text(steps[i], x = 0.42, y = y_pos,
            gp = gpar(fontsize = 7.5, col = "#37474F", fontface = "italic"))
}

# Arrow from linkage to output
grid.lines(x = c(0.51, 0.58), y = c(0.51, 0.51),
           arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
           gp = gpar(col = "#0D47A1", lwd = 4))

# ======== RIGHT SIDE: SINGLE SOURCE OF TRUTH ========
# Large output box with gold border
grid.roundrect(x = 0.75, y = 0.55, width = 0.30, height = 0.45,
               r = unit(0.02, "npc"),
               gp = gpar(fill = "#FFFDE7", col = "#F57F17", lwd = 3))

# Gold star
grid.circle(x = 0.75, y = 0.74, r = 0.025,
            gp = gpar(fill = "#FFD600", col = "#F57F17", lwd = 2))
grid.text("*", x = 0.75, y = 0.74,
          gp = gpar(fontsize = 20, fontface = "bold", col = "#F57F17"))

grid.text("Single Source of Truth", x = 0.75, y = 0.69,
          gp = gpar(fontsize = 14, fontface = "bold", col = "#E65100"))
grid.text("Verified ENT Physician Cohort", x = 0.75, y = 0.66,
          gp = gpar(fontsize = 10, col = "#BF360C"))

# Attributes in the output
grid.rect(x = 0.75, y = 0.635, width = 0.26, height = 0.002,
          gp = gpar(fill = "#FFE082", col = NA))

attrs <- c(
  "NPI (unique identifier)",
  "Board-certified subspecialty (ABMS)",
  "Geocoded practice address (lat/lon)",
  "Active/retired status (claims-based)",
  "Gender, age, medical school",
  "State(s) of licensure",
  "Years in practice",
  "Solo vs group practice"
)
attr_icons <- c("ID", "CERT", "GEO", "STATUS", "DEMO", "STATE", "EXP", "PRAC")

for (i in seq_along(attrs)) {
  y_pos <- 0.61 - (i - 1) * 0.04
  # Small colored dot
  grid.circle(x = 0.635, y = y_pos, r = 0.005,
              gp = gpar(fill = "#F57F17", col = NA))
  grid.text(attrs[i], x = 0.645, y = y_pos, just = "left",
            gp = gpar(fontsize = 8, col = "gray20"))
}

# ======== BOTTOM: What this enables ========
grid.rect(x = 0.5, y = 0.11, width = 0.92, height = 0.14,
          gp = gpar(fill = "#E3F2FD", col = "#0D47A1", lwd = 1.5))
grid.text("What the Single Source of Truth Enables", x = 0.5, y = 0.16,
          gp = gpar(fontsize = 14, fontface = "bold", col = "#0D47A1"))

enables <- c(
  "Spatial accessibility analysis\n(drive-time isochrones)",
  "Workforce trend tracking\n(11-year longitudinal panel)",
  "Rural/urban disparity\nmeasurement (RUCA codes)",
  "Subspecialty-specific\nworkforce planning"
)
enable_x <- c(0.16, 0.38, 0.62, 0.84)

for (i in seq_along(enables)) {
  grid.roundrect(x = enable_x[i], y = 0.09, width = 0.20, height = 0.07,
                 r = unit(0.008, "npc"),
                 gp = gpar(fill = "white", col = "#1565C0", lwd = 1))
  grid.text(enables[i], x = enable_x[i], y = 0.09,
            gp = gpar(fontsize = 7.5, fontface = "bold", col = "#1565C0", lineheight = 1.1))
}

dev.off()
cat("Saved:", out, "\n")
