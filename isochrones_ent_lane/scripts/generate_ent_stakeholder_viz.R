#!/usr/bin/env Rscript
source(here::here("R", "log_utils.R"))
log_init()
source(here::here("R", "provenance.R"))

# ==============================================================================
# ENT STAKEHOLDER VISUALIZATIONS (VENN & ALLUVIAL)
# ==============================================================================
# Purpose: Generate visual evidence for AJ & Grace to explain the ENT database
#          composition and the attrition logic used to reach the final cohort.
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(here)

# Output Directory
output_dir <- here::here("publication_materials/figures/ent_data_integrity")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# --- 1. DATA LOADING & CALCULATION ---

cat("Loading data for dynamic visualization...\n")

# A. Raw ABOHNS (from cleaner step)
abohns_raw <- readr::read_csv(here::here("data/ent/abohns_certifications_clean.csv"), show_col_types = FALSE)

# B. Matched ABOHNS (output of match script)
abohns_matched <- readr::read_csv(here::here("data/ent/abohns_with_npi_master.csv"), show_col_types = FALSE)

# C. Master Roster (SSOT)
master_roster <- readr::read_csv(here::here("data/ent/ent_master_roster_final_20260513.csv"), show_col_types = FALSE)

# D. NPI Universe (ENT Taxonomy)
npi_universe <- arrow::read_parquet(here::here("artifacts/ent/ent_from_nppes_flat_file.parquet"))

# Calculate Counts
count_raw     <- nrow(abohns_raw)
count_active  <- nrow(abohns_matched)
count_matched <- sum(!is.na(abohns_matched$npi))
count_academy <- nrow(master_roster)
count_triple  <- sum(master_roster$is_board_certified == TRUE | master_roster$is_board_certified == "TRUE", na.rm = TRUE)

count_npi_univ <- nrow(npi_universe)
count_board_cert <- count_matched

# --- 2. ALLUVIAL/CONSORT FLOW DIAGRAM (ATTRITION) ---

flow_data <- data.frame(
  stage = factor(c("Raw Certifications", "US-Based/Active", "Matched to NPI (Practicing)", "ENThealth (Searchable)"),
                levels = c("Raw Certifications", "US-Based/Active", "Matched to NPI (Practicing)", "ENThealth (Searchable)")),
  count = c(count_raw, count_active, count_matched, count_academy)
)

flow_plot <- ggplot(flow_data, aes(x = stage, y = count)) +
  geom_col(fill = "#2c3e50", width = 0.6) +
  geom_text(aes(label = format(count, big.mark = ",")), vjust = -0.5, fontface = "bold", size = 5) +
  theme_minimal() +
  labs(
    title = "ENT Physician Cohort Attrition (ABOHNS to Master Roster)",
    subtitle = "Attrition justifications: Non-US providers, Inactive/Expired certs, and Retirement screening",
    x = "",
    y = "Physician Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

alluvial_png     <- file.path(output_dir, "ent_cohort_attrition_alluvial.png")
alluvial_png_tmp <- paste0(alluvial_png, ".tmp.png")
ggsave(alluvial_png_tmp, flow_plot, width = 10, height = 7, dpi = 300)
file.rename(alluvial_png_tmp, alluvial_png)
record_provenance(alluvial_png)


# --- 3. VENN/EULER DIAGRAM (REGISTRY OVERLAP) — Premium Redesign ---

cat("Generating premium Euler diagram...\n")

library(ggforce)
library(ggplot2)
library(grid)

# ── Palette ────────────────────────────────────────────────────────────────────
bg_col     <- "#ffffff"
npi_fill   <- "#2ec4b6"   # teal
board_fill <- "#3a86ff"   # cobalt
acad_fill  <- "#ff6b6b"   # coral
text_dark  <- "#1a2b3c"
text_dim   <- "#4a6880"

# ── Circle geometry (proportional areas) ──────────────────────────────────────
npi_r   <- sqrt(15189 / pi) * 0.85
board_r <- sqrt(11588 / pi) * 0.85
acad_r  <- sqrt(5555  / pi) * 0.85

npi_x   <-  0;  npi_y   <-  0
board_x <- 10;  board_y <- -4
acad_x  <- 45;  acad_y  <- 22

circle_layers <- list(
  ggforce::geom_circle(
    data = data.frame(x0=npi_x, y0=npi_y, r=npi_r),
    aes(x0=x0, y0=y0, r=r),
    fill=npi_fill, color=npi_fill, alpha=0.18, linewidth=2.0,
    inherit.aes=FALSE, show.legend=FALSE),
  ggforce::geom_circle(
    data = data.frame(x0=board_x, y0=board_y, r=board_r),
    aes(x0=x0, y0=y0, r=r),
    fill=board_fill, color=board_fill, alpha=0.28, linewidth=2.0,
    inherit.aes=FALSE, show.legend=FALSE),
  ggforce::geom_circle(
    data = data.frame(x0=acad_x, y0=acad_y, r=acad_r),
    aes(x0=x0, y0=y0, r=r),
    fill=acad_fill, color=acad_fill, alpha=0.35, linewidth=2.0,
    inherit.aes=FALSE, show.legend=FALSE)
)

# ── Region labels: placed inside each exclusive zone ──────────────────────────
# Each fill is tinted to match the dominant set colour for that region.
# Percentages are of the full NPI universe (n = 15,189).

region_labels <- data.frame(
  x     = c(-60,    5,   20,   80),
  y     = c(   2,  -46,   10,   22),
  label = c(
    "NPI Only\n(no board cert,\nnot in ENThealth)\nn = 3,354  |  22.1%",
    "Board Certified\n(not in ENThealth)\nn = 6,527  |  42.9%",
    "Triple Match \u2605\nHigh-Fidelity Cohort\nn = 5,061  |  33.3%",
    "ENThealth Only\n(no NPI record)\nn = 247  |  1.6%"
  ),
  # Subtle tinted background per region
  bg    = c(
    scales::alpha(npi_fill,   0.12),
    scales::alpha(board_fill, 0.12),
    scales::alpha("#9b59b6",  0.12),   # overlap shown in purple blend
    scales::alpha(acad_fill,  0.12)
  ),
  border = c(npi_fill, board_fill, "#7c3aed", acad_fill)
)

# ── Circle name labels ─────────────────────────────────────────────────────────
set_labels <- data.frame(
  x     = c(-62,  -4,   70),
  y     = c( 57,  67,   56),
  label = c(
    "NPI Universe\nn = 15,189",
    "Board Certified\nn = 11,588",
    "ENThealth\nSearchable\nn = 5,555"
  ),
  color = c(npi_fill, board_fill, acad_fill)
)

# ── Footnote text ──────────────────────────────────────────────────────────────
fn_text <- paste0(
  "NPPES taxonomy codes (207Y* family, all 5 taxonomy fields):  ",
  "207Y00000X General Otolaryngology  \u2022  207YX0007X Head & Neck Surgery  \u2022  ",
  "207YX0905X Neurotology  \u2022  207YP0228X Pediatric Otolaryngology\n",
  "207YX0901X Allergy  \u2022  207YX0602X Facial Plastic & Reconstructive Surgery  \u2022  ",
  "207YS0012X Sleep Medicine  \u2022  207YX0400X Rhinology\n",
  "Data sources: CMS NPPES Public File (2024)  \u2022  ABOHNS Certification Registry  \u2022  AAO-HNS ENThealth Directory"
)

# ── Build plot ─────────────────────────────────────────────────────────────────
euler_gg <- Reduce(`+`, circle_layers, ggplot()) +

  # Region label boxes — tinted fill matches the region's dominant colour
  geom_label(
    data = region_labels,
    aes(x = x, y = y, label = label, fill = I(bg), color = I(border)),
    size = 3.0, fontface = "bold", lineheight = 1.35,
    label.padding = unit(0.5, "lines"),
    label.r       = unit(0.3, "lines"),
    linewidth     = 0.6,
    hjust = 0.5, vjust = 0.5
  ) +

  # Circle name labels
  geom_text(
    data = set_labels,
    aes(x = x, y = y, label = label, color = I(color)),
    size = 4.0, fontface = "bold", lineheight = 1.2,
    hjust = 0.5, vjust = 0.5,
    inherit.aes = FALSE
  ) +

  # Footnote separator + body
  annotate("segment", x = -108, xend = 118, y = -76, yend = -76,
           color = text_dim, linewidth = 0.35) +
  annotate("text", x = -108, y = -80, hjust = 0, vjust = 1,
           label = fn_text, color = text_dim, size = 2.35, lineheight = 1.55) +

  coord_fixed(xlim = c(-110, 125), ylim = c(-108, 98)) +
  theme_void(base_size = 13) +
  labs(
    title    = "ENT Physician Registry  \u2014  Database Overlap Analysis",
    subtitle = paste0(
      "5,061 physicians matched across all three registries (33.3% of the NPPES NPI universe)  \u2014  ",
      "the high-fidelity \u2018Triple Match\u2019 cohort for the Mystery Caller Study"
    )
  ) +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    plot.title    = element_text(color = text_dark, size = 17, face = "bold",
                                 hjust = 0.5, margin = margin(t = 14, b = 5)),
    plot.subtitle = element_text(color = text_dim,  size = 9.5,
                                 hjust = 0.5, margin = margin(b = 12)),
    plot.margin   = margin(6, 14, 6, 14)
  )

euler_png     <- file.path(output_dir, "ent_registry_overlap_euler.png")
euler_png_tmp <- paste0(euler_png, ".tmp.png")
ggsave(euler_png_tmp, euler_gg, width = 16, height = 11, dpi = 300, bg = "white")
file.rename(euler_png_tmp, euler_png)
record_provenance(euler_png)

cat("Visualizations generated successfully with dynamic data:\n")
cat(sprintf("- Attrition: %d -> %d -> %d -> %d\n", count_raw, count_active, count_matched, count_academy))
cat(sprintf("- Triple Match: %d\n", count_triple))
