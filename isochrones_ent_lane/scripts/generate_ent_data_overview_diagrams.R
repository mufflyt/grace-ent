#!/usr/bin/env Rscript
# =============================================================================
# ENT DATA OVERVIEW DIAGRAMS (3 charts)
# =============================================================================
#
# Diagram 1 — Data Sources Overview
#   All 6 input sources, processing steps, and outputs in a 3-column layout.
#   Good for: manuscript Methods section intro figure.
#
# Diagram 2 — Data Acquisition Timeline
#   Horizontal Gantt showing when each source was obtained (Jan–May 2026).
#   Good for: reproducibility supplement.
#
# Diagram 3 — Updated STROBE with ABOHNS + ABMS shown as inputs
#   Extends the Laryngoscope STROBE to show board certification data
#   feeding the SSOT hierarchy (left-side dashed boxes).
#   Good for: full data-provenance figure.
#
# Outputs (all to artifacts/ent/):
#   ent_data_sources_overview.png
#   ent_data_acquisition_timeline.png
#   strobe_diagram_ent_full_sources.png
#
# Usage:
#   Rscript scripts/generate_ent_data_overview_diagrams.R
#
# Author: Tyler Muffly MD + Claude Code
# Date:   2026-05-10
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(ggplot2)
  library(dplyr)
})

OUT_DIR <- here::here("artifacts", "ent")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

FONT       <- "Arial"
COL_PRI    <- "#1A3A5C"   # deep navy   — primary cohort sources (ENTHealth, AAO-HNS)
COL_ENRICH <- "#1F618D"   # medium blue — NPI enrichment (NPPES)
COL_BOARD  <- "#0E6655"   # dark teal   — board cert data (ABOHNS, ABMS)
COL_PROC   <- "#2C3E50"   # dark slate  — processing steps
COL_OUT    <- "#D6EAF8"   # light blue  — outputs
SIDE       <- "#F8F9FA"   # near-white  — annotation boxes
# Legacy aliases used by diagram 2 timeline and diagram 3 STROBE
GRAY1 <- COL_PRI
GRAY2 <- COL_ENRICH
GRAY3 <- COL_OUT
LW    <- 0.55
FS    <- 3.0         # body text
FS_SM <- 2.4         # small annotation
FS_TI <- 11          # title

.save_png <- function(p, filename, width = 8, height = 6, dpi = 300) {
  path     <- file.path(OUT_DIR, filename)
  tmp_path <- paste0(path, ".tmp.png")
  ggplot2::ggsave(tmp_path, p, width = width, height = height, dpi = dpi, bg = "white")
  file.rename(tmp_path, path)
  cat(sprintf("  Saved: %s\n", path))
  invisible(path)
}

# ─────────────────────────────────────────────────────────────────────────────
# DIAGRAM 1 — DATA SOURCES OVERVIEW
# Layout: Sources (x≈0.1) → Processing (x≈0.5) → Outputs (x≈0.9)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[1/3] Data sources overview...\n")

# Box helper — text_col defaults to "white" for dark fills, "black" for light fills
box <- function(x, y, label, fill = SIDE, fw = 0.20, fh = 0.065,
                font = "plain", size = FS, col = "black", text_col = NULL) {
  if (is.null(text_col))
    text_col <- if (fill %in% c(SIDE, COL_OUT, "#F8F9FA", "white")) "#1A3A5C" else "white"
  list(
    annotate("rect", xmin = x - fw/2, xmax = x + fw/2,
             ymin = y - fh/2, ymax = y + fh/2,
             fill = fill, color = col, linewidth = LW),
    annotate("text", x = x, y = y, label = label,
             size = size, fontface = font, family = FONT,
             hjust = 0.5, vjust = 0.5, lineheight = 1.2, color = text_col)
  )
}

# Arrow helper
arr <- function(x1, y1, x2, y2, color = "black") {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           linewidth = LW, color = color,
           arrow = arrow(length = unit(0.14, "cm"), type = "closed"))
}

p1 <- ggplot() + theme_void() +
  xlim(0, 1) + ylim(0.02, 1.0) +
  coord_cartesian(clip = "off")

# ── Column headers ────────────────────────────────────────────────────────────
p1 <- p1 +
  annotate("text", x = 0.12, y = 0.97, label = "DATA SOURCES", size = FS + 0.4,
           fontface = "bold", family = FONT, color = COL_PRI) +
  annotate("text", x = 0.50, y = 0.97, label = "PROCESSING", size = FS + 0.4,
           fontface = "bold", family = FONT, color = COL_PROC) +
  annotate("text", x = 0.88, y = 0.97, label = "OUTPUTS", size = FS + 0.4,
           fontface = "bold", family = FONT, color = COL_PRI)

# ── SOURCE BOXES (left column, x=0.12) ───────────────────────────────────────
SRC_X <- 0.12
src_positions <- list(
  list(y = 0.86, label = "ENTHealth.org\nPhysician Directory\n(N = 7,462, Feb 2026)",
       fill = COL_PRI, col = COL_PRI, font = "bold"),
  list(y = 0.72, label = "AAO-HNS Member List\n(N = 7,198)\nT. Maggio, March 2026",
       fill = COL_PRI, col = COL_PRI, font = "bold"),
  list(y = 0.55, label = "NPPES Flat File\n(Feb 2026)\nTaxonomy 207Y*",
       fill = COL_ENRICH, col = COL_ENRICH, font = "plain"),
  list(y = 0.38, label = "ABOHNS\nCertification Hand Search\n(abohns_certifications.csv)",
       fill = COL_BOARD, col = COL_BOARD, font = "plain"),
  list(y = 0.22, label = "ABMS Certifications\nDatabase (2026)\nabms_ent_certifications.csv",
       fill = COL_BOARD, col = COL_BOARD, font = "plain")
)

for (s in src_positions) {
  p1 <- p1 + box(SRC_X, s$y, s$label, fill = s$fill, col = s$col,
                 font = s$font, size = FS - 0.2, fw = 0.22, fh = 0.09)
}

# ── PROCESSING BOXES (center column, x=0.50) ─────────────────────────────────
PROC_X <- 0.50
proc_positions <- list(
  list(y = 0.79,
       label = "Name Matching & Inner Join\nENTHealth (7,462) matched to\nAAO-HNS (7,198)\n5,735 confirmed  |  1,727 excluded\n(not confirmed AAO-HNS members)"),
  list(y = 0.52,
       label = "NPI Identifier Enrichment\nOf 5,735 confirmed members:\n5,484 NPI found (98.8%)\n251 no NPI — retained in cohort"),
  list(y = 0.24,
       label = "Board Certification\nOverlay\n(ABOHNS + ABMS quality annotation)")
)

proc_fh <- c(0.155, 0.12, 0.10)  # Name Matching needs 5 lines; others 4/3
for (i in seq_along(proc_positions)) {
  p1 <- p1 + box(PROC_X, proc_positions[[i]]$y, proc_positions[[i]]$label,
                 fill = COL_PROC, col = COL_PROC,
                 font = "plain", size = FS - 0.3, fw = 0.26, fh = proc_fh[i])
}

# ── OUTPUT BOXES (right column, x=0.88) ──────────────────────────────────────
OUT_X <- 0.88
out_positions <- list(
  list(y = 0.67, label = "ENT Master Roster\nSingle Source of Truth\n(ent_master_roster_ssot_*.csv)",
       fw = 0.22, fh = 0.10),
  list(y = 0.40, label = "Final Analytic Cohort\nN = 5,735 (all confirmed)\nN = 5,549 (mystery-caller eligible)\nNPI available: 5,484 (98.8%)",
       fw = 0.24, fh = 0.14)
)

for (o in out_positions) {
  p1 <- p1 + box(OUT_X, o$y, o$label, fill = COL_OUT, col = COL_PRI,
                 font = "plain", size = FS - 0.2, fw = o$fw, fh = o$fh)
}

# ── ARROWS ────────────────────────────────────────────────────────────────────
# ENTHealth (0.86) + AAO-HNS (0.72) → Name Matching top (0.79+0.06=0.85)
p1 <- p1 + arr(SRC_X + 0.11, 0.86, PROC_X - 0.12, 0.83)
p1 <- p1 + arr(SRC_X + 0.11, 0.72, PROC_X - 0.12, 0.77)

# Name Matching bottom (0.79-0.0775=0.712) → NPI Enrichment top (0.52+0.06=0.58)
p1 <- p1 + annotate("segment", x = PROC_X, y = 0.712, xend = PROC_X, yend = 0.58,
                    linewidth = LW, arrow = arrow(length = unit(0.14, "cm"), type = "closed"))

# Name Matching → Master Roster Single Source of Truth (y=0.66)
p1 <- p1 + arr(PROC_X + 0.12, 0.79, OUT_X - 0.11, 0.68)

# NPPES (0.55) → NPI Enrichment center (0.52)
p1 <- p1 + arr(SRC_X + 0.11, 0.55, PROC_X - 0.12, 0.53)

# NPI Enrichment → Final Cohort top edge (y=0.40+0.07=0.47)
p1 <- p1 + arr(PROC_X + 0.12, 0.52, OUT_X - 0.12, 0.47)

# ABOHNS (0.38) + ABMS (0.22) → Board Cert Overlay center (0.24)
p1 <- p1 + arr(SRC_X + 0.11, 0.38, PROC_X - 0.12, 0.27)
p1 <- p1 + arr(SRC_X + 0.11, 0.22, PROC_X - 0.12, 0.23)

# Board Cert → Single Source of Truth (dotted — quality annotation only)
p1 <- p1 + annotate("segment", x = PROC_X + 0.12, y = 0.24,
                    xend = OUT_X - 0.11, yend = 0.62,
                    linewidth = LW, linetype = "dotted", color = COL_PRI,
                    arrow = arrow(length = unit(0.14, "cm"), type = "closed"))

# ── Column separators ─────────────────────────────────────────────────────────
p1 <- p1 +
  annotate("segment", x = 0.275, y = 0.04, xend = 0.275, yend = 0.94,
           linewidth = 0.3, color = "grey80", linetype = "dashed") +
  annotate("segment", x = 0.715, y = 0.04, xend = 0.715, yend = 0.94,
           linewidth = 0.3, color = "grey80", linetype = "dashed")

# ── Footnote legend ───────────────────────────────────────────────────────────
p1 <- p1 +
  annotate("rect", xmin = 0.01, xmax = 0.99, ymin = 0.02, ymax = 0.12,
           fill = SIDE, color = "grey80", linewidth = 0.3) +
  annotate("text", x = 0.50, y = 0.09,
           label = paste0(
             "Navy = primary cohort sources (ENTHealth.org physician directory + AAO-HNS member list).\n",
             "Blue = NPI enrichment (NPPES).  Teal = board certification data (ABOHNS, ABMS).\n",
             "Dotted arrow = board cert feeds SSOT as a quality annotation, not a cohort inclusion criterion."
           ),
           hjust = 0.5, vjust = 1, size = FS_SM, family = FONT, lineheight = 1.35)

p1 <- p1 +
  labs(title = "ENT Workforce Study — Data Sources & Processing Pipeline",
       subtitle = "Six input sources → three processing stages → two primary outputs") +
  theme(
    plot.title    = element_text(family = FONT, hjust = 0.5, size = FS_TI,
                                 face = "bold", margin = ggplot2::margin(b = 3)),
    plot.subtitle = element_text(family = FONT, hjust = 0.5, size = FS_TI - 2,
                                 face = "italic", margin = ggplot2::margin(b = 8)),
    plot.margin   = ggplot2::margin(12, 8, 8, 8)
  )

.save_png(p1, "ent_data_sources_overview.png", width = 9, height = 6.5)

# ─────────────────────────────────────────────────────────────────────────────
# DIAGRAM 2 — DATA ACQUISITION TIMELINE
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[2/3] Data acquisition timeline...\n")

# date_decimal converts YYYY-MM-DD to a numeric month fraction relative to Jan 2026
to_month <- function(date_str) {
  d <- as.Date(date_str)
  as.numeric(format(d, "%Y")) * 12 + as.numeric(format(d, "%m")) - (2026 * 12 + 1)
}

sources_tbl <- tibble::tribble(
  ~source,                  ~category,     ~acquired,     ~available_through, ~notes,
  "ENTHealth.org Hand Search",   "Primary",     "2026-02-01",  "2026-05-10",        "WordPress AJAX hand search\nN = 7,462 physicians",
  "AAO-HNS Member List",    "Primary",     "2026-03-05",  "2026-05-10",        "Provided by Tina Maggio\n(CCO, AAO-HNS)",
  "NPPES Flat File",        "Enrichment",  "2026-02-08",  "2026-05-10",        "Feb 2026 dissemination\n207Y* taxonomy",
  "ABOHNS Certifications",  "Board Data",  "2026-02-15",  "2026-05-10",        "abohns_certifications.csv\n(certification database)",
  "ABMS Certifications",    "Board Data",  "2026-04-01",  "2026-05-10",        "Certifications database\nabms_ent_certifications\n_checkpoint.csv"
)

sources_tbl <- sources_tbl %>%
  mutate(
    y        = rev(seq_len(n())),
    x_start  = to_month(acquired),
    x_end    = to_month(available_through),
    fill_col = case_when(
      category == "Primary"    ~ COL_PRI,
      category == "Enrichment" ~ COL_ENRICH,
      TRUE                     ~ COL_BOARD
    ),
    label_col = "white"
  )

month_breaks <- 0:4
month_labels <- c("Jan 2026", "Feb 2026", "Mar 2026", "Apr 2026", "May 2026")

p2 <- ggplot(sources_tbl) +
  geom_rect(aes(xmin = x_start, xmax = x_end, ymin = y - 0.38, ymax = y + 0.38,
                fill = fill_col), color = "black", linewidth = 0.4) +
  geom_point(aes(x = x_start, y = y), shape = 21, size = 4.5,
             fill = "white", color = "black", stroke = 0.8) +
  geom_text(aes(x = x_start, y = y, label = format(as.Date(acquired), "%b %d")),
            size = FS_SM, vjust = -1.0, family = FONT, color = "black") +
  geom_text(aes(x = (x_start + x_end) / 2, y = y, label = notes),
            size = FS_SM - 0.15, color = "white", family = FONT,
            hjust = 0.5, vjust = 0.5, lineheight = 1.1) +
  scale_fill_identity() +
  scale_x_continuous(breaks = month_breaks, labels = month_labels,
                     limits = c(-0.2, 4.5)) +
  scale_y_continuous(breaks = sources_tbl$y, labels = sources_tbl$source,
                     limits = c(0.4, max(sources_tbl$y) + 0.8)) +
  labs(
    title    = "ENT Workforce Study — Data Acquisition Timeline",
    subtitle = "When each data source was obtained (January–May 2026)",
    x        = NULL,
    y        = NULL,
    caption  = paste0(
      "Dark gray = primary cohort sources. Medium gray = NPI enrichment. Light gray = board certification data.\n",
      "White circle = acquisition date. Bar extends to date of analysis (May 2026)."
    )
  ) +
  theme_minimal(base_family = FONT) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
    axis.text.y        = element_text(size = 9, hjust = 1, color = "black"),
    axis.text.x        = element_text(size = 8.5, color = "black"),
    plot.title         = element_text(hjust = 0.5, size = FS_TI, face = "bold",
                                      margin = ggplot2::margin(b = 3)),
    plot.subtitle      = element_text(hjust = 0.5, size = FS_TI - 2, face = "italic",
                                      margin = ggplot2::margin(b = 8)),
    plot.caption       = element_text(hjust = 0, size = 7, color = "grey40",
                                      margin = ggplot2::margin(t = 8)),
    plot.margin        = ggplot2::margin(12, 20, 10, 8)
  )

.save_png(p2, "ent_data_acquisition_timeline.png", width = 9, height = 5)

# ─────────────────────────────────────────────────────────────────────────────
# DIAGRAM 3 — UPDATED STROBE WITH ABOHNS + ABMS SHOWN AS LEFT-SIDE INPUTS
# Extends the Laryngoscope STROBE (generate_ent_strobe_diagram.R):
#   adds two additional dashed left-side source boxes feeding the SSOT layer
# ─────────────────────────────────────────────────────────────────────────────
cat("\n[3/3] Updated STROBE with board cert sources...\n")

# Style — same constants as generate_ent_strobe_diagram.R
LRY_TOP   <- "#C0C0C0"
LRY_MID   <- "#EBEBEB"
LRY_FINAL <- "#7A7A7A"
LRY_SIDE  <- "#F7F7F7"
LRY_EXCL  <- "#FFFFFF"
LRY_LW    <- 0.6
LRY_CONN  <- 0.45

FS_BOX    <- 3.1
FS_DETAIL <- 2.4
FS_TITLE  <- 11
FS_SUB    <- 7.5
FS_CAP    <- 6

create_box <- function(label, x, y, width = 0.36, height = 0.075,
                       fill = LRY_MID, fontface = "plain", size = FS_BOX) {
  list(
    annotate("rect",
             xmin = x - width/2, xmax = x + width/2,
             ymin = y - height/2, ymax = y + height/2,
             fill = fill, color = "black", linewidth = LRY_LW),
    annotate("text", x = x, y = y, label = label,
             size = size, fontface = fontface, family = FONT, lineheight = 1.25)
  )
}

create_arrow3 <- function(x1, y1, x2, y2) {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
           linewidth = LRY_LW)
}

create_side_box <- function(p, items, y_center,
                             x_from     = X_CTR + 0.21,
                             x_box_left = 0.63,
                             box_width  = 0.52,
                             fill       = LRY_SIDE,
                             lty        = "solid",
                             size       = FS_DETAIL) {
  n_lines  <- length(items)
  line_ht  <- 0.027
  pad      <- 0.016
  box_h    <- n_lines * line_ht + 2 * pad
  x_box_r  <- x_box_left + box_width

  p <- p + annotate("segment",
    x = x_from, y = y_center, xend = x_box_left, yend = y_center,
    linewidth = LRY_CONN, color = "black")
  p <- p + annotate("rect",
    xmin = x_box_left, xmax = x_box_r,
    ymin = y_center - box_h/2, ymax = y_center + box_h/2,
    fill = fill, color = "black", linewidth = LRY_LW, linetype = lty)

  dy <- y_center + (n_lines - 1) * line_ht / 2
  for (item in items) {
    p <- p + annotate("text",
      x = x_box_left + 0.014, y = dy,
      label = item, hjust = 0, vjust = 0.5,
      size = size, family = FONT)
    dy <- dy - line_ht
  }
  p
}

# Layout constants — identical to original STROBE script
BOX_H  <- 0.075
STEP   <- 0.17
X_CTR  <- 0.42
BOX_HW <- 0.18

y1      <- 0.91
y_aao   <- y1 - STEP / 2
y3      <- y1 - STEP
y_ex    <- (y3 + BOX_H/2 + y1 - BOX_H/2) / 2
y_elg   <- y3 - STEP
y_excl2 <- (y_elg + BOX_H/2 + y3 - BOX_H/2) / 2
y4      <- y_elg - STEP
y5      <- y4   - STEP

# NEW: additional row for SSOT/board overlay above y5
y_ssot  <- y5 - STEP * 0.6
y_final <- y5 - STEP * 1.2

BOX_RIGHT <- X_CTR + BOX_HW
BOX_LEFT  <- X_CTR - BOX_HW
X_BOX_L   <- BOX_RIGHT + 0.04
BOX_W     <- 0.49

p3 <- ggplot() + theme_void() + xlim(0, 1.16) + ylim(0.02, 1)

# ── L1: ENTHealth scrape ──────────────────────────────────────────────────────
p3 <- p3 + create_box(
  "Physicians Identified\nvia ENTHealth.org  (N = 7,462)",
  X_CTR, y1, fill = LRY_TOP, fontface = "bold"
)
p3 <- create_side_box(p3,
  items = c(
    "Source: ENTHealth.org/find-ent/ (February 2026)",
    "Quality note: n = 250 without geocoded coordinates (retained)"
  ),
  y_center = y1, x_from = BOX_RIGHT, x_box_left = X_BOX_L, box_width = BOX_W
)
p3 <- p3 + create_arrow3(X_CTR, y1 - BOX_H/2, X_CTR, y3 + BOX_H/2)

# ── AAO-HNS sidebar (left, dashed) ───────────────────────────────────────────
AAO_R <- BOX_LEFT - 0.04
AAO_L <- 0.01
AAO_X <- (AAO_L + AAO_R) / 2
p3 <- p3 +
  annotate("rect", xmin = AAO_L, xmax = AAO_R,
           ymin = y_aao - 0.050, ymax = y_aao + 0.050,
           fill = LRY_SIDE, color = "black", linewidth = LRY_LW, linetype = "dashed") +
  annotate("text", x = AAO_X, y = y_aao,
           label = "AAO-HNS Member List\n(N = 7,198)\nT. Maggio, CCO\nAAO-HNS, March 5, 2026",
           size = 2.45, lineheight = 1.2, hjust = 0.5, family = FONT) +
  annotate("segment", x = AAO_R, y = y_aao, xend = BOX_LEFT, yend = y_aao,
           arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
           linewidth = LRY_LW)

# Exclusion side box
p3 <- create_side_box(p3,
  items = c(
    "Excluded — ENThealth-only (not AAO-HNS member): n = 1,727",
    "AAO-HNS-only (name mismatch; not excluded): n = 1,463",
    "Name matching: humaniformat::parse_names() + state"
  ),
  y_center = y_ex, x_from = BOX_RIGHT, x_box_left = X_BOX_L,
  box_width = BOX_W, fill = LRY_EXCL
)

# ── L2: Confirmed active otolaryngologists ────────────────────────────────────
p3 <- p3 + create_box(
  "Confirmed Active Otolaryngologists\n(N = 5,735)",
  X_CTR, y3, fill = LRY_MID
)
p3 <- p3 + create_arrow3(X_CTR, y3 - BOX_H/2, X_CTR, y_elg + BOX_H/2)
p3 <- create_side_box(p3,
  items = c(
    "Excluded — no phone number on ENThealth.org: n = 173",
    "Excluded — Puerto Rico address (no English-speaking callers): n = 14",
    "Total excluded: n = 186"
  ),
  y_center = y_excl2, x_from = BOX_RIGHT, x_box_left = X_BOX_L,
  box_width = BOX_W, fill = LRY_EXCL
)

# ── L3: Eligible for mystery-caller study ────────────────────────────────────
p3 <- p3 + create_box(
  "Eligible for Mystery-Caller Study\n(N = 5,549)",
  X_CTR, y_elg, fill = LRY_MID
)
p3 <- p3 + create_arrow3(X_CTR, y_elg - BOX_H/2, X_CTR, y4 + BOX_H/2)

# ── L4: NPI identifier search ─────────────────────────────────────────────────
p3 <- p3 + create_box(
  "NPI Identifier Search\n(NPPES Feb 2026;  N = 5,549)",
  X_CTR, y4, fill = LRY_MID
)
p3 <- create_side_box(p3,
  items = c(
    "NPI identified: n = 5,484 (98.8%)",
    "  Pass 1 — exact (last + state + initial): n = 5,010",
    "  Pass 2 — relax first initial: n = 95",
    "  Pass 3 — fuzzy / special-char strip: n = 17",
    "No NPI found — retained: n = 251 (4.5%; not excluded)"
  ),
  y_center = y4, x_from = BOX_RIGHT, x_box_left = X_BOX_L, box_width = BOX_W
)
p3 <- p3 + create_arrow3(X_CTR, y4 - BOX_H/2, X_CTR, y5 + BOX_H/2)

# ── L5: Board certification SSOT overlay (NEW) ───────────────────────────────
# Two new left-side dashed boxes: ABOHNS + ABMS
ABOHNS_R <- BOX_LEFT - 0.04
ABOHNS_L <- 0.01
ABOHNS_X <- (ABOHNS_L + ABOHNS_R) / 2

# ABOHNS box (higher) — half-height 0.058 fits 3 lines comfortably
abohns_y <- y5 + 0.09
p3 <- p3 +
  annotate("rect", xmin = ABOHNS_L, xmax = ABOHNS_R,
           ymin = abohns_y - 0.058, ymax = abohns_y + 0.058,
           fill = LRY_SIDE, color = "black", linewidth = LRY_LW, linetype = "dashed") +
  annotate("text", x = ABOHNS_X, y = abohns_y,
           label = "ABOHNS\nCertifications\n(abohns_certifications.csv)",
           size = 2.3, lineheight = 1.3, hjust = 0.5, family = FONT) +
  annotate("segment", x = ABOHNS_R, y = abohns_y,
           xend = BOX_LEFT, yend = y5 + 0.020,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           linewidth = LRY_LW, linetype = "dotted")

# ABMS box (lower) — same height, spaced to avoid overlap with ABOHNS
abms_y <- y5 - 0.09
p3 <- p3 +
  annotate("rect", xmin = ABOHNS_L, xmax = ABOHNS_R,
           ymin = abms_y - 0.058, ymax = abms_y + 0.058,
           fill = LRY_SIDE, color = "black", linewidth = LRY_LW, linetype = "dashed") +
  annotate("text", x = ABOHNS_X, y = abms_y,
           label = "ABMS Certifications\nDatabase (2026)\nabms_ent_certifications.csv",
           size = 2.3, lineheight = 1.3, hjust = 0.5, family = FONT) +
  annotate("segment", x = ABOHNS_R, y = abms_y,
           xend = BOX_LEFT, yend = y5 - 0.020,
           arrow = arrow(length = unit(0.13, "cm"), type = "closed"),
           linewidth = LRY_LW, linetype = "dotted")

# ── L5: Final analytic cohort ─────────────────────────────────────────────────
p3 <- p3 + create_box(
  "Final Analytic Cohort\n(N = 5,735 / 5,549)",
  X_CTR, y5, fill = LRY_FINAL, fontface = "bold", size = FS_BOX + 0.2
)
p3 <- create_side_box(p3,
  items = c(
    "US attending otolaryngologists",
    "Confirmed active AAO-HNS members (March 2026)",
    "NPPES NPI available: n = 5,484 (99%)",
    "Geocoded coordinates: n = 5,526 of 5,549",
    "Board cert overlay: ABOHNS + ABMS (quality annotation)"
  ),
  y_center = y5, x_from = BOX_RIGHT, x_box_left = X_BOX_L, box_width = BOX_W
)

# ── Title ─────────────────────────────────────────────────────────────────────
p3 <- p3 +
  labs(
    title    = "ENT Workforce Study — Complete Data Provenance",
    subtitle = paste0(
      "ENTHealth.org directory → AAO-HNS member confirmation → NPPES NPI enrichment ",
      "→ ABOHNS/ABMS board overlay"
    ),
    caption  = paste0(
      "ENTHealth.org: AAO-HNS public directory (Feb 2026). ",
      "AAO-HNS list: Tina Maggio, CCO, AAO-HNS (received March 5, 2026). ",
      "NPPES: Feb 2026 CMS flat file. ",
      "ABOHNS: American Board of Otolaryngology–Head and Neck Surgery (bulk download). ",
      "ABMS: American Board of Medical Specialties (Playwright hand search, 2026). ",
      "Dashed left boxes = external reference data (enrichment/quality annotation only, not cohort inclusion criteria)."
    )
  ) +
  theme(
    text          = element_text(family = FONT),
    plot.title    = element_text(hjust = 0.5, size = FS_TITLE, face = "bold",
                                 margin = ggplot2::margin(b = 3)),
    plot.subtitle = element_text(hjust = 0.5, size = FS_SUB, face = "italic",
                                 margin = ggplot2::margin(b = 6)),
    plot.caption  = element_text(hjust = 0, size = FS_CAP,
                                 margin = ggplot2::margin(t = 9)),
    plot.margin   = ggplot2::margin(12, 8, 14, 8)
  )

.save_png(p3, "strobe_diagram_ent_full_sources.png", width = 7, height = 11)

cat("\n=== COMPLETE ===\n")
cat(sprintf("All 3 diagrams saved to: %s\n", OUT_DIR))
cat("  ent_data_sources_overview.png\n")
cat("  ent_data_acquisition_timeline.png\n")
cat("  strobe_diagram_ent_full_sources.png\n")
