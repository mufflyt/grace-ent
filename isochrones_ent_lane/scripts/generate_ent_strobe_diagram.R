#!/usr/bin/env Rscript
# =============================================================================
# STROBE FLOW DIAGRAM — ENT Workforce Cohort Assembly
# =============================================================================
#
# Cohort assembly pipeline (three stages):
#
#   Stage 1 — ENThealth.org scrape
#     enthealth.org/find-ent/ → results_geocoded.xlsx (Corbi Dropbox)
#     All 7,462 scraped physicians included (non-geocoded flagged, not excluded)
#
#   Stage 2 — Inner join with AAO-HNS attending list
#     /Users/tylermuffly/Documents/ResultsGrid_ExportData.xlsx
#     Provided March 5, 2026, by Tina Maggio (Chief Communications Officer,
#     AAO-HNS).  Contains only US actively practicing otolaryngologists
#     (no residents, no retired members).
#     Name standardization: humaniformat::parse_names() after credential strip
#     Join key: strip_special(last) + first_initial + state
#
#   Stage 3 — NPI matching (enrichment, NOT exclusion)
#     3-pass algorithm vs NPPES February 2026 flat file (207Y* taxonomy)
#     Physicians without NPI are RETAINED in final cohort
#     Results: data/ent/aao_npi_matched.csv
#
# Non-exclusions (by design):
#   - 250 ENThealth physicians not geocoded → quality note, retained
#   - Physicians without NPPES NPI        → quality note, retained
#
# Outputs:
#   artifacts/ent/strobe_diagram_ent.png    (300 DPI)
#   artifacts/ent/strobe_diagram_ent.tiff   (300 DPI, LZW)
#   artifacts/ent/strobe_diagram_ent.csv    (machine-readable counts)
#
# Usage:
#   Rscript scripts/generate_ent_strobe_diagram.R
#
# Author: Tyler Muffly MD + Claude Code
# Date:   2026-05-04
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(readxl)
  library(ggplot2)
  library(stringr)
  library(humaniformat)
})

source(here::here("R", "ent_cohort_counts.R"))

# ── Output directory ──────────────────────────────────────────────────────────
OUT_DIR <- here::here("artifacts", "ent")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_PNG  <- file.path(OUT_DIR, "strobe_diagram_ent_laryngoscope.png")
OUT_TIFF <- file.path(OUT_DIR, "strobe_diagram_ent_laryngoscope.tiff")
OUT_CSV  <- file.path(OUT_DIR, "strobe_diagram_ent.csv")

cat("\n=======================================================================\n")
cat("  ENT STROBE FLOW DIAGRAM\n")
cat(sprintf("  Started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=======================================================================\n\n")

# ── Input paths ───────────────────────────────────────────────────────────────
DROPBOX_DATA <- if (dir.exists(file.path(
    path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data"
  ))) {
  file.path(path.expand("~"), "Dropbox (Personal)", "mystery_shopper", "Corbi study", "ENT", "Data")
} else {
  file.path(path.expand("~"), "Dropbox", "mystery_shopper", "Corbi study", "ENT", "Data")
}
if (!dir.exists(DROPBOX_DATA)) stop("Dropbox ENT/Data directory not found")

ENTHEALTH_CSV <- file.path(DROPBOX_DATA, "results_geocoded.csv")
AAO_XLSX      <- Sys.getenv("AAO_XLSX_PATH",
  unset = file.path(path.expand("~"), "Documents", "ResultsGrid_ExportData.xlsx"))
NPI_MATCH_CSV <- here::here("data", "ent", "aao_npi_matched.csv")

for (p in c(ENTHEALTH_CSV, AAO_XLSX, NPI_MATCH_CSV)) {
  if (!file.exists(p)) stop("Required file not found: ", p)
}

# =============================================================================
# SECTION 1+2 — LOAD DATA AND COMPUTE ALL STAGE COUNTS
# =============================================================================
# All counts are computed live from source files via the shared helper.
# See R/ent_cohort_counts.R for the full computation.

ct <- compute_ent_cohort_counts(ENTHEALTH_CSV, AAO_XLSX, NPI_MATCH_CSV, verbose = TRUE)

# Unpack into local variables for the diagram code below
n_enthealth_raw     <- ct$n_enthealth_raw
n_not_geocoded      <- ct$n_not_geocoded
n_geocoded          <- ct$n_geocoded
n_aao_total         <- ct$n_aao_total
n_inner_join        <- ct$n_inner_join
n_enth_only         <- ct$n_enth_only
n_aao_only          <- ct$n_aao_only
n_npi_matched_inner <- ct$n_npi_matched_inner
n_no_npi_total      <- ct$n_no_npi_total
pass_counts         <- ct$pass_counts
n_no_phone          <- ct$n_no_phone
n_puerto_rico       <- ct$n_puerto_rico
n_excl_elig         <- ct$n_excl_elig
n_after_elig        <- ct$n_after_elig
n_final             <- ct$n_final
matched_keys        <- ct$matched_keys
enth_keys_df        <- ct$enth_keys_df
enthealth           <- ct$enthealth

cat(sprintf("\n  FINAL ANALYTIC COHORT: %s\n", format(n_final, big.mark = ",")))

# =============================================================================
# SECTION 3 — SAVE COUNTS TABLE
# =============================================================================

counts_tbl <- tibble::tribble(
  ~stage, ~label,                                                               ~n,
  "S0",   "ENThealth physicians scraped (all)",                                 n_enthealth_raw,
  "S0q",  "Not geocoded (quality note — retained)",                             n_not_geocoded,
  "S0g",  "Geocoded",                                                           n_geocoded,
  "S1",   "AAO-HNS attending list (US, active, no residents/retired)",           n_aao_total,
  "S2",   "Physicians in both ENThealth & AAO-HNS (inner join)",                n_inner_join,
  "S2x",  "ENThealth-only: not confirmed AAO-HNS member (excluded)",            n_enth_only,
  "S2n",  "AAO-HNS-only: not matched in ENThealth scrape (name mismatch)",      n_aao_only,
  "S3",   "Eligible for mystery-caller study (after exclusions)",               n_after_elig,
  "S3a",  "Excluded — no phone number on ENThealth.org",                        n_no_phone,
  "S3b",  "Excluded — Puerto Rico address (no native-Spanish callers)",         n_puerto_rico,
  "S4",   "Final analytic cohort",                                              n_final,
  "S4q",  "NPI found in NPPES Feb 2026 (enrichment)",                           n_npi_matched_inner,
  "S4q2", "No NPI found (retained — NPI not required for inclusion)",           n_no_npi_total
)

tmp_counts_csv <- paste0(OUT_CSV, ".tmp")
write_csv(counts_tbl, tmp_counts_csv)
file.rename(tmp_counts_csv, OUT_CSV)
cat(sprintf("\nCounts table saved: %s\n", OUT_CSV))

# =============================================================================
# SECTION 4 — DRAW STROBE DIAGRAM  (The Laryngoscope journal style)
# =============================================================================
# Style spec:
#   Font      : Arial (Wiley standard for The Laryngoscope)
#   Fills     : Grayscale only (print-safe; color figures require author fee)
#   Annotations: Bordered side boxes connected by plain horizontal lines
#                (CONSORT standard; no floating-text arrows)
#   Line art  : 600 DPI TIFF with LZW compression
#   Width     : 7 in (178 mm — within 80–180 mm Wiley column spec)
# =============================================================================
cat("\nBuilding STROBE diagram (The Laryngoscope style)...\n")

# ── Style constants ───────────────────────────────────────────────────────────
LRY_FONT  <- "Arial"
LRY_TOP   <- "#C0C0C0"  # starting box:  medium-light gray
LRY_MID   <- "#EBEBEB"  # middle boxes:  light gray (slightly warmer than white)
LRY_FINAL <- "#7A7A7A"  # final cohort:  medium gray
LRY_SIDE  <- "#F7F7F7"  # info side boxes: near-white
LRY_EXCL  <- "#FFFFFF"  # exclusion box: white (distinguished from info boxes)
LRY_LW    <- 0.6        # box border line width
LRY_CONN  <- 0.45       # connector line width (thinner for hierarchy)

FS_BOX    <- 3.1        # ~8.8 pt — main box text
FS_DETAIL <- 2.4        # ~7 pt   — side box text
FS_TITLE  <- 11
FS_SUB    <- 7.5
FS_CAP    <- 6

# ── Helper: main flow box ────────────────────────────────────────────────────
create_box <- function(label, x, y, width = 0.36, height = 0.075,
                       fill = LRY_MID, fontface = "plain", size = FS_BOX) {
  list(
    annotate("rect",
             xmin = x - width/2, xmax = x + width/2,
             ymin = y - height/2, ymax = y + height/2,
             fill = fill, color = "black", linewidth = LRY_LW),
    annotate("text", x = x, y = y, label = label,
             size = size, fontface = fontface,
             family = LRY_FONT, lineheight = 1.25)
  )
}

# ── Helper: vertical arrow between boxes ────────────────────────────────────
create_arrow <- function(x1, y1, x2, y2) {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
           linewidth = LRY_LW)
}

# ── Helper: bordered side box connected by a plain horizontal line ───────────
# x_from  : right edge of the main box (or X_CTR for arrow-midpoint branches)
# y_center: vertical centre of this side box and its connector
# items   : character vector, one line per element
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

  # Plain horizontal connector (no arrowhead — cleaner than arrow-to-text)
  p <- p + annotate("segment",
    x = x_from, y = y_center, xend = x_box_left, yend = y_center,
    linewidth = LRY_CONN, color = "black")

  # Bordered rectangle
  p <- p + annotate("rect",
    xmin = x_box_left, xmax = x_box_r,
    ymin = y_center - box_h/2, ymax = y_center + box_h/2,
    fill = fill, color = "black", linewidth = LRY_LW, linetype = lty)

  # Text lines, top-aligned inside box
  dy <- y_center + (n_lines - 1) * line_ht / 2
  for (item in items) {
    p <- p + annotate("text",
      x = x_box_left + 0.014, y = dy,
      label = item, hjust = 0, vjust = 0.5,
      size = size, family = LRY_FONT)
    dy <- dy - line_ht
  }
  p
}

# ── Layout constants ──────────────────────────────────────────────────────────
BOX_H  <- 0.075
STEP   <- 0.17   # reduced from 0.21 to fit the extra eligibility level
X_CTR  <- 0.42   # main-flow column — shifted right to clear the AAO-HNS sidebar
BOX_HW <- 0.18   # half-width of main flow boxes  (full width = 0.36)

# Computed positions (6-level flow)
y1      <- 0.91                                      # L1: ENThealth scrape
y_aao   <- y1 - STEP / 2                             #     AAO-HNS sidebar (left)
y3      <- y1 - STEP                                 # L2: Inner join
y_ex    <- (y3 + BOX_H/2 + y1 - BOX_H/2) / 2        #     ENThealth-only exclusion
y_elg   <- y3 - STEP                                 # L3: Eligible physicians (new)
y_excl2 <- (y_elg + BOX_H/2 + y3 - BOX_H/2) / 2    #     Phone + PR exclusion
y4      <- y_elg - STEP                              # L4: NPI identifier search
y5      <- y4   - STEP                               # L5: Final analytic cohort

BOX_RIGHT <- X_CTR + BOX_HW   # right edge of main flow boxes  (0.60)
BOX_LEFT  <- X_CTR - BOX_HW   # left edge of main flow boxes   (0.24)
X_BOX_L   <- BOX_RIGHT + 0.04 # left edge of all right-side boxes (0.64)
BOX_W     <- 0.49              # side box width (0.64 → 1.13)

p <- ggplot() + theme_void() + xlim(0, 1.16) + ylim(0.07, 1)

# ── Level 1: ENThealth scrape ─────────────────────────────────────────────────
p <- p + create_box(
  sprintf("Physicians Identified\nvia ENThealth.org  (N = %s)",
          format(n_enthealth_raw, big.mark = ",")),
  X_CTR, y1, fill = LRY_TOP, fontface = "bold"
)

# Side box: source + quality note (connects to right edge of box)
p <- create_side_box(p,
  items = c(
    "Source: ENThealth.org/find-ent/ (February 2026)",
    sprintf("Quality note: n = %s without geocoded coordinates (retained)",
            format(n_not_geocoded, big.mark = ","))
  ),
  y_center   = y1,
  x_from     = BOX_RIGHT,
  x_box_left = X_BOX_L,
  box_width  = BOX_W,
  fill       = LRY_SIDE
)

# Vertical arrow down to inner-join box
p <- p + create_arrow(X_CTR, y1 - BOX_H/2, X_CTR, y3 + BOX_H/2)

# ── AAO-HNS sidebar (left side, dashed = external source) ────────────────────
# Sidebar spans 0.00 → 0.20; main box left edge is at BOX_LEFT (0.24); gap = 0.04
AAO_R <- BOX_LEFT - 0.04   # right edge of AAO-HNS box
AAO_L <- 0.01
AAO_X <- (AAO_L + AAO_R) / 2

p <- p +
  annotate("rect",
           xmin = AAO_L, xmax = AAO_R,
           ymin = y_aao - 0.050, ymax = y_aao + 0.050,
           fill = LRY_SIDE, color = "black", linewidth = LRY_LW,
           linetype = "dashed") +
  annotate("text", x = AAO_X, y = y_aao,
           label = sprintf(
             "AAO-HNS Member List\n(N = %s)\nT. Maggio, CCO\nAAO-HNS, March 5, 2026",
             format(n_aao_total, big.mark = ",")),
           size = 2.45, lineheight = 1.2, hjust = 0.5, family = LRY_FONT) +
  annotate("segment",
           x = AAO_R, y = y_aao,
           xend = BOX_LEFT, yend = y_aao,
           arrow = arrow(length = unit(0.16, "cm"), type = "closed"),
           linewidth = LRY_LW)

# Exclusion box branches RIGHT from midpoint of arrow between y1 and y3
p <- create_side_box(p,
  items = c(
    sprintf("Excluded — ENThealth-only (not AAO-HNS member): n = %s",
            format(n_enth_only, big.mark = ",")),
    sprintf("AAO-HNS-only (name mismatch; not excluded): n = %s",
            format(n_aao_only, big.mark = ",")),
    "Name matching: humaniformat::parse_names() + state"
  ),
  y_center   = y_ex,
  x_from     = BOX_RIGHT,
  x_box_left = X_BOX_L,
  box_width  = BOX_W,
  fill       = LRY_EXCL
)

# ── Level 2: Inner join result ────────────────────────────────────────────────
p <- p + create_box(
  sprintf("Confirmed Active Otolaryngologists\n(N = %s)",
          format(n_inner_join, big.mark = ",")),
  X_CTR, y3, fill = LRY_MID
)

# Vertical arrow from inner join to eligibility box
p <- p + create_arrow(X_CTR, y3 - BOX_H/2, X_CTR, y_elg + BOX_H/2)

# Exclusion side box — phone + Puerto Rico (branches from midpoint of arrow)
p <- create_side_box(p,
  items = c(
    sprintf("Excluded — no phone number on ENThealth.org: n = %s",
            format(n_no_phone, big.mark = ",")),
    sprintf("Excluded — Puerto Rico address (no English-speaking callers): n = %s",
            format(n_puerto_rico, big.mark = ",")),
    sprintf("Total excluded: n = %s", format(n_excl_elig, big.mark = ","))
  ),
  y_center   = y_excl2,
  x_from     = BOX_RIGHT,
  x_box_left = X_BOX_L,
  box_width  = BOX_W,
  fill       = LRY_EXCL
)

# ── Level 3: Eligible physicians (after study exclusions) ─────────────────────
p <- p + create_box(
  sprintf("Eligible for Mystery-Caller Study\n(N = %s)",
          format(n_after_elig, big.mark = ",")),
  X_CTR, y_elg, fill = LRY_MID
)

# Vertical arrow to NPI box
p <- p + create_arrow(X_CTR, y_elg - BOX_H/2, X_CTR, y4 + BOX_H/2)

# ── Level 4: NPI identifier search ───────────────────────────────────────────
p <- p + create_box(
  sprintf("NPI Identifier Search\n(NPPES Feb 2026;  N = %s)",
          format(n_after_elig, big.mark = ",")),
  X_CTR, y4, fill = LRY_MID
)

npi_p1 <- if ("1" %in% names(pass_counts)) as.integer(pass_counts["1"]) else NA_integer_
npi_p2 <- if ("2" %in% names(pass_counts)) as.integer(pass_counts["2"]) else NA_integer_
npi_p3 <- if ("3" %in% names(pass_counts)) as.integer(pass_counts["3"]) else NA_integer_

# Round-36 audit fix (2026-05-21): the prior code used max(n, 1) to guard
# against divide-by-zero, but max(NA, 1) is NA — and sprintf("%.1f%%", NA)
# renders the literal text "NA%" in the published figure. .safe_pct()
# returns 0 for any non-finite or NA numerator/denominator so callers
# always get a renderable number; the underlying NA is upstream's bug to
# surface, not ours to publish.
.safe_pct <- function(numerator, denominator) {
  if (length(numerator) == 0L || length(denominator) == 0L) return(0)
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) return(0)
  100 * numerator / denominator
}
npi_side <- c(
  sprintf("NPI identified: n = %s (%.1f%%)",
          format(n_npi_matched_inner, big.mark = ","),
          .safe_pct(n_npi_matched_inner, n_after_elig))
)
if (!is.na(npi_p1)) npi_side <- c(npi_side,
  sprintf("  Pass 1 — exact (last + state + initial): n = %s", format(npi_p1, big.mark = ",")))
if (!is.na(npi_p2)) npi_side <- c(npi_side,
  sprintf("  Pass 2 — relax first initial: n = %s",            format(npi_p2, big.mark = ",")))
if (!is.na(npi_p3)) npi_side <- c(npi_side,
  sprintf("  Pass 3 — fuzzy / special-char strip: n = %s",     format(npi_p3, big.mark = ",")))
npi_side <- c(npi_side,
  sprintf("No NPI found — retained: n = %s (%.1f%%; not excluded)",
          format(n_no_npi_total, big.mark = ","),
          .safe_pct(n_no_npi_total, n_after_elig))
)

p <- create_side_box(p,
  items      = npi_side,
  y_center   = y4,
  x_from     = BOX_RIGHT,
  x_box_left = X_BOX_L,
  box_width  = BOX_W,
  fill       = LRY_SIDE
)

# Vertical arrow to final cohort
p <- p + create_arrow(X_CTR, y4 - BOX_H/2, X_CTR, y5 + BOX_H/2)

# ── Level 5: Final analytic cohort ────────────────────────────────────────────
n_geo <- ct$n_geo_final

p <- p + create_box(
  sprintf("Final Analytic Cohort\n(N = %s)", format(n_final, big.mark = ",")),
  X_CTR, y5, fill = LRY_FINAL, fontface = "bold", size = FS_BOX + 0.2
)

p <- create_side_box(p,
  items = c(
    "US attending otolaryngologists",
    "Confirmed active AAO-HNS members (March 2026)",
    sprintf("NPPES NPI available: n = %s (%.0f%%)",
            format(n_npi_matched_inner, big.mark = ","),
            .safe_pct(n_npi_matched_inner, n_final)),
    sprintf("Geocoded coordinates: n = %s of %s",
            format(n_geo, big.mark = ","), format(n_final, big.mark = ","))
  ),
  y_center   = y5,
  x_from     = BOX_RIGHT,
  x_box_left = X_BOX_L,
  box_width  = BOX_W,
  fill       = LRY_SIDE
)

# ── Title & caption ───────────────────────────────────────────────────────────
p <- p +
  labs(
    title   = "Figure 1. Cohort Assembly Flow Diagram",
    subtitle = paste0(
      "ENThealth.org directory scrape → AAO-HNS member confirmation → ",
      "NPPES NPI enrichment"
    ),
    caption = paste0(
      "ENThealth.org: American Academy of Otolaryngology–Head and Neck Surgery ",
      "public physician directory (February 2026). ",
      "AAO-HNS member list provided by Tina Maggio, Chief Communications Officer, ",
      "AAO-HNS (received March 5, 2026); includes U.S. actively practicing ",
      "otolaryngologists only (residents and retired members excluded). ",
      "NPPES: National Provider Identifier Registry, February 2026 (Centers for ",
      "Medicare & Medicaid Services). Name standardization: humaniformat v",
      packageVersion("humaniformat"), " (R Project for Statistical Computing)."
    )
  ) +
  theme(
    text          = element_text(family = LRY_FONT),
    plot.title    = element_text(family = LRY_FONT, hjust = 0.5, size = FS_TITLE,
                                 face = "bold",  margin = ggplot2::margin(b = 3)),
    plot.subtitle = element_text(family = LRY_FONT, hjust = 0.5, size = FS_SUB,
                                 face = "italic", margin = ggplot2::margin(b = 6)),
    plot.caption  = element_text(family = LRY_FONT, hjust = 0,   size = FS_CAP,
                                 face = "plain", margin = ggplot2::margin(t = 9)),
    plot.margin   = ggplot2::margin(12, 8, 14, 8)
  )

# =============================================================================
# SECTION 5 — SAVE IMAGES
# Laryngoscope spec: 600 DPI TIFF (line art), 7 in wide (178 mm)
# =============================================================================

.save_image <- function(path, plot_obj, fmt, width = 7, height = 10, dpi = 600) {
  plot_tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(plot_tmp), add = TRUE)
  saveRDS(plot_obj, plot_tmp)

  tmp_path <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp_path)) unlink(tmp_path), add = TRUE)
  tryCatch({
    callr::r(
      func = function(pf, op, fmt, w, h, d) {
        p <- readRDS(pf)
        if (fmt == "png" && requireNamespace("ragg", quietly = TRUE)) {
          ragg::agg_png(op, width = w, height = h, units = "in", res = d)
          on.exit(try(dev.off(), silent = TRUE), add = TRUE)
          print(p)
        } else if (fmt == "tiff" && requireNamespace("ragg", quietly = TRUE)) {
          ragg::agg_tiff(op, width = w, height = h, units = "in", res = d,
                         compression = "lzw")
          on.exit(try(dev.off(), silent = TRUE), add = TRUE)
          print(p)
        } else if (fmt == "tiff") {
          ggplot2::ggsave(op, p, width = w, height = h, dpi = d,
                          device = function(...) grDevices::tiff(..., compression = "lzw"))
        } else {
          ggplot2::ggsave(op, p, width = w, height = h, dpi = d)
        }
        TRUE
      },
      args    = list(pf = plot_tmp, op = tmp_path, fmt = fmt,
                     w = width, h = height, d = dpi),
      timeout = 120
    )
    ok <- file.exists(tmp_path) && file.size(tmp_path) > 1024
    if (ok) {
      file.rename(tmp_path, path)
      cat(sprintf("  Saved %s: %s (%.0f KB)\n",
                  toupper(fmt), basename(path), file.size(path) / 1024))
    } else {
      unlink(tmp_path)
      warning(sprintf("  %s produced empty file — removed", toupper(fmt)), call. = FALSE)
    }
    ok
  }, error = function(e) {
    if (file.exists(tmp_path)) unlink(tmp_path)
    warning(sprintf("  %s save failed: %s", toupper(fmt), e$message), call. = FALSE)
    FALSE
  })
}

# 300 DPI PNG for screen preview; 600 DPI TIFF for journal submission
png_ok  <- .save_image(OUT_PNG,  p, "png",  width = 7, height = 10, dpi = 300)
tiff_ok <- .save_image(OUT_TIFF, p, "tiff", width = 7, height = 10, dpi = 600)

# =============================================================================
# SECTION 6 — SUMMARY
# =============================================================================

cat("\n=======================================================================\n")
cat("  STROBE DIAGRAM (LARYNGOSCOPE STYLE) — COMPLETE\n")
cat(sprintf("  ENThealth scraped (total)            : %s\n", format(n_enthealth_raw,    big.mark = ",")))
cat(sprintf("    Not geocoded (retained)            : %s\n", format(n_not_geocoded,      big.mark = ",")))
cat(sprintf("  AAO-HNS attendings                   : %s\n", format(n_aao_total,         big.mark = ",")))
cat(sprintf("  Inner join (both sources)            : %s\n", format(n_inner_join,        big.mark = ",")))
cat(sprintf("    ENThealth-only (excluded)          : %s\n", format(n_enth_only,         big.mark = ",")))
cat(sprintf("    AAO-HNS-only (name mismatch)       : %s\n", format(n_aao_only,          big.mark = ",")))
cat(sprintf("  Study eligibility exclusions         : %s\n", format(n_excl_elig,         big.mark = ",")))
cat(sprintf("    No phone on ENThealth.org          : %s\n", format(n_no_phone,          big.mark = ",")))
cat(sprintf("    Puerto Rico address                : %s\n", format(n_puerto_rico,       big.mark = ",")))
cat(sprintf("  Eligible for mystery-caller study    : %s\n", format(n_after_elig,        big.mark = ",")))
cat(sprintf("  NPI found                            : %s (%.1f%%)\n",
            format(n_npi_matched_inner, big.mark = ","),
            100 * n_npi_matched_inner / max(n_after_elig, 1)))
cat(sprintf("  No NPI (retained)                    : %s (%.1f%%)\n",
            format(n_no_npi_total, big.mark = ","),
            100 * n_no_npi_total / max(n_after_elig, 1)))
cat(sprintf("  FINAL ANALYTIC COHORT                : %s\n", format(n_final, big.mark = ",")))
if (png_ok)  cat(sprintf("  PNG  (300 DPI, preview) : %s\n", OUT_PNG))
if (tiff_ok) cat(sprintf("  TIFF (600 DPI, journal) : %s\n", OUT_TIFF))
cat(sprintf("  CSV                     : %s\n", OUT_CSV))
cat("=======================================================================\n\n")
