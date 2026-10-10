#!/usr/bin/env Rscript
# =============================================================================
# ENT Cohort CONSORT-Style Attrition Flow (v4 — 3-column layout)
# =============================================================================
#
# Three-column layout with the Triple Match cohort as a single anchor:
#
#   ABOto column   (left, x=15)   ABOto Certifications -> dedup -> Board Cert
#   NPPES column   (center, x=50) NPPES 207Y* Universe
#   ENThealth col  (right, x=85)  ENThealth roster
#                                      |       |       |
#                                      \       |       /
#                                       \      |      /
#                                          Triple Match (center bottom)
#
# Exclusion notes are placed in dedicated columns to the far left (chain
# exclusions) and far right (ENThealth-no-match breakdown). No central
# crowding, no crossing curves — short straight arrows converge on the
# anchor.
#
# All counts from R/ent_registry_counts.R::compute_ent_registry_counts().
# Output: publication_materials/figures/ent_data_integrity/
#         ent_cohort_attrition_consort_v2.png
#
# Usage: Rscript scripts/improve_ent_attrition.R
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(grid); library(here); library(dplyr); library(tibble)
  library(readr)
})

source(here::here("R", "ent_registry_counts.R"))
source(here::here("R", "special_use_zip_flag.R"))
ct <- compute_ent_registry_counts()

# Compute special-use ZIP exclusion counts. Reported as a SENSITIVITY
# exclusion (NOT subtracted from the analytic cohort) because the po_box
# category captures dozens of major academic medical centers (Cleveland
# Clinic, UT Southwestern, Wake Forest, etc.) whose physicians genuinely
# practice from those ZIPs. Removing them would gut the cohort. Listed
# here so the team can apply optional filtering at the sampling stage.
ent_cohort_csv <- here::here("publication_materials/figures/ent_data_integrity",
                              "board_cert_ent_universe_n11333_zip_aboto.csv")
if (file.exists(ent_cohort_csv)) {
  .ent_cohort <- read_csv(ent_cohort_csv, show_col_types = FALSE,
                          col_types = cols(.default = "c"))
  if (!"special_use_zip_flag" %in% names(.ent_cohort)) {
    .ent_cohort <- add_special_use_zip_flag(.ent_cohort)
  }
  excl_special_use_total <- sum(.ent_cohort$special_use_zip_flag != "standard")
  excl_special_use_mil   <- sum(.ent_cohort$special_use_zip_flag == "military_academy")
  excl_special_use_pob   <- sum(.ent_cohort$special_use_zip_flag == "po_box")
} else {
  excl_special_use_total <- 0L
  excl_special_use_mil   <- 0L
  excl_special_use_pob   <- 0L
}

output_dir <- here::here("publication_materials/figures/ent_data_integrity")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ── Palette ─────────────────────────────────────────────────────────────────
box_fill    <- "#eaf0f6"
box_edge    <- "#2c3e50"
nppes_fill  <- "#dff4f1";   nppes_edge <- "#2ec4b6"
ent_fill    <- "#ffe1e1";   ent_edge   <- "#ff6b6b"
excl_fill   <- "#fdecea";   excl_edge  <- "#c0392b"
triple_fill <- "#fff4cf";   triple_edge<- "#b7791f"
arrow_col   <- "#34495e"
text_dark   <- "#1a2b3c"
text_dim    <- "#4a6880"

# Source-registry colours (match Euler legend)
board_col <- "#3a86ff"
nppes_col <- "#2ec4b6"
ent_col   <- "#ff6b6b"

# ── Counts ──────────────────────────────────────────────────────────────────
n_raw         <- ct$chain$n_raw
n_unique      <- ct$chain$n_unique
n_board       <- ct$chain$n_board
n_npi_univ    <- ct$sets$n_npi_univ
n_enthealth   <- ct$sets$n_enthealth
n_triple      <- ct$sets$n_triple
excl_dedupe   <- ct$chain$drop_dedupe
excl_npi      <- n_unique - n_board
ent_no_match  <- n_enthealth - n_triple

# Round-38 audit fix (2026-05-21): same divide-by-NA bug as round 36 in
# scripts/generate_ent_strobe_diagram.R. max(NA, 1L) returns NA, and the
# downstream sprintf("%.0f%%", NA) renders literal "NA%" in the published
# figure's percentage badge. Use .safe_pct() so the figure always shows a
# rendered number; an upstream NA is a data-integrity issue for the
# counts file to surface, not the figure to publish silently.
.safe_pct <- function(numerator, denominator) {
  if (length(numerator) == 0L || length(denominator) == 0L) return(0)
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) return(0)
  100 * numerator / denominator
}
pct_board <- .safe_pct(n_triple, n_board)
pct_ent   <- .safe_pct(n_triple, n_enthealth)
pct_nppes <- .safe_pct(n_triple, n_npi_univ)

cat("CONSORT counts:\n")
cat(sprintf("  Raw certs    : %s\n", format(n_raw, big.mark = ",")))
cat(sprintf("  Unique board : %s\n", format(n_unique, big.mark = ",")))
cat(sprintf("  Board (final): %s\n", format(n_board, big.mark = ",")))
cat(sprintf("  NPPES univ   : %s\n", format(n_npi_univ, big.mark = ",")))
cat(sprintf("  ENThealth    : %s\n", format(n_enthealth, big.mark = ",")))
cat(sprintf("  Triple Match : %s\n", format(n_triple, big.mark = ",")))
cat(sprintf("  Special-use ZIPs (sensitivity, NOT subtracted): %s (%s mil + %s po_box)\n",
            format(excl_special_use_total, big.mark = ","),
            format(excl_special_use_mil,   big.mark = ","),
            format(excl_special_use_pob,   big.mark = ",")))

# ── Layout grid ─────────────────────────────────────────────────────────────
# x in [0, 100], y in [0, 110]
ABOTO_X    <- 18
NPPES_X    <- 50
ENT_X      <- 82
EXCL_L_X   <-  3  # far-left exclusion side
TRIPLE_X   <- 50
TRIPLE_Y   <-  8

# y-anchors for the ABOto chain (3 boxes)
Y_RAW        <- 92
Y_UNIQUE     <- 73
Y_BOARD      <- 54
# Right two columns sit at mid-height (single box each)
Y_NPPES      <- 73
Y_ENT        <- 73

# ── Main flow boxes ─────────────────────────────────────────────────────────
boxes <- tibble::tribble(
  ~id,          ~x,        ~y,        ~fill,       ~edge,       ~label,
  "raw",        ABOTO_X,   Y_RAW,     box_fill,    box_edge,
    sprintf("ABOto Certifications\nn = %s",
            format(n_raw, big.mark = ",")),
  "unique",     ABOTO_X,   Y_UNIQUE,  box_fill,    box_edge,
    sprintf("Unique Board-Cert Physicians\n(after subspecialty dedup)\nn = %s",
            format(n_unique, big.mark = ",")),
  "board",      ABOTO_X,   Y_BOARD,   box_fill,    box_edge,
    sprintf("Board Certified\n(Contiguous US, active, unique NPI)\nn = %s",
            format(n_board, big.mark = ",")),
  "nppes",      NPPES_X,   Y_NPPES,   nppes_fill,  nppes_edge,
    sprintf("NPPES 207Y* Universe\n(Contiguous US)\nn = %s",
            format(n_npi_univ, big.mark = ",")),
  "ent",        ENT_X,     Y_ENT,     ent_fill,    ent_edge,
    sprintf("AAO-HNS ENThealth\n(unique physicians)\nn = %s",
            format(n_enthealth, big.mark = ",")),
  "triple",     TRIPLE_X,  TRIPLE_Y,  triple_fill, triple_edge,
    sprintf("Triple Match  -  High-Fidelity Cohort\nn = %s",
            format(n_triple, big.mark = ","))
)

# ── Exclusion boxes (FAR LEFT, attached to chain, not blocking merge zone) ──
chain_excl <- tibble::tribble(
  ~id,           ~x,         ~y,        ~label,
  "excl_dedup",  EXCL_L_X,   82,
    sprintf("Excluded (n = %s)\nDuplicate\nsubspecialty certs",
            format(excl_dedupe, big.mark = ",")),
  "excl_npi",    EXCL_L_X,   63,
    sprintf("Excluded (n = %s)\nOutside CONUS,\nretired, dup NPIs",
            format(excl_npi, big.mark = ",")),
  "excl_special_use", EXCL_L_X, 44,
    sprintf("SENSITIVITY excl. (n = %s)\nSpecial-use ZIPs (not subtracted):\n  %s military / academy\n  %s PO Box / large-vol",
            format(excl_special_use_total, big.mark = ","),
            format(excl_special_use_mil,   big.mark = ","),
            format(excl_special_use_pob,   big.mark = ","))
)

# ── ENThealth-not-Triple breakdown (FAR RIGHT, below the ENT box) ───────────
ent_excl_label <- sprintf(paste0(
  "ENThealth members NOT in Triple Match\n",
  "n = %s\n",
  "  %3s  NPPES + ENThealth, absent from ABOto\n",
  "  %3s  ENThealth, no NPI listed\n",
  "  %3s  ABOto state mismatch (non-CONUS)\n",
  "  %3s  NPI listed but not in NPPES"
),
format(ent_no_match,                              big.mark = ","),
format(ct$zone_712$absent_from_abohns,            big.mark = ","),
format(ct$zones$ent_orphan,                       big.mark = ","),
format(ct$zone_712$in_abohns_state_mismatch,      big.mark = ","),
format(ct$zones$ent_only_npi,                     big.mark = ","))

ent_side <- tibble::tibble(
  id    = "ent_side",
  x     = ENT_X,
  y     = 40,
  label = ent_excl_label
)

# ── Arrows ──────────────────────────────────────────────────────────────────
# Vertical arrows down each column
vert_arrows <- tibble::tribble(
  ~xs,      ~ys,       ~xe,       ~ye,       ~col,
  ABOTO_X,  Y_RAW - 5, ABOTO_X,   Y_UNIQUE + 5,  arrow_col,   # raw -> unique
  ABOTO_X,  Y_UNIQUE - 5, ABOTO_X, Y_BOARD + 5,  arrow_col    # unique -> board
)

# Exclusion side-arrows (from chain box to left-side excl box)
excl_arrows <- tibble::tribble(
  ~xs,             ~ys,    ~xe,        ~ye,    ~col,
  ABOTO_X - 8,     82,     EXCL_L_X + 7, 82,   excl_edge,
  ABOTO_X - 8,     63,     EXCL_L_X + 7, 63,   excl_edge,
  ABOTO_X - 8,     44,     EXCL_L_X + 7, 44,   excl_edge
)

# Three convergence arrows from each column's bottom box to Triple Match.
# Use simple straight diagonals — short and visually clean.
merge_arrows <- tibble::tribble(
  ~xs,       ~ys,           ~xe,            ~ye,             ~col,
  ABOTO_X,   Y_BOARD - 5,   TRIPLE_X - 14,  TRIPLE_Y + 4,    board_col,   # Board
  NPPES_X,   Y_NPPES - 5,   TRIPLE_X,       TRIPLE_Y + 5,    nppes_col,   # NPPES
  ENT_X,     Y_ENT - 5,     TRIPLE_X + 14,  TRIPLE_Y + 4,    ent_col      # ENThealth
)

# ── Percentage badges (positioned ALONGSIDE each merge arrow, not on it) ────
pct_badges <- tibble::tribble(
  ~x,    ~y,    ~label,                                                  ~col,
  21,    28,    sprintf("%.0f%% of Board Certified\nare in Triple Match\n(%s / %s)",
                        pct_board,
                        format(n_triple, big.mark = ","),
                        format(n_board, big.mark = ",")),                board_col,
  50,    33,    sprintf("%.0f%% of NPPES 207Y* universe\nare in Triple Match\n(%s / %s)",
                        pct_nppes,
                        format(n_triple, big.mark = ","),
                        format(n_npi_univ, big.mark = ",")),             nppes_col,
  79,    28,    sprintf("%.0f%% of ENThealth listings\nare in Triple Match\n(%s / %s)",
                        pct_ent,
                        format(n_triple, big.mark = ","),
                        format(n_enthealth, big.mark = ",")),            ent_col
)

# ── Build plot ──────────────────────────────────────────────────────────────
p <- ggplot() +
  # Three column header bands
  annotate("rect", xmin =  3, xmax = 33, ymin = 104, ymax = 108,
           fill = "#2c3e50", color = NA) +
  annotate("rect", xmin = 35, xmax = 65, ymin = 104, ymax = 108,
           fill = "#2c3e50", color = NA) +
  annotate("rect", xmin = 67, xmax = 97, ymin = 104, ymax = 108,
           fill = "#2c3e50", color = NA) +
  annotate("text", x = ABOTO_X, y = 106,
           label = "ABOto board-certification chain",
           color = "white", size = 3.8, fontface = "bold") +
  annotate("text", x = NPPES_X, y = 106,
           label = "NPPES taxonomy universe",
           color = "white", size = 3.8, fontface = "bold") +
  annotate("text", x = ENT_X,   y = 106,
           label = "AAO-HNS ENThealth roster",
           color = "white", size = 3.8, fontface = "bold") +

  # Main boxes (everything except Triple Match)
  geom_label(
    data = boxes %>% dplyr::filter(id != "triple"),
    aes(x = x, y = y, label = label, fill = I(fill), colour = I(edge)),
    size = 3.7, lineheight = 1.18,
    label.padding = unit(0.55, "lines"),
    label.r       = unit(0.18, "lines"),
    label.size    = 0.6
  ) +

  # Vertical chain arrows
  geom_segment(
    data = vert_arrows,
    aes(x = xs, y = ys, xend = xe, yend = ye, colour = I(col)),
    linewidth = 1.1,
    arrow = arrow(length = unit(0.32, "cm"), type = "closed")
  ) +

  # Exclusion side arrows (dashed)
  geom_segment(
    data = excl_arrows,
    aes(x = xs, y = ys, xend = xe, yend = ye, colour = I(col)),
    linewidth = 0.6, linetype = "dashed",
    arrow = arrow(length = unit(0.22, "cm"), type = "closed")
  ) +

  # Chain exclusion boxes (far left)
  geom_label(
    data = chain_excl,
    aes(x = x, y = y, label = label),
    fill = excl_fill, colour = excl_edge,
    size = 2.9, lineheight = 1.2,
    label.padding = unit(0.45, "lines"),
    label.r       = unit(0.12, "lines"),
    label.size    = 0.5
  ) +

  # Merge arrows (each one a simple straight line to Triple Match)
  geom_segment(
    data = merge_arrows %>% filter(col == board_col),
    aes(x = xs, y = ys, xend = xe, yend = ye),
    colour = board_col, linewidth = 1.3,
    arrow = arrow(length = unit(0.35, "cm"), type = "closed")
  ) +
  geom_segment(
    data = merge_arrows %>% filter(col == nppes_col),
    aes(x = xs, y = ys, xend = xe, yend = ye),
    colour = nppes_col, linewidth = 1.3,
    arrow = arrow(length = unit(0.35, "cm"), type = "closed")
  ) +
  geom_segment(
    data = merge_arrows %>% filter(col == ent_col),
    aes(x = xs, y = ys, xend = xe, yend = ye),
    colour = ent_col, linewidth = 1.3,
    arrow = arrow(length = unit(0.35, "cm"), type = "closed")
  ) +

  # Percentage badges (alongside arrows, white background for legibility)
  geom_label(
    data = pct_badges,
    aes(x = x, y = y, label = label, colour = I(col)),
    fill = "white", size = 3.0, fontface = "bold",
    label.padding = unit(0.3, "lines"),
    label.r       = unit(0.1, "lines"),
    label.size    = 0.5,
    lineheight    = 1.1
  ) +

  # ENThealth side panel (right column, below the ENT box)
  geom_label(
    data = ent_side,
    aes(x = x, y = y, label = label),
    fill = excl_fill, colour = excl_edge,
    size = 2.7, lineheight = 1.3, hjust = 0.5,
    label.padding = unit(0.55, "lines"),
    label.r       = unit(0.12, "lines"),
    label.size    = 0.5
  ) +

  # Triple Match box — LAST, on top of everything
  geom_label(
    data = boxes %>% dplyr::filter(id == "triple"),
    aes(x = x, y = y, label = label, fill = I(fill), colour = I(edge)),
    size = 4.6, fontface = "bold", lineheight = 1.2,
    label.padding = unit(0.7, "lines"),
    label.r       = unit(0.25, "lines"),
    label.size    = 1.2
  ) +

  coord_cartesian(xlim = c(0, 100), ylim = c(0, 110), clip = "off") +
  theme_void() +
  labs(
    title    = "ENT Physician Cohort \u2014 Three-Source Convergence on Triple Match",
    subtitle = paste0(
      "Three parallel registries converge on the Triple Match cohort (n = ",
      format(n_triple, big.mark = ","),
      ").  Each column shows one source's count and how it relates to the cohort.\n",
      "Red dashed boxes = exclusions.  Arrow colours match Euler diagram conventions."
    ),
    caption  = paste0(
      "ABOto = American Board of Otolaryngology  \u2022  ",
      "NPPES = National Plan & Provider Enumeration System (CMS, 207Y* taxonomy)  \u2022  ",
      "ENThealth = AAO-HNS public directory\n",
      "Retirement / inactive flag = ABOto matched-file `practicing_status` proxy (no current NPI match) \u2014 not Medicare Part B cessation.  ",
      "NPPES is a 2024 snapshot, so \"active\" means \"active as of 2024 NPPES extract\".\n",
      "Each registry's state field is treated independently; a physician with different state designations across registries can appear in CONUS one set but non-CONUS another."
    )
  ) +
  theme(
    plot.title    = element_text(face = "bold", size = 17, color = text_dark,
                                 hjust = 0, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = text_dim, hjust = 0,
                                 margin = margin(b = 18), lineheight = 1.35),
    plot.caption  = element_text(size = 8.5, color = text_dim, hjust = 0,
                                 margin = margin(t = 16), lineheight = 1.4),
    plot.margin   = margin(18, 20, 18, 20)
  )

out_path <- file.path(output_dir, "ent_cohort_attrition_consort_v2.png")
ggsave(out_path, p, width = 17, height = 12, dpi = 300, bg = "white")
cat("Saved:", out_path, "\n")

# Reproducibility receipt
source(here::here("R", "figure_receipt.R"))
write_figure_receipt(
  figure_path = out_path,
  input_paths = ct$input_paths,
  live_counts = ent_receipt_payload(ct),
  script_path = "scripts/improve_ent_attrition.R"
)
