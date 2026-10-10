#!/usr/bin/env Rscript
# =============================================================================
# Figure: ENT specialty-identification accuracy of enthealth.org and NPPES
#         vs ABOHNS as the gold standard
# =============================================================================
# Two-panel publication figure built from the validation appendix numbers
# (docs/appendix_enthealth_nppes_validation.{Rmd,html}).  All inputs are
# hardcoded from the May 2026 hand-search so this script can be re-run
# without the external drive mounted.  When the cohort is refreshed, update
# the constants in the DATA section below.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(scales)
  library(patchwork)
  library(here)
})

# -----------------------------------------------------------------------------
# DATA — hardcoded from appendix_enthealth_nppes_validation.html (2026-05-05)
# -----------------------------------------------------------------------------

# Sample used for the agreement comparison: physicians appearing in
# enthealth + NPPES + ABOHNS board verification simultaneously.
N_THREE_WAY <- 1091L

# Panel A — exact subspecialty match against ABOHNS board truth
panel_a <- tibble::tribble(
  ~source,       ~matches, ~total,
  "enthealth",        961,  N_THREE_WAY,
  "NPPES",            331,  N_THREE_WAY
) |>
  mutate(
    pct        = matches / total,
    label      = sprintf("%s\n(%s / %s)",
                         scales::percent(pct, accuracy = 0.1),
                         format(matches, big.mark = ","),
                         format(total,   big.mark = ",")),
    source     = factor(source, levels = c("enthealth", "NPPES"))
  )

# Panel B — NPPES failure modes among the 2,766 enthealth subspecialists
# with an NPPES match (1,773 + 118 + 101 known + 774 residual disagreements).
# Residual = sum of all other discrepancy buckets in the appendix table.
panel_b <- tibble::tribble(
  ~failure_mode,                         ~n,    ~example_note,
  "Generic \"General Otolaryngology\"",  1773L, "NPPES dropped subspec code; assigned 207Y00000X",
  "Other label disagreements",            774L, "Mostly Otolaryngology vs sub-cert in slot 1",
  "Facial Plastic → Sleep Medicine",      118L, "207YX0602X vs 207YS0123X (NUCC dropdown error)",
  "Neurotology → Otolaryngic Allergy",    101L, "207YX0905X vs 207YX0901X (one-digit-off)"
) |>
  mutate(
    failure_mode = factor(failure_mode, levels = rev(failure_mode)),
    pct_of_total = n / sum(n),
    label        = sprintf("%s  (%s)",
                           format(n, big.mark = ","),
                           scales::percent(pct_of_total, accuracy = 0.1))
  )

# Panel C — Sensitivity of the match rule.  The strict-match number in
# Panel A counts only NPPES taxonomy slot 1.  NPPES allows up to FIVE
# taxonomy codes per provider; a physician may carry the right subspecialty
# code in slot 2-5 even when slot 1 is generic "Otolaryngology".
# Loosening the rule to "any slot" therefore moves rows from "Disagree" to
# "Match".  The size of that move isn't computed against ABOHNS in the
# appendix, but it IS computed for the analogous enthealth ↔ NPPES
# three-way classification (n = 5,987 physicians with specialty in both):
#
#   Exact match (slot 1)            3,359   56.1%
#   Secondary taxonomy match (2-5)    282    4.7%
#   Disagree                         2,346   39.2%
#
# Reading: the slot 1-only rule excludes ~4.7 pp of physicians who
# actually carry the right code in NPPES — just not in the headline slot.
# Treat the same ~4-5 pp as an ORDER-OF-MAGNITUDE estimate of how much
# Panel A's NPPES bar would shift if we credited slot 2-5 matches.  An
# exact NPPES-vs-ABOHNS loose-match number requires re-running the
# appendix Rmd; the bound shown here is the analogue, not the target.
panel_c <- tibble::tribble(
  ~bucket,                       ~n,
  "Exact match (slot 1)",         3359L,
  "Secondary taxonomy match",      282L,
  "Disagree",                     2346L
) |>
  mutate(
    # Reverse the factor order so position_stack draws left-to-right in the
    # listed order (ggplot stacks "first level = bottom/right").
    bucket = factor(bucket, levels = rev(c(
      "Exact match (slot 1)",
      "Secondary taxonomy match",
      "Disagree"
    ))),
    row    = "All physicians",
    pct    = n / sum(n),
    label  = sprintf("%s\n(%s)",
                     format(n, big.mark = ","),
                     scales::percent(pct, accuracy = 0.1))
  )

# -----------------------------------------------------------------------------
# THEME — publication-clean
# -----------------------------------------------------------------------------
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title         = element_text(face = "bold", size = base_size + 2,
                                        margin = margin(b = 6)),
      plot.subtitle      = element_text(color = "grey30", size = base_size - 1,
                                        margin = margin(b = 10)),
      plot.caption       = element_text(color = "grey40", size = base_size - 2,
                                        hjust = 0, margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(color = "black", size = base_size),
      axis.text.x        = element_text(color = "grey30", size = base_size - 1),
      legend.position    = "none",
      plot.margin        = margin(10, 14, 10, 10)
    )
}

# -----------------------------------------------------------------------------
# PANEL A — % subspecialty agreement with ABOHNS
# -----------------------------------------------------------------------------
p_a <- ggplot(panel_a, aes(x = pct, y = source, fill = source)) +
  geom_col(width = 0.55, color = NA) +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_text(aes(label = label),
            hjust = -0.05, size = 3.6, lineheight = 0.95, color = "grey15") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.32),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = c("enthealth" = "#1F4E79", "NPPES" = "#C0392B")) +
  labs(
    title    = "A. Subspecialty agreement with ABOHNS board truth",
    subtitle = sprintf(
      "Exact-match rate among %s physicians appearing in all three sources",
      format(N_THREE_WAY, big.mark = ",")
    ),
    x        = "Exact subspecialty match (%)"
  ) +
  theme_pub()

# -----------------------------------------------------------------------------
# PANEL B — NPPES failure modes for known-subspecialist enthealth physicians
# -----------------------------------------------------------------------------
b_total <- sum(panel_b$n)

p_b <- ggplot(panel_b, aes(x = n, y = failure_mode)) +
  geom_col(width = 0.55, fill = "#C0392B", alpha = 0.85, color = NA) +
  geom_text(aes(label = label),
            hjust = -0.05, size = 3.6, color = "grey15") +
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, max(panel_b$n) * 1.35),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "B. How NPPES misses ABOHNS subspecialty",
    subtitle = sprintf(
      "Distribution of the %s disagreements between NPPES and ABOHNS",
      format(b_total, big.mark = ",")
    ),
    x        = "Physicians",
    caption  = paste0(
      "Source: appendix_enthealth_nppes_validation (Rmd: 2026-05-05).\n",
      "Note: rates use exact-label match; loosening to 'any NPPES taxonomy ",
      "slot' would raise NPPES by ~10–15 pp."
    )
  ) +
  theme_pub()

# -----------------------------------------------------------------------------
# PANEL C — match-rule sensitivity (slot 1 only vs any taxonomy slot)
# -----------------------------------------------------------------------------
c_total <- sum(panel_c$n)

p_c <- ggplot(panel_c, aes(x = n, y = row, fill = bucket)) +
  geom_col(width = 0.55, color = "white", linewidth = 0.6) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.4,
            lineheight = 0.95) +
  scale_fill_manual(
    values = c(
      "Exact match (slot 1)"     = "#1F4E79",  # blue: counted by strict rule
      "Secondary taxonomy match" = "#F39C12",  # orange: only counted by loose
      "Disagree"                 = "#C0392B"   # red: never counted
    ),
    breaks = c("Exact match (slot 1)", "Secondary taxonomy match", "Disagree"),
    name   = NULL
  ) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_discrete(expand = expansion(mult = c(0.4, 0.4))) +
  labs(
    title    = "C. Match-rule sensitivity (analogue: enthealth vs NPPES)",
    subtitle = sprintf(paste0(
      "Of %s physicians with a specialty label in both sources, %s carry the ",
      "matching code in a NPPES SECONDARY taxonomy slot — invisible to a ",
      "slot-1-only rule. A similar ~5 pp lift would apply to Panel A's NPPES ",
      "bar under loose matching (analogue, not measured against ABOHNS)."
    ), format(c_total, big.mark = ","),
       scales::percent(282 / c_total, accuracy = 0.1)),
    x        = "Physicians",
    caption  = paste0(
      "Source: appendix_enthealth_nppes_validation (Rmd: 2026-05-05).\n",
      "Loose-match rule credits any of the five NPPES taxonomy slots."
    )
  ) +
  theme_pub() +
  theme(
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(10, "pt")
  )

# -----------------------------------------------------------------------------
# COMBINE — stacked three-panel layout
# -----------------------------------------------------------------------------
fig <- (p_a / p_b / p_c) +
  patchwork::plot_layout(heights = c(1, 1.7, 1.1)) +
  patchwork::plot_annotation(
    title    = "ENT specialty identification: enthealth.org vs NPPES vs ABOHNS",
    subtitle = "ABOHNS board verification as the gold standard",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(color = "grey30", size = 11,
                                   margin = margin(b = 12))
    )
  )

# -----------------------------------------------------------------------------
# SAVE — PNG + PDF
# -----------------------------------------------------------------------------
out_dir <- here::here("manuscript", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

png_path <- file.path(out_dir, "figure_ent_specialty_accuracy_vs_abohns.png")
pdf_path <- file.path(out_dir, "figure_ent_specialty_accuracy_vs_abohns.pdf")

tmp_png <- paste0(png_path, ".tmp.png")
tmp_pdf <- paste0(pdf_path, ".tmp.pdf")
ggsave(tmp_png, fig, width = 9, height = 9.5, dpi = 300, bg = "white")
ggsave(tmp_pdf, fig, width = 9, height = 9.5, device = cairo_pdf, bg = "white")
file.rename(tmp_png, png_path)
file.rename(tmp_pdf, pdf_path)

cat("Wrote:\n  ", png_path, "\n  ", pdf_path, "\n", sep = "")
