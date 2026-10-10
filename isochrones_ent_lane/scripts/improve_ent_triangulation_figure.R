#!/usr/bin/env Rscript
# =============================================================================
# Subspecialty triangulation figure
# =============================================================================
# Three-panel figure visualizing the agreement between ABOto board cert,
# NPPES primary_taxonomy, and ENThealth specialty_primary across the
# 11,350 active board-certified ENT cohort.
#
# Panel A: ABOto x NPPES heatmap (4x4) — diagonal = agreement
# Panel B: ABOto x ENThealth heatmap (4x4) — diagonal = agreement
# Panel C: 9-level agreement-bucket distribution bar chart
#
# Output: publication_materials/figures/ent_data_integrity/
#         ent_subspecialty_triangulation_v1.png
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(patchwork);
  library(here); library(readr); library(grid)
})

source(here::here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()
tri <- ct$qa$subspecialty_triangulation

# Palette (consistent with other ENT figures)
text_dark   <- "#1a2b3c"
text_dim    <- "#4a6880"
diag_color  <- "#2ec4b6"   # teal for agreement
off_color   <- "#ff6b6b"   # coral for disagreement
neutral     <- "#cccccc"   # grey for missing

# Short labels for axis ticks
short_label <- function(x) dplyr::recode(x,
  "General Otolaryngology"  = "General",
  "Pediatric Otolaryngology"= "Pediatric",
  "Neurotology"             = "Neuro",
  "Sleep Medicine"          = "Sleep"
)
abo_levels <- c("Pediatric","Neuro","Sleep","General")  # display order

# ── Panel A: ABOto x NPPES ──────────────────────────────────────────────────
ct_nppes <- tri %>%
  filter(!is.na(nppes_subspec_aboto)) %>%
  count(aboto_subspecialty, nppes_subspec_aboto) %>%
  mutate(aboto = factor(short_label(aboto_subspecialty), levels = abo_levels),
         other = factor(short_label(nppes_subspec_aboto), levels = abo_levels),
         is_diag = as.character(aboto) == as.character(other))

n_nppes_total <- sum(ct_nppes$n)
n_nppes_diag  <- sum(ct_nppes$n[ct_nppes$is_diag])

panel_a <- ggplot(ct_nppes, aes(x = other, y = aboto)) +
  geom_tile(aes(fill = is_diag, alpha = sqrt(n)), color = "white", linewidth = 1) +
  scale_fill_manual(values = c("TRUE" = diag_color, "FALSE" = off_color),
                    guide = "none") +
  scale_alpha_continuous(range = c(0.18, 1.0), guide = "none") +
  geom_text(aes(label = format(n, big.mark = ","),
                color = ifelse(n > 1000, "white", text_dark)),
            size = 4.5, fontface = "bold") +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  labs(
    title = sprintf("A. ABOto x NPPES taxonomy   (%d/%d agree on diagonal, %.0f%%)",
                    n_nppes_diag, n_nppes_total, 100 * n_nppes_diag / n_nppes_total),
    x     = "NPPES primary_taxonomy (mapped to ABOto)",
    y     = "ABOto certification (priority-resolved)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = text_dark),
    axis.title    = element_text(size = 10, color = text_dim),
    axis.text     = element_text(color = text_dark),
    panel.grid    = element_blank(),
    plot.margin   = margin(8, 8, 8, 8)
  )

# ── Panel B: ABOto x ENThealth ──────────────────────────────────────────────
ct_ent <- tri %>%
  filter(!is.na(ent_subspec_aboto)) %>%
  count(aboto_subspecialty, ent_subspec_aboto) %>%
  mutate(aboto = factor(short_label(aboto_subspecialty), levels = abo_levels),
         other = factor(short_label(ent_subspec_aboto), levels = abo_levels),
         is_diag = as.character(aboto) == as.character(other))

n_ent_total <- sum(ct_ent$n)
n_ent_diag  <- sum(ct_ent$n[ct_ent$is_diag])

panel_b <- ggplot(ct_ent, aes(x = other, y = aboto)) +
  geom_tile(aes(fill = is_diag, alpha = sqrt(n)), color = "white", linewidth = 1) +
  scale_fill_manual(values = c("TRUE" = diag_color, "FALSE" = off_color),
                    guide = "none") +
  scale_alpha_continuous(range = c(0.18, 1.0), guide = "none") +
  geom_text(aes(label = format(n, big.mark = ","),
                color = ifelse(n > 1000, "white", text_dark)),
            size = 4.5, fontface = "bold") +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  labs(
    title = sprintf("B. ABOto x ENThealth specialty   (%d/%d agree on diagonal, %.0f%%)",
                    n_ent_diag, n_ent_total, 100 * n_ent_diag / n_ent_total),
    x     = "ENThealth specialty_primary (mapped to ABOto)",
    y     = "ABOto certification (priority-resolved)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, color = text_dark),
    axis.title    = element_text(size = 10, color = text_dim),
    axis.text     = element_text(color = text_dark),
    panel.grid    = element_blank(),
    plot.margin   = margin(8, 8, 8, 8)
  )

# ── Panel C: agreement-bucket bar chart ─────────────────────────────────────
buckets <- tri %>%
  count(subspecialty_agreement) %>%
  mutate(
    pct = 100 * n / sum(n),
    label_short = dplyr::recode(subspecialty_agreement,
      "all_three_agree"                       = "All three agree",
      "aboto_nppes_agree_enthealth_missing"   = "ABOto = NPPES (no ENT signal)",
      "aboto_nppes_disagree_enthealth_missing"= "ABOto != NPPES (no ENT signal)",
      "aboto_enthealth_agree_nppes_disagree"  = "ABOto = ENT (NPPES disagrees)",
      "aboto_enthealth_agree_nppes_missing"   = "ABOto = ENT (NPPES missing)",
      "aboto_enthealth_disagree_nppes_missing"= "ABOto != ENT (NPPES missing)",
      "aboto_nppes_agree_enthealth_disagree"  = "ABOto = NPPES (ENT disagrees)",
      "nppes_enthealth_agree_aboto_disagree"  = "NPPES = ENT (ABOto disagrees)",
      "all_three_disagree"                    = "All three disagree",
      "no_nppes_or_enthealth_signal"          = "No NPPES or ENT signal"
    ),
    is_full_agree = subspecialty_agreement == "all_three_agree",
    is_full_disagree = subspecialty_agreement == "all_three_disagree",
    bar_color = case_when(
      is_full_agree    ~ diag_color,
      is_full_disagree ~ off_color,
      TRUE             ~ "#3a86ff"
    )
  ) %>%
  arrange(desc(n)) %>%
  mutate(label_short = factor(label_short, levels = rev(label_short)))

panel_c <- ggplot(buckets, aes(x = n, y = label_short, fill = I(bar_color))) +
  geom_col() +
  geom_text(aes(label = sprintf("%s  (%.1f%%)",
                                format(n, big.mark = ","), pct)),
            hjust = -0.05, size = 3.4, color = text_dark) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = function(x) format(x, big.mark = ",")) +
  labs(
    title = "C. 9-way agreement bucket distribution",
    subtitle = sprintf("Total active Board cohort: %s",
                        format(nrow(tri), big.mark = ",")),
    x = "Number of physicians",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = text_dark),
    plot.subtitle    = element_text(size = 9, color = text_dim,
                                    margin = margin(b = 6)),
    axis.title.x     = element_text(color = text_dim, margin = margin(t = 4)),
    axis.text.y      = element_text(color = text_dark, size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin      = margin(8, 8, 8, 8)
  )

# ── Combine ─────────────────────────────────────────────────────────────────
final <- (panel_a | panel_b) / panel_c +
  patchwork::plot_layout(heights = c(1.0, 0.85)) +
  patchwork::plot_annotation(
    title = "ENT subspecialty triangulation: ABOto vs NPPES vs ENThealth",
    subtitle = paste0(
      "All three sources mapped to the four ABOto categories ",
      "(Pediatric > Neurotology > Sleep > General).  ",
      "ENThealth recovers ABOto subspecialty far more reliably than NPPES taxonomy."
    ),
    caption = paste0(
      "Diagonal cells (teal) = agreement; off-diagonal cells (coral) = disagreement.  ",
      "Cell shade = sqrt(count) for visibility of small cells.\n",
      "Subspecialty mapping in R/ent_registry_counts.R: NPPES 207Y* family -> ABOto 4 categories; ",
      "ENThealth specialty_primary -> ABOto 4 categories."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 17, color = text_dark,
                                   hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, color = text_dim,
                                   hjust = 0, margin = margin(b = 14),
                                   lineheight = 1.25),
      plot.caption  = element_text(size = 8.5, color = text_dim,
                                   hjust = 0, margin = margin(t = 12),
                                   lineheight = 1.4)
    )
  )

out_dir <- here::here("publication_materials", "figures", "ent_data_integrity")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "ent_subspecialty_triangulation_v1.png")
ggsave(out_path, final, width = 14, height = 10, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_path))

# Reproducibility receipt
source(here::here("R", "figure_receipt.R"))
write_figure_receipt(
  figure_path = out_path,
  input_paths = ct$input_paths,
  live_counts = list(
    n_active_board   = nrow(tri),
    n_aboto_x_nppes_diag = n_nppes_diag,
    n_aboto_x_nppes_total = n_nppes_total,
    pct_aboto_x_nppes_diag = round(100 * n_nppes_diag / n_nppes_total, 1),
    n_aboto_x_ent_diag   = n_ent_diag,
    n_aboto_x_ent_total  = n_ent_total,
    pct_aboto_x_ent_diag = round(100 * n_ent_diag / n_ent_total, 1),
    bucket_counts        = setNames(buckets$n, as.character(buckets$label_short))
  ),
  script_path = "scripts/improve_ent_triangulation_figure.R"
)
