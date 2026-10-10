#!/usr/bin/env Rscript
# =============================================================================
# Subspecialty triangulation — alluvial / Sankey-style flow diagram
# =============================================================================
# Complement to the heatmap figure (improve_ent_triangulation_figure.R).
# Shows physicians "flowing" from ABOto subspecialty (left) -> NPPES
# (middle) -> ENThealth (right), with band widths proportional to counts.
#
# Two facets:
#   Top:    All 11,350 active board-cert physicians (with "(missing)"
#           for those without an NPPES taxonomy match or no ENThealth row)
#   Bottom: Same view restricted to the Triple Match subset (n=4,578) —
#           every physician has all three signals, so no missing strata
#
# Output: publication_materials/figures/ent_data_integrity/
#         ent_subspecialty_triangulation_alluvial_v1.png
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(ggalluvial); library(dplyr); library(tidyr);
  library(here); library(patchwork)
})

source(here::here("R", "ent_registry_counts.R"))
ct  <- compute_ent_registry_counts()
tri <- ct$qa$subspecialty_triangulation

# Palette
text_dark   <- "#1a2b3c"
text_dim    <- "#4a6880"
strat_fill  <- c(
  "Pediatric"  = "#e76f51",   # warm orange
  "Neuro"      = "#2a9d8f",   # teal
  "Sleep"      = "#264653",   # navy
  "General"    = "#a8dadc",   # light teal
  "(missing)"  = "#cccccc"    # grey
)
strat_levels <- names(strat_fill)

# Short labels
short_label <- function(x) dplyr::recode(x,
  "General Otolaryngology"   = "General",
  "Pediatric Otolaryngology" = "Pediatric",
  "Neurotology"              = "Neuro",
  "Sleep Medicine"           = "Sleep",
  .missing = "(missing)"
)

# ── Build long-format alluvium for both facets ──────────────────────────────
make_alluvium <- function(df, label) {
  df %>%
    transmute(
      ABOto     = factor(short_label(aboto_subspecialty),    levels = strat_levels),
      NPPES     = factor(short_label(nppes_subspec_aboto),   levels = strat_levels),
      ENThealth = factor(short_label(ent_subspec_aboto),     levels = strat_levels)
    ) %>%
    count(ABOto, NPPES, ENThealth, name = "Freq") %>%
    mutate(facet = label)
}

allu_full <- make_alluvium(tri, sprintf("All active Board cohort (n = %s)",
                                         format(nrow(tri), big.mark = ",")))
tri_triple <- tri %>% filter(!is.na(nppes_subspec_aboto), !is.na(ent_subspec_aboto))
allu_triple <- make_alluvium(tri_triple, sprintf(
  "Triple Match subset (n = %s — every physician has all three signals)",
  format(nrow(tri_triple), big.mark = ",")))

# ── Single-panel renderer ───────────────────────────────────────────────────
make_panel <- function(allu_df, panel_title) {
  ggplot(allu_df,
         aes(axis1 = ABOto, axis2 = NPPES, axis3 = ENThealth, y = Freq)) +
    geom_alluvium(aes(fill = ABOto), alpha = 0.65, width = 0.18,
                  knot.pos = 0.4, decreasing = FALSE) +
    geom_stratum(aes(fill = stat(stratum)), width = 0.18,
                 color = "white", linewidth = 0.6) +
    geom_text(stat = "stratum",
              aes(label = ifelse(after_stat(count) >= 50,
                                 sprintf("%s\n(%s)",
                                         after_stat(stratum),
                                         format(after_stat(count), big.mark = ",")),
                                 "")),
              size = 3.0, color = text_dark, lineheight = 1.0) +
    scale_x_discrete(limits = c("ABOto", "NPPES", "ENThealth"),
                     position = "top", expand = expansion(mult = 0.05)) +
    scale_fill_manual(values = strat_fill, na.value = "#cccccc",
                      breaks = strat_levels[1:4], guide = "none") +
    labs(title = panel_title, x = NULL, y = "physicians") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, color = text_dark,
                                   margin = margin(b = 6)),
      axis.text.x   = element_text(face = "bold", size = 12, color = text_dark),
      axis.text.y   = element_text(color = text_dim),
      axis.title.y  = element_text(color = text_dim),
      panel.grid    = element_blank(),
      plot.margin   = margin(8, 8, 8, 8)
    )
}

panel_full   <- make_panel(allu_full,   sprintf("A. %s", unique(allu_full$facet)))
panel_triple <- make_panel(allu_triple, sprintf("B. %s", unique(allu_triple$facet)))

final <- panel_full / panel_triple +
  patchwork::plot_layout(heights = c(1.0, 1.0)) +
  patchwork::plot_annotation(
    title = "ENT subspecialty triangulation flow: ABOto -> NPPES -> ENThealth",
    subtitle = paste0(
      "Each band represents physicians whose subspecialty matches that ",
      "left-to-right path.  Bands that flow STRAIGHT across (e.g. ",
      "Pediatric -> Pediatric -> Pediatric) are agreement; bands that ",
      "CROSS to a different stratum are disagreement.\n",
      "Top: full active board cohort with '(missing)' strata for ",
      "physicians lacking a NPPES taxonomy or ENThealth listing.  ",
      "Bottom: subset where every physician has all three signals."
    ),
    caption = paste0(
      "Subspecialty mapping in R/ent_registry_counts.R: ",
      "NPPES 207Y* family -> ABOto 4 categories; ",
      "ENThealth specialty_primary -> ABOto 4 categories.  ",
      "Band colour = source ABOto category."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 17, color = text_dark,
                                   hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10.5, color = text_dim,
                                   hjust = 0, margin = margin(b = 14),
                                   lineheight = 1.3),
      plot.caption  = element_text(size = 8.5, color = text_dim, hjust = 0,
                                   margin = margin(t = 12), lineheight = 1.4)
    )
  )

out_dir  <- here::here("publication_materials", "figures", "ent_data_integrity")
out_path <- file.path(out_dir, "ent_subspecialty_triangulation_alluvial_v1.png")
ggsave(out_path, final, width = 14, height = 12, dpi = 300, bg = "white")
cat(sprintf("Saved: %s\n", out_path))

# Reproducibility receipt
source(here::here("R", "figure_receipt.R"))
write_figure_receipt(
  figure_path = out_path,
  input_paths = ct$input_paths,
  live_counts = list(
    n_active_board_full       = nrow(tri),
    n_triple_with_all_signals = nrow(tri_triple),
    n_alluvium_strata_full    = nrow(allu_full),
    n_alluvium_strata_triple  = nrow(allu_triple)
  ),
  script_path = "scripts/improve_ent_triangulation_alluvial.R"
)
