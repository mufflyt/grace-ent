#!/usr/bin/env Rscript
# =============================================================================
# ENT Subspecialty Discoverability — ABOto-Based Bar Chart
# =============================================================================
#
# Horizontal bar chart showing what percentage of board-certified physicians
# in each ABOto subspecialty are findable on ENThealth.
#
# Key finding: the 40% aggregate is driven by General Otolaryngology
# (10,357/11,350 of the cohort at 38%); subspecialty-certified ENTs are
# well-represented: Pediatric 72%, Neurotology 62%, Sleep 60%.
#
# Classification: ABOto certifications ONLY (not NPPES taxonomy).
#   Numerator   = board-cert active CONUS physicians ALSO on ENThealth
#   Denominator = board-cert active CONUS physicians (unique NPI)
#   Stratifier  = ABOto subspecialty cert (Pediatric > Neurotology > Sleep > General)
#
# All counts from R/ent_registry_counts.R::compute_ent_registry_counts()$by_subspecialty.
# Output: publication_materials/figures/ent_data_integrity/
#         ent_subspecialty_findability_v2.png
#
# Usage: Rscript scripts/improve_ent_subspecialty_findability.R
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(here)
})

source(here::here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()

output_dir <- here::here("publication_materials/figures/ent_data_integrity")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Palette
text_dark <- "#1a2b3c"
text_dim  <- "#4a6880"
bar_fill  <- "#3a86ff"
bar_high  <- "#2ec4b6"
bar_low   <- "#c0392b"

# Aggregate stat for subtitle
n_board  <- ct$chain$n_board
n_triple <- ct$sets$n_triple
overall_pct <- 100 * n_triple / n_board

df <- ct$by_subspecialty %>%
  mutate(
    subspecialty = factor(subspecialty, levels = subspecialty[order(pct_findable)]),
    bar_color    = case_when(
      pct_findable >= 50 ~ bar_high,
      pct_findable <= 33 ~ bar_low,
      TRUE               ~ bar_fill
    ),
    label_inside  = sprintf("%.0f%%", pct_findable),
    label_outside = sprintf("%s findable / %s total",
                            format(n_findable, big.mark = ","),
                            format(n_total,    big.mark = ","))
  )

p <- ggplot(df, aes(x = pct_findable, y = subspecialty)) +
  geom_segment(aes(x = 0, xend = pct_findable, yend = subspecialty,
                   colour = I(bar_color)),
               linewidth = 14, lineend = "butt") +
  # Overall reference line
  geom_vline(xintercept = overall_pct, linetype = "dashed",
             colour = text_dim, linewidth = 0.5) +
  annotate("text",
           x = overall_pct + 1.2, y = 0.7,
           label = sprintf("Overall: %.0f%%", overall_pct),
           colour = text_dim, size = 3.2, fontface = "italic", hjust = 0) +
  # Percent label inside the bar
  geom_text(aes(label = label_inside),
            x = 1.5, hjust = 0,
            colour = "white", fontface = "bold", size = 4.0) +
  # n findable / total label outside
  geom_text(aes(label = label_outside, x = pct_findable + 1.5),
            hjust = 0, colour = text_dim, size = 3.4) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0),
                     breaks = seq(0, 100, by = 25),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "ENThealth findability by ENT subspecialty",
    subtitle = sprintf(
      "ABOto-certified subspecialists are 60-72%% findable on ENThealth; general otolaryngologists only 38%%.\nOverall %.0f%% of %s active board-certified ENT physicians are listed in the AAO-HNS public directory (CONUS, unique NPI).",
      overall_pct, format(n_board, big.mark = ",")
    ),
    x        = "% of board-certified physicians listed on ENThealth",
    y        = NULL,
    caption  = paste0(
      "Subspecialty assignment comes from ABOto certifications ONLY (not NPPES taxonomy). ABOto offers four certs: General (Otolaryngology - ",
      "Head & Neck Surgery), Complex Pediatric Otolaryngology, Neurotology, Sleep Medicine.\n",
      "Physicians with a subspecialty cert are classified by that subspecialty (priority: Pediatric > Neurotology > Sleep); ",
      "those with only the General cert are classified as General Otolaryngology.\n",
      "Denominator: ", format(n_board, big.mark = ","), " board-cert active CONUS physicians.  ",
      "Numerator: those also in AAO-HNS ENThealth (by NPI).  ",
      "Counts from R/ent_registry_counts.R."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 18, colour = text_dark,
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 11, colour = text_dim,
                                     margin = margin(b = 14), lineheight = 1.25),
    plot.caption      = element_text(size = 8.5, colour = text_dim, hjust = 0,
                                     margin = margin(t = 12)),
    axis.text.y       = element_text(face = "bold", colour = text_dark, size = 11),
    axis.text.x       = element_text(colour = text_dim),
    axis.title.x      = element_text(colour = text_dim, margin = margin(t = 6)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin       = margin(14, 100, 14, 14)
  )

out_path <- file.path(output_dir, "ent_subspecialty_findability_v2.png")
ggsave(out_path, p, width = 13, height = 7, dpi = 300, bg = "white")

# Reproducibility receipt
source(here::here("R", "figure_receipt.R"))
write_figure_receipt(
  figure_path = out_path,
  input_paths = ct$input_paths,
  live_counts = ent_receipt_payload(ct),
  script_path = "scripts/improve_ent_subspecialty_findability.R"
)

cat("Saved:", out_path, "\n\n")
cat("Subspecialty discoverability (live):\n")
ct$by_subspecialty %>%
  arrange(desc(pct_findable)) %>%
  mutate(pct_findable = sprintf("%.1f%%", pct_findable)) %>%
  print(n = 20)
