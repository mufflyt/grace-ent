#!/usr/bin/env Rscript
# Executive one-look figure: urogynecologist access collapses from city to country.
# Data (committed, real): manuscript/stats/tableS_ruca4_access.csv, URPS column:
#   Metropolitan 67.7, Micropolitan 39.1, Small town 28.5, Rural 24.9 (% within 60-min drive).
suppressWarnings(suppressMessages({ library(ggplot2) }))

RED <- "#C0392B"; INK <- "#161A1D"; SOFT <- "#5B6670"

d <- data.frame(
  setting = factor(c("Big cities", "Small cities", "Small towns", "Rural"),
                   levels = c("Big cities", "Small cities", "Small towns", "Rural")),
  access  = c(67.7, 39.1, 28.5, 24.9),
  fill    = c("#F1B7AD", "#DE8271", "#CD5942", "#C0392B"))

fig <- ggplot(d, aes(setting, access, fill = fill)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", access)), vjust = -0.45,
            size = 17, fontface = "bold", colour = INK, family = "Helvetica") +
  scale_fill_identity() +
  scale_y_continuous(limits = c(0, 82), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "[PROVISIONAL] The farther from a city, the harder it is to reach a urogynecologist",
    subtitle = "Share of U.S. women within a 60-minute drive of a urogynecologist — about 7 in 10 in big cities, just 1 in 4 in rural America.",
    x = NULL, y = NULL,
    caption = "PROVISIONAL — under-counts access (2010-crosswalked tracts) and overstates the shortage; regenerate from the native 2020-tract build before external use. Board-certified urogynecologists (ABOG), 2023, by Rural-Urban Commuting Area class; contiguous U.S.; ACS female population.") +
  theme_minimal(base_size = 15, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 27, colour = INK, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 16, colour = SOFT, margin = margin(b = 20), lineheight = 1.05),
    plot.title.position = "plot",
    axis.text.x = element_text(size = 21, face = "bold", colour = INK, margin = margin(t = 6)),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(size = 11, colour = SOFT, hjust = 0, margin = margin(t = 18)),
    plot.margin = margin(28, 34, 20, 30),
    plot.background = element_rect(fill = "white", colour = NA))

dir.create("artifacts/2sfca/figures", showWarnings = FALSE, recursive = TRUE)
out <- "artifacts/2sfca/figures/urogyn_gradient_exec.png"
ggsave(out, fig, width = 13.5, height = 7.2, dpi = 200, bg = "white")
cat("wrote", out, "\n")
