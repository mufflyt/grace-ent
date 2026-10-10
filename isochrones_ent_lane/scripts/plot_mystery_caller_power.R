#!/usr/bin/env Rscript
# Plot the NB power curves saved by mystery_caller_power_NB.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

csv_path  <- file.path("artifacts", "power_analysis", "nb_power_50_50.csv")
out_dir   <- file.path("artifacts", "power_analysis", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

res <- utils::read.csv(csv_path)

long <- res %>%
  dplyr::select(n_npi, n_calls,
                "Conditional interaction (rural:insurance)" = pow_cond_interact,
                "Marginal Medicaid (unweighted, biased)"    = pow_marg_unwtd,
                "Marginal Medicaid (pop-weighted, 15% rural)" = pow_marg_popwtd) %>%
  tidyr::pivot_longer(cols = -c(n_npi, n_calls),
                      names_to = "estimand", values_to = "power")

p <- ggplot(long, aes(x = n_calls, y = power, color = estimand, group = estimand)) +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0.80, linetype = "dotted", color = "grey70") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 1.02), breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(400, 800, 1600, 3000)) +
  scale_color_manual(values = c(
    "Conditional interaction (rural:insurance)" = "#D55E00",
    "Marginal Medicaid (unweighted, biased)"    = "#F0E442",
    "Marginal Medicaid (pop-weighted, 15% rural)" = "#0072B2"
  )) +
  labs(
    title    = "Mystery caller study: power vs. number of phone calls",
    subtitle = "NB GLMM, 50/50 rural-urban sampling, overdispersion dispersion ratio approx 3.3 at mu=14",
    x        = "Total phone calls (each NPI called twice)",
    y        = "Power",
    color    = NULL,
    caption  = "Dashed line = 90% target. NB2 family via glmmTMB. 30 simulations per point."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title.position = "plot")

png_path <- file.path(out_dir, "mystery_caller_power_curves.png")
ggsave(png_path, p, width = 8, height = 5.5, dpi = 150)
cat("Saved figure:", png_path, "\n")
