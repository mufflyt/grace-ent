#!/usr/bin/env Rscript
# =============================================================================
# Secondary access measures ("access cascade") for the ENT mystery-caller study.
#
# The primary model reports two outcomes (appointment OFFER, business-day WAIT).
# The call log also captured several intermediate access constructs that
# comparator simulated-patient studies treat as distinct dimensions (Campbell
# 2013; Wilkinson 2018; Hodson 2025; Pollack 2016): reachability, new-patient
# acceptance (with itemized refusal reasons), whether the practice sees the
# presented complaint, and -- among offers -- whether the appointment was with
# the sampled physician or a redirect. This script summarizes them.
#
# Output: model_output/supp/S10_access_cascade.csv          (table)
#         model_output/supp/figS7_access_cascade.png         (funnel figure)
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))
OUT <- "model_output"; SUPP <- file.path(OUT, "supp")
dir.create(SUPP, showWarnings = FALSE, recursive = TRUE)

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character", check.names = FALSE)
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
N <- nrow(d)
pct <- function(x) sprintf("%.1f", 100 * x)

# ---- access-cascade measures (proportion of the analytic sample) ------------
reach   <- sum(d$office_answered == "TRUE", na.rm = TRUE)
accept  <- sum(d$new_patient_status == "Accepting", na.rm = TRUE)
sees    <- sum(d$sees_chief_complaint == "Yes", na.rm = TRUE)
offered <- sum(d$appointment_offered == "TRUE", na.rm = TRUE)
w_samp  <- sum(d$appointment_outcome == "With sampled physician", na.rm = TRUE)

cascade <- data.frame(
  group = "Access cascade (% of analytic sample)",
  measure = c("Reached a live office (office answered)",
              "Practice accepting new patients",
              "Practice sees the presented complaint",
              "New-patient appointment offered",
              "Appointment with the sampled physician"),
  n = c(reach, accept, sees, offered, w_samp),
  denom = N, stringsAsFactors = FALSE)

# ---- reasons a practice was not accepting new patients ----------------------
reasons <- table(factor(d$new_patient_status))
reasons <- reasons[grepl("^Not accepting", names(reasons))]
non_acc <- sum(reasons)
reason_df <- data.frame(
  group = "Reasons not accepting new patients (% of non-accepting)",
  measure = sub("^Not accepting: ", "", names(reasons)),
  n = as.integer(reasons), denom = non_acc, stringsAsFactors = FALSE)

# ---- among appointment offers: whom the appointment was with ----------------
who <- table(factor(d$appointment_outcome))
who <- who[c("With sampled physician", "With different physician", "With APP only")]
who[is.na(who)] <- 0
who_df <- data.frame(
  group = "Whom the appointment was with (% of offers)",
  measure = c("Sampled physician", "A different physician", "Advanced practice provider only"),
  n = as.integer(who), denom = offered, stringsAsFactors = FALSE)

tab <- rbind(cascade, reason_df, who_df)
tab$pct <- pct(tab$n / tab$denom)
tab <- tab[, c("group", "measure", "n", "denom", "pct")]
names(tab) <- c("Group", "Measure", "n", "Denominator", "%")
write.csv(tab, file.path(SUPP, "S10_access_cascade.csv"), row.names = FALSE)
cat("Wrote S10_access_cascade.csv\n"); print(tab, row.names = FALSE)

# ---- funnel figure ----------------------------------------------------------
fig <- cascade
fig$measure <- factor(fig$measure, levels = rev(fig$measure))
fig$p <- 100 * fig$n / fig$denom
g <- ggplot(fig, aes(p, measure, fill = p)) +
  # faint full-width track behind each bar for a funnel feel
  geom_col(aes(x = 100), fill = "grey93", width = 0.66) +
  geom_col(width = 0.66) +
  geom_text(aes(label = sprintf("%d  (%.0f%%)", n, p)), hjust = -0.12,
            size = 3.5, fontface = "bold", colour = "grey20") +
  scale_fill_viridis_c(option = "D", direction = -1, begin = 0.15, end = 0.85,
                       guide = "none") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(x = NULL, y = NULL,
       title = "New-patient access cascade",
       subtitle = sprintf("Share of the %d analytic calls reaching each step", N)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92"),
    axis.text.y  = element_text(colour = "grey15"),
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 10,
                                 margin = margin(t = 2, b = 8)),
    plot.margin   = margin(8, 12, 8, 8))
ggsave(file.path(SUPP, "figS7_access_cascade.png"), g, width = 8.6, height = 3.9, dpi = 200)
cat("Wrote figS7_access_cascade.png\n")
