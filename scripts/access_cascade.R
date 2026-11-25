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
#         model_output/supp/figS6_access_cascade.png         (access-measures figure)
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))
OUT <- "model_output"; SUPP <- file.path(OUT, "supp")
dir.create(SUPP, showWarnings = FALSE, recursive = TRUE)

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character", check.names = FALSE)
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
N <- nrow(d)
pct <- function(x) sprintf("%.1f", 100 * x)

# ---- access-cascade measures (proportion of the analytic sample) ------------
cascade_group <- "Access cascade (% of analytic sample)"
cascade_result <- mysterycall::mysterycall_access_cascade(
  d,
  stages = list(
    list(label = "Reached a live office (office answered)",
         column = "office_answered", success = "TRUE", group = cascade_group),
    list(label = "Practice accepting new patients",
         column = "new_patient_status", success = "Accepting", group = cascade_group),
    list(label = "Practice sees the presented complaint",
         column = "sees_chief_complaint", success = "Yes", group = cascade_group),
    list(label = "New-patient appointment offered",
         column = "appointment_offered", success = "TRUE", group = cascade_group),
    list(label = "Appointment with the sampled physician",
         column = "appointment_outcome", success = "With sampled physician", group = cascade_group)
  ),
  plot = TRUE,
  title = "New-patient access measures across the call pathway",
  subtitle = sprintf("Share of the %d analytic calls; overlapping measures, not a strictly nested funnel", N)
)

cascade <- data.frame(
  group = cascade_result$table$group,
  measure = cascade_result$table$measure,
  n = cascade_result$table$n,
  denom = cascade_result$table$denominator,
  stringsAsFactors = FALSE)
offered <- cascade$n[cascade$measure == "New-patient appointment offered"]

# ---- reasons a practice was not accepting new patients ----------------------
reasons <- table(factor(d$new_patient_status))
reasons <- reasons[grepl("^Not accepting", names(reasons))]
non_acc <- sum(reasons)
reason_df <- data.frame(
  group = "Reasons not accepting new patients (% of non-accepting)",
  measure = sub("^Not accepting: ", "", names(reasons)),
  n = as.integer(reasons), denom = non_acc, stringsAsFactors = FALSE)

# ---- among appointment offers: whom the appointment was with ----------------
# restrict to the ACTUAL offers (appointment_offered == TRUE) so the categories
# sum to `offered`; the four "different physician mentioned but no appointment
# made" calls are NOT offers and are excluded here (they sit in the no-offer arm).
od <- d[d$appointment_offered == "TRUE", ]
who_n <- c(
  sum(od$appointment_outcome == "With sampled physician"),
  sum(od$appointment_outcome == "With different physician"),
  sum(od$appointment_outcome == "With APP only"),
  sum(!(od$appointment_outcome %in%
        c("With sampled physician", "With different physician", "With APP only"))))
who_df <- data.frame(
  group = "Whom the appointment was with (% of offers)",
  measure = c("Sampled physician", "A different physician",
              "Advanced practice provider only", "Provider type not recorded"),
  n = as.integer(who_n), denom = offered, stringsAsFactors = FALSE)

tab <- rbind(cascade, reason_df, who_df)
tab$pct <- pct(tab$n / tab$denom)
tab <- tab[, c("group", "measure", "n", "denom", "pct")]
names(tab) <- c("Group", "Measure", "n", "Denominator", "%")
write.csv(tab, file.path(SUPP, "S10_access_cascade.csv"), row.names = FALSE)
cat("Wrote S10_access_cascade.csv\n"); print(tab, row.names = FALSE)

# ---- funnel figure ----------------------------------------------------------
ggsave(file.path(SUPP, "figS6_access_cascade.png"), cascade_result$plot,
       width = 8.6, height = 3.9, dpi = 200)
cat("Wrote figS6_access_cascade.png\n")
