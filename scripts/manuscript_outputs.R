#!/usr/bin/env Rscript
# =============================================================================
# Manuscript-ready outputs for the ENT mystery-caller access study. Built
# self-contained (ggplot2/base) so they do not depend on package reporting
# functions:
#
#   1. Table 1  -> model_output/table1_characteristics.csv  (sample description)
#   2. STROBE flow -> model_output/strobe_flow.dot           (Graphviz; + text)
#   3. Forest plot -> model_output/forest_access_timeliness.png
#      (access OR + timeliness IRR for subspecialty and geographic covariates)
#
# Reads the fitted-model tables written by scripts/model_wait_days.R.
# Run:  Rscript scripts/manuscript_outputs.R
# =============================================================================

suppressPackageStartupMessages({library(ggplot2)})
options(stringsAsFactors = FALSE)
outdir <- "model_output"; dir.create(outdir, showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
comp <- d[d$complete == "Complete" & d$ent_type != "", ]
comp$offered <- comp$appointment_offered == "TRUE"
comp$w <- num(comp$wait_days_business)

# ---- 1. TABLE 1 -------------------------------------------------------------
cat_row <- function(var, label) {
  tb <- table(comp[[var]]); pct <- round(100*prop.table(tb),1)
  data.frame(Characteristic = paste0(label, ": ", names(tb)),
             Value = sprintf("%d (%.1f%%)", as.integer(tb), pct))
}
num_row <- function(var, label) {
  x <- num(comp[[var]])
  data.frame(Characteristic = paste0(label, " [median (IQR)]"),
             Value = sprintf("%.2f (%.2f-%.2f)", median(x,na.rm=TRUE),
                             quantile(x,.25,na.rm=TRUE), quantile(x,.75,na.rm=TRUE)))
}
t1 <- rbind(
  data.frame(Characteristic = sprintf("N (complete calls, subspecialty known)"),
             Value = as.character(nrow(comp))),
  cat_row("ent_type", "Subspecialty"),
  cat_row("ruca_category", "Rurality"),
  cat_row("aao_hns_region", "AAO-HNS region"),
  num_row("ent_per_100k", "ENT per 100k"),
  num_row("medicaid_fee_index", "Medicaid fee index"),
  num_row("svi_overall", "SVI (overall)"),
  num_row("dual_pct", "Dual-eligible fraction"),
  data.frame(Characteristic = "Appointment offered",
             Value = sprintf("%d (%.1f%%)", sum(comp$offered), 100*mean(comp$offered))),
  data.frame(Characteristic = "Business-day wait, if offered [median (IQR)]",
             Value = sprintf("%.0f (%.0f-%.0f)", median(comp$w[comp$offered],na.rm=TRUE),
                             quantile(comp$w[comp$offered],.25,na.rm=TRUE),
                             quantile(comp$w[comp$offered],.75,na.rm=TRUE))))
write.csv(t1, file.path(outdir, "table1_characteristics.csv"), row.names = FALSE)
cat("Wrote table1_characteristics.csv (", nrow(t1), "rows)\n")

# ---- 2. STROBE FLOW ---------------------------------------------------------
n_total <- nrow(d); n_cplt <- sum(d$complete=="Complete")
n_analytic <- nrow(comp); n_off <- sum(comp$offered); n_wait <- sum(!is.na(comp$w))
dot <- sprintf('digraph strobe {
  rankdir=TB; node [shape=box, style=rounded, fontname="Helvetica"];
  a [label="Calls placed\\nn = %d"];
  b [label="Complete calls\\nn = %d"];
  c [label="Analytic sample\\n(subspecialty known)\\nn = %d"];
  e [label="Appointment offered\\nn = %d (%.1f%%)"];
  f [label="Business-day wait recorded\\nn = %d"];
  xa [shape=box, style=dashed, label="Incomplete data collection\\nn = %d"];
  xb [shape=box, style=dashed, label="Subspecialty undetermined\\nn = %d"];
  xe [shape=box, style=dashed, label="No appointment offered\\nn = %d"];
  a -> b; b -> c; c -> e; e -> f;
  a -> xa [style=dashed]; b -> xb [style=dashed]; c -> xe [style=dashed];
}', n_total, n_cplt, n_analytic, n_off, 100*n_off/n_analytic, n_wait,
    n_total-n_cplt, n_cplt-n_analytic, n_analytic-n_off)
writeLines(dot, file.path(outdir, "strobe_flow.dot"))
cat("Wrote strobe_flow.dot (render: dot -Tpng strobe_flow.dot -o strobe_flow.png)\n")

# ---- 3. FOREST PLOT ---------------------------------------------------------
pretty <- function(t) {
  map <- c(
    "ent_typeFacial Plastics"      = "Facial plastic surgery",
    "ent_typeHead and Neck Cancer" = "Head and neck oncology",
    "ent_typeLaryngology"          = "Laryngology",
    "ent_typeOtology/neurotology"  = "Otology/neurotology",
    "ent_typePediatrics"           = "Pediatric otolaryngology",
    "ent_typeRhinology"            = "Rhinology",
    "ent_typeSleep"                = "Sleep medicine",
    "ent_per_100k_z"               = "Otolaryngologist density (per SD)",
    "medicaid_fee_index_z"         = "Medicaid fee index (per SD)",
    "svi_overall_z"                = "Social Vulnerability Index (per SD)",
    "dual_pct_z"                   = "Dual-eligible share (per SD)",
    "ruralRural"                   = "Rural (vs urban)",
    "ruralSuburban"                = "Suburban (vs urban)")
  out <- unname(map[t]); ifelse(is.na(out), t, out)
}
load_eff <- function(csv, est, panel) {
  x <- read.csv(csv)
  x <- x[!grepl("Intercept|caller_f", x$term), ]
  data.frame(term = pretty(x$term), est = x[[est]], lo = x$ci_lower, hi = x$ci_upper,
             panel = panel, p = num(x$p_value))
}
fa <- load_eff(file.path(outdir,"part1_access_OR.csv"),  "or",  "Access (OR)")
fw <- load_eff(file.path(outdir,"part2_wait_IRR.csv"),   "irr", "Timeliness (IRR)")
fp <- rbind(fa, fw)
fp$term <- factor(fp$term, levels = rev(unique(fp$term)))

# honest subtitle: the joint subspecialty tests are the headline, not the
# individual reference-category contrasts, so avoid colour-coding by nominal
# significance (which would over-emphasise non-robust contrasts).
gt <- tryCatch(read.csv(file.path(outdir, "supp", "S14_global_subspecialty_test.csv"),
                        check.names = FALSE), error = function(e) NULL)
gp_txt <- if (!is.null(gt))
  sprintf("Joint subspecialty test not significant (access p = %.2f, timeliness p = %.2f).",
          num(gt[["p-value"]][grepl("Access", gt[["Model part"]])]),
          num(gt[["p-value"]][grepl("Timeliness", gt[["Model part"]])])) else ""

g <- ggplot(fp, aes(est, term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25, color = "grey45") +
  geom_point(size = 2, color = "#2166ac") +
  facet_wrap(~panel, scales = "free_x") +
  scale_x_log10() +
  labs(x = "Ratio (log scale) — OR for access, IRR for wait days",
       y = NULL,
       title = "Otolaryngology appointment access and timeliness",
       subtitle = paste("Nominal, unadjusted reference-category contrasts (reference: general otolaryngology, urban).",
                        gp_txt)) +
  theme_bw(base_size = 11) +
  theme(plot.subtitle = element_text(size = 8.5, colour = "grey30"))
ggsave(file.path(outdir, "forest_access_timeliness.png"), g, width = 10, height = 6, dpi = 150)
cat("Wrote forest_access_timeliness.png\n")
