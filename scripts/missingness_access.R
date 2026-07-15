#!/usr/bin/env Rscript
# =============================================================================
# Missing-data & access analysis for the ENT mystery-caller study.
#
# Three nested selection stages, each a potential source of bias:
#   960 calls -> 749 complete (call-completion / data-collection missingness)
#            -> 433 offered an appointment (ACCESS)
#            -> 430 with a business-day wait (TIMELINESS; MNAR by design)
#
# We (1) quantify the attrition, (2) test whether call INCOMPLETENESS is related
# to observed covariates (a MAR/MCAR-style check on the data-collection stage),
# and (3) characterize offered-vs-not among complete calls (the access stage).
#
# Output: model_output/missingness_access_summary.txt
# =============================================================================

suppressPackageStartupMessages({library(dplyr)})
options(stringsAsFactors = FALSE, width = 105)
outdir <- "model_output"; dir.create(outdir, showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))
d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")

d$complete_f <- d$complete == "Complete"
d$offered    <- ifelse(d$appointment_offered == "TRUE", 1L,
                ifelse(d$appointment_offered == "FALSE", 0L, NA_integer_))
d$wait_obs   <- !is.na(num(d$wait_days_business))

sink(file.path(outdir, "missingness_access_summary.txt"))

cat("################ 1. ATTRITION WATERFALL ################\n\n")
comp <- d[d$complete_f & d$ent_type != "", ]
cat(sprintf("  Total calls placed .................... %4d\n", nrow(d)))
cat(sprintf("  Complete (data collection succeeded).. %4d (%.1f%%)\n",
            sum(d$complete_f), 100*mean(d$complete_f)))
cat(sprintf("  Complete + subspecialty known ........ %4d\n", nrow(comp)))
cat(sprintf("  Offered an appointment ............... %4d (%.1f%% of analytic)\n",
            sum(comp$offered==1), 100*mean(comp$offered==1)))
cat(sprintf("  Wait time recorded ................... %4d\n", sum(comp$wait_obs)))
cat("\n  Incomplete-call reasons (taking_new_patients among incomplete):\n")
print(sort(table(d$taking_new_patients[!d$complete_f]), decreasing = TRUE))

cat("\n\n################ 2. IS CALL COMPLETENESS RELATED TO COVARIATES? ################\n")
cat("(If completeness depends on geography/subspecialty, the data-collection\n")
cat(" missingness is MAR, not MCAR, and could bias naive estimates.)\n\n")
tests <- list(
  rural            = d$ruca_category,
  aao_hns_region   = d$aao_hns_region,
  ent_type         = d$ent_type)
for (nm in names(tests)) {
  tb <- table(tests[[nm]], d$complete_f)
  p  <- tryCatch(suppressWarnings(chisq.test(tb)$p.value), error = function(e) NA)
  cat(sprintf("  completeness vs %-16s chisq p = %.3f\n", nm, p))
}
# continuous covariates: complete vs incomplete
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct")) {
  x <- num(d[[v]])
  p <- tryCatch(wilcox.test(x ~ d$complete_f)$p.value, error = function(e) NA)
  cat(sprintf("  completeness vs %-16s wilcox p = %.3f  (median cplt=%.3f, incplt=%.3f)\n",
      v, p, median(x[d$complete_f], na.rm=TRUE), median(x[!d$complete_f], na.rm=TRUE)))
}

cat("\n\n################ 3. OFFERED vs NOT-OFFERED (access stage) ################\n")
cat("Among complete calls with known subspecialty (n=", nrow(comp), ").\n\n", sep="")
cat("  Offer rate by subspecialty:\n")
print(round(sort(tapply(comp$offered, comp$ent_type, mean)), 3))
cat("\n  Offer rate by rurality:\n")
print(round(tapply(comp$offered, comp$ruca_category, mean), 3))
cat("\n  Offer rate by AAO-HNS region:\n")
print(round(sort(tapply(comp$offered, comp$aao_hns_region, mean)), 3))
# covariate medians offered vs not
cat("\n  Covariate medians, offered vs not-offered:\n")
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct")) {
  x <- num(comp[[v]]); g <- comp$offered==1
  p <- tryCatch(wilcox.test(x ~ g)$p.value, error = function(e) NA)
  cat(sprintf("    %-18s offered=%.3f  not=%.3f  wilcox p=%.3f\n",
      v, median(x[g],na.rm=TRUE), median(x[!g],na.rm=TRUE), p))
}

cat("\n\n################ 4. TIMELINESS MISSINGNESS ################\n")
cat("  wait_days is observed for", sum(comp$wait_obs), "of", nrow(comp),
    "complete calls -- structurally MNAR: it exists only when an appointment\n")
cat("  was offered (offered==1 for", sum(comp$offered==1),
    "; wait recorded for", sum(comp$wait_obs), ").\n")
cat("  The two-part hurdle model (scripts/model_wait_days.R) handles this by\n")
cat("  modeling the offer stage separately, so wait-time estimates are NOT\n")
cat("  extrapolated to offices that never offered an appointment.\n")
sink()

cat(readLines(file.path(outdir, "missingness_access_summary.txt")), sep = "\n")
cat("\n\nWrote", file.path(outdir, "missingness_access_summary.txt"), "\n")
