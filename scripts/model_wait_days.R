#!/usr/bin/env Rscript
# =============================================================================
# Two-part (hurdle) model of ENT appointment ACCESS & TIMELINESS, built on the
# study's own package (mysterycall) so it matches the manuscript pipeline.
#
#   Part 1  ACCESS      mysterycall_logistic_model  P(appointment offered)
#   Part 2  TIMELINESS  mysterycall_nb_model        E[business-day wait | offered]
#
# Why two parts: wait_days_business is observed ONLY when an appointment was
# offered (430 of 960 rows). Modeling each stage separately keeps "can I get
# in?" distinct from "how long is the wait?".
#
# Analytic sample: complete calls only (complete == "Complete"), excluding 18
# calls with a blank subspecialty (0/18 offered -> complete separation, and none
# reach Part 2). The near-zero-offer callers (Chin/Chow/na) are INCOMPLETE data
# collection, not a scenario arm, so they drop out with the complete-call filter.
#
# caller = research assistant (operator), adjusted as a fixed nuisance factor
# (levels with <15 complete calls pooled to "Other").
#
# Random intercept: CBSA (metro) with county FIPS fallback for non-metro rows.
#
# Output: model_output/  (mysterycall OR/IRR tables, diagnostics, prose)
# =============================================================================

suppressPackageStartupMessages({library(mysterycall); library(dplyr)})
options(stringsAsFactors = FALSE, width = 105)
outdir <- "model_output"; dir.create(outdir, showWarnings = FALSE)

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
num <- function(x) suppressWarnings(as.numeric(x))

# ---- analytic sample --------------------------------------------------------
d <- d[d$complete == "Complete", ]
n_blank <- sum(d$ent_type == "" | is.na(d$ent_type))
d <- d[d$ent_type != "" & !is.na(d$ent_type), ]
cat(sprintf("Complete calls, subspecialty known: %d (dropped %d blank-subspecialty)\n",
            nrow(d), n_blank))

# ---- outcomes ---------------------------------------------------------------
d$offered <- ifelse(d$appointment_offered == "TRUE", 1L,
             ifelse(d$appointment_offered == "FALSE", 0L, NA_integer_))
d$wait_days <- num(d$wait_days_business)      # observed only where offered == 1

# ---- predictors -------------------------------------------------------------
# continuous covariates -> z-scores (comparable coefficients), median-imputed
zf <- function(x){v<-num(x); v[is.na(v)]<-median(v,na.rm=TRUE); as.numeric(scale(v))}
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
  d[[paste0(v,"_z")]] <- zf(d[[v]])

d$ent_type <- relevel(factor(d$ent_type), ref = "General")   # ref = General ENT
d$rural    <- relevel(factor(d$ruca_category), ref = "Urban")

ct <- table(d$caller); small <- names(ct)[ct < 15]
d$caller_f <- ifelse(d$caller %in% small, "Other", d$caller)
d$caller_f <- relevel(factor(d$caller_f), ref = names(sort(ct, decreasing = TRUE))[1])

clust <- d$cbsa_code
clust[is.na(clust) | clust == ""] <- d$county_fips[is.na(clust) | clust == ""]
clust[is.na(clust) | clust == ""] <- paste0("solo_", which(is.na(clust) | clust == ""))
d$cluster <- clust

predictors <- c("ent_type","ent_per_100k_z","medicaid_fee_index_z",
                "svi_overall_z","dual_pct_z","rural","caller_f")

# =============================================================================
# PART 1 -- ACCESS  (mysterycall_logistic_model)
# =============================================================================
d1 <- d[!is.na(d$offered), ]
cat(sprintf("\nPart 1 ACCESS: n=%d, offered=%d (%.1f%%)\n",
            nrow(d1), sum(d1$offered), 100*mean(d1$offered)))
access <- mysterycall_logistic_model(
  data = d1, outcome = "offered", predictors = predictors,
  random_intercept = "cluster")
cat("Converged:", isTRUE(access$convergence$converged),
    "| AIC:", round(access$aic,1), "\n")
print(access$or_table, n = 40)

# =============================================================================
# PART 2 -- TIMELINESS  (mysterycall_nb_model), offered subset
# =============================================================================
d2 <- d[which(d$offered == 1 & !is.na(d$wait_days)), ]
cat(sprintf("\nPart 2 TIMELINESS: n=%d, median wait=%g, max=%g\n",
            nrow(d2), median(d2$wait_days), max(d2$wait_days)))

# Confirm the package's automatic family selection agrees the count is NB
auto <- tryCatch(mysterycall_auto_model(d2, "wait_days", predictors, "cluster"),
                 error = function(e) NULL)
if (!is.null(auto$model_chosen)) cat("auto_model chose:", auto$model_chosen,
    "-", auto$reason, "\n")

wait <- mysterycall_nb_model(
  data = d2, outcome = "wait_days", predictors = predictors,
  random_intercept = "cluster")
cat("Converged:", isTRUE(wait$convergence$converged),
    "| theta:", round(wait$theta,3),
    "| residual overdispersion:", round(wait$overdispersion,3), "\n")
print(wait$irr_table, n = 40)

# ---- package diagnostics ----------------------------------------------------
od  <- tryCatch(mysterycall_overdispersion_test(wait$model), error=function(e) NULL)
rev <- tryCatch(mysterycall_random_effect_variance(wait$model), error=function(e) NULL)

# ---- IRR -> absolute wait-day differences (subspecialty) --------------------
base_wait <- mean(d2$wait_days[d2$ent_type == "General"], na.rm = TRUE)
days <- tryCatch(mysterycall_irr_to_days(wait, baseline_mean = base_wait,
          exposure_col = "ent_type", ref_group = "General"),
          error = function(e) NULL)

# ---- manuscript prose (returns a single character string) -------------------
prose <- tryCatch(mysterycall_results_paragraph(access, exposure_col = "ent_type",
          ref_group = "General", outcome_label = "an appointment offer"),
          error = function(e) NULL)

# ---- persist ----------------------------------------------------------------
write.csv(access$or_table, file.path(outdir,"part1_access_OR.csv"), row.names=FALSE)
write.csv(wait$irr_table,  file.path(outdir,"part2_wait_IRR.csv"),  row.names=FALSE)
if (!is.null(days$table)) write.csv(days$table, file.path(outdir,"part2_wait_days_by_subspecialty.csv"), row.names=FALSE)
saveRDS(list(access=access, wait=wait), file.path(outdir,"models.rds"))

sink(file.path(outdir,"wait_days_model_summary.txt"))
cat("================ PART 1: ACCESS (odds ratios) ================\n")
cat("mysterycall_logistic_model | OR>1 = higher odds of an appointment offer\n\n")
print(as.data.frame(access$or_table), digits=3)
cat(sprintf("\nAIC=%.1f BIC=%.1f  n=%d clusters=%d\n", access$aic, access$bic,
            access$n, access$n_clusters))
cat("\n\n================ PART 2: TIMELINESS (incidence rate ratios) ================\n")
cat("mysterycall_nb_model | IRR>1 = LONGER wait | theta=", round(wait$theta,3), "\n\n")
print(as.data.frame(wait$irr_table), digits=3)
if (!is.null(od$interpretation)) cat("\nOverdispersion:", od$interpretation, "\n")
if (!is.null(rev)) { cat("\nRandom-effect (market) variance:\n"); print(rev) }
if (!is.null(days$sentences)) { cat("\nSubspecialty wait in absolute days:\n");
  cat(paste(" -", days$sentences), sep="\n") }
if (!is.null(auto$model_chosen)) cat("\nauto_model family choice:", auto$model_chosen,
    "-", auto$reason, "\n")
if (is.character(prose) && nzchar(prose)) cat("\n\nAccess results paragraph:\n", prose, "\n")
sink()

cat("\nWrote", file.path(outdir,"wait_days_model_summary.txt"),
    "and per-part CSVs + models.rds\n")
if (!is.null(days$sentences)) { cat("\nAbsolute wait-day differences:\n");
  cat(paste(" -", days$sentences), sep="\n"); cat("\n") }
