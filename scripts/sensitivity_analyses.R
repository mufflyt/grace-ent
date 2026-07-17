#!/usr/bin/env Rscript
# =============================================================================
# Sensitivity analyses for the ENT mystery-caller two-part model.
#
# Primary model (scripts/model_wait_days.R): binary rurality (RUCA 1-3 urban vs
# 4-10 rural), continuous covariates median-imputed (<3% missing).
#
# This script refits both parts (access = logistic, timeliness = NB, each with a
# CBSA random intercept and the same adjustment set) under two alternative
# specifications and compares the key coefficients to the primary model:
#
#   S-A  Three-level rurality  : Urban (RUCA 1-3, ref), Suburban (4-6),
#                                Rural (7-10) -- tests whether collapsing to a
#                                binary contrast masks a gradient.
#   S-B  Complete-case         : drop the rows with any missing covariate
#                                (no median imputation) -- tests imputation
#                                sensitivity.
#
# Output: model_output/supp/S8_sensitivity_analyses.csv  (+ console summary)
# =============================================================================

suppressMessages(devtools::load_all("~/mysterycall", quiet = TRUE))
num <- function(x) suppressWarnings(as.numeric(x))
OUT <- "model_output"; SUPP <- file.path(OUT, "supp")
dir.create(SUPP, showWarnings = FALSE, recursive = TRUE)

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L,
               ifelse(d$appointment_offered == "FALSE", 0L, NA_integer_))
d$wait_days <- num(d$wait_days_business)

# --- shared covariate construction (mirror the primary model) ----------------
covs <- c("ent_per_100k", "medicaid_fee_index", "svi_overall", "dual_pct")
zf_impute <- function(x){ v <- num(x); v[is.na(v)] <- median(v, na.rm = TRUE); as.numeric(scale(v)) }
zf_raw    <- function(x) as.numeric(scale(num(x)))          # NAs kept -> dropped

d$ent_type <- relevel(factor(d$ent_type), ref = "General")
ct <- table(d$caller); small <- names(ct)[ct < 15]
d$caller_f <- ifelse(d$caller %in% small, "Other", d$caller)
d$caller_f <- relevel(factor(d$caller_f), ref = names(sort(ct, decreasing = TRUE))[1])
clust <- d$cbsa_code
clust[is.na(clust) | clust == ""] <- d$county_fips[is.na(clust) | clust == ""]
clust[is.na(clust) | clust == ""] <- paste0("solo_", which(is.na(clust) | clust == ""))
d$cluster <- clust

rc <- num(d$ruca_code)
d$rural  <- relevel(factor(d$ruca_category), ref = "Urban")   # binary (primary)
d$rural3 <- relevel(factor(ifelse(rc <= 3, "Urban",
                    ifelse(rc <= 6, "Suburban", "Rural")),
                    levels = c("Urban", "Suburban", "Rural")), ref = "Urban")

# --- fit one specification, return tidy OR/IRR rows for terms of interest ----
fit_spec <- function(dat, rural_var, impute, label) {
  zf <- if (impute) zf_impute else zf_raw
  for (v in covs) dat[[paste0(v, "_z")]] <- zf(dat[[v]])
  preds <- c("ent_type", paste0(covs, "_z"), rural_var, "caller_f")
  if (!impute) dat <- dat[complete.cases(dat[, paste0(covs, "_z")]), ]

  d1 <- dat[!is.na(dat$offered), ]
  acc <- mysterycall_logistic_model(d1, "offered", preds, random_intercept = "cluster")
  d2  <- dat[which(dat$offered == 1 & !is.na(dat$wait_days)), ]
  tim <- mysterycall_nb_model(d2, "wait_days", preds, random_intercept = "cluster")

  keep <- c("ent_typeLaryngology", "ent_typePediatrics",
            paste0(rural_var, "Rural"), paste0(rural_var, "Suburban"))
  grab <- function(tab, est) {
    t <- tab[tab$term %in% keep, ]
    data.frame(spec = label, part = if (est == "or") "Access (OR)" else "Timeliness (IRR)",
               term = t$term, estimate = round(num(t[[est]]), 2),
               ci = sprintf("%.2f-%.2f", num(t$ci_lower), num(t$ci_upper)),
               p = round(num(t$p_value), 3),
               n_access = nrow(d1), n_timeliness = nrow(d2), stringsAsFactors = FALSE)
  }
  rbind(grab(acc$or_table, "or"), grab(tim$irr_table, "irr"))
}

res <- rbind(
  fit_spec(d, "rural",  TRUE,  "Primary (binary RUCA, imputed)"),
  fit_spec(d, "rural3", TRUE,  "S-A: 3-level RUCA"),
  fit_spec(d, "rural",  FALSE, "S-B: Complete-case")
)
# clean term labels
lab <- c(ent_typeLaryngology = "Laryngology", ent_typePediatrics = "Pediatric otolaryngology",
         ruralRural = "Rural (vs urban)", rural3Rural = "Rural, RUCA 7-10 (vs urban)",
         rural3Suburban = "Suburban, RUCA 4-6 (vs urban)")
res$term <- ifelse(res$term %in% names(lab), lab[res$term], res$term)

write.csv(res, file.path(SUPP, "S8_sensitivity_analyses.csv"), row.names = FALSE)
cat("\n==== SENSITIVITY ANALYSES ====\n")
print(res, row.names = FALSE)
cat(sprintf("\nWrote %s\n", file.path(SUPP, "S8_sensitivity_analyses.csv")))
