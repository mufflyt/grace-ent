#!/usr/bin/env Rscript
# =============================================================================
# Robustness / sensitivity analyses for the two-part ENT access-timeliness model
# (see scripts/model_wait_days.R). The subspecialty effect is the headline, so it
# is pressure-tested:
#
#   1. emmeans pairwise contrasts between subspecialties (which pairs differ),
#      access on the OR scale and wait on the response/day scale.
#   2. Interaction screen: does subspecialty modify the geographic covariates?
#      LRT of ent_type x {rural, ent_per_100k} against the main-effects model.
#   3. ICC / market random-effect share (how much clustering by CBSA).
#   4. HHI sensitivity: refit the wait model on the metro subset with hhi_2024
#      added, to confirm the null geographic story is not hiding a market effect.
#
# Output: model_output/robustness_summary.txt (+ CSVs)
# =============================================================================

suppressPackageStartupMessages({library(glmmTMB); library(emmeans); library(dplyr)})
options(stringsAsFactors = FALSE, width = 105)
outdir <- "model_output"; dir.create(outdir, showWarnings = FALSE)
num <- function(x) suppressWarnings(as.numeric(x))

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L, 0L)
d$wait_days <- num(d$wait_days_business)
zf <- function(x){v<-num(x); v[is.na(v)]<-median(v,na.rm=TRUE); as.numeric(scale(v))}
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
  d[[paste0(v,"_z")]] <- zf(d[[v]])
d$ent_type <- relevel(factor(d$ent_type), ref = "General")
d$rural    <- relevel(factor(d$ruca_category), ref = "Urban")
ct <- table(d$caller); d$caller_f <- ifelse(d$caller %in% names(ct)[ct<15], "Other", d$caller)
d$caller_f <- relevel(factor(d$caller_f), ref = names(sort(ct, decreasing=TRUE))[1])
clust <- d$cbsa_code; clust[!nzchar(clust)] <- d$county_fips[!nzchar(clust)]
clust[!nzchar(clust)] <- paste0("solo_", which(!nzchar(clust))); d$cluster <- clust

rhs <- "ent_type + ent_per_100k_z + medicaid_fee_index_z + svi_overall_z + dual_pct_z + rural + caller_f"
d1 <- d[!is.na(d$offered), ]
d2 <- d[which(d$offered == 1 & !is.na(d$wait_days)), ]

m_acc  <- glmmTMB(as.formula(paste("offered ~", rhs, "+ (1|cluster)")), d1, family = binomial)
m_wait <- glmmTMB(as.formula(paste("wait_days ~", rhs, "+ (1|cluster)")), d2, family = nbinom2)

sink(file.path(outdir, "robustness_summary.txt"))

cat("################ 1. SUBSPECIALTY PAIRWISE CONTRASTS ################\n\n")
cat("--- ACCESS: odds ratios between subspecialties (Tukey-adjusted) ---\n")
emm_a <- emmeans(m_acc, "ent_type", type = "response")
ca <- as.data.frame(pairs(emm_a))
ca <- ca[order(ca$p.value), ]
print(head(ca, 12), digits = 3, row.names = FALSE)
write.csv(ca, file.path(outdir, "robustness_access_pairwise.csv"), row.names = FALSE)

cat("\n--- TIMELINESS: wait-day rate ratios between subspecialties (Tukey) ---\n")
emm_w <- emmeans(m_wait, "ent_type", type = "response")
cw <- as.data.frame(pairs(emm_w))
cw <- cw[order(cw$p.value), ]
print(head(cw, 12), digits = 3, row.names = FALSE)
write.csv(cw, file.path(outdir, "robustness_wait_pairwise.csv"), row.names = FALSE)

cat("\n--- estimated marginal wait days per subspecialty ---\n")
print(as.data.frame(emm_w), digits = 3, row.names = FALSE)

cat("\n\n################ 2. INTERACTION SCREEN (LRT vs main-effects) ################\n")
cat("Does subspecialty modify the geographic covariates? (wait model)\n\n")
for (iv in c("rural", "ent_per_100k_z")) {
  f_int <- as.formula(paste("wait_days ~", rhs, "+ ent_type:", iv, "+ (1|cluster)"))
  m_int <- tryCatch(glmmTMB(f_int, d2, family = nbinom2), error = function(e) NULL)
  if (!is.null(m_int)) {
    lr <- anova(m_wait, m_int)
    cat(sprintf("  ent_type x %-14s  LRT chisq=%.2f  df=%d  p=%.3f\n",
                iv, lr$Chisq[2], lr$Df[2], lr$`Pr(>Chisq)`[2]))
  } else cat(sprintf("  ent_type x %-14s  (did not converge)\n", iv))
}

cat("\n\n################ 3. ICC / MARKET RANDOM-EFFECT SHARE ################\n")
vc_a <- as.numeric(VarCorr(m_acc)$cond$cluster)
icc_a <- vc_a / (vc_a + pi^2/3)   # latent-scale ICC for logistic
vc_w <- as.numeric(VarCorr(m_wait)$cond$cluster)
cat(sprintf("  ACCESS   : CBSA var=%.3f  ->  latent ICC=%.3f\n", vc_a, icc_a))
cat(sprintf("  TIMELINESS: CBSA var=%.3f (log scale); theta=%.3f\n", vc_w, sigma(m_wait)))

cat("\n\n################ 4. HHI SENSITIVITY (metro subset) ################\n")
d2h <- d2[nzchar(d2$hhi_2024) & !is.na(num(d2$hhi_2024)), ]
d2h$hhi_z <- as.numeric(scale(num(d2h$hhi_2024)))
cat(sprintf("  metro subset with HHI: n=%d of %d offered\n", nrow(d2h), nrow(d2)))
m_hhi <- tryCatch(glmmTMB(as.formula(paste("wait_days ~", rhs, "+ hhi_z + (1|cluster)")),
                  d2h, family = nbinom2), error = function(e) NULL)
if (!is.null(m_hhi)) {
  co <- summary(m_hhi)$coefficients$cond
  hhi_row <- co["hhi_z", ]
  cat(sprintf("  hhi_z (market concentration) IRR=%.3f  95%% CI %.3f-%.3f  p=%.3f\n",
              exp(hhi_row[1]), exp(hhi_row[1]-1.96*hhi_row[2]),
              exp(hhi_row[1]+1.96*hhi_row[2]), hhi_row[4]))
  # does subspecialty survive in the metro subset?
  et <- co[grep("^ent_type", rownames(co)), , drop = FALSE]
  cat("  subspecialty effects in metro subset (IRR, p):\n")
  for (r in rownames(et))
    cat(sprintf("    %-30s IRR=%.2f p=%.3f\n", sub("ent_type","",r),
                exp(et[r,1]), et[r,4]))
}
sink()

cat(readLines(file.path(outdir, "robustness_summary.txt")), sep = "\n")
cat("\n\nWrote", file.path(outdir, "robustness_summary.txt"), "+ pairwise CSVs\n")
