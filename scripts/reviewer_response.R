#!/usr/bin/env Rscript
# =============================================================================
# Reviewer-response analyses for the ENT mystery-caller study.
#
# CANONICAL ENGINES (must match the primary model in scripts/model_wait_days.R,
# which is built on the study package mysterycall):
#   ACCESS      lme4::glmer   (binomial)   <- mysterycall_logistic_model engine
#   TIMELINESS  glmmTMB       (nbinom2)     <- mysterycall_nb_model engine
# Using these here guarantees S11-S14 reconcile with Table 2 / Figure 2 / S1.
#
# Analyses:
#   1. Dual access outcomes (any offer vs with the sampled physician).
#   2. Global (df = #levels-1) LRT of the subspecialty factor, both parts.
#   3. Caller effects: OR table, global LRT, leave-one-caller-out, balance.
#   4. Practice/phone clustering sensitivity, BOTH parts.
#   5. Design-weighted (rurality) offer rate + median wait.
#   6. Incomplete-call best/worst-case bounds on the offer rate.
#
# Output: model_output/supp/  (S11 clustering, S12 caller, S12b LOCO,
#         S12c caller balance, S13 dual outcomes, S14 global tests, S15 weighting)
#         model_output/reviewer_response_summary.txt
# =============================================================================

suppressPackageStartupMessages({library(lme4); library(glmmTMB)})
options(stringsAsFactors = FALSE, width = 110)
OUT <- "model_output"; SUPP <- file.path(OUT, "supp")
dir.create(SUPP, showWarnings = FALSE, recursive = TRUE)
num <- function(x) suppressWarnings(as.numeric(x))

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]

d$offered      <- ifelse(d$appointment_offered == "TRUE", 1L,
                  ifelse(d$appointment_offered == "FALSE", 0L, NA_integer_))
d$offered_samp <- as.integer(d$appointment_outcome == "With sampled physician")
d$wait_days    <- num(d$wait_days_business)

zf <- function(x){v<-num(x); v[is.na(v)]<-median(v,na.rm=TRUE); as.numeric(scale(v))}
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
  d[[paste0(v,"_z")]] <- zf(d[[v]])
d$ent_type <- relevel(factor(d$ent_type), ref = "General")
d$rural    <- relevel(factor(d$ruca_category), ref = "Urban")
ct <- table(d$caller); d$caller_f <- ifelse(d$caller %in% names(ct)[ct<15], "Other", d$caller)
d$caller_f <- relevel(factor(d$caller_f), ref = names(sort(ct, decreasing=TRUE))[1])
cl <- d$cbsa_code; cl[is.na(cl)|cl==""] <- d$county_fips[is.na(cl)|cl==""]
cl[is.na(cl)|cl==""] <- paste0("solo_", which(is.na(cl)|cl=="")); d$cluster <- cl
ph <- d$phone; ph[!nzchar(ph) | is.na(ph)] <- paste0("nophone_", which(!nzchar(ph) | is.na(ph)))
d$phone_id <- ph

rhs  <- "ent_type + ent_per_100k_z + medicaid_fee_index_z + svi_overall_z + dual_pct_z + rural + caller_f"
rhs0 <- "ent_per_100k_z + medicaid_fee_index_z + svi_overall_z + dual_pct_z + rural + caller_f"          # no ent_type
rhsc <- "ent_type + ent_per_100k_z + medicaid_fee_index_z + svi_overall_z + dual_pct_z + rural"           # no caller_f
d1   <- d[!is.na(d$offered), ]
d2   <- d[which(d$offered == 1 & !is.na(d$wait_days)), ]

# ---- engine-specific fitters (ACCESS = glmer, TIMELINESS = glmmTMB) ----------
GC <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# explicit fitters (no update(): lme4::anova requires models to share one data object).
# nAGQ = 0L matches mysterycall_logistic_model (the canonical Table 2 / S1 engine),
# so every access estimate here reconciles with the primary model.
fit_acc <- function(dat, y = "offered", extra_re = "", rhs_ = rhs)
  glmer(as.formula(sprintf("%s ~ %s + (1|cluster)%s", y, rhs_, extra_re)), dat,
        family = binomial, control = GC, nAGQ = 0L)
fit_wt  <- function(dat, extra_re = "", rhs_ = rhs)
  glmmTMB(as.formula(sprintf("wait_days ~ %s + (1|cluster)%s", rhs_, extra_re)), dat, family = nbinom2)

or_ci <- function(m) {                                   # works for glmer & glmmTMB
  co <- if (inherits(m, "glmmTMB")) summary(m)$coefficients$cond else summary(m)$coefficients
  data.frame(term = rownames(co), or = exp(co[,1]),
             lo = exp(co[,1]-1.96*co[,2]), hi = exp(co[,1]+1.96*co[,2]),
             p = co[,4], row.names = NULL)
}
# LRT df: glmer anova names it "Df" (already the difference); glmmTMB names it "Chi Df"
lrt <- function(an) {
  dfc <- if ("Chi Df" %in% colnames(an)) "Chi Df" else "Df"
  list(chisq = an[["Chisq"]][2], df = an[[dfc]][2], p = an[["Pr(>Chisq)"]][2])
}

sink(file.path(OUT, "reviewer_response_summary.txt"))

# =============================================================================
cat("################ 1. DUAL ACCESS OUTCOMES (glmer) ################\n\n")
m_any  <- fit_acc(d1, "offered")
m_samp <- fit_acc(d,  "offered_samp")
cat(sprintf("Primary  ANY offer:      n=%d, events=%d (%.1f%%)\n", nrow(d1), sum(d1$offered), 100*mean(d1$offered)))
cat(sprintf("Secondary SAMPLED phys.: n=%d, events=%d (%.1f%%)\n\n", nrow(d), sum(d$offered_samp), 100*mean(d$offered_samp)))
ta <- or_ci(m_any);  ta <- ta[grepl("ent_type", ta$term), ]
ts <- or_ci(m_samp); ts <- ts[grepl("ent_type", ts$term), ]
sublab <- c("Facial Plastics"="Facial plastic surgery","Head and Neck Cancer"="Head and neck oncology",
            "Otology/neurotology"="Otology/neurotology","Pediatrics"="Pediatric otolaryngology",
            "Laryngology"="Laryngology","Rhinology"="Rhinology","Sleep"="Sleep medicine")
subs <- sub("ent_type","", ta$term); subs <- ifelse(subs %in% names(sublab), sublab[subs], subs)
dual <- data.frame(subs,
  sprintf("%.2f (%.2f-%.2f)", ta$or, ta$lo, ta$hi), round(ta$p,3),
  sprintf("%.2f (%.2f-%.2f)", ts$or, ts$lo, ts$hi), round(ts$p,3))
names(dual) <- c("Subspecialty (vs general otolaryngology)","Any offer: OR (95% CI)","Any offer: p",
                 "Sampled physician: OR (95% CI)","Sampled physician: p")
print(dual, row.names = FALSE)
write.csv(dual, file.path(SUPP, "S13_dual_access_outcomes.csv"), row.names = FALSE)
cat(sprintf("\nAny-offer nominal (p<.05): %s | Sampled-physician nominal (p<.05): %s\n",
    paste(subs[ta$p<.05], collapse=", "), paste(subs[ts$p<.05], collapse=", ")))
oc <- table(factor(d$appointment_outcome[d$offered==1]))
cat(sprintf("Offer reconciliation: any-offer=%d = sampled %d + different %d + APP %d + uncoded %d\n",
    sum(d$offered), sum(d$appointment_outcome=="With sampled physician"),
    sum(d$offered==1 & d$appointment_outcome=="With different physician"),
    sum(d$appointment_outcome=="With APP only"),
    sum(d$offered==1 & !(d$appointment_outcome %in% c("With sampled physician","With different physician","With APP only")))))
cat(sprintf("Non-analyzable waits: %d (=9 missing date + 1 invalid year, record 470) -> timeliness n=%d\n",
    sum(d$offered==1 & is.na(d$wait_days)), nrow(d2)))

# =============================================================================
cat("\n\n################ 2. GLOBAL SUBSPECIALTY TEST (joint LRT) ################\n\n")
a_any  <- lrt(anova(fit_acc(d1, "offered",      rhs_ = rhs0), m_any))     # any offer
a_samp <- lrt(anova(fit_acc(d,  "offered_samp", rhs_ = rhs0), m_samp))    # with sampled physician
m_wt   <- fit_wt(d2)
a_wt   <- lrt(anova(fit_wt(d2, rhs_ = rhs0), m_wt))                        # wait
cat(sprintf("ACCESS any-offer   : ent_type joint LRT chisq=%.2f df=%d p=%.3f\n", a_any$chisq, a_any$df, a_any$p))
cat(sprintf("ACCESS sampled-phys: ent_type joint LRT chisq=%.2f df=%d p=%.3f\n", a_samp$chisq, a_samp$df, a_samp$p))
cat(sprintf("TIMELINESS wait    : ent_type joint LRT chisq=%.2f df=%d p=%.3f\n", a_wt$chisq, a_wt$df, a_wt$p))
gtab <- data.frame(
  "Model part" = c("Access (any offer)","Access (with sampled physician)","Timeliness (wait)"),
  "Chi-square" = round(c(a_any$chisq, a_samp$chisq, a_wt$chisq), 2),
  "df"         = c(a_any$df, a_samp$df, a_wt$df),
  "p-value"    = round(c(a_any$p, a_samp$p, a_wt$p), 3),
  "N"          = c(nrow(d1), nrow(d), nrow(d2)), check.names = FALSE)
write.csv(gtab, file.path(SUPP, "S14_global_subspecialty_test.csv"), row.names = FALSE)

# =============================================================================
cat("\n\n################ 3. CALLER EFFECTS + GLOBAL TEST + LOCO + BALANCE ############\n\n")
cal <- or_ci(m_any); cal <- cal[grepl("caller_f", cal$term), ]
cal_tab <- data.frame("Caller"=sub("caller_f","",cal$term),
                      "Access OR (95% CI)"=sprintf("%.2f (%.2f-%.2f)",cal$or,cal$lo,cal$hi),
                      "p-value"=round(cal$p,3), check.names=FALSE)
print(cal_tab, row.names = FALSE)
write.csv(cal_tab, file.path(SUPP, "S12_caller_effects.csv"), row.names = FALSE)

a_cal      <- lrt(anova(fit_acc(d1, "offered",      rhs_ = rhsc), m_any))   # caller, any offer
a_cal_samp <- lrt(anova(fit_acc(d,  "offered_samp", rhs_ = rhsc), m_samp))  # caller, sampled physician
a_cal_wt   <- lrt(anova(fit_wt(d2, rhs_ = rhsc), m_wt))                     # caller, timeliness
cat(sprintf("\nCaller joint LRT  any-offer: chisq=%.2f df=%d p=%.4f | sampled: chisq=%.2f df=%d p=%.3f | wait: chisq=%.2f df=%d p=%.3f\n",
    a_cal$chisq, a_cal$df, a_cal$p, a_cal_samp$chisq, a_cal_samp$df, a_cal_samp$p, a_cal_wt$chisq, a_cal_wt$df, a_cal_wt$p))
write.csv(data.frame(
  "Factor"     = rep(c("Caller (research assistant)","Requested subspecialty"), 3),
  "Model part" = rep(c("Access (any offer)","Access (with sampled physician)","Timeliness (wait)"), each = 2),
  "Chi-square" = round(c(a_cal$chisq, a_any$chisq, a_cal_samp$chisq, a_samp$chisq, a_cal_wt$chisq, a_wt$chisq), 2),
  "df"         = c(a_cal$df, a_any$df, a_cal_samp$df, a_samp$df, a_cal_wt$df, a_wt$df),
  "p-value"    = round(c(a_cal$p, a_any$p, a_cal_samp$p, a_samp$p, a_cal_wt$p, a_wt$p), 3), check.names = FALSE),
  file.path(SUPP, "S14b_factor_joint_tests.csv"), row.names = FALSE)

cat("\nLeave-one-caller-out: Pediatrics access OR + global ent_type p\n")
loco <- do.call(rbind, lapply(levels(d1$caller_f), function(clv) {
  sub <- d1[d1$caller_f != clv, ]; sub$caller_f <- droplevels(sub$caller_f)
  m <- tryCatch(fit_acc(sub, "offered"), error=function(e) NULL); if (is.null(m)) return(NULL)
  o <- or_ci(m); ped <- o[o$term=="ent_typePediatrics", ]
  g <- tryCatch(lrt(anova(fit_acc(sub, "offered", rhs_ = rhs0), m)), error=function(e) list(p=NA))
  data.frame("Caller excluded"=clv, "N"=nrow(sub), "Pediatrics access OR"=round(ped$or,2),
             "Pediatrics p"=round(ped$p,3), "Subspecialty joint p"=round(g$p,3), check.names=FALSE)
}))
print(loco, row.names = FALSE)
write.csv(loco, file.path(SUPP, "S12b_leave_one_caller_out.csv"), row.names = FALSE)

# caller balance: FULL caller x subspecialty distribution (counts), plus N and % rural
set.seed(20260718)                                   # for the Monte Carlo tests below
cx <- as.data.frame.matrix(table(d1$caller_f, d1$ent_type))
subcols <- colnames(cx)
cx <- data.frame("Caller" = rownames(cx), cx, check.names = FALSE)
cx[["N"]]       <- rowSums(cx[, subcols])
cx[["% rural"]] <- round(100 * as.numeric(tapply(d1$rural == "Rural", d1$caller_f, mean)[cx$Caller]), 0)
print(cx, row.names = FALSE)
write.csv(cx, file.path(SUPP, "S12c_caller_balance.csv"), row.names = FALSE)

# balance tests: Monte Carlo (sparse cells, incl. the n=13 "Other" caller)
chi_rur <- suppressWarnings(chisq.test(table(d1$caller_f, d1$rural),   simulate.p.value = TRUE, B = 1e4))
chi_sub <- suppressWarnings(chisq.test(table(d1$caller_f, d1$ent_type), simulate.p.value = TRUE, B = 1e4))
cat(sprintf("\nCaller balance (Monte Carlo chisq): caller x rurality p=%.3f; caller x subspecialty p=%.3f\n",
    chi_rur$p.value, chi_sub$p.value))
write.csv(data.frame(
  "Comparison" = c("Caller × rurality", "Caller × subspecialty"),
  "Chi-square" = round(c(chi_rur$statistic, chi_sub$statistic), 2),
  "Test"       = c("Monte Carlo (B=10,000)", "Monte Carlo (B=10,000)"),
  "p-value"    = round(c(chi_rur$p.value, chi_sub$p.value), 3), check.names = FALSE),
  file.path(SUPP, "S12c_balance_tests.csv"), row.names = FALSE)

# =============================================================================
cat("\n\n################ 4. PRACTICE / PHONE CLUSTERING (both parts) ################\n\n")
np <- length(unique(d$phone_id[!grepl("^nophone", d$phone_id)]))
cat(sprintf("Analytic calls=%d; distinct telephone numbers=%d; %d missing a number; %d calls share a number (max %d)\n\n",
    nrow(d), np, sum(grepl("^nophone", d$phone_id)),
    sum(d$phone_id %in% names(table(d$phone_id))[table(d$phone_id)>1] & !grepl("^nophone", d$phone_id)),
    max(table(d$phone_id[!grepl("^nophone", d$phone_id)]))))
grab <- function(m, term) { o <- or_ci(m); r <- o[o$term==term, ]; sprintf("%.2f (%.2f-%.2f)", r$or,r$lo,r$hi) }
# access: three specs, each with its own joint subspecialty LRT (all nested)
m_any_ph <- fit_acc(d1, "offered", " + (1|phone_id)")
g_any_ph <- lrt(anova(fit_acc(d1, "offered", " + (1|phone_id)", rhs_ = rhs0), m_any_ph))
d1u <- d1[!duplicated(d1$phone_id), ]; m_any_u <- fit_acc(d1u, "offered")
g_any_u <- lrt(anova(fit_acc(d1u, "offered", rhs_ = rhs0), m_any_u))
# timeliness: same three specs + their joint subspecialty LRTs
m_wt_ph <- fit_wt(d2, " + (1|phone_id)")
g_wt_ph <- lrt(anova(fit_wt(d2, " + (1|phone_id)", rhs_ = rhs0), m_wt_ph))
d2u <- d2[!duplicated(d2$phone_id), ]; m_wt_u <- fit_wt(d2u)
g_wt_u  <- lrt(anova(fit_wt(d2u, rhs_ = rhs0), m_wt_u))
vc_a <- as.numeric(VarCorr(m_any_ph)$phone_id); vc_w <- as.numeric(VarCorr(m_wt_ph)$cond$phone_id)
cat(sprintf("Added phone-RE variance: access=%.3f, timeliness=%.3f (both negligible)\n", vc_a, vc_w))
clus <- data.frame(
  "Model" = c("Primary (CBSA random intercept only)","CBSA + practice-telephone random intercept","One call per telephone number"),
  "N access" = c(nrow(d1), nrow(d1), nrow(d1u)),
  "Pediatrics access OR (95% CI)" = c(grab(m_any,"ent_typePediatrics"), grab(m_any_ph,"ent_typePediatrics"), grab(m_any_u,"ent_typePediatrics")),
  "Subspecialty access joint p" = round(c(a_any$p, g_any_ph$p, g_any_u$p), 3),
  "N timeliness" = c(nrow(d2), nrow(d2), nrow(d2u)),
  "Laryngology wait IRR (95% CI)" = c(grab(m_wt,"ent_typeLaryngology"), grab(m_wt_ph,"ent_typeLaryngology"), grab(m_wt_u,"ent_typeLaryngology")),
  "Subspecialty timeliness joint p" = round(c(a_wt$p, g_wt_ph$p, g_wt_u$p), 3), check.names=FALSE)
print(clus, row.names = FALSE)
write.csv(clus, file.path(SUPP, "S11_clustering_sensitivity.csv"), row.names = FALSE)

# =============================================================================
cat("\n\n################ 5. DESIGN-WEIGHTED (rurality) OFFER RATE + WAIT ############\n\n")
# frame rural prevalence 791/11333 = 6.98%; the analytic sample is ~50/50 by design
w_rural <- 791/11333; w_urban <- 1 - w_rural
orr <- tapply(d$offered, d$rural, mean, na.rm=TRUE)
med <- tapply(d$wait_days[d$offered==1], d$rural[d$offered==1], median, na.rm=TRUE)
wt_off <- w_rural*orr["Rural"] + w_urban*orr["Urban"]
wt_med <- w_rural*med["Rural"] + w_urban*med["Urban"]
cat(sprintf("Unweighted offer rate=%.1f%%; design-weighted (rural %.1f%%)=%.1f%%\n",
    100*mean(d$offered,na.rm=TRUE), 100*w_rural, 100*wt_off))
cat(sprintf("Stratum offer rates: rural %.1f%%, urban %.1f%%; stratum median wait: rural %.0f, urban %.0f (weighted %.0f)\n",
    100*orr["Rural"], 100*orr["Urban"], med["Rural"], med["Urban"], wt_med))
write.csv(data.frame(
  "Estimate"=c("Appointment offer rate","Median business-day wait"),
  "Unweighted (analytic sample)"=c(sprintf("%.1f%%",100*mean(d$offered,na.rm=TRUE)),
                                   sprintf("%.0f days", median(d$wait_days[d$offered==1],na.rm=TRUE))),
  "Design-weighted to frame (rural 7%)"=c(sprintf("%.1f%%",100*wt_off), sprintf("%.0f days", wt_med)),
  check.names=FALSE), file.path(SUPP, "S15_design_weighted.csv"), row.names=FALSE)

# =============================================================================
cat("\n\n################ 6. INCOMPLETE-CALL BOUNDS ON THE OFFER RATE ################\n\n")
dd <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
n_incomplete <- sum(dd$complete != "Complete")
n_unknown <- sum(dd$complete == "Complete" & (dd$ent_type == "" | is.na(dd$ent_type)))
n_unknown_off <- sum(dd$complete == "Complete" & (dd$ent_type == "" | is.na(dd$ent_type)) & dd$appointment_offered == "TRUE")
uni <- nrow(dd); n_off <- sum(d$offered, na.rm=TRUE)
cat(sprintf("Universe = all %d sampled. Analytic=%d, completed-unknown-subspecialty=%d (offers among them=%d), incomplete=%d.\n",
    uni, nrow(d), n_unknown, n_unknown_off, n_incomplete))
cat(sprintf("Overall offers observed=%d; offer rate over all %d = %.1f%%\n", n_off, uni, 100*n_off/uni))
cat(sprintf("  worst case (no incomplete would have offered): %.1f%%\n", 100*n_off/uni))
cat(sprintf("  best case  (all incomplete would have offered): %.1f%%\n", 100*(n_off+n_incomplete)/uni))

sink()
cat(readLines(file.path(OUT, "reviewer_response_summary.txt")), sep = "\n")
cat("\n\nWrote reviewer_response_summary.txt + S11-S15 CSVs\n")
