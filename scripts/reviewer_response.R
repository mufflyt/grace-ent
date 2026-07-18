#!/usr/bin/env Rscript
# =============================================================================
# Reviewer-response analyses for the ENT mystery-caller study.
#
# These four analyses harden the paper against the peer-review critique:
#
#   1. DUAL ACCESS OUTCOMES. The primary access outcome is ANY new-patient
#      appointment offered (with the sampled physician, another physician in the
#      practice, or an APP). A stricter SECONDARY outcome counts only an
#      appointment WITH THE SAMPLED PHYSICIER. Both are modeled so the reader can
#      see the estimates are not an artifact of the offer definition.
#
#   2. GLOBAL SUBSPECIALTY TEST. A single joint likelihood-ratio test of the
#      whole ent_type factor (not the reference-category contrasts) in each part,
#      reported alongside the Tukey-adjusted pairwise contrasts.
#
#   3. CALLER (research-assistant) EFFECTS. The caller fixed effect is surfaced
#      as its own OR table, and a leave-one-caller-out refit shows how stable the
#      headline subspecialty estimate is to any single caller.
#
#   4. PRACTICE / PHONE CLUSTERING. 731 analytic calls reached 670 distinct
#      phone numbers; 76 calls share a number with >=1 other call (max 6/number).
#      The primary model clusters on CBSA only. Here we (a) add a phone-number
#      random intercept and (b) refit on one-call-per-phone, to show the CBSA-only
#      model is not understating uncertainty from repeated practices.
#
# Output: model_output/supp/  (S11 clustering, S12 caller, S13 sampled-physician)
#         model_output/reviewer_response_summary.txt
# =============================================================================

suppressPackageStartupMessages({library(glmmTMB); library(emmeans)})
options(stringsAsFactors = FALSE, width = 110)
OUT <- "model_output"; SUPP <- file.path(OUT, "supp")
dir.create(SUPP, showWarnings = FALSE, recursive = TRUE)
num <- function(x) suppressWarnings(as.numeric(x))

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]

# ---- outcomes ---------------------------------------------------------------
d$offered      <- ifelse(d$appointment_offered == "TRUE", 1L,
                  ifelse(d$appointment_offered == "FALSE", 0L, NA_integer_))
# stricter secondary access outcome: appointment WITH THE SAMPLED PHYSICIAN
d$offered_samp <- as.integer(d$appointment_outcome == "With sampled physician")
d$wait_days    <- num(d$wait_days_business)

# ---- predictors (mirror the primary model) ----------------------------------
zf <- function(x){v<-num(x); v[is.na(v)]<-median(v,na.rm=TRUE); as.numeric(scale(v))}
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
  d[[paste0(v,"_z")]] <- zf(d[[v]])
d$ent_type <- relevel(factor(d$ent_type), ref = "General")
d$rural    <- relevel(factor(d$ruca_category), ref = "Urban")
ct <- table(d$caller); d$caller_f <- ifelse(d$caller %in% names(ct)[ct<15], "Other", d$caller)
d$caller_f <- relevel(factor(d$caller_f), ref = names(sort(ct, decreasing=TRUE))[1])

# CBSA (metro) cluster with county fallback = the PRIMARY random intercept
clust <- d$cbsa_code; clust[!nzchar(clust)] <- d$county_fips[!nzchar(clust)]
clust[!nzchar(clust)] <- paste0("solo_", which(!nzchar(clust))); d$cluster <- clust

# phone-number id (the practice's front desk); solo id where phone is missing
ph <- d$phone; ph[!nzchar(ph) | is.na(ph)] <- paste0("nophone_", which(!nzchar(ph) | is.na(ph)))
d$phone_id <- ph

rhs   <- "ent_type + ent_per_100k_z + medicaid_fee_index_z + svi_overall_z + dual_pct_z + rural + caller_f"
d1    <- d[!is.na(d$offered), ]
d2    <- d[which(d$offered == 1 & !is.na(d$wait_days)), ]

fit_logit <- function(dat, y, re = "(1|cluster)")
  glmmTMB(as.formula(sprintf("%s ~ %s + %s", y, rhs, re)), dat, family = binomial)
fit_nb <- function(dat, y = "wait_days", re = "(1|cluster)")
  glmmTMB(as.formula(sprintf("%s ~ %s + %s", y, rhs, re)), dat, family = nbinom2)

or_ci <- function(m) {
  co <- summary(m)$coefficients$cond
  data.frame(term = rownames(co), or = exp(co[,1]),
             lo = exp(co[,1]-1.96*co[,2]), hi = exp(co[,1]+1.96*co[,2]),
             p = co[,4], row.names = NULL)
}
# joint LRT of the whole ent_type factor
global_ent <- function(m, dat, fam) {
  rhs0 <- sub("ent_type \\+ ", "", rhs)
  re   <- if (fam == "nb") "(1|cluster)" else "(1|cluster)"
  m0 <- if (fam == "nb")
          glmmTMB(as.formula(sprintf("%s ~ %s + %s", all.vars(formula(m))[1], rhs0, re)), dat, family = nbinom2)
        else
          glmmTMB(as.formula(sprintf("%s ~ %s + %s", all.vars(formula(m))[1], rhs0, re)), dat, family = binomial)
  an <- anova(m0, m)
  data.frame(chisq = an$Chisq[2], df = an$Df[2], p = an$`Pr(>Chisq)`[2])
}

sink(file.path(OUT, "reviewer_response_summary.txt"))

# =============================================================================
cat("################ 1. DUAL ACCESS OUTCOMES ################\n\n")
m_any  <- fit_logit(d1, "offered")
m_samp <- fit_logit(d, "offered_samp")
cat(sprintf("Primary  ANY offer:      n=%d, events=%d (%.1f%%)\n",
            nrow(d1), sum(d1$offered), 100*mean(d1$offered)))
cat(sprintf("Secondary SAMPLED phys.: n=%d, events=%d (%.1f%%)\n\n",
            nrow(d), sum(d$offered_samp), 100*mean(d$offered_samp)))
cmp <- merge(
  transform(or_ci(m_any)[grepl("ent_type", or_ci(m_any)$term), c("term","or","lo","hi","p")],
            outcome = "Any offer"),
  NULL, all = TRUE)
tab_any  <- or_ci(m_any);  tab_any  <- tab_any [grepl("ent_type", tab_any$term), ]
tab_samp <- or_ci(m_samp); tab_samp <- tab_samp[grepl("ent_type", tab_samp$term), ]
sublab <- c("Facial Plastics"="Facial plastic surgery","Head and Neck Cancer"="Head and neck oncology",
            "Otology/neurotology"="Otology/neurotology","Pediatrics"="Pediatric otolaryngology",
            "Laryngology"="Laryngology","Rhinology"="Rhinology","Sleep"="Sleep medicine")
subs <- sub("ent_type","", tab_any$term); subs <- ifelse(subs %in% names(sublab), sublab[subs], subs)
dual <- data.frame(
  subspecialty = subs,
  any_or  = sprintf("%.2f (%.2f-%.2f)", tab_any$or,  tab_any$lo,  tab_any$hi),
  any_p   = round(tab_any$p, 3),
  samp_or = sprintf("%.2f (%.2f-%.2f)", tab_samp$or, tab_samp$lo, tab_samp$hi),
  samp_p  = round(tab_samp$p, 3))
print(dual, row.names = FALSE)
dual_out <- setNames(dual, c("Subspecialty (vs general otolaryngology)",
  "Any offer: OR (95% CI)", "Any offer: p", "Sampled physician: OR (95% CI)", "Sampled physician: p"))
write.csv(dual_out, file.path(SUPP, "S13_dual_access_outcomes.csv"), row.names = FALSE)

# offer arithmetic reconciliation (for the manuscript footnote)
oc <- table(factor(d$appointment_outcome[d$offered == 1]))
cat(sprintf("\nOffer reconciliation: any-offer=%d = sampled %d + different %d + APP %d + uncoded %d\n",
    sum(d$offered), sum(d$appointment_outcome=="With sampled physician"),
    sum(d$appointment_outcome=="With different physician" & d$offered==1),
    sum(d$appointment_outcome=="With APP only"),
    sum(d$offered==1 & !(d$appointment_outcome %in%
        c("With sampled physician","With different physician","With APP only")))))
cat(sprintf("Offers missing a recorded wait: %d (all 'sampled physician') -> timeliness n=%d\n",
    sum(d$offered==1 & is.na(d$wait_days)), nrow(d2)))

# =============================================================================
cat("\n\n################ 2. GLOBAL SUBSPECIALTY TEST (joint LRT) ################\n\n")
g_any  <- global_ent(m_any,  d1, "logit")
g_wait <- global_ent(fit_nb(d2), d2, "nb")
cat(sprintf("ACCESS (any offer): ent_type joint LRT chisq=%.2f df=%d p=%.3f\n",
            g_any$chisq, g_any$df, g_any$p))
cat(sprintf("TIMELINESS (wait) : ent_type joint LRT chisq=%.2f df=%d p=%.3f\n",
            g_wait$chisq, g_wait$df, g_wait$p))
gtab <- data.frame(
  "Model part" = c("Access (any offer)", "Timeliness (wait)"),
  "Chi-square" = round(c(g_any$chisq, g_wait$chisq), 2),
  "df"         = c(g_any$df, g_wait$df),
  "p-value"    = round(c(g_any$p, g_wait$p), 3),
  check.names = FALSE)
write.csv(gtab, file.path(SUPP, "S14_global_subspecialty_test.csv"), row.names = FALSE)

# =============================================================================
cat("\n\n################ 3. CALLER EFFECTS + LEAVE-ONE-CALLER-OUT ################\n\n")
cal <- or_ci(m_any); cal <- cal[grepl("caller_f", cal$term), ]
cal_tab <- data.frame("Caller" = sub("caller_f","", cal$term),
                      "Access OR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", cal$or, cal$lo, cal$hi),
                      "p-value" = round(cal$p, 3), check.names = FALSE)
cat("Access OR for each caller vs the reference caller:\n")
print(cal_tab, row.names = FALSE)
write.csv(cal_tab, file.path(SUPP, "S12_caller_effects.csv"), row.names = FALSE)

cat("\nLeave-one-caller-out: headline Pediatrics access OR + global ent_type p\n")
loco <- do.call(rbind, lapply(levels(d1$caller_f), function(cl) {
  sub <- d1[d1$caller_f != cl, ]
  sub$caller_f <- droplevels(sub$caller_f)
  m <- tryCatch(fit_logit(sub, "offered"), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  o <- or_ci(m); ped <- o[o$term == "ent_typePediatrics", ]
  g <- tryCatch(global_ent(m, sub, "logit"), error = function(e) data.frame(p=NA))
  data.frame("Caller excluded" = cl, "N" = nrow(sub),
             "Pediatrics access OR" = round(ped$or,2), "Pediatrics p" = round(ped$p,3),
             "Subspecialty joint p" = round(g$p,3), check.names = FALSE)
}))
print(loco, row.names = FALSE)
write.csv(loco, file.path(SUPP, "S12b_leave_one_caller_out.csv"), row.names = FALSE)

# =============================================================================
cat("\n\n################ 4. PRACTICE / PHONE CLUSTERING ################\n\n")
tp <- table(d$phone_id[grepl("^[0-9]", d$phone_id) | !grepl("^nophone", d$phone_id)])
cat(sprintf("Analytic calls=%d, distinct phone numbers=%d, calls sharing a number=%d (max %d/number)\n\n",
    nrow(d), length(unique(d$phone_id)),
    sum(d$phone_id %in% names(table(d$phone_id))[table(d$phone_id)>1]),
    max(table(d$phone_id))))

# (a) add a phone-number random intercept alongside CBSA
m_any_ph  <- fit_logit(d1, "offered", "(1|cluster) + (1|phone_id)")
m_wait_ph <- fit_nb(d2, "wait_days", "(1|cluster) + (1|phone_id)")
vc_a <- VarCorr(m_any_ph)$cond
cat(sprintf("(a) Access, CBSA+phone REs: CBSA var=%.3f  phone var=%.3f\n",
    as.numeric(vc_a$cluster), as.numeric(vc_a$phone_id)))
# compare headline Pediatrics OR: CBSA-only vs CBSA+phone
p_only <- or_ci(m_any)[or_ci(m_any)$term=="ent_typePediatrics", ]
p_ph   <- or_ci(m_any_ph)[or_ci(m_any_ph)$term=="ent_typePediatrics", ]
cat(sprintf("    Pediatrics access OR: CBSA-only %.2f (%.2f-%.2f); +phone %.2f (%.2f-%.2f)\n",
    p_only$or,p_only$lo,p_only$hi, p_ph$or,p_ph$lo,p_ph$hi))

# (b) one-call-per-phone (keep the first record per number)
d1u <- d1[!duplicated(d1$phone_id), ]
d2u <- d2[!duplicated(d2$phone_id), ]
m_any_u  <- fit_logit(d1u, "offered")
g_any_u  <- global_ent(m_any_u, d1u, "logit")
p_u <- or_ci(m_any_u)[or_ci(m_any_u)$term=="ent_typePediatrics", ]
cat(sprintf("(b) One-call-per-phone: n=%d, Pediatrics access OR %.2f (%.2f-%.2f); ent_type global p=%.3f\n",
    nrow(d1u), p_u$or, p_u$lo, p_u$hi, g_any_u$p))

clus <- data.frame(
  "Model" = c("Primary (CBSA random intercept only)", "CBSA + practice-telephone random intercept",
              "One call per telephone number"),
  "N" = c(nrow(d1), nrow(d1), nrow(d1u)),
  "Pediatrics access OR (95% CI)" = c(sprintf("%.2f (%.2f-%.2f)", p_only$or,p_only$lo,p_only$hi),
                           sprintf("%.2f (%.2f-%.2f)", p_ph$or,p_ph$lo,p_ph$hi),
                           sprintf("%.2f (%.2f-%.2f)", p_u$or,p_u$lo,p_u$hi)),
  "Subspecialty joint p" = c(round(g_any$p,3), NA, round(g_any_u$p,3)), check.names = FALSE)
write.csv(clus, file.path(SUPP, "S11_clustering_sensitivity.csv"), row.names = FALSE)

sink()
cat(readLines(file.path(OUT, "reviewer_response_summary.txt")), sep = "\n")
cat("\n\nWrote reviewer_response_summary.txt + S11-S14 CSVs\n")
