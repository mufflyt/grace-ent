#!/usr/bin/env Rscript
# =============================================================================
# Re-analysis with the RUCA cutoff urban = 1-3, rural = 4-10 (suburban folded
# into rural, as specified). Recomputes (1) the power reconciliation vs the
# pre-specified plan and (2) the two-part access/timeliness model with the
# binary rural indicator.
#
# Output: model_output/reanalysis_ruca_binary.txt
# =============================================================================

suppressPackageStartupMessages({library(glmmTMB); library(dplyr)})
options(stringsAsFactors = FALSE, width = 105)
num <- function(x) suppressWarnings(as.numeric(x))
d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L, 0L)
d$wait_days <- num(d$wait_days_business)
# NEW binary rurality: urban = RUCA 1-3, rural = RUCA 4-10 (Suburban + Rural)
d$rural2 <- factor(ifelse(d$ruca_category == "Urban", "Urban", "Rural"),
                   levels = c("Urban", "Rural"))
zf <- function(x){v<-num(x); v[is.na(v)]<-median(v,na.rm=TRUE); as.numeric(scale(v))}
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
  d[[paste0(v,"_z")]] <- zf(d[[v]])
d$ent_type <- relevel(factor(d$ent_type), ref = "General")
ct <- table(d$caller); d$caller_f <- relevel(factor(ifelse(d$caller %in% names(ct)[ct<15],"Other",d$caller)),
                                              ref = names(sort(ct,decreasing=TRUE))[1])
clust <- d$cbsa_code; clust[!nzchar(clust)] <- d$county_fips[!nzchar(clust)]
clust[!nzchar(clust)] <- paste0("solo_", which(!nzchar(clust))); d$cluster <- clust

sink("model_output/reanalysis_ruca_binary.txt")

cat("################ RUCA cutoff: urban = 1-3, rural = 4-10 ################\n\n")
cat("Rurality distribution (binary):\n")
cat("  Complete/analytic:\n"); print(table(d$rural2))
wait <- d[which(d$offered==1 & !is.na(d$wait_days)), ]
cat("  Wait-time sample (primary rural-vs-urban contrast):\n"); print(table(wait$rural2))

cat("\n################ POWER RECONCILIATION vs the plan (email) ################\n")
r <- sum(wait$rural2=="Rural"); u <- sum(wait$rural2=="Urban"); tot <- nrow(wait); sdv <- sd(wait$wait_days)
cat(sprintf("Analyzable wait-time n = %d (rural %d / urban %d = %.1f%% rural)\n", tot, r, u, 100*r/tot))
cat(sprintf("Realized wait SD = %.0f days\n", sdv))
za<-qnorm(.975); zb<-qnorm(.90); ne <- 1/(1/r+1/u)
mde_d <- (za+zb)*sqrt(2/ne); mde_days <- mde_d*sdv
cat(sprintf("Detectable rural-urban gap @ 90%% power (SD=%.0f): ~%.0f days  [plan target: 6 days]\n", sdv, mde_days))
# power to detect the plan's 8-day gap and 6-day gap at realized n/SD
pw <- function(delta){ d0<-delta/sdv; pnorm( sqrt(ne/2)*d0 - za ) }
cat(sprintf("Power to detect the plan's 8-day gap = %.0f%%;  6-day gap = %.0f%%  [plan: >99%%, 90%%]\n",
            100*pw(8), 100*pw(6)))

cat("\n################ MODEL with binary rurality ################\n")
rhs <- "ent_type + ent_per_100k_z + medicaid_fee_index_z + svi_overall_z + dual_pct_z + rural2 + caller_f"
d1 <- d[!is.na(d$offered), ]
m_acc  <- glmmTMB(as.formula(paste("offered ~", rhs, "+ (1|cluster)")), d1, family = binomial)
m_wait <- glmmTMB(as.formula(paste("wait_days ~", rhs, "+ (1|cluster)")), wait, family = nbinom2)
ci <- function(m, term, f){ co<-summary(m)$coefficients$cond; r<-co[term,]
  sprintf("%s = %.2f (95%% CI %.2f-%.2f), p = %.3f", f, exp(r[1]), exp(r[1]-1.96*r[2]), exp(r[1]+1.96*r[2]), r[4]) }
cat("ACCESS (rural vs urban):     ", ci(m_acc,  "rural2Rural", "OR"),  "\n")
cat("TIMELINESS (rural vs urban): ", ci(m_wait, "rural2Rural", "IRR"), "\n")
# marginal median wait by binary rurality
cat("\nMedian business-day wait: urban =", median(wait$wait_days[wait$rural2=="Urban"]),
    " rural =", median(wait$wait_days[wait$rural2=="Rural"]),
    " (difference =", median(wait$wait_days[wait$rural2=="Rural"])-median(wait$wait_days[wait$rural2=="Urban"]), "days)\n")
cat("Mean business-day wait:   urban =", round(mean(wait$wait_days[wait$rural2=="Urban"]),1),
    " rural =", round(mean(wait$wait_days[wait$rural2=="Rural"]),1), "\n")
sink()
cat(readLines("model_output/reanalysis_ruca_binary.txt"), sep="\n")
