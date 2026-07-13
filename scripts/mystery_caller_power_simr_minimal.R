#!/usr/bin/env Rscript
# Mystery caller power analysis - minimal simr example.
# Design: each NPI called twice (BCBS, Medicaid). Poisson wait time.
# Question: power to detect a Medicaid penalty at the chosen NPI count?

suppressPackageStartupMessages({
  library(simr)
  library(lme4)
})
set.seed(2026)

# ---- 1. Design data (200 NPIs x 2 calls = 400 rows) ----
n_npi <- 200
dat <- expand.grid(
  npi       = sprintf("NPI_%04d", seq_len(n_npi)),
  insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
  stringsAsFactors = FALSE
)

# ---- 2. Truth we want to detect ----
#   BCBS mean wait     = 14 business days
#   Medicaid mean wait = 17 days (a 3-day penalty)
#   Physician random intercept SD = 0.30 on log scale
baseline   <- 14
mde_days   <- 3
fix_effs   <- c("(Intercept)"       = log(baseline),
                "insuranceMedicaid" = log((baseline + mde_days) / baseline))
rand_var   <- list(0.30^2)

# ---- 3. Build the seed model ----
mod <- simr::makeGlmer(
  formula = y ~ insurance + (1 | npi),
  family  = poisson,
  fixef   = fix_effs,
  VarCorr = rand_var,
  data    = dat
)

# ---- 4. Power at the seed NPI count ----
ps <- simr::powerSim(
  mod,
  test     = simr::fixed("insuranceMedicaid", method = "z"),
  nsim     = 50,
  progress = FALSE,
  alpha    = 0.05
)

cat("Detecting a", mde_days, "day Medicaid penalty with", n_npi, "NPIs:\n")
print(summary(ps))

# ---- 5. Power curve: how does power change as we shrink the NPI count? ----
# powerCurve resamples NPI subsets from the seed model. To go ABOVE the seed
# size, you would extend() first; here we are sweeping down to find the cutoff.
pc <- simr::powerCurve(
  mod,
  test     = simr::fixed("insuranceMedicaid", method = "z"),
  along    = "npi",
  breaks   = c(20, 40, 60, 80, 100, 150, 200),
  nsim     = 50,
  progress = FALSE,
  alpha    = 0.05
)

cat("\nPower curve along NPI count:\n")
print(summary(pc))
