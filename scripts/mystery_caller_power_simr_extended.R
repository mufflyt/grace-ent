#!/usr/bin/env Rscript
# Mystery caller power analysis - extended simr example.
# Builds on mystery_caller_power_simr_minimal.R with three additions:
#   (a) overdispersion sensitivity via observation-level random effect (OLRE)
#   (b) tighter NPI breaks (10-60) to pin down the main-effect cutoff
#   (c) rural x insurance interaction power
#
# Same truth as minimal:
#   BCBS mean 14 days, Medicaid 17 days (3-day penalty),
#   physician random SD 0.30 on log scale.
# Interaction truth: rural urban gap is the same for BCBS but Medicaid
# adds an extra 3-day penalty in rural areas.

suppressPackageStartupMessages({
  library(simr)
  library(lme4)
})
set.seed(2026)

baseline   <- 14
mde_days   <- 3
rural_eff  <- 4   # rural baseline is 18 days
interact   <- 3   # rural Medicaid is 24 days (18 + 3 mde + 3 interaction)

# =============================================================================
# (a) + (b): main-effect models with and without OLRE, tight break grid
# =============================================================================
n_seed <- 200
dat_main <- expand.grid(
  npi       = sprintf("NPI_%04d", seq_len(n_seed)),
  insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
  stringsAsFactors = FALSE
)
dat_main$obs_id <- sprintf("OBS_%05d", seq_len(nrow(dat_main)))

fix_main <- c("(Intercept)"       = log(baseline),
              "insuranceMedicaid" = log((baseline + mde_days) / baseline))

mod_poisson <- simr::makeGlmer(
  formula = y ~ insurance + (1 | npi),
  family  = poisson, fixef = fix_main,
  VarCorr = list(0.30^2), data = dat_main
)

mod_olre <- simr::makeGlmer(
  formula = y ~ insurance + (1 | npi) + (1 | obs_id),
  family  = poisson, fixef = fix_main,
  VarCorr = list(0.30^2, 0.40^2),   # OLRE SD = 0.40 (moderate overdispersion)
  data    = dat_main
)

tight_breaks <- c(10, 15, 20, 25, 30, 40, 50, 60)

cat("\n[a/b] Power curve - Poisson, no overdispersion:\n")
pc_pois <- simr::powerCurve(
  mod_poisson,
  test = simr::fixed("insuranceMedicaid", method = "z"),
  along = "npi", breaks = tight_breaks,
  nsim = 50, progress = FALSE, alpha = 0.05
)
print(summary(pc_pois))

cat("\n[a/b] Power curve - Poisson + OLRE (overdispersion sigma = 0.40):\n")
pc_olre <- simr::powerCurve(
  mod_olre,
  test = simr::fixed("insuranceMedicaid", method = "z"),
  along = "npi", breaks = tight_breaks,
  nsim = 50, progress = FALSE, alpha = 0.05
)
print(summary(pc_olre))

# =============================================================================
# (c) Rural x Insurance interaction
# =============================================================================
# rural is assigned at the NPI level (paired calls share rural status).
n_seed_int <- 3000
npi_ids <- sprintf("NPI_%05d", seq_len(n_seed_int))
rural_per_npi <- rbinom(n_seed_int, 1, 0.15)

dat_int <- expand.grid(
  npi       = npi_ids,
  insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
  stringsAsFactors = FALSE
)
dat_int$rural <- rural_per_npi[match(dat_int$npi, npi_ids)]

# Cell means:    BCBS-urban=14, Med-urban=17, BCBS-rural=18, Med-rural=24
fix_int <- c(
  "(Intercept)"             = log(14),
  "insuranceMedicaid"       = log(17 / 14),
  "rural"                   = log(18 / 14),
  "insuranceMedicaid:rural" = log(24) - log(14) - log(17 / 14) - log(18 / 14)
)

mod_int <- simr::makeGlmer(
  formula = y ~ insurance * rural + (1 | npi),
  family  = poisson, fixef = fix_int,
  VarCorr = list(0.30^2), data = dat_int
)

int_breaks <- c(200, 500, 1000, 1500, 2000, 3000)

cat("\n[c] Power curve - rural x insurance interaction:\n")
pc_int <- simr::powerCurve(
  mod_int,
  test = simr::fixed("insuranceMedicaid:rural", method = "z"),
  along = "npi", breaks = int_breaks,
  nsim = 50, progress = FALSE, alpha = 0.05
)
print(summary(pc_int))
