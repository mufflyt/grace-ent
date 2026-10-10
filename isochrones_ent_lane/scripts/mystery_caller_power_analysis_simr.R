#!/usr/bin/env Rscript
# =============================================================================
# Mystery Caller Power Analysis  -  simr / lme4 (mixed model)
# =============================================================================
# Design:
#   - Each physician (NPI) is called twice in the same window:
#       call 1 = Blue Cross / Blue Shield (BCBS)
#       call 2 = Medicaid
#   - Calls nested within NPI: (1 | npi) random intercept
#   - Optional rural x insurance interaction
#
# Outcome: business_days_until_appointment (count) modeled as Poisson with
#          a physician-level random intercept (and optional observation-level
#          random effect for overdispersion).
#
# Test:    fixed effect of insuranceMedicaid (and the rural:insurance
#          interaction in the extended model).
# Alpha:   0.05    Power target: 0.90
# MDE:     3 and 5 business days
#
# Outputs to artifacts/power_analysis/simr_*.csv
# =============================================================================

suppressPackageStartupMessages({
  library(simr)
  library(lme4)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})

set.seed(2026)

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------
BASELINE_WAIT     <- 14            # mean BCBS, urban (business days)
MDE_DAYS          <- c(3, 5)       # Medicaid penalty to detect
RURAL_EFFECT_DAYS <- 4             # rural baseline penalty (days)
INTERACTION_DAYS  <- 3             # additional rural x Medicaid penalty
NPI_RANDOM_SD     <- 0.30          # log-scale physician intercept SD
OBS_RANDOM_SD     <- 0.20          # log-scale OLRE SD (overdispersion)
RURAL_FRACTION    <- 0.15
ALPHA             <- 0.05
POWER_TARGET      <- 0.90

# Simulation grid
NPI_SEED          <- 200           # NPI count used to build the seed model
NPI_GRID          <- c(100, 200, 300, 500, 750, 1000)
N_SIM_AT_N        <- 100           # sims for the at-N power estimate
N_SIM_CURVE       <- 50            # sims per grid point in powerCurve

OUT_DIR <- here::here("artifacts", "power_analysis")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
build_design <- function(n_npi, rural_frac = RURAL_FRACTION) {
  npi_ids <- sprintf("NPI_%05d", seq_len(n_npi))
  rural_assign <- rbinom(n_npi, 1, rural_frac)
  dat <- expand.grid(
    npi = npi_ids,
    insurance = c("BCBS", "Medicaid"),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  dat$insurance <- factor(dat$insurance, levels = c("BCBS", "Medicaid"))
  dat$rural <- rural_assign[match(dat$npi, npi_ids)]
  dat$obs_id <- sprintf("OBS_%07d", seq_len(nrow(dat)))
  dat
}

# Fixed effects from cell means on the response scale (mean days)
fixed_from_cells <- function(m_bcbs_urb, m_med_urb, m_bcbs_rural, m_med_rural) {
  b0      <- log(m_bcbs_urb)
  b_insur <- log(m_med_urb)    - b0
  b_rural <- log(m_bcbs_rural) - b0
  b_int   <- log(m_med_rural)  - b0 - b_insur - b_rural
  c("(Intercept)"             = b0,
    "insuranceMedicaid"       = b_insur,
    "rural"                   = b_rural,
    "insuranceMedicaid:rural" = b_int)
}

build_seed_model <- function(n_npi, baseline, mde, rural_eff, interact,
                             include_olre = FALSE) {
  dat <- build_design(n_npi)
  fx <- fixed_from_cells(
    m_bcbs_urb   = baseline,
    m_med_urb    = baseline + mde,
    m_bcbs_rural = baseline + rural_eff,
    m_med_rural  = baseline + mde + rural_eff + interact
  )
  if (include_olre) {
    form <- y ~ insurance * rural + (1 | npi) + (1 | obs_id)
    vc   <- list(NPI_RANDOM_SD^2, OBS_RANDOM_SD^2)
  } else {
    form <- y ~ insurance * rural + (1 | npi)
    vc   <- list(NPI_RANDOM_SD^2)
  }
  simr::makeGlmer(
    formula = form, family = poisson, fixef = fx,
    VarCorr = vc, data = dat
  )
}

# -----------------------------------------------------------------------------
# Section A: Power at NPI_SEED for the Medicaid main effect
# -----------------------------------------------------------------------------
cat("\n[A] Power at N =", NPI_SEED, "NPIs for insurance main effect\n")

sectionA <- lapply(MDE_DAYS, function(mde) {
  cat("  MDE =", mde, "days ... ")
  mod <- build_seed_model(NPI_SEED, BASELINE_WAIT, mde,
                          RURAL_EFFECT_DAYS, INTERACTION_DAYS,
                          include_olre = FALSE)
  ps <- simr::powerSim(
    mod, test = simr::fixed("insuranceMedicaid", method = "z"),
    nsim = N_SIM_AT_N, progress = FALSE, alpha = ALPHA
  )
  s <- summary(ps)
  cat(sprintf("power = %.3f (95%% CI %.3f-%.3f, %d sims)\n",
              s$mean, s$lower, s$upper, s$trials))
  tibble::tibble(
    mde_days   = mde,
    n_npi      = NPI_SEED,
    n_calls    = NPI_SEED * 2L,
    term       = "insuranceMedicaid",
    power      = s$mean,
    ci_low     = s$lower,
    ci_high    = s$upper,
    n_sim      = s$trials
  )
})
sectionA_tbl <- dplyr::bind_rows(sectionA)

# -----------------------------------------------------------------------------
# Section B: Power curve along NPI count for insurance main effect
# -----------------------------------------------------------------------------
cat("\n[B] Power curve along NPI for insurance main effect (",
    length(NPI_GRID), "points x", N_SIM_CURVE, "sims each)\n", sep = "")

sectionB <- lapply(MDE_DAYS, function(mde) {
  cat("  MDE =", mde, "days ...\n")
  mod <- build_seed_model(
    max(NPI_GRID), BASELINE_WAIT, mde,
    RURAL_EFFECT_DAYS, INTERACTION_DAYS, include_olre = FALSE
  )
  pc <- simr::powerCurve(
    mod, test = simr::fixed("insuranceMedicaid", method = "z"),
    along = "npi", breaks = NPI_GRID,
    nsim = N_SIM_CURVE, progress = FALSE, alpha = ALPHA
  )
  s <- summary(pc)
  tibble::tibble(
    mde_days = mde,
    n_npi    = s$nlevels,
    n_calls  = s$nlevels * 2L,
    term     = "insuranceMedicaid",
    power    = s$mean,
    ci_low   = s$lower,
    ci_high  = s$upper,
    n_sim    = s$trials
  )
})
sectionB_tbl <- dplyr::bind_rows(sectionB)

# -----------------------------------------------------------------------------
# Section C: Interaction (rural x insurance) power at the largest grid point
# -----------------------------------------------------------------------------
cat("\n[C] Power for rural x insurance interaction at N =", max(NPI_GRID), "NPIs\n")

sectionC <- lapply(MDE_DAYS, function(mde) {
  cat("  MDE =", mde, "days, interaction = ", INTERACTION_DAYS, " days ... ", sep = "")
  mod <- build_seed_model(max(NPI_GRID), BASELINE_WAIT, mde,
                          RURAL_EFFECT_DAYS, INTERACTION_DAYS,
                          include_olre = FALSE)
  ps <- simr::powerSim(
    mod, test = simr::fixed("insuranceMedicaid:rural", method = "z"),
    nsim = N_SIM_AT_N, progress = FALSE, alpha = ALPHA
  )
  s <- summary(ps)
  cat(sprintf("power = %.3f (95%% CI %.3f-%.3f)\n",
              s$mean, s$lower, s$upper))
  tibble::tibble(
    mde_days   = mde,
    n_npi      = max(NPI_GRID),
    n_calls    = max(NPI_GRID) * 2L,
    term       = "insuranceMedicaid:rural",
    power      = s$mean,
    ci_low     = s$lower,
    ci_high    = s$upper,
    n_sim      = s$trials
  )
})
sectionC_tbl <- dplyr::bind_rows(sectionC)

# -----------------------------------------------------------------------------
# Section D: Overdispersion sensitivity (add OLRE) for main effect at NPI_SEED
# -----------------------------------------------------------------------------
cat("\n[D] Overdispersion check: add observation-level random effect (OLRE)\n")

sectionD <- lapply(MDE_DAYS, function(mde) {
  cat("  MDE =", mde, "days with OLRE sd =", OBS_RANDOM_SD, " ... ", sep = "")
  mod <- build_seed_model(NPI_SEED, BASELINE_WAIT, mde,
                          RURAL_EFFECT_DAYS, INTERACTION_DAYS,
                          include_olre = TRUE)
  ps <- simr::powerSim(
    mod, test = simr::fixed("insuranceMedicaid", method = "z"),
    nsim = N_SIM_AT_N, progress = FALSE, alpha = ALPHA
  )
  s <- summary(ps)
  cat(sprintf("power = %.3f (95%% CI %.3f-%.3f)\n",
              s$mean, s$lower, s$upper))
  tibble::tibble(
    mde_days = mde,
    n_npi    = NPI_SEED,
    n_calls  = NPI_SEED * 2L,
    term     = "insuranceMedicaid (OLRE)",
    power    = s$mean,
    ci_low   = s$lower,
    ci_high  = s$upper,
    n_sim    = s$trials
  )
})
sectionD_tbl <- dplyr::bind_rows(sectionD)

# -----------------------------------------------------------------------------
# Write outputs
# -----------------------------------------------------------------------------
readr::write_csv(sectionA_tbl,
                 file.path(OUT_DIR, "simr_main_effect_at_seed_n.csv"))
readr::write_csv(sectionB_tbl,
                 file.path(OUT_DIR, "simr_power_curve_main_effect.csv"))
readr::write_csv(sectionC_tbl,
                 file.path(OUT_DIR, "simr_interaction_power.csv"))
readr::write_csv(sectionD_tbl,
                 file.path(OUT_DIR, "simr_overdispersion_sensitivity.csv"))

# Minimum NPI for 90% power on main effect, from the curve
min_n_main <- sectionB_tbl %>%
  dplyr::filter(power >= POWER_TARGET) %>%
  dplyr::group_by(mde_days) %>%
  dplyr::summarise(min_npi_for_90pct = min(n_npi),
                   min_calls_for_90pct = min(n_calls),
                   .groups = "drop")

readr::write_csv(min_n_main, file.path(OUT_DIR, "simr_min_npi_for_90pct.csv"))

# -----------------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n", sep = "")
cat("MYSTERY CALLER POWER (simr / lme4)  alpha=", ALPHA,
    "  target power=", POWER_TARGET, "\n", sep = "")
cat("Baseline wait =", BASELINE_WAIT, "d  ",
    "NPI random SD =", NPI_RANDOM_SD,
    "  rural effect =", RURAL_EFFECT_DAYS, "d  ",
    "interaction =", INTERACTION_DAYS, "d\n", sep = "")
cat(strrep("=", 78), "\n\n", sep = "")

cat("[A] Insurance main effect at N =", NPI_SEED, "NPIs:\n")
print(as.data.frame(sectionA_tbl), row.names = FALSE)

cat("\n[B] Power curve (insurance main effect):\n")
print(as.data.frame(sectionB_tbl), row.names = FALSE)

cat("\n[C] Rural x insurance interaction at N =", max(NPI_GRID), ":\n")
print(as.data.frame(sectionC_tbl), row.names = FALSE)

cat("\n[D] Overdispersion sensitivity (OLRE added):\n")
print(as.data.frame(sectionD_tbl), row.names = FALSE)

cat("\nMin NPI count for 90% power on insurance main effect:\n")
print(as.data.frame(min_n_main), row.names = FALSE)

cat("\nOutputs:\n  ", OUT_DIR, "\n  - simr_main_effect_at_seed_n.csv",
    "\n  - simr_power_curve_main_effect.csv",
    "\n  - simr_interaction_power.csv",
    "\n  - simr_overdispersion_sensitivity.csv",
    "\n  - simr_min_npi_for_90pct.csv\n", sep = "")
