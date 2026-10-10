#!/usr/bin/env Rscript
# Power for two estimands under a 50/50 stratified design, ASSUMING
# overdispersion is present in every sample.
#   1. Conditional interaction (rural x insurance), from the GLMM coefficient
#   2. Population-marginal Medicaid effect, post-stratified to 15% rural
# Same cell-mean truth as the prior interaction script.
#
# Data-generating model:
#   y ~ Poisson(exp(eta))
#   eta = beta0 + beta_insur*insurance + beta_rural*rural +
#         beta_int*(insurance:rural) + u_npi + u_obs
#   u_npi ~ N(0, 0.30^2)        physician-level variance
#   u_obs ~ N(0, 0.40^2)        observation-level variance  (overdispersion)
#
# Fitting strategy:
#   FIT_A (naive Poisson + NPI only)         -> diagnostic for dispersion
#   FIT_B (Poisson + NPI + OLRE, matches DGP) -> the inference we report
# Both are fit on every simulation so we can see what would happen if the
# analyst ignored overdispersion vs. modeled it.

suppressPackageStartupMessages({
  library(lme4)
  library(marginaleffects)
})
set.seed(2026)

# -----------------------------------------------------------------------------
# Truth (cell means in business days), random-effect SDs, sampling design
# -----------------------------------------------------------------------------
cells <- c(BCBS_urban = 14, Med_urban = 17, BCBS_rural = 18, Med_rural = 24)
POP_RURAL      <- 0.15      # population share of rural Otolaryngologys
SD_NPI         <- 0.30      # log-scale physician random intercept
SD_OBS         <- 0.40      # log-scale observation random intercept (overdispersion)
ALPHA          <- 0.05
NSIM           <- 25        # sims per grid point (modest; expect ~15-25 min runtime)
NPI_GRID       <- c(200, 400, 800)
RURAL_SAMPLING <- 0.50      # 50/50 stratified design

fix_int <- c(
  "(Intercept)"             = log(cells["BCBS_urban"]),
  "insuranceMedicaid"       = log(cells["Med_urban"]   / cells["BCBS_urban"]),
  "rural"                   = log(cells["BCBS_rural"]  / cells["BCBS_urban"]),
  "insuranceMedicaid:rural" = log(cells["Med_rural"])  - log(cells["BCBS_urban"]) -
                              log(cells["Med_urban"]   / cells["BCBS_urban"]) -
                              log(cells["BCBS_rural"]  / cells["BCBS_urban"])
)
names(fix_int) <- c("(Intercept)", "insuranceMedicaid", "rural",
                    "insuranceMedicaid:rural")

true_marg_med <- POP_RURAL       * (cells["Med_rural"] - cells["BCBS_rural"]) +
                 (1 - POP_RURAL) * (cells["Med_urban"] - cells["BCBS_urban"])
cat(sprintf("True marginal Medicaid effect (pop-weighted, %g%% rural): %.2f days\n",
            POP_RURAL * 100, true_marg_med))
cat(sprintf("Unweighted 50/50 average would be: %.2f days\n\n",
            0.5 * (cells["Med_rural"] - cells["BCBS_rural"]) +
            0.5 * (cells["Med_urban"] - cells["BCBS_urban"])))

# -----------------------------------------------------------------------------
# Build a design grid (NPI x insurance) with population weights
# -----------------------------------------------------------------------------
build_design <- function(n_npi, rural_frac, pop_rural = POP_RURAL) {
  npi_ids <- sprintf("NPI_%05d", seq_len(n_npi))
  rural_assign <- rbinom(n_npi, 1, rural_frac)
  dat <- expand.grid(
    npi       = npi_ids,
    insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
    stringsAsFactors = FALSE
  )
  dat$rural  <- rural_assign[match(dat$npi, npi_ids)]
  dat$obs_id <- sprintf("OBS_%07d", seq_len(nrow(dat)))
  dat$popwt  <- ifelse(dat$rural == 1,
                       pop_rural / rural_frac,
                       (1 - pop_rural) / (1 - rural_frac))
  dat
}

# -----------------------------------------------------------------------------
# Simulate one outcome vector from the overdispersed Poisson DGP
# -----------------------------------------------------------------------------
simulate_y <- function(dat) {
  X <- model.matrix(~ insurance * rural, data = dat)
  eta_fixed <- as.numeric(X %*% fix_int[colnames(X)])
  u_npi <- stats::setNames(rnorm(length(unique(dat$npi)), 0, SD_NPI),
                           unique(dat$npi))
  u_obs <- rnorm(nrow(dat), 0, SD_OBS)
  lambda <- exp(eta_fixed + u_npi[dat$npi] + u_obs)
  rpois(nrow(dat), lambda)
}

# -----------------------------------------------------------------------------
# One iteration: fit both models, return power flags + dispersion diagnostic
# -----------------------------------------------------------------------------
fit_naive   <- function(dat) lme4::glmer(
  y ~ insurance * rural + (1 | npi), family = poisson(), data = dat,
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
fit_olre    <- function(dat) lme4::glmer(
  y ~ insurance * rural + (1 | npi) + (1 | obs_id), family = poisson(), data = dat,
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
dispersion  <- function(fit) {
  rdf <- stats::df.residual(fit)
  pearson <- sum(stats::residuals(fit, type = "pearson")^2)
  pearson / rdf
}

sim_one <- function(dat) {
  dat$y <- simulate_y(dat)

  fitA <- tryCatch(fit_naive(dat), error = function(e) NULL,
                   warning = function(w) suppressWarnings(fit_naive(dat)))
  fitB <- tryCatch(fit_olre(dat),  error = function(e) NULL,
                   warning = function(w) suppressWarnings(fit_olre(dat)))
  if (is.null(fitA) || is.null(fitB)) {
    return(c(cond_naive = NA, cond_olre = NA,
             marg_unw = NA,   marg_pop = NA,
             disp_naive = NA, disp_olre = NA))
  }

  p_cond_A <- summary(fitA)$coefficients["insuranceMedicaid:rural", "Pr(>|z|)"]
  p_cond_B <- summary(fitB)$coefficients["insuranceMedicaid:rural", "Pr(>|z|)"]

  me_unw <- tryCatch(
    marginaleffects::avg_comparisons(fitB, variables = "insurance",
                                     type = "response"),
    error = function(e) NULL)
  me_pop <- tryCatch(
    marginaleffects::avg_comparisons(fitB, variables = "insurance",
                                     wts = "popwt", type = "response"),
    error = function(e) NULL)

  c(
    cond_naive  = as.integer(p_cond_A < ALPHA),
    cond_olre   = as.integer(p_cond_B < ALPHA),
    marg_unw    = if (is.null(me_unw)) NA else as.integer(me_unw$p.value[1] < ALPHA),
    marg_pop    = if (is.null(me_pop)) NA else as.integer(me_pop$p.value[1] < ALPHA),
    disp_naive  = dispersion(fitA),
    disp_olre   = dispersion(fitB)
  )
}

# -----------------------------------------------------------------------------
# Run grid
# -----------------------------------------------------------------------------
run_grid_one <- function(n_npi, rural_frac, n_sim = NSIM) {
  dat <- build_design(n_npi, rural_frac)
  cat("  N =", n_npi, "(", n_sim, "sims) ... ")
  t0 <- Sys.time()
  M <- replicate(n_sim, sim_one(dat))
  cat(sprintf("done (%.0fs)\n", as.numeric(Sys.time() - t0, units = "secs")))

  power_row <- function(v) mean(v, na.rm = TRUE)
  data.frame(
    n_npi              = n_npi,
    n_calls            = n_npi * 2L,
    pow_cond_naive     = power_row(M["cond_naive", ]),
    pow_cond_olre      = power_row(M["cond_olre", ]),
    pow_marg_unw       = power_row(M["marg_unw", ]),
    pow_marg_pop       = power_row(M["marg_pop", ]),
    disp_naive_median  = median(M["disp_naive", ], na.rm = TRUE),
    disp_olre_median   = median(M["disp_olre", ],  na.rm = TRUE),
    n_sim              = n_sim
  )
}

cat("Running 50/50 stratified design with overdispersed DGP...\n")
grid <- do.call(rbind, lapply(NPI_GRID, run_grid_one, rural_frac = RURAL_SAMPLING))

cat("\n", strrep("=", 78), "\n", sep = "")
cat("POWER UNDER OVERDISPERSED DGP, 50/50 STRATIFIED SAMPLE\n")
cat("  pow_cond_naive    : Wald rural:insur, fit ignores overdispersion (anti-conservative)\n")
cat("  pow_cond_olre     : Wald rural:insur, fit includes OLRE (correctly specified)\n")
cat("  pow_marg_unw      : marginal Medicaid effect from raw 50/50 sample (biased high)\n")
cat("  pow_marg_pop      : marginal Medicaid effect post-stratified to 15% rural\n")
cat("  disp_naive_median : Pearson dispersion of the NAIVE fit (signals NB need if > 1.5)\n")
cat("  disp_olre_median  : Pearson dispersion of the OLRE fit (should be ~ 1)\n")
cat(strrep("=", 78), "\n", sep = "")
print(grid, row.names = FALSE, digits = 3)
