#!/usr/bin/env Rscript
# Mystery caller power analysis - Negative Binomial GLMM with glmmTMB.
# Always assumes overdispersion is present.
#
# Design: each NPI (National Provider Identifier) called twice, once with
# Blue Cross / Blue Shield (BCBS) and once with Medicaid.
# Outcome: business_days_until_appointment (count).
#
# Data-generating model = NB2:
#   y | u_npi ~ NB(mu, phi)   with   E[y] = mu,   Var[y] = mu + mu^2 / phi
#   log(mu) = X*beta + u_npi
#   u_npi ~ N(0, sigma_npi^2)        physician random intercept
#
# Estimands tested per simulation:
#   1. Conditional rural:insurance interaction (Wald z on the coefficient)
#   2. Marginal Medicaid effect, post-stratified to 15% rural population

suppressPackageStartupMessages({
  library(glmmTMB)
  library(marginaleffects)
})
set.seed(2026)

# -----------------------------------------------------------------------------
# Truth on the response (days) scale
# -----------------------------------------------------------------------------
cells <- c(BCBS_urban = 14, Med_urban = 17, BCBS_rural = 18, Med_rural = 24)
POP_RURAL        <- 0.15
RURAL_SAMPLING   <- 0.50
SIGMA_NPI        <- 0.30           # log-scale physician random intercept SD
PHI_NB           <- 6              # NB dispersion; Var = mu + mu^2/phi (smaller = more overdispersed)
ALPHA            <- 0.05
NSIM             <- 30
NPI_GRID         <- c(200, 400, 800, 1500)

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

cat(sprintf("Truth: BCBS-urban=%g, Med-urban=%g, BCBS-rural=%g, Med-rural=%g\n",
            cells["BCBS_urban"], cells["Med_urban"], cells["BCBS_rural"], cells["Med_rural"]))
cat(sprintf("NB dispersion phi = %g  (Var(Y|mu) = mu + mu^2/phi)\n", PHI_NB))
cat(sprintf("Implied dispersion ratio at mu=14: %.2f\n", 1 + 14 / PHI_NB))
cat(sprintf("True marginal Medicaid effect (pop %g%% rural): %.2f days\n\n",
            POP_RURAL * 100, true_marg_med))

# -----------------------------------------------------------------------------
# Design grid + population weights
# -----------------------------------------------------------------------------
build_design <- function(n_npi, rural_frac, pop_rural = POP_RURAL) {
  npi_ids <- sprintf("NPI_%05d", seq_len(n_npi))
  rural_assign <- rbinom(n_npi, 1, rural_frac)
  dat <- expand.grid(
    npi       = npi_ids,
    insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
    stringsAsFactors = FALSE
  )
  dat$rural <- rural_assign[match(dat$npi, npi_ids)]
  dat$popwt <- ifelse(dat$rural == 1,
                      pop_rural / rural_frac,
                      (1 - pop_rural) / (1 - rural_frac))
  dat
}

simulate_y <- function(dat) {
  X <- model.matrix(~ insurance * rural, data = dat)
  eta_fixed <- as.numeric(X %*% fix_int[colnames(X)])
  u_npi <- stats::setNames(rnorm(length(unique(dat$npi)), 0, SIGMA_NPI),
                           unique(dat$npi))
  mu <- exp(eta_fixed + u_npi[dat$npi])
  rnbinom(nrow(dat), mu = mu, size = PHI_NB)
}

# -----------------------------------------------------------------------------
# One iteration: fit NB GLMM and extract conditional + marginal power flags
# -----------------------------------------------------------------------------
sim_one <- function(dat) {
  dat$y <- simulate_y(dat)
  fit <- tryCatch(
    glmmTMB::glmmTMB(y ~ insurance * rural + (1 | npi),
                     family = nbinom2, data = dat),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(c(cond = NA, marg_unw = NA, marg_pop = NA, est_marg_pop = NA))
  }

  cf <- summary(fit)$coefficients$cond
  p_cond <- cf["insuranceMedicaid:rural", "Pr(>|z|)"]

  me_unw <- tryCatch(
    marginaleffects::avg_comparisons(fit, variables = "insurance",
                                     type = "response"),
    error = function(e) NULL)
  me_pop <- tryCatch(
    marginaleffects::avg_comparisons(fit, variables = "insurance",
                                     wts = "popwt", type = "response"),
    error = function(e) NULL)

  c(
    cond         = as.integer(p_cond < ALPHA),
    marg_unw     = if (is.null(me_unw)) NA else as.integer(me_unw$p.value[1] < ALPHA),
    marg_pop     = if (is.null(me_pop)) NA else as.integer(me_pop$p.value[1] < ALPHA),
    est_marg_pop = if (is.null(me_pop)) NA else me_pop$estimate[1]
  )
}

# -----------------------------------------------------------------------------
# Run the grid
# -----------------------------------------------------------------------------
run_one_n <- function(n_npi, rural_frac = RURAL_SAMPLING, n_sim = NSIM) {
  cat("  N =", n_npi, "(", n_sim, "sims) ... "); t0 <- Sys.time()
  dat <- build_design(n_npi, rural_frac)
  M <- replicate(n_sim, sim_one(dat))
  cat(sprintf("done (%.0fs)\n", as.numeric(Sys.time() - t0, units = "secs")))
  data.frame(
    n_npi              = n_npi,
    n_calls            = n_npi * 2L,
    pow_cond_interact  = mean(M["cond", ],     na.rm = TRUE),
    pow_marg_unwtd     = mean(M["marg_unw", ], na.rm = TRUE),
    pow_marg_popwtd    = mean(M["marg_pop", ], na.rm = TRUE),
    mean_marg_pop_est  = mean(M["est_marg_pop", ], na.rm = TRUE),
    n_sim              = n_sim
  )
}

# Suppress the demo grid run when this script is sourced as a backend
# (e.g. by inst/shiny/mystery_caller_power/app.R). Set MCP_BACKEND_ONLY=1
# in the env, or `options(mcp_backend_only = TRUE)` before source().
if (!isTRUE(getOption("mcp_backend_only", FALSE)) &&
    !nzchar(Sys.getenv("MCP_BACKEND_ONLY"))) {
  cat("Running NB power grid (50/50 stratified)...\n")
  res <- do.call(rbind, lapply(NPI_GRID, run_one_n))

  out_csv <- file.path("artifacts", "power_analysis", "nb_power_50_50.csv")
  dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(res, out_csv, row.names = FALSE)

  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("NEGATIVE BINOMIAL GLMM POWER, 50/50 STRATIFIED SAMPLE\n")
  cat(strrep("=", 78), "\n", sep = "")
  print(res, row.names = FALSE, digits = 3)
  cat("\nWrote:", out_csv, "\n")
}
