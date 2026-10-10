# Semantic tests for the mystery caller NB2 GLMM power simulation.
# These prove that:
#   (1) The fixed-effect parameterization reproduces the chosen cell means
#   (2) The NB2 data-generating process produces the correct dispersion ratio
#   (3) Post-stratification weighting recovers the population-marginal effect,
#       while unweighted averaging recovers the (biased) sample-average effect
#   (4) Coefficient recovery from glmmTMB matches truth at large N
#   (5) Type I error rate is near nominal alpha when the true interaction is 0
#   (6) Power is monotone increasing in N

suppressPackageStartupMessages({
  library(testthat)
})

source(here::here("R", "mystery_caller_power_core.R"))

CELLS_DEFAULT <- c(BCBS_urban = 14, Med_urban = 17,
                   BCBS_rural = 18, Med_rural = 24)

# =============================================================================
# (1) Fixed-effect parameterization reproduces cell means
# =============================================================================

test_that("mc_fixed_effects() inverts cell means exactly", {
  fx <- mc_fixed_effects(14, 17, 18, 24)

  # Reconstruct the four expected log-rates from coefficients
  expect_equal(exp(fx["(Intercept)"]),
               14, tolerance = 1e-10, ignore_attr = TRUE,
               info = "BCBS-urban cell")
  expect_equal(exp(fx["(Intercept)"] + fx["insuranceMedicaid"]),
               17, tolerance = 1e-10, ignore_attr = TRUE,
               info = "Medicaid-urban cell")
  expect_equal(exp(fx["(Intercept)"] + fx["rural"]),
               18, tolerance = 1e-10, ignore_attr = TRUE,
               info = "BCBS-rural cell")
  expect_equal(exp(sum(fx)),
               24, tolerance = 1e-10, ignore_attr = TRUE,
               info = "Medicaid-rural cell (all four coefs sum)")
})

test_that("mc_fixed_effects() returns correct order for model.matrix", {
  fx <- mc_fixed_effects(14, 17, 18, 24)
  expect_named(fx, c("(Intercept)", "insuranceMedicaid",
                     "rural", "insuranceMedicaid:rural"))
})

# =============================================================================
# context("(2) NB2 DGP gives the right dispersion ratio")
# =============================================================================

test_that("Empirical dispersion ratio matches the theoretical 1 + mu/phi", {
  # NB2: Var(Y|mu) = mu + mu^2/phi. Dispersion ratio = Var/mu = 1 + mu/phi.
  set.seed(101)
  mu  <- 14
  phi <- 6
  y   <- rnbinom(50000, mu = mu, size = phi)
  emp_var      <- var(y)
  emp_mean     <- mean(y)
  emp_disp     <- emp_var / emp_mean
  theory_disp  <- 1 + mu / phi  # = 1 + 14/6 = 3.33

  expect_equal(emp_disp, theory_disp, tolerance = 0.05,
               info = "Empirical Var/mean should match theoretical 1 + mu/phi")
})

# =============================================================================
# context("(3) Post-stratification weighting recovers the population truth")
# =============================================================================

test_that("Weights are correct: rural gets p/q, urban gets (1-p)/(1-q)", {
  dat <- mc_build_design(n_npi = 5000, rural_sampling = 0.50, pop_rural = 0.15)
  rural_w  <- unique(dat$popwt[dat$rural == 1])
  urban_w  <- unique(dat$popwt[dat$rural == 0])
  expect_equal(rural_w, 0.15 / 0.50, tolerance = 1e-12)   # = 0.30
  expect_equal(urban_w, 0.85 / 0.50, tolerance = 1e-12)   # = 1.70
})

test_that("Mean of popwt over the sample equals 1", {
  # If weights are unbiased, sum(w)/n = 1 in expectation.
  set.seed(202)
  dat <- mc_build_design(n_npi = 10000, rural_sampling = 0.50, pop_rural = 0.15)
  expect_equal(mean(dat$popwt), 1, tolerance = 0.02,
               info = "Weight column average should center at 1")
})

test_that("Pop-weighted sample mean recovers the population-marginal truth", {
  # Pure arithmetic check on the design grid, no GLMM yet.
  set.seed(303)
  cells <- CELLS_DEFAULT
  pop_rural <- 0.15
  truth <- mc_marginal_truth(cells["BCBS_urban"], cells["Med_urban"],
                             cells["BCBS_rural"], cells["Med_rural"],
                             pop_rural)
  expect_equal(unname(truth), unname(0.15 * (24 - 18) + 0.85 * (17 - 14)),
               tolerance = 1e-12)

  # Simulate at huge N from 50/50 sampling, compute weighted mean of
  # (Medicaid_y - BCBS_y) per NPI, weighted by popwt
  dat <- mc_build_design(n_npi = 20000, rural_sampling = 0.50,
                         pop_rural = pop_rural)
  fix_int <- mc_fixed_effects(cells["BCBS_urban"], cells["Med_urban"],
                              cells["BCBS_rural"], cells["Med_rural"])
  dat$y <- mc_simulate_y(dat, fix_int, sigma_npi = 0.30, phi = 6)

  # Per-NPI Medicaid - BCBS difference
  ag <- stats::aggregate(y ~ npi + rural + popwt, data = dat, FUN = function(z) {
    z[1] - z[2]   # BCBS row first, Medicaid row second (factor order)
  })
  ag$diff_medicaid_minus_bcbs <- -ag$y   # Med - BCBS = -(BCBS - Med)
  pop_weighted <- stats::weighted.mean(ag$diff_medicaid_minus_bcbs,
                                       w = ag$popwt)
  unweighted   <- mean(ag$diff_medicaid_minus_bcbs)

  expect_equal(pop_weighted, unname(truth), tolerance = 0.20,
               info = "Pop-weighted sample mean recovers 3.45 days")
  # Unweighted from 50/50 should be biased high toward the 4.5-day midpoint
  expect_gt(unweighted, unname(truth))
  expect_equal(unweighted, 4.5, tolerance = 0.20,
               info = "Unweighted 50/50 sample mean centers at 4.5 days (biased)")
})

# =============================================================================
# context("(4) glmmTMB recovers coefficients at large N")
# =============================================================================

test_that("Fitted NB2 GLMM recovers all four fixed effects within MC tolerance", {
  skip_if_not_installed("glmmTMB")
  set.seed(404)
  cells <- CELLS_DEFAULT
  fix_int <- mc_fixed_effects(cells["BCBS_urban"], cells["Med_urban"],
                              cells["BCBS_rural"], cells["Med_rural"])

  dat <- mc_build_design(n_npi = 1500, rural_sampling = 0.50, pop_rural = 0.15)
  dat$y <- mc_simulate_y(dat, fix_int, sigma_npi = 0.30, phi = 6)

  fit <- glmmTMB::glmmTMB(y ~ insurance * rural + (1 | npi),
                          family = glmmTMB::nbinom2, data = dat)

  # All four coefficients should be within ~3 SE of the truth at N = 1500.
  cf_all <- summary(fit)$coefficients$cond
  for (nm in names(fix_int)) {
    cf <- cf_all[nm, ]
    z <- (cf[["Estimate"]] - fix_int[[nm]]) / cf[["Std. Error"]]
    expect_lt(abs(z), 3,
              label = paste0("|z| for ", nm, " (should be < 3 at N=1500)"))
  }
})

# =============================================================================
# context("(5) Type I error rate when the true interaction is zero")
# =============================================================================

test_that("Wald rejection rate is near alpha=0.05 under the null", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("marginaleffects")
  skip_on_ci()  # 60-90 sec; run locally not in CI

  set.seed(505)
  # NULL: zero log-scale interaction means Med_rural = Med_urban * BCBS_rural / BCBS_urban
  # i.e., 17 * 18 / 14 = 21.857. The log-link makes "no interaction" multiplicative.
  null_cells <- c(BCBS_urban = 14, Med_urban = 17,
                  BCBS_rural = 18, Med_rural = 17 * 18 / 14)
  fix_int_null <- mc_fixed_effects(null_cells["BCBS_urban"],
                                   null_cells["Med_urban"],
                                   null_cells["BCBS_rural"],
                                   null_cells["Med_rural"])
  # Interaction coefficient should be 0 under multiplicative null (within rounding)
  expect_equal(unname(fix_int_null["insuranceMedicaid:rural"]), 0,
               tolerance = 1e-12)

  n_sim <- 100
  dat <- mc_build_design(n_npi = 400, rural_sampling = 0.50, pop_rural = 0.15)
  rejections <- replicate(n_sim, {
    dat$y <- mc_simulate_y(dat, fix_int_null, sigma_npi = 0.30, phi = 6)
    fit <- tryCatch(
      glmmTMB::glmmTMB(y ~ insurance * rural + (1 | npi),
                       family = glmmTMB::nbinom2, data = dat),
      error = function(e) NULL)
    if (is.null(fit)) return(NA)
    cf <- summary(fit)$coefficients$cond
    as.integer(cf["insuranceMedicaid:rural", "Pr(>|z|)"] < 0.05)
  })
  rate <- mean(rejections, na.rm = TRUE)

  # 100 sims: 95% binomial CI around 0.05 is roughly [0.018, 0.107]
  expect_gt(rate, 0.01)
  expect_lt(rate, 0.12)
})

# =============================================================================
# context("(6) Power is monotone in N")
# =============================================================================

test_that("Power for the conditional interaction increases with N", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("marginaleffects")
  skip_on_ci()  # 60-90 sec

  set.seed(606)
  cells <- CELLS_DEFAULT

  res_small <- mc_run_power_at_n(
    n_npi = 200, cells = cells, sigma_npi = 0.30, phi = 6,
    rural_sampling = 0.50, pop_rural = 0.15, n_sim = 20
  )
  res_large <- mc_run_power_at_n(
    n_npi = 1500, cells = cells, sigma_npi = 0.30, phi = 6,
    rural_sampling = 0.50, pop_rural = 0.15, n_sim = 20
  )
  expect_gt(res_large$pow_cond_interact, res_small$pow_cond_interact,
            label = "Power should rise with N")
  expect_gte(res_large$convergence_rate, 0.90,
             label = "Convergence rate >= 90% at large N")
})

# =============================================================================
# context("(7) Design grid has correct shape")
# =============================================================================

test_that("Design grid has 2 rows per NPI and correct rural marginals", {
  set.seed(707)
  n_npi <- 1000
  dat <- mc_build_design(n_npi = n_npi, rural_sampling = 0.50, pop_rural = 0.15)
  expect_equal(nrow(dat), n_npi * 2L)
  expect_equal(length(unique(dat$npi)), n_npi)
  # Each NPI has exactly one BCBS row and one Medicaid row
  tab <- table(dat$npi, dat$insurance)
  expect_true(all(tab == 1))
  # Rural fraction is near 0.50 in the sample (allow Monte Carlo wiggle)
  rural_frac <- mean(dat$rural[!duplicated(dat$npi)])
  expect_equal(rural_frac, 0.50, tolerance = 0.05)
})
