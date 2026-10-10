#' @title Mystery Caller Power Analysis Core
#'
#' @description
#' Core statistical functions for executing power analyses related to the
#' mystery caller study design.
#'
#' @details
#' Supports simulating wait times using Negative Binomial GLMMs with
#' post-stratification weighting. Used by scripts and Shiny apps to evaluate
#' sample size requirements.
#'
#' @family testing-framework
#' @name mystery_caller_power_core
NULL

# Core functions for mystery caller power analysis.
# Used by:
#   scripts/mystery_caller_power_NB.R
#   inst/shiny/mystery_caller_power/app.R
#   tests/testthat/test-mystery-caller-power.R

#' Compute the fixed-effect coefficient vector from four cell means.
#' Order matches `model.matrix(~ insurance * rural)` with BCBS, urban as
#' reference.
#' @param bcbs_urban,med_urban,bcbs_rural,med_rural Mean wait days in each cell.
#' @return Named numeric vector with intercept, insuranceMedicaid, rural,
#'   insuranceMedicaid:rural on the log scale.
mc_fixed_effects <- function(bcbs_urban, med_urban, bcbs_rural, med_rural) {
  # Strip names from named-vector subsets like cells["BCBS_urban"] so they do
  # not contaminate the output names.
  bcbs_urban <- unname(bcbs_urban)
  med_urban  <- unname(med_urban)
  bcbs_rural <- unname(bcbs_rural)
  med_rural  <- unname(med_rural)
  stopifnot(bcbs_urban > 0, med_urban > 0, bcbs_rural > 0, med_rural > 0)
  b0      <- log(bcbs_urban)
  b_insur <- log(med_urban)  - b0
  b_rural <- log(bcbs_rural) - b0
  b_int   <- log(med_rural)  - b0 - b_insur - b_rural
  c("(Intercept)"             = b0,
    "insuranceMedicaid"       = b_insur,
    "rural"                   = b_rural,
    "insuranceMedicaid:rural" = b_int)
}

#' True population-marginal Medicaid effect (days) under post-stratification.
mc_marginal_truth <- function(bcbs_urban, med_urban, bcbs_rural, med_rural,
                              pop_rural) {
  bcbs_urban <- unname(bcbs_urban)
  med_urban  <- unname(med_urban)
  bcbs_rural <- unname(bcbs_rural)
  med_rural  <- unname(med_rural)
  pop_rural  <- unname(pop_rural)
  stopifnot(pop_rural >= 0, pop_rural <= 1)
  pop_rural       * (med_rural - bcbs_rural) +
    (1 - pop_rural) * (med_urban - bcbs_urban)
}

#' Build a paired-call design grid with post-stratification weights.
#' Each NPI is called twice (BCBS, Medicaid). rural is fixed per NPI.
#' Post-stratification weights map the sample to the population mix.
mc_build_design <- function(n_npi, rural_sampling, pop_rural) {
  stopifnot(n_npi >= 2, rural_sampling > 0, rural_sampling < 1,
            pop_rural   > 0, pop_rural   < 1)
  npi_ids <- sprintf("NPI_%05d", seq_len(n_npi))
  rural_assign <- stats::rbinom(n_npi, 1, rural_sampling)
  dat <- expand.grid(
    npi       = npi_ids,
    insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
    stringsAsFactors = FALSE
  )
  dat$rural <- rural_assign[match(dat$npi, npi_ids)]
  dat$popwt <- ifelse(dat$rural == 1,
                      pop_rural / rural_sampling,
                      (1 - pop_rural) / (1 - rural_sampling))
  dat
}

#' Simulate y from the NB2 GLMM data-generating process.
#' Var(Y | mu) = mu + mu^2 / phi. Random NPI intercepts u_i ~ N(0, sigma_npi^2).
mc_simulate_y <- function(dat, fix_int, sigma_npi, phi) {
  X <- stats::model.matrix(~ insurance * rural, data = dat)
  eta_fixed <- as.numeric(X %*% fix_int[colnames(X)])
  npis <- unique(dat$npi)
  u_npi <- stats::setNames(stats::rnorm(length(npis), 0, sigma_npi), npis)
  mu <- exp(eta_fixed + u_npi[dat$npi])
  stats::rnbinom(nrow(dat), mu = mu, size = phi)
}

#' Fit a single Poisson or Negative Binomial GLMM to a simulated dataset.
#' @keywords internal
mc_fit_glmm <- function(dat, family_obj) {
  suppressWarnings(tryCatch(
    glmmTMB::glmmTMB(y ~ insurance * rural + (1 | npi),
                     family = family_obj, data = dat),
    error = function(e) NULL
  ))
}

#' Fit a count GLMM and return power flags plus the population-marginal
#' estimate. fit_family is one of:
#'   "negbin"  - always fit Negative Binomial (NB2). Safe default.
#'   "poisson" - always fit Poisson. Use only if dispersion is known to be ~1.
#'   "auto"    - try Poisson first. If the Pearson dispersion ratio exceeds
#'               disp_threshold, refit with Negative Binomial. Reports the
#'               family actually used.
mc_fit_one <- function(dat, alpha = 0.05, fit_family = c("negbin", "poisson", "auto"),
                       disp_threshold = 1.5) {
  fit_family <- match.arg(fit_family)
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("glmmTMB is required.")
  }
  if (!requireNamespace("marginaleffects", quietly = TRUE)) {
    stop("marginaleffects is required.")
  }

  family_used <- NA_character_
  disp_ratio  <- NA_real_
  fit         <- NULL

  if (fit_family == "negbin") {
    fit <- mc_fit_glmm(dat, glmmTMB::nbinom2)
    family_used <- "negbin"
  } else if (fit_family == "poisson") {
    fit <- mc_fit_glmm(dat, stats::poisson)
    family_used <- "poisson"
  } else { # auto
    fit_pois <- mc_fit_glmm(dat, stats::poisson)
    if (is.null(fit_pois)) {
      fit <- mc_fit_glmm(dat, glmmTMB::nbinom2)
      family_used <- "negbin (Poisson failed)"
    } else {
      disp_ratio <- mc_pearson_dispersion(fit_pois)
      if (!is.finite(disp_ratio) || disp_ratio > disp_threshold) {
        fit <- mc_fit_glmm(dat, glmmTMB::nbinom2)
        family_used <- "negbin (auto, dispersion > threshold)"
      } else {
        fit <- fit_pois
        family_used <- "poisson (auto, dispersion <= threshold)"
      }
    }
  }

  if (is.null(fit)) {
    return(list(
      cond_p       = NA_real_,
      marg_unw_p   = NA_real_,
      marg_pop_p   = NA_real_,
      marg_unw_est = NA_real_,
      marg_pop_est = NA_real_,
      converged    = FALSE,
      family_used  = family_used,
      disp_ratio   = disp_ratio
    ))
  }

  cf       <- summary(fit)$coefficients$cond
  cond_p   <- cf["insuranceMedicaid:rural", "Pr(>|z|)"]

  # re.form = NA marginalizes over the population of NPIs (sets u_i = 0 for
  # SE computation). This is what we want for a population-marginal estimand
  # and also silences the marginaleffects warning about RE uncertainty.
  me_unw <- tryCatch(
    suppressMessages(marginaleffects::avg_comparisons(
      fit, variables = "insurance", type = "response", re.form = NA)),
    error = function(e) NULL)
  me_pop <- tryCatch(
    suppressMessages(marginaleffects::avg_comparisons(
      fit, variables = "insurance", wts = "popwt",
      type = "response", re.form = NA)),
    error = function(e) NULL)

  list(
    cond_p       = cond_p,
    marg_unw_p   = if (is.null(me_unw)) NA_real_ else me_unw$p.value[1],
    marg_pop_p   = if (is.null(me_pop)) NA_real_ else me_pop$p.value[1],
    marg_unw_est = if (is.null(me_unw)) NA_real_ else me_unw$estimate[1],
    marg_pop_est = if (is.null(me_pop)) NA_real_ else me_pop$estimate[1],
    converged    = TRUE,
    family_used  = family_used,
    disp_ratio   = disp_ratio
  )
}

#' One Monte Carlo replicate: simulate then fit.
mc_sim_one <- function(dat, fix_int, sigma_npi, phi, alpha = 0.05,
                       fit_family = "negbin", disp_threshold = 1.5) {
  dat$y <- mc_simulate_y(dat, fix_int, sigma_npi, phi)
  mc_fit_one(dat, alpha = alpha, fit_family = fit_family,
             disp_threshold = disp_threshold)
}

#' Run the Monte Carlo power simulation at a single sample size.
mc_run_power_at_n <- function(n_npi, cells, sigma_npi, phi,
                              rural_sampling, pop_rural,
                              alpha = 0.05, n_sim = 30,
                              fit_family = "negbin",
                              disp_threshold = 1.5,
                              progress_callback = NULL) {
  fix_int <- mc_fixed_effects(cells["BCBS_urban"], cells["Med_urban"],
                              cells["BCBS_rural"], cells["Med_rural"])
  dat <- mc_build_design(n_npi, rural_sampling, pop_rural)

  rows <- vector("list", n_sim)
  for (i in seq_len(n_sim)) {
    rows[[i]] <- mc_sim_one(dat, fix_int, sigma_npi, phi, alpha,
                            fit_family = fit_family,
                            disp_threshold = disp_threshold)
    if (!is.null(progress_callback)) progress_callback(i, n_sim)
  }
  flat <- do.call(rbind, lapply(rows, function(r) {
    data.frame(cond_p = r$cond_p, marg_unw_p = r$marg_unw_p,
               marg_pop_p = r$marg_pop_p,
               marg_unw_est = r$marg_unw_est,
               marg_pop_est = r$marg_pop_est,
               converged = r$converged,
               family_used = r$family_used,
               disp_ratio = r$disp_ratio,
               stringsAsFactors = FALSE)
  }))

  power_safe <- function(p) mean(p < alpha, na.rm = TRUE)
  # Summarise which family each replicate actually used. Useful for auto mode.
  fam_tab <- table(flat$family_used)
  fam_summary <- if (length(fam_tab) > 0L) {
    paste(sprintf("%s: %d", names(fam_tab), as.integer(fam_tab)),
          collapse = "; ")
  } else NA_character_
  pct_fell_back_to_nb <- mean(grepl("^negbin", flat$family_used), na.rm = TRUE)

  data.frame(
    n_npi                 = n_npi,
    n_calls               = n_npi * 2L,
    pow_cond_interact     = power_safe(flat$cond_p),
    pow_marg_unwtd        = power_safe(flat$marg_unw_p),
    pow_marg_popwtd       = power_safe(flat$marg_pop_p),
    mean_marg_unw_est     = mean(flat$marg_unw_est, na.rm = TRUE),
    mean_marg_pop_est     = mean(flat$marg_pop_est, na.rm = TRUE),
    convergence_rate      = mean(flat$converged),
    family_summary        = fam_summary,
    pct_fell_back_to_nb   = pct_fell_back_to_nb,
    median_disp_ratio     = stats::median(flat$disp_ratio, na.rm = TRUE),
    n_sim                 = n_sim,
    stringsAsFactors      = FALSE
  )
}

#' Pearson dispersion ratio for a fitted Poisson or NB GLMM.
#' phi_hat > 1.5 in a Poisson fit signals overdispersion -> use NB.
mc_pearson_dispersion <- function(fit) {
  rdf <- stats::df.residual(fit)
  pearson <- sum(stats::residuals(fit, type = "pearson")^2)
  pearson / rdf
}


# =============================================================================
# Rural vs urban single-call design (Grace's planned ENT study)
# =============================================================================
# Each physician is called ONCE. The contrast is rural vs urban (between-
# physician), not insurance arm and not subspecialty. Two outcomes:
#   1. Whether the office offered any appointment (binary)         -> logistic GLM
#   2. Business days until appointment, given one was offered (NB) -> NB GLM
# No random effect is needed because each physician contributes one row.

#' Build a single-call rural-vs-urban design grid.
#' @param n_total Total physicians called.
#' @param rural_frac Fraction of physicians that are rural.
mc_ru_build_design <- function(n_total, rural_frac = 0.50) {
  stopifnot(n_total >= 4, rural_frac > 0, rural_frac < 1)
  data.frame(
    npi   = sprintf("NPI_%05d", seq_len(n_total)),
    rural = stats::rbinom(n_total, 1, rural_frac),
    stringsAsFactors = FALSE
  )
}

#' Simulate appointment-offered and wait-time outcomes for a single-call
#' rural-vs-urban study.
#' @param offer_urban Probability an urban physician offers any appointment.
#' @param offer_rural Probability a rural physician offers any appointment.
#' @param wait_urban Mean wait (days) if offered, urban.
#' @param wait_rural Mean wait (days) if offered, rural.
#' @param phi NB2 dispersion (Var = mu + mu^2 / phi).
mc_ru_simulate <- function(dat, offer_urban, offer_rural,
                           wait_urban, wait_rural, phi) {
  # Per-physician offer probability and wait mean
  p_offer <- ifelse(dat$rural == 1, offer_rural, offer_urban)
  mu_wait <- ifelse(dat$rural == 1, wait_rural, wait_urban)

  # Step 1: binary outcome
  dat$offered <- stats::rbinom(nrow(dat), 1, p_offer)
  # Step 2: wait time, only if offered
  dat$wait_days <- NA_integer_
  has_offer <- dat$offered == 1
  dat$wait_days[has_offer] <- stats::rnbinom(
    sum(has_offer), mu = mu_wait[has_offer], size = phi
  )
  dat
}

#' Fit both models and return p-values + estimates.
mc_ru_fit_one <- function(dat) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("glmmTMB is required.")
  }
  # Logistic regression for offered yes/no, on the full sample
  fit_offer <- suppressWarnings(tryCatch(
    stats::glm(offered ~ rural, family = stats::binomial("logit"), data = dat),
    error = function(e) NULL
  ))
  # NB regression for wait days, on physicians with an offer
  with_offer <- subset(dat, offered == 1 & !is.na(wait_days))
  fit_wait <- if (nrow(with_offer) >= 6) suppressWarnings(tryCatch(
    glmmTMB::glmmTMB(wait_days ~ rural, family = glmmTMB::nbinom2,
                     data = with_offer),
    error = function(e) NULL
  )) else NULL

  pull_p <- function(fit, term, source = c("glm", "glmmTMB")) {
    if (is.null(fit)) return(c(est = NA_real_, p = NA_real_))
    if (match.arg(source) == "glmmTMB") {
      cf <- summary(fit)$coefficients$cond
    } else {
      cf <- summary(fit)$coefficients
    }
    if (!term %in% rownames(cf)) return(c(est = NA_real_, p = NA_real_))
    c(est = cf[term, "Estimate"], p = cf[term, "Pr(>|z|)"])
  }
  o <- pull_p(fit_offer, "rural", "glm")
  w <- pull_p(fit_wait,  "rural", "glmmTMB")
  list(
    offer_est = o[["est"]], offer_p = o[["p"]],
    wait_est  = w[["est"]], wait_p  = w[["p"]],
    n_offered = sum(dat$offered == 1, na.rm = TRUE),
    converged = !is.null(fit_offer) && !is.null(fit_wait)
  )
}

#' Adjusted-model variant: simulates the full analysis model Grace will fit,
#' including a state-level random intercept and an optional subspecialty
#' fixed effect plus a configurable number of adjustment covariates (gender,
#' degree, group size, fellowship, board cert, years-in-practice, day of week,
#' central appointment, transfers, locations, hold time). Replaces the +20%
#' state-clustering rule of thumb with a direct simulation under the
#' analysis model. Slower than the simple wait-only function (each fit is a
#' full GLMM), so use moderate n_sim and a coarser grid.
mc_ru_run_power_adjusted <- function(n_total, rural_frac,
                                     wait_urban, wait_rural, phi,
                                     state_icc      = 0.05,
                                     n_states       = 51,
                                     n_subspecs     = 7,
                                     n_extra_covars = 6,
                                     alpha = 0.05, n_sim = 25,
                                     progress_callback = NULL) {
  stopifnot(n_total >= 10, rural_frac > 0, rural_frac < 1,
            state_icc >= 0, state_icc < 0.9,
            n_states >= 2, n_subspecs >= 1, n_extra_covars >= 0,
            wait_urban > 0, wait_rural > 0, phi > 0)

  # ICC -> state random-intercept SD on log scale. Map ICC to a sigma_state
  # such that ICC ~ sigma_state^2 / (sigma_state^2 + 1/phi). For the pooled
  # ENT defaults (phi=1.7), the 1/phi residual variance is ~0.59, so:
  #   ICC = 0.05 -> sigma_state ~ 0.176
  #   ICC = 0.10 -> sigma_state ~ 0.256
  #   ICC = 0.20 -> sigma_state ~ 0.384
  sigma_state <- sqrt(state_icc / (1 - state_icc) * (1 / phi))

  # Pre-build design template (fixed across replicates so power reflects
  # outcome variability, not design variability).
  rural   <- stats::rbinom(n_total, 1, rural_frac)
  state   <- sample.int(n_states, n_total, replace = TRUE)
  subspec <- if (n_subspecs >= 2) sample.int(n_subspecs, n_total, replace = TRUE)
             else rep(1L, n_total)
  extra   <- if (n_extra_covars > 0) {
    matrix(stats::rbinom(n_total * n_extra_covars, 1, 0.5),
           nrow = n_total)
  } else NULL

  # Subspecialty fixed-effect coefficients on log scale (drawn once so each
  # replicate uses the same subspecialty truth). SD = 0.25 -> subspec means
  # span roughly half-fold to two-fold around the baseline.
  subspec_eff <- if (n_subspecs >= 2) c(0, stats::rnorm(n_subspecs - 1, 0, 0.25))
                 else 0
  # Adjustment covariate effects are small but nonzero
  extra_eff <- if (n_extra_covars > 0) stats::rnorm(n_extra_covars, 0, 0.08)
               else numeric(0)

  one_rep <- function() {
    u_state <- stats::rnorm(n_states, 0, sigma_state)
    eta <- log(wait_urban) +
           ifelse(rural == 1, log(wait_rural / wait_urban), 0) +
           subspec_eff[subspec] +
           u_state[state]
    if (!is.null(extra)) {
      eta <- eta + as.numeric(extra %*% extra_eff)
    }
    mu <- exp(eta)
    y  <- stats::rnbinom(n_total, mu = mu, size = phi)

    dat <- data.frame(
      y       = y,
      rural   = rural,
      state   = factor(state),
      subspec = factor(subspec)
    )
    if (!is.null(extra)) {
      for (k in seq_len(n_extra_covars)) {
        dat[[paste0("x", k)]] <- extra[, k]
      }
    }

    rhs <- "rural + (1 | state)"
    if (n_subspecs >= 2) rhs <- paste("rural + subspec + (1 | state)")
    if (n_extra_covars > 0) {
      extras_str <- paste(paste0("x", seq_len(n_extra_covars)), collapse = " + ")
      rhs <- if (n_subspecs >= 2) {
        sprintf("rural + subspec + %s + (1 | state)", extras_str)
      } else {
        sprintf("rural + %s + (1 | state)", extras_str)
      }
    }
    form <- stats::as.formula(paste("y ~", rhs))

    fit <- suppressWarnings(tryCatch(
      glmmTMB::glmmTMB(form, family = glmmTMB::nbinom2, data = dat),
      error = function(e) NULL
    ))
    if (is.null(fit)) return(c(est = NA_real_, se = NA_real_,
                               p = NA_real_, ok = FALSE))
    cf <- summary(fit)$coefficients$cond
    if (!"rural" %in% rownames(cf)) return(c(est = NA_real_, se = NA_real_,
                                              p = NA_real_, ok = TRUE))
    c(est = cf["rural", "Estimate"],
      se  = cf["rural", "Std. Error"],
      p   = cf["rural", "Pr(>|z|)"],
      ok  = TRUE)
  }

  rows <- vector("list", n_sim)
  for (i in seq_len(n_sim)) {
    rows[[i]] <- one_rep()
    if (!is.null(progress_callback)) progress_callback(i, n_sim)
  }
  flat <- do.call(rbind, rows)
  data.frame(
    n_total           = n_total,
    n_rural           = round(n_total * rural_frac),
    n_urban           = n_total - round(n_total * rural_frac),
    pow_wait          = mean(flat[, "p"] < alpha, na.rm = TRUE),
    mean_log_effect   = mean(flat[, "est"], na.rm = TRUE),
    mean_se_rural     = mean(flat[, "se"],  na.rm = TRUE),
    median_se_rural   = stats::median(flat[, "se"], na.rm = TRUE),
    convergence_rate  = mean(as.logical(flat[, "ok"])),
    state_icc         = state_icc,
    sigma_state       = sigma_state,
    n_states          = n_states,
    n_subspecs        = n_subspecs,
    n_extra_covars    = n_extra_covars,
    n_sim             = n_sim
  )
}

#' Type I error self-check: re-run the same simulator with rural and urban
#' wait times set EQUAL (i.e., the null is true). Under correct model
#' specification the rejection rate should land at alpha ~= 0.05 within
#' Monte Carlo noise. Returns a tibble with rejection rate, 95% CI, and a
#' verdict flag.
mc_ru_type_i_check <- function(n_total, rural_frac,
                               wait_baseline, phi,
                               alpha = 0.05, n_sim = 100,
                               use_adjusted = FALSE,
                               state_icc = 0.08, n_subspecs = 7,
                               n_extra_covars = 6,
                               progress_callback = NULL) {
  fn <- if (use_adjusted) mc_ru_run_power_adjusted else mc_ru_run_power_wait_only
  args <- list(
    n_total    = n_total,
    rural_frac = rural_frac,
    wait_urban = wait_baseline,
    wait_rural = wait_baseline,  # null: no effect
    phi        = phi,
    alpha      = alpha,
    n_sim      = n_sim,
    progress_callback = progress_callback
  )
  if (use_adjusted) {
    args$state_icc      <- state_icc
    args$n_subspecs     <- n_subspecs
    args$n_extra_covars <- n_extra_covars
  }
  out <- do.call(fn, args)
  k <- round(out$pow_wait * n_sim)
  ci <- stats::binom.test(k, n_sim)$conf.int
  # Verdict: is nominal alpha plausibly in the observed CI? If yes, the
  # simulation is well-calibrated. If alpha is OUTSIDE the CI we flag it.
  verdict <- if (alpha < ci[1]) "high (model is anti-conservative)"
             else if (alpha > ci[2]) "low (model is conservative; could be fine)"
             else "consistent with nominal alpha"
  data.frame(
    rejection_rate = out$pow_wait,
    ci_low         = ci[1],
    ci_high        = ci[2],
    n_sim          = n_sim,
    verdict        = verdict,
    alpha_in_ci    = (alpha >= ci[1] && alpha <= ci[2])
  )
}

#' Binary search for the minimum detectable rural-vs-urban gap at a fixed
#' total physician count and target power. Returns the smallest wait_rural
#' (in days, with sub-day precision) at which power is >= target.
mc_ru_find_mde <- function(n_total, rural_frac,
                           wait_urban, phi,
                           target_power = 0.90, alpha = 0.05, n_sim = 40,
                           use_adjusted = FALSE,
                           state_icc = 0.08, n_subspecs = 7,
                           n_extra_covars = 6,
                           tol_days = 0.5, max_iter = 12,
                           progress_callback = NULL) {
  fn <- if (use_adjusted) mc_ru_run_power_adjusted else mc_ru_run_power_wait_only
  one_pow <- function(wait_rural) {
    args <- list(
      n_total    = n_total,
      rural_frac = rural_frac,
      wait_urban = wait_urban,
      wait_rural = wait_rural,
      phi        = phi,
      alpha      = alpha,
      n_sim      = n_sim
    )
    if (use_adjusted) {
      args$state_icc      <- state_icc
      args$n_subspecs     <- n_subspecs
      args$n_extra_covars <- n_extra_covars
    }
    do.call(fn, args)$pow_wait
  }
  # Bracket: lower bound is wait_urban + 0.5 day, upper bound is 3x urban
  lo <- wait_urban + 0.5
  hi <- wait_urban * 3
  hi_pow <- one_pow(hi)
  if (is.na(hi_pow) || hi_pow < target_power) {
    return(list(mde_days = NA_real_, reached = FALSE,
                upper_power = hi_pow, message = "Target power not reached even at 3x urban wait."))
  }
  lo_pow <- one_pow(lo)
  if (!is.na(lo_pow) && lo_pow >= target_power) {
    return(list(mde_days = 0.5, reached = TRUE,
                upper_power = lo_pow,
                message = "Even a 0.5-day gap is detectable at this N."))
  }
  # Binary search
  iter <- 0L
  while ((hi - lo) > tol_days && iter < max_iter) {
    iter <- iter + 1L
    mid <- (lo + hi) / 2
    pw <- one_pow(mid)
    if (!is.null(progress_callback))
      progress_callback(iter, max_iter, mid, pw)
    if (is.na(pw) || pw < target_power) {
      lo <- mid
    } else {
      hi <- mid
    }
  }
  list(mde_days = round(hi - wait_urban, 2),
       reached = TRUE,
       message = sprintf("Converged in %d iterations.", iter))
}

#' Wait-time-only version: every called physician is assumed to offer an
#' appointment (Phase 1 phone validation + commercial insurance). The model
#' is a Negative Binomial GLM on wait days; rural is the lone covariate.
mc_ru_run_power_wait_only <- function(n_total, rural_frac,
                                      wait_urban, wait_rural, phi,
                                      alpha = 0.05, n_sim = 30,
                                      progress_callback = NULL) {
  stopifnot(n_total >= 4, rural_frac > 0, rural_frac < 1,
            wait_urban > 0, wait_rural > 0, phi > 0)
  dat0 <- mc_ru_build_design(n_total, rural_frac)

  one_rep <- function() {
    mu <- ifelse(dat0$rural == 1, wait_rural, wait_urban)
    dat0$wait_days <- stats::rnbinom(nrow(dat0), mu = mu, size = phi)
    fit <- suppressWarnings(tryCatch(
      glmmTMB::glmmTMB(wait_days ~ rural, family = glmmTMB::nbinom2, data = dat0),
      error = function(e) NULL
    ))
    if (is.null(fit)) return(c(est = NA_real_, se = NA_real_,
                               p = NA_real_, ok = FALSE))
    cf <- summary(fit)$coefficients$cond
    if (!"rural" %in% rownames(cf)) return(c(est = NA_real_, se = NA_real_,
                                              p = NA_real_, ok = TRUE))
    c(est = cf["rural", "Estimate"],
      se  = cf["rural", "Std. Error"],
      p   = cf["rural", "Pr(>|z|)"],
      ok  = TRUE)
  }

  rows <- vector("list", n_sim)
  for (i in seq_len(n_sim)) {
    rows[[i]] <- one_rep()
    if (!is.null(progress_callback)) progress_callback(i, n_sim)
  }
  flat <- do.call(rbind, rows)
  data.frame(
    n_total           = n_total,
    n_rural           = round(n_total * rural_frac),
    n_urban           = n_total - round(n_total * rural_frac),
    pow_wait          = mean(flat[, "p"] < alpha, na.rm = TRUE),
    mean_log_effect   = mean(flat[, "est"], na.rm = TRUE),
    mean_se_rural     = mean(flat[, "se"],  na.rm = TRUE),
    median_se_rural   = stats::median(flat[, "se"], na.rm = TRUE),
    convergence_rate  = mean(as.logical(flat[, "ok"])),
    n_sim             = n_sim
  )
}

#' Run a Monte Carlo power simulation for the rural-vs-urban single-call
#' design at one sample size.
mc_ru_run_power_at_n <- function(n_total, rural_frac,
                                 offer_urban, offer_rural,
                                 wait_urban, wait_rural, phi,
                                 alpha = 0.05, n_sim = 30,
                                 progress_callback = NULL) {
  dat0 <- mc_ru_build_design(n_total, rural_frac)
  rows <- vector("list", n_sim)
  for (i in seq_len(n_sim)) {
    dat <- mc_ru_simulate(dat0, offer_urban, offer_rural,
                          wait_urban, wait_rural, phi)
    rows[[i]] <- mc_ru_fit_one(dat)
    if (!is.null(progress_callback)) progress_callback(i, n_sim)
  }
  flat <- do.call(rbind, lapply(rows, function(r) {
    data.frame(offer_est = r$offer_est, offer_p = r$offer_p,
               wait_est  = r$wait_est,  wait_p  = r$wait_p,
               n_offered = r$n_offered, converged = r$converged)
  }))
  pow_safe <- function(p) mean(p < alpha, na.rm = TRUE)
  data.frame(
    n_total          = n_total,
    n_rural          = round(n_total * rural_frac),
    n_urban          = n_total - round(n_total * rural_frac),
    pow_offer        = pow_safe(flat$offer_p),
    pow_wait         = pow_safe(flat$wait_p),
    mean_offered     = mean(flat$n_offered),
    convergence_rate = mean(flat$converged),
    n_sim            = n_sim
  )
}
