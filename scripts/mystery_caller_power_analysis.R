#!/usr/bin/env Rscript
# =============================================================================
# Mystery Caller Power Analysis
# =============================================================================
# Outcome     : appointment_wait_time (continuous, days)
# Primary     : rural vs urban (two-tailed)
# Interactions: rural x subspecialty x insurance (three-way factorial)
# Alpha       : 0.05
# Power       : 0.90
# MDE         : 3 and 5 days
# SD assumed  : 14 days
#
# Outputs CSVs to artifacts/power_analysis/ and prints a console summary.
# Run: Rscript scripts/mystery_caller_power_analysis.R
# =============================================================================

suppressPackageStartupMessages({
  library(pwr)
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
})

set.seed(2026)

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------
ALPHA          <- 0.05
POWER          <- 0.90
SD_DAYS        <- 14
MDE_DAYS       <- c(3, 5)
RURAL_FRACTION <- 0.15                # natural RUCA distribution
N_SUBSPEC      <- 7                   # subspecialty levels
N_INSURANCE    <- 2                   # Medicaid vs commercial

OUT_DIR <- here::here("artifacts", "power_analysis")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

cohen_d <- function(mde, sd) mde / sd

# -----------------------------------------------------------------------------
# Section 1: Main effect (rural vs urban, two-sample t)
# -----------------------------------------------------------------------------
# Equal allocation: pwr.t.test
# Natural allocation: iterate total N, split by rural fraction, use pwr.t2n.test

solve_unequal_n <- function(d, rural_frac, alpha, power) {
  # Binary search for total N achieving target power with rural_frac allocation
  lo <- 20; hi <- 500000
  while (hi - lo > 1) {
    mid <- (lo + hi) %/% 2
    n_rural <- max(2, round(mid * rural_frac))
    n_urban <- mid - n_rural
    if (n_urban < 2) { lo <- mid; next }
    pw <- tryCatch(
      pwr.t2n.test(n1 = n_rural, n2 = n_urban, d = d,
                   sig.level = alpha, alternative = "two.sided")$power,
      error = function(e) NA_real_
    )
    if (is.na(pw)) { lo <- mid; next }
    if (pw < power) lo <- mid else hi <- mid
  }
  n_total <- hi
  n_rural <- max(2, round(n_total * rural_frac))
  n_urban <- n_total - n_rural
  list(n_total = n_total, n_rural = n_rural, n_urban = n_urban)
}

main_effect_rows <- lapply(MDE_DAYS, function(mde) {
  d <- cohen_d(mde, SD_DAYS)
  eq <- pwr.t.test(d = d, sig.level = ALPHA, power = POWER,
                   type = "two.sample", alternative = "two.sided")
  n_per_group_eq <- ceiling(eq$n)
  uneq <- solve_unequal_n(d, RURAL_FRACTION, ALPHA, POWER)
  tibble::tibble(
    mde_days          = mde,
    cohens_d          = round(d, 3),
    equal_n_per_group = n_per_group_eq,
    equal_total_n     = 2L * n_per_group_eq,
    natural_n_rural   = uneq$n_rural,
    natural_n_urban   = uneq$n_urban,
    natural_total_n   = uneq$n_total
  )
})
main_effect_tbl <- dplyr::bind_rows(main_effect_rows)

# -----------------------------------------------------------------------------
# Section 2: Interaction terms (linear model F-tests via pwr.f2.test)
# -----------------------------------------------------------------------------
# Full model: y ~ rural * subspec * insurance
# Numerator df for each term:
#   rural                          : (2-1)                       = 1
#   subspec                        : (S-1)                       = 6
#   insurance                      : (2-1)                       = 1
#   rural:subspec                  : (2-1)(S-1)                  = 6
#   rural:insurance                : (2-1)(2-1)                  = 1
#   subspec:insurance              : (S-1)(2-1)                  = 6
#   rural:subspec:insurance        : (2-1)(S-1)(2-1)             = 6
#
# Cohen's f^2 benchmarks: small = 0.02, medium = 0.15, large = 0.35
# We report N for small and medium because mystery-caller interactions in
# published audit studies typically fall in the small-to-medium range.

n_params_full <- 2 * N_SUBSPEC * N_INSURANCE  # all cell means in saturated model

interaction_terms <- tibble::tribble(
  ~term,                          ~df_num,
  "rural (main)",                  1L,
  "subspecialty (main)",           N_SUBSPEC - 1L,
  "insurance (main)",              1L,
  "rural x subspecialty",          N_SUBSPEC - 1L,
  "rural x insurance",             1L,
  "subspecialty x insurance",      N_SUBSPEC - 1L,
  "rural x subspec x insurance",   N_SUBSPEC - 1L
)

solve_v <- function(u, f2, alpha, power) {
  res <- pwr.f2.test(u = u, f2 = f2, sig.level = alpha, power = power)
  ceiling(res$v)
}

interaction_tbl <- interaction_terms %>%
  rowwise() %>%
  mutate(
    n_small_f2_0_02  = solve_v(df_num, 0.02, ALPHA, POWER) + n_params_full + 1L,
    n_medium_f2_0_15 = solve_v(df_num, 0.15, ALPHA, POWER) + n_params_full + 1L
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# Section 3: Simulation - three-way ANOVA with explicit MDE pattern
# -----------------------------------------------------------------------------
# Pattern (in days, relative to grand mean):
#   rural main effect     : +MDE (rural slower)
#   subspec main effect   : draw from N(0, MDE/2) per subspec
#   insurance main effect : +MDE/2 (Medicaid slower)
#   rural x insurance     : +MDE/2 additional rural-Medicaid penalty
#   rural x subspec       : draw from N(0, MDE/3) per subspec
#   3-way                 : small, N(0, MDE/4)

simulate_power <- function(n_total, mde, sd_resid, rural_frac, n_subspec, n_sim = 300) {
  n_rural <- max(2, round(n_total * rural_frac))
  n_urban <- n_total - n_rural

  # Pre-build design once
  alloc <- data.frame(
    rural = c(rep(1L, n_rural), rep(0L, n_urban)),
    subspec = sample.int(n_subspec, n_total, replace = TRUE),
    insur = rbinom(n_total, 1, 0.5)
  )
  alloc$subspec <- factor(alloc$subspec)
  alloc$rural <- factor(alloc$rural)
  alloc$insur <- factor(alloc$insur)

  # Deterministic per-subspec effects (drawn once so each n_total uses same truth)
  subspec_main_eff   <- stats::setNames(rnorm(n_subspec, 0, mde / 2),     seq_len(n_subspec))
  rural_x_subspec    <- stats::setNames(rnorm(n_subspec, 0, mde / 3),     seq_len(n_subspec))
  threeway_eff       <- stats::setNames(rnorm(n_subspec, 0, mde / 4),     seq_len(n_subspec))

  true_mean <- with(alloc,
    20 +                                              # grand mean wait (days)
    mde            * (rural == "1") +                # rural main
    subspec_main_eff[as.character(subspec)] +        # subspec main
    (mde / 2)      * (insur == "1") +                # insurance main
    (mde / 2)      * (rural == "1") * (insur == "1") +  # rural x insur
    rural_x_subspec[as.character(subspec)] * (rural == "1") +
    threeway_eff[as.character(subspec)] * (rural == "1") * (insur == "1")
  )

  terms_of_interest <- c("rural", "subspec", "insur",
                         "rural:subspec", "rural:insur", "subspec:insur",
                         "rural:subspec:insur")
  hits <- matrix(0L, nrow = n_sim, ncol = length(terms_of_interest),
                 dimnames = list(NULL, terms_of_interest))

  for (i in seq_len(n_sim)) {
    y <- true_mean + rnorm(n_total, 0, sd_resid)
    fit <- stats::lm(y ~ rural * subspec * insur, data = alloc)
    av  <- stats::anova(fit)
    pvals <- av[["Pr(>F)"]]
    names(pvals) <- rownames(av)
    for (tm in terms_of_interest) {
      if (tm %in% names(pvals) && !is.na(pvals[tm])) {
        hits[i, tm] <- as.integer(pvals[tm] < ALPHA)
      }
    }
  }
  colMeans(hits)
}

run_sim_grid <- function(mde, sd_resid, rural_frac, n_subspec, n_grid, n_sim) {
  rows <- lapply(n_grid, function(N) {
    pw <- simulate_power(N, mde, sd_resid, rural_frac, n_subspec, n_sim)
    tibble::tibble(
      n_total    = N,
      term       = names(pw),
      power      = unname(pw),
      mde_days   = mde,
      rural_frac = rural_frac
    )
  })
  dplyr::bind_rows(rows)
}

cat("\n[sim] running simulation grid (this takes ~1-2 min)...\n")

n_grid_natural <- c(500, 1000, 2000, 3000, 5000, 7500, 10000)
n_grid_equal   <- c(200, 400, 800, 1500, 2500, 4000, 6000)

sim_results <- dplyr::bind_rows(
  run_sim_grid(mde = 3, sd_resid = SD_DAYS, rural_frac = RURAL_FRACTION,
               n_subspec = N_SUBSPEC, n_grid = n_grid_natural, n_sim = 300) %>%
    dplyr::mutate(scenario = "natural_15pct_rural"),
  run_sim_grid(mde = 5, sd_resid = SD_DAYS, rural_frac = RURAL_FRACTION,
               n_subspec = N_SUBSPEC, n_grid = n_grid_natural, n_sim = 300) %>%
    dplyr::mutate(scenario = "natural_15pct_rural"),
  run_sim_grid(mde = 3, sd_resid = SD_DAYS, rural_frac = 0.50,
               n_subspec = N_SUBSPEC, n_grid = n_grid_equal, n_sim = 300) %>%
    dplyr::mutate(scenario = "equal_50_50"),
  run_sim_grid(mde = 5, sd_resid = SD_DAYS, rural_frac = 0.50,
               n_subspec = N_SUBSPEC, n_grid = n_grid_equal, n_sim = 300) %>%
    dplyr::mutate(scenario = "equal_50_50")
)

# Minimum N achieving 90% power per (scenario, mde, term)
min_n_for_90 <- sim_results %>%
  dplyr::filter(power >= POWER) %>%
  dplyr::group_by(scenario, mde_days, term) %>%
  dplyr::summarise(min_n_for_90pct_power = min(n_total), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = term, values_from = min_n_for_90pct_power)

# -----------------------------------------------------------------------------
# Write outputs
# -----------------------------------------------------------------------------
main_effect_csv     <- file.path(OUT_DIR, "main_effect_rural_vs_urban.csv")
main_effect_csv_tmp <- paste0(main_effect_csv, ".tmp")
readr::write_csv(main_effect_tbl, main_effect_csv_tmp)
file.rename(main_effect_csv_tmp, main_effect_csv)

interaction_csv     <- file.path(OUT_DIR, "interaction_terms_analytical.csv")
interaction_csv_tmp <- paste0(interaction_csv, ".tmp")
readr::write_csv(interaction_tbl, interaction_csv_tmp)
file.rename(interaction_csv_tmp, interaction_csv)

sim_results_csv     <- file.path(OUT_DIR, "simulation_power_curve.csv")
sim_results_csv_tmp <- paste0(sim_results_csv, ".tmp")
readr::write_csv(sim_results, sim_results_csv_tmp)
file.rename(sim_results_csv_tmp, sim_results_csv)

min_n_csv     <- file.path(OUT_DIR, "min_n_for_90pct_power.csv")
min_n_csv_tmp <- paste0(min_n_csv, ".tmp")
readr::write_csv(min_n_for_90, min_n_csv_tmp)
file.rename(min_n_csv_tmp, min_n_csv)

# -----------------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n", sep = "")
cat("MYSTERY CALLER POWER ANALYSIS  (alpha = 0.05, power = 0.90, SD = 14 days)\n")
cat(strrep("=", 78), "\n\n", sep = "")

cat("SECTION 1: MAIN EFFECT (rural vs urban, two-sample t, two-tailed)\n")
cat(strrep("-", 78), "\n", sep = "")
print(as.data.frame(main_effect_tbl), row.names = FALSE)

cat("\nSECTION 2: INTERACTION TERMS (analytical, pwr.f2.test, Cohen's f^2)\n")
cat("  small f^2  = 0.02   medium f^2 = 0.15\n")
cat("  N reported = total observations needed for 90% power\n")
cat(strrep("-", 78), "\n", sep = "")
print(as.data.frame(interaction_tbl), row.names = FALSE)

cat("\nSECTION 3: SIMULATION (min N for 90% power, MDE pattern in script header)\n")
cat(strrep("-", 78), "\n", sep = "")
print(as.data.frame(min_n_for_90), row.names = FALSE)

cat("\nOutputs written to:\n  ", OUT_DIR, "\n", sep = "")
cat("Files:\n")
cat("  - main_effect_rural_vs_urban.csv\n")
cat("  - interaction_terms_analytical.csv\n")
cat("  - simulation_power_curve.csv\n")
cat("  - min_n_for_90pct_power.csv\n")
