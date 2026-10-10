#!/usr/bin/env Rscript
# Rural x insurance interaction power: 50/50 rural-urban allocation
# vs natural 15% rural. Same cell-mean truth as the extended script:
#   BCBS-urban  = 14 days,  Medicaid-urban  = 17
#   BCBS-rural  = 18 days,  Medicaid-rural  = 24 (extra 3-day rural Medicaid penalty)
# NPI random SD = 0.30 (log scale), paired calls per NPI.

suppressPackageStartupMessages({
  library(simr)
  library(lme4)
})
set.seed(2026)

fix_int <- c(
  "(Intercept)"             = log(14),
  "insuranceMedicaid"       = log(17 / 14),
  "rural"                   = log(18 / 14),
  "insuranceMedicaid:rural" = log(24) - log(14) - log(17 / 14) - log(18 / 14)
)

build_int_model <- function(n_npi, rural_frac) {
  npi_ids <- sprintf("NPI_%05d", seq_len(n_npi))
  rural_per_npi <- rbinom(n_npi, 1, rural_frac)
  dat <- expand.grid(
    npi       = npi_ids,
    insurance = factor(c("BCBS", "Medicaid"), levels = c("BCBS", "Medicaid")),
    stringsAsFactors = FALSE
  )
  dat$rural <- rural_per_npi[match(dat$npi, npi_ids)]
  simr::makeGlmer(
    formula = y ~ insurance * rural + (1 | npi),
    family  = poisson, fixef = fix_int,
    VarCorr = list(0.30^2), data = dat
  )
}

run_curve <- function(rural_frac, n_seed, breaks, label) {
  mod <- build_int_model(n_seed, rural_frac)
  cat("\n[", label, "] ", rural_frac * 100, "% rural, seed N = ", n_seed, "\n", sep = "")
  pc <- simr::powerCurve(
    mod,
    test     = simr::fixed("insuranceMedicaid:rural", method = "z"),
    along    = "npi", breaks = breaks,
    nsim     = 50, progress = FALSE, alpha = 0.05
  )
  s <- summary(pc)
  data.frame(allocation = label, n_npi = s$nlevels, n_calls = s$nlevels * 2L,
             power = round(s$mean, 3), ci_low = round(s$lower, 3),
             ci_high = round(s$upper, 3))
}

res_5050    <- run_curve(0.50, 1000, c(100, 200, 300, 500, 750, 1000), "50/50")
res_natural <- run_curve(0.15, 3000, c(200, 500, 1000, 1500, 2000, 3000), "natural 15%")

cat("\n", strrep("=", 60), "\n", sep = "")
cat("RURAL x INSURANCE INTERACTION POWER  (90% target)\n")
cat(strrep("=", 60), "\n", sep = "")
print(rbind(res_5050, res_natural), row.names = FALSE)
