#!/usr/bin/env Rscript
# =============================================================================
# Run the full grace-ent test suite. Invoke from the repository root:
#   Rscript tests/run_tests.R        (or:  Rscript run_all.R test)
#
# Covers:
#   tests/testthat/*                 model outputs, access cascade, sensitivity,
#                                    data corrections, output-artifact presence
#   scripts/test_enriched_covariates.R   geographic-covariate join validation
# Exits non-zero if any test fails.
# =============================================================================

suppressPackageStartupMessages(library(testthat))

cat("== grace-ent test suite ==\n\n")

ok <- TRUE

cat("-- tests/testthat --\n")
res <- test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
df  <- as.data.frame(res)
if (sum(df$failed) > 0 || any(df$error)) ok <- FALSE

cat("\n-- scripts/test_enriched_covariates.R --\n")
enriched_ok <- tryCatch({ source("scripts/test_enriched_covariates.R"); TRUE },
                        error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); FALSE })
if (!enriched_ok) ok <- FALSE

cat("\n== ", if (ok) "ALL TESTS PASSED" else "TEST FAILURES PRESENT", " ==\n", sep = "")
quit(status = if (ok) 0 else 1)
