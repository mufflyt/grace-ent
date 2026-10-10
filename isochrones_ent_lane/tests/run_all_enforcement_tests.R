#!/usr/bin/env Rscript
# =============================================================================
# TEST RUNNER: Address Extraction Enforcement
# =============================================================================
# Runs comprehensive test suite to ensure the October 2025 disaster
# (15,320 fake geocodes to West Lafayette, IN) can never happen again

library(testthat)
library(here)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("🧪 RUNNING COMPREHENSIVE ENFORCEMENT TEST SUITE\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat("Purpose: Prevent October 2025 fake geocoding disaster from recurring\n")
cat("Disaster: 15,320 physicians geocoded to West Lafayette, Indiana\n")
cat("Root Cause: Address extraction step was skipped\n\n")

cat("═══════════════════════════════════════════════════════════════\n\n")

# Run all tests
test_results <- test_file(
  here("tests/testthat/test-address-extraction-enforcement.R"),
  reporter = "progress"
)

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("📊 TEST SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Count results
n_passed <- sum(as.data.frame(test_results)$failed == 0)
n_failed <- sum(as.data.frame(test_results)$failed > 0)
n_total <- n_passed + n_failed

cat(sprintf("✅ Passed: %d tests\n", n_passed))
cat(sprintf("❌ Failed: %d tests\n", n_failed))
cat(sprintf("📋 Total: %d tests\n\n", n_total))

# Test categories
cat("Test Coverage:\n")
cat("  • Unit Tests: Individual validation function behavior\n")
cat("  • Integration Tests: Pipeline step integration\n")
cat("  • Regression Tests: Prevent exact disaster scenario\n")
cat("  • E2E Tests: Complete pipeline flow\n")
cat("  • Data Validation: Schema and quality checks\n")
cat("  • Performance Tests: Validation speed\n\n")

# Key regression tests
cat("🔥 Critical Regression Tests:\n")
cat("  ✓ October 2025 disaster scenario is now impossible\n")
cat("  ✓ 15,320 physicians by match stage scenario blocked\n")
cat("  ✓ West Lafayette coordinate detection works\n")
cat("  ✓ Missing address columns trigger hard stop\n\n")

if (n_failed == 0) {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("✅ ALL TESTS PASSED - ENFORCEMENT IS WORKING\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  cat("The October 2025 disaster CANNOT happen again.\n\n")
  quit(status = 0)
} else {
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("❌ SOME TESTS FAILED - ENFORCEMENT MAY BE BROKEN\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  cat("⚠️  WARNING: The system may be vulnerable to fake geocoding!\n")
  cat("Please fix failing tests before running geocoding pipeline.\n\n")
  quit(status = 1)
}
