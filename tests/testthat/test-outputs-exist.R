# Every manuscript-referenced output artifact is produced and non-empty.

test_that("model tables, main figures, and supplementary artifacts exist", {
  files <- c(
    # model outputs
    "model_output/part1_access_OR.csv",
    "model_output/part2_wait_IRR.csv",
    "model_output/table1_characteristics.csv",
    # main figures
    "model_output/forest_access_timeliness.png",
    "model_output/strobe_flow_mysterycall.png",
    # supplementary tables
    "model_output/supp/S8_sensitivity_analyses.csv",
    "model_output/supp/S10_access_cascade.csv",
    # reviewer-response tables (global test, dual outcome, callers, clustering)
    "model_output/supp/S11_clustering_sensitivity.csv",
    "model_output/supp/S12_caller_effects.csv",
    "model_output/supp/S12b_leave_one_caller_out.csv",
    "model_output/supp/S12c_caller_balance.csv",
    "model_output/supp/S13_dual_access_outcomes.csv",
    "model_output/supp/S14_global_subspecialty_test.csv",
    "model_output/supp/S14b_factor_joint_tests.csv",
    "model_output/supp/S15_design_weighted.csv",
    # supplementary figures
    "model_output/supp/figS1_km_time_to_appointment.png",
    "model_output/supp/figS2_wait_distribution.png",
    "model_output/supp/figS3_nb_diagnostics.png",
    "model_output/supp/figS6_access_cascade.png"
  )
  for (f in files) {
    expect_true(file.exists(P(f)), info = f)
    expect_gt(file.size(P(f)), 0L)
  }
})

test_that("the reproducibility snapshot is present", {
  expect_true(file.exists(P("repro/sessionInfo.txt")))
  expect_true(file.exists(P("repro/package_versions.csv")))
})

test_that("bibliography and reporting checklist are present", {
  expect_true(file.exists(P("manuscript/references.bib")))
  expect_true(file.exists(P("manuscript/american-medical-association.csl")))
  expect_true(file.exists(P("manuscript/STROBE_checklist.md")))
})
