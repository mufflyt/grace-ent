# Adversarial invariants for the audited scripts:
#  05_database_accuracy_paper.R, 07_publication_figures.R,
#  08_six_maps.R, missingness_access.R
#
# Bug classes pinned:
#  1. datasets->overlap_zone mapping must fail closed: an unrecognised label
#     that silently maps to NA would drop the row's stratum and let it fall
#     through to the weight-1 fallback as if it were an unmatched record.
#  2. The Euler zone file contains 247 rows with a BLANK npi (ENTHealth-only
#     listings without an NPI). Bare left joins by npi are safe today only
#     because no call record has a blank npi - one future blank key would
#     multiply that row 247x. Blank-keyed rows must be filtered before joining.
#  3. 07's population_tbl hardcodes Euler zone counts; a regenerated Euler
#     file would silently stale every weighted figure unless the script
#     validates counts at runtime.
#  4. missingness_access.R's comp filter must not inject phantom NA rows when
#     ent_type contains NA (NA in a logical index yields NA rows, not FALSE).

library(testthat)

test_that("datasets drift produces NA zone that must be caught, not silently weighted", {
  # Replicates the case_when mapping from 05/07 with an adversarial label.
  zone_of <- function(datasets) {
    dplyr::case_when(
      datasets %in% c("ABO, NPI but not ENT Health",
                      "ABO, NPI but no ENTHealth") ~ "NPPES+ABOto",
      datasets == "ABO, NPI, ENT Health"            ~ "Triple match",
      TRUE                                          ~ NA_character_
    )
  }
  expect_equal(zone_of("ABO, NPI, ENT Health"), "Triple match")
  expect_true(is.na(zone_of("ABO only, ENT Health"))) # drift -> NA, must error upstream
})

test_that("both scripts fail closed on unmapped datasets labels", {
  for (f in c("../../scripts/05_database_accuracy_paper.R",
              "../../scripts/07_publication_figures.R")) {
    src <- readLines(test_path(f), warn = FALSE)
    expect_true(any(grepl("unrecognised datasets values", src)),
                info = paste(f, "must error on unmapped datasets labels"))
  }
})

test_that("blank-npi Euler rows cannot multiply call records", {
  # The real Euler file hazard: 247 rows share npi_chr == "" (ENTHealth-only).
  # Replicate the join with an adversarial blank-keyed call row.
  euler <- data.frame(npi = c("111", "222", "", ""), zone = 1:4)
  euler <- euler |> dplyr::filter(!is.na(npi) & nzchar(trimws(npi)))
  calls <- data.frame(npi = c("111", ""))  # one legit, one adversarial blank
  joined <- dplyr::left_join(calls, euler, by = "npi")
  # The blank call row must NOT multiply against the 247 blank euler rows.
  expect_equal(nrow(joined), 2)
  expect_true(is.na(joined$zone[2]))
})

test_that("05 and 08 filter blank-npi Euler rows before joining", {
  for (f in c("../../scripts/05_database_accuracy_paper.R",
              "../../scripts/08_six_maps.R")) {
    src <- readLines(test_path(f), warn = FALSE)
    expect_true(any(grepl("nzchar\\(trimws\\(npi\\)", src)),
                info = paste(f, "must filter blank npi keys from the Euler join"))
  }
})

test_that("07 validates hardcoded population counts against the Euler file", {
  src <- readLines(test_path("../../scripts/07_publication_figures.R"), warn = FALSE)
  expect_true(any(grepl("euler_zone_counts", src)),
              info = "stale hardcoded denominators must fail loud")
  expect_true(any(grepl("population_tbl hardcodes", src)))
})

test_that("NA ent_type cannot inject phantom rows into the analytic sample", {
  d <- data.frame(
    complete_f = c(TRUE, TRUE, TRUE),
    ent_type  = c("General", NA, "Pediatrics"),
    stringsAsFactors = FALSE
  )
  # Old fragile pattern: NA in the index yields NA rows, not exclusion.
  old_comp <- d[d$complete_f & d$ent_type != "", ]
  expect_true(any(is.na(old_comp$ent_type))) # demonstrates the hazard
  # New guarded pattern (as fixed in missingness_access.R).
  new_comp <- d[d$complete_f & !is.na(d$ent_type) & d$ent_type != "", ]
  expect_equal(nrow(new_comp), 2)
  expect_false(any(is.na(new_comp$ent_type)))
})

test_that("missingness_access.R source uses the guarded index", {
  src <- readLines(test_path("../../scripts/missingness_access.R"), warn = FALSE)
  comp_line <- grep("comp <- d\\[", src, value = TRUE)
  expect_true(any(grepl("!is.na\\(d\\$ent_type\\)", comp_line)))
})

test_that("manuscript_outputs.R guards NA in its analytic-sample index", {
  src <- readLines(test_path("../../scripts/manuscript_outputs.R"), warn = FALSE)
  expect_true(any(grepl("!is.na\\(d\\$ent_type\\)", src)))
  expect_true(any(grepl("!is.na\\(d\\$complete\\)", src)))
})

test_that("NA appointment_offered cannot poison Table 1 or STROBE counts", {
  # Replicates the fixed construction: one NA must not reach sum()/mean().
  ao <- c("TRUE", "FALSE", NA)
  offered <- !is.na(ao) & ao == "TRUE"
  expect_equal(sum(offered), 1)
  expect_equal(mean(offered), 1/3)
  # The fragile pattern this replaces:
  fragile <- ao == "TRUE"
  expect_true(is.na(sum(fragile))) # demonstrates the poisoning hazard
})

test_that("manuscript_outputs.R source uses the NA-safe offered construction", {
  src <- readLines(test_path("../../scripts/manuscript_outputs.R"), warn = FALSE)
  expect_true(any(grepl("!is.na\\(comp\\$appointment_offered\\)", src)))
})

test_that("latest committed call-log intermediate has the columns 05 requires", {
  # 05_database_accuracy_paper.R filters on ruca_binary; the July intermediates
  # predated that column and broke the script. Pin the drift: the newest
  # ent_phase2_all file must carry every column 05 touches.
  files <- list.files(test_path("../../data/processed"),
                      pattern = "^ent_phase2_all_.*\\.csv$", full.names = TRUE)
  latest <- sort(files)[length(files)]
  hdr <- names(read.csv(latest, nrows = 1, check.names = FALSE))
  for (col in c("ruca_binary", "datasets", "appointment_offered",
                "wait_days_business")) {
    expect_true(col %in% hdr,
                info = paste("latest intermediate missing column:", col))
  }
})

test_that("no script depends on a machine-specific load_all path", {
  # strobe_flow.R was fixed earlier for exactly this; six more scripts still
  # loaded dev checkouts via devtools::load_all("~/...") which fails on any
  # other machine and silently uses a different package version than installed.
  for (f in list.files(test_path("../../scripts"), pattern = "\\.R$", full.names = TRUE)) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_false(grepl('load_all\\("~/', src),
                 info = paste(basename(f), "must not load_all a home-dir checkout"))
  }
})

test_that("cluster construction is prefixed in every model script", {
  for (f in c("sensitivity_analyses.R", "reanalyze_ruca_binary.R")) {
    src <- paste(readLines(test_path(paste0("../../scripts/", f)), warn = FALSE), collapse = "\n")
    expect_true(grepl("cbsa_", src), info = paste(f, "must prefix cbsa cluster ids"))
    expect_true(grepl("cty_", src), info = paste(f, "must prefix county cluster ids"))
  }
})

test_that("reanalyze cluster fallback handles NA codes (nzchar(NA) is TRUE)", {
  src <- paste(readLines(test_path("../../scripts/reanalyze_ruca_binary.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("is.na\\(d\\$cbsa_code\\)", src),
              info = "nzchar(NA)==TRUE means !nzchar() never falls back for NA codes")
})

test_that("two-sample power formula uses the correct noncentrality", {
  # For groups r and u with effective n = 1/(1/r + 1/u), the z-statistic's
  # noncentrality under a true difference delta/sd is delta*sqrt(ne)/sd.
  # The old code used sqrt(ne/2) - understating power and inflating the MDE
  # by sqrt(2). Pin the correct formula on a worked example.
  r <- 200; u <- 226; ne <- 1/(1/r + 1/u)
  sdv <- 30; delta <- 8; za <- qnorm(.975)
  correct_power <- pnorm(sqrt(ne) * (delta/sdv) - za)
  buggy_power   <- pnorm(sqrt(ne/2) * (delta/sdv) - za)
  expect_gt(correct_power, buggy_power) # the bug understated power
  # The correction is material: the buggy formula loses ~12 points here.
  expect_lt(buggy_power, correct_power - 0.05)
})

test_that("reanalyze_ruca_binary.R source uses the corrected power formulas", {
  src <- paste(readLines(test_path("../../scripts/reanalyze_ruca_binary.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("sqrt\\(ne\\)\\*d0", src))
  expect_false(grepl("sqrt\\(ne/2\\)", src), info = "sqrt(ne/2) understates power by sqrt(2)")
  expect_true(grepl("\\(za\\+zb\\)/sqrt\\(ne\\)", src))
})

test_that("analytic-sample filters across scripts guard NA complete", {
  for (f in c("sensitivity_analyses.R", "reanalyze_ruca_binary.R", "table1_by_subspecialty.R")) {
    src <- paste(readLines(test_path(paste0("../../scripts/", f)), warn = FALSE), collapse = "\n")
    expect_true(grepl("!is.na\\(d\\$complete\\)", src),
                info = paste(f, "must guard NA complete in the sample filter"))
  }
})
