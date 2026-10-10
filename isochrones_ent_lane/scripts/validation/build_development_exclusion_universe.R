#!/usr/bin/env Rscript
# =============================================================================
# Development-exclusion universe: every NPI that informed b7403fe8
# =============================================================================
#
# WHY THIS RUNS FIRST. An "external validation" whose holdout contains cases the
# consensus was developed on is not external. This builds the exclusion set
# BEFORE any candidate is sampled or adjudicated, so exclusion cannot be
# influenced -- even unconsciously -- by which cases would look good.
#
# TWO VINTAGES, BOTH INCLUDED. The gold standard exists at two sizes: 71 rows on
# main and 221 on slice/retirement-gold-standard-clean (#657), with rejected
# candidates at 29 and 108 respectively. Both materially informed development,
# so both are excluded. Taking only main's would leak 150 expansion cases into
# the holdout.
#
# REJECTED CANDIDATES ARE EXCLUDED TOO. A case examined and rejected during
# development is still a case the developers looked at. Re-using it as
# "independent" evidence would be quietly circular.
#
# Branch blobs are read with `git cat-file blob`, never `git show <ref>:<path>`:
# in zsh the latter is subject to the `$var:` modifier and returns zero bytes,
# which parses cleanly as an empty file and would silently shrink this universe.
# =============================================================================

suppressWarnings(suppressMessages({ ok <- requireNamespace("digest", quietly = TRUE) }))

EXPANSION_REF <- "origin/slice/retirement-gold-standard-clean"

# path, ref (NA = working tree), why it counts as development use
SOURCES <- list(
  list(path = "data/reference/retirement_gold_standard/retirement_gold_standard_v1.csv",
       ref = NA, why = "development gold standard, main vintage"),
  list(path = "data/reference/retirement_gold_standard/candidate_ledger_2026-09-03.csv",
       ref = NA, why = "candidate ledger examined during expansion"),
  list(path = "data/reference/retirement_gold_standard/candidate_ledger_already_present_2026-09-03.csv",
       ref = NA, why = "candidate ledger, already-present arm"),
  list(path = "data/reference/retirement_gold_standard/candidate_ledger_round2_additions.csv",
       ref = NA, why = "candidate ledger round 2"),
  list(path = "data/reference/retirement_gold_standard/candidate_ledger_round3_additions.csv",
       ref = NA, why = "candidate ledger round 3"),
  list(path = "data/reference/retirement_candidates/physician_deaths_obituary_2026-09-02.csv",
       ref = NA, why = "obituary-derived retirement candidates"),
  list(path = "data/reference/retirement_candidates/retirement_candidates_web_academic_2026-09-02.csv",
       ref = NA, why = "web/academic retirement candidates"),
  list(path = "tests/fixtures/negative_gold_standard_npi.csv",
       ref = NA, why = "negative controls used in development"),
  list(path = "tests/testthat/fixtures/retirement_gold_standard.csv",
       ref = NA, why = "gold standard test fixture"),
  list(path = "tests/testthat/fixtures/retirement_gold_standard_named_subspecialists.csv",
       ref = NA, why = "named-subspecialist fixture, main vintage"),
  list(path = "tests/testthat/fixtures/retirement_gold_standard_rejected_candidates.csv",
       ref = NA, why = "rejected candidates, main vintage"),
  list(path = "data/reference/retirement_gold_standard/retirement_gold_standard_v1.csv",
       ref = EXPANSION_REF, why = "development gold standard, EXPANSION vintage (222 rows)"),
  list(path = "tests/testthat/fixtures/retirement_gold_standard_named_subspecialists.csv",
       ref = EXPANSION_REF, why = "named-subspecialist fixture, EXPANSION vintage"),
  list(path = "tests/testthat/fixtures/retirement_gold_standard_rejected_candidates.csv",
       ref = EXPANSION_REF, why = "rejected candidates, EXPANSION vintage"),
  # DECLARED column: this file calls the identifier `provider_id`, not `npi`.
  # The heuristic extractor found 0 NPIs here and would have leaked 165
  # development-adjudicated cases into the "independent" holdout.
  list(path = "scripts/urps_exit_hazard/urps_candidate_adjudication.csv",
       ref = EXPANSION_REF, why = "URPS exit-hazard candidate adjudication",
       npi_columns = "provider_id")
)

read_source <- function(path, ref) {
  if (is.na(ref)) {
    if (!file.exists(path)) return(NULL)
    return(utils::read.csv(path, colClasses = "character", check.names = FALSE))
  }
  txt <- suppressWarnings(system2("git", c("cat-file", "blob", paste0(ref, ":", path)),
                                  stdout = TRUE, stderr = FALSE))
  if (!length(txt)) return(NULL)
  utils::read.csv(text = paste(txt, collapse = "\n"), colClasses = "character",
                  check.names = FALSE)
}

# An NPI is 10 digits. Anything else in an npi-named column is not an identity
# and must not silently become one.
extract_npis <- function(df, declared = NULL) {
  if (is.null(df)) return(character(0))
  cn <- if (!is.null(declared)) intersect(declared, names(df)) else
    names(df)[grepl("^npi$|_npi$|^npi_", names(df), ignore.case = TRUE)]
  if (!length(cn)) return(character(0))
  v <- unlist(lapply(cn, function(c) as.character(df[[c]])), use.names = FALSE)
  v <- trimws(v)
  v <- v[grepl("^[0-9]{10}$", v)]
  unique(v)
}

main <- function() {
  rows <- list(); all_npis <- character(0)
  for (s in SOURCES) {
    df <- read_source(s$path, s$ref)
    npis <- extract_npis(df, s$npi_columns)
    # FAIL CLOSED ON SILENT UNDER-EXCLUSION. A source that yields no NPIs but
    # visibly contains 10-digit tokens has an identifier column this extractor
    # did not recognise -- exactly how `provider_id` hid 165 development cases.
    # Fixing the class, not the instance: any future source with a differently
    # named identifier stops the build instead of quietly shrinking the universe.
    if (!is.null(df) && length(npis) == 0L) {
      flat <- paste(unlist(lapply(df, as.character)), collapse = " ")
      stray <- unique(unlist(regmatches(flat, gregexpr("\\b[0-9]{10}\\b", flat))))
      if (length(stray) > 0L) {
        stop(sprintf(paste0(
          "Source '%s' (%s) yielded 0 NPIs but contains %d distinct 10-digit ",
          "tokens. Its identifier column is not named npi-like. Declare it with ",
          "npi_columns=, or the exclusion universe silently shrinks and the ",
          "holdout stops being independent.\n  columns: %s"),
          s$path, ifelse(is.na(s$ref), "<working tree>", s$ref), length(stray),
          paste(names(df), collapse = ", ")), call. = FALSE)
      }
    }
    rows[[length(rows) + 1L]] <- data.frame(
      source_path = s$path,
      source_ref = ifelse(is.na(s$ref), "<working tree>", s$ref),
      why = s$why,
      file_rows = if (is.null(df)) 0L else nrow(df),
      npis_found = length(npis),
      present = !is.null(df),
      stringsAsFactors = FALSE)
    all_npis <- c(all_npis, npis)
  }
  manifest <- do.call(rbind, rows)
  uni <- sort(unique(all_npis))

  dir.create("validation/reference", recursive = TRUE, showWarnings = FALSE)
  out_universe <- "validation/reference/development_exclusion_universe.csv"
  out_manifest <- "validation/reference/development_exclusion_sources.csv"
  utils::write.csv(data.frame(npi = uni, stringsAsFactors = FALSE),
                   out_universe, row.names = FALSE)
  utils::write.csv(manifest, out_manifest, row.names = FALSE)

  cat("Development-exclusion universe\n")
  cat(sprintf("  source artifacts declared     %d\n", nrow(manifest)))
  cat(sprintf("  source artifacts found        %d\n", sum(manifest$present)))
  cat(sprintf("  NPI occurrences before dedup  %d\n", length(all_npis)))
  cat(sprintf("  UNIQUE NPIs excluded          %d\n", length(uni)))
  missing <- manifest[!manifest$present, ]
  if (nrow(missing)) {
    cat("\n  DECLARED BUT NOT FOUND (fail closed -- do not proceed):\n")
    for (i in seq_len(nrow(missing))) cat(sprintf("    %s @ %s\n",
      missing$source_path[i], missing$source_ref[i]))
  }
  cat("\n  per-source contribution:\n")
  for (i in seq_len(nrow(manifest))) cat(sprintf("    %-6s %4d npi  %-64s %s\n",
    ifelse(manifest$present[i], "ok", "MISS"), manifest$npis_found[i],
    substr(manifest$source_path[i], 1, 64), substr(manifest$source_ref[i], 1, 40)))
  if (ok) {
    cat(sprintf("\n  sha256 universe : %s\n", digest::digest(file = out_universe, algo = "sha256")))
    cat(sprintf("  sha256 manifest : %s\n", digest::digest(file = out_manifest, algo = "sha256")))
  }
  if (nrow(missing)) quit(status = 1L)
  invisible(TRUE)
}
if (sys.nframe() == 0L) main()
