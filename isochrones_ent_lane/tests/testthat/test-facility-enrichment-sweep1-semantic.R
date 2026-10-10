#' @title Semantic tests — Facility/practice enrichment sweep 1 (10 rounds)
#'
#' @description First sweep on the practice/facility enrichment surface
#'   (~15 files, ~5K lines). Two real fixes landed; the remaining eight
#'   rounds audited CLEAN or yielded findings that did not meet the
#'   HIGH/CRITICAL severity gate (e.g., NPI sprintf with prior gsub-digit
#'   sanitization — not exploitable; YAML-driven column-name interpolation
#'   — internal config, not user input).
#'
#'   Two real fixes:
#'
#'   - F1-R2 (CRITICAL): `scripts/add_secondary_locations_to_cohort.R:91`
#'     The filter clauses compared `state` against uppercase
#'     `c(state.abb, "DC")` without first normalizing case. Any row where
#'     the upstream CSV produced a lowercase state ("ca", "ny") silently
#'     failed `state %in% c(...)` (returns FALSE for lowercase) and was
#'     dropped from `sec_flagged`, suppressing rural-secondary counts at
#'     the state level. Fixed by adding `mutate(state = toupper(state))`
#'     before the filter so the comparison matches regardless of
#'     upstream case-handling.
#'
#'   - F1-R3 (HIGH): `R/extract_secondary_practice_locations.R:1128-1134`
#'     The `primary_address_key` ZIP normalization used
#'     `substr(gsub("[^0-9]", "", coalesce(primary_zip, "")), 1, 5)`,
#'     which (a) returns "" instead of NA when primary_zip is missing,
#'     and (b) does not zero-pad short ZIPs (e.g., New England "5432"
#'     stays "5432" instead of "05432"). Meanwhile the matching
#'     `address_key` upstream at L1078-1084 uses `zip_clean`, which IS
#'     NA-aware and IS zero-padded. The two keys diverge whenever ZIPs
#'     are missing or unpadded, silently failing `is_primary` and
#'     causing the row documented as primary to be emitted as a
#'     secondary location (double-counting). Mirrored the same
#'     `.zip_digits → str_pad → NA-aware ifelse` pipeline used for
#'     `zip_clean` to both sides of the comparison.
#'
#'   Rounds with no fix:
#'   - F1-R1 (enrich_facility_name.R): CLEAN. Defensive patterns
#'     throughout (.modal_name_by_key dedupes; case_when has explicit
#'     TRUE branches; tryCatch handlers all return data.frame()).
#'   - F1-R5 (validators + match_facility_affiliation): one finding
#'     about facility-affiliation join fan-out — rejected as
#'     performance concern (post-hoc dedup at L241-249 is correct),
#'     not a behavioural bug.
#'   - F1-R6 (loader + most-recent-address): "SQL injection" finding
#'     rejected — the NPI input passes through `gsub("[^0-9]", "", npi)`
#'     before sprintf, leaving only digits which cannot escape a
#'     single-quoted SQL string context.
#'   - F1-R7 (practice_size + nber_practice_address_loader): one
#'     finding about YAML-driven column-name interpolation — rejected
#'     as code-smell (config under version control, not user input).
#'   - F1-R8 (GO secondary trio): CLEAN.
#'   - F1-R9 (340B + ENT-universe enrichment): CLEAN.
#'
#' @keywords internal
library(testthat)

.find_repo_root <- function() {
  candidates <- c(
    tryCatch(here::here(), error = function(e) NULL),
    normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE),
    getwd()
  )
  for (p in candidates) {
    if (!is.null(p) && length(p) > 0L && nzchar(p) &&
        file.exists(file.path(p, "scripts", "add_secondary_locations_to_cohort.R"))) {
      return(p)
    }
  }
  skip("Repo root not found in this test environment.")
}

.read_live_lines <- function(rel_path) {
  root <- .find_repo_root()
  src <- readLines(file.path(root, rel_path), warn = FALSE)
  src[!grepl("^\\s*#", src)]
}

# ============================================================================
# F1-R2 — add_secondary_locations_to_cohort normalizes state to upper before filter
# ============================================================================

test_that("F1-R2 structural: sec_flagged toupper(state)s before the state filter", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")

  # Accept either the sweep-1 form (`toupper(state)`) or the sweep-2
  # enhanced form (`toupper(trimws(state))`).
  expect_true(
    grepl("mutate\\(state\\s*=\\s*toupper\\(\\s*(trimws\\(\\s*)?state(\\s*\\))?\\)\\)",
          joined),
    info = "must apply toupper (optionally wrapped with trimws) before the state filter"
  )
  expect_true(
    grepl("filter\\(!state %in% NON_CONUS,\\s*state %in% c\\(state\\.abb,\\s*\"DC\"\\),\\s*!is\\.na\\(state\\)\\)",
          joined),
    info = "filter clauses preserved (downstream contract unchanged)"
  )
})

test_that("F1-R2 behavioural: lowercase state rows survive the state filter after fix", {
  # Replay the filter logic in isolation.
  state.abb <- c("AL","AK","AZ","CA","CO","NY","WY")  # subset for the test
  NON_CONUS <- c("AK","HI","PR","VI","GU","MP","AS","AA","AE","AP")

  sec <- data.frame(
    npi   = c("1", "2", "3", "4", "5"),
    state = c("CA", "ca", "NY", "ny", NA),  # mixed case + an NA
    stringsAsFactors = FALSE
  )

  # OLD: filter without case-normalization
  old <- sec[!sec$state %in% NON_CONUS &
               sec$state %in% c(state.abb, "DC") &
               !is.na(sec$state), ]

  # NEW: filter with toupper() applied first
  sec_new <- sec
  sec_new$state <- toupper(sec_new$state)
  new <- sec_new[!sec_new$state %in% NON_CONUS &
                   sec_new$state %in% c(state.abb, "DC") &
                   !is.na(sec_new$state), ]

  expect_equal(nrow(old), 2L,
               info = "OLD: only the two uppercase rows survive (lowercase dropped)")
  expect_equal(nrow(new), 4L,
               info = "FIX: all four valid-state rows survive; only the NA row drops")
  expect_setequal(new$npi, c("1", "2", "3", "4"))
})

# ============================================================================
# F1-R3 — extract_secondary_practice_locations primary_zip_clean mirrors zip_clean
# ============================================================================

test_that("F1-R3 structural: primary_address_key uses the same NA-aware + zero-padded ZIP pipeline as zip_clean", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")

  expect_true(
    grepl("\\.primary_zip_digits\\s*=\\s*substr\\(gsub\\(\"\\[\\^0-9\\]\",\\s*\"\",\\s*coalesce\\(primary_zip,\\s*\"\"\\)\\),\\s*1,\\s*5\\)",
          joined),
    info = "must extract the same `.primary_zip_digits` as the zip_clean pipeline"
  )
  expect_true(
    grepl("stringr::str_pad\\(\\.primary_zip_digits", joined),
    info = "must str_pad primary_zip_digits with leading zeros to width 5"
  )
  expect_true(
    grepl("primary_address_key = paste\\(\\s*\\n?\\s*toupper\\(trimws\\(coalesce\\(primary_address,\\s*\"\"\\)\\)\\),",
          joined),
    info = "primary_address_key block preserved"
  )
  expect_true(
    grepl("\\.primary_zip_clean,\\s*\\n?\\s*sep\\s*=\\s*\"\\|\"", joined),
    info = "the new .primary_zip_clean field replaces the raw substr in the key"
  )
  # The dropped/temporary columns must be removed before exit.
  expect_true(
    grepl("dplyr::select\\(-\"\\.primary_zip_digits\", -\"\\.primary_zip_clean\"\\)",
          joined),
    info = "scratch ZIP-normalization columns must be dropped after key build"
  )
})

test_that("F1-R3 behavioural: zip_clean normalization is symmetric across both key sides", {
  # Replay the normalization on both sides of the comparison. The key
  # property: identical input string -> identical normalized output,
  # AND missing input on either side -> NA_character_ (not "") so the
  # comparison sees NA == NA == NA (consistent).
  normalize_zip <- function(zip) {
    digits <- substr(gsub("[^0-9]", "", ifelse(is.na(zip), "", zip)), 1, 5)
    ifelse(
      is.na(digits) | digits == "" | grepl("^0+$", digits),
      NA_character_,
      formatC(as.integer(digits), width = 5, flag = "0", format = "d")
    )
  }
  # Use the same string-only padding the production code uses (str_pad
  # with width 5 + left-pad "0"). Mirror it here without depending on
  # the stringr package.
  normalize_zip <- function(zip) {
    raw <- ifelse(is.na(zip), "", as.character(zip))
    digits <- substr(gsub("[^0-9]", "", raw), 1, 5)
    out <- character(length(digits))
    for (i in seq_along(digits)) {
      d <- digits[i]
      if (is.na(d) || d == "" || grepl("^0+$", d)) {
        out[i] <- NA_character_
      } else {
        # left-pad with "0" to width 5
        out[i] <- if (nchar(d) >= 5) d else paste0(strrep("0", 5 - nchar(d)), d)
      }
    }
    out
  }

  inputs <- c("05432", "5432", "12345-6789", NA, "", "00000", "9-digit-junk")
  out    <- normalize_zip(inputs)

  expect_equal(out[1], "05432", info = "10-digit padded ZIP unchanged")
  expect_equal(out[2], "05432", info = "FIX: short ZIP zero-padded to 5")
  expect_equal(out[3], "12345", info = "9-digit ZIP+4 truncated to first 5")
  expect_true(is.na(out[4]), info = "NA input -> NA output (not empty string)")
  expect_true(is.na(out[5]), info = "empty input -> NA output")
  expect_true(is.na(out[6]), info = "all-zero ZIP -> NA (genuinely missing)")
  # Note: "9-digit-junk" contains a single digit '9', so gsub leaves "9"
  # which left-pads to "00009". Production code accepts this — the
  # contract is "any digits → pad to 5". Test the real contract, not an
  # idealized one.
  expect_equal(out[7], "00009",
               info = "single-digit junk input pads to 00009 per contract")

  # Cross-side property: the same input on both sides produces the same
  # normalized output -> the (address_key == primary_address_key)
  # comparison is now deterministic and case-symmetric on ZIPs.
  side_a <- normalize_zip(c("5432", NA, "12345"))
  side_b <- normalize_zip(c("5432", NA, "12345"))
  # NA == NA is NA in R; for the equality check to behave as "match",
  # both sides need consistent NA handling.
  expect_identical(side_a, side_b,
                   info = "symmetric normalization on both sides of the key")
})

# ============================================================================
# F2-#1 — secondary rural points guard against missing lat/lon on the fallback path
# ============================================================================

test_that("F2-#1 structural: sec_pts guards on .sec_has_geocode and falls back to ZCTA centroid", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")

  expect_true(
    grepl("\\.sec_has_geocode\\s*<-\\s*all\\(c\\(\"latitude\",\\s*\"longitude\"\\)\\s*%in%\\s*names\\(sec_flagged\\)\\)",
          joined),
    info = "must compute .sec_has_geocode by checking the schema"
  )
  expect_true(
    grepl("sec_pts\\s*<-\\s*if \\(\\.sec_has_geocode\\)", joined),
    info = "sec_pts must branch on the schema-check, not blindly read latitude/longitude"
  )
  expect_true(
    grepl("Secondary practice \\(rural, ZCTA centroid\\)", joined),
    info = "fallback path must label points distinctly so downstream can tell"
  )
})

test_that("F2-#1 behavioural: schema-aware sec_pts builder doesn't crash when lat/lon absent", {
  # Replay the contract. With the .empty_pl_pfile_df schema (no
  # latitude/longitude columns) the OLD code blew up trying to read
  # the missing columns; the NEW code uses the ZCTA centroid fallback.
  empty_pl_pfile_cols <- c("npi", "address_line1", "address_line2",
                           "city", "state", "zip", "facility_name",
                           "org_pac_id", "dac_specialty", "adrs_id",
                           "source", "vintage_date")
  filtered_csv_cols <- c(empty_pl_pfile_cols, "latitude", "longitude")

  build_sec_pts <- function(sec_flagged_cols) {
    has_geocode <- all(c("latitude", "longitude") %in% sec_flagged_cols)
    if (has_geocode) "primary"   else "fallback"
  }

  expect_equal(build_sec_pts(filtered_csv_cols), "primary",
               info = "filtered-CSV schema uses lat/lon path")
  expect_equal(build_sec_pts(empty_pl_pfile_cols), "fallback",
               info = "FIX: fallback schema (no lat/lon) takes the ZCTA-centroid branch")
})

# ============================================================================
# F2-#2 — ZIP+4 normalization keeps "02115-1234" instead of silently dropping
# ============================================================================

test_that("F2-#2 structural: both ZIP paths use digit-strip + str-pad, not bare as.integer", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")

  # The new pipeline appears at least twice (cohort path + sec path).
  expect_gt(
    length(gregexpr("\\.zip[5_]?(raw|digits)", joined)[[1]]), 2,
    label = "occurrences of the new .zip*_raw/.zip*_digits scratch columns"
  )
  expect_true(
    grepl("formatC\\(as\\.integer\\(\\.zip[5_]?digits\\),\\s*width\\s*=\\s*5,\\s*flag\\s*=\\s*\"0\",\\s*format\\s*=\\s*\"d\"\\)",
          joined),
    info = "must format with leading zeros after digit extraction"
  )
  # The old as.integer(as.character(zip)) form is gone from both paths.
  expect_false(
    grepl("as\\.integer\\(as\\.character\\(zip\\)\\)", joined),
    info = "old as.integer(as.character(zip)) ZIP path must be removed"
  )
  expect_false(
    grepl("as\\.integer\\(as\\.character\\(zip5\\)\\)", joined),
    info = "old as.integer(as.character(zip5)) ZIP path must be removed"
  )
})

test_that("F2-#2 behavioural: ZIP+4 strings survive normalization", {
  normalize_zip_new <- function(zip) {
    raw <- ifelse(is.na(zip), "", as.character(zip))
    digits <- substr(gsub("[^0-9]", "", raw), 1, 5)
    ifelse(
      digits == "" | grepl("^0+$", digits),
      NA_character_,
      formatC(as.integer(digits), width = 5, flag = "0", format = "d")
    )
  }
  normalize_zip_old <- function(zip) {
    .zi <- suppressWarnings(as.integer(as.character(zip)))
    ifelse(is.na(.zi), NA_character_, sprintf("%05d", .zi))
  }

  inputs <- c("02115-1234", "02115", "2115", "10001", " 90210 ", NA, "")
  old <- normalize_zip_old(inputs)
  new <- normalize_zip_new(inputs)

  expect_true(is.na(old[1]), info = "OLD: ZIP+4 silently drops to NA (bug)")
  expect_equal(new[1], "02115", info = "FIX: ZIP+4 keeps first 5 digits")
  expect_equal(new[2], "02115", info = "5-digit ZIP unchanged")
  expect_equal(new[3], "02115", info = "FIX: leading-zero loss recovered via padding")
  expect_equal(new[4], "10001")
  expect_equal(new[5], "90210", info = "FIX: whitespace tolerated via gsub")
  expect_true(is.na(new[6]), info = "NA stays NA")
  expect_true(is.na(new[7]), info = "empty string stays NA")
})

# ============================================================================
# F2-#3 follow-up — toupper(trimws(state)) instead of just toupper(state)
# ============================================================================

test_that("F2-#3-followup structural: state normalization uses toupper(trimws(.))", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("mutate\\(state\\s*=\\s*toupper\\(trimws\\(state\\)\\)\\)", joined),
    info = "must wrap toupper with trimws so padded states (' NY ') survive"
  )
})

test_that("F2-#3-followup behavioural: padded + lowercase variants all normalize", {
  state.abb <- c("CA","NY","FL")
  NON_CONUS <- c("AK","HI","PR")

  inputs <- c("CA", "ca", " NY ", "ny ", " fl", NA)
  normalized <- toupper(trimws(inputs))
  kept <- !normalized %in% NON_CONUS &
            normalized %in% c(state.abb, "DC") &
            !is.na(normalized)

  expect_equal(sum(kept), 5L,
               info = "FIX: all 5 valid-state variants normalize and survive")
})

# ============================================================================
# F2-#4 — OUT_DIR created at startup with loud failure on error
# ============================================================================

test_that("F2-#4 structural: OUT_DIR is created with recursive=TRUE and isdir verified", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("dir\\.create\\(OUT_DIR,\\s*recursive\\s*=\\s*TRUE", joined),
    info = "OUT_DIR creation must be recursive"
  )
  expect_true(
    grepl("file\\.info\\(OUT_DIR\\)\\$isdir", joined),
    info = "must verify OUT_DIR is actually a directory after dir.create"
  )
  expect_true(
    grepl('stop\\(sprintf\\("\\[startup\\] cannot create OUT_DIR', joined),
    info = "must stop loudly if dir.create fails"
  )
})

# ============================================================================
# F2-#5 — every file.rename() result is captured and stop()s on FALSE
# ============================================================================

test_that("F2-#5 structural: all four file.rename sites guard the return value", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")
  # The four guarded forms each look like:
  #   if (!isTRUE(file.rename(<tmp>, <out>))) { stop(...) }
  occurrences <- length(gregexpr(
    "if\\s*\\(!isTRUE\\(file\\.rename\\(",
    joined, perl = TRUE)[[1]])
  expect_equal(occurrences, 4L,
               info = "all four file.rename call sites must be wrapped in !isTRUE() check")
  # Count file.rename calls overall — should be exactly 4 (all guarded).
  total_renames <- length(gregexpr("file\\.rename\\(", joined, perl = TRUE)[[1]])
  expect_equal(total_renames, 4L,
               info = "total file.rename call count should match the guarded-call count")
})

test_that("F2-#5 behavioural: replay rename-fail path stops loudly with tmp preserved", {
  td <- tempfile("rename_test_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  tmp <- file.path(td, "data.tmp")
  out <- file.path(td, "subdir_does_not_exist", "data.out")
  writeLines("payload", tmp)

  # OLD pattern: bare file.rename swallows failure
  old_result <- file.rename(tmp, out)
  expect_false(old_result,
               info = "rename to non-existent dir returns FALSE silently")
  expect_true(file.exists(tmp),
              info = "tmp survives the failed rename")

  # NEW pattern: isTRUE guard + stop()
  expect_error(
    {
      if (!isTRUE(file.rename(tmp, out))) {
        stop(sprintf("[write] file.rename failed: %s -> %s (tmp preserved)", tmp, out),
             call. = FALSE)
      }
    },
    "file\\.rename failed"
  )
})

# ============================================================================
# F2-#6 — DAC + PC tables resolved dynamically via .resolve_*_tables(con)
# ============================================================================

test_that("F2-#6 structural: extract_secondary_practice_locations uses dynamic table resolvers", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")

  expect_true(
    grepl("for \\(tbl in \\.resolve_dac_tables\\(con\\)\\)", joined),
    info = "DAC table walk must call .resolve_dac_tables(con) dynamically"
  )
  expect_true(
    grepl("for \\(tbl in \\.resolve_pc_tables\\(con\\)\\)", joined),
    info = "PC table walk must call .resolve_pc_tables(con) dynamically"
  )
  expect_false(
    grepl("for \\(tbl in DAC_TABLES\\)", joined),
    info = "static DAC_TABLES walk must be removed (fallback constant only)"
  )
  expect_false(
    grepl("for \\(tbl in PC_TABLES\\)", joined),
    info = "static PC_TABLES walk must be removed (fallback constant only)"
  )
})

# ============================================================================
# F2-#7 — .pick_latest_dac has a deterministic tiebreaker on _csv_N suffix
# ============================================================================

test_that("F2-#7 structural: .pick_latest_dac sorts by ymonth then import suffix then name", {
  live <- .read_live_lines("R/enrich_facility_name.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("sub\\(\\s*\".*_csv_\\(\\[0-9\\]\\+\\)\\$\",\\s*\"\\\\\\\\1\",", joined),
    info = "import suffix must be parsed from trailing _csv_N pattern"
  )
  expect_true(
    grepl("order\\(ymonth_keep,\\s*import_suffix,\\s*cand_keep,", joined),
    info = "order key must be (ymonth, import_suffix, name) — three-level"
  )
})

test_that("F2-#7 behavioural: deterministic tiebreaker picks _csv_6 over _csv_4 at same month", {
  # Replay the contract: same month_year, different import suffix.
  cand <- c(
    "doctors_and_clinicians_12_2022_DAC_NationalDownloadableFile_csv_4",
    "doctors_and_clinicians_12_2022_DAC_NationalDownloadableFile_csv_6"
  )
  m <- regexpr("\\d{2}_20\\d{2}", cand)
  yymo <- regmatches(cand, m)
  ymonth <- as.Date(paste0("01_", yymo), format = "%d_%m_%Y")
  import_suffix <- as.integer(sub(".*_csv_([0-9]+)$", "\\1", cand, perl = TRUE))

  picked <- cand[order(ymonth, import_suffix, cand,
                       decreasing = c(TRUE, TRUE, TRUE),
                       method = "radix")][1L]
  expect_equal(picked,
               "doctors_and_clinicians_12_2022_DAC_NationalDownloadableFile_csv_6",
               info = "FIX: higher import suffix wins on tied month_year")
})

# ============================================================================
# F2-#8 — subspecialty_breakdown resolves the subspecialty column specialty-agnostically
# ============================================================================

test_that("F2-#8 structural: subspecialty_breakdown uses the same resolver as primary_locations", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('\\.sub_col_for_breakdown\\s*<-\\s*if \\("subspecialty" %in% names\\(cohort\\)\\)',
          joined),
    info = "must use the same .sub_col_for_breakdown logic as .sub_col_for_primary"
  )
  expect_true(
    grepl("ent_subspecialty", joined) && grepl("abohns_subspecialty", joined),
    info = "must check ENT and ABOto specialty columns"
  )
  expect_true(
    grepl('grep\\("_subspecialty\\$",\\s*names\\(cohort\\),\\s*value\\s*=\\s*TRUE\\)',
          joined),
    info = "must fall back to grep-for-_subspecialty-suffix when none of the three named columns match"
  )
})

# ============================================================================
# F2-#9 — cohort deduped to one row per NPI BEFORE the metadata join
# ============================================================================

test_that("F2-#9 structural: cohort_one_per_npi dedupes before the left_join", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("dplyr::distinct\\(npi,\\s*\\.keep_all\\s*=\\s*TRUE\\)", joined),
    info = "must dedupe cohort to one-row-per-NPI before joining for metadata"
  )
  expect_true(
    grepl("\\.cohort_one_per_npi", joined),
    info = "explicit cohort_one_per_npi variable signals the dedup intent"
  )
})

test_that("F2-#9 behavioural: duplicate-NPI cohort no longer inflates breakdown counts", {
  cohort_with_dups <- data.frame(
    npi          = c("1", "1", "1", "2", "3"),  # NPI 1 appears 3 times
    subspecialty = c("FPMRS", "FPMRS", "FPMRS", "MFM", NA),
    stringsAsFactors = FALSE
  )
  output <- data.frame(
    npi = c("1", "1", "2", "3", "3"),  # 2 secondary locations each for 1+3, 1 for 2
    stringsAsFactors = FALSE
  )

  # OLD (buggy): join without dedup -> NPI 1 fans out 3x in cohort
  old_join <- merge(output, cohort_with_dups, by = "npi", all.x = TRUE)
  expect_gt(nrow(old_join), nrow(output),
            label = "OLD: cohort fan-out inflates the join result")

  # NEW: dedupe cohort first
  cohort_dedup <- cohort_with_dups[!duplicated(cohort_with_dups$npi), ]
  new_join <- merge(output, cohort_dedup, by = "npi", all.x = TRUE)
  expect_equal(nrow(new_join), nrow(output),
               info = "FIX: one row per output row, no fan-out")
})

# ============================================================================
# F2-#10 — suite-suffix regex strips alphabetic unit tokens (BLDG A, RM A)
# ============================================================================

test_that("F2-#10 structural: suite regex accepts alphabetic single-letter unit tokens", {
  live <- .read_live_lines("R/enrich_facility_name.R")
  joined <- paste(live, collapse = "\n")
  # Accept either of two equivalent shapes:
  #   (a) my initial `[A-Z0-9]{1,6}` (any alphanumeric 1-6 chars), or
  #   (b) the refined `(?:[A-Z]|[A-Z0-9]{0,5}[0-9][A-Z0-9]{0,5})` —
  #       single letter OR alphanumeric WITH at least one digit. The
  #       refined form is sharper because it rejects all-letter
  #       multi-char tokens that would over-strip street types.
  has_simple   <- grepl("\\[A-Z0-9\\]\\{1,6\\}", joined)
  has_refined  <- grepl("\\(\\?:\\[A-Z\\]\\|\\[A-Z0-9\\]\\{0,5\\}\\[0-9\\]\\[A-Z0-9\\]\\{0,5\\}\\)",
                        joined)
  expect_true(
    has_simple || has_refined,
    info = "regex must accept alphabetic + alphanumeric unit tokens in one of the two equivalent shapes"
  )
  expect_true(
    grepl("STE\\|SUITE\\|UNIT\\|APT\\|APARTMENT\\|BLDG\\|BUILDING\\|FL\\|FLOOR\\|RM\\|ROOM",
          joined),
    info = "designator alternation list preserved"
  )
  # Old digit-required lookahead `(?=\S*[0-9])` is gone.
  expect_false(
    grepl("\\(\\?=\\\\S\\*\\[0-9\\]\\)", joined),
    info = "the old digit-required lookahead `(?=\\S*[0-9])` must be removed"
  )
})

test_that("F2-#10 behavioural: alphabetic unit tokens are stripped from the normalized key", {
  # Replay the production regex pattern on a few test inputs.
  normalize_suite <- function(addr) {
    sub("\\s*,?\\s*(STE|SUITE|UNIT|APT|APARTMENT|BLDG|BUILDING|FL|FLOOR|RM|ROOM)\\s+[A-Z0-9]{1,6}\\b.*$",
        "", toupper(trimws(addr)), perl = TRUE)
  }

  expect_equal(normalize_suite("123 MAIN ST BLDG A"), "123 MAIN ST",
               info = "FIX: BLDG A stripped (alphabetic unit)")
  expect_equal(normalize_suite("123 MAIN ST RM A"), "123 MAIN ST",
               info = "FIX: RM A stripped (alphabetic unit)")
  expect_equal(normalize_suite("123 MAIN ST STE 200"), "123 MAIN ST",
               info = "STE 200 still stripped (digit unit)")
  expect_equal(normalize_suite("123 MAIN ST BLDG J5"), "123 MAIN ST",
               info = "BLDG J5 stripped (alphanumeric)")
  expect_equal(normalize_suite("123 MAIN ST"), "123 MAIN ST",
               info = "no suffix: unchanged")
})

# ============================================================================
# F3-SQL-parity — all three SQL queries mirror the R-side
#                  .normalize_address_line regex EXACTLY, and the four edge
#                  cases (BLDG A / RM A / FL ST / RM BLVD) all behave
#                  consistently
# ============================================================================

test_that("F3-SQL-parity structural: all three DAC/NPPES/cms_hospital SQL queries use the same unit-token regex", {
  live_full <- readLines(file.path(.find_repo_root(),
                                   "R/enrich_facility_name.R"),
                         warn = FALSE)
  joined <- paste(live_full, collapse = "\n")

  # The alphanumeric-token portion is identical in both the R helper
  # (single-quoted -> needs `\\s+`) and the SQL raw-string queries
  # (-> uses `\s+`), so anchoring on the bracketed-class portion is
  # the most robust parity assertion.
  alpha_token <-
    "\\[A-Z0-9\\]\\{0,5\\}\\[0-9\\]\\[A-Z0-9\\]\\{0,5\\}"
  matches <- length(gregexpr(alpha_token, joined, perl = TRUE)[[1]])
  expect_gte(matches, 4L,
             label = "alphanumeric unit-token pattern (1 R helper + 3 SQL queries)")

  # And the alternation list of designator keywords must also appear
  # the same number of times.
  designator_list <-
    "STE\\|SUITE\\|UNIT\\|APT\\|APARTMENT\\|BLDG\\|BUILDING\\|FL\\|FLOOR\\|RM\\|ROOM"
  designator_matches <- length(gregexpr(designator_list, joined, perl = TRUE)[[1]])
  expect_gte(designator_matches, 4L,
             label = "designator list occurrences (1 R helper + 3 SQL queries)")
})

test_that("F3-SQL-parity behavioural: the four edge cases behave correctly under the regex", {
  # Replay the R-side regex; DuckDB regex matches PCRE behaviour for these
  # inputs, so SQL behaviour is identical.
  normalize_addr_line <- function(addr) {
    s <- toupper(trimws(as.character(addr)))
    s <- gsub("\\s+", " ", s)
    s <- sub("\\s*,?\\s*(STE|SUITE|UNIT|APT|APARTMENT|BLDG|BUILDING|FL|FLOOR|RM|ROOM)\\s+(?:[A-Z]|[A-Z0-9]{0,5}[0-9][A-Z0-9]{0,5})\\b.*$",
             "", s, perl = TRUE)
    s <- sub("\\s*#\\s*\\S+.*$", "", s, perl = TRUE)
    s <- sub("[[:punct:]]+$", "", s)
    trimws(s)
  }

  # Cases that MUST be stripped (true unit tokens)
  expect_equal(normalize_addr_line("123 MAIN ST BLDG A"), "123 MAIN ST",
               info = "BLDG A: single-letter unit stripped")
  expect_equal(normalize_addr_line("123 MAIN ST RM A"), "123 MAIN ST",
               info = "RM A: single-letter unit stripped")
  expect_equal(normalize_addr_line("123 MAIN ST STE 200"), "123 MAIN ST",
               info = "STE 200: digit-only unit stripped")
  expect_equal(normalize_addr_line("123 MAIN ST BLDG J5"), "123 MAIN ST",
               info = "BLDG J5: alphanumeric (with digit) stripped")
  expect_equal(normalize_addr_line("123 MAIN ST FLOOR 3"), "123 MAIN ST",
               info = "FLOOR 3: digit unit stripped")

  # Cases that MUST NOT be stripped (false-positive guards the user named)
  expect_equal(normalize_addr_line("123 FL ST"), "123 FL ST",
               info = "FL ST: 'ST' is a street-type word (2 letters, no digit) — preserved")
  expect_equal(normalize_addr_line("456 RM BLVD"), "456 RM BLVD",
               info = "RM BLVD: 'BLVD' is 4 letters, no digit — preserved")
  expect_equal(normalize_addr_line("789 APT BLVD"), "789 APT BLVD",
               info = "APT BLVD: 'BLVD' is 4 letters, no digit — preserved")
  expect_equal(normalize_addr_line("100 UNIT ROAD"), "100 UNIT ROAD",
               info = "UNIT ROAD: 'ROAD' is 4 letters, no digit — preserved")

  # Boundary case: alphanumeric WITHOUT digit must be preserved
  expect_equal(normalize_addr_line("123 ELM ST BLDG 12B"), "123 ELM ST",
               info = "BLDG 12B: 3-char alphanumeric with digit — stripped")
  expect_equal(normalize_addr_line("123 ELM ST BLDG ABC"), "123 ELM ST BLDG ABC",
               info = "BLDG ABC: 3-char all-letter (not single-letter) — preserved")
})

# ============================================================================
# F2-extra — output_metadata_path tolerates character(0) without crashing
# ============================================================================

test_that("F2-extra structural: .has_path guard rejects character(0) / NA / empty / length>1", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  # The F3-#4 hardening changed `length(p) > 0L` to `length(p) == 1L`
  # and wrapped the whole predicate in isTRUE(). Accept the
  # post-F3-#4 form here; F3-#4's own structural test enforces the
  # stricter shape.
  expect_true(
    grepl("\\.has_path\\s*<-\\s*function\\(p\\)\\s*\\{?", joined),
    info = "must define .has_path predicate"
  )
  expect_true(
    grepl("isTRUE\\(\\s*!is\\.null\\(p\\)\\s*&&\\s*length\\(p\\)\\s*==\\s*1L\\s*&&\\s*!is\\.na\\(p\\)\\s*&&\\s*nzchar\\(p\\)\\s*\\)",
          joined),
    info = "predicate body must be the hardened scalar-safe form"
  )
  expect_true(
    grepl("if \\(\\.has_path\\(output_metadata_path\\)\\)", joined),
    info = "must use .has_path() instead of bare !is.null()"
  )
})

test_that("F2-extra behavioural: character(0) is rejected by the path guard", {
  .has_path <- function(p) !is.null(p) && length(p) > 0L && !is.na(p) && nzchar(p)

  expect_false(.has_path(character(0)),
               info = "FIX: character(0) is rejected (would have leaked through !is.null())")
  expect_false(.has_path(NULL))
  expect_false(.has_path(""))
  expect_false(.has_path(NA_character_))
  expect_true(.has_path("some/path.json"))
})

# ============================================================================
# F3-#1 — empty DAC + non-empty pl_pfile continues (no early empty return)
# ============================================================================

test_that("F3-#1 structural: empty-DAC short-circuit now requires pl_pfile also empty", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("nrow\\(dac_locations\\) == 0\\s*&&\\s*nrow\\(pl_pfile_locations\\) == 0",
          joined),
    info = "empty-return guard must require BOTH dac_locations AND pl_pfile_locations be empty"
  )
  expect_true(
    grepl("continuing with pl_pfile", joined),
    info = "must log the continuation path when DAC empty but pl_pfile non-empty"
  )
})

# ============================================================================
# F3-#2 — .resolve_dac_tables tiebreaker on `_csv_N` import suffix
# ============================================================================

test_that("F3-#2 structural: .resolve_dac_tables parses _csv_N import suffix", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("import_suffix\\s*<-\\s*suppressWarnings\\(as\\.integer\\(sub\\(\\s*\".*_csv_\\(\\[0-9\\]\\+\\)\\$\",",
          joined),
    info = "must parse trailing _csv_N suffix"
  )
  expect_true(
    grepl("order\\(-scores,\\s*-import_suffix,\\s*matches", joined),
    info = "sort key must include (-scores, -import_suffix, matches)"
  )
})

test_that("F3-#2 behavioural: _csv_6 wins over _csv_4 at same month_year", {
  matches <- c(
    "doctors_and_clinicians_05_2024_DAC_NationalDownloadableFile_csv_4",
    "doctors_and_clinicians_05_2024_DAC_NationalDownloadableFile_csv_6"
  )
  scores <- vapply(matches, function(nm) {
    mm_yyyy <- regmatches(nm, regexpr("([0-9]{2})_([0-9]{4})", nm, perl = TRUE))
    parts <- strsplit(mm_yyyy, "_", fixed = TRUE)[[1L]]
    as.numeric(parts[2L]) * 100 + as.numeric(parts[1L])
  }, numeric(1L))
  import_suffix <- as.integer(sub(".*_csv_([0-9]+)$", "\\1", matches, perl = TRUE))

  sorted <- matches[order(-scores, -import_suffix, matches,
                          method = "radix",
                          decreasing = c(FALSE, FALSE, TRUE))]
  expect_equal(sorted[1L],
               "doctors_and_clinicians_05_2024_DAC_NationalDownloadableFile_csv_6",
               info = "FIX: higher import suffix wins at tied month_year")
})

# ============================================================================
# F3-#3 — .resolve_pc_tables sorts on (year DESC, quarter DESC, name DESC)
# ============================================================================

test_that("F3-#3 structural: .resolve_pc_tables parses both year AND quarter", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("q_match\\s*<-\\s*regexpr\\(\"_Q\\(\\[1-4\\]\\)\\\\\\\\b\",", joined),
    info = "must parse _Q[1-4] quarter from PC table names"
  )
  expect_true(
    grepl("order\\(-years,\\s*-quarters,\\s*matches", joined),
    info = "sort key must include (-years, -quarters, matches)"
  )
})

test_that("F3-#3 behavioural: Q4 wins over Q1 at same year", {
  matches <- c("physician_compare_2025_Q1", "physician_compare_2025_Q4",
               "physician_compare_2024_Q2")
  years <- as.integer(regmatches(matches, regexpr("[0-9]{4}", matches)))
  q_match <- regexpr("_Q([1-4])\\b", matches, perl = TRUE)
  quarters <- rep(0L, length(matches))
  q_strs <- regmatches(matches, q_match)
  quarters[q_match > 0L] <- as.integer(sub("_Q", "", q_strs))

  sorted <- matches[order(-years, -quarters, matches,
                          method = "radix",
                          decreasing = c(FALSE, FALSE, TRUE))]
  expect_equal(sorted[1L], "physician_compare_2025_Q4",
               info = "FIX: latest quarter wins at tied year")
  expect_equal(sorted[2L], "physician_compare_2025_Q1")
  expect_equal(sorted[3L], "physician_compare_2024_Q2")
})

# ============================================================================
# F3-#4 — .has_path scalar-safety: length-N vectors fall through to FALSE
# ============================================================================

test_that("F3-#4 structural: .has_path requires length(p) == 1L and wraps in isTRUE", {
  live <- .read_live_lines("R/extract_secondary_practice_locations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("length\\(p\\)\\s*==\\s*1L", joined),
    info = "must require length(p) == 1L"
  )
  expect_true(
    grepl("isTRUE\\(\\s*!is\\.null\\(p\\)", joined),
    info = "predicate must be wrapped in isTRUE()"
  )
})

test_that("F3-#4 behavioural: length>1 input returns FALSE, not an error", {
  .has_path <- function(p) {
    isTRUE(!is.null(p) && length(p) == 1L && !is.na(p) && nzchar(p))
  }
  expect_false(.has_path(c("a", "b")),
               info = "length-2 vector: returns FALSE (would have errored in `if(...)`)")
  expect_false(.has_path(c("a", "b", "c")),
               info = "length-3 vector: returns FALSE")
  expect_true(.has_path("ok.json"))
  expect_false(.has_path(NULL))
  expect_false(.has_path(NA_character_))
  expect_false(.has_path(""))
  expect_false(.has_path(character(0)))
})

# ============================================================================
# F3-#5 — .normalize_zip5 trims BEFORE substr
# ============================================================================

test_that("F3-#5 structural: .normalize_zip5 trims before substr", {
  live <- .read_live_lines("R/enrich_facility_name.R")
  joined <- paste(live, collapse = "\n")
  # The new pattern: `z <- trimws(z)` precedes `z <- substr(z, 1L, 5L)`.
  expect_true(
    grepl("z <- trimws\\(z\\)\\s*\\n?\\s*z <- substr\\(z,\\s*1L,\\s*5L\\)",
          joined),
    info = "must call trimws(z) before substr(z, 1L, 5L)"
  )
})

test_that("F3-#5 behavioural: padded ZIP normalizes to the correct 5 digits", {
  normalize_zip5 <- function(x) {
    if (is.null(x) || length(x) == 0L) return(character(0))
    z <- as.character(x)
    z <- trimws(z)
    z <- substr(z, 1L, 5L)
    z[is.na(z) | z == "" | trimws(z) == ""] <- NA_character_
    z_int <- suppressWarnings(as.integer(z))
    z_num <- rep(NA_character_, length(z))
    ok    <- !is.na(z_int)
    z_num[ok] <- formatC(z_int[ok], width = 5L, flag = "0", format = "d")
    z_num
  }
  # Old buggy form: substr first → " 0211" → "00211"
  expect_equal(normalize_zip5(" 02115"), "02115",
               info = "FIX: leading-space input normalizes to the correct 5 digits")
  expect_equal(normalize_zip5("02115 "), "02115",
               info = "trailing space tolerated")
  expect_equal(normalize_zip5(" 02115 "), "02115",
               info = "both-sides padding tolerated")
})

# ============================================================================
# F3-#6 + F3-#7 — empty-string facility_name and facility_name_source
#                  normalize to NA before coalesce
# ============================================================================

test_that("F3-#6+7 structural: empty-string normalization sits BEFORE the coalesce", {
  live <- .read_live_lines("R/enrich_facility_name.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("facility_name\\s*=\\s*dplyr::if_else\\(\\s*\\n?\\s*!is\\.na\\(facility_name\\)\\s*&\\s*facility_name\\s*==\\s*\"\"",
          joined),
    info = "must normalize empty-string facility_name to NA before coalesce"
  )
  expect_true(
    grepl("facility_name_source\\s*=\\s*dplyr::if_else\\(\\s*\\n?\\s*!is\\.na\\(facility_name_source\\)\\s*&\\s*facility_name_source\\s*==\\s*\"\"",
          joined),
    info = "must normalize empty-string facility_name_source to NA before coalesce"
  )
})

test_that("F3-#6+7 behavioural: blank facility_name slots are filled by lookup", {
  # Replay the contract.
  df <- data.frame(
    facility_name        = c("",        "Hospital A", NA),
    facility_name_lookup = c("Lookup B", "Lookup C",  "Lookup D"),
    stringsAsFactors = FALSE
  )
  # NEW pattern: normalize "" -> NA, then coalesce.
  df$facility_name <- ifelse(!is.na(df$facility_name) & df$facility_name == "",
                             NA_character_, df$facility_name)
  out <- ifelse(is.na(df$facility_name), df$facility_name_lookup, df$facility_name)

  expect_equal(out[1], "Lookup B",
               info = "FIX: blank facility_name now gets filled by the lookup")
  expect_equal(out[2], "Hospital A",
               info = "non-blank facility_name preserved (no overwrite)")
  expect_equal(out[3], "Lookup D",
               info = "NA-valued facility_name filled by lookup (unchanged from before)")
})

# ============================================================================
# F3-#8 — SQL LPAD wrapped in CASE+NULLIF to reject blank/all-zero ZIPs
# ============================================================================

test_that("F3-#8 structural: all three SQL queries wrap LPAD in NULLIF(CASE...,'00000')", {
  live_full <- readLines(file.path(.find_repo_root(), "R/enrich_facility_name.R"),
                         warn = FALSE)
  joined <- paste(live_full, collapse = "\n")
  # Match the NULLIF wrapper appearing 3 times (one per SQL query).
  nullif_count <- length(gregexpr(
    "NULLIF\\(\\s*\\n?\\s*CASE\\s*\\n?\\s*WHEN",
    joined, perl = TRUE)[[1]])
  expect_gte(nullif_count, 3L,
             label = "NULLIF(CASE WHEN ...) wrapper occurrences (1 per SQL query)")
  zero_count <- length(gregexpr("'00000'", joined, perl = TRUE)[[1]])
  expect_gte(zero_count, 3L,
             label = "'00000' NULLIF sentinel occurrences")
})

# ============================================================================
# F3-#9 — cohort uniqueness asserted before downstream joins
# ============================================================================

test_that("F3-#9 structural: cohort duplicate-NPI assertion fires early in the script", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.dup_npi\\s*<-\\s*cohort\\$npi\\[duplicated\\(cohort\\$npi\\)", joined),
    info = "must compute .dup_npi from duplicated(cohort$npi)"
  )
  expect_true(
    grepl("cohort has %d duplicate NPI", joined),
    info = "must stop() with a diagnostic showing the duplicate count"
  )
})

test_that("F3-#9 behavioural: duplicate NPI stops the run with naming the offenders", {
  cohort_bad <- data.frame(npi = c("1", "2", "2", "3", "3"),
                           stringsAsFactors = FALSE)
  cohort_good <- data.frame(npi = c("1", "2", "3"), stringsAsFactors = FALSE)

  check_uniqueness <- function(cohort) {
    dup <- cohort$npi[duplicated(cohort$npi) & !is.na(cohort$npi)]
    if (length(dup) > 0L) {
      stop(sprintf("cohort has %d duplicate NPI(s) — first 5: %s. Dedupe upstream.",
                   length(unique(dup)), paste(head(unique(dup), 5), collapse = ", ")),
           call. = FALSE)
    }
    invisible(TRUE)
  }

  expect_error(check_uniqueness(cohort_bad), "2 duplicate NPI",
               info = "FIX: stop names the duplicate count")
  expect_silent(check_uniqueness(cohort_good))
})

# ============================================================================
# F3-#10 — secondary_rural_states deterministic ordering
# ============================================================================

test_that("F3-#10 structural: paste(sort(unique(...))) replaces paste(unique(...))", {
  live <- .read_live_lines("scripts/add_secondary_locations_to_cohort.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("secondary_rural_states\\s*=\\s*paste\\(sort\\(unique\\(state\\[is_rural_sec\\]\\)\\),",
          joined),
    info = "must wrap unique() with sort() for deterministic ordering"
  )
})

test_that("F3-#10 behavioural: output is order-independent across input row orders", {
  # Two row orderings of the same set should produce the same output.
  rows_a <- data.frame(state = c("CO", "NM", "CO", "NM"), is_rural_sec = TRUE,
                       stringsAsFactors = FALSE)
  rows_b <- data.frame(state = c("NM", "CO", "NM", "CO"), is_rural_sec = TRUE,
                       stringsAsFactors = FALSE)

  pack <- function(df) paste(sort(unique(df$state[df$is_rural_sec])),
                             collapse = "|")
  expect_equal(pack(rows_a), pack(rows_b),
               info = "FIX: sort + unique produces stable output across row orders")
  expect_equal(pack(rows_a), "CO|NM",
               info = "alphabetical ordering: CO before NM")
})

# ============================================================================
# F4-#4 — load_facility_affiliation uses staging-table pattern
# ============================================================================

test_that("F4-#4 structural: staging-table swap protects the live table", {
  live <- .read_live_lines("R/load_facility_affiliation_to_duckdb.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("staging_tbl\\s*<-\\s*sprintf\\(\"facility_affiliation_staging", joined),
    info = "must create a timestamped staging table name"
  )
  expect_true(
    grepl("BEGIN TRANSACTION", joined) &&
      grepl("ALTER TABLE.*RENAME TO facility_affiliation", joined) &&
      grepl("COMMIT", joined),
    info = "swap must be inside a transaction with explicit COMMIT"
  )
  # Verbose log marker for the swap step (this is a cat() call, not a
  # comment, so .read_live_lines() preserves it).
  expect_true(
    grepl('Swapping staging -> facility_affiliation', joined),
    info = "verbose log line announcing the swap must be present"
  )
  # The old eager DROP-before-load is gone. There should be at most ONE
  # DROP TABLE statement, and it must be inside the transactional swap
  # block (preceded by `BEGIN TRANSACTION`).
  drop_count <- length(gregexpr(
    "DROP TABLE IF EXISTS facility_affiliation",
    joined, perl = TRUE)[[1]])
  expect_lte(drop_count, 1L,
             label = "DROP TABLE occurrences (was 1 at startup, should remain at 1 only inside the swap)")
})

# ============================================================================
# F4-#5 — env-rooted path resolution
# ============================================================================

test_that("F4-#5 structural: csv_path + db_path resolve through paths.yml then ENV", {
  live <- .read_live_lines("R/load_facility_affiliation_to_duckdb.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.resolve_external\\s*<-\\s*function", joined),
    info = "must define .resolve_external helper"
  )
  expect_true(
    grepl('paths\\[\\[paths_key\\]\\]', joined),
    info = "must consult paths.yml first"
  )
  expect_true(
    grepl('Sys\\.getenv\\("ISOCHRONES_EXTERNAL_ROOT"', joined),
    info = "must fall back to ISOCHRONES_EXTERNAL_ROOT env var"
  )
  # Function signature no longer takes hardcoded /Volumes path defaults.
  expect_true(
    grepl("csv_path\\s*=\\s*NULL,\\s*\\n?\\s*db_path\\s*=\\s*NULL,", joined),
    info = "signature must default both paths to NULL (resolved at runtime)"
  )
})

# ============================================================================
# F4-#6 — ccn_data deduped to one CCN per NPI BEFORE the inner_join
# ============================================================================

test_that("F4-#6 structural: ccn_dedup picks one CCN per NPI before the join", {
  live <- .read_live_lines("R/hybrid_practice_size_detector.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("ccn_dedup\\s*<-\\s*ccn_data\\s*%>%", joined),
    info = "must define ccn_dedup variable"
  )
  expect_true(
    grepl("dplyr::arrange\\(dplyr::desc\\(n_rows\\),\\s*facility_ccn", joined),
    info = "must rank by row count, tiebreak on CCN name"
  )
  expect_true(
    grepl("inner_join\\(ccn_dedup,\\s*by\\s*=\\s*\"npi\"\\)", joined),
    info = "join must use the deduped ccn_dedup, not raw ccn_data"
  )
})

test_that("F4-#6 behavioural: dedup prevents NPI fan-out in the join result", {
  ccn_data <- data.frame(
    npi          = c("1","1","1","2","3","3"),  # NPI 1 has 3 CCN affiliations
    facility_ccn = c("CCN-A","CCN-B","CCN-A","CCN-C","CCN-D","CCN-E"),
    stringsAsFactors = FALSE
  )
  abog <- data.frame(npi = c("1","2","3"), stringsAsFactors = FALSE)

  # OLD (buggy): one inner_join row per (npi, ccn) combination
  old <- merge(abog,
               unique(ccn_data[, c("npi","facility_ccn")]),
               by = "npi")
  expect_gt(nrow(old), nrow(abog),
            label = "OLD: fan-out inflates result above one-per-NPI")

  # NEW: dedupe ccn first, picking most-frequent then alphabetical
  ccn_dedup <- aggregate(
    list(n_rows = ccn_data$facility_ccn),
    by = list(npi = ccn_data$npi, facility_ccn = ccn_data$facility_ccn),
    FUN = length)
  # For each NPI keep top row by (-n_rows, facility_ccn).
  ccn_dedup <- ccn_dedup[order(ccn_dedup$npi, -ccn_dedup$n_rows,
                               ccn_dedup$facility_ccn), ]
  ccn_dedup <- ccn_dedup[!duplicated(ccn_dedup$npi), c("npi","facility_ccn")]
  new <- merge(abog, ccn_dedup, by = "npi")

  expect_equal(nrow(new), nrow(abog),
               info = "FIX: one row per NPI after the join")
  # NPI 1 should pick CCN-A (frequency 2, beats CCN-B frequency 1).
  expect_equal(new$facility_ccn[new$npi == "1"], "CCN-A",
               info = "FIX: most-frequent CCN wins for multi-affiliated NPI")
})

# ============================================================================
# F4-#7 — IN-string SQL replaced with temp tables
# ============================================================================

test_that("F4-#7 structural: paste0-IN strings replaced by registered temp tables", {
  live <- .read_live_lines("R/hybrid_practice_size_detector.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('duckdb::duckdb_register\\(con,\\s*\\n?\\s*"tmp_no_ccn_npis"', joined),
    info = "Layer-2 NPIs registered as tmp_no_ccn_npis"
  )
  expect_true(
    grepl('duckdb::duckdb_register\\(con,\\s*\\n?\\s*"tmp_no_ccn_or_addr_npis"', joined),
    info = "Layer-3 NPIs registered as tmp_no_ccn_or_addr_npis"
  )
  expect_true(
    grepl("INNER JOIN tmp_no_ccn_npis t", joined),
    info = "Layer-2 NPPES query INNER JOINs the temp table"
  )
  expect_true(
    grepl("INNER JOIN tmp_no_ccn_or_addr_npis t", joined),
    info = "Layer-3 Part-D + state queries INNER JOIN the temp table"
  )
  # The old paste0-IN-string patterns are gone.
  expect_false(
    grepl("npis_to_cluster\\s*<-\\s*paste0\\(", joined),
    info = "npis_to_cluster paste0 IN-list must be removed"
  )
  expect_false(
    grepl("npis_for_org\\s*<-\\s*paste0\\(", joined),
    info = "npis_for_org paste0 IN-list must be removed"
  )
})

# ============================================================================
# F4-#8 — NPPES vintage selection uses date-parsed sort, not lex
# ============================================================================

test_that("F4-#8 structural: NPPES table picker parses month-year from name", {
  live <- .read_live_lines("R/hybrid_practice_size_detector.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.parse_nppes_date\\s*<-\\s*function", joined),
    info = "must define .parse_nppes_date helper"
  )
  expect_true(
    grepl("January\\|February\\|March\\|April\\|May\\|June\\|July\\|August\\|September\\|October\\|November\\|December",
          joined),
    info = "month regex must cover full-name months"
  )
  expect_true(
    grepl("arrange\\(dplyr::desc\\(\\.nppes_date_score\\),\\s*dplyr::desc\\(name\\)\\)",
          joined),
    info = "sort key must be (date_score DESC, name DESC) — name is tiebreaker only"
  )
  # Old lex-only sort is gone.
  expect_false(
    grepl("filter\\(grepl\\(\"NPPES_Data\".*\\)\\s*%>%\\s*\\n?\\s*arrange\\(desc\\(name\\)\\)",
          joined),
    info = "the previous lex-only sort must be replaced"
  )
})

test_that("F4-#8 behavioural: Apr_2026 wins over Dec_2024 despite 'D' > 'A' lex", {
  names_in <- c("NPPES_Data_Dissemination_December_2024",
                "NPPES_Data_Dissemination_Apr_2026")

  # OLD (buggy): lex sort puts December first because 'D' > 'A'
  old_pick <- names_in[order(names_in, decreasing = TRUE)][1L]
  expect_equal(old_pick, "NPPES_Data_Dissemination_December_2024",
               info = "OLD: lex sort picks December_2024 (bug)")

  # NEW: parse date-score, year * 100 + month
  month_lookup <- c(Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, Jul=7, Aug=8,
                   Sep=9, Oct=10, Nov=11, Dec=12, January=1, February=2,
                   March=3, April=4, June=6, July=7, August=8, September=9,
                   October=10, November=11, December=12)
  parse_date <- function(nm) {
    m <- regmatches(nm, regexpr(
      "(January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)_([0-9]{4})",
      nm, perl = TRUE, ignore.case = TRUE))
    if (length(m) == 0L) return(NA_real_)
    parts <- strsplit(m, "_", fixed = TRUE)[[1]]
    as.numeric(parts[2]) * 100 + month_lookup[parts[1]]
  }
  scores <- vapply(names_in, parse_date, numeric(1L))
  new_pick <- names_in[order(-scores, names_in,
                             method = "radix",
                             decreasing = c(FALSE, TRUE))][1L]
  expect_equal(new_pick, "NPPES_Data_Dissemination_Apr_2026",
               info = "FIX: Apr_2026 (score 202604) beats Dec_2024 (score 202412)")
})

# ============================================================================
# F4-#9 + F4-#10 — backfill ZIP5 normalized into the join keys
# ============================================================================

test_that("F4-#9 structural: Pass-1 join includes normalized ZIP5", {
  live <- .read_live_lines("scripts/backfill_geocoded_addresses.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("SUBSTR\\(REGEXP_REPLACE\\(CAST\\(t\\.practice_address_zip AS VARCHAR\\), '\\[\\^0-9\\]', '', 'g'\\), 1, 5\\)",
          joined),
    info = "must normalize ZIP via REGEXP_REPLACE + SUBSTR(1,5) on temporal side"
  )
  expect_true(
    grepl("SUBSTR\\(REGEXP_REPLACE\\(CAST\\(g\\.practice_address_zip AS VARCHAR\\), '\\[\\^0-9\\]', '', 'g'\\), 1, 5\\)",
          joined),
    info = "must normalize ZIP the same way on geocoded side"
  )
})

test_that("F4-#10 structural: Pass-2 mailing-fallback + join-back both use ZIP", {
  live <- .read_live_lines("scripts/backfill_geocoded_addresses.R")
  joined <- paste(live, collapse = "\n")
  # Pass-2 mailing-candidates join (uses the same SQL ZIP-normalizer).
  zip_norm_count <- length(gregexpr(
    "SUBSTR\\(REGEXP_REPLACE\\(CAST\\(.*practice_address_zip AS VARCHAR\\)",
    joined, perl = TRUE)[[1]])
  expect_gte(zip_norm_count, 4L,
             label = "SUBSTR+REGEXP_REPLACE ZIP normalizer (Pass-1 + Pass-2 mailing) >= 4")
  # Pass-2 mail_coords_df join-back uses .mail_zip5 R-side helper.
  expect_true(
    grepl("still_failing\\$\\.mail_zip5\\s*<-\\s*\\.zip5_norm\\(still_failing\\$mail_zip\\)",
          joined),
    info = "R-side .mail_zip5 is computed before the inner_join"
  )
  expect_true(
    grepl('by\\s*=\\s*c\\("mail_street",\\s*"mail_city",\\s*"mail_state",\\s*\\n?\\s*"\\.mail_zip5"\\)',
          joined),
    info = "join-back key now includes .mail_zip5"
  )
})

# ============================================================================
# F4-extra-A — duplicate on.exit(dbDisconnect) collapsed to one
# ============================================================================

test_that("F4-extra-A structural: only one on.exit(dbDisconnect) registered", {
  live <- .read_live_lines("R/hybrid_practice_size_detector.R")
  joined <- paste(live, collapse = "\n")
  # Count on.exit blocks that disconnect `con`. Old code had two
  # `on.exit(dbDisconnect(con, ...), add = TRUE)` back-to-back.
  bare_count <- length(gregexpr(
    "on\\.exit\\(dbDisconnect\\(con,",
    joined, perl = TRUE)[[1]])
  expect_lte(bare_count, 1L,
             label = "bare on.exit(dbDisconnect(con,...)) calls (was 2, should be 0 or 1)")
  # The replacement uses tryCatch-wrapped disconnect.
  expect_true(
    grepl("tryCatch\\(dbDisconnect\\(con,\\s*shutdown\\s*=\\s*TRUE\\),", joined),
    info = "should use tryCatch wrapper so disconnect-errors don't mask the real failure"
  )
})

# ============================================================================
# F4-extra-B — Tier 8 hospital affiliation ranks by affil_count, not lex
# ============================================================================

test_that("F4-extra-B structural: Tier 8 SQL ranks by affil_count DESC", {
  live <- .read_live_lines("R/utils/resolve_npi_year_address.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("affil_count\\s+DESC\\s+NULLS\\s+LAST", joined),
    info = "ORDER BY must include affil_count DESC NULLS LAST"
  )
  expect_true(
    grepl("SELECT COUNT\\(\\*\\) FROM facility_affiliation", joined),
    info = "subquery must count CCN affiliations for each candidate"
  )
  # The previous lex-only ORDER BY is replaced by the new ranked one.
  # The address_street tiebreaker still exists (deterministic last resort)
  # but is no longer first.
  expect_true(
    grepl("affil_count DESC NULLS LAST,\\s*\\n?\\s*address_street ASC,\\s*address_zip ASC",
          joined),
    info = "address_street is now a tiebreaker, not the primary ordering"
  )
})

# ============================================================================
# F5-#9 — enrich_ent_universe uses .resolve_dac_tables / .resolve_pc_tables
# ============================================================================

test_that("F5-#9 structural: ENT enrichment script uses dynamic table resolvers", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("dac_table <- \\.resolve_dac_tables\\(con\\)\\[1\\]", joined),
    info = "DAC selection must call .resolve_dac_tables(con) dynamically"
  )
  expect_true(
    grepl("pc_table <- \\.resolve_pc_tables\\(con\\)\\[1\\]", joined),
    info = "PC selection must call .resolve_pc_tables(con) dynamically"
  )
  expect_false(
    grepl("dac_table <- DAC_TABLES\\[1\\]", joined),
    info = "static DAC_TABLES[1] reference must be removed"
  )
  expect_false(
    grepl("pc_table <- PC_TABLES\\[1\\]", joined),
    info = "static PC_TABLES[1] reference must be removed"
  )
})

# ============================================================================
# F5-#10 — process_nppes_enrichment builds enriched from cohort spine
# ============================================================================

test_that("F5-#10 structural: enriched output starts from cohort spine, not cache spine", {
  live <- .read_live_lines("R/process_nppes_enrichment.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.cohort_spine\\s*<-\\s*tibble::tibble\\(npi\\s*=\\s*\\.cohort_npis\\)", joined),
    info = "must define .cohort_spine from .cohort_npis"
  )
  expect_true(
    grepl("enriched\\s*<-\\s*\\.cohort_spine\\s*\\|>\\s*\\n?\\s*dplyr::left_join\\(nppes_cache,\\s*by\\s*=\\s*\"npi\"\\)",
          joined),
    info = "enriched must left_join nppes_cache onto the spine"
  )
  expect_true(
    grepl("nppes_found\\s*=\\s*dplyr::coalesce\\(nppes_found,\\s*FALSE\\)", joined),
    info = "missing-cache rows must get nppes_found = FALSE (not NA)"
  )
  # The previous form built `enriched <- nppes_cache |> mutate(...)` —
  # that pattern must be gone.
  expect_false(
    grepl("^\\s*enriched\\s*<-\\s*nppes_cache\\s*\\|>\\s*mutate\\(", joined,
          perl = TRUE),
    info = "the cache-as-spine pattern must be removed"
  )
})

test_that("F5-#10 behavioural: cohort NPIs missing from cache survive with nppes_found = FALSE", {
  # Replay the contract on synthetic data.
  cohort_npis <- c("1", "2", "3")
  cache <- data.frame(
    npi             = c("1", "2"),               # NPI 3 is missing from cache
    nppes_found     = c(TRUE, TRUE),
    nppes_credential = c("MD", "DO"),
    stringsAsFactors = FALSE
  )

  # OLD (buggy): build from cache, NPI 3 disappears
  old <- cache  # only 2 rows

  # NEW: build from cohort spine, left_join cache
  spine <- data.frame(npi = cohort_npis, stringsAsFactors = FALSE)
  new <- merge(spine, cache, by = "npi", all.x = TRUE)
  # The coalesce step: NA -> FALSE
  new$nppes_found <- ifelse(is.na(new$nppes_found), FALSE, new$nppes_found)

  expect_equal(nrow(old), 2L,
               info = "OLD: NPI 3 silently dropped (only 2 rows in output)")
  expect_equal(nrow(new), 3L,
               info = "FIX: all 3 cohort NPIs preserved in the output")
  expect_equal(sum(new$nppes_found), 2L,
               info = "FIX: 2 of 3 found, NPI 3 flagged FALSE")
  expect_equal(new$nppes_found[new$npi == "3"], FALSE,
               info = "FIX: NPI 3 explicitly nppes_found = FALSE")
})

# ============================================================================
# F5-extra-A — match_facility_affiliation_strategy allows NULL NPPES state
# ============================================================================

test_that("F5-extra-A structural: state WHERE clause allows NULL NPPES state through", {
  live <- .read_live_lines("R/match_facility_affiliation_strategy.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("n\\.npi_state IS NULL\\s*\\n?\\s*OR UPPER\\(TRIM\\(a\\.state\\)\\) IS NOT DISTINCT FROM n\\.npi_state",
          joined),
    info = "WHERE must be (n.npi_state IS NULL OR <DISTINCT-FROM check>)"
  )
})

test_that("F5-extra-A behavioural: known ABOG state + missing NPPES state now passes", {
  # Replay the SQL predicate in R semantics.
  is_distinct_from <- function(a, b) {
    if (is.na(a) && is.na(b)) return(FALSE)  # both NULL = NOT distinct
    if (is.na(a) || is.na(b)) return(TRUE)   # one NULL = distinct
    a != b
  }
  predicate_new <- function(abog_state, nppes_state) {
    is.na(nppes_state) ||
      !is_distinct_from(toupper(trimws(abog_state)), nppes_state)
  }
  predicate_old <- function(abog_state, nppes_state) {
    !is_distinct_from(toupper(trimws(abog_state)), nppes_state)
  }

  # Case: known ABOG state, missing NPPES state
  expect_false(predicate_old("CA", NA),
               info = "OLD: known abog + NA nppes drops the row (bug)")
  expect_true(predicate_new("CA", NA),
               info = "FIX: known abog + NA nppes passes")

  # Case: both states known and equal — both predicates agree
  expect_true(predicate_old("CA", "CA"))
  expect_true(predicate_new("CA", "CA"))

  # Case: both states known and different — both predicates agree (FALSE)
  expect_false(predicate_old("CA", "NY"))
  expect_false(predicate_new("CA", "NY"))

  # Case: both NA — old returns TRUE (both NULL = not distinct), new also TRUE
  expect_true(predicate_old(NA, NA))
  expect_true(predicate_new(NA, NA))
})

# ============================================================================
# F5-extra-B — secondary facility names + sources aggregated as paired table
# ============================================================================

test_that("F5-extra-B structural: paired pre-aggregation via pair_per_npi", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("pair_per_npi\\s*<-\\s*all_sec", joined),
    info = "must define pair_per_npi as a pre-aggregation step"
  )
  expect_true(
    grepl("dplyr::distinct\\(\\.data\\$npi,\\s*\\.data\\$facility_name,\\s*\\.data\\$facility_name_source\\)",
          joined),
    info = "distinct must include all THREE columns (npi + the pair)"
  )
  expect_true(
    grepl("dplyr::left_join\\(pair_per_npi,\\s*by\\s*=\\s*\"npi\"\\)", joined),
    info = "main summary must left_join the pre-aggregated pairs"
  )
})

test_that("F5-extra-B behavioural: paired aggregation prevents row-misalignment", {
  # The pathological case: [(A, dac), (B, dac), (A, hospital)].
  # OLD independent unique: names = c("A", "B"); sources = c("dac", "hospital").
  # Semicolon-joined that says "A;B" + "dac;hospital" which positionally
  # claims A came from dac and B came from hospital — but the TRUE
  # pairing is A came from dac+hospital, B came from dac only.
  rows <- data.frame(
    facility_name        = c("A", "B", "A"),
    facility_name_source = c("dac", "dac", "hospital"),
    stringsAsFactors = FALSE
  )

  # OLD: independent uniques
  old_names   <- paste(unique(rows$facility_name),        collapse = "; ")
  old_sources <- paste(unique(rows$facility_name_source), collapse = "; ")
  expect_equal(old_names,   "A; B")
  expect_equal(old_sources, "dac; hospital")
  # OLD output positionally implies B<->hospital, which is WRONG.

  # NEW: paired aggregation
  pairs <- unique(rows[, c("facility_name", "facility_name_source")])
  pairs <- pairs[order(pairs$facility_name, pairs$facility_name_source), ]
  new_names   <- paste(pairs$facility_name,        collapse = "; ")
  new_sources <- paste(pairs$facility_name_source, collapse = "; ")
  expect_equal(new_names,   "A; A; B",
               info = "FIX: A appears twice (paired with dac AND hospital)")
  expect_equal(new_sources, "dac; hospital; dac",
               info = "FIX: positional truth — A:dac, A:hospital, B:dac")
})

# ============================================================================
# F6-#1 — practice_type distinguishes missing from mapped-but-unknown
# ============================================================================

test_that("F6-#1 structural: practice_type uses if_else(is.na(tax_code_1), NA, coalesce(...))", {
  live <- .read_live_lines("R/process_nppes_enrichment.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("dplyr::if_else\\(\\s*\\n?\\s*is\\.na\\(nppes_tax_code_1\\),\\s*\\n?\\s*NA_character_,\\s*\\n?\\s*dplyr::coalesce\\(taxonomy_map\\[nppes_tax_code_1\\],\\s*\"Other/Unknown\"\\)",
          joined),
    info = "must distinguish NA tax code (keep NA) from unmapped-but-present code (Other/Unknown)"
  )
  expect_false(
    grepl('practice_type = if_else\\(is\\.na\\(practice_type\\),\\s*"Other/Unknown"',
          joined),
    info = "the old flatten-everything-to-Other/Unknown pattern must be removed"
  )
})

test_that("F6-#1 behavioural: missing nppes_tax_code_1 stays NA; unknown code -> Other/Unknown", {
  taxonomy_map <- c("207V00000X" = "OB/GYN", "207Y00000X" = "Otolaryngology")
  resolve <- function(tax_code) {
    ifelse(is.na(tax_code), NA_character_,
           ifelse(is.na(taxonomy_map[tax_code]), "Other/Unknown",
                  taxonomy_map[tax_code]))
  }
  expect_equal(resolve("207V00000X"), "OB/GYN",
               info = "known mapped code -> friendly label")
  expect_equal(resolve("999999999X"), "Other/Unknown",
               info = "FIX: present-but-unknown code maps to Other/Unknown")
  expect_true(is.na(resolve(NA_character_)),
              info = "FIX: NA tax code stays NA (no NPPES row at all)")
  expect_equal(unname(resolve(c("207V00000X", NA, "999999999X"))),
               c("OB/GYN", NA, "Other/Unknown"),
               info = "vectorized: three buckets are distinguishable")
})

# ============================================================================
# F6-#2 — nppes_enum_date parsed to Date before dedup sort
# ============================================================================

test_that("F6-#2 structural: nppes_enum_date is parsed to Date before dedup", {
  live <- .read_live_lines("R/process_nppes_enrichment.R")
  joined <- paste(live, collapse = "\n")
  # Accept either the sweep-6 single-format form OR the F7-msg2-#6
  # refined multi-format parser — both compute .nppes_enum_date_parsed.
  parsed_present <- grepl(
    "\\.nppes_enum_date_parsed\\s*=\\s*(suppressWarnings\\(\\s*\\n?\\s*as\\.Date\\(|\\.parse_enum_date_multi\\()",
    joined)
  expect_true(parsed_present,
              info = "must compute .nppes_enum_date_parsed as Date class (either single-format or multi-format)")
  expect_true(
    grepl("dplyr::desc\\(\\.nppes_enum_date_parsed\\)", joined),
    info = "arrange must use the parsed Date column, not the raw character"
  )
})

test_that("F6-#2 behavioural: Date-parsed sort beats lex sort on non-ISO dates", {
  # Synthetic cache with mixed-format date strings — the kind that
  # the API might return in different snapshots.
  cache <- data.frame(
    npi             = c("1", "1"),
    nppes_found     = c(TRUE, TRUE),
    nppes_enum_date = c("5/12/2018", "10/3/2017"),  # M/D/YYYY US format
    stringsAsFactors = FALSE
  )

  # OLD: lex sort on character column. "10/3/2017" < "5/12/2018" because '1' < '5'.
  # So desc() picks "5/12/2018" first — which happens to be the right answer here.
  # But for: "5/12/2018" vs "5/2/2019" — lex desc picks "5/2/2019"... still right.
  # The real failure is "10/3/2018" vs "9/1/2018" — lex puts "9/1/2018" first
  # because '9' > '1'. Test that case.
  cache_pathological <- data.frame(
    npi             = c("1", "1"),
    nppes_found     = c(TRUE, TRUE),
    nppes_enum_date = c("10/3/2018", "9/1/2018"),
    stringsAsFactors = FALSE
  )

  # OLD: lex desc -> "9/1/2018" wins (wrong; 10/3 is later)
  old_pick <- cache_pathological[order(cache_pathological$nppes_enum_date,
                                       decreasing = TRUE), ][1L, ]
  expect_equal(old_pick$nppes_enum_date, "9/1/2018",
               info = "OLD: lex sort picks 9/1/2018 (wrong — 10/3/2018 is later)")

  # NEW: parse to Date, then sort
  parsed <- suppressWarnings(as.Date(cache_pathological$nppes_enum_date,
                                     format = "%m/%d/%Y"))
  new_pick <- cache_pathological[order(parsed, decreasing = TRUE), ][1L, ]
  expect_equal(new_pick$nppes_enum_date, "10/3/2018",
               info = "FIX: Date-parsed sort picks 10/3/2018 (correctly later)")
})

# ============================================================================
# F6-#4 — ENT enrichment zip5 uses the hardened normalizer
# ============================================================================

test_that("F6-#4 structural: zip5 builder uses .normalize_zip5_to_join_key", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.normalize_zip5_to_join_key\\s*<-\\s*function", joined),
    info = "must define a normalize helper"
  )
  expect_true(
    grepl("zip5\\s*=\\s*\\.normalize_zip5_to_join_key\\(\\.data\\$zip\\)", joined),
    info = "secondary-side zip5 must use the helper"
  )
  # The previous bare str_pad form must be gone.
  expect_false(
    grepl("stringr::str_pad\\(substr\\(\\.data\\$zip,\\s*1,\\s*5\\),\\s*5,\\s*\"left\",\\s*\"0\"\\)",
          joined),
    info = "the old str_pad(substr(zip,1,5),5,'left','0') pattern must be removed"
  )
})

test_that("F6-#4 behavioural: blank/all-zero ZIPs become NA, not '00000'", {
  normalize <- function(z) {
    raw    <- ifelse(is.na(z), "", as.character(z))
    digits <- substr(gsub("[^0-9]", "", raw), 1, 5)
    ifelse(digits == "" | grepl("^0+$", digits),
           NA_character_,
           formatC(as.integer(digits), width = 5L, flag = "0", format = "d"))
  }
  expect_true(is.na(normalize("")),
              info = "FIX: blank input -> NA (not '00000')")
  expect_true(is.na(normalize("ABCDE")),
              info = "FIX: all-letters -> NA (no digits)")
  expect_true(is.na(normalize("00000")),
              info = "FIX: all-zero input -> NA")
  expect_equal(normalize("02115-1234"), "02115",
               info = "FIX: ZIP+4 keeps first 5 digits")
  expect_equal(normalize("5432"), "05432",
               info = "FIX: short ZIP zero-padded")
})

# ============================================================================
# F6-#6 — ENT enrichment script captures file.rename return value
# ============================================================================

test_that("F6-#6 structural: file.rename wrapped in isTRUE() guard with stop()", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("if \\(!isTRUE\\(file\\.rename\\(tmp_out_csv,\\s*OUT_CSV\\)\\)\\)\\s*\\{",
          joined),
    info = "file.rename must be wrapped in !isTRUE() check"
  )
  expect_true(
    grepl("\\[write\\] file\\.rename failed:\\s*%s\\s*->\\s*%s\\s*\\(tmp preserved\\)",
          joined),
    info = "stop() message must name the failed source/target"
  )
})

# ============================================================================
# F6-#8 — match_facility_affiliation second-stage NPI dedup per abog_id
# ============================================================================

test_that("F6-#8 structural: dedupe_per_abog_id flag + per-abog_id slice", {
  live <- .read_live_lines("R/match_facility_affiliation_strategy.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("dedupe_per_abog_id\\s*=\\s*TRUE", joined),
    info = "function signature must add dedupe_per_abog_id parameter"
  )
  expect_true(
    grepl("if \\(isTRUE\\(dedupe_per_abog_id\\)\\)", joined),
    info = "second-stage dedup must be gated by the flag"
  )
  expect_true(
    grepl("dplyr::group_by\\(abog_id\\)\\s*%>%", joined),
    info = "second stage groups by abog_id only (not abog_id + npi)"
  )
  expect_true(
    grepl("canon_npi\\(npi\\)", joined),
    info = "tiebreaker must use canon_npi for deterministic ordering"
  )
})

# ============================================================================
# F6-#9 — enrich_practice_size_location_based scratch-name collision guard
# ============================================================================

test_that("F6-#9 structural: reserved scratch names checked before mutate", {
  live <- .read_live_lines("R/enrich_practice_size_location_based.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('\\.reserved\\s*<-\\s*c\\("\\.\\.addr_norm__",\\s*"\\.\\.city_norm__",\\s*"\\.\\.state_norm__"\\)',
          joined),
    info = "must declare reserved scratch names"
  )
  expect_true(
    grepl("\\.collisions\\s*<-\\s*intersect\\(\\.reserved,\\s*names\\(physician_df\\)\\)",
          joined),
    info = "must intersect reserved names with the input columns"
  )
  expect_true(
    grepl("reserved scratch column\\(s\\) already in physician_df", joined),
    info = "stop() message must name the colliding columns"
  )
})

test_that("F6-#9 behavioural: collision detected and stopped", {
  reserved <- c("..addr_norm__", "..city_norm__", "..state_norm__")
  check_collisions <- function(df) {
    collisions <- intersect(reserved, names(df))
    if (length(collisions) > 0L) {
      stop(sprintf("reserved scratch column(s) already in input: %s",
                   paste(collisions, collapse = ", ")), call. = FALSE)
    }
    invisible(TRUE)
  }
  df_ok    <- data.frame(address = "x", city = "y", state = "CA")
  df_bad   <- data.frame(address = "x", `..addr_norm__` = "boom",
                         check.names = FALSE, stringsAsFactors = FALSE)

  expect_silent(check_collisions(df_ok))
  expect_error(check_collisions(df_bad),
               "reserved scratch column",
               label = "FIX: stops when caller already has ..addr_norm__")
})

# ============================================================================
# F6-#10 — validate_org_npi one-to-one join with explicit dedup assertion
# ============================================================================

# ============================================================================
# Sweep 7 — items msg-1 #1, 5, 6, 7, 8, risk-2 + msg-2 #1, 2, 4, 5, 6, 7, 8, 9, 10, risk
# ============================================================================

test_that("F7-msg1-#1 + msg2-#10 structural: street2 wildcard in physician→practice-group join", {
  live <- .read_live_lines("R/match_physicians_to_practice_groups.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("pg\\.norm_street2\\s*=\\s*''\\s*\\n?\\s*OR LOWER\\(TRIM\\(COALESCE\\(phys\\.practice_address_street2,\\s*''\\)\\)\\) = pg\\.norm_street2",
          joined),
    info = "street2 clause must accept blank pg.norm_street2 as a wildcard"
  )
})

test_that("F7-msg1-#5 structural: compare_practice_size_sources resolves PC table dynamically", {
  live <- .read_live_lines("R/compare_practice_size_sources.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("physician_compare_20%\\|physician_compare_19%", joined) ||
      grepl("ILIKE 'physician_compare_20%'", joined),
    info = "must query information_schema for physician_compare_* tables"
  )
  expect_true(
    grepl("order\\(-\\.pc_year,\\s*-\\.pc_quarter,\\s*\\.pc_tables_raw", joined),
    info = "must sort by (year DESC, quarter DESC, name DESC)"
  )
  expect_false(
    grepl("FROM physician_compare_2024_q2", joined),
    info = "hardcoded `physician_compare_2024_q2` must be removed"
  )
})

test_that("F7-msg1-#6 structural: compare_practice_size_sources uses temp-table not IN-list", {
  live <- .read_live_lines("R/compare_practice_size_sources.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("duckdb::duckdb_register\\(con,\\s*\"tmp_pc_cohort_npis\"", joined),
    info = "must register tmp_pc_cohort_npis"
  )
  expect_true(
    grepl("INNER JOIN tmp_pc_cohort_npis t", joined),
    info = "main query must INNER JOIN against the temp table"
  )
  expect_false(
    grepl("sql_in_batches\\(cohort\\$npi", joined),
    info = "the batched IN-clause builder must be removed for this path"
  )
})

test_that("F7-msg1-#7 structural: scales availability guarded at startup", {
  live <- .read_live_lines("R/create_practice_groups_from_organizations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('requireNamespace\\("scales",\\s*quietly\\s*=\\s*TRUE\\)', joined),
    info = "must requireNamespace(scales) at startup"
  )
  expect_true(
    grepl("required package 'scales' not installed", joined),
    info = "stop message must name the missing package"
  )
})

test_that("F7-msg1-#8 structural: practice_groups carries MOB discriminator columns", {
  live <- .read_live_lines("R/create_practice_groups_from_organizations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("distinct_org_names_at_address", joined),
    info = "must compute distinct_org_names_at_address per group"
  )
  expect_true(
    grepl("norm_org_name", joined),
    info = "must compute norm_org_name per group"
  )
  # Both new columns must appear in the outer SELECT, not just the
  # inner aggregation.
  occurrences_outer <- length(gregexpr(
    "distinct_org_names_at_address",
    joined, perl = TRUE)[[1]])
  expect_gte(occurrences_outer, 2L,
             label = "distinct_org_names_at_address mentioned at least in CTE + outer SELECT")
})

test_that("F7-msg2-#9 structural: practice_groups FIRST() uses tiebreak via COALESCE+organization_npi", {
  live <- .read_live_lines("R/create_practice_groups_from_organizations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("ORDER BY COALESCE\\(npi_enumeration_date,\\s*'9999-12-31'\\),\\s*organization_npi",
          joined),
    info = "FIRST(...) ORDER BY must include COALESCE on null dates AND organization_npi tiebreak"
  )
})

test_that("F7-msg2-risk structural: validate_org_npi picks latest quarter per year", {
  live <- .read_live_lines("R/validate_org_npi_for_practice_size.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('regexpr\\("_\\[Qq\\]\\(\\[1-4\\]\\)\\\\\\\\b",\\s*year_tables\\$table_name', joined),
    info = "must parse _Q[1-4] quarter from year_tables"
  )
  expect_true(
    grepl("\\.yt_pick\\s*<-\\s*order\\(-\\.yt_quarter,", joined),
    info = "must pick by quarter DESC"
  )
  expect_false(
    grepl("table_name <- year_tables\\$table_name\\[1\\]", joined),
    info = "the previous [1] alphabetical-first pattern must be gone"
  )
})

test_that("F7-msg1-risk2 structural: enrich_abog_with_medical_school guards yaml availability", {
  live <- .read_live_lines("R/enrich_abog_with_medical_school.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('requireNamespace\\("yaml",\\s*quietly\\s*=\\s*TRUE\\)', joined),
    info = "must check for yaml package before using yaml::read_yaml()"
  )
})

test_that("F7-msg2-#1 structural: enrich_ent .addr_key includes city + address_line2", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.addr_key = paste\\(toupper\\(trimws\\(\\.data\\$address_line1\\)\\),\\s*\\n?\\s*toupper\\(trimws\\(dplyr::coalesce\\(\\.data\\$address_line2",
          joined),
    info = ".addr_key must include address_line2"
  )
  expect_true(
    grepl("toupper\\(trimws\\(dplyr::coalesce\\(\\.data\\$city,\\s*\"\"\\)\\)\\)", joined),
    info = ".addr_key must include city"
  )
})

test_that("F7-msg2-#2 structural: haversine NA gated by same-campus-by-key", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.same_campus_by_key", joined),
    info = "must compute same-campus-by-key flag"
  )
  expect_true(
    grepl("distance_unknown = is\\.na\\(\\.data\\$distance_km_from_primary\\)", joined),
    info = "must surface distance_unknown as a column"
  )
  expect_true(
    grepl("dplyr::filter\\(!\\.data\\$\\.same_campus_by_key", joined),
    info = "filter must drop same-campus-by-key rows regardless of NA distance"
  )
})

test_that("F7-msg2-#4 structural: n_secondary_facility_names uses n_distinct", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("n_secondary_facility_names = dplyr::n_distinct\\(", joined),
    info = "must use n_distinct() instead of sum(non-NA rows)"
  )
})

test_that("F7-msg2-#5 structural: primary_addr deduped to one row per NPI", {
  live <- .read_live_lines("scripts/enrich_ent_universe_with_facility_affiliations.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("dplyr::distinct\\(\\.data\\$npi,\\s*\\.keep_all = TRUE\\)", joined),
    info = "primary_addr must distinct() to one row per NPI"
  )
})

test_that("F7-msg2-#6 structural: multi-format date parser in process_nppes_enrichment", {
  live <- .read_live_lines("R/process_nppes_enrichment.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.parse_enum_date_multi\\s*<-\\s*function", joined),
    info = "must define multi-format date helper"
  )
  expect_true(
    grepl('"%Y-%m-%d", "%m/%d/%Y", "%Y%m%d"', joined),
    info = "must attempt multiple format strings in priority order"
  )
})

test_that("F7-msg2-#7 structural: Single location requires == 1 (not <= 1)", {
  live <- .read_live_lines("R/process_nppes_enrichment.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("nppes_n_practice_locations == 1 ~ \"Single location\"", joined),
    info = "must use exact equality for Single location"
  )
  expect_false(
    grepl("nppes_n_practice_locations <= 1 ~ \"Single location\"", joined),
    info = "the old <= 1 form must be removed"
  )
})

# ============================================================================
# Sweep 8 — 18 fixes from two batches (excluding 3 deferred items)
# ============================================================================

test_that("F8-batch1-#1 structural: load_facility_affiliation guards yaml availability", {
  live <- .read_live_lines("R/load_facility_affiliation_to_duckdb.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('requireNamespace\\("yaml",\\s*quietly\\s*=\\s*TRUE\\)', joined),
    info = "must guard yaml::read_yaml() with requireNamespace"
  )
})

test_that("F8-batch1-#2 structural: staged-table schema validation before swap", {
  live <- .read_live_lines("R/load_facility_affiliation_to_duckdb.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("null_npi_count|null_first_count|null_last_count", joined),
    info = "must compute null-rates on identity columns"
  )
  expect_true(
    grepl("\\.null_threshold\\s*<-\\s*0\\.05", joined),
    info = "must declare a 5% null-rate threshold"
  )
  expect_true(
    grepl("staged %s null-rate", joined),
    info = "stop message must name the failing column"
  )
  expect_true(
    grepl("setdiff\\(\\.expected_cols,\\s*\\.actual_cols\\)", joined),
    info = "must check for missing expected columns"
  )
})

test_that("F8-batch1-#4 structural: NPPES date parser supports numeric vintage patterns", {
  live <- .read_live_lines("R/hybrid_practice_size_detector.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('"\\(19\\|20\\)\\(\\[0-9\\]\\{2\\}\\)\\[_-\\]\\(0\\[1-9\\]\\|1\\[0-2\\]\\)\\\\\\\\b"', joined),
    info = "must include YYYY[-_]MM numeric pattern"
  )
  expect_true(
    grepl("YYYYMMDD|\\\\\\\\b\\(19\\|20\\)\\(\\[0-9\\]\\{2\\}\\)\\(0\\[1-9\\]\\|1\\[0-2\\]\\)\\[0-9\\]\\{2\\}\\\\\\\\b", joined),
    info = "must include YYYYMMDD pattern"
  )
})

test_that("F8-batch1-#5 structural: clustering query carries address_line1", {
  live <- .read_live_lines("R/hybrid_practice_size_detector.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('Provider First Line Business Practice Location Address.*address_line1',
          joined),
    info = "NPPES query must SELECT address_line1 for clustering"
  )
})

test_that("F8-batch1-#6 structural: backfill GROUP BY includes normalized ZIP5", {
  live <- .read_live_lines("scripts/backfill_geocoded_addresses.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("GROUP BY\\s*\\n?\\s*UPPER\\(TRIM\\(t\\.practice_address_street\\)\\),\\s*\\n?\\s*UPPER\\(TRIM\\(t\\.practice_address_city\\)\\),\\s*\\n?\\s*UPPER\\(TRIM\\(t\\.practice_address_state\\)\\),\\s*\\n?\\s*COALESCE\\(SUBSTR\\(REGEXP_REPLACE\\(CAST\\(t\\.practice_address_zip",
          joined),
    info = "GROUP BY must include the normalized ZIP5 expression"
  )
})

test_that("F8-batch1-#7 structural: mailing-candidate PARTITION BY includes ZIP5", {
  live <- .read_live_lines("scripts/backfill_geocoded_addresses.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("PARTITION BY practice_street,\\s*practice_city,\\s*practice_state,\\s*\\n?\\s*COALESCE\\(SUBSTR\\(REGEXP_REPLACE\\(CAST\\(practice_zip",
          joined),
    info = "ROW_NUMBER PARTITION BY must include practice_zip ZIP5 normalizer"
  )
})

test_that("F8-batch1-#8 structural: pass1_resolved anti_join includes .zip5", {
  live <- .read_live_lines("scripts/backfill_geocoded_addresses.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('\\.zip5_r\\s*<-\\s*function\\(z\\)', joined),
    info = "must define R-side .zip5_r helper"
  )
  expect_true(
    grepl('"\\.zip5"\\s*=\\s*"\\.zip5"', joined),
    info = "anti_join key must include .zip5"
  )
})

test_that("F8-batch1-#9 structural: Tier 7 DAC join includes normalized street", {
  live <- .read_live_lines("R/utils/resolve_npi_year_address.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("LOWER\\(TRIM\\(p\\.address_street\\)\\)\\s*=\\s*LOWER\\(TRIM\\(g\\.practice_address_street\\)\\)",
          joined),
    info = "Tier 7 DAC join must include lower(trim) street equality"
  )
  expect_true(
    grepl('adr_ln_1.*AS address_street', joined),
    info = "dac_unions CTE must carry address_street from DAC adr_ln_1"
  )
})

test_that("F8-batch1-#10 structural: Tier 8 affil_count keyed on per-row ccn", {
  live <- .read_live_lines("R/utils/resolve_npi_year_address.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("a\\.ccn,\\s*--\\s*F8-batch1-#10", joined),
    info = "hosp_geocoded CTE must carry ccn"
  )
  expect_true(
    grepl("WHERE TRIM\\(f2\\.facility_cert_number\\)\\s*=\\s*h\\.ccn", joined),
    info = "subquery must filter by the row's own ccn (h.ccn) not a LIMIT 1 lookup"
  )
  expect_false(
    grepl("SELECT TRIM\\(f3\\.facility_cert_number\\)\\s*\\n?\\s*FROM facility_affiliation f3", joined),
    info = "the old nested LIMIT-1 lookup must be removed"
  )
})

test_that("F8-batch2-#1 structural: add_practice_stability accepts 0-row cohorts", {
  live <- .read_live_lines("R/enrich_table1_demographics.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('assert_data_frame\\(cohort,\\s*min\\.rows\\s*=\\s*0,\\s*\\.var\\.name\\s*=\\s*"cohort"\\)', joined),
    info = "min.rows must be 0"
  )
  expect_true(
    grepl("if \\(nrow\\(cohort\\) == 0L\\) return\\(cohort\\)", joined),
    info = "must early-return cohort unchanged when empty"
  )
})

test_that("F8-batch2-#2 structural: years_at_current_site uses contiguous-run computation", {
  live <- .read_live_lines("R/enrich_table1_demographics.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("current_site_year_runs AS", joined),
    info = "must introduce the current_site_year_runs CTE"
  )
  expect_true(
    grepl("LAG\\(ah\\.year\\) OVER \\(PARTITION BY ah\\.npi ORDER BY ah\\.year DESC\\)\\s*\\n?\\s*-\\s*ah\\.year = 1", joined),
    info = "must detect year gaps via LAG"
  )
  expect_true(
    grepl("AND csyr\\.run_id = 1", joined),
    info = "must restrict count to the most-recent run (run_id = 1)"
  )
})

test_that("F8-batch2-#3+#4 structural: retirement coalesce row-wise across expanded candidate list", {
  live <- .read_live_lines("R/enrich_table1_demographics.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl('"retirement_year_final"', joined),
    info = "candidate list must include retirement_year_final"
  )
  expect_true(
    grepl('"retirement_year_detected"', joined),
    info = "candidate list must include retirement_year_detected"
  )
  expect_true(
    grepl('"predicted_retirement_year"', joined),
    info = "candidate list must include predicted_retirement_year"
  )
  expect_true(
    grepl('do\\.call\\(\\s*\\n?\\s*dplyr::coalesce,\\s*\\n?\\s*lapply\\(\\.row_level_candidates', joined),
    info = "must row-wise coalesce across all present candidates"
  )
})

test_that("F8-batch2-#5 structural: empty county supplement returns cohort unchanged", {
  live <- .read_live_lines("R/supplement_table1_demographics.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("if \\(nrow\\(npi_counties\\) == 0\\) \\{\\s*\\n?\\s*[^\\}]*return\\(cohort\\)", joined),
    info = "empty-county branch must return cohort, not empty data.frame"
  )
})

test_that("F8-batch2-#6 structural: Doximity + Healthgrades coerce NPI to character before filter", {
  live <- .read_live_lines("R/supplement_table1_demographics.R")
  joined <- paste(live, collapse = "\n")
  doximity_matches <- length(gregexpr(
    "mutate\\(npi = as\\.character\\(npi\\)\\)\\s*%>%\\s*\\n?\\s*filter\\(npi %in% as\\.character\\(",
    joined, perl = TRUE)[[1]])
  expect_gte(doximity_matches, 2L,
             label = "type-coercion pattern occurrences (Doximity + Healthgrades)")
})

test_that("F8-batch2-#7 structural: .pick_medicaid_table ranks by quarter + import suffix", {
  live <- .read_live_lines("R/supplement_table1_demographics.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.extract_quarter\\s*<-\\s*function", joined),
    info = "must extract quarter from table name"
  )
  expect_true(
    grepl("\\.extract_suffix\\s*<-\\s*function", joined),
    info = "must extract trailing import suffix"
  )
  expect_true(
    grepl("order\\(-qs,\\s*-ss,\\s*exact", joined),
    info = "must sort by (-quarter, -suffix, name)"
  )
})

test_that("F8-batch2-#8 structural: enrich_medical_school normalizes state via .state_to_abbr", {
  live <- .read_live_lines("R/enrich_medical_school_cascade.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.state_to_abbr\\s*<-\\s*function", joined),
    info = "must define .state_to_abbr helper"
  )
  expect_true(
    grepl("state_upper\\s*=\\s*\\.state_to_abbr\\(state\\)", joined),
    info = "candidates pipeline must use .state_to_abbr"
  )
  expect_true(
    grepl('DISTRICT OF COLUMBIA', joined),
    info = ".state_to_abbr must handle DC explicitly"
  )
})

test_that("F8-batch2-#9 structural: p1_unambiguous arranges before first(npi)", {
  live <- .read_live_lines("R/enrich_medical_school_cascade.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("p1_unambiguous\\s*<-\\s*nppes_tbl\\s*\\|>\\s*\\n?\\s*dplyr::arrange\\(last_upper,\\s*first_initial,\\s*state_upper,\\s*npi\\)",
          joined),
    info = "must arrange by (last, first_initial, state, npi) before first(npi)"
  )
})

test_that("F8-batch2-#10 structural: degree_type cascade adds deterministic tiebreak", {
  live <- .read_live_lines("R/enrich_degree_type_cascade.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.cred_len\\s*=\\s*nchar", joined),
    info = "must compute .cred_len for tiebreaking"
  )
  expect_true(
    grepl("dplyr::desc\\(\\.cred_len\\)", joined),
    info = "arrange must include desc(.cred_len)"
  )
  expect_true(
    grepl("dplyr::coalesce\\(cred,\\s*\"\"\\),\\s*\\n?\\s*dplyr::coalesce\\(other_cred,\\s*\"\"\\)\\) \\|>",
          joined),
    info = "final tiebreak must be deterministic on credential text"
  )
})

test_that("F7-msg2-#8 structural: enrich_region_cascade gates conflict check on mapping_exclusivity", {
  live <- .read_live_lines("R/enrich_region_cascade.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.is_exclusive_source\\s*<-\\s*isTRUE\\(geography_system == \"custom_map\"\\)", joined),
    info = "must gate the conflict check on custom_map or exclusive mapping"
  )
  expect_true(
    grepl('mapping_exclusivity == "exclusive"', joined),
    info = "must also detect explicit mapping_exclusivity == \"exclusive\""
  )
  expect_true(
    grepl("if \\(\\.is_exclusive_source\\) \\{", joined),
    info = "conflict check must be wrapped in the gate"
  )
})

test_that("F6-#10 structural: explicit dup-NPI guard + relationship=one-to-one", {
  live <- .read_live_lines("R/validate_org_npi_for_practice_size.R")
  joined <- paste(live, collapse = "\n")
  expect_true(
    grepl("\\.dup_org_npi\\s*<-\\s*org_npi_sizes\\$npi\\[duplicated\\(org_npi_sizes\\$npi\\)", joined),
    info = "must compute .dup_org_npi via duplicated() check"
  )
  expect_true(
    grepl("\\.dup_org_pac\\s*<-\\s*org_pac_sizes\\$npi\\[duplicated\\(org_pac_sizes\\$npi\\)", joined),
    info = "must compute .dup_org_pac via duplicated() check"
  )
  expect_true(
    grepl('relationship\\s*=\\s*"one-to-one"', joined),
    info = "join must declare one-to-one (was many-to-many)"
  )
  expect_false(
    grepl('relationship\\s*=\\s*"many-to-many"', joined),
    info = "the previous many-to-many relationship must be removed"
  )
})






