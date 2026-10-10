#' ABMS-NPI Matching: mysterynpi Agreement-Function A/B Test
#'
#' Empirically tests the 2026-09-18 architectural evaluation ("should ABMS-NPI's
#' bespoke matching logic be replaced with mysterynpi's agreement functions?")
#' against real data, without touching the production pipeline file
#' (R/canonical_abms_npi_matching_FIXED_STABLE_2026-02-08.R).
#'
#' Three comparisons, each read-only against the real attached DuckDB:
#'
#' 1. MIDDLE-INITIAL GUARD REPLACEMENT: sql_middle_initial_guard() (current
#'    production, single-position first-letter check) vs mysterynpi's
#'    middle_agreement() (token-set based). Query WITHOUT the guard to get the
#'    full candidate superset, then classify every candidate by both methods'
#'    verdicts to see (a) how many guard-REJECTED candidates would be RESCUED
#'    by the smarter agreement function (the documented failure mode --
#'    maiden name in the middle slot), and (b) how many guard-PASSED
#'    candidates would be newly vetoed (does the guard let through anything
#'    mysterynpi would catch).
#' 2. NICKNAME RECALL WIDENING: candidates sharing last_name_key + state but
#'    NOT first_name_key (i.e. invisible to every current SQL stage) that
#'    mysterynpi's nickname_agreement() would corroborate.
#' 3. SUFFIX CONFLICT AUDIT: among candidates ABMS currently accepts (Stage 1
#'    exact_full), how many carry a RECORDED suffix on both sides that
#'    suffix_agreement() flags as a generational conflict (Jr vs Sr, II vs III)
#'    -- a dimension the current pipeline never checks at all.
#'
#' Scope: single year (2024, the richest/most complete temporal table) against
#' the FULL ABMS_cleaned.csv (~22.8k rows). This is bounded on purpose (one
#' year, not the production 11-year loop) so it runs in minutes, not hours --
#' same "TEST_MODE sample first" pattern already established for the
#' compute_match_score-vs-confidence_score A/B test (GitHub issue #965).
#'
#' Read-only: attaches nber_my_duckdb READ_ONLY, writes nothing back to it.
#' Output: console summary + a CSV of disagreement examples for manual review.
#'
#' @noRd
suppressMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
  library(logger)
})

source(here::here("R", "utils", "load_paths.R"))
# Sourcing the production file only DEFINES functions (main() is guarded by
# sys.nframe() == 0, which is false here because source() adds a frame) --
# reuses parse_abms_name(), normalize_state_to_usps(), sql_clean_name(),
# sql_compact_name(), sql_middle_initial_guard() verbatim rather than
# re-deriving them, so this test exercises the SAME normalization the
# production pipeline uses.
source(here::here("R", "canonical_abms_npi_matching_FIXED_STABLE_2026-02-08.R"))
library(mysterynpi)

TEST_YEAR <- as.integer(Sys.getenv("ABMS_AGREEMENT_TEST_YEAR", "2024"))
OUT_DIR <- here::here("artifacts", "abms_mysterynpi_agreement_test")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log_info("=== ABMS-NPI mysterynpi agreement A/B test (year %d) ===", TEST_YEAR)

# --- Load + parse ABMS (same parser the production pipeline uses) ---
paths <- load_paths(verbose = FALSE)
abms_csv <- file.path(paths$nber_retirement_data_dir, "ABMS_cleaned.csv")
stopifnot(file.exists(abms_csv))

abms_raw <- readr::read_csv(abms_csv, show_col_types = FALSE)
parsed_names <- parse_abms_name(abms_raw$physician_name)

abms_parsed <- abms_raw %>%
  bind_cols(parsed_names) %>%
  mutate(
    rowid = row_number(),
    state_raw = if ("state" %in% names(.)) as.character(state) else NA_character_,
    state_from_location = if ("location" %in% names(.)) {
      unname(sapply(location, parse_state_from_location))
    } else NA_character_,
    state_combined = coalesce(state_raw, state_from_location, ""),
    state_clean = unname(sapply(state_combined, normalize_state_to_usps))
  ) %>%
  filter(!is.na(first_name_clean), !is.na(last_name_clean))

log_info("Parsed ABMS: %d valid names", nrow(abms_parsed))

# --- Connect: in-memory workspace + read-only attach of the real DB ---
nber_db_path <- paths$nber_duckdb_main
stopifnot(file.exists(nber_db_path))

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbExecute(con, sprintf("ATTACH '%s' AS nber_my_duckdb (READ_ONLY);", nber_db_path))
duckdb::duckdb_register(con, "abms_df", abms_parsed)
dbExecute(con, "CREATE OR REPLACE TEMP TABLE abms_source AS SELECT * FROM abms_df")
duckdb::duckdb_unregister(con, "abms_df")

temporal_table <- sprintf("nber_my_duckdb.credentials.temporal_nppes_%d_fixed", TEST_YEAR)
n_temporal <- dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", temporal_table))$n
log_info("Temporal table %s: %d rows", temporal_table, n_temporal)

# =============================================================================
# TEST 1: middle-initial guard -- current sql_middle_initial_guard() vs
# mysterynpi::middle_agreement()
# =============================================================================
log_info("--- TEST 1: middle-initial guard replacement ---")

# Superset: exact last+first+state match, guard DROPPED entirely, plus the
# raw middle-name strings from both sides needed to score independently.
query_superset <- sprintf("
  SELECT
    a.rowid AS abms_id,
    CAST(n.npi AS VARCHAR) AS npi,
    a.middle_initial AS abms_middle,
    n.middle_name AS nppes_middle,
    CASE WHEN (
      a.middle_initial IS NULL OR TRIM(a.middle_initial) = ''
      OR n.middle_name IS NULL OR TRIM(n.middle_name) = ''
      OR SUBSTR(REGEXP_REPLACE(UPPER(COALESCE(n.middle_name, '')), '[^A-Z]', '', 'g'), 1, 1) = a.middle_initial
    ) THEN 1 ELSE 0 END AS passes_current_guard
  FROM abms_source a
  INNER JOIN %s n ON
    %s = a.last_name_key
    AND %s = a.first_name_key
    AND UPPER(TRIM(n.practice_address_state)) = a.state_clean
  WHERE a.state_clean IS NOT NULL
", temporal_table, sql_compact_name("n.last_name"), sql_compact_name("n.first_name"))

superset <- dbGetQuery(con, query_superset)
log_info("Candidate superset (exact last+first+state, no middle guard): %d rows", nrow(superset))

superset$new_verdict <- mysterynpi::middle_agreement(
  mysterynpi::middle_tokens(superset$abms_middle),
  mysterynpi::middle_tokens(superset$nppes_middle)
)

crosstab1 <- superset %>%
  count(passes_current_guard, new_verdict) %>%
  arrange(desc(passes_current_guard), new_verdict)

log_info("Crosstab: current guard pass/fail x mysterynpi middle_agreement() verdict")
print(crosstab1)

rescued <- superset %>% filter(passes_current_guard == 0, new_verdict != "conflicts")
newly_vetoed <- superset %>% filter(passes_current_guard == 1, new_verdict == "conflicts")

log_info("RESCUED by mysterynpi (guard rejects today, agreement does not conflict): %d", nrow(rescued))
log_info("NEWLY VETOED by mysterynpi (guard accepts today, agreement conflicts): %d", nrow(newly_vetoed))

readr::write_csv(rescued, file.path(OUT_DIR, sprintf("test1_rescued_%d.csv", TEST_YEAR)))
readr::write_csv(newly_vetoed, file.path(OUT_DIR, sprintf("test1_newly_vetoed_%d.csv", TEST_YEAR)))

# =============================================================================
# TEST 2: nickname recall widening -- candidates sharing last_name_key+state
# but NOT first_name_key (invisible to every current SQL stage), that
# mysterynpi::nickname_agreement() would corroborate.
# =============================================================================
log_info("--- TEST 2: nickname recall widening ---")

query_lastname_block <- sprintf("
  SELECT
    a.rowid AS abms_id,
    CAST(n.npi AS VARCHAR) AS npi,
    a.first_name_clean AS abms_first,
    n.first_name AS nppes_first
  FROM abms_source a
  INNER JOIN %s n ON
    %s = a.last_name_key
    AND UPPER(TRIM(n.practice_address_state)) = a.state_clean
  WHERE a.state_clean IS NOT NULL
    AND %s != a.first_name_key
", temporal_table, sql_compact_name("n.last_name"), sql_compact_name("n.first_name"))

lastname_block <- dbGetQuery(con, query_lastname_block)
log_info("Candidates sharing last_name+state but NOT first_name_key (currently invisible to Stage 1/2): %d", nrow(lastname_block))

if (nrow(lastname_block) > 0) {
  lastname_block$nickname_verdict <- mysterynpi::nickname_agreement(
    lastname_block$abms_first, lastname_block$nppes_first
  )
  print(count(lastname_block, nickname_verdict))
  nickname_rescued <- lastname_block %>% filter(nickname_verdict == "corroborates")
  log_info("Would be RESCUED via nickname_agreement (new candidates, currently 0 stages see these): %d", nrow(nickname_rescued))
  readr::write_csv(nickname_rescued, file.path(OUT_DIR, sprintf("test2_nickname_rescued_%d.csv", TEST_YEAR)))
} else {
  log_info("No candidates in this blocking set -- test 2 skipped.")
}

# =============================================================================
# TEST 3: suffix conflict audit -- among CURRENTLY ACCEPTED Stage 1 matches,
# how many carry a recorded generational suffix conflict mysterynpi would flag
# (a dimension the current pipeline never checks at all)?
# =============================================================================
log_info("--- TEST 3: suffix conflict audit (Stage 1 exact_full matches) ---")

# parse_abms_name() DETECTS a trailing generational suffix (single-letter
# token, e.g. "David Spencer Goodyear V") only to DISCARD it -- it is never
# returned as a column, so the production matcher has no ABMS-side suffix
# field to compare at all today. Extracted here directly from the raw name
# string (same detection the parser already does internally) purely to let
# this read-only test measure what suffix_agreement() would see if it were
# wired in -- does NOT modify parse_abms_name() or any production code.
extract_abms_suffix <- function(full_name) {
  # NOTE: normalize_text() itself strips JR/SR/II/III/IV as noise tokens
  # (line ~469 of the production file) BEFORE parse_abms_name() ever sees the
  # string -- so the suffix must be pulled off the TRULY raw name (just
  # trimmed/uppercased/comma-stripped) or it is already gone.
  raw <- toupper(trimws(gsub(",", " ", full_name)))
  vapply(raw, function(name) {
    parts <- strsplit(name, "\\s+")[[1]]
    if (length(parts) >= 3 && nchar(parts[length(parts)]) <= 3 &&
        grepl("^(JR|SR|II|III|IV|V|VI)$", parts[length(parts)])) {
      parts[length(parts)]
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
}
abms_parsed$abms_suffix_extracted <- extract_abms_suffix(abms_raw$physician_name[abms_parsed$rowid])
duckdb::duckdb_register(con, "abms_df2", abms_parsed)
dbExecute(con, "CREATE OR REPLACE TEMP TABLE abms_source AS SELECT * FROM abms_df2")
duckdb::duckdb_unregister(con, "abms_df2")

query_stage1_accepted <- sprintf("
  SELECT
    a.rowid AS abms_id,
    CAST(n.npi AS VARCHAR) AS npi,
    a.abms_suffix_extracted AS abms_suffix,
    n.suffix AS nppes_suffix
  FROM abms_source a
  INNER JOIN %s n ON
    %s = a.last_name_key
    AND %s = a.first_name_key
    AND UPPER(TRIM(n.practice_address_state)) = a.state_clean
    AND %s
  WHERE a.state_clean IS NOT NULL
", temporal_table, sql_compact_name("n.last_name"), sql_compact_name("n.first_name"),
   sql_middle_initial_guard("n.middle_name"))

stage1_accepted <- dbGetQuery(con, query_stage1_accepted)
log_info("Stage 1 (exact_full) accepted candidates: %d", nrow(stage1_accepted))

stage1_accepted$suffix_verdict <- mysterynpi::suffix_agreement(
  stage1_accepted$abms_suffix, stage1_accepted$nppes_suffix
)
print(count(stage1_accepted, suffix_verdict))

suffix_conflicts <- stage1_accepted %>% filter(suffix_verdict == "conflicts")
log_info("Stage 1 matches with a RECORDED generational suffix CONFLICT (never checked today): %d", nrow(suffix_conflicts))
readr::write_csv(suffix_conflicts, file.path(OUT_DIR, sprintf("test3_suffix_conflicts_%d.csv", TEST_YEAR)))

# =============================================================================
# Summary
# =============================================================================
log_info("=== SUMMARY (year %d) ===", TEST_YEAR)
log_info("Test 1 (middle guard):    %d superset candidates | %d rescued | %d newly vetoed",
          nrow(superset), nrow(rescued), nrow(newly_vetoed))
log_info("Test 2 (nickname recall): %d blocked candidates  | %d rescued (new matches Stage 1/2 never see)",
          nrow(lastname_block), if (exists("nickname_rescued")) nrow(nickname_rescued) else 0L)
log_info("Test 3 (suffix audit):    %d Stage-1 matches      | %d carry an unflagged suffix conflict",
          nrow(stage1_accepted), nrow(suffix_conflicts))
log_info("CSVs written to: %s", OUT_DIR)
