#!/usr/bin/env Rscript
# =============================================================================
# MATCH AAO-HNS MEMBER LIST TO NPPES NPIs
# =============================================================================
# Source: AAO-HNS member roster provided March 5, 2026 by Tina Maggio
#         (Chief Communications Officer) — U.S. actively practicing
#         otolaryngologists, no residents or retired members.
#
# Input:  data/ent/aao_member_list_20260305.xlsx
#          Cols: Full Name, Company, State Province, Country (all US)
#          Rows: 7,198
#
# Method: Six-pass matching against npi_2026 in DuckDB
#   Pass 1 — Exact: last_clean == nppes_last AND state == nppes_state
#                   AND first_initial == nppes_first_initial
#   Pass 2 — Exact name, relax first-initial constraint (still within state)
#   Pass 3 — Fuzzy last-name (Levenshtein ≤ 1) within state
#   Pass 4a — Strip apostrophes/hyphens/spaces from last name, retry state
#   Pass 4b — National match when only 1 ENT shares last+first-initial in US
#   Pass 5  — Nickname expansion (Josh→Joshua, Bob→Robert, etc.) + state retry
#   Pass 6  — First-name Jaro-Winkler ≥ 0.85 within last (exact) + state + city
#             City agreement: city_nppes must appear in AAO company string
#   Tiebreaker: highest Jaro-Winkler similarity between AAO Company and NPPES org
#
# Name preprocessing (strip_aao_name):
#   1. Remove trailing commas   "Morris, MD,"  → "Morris, MD"
#   2. Remove generational suffixes   ", IV, MD" → ", MD"
#   3. Strip everything from first post-nominal credential marker
#   4. humaniformat::parse_names() for standardized first/last extraction
#
# Output:
#   data/ent/aao_npi_matched.csv   — all 7,198 rows + npi + match columns
#   data/ent/aao_npi_unmatched.csv — rows where no NPI was found
#
# Usage:
#   caffeinate -i Rscript scripts/match_aao_to_nppes.R 2>&1 | tee logs/aao_match_$(date +%Y%m%d_%H%M%S).log
#
# Author: Tyler Muffly MD + Claude Code
# Date:   2026-05-04
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(readr)
  library(duckdb)
  library(DBI)
  library(here)
  library(stringdist)
  library(humaniformat)
})

source(here("R", "name_parsing_protocol_enhanced.R"))

# ── Config ────────────────────────────────────────────────────────────────────
AAO_XLSX    <- here("data", "ent", "aao_member_list_20260305.xlsx")
DB_PATH     <- "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb"
OUT_CSV     <- here("data", "ent", "aao_npi_matched.csv")
UNMATCH_CSV <- here("data", "ent", "aao_npi_unmatched.csv")

DATESTAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")

cat("\n=======================================================================\n")
cat("  MATCH AAO-HNS MEMBER LIST → NPPES NPIs\n")
cat(sprintf("  Started : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=======================================================================\n\n")

# ── Name-cleaning helpers ─────────────────────────────────────────────────────

# All post-nominal credential tokens (order: longest first to avoid partial hits)
.CREDS <- paste(c(
  "FAAOHNS","FRCSC","FRCS","FAAP","FACOG","FAAOA","FAAOT","FAANS","FASCRS",
  "FASPO","FAHA","FACR","FACS","FAAO","ABOHNS","ABMS",
  "DrPH","DNSc","MHSA","MMSc","MSPh","MSc",
  "PhD","DPH","ScD","DSc","MPH","MBA","MHA","MPA","MEd","MBS","JD","MS","DM",
  "MBBS","DMD","DDS",   # dental/international degrees that end up as false last names
  "MD","DO"
), collapse = "|")

# Pre-compiled patterns (faster than building each call)
.PAT_TRAIL_COMMA <- ",\\s*$"
.PAT_GEN_SUFFIX  <- "(?i),?\\s*\\b(Jr\\.?|Sr\\.?|II|III|IV|V|VI)\\b,?"
.PAT_CRED_COMMA  <- paste0("(?i),\\s*(", .CREDS, ").*$")
.PAT_CRED_SPACE  <- paste0("(?i)\\s+(", .CREDS, ")(\\s+(", .CREDS, "))*\\s*$")

strip_aao_name <- function(x) {
  x <- trimws(x)
  x <- gsub(.PAT_TRAIL_COMMA, "",  x, perl = TRUE)         # trailing commas
  x <- gsub(.PAT_GEN_SUFFIX,  " ", x, perl = TRUE)         # Jr/Sr/II/III/IV/V
  x <- gsub(.PAT_CRED_COMMA,  "",  x, perl = TRUE)         # ", MD, FACS, ..."
  x <- gsub(.PAT_CRED_SPACE,  "",  x, perl = TRUE)         # " MD FACS" at end
  trimws(gsub("\\s{2,}", " ", x))
}

# Nickname → most common legal first name (upper-case → upper-case)
NICKNAMES <- c(
  "BILL" = "WILLIAM", "BILLY" = "WILLIAM", "WILL" = "WILLIAM",
  "BOB"  = "ROBERT",  "ROB"   = "ROBERT",  "BOBBY" = "ROBERT",
  "JACK" = "JOHN",    "JOHNNY"= "JOHN",
  "JIM"  = "JAMES",   "JIMMY" = "JAMES",   "JAY" = "JAMES",
  "TOM"  = "THOMAS",  "TOMMY" = "THOMAS",
  "DICK" = "RICHARD", "RICH"  = "RICHARD", "RICK" = "RICHARD",
  "RICKY"= "RICHARD",
  "MIKE" = "MICHAEL", "MICK"  = "MICHAEL",
  "DAN"  = "DANIEL",  "DANNY" = "DANIEL",
  "DAVE" = "DAVID",   "DAVY"  = "DAVID",
  "CHRIS"= "CHRISTOPHER",
  "MATT" = "MATTHEW", "MATTY" = "MATTHEW",
  "STEVE"= "STEVEN",  "STEPH" = "STEPHEN",
  "ANDY" = "ANDREW",  "DREW"  = "ANDREW",
  "TONY" = "ANTHONY",
  "NICK" = "NICHOLAS","NICKY" = "NICHOLAS",
  "ALEX" = "ALEXANDER",
  "JOE"  = "JOSEPH",  "JOEY"  = "JOSEPH",
  "PETE" = "PETER",
  "PAT"  = "PATRICK",
  "SAM"  = "SAMUEL",
  "JOSH" = "JOSHUA",
  "KEN"  = "KENNETH",
  "JEFF" = "JEFFREY",
  "GREG" = "GREGORY",
  "FRED" = "FREDERICK","FREDDY"= "FREDERICK",
  "CHUCK"= "CHARLES", "CHARLIE"= "CHARLES",
  "AL"   = "ALBERT",  "BERT"  = "ALBERT",
  "BEN"  = "BENJAMIN",
  "NATE" = "NATHANIEL","NAT"   = "NATHANIEL",
  "FRANK"= "FRANCIS",
  "LIZ"  = "ELIZABETH","BETH"  = "ELIZABETH","BETTY" = "ELIZABETH",
  "KATE" = "KATHERINE","KATHY" = "KATHERINE","KAY"  = "KATHERINE",
  "CATHY"= "CATHERINE","CAT"   = "CATHERINE",
  "SUE"  = "SUSAN",   "SUSIE" = "SUSAN",
  "CINDY"= "CYNTHIA",
  "PATTY"= "PATRICIA",
  "JENNY"= "JENNIFER","JEN"   = "JENNIFER",
  "ANNIE"= "ANNE",    "ANN"   = "ANNE",
  "MAGGIE"="MARGARET","MEG"   = "MARGARET", "PEGGY"= "MARGARET",
  "TRISH"= "PATRICIA"
)

strip_special <- function(x) gsub("[' \\-]", "", toupper(trimws(x)))

# ── 1. Load AAO list ──────────────────────────────────────────────────────────
if (!file.exists(AAO_XLSX))
  stop("AAO member list not found: ", AAO_XLSX)

aao_raw <- read_excel(AAO_XLSX) |>
  rename_with(trimws) |>
  rename(
    full_name      = `Full Name`,
    company        = Company,
    state_province = `State Province`,
    country        = Country
  ) |>
  filter(country == "United States") |>
  mutate(aao_row_id = row_number())

cat(sprintf("AAO list loaded : %s rows\n", format(nrow(aao_raw), big.mark = ",")))

# ── 2. Clean & parse physician names ─────────────────────────────────────────
cat("Cleaning and parsing physician names (humaniformat)...\n")

cleaned_names <- sapply(aao_raw$full_name, strip_aao_name, USE.NAMES = FALSE)
cat(sprintf("  Pre-cleaned %d names\n", sum(cleaned_names != aao_raw$full_name, na.rm = TRUE)))

# humaniformat::parse_names returns: salutation, first_name, middle_name, last_name, suffix, full_name
# Drop full_name — aao_raw already carries it; keeping both causes bind_cols rename conflict
parsed_hf <- humaniformat::parse_names(cleaned_names) |>
  select(-any_of(c("full_name", "salutation", "suffix")))

aao <- bind_cols(aao_raw, parsed_hf) |>
  mutate(
    last_clean    = toupper(trimws(last_name)),
    first_clean   = toupper(trimws(first_name)),
    first_initial = substr(first_clean, 1L, 1L),
    state         = toupper(trimws(state_province))
  )

cat(sprintf("  Name parse coverage: last=%d%%, first=%d%%\n",
            round(100 * mean(nzchar(aao$last_clean,  keepNA = FALSE))),
            round(100 * mean(nzchar(aao$first_clean, keepNA = FALSE)))))

# ── 3. Extract all active ENTs from npi_2026 ─────────────────────────────────
cat("\nQuerying DuckDB for active ENT physicians (207Y*)...\n")

if (!file.exists(DB_PATH))
  stop("DuckDB not found: ", DB_PATH)

con <- DBI::dbConnect(duckdb::duckdb(), DB_PATH, read_only = TRUE)
on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)

ent_q <- paste0(
  'SELECT ',
  '  "NPI"                                                          AS npi, ',
  '  UPPER(TRIM("Provider Last Name (Legal Name)"))                 AS last_nppes, ',
  '  UPPER(TRIM("Provider First Name"))                             AS first_nppes, ',
  '  UPPER(TRIM("Provider Business Practice Location Address State Name")) AS state_nppes, ',
  '  TRIM("Provider Business Practice Location Address City Name")  AS city_nppes, ',
  '  TRIM("Provider Business Practice Location Address Postal Code") AS zip_nppes, ',
  '  TRIM("Provider First Line Business Practice Location Address") AS address_nppes, ',
  '  TRIM("Provider Credential Text")                               AS credential, ',
  '  "Provider Enumeration Date"                                    AS enum_date, ',
  "  COALESCE(\"Healthcare Provider Taxonomy Code_1\",'') AS taxonomy_1, ",
  "  COALESCE(\"Healthcare Provider Taxonomy Code_2\",'') AS taxonomy_2, ",
  "  COALESCE(\"Healthcare Provider Taxonomy Code_3\",'') AS taxonomy_3 ",
  'FROM npi_2026 ',
  "WHERE \"Entity Type Code\" = '1' ",
  "  AND (\"NPI Deactivation Date\" IS NULL OR \"NPI Deactivation Date\" = '') ",
  "  AND (\"Healthcare Provider Taxonomy Code_1\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_2\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_3\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_4\" LIKE '207Y%' ",
  "    OR \"Healthcare Provider Taxonomy Code_5\" LIKE '207Y%')"
)

t0 <- Sys.time()
nppes_ent <- DBI::dbGetQuery(con, ent_q)
elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
cat(sprintf("  NPPES ENTs found: %s (%d sec)\n",
            format(nrow(nppes_ent), big.mark = ","), elapsed))

nppes_ent <- nppes_ent |>
  mutate(
    first_initial_nppes = substr(first_nppes, 1L, 1L),
    last_stripped       = strip_special(last_nppes),
    ent_taxonomy = case_when(
      grepl("^207Y", taxonomy_1) ~ taxonomy_1,
      grepl("^207Y", taxonomy_2) ~ taxonomy_2,
      grepl("^207Y", taxonomy_3) ~ taxonomy_3,
      TRUE                       ~ taxonomy_1
    )
  )

# Pre-group by state for O(1) state lookups in loop passes
nppes_by_state <- split(nppes_ent, nppes_ent$state_nppes)

# Columns to carry forward from every successful match
nppes_cols <- c("npi", "last_nppes", "first_nppes", "state_nppes",
                "city_nppes", "zip_nppes", "address_nppes",
                "credential", "enum_date", "ent_taxonomy")

# ── 4. Matching passes ────────────────────────────────────────────────────────
results <- vector("list", nrow(aao))
matched <- logical(nrow(aao))
pass_tag <- integer(nrow(aao))   # which pass made the match

.best_match <- function(candidates, aao_company) {
  if (nrow(candidates) == 1L) return(candidates)
  sims <- stringsim(toupper(aao_company), toupper(candidates$address_nppes),
                    method = "jw", p = 0.1)
  candidates[which.max(sims), , drop = FALSE]
}

.record <- function(i, best, pass) {
  results[[i]] <<- best[1L, ]
  matched[i]   <<- TRUE
  pass_tag[i]  <<- pass
}

# --- Pass 1: exact last + state + first-initial --------------------------------
cat("\nPass 1: exact last + state + first-initial...\n")
for (i in seq_len(nrow(aao))) {
  cands <- nppes_ent |>
    filter(last_nppes           == aao$last_clean[i],
           state_nppes          == aao$state[i],
           first_initial_nppes  == aao$first_initial[i])
  if (nrow(cands) == 0L) next
  .record(i, .best_match(cands, aao$company[i]), 1L)
}
n1 <- sum(matched)
cat(sprintf("  Pass 1 matched: %d / %d\n", n1, nrow(aao)))

# --- Pass 2: exact last + state, relax first-initial --------------------------
cat("Pass 2: exact last + state (relax first-initial)...\n")
todo2 <- which(!matched & nzchar(aao$last_clean, keepNA = FALSE))
for (i in todo2) {
  cands <- nppes_ent |>
    filter(last_nppes  == aao$last_clean[i],
           state_nppes == aao$state[i])
  if (nrow(cands) == 0L) next
  .record(i, .best_match(cands, aao$company[i]), 2L)
}
n2 <- sum(matched)
cat(sprintf("  Pass 2 matched: %d additional (total %d)\n", n2 - n1, n2))

# --- Pass 3: fuzzy last (Levenshtein ≤ 1) + state + first-initial -------------
cat("Pass 3: fuzzy last-name (Levenshtein ≤ 1) + state + first-initial...\n")
todo3 <- which(!matched & nzchar(aao$last_clean, keepNA = FALSE))
n_pass3 <- 0L
for (i in todo3) {
  pool <- nppes_by_state[[aao$state[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next
  dists <- stringdist(aao$last_clean[i], pool$last_nppes, method = "lv")
  close <- pool[dists <= 1L & pool$first_initial_nppes == aao$first_initial[i], ]
  if (nrow(close) == 0L) next
  .record(i, .best_match(close, aao$company[i]), 3L)
  n_pass3 <- n_pass3 + 1L
}
n3 <- sum(matched)
cat(sprintf("  Pass 3 matched: %d additional (total %d)\n", n3 - n2, n3))

# --- Pass 4a: strip special chars from last, retry state ---------------------
cat("Pass 4a: strip special chars from last name, retry state match...\n")
todo4a <- which(!matched & nzchar(aao$last_clean, keepNA = FALSE))
n_pass4a <- 0L
for (i in todo4a) {
  stripped <- strip_special(aao$last_clean[i])
  if (!nzchar(stripped)) next
  pool <- nppes_by_state[[aao$state[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next
  cands <- pool |>
    filter(last_stripped        == stripped,
           first_initial_nppes  == aao$first_initial[i])
  if (nrow(cands) == 0L) next
  .record(i, .best_match(cands, aao$company[i]), 4L)
  n_pass4a <- n_pass4a + 1L
}
n4a <- sum(matched)
cat(sprintf("  Pass 4a matched: %d additional (total %d)\n", n4a - n3, n4a))

# --- Pass 4b: national unique last+first-initial match -----------------------
cat("Pass 4b: national match for unique last names...\n")
last_counts      <- nppes_ent |> count(last_nppes, name = "n_national")
nppes_ent_counts <- left_join(nppes_ent, last_counts, by = "last_nppes")

todo4b <- which(!matched & nzchar(aao$last_clean, keepNA = FALSE))
n_pass4b <- 0L
for (i in todo4b) {
  pool <- nppes_ent_counts |>
    filter(last_nppes          == aao$last_clean[i],
           first_initial_nppes == aao$first_initial[i])
  if (nrow(pool) != 1L) next
  .record(i, pool[1L, nppes_cols], 4L)   # tag same as 4a for simplicity
  n_pass4b <- n_pass4b + 1L
}
n4b <- sum(matched)
cat(sprintf("  Pass 4b matched: %d additional (total %d)\n", n4b - n4a, n4b))

# --- Pass 4c: hyphenated last name — try each component independently ---------
# NPPES frequently stores only one half of a hyphenated name.
# e.g. AAO "STEGNER-WILSON" → try "STEGNER" and "WILSON" separately.
cat("Pass 4c: hyphenated last name — try each component in state...\n")
todo4c <- which(!matched & grepl("-", aao$last_clean, fixed = TRUE))
n_pass4c <- 0L
for (i in todo4c) {
  parts <- strsplit(aao$last_clean[i], "-", fixed = TRUE)[[1]]
  pool  <- nppes_by_state[[aao$state[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next
  found <- FALSE
  for (part in parts) {
    if (!nzchar(part)) next
    cands <- pool |>
      filter(last_nppes          == part,
             first_initial_nppes == aao$first_initial[i])
    if (nrow(cands) == 0L)
      cands <- pool |> filter(last_nppes == part)
    if (nrow(cands) == 0L) next
    .record(i, .best_match(cands, aao$company[i]), 4L)
    n_pass4c <- n_pass4c + 1L
    found <- TRUE
    break
  }
}
n4c <- sum(matched)
cat(sprintf("  Pass 4c matched: %d additional (total %d)\n", n4c - n4b, n4c))

# --- Pass 5: nickname expansion → legal first name, retry state --------------
cat("Pass 5: nickname expansion + state retry...\n")
todo5 <- which(!matched & nzchar(aao$first_clean, keepNA = FALSE))
n_pass5 <- 0L
for (i in todo5) {
  legal_first <- NICKNAMES[aao$first_clean[i]]
  if (is.na(legal_first)) next
  legal_init <- substr(legal_first, 1L, 1L)
  pool <- nppes_by_state[[aao$state[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next

  # Try exact last + legal first initial
  cands <- pool |>
    filter(last_nppes          == aao$last_clean[i],
           first_initial_nppes == legal_init)
  if (nrow(cands) == 0L) {
    # Relax: exact last, any first
    cands <- pool |> filter(last_nppes == aao$last_clean[i])
  }
  if (nrow(cands) == 0L) next
  .record(i, .best_match(cands, aao$company[i]), 5L)
  n_pass5 <- n_pass5 + 1L
}
n5 <- sum(matched)
cat(sprintf("  Pass 5 matched: %d additional (total %d)\n", n5 - n4c, n5))

# --- Pass 6: first-name Jaro-Winkler ≥ 0.85 within exact last + state + city -
# City agreement: city_nppes (NPPES) must appear as a substring inside the AAO
# company field (practice name), which reliably encodes the practice city.
# Strict — no city agreement → skip (prevents spurious cross-city matches).
cat("Pass 6: first-name fuzzy (Jaro-Winkler ≥ 0.85) + exact last + state + city...\n")
todo6 <- which(!matched & nzchar(aao$last_clean, keepNA = FALSE) &
                           nzchar(aao$first_clean, keepNA = FALSE))
n_pass6 <- 0L
JW_THRESHOLD <- 0.85
for (i in todo6) {
  pool <- nppes_by_state[[aao$state[i]]]
  if (is.null(pool) || nrow(pool) == 0L) next
  exact_last <- pool |> filter(last_nppes == aao$last_clean[i])
  if (nrow(exact_last) == 0L) next

  # City gate: city_nppes must appear (case-insensitive) in AAO company string
  co       <- toupper(trimws(aao$company[i]))
  city_ok  <- vapply(exact_last$city_nppes,
                     function(c) nzchar(c) && grepl(toupper(trimws(c)), co, fixed = TRUE),
                     logical(1L))
  city_pool <- exact_last[city_ok, , drop = FALSE]
  if (nrow(city_pool) == 0L) next   # strict: require city agreement

  jw   <- stringsim(aao$first_clean[i], city_pool$first_nppes,
                    method = "jw", p = 0.1)
  hits <- city_pool[jw >= JW_THRESHOLD, , drop = FALSE]
  if (nrow(hits) == 0L) next
  .record(i, .best_match(hits, aao$company[i]), 6L)
  n_pass6 <- n_pass6 + 1L
}
n6 <- sum(matched)
cat(sprintf("  Pass 6 matched: %d additional (total %d)\n", n6 - n5, n6))

# ── 5. Assemble output ────────────────────────────────────────────────────────
match_df <- bind_rows(lapply(seq_len(nrow(aao)), function(i) {
  if (!matched[i]) {
    return(as.data.frame(
      setNames(as.list(rep(NA_character_, length(nppes_cols))), nppes_cols)
    ))
  }
  results[[i]][, nppes_cols, drop = FALSE]
}))

output <- bind_cols(aao |> select(-aao_row_id), match_df) |>
  mutate(
    match_found = matched,
    match_pass  = as.integer(pass_tag),
    match_pass  = if_else(match_found, match_pass, NA_integer_)
  )

# ── 6. Coverage report ────────────────────────────────────────────────────────
n_matched   <- sum(output$match_found, na.rm = TRUE)
n_unmatched <- nrow(output) - n_matched

cat(sprintf("\n=======================================================================\n"))
cat(sprintf("  MATCHING RESULTS\n"))
cat(sprintf("  Total AAO physicians : %s\n", format(nrow(output), big.mark = ",")))
cat(sprintf("  Matched to NPI       : %s (%.1f%%)\n",
            format(n_matched, big.mark = ","), 100 * n_matched / nrow(output)))
cat(sprintf("  Unmatched            : %s (%.1f%%)\n",
            format(n_unmatched, big.mark = ","), 100 * n_unmatched / nrow(output)))
cat("\n  By pass:\n")
for (p in 1:6) {
  n <- sum(output$match_pass == p, na.rm = TRUE)
  if (n > 0L) cat(sprintf("    Pass %d: %s\n", p, format(n, big.mark = ",")))
}

if (n_unmatched > 0L) {
  cat("\n  Top unmatched states:\n")
  top_states <- output |> filter(!match_found) |> count(state, sort = TRUE) |> head(10)
  for (j in seq_len(nrow(top_states)))
    cat(sprintf("    %s: %d\n", top_states$state[j], top_states$n[j]))
  cat("\n  Sample unmatched names (first 20):\n")
  samples <- output |>
    filter(!match_found) |>
    slice_head(n = 20) |>
    select(state, last_clean, first_clean, company)
  for (j in seq_len(nrow(samples)))
    cat(sprintf("    [%s] last=%-20s first=%-15s company=%s\n",
                samples$state[j], samples$last_clean[j],
                samples$first_clean[j], samples$company[j]))
}

# ── 7. Save ───────────────────────────────────────────────────────────────────
cat("\nSaving...\n")

tmp <- paste0(OUT_CSV, ".tmp")
write_csv(output, tmp, na = "")
invisible(file.rename(tmp, OUT_CSV))
cat(sprintf("  Matched CSV  : %s (%s rows)\n",
            OUT_CSV, format(nrow(output), big.mark = ",")))

unmatched_rows <- output |> filter(!match_found)
if (nrow(unmatched_rows) > 0L) {
  tmp2 <- paste0(UNMATCH_CSV, ".tmp")
  write_csv(unmatched_rows, tmp2, na = "")
  file.rename(tmp2, UNMATCH_CSV)
  cat(sprintf("  Unmatched CSV: %s (%s rows)\n",
              UNMATCH_CSV, format(nrow(unmatched_rows), big.mark = ",")))
}

cat("\n=======================================================================\n")
cat("  DONE\n")
cat(sprintf("  NPI coverage: %.1f%%  (%s / %s physicians)\n",
            100 * n_matched / nrow(output),
            format(n_matched, big.mark = ","),
            format(nrow(output), big.mark = ",")))
cat("=======================================================================\n\n")
