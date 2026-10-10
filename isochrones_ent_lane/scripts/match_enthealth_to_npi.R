#' @title ENTHealth.org Scrape → NPI Matching Pipeline
#'
#' @description
#' **Standalone script** — links the 7,911-physician scrape from
#' \url{https://www.enthealth.org/find-ent/} to National Provider Identifiers
#' (NPIs) from the NPPES system.
#'
#' Unlike the ABOG pipeline (which starts from a board certification roster with
#' IDs and graduation years), the ENTHealth dataset has only:
#' \itemize{
#'   \item \code{full_name} — free-text physician name
#'   \item \code{company_name} — practice name
#'   \item \code{address}, \code{city}, \code{state_code}, \code{post_code} —
#' practice location
#'   \item \code{phone_number} — practice phone
#'   \item \code{specialty_primary}, \code{specialty_secondary} — ENT
#' subspecialty
#' }
#' There is no board ID, graduation year, or NPI. Matching relies entirely on
#' name + address + phone signals against NPPES ENT physicians.
#'
#' @section Input Data:
#' Source file (Excel, read-only):
#' \code{mystery_shopper/Corbi study/ENT/Data/results.xlsx}
#' (Dropbox path — copied to \code{data/ent/enthealth_raw.xlsx} on first run)
#'
#' NPPES candidate pool (pre-built; requires running
#' \code{scripts/geocode_ent_from_nppes_api.R} first):
#' \code{artifacts/ent/ent_physicians_geocoded.parquet}
#'
#' @section Multi-Strategy Matching Cascade:
#' \preformatted{
#'   Priority | Strategy                              | Signal
#' ---------+---------------------------------------+---------------------------
#'   1        | Exact last name + state               | last_clean + state + ENT
#' taxonomy
#'   2        | Fuzzy last name + zip code            | Levenshtein ≤2 + zip5
#' match
#'   3        | Normalized phone number               | 10-digit phone + state
#'   4        | Organization / practice name          | company_name fuzzy +
#' state
#'
#'   Cascade stops at first strategy returning confidence >= 0.85
#' }
#'
#' @section Scoring Framework:
#' Identical weight system to canonical_abog_npi_pipeline_STABLE.R:
#' \preformatted{
#'   Component          Weight   Notes
#'   ─────────────────  ──────   ────────────────────────────────────────
#'   exact_last_name      40     UPPER(trimws()) equality
#'   exact_first_name     20     full first name match (vs 12 for initial only)
#'   first_initial_ok     12     first character of first name matches
#'   zip_match            10     5-digit zip equality
#'   city_match            6     UPPER(trimws()) city equality
#'   specialty_signal     12     NPPES taxonomy in 207Y family
#'   phone_match           8     10-digit normalized phone equality
#'   ─────────────────  ──────
#'   MAX POSSIBLE        108
#'   ACCEPT THRESHOLD     70     require at least name + one corroborating
#' signal
#' }
#'
#' @section Output Schema:
#' \code{data/ent/enthealth_npi_LATEST.csv}
#' \describe{
#'   \item{enthealth_id}{Stable row identifier (SHA-1 of url)}
#'   \item{npi}{10-digit National Provider Identifier}
#'   \item{ent_subspecialty}{Pipeline subspecialty code from
#' config/taxonomy_codes_ent.yml}
#'   \item{full_name}{Original name from enthealth.org}
#'   \item{first_clean}{Parsed first name}
#'   \item{last_clean}{Parsed last name}
#'   \item{company_name}{Practice name from enthealth.org}
#'   \item{address}{Street address}
#'   \item{city}{City}
#'   \item{state}{State code}
#'   \item{zip}{5-digit ZIP}
#'   \item{phone}{Normalized phone (10-digit)}
#'   \item{specialty_primary}{Specialty from enthealth.org}
#'   \item{match_strategy}{Strategy that produced the match (1–4)}
#'   \item{match_confidence}{Confidence score (0.0–1.0)}
#'   \item{match_score}{Raw numeric score (0–108)}
#'   \item{nppes_first_name}{NPPES first name for audit}
#'   \item{nppes_last_name}{NPPES last name for audit}
#'   \item{nppes_state}{NPPES state for audit}
#' }
#'
#' @section Environment Variables:
#' \describe{
#'   \item{TEST_MODE (0|1)}{RI+NJ subset only (default: 0)}
#'   \item{MOCK_MODE (0|1)}{In-memory synthetic data (default: 0)}
#'   \item{PIPELINE_LOAD_ONLY (0|1)}{Load functions without executing (default:
#' 0)}
#'   \item{ENT_NPPES_PARQUET}{Override path to NPPES candidate parquet}
#'   \item{ENT_HEALTH_XLSX}{Override path to enthealth.org Excel file}
#' }
#'
#' @section Usage:
#' \preformatted{
#' # Production run
#' Rscript scripts/match_enthealth_to_npi.R
#'
#' # TEST_MODE (RI + NJ subset, ~1,500 NPPES candidates)
#' TEST_MODE=1 Rscript scripts/match_enthealth_to_npi.R
#'
#' # Load functions without executing
#' PIPELINE_LOAD_ONLY=1 Rscript scripts/match_enthealth_to_npi.R
#'
#' # Prerequisite: build NPPES ENT candidate pool first (30-60 min)
#' caffeinate -idms Rscript scripts/geocode_ent_from_nppes_api.R
#' }
#'
#' @section Prerequisites:
#' \itemize{
#'   \item Run \code{scripts/geocode_ent_from_nppes_api.R} first to build
#'     \code{artifacts/ent/ent_physicians_geocoded.parquet}
#'   \item \code{readxl} package installed (\code{install.packages("readxl")})
#'   \item enthealth.org Excel file accessible at path in \code{ENT_HEALTH_XLSX}
#'     or the default Dropbox path
#' }
#'
#' @author Tyler Muffly, MD + Claude Code
#' @date 2026-05-02

# ==============================================================================
# HALL OF SHAME #28 GUARD: Validate here::here() resolves to project root
# ==============================================================================
if (!file.exists(here::here("CLAUDE.md"))) {
  stop(
    "[FATAL] here::here() resolves to '", here::here(), "' which is not the ",
    "isochrones project root (CLAUDE.md not found). Run this script from the ",
    "project root: Rscript scripts/match_enthealth_to_npi.R",
    call. = FALSE
  )
}

# ==============================================================================
# PACKAGES
# ==============================================================================
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(readxl)
  library(arrow)
  library(stringr)
  library(stringdist)
  library(digest)
  library(yaml)
  library(checkmate)
})

# Source the name parser once at startup (not inside a function with local=TRUE,
# which isolates the parser's own internal functions from its auto-run test block)
.name_parser_path <- here("R", "name_parsing_protocol_enhanced.R")
if (file.exists(.name_parser_path)) {
  tryCatch(
    source(.name_parser_path),
    error = function(e) message("[STARTUP] Name parser load warning: ", e$message)
  )
}

# Canonical one-to-one resolver. Unlike the name parser above, a failure here is
# NOT recoverable and must not degrade to a message: without it there is no
# identity-resolution rule at all, and the failure would surface much later as a
# missing-function error mid-pipeline. Fail at startup instead.
.npi_resolution_path <- here("R", "npi_resolution.R")
if (!file.exists(.npi_resolution_path)) {
  stop("[STARTUP] R/npi_resolution.R not found; rank_one_to_one() is unavailable.",
       call. = FALSE)
}
source(.npi_resolution_path)
stopifnot(exists("rank_one_to_one", mode = "function"))

# ==============================================================================
# TEST MODE
# ==============================================================================
is_test_mode <- function() identical(Sys.getenv("TEST_MODE"), "1")
is_mock_mode <- function() identical(Sys.getenv("MOCK_MODE"), "1")
is_load_only <- function() identical(Sys.getenv("PIPELINE_LOAD_ONLY"), "1")

if (is_test_mode()) {
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                    TEST MODE ENABLED                          ║\n")
  cat("║  Subset: RI + NJ only (~1,500 NPPES ENT candidates)           ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n\n")
  TEST_STATES <- c("RI", "NJ")
} else {
  TEST_STATES <- NULL
}

# ==============================================================================
# CONSTANTS
# ==============================================================================
OUTPUT_DIR        <- here("data", "ent")
NPPES_PARQUET_DEFAULT <- here("artifacts", "ent", "ent_physicians_geocoded.parquet")
# Filtered-and-cached parquet extracted from the Feb 2026 flat file (built once, reused)
NPPES_ENT_EXTRACTED_PARQUET <- here("artifacts", "ent", "ent_from_nppes_flat_file.parquet")
# February 2026 NPPES full dissemination file (10 GB, on external drive)
NPPES_FLAT_FILE_DEFAULT <- file.path(
  "/Volumes/MufflySamsung 1",
  "nppes_historical_downloads",
  "NPPES_Data_Dissemination_February_2026",
  "npidata_pfile_20050523-20260208.csv"
)
DROPBOX_XLSX_DEFAULT  <- file.path(
  path.expand("~"), "Dropbox (Personal)",
  "mystery_shopper", "Corbi study", "ENT", "Data", "results.xlsx"
)
ENT_TAXONOMY_PATTERN <- "^207Y"
SCORE_ACCEPT_THRESHOLD <- 70L   # min score to emit a match
CONFIDENCE_ACCEPT_THRESHOLD <- 0.85

# ==============================================================================
# SAFE ARITHMETIC HELPERS (identical contract to canonical_abog_npi_pipeline)
# ==============================================================================

#' Division-Safe Percentage
safe_pct <- function(n, d) {
  if (length(d) != 1L || is.na(d) || d == 0) return(0)
  if (length(n) != 1L) return(0)
  100 * n / d
}

#' Division-Safe Rate
safe_rate <- function(n, d) {
  if (length(d) != 1L || is.na(d) || d == 0) return(0)
  if (length(n) != 1L) return(0)
  n / d
}

# ==============================================================================
# UTILITY: PHONE NORMALIZATION
# ==============================================================================

#' Normalize phone numbers to 10-digit strings
#' Strips all non-digit characters; returns NA if not exactly 10 digits.
normalize_phone <- function(x) {
  digits <- gsub("[^0-9]", "", as.character(x))
  dplyr::if_else(nchar(digits) == 10L, digits, NA_character_)
}

#' Strip leading country code from 11-digit US numbers (1XXXXXXXXXX →
#' XXXXXXXXXX)
strip_country_code <- function(x) {
  dplyr::if_else(
    !is.na(x) & nchar(x) == 11L & substr(x, 1, 1) == "1",
    substr(x, 2, 11),
    x
  )
}

# ==============================================================================
# UTILITY: ENTHEALTH_ID GENERATION
# ==============================================================================

#' Generate a stable enthealth_id from the URL column
#' Uses SHA-1 of the URL; falls back to row index if URL is missing.
make_enthealth_id <- function(url_col, row_idx) {
  vapply(seq_along(url_col), function(i) {
    if (!is.na(url_col[i]) && nzchar(url_col[i])) {
      paste0("EH_", substr(digest::digest(url_col[i], algo = "sha1"), 1, 12))
    } else {
      sprintf("EH_ROW%06d", row_idx[i])
    }
  }, character(1))
}

# ==============================================================================
# UTILITY: NAME NORMALIZATION
# ==============================================================================
# normalize_name_key() lives in R/string_normalization.R, which is the single
# definition of name normalisation for this repo. Sourced rather than copied:
# a second copy is how two pipelines quietly disagree about who matched whom.
if (!exists("normalize_name_key")) source(here::here("R", "string_normalization.R"))

#' Normalize name string: upper, trim, collapse internal whitespace
norm_name <- function(x) {
  # NOTE (2026-08-08): this function was briefly "fixed" to preserve NA on the
  # mistaken belief that toupper() stringified missing values. It does not --
  # as.character/gsub/trimws/toupper all propagate NA correctly -- so the guard
  # was a no-op and has been reverted. The real defect was in a CALLER, which
  # built a middle name with paste() (paste turns NA into the literal "NA").
  # See tests/testthat/test-norm-name-missingness.R.
  #
  # UNICODE (2026-08-10). The body was toupper(trimws(gsub("\\s+", " ", x))),
  # which PRESERVES accents rather than transliterating them: "Álvarez" became
  # "ÁLVAREZ" and first_initial() returned "Á", which can never equal NPPES
  # "A". Both sides of every join below run through this, so an accented name
  # could not reach its unaccented registry spelling by any route, and the row
  # reported as "no candidate" rather than as a failure. Delegates to the
  # canonical normalizer; BUG FIX #7 made the same migration on the ABOG path.
  #
  # normalize_name_key(), not normalize_string(): the latter leaves INTERNAL
  # whitespace alone, so a bare swap would turn "John  Smith" into
  # "JOHN  SMITH" and break matches that currently work.
  normalize_name_key(x)
}

# H2 migration (PI ruling 2026-09-27): the local first_initial() shadow is
# RETIRED; both call sites use the package-owned primitive directly -- one
# rule, one implementation (shadow ban:
# tests/testthat/test-identity-negative-contracts.R, baseline shrunk in the
# same change).

# ==============================================================================
# DATA LOADING
# ==============================================================================

#' Load and validate the enthealth.org scrape
#'
#' @param xlsx_path Path to results.xlsx. Defaults to ENV var ENT_HEALTH_XLSX
#'   or the Dropbox path.
#' @return data.frame with enthealth_id + cleaned columns
load_enthealth_data <- function(xlsx_path = NULL) {
  if (is.null(xlsx_path)) {
    xlsx_path <- Sys.getenv("ENT_HEALTH_XLSX", unset = "")
    if (!nzchar(xlsx_path)) xlsx_path <- DROPBOX_XLSX_DEFAULT
  }

  checkmate::assert_file_exists(xlsx_path, .var.name = "ENT Health Excel file")

  cat(sprintf("[LOAD] Reading enthealth.org scrape: %s\n", xlsx_path))
  df <- readxl::read_excel(xlsx_path, col_types = "text")
  n_raw <- nrow(df)
  cat(sprintf("[LOAD]   %s rows loaded\n", format(n_raw, big.mark = ",")))

  # Validate expected columns
  required_cols <- c("url", "full_name", "address", "city", "state_code",
                     "post_code", "phone_number", "specialty_primary")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "[LOAD] Missing expected columns in enthealth Excel: %s\n  Available: %s",
      paste(missing_cols, collapse = ", "),
      paste(names(df), collapse = ", ")
    ), call. = FALSE)
  }

  df <- df %>%
    dplyr::mutate(
      enthealth_id    = make_enthealth_id(url, seq_len(n())),
      zip5            = substr(gsub("[^0-9]", "", post_code), 1, 5),
      phone_norm      = strip_country_code(normalize_phone(phone_number)),
      state           = toupper(trimws(state_code)),
      city_norm       = norm_name(city),
      # Name parsing: split full_name into first/last using project parser
      first_raw       = NA_character_,
      last_raw        = NA_character_
    )

  # Use project name parser (sourced at startup — do NOT re-source here)
  if (exists("parse_physician_name_enhanced", mode = "function")) {
    cat("[LOAD]   Applying parse_physician_name_enhanced() to full_name...\n")
    parsed_names <- tryCatch(
      parse_physician_name_enhanced(df$full_name),
      error = function(e) {
        warning(sprintf("[LOAD] Name parser failed: %s — falling back to split", e$message))
        NULL
      }
    )
    if (!is.null(parsed_names) && is.data.frame(parsed_names)) {
      first_col <- intersect(c("first_name", "first", "fname"), names(parsed_names))[1]
      last_col  <- intersect(c("last_name",  "last",  "lname"), names(parsed_names))[1]
      if (!is.na(first_col)) df$first_raw <- parsed_names[[first_col]]
      if (!is.na(last_col))  df$last_raw  <- parsed_names[[last_col]]
    }
  }

  # Fallback: split on last space
  needs_parse <- is.na(df$first_raw) | is.na(df$last_raw)
  if (any(needs_parse)) {
    cat(sprintf("[LOAD]   Fallback name split for %d records\n", sum(needs_parse)))
    split_names <- stringr::str_split_fixed(trimws(df$full_name[needs_parse]), "\\s+", n = 2)
    df$first_raw[needs_parse] <- split_names[, 1]
    df$last_raw[needs_parse]  <- split_names[, 2]
  }

  df <- df %>%
    dplyr::mutate(
      first_clean = norm_name(first_raw),
      last_clean  = norm_name(last_raw),
      first_init  = mysterynpi::extract_first_initial(first_clean)
    )

  n_name_fail <- sum(is.na(df$last_clean) | df$last_clean == "", na.rm = TRUE)
  if (n_name_fail > 0) {
    warning(sprintf("[LOAD] %d records have empty last_clean — these will not match", n_name_fail))
  }

  # TEST_MODE: restrict to subset states
  if (is_test_mode() && !is.null(TEST_STATES)) {
    before <- nrow(df)
    df <- df %>% dplyr::filter(state %in% TEST_STATES)
    cat(sprintf("[TEST_MODE] Filtered enthealth to %s rows (%s states: %s)\n",
                format(nrow(df), big.mark = ","),
                length(TEST_STATES),
                paste(TEST_STATES, collapse = ", ")))
    if (nrow(df) < 5) {
      warning(sprintf(
        "[TEST_MODE] Only %d rows remain after state filter — enthealth may have few records in %s",
        nrow(df), paste(TEST_STATES, collapse = "/")
      ))
    }
  }

  cat(sprintf("[LOAD]   %s records after cleaning\n\n", format(nrow(df), big.mark = ",")))
  df
}

# ==============================================================================
# TEST_MODE NPPES API FALLBACK
# Queries the NPPES REST API for ENT physicians in a small set of states.
# Used only when the pre-built parquet is absent and TEST_MODE=1.
# ==============================================================================

#' Query NPPES API for ENT physicians in given states (TEST_MODE fallback)
.query_nppes_api_for_states <- function(states) {
  suppressPackageStartupMessages(library(httr))
  suppressPackageStartupMessages(library(jsonlite))

  ENT_CODES <- c("207Y00000X","207YS0123X","207YX0602X","207YX0901X",
                 "207YX0905X","207YP0228X","207YS0012X")
  NPPES_BASE <- "https://npiregistry.cms.hhs.gov/api/"
  all_rows <- list()

  for (state in states) {
    cat(sprintf("[NPPES API] Querying state %s...\n", state))
    skip <- 0L
    repeat {
      resp <- tryCatch(
        httr::GET(NPPES_BASE, query = list(
          version = "2.1", taxonomy_description = "otolaryngology",
          enumeration_type = "NPI-1", state = state,
          limit = 200, skip = skip
        ), httr::timeout(30)),
        error = function(e) NULL
      )
      if (is.null(resp) || httr::status_code(resp) != 200) break

      parsed <- tryCatch(
        jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                           simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (is.null(parsed) || is.null(parsed$results) ||
          length(parsed$results) == 0) break

      for (r in parsed$results) {
        basic  <- r$basic %||% list()
        addrs  <- r$addresses %||% list()
        loc    <- Filter(function(a) identical(a$address_purpose, "LOCATION"), addrs)
        addr   <- if (length(loc) > 0) loc[[1]] else if (length(addrs) > 0) addrs[[1]] else list()
        taxs   <- r$taxonomies %||% list()
        codes  <- vapply(taxs, function(t) t$code %||% "", character(1))
        if (!any(codes %in% ENT_CODES)) next

        all_rows[[length(all_rows) + 1]] <- data.frame(
          npi                = as.character(r$number %||% NA),
          first_name         = basic$first_name        %||% NA_character_,
          last_name          = basic$last_name         %||% NA_character_,
          organization_name  = basic$organization_name %||% NA_character_,
          credential         = basic$credential        %||% NA_character_,
          gender             = basic$gender            %||% NA_character_,
          address_line1      = addr$address_1          %||% NA_character_,
          city               = addr$city               %||% NA_character_,
          state              = addr$state              %||% NA_character_,
          zip                = addr$postal_code        %||% NA_character_,
          phone              = addr$telephone_number   %||% NA_character_,
          primary_taxonomy   = if (length(codes) > 0) codes[1] else NA_character_,
          all_taxonomy_codes = paste(codes, collapse = "|"),
          stringsAsFactors   = FALSE
        )
      }

      if (length(parsed$results) < 200) break
      skip <- skip + 200L
      Sys.sleep(0.5)
    }
    cat(sprintf("[NPPES API]   %s ENT rows so far\n",
                format(length(all_rows), big.mark = ",")))
  }

  if (length(all_rows) == 0) {
    stop("[NPPES API] No ENT records returned — check network connectivity.", call. = FALSE)
  }
  df <- dplyr::bind_rows(all_rows)
  df[!duplicated(df$npi), ]
}

#' Extract ENT physicians from the NPPES Feb 2026 flat file using DuckDB
#'
#' Filters the 10 GB npidata CSV for Entity Type = 1 and any 207Y% taxonomy
#' code.
#' Saves result as a parquet cache so subsequent calls are instantaneous.
#' Returns NULL (invisibly) when the flat file is not present (e.g., external
#' drive unmounted).
.load_nppes_ent_from_flat_file <- function(flat_file = NULL,
                                           cache_path = NPPES_ENT_EXTRACTED_PARQUET) {
  if (is.null(flat_file)) {
    flat_file <- Sys.getenv("ENT_NPPES_FLAT_FILE", unset = NPPES_FLAT_FILE_DEFAULT)
  }

  # Return cached parquet when available and newer than flat file
  if (file.exists(cache_path)) {
    if (!file.exists(flat_file) ||
        file.mtime(cache_path) >= file.mtime(flat_file)) {
      cat(sprintf("[FLAT_FILE] Using cached ENT extract: %s\n", cache_path))
      df <- arrow::read_parquet(cache_path)
      cat(sprintf("[FLAT_FILE]   %s ENT physicians (cached)\n", format(nrow(df), big.mark = ",")))
      return(df)
    }
  }

  if (!file.exists(flat_file)) {
    message(sprintf("[FLAT_FILE] Flat file not found: %s\n  (Is /Volumes/MufflySamsung 1 mounted?)", flat_file))
    return(invisible(NULL))
  }

  cat(sprintf("[FLAT_FILE] Extracting ENT physicians from Feb 2026 NPPES flat file (~30s)...\n  %s\n", flat_file))

  suppressPackageStartupMessages({
    library(DBI)
    library(duckdb)
  })

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit({try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)}, add = TRUE)

  sql <- sprintf("
    SELECT
      \"NPI\"                                                          AS npi,
      \"Provider Last Name (Legal Name)\"                              AS last_name,
      \"Provider First Name\"                                          AS first_name,
      \"Provider Middle Name\"                                         AS middle_name,
      \"Provider Organization Name (Legal Business Name)\"             AS organization_name,
      \"Provider Credential Text\"                                     AS credential,
      \"Provider First Line Business Practice Location Address\"       AS address_line1,
      \"Provider Business Practice Location Address City Name\"        AS city,
      \"Provider Business Practice Location Address State Name\"       AS state,
      \"Provider Business Practice Location Address Postal Code\"      AS zip,
      \"Provider Business Practice Location Address Telephone Number\" AS phone,
      \"Healthcare Provider Taxonomy Code_1\"                          AS taxonomy_1,
      \"Healthcare Provider Taxonomy Code_2\"                          AS taxonomy_2,
      \"Healthcare Provider Taxonomy Code_3\"                          AS taxonomy_3,
      \"Healthcare Provider Taxonomy Code_4\"                          AS taxonomy_4
    FROM read_csv_auto('%s', header = true, quote = '\"', escape = '\"')
    WHERE \"Entity Type Code\" = '1'
      AND (
           \"Healthcare Provider Taxonomy Code_1\" LIKE '207Y%%%%'
        OR \"Healthcare Provider Taxonomy Code_2\" LIKE '207Y%%%%'
        OR \"Healthcare Provider Taxonomy Code_3\" LIKE '207Y%%%%'
        OR \"Healthcare Provider Taxonomy Code_4\" LIKE '207Y%%%%'
      )
  ", flat_file)

  df <- DBI::dbGetQuery(con, sql)
  cat(sprintf("[FLAT_FILE]   %s ENT individual providers extracted\n", format(nrow(df), big.mark = ",")))

  ENT_CODES <- c("207Y00000X","207YS0123X","207YX0602X","207YX0901X",
                 "207YX0905X","207YP0228X","207YS0012X")

  df <- df %>%
    dplyr::mutate(
      primary_taxonomy = dplyr::coalesce(
        dplyr::if_else(taxonomy_1 %in% ENT_CODES, taxonomy_1, NA_character_),
        dplyr::if_else(taxonomy_2 %in% ENT_CODES, taxonomy_2, NA_character_),
        dplyr::if_else(taxonomy_3 %in% ENT_CODES, taxonomy_3, NA_character_),
        taxonomy_1
      ),
      all_taxonomy_codes = purrr::pmap_chr(
        list(taxonomy_1, taxonomy_2, taxonomy_3, taxonomy_4),
        function(t1, t2, t3, t4) paste(na.omit(c(t1, t2, t3, t4)), collapse = "|")
      )
    ) %>%
    dplyr::select(-taxonomy_1, -taxonomy_2, -taxonomy_3, -taxonomy_4)

  # Cache for reuse
  cache_dir <- dirname(cache_path)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, paste0(cache_path, ".tmp"))
  file.rename(paste0(cache_path, ".tmp"), cache_path)
  cat(sprintf("[FLAT_FILE]   Cached to %s\n", cache_path))

  df
}

#' Normalize a raw NPPES data.frame to the standard candidate schema
.normalize_nppes_df <- function(df) {
  has_org <- "organization_name" %in% names(df)
  df %>%
    dplyr::mutate(
      nppes_first_clean = norm_name(first_name),
      nppes_last_clean  = norm_name(last_name),
      nppes_first_init  = mysterynpi::extract_first_initial(nppes_first_clean),
      nppes_zip5        = substr(gsub("[^0-9]", "", zip), 1, 5),
      nppes_city_norm   = norm_name(city),
      nppes_phone_norm  = strip_country_code(normalize_phone(phone)),
      nppes_state       = toupper(trimws(state)),
      nppes_org_norm    = if (has_org) norm_name(organization_name) else NA_character_,
      is_ent_taxonomy   = grepl(ENT_TAXONOMY_PATTERN, all_taxonomy_codes)
    )
}

#' Load the NPPES ENT candidate pool
#'
#' Cascade (first hit wins):
#'   1. Pre-built geocoded parquet (fastest — run geocode_ent_from_nppes_api.R
#' once)
#'   2. NPPES Feb 2026 flat file on /Volumes/MufflySamsung 1 — filtered by
#' DuckDB (~30s),
#'      result cached as artifacts/ent/ent_from_nppes_flat_file.parquet
#'   3. TEST_MODE only: inline NPPES API query for test states (RI + NJ)
#'
#' @param parquet_path Path override for geocoded parquet. Defaults to ENV var
#'   ENT_NPPES_PARQUET or the standard artifacts path.
#' @param flat_file Path override for the NPPES CSV. Defaults to ENV var
#'   ENT_NPPES_FLAT_FILE or NPPES_FLAT_FILE_DEFAULT.
#' @return data.frame of NPPES ENT physicians with normalized name/address
#'   fields
load_nppes_ent_candidates <- function(parquet_path = NULL, flat_file = NULL) {

  # ── Path 1: pre-built geocoded parquet ────────────────────────────────────
  if (is.null(parquet_path)) {
    parquet_path <- Sys.getenv("ENT_NPPES_PARQUET", unset = "")
    if (!nzchar(parquet_path)) parquet_path <- NPPES_PARQUET_DEFAULT
  }

  if (file.exists(parquet_path)) {
    cat(sprintf("[LOAD] Reading geocoded NPPES ENT parquet: %s\n", parquet_path))
    df <- arrow::read_parquet(parquet_path)
    cat(sprintf("[LOAD]   %s ENT physicians loaded\n", format(nrow(df), big.mark = ",")))

    required_cols <- c("npi", "first_name", "last_name", "state", "zip")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf(
        "[LOAD] Missing columns in NPPES parquet: %s\n  Available: %s",
        paste(missing_cols, collapse = ", "),
        paste(names(df), collapse = ", ")
      ), call. = FALSE)
    }
    if (!"all_taxonomy_codes" %in% names(df)) {
      tax_col <- intersect(c("primary_taxonomy", "taxonomy_code", "taxonomies"), names(df))[1]
      df$all_taxonomy_codes <- if (!is.na(tax_col)) df[[tax_col]] else NA_character_
    }
    df <- .normalize_nppes_df(df)

    if (is_test_mode() && !is.null(TEST_STATES)) {
      df <- df %>% dplyr::filter(nppes_state %in% TEST_STATES)
      cat(sprintf("[TEST_MODE] NPPES pool restricted to %s physicians in %s\n",
                  format(nrow(df), big.mark = ","), paste(TEST_STATES, collapse = "/")))
    }
    cat(sprintf("[LOAD]   %s usable NPPES ENT candidates (geocoded parquet)\n\n",
                format(nrow(df), big.mark = ",")))
    return(df)
  }

  # ── Path 2: NPPES Feb 2026 flat file (DuckDB-filtered, cached as parquet) ─
  df_flat <- .load_nppes_ent_from_flat_file(flat_file = flat_file)
  if (!is.null(df_flat)) {
    df_flat <- .normalize_nppes_df(df_flat)
    if (is_test_mode() && !is.null(TEST_STATES)) {
      df_flat <- df_flat %>% dplyr::filter(nppes_state %in% TEST_STATES)
      cat(sprintf("[TEST_MODE] Flat file pool restricted to %s physicians in %s\n",
                  format(nrow(df_flat), big.mark = ","), paste(TEST_STATES, collapse = "/")))
    }
    cat(sprintf("[LOAD]   %s usable NPPES ENT candidates (Feb 2026 flat file)\n\n",
                format(nrow(df_flat), big.mark = ",")))
    return(df_flat)
  }

  # ── Path 3: TEST_MODE only — inline NPPES API query ───────────────────────
  if (is_test_mode()) {
    cat("[TEST_MODE] No parquet or flat file available — querying NPPES API for test states...\n")
    df_api <- .query_nppes_api_for_states(TEST_STATES)
    return(.normalize_nppes_df(df_api))
  }

  stop(sprintf(
    "[LOAD] NPPES ENT candidate pool not found.\n  Tried parquet: %s\n  Flat file: %s\n  Mount /Volumes/MufflySamsung 1 or run scripts/geocode_ent_from_nppes_api.R.",
    parquet_path,
    Sys.getenv("ENT_NPPES_FLAT_FILE", unset = NPPES_FLAT_FILE_DEFAULT)
  ), call. = FALSE)
}

# ==============================================================================
# MATCH SCORING (identical structure to canonical_abog_npi_pipeline_STABLE.R)
# ==============================================================================

#' Compute composite match score for enthealth ↔ NPPES candidate pairs
#'
#' @param candidates data.frame with boolean flag columns:
#'   exact_last, exact_first, first_init_ok, zip_match, city_match,
#'   specialty_signal, phone_match
#' @return candidates with score_name, score_geo, score_spec, score_phone,
#'   and score_total columns added
compute_match_score <- function(candidates) {
  ensure_col <- function(df, col) {
    if (!col %in% names(df)) df[[col]] <- 0L
    df
  }
  flag_cols <- c("exact_last", "exact_first", "first_init_ok",
                 "zip_match", "city_match", "specialty_signal", "phone_match")
  for (col in flag_cols) candidates <- ensure_col(candidates, col)

  # Guard: coerce factor flag columns to numeric (same saboteur-probe fix as ABOG)
  for (col in flag_cols) {
    if (is.factor(candidates[[col]])) {
      candidates[[col]] <- as.character(candidates[[col]])
    }
  }

  candidates %>%
    dplyr::mutate(
      score_name  = 40L * as.integer(dplyr::coalesce(as.numeric(exact_last),      0)) +
                    20L * as.integer(dplyr::coalesce(as.numeric(exact_first),     0)) +
                    12L * as.integer(dplyr::coalesce(as.numeric(first_init_ok),   0)) *
                          as.integer(!dplyr::coalesce(as.numeric(exact_first),    0)),
      score_geo   = 10L * as.integer(dplyr::coalesce(as.numeric(zip_match),       0)) +
                     6L * as.integer(dplyr::coalesce(as.numeric(city_match),      0)),
      score_spec  = 12L * as.integer(dplyr::coalesce(as.numeric(specialty_signal), 0)),
      score_phone =  8L * as.integer(dplyr::coalesce(as.numeric(phone_match),     0)),
      score_total = score_name + score_geo + score_spec + score_phone
    )
}

#' Resolve candidates to one-to-one matches (delegates to R/npi_resolution.R)
#'
#' This script previously carried its OWN greedy copy of rank_one_to_one():
#'   arrange(...) %>% distinct(npi) %>% arrange(...) %>% distinct(enthealth_id)
#' It was the fourth copy in the repo and survived the 2026-08-08 consolidation
#' of the other three, because it keys on `enthealth_id` rather than `abog_id`
#' and so did not look like a duplicate. It kept the two defects the
#' consolidation removed everywhere else: a record could look identified merely
#' because its only rival candidate had already been consumed by someone else,
#' and when two records claimed one NPI the winner was whoever sorted first.
#'
#' The canonical resolver is now parameterised by identity column, so this
#' delegates instead of reimplementing. Quarantined records (tied evidence, or a
#' contested NPI) are simply absent from the return value and therefore counted
#' as unmatched, which is the correct reading: they were never identified.
#'
#' @param candidates [data.frame]: scored candidates with `enthealth_id`, `npi`,
#'   `match_strategy`, `score_total`, `confidence_score`.
#' @return [data.frame] one row per `enthealth_id` and one row per `npi`, with a
#'   `quarantine` attribute carrying the unresolved claims.
resolve_enthealth_matches <- function(candidates) {
  if (nrow(candidates) == 0) return(candidates)

  # match_strategy (1-4) IS the priority order here; the canonical resolver
  # accepts a supplied method_priority and skips its ABOG method-name LUT.
  n_before <- nrow(candidates)
  candidates <- candidates %>%
    dplyr::mutate(method_priority = as.integer(match_strategy))
  stopifnot(nrow(candidates) == n_before)

  rank_one_to_one(candidates, id_col = "enthealth_id")
}

# ==============================================================================
# MATCHING STRATEGIES
# ==============================================================================

#' Strategy 1: Exact last name + state match
#'
#' Joins enthealth on UPPER(last_clean) + state against NPPES UPPER(last_name)
#' + state. Uses ENT taxonomy signal as a score boost.
#' Expected coverage: 55–70% of matchable records.
run_strategy_1_exact_name <- function(enthealth, nppes, verbose = TRUE) {
  if (verbose) cat("[STRATEGY 1] Exact last name + state...\n")

  candidates <- enthealth %>%
    dplyr::filter(!is.na(last_clean), nchar(last_clean) > 1L) %>%
    dplyr::inner_join(
      nppes %>% dplyr::select(
        npi, nppes_last_clean, nppes_first_clean, nppes_first_init,
        nppes_zip5, nppes_city_norm, nppes_state, nppes_phone_norm,
        is_ent_taxonomy, all_taxonomy_codes
      ),
      by = c("last_clean" = "nppes_last_clean", "state" = "nppes_state"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      exact_last      = TRUE,
      exact_first     = (first_clean == nppes_first_clean),
      first_init_ok   = (first_init  == nppes_first_init),
      zip_match       = (!is.na(zip5) & !is.na(nppes_zip5) & zip5 == nppes_zip5),
      city_match      = (!is.na(city_norm) & city_norm == nppes_city_norm),
      specialty_signal = is_ent_taxonomy,
      phone_match     = (!is.na(phone_norm) & !is.na(nppes_phone_norm) &
                          phone_norm == nppes_phone_norm),
      match_strategy   = 1L
    ) %>%
    compute_match_score() %>%
    dplyr::filter(score_total >= SCORE_ACCEPT_THRESHOLD) %>%
    dplyr::mutate(
      confidence_score = pmin(1.0, score_total / 108)
    )

  n <- nrow(dplyr::distinct(candidates, enthealth_id))
  if (verbose) cat(sprintf("[STRATEGY 1]   %s enthealth records matched\n", format(n, big.mark = ",")))
  candidates
}

#' Strategy 2: Fuzzy last name (Levenshtein ≤ 2) + zip code match
#'
#' Filters NPPES to same state, then applies stringdist to find near-matches on
#' last name. Requires zip5 match to control false-positive rate.
#' Expected coverage: additional 10–20%.
run_strategy_2_fuzzy_name_zip <- function(enthealth, nppes, verbose = TRUE) {
  if (verbose) cat("[STRATEGY 2] Fuzzy last name + zip code...\n")

  # Only run on unmatched records (passed in filtered)
  matchable <- enthealth %>%
    dplyr::filter(!is.na(last_clean), nchar(last_clean) > 2L,
                  !is.na(zip5),       nchar(zip5)       == 5L)

  if (nrow(matchable) == 0) {
    if (verbose) cat("[STRATEGY 2]   No matchable records — skipping\n")
    return(tibble::tibble())
  }

  # State-level join first (reduces cross-join size)
  state_pairs <- matchable %>%
    dplyr::inner_join(
      nppes %>% dplyr::select(
        npi, nppes_last_clean, nppes_first_clean, nppes_first_init,
        nppes_zip5, nppes_city_norm, nppes_state, nppes_phone_norm,
        is_ent_taxonomy
      ),
      by = c("state" = "nppes_state"),
      relationship = "many-to-many"
    )

  if (nrow(state_pairs) == 0) {
    if (verbose) cat("[STRATEGY 2]   0 state-level pairs — skipping\n")
    return(tibble::tibble())
  }

  # Levenshtein distance on last name; require zip5 match to reduce FP
  state_pairs <- state_pairs %>%
    dplyr::mutate(
      lv_last = stringdist::stringdist(last_clean, nppes_last_clean, method = "lv"),
      zip_match = (!is.na(zip5) & !is.na(nppes_zip5) & zip5 == nppes_zip5)
    ) %>%
    dplyr::filter(lv_last <= 2L, zip_match)

  if (nrow(state_pairs) == 0) {
    if (verbose) cat("[STRATEGY 2]   0 fuzzy+zip pairs — skipping\n")
    return(tibble::tibble())
  }

  candidates <- state_pairs %>%
    dplyr::mutate(
      exact_last      = (lv_last == 0L),
      exact_first     = (first_clean == nppes_first_clean),
      first_init_ok   = (first_init  == nppes_first_init),
      city_match      = (!is.na(city_norm) & city_norm == nppes_city_norm),
      specialty_signal = is_ent_taxonomy,
      phone_match     = (!is.na(phone_norm) & !is.na(nppes_phone_norm) &
                          phone_norm == nppes_phone_norm),
      match_strategy   = 2L
    ) %>%
    compute_match_score() %>%
    dplyr::filter(score_total >= SCORE_ACCEPT_THRESHOLD) %>%
    dplyr::mutate(
      confidence_score = pmin(1.0, score_total / 108) * (1 - 0.05 * lv_last)
    )

  n <- nrow(dplyr::distinct(candidates, enthealth_id))
  if (verbose) cat(sprintf("[STRATEGY 2]   %s additional enthealth records matched\n", format(n, big.mark = ",")))
  candidates
}

#' Strategy 3: Normalized phone number + state match
#'
#' Matches on 10-digit normalized phone number constrained to same state.
#' High precision (phone is near-unique) but lower recall (many NAs).
#' Expected coverage: additional 3–8%.
run_strategy_3_phone <- function(enthealth, nppes, verbose = TRUE) {
  if (verbose) cat("[STRATEGY 3] Phone number + state...\n")

  matchable <- enthealth %>%
    dplyr::filter(!is.na(phone_norm), nchar(phone_norm) == 10L)

  if (nrow(matchable) == 0) {
    if (verbose) cat("[STRATEGY 3]   No records with valid phone — skipping\n")
    return(tibble::tibble())
  }

  candidates <- matchable %>%
    dplyr::inner_join(
      nppes %>%
        dplyr::filter(!is.na(nppes_phone_norm), nchar(nppes_phone_norm) == 10L) %>%
        dplyr::select(
          npi, nppes_last_clean, nppes_first_clean, nppes_first_init,
          nppes_zip5, nppes_city_norm, nppes_state, nppes_phone_norm,
          is_ent_taxonomy
        ),
      by = c("phone_norm" = "nppes_phone_norm", "state" = "nppes_state"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      exact_last      = (last_clean  == nppes_last_clean),
      exact_first     = (first_clean == nppes_first_clean),
      first_init_ok   = (first_init  == nppes_first_init),
      zip_match       = (!is.na(zip5) & !is.na(nppes_zip5) & zip5 == nppes_zip5),
      city_match      = (!is.na(city_norm) & city_norm == nppes_city_norm),
      specialty_signal = is_ent_taxonomy,
      phone_match     = TRUE,
      match_strategy   = 3L
    ) %>%
    compute_match_score() %>%
    # Phone match alone is insufficient — require at least last name partial agreement
    dplyr::filter(score_total >= SCORE_ACCEPT_THRESHOLD | exact_last) %>%
    dplyr::mutate(
      confidence_score = pmin(1.0, score_total / 108)
    )

  n <- nrow(dplyr::distinct(candidates, enthealth_id))
  if (verbose) cat(sprintf("[STRATEGY 3]   %s additional enthealth records matched\n", format(n, big.mark = ",")))
  candidates
}

#' Strategy 4: Organization / practice name fuzzy match + state
#'
#' Matches enthealth \code{company_name} against NPPES organization name using
#' Jaro-Winkler distance. Constrained to same state. Low precision — only
#' accepts pairs where last name also matches approximately.
#' Expected coverage: additional 2–5%.
run_strategy_4_org_name <- function(enthealth, nppes, verbose = TRUE) {
  if (verbose) cat("[STRATEGY 4] Organization name + state...\n")

  # enthealth company_name (left side, 77% coverage) matched against
  # nppes_org_norm (right side — from flat file column 5; NA for most NPI-1 records
  # but populated for physicians enrolled under a group practice NPI).
  # Org-name similarity boosts the score for ambiguous name matches.

  matchable <- enthealth %>%
    dplyr::filter(!is.na(company_name), nchar(trimws(company_name)) > 3L)

  if (nrow(matchable) == 0) {
    if (verbose) cat("[STRATEGY 4]   No records with company_name — skipping\n")
    return(tibble::tibble())
  }

  # Only NPPES records that have a non-blank org name can be matched on org signal
  nppes_with_org <- nppes %>%
    dplyr::filter(!is.na(nppes_org_norm), nchar(trimws(nppes_org_norm)) > 3L)

  if (nrow(nppes_with_org) == 0) {
    if (verbose) cat("[STRATEGY 4]   NPPES candidate pool has no organization_name values — skipping\n")
    return(tibble::tibble())
  }

  if (verbose) cat(sprintf("[STRATEGY 4]   %s NPPES records with org names available\n",
                            format(nrow(nppes_with_org), big.mark = ",")))

  # State join first
  state_pairs <- matchable %>%
    dplyr::mutate(company_norm = norm_name(company_name)) %>%
    dplyr::inner_join(
      nppes_with_org %>%
        dplyr::select(npi, nppes_last_clean, nppes_first_clean, nppes_first_init,
                      nppes_zip5, nppes_city_norm, nppes_state, nppes_phone_norm,
                      is_ent_taxonomy, nppes_org_norm),
      by = c("state" = "nppes_state"),
      relationship = "many-to-many"
    )

  if (nrow(state_pairs) == 0) {
    if (verbose) cat("[STRATEGY 4]   0 state-level pairs — skipping\n")
    return(tibble::tibble())
  }

  # Jaro-Winkler on org name; require last name closeness to suppress FP
  candidates <- state_pairs %>%
    dplyr::mutate(
      jw_org  = stringdist::stringdist(company_norm, nppes_org_norm, method = "jw"),
      lv_last = stringdist::stringdist(last_clean, nppes_last_clean, method = "lv")
    ) %>%
    dplyr::filter(jw_org <= 0.15, lv_last <= 3L) %>%
    dplyr::mutate(
      exact_last      = (lv_last == 0L),
      exact_first     = (first_clean == nppes_first_clean),
      first_init_ok   = (first_init  == nppes_first_init),
      zip_match       = (!is.na(zip5) & !is.na(nppes_zip5) & zip5 == nppes_zip5),
      city_match      = (!is.na(city_norm) & city_norm == nppes_city_norm),
      specialty_signal = is_ent_taxonomy,
      phone_match     = (!is.na(phone_norm) & !is.na(nppes_phone_norm) &
                          phone_norm == nppes_phone_norm),
      match_strategy   = 4L
    ) %>%
    compute_match_score() %>%
    dplyr::filter(score_total >= SCORE_ACCEPT_THRESHOLD) %>%
    dplyr::mutate(
      confidence_score = pmin(1.0, score_total / 108) * (1 - 0.1 * jw_org)
    )

  n <- nrow(dplyr::distinct(candidates, enthealth_id))
  if (verbose) cat(sprintf("[STRATEGY 4]   %s additional enthealth records matched\n", format(n, big.mark = ",")))
  candidates
}

# ==============================================================================
# SUBSPECIALTY ASSIGNMENT
# ==============================================================================

#' Map enthealth specialty_primary to pipeline ENT subspecialty code
#'
#' Uses enthealth_specialty_map from config/taxonomy_codes_ent.yml.
assign_ent_subspecialty <- function(specialty_primary) {
  tax_cfg_path <- here("config", "taxonomy_codes_ent.yml")
  if (!file.exists(tax_cfg_path)) {
    warning("[SUBSPEC] config/taxonomy_codes_ent.yml not found — subspecialty will be NA")
    return(rep(NA_character_, length(specialty_primary)))
  }
  tax_cfg <- yaml::read_yaml(tax_cfg_path)
  sp_map  <- tax_cfg$enthealth_specialty_map

  vapply(specialty_primary, function(sp) {
    if (is.na(sp) || !nzchar(sp)) return(NA_character_)
    mapped <- sp_map[[sp]]
    if (is.null(mapped)) NA_character_ else as.character(mapped)
  }, character(1))
}

# ==============================================================================
# CORE MATCHING PIPELINE FUNCTION
# ==============================================================================

#' ENTHealth → NPI matching pipeline
#'
#' @param enthealth_file Path to enthealth.org Excel file (results.xlsx)
#' @param nppes_parquet  Path to pre-built NPPES ENT parquet
#' @param output_dir     Directory for output files
#' @param output_file    Exact output path (overrides output_dir + timestamp)
#' @param verbose        Print progress messages
#' @param run_id         Run identifier (embedded in output filenames)
#' @return data.frame of matched records (one row per enthealth_id)
enthealth_npi_pipeline <- function(
  enthealth_file = NULL,
  nppes_parquet  = NULL,
  output_dir     = OUTPUT_DIR,
  output_file    = NULL,
  verbose        = TRUE,
  run_id         = NULL
) {
  # ── PARAMETER VALIDATION ────────────────────────────────────────────────────
  checkmate::assert_string(enthealth_file, null.ok = TRUE, .var.name = "enthealth_file")
  checkmate::assert_string(nppes_parquet,  null.ok = TRUE, .var.name = "nppes_parquet")
  checkmate::assert_string(output_dir,     null.ok = TRUE, .var.name = "output_dir")
  checkmate::assert_string(output_file,    null.ok = TRUE, .var.name = "output_file")
  checkmate::assert_flag(verbose,           .var.name = "verbose")
  checkmate::assert_string(run_id,         null.ok = TRUE, .var.name = "run_id")
  # ────────────────────────────────────────────────────────────────────────────

  start_time <- Sys.time()
  timestamp  <- format(start_time, "%Y%m%d_%H%M%S")
  if (is.null(run_id)) run_id <- timestamp

  if (verbose) {
    cat("\n")
    cat("=======================================================================\n")
    cat("   ENTHealth.org → NPI MATCHING PIPELINE\n")
    cat(sprintf("   Run ID: %s\n", run_id))
    cat(sprintf("   Mode:   %s\n",
                if (is_test_mode()) "TEST (RI+NJ)" else "PRODUCTION (national)"))
    cat("=======================================================================\n\n")
  }

  # ── PHASE 1: LOAD DATA ──────────────────────────────────────────────────────
  enthealth <- load_enthealth_data(enthealth_file)
  nppes     <- load_nppes_ent_candidates(nppes_parquet)

  n_enthealth_total <- nrow(enthealth)

  # ── PHASE 2: MATCHING CASCADE ───────────────────────────────────────────────
  if (verbose) cat("[CASCADE] Starting multi-strategy matching cascade...\n\n")

  all_candidates <- list()
  remaining <- enthealth

  run_strategy_step <- function(strategy_fn, label) {
    if (nrow(remaining) == 0) {
      if (verbose) cat(sprintf("[%s] No remaining unmatched records — skipping\n\n", label))
      return(invisible(NULL))
    }
    result <- strategy_fn(remaining, nppes, verbose = verbose)
    if (!is.null(result) && nrow(result) > 0) {
      all_candidates[[label]] <<- result
      newly_matched <- unique(result$enthealth_id)
      remaining <<- remaining %>%
        dplyr::filter(!enthealth_id %in% newly_matched)
      if (verbose) {
        cat(sprintf("[%s] Matched: %s | Remaining: %s\n\n",
                    label,
                    format(length(newly_matched), big.mark = ","),
                    format(nrow(remaining), big.mark = ",")))
      }
    } else {
      if (verbose) cat(sprintf("[%s] 0 new matches\n\n", label))
    }
  }

  run_strategy_step(run_strategy_1_exact_name,    "S1")
  run_strategy_step(run_strategy_2_fuzzy_name_zip, "S2")
  run_strategy_step(run_strategy_3_phone,          "S3")
  run_strategy_step(run_strategy_4_org_name,       "S4")

  # ── PHASE 3: COMBINE + DEDUPLICATE ──────────────────────────────────────────
  if (length(all_candidates) == 0) {
    warning("[PIPELINE] No matches produced by any strategy.")
    return(tibble::tibble())
  }

  combined <- dplyr::bind_rows(all_candidates)

  if (verbose) {
    cat(sprintf("[DEDUP] %s total candidates before one-to-one ranking\n",
                format(nrow(combined), big.mark = ",")))
  }

  final_matches <- resolve_enthealth_matches(combined)

  n_matched   <- nrow(final_matches)
  n_unmatched <- n_enthealth_total - n_matched
  match_rate  <- safe_pct(n_matched, n_enthealth_total)

  if (verbose) {
    cat(sprintf("\n[RESULTS] Match rate: %.1f%% (%s / %s)\n",
                match_rate,
                format(n_matched, big.mark = ","),
                format(n_enthealth_total, big.mark = ",")))
    cat(sprintf("[RESULTS] Unmatched:  %s\n", format(n_unmatched, big.mark = ",")))
    cat("\nStrategy breakdown:\n")
    strat_tbl <- final_matches %>%
      dplyr::count(match_strategy, name = "n") %>%
      dplyr::arrange(match_strategy)
    for (i in seq_len(nrow(strat_tbl))) {
      cat(sprintf("  Strategy %d: %s (%.1f%%)\n",
                  strat_tbl$match_strategy[i],
                  format(strat_tbl$n[i], big.mark = ","),
                  safe_pct(strat_tbl$n[i], n_matched)))
    }
    cat("\nConfidence distribution:\n")
    cat(sprintf("  Median: %.2f\n", median(final_matches$confidence_score, na.rm = TRUE)))
    cat(sprintf("  ≥0.90:  %s (%.0f%%)\n",
                format(sum(final_matches$confidence_score >= 0.90, na.rm = TRUE), big.mark = ","),
                safe_pct(sum(final_matches$confidence_score >= 0.90, na.rm = TRUE), n_matched)))
  }

  # ── PHASE 4: SUBSPECIALTY ASSIGNMENT ─────────────────────────────────────
  final_matches <- final_matches %>%
    dplyr::mutate(
      ent_subspecialty = assign_ent_subspecialty(specialty_primary)
    )

  # ── PHASE 5: SELECT OUTPUT COLUMNS ──────────────────────────────────────────
  output_cols <- c(
    "enthealth_id", "npi", "ent_subspecialty",
    "full_name", "first_clean", "last_clean",
    "company_name", "address", "city", "state", "zip5", "phone_norm",
    "specialty_primary", "specialty_secondary",
    "match_strategy", "confidence_score", "match_score" = "score_total",
    "nppes_first_clean", "nppes_last_clean", "nppes_state"
  )
  # Use only columns that exist
  avail <- intersect(names(final_matches),
                     c("enthealth_id", "npi", "ent_subspecialty",
                       "full_name", "first_clean", "last_clean",
                       "company_name", "address", "city", "state", "zip5", "phone_norm",
                       "specialty_primary", "specialty_secondary",
                       "match_strategy", "confidence_score", "score_total",
                       "nppes_first_clean", "nppes_last_clean", "nppes_state"))
  final_matches <- final_matches %>% dplyr::select(dplyr::all_of(avail))
  if ("score_total" %in% names(final_matches)) {
    final_matches <- final_matches %>% dplyr::rename(match_score = score_total)
  }

  # ── PHASE 6: WRITE OUTPUTS ──────────────────────────────────────────────────
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  latest_path    <- file.path(output_dir, "enthealth_npi_LATEST.csv")
  timestamp_path <- file.path(output_dir, sprintf("enthealth_npi_%s.csv", run_id))

  write_path <- if (!is.null(output_file)) output_file else latest_path

  tmp_path <- paste0(write_path, ".tmp")
  readr::write_csv(final_matches, tmp_path)
  file.rename(tmp_path, write_path)
  if (write_path == latest_path) {
    file.copy(latest_path, timestamp_path, overwrite = TRUE)
  }

  # Unmatched artifact (for manual review)
  unmatched <- enthealth %>%
    dplyr::filter(!enthealth_id %in% final_matches$enthealth_id) %>%
    dplyr::select(enthealth_id, full_name, company_name, address, city, state,
                  zip5, specialty_primary)
  unmatched_path <- file.path(output_dir, "enthealth_npi_unmatched.csv")
  tmp_unmatched_path <- paste0(unmatched_path, ".tmp")
  readr::write_csv(unmatched, tmp_unmatched_path)
  file.rename(tmp_unmatched_path, unmatched_path)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  sha <- digest::digest(write_path, algo = "sha256", file = TRUE)

  receipt <- list(
    script          = "scripts/match_enthealth_to_npi.R",
    run_id          = run_id,
    timestamp       = timestamp,
    test_mode       = is_test_mode(),
    n_enthealth_in  = n_enthealth_total,
    n_matched       = n_matched,
    n_unmatched     = n_unmatched,
    match_rate_pct  = round(match_rate, 1),
    elapsed_minutes = round(elapsed, 1),
    output_path     = write_path,
    unmatched_path  = unmatched_path,
    sha256          = sha,
    hostname        = Sys.info()[["nodename"]]
  )
  receipt_path <- file.path(output_dir,
                            sprintf("receipt_enthealth_npi_%s.json", run_id))
  tmp_receipt_path <- paste0(receipt_path, ".tmp")
  writeLines(jsonlite::toJSON(receipt, auto_unbox = TRUE, pretty = TRUE),
             tmp_receipt_path)
  file.rename(tmp_receipt_path, receipt_path)

  # ── Dropbox backup ─────────────────────────────────────────────────────────
  # Upload enthealth_npi_LATEST.csv to /isochrones/ent/ on Dropbox so a clean
  # copy is preserved off-machine after every successful run.
  # Gate: AUTO_UPLOAD_ENT_NPI env var (default "1" = ON). Set to "0" to skip.
  # Non-fatal: upload failure never aborts the pipeline.
  tryCatch({
    if (!exists("auto_upload_ent_npi_to_dropbox", mode = "function"))
      source(here::here("R", "auto_upload_ent_npi.R"))
    auto_upload_ent_npi_to_dropbox(
      csv_path = latest_path,
      min_rows  = 50L,
      verbose   = verbose
    )
  }, error = function(e) {
    warning(sprintf("[ENT NPI Upload] Non-fatal error: %s", e$message))
  })

  if (verbose) {
    cat("\n=======================================================================\n")
    cat("                       COMPLETE\n")
    cat("=======================================================================\n")
    cat(sprintf("  Matched:    %s / %s (%.1f%%)\n",
                format(n_matched, big.mark = ","),
                format(n_enthealth_total, big.mark = ","),
                match_rate))
    cat(sprintf("  Elapsed:    %.1f minutes\n", elapsed))
    cat(sprintf("  Output:     %s\n", write_path))
    cat(sprintf("  Unmatched:  %s\n", unmatched_path))
    cat(sprintf("  Receipt:    %s\n", receipt_path))
    cat(sprintf("  SHA-256:    %s\n", sha))
    cat("=======================================================================\n\n")
  }

  invisible(final_matches)
}

# ==============================================================================
# RUN WRAPPER (callable by orchestrator via callr::r())
# ==============================================================================

#' Orchestrator-callable wrapper around enthealth_npi_pipeline()
#'
#' Accepts input_paths / output_paths lists (same contract as ABOG pipeline),
#' or reads from PIPELINE_INPUT_PATHS / PIPELINE_OUTPUT_PATHS env vars when
#' called from 00_MASTER_PUBLICATION_PIPELINE.R via callr::r().
run_enthealth_pipeline <- function(input_paths  = NULL,
                                   output_paths = NULL,
                                   run_id       = NULL) {
  checkmate::assert_list(input_paths,  null.ok = TRUE, .var.name = "input_paths")
  checkmate::assert_list(output_paths, null.ok = TRUE, .var.name = "output_paths")
  checkmate::assert_string(run_id,     null.ok = TRUE, .var.name = "run_id")

  # Resolve from env vars when called via callr (same pattern as ABOG pipeline)
  for (env_var in c("PIPELINE_INPUT_PATHS", "PIPELINE_OUTPUT_PATHS", "RUN_ID")) {
    val <- Sys.getenv(env_var, unset = "")
    if (!nzchar(val)) next
    if (env_var == "PIPELINE_INPUT_PATHS"  && is.null(input_paths))  input_paths  <- tryCatch(jsonlite::fromJSON(val), error = function(e) NULL)
    if (env_var == "PIPELINE_OUTPUT_PATHS" && is.null(output_paths)) output_paths <- tryCatch(jsonlite::fromJSON(val), error = function(e) NULL)
    if (env_var == "RUN_ID"                && is.null(run_id))       run_id       <- val
  }

  # Resolve file paths from input_paths list
  enthealth_file <- input_paths$enthealth_xlsx  %||% NULL
  nppes_parquet  <- input_paths$ent_nppes_parquet %||% NULL
  output_dir     <- output_paths$output_dir  %||% OUTPUT_DIR
  output_file    <- output_paths$matched_data %||% NULL

  enthealth_npi_pipeline(
    enthealth_file = enthealth_file,
    nppes_parquet  = nppes_parquet,
    output_dir     = output_dir,
    output_file    = output_file,
    verbose        = TRUE,
    run_id         = run_id
  )
}

# NULL-coalescing helper (base R)
#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# MAIN (script entry point — called by Rscript)
# ==============================================================================

#' Script entry point
#'
#' Thin wrapper around run_enthealth_pipeline(). Auto-runs when script is
#' executed directly. Do NOT call from orchestrator — use
#' run_enthealth_pipeline() instead.
main <- function(input_paths = NULL, output_paths = NULL, run_id = NULL) {
  checkmate::assert_list(input_paths,  null.ok = TRUE)
  checkmate::assert_list(output_paths, null.ok = TRUE)
  checkmate::assert_string(run_id,     null.ok = TRUE)

  # Notify start (reuse pipeline notify if available)
  notify_fn <- tryCatch({
    source(here("R", "utils", "notify.R"), local = TRUE)
    if (exists("pipeline_notify", mode = "function")) pipeline_notify else NULL
  }, error = function(e) NULL)

  if (!is.null(notify_fn)) {
    notify_fn("ENTHealth NPI matching started",
              sprintf("Mode: %s", if (is_test_mode()) "TEST" else "PRODUCTION"),
              priority = "info")
  }

  result <- tryCatch(
    run_enthealth_pipeline(
      input_paths  = input_paths,
      output_paths = output_paths,
      run_id       = run_id
    ),
    error = function(e) {
      if (!is.null(notify_fn)) {
        notify_fn("ENTHealth NPI matching FAILED", e$message, priority = "critical")
      }
      cat(sprintf("\nFATAL ERROR: %s\n", e$message))
      cat(sprintf("Traceback:\n%s\n",
                  paste(capture.output(traceback()), collapse = "\n")))
      quit(status = 1)
    }
  )

  if (!is.null(notify_fn) && !is.null(result)) {
    n <- nrow(result)
    notify_fn(
      sprintf("ENTHealth NPI matching DONE: %s matched", format(n, big.mark = ",")),
      sprintf("Output: data/ent/enthealth_npi_LATEST.csv"),
      priority = "critical"
    )
  }

  invisible(result)
}

# ==============================================================================
# AUTO-EXECUTE WHEN CALLED VIA Rscript (not sourced or LOAD_ONLY)
# ==============================================================================
if (sys.nframe() == 0L && !is_load_only()) {
  main()
}
