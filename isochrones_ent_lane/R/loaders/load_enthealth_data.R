#' @name load_enthealth_data
#' @title Load and Parse ENT Health Physician Scrape
#'
#' @description
#' \strong{DATA LOADER} - Self-contained loader for the enthealth.org Excel scrape. 
#' Produces a standardized data frame conforming to the roster contract expected
#' by the matching pipeline. Implements stable ID generation and name parsing 
#' with multiple fallbacks.
...
#' @section Pipeline Position:
#' \preformatted{
#'  Step 0: External data ingestion.
#'  Primary input for canonical_specialty_npi_pipeline.R when processing ENT.
#' }
...
#' @section Roster contract (output columns):
...
#' @author Tyler Muffly & Claude
#' @keywords data-ingestion specialty physician
#' @family data-loaders
NULL

#' Internal helper: Normalize name string
#' @keywords internal
#' UNICODE (2026-08-10): was toupper(trimws(gsub())), which preserves accents
#' rather than transliterating them, so an accented name never matched its
#' unaccented NPPES spelling. normalize_name_key() is the canonical normalizer
#' plus internal-whitespace collapsing (normalize_string() alone leaves
#' internal whitespace, which would break "John  Smith").
.enth_norm_name <- function(x) normalize_name_key(x)

#' Internal helper: Extract first initial
#' @keywords internal
.enth_first_init <- function(x) {
  s <- .enth_norm_name(x)
  dplyr::if_else(nchar(s) > 0L, substr(s, 1, 1), NA_character_)
}

#' Internal helper: Normalize phone number
#' @keywords internal
.enth_normalize_phone <- function(x) {
  digits <- gsub("[^0-9]", "", as.character(x))
  dplyr::if_else(nchar(digits) == 10L, digits, NA_character_)
}

#' Internal helper: Strip US country code from phone
#' @keywords internal
.enth_strip_country_code <- function(x) {
  dplyr::if_else(
    !is.na(x) & nchar(x) == 11L & substr(x, 1, 1) == "1",
    substr(x, 2, 11),
    x
  )
}

#' Internal helper: Generate stable SHA-1 IDs
#' @keywords internal
.enth_make_id <- function(url_col, row_idx) {
  vapply(seq_along(url_col), function(i) {
    if (!is.na(url_col[i]) && nzchar(url_col[i])) {
      paste0("EH_", substr(digest::digest(url_col[i], algo = "sha1"), 1, 12))
    } else {
      sprintf("EH_ROW%06d", row_idx[i])
    }
  }, character(1))
}

# ── Defaults ───────────────────────────────────────────────────────────────────

.ENTHEALTH_XLSX_DEFAULT <- file.path(
  path.expand("~"), "Dropbox (Personal)",
  "mystery_shopper", "Corbi study", "ENT", "Data", "results.xlsx"
)

# ── Main loader ────────────────────────────────────────────────────────────────

#' @title Load and Parse ENT Health Data
#'
#' @description
#' Main orchestrator for loading the enthealth.org Excel export. Performs 
#' column validation, ID generation, and name parsing.
#'
#' @param xlsx_path `character(1)`: Path to results.xlsx. Defaults to ENV var
#'   \code{ENT_HEALTH_XLSX}.
#' @param test_states `character`: State abbreviations to restrict in TEST_MODE.
#' @param is_test `logical(1)`: If TRUE, applies state filtering.
...
#' @return [data.frame]: Standardized physician roster.
...
#' @section Contract:
...
#' @section Disaster Prevention:
#' \describe{
#'   \item{Stable ID generation}{Uses SHA-1 hash of the source URL to create 
...
#' }
...
#' @keywords data-ingestion
#' @family data-loaders
#' @export
load_enthealth_data <- function(xlsx_path  = NULL,
                                test_states = NULL,
                                is_test     = identical(Sys.getenv("TEST_MODE"), "1")) {
  if (!requireNamespace("dplyr",   quietly = TRUE)) stop("Package 'dplyr' is required",   call. = FALSE)
  if (!requireNamespace("readxl",  quietly = TRUE)) stop("Package 'readxl' is required",  call. = FALSE)
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required", call. = FALSE)

  if (is.null(xlsx_path)) {
    xlsx_path <- Sys.getenv("ENT_HEALTH_XLSX", unset = "")
    if (!nzchar(xlsx_path)) xlsx_path <- .ENTHEALTH_XLSX_DEFAULT
  }

  if (!file.exists(xlsx_path)) {
    stop(sprintf(
      "[load_enthealth] File not found: %s\n  Set ENV var ENT_HEALTH_XLSX to override.",
      xlsx_path
    ), call. = FALSE)
  }

  cat(sprintf("[load_enthealth] Reading: %s\n", xlsx_path))
  df <- readxl::read_excel(xlsx_path, col_types = "text")
  n_raw <- nrow(df)
  cat(sprintf("[load_enthealth]   %s rows loaded\n", format(n_raw, big.mark = ",")))

  # Validate expected columns
  required_cols <- c("url", "full_name", "address", "city", "state_code",
                     "post_code", "phone_number")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "[load_enthealth] Missing columns: %s\n  Available: %s",
      paste(missing_cols, collapse = ", "),
      paste(names(df), collapse = ", ")
    ), call. = FALSE)
  }

  # Stable IDs + address normalization
  df <- df %>%
    dplyr::mutate(
      roster_id    = .enth_make_id(url, seq_len(n())),
      enthealth_id = roster_id,               # alias for backward-compatibility
      zip5         = substr(gsub("[^0-9]", "", post_code), 1, 5),
      phone_norm   = .enth_strip_country_code(.enth_normalize_phone(phone_number)),
      state        = toupper(trimws(state_code)),
      city_norm    = .enth_norm_name(city),
      company_name = if ("company_name" %in% names(df)) company_name else NA_character_,
      specialty_primary = if ("specialty_primary" %in% names(df)) specialty_primary else NA_character_
    )

  # ── Name parsing ──────────────────────────────────────────────────────────
  df$first_raw <- NA_character_
  df$last_raw  <- NA_character_
  df$middle_raw <- NA_character_

  if (exists("parse_physician_name_enhanced", mode = "function")) {
    cat("[load_enthealth]   Applying parse_physician_name_enhanced()...\n")
    parsed <- tryCatch(
      parse_physician_name_enhanced(df$full_name),
      error = function(e) {
        warning(sprintf("[load_enthealth] Name parser failed: %s — fallback to split", e$message))
        NULL
      }
    )
    if (!is.null(parsed) && is.data.frame(parsed)) {
      first_col  <- intersect(c("first_name", "first",  "fname"), names(parsed))[1]
      last_col   <- intersect(c("last_name",  "last",   "lname"), names(parsed))[1]
      middle_col <- intersect(c("middle_name","middle", "mname"), names(parsed))[1]
      if (!is.na(first_col))  df$first_raw  <- parsed[[first_col]]
      if (!is.na(last_col))   df$last_raw   <- parsed[[last_col]]
      if (!is.na(middle_col)) df$middle_raw <- parsed[[middle_col]]
    }
  }

  # Fallback: split on last space for unparsed records
  needs <- is.na(df$first_raw) | is.na(df$last_raw)
  if (any(needs)) {
    cat(sprintf("[load_enthealth]   Fallback name split for %d records\n", sum(needs)))
    parts <- stringr::str_split_fixed(trimws(df$full_name[needs]), "\\s+", n = 2)
    df$first_raw[needs] <- parts[, 1]
    df$last_raw[needs]  <- parts[, 2]
  }

  # ── Roster contract columns ───────────────────────────────────────────────
  df <- df %>%
    dplyr::mutate(
      first_clean   = .enth_norm_name(first_raw),
      last_clean    = .enth_norm_name(last_raw),
      middle_clean  = .enth_norm_name(middle_raw),
      first_initial = .enth_first_init(first_clean),
      abog_gender   = NA_character_,
      abog_gender_conf = 0,
      cert_year     = NA_integer_,
      is_common_name = FALSE
    )

  # Warn on empty last names
  n_empty_last <- sum(is.na(df$last_clean) | (!is.na(df$last_clean) & df$last_clean == ""))
  if (n_empty_last > 0) {
    warning(sprintf("[load_enthealth] %d records have empty last_clean — these will not match",
                    n_empty_last))
  }

  # ── TEST_MODE state filter ────────────────────────────────────────────────
  if (isTRUE(is_test) && length(test_states) > 0) {
    before <- nrow(df)
    df <- df %>% dplyr::filter(state %in% test_states)
    cat(sprintf("[load_enthealth][TEST_MODE] %s → %s rows (%s)\n",
                format(before, big.mark = ","),
                format(nrow(df), big.mark = ","),
                paste(test_states, collapse = "/")))
    if (nrow(df) < 5) {
      warning(sprintf("[load_enthealth] Only %d rows after test_states filter", nrow(df)))
    }
  }

  cat(sprintf("[load_enthealth]   %s records ready\n\n", format(nrow(df), big.mark = ",")))
  df
}
