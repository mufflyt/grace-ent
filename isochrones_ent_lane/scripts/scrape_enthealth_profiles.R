#!/usr/bin/env Rscript
#' @title Scrape enthealth.org physician profile pages
#'
#' @description
#' For each physician in data/ent/enthealth_npi_LATEST.csv, fetches their
#' profile page on enthealth.org and extracts:
#'   - Residency institution + year
#'   - Medical school institution + year
#'   - Phone number (from profile page, may differ from directory listing)
#'   - Board certification(s)
#'
#' Outputs are merged back into the LATEST.csv and saved as:
#'   data/ent/enthealth_profiles_LATEST.csv   (profile fields only +
#' enthealth_id)
#'   data/ent/enthealth_npi_enriched.csv      (full merged file)
#'
#' @section Rate limiting:
#' Waits RATE_LIMIT_SECS seconds between requests (default 1.5).
#' A local JSON cache at data/ent/cache/profile_cache.json avoids re-fetching.
#' Run with FORCE_RESCRAPE=1 to ignore the cache.
#'
#' @section Invocation:
#' ```
#' Rscript scripts/scrape_enthealth_profiles.R
#' # Test mode — 10 records only:
#' TEST_MODE=1 Rscript scripts/scrape_enthealth_profiles.R
#' # Force re-scrape (ignore cache):
#' FORCE_RESCRAPE=1 Rscript scripts/scrape_enthealth_profiles.R
#' ```

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(rvest)
  library(httr)
  library(jsonlite)
  library(readxl)
})

RATE_LIMIT_SECS <- as.numeric(Sys.getenv("RATE_LIMIT_SECS", "1.5"))
FORCE_RESCRAPE  <- identical(Sys.getenv("FORCE_RESCRAPE", "0"), "1")
TEST_MODE       <- identical(Sys.getenv("TEST_MODE", "0"), "1")
CACHE_PATH      <- here::here("data", "ent", "cache", "profile_cache.json")
LATEST_CSV      <- here::here("data", "ent", "enthealth_npi_LATEST.csv")
PROFILES_CSV    <- here::here("data", "ent", "enthealth_profiles_LATEST.csv")
ENRICHED_CSV    <- here::here("data", "ent", "enthealth_npi_enriched.csv")

XLSX_PATHS <- c(
  here::here("data", "ent", "results.xlsx"),
  file.path(path.expand("~/Dropbox (Personal)"),
            "mystery_shopper", "Corbi study", "ENT", "Data", "results.xlsx"),
  file.path(path.expand("~/Dropbox"),
            "mystery_shopper", "Corbi study", "ENT", "Data", "results.xlsx"),
  file.path(path.expand("~/Documents/Documents - TMuff/enthealth"), "results.xlsx")
)

UA <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
             "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36")

# ── Helpers ──────────────────────────────────────────────────────────────────

make_enthealth_id <- function(url_col) {
  vapply(seq_along(url_col), function(i) {
    if (!is.na(url_col[i]) && nzchar(url_col[i])) {
      paste0("EH_", substr(digest::digest(url_col[i], algo = "sha1"), 1, 12))
    } else {
      paste0("EH_row", sprintf("%05d", i))
    }
  }, character(1))
}

profile_base_url <- function(url) {
  # Strip query params — profile is at the path only
  gsub("\\?.*$", "", url)
}

parse_edu_item <- function(text, prefix) {
  # "Residency: ICAHN School of Medicine - 1996" → list(name, year)
  text <- trimws(sub(paste0("^", prefix, ":?\\s*"), "", text))
  m <- regmatches(text, regexpr("^(.*?)\\s*[-–]\\s*(\\d{4})$", text, perl = TRUE))
  if (length(m) == 0 || !nzchar(m)) {
    return(list(name = text, year = NA_integer_))
  }
  parts <- strsplit(m, "\\s*[-–]\\s*(?=\\d{4}$)", perl = TRUE)[[1]]
  list(
    name = trimws(parts[1]),
    year = as.integer(trimws(parts[2]))
  )
}

scrape_one_profile <- function(base_url) {
  tryCatch({
    Sys.sleep(RATE_LIMIT_SECS)
    resp <- httr::GET(base_url,
                      httr::timeout(25),
                      httr::user_agent(UA),
                      httr::add_headers(Accept = "text/html,application/xhtml+xml"))
    if (httr::status_code(resp) != 200) {
      return(list(status = httr::status_code(resp),
                  residency_name = NA_character_, residency_year = NA_integer_,
                  medical_school_name = NA_character_, medical_school_year = NA_integer_,
                  fellowship_name = NA_character_, fellowship_year = NA_integer_,
                  board_cert = NA_character_))
    }
    page <- rvest::read_html(httr::content(resp, "text", encoding = "UTF-8"))

    # All <li> text items
    li_text <- page |> rvest::html_elements("li") |> rvest::html_text(trim = TRUE)

    residency_raw   <- li_text[startsWith(li_text, "Residency:")][1]
    med_school_raw  <- li_text[startsWith(li_text, "Medical School:")][1]
    fellowship_raw  <- li_text[startsWith(li_text, "Fellowship:")][1]
    board_cert_raw  <- li_text[startsWith(li_text, "American Board")]

    res <- parse_edu_item(residency_raw %||% "", "Residency")
    med <- parse_edu_item(med_school_raw %||% "", "Medical School")
    fel <- parse_edu_item(fellowship_raw %||% "", "Fellowship")

    list(
      status              = 200L,
      residency_name      = res$name,
      residency_year      = res$year,
      medical_school_name = med$name,
      medical_school_year = med$year,
      fellowship_name     = fel$name,
      fellowship_year     = fel$year,
      board_cert          = if (length(board_cert_raw) > 0)
                              paste(board_cert_raw, collapse = " | ") else NA_character_
    )
  }, error = function(e) {
    list(status = -1L,
         residency_name = NA_character_, residency_year = NA_integer_,
         medical_school_name = NA_character_, medical_school_year = NA_integer_,
         fellowship_name = NA_character_, fellowship_year = NA_integer_,
         board_cert = NA_character_,
         error_msg = conditionMessage(e))
  })
}

#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a) && nzchar(a)) a else b

# ── Load URL index from original Excel ──────────────────────────────────────

xlsx_path <- Filter(file.exists, XLSX_PATHS)[1]
if (is.na(xlsx_path)) stop("[scrape] Cannot find results.xlsx. Set ENT_HEALTH_XLSX or place at data/ent/results.xlsx")
cat(sprintf("[scrape] Reading URL index from: %s\n", xlsx_path))

url_index <- readxl::read_excel(xlsx_path, col_types = "text") |>
  dplyr::transmute(
    enthealth_id = make_enthealth_id(url),
    profile_url  = profile_base_url(url)
  ) |>
  dplyr::distinct(enthealth_id, .keep_all = TRUE)

cat(sprintf("[scrape] %d unique profile URLs indexed\n", nrow(url_index)))

# ── Load LATEST.csv (matched NPIs) ──────────────────────────────────────────

if (!file.exists(LATEST_CSV)) stop("[scrape] Cannot find enthealth_npi_LATEST.csv")
latest <- readr::read_csv(LATEST_CSV, show_col_types = FALSE)
cat(sprintf("[scrape] %d matched physicians in LATEST.csv\n", nrow(latest)))

# Join URL to matched records
to_scrape <- latest |>
  dplyr::inner_join(url_index, by = "enthealth_id")
cat(sprintf("[scrape] %d records have a profile URL\n", nrow(to_scrape)))

if (TEST_MODE) {
  to_scrape <- head(to_scrape, 10)
  cat("[scrape] TEST_MODE: limiting to first 10 records\n")
}

# ── Load or initialise cache ──────────────────────────────────────────────────

dir.create(dirname(CACHE_PATH), showWarnings = FALSE, recursive = TRUE)
cache <- if (!FORCE_RESCRAPE && file.exists(CACHE_PATH)) {
  tryCatch(jsonlite::read_json(CACHE_PATH, simplifyVector = FALSE),
           error = function(e) list())
} else {
  list()
}
cat(sprintf("[scrape] Cache: %d entries loaded\n", length(cache)))

# ── Main scraping loop ────────────────────────────────────────────────────────

results <- vector("list", nrow(to_scrape))
n_new   <- 0L
n_cached <- 0L

for (i in seq_len(nrow(to_scrape))) {
  eid  <- to_scrape$enthealth_id[i]
  purl <- to_scrape$profile_url[i]

  if (!FORCE_RESCRAPE && eid %in% names(cache)) {
    results[[i]] <- cache[[eid]]
    n_cached <- n_cached + 1L
    if (i %% 50 == 0) cat(sprintf("[scrape] %d / %d (cached)\r", i, nrow(to_scrape)))
    next
  }

  cat(sprintf("[scrape] %d / %d fetching: %s\n", i, nrow(to_scrape), purl))
  hit <- scrape_one_profile(purl)
  hit$enthealth_id  <- eid
  hit$profile_url   <- purl
  hit$scraped_at    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  results[[i]]      <- hit
  cache[[eid]]      <- hit
  n_new <- n_new + 1L

  # Flush cache every 25 new fetches
  if (n_new %% 25 == 0) {
    tmp <- paste0(CACHE_PATH, ".tmp")
    jsonlite::write_json(cache, tmp, pretty = FALSE, auto_unbox = TRUE)
    file.rename(tmp, CACHE_PATH)
    cat(sprintf("[scrape] Cache saved (%d entries)\n", length(cache)))
  }
}

# Final cache flush
tmp <- paste0(CACHE_PATH, ".tmp")
jsonlite::write_json(cache, tmp, pretty = FALSE, auto_unbox = TRUE)
file.rename(tmp, CACHE_PATH)
cat(sprintf("[scrape] Done. %d fetched, %d from cache\n", n_new, n_cached))

# ── Build output data frame ───────────────────────────────────────────────────

profile_df <- dplyr::bind_rows(lapply(results, function(r) {
  dplyr::tibble(
    enthealth_id        = r$enthealth_id %||% NA_character_,
    profile_url         = r$profile_url  %||% NA_character_,
    residency_name      = r$residency_name      %||% NA_character_,
    residency_year      = as.integer(r$residency_year %||% NA_integer_),
    medical_school_name = r$medical_school_name %||% NA_character_,
    medical_school_year = as.integer(r$medical_school_year %||% NA_integer_),
    fellowship_name     = r$fellowship_name     %||% NA_character_,
    fellowship_year     = as.integer(r$fellowship_year %||% NA_integer_),
    board_cert          = r$board_cert    %||% NA_character_,
    http_status         = as.integer(r$status %||% NA_integer_),
    scraped_at          = r$scraped_at    %||% NA_character_
  )
}))

# ── Coverage report ───────────────────────────────────────────────────────────

n <- nrow(profile_df)
cat(sprintf("\n[scrape] Coverage (N=%d):\n", n))
has_val <- function(x) !is.na(x) & nzchar(x, keepNA = FALSE)
cat(sprintf("  residency_name:      %d / %d (%.0f%%)\n",
    sum(has_val(profile_df$residency_name)), n,
    100*mean(has_val(profile_df$residency_name))))
cat(sprintf("  medical_school_name: %d / %d (%.0f%%)\n",
    sum(has_val(profile_df$medical_school_name)), n,
    100*mean(has_val(profile_df$medical_school_name))))
cat(sprintf("  fellowship_name:     %d / %d (%.0f%%)\n",
    sum(has_val(profile_df$fellowship_name)), n,
    100*mean(has_val(profile_df$fellowship_name))))
cat(sprintf("  board_cert:          %d / %d (%.0f%%)\n",
    sum(has_val(profile_df$board_cert)), n,
    100*mean(has_val(profile_df$board_cert))))

# ── Write profiles CSV ────────────────────────────────────────────────────────

readr::write_csv(profile_df, PROFILES_CSV)
cat(sprintf("[scrape] Profiles saved: %s (%d rows)\n", PROFILES_CSV, nrow(profile_df)))

# ── Merge back to LATEST and write enriched CSV ───────────────────────────────

enriched <- latest |>
  dplyr::left_join(
    dplyr::select(profile_df, enthealth_id, residency_name, residency_year,
                  medical_school_name, medical_school_year,
                  fellowship_name, fellowship_year, board_cert),
    by = "enthealth_id"
  )

readr::write_csv(enriched, ENRICHED_CSV)
cat(sprintf("[scrape] Enriched CSV saved: %s (%d rows, %d cols)\n",
            ENRICHED_CSV, nrow(enriched), ncol(enriched)))
cat("[scrape] New columns: residency_name, residency_year, medical_school_name, medical_school_year, fellowship_name, fellowship_year, board_cert\n")
