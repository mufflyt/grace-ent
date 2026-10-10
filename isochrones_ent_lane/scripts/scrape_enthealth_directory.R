#!/usr/bin/env Rscript
# =============================================================================
# SCRAPE ENTHEALTH.ORG PHYSICIAN DIRECTORY
# =============================================================================
# Updated from Bartosz / Upwork script (2023-04-04).
# Scrapes the full AAO-HNS physician directory at enthealth.org/find-ent/.
# Uses the WordPress AJAX endpoint (admin-ajax.php) to paginate results,
# then fetches each individual profile page.
#
# Usage:
#   Rscript scripts/scrape_enthealth_directory.R
#   TEST_MODE=1 Rscript scripts/scrape_enthealth_directory.R   # first 5 pages only
#   FORCE_RESCRAPE=1 Rscript scripts/scrape_enthealth_directory.R  # ignore cache
#
# Output (written to Dropbox):
#   ~/Dropbox (Personal)/mystery_shopper/Corbi study/ENT/Data/results.xlsx
#   ~/Dropbox (Personal)/mystery_shopper/Corbi study/ENT/Data/results.csv   (bonus)
#
# Expected runtime: 3-6 hours (8,000-10,000 profiles at 1.5 s/profile)
#
# Author: Bartosz (Upwork) + Tyler Muffly (project integration)
# =============================================================================

suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(jsonlite)
  library(stringi)
  library(dplyr)
  library(openxlsx)
  library(pbapply)
  library(here)
})

TEST_MODE      <- identical(Sys.getenv("TEST_MODE",      "0"), "1")
FORCE_RESCRAPE <- identical(Sys.getenv("FORCE_RESCRAPE", "0"), "1")
RATE_LIMIT_S   <- as.numeric(Sys.getenv("RATE_LIMIT_SECS", "1.5"))
COUNTRY        <- "United States"

# ── Output paths ──────────────────────────────────────────────────────────────
DROPBOX_DATA <- file.path(
  path.expand("~"), "Dropbox (Personal)",
  "mystery_shopper", "Corbi study", "ENT", "Data"
)
if (!dir.exists(DROPBOX_DATA)) {
  DROPBOX_DATA <- file.path(
    path.expand("~"), "Dropbox",
    "mystery_shopper", "Corbi study", "ENT", "Data"
  )
}
if (!dir.exists(DROPBOX_DATA)) stop("Dropbox ENT/Data directory not found")

OUTPUT_XLSX <- file.path(DROPBOX_DATA, "results.xlsx")
OUTPUT_CSV  <- file.path(DROPBOX_DATA, "results.csv")

# Temp cache files (written alongside the output, survives partial runs)
CACHE_URLS     <- file.path(DROPBOX_DATA, "cache_urls.rds")
CACHE_PROFILES <- file.path(DROPBOX_DATA, "cache_profiles.rds")

cat("\n=======================================================================\n")
cat("  SCRAPE ENTHEALTH.ORG PHYSICIAN DIRECTORY\n")
cat(sprintf("  Started : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  Output  : %s\n", OUTPUT_XLSX))
cat(sprintf("  TestMode: %s   ForceRescrape: %s\n",
            TEST_MODE, FORCE_RESCRAPE))
cat("=======================================================================\n\n")

# ── Get nonce (session token required by the WordPress form) ──────────────────
cat("Fetching nonce from enthealth.org/find-ent/ ...\n")
nonce_page <- tryCatch(
  session("https://www.enthealth.org/find-ent/"),
  error = function(e) stop(sprintf("Cannot reach enthealth.org: %s", e$message))
)
ent_nonce <- nonce_page |>
  html_node('input[name="_ent_nonce"]') |>
  html_attr("value")
if (is.na(ent_nonce) || !nzchar(ent_nonce))
  stop("Could not extract _ent_nonce from enthealth.org/find-ent/")
cat(sprintf("  Nonce: %s\n\n", ent_nonce))

# ── HTTP config ───────────────────────────────────────────────────────────────
AJAX_URL <- "https://www.enthealth.org/wp-admin/admin-ajax.php"

HEADERS <- c(
  "authority"        = "www.enthealth.org",
  "accept"           = "application/json, text/javascript, */*; q=0.01",
  "accept-language"  = "en-US,en;q=0.9",
  "content-type"     = "application/x-www-form-urlencoded; charset=UTF-8",
  "origin"           = "https://www.enthealth.org",
  "referer"          = "https://www.enthealth.org/find-ent/",
  "user-agent"       = paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
  ),
  "x-requested-with" = "XMLHttpRequest"
)

make_body <- function(page_number) {
  paste0(
    "action=find_ent_response&_ent_nonce=", ent_nonce,
    "&paged=", page_number,
    "&radiuszip=&distance=&lname=&city=&state=&country=", COUNTRY,
    "&specialty=&ajaxrequest=true&submit=Submit+Form"
  )
}

# ── Helper: extract text, NA on failure ───────────────────────────────────────
html_text_na <- function(x, ...) {
  txt <- tryCatch(html_text(x, ...), error = function(e) character(0))
  if (length(txt) == 0) return(NA_character_)
  txt
}

# "Residency: ICAHN School of Medicine - 1996" → list(name, year)
parse_edu_item <- function(text, prefix) {
  if (is.na(text) || !nzchar(text))
    return(list(name = NA_character_, year = NA_integer_))
  text <- trimws(sub(paste0("^", prefix, ":?\\s*"), "", text))
  m <- regmatches(text, regexpr("^(.*?)\\s*[-–]\\s*(\\d{4})$", text, perl = TRUE))
  if (length(m) == 0 || !nzchar(m))
    return(list(name = text, year = NA_integer_))
  parts <- strsplit(m, "\\s*[-–]\\s*(?=\\d{4}$)", perl = TRUE)[[1]]
  list(name = trimws(parts[1]), year = as.integer(trimws(parts[2])))
}

# =============================================================================
# PHASE 1 — collect all profile URLs from the paginated search
# =============================================================================
scrape_search_page <- function(page_number) {
  Sys.sleep(RATE_LIMIT_S)
  r <- httr::POST(AJAX_URL, add_headers(.headers = HEADERS),
                  body = make_body(page_number), encode = "form",
                  httr::timeout(30))
  if (httr::status_code(r) != 200)
    return(list(urls = character(0), next_page = 0L, total_pages = 0L))

  s <- tryCatch(
    content(r, "text", encoding = "UTF-8") |> fromJSON(),
    error = function(e) NULL
  )
  if (is.null(s)) return(list(urls = character(0), next_page = 0L, total_pages = 0L))

  results_html  <- tryCatch(s$results_html |> read_html(), error = function(e) NULL)
  if (is.null(results_html))
    return(list(urls = character(0), next_page = 0L, total_pages = 0L))

  result_nodes <- results_html |> html_nodes(".result")

  urls <- character(0)
  for (node in result_nodes) {
    addr_text <- node |> html_node(".address") |> html_text_na(trim = TRUE)
    is_us <- if (is.na(addr_text)) TRUE else stri_detect_regex(addr_text, COUNTRY)
    if (isTRUE(is_us)) {
      raw_url <- node |>
        html_nodes("a") |>
        html_attr("href") |>
        paste0("https://www.enthealth.org", x = _) |>
        stri_trans_general(id = "Latin-ASCII") |>
        stri_split_fixed("?") |>
        sapply(\(x) x[[1]])
      urls <- c(urls, raw_url)
    }
  }

  next_page <- tryCatch({
    pg <- s$results_pagination |> read_html() |>
      html_node(".next") |> html_attr("href")
    if (is.na(pg)) 0L else as.integer(stri_extract_last_regex(pg, "[0-9]+"))
  }, error = function(e) 0L)

  total_pages <- as.integer((s$results_rows %/% 10) + 1)

  list(urls = urls, next_page = next_page, total_pages = total_pages)
}

if (!FORCE_RESCRAPE && file.exists(CACHE_URLS)) {
  cat("Phase 1: Loading URL cache ...\n")
  all_urls <- readRDS(CACHE_URLS)
  cat(sprintf("  Loaded %s URLs from cache.\n\n", format(length(all_urls), big.mark = ",")))
} else {
  cat("Phase 1: Discovering total page count ...\n")
  first_page <- scrape_search_page(1)
  total_pages <- first_page$total_pages
  if (TEST_MODE) total_pages <- min(total_pages, 5L)
  cat(sprintf("  Total pages: %d\n", total_pages))

  cat("  Collecting profile URLs from all search pages ...\n")
  page_results <- pblapply(seq_len(total_pages), scrape_search_page)
  all_urls <- unique(unlist(lapply(page_results, `[[`, "urls")))
  cat(sprintf("  Collected %s unique URLs.\n\n", format(length(all_urls), big.mark = ",")))
  saveRDS(all_urls, paste0(CACHE_URLS, ".tmp"))
  file.rename(paste0(CACHE_URLS, ".tmp"), CACHE_URLS)
  cat(sprintf("  URL cache saved: %s\n\n", CACHE_URLS))
}

# =============================================================================
# PHASE 2 — scrape each individual profile page
# =============================================================================
scrape_one_profile <- function(url) {
  Sys.sleep(RATE_LIMIT_S)
  s <- tryCatch(session(url), error = function(e) NULL)
  if (is.null(s)) {
    return(list(url = url, full_name = NA, specialty_primary = NA,
                specialty_secondary = NA, company = NA,
                address_line_1 = NA, address_line_2 = NA, phone_number = NA))
  }
  addr_nodes <- s |> html_nodes(".address")
  li_text    <- s |> html_nodes("li") |> html_text(trim = TRUE)

  res <- parse_edu_item(li_text[startsWith(li_text, "Residency:")][1],    "Residency")
  med <- parse_edu_item(li_text[startsWith(li_text, "Medical School:")][1], "Medical School")
  fel <- parse_edu_item(li_text[startsWith(li_text, "Fellowship:")][1],   "Fellowship")
  board_raw <- li_text[startsWith(li_text, "American Board")]

  list(
    url                 = url,
    full_name           = s |> html_nodes(".name")              |> html_text_na(trim = TRUE),
    specialty_primary   = s |> html_nodes(".primary-specialty") |> html_text_na(trim = TRUE),
    specialty_secondary = s |> html_nodes(".secondary-specialty") |> html_text_na(trim = TRUE),
    company             = s |> html_nodes(".company")           |> html_text_na(trim = TRUE),
    address_line_1      = if (length(addr_nodes) >= 1) html_text(addr_nodes[[1]], trim = TRUE) else NA_character_,
    address_line_2      = if (length(addr_nodes) >= 2) html_text(addr_nodes[[2]], trim = TRUE) else NA_character_,
    phone_number        = s |> html_nodes(".phone")             |> html_text_na(trim = TRUE),
    residency_name      = res$name,
    residency_year      = res$year,
    medical_school_name = med$name,
    medical_school_year = med$year,
    fellowship_name     = fel$name,
    fellowship_year     = fel$year,
    board_cert          = if (length(board_raw) > 0) paste(board_raw, collapse = " | ") else NA_character_
  )
}

if (!FORCE_RESCRAPE && file.exists(CACHE_PROFILES)) {
  cat("Phase 2: Loading profile cache ...\n")
  profiles_raw <- readRDS(CACHE_PROFILES)
  cached_urls  <- sapply(profiles_raw, `[[`, "url")
  remaining    <- setdiff(all_urls, cached_urls)
  cat(sprintf("  %s profiles cached, %s remaining.\n\n",
              format(length(profiles_raw), big.mark = ","),
              format(length(remaining),   big.mark = ",")))
} else {
  profiles_raw <- list()
  remaining    <- all_urls
}

if (length(remaining) > 0) {
  cat(sprintf("Phase 2: Scraping %s profile pages (~%.1f hours at %.1f s/page) ...\n",
              format(length(remaining), big.mark = ","),
              length(remaining) * RATE_LIMIT_S / 3600,
              RATE_LIMIT_S))
  new_profiles <- pblapply(remaining, scrape_one_profile)
  profiles_raw <- c(profiles_raw, new_profiles)
  saveRDS(profiles_raw, paste0(CACHE_PROFILES, ".tmp"))
  file.rename(paste0(CACHE_PROFILES, ".tmp"), CACHE_PROFILES)
  cat(sprintf("\n  Profile cache saved: %s (%s total)\n\n",
              CACHE_PROFILES, format(length(profiles_raw), big.mark = ",")))
}

# =============================================================================
# PHASE 3 — clean and structure
# =============================================================================
cat("Phase 3: Cleaning and structuring ...\n")

df <- bind_rows(lapply(profiles_raw, function(r) {
  tibble(
    url                 = r$url                 %||% NA_character_,
    full_name           = r$full_name           %||% NA_character_,
    specialty_primary   = r$specialty_primary   %||% NA_character_,
    specialty_secondary = r$specialty_secondary %||% NA_character_,
    company             = r$company             %||% NA_character_,
    address_line_1      = r$address_line_1      %||% NA_character_,
    address_line_2      = r$address_line_2      %||% NA_character_,
    phone_number        = r$phone_number        %||% NA_character_,
    residency_name      = r$residency_name      %||% NA_character_,
    residency_year      = as.integer(r$residency_year      %||% NA_integer_),
    medical_school_name = r$medical_school_name %||% NA_character_,
    medical_school_year = as.integer(r$medical_school_year %||% NA_integer_),
    fellowship_name     = r$fellowship_name     %||% NA_character_,
    fellowship_year     = as.integer(r$fellowship_year     %||% NA_integer_),
    board_cert          = r$board_cert          %||% NA_character_
  )
}))

#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a[1] else b

# Shift rows where country ended up in address_line_1
us_in_line1 <- !is.na(df$address_line_1) &
               stri_detect_regex(df$address_line_1, COUNTRY) &
               is.na(df$address_line_2)
df$address_line_2[us_in_line1] <- df$address_line_1[us_in_line1]
df$address_line_1[us_in_line1] <- NA_character_

# Strip country from address_line_2, then parse city / state / zip
df <- df |>
  mutate(
    country      = COUNTRY,
    address_line_2 = stri_replace_all_regex(address_line_2, COUNTRY, "") |> trimws(),
    city         = stri_split_regex(address_line_2, " [A-Z]{2} ") |>
                     sapply(\(x) if (length(x) >= 1) trimws(x[1]) else NA_character_),
    address_line_2 = stri_replace_all_regex(address_line_2, city, "") |> trimws(),
    state_code   = stri_extract_first_regex(address_line_2, "[A-Z]{2}"),
    address_line_2 = stri_replace_all_regex(address_line_2, state_code, "") |> trimws(),
    post_code    = stri_extract_first_regex(address_line_2, "[0-9]+(-[0-9]+)?"),
  ) |>
  select(
    url, full_name,
    company_name = company, address = address_line_1,
    city, state_code, post_code, country,
    phone_number, specialty_primary, specialty_secondary,
    residency_name, residency_year,
    medical_school_name, medical_school_year,
    fellowship_name, fellowship_year,
    board_cert
  ) |>
  mutate(across(where(is.character), \(x) {
    x <- trimws(x)
    if_else(x == "", NA_character_, x)
  }))

cat(sprintf("  Final dataset: %s rows, %d columns\n\n",
            format(nrow(df), big.mark = ","), ncol(df)))

# =============================================================================
# PHASE 4 — save
# =============================================================================
cat("Phase 4: Saving results ...\n")

tmp_xlsx <- paste0(OUTPUT_XLSX, ".tmp")
openxlsx::write.xlsx(df, tmp_xlsx, overwrite = TRUE)
file.rename(tmp_xlsx, OUTPUT_XLSX)
cat(sprintf("  XLSX: %s (%s rows)\n", OUTPUT_XLSX, format(nrow(df), big.mark = ",")))

tmp_out_csv <- paste0(OUTPUT_CSV, ".tmp")
write.csv(df, tmp_out_csv, row.names = FALSE, na = "")
file.rename(tmp_out_csv, OUTPUT_CSV)
cat(sprintf("  CSV : %s\n", OUTPUT_CSV))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=======================================================================\n")
cat("  COMPLETE\n")
cat(sprintf("  Physicians  : %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("  With address: %s (%.0f%%)\n",
            format(sum(!is.na(df$address)), big.mark = ","),
            100 * mean(!is.na(df$address))))
cat(sprintf("  With phone  : %s (%.0f%%)\n",
            format(sum(!is.na(df$phone_number)), big.mark = ","),
            100 * mean(!is.na(df$phone_number))))
cat(sprintf("  With residency   : %s (%.0f%%)\n",
            format(sum(!is.na(df$residency_name)), big.mark = ","),
            100 * mean(!is.na(df$residency_name))))
cat(sprintf("  With med school  : %s (%.0f%%)\n",
            format(sum(!is.na(df$medical_school_name)), big.mark = ","),
            100 * mean(!is.na(df$medical_school_name))))
cat(sprintf("  With fellowship  : %s (%.0f%%)\n",
            format(sum(!is.na(df$fellowship_name)), big.mark = ","),
            100 * mean(!is.na(df$fellowship_name))))
cat(sprintf("  With board cert  : %s (%.0f%%)\n",
            format(sum(!is.na(df$board_cert)), big.mark = ","),
            100 * mean(!is.na(df$board_cert))))
cat(sprintf("  Output      : %s\n", OUTPUT_XLSX))
cat("=======================================================================\n\n")
