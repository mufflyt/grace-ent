#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(httr2)
  library(rvest)
  library(xml2)
  library(here)
})

dropbox_root <- file.path(path.expand("~"), "Dropbox (Personal)")
if (!dir.exists(dropbox_root)) {
  dropbox_root <- file.path(path.expand("~"), "Dropbox")
}

source_csv <- file.path(
  dropbox_root, "mystery_shopper", "Corbi study", "ENT", "Data", "results_geocoded.csv"
)
board_csv <- here::here("data", "ent", "board_results_abohns_verification_results.csv")
progress_csv <- here::here("data", "ent", "abohns_backfill_progress.csv")

limit_n <- suppressWarnings(as.integer(Sys.getenv("ABOHNS_BACKFILL_LIMIT", "")))
delay_s <- suppressWarnings(as.numeric(Sys.getenv("ABOHNS_BACKFILL_DELAY", "0.5")))
if (is.na(delay_s) || delay_s < 0) delay_s <- 0.5

normalize_key <- function(first, last, state) {
  paste(toupper(trimws(first)), toupper(trimws(last)), toupper(trimws(state)), sep = "|")
}

extract_text <- function(node, xpath) {
  found <- xml2::xml_find_first(node, xpath)
  if (inherits(found, "xml_missing")) return("")
  rvest::html_text2(found)
}

scrape_abohns_query <- function(first_name, last_name, state) {
  req <- httr2::request("https://abohns.org/verify-certification") |>
    httr2::req_url_query(
      first_name = first_name,
      last_name = last_name,
      city = "",
      state = state,
      id = "",
      op = "Verify Status",
      form_id = "abohns_verification_form"
    ) |>
    httr2::req_user_agent(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36"
    ) |>
    httr2::req_timeout(20)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_string(resp)
  page <- read_html(body)

  table <- xml2::xml_find_first(page, "//table[contains(@class, 'table--cert')]")
  if (inherits(table, "xml_missing")) {
    return(list(rows = tibble(), source_url = httr2::resp_url(resp)))
  }

  result_rows <- xml2::xml_find_all(table, ".//tbody/tr[td]")
  out <- vector("list", length(result_rows))
  keep_idx <- 0L

  for (row in result_rows) {
    strong_node <- xml2::xml_find_first(row, ".//span[contains(@class,'physician__name')]/strong")
    if (inherits(strong_node, "xml_missing")) next

    name_text <- rvest::html_text2(strong_node)
    name_container <- xml2::xml_find_first(row, ".//span[contains(@class,'physician__name')]")
    container_text <- if (!inherits(name_container, "xml_missing")) rvest::html_text2(name_container) else ""
    abohns_id <- sub(".*(#\\d+).*", "\\1", container_text)
    if (identical(abohns_id, container_text)) abohns_id <- ""

    location <- extract_text(row, ".//span[contains(@class,'physician__location')]")
    cc_req <- extract_text(row, ".//td[@data-title='Meets Reqs*']")

    cert_rows <- xml2::xml_find_all(row, ".//table[contains(@class,'table--stack')]//tbody/tr")
    if (length(cert_rows) == 0) next

    for (cert_row in cert_rows) {
      cols <- xml2::xml_find_all(cert_row, "./td")
      if (length(cols) < 3) next
      keep_idx <- keep_idx + 1L
      out[[keep_idx]] <- tibble(
        query_first_name = first_name,
        query_last_name = last_name,
        query_state = state,
        abohns_id = abohns_id,
        full_name = name_text,
        location = location,
        certification = rvest::html_text2(cols[[1]]),
        initial_certification = rvest::html_text2(cols[[2]]),
        expiration = rvest::html_text2(cols[[3]]),
        meeting_cc_reqs = cc_req,
        source_url = as.character(httr2::resp_url(resp)),
        scrape_date = Sys.time()
      )
    }
  }

  list(
    rows = dplyr::bind_rows(out[seq_len(keep_idx)]),
    source_url = as.character(httr2::resp_url(resp))
  )
}

src <- readr::read_csv(source_csv, show_col_types = FALSE) |>
  transmute(
    first = trimws(first_name),
    last = trimws(last_name),
    state = trimws(state_code),
    full_name = trimws(full_name)
  ) |>
  filter(!is.na(first), first != "", !is.na(last), last != "", !is.na(state), state != "") |>
  distinct(first, last, state, .keep_all = TRUE)

board <- readr::read_csv(board_csv, show_col_types = FALSE)
existing <- board |>
  distinct(first = trimws(query_first_name), last = trimws(query_last_name), state = trimws(query_state))

missing <- anti_join(src, existing, by = c("first", "last", "state")) |>
  mutate(query_key = normalize_key(first, last, state))

if (file.exists(progress_csv)) {
  progress <- readr::read_csv(progress_csv, show_col_types = FALSE)
  attempted <- unique(progress$query_key)
  missing <- missing |> filter(!query_key %in% attempted)
} else {
  progress <- tibble(
    query_key = character(),
    first = character(),
    last = character(),
    state = character(),
    found = logical(),
    n_records = integer(),
    source_url = character(),
    scrape_date = as.POSIXct(character())
  )
}

if (!is.na(limit_n) && limit_n > 0) {
  warning(sprintf("[ABOHNS backfill] ABOHNS_BACKFILL_LIMIT=%d is set — processing only %d rows. Unset for full run.", limit_n, min(limit_n, nrow(missing))))
  missing <- missing[seq_len(min(limit_n, nrow(missing))), ]
}

cat("ABOHNS backfill\n")
cat("  Source missing queries:", nrow(missing), "\n")
cat("  Delay seconds        :", delay_s, "\n")

if (nrow(missing) == 0) {
  quit(save = "no", status = 0)
}

new_rows <- list()
progress_rows <- list()

for (i in seq_len(nrow(missing))) {
  row <- missing[i, ]
  cat(sprintf("[%d/%d] %s %s (%s)\n", i, nrow(missing), row$first, row$last, row$state))

  result <- tryCatch(
    scrape_abohns_query(row$first, row$last, row$state),
    error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      list(rows = tibble(), source_url = NA_character_)
    }
  )

  n_found <- nrow(result$rows)
  if (n_found > 0) {
    cat("  Found records:", n_found, "\n")
    new_rows[[length(new_rows) + 1L]] <- result$rows
  } else {
    cat("  No results\n")
  }

  progress_rows[[length(progress_rows) + 1L]] <- tibble(
    query_key = row$query_key,
    first = row$first,
    last = row$last,
    state = row$state,
    found = n_found > 0,
    n_records = n_found,
    source_url = if (is.null(result$source_url) || length(result$source_url) == 0) NA_character_ else result$source_url,
    scrape_date = Sys.time()
  )

  progress <- bind_rows(progress, bind_rows(progress_rows[length(progress_rows)]))
  tmp_progress <- paste0(progress_csv, ".tmp")
  readr::write_csv(progress, tmp_progress, na = "")
  file.rename(tmp_progress, progress_csv)

  if (delay_s > 0 && i < nrow(missing)) Sys.sleep(delay_s)
}

appended <- bind_rows(new_rows)
if (nrow(appended) > 0) {
  combined <- bind_rows(board, appended) |>
    distinct(query_first_name, query_last_name, query_state, certification, .keep_all = TRUE)
  tmp_board <- paste0(board_csv, ".tmp")
  readr::write_csv(combined, tmp_board, na = "")
  file.rename(tmp_board, board_csv)
  cat("Appended certification rows:", nrow(appended), "\n")
  cat("Board CSV rows now         :", nrow(combined), "\n")
} else {
  cat("No new certification rows appended.\n")
}
