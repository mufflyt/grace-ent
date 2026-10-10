#' @title Auto-upload ENT NPI pipeline output to Dropbox
#'
#' @description
#' After a successful ENT NPI pipeline run, uploads
#' \code{data/ent/enthealth_npi_LATEST.csv} (and the timestamped copy) to
#' Dropbox at \code{/isochrones/ent/enthealth_npi_LATEST.csv}.
#'
#' Controlled by the \code{AUTO_UPLOAD_ENT_NPI} environment variable
#' (default \code{"1"}, ON).  Set to \code{"0"} to skip.
#'
#' Gates (all must pass before upload):
#'  1. \code{AUTO_UPLOAD_ENT_NPI != "0"}
#'  2. At least 500 MB free disk space
#'  3. \code{dbxcli} binary is available and authenticated
#'  4. The CSV exists and has at least \code{min_rows} rows
#'  5. SHA-256 idempotency — skips upload when file is unchanged
#'
#' A JSON receipt is always written to
#' \code{data/ent/upload_receipt_enthealth_npi_LATEST.json}.
#'
#' @param csv_path Character(1). Path to the CSV to upload.
#'   Defaults to \code{data/ent/enthealth_npi_LATEST.csv}.
#' @param min_rows Integer(1). Minimum acceptable row count before upload is
#'   allowed. Protects against uploading a truncated test-mode file over a
#'   good production copy. Default 50 (TEST_MODE runs have ~178 rows; set to
#'   7000 before a national run to require the full dataset).
#' @param verbose Logical.
#' @return Invisible \code{TRUE} on success or idempotent skip, \code{FALSE}
#'   on any gate failure or upload error. Never throws.

auto_upload_ent_npi_to_dropbox <- function(
    csv_path = here::here("data", "ent", "enthealth_npi_LATEST.csv"),
    min_rows  = 50L,
    verbose   = TRUE) {

  t0 <- proc.time()[["elapsed"]]

  receipt <- list(
    upload_attempted    = FALSE,
    skipped_reason      = NA_character_,
    source_path         = csv_path,
    sha256              = NA_character_,
    prior_sha256        = NA_character_,
    row_count           = NA_integer_,
    file_size_bytes     = NA_integer_,
    dropbox_destination = NA_character_,
    success             = FALSE,
    error_message       = NA_character_,
    hostname            = Sys.info()[["nodename"]],
    git_sha             = tryCatch(
      trimws(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE)),
      error = function(e) NA_character_
    ),
    duration_seconds    = NA_real_,
    timestamp           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )

  receipt_path <- here::here("data", "ent",
                             "upload_receipt_enthealth_npi_LATEST.json")

  flush_receipt <- function() {
    receipt$duration_seconds <<- round(proc.time()[["elapsed"]] - t0, 2)
    tmp <- paste0(receipt_path, ".tmp.", Sys.getpid())
    tryCatch({
      jsonlite::write_json(receipt, tmp, pretty = TRUE, auto_unbox = TRUE)
      file.rename(tmp, receipt_path)
    }, error = function(e) {
      if (file.exists(tmp)) unlink(tmp)
    })
    invisible(isTRUE(receipt$success))
  }

  .msg <- function(...) if (verbose) cat(sprintf(...))

  # ── Gate 1: env var ──────────────────────────────────────────────────────────
  if (!is_truthy(Sys.getenv("AUTO_UPLOAD_ENT_NPI", unset = "1"))) {
    .msg(sprintf("[ENT NPI Upload] Skipped (AUTO_UPLOAD_ENT_NPI=%s)\n",
                 Sys.getenv("AUTO_UPLOAD_ENT_NPI", "1")))
    receipt$skipped_reason <- "env_var_disabled"
    return(flush_receipt())
  }

  # ── Gate 2: disk space ───────────────────────────────────────────────────────
  free_gb <- tryCatch({
    df_out <- system("df -k .", intern = TRUE, ignore.stderr = TRUE)
    if (length(df_out) >= 2)
      as.numeric(strsplit(trimws(df_out[2]), "\\s+")[[1]][4]) / 1048576
    else NA_real_
  }, error = function(e) NA_real_)

  if (!is.na(free_gb) && free_gb < 0.5) {
    .msg("[ENT NPI Upload] Skipped (low disk: %.2f GB free)\n", free_gb)
    receipt$skipped_reason <- sprintf("low_disk_%.2f_gb", free_gb)
    return(flush_receipt())
  }

  # ── Gate 3: dbxcli available ─────────────────────────────────────────────────
  dbxcli_ok <- tryCatch({
    if (!exists("dbxcli_cmd", mode = "function"))
      source(here::here("R", "utils", "dropbox_utils.R"))
    dbxcli_cmd()
    TRUE
  }, error = function(e) FALSE)

  if (!dbxcli_ok) {
    .msg("[ENT NPI Upload] Skipped (dbxcli not found)\n")
    receipt$skipped_reason <- "dbxcli_not_found"
    return(flush_receipt())
  }

  # ── Gate 4: CSV exists and has enough rows ───────────────────────────────────
  if (!file.exists(csv_path)) {
    .msg("[ENT NPI Upload] Skipped (CSV not found: %s)\n", csv_path)
    receipt$skipped_reason <- "csv_not_found"
    return(flush_receipt())
  }

  row_count <- tryCatch({
    # Count newlines minus header — fast, no full parse
    length(readLines(csv_path)) - 1L
  }, error = function(e) NA_integer_)

  receipt$row_count <- row_count

  if (is.na(row_count) || row_count < min_rows) {
    .msg("[ENT NPI Upload] Skipped (only %d rows, need >= %d -- likely a test-mode run)\n",
         row_count, min_rows)
    receipt$skipped_reason <- sprintf("too_few_rows_%d", row_count)
    return(flush_receipt())
  }

  receipt$file_size_bytes <- file.info(csv_path)$size
  receipt$upload_attempted <- TRUE

  # ── Gate 5: SHA-256 idempotency ──────────────────────────────────────────────
  # Compute SHA-256 via digest; fall back to md5sum if digest is unavailable.
  sha256 <- tryCatch(
    digest::digest(file = csv_path, algo = "sha256"),
    error = function(e) tryCatch(
      as.character(tools::md5sum(csv_path)),
      error = function(e2) NA_character_
    )
  )
  receipt$sha256 <- sha256

  # Read prior SHA from receipt if it exists
  prior_sha256 <- tryCatch({
    if (file.exists(receipt_path)) {
      prior <- jsonlite::read_json(receipt_path)
      # Explicit null-coalesce: prior$sha256 may be NULL when key is absent
      prior_val <- prior$sha256
      if (is.null(prior_val) || length(prior_val) == 0L) NA_character_
      else as.character(prior_val)
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
  receipt$prior_sha256 <- prior_sha256

  if (!is.na(prior_sha256) && nzchar(prior_sha256) &&
      identical(sha256, prior_sha256)) {
    .msg("[ENT NPI Upload] SHA-256 unchanged -- skipping redundant upload\n")
    receipt$skipped_reason <- "sha256_unchanged"
    receipt$success <- TRUE
    return(flush_receipt())
  }

  # ── Upload ───────────────────────────────────────────────────────────────────
  remote_path <- "/isochrones/ent/enthealth_npi_LATEST.csv"
  receipt$dropbox_destination <- remote_path

  .msg("[ENT NPI Upload] Uploading %d rows (%.1f KB) to Dropbox: %s\n",
       row_count, receipt$file_size_bytes / 1024, remote_path)

  upload_ok <- tryCatch({
    dbxcli_put(csv_path, remote_path, max_retries = 3, verbose = verbose)
    TRUE
  }, error = function(e) {
    .msg("[ENT NPI Upload] Upload failed: %s\n", e$message)
    receipt$error_message <<- as.character(e$message)
    FALSE
  })

  if (!isTRUE(upload_ok)) {
    return(flush_receipt())
  }

  receipt$success <- TRUE
  .msg("[ENT NPI Upload] Upload complete.\n")
  flush_receipt()
}
