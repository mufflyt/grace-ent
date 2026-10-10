#' @title ENT NPPES Flat-File Parquet Fallback Strategy
#'
#' @description
#' Matches unmatched ENT physicians against the pre-built NPPES flat-file
#' parquet cache (`artifacts/ent/ent_from_nppes_flat_file.parquet`).
#'
#' This strategy runs AFTER the main DuckDB cascade. It targets physicians
#' not found in `temporal_all_years_fixed` (e.g., recently-enumerated
#' providers or those whose temporal snapshot is stale).
#'
#' @section Match logic:
#' \itemize{
#'   \item Exact UPPER(last_name) + UPPER(first_name) join
#'   \item State match → confidence 0.84; name-only → confidence 0.76
#'   \item Skips abog_ids already in pipeline_all_matches
#'   \item Zero-ambiguity guard: drops rows where the same name maps to > 1 NPI
#' }
#'
#' @param abog_parsed data.frame. Roster with columns abog_id, first_clean,
#'   last_clean, state.
#' @param con DuckDB connection. Used to check already-matched IDs.
#' @param specialty_config Named list. Must include nppes_flat_file_cache
#'   (path to parquet) or falls back to
#' artifacts/ent/ent_from_nppes_flat_file.parquet.
#' @param verbose logical.
#' @return data.frame with standardized output columns.
execute_strategy_ent_nppes_cache <- function(abog_parsed, con,
                                              specialty_config = list(),
                                              verbose = TRUE) {
  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 72), collapse = ""), "\n")
    cat("  STRATEGY ENT_CACHE: NPPES FLAT-FILE PARQUET FALLBACK\n")
    cat(paste(rep("=", 72), collapse = ""), "\n")
  }

  # Resolve parquet path from config or default
  cache_path <- specialty_config$nppes_flat_file_cache %||%
    here::here("artifacts", "ent", "ent_from_nppes_flat_file.parquet")

  if (!file.exists(cache_path)) {
    if (verbose) cat(sprintf("  [ENT_CACHE] Parquet not found: %s — skipping\n", cache_path))
    return(data.frame())
  }

  # Identify already-matched IDs so we only work on the unmatched tail
  already_matched_ids <- character(0)
  if (!is.null(con) && DBI::dbIsValid(con)) {
    tbl_exists <- tryCatch(
      DBI::dbExistsTable(con, "pipeline_all_matches"),
      error = function(e) FALSE
    )
    if (tbl_exists) {
      already_matched_ids <- tryCatch(
        DBI::dbGetQuery(con, "SELECT DISTINCT abog_id FROM pipeline_all_matches")$abog_id,
        error = function(e) character(0)
      )
    }
  }

  # Normalise ID column name — pipeline uses abog_id; standalone may use roster_id
  id_col <- if ("abog_id" %in% names(abog_parsed)) "abog_id"
            else if ("roster_id" %in% names(abog_parsed)) "roster_id"
            else if ("enthealth_id" %in% names(abog_parsed)) "enthealth_id"
            else stop("[ENT_CACHE] abog_parsed must have abog_id, roster_id, or enthealth_id column")
  if (id_col != "abog_id") {
    abog_parsed <- abog_parsed %>% dplyr::mutate(abog_id = .data[[id_col]])
  }

  candidates <- abog_parsed %>%
    dplyr::filter(!abog_id %in% already_matched_ids)

  if (nrow(candidates) == 0) {
    if (verbose) cat("  [ENT_CACHE] All physicians already matched — skipping\n")
    return(data.frame())
  }
  if (verbose) cat(sprintf("  [ENT_CACHE] %d unmatched candidates to check\n", nrow(candidates)))

  # Load parquet — as.data.frame() forces Arrow Table → R data.frame so dplyr
  # operates on local memory (avoids Arrow compute errors when DuckDB is loaded).
  nppes <- tryCatch(
    as.data.frame(arrow::read_parquet(cache_path)),
    error = function(e) {
      warning(sprintf("[ENT_CACHE] Failed to read parquet: %s", e$message))
      NULL
    }
  )
  if (is.null(nppes)) return(data.frame())

  nppes <- nppes %>%
    dplyr::mutate(
      last_upper  = toupper(trimws(last_name)),
      first_upper = toupper(trimws(first_name)),
      npi         = stringr::str_pad(as.character(trimws(npi)), 10, side = "left", pad = "0")
    )

  # Zero-ambiguity guard: drop any name that maps to > 1 NPI
  name_counts <- nppes %>%
    dplyr::count(last_upper, first_upper) %>%
    dplyr::filter(n == 1L) %>%
    dplyr::select(last_upper, first_upper)
  nppes_unambiguous <- nppes %>%
    dplyr::semi_join(name_counts, by = c("last_upper", "first_upper"))

  n_ambiguous <- nrow(nppes) - nrow(nppes_unambiguous)
  if (verbose && n_ambiguous > 0)
    cat(sprintf("  [ENT_CACHE] Dropped %d ambiguous name→NPI records\n", n_ambiguous))

  # Build join table from parquet side
  nppes_join_tbl <- nppes_unambiguous %>%
    dplyr::select(
      last_upper, first_upper,
      npi,
      npi_first_name  = first_name,
      npi_last_name   = last_name,
      npi_middle_name = middle_name,
      nppes_state     = state,
      primary_taxonomy
    )

  # Drop any pre-existing npi column from candidates to avoid npi.x/npi.y fan-out
  # after the join (pipeline adds npi to abog_parsed during matching).
  candidates_for_join <- candidates %>% dplyr::select(-dplyr::any_of("npi"))

  hits <- candidates_for_join %>%
    dplyr::inner_join(
      nppes_join_tbl,
      by = c("last_clean" = "last_upper", "first_clean" = "first_upper")
    )

  if (nrow(hits) == 0) {
    if (verbose) cat("  [ENT_CACHE] No matches found in parquet\n")
    return(data.frame())
  }

  # Assign confidence: state match boosts to 0.84, name-only → 0.76
  hits <- hits %>%
    dplyr::mutate(
      state_match      = (!is.na(state) & !is.na(nppes_state) & state == nppes_state),
      confidence_score = dplyr::if_else(state_match, 0.84, 0.76),
      match_method     = "strategy_ent_nppes_cache",
      npi_gender       = NA_character_,
      abog_gender      = if ("abog_gender" %in% names(.)) abog_gender else NA_character_,
      abog_gender_conf = if ("abog_gender_conf" %in% names(.)) abog_gender_conf else 0,
      exact_name       = TRUE,
      middle_ok        = NA,
      zip_match        = NA,
      city_match       = NA,
      spec_signal      = TRUE,
      gender_agreement = NA,
      year_proximity   = NA_real_
    )

  # Deduplicate within this strategy (prefer state-match over name-only)
  hits <- hits %>%
    dplyr::arrange(abog_id, dplyr::desc(confidence_score), npi) %>%
    dplyr::distinct(abog_id, .keep_all = TRUE)

  # Validate NPI format
  invalid_npi <- sum(!grepl("^[0-9]{10}$", hits$npi), na.rm = TRUE)
  if (invalid_npi > 0) {
    warning(sprintf("[ENT_CACHE] %d rows have invalid NPI format — dropping", invalid_npi))
    hits <- hits %>% dplyr::filter(grepl("^[0-9]{10}$", npi))
  }

  out_cols <- c(
    "abog_id", "npi", "confidence_score", "match_method",
    "npi_first_name", "npi_last_name", "npi_middle_name", "npi_gender",
    "abog_gender", "abog_gender_conf",
    "exact_name", "middle_ok", "zip_match", "city_match",
    "spec_signal", "gender_agreement", "year_proximity"
  )
  for (col in setdiff(out_cols, names(hits))) hits[[col]] <- NA
  hits <- hits %>% dplyr::select(dplyr::all_of(out_cols))

  if (verbose) {
    cat(sprintf("  [ENT_CACHE] %d matches (%.0f%% state-confirmed)\n",
                nrow(hits),
                100 * mean(hits$confidence_score >= 0.84, na.rm = TRUE)))
  }
  hits
}

# Null-coalescing fallback (defensive — pipeline defines this globally)
if (!exists("%||%", mode = "function")) {
#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}
