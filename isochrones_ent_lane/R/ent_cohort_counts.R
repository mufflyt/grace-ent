#' @title ENT Cohort Count Computation
#'
#' @description
#' Shared helper to compute physician counts at every stage of the ENT cohort
#' definition pipeline for STROBE diagrams and inclusion/exclusion appendices.
#'
#' @details
#' Processes ENThealth scrape data, AAO-HNS attending lists, and NPI match
#' results to produce a validated analytic cohort dynamically from source files.
#'
#' @family reporting-stats
#' @name ent_cohort_counts
NULL

# =============================================================================
# ENT COHORT COUNT COMPUTATION — shared helper
# =============================================================================
# Sourced by:
#   scripts/generate_ent_strobe_diagram.R
#   scripts/generate_ent_inclusion_exclusion_appendix.R
#   scripts/create_ent_table1.R
#
# Call:
#   ct <- compute_ent_cohort_counts(enthealth_csv, aao_xlsx, npi_match_csv)
#
# Returns a named list with:
#   - Every stage count for the STROBE diagram and inclusion/exclusion appendix
#   - cohort_df: the 5,549-row eligible cohort dataframe for Table 1
# All counts are derived live from the source files — no intermediate CSV is read.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(humaniformat)
})

#' Strip Special Characters from Names
#'
#' Internal helper to normalize names for fuzzy matching by converting to
#' uppercase, trimming whitespace, and removing hyphens, spaces, and
#' apostrophes.
#'
#' @param x `character vector`: The names to clean.
#'
#' @return Character vector of cleaned names.
#' @keywords internal
# H2 migration (PI ruling 2026-09-27): the local .strip_special()/substr
# key components are replaced by the mysterynpi primitives underlying
# blocking_key() -- measured ZERO DELTA on this file's frame (6,029 rows,
# docs/BLOCKING_KEY_PARITY_2026-09-27.md) before the swap.
if (!requireNamespace("mysterynpi", quietly = TRUE) ||
    utils::packageVersion("mysterynpi") < "0.7.0") {
  stop("ent_cohort_counts requires mysterynpi >= 0.7.0 (governed name keys).",
       call. = FALSE)
}

# Map ENThealth specialty_primary label → ent_subspecialty code
.SUBSPEC_FROM_SPECIALTY <- c(
  "General Otolaryngology"                    = "GEN_ENT",
  "Head and Neck Surgery"                     = "H_N_ONCO",
  "Neurotology"                               = "OTOL",
  "Otology/Audiology"                         = "OTOL",
  "Pediatric Otolaryngology"                  = "PEDS_ENT",
  "Allergy"                                   = "OTHER_ENT",
  "Facial Plastic and Reconstructive Surgery" = "FACIAL_PLAST",
  "Sleep Medicine"                            = "SLEEP",
  "Rhinology"                                 = "RHINO",
  "Laryngology"                               = "LARYN"
)

#' Compute ENT Cohort Counts
#'
#' Shared helper to compute physician counts at each stage of the ENT cohort 
#' definition pipeline. It processes ENThealth scrape data, AAO-HNS attending 
#' lists, and NPI match results to produce a validated analytic cohort.
#'
#' @param enthealth_csv `character`: Path to the geocoded ENThealth CSV.
#' @param aao_xlsx `character`: Path to the AAO-HNS attending list Excel file.
#' @param npi_match_csv `character`: Path to the NPI matching results CSV.
#' @param verbose `logical`: If TRUE, prints processing steps and summaries.
#'
#' @return A named list containing counts for every pipeline stage (STROBE) 
#'   and the final eligible \code{cohort_df}.
#'
#' @details
#' The computation follows four main stages:
#' \enumerate{
#'   \item \strong{Scrape Analysis}: Counts raw scraped records and geocoding
#' status.
#'   \item \strong{Directory Cross-check}: Performs an inner join between 
#'     ENThealth and AAO-HNS lists using a normalized name/state key.
#'   \item \strong{NPI Enrichment}: Integrates matched NPIs and handles 
#'     ambiguous matches (collisions and fan-outs).
#'   \item \strong{Eligibility Filters}: Applies clinical exclusions (e.g., 
#'     missing phone number) and geographic exclusions (e.g., Puerto Rico).
#' }
#'
#' @export
compute_ent_cohort_counts <- function(enthealth_csv, aao_xlsx, npi_match_csv,
                                      verbose = TRUE) {

  for (p in c(enthealth_csv, aao_xlsx, npi_match_csv)) {
    if (!file.exists(p)) stop("Required file not found: ", p)
  }

  # ── 1. Load raw files ────────────────────────────────────────────────────────
  if (verbose) cat("Loading ENThealth geocoded data ...\n")
  enthealth <- read_csv(enthealth_csv, show_col_types = FALSE)
  if (verbose) cat(sprintf("  %s rows, %d cols\n",
                            format(nrow(enthealth), big.mark = ","), ncol(enthealth)))

  if (verbose) cat("Loading AAO-HNS attending list ...\n")
  aao_raw <- read_excel(aao_xlsx) |> rename_with(trimws)
  if ("Country" %in% names(aao_raw))
    aao_raw <- filter(aao_raw, Country == "United States")
  if (verbose) cat(sprintf("  %s rows (US attendings)\n",
                            format(nrow(aao_raw), big.mark = ",")))

  if (verbose) cat("Loading NPI match results ...\n")
  npi_match <- read_csv(npi_match_csv, show_col_types = FALSE)
  if (verbose) cat(sprintf("  %s rows\n", format(nrow(npi_match), big.mark = ",")))

  # ── 2. Stage 1: ENThealth scrape counts ─────────────────────────────────────
  n_enthealth_raw <- nrow(enthealth)
  n_not_geocoded  <- sum(!enthealth$geocoded, na.rm = TRUE)
  n_geocoded      <- sum( enthealth$geocoded, na.rm = TRUE)

  # ── 3. Stage 2: Inner join ENThealth × AAO-HNS ──────────────────────────────
  # Join key: compact surname + first initial (mysterynpi) + state (all UPPER)
  enth_keys_df <- enthealth |>
    dplyr::filter(!is.na(last_name), !is.na(first_name), !is.na(state_code)) |>
    transmute(
      last_key   = mysterynpi::compact_name_key(last_name),
last_namefirst_init = mysterynpi::extract_first_initial(first_name),
      state_key  = toupper(trimws(state_code)),
      join_key   = paste(last_key, first_init, state_key, sep = "|")
    )

  aao_names_stripped <- sub(",.*$", "", trimws(aao_raw[["Full Name"]]))
  aao_parsed         <- humaniformat::parse_names(aao_names_stripped)
  aao_states         <- toupper(trimws(aao_raw[["State Province"]]))

  aao_keys_df <- tibble(
    last_raw  = aao_parsed$last_name,
    first_raw = aao_parsed$first_name,
    state_key = aao_states
  ) |>
    dplyr::filter(!is.na(last_raw), !is.na(first_raw), !is.na(state_key)) |>
    mutate(
      last_key   = mysterynpi::compact_name_key(last_raw),
last_rawfirst_init = mysterynpi::extract_first_initial(first_raw),
      join_key   = paste(last_key, first_init, state_key, sep = "|")
    )

  n_aao_total  <- nrow(aao_raw)
  matched_keys <- intersect(enth_keys_df$join_key, aao_keys_df$join_key)
  n_inner_join <- length(matched_keys)
  n_enth_only  <- n_enthealth_raw - n_inner_join
  n_aao_only   <- n_aao_total     - n_inner_join

  # ── 4. Stage 3: NPI enrichment ───────────────────────────────────────────────
  npi_keys <- npi_match |>
    mutate(
      last_key_m   = mysterynpi::compact_name_key(last_name),
last_namefirst_init_m = mysterynpi::extract_first_initial(first_name),
      state_key_m  = toupper(trimws(state_province)),
      join_key_m   = paste(last_key_m, first_init_m, state_key_m, sep = "|")
    )

  npi_inner             <- filter(npi_keys, join_key_m %in% matched_keys)
  n_npi_matched_inner   <- sum(npi_inner$match_found, na.rm = TRUE)
  n_npi_unmatched_inner <- nrow(npi_inner) - n_npi_matched_inner
  n_npi_not_in_file     <- n_inner_join - nrow(npi_inner)
  n_no_npi_total        <- n_npi_unmatched_inner + n_npi_not_in_file

  pass_counts <- if ("match_pass" %in% names(npi_inner))
    table(npi_inner$match_pass[npi_inner$match_found])
  else
    integer(0)

  # ── 5. Stage 4: Study eligibility exclusions ─────────────────────────────────
  # Mystery-caller design: physician must have a callable phone number listed
  # on ENThealth.org so a patient can find and call them.
  # Puerto Rico excluded: study callers are English-speaking only.
  in_join     <- enth_keys_df$join_key %in% matched_keys  # length = nrow(enthealth)
  in_join_idx <- which(in_join)                           # numeric indices for subsetting

  # Compute exclusion flags using numeric indices to avoid recycling warning
  no_phone    <- is.na(enthealth$phone_number[in_join_idx]) |
                 trimws(as.character(enthealth$phone_number[in_join_idx])) == ""
  is_pr       <- toupper(trimws(enthealth$state_code[in_join_idx])) == "PR"

  n_no_phone    <- sum(no_phone, na.rm = TRUE)
  n_puerto_rico <- sum(is_pr,    na.rm = TRUE)
  n_excl_elig   <- sum(no_phone | is_pr, na.rm = TRUE)
  n_after_elig  <- n_inner_join - n_excl_elig

  # ── 6. Final geocoded count within eligible cohort ───────────────────────────
  # Build a full-length in_elig mask (avoids length-mismatch recycling)
  in_elig              <- rep(FALSE, nrow(enthealth))
  in_elig[in_join_idx] <- !(no_phone | is_pr)
  n_geo_final          <- sum(enthealth$geocoded[in_elig], na.rm = TRUE)

  n_final <- n_after_elig

  # ── 7. Build eligible cohort dataframe (for Table 1) ─────────────────────────
  # Eligible ENThealth rows joined with NPIs from aao_npi_matched.csv
  elig_rows <- enthealth[in_elig, ] |>
    mutate(
      last_key_e  = mysterynpi::compact_name_key(last_name),
last_namefirst_init_e = mysterynpi::extract_first_initial(first_name),
      state_key_e  = toupper(trimws(state_code)),
      join_key_e   = paste(last_key_e, first_init_e, state_key_e, sep = "|")
    )

  # Build the raw (key, npi) pairs — one row per unique (key, NPI) combination.
  # Then remove two classes of ambiguous entries before joining:
  #
  #   Collision keys:  same LAST|INIT|STATE key → multiple distinct NPIs.
  #                    distinct(.keep_all=TRUE) would pick one arbitrarily.
  #
  #   Fan-out NPIs:    same NPI → multiple distinct keys.
  #                    One NPI was incorrectly matched to two different people
  #                    in aao_npi_matched.csv.
  #
  # In both cases it is safer to leave NPI = NA than to assign the wrong value.
  npi_lookup_raw <- npi_match |>
    filter(!is.na(npi)) |>
    mutate(
      last_key_m   = mysterynpi::compact_name_key(last_name),
last_namefirst_init_m = mysterynpi::extract_first_initial(first_name),
      state_key_m  = toupper(trimws(state_province)),
      join_key_m   = paste(last_key_m, first_init_m, state_key_m, sep = "|")
    ) |>
    select(join_key_m, npi) |>
    distinct(join_key_m, npi)   # unique (key, NPI) pairs; no arbitrary pick yet

  collision_keys <- npi_lookup_raw |>
    group_by(join_key_m) |>
    filter(dplyr::n_distinct(npi) > 1L) |>
    pull(join_key_m) |>
    unique()

  fanout_npis <- npi_lookup_raw |>
    group_by(npi) |>
    filter(dplyr::n_distinct(join_key_m) > 1L) |>
    pull(npi) |>
    unique()

  n_ambiguous <- length(collision_keys) + length(unique(
    npi_lookup_raw$join_key_m[npi_lookup_raw$npi %in% fanout_npis]
  ))
  if (n_ambiguous > 0 && verbose) {
    message(sprintf(
      "[NPI LOOKUP] Dropping %d collision keys and %d fan-out NPIs — NPI will be NA for those physicians.",
      length(collision_keys), length(fanout_npis)
    ))
  }

  npi_lookup <- npi_lookup_raw |>
    filter(!join_key_m %in% collision_keys, !npi %in% fanout_npis) |>
    distinct(join_key_m, .keep_all = TRUE)

  cohort_df <- elig_rows |>
    left_join(npi_lookup, by = c("join_key_e" = "join_key_m")) |>
    rename(state = state_code, phone_norm = phone_number) |>
    mutate(
      ent_subspecialty = .SUBSPEC_FROM_SPECIALTY[specialty_primary],
      npi              = as.character(npi)
    ) |>
    select(-last_key_e, -first_init_e, -state_key_e, -join_key_e)

  # Post-join guard: if the same NPI was assigned to multiple distinct physicians
  # (different URLs), the join key was non-unique within the ENTHealth cohort — the
  # NPI was legitimately unresolvable. Clear those assignments to NA.
  fanout_post <- cohort_df |>
    filter(!is.na(npi)) |>
    group_by(npi) |>
    filter(dplyr::n_distinct(url) > 1L) |>
    pull(npi) |>
    unique()

  if (length(fanout_post) > 0) {
    if (verbose) message(sprintf(
      "[NPI LOOKUP] Post-join: clearing %d fan-out NPIs (shared by multiple physicians) → NA.",
      length(fanout_post)
    ))
    cohort_df <- cohort_df |>
      dplyr::mutate(npi = dplyr::if_else(npi %in% fanout_post, NA_character_, npi))
  }

  # ── 8. Verbose summary ───────────────────────────────────────────────────────
  if (verbose) {
    cat(sprintf("\n--- Cohort count summary ---\n"))
    cat(sprintf("  ENThealth scraped           : %s\n", format(n_enthealth_raw, big.mark=",")))
    cat(sprintf("    Not geocoded (retained)   : %s\n", format(n_not_geocoded,  big.mark=",")))
    cat(sprintf("  AAO-HNS attendings          : %s\n", format(n_aao_total,     big.mark=",")))
    cat(sprintf("  Inner join                  : %s\n", format(n_inner_join,    big.mark=",")))
    cat(sprintf("    ENThealth-only (excl.)    : %s\n", format(n_enth_only,     big.mark=",")))
    cat(sprintf("  Excl. — no phone            : %s\n", format(n_no_phone,      big.mark=",")))
    cat(sprintf("  Excl. — Puerto Rico         : %s\n", format(n_puerto_rico,   big.mark=",")))
    cat(sprintf("  FINAL ANALYTIC COHORT       : %s\n", format(n_final,         big.mark=",")))
  }

  list(
    # raw inputs
    n_enthealth_raw   = n_enthealth_raw,
    n_not_geocoded    = n_not_geocoded,
    n_geocoded        = n_geocoded,
    # Stage 2
    n_aao_total       = n_aao_total,
    n_inner_join      = n_inner_join,
    n_enth_only       = n_enth_only,
    n_aao_only        = n_aao_only,
    # Stage 3
    n_npi_matched_inner   = n_npi_matched_inner,
    n_no_npi_total        = n_no_npi_total,
    pass_counts           = pass_counts,
    # Stage 4
    n_no_phone        = n_no_phone,
    n_puerto_rico     = n_puerto_rico,
    n_excl_elig       = n_excl_elig,
    n_after_elig      = n_after_elig,
    # final
    n_geo_final       = n_geo_final,
    n_final           = n_final,
    # keys (for the diagram's geo lookup)
    matched_keys      = matched_keys,
    enth_keys_df      = enth_keys_df,
    enthealth         = enthealth,
    # eligible cohort dataframe (for Table 1)
    cohort_df         = cohort_df
  )
}
