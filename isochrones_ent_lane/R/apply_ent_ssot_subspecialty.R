#' @title Apply SSOT Hierarchy for ENT Subspecialty Assignment
#'
#' @description
#' Assigns canonical ENT subspecialty codes using a 5-tier hierarchy:
#'
#'   Tier 1  – Board Certification (subspecialty cert only)
#'             ABMS scrape + ABOto targeted scrape + ABOHNS bulk are all treated
#' as
#'             equivalent sources — ABOto IS a member board of ABMS. All three
#' data
#'             files are pooled into a single lookup before the hierarchy is
#' applied.
#'             A subspecialty cert (Neurotology, CPO, Sleep, FPRS) beats a
#' general cert.
#'
#'   Tier 2  – ENThealth.org specialty_primary (direct subspecialty self-report)
#'
#'   Tier 2.5– fellowship_name (ENThealth fellowship training; Tier 2 skip on
#' GEN_ENT)
#'
#'   Tier 3  – NPPES taxonomy code (NPPES parquet + AAO supplemental)
#'
#'   Tier 4  – Board Certification (general ENT cert — confirms specialty, not
#' subspecialty)
#'
#'   Tier 5  – Default "GEN_ENT"
#'
#' Key design decisions:
#'   - ABMS and ABOto data pooled; subspecialty cert from either source wins
#'   - General ENT cert (Tier 4) never overrides fellowship/ENThealth
#' subspecialty
#'   - Bulk ABOHNS state parsing uses a full state-name lookup table (not regex)
#'   - ENThealth GEN_ENT is treated as NA so fellowship_name can override it
#' (Tier 2.5)
#'   - AAO membership flag (is_aao_member) + AAO taxonomy written to every row
#'
#' @param export_data \code{data.frame}. Pipeline output (must contain columns
#'   \code{first}, \code{last}, \code{state}, \code{npi}).
#' @param config \code{list}. Loaded \code{ent.yml} specialty config (must
#'   include
#'   \code{ssot_hierarchy}).
#' @return \code{data.frame}. Input with new or updated columns:
#'   \code{ent_subspecialty}, \code{ssot_source}, \code{taxonomy_code},
#'   \code{subspecialty}, \code{is_aao_member}, \code{aao_taxonomy}.
#' @export
apply_ent_ssot_subspecialty <- function(export_data, config) {
  if (is.null(config$ssot_hierarchy)) {
    warning("[SSOT] config$ssot_hierarchy is NULL; ENT subspecialty hierarchy skipped.", call. = FALSE)
    return(export_data)
  }

  cat("\n[SSOT] Applying ENT Subspecialty Hierarchy...\n")

  # ── Lookup tables ─────────────────────────────────────────────────────────────

  # Subspecialty-only board cert map — these certificates name a specific fellowship
  # training area recognised by ABOto / ABMS.  The general "Otolaryngology - Head &
  # Neck Surgery" cert is intentionally omitted here; it is handled at Tier 4.
  .BOARD_SUBSPEC_ONLY <- c(
    "Neurotology"                      = "OTOL_NEURO",
    "Pediatric Otolaryngology"         = "PED_ENT",
    "Complex Pediatric Otolaryngology" = "PED_ENT",
    "Sleep Medicine"                   = "SLEEP",
    "Facial Plastic Surgery"           = "FPRS"
  )

  # Full board map (adds the general cert for Tier 4 fallback)
  .BOARD_TO_SUBSPEC <- c(
    .BOARD_SUBSPEC_ONLY,
    "Otolaryngology - Head & Neck Surgery" = "GEN_ENT"
  )

  # ENThealth specialty_primary → canonical code
  .ENTHEALTH_TO_SUBSPEC <- c(
    "Head and Neck Surgery"                     = "H_N_ONCO",
    "Rhinology"                                 = "RHINOL",
    "Facial Plastic Surgery"                    = "FPRS",
    "Facial Plastic and Reconstructive Surgery" = "FPRS",
    "Otology"                                   = "OTOL_NEURO",
    "Otology/Audiology"                         = "OTOL_NEURO",
    "Neurotology"                               = "OTOL_NEURO",
    "Pediatric Otolaryngology"                  = "PED_ENT",
    "Laryngology"                               = "LARYNGOL",
    "Sleep Medicine"                            = "SLEEP",
    "Allergy"                                   = "ALLERGY",
    "Endocrine Surgery"                         = "H_N_ONCO",
    "General Otolaryngology"                    = "GEN_ENT"
  )

  # fellowship_name → canonical code (Tier 2.5)
  # Only maps fellowships with an unambiguous subspecialty signal.
  .FELLOWSHIP_TO_SUBSPEC <- c(
    "Pediatric Otolaryngology"                   = "PED_ENT",
    "Plastic & Reconstructive Surgery"           = "FPRS",
    "Head & Neck Oncology"                       = "H_N_ONCO",
    "Rhinology"                                  = "RHINOL",
    "Laryngology and the Voice"                  = "LARYNGOL",
    "Otology & Neurotology"                      = "OTOL_NEURO",
    "Neurotology and Skull Base Surgery"         = "OTOL_NEURO",
    "Microvascular and Head & Neck Reconstruction" = "H_N_ONCO",
    "Otology"                                    = "OTOL_NEURO",
    "Otolaryngic Allergy"                        = "ALLERGY",
    "Sleep Surgery"                              = "SLEEP",
    "Endocrine"                                  = "H_N_ONCO",
    "Oral & Facial Surgery"                      = "FPRS",
    "Clinical Research"                          = NA_character_,
    "General Surgery"                            = NA_character_
  )

  # NUCC taxonomy codes
  .TAXONOMY_TO_SUBSPEC <- c(
    "207Y00000X" = "GEN_ENT",
    "207YS0123X" = "FPRS",
    "207YX0602X" = "OTOL_NEURO",
    "207YX0901X" = "PED_ENT",
    "207YX0905X" = "SLEEP",
    "207YP0228X" = "PED_ENT",
    "207YS0012X" = "SLEEP",
    "207YX0007X" = "FPRS"
  )

  # Full-state-name → abbreviation (bulk ABOHNS stores "Cincinnati, Ohio")
  .STATE_NAME_TO_ABBR <- c(
    "ALABAMA" = "AL", "ALASKA" = "AK", "ARIZONA" = "AZ", "ARKANSAS" = "AR",
    "CALIFORNIA" = "CA", "COLORADO" = "CO", "CONNECTICUT" = "CT",
    "DELAWARE" = "DE", "FLORIDA" = "FL", "GEORGIA" = "GA", "HAWAII" = "HI",
    "IDAHO" = "ID", "ILLINOIS" = "IL", "INDIANA" = "IN", "IOWA" = "IA",
    "KANSAS" = "KS", "KENTUCKY" = "KY", "LOUISIANA" = "LA", "MAINE" = "ME",
    "MARYLAND" = "MD", "MASSACHUSETTS" = "MA", "MICHIGAN" = "MI",
    "MINNESOTA" = "MN", "MISSISSIPPI" = "MS", "MISSOURI" = "MO",
    "MONTANA" = "MT", "NEBRASKA" = "NE", "NEVADA" = "NV",
    "NEW HAMPSHIRE" = "NH", "NEW JERSEY" = "NJ", "NEW MEXICO" = "NM",
    "NEW YORK" = "NY", "NORTH CAROLINA" = "NC", "NORTH DAKOTA" = "ND",
    "OHIO" = "OH", "OKLAHOMA" = "OK", "OREGON" = "OR", "PENNSYLVANIA" = "PA",
    "RHODE ISLAND" = "RI", "SOUTH CAROLINA" = "SC", "SOUTH DAKOTA" = "SD",
    "TENNESSEE" = "TN", "TEXAS" = "TX", "UTAH" = "UT", "VERMONT" = "VT",
    "VIRGINIA" = "VA", "WASHINGTON" = "WA", "WEST VIRGINIA" = "WV",
    "WISCONSIN" = "WI", "WYOMING" = "WY", "DISTRICT OF COLUMBIA" = "DC"
  )

  # ── Helpers ───────────────────────────────────────────────────────────────────
  .strip_special <- function(x) gsub("[^A-Z]", "", toupper(as.character(x)))

  # ── Detect join columns ───────────────────────────────────────────────────────
  first_col <- intersect(c("first_clean", "first_name", "first", "query_first_name"), names(export_data))[1]
  last_col  <- intersect(c("last_clean",  "last_name",  "last",  "query_last_name"),  names(export_data))[1]
  state_col <- intersect(c("state", "query_state"), names(export_data))[1]

  if (is.na(first_col) || is.na(last_col) || is.na(state_col)) {
    warning("[SSOT] Missing name/state columns. Skipping SSOT.")
    return(export_data)
  }

  n_in <- nrow(export_data)

  export_data <- export_data |>
    dplyr::mutate(
      .join_first = .strip_special(!!rlang::sym(first_col)),
      .join_last  = .strip_special(!!rlang::sym(last_col)),
      .join_state = toupper(trimws(!!rlang::sym(state_col)))
    )

  # ── AAO membership + supplemental taxonomy (joined before hierarchy) ──────────
  aao_file <- here::here(config$ssot_hierarchy$aao_member_file)
  if (!is.null(aao_file) && file.exists(aao_file) && "npi" %in% names(export_data)) {
    aao_df <- suppressMessages(readr::read_csv(aao_file, show_col_types = FALSE))

    aao_lookup <- aao_df |>
      dplyr::filter(!is.na(npi), match_found == TRUE) |>
      dplyr::mutate(
        npi             = as.character(npi),
        is_aao_member   = TRUE,
        aao_taxonomy    = ent_taxonomy,
        aao_tax_subspec = .TAXONOMY_TO_SUBSPEC[ent_taxonomy]
      ) |>
      dplyr::select(npi, is_aao_member, aao_taxonomy, aao_tax_subspec) |>
      dplyr::distinct(npi, .keep_all = TRUE)

    export_data <- export_data |>
      dplyr::mutate(npi = as.character(npi)) |>
      dplyr::left_join(aao_lookup, by = "npi") |>
      dplyr::mutate(is_aao_member = dplyr::coalesce(is_aao_member, FALSE))

    cat(sprintf(
      "  [SSOT] AAO: %d members flagged; %d with subspecialty taxonomy\n",
      sum(export_data$is_aao_member, na.rm = TRUE),
      sum(!is.na(export_data$aao_tax_subspec), na.rm = TRUE)
    ))
  } else {
    export_data$is_aao_member   <- NA
    export_data$aao_taxonomy    <- NA_character_
    export_data$aao_tax_subspec <- NA_character_
    cat("  [SSOT] AAO file not found\n")
  }

  # ── TIER 1: Pool all board cert sources (ABMS + ABOto = same certifying body) ──
  #
  # Three files, same certification authority:
  #   a) ABMS certificationmatters.org scrape  — has subspecialty + specialty fields
  #   b) ABOHNS.org targeted scrape            — queried by physician name/state
  #   c) ABOHNS.org bulk download              — full certified physician list
  #
  # Steps:
  #   1. Normalise each file to (join_first, join_last, join_state, certification)
  #   2. Stack all three into one table
  #   3. Per physician: prefer subspecialty cert over general cert
  #   4. Derive board_subspec_specific (subspecialty only) and board_subspec (any)

  all_certs <- list()

  # a) ABMS scrape ─────────────────────────────────────────────────────────────
  abms_file <- here::here(config$ssot_hierarchy$abms_results_file)
  if (file.exists(abms_file)) {
    abms_raw <- suppressMessages(readr::read_csv(abms_file, show_col_types = FALSE))

    abms_rows <- abms_raw |>
      dplyr::filter(
        abms_found == TRUE,
        grepl("Otolaryngol", abms_specialty, ignore.case = TRUE)
      ) |>
      dplyr::mutate(
        .join_first   = .strip_special(search_first),
        .join_last    = .strip_special(search_last),
        .join_state   = toupper(trimws(search_state)),
        # Prefer subspecialty over secondary subspecialty over specialty
        certification = dplyr::case_when(
          abms_subspecialty %in% names(.BOARD_TO_SUBSPEC) ~ abms_subspecialty,
          abms_subspecialty_2 %in% names(.BOARD_TO_SUBSPEC) ~ abms_subspecialty_2,
          TRUE ~ abms_specialty
        )
      ) |>
      dplyr::select(.join_first, .join_last, .join_state, certification)

    all_certs[["abms"]] <- abms_rows
    cat(sprintf("  [Board] ABMS scrape: %d ENT-verified rows\n", nrow(abms_rows)))
  } else {
    cat("  [Board] ABMS scrape file not found\n")
  }

  # b) ABOto targeted scrape ───────────────────────────────────────────────────
  board_results_file <- here::here(config$ssot_hierarchy$board_results_file)
  if (file.exists(board_results_file)) {
    targeted_raw <- suppressMessages(readr::read_csv(board_results_file, show_col_types = FALSE))

    targeted_rows <- targeted_raw |>
      dplyr::mutate(
        .join_first = .strip_special(query_first_name),
        .join_last  = .strip_special(query_last_name),
        .join_state = toupper(trimws(query_state))
      ) |>
      dplyr::select(.join_first, .join_last, .join_state, certification)

    all_certs[["targeted"]] <- targeted_rows
    cat(sprintf("  [Board] ABOto targeted scrape: %d rows\n", nrow(targeted_rows)))
  } else {
    cat("  [Board] ABOto targeted scrape file not found\n")
  }

  # c) ABOHNS bulk ─────────────────────────────────────────────────────────────
  bulk_file <- here::here(config$ssot_hierarchy$abohns_bulk_file)
  if (file.exists(bulk_file)) {
    bulk_raw <- suppressMessages(readr::read_csv(bulk_file, show_col_types = FALSE))

    bulk_rows <- bulk_raw |>
      dplyr::mutate(
        .name_split      = trimws(name),
        .join_last       = .strip_special(sub(".* ", "", .name_split)),
        .join_first_full = .strip_special(sub(" .*", "", .name_split)),
        .join_first      = substr(.join_first_full, 1, 5),
        .state_full      = toupper(trimws(sub("^.*, ", "", location))),
        .join_state      = .STATE_NAME_TO_ABBR[.state_full]
      ) |>
      dplyr::filter(!is.na(.join_state)) |>
      dplyr::select(.join_first, .join_last, .join_state, certification)

    all_certs[["bulk"]] <- bulk_rows
    cat(sprintf("  [Board] ABOHNS bulk: %d rows\n", nrow(bulk_rows)))
  } else {
    cat("  [Board] ABOHNS bulk file not found\n")
  }

  # ── Pool and resolve per-physician best certification ────────────────────────
  if (length(all_certs) > 0) {
    pooled <- dplyr::bind_rows(all_certs) |>
      dplyr::filter(!is.na(certification), certification %in% names(.BOARD_TO_SUBSPEC)) |>
      dplyr::mutate(
        # Subspecialty cert scores higher than general ENT cert
        .cert_score = dplyr::if_else(
          certification == "Otolaryngology - Head & Neck Surgery", 1L, 2L
        )
      ) |>
      dplyr::arrange(dplyr::desc(.cert_score)) |>
      dplyr::distinct(.join_first, .join_last, .join_state, .keep_all = TRUE) |>
      dplyr::mutate(
        board_subspec = .BOARD_TO_SUBSPEC[certification],
        board_subspec_specific = dplyr::if_else(
          certification %in% names(.BOARD_SUBSPEC_ONLY),
          .BOARD_SUBSPEC_ONLY[certification],
          NA_character_
        )
      ) |>
      dplyr::select(.join_first, .join_last, .join_state, board_subspec, board_subspec_specific)

    n_subspec <- sum(!is.na(pooled$board_subspec_specific))
    n_total   <- nrow(pooled)
    cat(sprintf(
      "  [Board] Combined pool: %d unique physicians; %d (%.0f%%) with subspecialty cert\n",
      n_total, n_subspec, 100 * n_subspec / n_total
    ))

    export_data <- export_data |>
      dplyr::left_join(pooled, by = c(".join_first", ".join_last", ".join_state"))
  } else {
    export_data$board_subspec          <- NA_character_
    export_data$board_subspec_specific <- NA_character_
  }

  # ── TIER 2: ENThealth specialty_primary ──────────────────────────────────────
  if ("specialty_primary" %in% names(export_data)) {
    export_data <- export_data |>
      dplyr::mutate(enthealth_subspec = .ENTHEALTH_TO_SUBSPEC[specialty_primary])
    cat(sprintf(
      "  [SSOT] ENThealth (specialty_primary): %d non-NA\n",
      sum(!is.na(export_data$enthealth_subspec))
    ))
  } else {
    export_data$enthealth_subspec <- NA_character_
  }

  # ── TIER 2.5: fellowship_name inference ──────────────────────────────────────
  # Only fires when enthealth_subspec is GEN_ENT or NA (see coalesce logic below).
  if ("fellowship_name" %in% names(export_data)) {
    export_data <- export_data |>
      dplyr::mutate(fellowship_subspec = .FELLOWSHIP_TO_SUBSPEC[fellowship_name])
    cat(sprintf(
      "  [SSOT] Fellowship name: %d non-NA inferences\n",
      sum(!is.na(export_data$fellowship_subspec))
    ))
  } else {
    export_data$fellowship_subspec <- NA_character_
  }

  # ── TIER 3: NPPES taxonomy (parquet + AAO supplemental) ──────────────────────
  nppes_file <- here::here(config$ssot_hierarchy$NPPES_taxonomy_file)
  if (file.exists(nppes_file) && "npi" %in% names(export_data)) {
    nppes_df <- if (grepl("\\.parquet$", nppes_file)) {
      arrow::read_parquet(nppes_file)
    } else {
      suppressMessages(readr::read_csv(nppes_file, show_col_types = FALSE))
    }

    tax_cols <- intersect(
      c("Healthcare Provider Taxonomy Code_1", "taxonomy_1",
        "npi_taxonomy_1", "primary_taxonomy", "taxonomy_code"),
      names(nppes_df)
    )
    npi_col <- intersect(c("NPI", "npi"), names(nppes_df))[1]

    if (length(tax_cols) > 0 && !is.na(npi_col)) {
      nppes_tax <- nppes_df |>
        dplyr::select(npi = !!rlang::sym(npi_col), taxonomy_code = !!rlang::sym(tax_cols[1])) |>
        dplyr::mutate(
          npi              = as.character(npi),
          taxonomy_subspec = .TAXONOMY_TO_SUBSPEC[taxonomy_code]
        ) |>
        dplyr::distinct(npi, .keep_all = TRUE)

      if ("taxonomy_code" %in% names(export_data)) {
        export_data <- export_data |>
          dplyr::mutate(npi = as.character(npi)) |>
          dplyr::left_join(dplyr::select(nppes_tax, npi, taxonomy_subspec), by = "npi")
      } else {
        export_data <- export_data |>
          dplyr::mutate(npi = as.character(npi)) |>
          dplyr::left_join(nppes_tax, by = "npi")
      }
    } else {
      export_data$taxonomy_subspec <- NA_character_
    }
  } else {
    export_data$taxonomy_subspec <- NA_character_
  }

  if (!"aao_tax_subspec" %in% names(export_data)) export_data$aao_tax_subspec <- NA_character_
  export_data <- export_data |>
    dplyr::mutate(combined_taxonomy_subspec = dplyr::coalesce(taxonomy_subspec, aao_tax_subspec))

  # ── Resolve final hierarchy ───────────────────────────────────────────────────
  #
  # ENThealth specialty_primary = "General Otolaryngology" maps to "GEN_ENT".
  # We set enthealth_subspec_specific = NA for those cases so fellowship_subspec
  # (Tier 2.5) can override it without a string-equality check inside coalesce().
  #
  export_data <- export_data |>
    dplyr::mutate(
      enthealth_subspec_specific = dplyr::if_else(
        !is.na(enthealth_subspec) & enthealth_subspec != "GEN_ENT",
        enthealth_subspec, NA_character_
      ),
      ent_subspecialty = dplyr::coalesce(
        board_subspec_specific,    # Tier 1   board subspecialty cert (all 3 sources)
        enthealth_subspec_specific,# Tier 2   ENThealth subspecialty (not GEN_ENT)
        fellowship_subspec,        # Tier 2.5 fellowship name inference
        combined_taxonomy_subspec, # Tier 3   NPPES taxonomy (parquet + AAO)
        enthealth_subspec,         # Tier 2b  ENThealth GEN_ENT confirmation
        board_subspec,             # Tier 4   board general ENT cert
        "GEN_ENT"                 # Tier 5   default
      ),
      ssot_source = dplyr::case_when(
        !is.na(board_subspec_specific)     ~ "Board Cert (Subspecialty)",
        !is.na(enthealth_subspec_specific) ~ "ENThealth",
        !is.na(fellowship_subspec)         ~ "Fellowship Name",
        !is.na(combined_taxonomy_subspec)  ~ "NPPES Taxonomy",
        !is.na(enthealth_subspec)          ~ "ENThealth (GEN_ENT)",
        !is.na(board_subspec)              ~ "Board Cert (General ENT)",
        TRUE                               ~ "Default (GEN_ENT)"
      )
    )

  # ── Summary ───────────────────────────────────────────────────────────────────
  cat("\n  [SSOT] Source distribution:\n")
  print(table(export_data$ssot_source, useNA = "always"))
  cat("\n  [SSOT] Subspecialty distribution:\n")
  print(table(export_data$ent_subspecialty, useNA = "always"))
  cat(sprintf(
    "\n  [SSOT] AAO member rate: %d / %d (%.1f%%)\n",
    sum(export_data$is_aao_member, na.rm = TRUE),
    nrow(export_data),
    100 * mean(export_data$is_aao_member, na.rm = TRUE)
  ))

  # ── Row-count guard ───────────────────────────────────────────────────────────
  if (nrow(export_data) != n_in) {
    stop(sprintf("[SSOT] Row count changed: %d → %d", n_in, nrow(export_data)), call. = FALSE)
  }

  # ── Drop internal work columns ────────────────────────────────────────────────
  cols_to_drop <- intersect(
    c(".join_first", ".join_last", ".join_state",
      "board_subspec", "board_subspec_specific",
      "enthealth_subspec", "enthealth_subspec_specific", "fellowship_subspec",
      "taxonomy_subspec", "aao_tax_subspec", "combined_taxonomy_subspec"),
    names(export_data)
  )
  export_data <- export_data |> dplyr::select(-dplyr::all_of(cols_to_drop))

  export_data$subspecialty <- export_data$ent_subspecialty

  cat(sprintf("\n  [SSOT] Done. %d / %d physicians assigned subspecialties.\n",
    sum(!is.na(export_data$ent_subspecialty)), nrow(export_data)
  ))

  return(export_data)
}

# ── Retired physician utilities ───────────────────────────────────────────────

#' Find rows where any column contains "retired" (case-insensitive)
#'
#' @param df \code{data.frame}. ENT physician roster.
#' @return Logical vector the same length as \code{nrow(df)}.
#' @keywords internal
find_retired <- function(df) {
  mask <- rep(FALSE, nrow(df))
  for (col in names(df)) {
    mask <- mask | grepl("retired", as.character(df[[col]]), ignore.case = TRUE)
  }
  mask
}

#' Remove Retired Physicians from the ENT Roster
#'
#' Drops any row where "retired" appears in any column and prints a summary.
#'
#' @param df \code{data.frame}. ENT physician roster.
#' @param verbose Logical. Print removed records (default \code{TRUE}).
#' @return Filtered \code{data.frame}.
#' @keywords internal
remove_retired <- function(df, verbose = TRUE) {
  mask <- find_retired(df)
  if (verbose && any(mask)) {
    removed <- df[mask, intersect(c("full_name", "address", "company_name"), names(df))]
    message(sprintf("Removing %d retired physician(s):", sum(mask)))
    for (i in seq_len(nrow(removed))) {
      tag <- removed$address[i]
      if (is.na(tag)) tag <- removed$company_name[i]
      message(sprintf("  %s  [%s]", removed$full_name[i], tag))
    }
  }
  df[!mask, ]
}

# ── General roster cleaning utilities ────────────────────────────────────────

#' Normalize Credential Strings to a Canonical Form
#'
#' Fixes case ("Md" -> "MD"), collapses whitespace/comma variants
#' ("MD  PhD", "MD,PhD", "MD, PhD" -> "MD PhD"), and trims surrounding
#' whitespace.
#'
#' @param df \code{data.frame}. ENT physician roster (must have a
#'   \code{credentials} column).
#' @return \code{data.frame} with \code{credentials} normalized in place.
#' @keywords internal
normalize_credentials <- function(df) {
  if (!"credentials" %in% names(df)) return(df)
  df$credentials <- df$credentials |>
    trimws() |>
    toupper() |>
    gsub(",\\s*", " ", x = _) |>   # "MD, PhD" -> "MD PhD"
    gsub("\\s{2,}", " ", x = _)     # "MD  PhD" -> "MD PhD"
  df
}

#' Fix zip5 stored as float (e.g. 8901.0 -> "08901", 44122.0 -> "44122")
#'
#' Converts numeric zip5 values to zero-padded 5-character strings.
#' Already-correct 5-character strings are left unchanged.
#'
#' @param df data.frame  ENT physician roster (must have a \code{zip5} column)
#' @return data.frame with \code{zip5} as zero-padded character strings
#' @keywords internal
fix_zip5 <- function(df) {
  if (!"zip5" %in% names(df)) return(df)
  z <- df$zip5
  numeric_mask <- !is.na(z) & grepl("^[0-9]+\\.?[0-9]*$", as.character(z))
  z[numeric_mask] <- sprintf("%05d", as.integer(as.numeric(z[numeric_mask])))
  df$zip5 <- z
  df
}

#' Fix phone_digits stored in scientific notation (e.g. 2.168393e+09 ->
#' "2168393000")
#'
#' Converts float phone_digits to 10-character strings and blanks out values
#' that are not exactly 10 digits.
#'
#' @param df data.frame  ENT physician roster (must have a \code{phone_digits}
#'   column)
#' @return data.frame with \code{phone_digits} as 10-digit character strings (or
#'   NA)
#' @keywords internal
fix_phone_digits <- function(df) {
  if (!"phone_digits" %in% names(df)) return(df)
  d <- as.character(df$phone_digits)
  # Convert scientific-notation floats to integer strings
  float_mask <- !is.na(df$phone_digits) & grepl("e|\\.", d, ignore.case = TRUE)
  d[float_mask] <- as.character(as.integer(round(as.numeric(df$phone_digits[float_mask]))))
  # Blank non-10-digit values
  bad <- !is.na(d) & nchar(d) != 10L
  if (any(bad)) {
    message(sprintf("fix_phone_digits: blanking %d phone_digits values with wrong length.", sum(bad)))
    d[bad] <- NA_character_
  }
  df$phone_digits <- d
  df
}

#' Flag or nullify future training years
#'
#' Sets \code{residency_year} and/or \code{medical_school_year} to NA when
#' they exceed \code{max_year} (default: current calendar year).  Updates
#' \code{career_stage} to "In Training" for affected residency rows.
#'
#' @param df       data.frame  ENT physician roster
#' @param max_year integer     Ceiling year; years above this are treated as
#'   errors
#' @return data.frame with corrected year columns
#' @keywords internal
fix_future_years <- function(df, max_year = as.integer(format(Sys.Date(), "%Y"))) {
  if ("residency_year" %in% names(df)) {
    bad_res <- !is.na(df$residency_year) & df$residency_year > max_year
    if (any(bad_res)) {
      message(sprintf("fix_future_years: setting %d future residency_year(s) to NA.", sum(bad_res)))
      df$residency_year[bad_res]   <- NA_real_
      df$years_since_residency[bad_res] <- NA_real_
      if ("career_stage" %in% names(df)) df$career_stage[bad_res] <- "In Training"
    }
  }
  if ("medical_school_year" %in% names(df)) {
    bad_ms <- !is.na(df$medical_school_year) & df$medical_school_year > max_year
    if (any(bad_ms)) {
      message(sprintf("fix_future_years: setting %d future medical_school_year(s) to NA.", sum(bad_ms)))
      df$medical_school_year[bad_ms] <- NA_real_
    }
  }
  df
}

#' Collapse whitespace across all free-text columns
#'
#' Applies \code{stringr::str_squish()} to name, address, and training
#' columns — collapsing any run of whitespace to a single space and
#' trimming both ends.  Covers the ENThealth double-space scrape artifact
#' (e.g. "Aaron  Wood, MD", "3rd  Fl.") across all affected fields.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with whitespace-normalized text columns
#' @keywords internal
fix_name_whitespace <- function(df) {
  cols <- c("full_name", "clean_name", "company_name",
            "address", "residency_name", "medical_school_name", "fellowship_name")
  for (col in intersect(cols, names(df))) {
    df[[col]] <- stringr::str_squish(df[[col]])
  }
  df
}

#' Replace semicolons with commas in address
#'
#' A small number of ENThealth addresses use semicolons as suite separators
#' (e.g. "2233 Post Street; Room 309").  Standardises to comma.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with address semicolons replaced
#' @keywords internal
fix_address_semicolons <- function(df) {
  if (!"address" %in% names(df)) return(df)
  has_semi <- !is.na(df$address) & stringr::str_detect(df$address, ";")
  if (any(has_semi)) {
    df$address[has_semi] <- stringr::str_replace_all(df$address[has_semi], ";\\s*", ", ")
    message(sprintf("fix_address_semicolons: fixed %d address(es).", sum(has_semi)))
  }
  df
}

#' Normalise city casing and strip embedded state suffix
#'
#' Two issues fixed in one pass:
#' \enumerate{
#'   \item ALL CAPS cities (e.g. "ORLANDO") converted to title case.
#'         Known military/abbreviation codes (FPO, APO, JBER, ABQ) are left
#'         as-is because they are not prose city names.
#'   \item City values with a trailing state code (e.g. "Colonial Heights, VA")
#'         have the suffix stripped — the state is already in \code{state_code}.
#' }
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with cleaned city column
#' @keywords internal
fix_city <- function(df) {
  if (!"city" %in% names(df)) return(df)

  keep_upper <- c("FPO", "APO", "JBER", "ABQ", "NY")   # known codes/abbrevs

  city <- df$city

  # Strip trailing ", ST" or " ST" state suffix
  embedded <- !is.na(city) & stringr::str_detect(city, ",?\\s+[A-Z]{2}$")
  if (any(embedded)) {
    city[embedded] <- stringr::str_remove(city[embedded], ",?\\s+[A-Z]{2}$")
    message(sprintf("fix_city: stripped embedded state from %d city value(s).", sum(embedded)))
  }

  # Title-case ALL CAPS cities, preserving known codes
  all_caps <- !is.na(city) & city == toupper(city) & nchar(city) > 4 & !(city %in% keep_upper)
  if (any(all_caps)) {
    city[all_caps] <- stringr::str_to_title(city[all_caps])
    message(sprintf("fix_city: title-cased %d ALL CAPS city value(s).", sum(all_caps)))
  }

  df$city <- city
  df
}

#' Remove company_name values that are clearly scrape artifacts
#'
#' Single-token values like "Dr", "Dr.", "Y", "MC", "CHI", "FAA" are not
#' real practice names — they are HTML parsing artifacts from ENThealth.
#' Replaced with NA so downstream code does not treat them as practice names.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with artifact company_name values set to NA
#' @keywords internal
fix_company_name_artifacts <- function(df) {
  if (!"company_name" %in% names(df)) return(df)
  artifacts <- c("Dr", "Dr.", "Y", "MC", "CHI", "FAA", "VA", "UAB", "WVU", "MCW", "NIH", "FDA")
  bad <- !is.na(df$company_name) & df$company_name %in% artifacts
  if (any(bad)) {
    message(sprintf("fix_company_name_artifacts: cleared %d artifact company_name value(s).", sum(bad)))
    df$company_name[bad] <- NA_character_
  }
  df
}

#' Strip "United State" scrape artifact from address and recover embedded
#' city/state
#'
#' ENThealth sometimes produced addresses like "Chicago IL  United State" when
#' the city/state were not parsed into their own columns.  This function:
#'   1. Extracts embedded "CITY ST" before "United State" into \code{city} /
#'      \code{state_code} when those columns are currently NA.
#'   2. Removes the trailing "United State" (and any preceding zip-only prefix
#'      like "70118 United State") from the address string.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with cleaned address and recovered city/state where
#'   possible
#' @keywords internal
fix_address_artifact <- function(df) {
  if (!"address" %in% names(df)) return(df)

  us_mask <- grepl("United State", df$address, ignore.case = TRUE, fixed = FALSE)
  if (!any(us_mask, na.rm = TRUE)) return(df)

  # Pattern: "City Name ST  United State" — capture city + 2-letter state code
  city_state_pattern <- "^(.+?)\\s+([A-Z]{2})\\s+United State"

  for (i in which(us_mask)) {
    addr <- df$address[i]
    if (is.na(addr)) next

    m <- regmatches(addr, regexpr(city_state_pattern, addr, perl = TRUE))
    if (length(m) == 1L) {
      parts <- regmatches(addr, regexec(city_state_pattern, addr, perl = TRUE))[[1]]
      # parts[2] = city, parts[3] = state code
      if (is.na(df$city[i])) {
        if (is.na(df$city[i]))       df$city[i]       <- trimws(parts[2])
        if (is.na(df$state_code[i])) df$state_code[i] <- parts[3]
      }
      df$address[i] <- NA_character_   # address is now just city+state; not useful
    } else {
      # Pattern is "70118 United State" or similar — no city/state recoverable
      df$address[i] <- NA_character_
    }
  }

  n <- sum(us_mask, na.rm = TRUE)
  message(sprintf("fix_address_artifact: cleaned %d address(es) with 'United State' artifact.", n))
  df
}

#' Flag inconsistency between is_board_certified and has_abohns
#'
#' 72 records have \code{is_board_certified = TRUE} but \code{has_abohns =
#' FALSE}.
#' These physicians hold a non-ABOHNS board certification.  This function adds
#' a \code{board_cert_source_conflict} flag so analysts can review them, but
#' does NOT alter either flag.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with added \code{board_cert_source_conflict} logical
#'   column
#' @keywords internal
flag_board_cert_conflicts <- function(df) {
  needed <- c("is_board_certified", "has_abohns")
  if (!all(needed %in% names(df))) return(df)

  conflict <- !is.na(df$is_board_certified) & df$is_board_certified &
              !is.na(df$has_abohns)          & !df$has_abohns

  df$board_cert_source_conflict <- conflict

  if (any(conflict, na.rm = TRUE)) {
    message(sprintf(
      "flag_board_cert_conflicts: %d record(s) have is_board_certified=TRUE but has_abohns=FALSE.",
      sum(conflict, na.rm = TRUE)
    ))
  }
  df
}

#' Fix zip4 stored as float (e.g. 1191.0 -> "1191", 1.0 -> "0001")
#'
#' Mirrors \code{fix_zip5()}: converts numeric zip4 values to zero-padded
#' 4-character strings.
#'
#' @param df data.frame  ENT physician roster (must have a \code{zip4} column)
#' @return data.frame with \code{zip4} as zero-padded 4-character strings
#' @keywords internal
fix_zip4 <- function(df) {
  if (!"zip4" %in% names(df)) return(df)
  z <- df$zip4
  numeric_mask <- !is.na(z) & grepl("^[0-9]+\\.?[0-9]*$", as.character(z))
  z[numeric_mask] <- sprintf("%04d", as.integer(as.numeric(z[numeric_mask])))
  df$zip4 <- z
  df
}

#' Repair last_name / middle_name when the name parser placed a suffix in
#' last_name
#'
#' ENThealth names like "Alfred A. Simental Jr, MD" were sometimes parsed as:
#'   first_name = "Alfred", middle_name = "A. Simental", last_name = "Jr"
#'
#' This function detects rows where \code{last_name} is a generation suffix
#' (Jr, Sr, II, III, IV, V), then:
#'   \enumerate{
#'     \item Moves the suffix to a new \code{name_suffix} column.
#'     \item Promotes the last token of \code{middle_name} to \code{last_name}.
#'     \item Trims \code{middle_name} to its remaining tokens.
#'   }
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with corrected name columns and a \code{name_suffix}
#'   column
#' @keywords internal
fix_last_name_suffix <- function(df) {
  needed <- c("last_name", "middle_name")
  if (!all(needed %in% names(df))) return(df)

  suffix_re <- "^(Jr\\.?|Sr\\.?|II|III|IV|V)$"
  is_suffix  <- grepl(suffix_re, df$last_name, ignore.case = FALSE)

  if (!any(is_suffix)) return(df)

  if (!"name_suffix" %in% names(df)) df$name_suffix <- NA_character_

  for (i in which(is_suffix)) {
    df$name_suffix[i] <- df$last_name[i]

    tokens <- stringr::str_split(trimws(df$middle_name[i]), "\\s+")[[1]]
    tokens <- tokens[nchar(tokens) > 0]

    if (length(tokens) >= 1) {
      df$last_name[i]   <- tokens[length(tokens)]
      df$middle_name[i] <- if (length(tokens) > 1)
                             paste(tokens[-length(tokens)], collapse = " ")
                           else NA_character_
    }
  }

  message(sprintf("fix_last_name_suffix: repaired %d record(s) with suffix in last_name.", sum(is_suffix)))
  df
}

# ── Derivable-column enrichment ──────────────────────────────────────────────

#' Fill missing us_region and bog_region from state_code
#'
#' Both columns are fully deterministic from state_code.  Fills any NA values
#' using hardcoded state → region lookup tables derived from the existing data.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with \code{us_region} and \code{bog_region} filled
#' @keywords internal
fill_regions_from_state <- function(df) {
  if (!"state_code" %in% names(df)) return(df)

  if (!exists("classify_census_region", mode = "function")) {
    source(here::here("R", "region_system_registry.R"))
  }

  bog_region_map <- c(
    ME="Region 1 – New England", NH="Region 1 – New England",
    VT="Region 1 – New England", MA="Region 1 – New England",
    RI="Region 1 – New England", CT="Region 1 – New England",
    NY="Region 2 – Mid-Atlantic", NJ="Region 2 – Mid-Atlantic",
    PA="Region 2 – Mid-Atlantic",
    DE="Region 3 – South Atlantic", MD="Region 3 – South Atlantic",
    DC="Region 3 – South Atlantic", VA="Region 3 – South Atlantic",
    WV="Region 3 – South Atlantic", NC="Region 3 – South Atlantic",
    SC="Region 3 – South Atlantic", GA="Region 3 – South Atlantic",
    FL="Region 3 – South Atlantic",
    OH="Region 4 – Great Lakes", IN="Region 4 – Great Lakes",
    IL="Region 4 – Great Lakes", MI="Region 4 – Great Lakes",
    WI="Region 4 – Great Lakes",
    KY="Region 5 – South Central", TN="Region 5 – South Central",
    AL="Region 5 – South Central", MS="Region 5 – South Central",
    AR="Region 5 – South Central", LA="Region 5 – South Central",
    MN="Region 6 – North Central", IA="Region 6 – North Central",
    MO="Region 6 – North Central", ND="Region 6 – North Central",
    SD="Region 6 – North Central", NE="Region 6 – North Central",
    KS="Region 6 – North Central", WI_dup="Region 6 – North Central",
    OK="Region 7 – Southwest", TX="Region 7 – Southwest",
    NM="Region 7 – Southwest",
    WA="Region 8 – Pacific Coast", OR="Region 8 – Pacific Coast",
    CA="Region 8 – Pacific Coast", AK="Region 8 – Pacific Coast",
    HI="Region 8 – Pacific Coast",
    MT="Region 9 – Mountain West", ID="Region 9 – Mountain West",
    WY="Region 9 – Mountain West", CO="Region 9 – Mountain West",
    AZ="Region 9 – Mountain West", UT="Region 9 – Mountain West",
    NV="Region 9 – Mountain West"
  )

  filled_us  <- 0L
  filled_bog <- 0L

  if ("us_region" %in% names(df)) {
    mask <- is.na(df$us_region) & !is.na(df$state_code)
    if (any(mask)) {
      df$us_region[mask] <- classify_census_region(df$state_code[mask])
      filled_us <- sum(!is.na(df$us_region[mask]))
    }
  }

  if ("bog_region" %in% names(df)) {
    mask <- is.na(df$bog_region) & !is.na(df$state_code)
    if (any(mask)) {
      df$bog_region[mask] <- bog_region_map[df$state_code[mask]]
      filled_bog <- sum(!is.na(df$bog_region[mask]))
    }
  }

  if (filled_us > 0 || filled_bog > 0)
    message(sprintf("fill_regions_from_state: filled %d us_region, %d bog_region.", filled_us, filled_bog))
  df
}

#' Strip numeric renewal-index artifacts from board_names
#'
#' ENThealth stores repeated board certifications with a trailing index like
#' "American Board of Otolaryngology - Head and Neck Surgery - 7".  The
#' number is a scrape artifact; the board name itself is correct.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with \code{board_names} and \code{primary_board} cleaned
#' @keywords internal
fix_board_names_artifacts <- function(df) {
  strip <- function(x) {
    stringr::str_remove_all(x, "\\s*-\\s*\\d+(?=\\s*($|\\|))")
  }
  changed <- 0L
  if ("board_names" %in% names(df)) {
    cleaned <- strip(df$board_names)
    changed <- sum(!is.na(df$board_names) & df$board_names != cleaned, na.rm = TRUE)
    df$board_names <- cleaned
  }
  if ("primary_board" %in% names(df))
    df$primary_board <- strip(df$primary_board)
  if ("board_cert" %in% names(df))
    df$board_cert <- strip(df$board_cert)
  if (changed > 0)
    message(sprintf("fix_board_names_artifacts: cleaned index artifact from %d board_names value(s).", changed))
  df
}

#' Extract credentials from full_name for records missing the credentials column
#'
#' When the ENThealth parser could not isolate credentials into their own column
#' (e.g. "Aida Nourbakhsh, M.D., Ph.D." or "Ahmad Al-Omari, MBBS"), this
#' function extracts recognized degree tokens from \code{full_name} and
#' populates \code{credentials}, \code{degree_type}, \code{is_md}, and
#' \code{is_do} for those rows.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with credentials-derived columns filled where previously
#'   NA
#' @keywords internal
fill_credentials_from_name <- function(df) {
  if (!all(c("full_name", "credentials") %in% names(df))) return(df)

  # Degree patterns to extract, in priority order
  degree_map <- list(
    "MD PHD"  = c("M\\.D\\..*Ph\\.D\\.", "MD.*PhD", "MD/PhD", "M\\.D\\.,?\\s*Ph\\.D\\."),
    "MD"      = c("\\bM\\.D\\.\\b", "\\bMD\\b"),
    "DO"      = c("\\bD\\.O\\.?\\b"),
    "MBBS"    = c("\\bMBBS\\b", "\\bMB\\s*BCh\\b", "\\bMB\\s*ChB\\b", "\\bMB\\s*BCh\\s*BAO\\b"),
    "MPAS"    = c("\\bMPAS\\b"),
    "DDS"     = c("\\bDDS\\b"),
    "DMD"     = c("\\bDMD\\b")
  )

  rows_filled <- 0L
  missing_mask <- is.na(df$credentials)

  for (i in which(missing_mask)) {
    nm <- df$full_name[i]
    if (is.na(nm)) next
    extracted <- NA_character_
    for (deg in names(degree_map)) {
      if (any(sapply(degree_map[[deg]], function(p) grepl(p, nm, ignore.case = TRUE, perl = TRUE)))) {
        extracted <- deg
        break
      }
    }
    if (!is.na(extracted)) {
      df$credentials[i] <- extracted
      df$degree_type[i] <- dplyr::case_when(
        extracted == "MD PHD" ~ "MD",
        extracted == "MD"     ~ "MD",
        extracted == "DO"     ~ "DO",
        TRUE                  ~ "Other"
      )
      df$is_md[i] <- extracted %in% c("MD", "MD PHD")
      df$is_do[i] <- extracted == "DO"
      rows_filled <- rows_filled + 1L
    }
  }

  if (rows_filled > 0)
    message(sprintf("fill_credentials_from_name: filled credentials for %d record(s).", rows_filled))
  df
}

#' Recalculate board_cert_lapsed using has_abohns as ground truth
#'
#' The original \code{board_cert_lapsed} flag was set by a simple
#' \code{years_since_board_cert >= 10} rule, which marks 4,757 actively
#' ABOHNS-certified physicians as "lapsed".  The correct definition is:
#' a certification is lapsed when the physician has cert data
#' (\code{n_board_certs > 0}) but \code{has_abohns = FALSE} (no current
#' ABOHNS certification confirmed on ABOHNS.org).
#'
#' New values:
#' \itemize{
#'   \item \code{FALSE} — \code{has_abohns = TRUE} (confirmed current)
#'   \item \code{TRUE}  — has cert data but \code{has_abohns = FALSE}
#'   \item \code{NA}    — no board cert data at all
#' }
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with recalculated \code{board_cert_lapsed}
#' @keywords internal
recalculate_board_cert_lapsed <- function(df) {
  needed <- c("has_abohns", "n_board_certs", "board_cert_lapsed")
  if (!all(needed %in% names(df))) return(df)

  old_true  <- sum(df$board_cert_lapsed == TRUE,  na.rm = TRUE)
  old_false <- sum(df$board_cert_lapsed == FALSE, na.rm = TRUE)

  df$board_cert_lapsed <- dplyr::case_when(
    df$has_abohns  == TRUE              ~ FALSE,
    df$n_board_certs > 0 & !df$has_abohns ~ TRUE,
    TRUE                                ~ NA
  )

  new_true  <- sum(df$board_cert_lapsed == TRUE,  na.rm = TRUE)
  new_false <- sum(df$board_cert_lapsed == FALSE, na.rm = TRUE)

  message(sprintf(
    "recalculate_board_cert_lapsed: lapsed TRUE %d->%d, FALSE %d->%d.",
    old_true, new_true, old_false, new_false
  ))
  df
}

#' Run all roster cleaning steps in sequence
#'
#' Convenience wrapper that applies every cleaning function above.
#' Safe to call multiple times (idempotent).
#'
#' @param df data.frame  ENT physician roster
#' @return Cleaned data.frame
#' @keywords internal
clean_ent_roster <- function(df) {
  df |>
    remove_retired()                |>
    normalize_credentials()         |>
    fix_zip5()                      |>
    fix_zip4()                      |>
    fix_phone_digits()              |>
    fix_future_years()              |>
    fix_name_whitespace()           |>
    fix_address_artifact()          |>
    fix_address_semicolons()        |>
    fix_city()                      |>
    fix_company_name_artifacts()    |>
    fix_last_name_suffix()          |>
    fill_regions_from_state()       |>
    fix_board_names_artifacts()     |>
    fill_credentials_from_name()    |>
    fill_boolean_flags_from_credentials() |>
    fill_specialty_from_board()     |>
    flag_po_box_address()                |>
    supplement_po_box_from_nppes()       |>
    supplement_company_name_from_dac()   |>
    supplement_company_name_from_nppes() |>
    recalculate_board_cert_lapsed()      |>
    recalculate_data_completeness()      |>
    flag_board_cert_conflicts()
}

#' Fill missing company_name from the CMS DAC (Doctors and Clinicians) file
#'
#' The DAC national downloadable file contains a \code{Facility Name} column
#' (the physician's enrolled group practice) for ~18,000 ENT rows.  This
#' function:
#' \enumerate{
#'   \item Loads the DAC file and filters to otolaryngology rows.
#'   \item Joins to \code{aao_npi_matched.csv} to obtain NPIs for roster
#' records.
#'   \item Matches roster records missing \code{company_name} by first + last
#' name
#'         through the NPI bridge.
#'   \item Fills \code{company_name} with the title-cased DAC Facility Name.
#'   \item Sets \code{company_name_source = "DAC"} for filled rows;
#'         existing values retain \code{"ENThealth"}.
#' }
#' Resolves ~428 of the 1,208 missing company_name records.
#'
#' @param df       data.frame  ENT physician roster
#' @param dac_path character   Path to \code{DAC_NationalDownloadableFile.csv}
#' @param aao_path character   Path to \code{aao_npi_matched.csv}
#' @return data.frame with \code{company_name} and \code{company_name_source}
#'   filled
#' @keywords internal
supplement_company_name_from_dac <- function(
    df,
    dac_path = resolve_external_path("nppes_historical_downloads/DAC_NationalDownloadableFile.csv"),
    aao_path = here::here("data/ent/aao_npi_matched.csv")
) {
  needed <- c("company_name", "first_name", "last_name")
  if (!all(needed %in% names(df))) return(df)
  if (!file.exists(dac_path)) {
    message("supplement_company_name_from_dac: DAC file not found at ", dac_path, " — skipping.")
    return(df)
  }
  if (!file.exists(aao_path)) {
    message("supplement_company_name_from_dac: AAO file not found — skipping.")
    return(df)
  }

  # UNICODE (2026-08-10): str_to_upper + str_squish preserves accents rather
  # than transliterating, so accented names never matched. Canonical normalizer.
  norm_name <- function(x) normalize_name_key(x)

  # ── Load and filter DAC to otolaryngology ──────────────────────────────────
  message("supplement_company_name_from_dac: loading DAC file...")
  dac <- readr::read_csv(dac_path, col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE) |>
    dplyr::filter(stringr::str_detect(pri_spec, stringr::fixed("Otolaryngology", ignore_case = TRUE))) |>
    dplyr::filter(!is.na(`Facility Name`)) |>
    dplyr::distinct(NPI, .keep_all = TRUE) |>
    dplyr::select(NPI, facility_name_dac = `Facility Name`)

  # ── Build NPI bridge from AAO file ────────────────────────────────────────
  aao <- readr::read_csv(aao_path, col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE) |>
    dplyr::mutate(
      NPI = stringr::str_remove(as.character(npi), "\\.0$"),
      .fn = norm_name(first_name),
      .ln = norm_name(last_name)
    ) |>
    dplyr::distinct(.fn, .ln, .keep_all = TRUE) |>
    dplyr::select(.fn, .ln, NPI)

  # ── Join: AAO -> DAC to get facility name per first+last ──────────────────
  name_to_facility <- aao |>
    dplyr::inner_join(dac, by = "NPI") |>
    dplyr::distinct(.fn, .ln, .keep_all = TRUE)

  # ── Add company_name_source tracking column ───────────────────────────────
  if (!"company_name_source" %in% names(df))
    df$company_name_source <- dplyr::if_else(!is.na(df$company_name), "ENThealth", NA_character_)

  # ── Fill missing company_name ──────────────────────────────────────────────
  df <- df |>
    dplyr::mutate(.fn = norm_name(first_name), .ln = norm_name(last_name)) |>
    dplyr::left_join(name_to_facility, by = c(".fn", ".ln")) |>
    dplyr::mutate(
      company_name_source = dplyr::case_when(
        is.na(company_name) & !is.na(facility_name_dac) ~ "DAC",
        TRUE                                             ~ company_name_source
      ),
      company_name = dplyr::coalesce(
        company_name,
        stringr::str_to_title(facility_name_dac)
      )
    ) |>
    dplyr::select(-facility_name_dac, -.fn, -.ln)

  filled <- sum(df$company_name_source == "DAC", na.rm = TRUE)
  message(sprintf(
    "supplement_company_name_from_dac: filled company_name for %d record(s).", filled
  ))
  df
}

#' Fill remaining missing company_name from the AAO bridge and NPPES flat file
#'
#' After the DAC pass, ~779 records still lack \code{company_name}.  This
#' function uses two sources matched by first + last name:
#' \enumerate{
#'   \item \strong{\code{aao_npi_matched.csv} \code{company} column} — the
#'         organisation name self-reported to ENThealth.org.  This file is
#'         already on-disk and loads in < 1 sec.  Covers ~92 \% of physicians.
#'   \item \strong{NPPES npidata flat file} — falls back to
#'         \code{Parent Organization LBN} (col 310) for remaining gaps.
#'         Loading this 7 GB file takes ~10 min on an external drive; pass
#'         \code{nppes_path = NULL} to skip this tier.
#' }
#' Fills \code{company_name} for rows where it is still \code{NA} and sets
#' \code{company_name_source} to \code{"AAO"} or \code{"NPPES"} accordingly.
#'
#' @param df         data.frame  ENT physician roster
#' @param aao_path   character   Path to \code{aao_npi_matched.csv}
#' @param nppes_path character   Path to NPPES npidata CSV; \code{NULL} to skip
#' @return data.frame with \code{company_name} and \code{company_name_source}
#'   updated
#' @keywords internal
supplement_company_name_from_nppes <- function(
    df,
    aao_path   = here::here("data/ent/aao_npi_matched.csv"),
    nppes_path = resolve_external_path("nppes_historical_downloads/NPPES_Data_Dissemination_October_2020/npidata_pfile_20050523-20201011.csv")
) {
  needed <- c("company_name", "first_name", "last_name")
  if (!all(needed %in% names(df))) return(df)
  if (!file.exists(aao_path)) {
    message("supplement_company_name_from_nppes: AAO NPI bridge not found — skipping.")
    return(df)
  }

  # UNICODE (2026-08-10): str_to_upper + str_squish preserves accents rather
  # than transliterating, so accented names never matched. Canonical normalizer.
  norm_name <- function(x) normalize_name_key(x)

  # ── Ensure tracking column exists ─────────────────────────────────────────
  if (!"company_name_source" %in% names(df))
    df$company_name_source <- dplyr::if_else(!is.na(df$company_name), "ENThealth", NA_character_)

  # ── TIER 1: AAO bridge company column (fast, ~7k rows) ────────────────────
  aao <- readr::read_csv(aao_path, col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE) |>
    dplyr::filter(!is.na(company) & nchar(stringr::str_squish(company)) > 0) |>
    dplyr::mutate(
      .fn      = norm_name(first_name),
      .ln      = norm_name(last_name),
      aao_company = stringr::str_squish(company)
    ) |>
    dplyr::distinct(.fn, .ln, .keep_all = TRUE) |>
    dplyr::select(.fn, .ln, aao_company)

  df <- df |>
    dplyr::mutate(.fn = norm_name(first_name), .ln = norm_name(last_name)) |>
    dplyr::left_join(aao, by = c(".fn", ".ln")) |>
    dplyr::mutate(
      company_name_source = dplyr::case_when(
        is.na(company_name) & !is.na(aao_company) ~ "AAO",
        TRUE                                       ~ company_name_source
      ),
      company_name = dplyr::coalesce(company_name, stringr::str_to_title(aao_company))
    ) |>
    dplyr::select(-aao_company, -.fn, -.ln)

  filled_aao <- sum(df$company_name_source == "AAO", na.rm = TRUE)
  message(sprintf(
    "supplement_company_name_from_nppes: tier 1 (AAO) filled %d record(s).", filled_aao
  ))

  # ── TIER 2: NPPES flat file Parent Organization LBN (slow, ~7 GB) ─────────
  still_missing <- sum(is.na(df$company_name))
  if (still_missing > 0 && !is.null(nppes_path) && file.exists(nppes_path)) {
    aao_npi <- readr::read_csv(aao_path, col_types = readr::cols(.default = "c"),
                               show_col_types = FALSE) |>
      dplyr::mutate(
        NPI = stringr::str_remove(as.character(npi), "\\.0$"),
        .fn = norm_name(first_name),
        .ln = norm_name(last_name)
      ) |>
      dplyr::distinct(.fn, .ln, .keep_all = TRUE) |>
      dplyr::select(.fn, .ln, NPI)

    sel_cols <- c("NPI",
                  "Provider Organization Name (Legal Business Name)",
                  "Parent Organization LBN")
    message(sprintf(
      "supplement_company_name_from_nppes: tier 2 — loading NPPES flat file for %d remaining record(s)...",
      still_missing
    ))
    nppes_org_tbl <- vroom::vroom(
      nppes_path,
      col_select     = dplyr::all_of(sel_cols),
      col_types      = vroom::cols(.default = vroom::col_character()),
      show_col_types = FALSE,
      progress       = FALSE
    ) |>
      dplyr::filter(NPI %in% aao_npi$NPI) |>
      dplyr::mutate(
        nppes_org = dplyr::coalesce(
          dplyr::na_if(stringr::str_squish(`Parent Organization LBN`), ""),
          dplyr::na_if(stringr::str_squish(`Provider Organization Name (Legal Business Name)`), "")
        )
      ) |>
      dplyr::filter(!is.na(nppes_org)) |>
      dplyr::distinct(NPI, .keep_all = TRUE) |>
      dplyr::select(NPI, nppes_org)

    name_to_org <- aao_npi |>
      dplyr::inner_join(nppes_org_tbl, by = "NPI") |>
      dplyr::distinct(.fn, .ln, .keep_all = TRUE)

    df <- df |>
      dplyr::mutate(.fn = norm_name(first_name), .ln = norm_name(last_name)) |>
      dplyr::left_join(name_to_org, by = c(".fn", ".ln")) |>
      dplyr::mutate(
        company_name_source = dplyr::case_when(
          is.na(company_name) & !is.na(nppes_org) ~ "NPPES",
          TRUE                                     ~ company_name_source
        ),
        company_name = dplyr::coalesce(company_name, stringr::str_to_title(nppes_org))
      ) |>
      dplyr::select(-nppes_org, -.fn, -.ln)

    filled_nppes <- sum(df$company_name_source == "NPPES", na.rm = TRUE)
    message(sprintf(
      "supplement_company_name_from_nppes: tier 2 (NPPES) filled %d additional record(s).", filled_nppes
    ))
  } else if (still_missing > 0) {
    message(sprintf(
      "supplement_company_name_from_nppes: %d record(s) still missing (NPPES tier skipped).",
      still_missing
    ))
  }

  message(sprintf(
    "supplement_company_name_from_nppes: total filled %d; still missing %d.",
    filled_aao + sum(df$company_name_source == "NPPES", na.rm = TRUE),
    sum(is.na(df$company_name))
  ))
  df
}

#' Replace PO Box addresses with physical practice addresses from NPPES
#'
#' For the 151 PO Box addresses, looks up a physical practice location from
#' two on-disk sources matched by first + last name:
#' \enumerate{
#'   \item \code{data/ent/aao_npi_matched.csv} — resolves ~103 records
#'   \item \code{data/ent/enthealth_npi_LATEST.csv} — resolves ~1 more
#' }
#' Only substitutes when the replacement address is itself NOT a PO Box.
#' Sets \code{address_source} to \code{"NPPES"} or \code{"ENThealth_NPI"}
#' for substituted rows; original rows stay \code{"ENThealth"}.
#' \code{is_po_box} is updated to \code{FALSE} for resolved records.
#'
#' For the ~47 records that remain unresolved, use
#' \code{supplement_po_box_from_nppes_api()} (requires internet access).
#'
#' @param df        data.frame  ENT physician roster (must have
#'   \code{is_po_box})
#' @param aao_path  character   Path to \code{aao_npi_matched.csv}
#' @param enh_path  character   Path to \code{enthealth_npi_LATEST.csv}
#' @return data.frame with PO Box addresses replaced where possible and an
#' @keywords internal
#'         \code{address_source} column added
supplement_po_box_from_nppes <- function(
    df,
    aao_path = here::here("data/ent/aao_npi_matched.csv"),
    enh_path = here::here("data/ent/enthealth_npi_LATEST.csv")
) {
  if (!"is_po_box" %in% names(df)) return(df)
  if (!any(df$is_po_box, na.rm = TRUE)) return(df)

  po_re <- "P\\.?\\s*O\\.?\\s*[Bb]ox|PO\\s+Box"

  # UNICODE (2026-08-10): str_to_upper + str_squish preserves accents rather
  # than transliterating, so accented names never matched. Canonical normalizer.
  norm_name <- function(x) normalize_name_key(x)

  if (!"address_source" %in% names(df))
    df$address_source <- "ENThealth"

  df <- df |>
    dplyr::mutate(.fn = norm_name(first_name), .ln = norm_name(last_name))

  resolved <- 0L

  # ── Source 1: aao_npi_matched ──────────────────────────────────────────────
  if (file.exists(aao_path)) {
    aao <- readr::read_csv(aao_path, show_col_types = FALSE) |>
      dplyr::mutate(.fn = norm_name(first_name), .ln = norm_name(last_name)) |>
      dplyr::distinct(.fn, .ln, .keep_all = TRUE)

    for (i in which(df$is_po_box)) {
      fn <- df$.fn[i]; ln <- df$.ln[i]
      hit <- dplyr::filter(aao, .fn == fn, .ln == ln)
      if (nrow(hit) == 0) next
      addr <- hit$address_nppes[1]
      if (is.na(addr) || grepl(po_re, addr, ignore.case = TRUE)) next

      df$address[i]        <- stringr::str_to_title(addr)
      df$city[i]           <- stringr::str_to_title(hit$city_nppes[1])
      df$state_code[i]     <- hit$state_nppes[1]
      raw_zip              <- as.character(hit$zip_nppes[1])
      .z_int <- suppressWarnings(as.integer(as.numeric(raw_zip)))
      df$zip5[i]           <- if (is.na(.z_int)) NA_character_ else sprintf("%05d", .z_int)
      df$is_po_box[i]      <- FALSE
      df$address_source[i] <- "NPPES"
      resolved             <- resolved + 1L
    }
  }

  # ── Source 2: enthealth_npi_LATEST (for remaining PO boxes) ────────────────
  if (file.exists(enh_path)) {
    enh <- readr::read_csv(enh_path, show_col_types = FALSE) |>
      dplyr::mutate(
        .fn = norm_name(first_clean),
        .ln = norm_name(last_clean)
      ) |>
      dplyr::distinct(.fn, .ln, .keep_all = TRUE)

    for (i in which(df$is_po_box)) {
      fn <- df$.fn[i]; ln <- df$.ln[i]
      hit <- dplyr::filter(enh, .fn == fn, .ln == ln)
      if (nrow(hit) == 0) next
      addr <- hit$address[1]
      if (is.na(addr) || grepl(po_re, addr, ignore.case = TRUE)) next

      df$address[i]        <- addr
      df$city[i]           <- hit$city[1]
      df$state_code[i]     <- hit$state[1]
      if (!is.na(hit$zip5[1]))
        .z_int <- suppressWarnings(as.integer(as.numeric(hit$zip5[1])))
        df$zip5[i]         <- if (is.na(.z_int)) NA_character_ else sprintf("%05d", .z_int)
      df$is_po_box[i]      <- FALSE
      df$address_source[i] <- "ENThealth_NPI"
      resolved             <- resolved + 1L
    }
  }

  df <- dplyr::select(df, -.fn, -.ln)

  message(sprintf(
    "supplement_po_box_from_nppes: replaced %d PO Box address(es); %d remain.",
    resolved, sum(df$is_po_box, na.rm = TRUE)
  ))
  df
}

#' Resolve remaining PO Box addresses via the NPPES public API
#'
#' Called after \code{supplement_po_box_from_nppes()} to handle the ~47
#' records not covered by on-disk files.  For each remaining PO Box row,
#' queries the CMS NPPES Registry API
#' (\url{https://npiregistry.cms.hhs.gov/api/}) with a three-tier strategy:
#' \enumerate{
#'   \item \code{first_name + last_name + state} (exact)
#'   \item \code{first_name + last_name} (no state — handles moves/PO Box state)
#'   \item \code{last_name + state} only — for records where first_name is a
#'         single initial (e.g. "F. Thomas Sporck")
#' }
#' Prefers the \code{LOCATION} address over \code{MAILING}; skips any result
#' that is itself a PO Box.  Respects a \code{rate_limit_s} pause between
#' calls to avoid hammering the public API.
#'
#' @param df            data.frame  ENT physician roster
#' @param rate_limit_s  numeric     Seconds to wait between API calls (default
#'   0.5)
#' @param verbose       logical     Print per-record resolution status (default
#'   TRUE)
#' @return data.frame with resolved addresses and \code{address_source =
#'   "NPPES_API"}
#' @keywords internal
supplement_po_box_from_nppes_api <- function(df, rate_limit_s = 0.5, verbose = TRUE) {
  if (!requireNamespace("httr",    quietly = TRUE)) stop("Package 'httr' is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
  if (!"is_po_box" %in% names(df)) return(df)

  remaining <- which(df$is_po_box)
  if (length(remaining) == 0) return(df)

  po_re      <- "P\\.?\\s*O\\.?\\s*[Bb]ox|PO\\s+Box"
  api_base   <- "https://npiregistry.cms.hhs.gov/api/"
  resolved   <- 0L
  not_found  <- character(0)

  # Extract best (non-PO-Box LOCATION preferred) address from API results
  best_address <- function(results) {
    for (r in results) {
      addrs <- r$addresses
      # Prefer LOCATION over MAILING
      for (purpose in c("LOCATION", "MAILING")) {
        for (a in addrs) {
          if (!identical(a$address_purpose, purpose)) next
          if (is.null(a$address_1) || grepl(po_re, a$address_1, ignore.case = TRUE)) next
          zip_raw <- as.character(a$postal_code %||% "")
          zip5 <- if (nchar(zip_raw) >= 5) substr(zip_raw, 1, 5) else zip_raw
          return(list(
            address    = stringr::str_to_title(a$address_1),
            city       = stringr::str_to_title(a$city %||% NA_character_),
            state_code = a$state %||% NA_character_,
            zip5       = zip5
          ))
        }
      }
    }
    NULL
  }

  query_api <- function(params) {
    params$version         <- "2.1"
    params$enumeration_type <- "NPI-1"
    params$limit           <- 10L
    resp <- tryCatch(
      httr::GET(api_base, query = params, httr::timeout(10)),
      error = function(e) NULL
    )
    if (is.null(resp) || httr::http_error(resp)) return(NULL)
    parsed <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                         simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed) || is.null(parsed$result_count) || parsed$result_count == 0)
      return(NULL)
    parsed$results
  }

  for (i in remaining) {
    fn    <- trimws(as.character(df$first_name[i]))
    ln    <- trimws(as.character(df$last_name[i]))
    state <- trimws(as.character(df$state_code[i]))
    is_initial <- grepl("^[A-Z]\\.?$", fn)

    result <- NULL

    # Tier 1: first + last + state (skip for initials)
    if (!is_initial && !is.na(state) && nchar(state) == 2) {
      result <- query_api(list(first_name = fn, last_name = ln, state = state))
      Sys.sleep(rate_limit_s)
    }

    # Tier 2: first + last, no state filter
    if (is.null(result) && !is_initial) {
      result <- query_api(list(first_name = fn, last_name = ln))
      Sys.sleep(rate_limit_s)
    }

    # Tier 3: last name + state only (for initial first names or no match above)
    if (is.null(result) && !is.na(state) && nchar(state) == 2) {
      result <- query_api(list(last_name = ln, state = state))
      Sys.sleep(rate_limit_s)
    }

    addr <- if (!is.null(result)) best_address(result) else NULL

    if (!is.null(addr)) {
      df$address[i]        <- addr$address
      df$city[i]           <- addr$city
      df$state_code[i]     <- addr$state_code
      df$zip5[i]           <- addr$zip5
      df$is_po_box[i]      <- FALSE
      df$address_source[i] <- "NPPES_API"
      resolved             <- resolved + 1L
      if (verbose) message(sprintf("  [RESOLVED] %s -> %s, %s",
        df$full_name[i], addr$address, addr$city))
    } else {
      not_found <- c(not_found, df$full_name[i])
      if (verbose) message(sprintf("  [NOT FOUND] %s", df$full_name[i]))
    }
  }

  message(sprintf(
    "supplement_po_box_from_nppes_api: resolved %d; %d still unresolved.",
    resolved, length(not_found)
  ))
  df
}

# ── Derived boolean flags ─────────────────────────────────────────────────────

#' Fill is_phd and is_facs from credentials where currently NA
#'
#' After \code{fill_credentials_from_name()} populates credentials for 18
#' previously-blank rows, \code{is_phd} and \code{is_facs} remain NA for
#' those rows.  This function derives them from the now-populated credentials.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with \code{is_phd} and \code{is_facs} filled
#' @keywords internal
fill_boolean_flags_from_credentials <- function(df) {
  needed <- c("credentials", "is_phd", "is_facs")
  if (!all(needed %in% names(df))) return(df)

  mask <- !is.na(df$credentials) & (is.na(df$is_phd) | is.na(df$is_facs))
  if (!any(mask)) return(df)

  df$is_phd[mask] <- stringr::str_detect(
    df$credentials[mask], stringr::regex("PHD|PH\\.D", ignore_case = TRUE)
  )
  df$is_facs[mask] <- stringr::str_detect(
    df$credentials[mask], stringr::regex("FACS", ignore_case = TRUE)
  )

  message(sprintf(
    "fill_boolean_flags_from_credentials: filled is_phd/is_facs for %d record(s).", sum(mask)
  ))
  df
}

#' Infer specialty_primary from board_names when specialty is missing
#'
#' 216 physicians have an ABOHNS certification but no ENThealth specialty tag.
#' Board name maps to a canonical specialty:
#' \itemize{
#'   \item ABOHNS only           -> General Otolaryngology
#'   \item ABOHNS + Sleep Medicine board -> Sleep Medicine
#' }
#' \code{specialty_inferred_source} is set to \code{"board_names"} for these rows.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with \code{specialty_primary} and
#' @keywords internal
#'         \code{specialty_inferred_source} filled where inferred
fill_specialty_from_board <- function(df) {
  needed <- c("specialty_primary", "board_names")
  if (!all(needed %in% names(df))) return(df)

  missing_sp <- is.na(df$specialty_primary)
  filled <- 0L

  # ABOHNS + Sleep Medicine board -> Sleep Medicine
  sleep_mask <- missing_sp &
    stringr::str_detect(df$board_names, stringr::fixed("Sleep Medicine", ignore_case = TRUE))
  if (any(sleep_mask, na.rm = TRUE)) {
    df$specialty_primary[sleep_mask] <- "Sleep Medicine"
    if ("specialty_inferred_source" %in% names(df))
      df$specialty_inferred_source[sleep_mask] <- "board_names"
    filled <- filled + sum(sleep_mask, na.rm = TRUE)
  }

  # ABOHNS general only -> General Otolaryngology
  abohns_mask <- missing_sp & !sleep_mask &
    stringr::str_detect(df$board_names,
      stringr::fixed("American Board of Otolaryngology", ignore_case = TRUE))
  if (any(abohns_mask, na.rm = TRUE)) {
    df$specialty_primary[abohns_mask] <- "General Otolaryngology"
    if ("specialty_inferred_source" %in% names(df))
      df$specialty_inferred_source[abohns_mask] <- "board_names"
    filled <- filled + sum(abohns_mask, na.rm = TRUE)
  }

  if (filled > 0)
    message(sprintf("fill_specialty_from_board: inferred specialty_primary for %d record(s).", filled))
  df
}

#' Flag PO Box addresses
#'
#' 150 physicians list a PO Box as their address.  This is a mailing address,
#' not a practice location.  Adds a logical \code{is_po_box} column so callers
#' know the address cannot be used for navigation or geocoding.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with \code{is_po_box} logical column added
#' @keywords internal
flag_po_box_address <- function(df) {
  if (!"address" %in% names(df)) return(df)

  po_pattern <- stringr::regex("P\\.?\\s*O\\.?\\s*Box|PO\\s+Box", ignore_case = TRUE)
  df$is_po_box <- !is.na(df$address) & stringr::str_detect(df$address, po_pattern)

  n <- sum(df$is_po_box)
  if (n > 0)
    message(sprintf("flag_po_box_address: flagged %d PO Box address(es).", n))
  df
}

#' Recalculate data_completeness from current field values
#'
#' The original score was computed at scrape time.  After cleaning, 2,982 rows
#' have a different score.  Recalculates from the same 10 core fields:
#' full_name, address, city, state_code, zip5, phone_number,
#' specialty_primary, medical_school_name, residency_name, board_cert.
#'
#' @param df data.frame  ENT physician roster
#' @return data.frame with \code{data_completeness} updated
#' @keywords internal
recalculate_data_completeness <- function(df) {
  score_fields <- c("full_name", "address", "city", "state_code", "zip5",
                    "phone_number", "specialty_primary", "medical_school_name",
                    "residency_name", "board_cert")
  present <- intersect(score_fields, names(df))
  if (length(present) == 0) return(df)

  old_mean <- mean(df$data_completeness, na.rm = TRUE)
  df$data_completeness <- rowSums(!is.na(df[present]))
  new_mean <- mean(df$data_completeness, na.rm = TRUE)

  message(sprintf(
    "recalculate_data_completeness: mean score %.3f -> %.3f.", old_mean, new_mean
  ))
  df
}
