# =============================================================================
# ENT registry validity figures
#
# Creates seven publication-ready figures for the OTO-HNS manuscript:
#   1. Registry overlap UpSet-style figure
#   2. Registry credibility forest plot
#   3. Listing validation journey
#   4. Reasons for invalid listings
#   5. NPPES record staleness curve
#   6. National listing-validity map
#   7. Rural versus nonrural dumbbell plot
#
# The script accepts a physician-level listing table and a population-count
# table. It standardizes common variable names, logs every transformation,
# and saves timestamped PNG and PDF files.
# =============================================================================

# -----------------------------------------------------------------------------
# Package checks
# -----------------------------------------------------------------------------

#' Check packages needed for the registry figures
#'
#' @return Invisibly returns TRUE.
check_registry_figure_packages <- function() {
  required_packages <- c(
    "dplyr",
    "forcats",
    "ggplot2",
    "maps",
    "patchwork",
    "purrr",
    "scales",
    "stringr",
    "survey",
    "tibble",
    "tidyr"
  )

  missing_packages <- required_packages[
    !base::vapply(
      required_packages,
      base::requireNamespace,
      quietly = TRUE,
      FUN.VALUE = base::logical(1)
    )
  ]

  if (base::length(missing_packages) > 0L) {
    base::stop(
      "Install these packages before running the script: ",
      base::paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  base::message(
    "Package check complete: ",
    base::length(required_packages),
    " packages available."
  )

  base::invisible(TRUE)
}


# -----------------------------------------------------------------------------
# General helpers
# -----------------------------------------------------------------------------

#' Return the first available column name
#'
#' @param listing_tbl A table.
#' @param candidates Candidate column names.
#'
#' @return A single column name or NA_character_.
first_existing_name <- function(listing_tbl, candidates) {
  available_names <- candidates[candidates %in% base::names(listing_tbl)]

  if (base::length(available_names) == 0L) {
    return(NA_character_)
  }

  available_names[[1L]]
}


#' Pull the first available column or return a default vector
#'
#' @param listing_tbl A table.
#' @param candidates Candidate column names.
#' @param default_value Scalar default value.
#'
#' @return A vector with one value per row.
pull_column_or <- function(
    listing_tbl,
    candidates,
    default_value = NA_character_) {
  chosen_name <- first_existing_name(listing_tbl, candidates)

  if (base::is.na(chosen_name)) {
    return(base::rep(default_value, base::nrow(listing_tbl)))
  }

  listing_tbl[[chosen_name]]
}


#' Convert common yes/no fields to logical values
#'
#' @param value_vec A vector containing logical-like values.
#'
#' @return A logical vector.
as_logical_flag <- function(value_vec) {
  if (base::is.logical(value_vec)) {
    return(value_vec)
  }

  if (base::is.numeric(value_vec)) {
    return(
      dplyr::case_when(
        base::is.na(value_vec) ~ NA,
        value_vec == 1 ~ TRUE,
        value_vec == 0 ~ FALSE,
        TRUE ~ NA
      )
    )
  }

  clean_value <- stringr::str_to_lower(
    stringr::str_squish(base::as.character(value_vec))
  )

  dplyr::case_when(
    clean_value %in% c(
      "yes",
      "y",
      "true",
      "t",
      "1",
      "accepted",
      "active",
      "valid",
      "confirmed_valid"
    ) ~ TRUE,
    clean_value %in% c(
      "no",
      "n",
      "false",
      "f",
      "0",
      "declined",
      "inactive",
      "invalid",
      "confirmed_invalid"
    ) ~ FALSE,
    TRUE ~ NA
  )
}


#' Normalize overlap-zone labels
#'
#' @param zone_vec Registry-overlap labels.
#'
#' @return An ordered factor.
normalize_overlap_zone <- function(zone_vec) {
  clean_zone <- stringr::str_to_lower(
    stringr::str_squish(base::as.character(zone_vec))
  )

  normalized_zone <- dplyr::case_when(
    stringr::str_detect(
      clean_zone,
      "triple|all three|nppes.*abo.*ent|abo.*npi.*ent"
    ) ~ "Triple match",
    stringr::str_detect(
      clean_zone,
      "nppes.*ent|npi.*ent"
    ) &
      !stringr::str_detect(clean_zone, "abo|board") ~
      "NPPES + ENTHealth",
    stringr::str_detect(
      clean_zone,
      "nppes.*abo|abo.*npi|board.*npi"
    ) &
      !stringr::str_detect(clean_zone, "enthealth|ent health") ~
      "NPPES + ABOto",
    stringr::str_detect(
      clean_zone,
      "nppes only|npi only|nppes-only|npi-only"
    ) ~ "NPPES only",
    clean_zone %in% c("nppes", "npi") ~ "NPPES only",
    base::is.na(clean_zone) | clean_zone == "" ~ NA_character_,
    TRUE ~ stringr::str_to_title(clean_zone)
  )

  base::factor(
    normalized_zone,
    levels = c(
      "NPPES only",
      "NPPES + ABOto",
      "NPPES + ENTHealth",
      "Triple match"
    ),
    ordered = TRUE
  )
}


#' Normalize rurality labels
#'
#' @param rural_vec Rurality values or RUCA codes.
#'
#' @return A factor with Nonrural and Rural levels.
normalize_rurality <- function(rural_vec) {
  if (base::is.numeric(rural_vec)) {
    rural_label <- dplyr::case_when(
      base::is.na(rural_vec) ~ NA_character_,
      rural_vec >= 7 ~ "Rural",
      TRUE ~ "Nonrural"
    )
  } else {
    clean_rural <- stringr::str_to_lower(
      stringr::str_squish(base::as.character(rural_vec))
    )

    rural_label <- dplyr::case_when(
      clean_rural %in% c(
        "rural",
        "yes",
        "y",
        "true",
        "1"
      ) ~ "Rural",
      clean_rural %in% c(
        "nonrural",
        "non-rural",
        "urban",
        "no",
        "n",
        "false",
        "0"
      ) ~ "Nonrural",
      base::suppressWarnings(
        base::as.numeric(clean_rural)
      ) >= 7 ~ "Rural",
      base::suppressWarnings(
        base::as.numeric(clean_rural)
      ) < 7 ~ "Nonrural",
      TRUE ~ NA_character_
    )
  }

  base::factor(
    rural_label,
    levels = c("Nonrural", "Rural")
  )
}


#' Convert state abbreviations or names to full state names
#'
#' @param state_vec State abbreviations or full names.
#'
#' @return Lowercase state names compatible with maps::map_data().
normalize_state_name <- function(state_vec) {
  clean_state <- stringr::str_to_upper(
    stringr::str_squish(base::as.character(state_vec))
  )

  state_lookup <- tibble::tibble(
    state_abbr = base::c(datasets::state.abb, "DC"),
    state_name = base::c(datasets::state.name, "District of Columbia")
  )

  matched_state <- state_lookup$state_name[
    base::match(clean_state, state_lookup$state_abbr)
  ]

  full_name_match <- state_lookup$state_name[
    base::match(
      stringr::str_to_title(clean_state),
      state_lookup$state_name
    )
  ]

  normalized_name <- dplyr::coalesce(
    matched_state,
    full_name_match
  )

  stringr::str_to_lower(normalized_name)
}


#' Derive Census region from a full state name
#'
#' @param state_name_vec Lowercase full state names.
#'
#' @return Census region labels.
derive_census_region <- function(state_name_vec) {
  title_names <- stringr::str_to_title(state_name_vec)

  region_lookup <- tibble::tibble(
    state_name = datasets::state.name,
    census_region = datasets::state.region
  )

  region_vec <- region_lookup$census_region[
    base::match(title_names, region_lookup$state_name)
  ]

  dplyr::case_when(
    stringr::str_to_lower(title_names) ==
      "district of columbia" ~ "South",
    TRUE ~ base::as.character(region_vec)
  )
}


#' Format a P value for manuscript text
#'
#' @param p_value Numeric P value.
#'
#' @return A formatted character string.
format_p_value <- function(p_value) {
  if (base::is.na(p_value)) {
    return("= NA")
  }

  if (p_value < 0.001) {
    return("< .001")
  }

  base::paste0(
    "= ",
    base::sub(
      "^0",
      "",
      base::formatC(
        p_value,
        format = "f",
        digits = 3
      )
    )
  )
}


#' Return the most common nonmissing value
#'
#' @param value_vec A vector.
#'
#' @return A scalar value.
mode_value <- function(value_vec) {
  nonmissing_vec <- value_vec[!base::is.na(value_vec)]

  if (base::length(nonmissing_vec) == 0L) {
    return(NA)
  }

  unique_values <- base::unique(nonmissing_vec)
  frequency_vec <- base::tabulate(
    base::match(nonmissing_vec, unique_values)
  )

  unique_values[[base::which.max(frequency_vec)]]
}


#' Collapse invalid-listing reasons into publication categories
#'
#' @param reason_vec Raw invalid-reason labels.
#'
#' @return A character vector.
collapse_invalid_reason <- function(reason_vec) {
  clean_reason <- stringr::str_to_lower(
    stringr::str_squish(base::as.character(reason_vec))
  )

  dplyr::case_when(
    stringr::str_detect(
      clean_reason,
      "relocat|changed practice|left practice|moved"
    ) ~ "Relocated",
    stringr::str_detect(
      clean_reason,
      "retir|deceas|died|no longer practicing|inactive"
    ) ~ "Retired or inactive",
    stringr::str_detect(
      clean_reason,
      "wrong number|disconnect|invalid phone|bad phone"
    ) ~ "Invalid contact",
    stringr::str_detect(
      clean_reason,
      "wrong specialty|not otolaryng|not ent"
    ) ~ "Wrong specialty",
    stringr::str_detect(
      clean_reason,
      "nonphysician|non-physician|\\bpa\\b|\\bnp\\b|audiolog|slp"
    ) ~ "Nonphysician taxonomy",
    stringr::str_detect(
      clean_reason,
      "duplicate|administrative"
    ) ~ "Duplicate or administrative",
    base::is.na(clean_reason) | clean_reason == "" ~ "Other",
    TRUE ~ "Other"
  )
}


#' Standard plotting theme
#'
#' @param base_size Base font size.
#'
#' @return A ggplot2 theme.
theme_registry <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        color = "grey35"
      ),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )
}


#' Color-blind-friendly registry palette
#'
#' @return A named color vector.
registry_zone_palette <- function() {
  c(
    "NPPES only" = "#0072B2",
    "NPPES + ABOto" = "#009E73",
    "NPPES + ENTHealth" = "#E69F00",
    "Triple match" = "#CC79A7"
  )
}


#' Save a figure as PNG and PDF
#'
#' @param plot_object A ggplot or patchwork object.
#' @param file_stem File stem without extension.
#' @param save_dir Directory for saved files.
#' @param width Figure width in inches.
#' @param height Figure height in inches.
#' @param timestamp Timestamp appended to filenames.
#'
#' @return A named character vector of saved paths.
save_registry_plot <- function(
    plot_object,
    file_stem,
    save_dir,
    width,
    height,
    timestamp) {
  base::dir.create(
    save_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  png_path <- base::file.path(
    save_dir,
    base::paste0(file_stem, "_", timestamp, ".png")
  )

  pdf_path <- base::file.path(
    save_dir,
    base::paste0(file_stem, "_", timestamp, ".pdf")
  )

  base::message("Saving PNG: ", base::normalizePath(
    png_path,
    winslash = "/",
    mustWork = FALSE
  ))

  ggplot2::ggsave(
    filename = png_path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = 600,
    bg = "white"
  )

  base::message("Saving PDF: ", base::normalizePath(
    pdf_path,
    winslash = "/",
    mustWork = FALSE
  ))

  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot_object,
    width = width,
    height = height,
    device = "pdf",
    bg = "white"
  )

  c(
    png = png_path,
    pdf = pdf_path
  )
}


# -----------------------------------------------------------------------------
# Standardize listing-level fields
# -----------------------------------------------------------------------------

#' Standardize registry listing fields
#'
#' The function recognizes common column names used in the ENT mystery-caller
#' project. Edit the candidate-name vectors if the source files use different
#' names.
#'
#' @param listing_tbl Physician-level listing table.
#' @param reference_date Date used to calculate years since NPPES update.
#'
#' @return A standardized tibble.
prepare_registry_listings <- function(
    listing_tbl,
    reference_date = base::Sys.Date()) {
  check_registry_figure_packages()

  if (!base::is.data.frame(listing_tbl)) {
    base::stop(
      "listing_tbl must be a data.frame or tibble.",
      call. = FALSE
    )
  }

  if (base::isTRUE(
    base::attr(listing_tbl, "registry_fields_prepared")
  )) {
    base::message("Registry fields are already standardized.")
    return(listing_tbl)
  }

  base::message(
    "Standardizing ",
    scales::comma(base::nrow(listing_tbl)),
    " physician listings."
  )

  row_count <- base::nrow(listing_tbl)

  overlap_raw <- pull_column_or(
    listing_tbl,
    c("overlap_zone", "registry_zone", "datasets"),
    NA_character_
  )

  status_raw <- pull_column_or(
    listing_tbl,
    c("listing_status", "status"),
    NA_character_
  )

  invalid_reason_raw <- pull_column_or(
    listing_tbl,
    c("invalid_reason", "new_patient_status", "status_reason"),
    NA_character_
  )

  location_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("location_valid", "confirmed_valid"),
      NA
    )
  )

  workforce_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("workforce_valid", "active_anywhere"),
      NA
    )
  )

  reached_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("stage1_reached", "office_answered", "practice_reached"),
      NA
    )
  )

  present_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c(
        "stage2_physician_present",
        "physician_present",
        "physician_at_location"
      ),
      NA
    )
  )

  active_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("stage3_active_ent", "active_ent"),
      NA
    )
  )

  accepting_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c(
        "stage4_new_patients",
        "accepted",
        "taking_new_patients"
      ),
      NA
    )
  )

  attempted_explicit <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("attempted", "call_attempted"),
      NA
    )
  )

  weight_raw <- base::suppressWarnings(
    base::as.numeric(
      pull_column_or(
        listing_tbl,
        c("sampling_weight", "survey_weight", "weight"),
        1
      )
    )
  )

  rural_raw <- pull_column_or(
    listing_tbl,
    c("ruca_binary", "rurality", "rural", "ruca_code"),
    NA_character_
  )

  state_raw <- pull_column_or(
    listing_tbl,
    c("state", "practice_state", "nppes_state"),
    NA_character_
  )

  region_raw <- pull_column_or(
    listing_tbl,
    c("census_region", "bog_region", "region"),
    NA_character_
  )

  credential_raw <- pull_column_or(
    listing_tbl,
    c("credential_class", "credential", "provider_credential"),
    NA_character_
  )

  years_update_raw <- base::suppressWarnings(
    base::as.numeric(
      pull_column_or(
        listing_tbl,
        c(
          "years_since_update",
          "years_since_nppes_update"
        ),
        NA_real_
      )
    )
  )

  update_date_raw <- pull_column_or(
    listing_tbl,
    c(
      "nppes_last_update",
      "last_nppes_update",
      "nppes_update_date"
    ),
    NA_character_
  )

  recent_billing_raw <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("billed_any_part_b", "recent_medicare_billing"),
      NA
    )
  )

  call_date_raw <- pull_column_or(
    listing_tbl,
    c("call_date", "audit_date"),
    NA_character_
  )

  in_nppes_raw <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("in_nppes"),
      NA
    )
  )

  in_aboto_raw <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("in_aboto", "in_abo", "aboto_flag"),
      NA
    )
  )

  in_enthealth_raw <- as_logical_flag(
    pull_column_or(
      listing_tbl,
      c("in_enthealth", "enthealth_flag"),
      NA
    )
  )

  clean_status <- stringr::str_to_lower(
    stringr::str_squish(base::as.character(status_raw))
  )

  clean_reason <- stringr::str_to_lower(
    stringr::str_squish(base::as.character(invalid_reason_raw))
  )

  overlap_zone <- normalize_overlap_zone(overlap_raw)

  location_from_status <- stringr::str_detect(
    clean_status,
    "confirmed.?valid|location.?valid"
  )

  invalid_from_status <- stringr::str_detect(
    clean_status,
    "confirmed.?invalid|location.?invalid"
  )

  unresolved_from_status <- stringr::str_detect(
    clean_status,
    "unresolved"
  )

  not_attempted_from_status <- stringr::str_detect(
    clean_status,
    "not.?attempted|pending"
  )

  active_elsewhere_from_text <- stringr::str_detect(
    base::paste(clean_status, clean_reason),
    "relocat|changed practice|active elsewhere|moved practice"
  )

  update_date <- base::suppressWarnings(
    base::as.Date(update_date_raw)
  )

  years_since_update <- dplyr::coalesce(
    years_update_raw,
    base::as.numeric(
      base::difftime(
        reference_date,
        update_date,
        units = "days"
      )
    ) / 365.25
  )

  state_name <- normalize_state_name(state_raw)

  region_from_state <- derive_census_region(state_name)

  clean_region <- stringr::str_to_title(
    stringr::str_squish(base::as.character(region_raw))
  )

  clean_region[clean_region == ""] <- NA_character_

  prepared_tbl <- tibble::tibble(
    record_id = base::seq_len(row_count),
    overlap_zone = overlap_zone,
    listing_status = clean_status,
    invalid_reason = collapse_invalid_reason(invalid_reason_raw),
    sampling_weight = dplyr::if_else(
      base::is.na(weight_raw) | weight_raw <= 0,
      1,
      weight_raw
    ),
    rurality = normalize_rurality(rural_raw),
    state_name = state_name,
    census_region = dplyr::coalesce(
      clean_region,
      region_from_state
    ),
    credential_class = stringr::str_to_upper(
      stringr::str_squish(base::as.character(credential_raw))
    ),
    years_since_update = years_since_update,
    recent_medicare_billing = recent_billing_raw,
    call_date = base::suppressWarnings(
      base::as.Date(call_date_raw)
    )
  ) |>
    dplyr::mutate(
      location_valid = dplyr::coalesce(
        location_explicit,
        location_from_status
      ),
      workforce_valid = dplyr::coalesce(
        workforce_explicit,
        dplyr::coalesce(
          .data$location_valid,
          FALSE
        ) |
          active_elsewhere_from_text
      ),
      practice_reached = reached_explicit,
      physician_present = dplyr::coalesce(
        present_explicit,
        .data$location_valid
      ),
      active_ent = dplyr::coalesce(
        active_explicit,
        .data$location_valid
      ),
      accepting_new_patients = accepting_explicit,
      confirmed_invalid = dplyr::coalesce(
        invalid_from_status,
        FALSE
      ) |
        (
          !base::is.na(invalid_reason_raw) &
            clean_reason != "" &
            !dplyr::coalesce(
              .data$location_valid,
              FALSE
            )
        ),
      unresolved = dplyr::coalesce(
        unresolved_from_status,
        FALSE
      ),
      not_attempted = dplyr::coalesce(
        not_attempted_from_status,
        FALSE
      ),
      attempted = dplyr::coalesce(
        attempted_explicit,
        !.data$not_attempted &
          (
            !base::is.na(.data$practice_reached) |
              !base::is.na(.data$location_valid) |
              .data$confirmed_invalid |
              .data$unresolved
          )
      ),
      in_nppes = dplyr::coalesce(
        in_nppes_raw,
        !base::is.na(.data$overlap_zone)
      ),
      in_aboto = dplyr::coalesce(
        in_aboto_raw,
        .data$overlap_zone %in% c(
          "NPPES + ABOto",
          "Triple match"
        )
      ),
      in_enthealth = dplyr::coalesce(
        in_enthealth_raw,
        .data$overlap_zone %in% c(
          "NPPES + ENTHealth",
          "Triple match"
        )
      )
    )

  base::attr(
    prepared_tbl,
    "registry_fields_prepared"
  ) <- TRUE

  base::message(
    "Standardized fields complete. Overlap-zone counts: ",
    base::paste(
      base::names(base::table(prepared_tbl$overlap_zone)),
      base::as.integer(base::table(prepared_tbl$overlap_zone)),
      sep = "=",
      collapse = "; "
    )
  )

  prepared_tbl
}


#' Standardize overlap-population counts
#'
#' @param population_tbl Table with overlap zone and population count.
#'
#' @return A standardized tibble.
prepare_overlap_population <- function(population_tbl) {
  if (!base::is.data.frame(population_tbl)) {
    base::stop(
      "population_tbl must be a data.frame or tibble.",
      call. = FALSE
    )
  }

  zone_name <- first_existing_name(
    population_tbl,
    c("overlap_zone", "registry_zone", "zone")
  )

  count_name <- first_existing_name(
    population_tbl,
    c("population_n", "population", "n_population", "n")
  )

  if (base::is.na(zone_name) || base::is.na(count_name)) {
    base::stop(
      "population_tbl needs overlap-zone and population-count columns.",
      call. = FALSE
    )
  }

  prepared_population <- tibble::tibble(
    overlap_zone = normalize_overlap_zone(
      population_tbl[[zone_name]]
    ),
    population_n = base::as.numeric(
      population_tbl[[count_name]]
    )
  ) |>
    dplyr::filter(
      !base::is.na(.data$overlap_zone),
      !base::is.na(.data$population_n)
    ) |>
    dplyr::group_by(.data$overlap_zone) |>
    dplyr::summarise(
      population_n = base::sum(.data$population_n),
      .groups = "drop"
    )

  base::message(
    "Prepared population counts for ",
    scales::comma(base::nrow(prepared_population)),
    " overlap strata."
  )

  prepared_population
}


# -----------------------------------------------------------------------------
# Survey helpers
# -----------------------------------------------------------------------------

#' Summarize a binary outcome using survey weights
#'
#' @param prepared_tbl Standardized listing table.
#' @param outcome_name Name of a logical outcome.
#' @param by_names Grouping-variable names.
#'
#' @return A tibble with estimates and confidence intervals.
survey_binary_summary <- function(
    prepared_tbl,
    outcome_name,
    by_names) {
  required_names <- c(
    outcome_name,
    by_names,
    "sampling_weight"
  )

  missing_names <- base::setdiff(
    required_names,
    base::names(prepared_tbl)
  )

  if (base::length(missing_names) > 0L) {
    base::stop(
      "Missing columns for survey summary: ",
      base::paste(missing_names, collapse = ", "),
      call. = FALSE
    )
  }

  analysis_tbl <- prepared_tbl |>
    dplyr::filter(
      !base::is.na(.data[[outcome_name]]),
      .data$sampling_weight > 0
    )

  for (group_name in by_names) {
    analysis_tbl <- analysis_tbl |>
      dplyr::filter(
        !base::is.na(.data[[group_name]])
      )
  }

  if (base::nrow(analysis_tbl) == 0L) {
    base::stop(
      "No complete observations are available for ",
      outcome_name,
      ".",
      call. = FALSE
    )
  }

  analysis_tbl <- analysis_tbl |>
    dplyr::mutate(
      survey_binary = base::as.numeric(
        .data[[outcome_name]]
      )
    )

  survey_design <- survey::svydesign(
    ids = ~1,
    weights = ~sampling_weight,
    data = analysis_tbl
  )

  by_formula <- stats::as.formula(
    base::paste0(
      "~",
      base::paste(by_names, collapse = " + ")
    )
  )

  summary_object <- survey::svyby(
    ~survey_binary,
    by_formula,
    survey_design,
    survey::svymean,
    vartype = "ci",
    na.rm = TRUE,
    keep.names = FALSE
  )

  summary_tbl <- tibble::as_tibble(summary_object)

  estimate_name <- first_existing_name(
    summary_tbl,
    c("survey_binary")
  )

  lower_name <- first_existing_name(
    summary_tbl,
    c("ci_l", "ci_l.survey_binary")
  )

  upper_name <- first_existing_name(
    summary_tbl,
    c("ci_u", "ci_u.survey_binary")
  )

  if (
    base::is.na(estimate_name) ||
      base::is.na(lower_name) ||
      base::is.na(upper_name)
  ) {
    base::stop(
      "Could not identify survey estimate columns.",
      call. = FALSE
    )
  }

  summary_tbl |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(by_names)),
      estimate = .data[[estimate_name]],
      ci_low = .data[[lower_name]],
      ci_high = .data[[upper_name]]
    )
}


#' Extract fitted values and standard errors from survey predictions
#'
#' @param prediction_object Object returned by predict.svyglm().
#'
#' @return A list with estimate and standard_error vectors.
extract_survey_prediction <- function(prediction_object) {
  if (
    base::is.list(prediction_object) &&
      base::all(c("fit", "se.fit") %in%
        base::names(prediction_object))
  ) {
    return(
      base::list(
        estimate = base::as.numeric(prediction_object$fit),
        standard_error = base::as.numeric(
          prediction_object$se.fit
        )
      )
    )
  }

  standard_error <- base::tryCatch(
    base::as.numeric(survey::SE(prediction_object)),
    error = function(error_condition) {
      base::rep(
        NA_real_,
        base::length(prediction_object)
      )
    }
  )

  base::list(
    estimate = base::as.numeric(prediction_object),
    standard_error = standard_error
  )
}


# -----------------------------------------------------------------------------
# Figure 1: Registry overlap UpSet-style figure
# -----------------------------------------------------------------------------

#' Create an UpSet-style registry overlap figure
#'
#' @param listing_tbl Physician-level listing table.
#' @param population_tbl Overlap-zone population counts.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot and saved paths.
create_registry_upset_figure <- function(
    listing_tbl,
    population_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 1: preparing registry overlap figure.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)
  prepared_population <- prepare_overlap_population(
    population_tbl
  )

  audit_counts <- prepared_tbl |>
    dplyr::filter(!base::is.na(.data$overlap_zone)) |>
    dplyr::count(
      .data$overlap_zone,
      name = "audited_n"
    )

  overlap_counts <- prepared_population |>
    dplyr::left_join(
      audit_counts,
      by = "overlap_zone"
    ) |>
    dplyr::mutate(
      audited_n = dplyr::coalesce(
        .data$audited_n,
        0L
      ),
      overlap_zone = forcats::fct_drop(
        .data$overlap_zone
      )
    )

  shown_levels <- base::levels(
    overlap_counts$overlap_zone
  )

  shown_levels <- shown_levels[
    shown_levels %in%
      base::as.character(overlap_counts$overlap_zone)
  ]

  overlap_counts <- overlap_counts |>
    dplyr::mutate(
      overlap_zone = base::factor(
        .data$overlap_zone,
        levels = shown_levels,
        ordered = TRUE
      )
    )

  bar_plot <- ggplot2::ggplot(
    overlap_counts,
    ggplot2::aes(
      x = .data$overlap_zone,
      y = .data$population_n,
      fill = .data$overlap_zone
    )
  ) +
    ggplot2::geom_col(
      width = 0.68,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::comma(.data$population_n)
      ),
      vjust = -0.35,
      fontface = "bold",
      size = 3.6
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        y = 0,
        label = base::paste0(
          "Audited: ",
          scales::comma(.data$audited_n)
        )
      ),
      vjust = 1.2,
      size = 3,
      label.size = 0.2,
      fill = "white"
    ) +
    ggplot2::scale_fill_manual(
      values = registry_zone_palette()
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(
        mult = c(0.06, 0.15)
      )
    ) +
    ggplot2::labs(
      y = "Source-population listings",
      x = NULL
    ) +
    theme_registry() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  matrix_tbl <- tidyr::expand_grid(
    overlap_zone = base::factor(
      shown_levels,
      levels = shown_levels,
      ordered = TRUE
    ),
    registry = base::factor(
      c("NPPES", "ABOto", "ENTHealth"),
      levels = c(
        "ENTHealth",
        "ABOto",
        "NPPES"
      )
    )
  ) |>
    dplyr::mutate(
      included = dplyr::case_when(
        .data$registry == "NPPES" ~ TRUE,
        .data$registry == "ABOto" &
          .data$overlap_zone %in% c(
            "NPPES + ABOto",
            "Triple match"
          ) ~ TRUE,
        .data$registry == "ENTHealth" &
          .data$overlap_zone %in% c(
            "NPPES + ENTHealth",
            "Triple match"
          ) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  active_matrix <- matrix_tbl |>
    dplyr::filter(.data$included)

  matrix_plot <- ggplot2::ggplot(
    matrix_tbl,
    ggplot2::aes(
      x = .data$overlap_zone,
      y = .data$registry
    )
  ) +
    ggplot2::geom_point(
      color = "grey80",
      size = 4.4
    ) +
    ggplot2::geom_line(
      data = active_matrix,
      ggplot2::aes(
        group = .data$overlap_zone
      ),
      linewidth = 0.8,
      color = "grey25"
    ) +
    ggplot2::geom_point(
      data = active_matrix,
      size = 4.6,
      color = "grey10"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    theme_registry() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 25,
        hjust = 1
      )
    )

  plot_object <- bar_plot /
    matrix_plot +
    patchwork::plot_layout(
      heights = c(3.1, 1.35)
    ) +
    patchwork::plot_annotation(
      title = "National otolaryngology registry overlap",
      subtitle = paste0(
        "Population size, audited sample, and ",
        "registry membership by overlap stratum"
      ),
      caption = paste0(
        "Bars show source-population listings. ",
        "Labels show listings selected for telephone audit."
      )
    )

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_1_registry_overlap_upset",
      save_dir = save_dir,
      width = 8.5,
      height = 7.2,
      timestamp = timestamp
    )
  }

  base::message("Figure 1 complete.")

  base::list(
    plot = plot_object,
    files = saved_paths
  )
}


# -----------------------------------------------------------------------------
# Figure 2: Registry credibility forest plot
# -----------------------------------------------------------------------------

#' Create the registry credibility forest plot
#'
#' @param listing_tbl Physician-level listing table.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot and saved paths.
create_registry_credibility_figure <- function(
    listing_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 2: preparing registry credibility plot.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  location_tbl <- survey_binary_summary(
    prepared_tbl = prepared_tbl,
    outcome_name = "location_valid",
    by_names = "overlap_zone"
  ) |>
    dplyr::mutate(
      confirmation_type = "Location-confirmed"
    )

  workforce_tbl <- survey_binary_summary(
    prepared_tbl = prepared_tbl,
    outcome_name = "workforce_valid",
    by_names = "overlap_zone"
  ) |>
    dplyr::mutate(
      confirmation_type = "Workforce-confirmed"
    )

  metric_tbl <- dplyr::bind_rows(
    location_tbl,
    workforce_tbl
  ) |>
    dplyr::mutate(
      overlap_zone = forcats::fct_drop(
        .data$overlap_zone
      ),
      zone_number = base::as.numeric(
        .data$overlap_zone
      ),
      vertical_offset = dplyr::if_else(
        .data$confirmation_type ==
          "Location-confirmed",
        -0.12,
        0.12
      ),
      y_position = .data$zone_number +
        .data$vertical_offset,
      estimate_label = scales::percent(
        .data$estimate,
        accuracy = 0.1
      )
    )

  zone_levels <- base::levels(metric_tbl$overlap_zone)

  outcome_palette <- c(
    "Location-confirmed" = "#0072B2",
    "Workforce-confirmed" = "#D55E00"
  )

  plot_object <- ggplot2::ggplot(metric_tbl) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$ci_low,
        xend = .data$ci_high,
        y = .data$y_position,
        yend = .data$y_position,
        color = .data$confirmation_type
      ),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$estimate,
        y = .data$y_position,
        color = .data$confirmation_type,
        shape = .data$confirmation_type
      ),
      size = 3.2,
      stroke = 1.1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = base::pmin(.data$ci_high + 0.035, 0.97),
        y = .data$y_position,
        label = .data$estimate_label,
        color = .data$confirmation_type
      ),
      hjust = 0,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = outcome_palette
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Location-confirmed" = 16,
        "Workforce-confirmed" = 1
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(
        accuracy = 1
      ),
      limits = c(0, 1),
      breaks = base::seq(0, 1, by = 0.2)
    ) +
    ggplot2::scale_y_continuous(
      breaks = base::seq_along(zone_levels),
      labels = zone_levels,
      expand = ggplot2::expansion(
        mult = c(0.12, 0.12)
      )
    ) +
    ggplot2::labs(
      title = "Listing confirmation by registry corroboration",
      subtitle = paste0(
        "Design-weighted estimates; error bars represent ",
        "95% confidence intervals"
      ),
      x = "Listings confirmed",
      y = NULL,
      color = NULL,
      shape = NULL,
      caption = paste0(
        "Location-confirmed indicates practice at the indexed ",
        "location. Workforce-confirmed includes active practice ",
        "at another location."
      )
    ) +
    theme_registry() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_2_registry_credibility_forest",
      save_dir = save_dir,
      width = 8.5,
      height = 5.5,
      timestamp = timestamp
    )
  }

  base::message("Figure 2 complete.")

  base::list(
    plot = plot_object,
    files = saved_paths,
    estimates = metric_tbl
  )
}


# -----------------------------------------------------------------------------
# Figure 3: Listing validation journey
# -----------------------------------------------------------------------------

#' Create the listing validation journey figure
#'
#' @param listing_tbl Physician-level listing table.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot and saved paths.
create_validation_journey_figure <- function(
    listing_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 3: preparing listing validation journey.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  sampled_n <- base::nrow(prepared_tbl)
  attempted_n <- base::sum(
    prepared_tbl$attempted %in% TRUE,
    na.rm = TRUE
  )
  not_attempted_n <- base::sum(
    prepared_tbl$not_attempted %in% TRUE,
    na.rm = TRUE
  )
  location_n <- base::sum(
    prepared_tbl$location_valid %in% TRUE,
    na.rm = TRUE
  )
  active_elsewhere_n <- base::sum(
    prepared_tbl$workforce_valid %in% TRUE &
      prepared_tbl$location_valid %in% FALSE,
    na.rm = TRUE
  )
  inactive_invalid_n <- base::sum(
    prepared_tbl$confirmed_invalid %in% TRUE &
      prepared_tbl$workforce_valid %in% FALSE,
    na.rm = TRUE
  )
  unresolved_n <- base::sum(
    prepared_tbl$unresolved %in% TRUE,
    na.rm = TRUE
  )
  adjudicated_n <- location_n +
    active_elsewhere_n +
    inactive_invalid_n

  format_node_label <- function(node_name, node_n) {
    base::paste0(
      node_name,
      "\n",
      scales::comma(node_n),
      " (",
      scales::percent(
        node_n / sampled_n,
        accuracy = 0.1
      ),
      ")"
    )
  }

  node_tbl <- tibble::tribble(
    ~node_id, ~node_name, ~x, ~y, ~node_n,
    "sampled", "Selected", 1, 2.5, sampled_n,
    "attempted", "Attempted", 3, 2.5, attempted_n,
    "not_attempted", "Not attempted", 3, 0.5,
    not_attempted_n,
    "adjudicated", "Adjudicated", 5, 3.4,
    adjudicated_n,
    "unresolved", "Unresolved", 5, 1.5, unresolved_n,
    "location", "Location-confirmed", 7, 4.5, location_n,
    "elsewhere", "Active elsewhere", 7, 3.0,
    active_elsewhere_n,
    "inactive", "Inactive or invalid", 7, 1.5,
    inactive_invalid_n
  ) |>
    dplyr::mutate(
      label = purrr::map2_chr(
        .data$node_name,
        .data$node_n,
        format_node_label
      ),
      xmin = .data$x - 0.72,
      xmax = .data$x + 0.72,
      ymin = .data$y - 0.42,
      ymax = .data$y + 0.42
    )

  edge_tbl <- tibble::tribble(
    ~from_id, ~to_id,
    "sampled", "attempted",
    "sampled", "not_attempted",
    "attempted", "adjudicated",
    "attempted", "unresolved",
    "adjudicated", "location",
    "adjudicated", "elsewhere",
    "adjudicated", "inactive"
  ) |>
    dplyr::left_join(
      node_tbl |>
        dplyr::select(
          from_id = .data$node_id,
          from_x = .data$x,
          from_y = .data$y,
          from_xmax = .data$xmax
        ),
      by = "from_id"
    ) |>
    dplyr::left_join(
      node_tbl |>
        dplyr::select(
          to_id = .data$node_id,
          to_x = .data$x,
          to_y = .data$y,
          to_xmin = .data$xmin
        ),
      by = "to_id"
    )

  node_colors <- c(
    "Selected" = "#D9EAF7",
    "Attempted" = "#D9EAF7",
    "Not attempted" = "#F0F0F0",
    "Adjudicated" = "#D9EAF7",
    "Unresolved" = "#FEE8C8",
    "Location-confirmed" = "#C7E9C0",
    "Active elsewhere" = "#C6DBEF",
    "Inactive or invalid" = "#FDD0A2"
  )

  plot_object <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_tbl,
      ggplot2::aes(
        x = .data$from_xmax,
        xend = .data$to_xmin,
        y = .data$from_y,
        yend = .data$to_y
      ),
      arrow = grid::arrow(
        length = grid::unit(0.14, "inches"),
        type = "closed"
      ),
      linewidth = 0.7,
      color = "grey35"
    ) +
    ggplot2::geom_rect(
      data = node_tbl,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$node_name
      ),
      color = "grey25",
      linewidth = 0.6,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = node_tbl,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$label
      ),
      size = 3.25,
      lineheight = 0.95
    ) +
    ggplot2::scale_fill_manual(
      values = node_colors
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0.1, 7.9),
      ylim = c(0, 5.2),
      clip = "off"
    ) +
    ggplot2::labs(
      title = "From registry listing to practice confirmation",
      subtitle = paste0(
        "Counts and percentages use all selected listings ",
        "as the denominator"
      ),
      caption = paste0(
        "Active elsewhere indicates a location-invalid but ",
        "workforce-confirmed physician."
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        color = "grey35",
        margin = ggplot2::margin(t = 10)
      ),
      plot.margin = ggplot2::margin(
        10,
        20,
        10,
        20
      )
    )

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_3_validation_journey",
      save_dir = save_dir,
      width = 10,
      height = 5.8,
      timestamp = timestamp
    )
  }

  base::message(
    "Figure 3 complete: ",
    scales::comma(adjudicated_n),
    " listings adjudicated."
  )

  base::list(
    plot = plot_object,
    files = saved_paths,
    nodes = node_tbl
  )
}


# -----------------------------------------------------------------------------
# Figure 4: Reasons for invalid listings
# -----------------------------------------------------------------------------

#' Create the invalid-listing reason figure
#'
#' @param listing_tbl Physician-level listing table.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot and saved paths.
create_invalid_reason_figure <- function(
    listing_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 4: preparing invalid-listing reasons.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  invalid_tbl <- prepared_tbl |>
    dplyr::filter(
      .data$confirmed_invalid %in% TRUE,
      !base::is.na(.data$overlap_zone)
    ) |>
    dplyr::group_by(
      .data$overlap_zone,
      .data$invalid_reason
    ) |>
    dplyr::summarise(
      observed_n = dplyr::n(),
      weighted_n = base::sum(.data$sampling_weight),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$overlap_zone) |>
    dplyr::mutate(
      weighted_total = base::sum(.data$weighted_n),
      proportion = .data$weighted_n /
        .data$weighted_total,
      observed_total = base::sum(.data$observed_n)
    ) |>
    dplyr::ungroup()

  if (base::nrow(invalid_tbl) == 0L) {
    base::stop(
      "No confirmed-invalid listings are available.",
      call. = FALSE
    )
  }

  reason_levels <- invalid_tbl |>
    dplyr::group_by(.data$invalid_reason) |>
    dplyr::summarise(
      weighted_n = base::sum(.data$weighted_n),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$weighted_n)) |>
    dplyr::pull(.data$invalid_reason)

  invalid_tbl <- invalid_tbl |>
    dplyr::mutate(
      invalid_reason = base::factor(
        .data$invalid_reason,
        levels = reason_levels
      ),
      overlap_zone = forcats::fct_drop(
        .data$overlap_zone
      )
    )

  zone_totals <- invalid_tbl |>
    dplyr::distinct(
      .data$overlap_zone,
      .data$observed_total
    )

  reason_colors <- grDevices::hcl.colors(
    base::length(reason_levels),
    palette = "Dark 3"
  )

  base::names(reason_colors) <- reason_levels

  plot_object <- ggplot2::ggplot(
    invalid_tbl,
    ggplot2::aes(
      x = .data$proportion,
      y = .data$overlap_zone,
      fill = .data$invalid_reason
    )
  ) +
    ggplot2::geom_col(
      width = 0.68,
      color = "white",
      linewidth = 0.25
    ) +
    ggplot2::geom_text(
      data = zone_totals,
      ggplot2::aes(
        x = 1.025,
        y = .data$overlap_zone,
        label = base::paste0(
          "n = ",
          scales::comma(.data$observed_total)
        )
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 3.2
    ) +
    ggplot2::scale_fill_manual(
      values = reason_colors
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(
        accuracy = 1
      ),
      limits = c(0, 1.14),
      breaks = base::seq(0, 1, by = 0.25),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Why physician listings were invalid",
      subtitle = paste0(
        "Weighted distribution of confirmed-invalid reasons ",
        "within each registry stratum"
      ),
      x = "Proportion of confirmed-invalid listings",
      y = NULL,
      fill = "Reason"
    ) +
    theme_registry() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_4_invalid_listing_reasons",
      save_dir = save_dir,
      width = 9.2,
      height = 5.7,
      timestamp = timestamp
    )
  }

  base::message(
    "Figure 4 complete: ",
    scales::comma(base::sum(zone_totals$observed_total)),
    " confirmed-invalid listings summarized."
  )

  base::list(
    plot = plot_object,
    files = saved_paths,
    estimates = invalid_tbl
  )
}


# -----------------------------------------------------------------------------
# Figure 5: NPPES record staleness curve
# -----------------------------------------------------------------------------

#' Create adjusted NPPES record staleness curves
#'
#' @param listing_tbl Physician-level listing table.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot, model, and saved paths.
create_staleness_curve_figure <- function(
    listing_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 5: preparing NPPES staleness curves.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  model_tbl <- prepared_tbl |>
    dplyr::filter(
      !base::is.na(.data$location_valid),
      !base::is.na(.data$years_since_update),
      !base::is.na(.data$overlap_zone),
      .data$sampling_weight > 0
    ) |>
    dplyr::mutate(
      location_numeric = base::as.numeric(
        .data$location_valid
      ),
      overlap_zone = forcats::fct_drop(
        .data$overlap_zone
      )
    )

  if (base::nrow(model_tbl) < 30L) {
    base::stop(
      "At least 30 complete listings are needed for Figure 5.",
      call. = FALSE
    )
  }

  survey_design <- survey::svydesign(
    ids = ~1,
    weights = ~sampling_weight,
    data = model_tbl
  )

  interaction_fit <- survey::svyglm(
    location_numeric ~
      overlap_zone * years_since_update,
    design = survey_design,
    family = stats::quasibinomial()
  )

  additive_fit <- survey::svyglm(
    location_numeric ~
      overlap_zone + years_since_update,
    design = survey_design,
    family = stats::quasibinomial()
  )

  year_limit <- base::as.numeric(
    stats::quantile(
      model_tbl$years_since_update,
      probs = 0.95,
      na.rm = TRUE
    )
  )

  year_limit <- base::max(
    1,
    base::ceiling(year_limit)
  )

  prediction_tbl <- tidyr::expand_grid(
    years_since_update = base::seq(
      0,
      year_limit,
      length.out = 120
    ),
    overlap_zone = base::levels(
      model_tbl$overlap_zone
    )
  ) |>
    dplyr::mutate(
      overlap_zone = base::factor(
        .data$overlap_zone,
        levels = base::levels(
          model_tbl$overlap_zone
        ),
        ordered = TRUE
      )
    )

  prediction_object <- stats::predict(
    interaction_fit,
    newdata = prediction_tbl,
    type = "link",
    se.fit = TRUE
  )

  prediction_components <- extract_survey_prediction(
    prediction_object
  )

  prediction_tbl <- prediction_tbl |>
    dplyr::mutate(
      link_estimate = prediction_components$estimate,
      link_se = prediction_components$standard_error,
      estimate = stats::plogis(.data$link_estimate),
      ci_low = stats::plogis(
        .data$link_estimate -
          1.96 * .data$link_se
      ),
      ci_high = stats::plogis(
        .data$link_estimate +
          1.96 * .data$link_se
      )
    )

  coefficient_tbl <- base::as.data.frame(
    base::summary(additive_fit)$coefficients
  )

  p_value <- NA_real_
  direction <- "changed"

  if ("years_since_update" %in%
      base::rownames(coefficient_tbl)) {
    coefficient_row <- coefficient_tbl[
      "years_since_update",
      ,
      drop = FALSE
    ]

    p_value <- coefficient_row[[4L]]
    direction <- dplyr::if_else(
      coefficient_row[[1L]] < 0,
      "decreased",
      "increased"
    )
  }

  min_year <- base::min(
    model_tbl$years_since_update,
    na.rm = TRUE
  )

  max_year <- base::max(
    model_tbl$years_since_update,
    na.rm = TRUE
  )

  summary_sentence <- base::paste0(
    "Across ",
    scales::comma(base::nrow(model_tbl)),
    " listings spanning ",
    base::formatC(min_year, format = "f", digits = 1),
    " to ",
    base::formatC(max_year, format = "f", digits = 1),
    " years since update, confirmation ",
    direction,
    " as records aged (P ",
    format_p_value(p_value),
    ")."
  )

  base::message(summary_sentence)

  plot_object <- ggplot2::ggplot(
    prediction_tbl,
    ggplot2::aes(
      x = .data$years_since_update,
      y = .data$estimate,
      color = .data$overlap_zone,
      fill = .data$overlap_zone
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$ci_low,
        ymax = .data$ci_high
      ),
      alpha = 0.14,
      color = NA
    ) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_rug(
      data = model_tbl,
      ggplot2::aes(
        x = .data$years_since_update,
        color = .data$overlap_zone
      ),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.16,
      length = grid::unit(0.035, "npc")
    ) +
    ggplot2::scale_color_manual(
      values = registry_zone_palette()
    ) +
    ggplot2::scale_fill_manual(
      values = registry_zone_palette()
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(
        accuracy = 1
      ),
      limits = c(0, 1)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, year_limit),
      breaks = scales::breaks_pretty(n = 6)
    ) +
    ggplot2::labs(
      title = "Listing confirmation by NPPES record age",
      subtitle = summary_sentence,
      x = "Years since most recent NPPES update",
      y = "Adjusted probability of location confirmation",
      color = "Registry stratum",
      fill = "Registry stratum",
      caption = paste0(
        "Curves are survey-weighted logistic predictions. ",
        "Bands represent 95% confidence intervals; rug marks ",
        "show observed record ages."
      )
    ) +
    theme_registry()

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_5_nppes_staleness_curve",
      save_dir = save_dir,
      width = 9,
      height = 6.1,
      timestamp = timestamp
    )
  }

  base::message("Figure 5 complete.")

  base::list(
    plot = plot_object,
    files = saved_paths,
    model = interaction_fit,
    prediction_estimates = prediction_tbl,
    summary_sentence = summary_sentence
  )
}


# -----------------------------------------------------------------------------
# Figure 6: National validity map
# -----------------------------------------------------------------------------

#' Create national maps of confirmation and unresolved rates
#'
#' @param listing_tbl Physician-level listing table.
#' @param minimum_state_n Minimum attempted listings per state.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot and saved paths.
create_national_validity_map <- function(
    listing_tbl,
    minimum_state_n = 5L,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 6: preparing national validity maps.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  state_summary <- prepared_tbl |>
    dplyr::filter(
      .data$attempted %in% TRUE,
      !base::is.na(.data$state_name)
    ) |>
    dplyr::group_by(.data$state_name) |>
    dplyr::summarise(
      attempted_n = dplyr::n(),
      weight_total = base::sum(.data$sampling_weight),
      confirmed_rate = base::sum(
        .data$sampling_weight *
          base::as.numeric(
            .data$location_valid %in% TRUE
          )
      ) / .data$weight_total,
      unresolved_rate = base::sum(
        .data$sampling_weight *
          base::as.numeric(
            .data$unresolved %in% TRUE
          )
      ) / .data$weight_total,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      enough_observations = .data$attempted_n >=
        minimum_state_n,
      confirmed_rate = dplyr::if_else(
        .data$enough_observations,
        .data$confirmed_rate,
        NA_real_
      ),
      unresolved_rate = dplyr::if_else(
        .data$enough_observations,
        .data$unresolved_rate,
        NA_real_
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(
        "confirmed_rate",
        "unresolved_rate"
      ),
      names_to = "map_metric",
      values_to = "estimate"
    ) |>
    dplyr::mutate(
      map_metric = dplyr::recode(
        .data$map_metric,
        confirmed_rate = "Location-confirmed",
        unresolved_rate = "Unresolved"
      )
    )

  state_polygons <- ggplot2::map_data("state") |>
    dplyr::left_join(
      state_summary,
      by = c("region" = "state_name")
    )

  suppressed_states <- state_summary |>
    dplyr::filter(!.data$enough_observations) |>
    dplyr::distinct(.data$state_name) |>
    nrow()

  base::message(
    "Map preparation complete: ",
    scales::comma(suppressed_states),
    " states suppressed for fewer than ",
    minimum_state_n,
    " attempted listings."
  )

  plot_object <- ggplot2::ggplot(
    state_polygons,
    ggplot2::aes(
      x = .data$long,
      y = .data$lat,
      group = .data$group,
      fill = .data$estimate
    )
  ) +
    ggplot2::geom_polygon(
      color = "white",
      linewidth = 0.22
    ) +
    ggplot2::coord_quickmap() +
    ggplot2::facet_wrap(
      ~map_metric,
      ncol = 1
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      labels = scales::label_percent(
        accuracy = 1
      ),
      limits = c(0, 1),
      na.value = "grey88"
    ) +
    ggplot2::labs(
      title = "Geographic variation in registry confirmation",
      subtitle = paste0(
        "Design-weighted state estimates; grey states had fewer ",
        "than ",
        minimum_state_n,
        " attempted listings"
      ),
      fill = "Rate",
      caption = paste0(
        "Location-confirmed and unresolved rates use all ",
        "attempted listings in each state."
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 11
      ),
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        color = "grey35"
      )
    )

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_6_national_validity_map",
      save_dir = save_dir,
      width = 9,
      height = 8.5,
      timestamp = timestamp
    )
  }

  base::message("Figure 6 complete.")

  base::list(
    plot = plot_object,
    files = saved_paths,
    state_estimates = state_summary
  )
}


# -----------------------------------------------------------------------------
# Figure 7: Rural versus nonrural dumbbell plot
# -----------------------------------------------------------------------------

#' Create rural versus nonrural dumbbell plot
#'
#' @param listing_tbl Physician-level listing table.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to filenames.
#' @param save_files Whether to save PNG and PDF files.
#'
#' @return A list containing the plot, model, and saved paths.
create_rural_dumbbell_figure <- function(
    listing_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    save_files = TRUE) {
  base::message("Figure 7: preparing rural comparison.")

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  rural_tbl <- survey_binary_summary(
    prepared_tbl = prepared_tbl,
    outcome_name = "location_valid",
    by_names = c(
      "overlap_zone",
      "rurality"
    )
  ) |>
    dplyr::filter(
      .data$rurality %in% c(
        "Nonrural",
        "Rural"
      )
    ) |>
    dplyr::mutate(
      overlap_zone = forcats::fct_drop(
        .data$overlap_zone
      ),
      zone_number = base::as.numeric(
        .data$overlap_zone
      ),
      vertical_offset = dplyr::if_else(
        .data$rurality == "Rural",
        -0.11,
        0.11
      ),
      y_position = .data$zone_number +
        .data$vertical_offset,
      estimate_label = scales::percent(
        .data$estimate,
        accuracy = 0.1
      )
    )

  segment_tbl <- rural_tbl |>
    dplyr::select(
      .data$overlap_zone,
      .data$rurality,
      .data$estimate
    ) |>
    tidyr::pivot_wider(
      names_from = .data$rurality,
      values_from = .data$estimate
    ) |>
    dplyr::filter(
      !base::is.na(.data$Rural),
      !base::is.na(.data$Nonrural)
    ) |>
    dplyr::mutate(
      zone_number = base::as.numeric(
        .data$overlap_zone
      ),
      difference = .data$Rural -
        .data$Nonrural
    )

  model_tbl <- prepared_tbl |>
    dplyr::filter(
      !base::is.na(.data$location_valid),
      !base::is.na(.data$overlap_zone),
      !base::is.na(.data$rurality),
      .data$sampling_weight > 0
    ) |>
    dplyr::mutate(
      location_numeric = base::as.numeric(
        .data$location_valid
      ),
      overlap_zone = forcats::fct_drop(
        .data$overlap_zone
      ),
      rurality = forcats::fct_drop(
        .data$rurality
      )
    )

  interaction_p <- NA_real_
  interaction_fit <- NULL

  if (
    dplyr::n_distinct(model_tbl$rurality) == 2L &&
      dplyr::n_distinct(model_tbl$overlap_zone) >= 2L
  ) {
    survey_design <- survey::svydesign(
      ids = ~1,
      weights = ~sampling_weight,
      data = model_tbl
    )

    interaction_fit <- survey::svyglm(
      location_numeric ~ overlap_zone * rurality,
      design = survey_design,
      family = stats::quasibinomial()
    )

    term_test <- base::tryCatch(
      survey::regTermTest(
        interaction_fit,
        ~overlap_zone:rurality
      ),
      error = function(error_condition) {
        NULL
      }
    )

    if (!base::is.null(term_test)) {
      interaction_p <- term_test$p
    }
  }

  overall_by_rurality <- prepared_tbl |>
    dplyr::filter(
      !base::is.na(.data$location_valid),
      .data$rurality %in% c(
        "Nonrural",
        "Rural"
      )
    ) |>
    dplyr::group_by(.data$rurality) |>
    dplyr::summarise(
      estimate = stats::weighted.mean(
        base::as.numeric(.data$location_valid),
        .data$sampling_weight,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = .data$rurality,
      values_from = .data$estimate
    )

  overall_difference <- NA_real_
  direction <- "differed"

  if (
    base::all(c("Rural", "Nonrural") %in%
      base::names(overall_by_rurality))
  ) {
    overall_difference <- overall_by_rurality$Rural -
      overall_by_rurality$Nonrural

    direction <- dplyr::if_else(
      overall_difference < 0,
      "lower",
      "higher"
    )
  }

  call_years <- base::sort(
    base::unique(
      base::format(
        prepared_tbl$call_date[
          !base::is.na(prepared_tbl$call_date)
        ],
        "%Y"
      )
    )
  )

  if (base::length(call_years) == 0L) {
    year_text <- "the study period"
  } else if (base::length(call_years) == 1L) {
    year_text <- call_years[[1L]]
  } else {
    year_text <- base::paste0(
      call_years[[1L]],
      "-",
      call_years[[base::length(call_years)]]
    )
  }

  summary_sentence <- base::paste0(
    "During ",
    year_text,
    ", rural confirmation was ",
    direction,
    " by ",
    scales::percent(
      base::abs(overall_difference),
      accuracy = 0.1
    ),
    " overall; overlap-zone interaction P ",
    format_p_value(interaction_p),
    "."
  )

  base::message(summary_sentence)

  rural_palette <- c(
    "Nonrural" = "#0072B2",
    "Rural" = "#D55E00"
  )

  zone_levels <- base::levels(rural_tbl$overlap_zone)

  plot_object <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = segment_tbl,
      ggplot2::aes(
        x = .data$Nonrural,
        xend = .data$Rural,
        y = .data$zone_number,
        yend = .data$zone_number
      ),
      linewidth = 1.1,
      color = "grey65"
    ) +
    ggplot2::geom_segment(
      data = rural_tbl,
      ggplot2::aes(
        x = .data$ci_low,
        xend = .data$ci_high,
        y = .data$y_position,
        yend = .data$y_position,
        color = .data$rurality
      ),
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = rural_tbl,
      ggplot2::aes(
        x = .data$estimate,
        y = .data$y_position,
        color = .data$rurality,
        shape = .data$rurality
      ),
      size = 3.4,
      stroke = 1.1
    ) +
    ggplot2::geom_text(
      data = rural_tbl,
      ggplot2::aes(
        x = base::pmin(.data$ci_high + 0.025, 0.97),
        y = .data$y_position,
        label = .data$estimate_label,
        color = .data$rurality
      ),
      hjust = 0,
      size = 3.1,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = rural_palette
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        "Nonrural" = 16,
        "Rural" = 17
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(
        accuracy = 1
      ),
      limits = c(0, 1),
      breaks = base::seq(0, 1, by = 0.2)
    ) +
    ggplot2::scale_y_continuous(
      breaks = base::seq_along(zone_levels),
      labels = zone_levels,
      expand = ggplot2::expansion(
        mult = c(0.12, 0.12)
      )
    ) +
    ggplot2::labs(
      title = "Rural and nonrural listing confirmation",
      subtitle = summary_sentence,
      x = "Location-confirmed listings",
      y = NULL,
      color = NULL,
      shape = NULL,
      caption = paste0(
        "Points are design-weighted estimates. Horizontal ",
        "intervals represent 95% confidence intervals."
      )
    ) +
    theme_registry() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )

  saved_paths <- character(0)

  if (base::isTRUE(save_files)) {
    saved_paths <- save_registry_plot(
      plot_object = plot_object,
      file_stem = "figure_7_rural_nonrural_dumbbell",
      save_dir = save_dir,
      width = 8.8,
      height = 5.8,
      timestamp = timestamp
    )
  }

  base::message("Figure 7 complete.")

  base::list(
    plot = plot_object,
    files = saved_paths,
    model = interaction_fit,
    estimates = rural_tbl,
    summary_sentence = summary_sentence
  )
}


# -----------------------------------------------------------------------------
# Create all seven figures
# -----------------------------------------------------------------------------

#' Create all seven registry-validity figures
#'
#' @param listing_tbl Physician-level listing table.
#' @param population_tbl Overlap-zone population counts.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp appended to every filename.
#' @param minimum_state_n Minimum attempted listings per mapped state.
#'
#' @return An invisible named list of figure artifacts.
create_all_registry_figures <- function(
    listing_tbl,
    population_tbl,
    save_dir = "output/figures",
    timestamp = base::format(
      base::Sys.time(),
      "%Y%m%d_%H%M%S"
    ),
    minimum_state_n = 5L) {
  check_registry_figure_packages()

  base::message(
    "Starting seven-figure registry workflow."
  )
  base::message(
    "Input listings: ",
    scales::comma(base::nrow(listing_tbl))
  )
  base::message(
    "Figure directory: ",
    base::normalizePath(
      save_dir,
      winslash = "/",
      mustWork = FALSE
    )
  )
  base::message(
    "Shared filename timestamp: ",
    timestamp
  )

  prepared_tbl <- prepare_registry_listings(listing_tbl)

  figure_artifacts <- base::list(
    figure_1 = create_registry_upset_figure(
      listing_tbl = prepared_tbl,
      population_tbl = population_tbl,
      save_dir = save_dir,
      timestamp = timestamp
    ),
    figure_2 = create_registry_credibility_figure(
      listing_tbl = prepared_tbl,
      save_dir = save_dir,
      timestamp = timestamp
    ),
    figure_3 = create_validation_journey_figure(
      listing_tbl = prepared_tbl,
      save_dir = save_dir,
      timestamp = timestamp
    ),
    figure_4 = create_invalid_reason_figure(
      listing_tbl = prepared_tbl,
      save_dir = save_dir,
      timestamp = timestamp
    ),
    figure_5 = create_staleness_curve_figure(
      listing_tbl = prepared_tbl,
      save_dir = save_dir,
      timestamp = timestamp
    ),
    figure_6 = create_national_validity_map(
      listing_tbl = prepared_tbl,
      minimum_state_n = minimum_state_n,
      save_dir = save_dir,
      timestamp = timestamp
    ),
    figure_7 = create_rural_dumbbell_figure(
      listing_tbl = prepared_tbl,
      save_dir = save_dir,
      timestamp = timestamp
    )
  )

  base::message(
    "Seven-figure workflow complete."
  )
  base::message(
    "All files saved under: ",
    base::normalizePath(
      save_dir,
      winslash = "/",
      mustWork = FALSE
    )
  )

  base::invisible(figure_artifacts)
}


# -----------------------------------------------------------------------------
# Example use
# -----------------------------------------------------------------------------

# The physician-level table should contain as many of these fields as possible:
#
#   overlap_zone
#   listing_status
#   invalid_reason
#   location_valid
#   workforce_valid
#   office_answered
#   attempted
#   sampling_weight
#   ruca_binary
#   state
#   years_since_update OR nppes_last_update
#   credential_class
#   census_region
#   call_date
#
# The source-population table needs:
#
#   overlap_zone
#   population_n
#
# Example:
#
# timestamp <- base::format(
#   base::Sys.time(),
#   "%Y%m%d_%H%M%S"
# )
#
# figure_artifacts <- create_all_registry_figures(
#   listing_tbl = ent_listing_tbl,
#   population_tbl = ent_population_tbl,
#   save_dir = "output/figures",
#   timestamp = timestamp,
#   minimum_state_n = 5L
# )
