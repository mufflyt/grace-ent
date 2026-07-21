# =============================================================================
# ENT registry validity study: six publication-quality maps
#
# Maps created:
#   1. Access-penalty map
#   2. Population losing threshold access
#   3. Audited listing status point map
#   4. Rural single-point-failure map
#   5. State registry-corroboration map
#   6. Physician relocation flow map
#
# The travel-time maps require precomputed travel times. They do not estimate
# road-network travel time inside this script.
# =============================================================================

# -----------------------------------------------------------------------------
# Shared helpers
# -----------------------------------------------------------------------------

#' Confirm that required columns are available.
#'
#' @param tbl An object with column names.
#' @param required_cols Character vector of required columns.
#' @param object_name Name used in error messages.
#'
#' @return The input object, invisibly.
assert_columns <- function(
  tbl,
  required_cols,
  object_name = "object"
) {
  available_cols <- base::names(tbl)
  missing_cols <- base::setdiff(required_cols, available_cols)

  if (base::length(missing_cols) > 0L) {
    base::stop(
      base::sprintf(
        "%s is missing required columns: %s",
        object_name,
        base::paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  base::invisible(tbl)
}


#' Create a sortable timestamp for saved files.
#'
#' @return Character timestamp.
make_timestamp <- function() {
  base::format(base::Sys.time(), "%Y%m%d_%H%M%S")
}


#' Create a directory when it does not already exist.
#'
#' @param save_dir Directory for figure files.
#'
#' @return Normalized directory path.
ensure_save_dir <- function(save_dir) {
  if (!base::dir.exists(save_dir)) {
    base::message(
      base::sprintf("Creating figure directory: %s", save_dir)
    )
    base::dir.create(
      save_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  base::normalizePath(
    save_dir,
    winslash = "/",
    mustWork = TRUE
  )
}


#' Format a P value for manuscript-ready text.
#'
#' @param p_value Numeric P value.
#'
#' @return Formatted character value.
format_p_value <- function(p_value) {
  if (base::is.na(p_value)) {
    return("P = NA")
  }

  if (p_value < 0.001) {
    return("P < .001")
  }

  base::sprintf("P = %.3f", p_value)
}


#' Save one map as PNG and PDF.
#'
#' @param plot_obj A ggplot object.
#' @param file_stem Filename stem.
#' @param save_dir Directory for saved figures.
#' @param timestamp Timestamp shared across the map set.
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi PNG resolution.
#'
#' @return A tibble with saved paths.
save_map_plot <- function(
  plot_obj,
  file_stem,
  save_dir,
  timestamp,
  width = 10,
  height = 7,
  dpi = 600L
) {
  normalized_dir <- ensure_save_dir(save_dir)

  png_path <- base::file.path(
    normalized_dir,
    base::sprintf("%s_%s.png", file_stem, timestamp)
  )

  pdf_path <- base::file.path(
    normalized_dir,
    base::sprintf("%s_%s.pdf", file_stem, timestamp)
  )

  base::message(
    base::sprintf("Saving PNG: %s", png_path)
  )
  ggplot2::ggsave(
    filename = png_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white"
  )

  base::message(
    base::sprintf("Saving PDF: %s", pdf_path)
  )
  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot_obj,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    bg = "white"
  )

  tibble::tibble(
    figure = file_stem,
    format = base::c("PNG", "PDF"),
    path = base::c(png_path, pdf_path)
  )
}


#' Apply a clean national-map theme.
#'
#' @return A ggplot theme.
ent_map_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 14
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10,
        margin = ggplot2::margin(b = 8)
      )
    )
}


#' Convert common logical encodings to TRUE or FALSE.
#'
#' @param x Input vector.
#'
#' @return Logical vector.
as_logical_flag <- function(x) {
  normalized_x <- stringr::str_to_lower(
    stringr::str_trim(base::as.character(x))
  )

  normalized_x %in% base::c(
    "1",
    "true",
    "t",
    "yes",
    "y",
    "present",
    "in"
  )
}


#' Prepare tract-level access metrics.
#'
#' @param tract_sf Polygon sf object.
#' @param apparent_minutes_col Apparent travel-time column.
#' @param validated_minutes_col Validated travel-time column.
#' @param population_col Population column.
#' @param year_col Optional calendar-year column.
#'
#' @return An sf object with standardized columns.
prepare_access_metrics <- function(
  tract_sf,
  apparent_minutes_col = "apparent_minutes",
  validated_minutes_col = "validated_minutes",
  population_col = "population",
  year_col = NULL
) {
  required_cols <- base::c(
    apparent_minutes_col,
    validated_minutes_col,
    population_col
  )

  if (!base::is.null(year_col)) {
    required_cols <- base::c(required_cols, year_col)
  }

  assert_columns(
    tract_sf,
    required_cols,
    object_name = "tract_sf"
  )

  base::message("Standardizing tract-level access metrics.")

  access_sf <- tract_sf |>
    dplyr::mutate(
      apparent_minutes = base::as.numeric(
        .data[[apparent_minutes_col]]
      ),
      validated_minutes = base::as.numeric(
        .data[[validated_minutes_col]]
      ),
      tract_population = base::as.numeric(
        .data[[population_col]]
      ),
      no_validated_access = base::is.na(
        .data$validated_minutes
      ) |
        base::is.infinite(.data$validated_minutes),
      access_penalty_minutes = dplyr::if_else(
        .data$no_validated_access,
        NA_real_,
        base::pmax(
          .data$validated_minutes -
            .data$apparent_minutes,
          0
        )
      )
    )

  if (!base::is.null(year_col)) {
    access_sf <- access_sf |>
      dplyr::mutate(
        analysis_year = base::as.integer(
          .data[[year_col]]
        )
      )
  }

  base::message(
    base::sprintf(
      "Prepared %s tract records.",
      scales::comma(base::nrow(access_sf))
    )
  )

  access_sf
}


#' Build a dynamic summary sentence for access penalties.
#'
#' @param access_sf Standardized tract-level access metrics.
#' @param include_year_trend Whether to estimate a year trend.
#'
#' @return Character summary sentence.
build_access_penalty_sentence <- function(
  access_sf,
  include_year_trend = FALSE
) {
  penalty_tbl <- access_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      !base::is.na(.data$access_penalty_minutes),
      !base::is.na(.data$tract_population),
      .data$tract_population > 0
    )

  if (base::nrow(penalty_tbl) == 0L) {
    return("No finite access-penalty values were available.")
  }

  penalty_summary <- penalty_tbl |>
    dplyr::summarise(
      mean_penalty = stats::weighted.mean(
        .data$access_penalty_minutes,
        w = .data$tract_population,
        na.rm = TRUE
      ),
      penalty_sd = stats::sd(
        .data$access_penalty_minutes,
        na.rm = TRUE
      ),
      median_penalty = stats::median(
        .data$access_penalty_minutes,
        na.rm = TRUE
      ),
      p25_penalty = stats::quantile(
        .data$access_penalty_minutes,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      p75_penalty = stats::quantile(
        .data$access_penalty_minutes,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      )
    )

  base_sentence <- base::sprintf(
    paste0(
      "Mean access penalty was %.1f minutes (SD %.1f); ",
      "median %.1f (p25 %.1f, p75 %.1f)."
    ),
    penalty_summary$mean_penalty,
    penalty_summary$penalty_sd,
    penalty_summary$median_penalty,
    penalty_summary$p25_penalty,
    penalty_summary$p75_penalty
  )

  if (
    !include_year_trend ||
      !"analysis_year" %in% base::names(penalty_tbl)
  ) {
    return(base_sentence)
  }

  trend_tbl <- penalty_tbl |>
    dplyr::filter(!base::is.na(.data$analysis_year))

  year_values <- base::sort(
    base::unique(trend_tbl$analysis_year)
  )

  if (base::length(year_values) < 2L) {
    return(base_sentence)
  }

  trend_fit <- stats::lm(
    access_penalty_minutes ~ analysis_year,
    weights = tract_population,
    data = trend_tbl
  )

  trend_coef <- base::unname(
    stats::coef(trend_fit)[["analysis_year"]]
  )

  trend_p <- stats::coef(
    base::summary(trend_fit)
  )["analysis_year", "Pr(>|t|)"]

  direction_text <- dplyr::case_when(
    trend_coef > 0 ~ "increased",
    trend_coef < 0 ~ "decreased",
    TRUE ~ "did not change"
  )

  base::sprintf(
    paste0(
      "%s From %s to %s, the penalty %s by %.1f minutes ",
      "per year (%s)."
    ),
    base_sentence,
    scales::comma(base::min(year_values)),
    scales::comma(base::max(year_values)),
    direction_text,
    base::abs(trend_coef),
    format_p_value(trend_p)
  )
}


# -----------------------------------------------------------------------------
# Map 1: Access-penalty map
# -----------------------------------------------------------------------------

#' Map additional travel time after excluding unconfirmed listings.
#'
#' @param tract_sf Polygon sf object with precomputed travel times.
#' @param apparent_minutes_col Travel time using all listings.
#' @param validated_minutes_col Travel time using confirmed listings.
#' @param population_col Population column.
#' @param year_col Optional calendar-year column.
#' @param map_title Figure title.
#'
#' @return A ggplot object.
make_access_penalty_map <- function(
  tract_sf,
  apparent_minutes_col = "apparent_minutes",
  validated_minutes_col = "validated_minutes",
  population_col = "population",
  year_col = NULL,
  map_title = paste0(
    "Additional Travel Time After Excluding ",
    "Unconfirmed Otolaryngology Listings"
  )
) {
  base::message("Building Map 1: access penalty.")

  access_sf <- prepare_access_metrics(
    tract_sf = tract_sf,
    apparent_minutes_col = apparent_minutes_col,
    validated_minutes_col = validated_minutes_col,
    population_col = population_col,
    year_col = year_col
  ) |>
    dplyr::mutate(
      penalty_group = dplyr::case_when(
        .data$no_validated_access ~ "No validated ENT",
        .data$access_penalty_minutes == 0 ~ "No increase",
        .data$access_penalty_minutes <= 15 ~ "1-15 minutes",
        .data$access_penalty_minutes <= 30 ~ "16-30 minutes",
        .data$access_penalty_minutes <= 60 ~ "31-60 minutes",
        TRUE ~ ">60 minutes"
      ),
      penalty_group = base::factor(
        .data$penalty_group,
        levels = base::c(
          "No increase",
          "1-15 minutes",
          "16-30 minutes",
          "31-60 minutes",
          ">60 minutes",
          "No validated ENT"
        )
      )
    )

  summary_sentence <- build_access_penalty_sentence(
    access_sf,
    include_year_trend = !base::is.null(year_col)
  )

  base::message(summary_sentence)

  penalty_palette <- base::c(
    "No increase" = "#F2F2F2",
    "1-15 minutes" = "#FEE8C8",
    "16-30 minutes" = "#FDBB84",
    "31-60 minutes" = "#FC8D59",
    ">60 minutes" = "#D7301F",
    "No validated ENT" = "#54278F"
  )

  map_plot <- ggplot2::ggplot(access_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$penalty_group),
      color = NA
    ) +
    ggplot2::scale_fill_manual(
      values = penalty_palette,
      drop = FALSE,
      name = "Additional travel"
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = map_title,
      subtitle = summary_sentence,
      caption = paste0(
        "Apparent access uses all NPPES listings; validated ",
        "access uses location-confirmed listings."
      )
    ) +
    ent_map_theme()

  base::attr(map_plot, "summary_sentence") <- summary_sentence
  map_plot
}


# -----------------------------------------------------------------------------
# Map 2: Population losing threshold access
# -----------------------------------------------------------------------------

#' Map the population losing access within a travel-time threshold.
#'
#' @param tract_sf Polygon sf object with travel times and population.
#' @param apparent_minutes_col Travel time using all listings.
#' @param validated_minutes_col Travel time using confirmed listings.
#' @param population_col Population column.
#' @param threshold_minutes Access threshold in minutes.
#' @param map_title Figure title.
#'
#' @return A ggplot object.
make_population_losing_access_map <- function(
  tract_sf,
  apparent_minutes_col = "apparent_minutes",
  validated_minutes_col = "validated_minutes",
  population_col = "population",
  threshold_minutes = 60,
  map_title = "Population Losing Timely Otolaryngology Access"
) {
  base::message("Building Map 2: population losing access.")

  access_sf <- prepare_access_metrics(
    tract_sf = tract_sf,
    apparent_minutes_col = apparent_minutes_col,
    validated_minutes_col = validated_minutes_col,
    population_col = population_col
  ) |>
    dplyr::mutate(
      loses_threshold_access =
        .data$apparent_minutes <= threshold_minutes &
        (
          .data$validated_minutes > threshold_minutes |
            .data$no_validated_access
        ),
      affected_population = dplyr::if_else(
        .data$loses_threshold_access,
        .data$tract_population,
        NA_real_
      )
    )

  affected_total <- access_sf |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      affected_total = base::sum(
        .data$affected_population,
        na.rm = TRUE
      )
    ) |>
    dplyr::pull(.data$affected_total)

  affected_tracts <- access_sf |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      affected_tracts = base::sum(
        .data$loses_threshold_access,
        na.rm = TRUE
      )
    ) |>
    dplyr::pull(.data$affected_tracts)

  subtitle_text <- base::sprintf(
    paste0(
      "%s people in %s areas lose access within %s minutes ",
      "after unconfirmed listings are removed."
    ),
    scales::comma(affected_total),
    scales::comma(affected_tracts),
    scales::comma(threshold_minutes)
  )

  base::message(subtitle_text)

  ggplot2::ggplot(access_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$affected_population),
      color = NA
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "magma",
      trans = "sqrt",
      labels = scales::label_number(
        big.mark = ",",
        accuracy = 1
      ),
      na.value = "#F2F2F2",
      name = "People affected"
    ) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(
      title = map_title,
      subtitle = subtitle_text,
      caption = paste0(
        "Gray areas do not lose access within the selected ",
        "travel-time threshold."
      )
    ) +
    ent_map_theme()
}


# -----------------------------------------------------------------------------
# Map 3: Audited listing status point map
# -----------------------------------------------------------------------------

#' Normalize common listing-status labels.
#'
#' @param x Listing-status vector.
#'
#' @return Ordered factor.
normalize_listing_status <- function(x) {
  normalized_x <- stringr::str_to_lower(
    stringr::str_replace_all(
      base::as.character(x),
      "[^a-zA-Z0-9]+",
      "_"
    )
  )

  status_text <- dplyr::case_when(
    stringr::str_detect(
      normalized_x,
      "confirmed_valid|location_confirmed|location_valid"
    ) ~ "Location-confirmed",
    stringr::str_detect(
      normalized_x,
      "active_elsewhere|workforce_valid|relocat"
    ) ~ "Active elsewhere",
    stringr::str_detect(
      normalized_x,
      "retir|deceased|inactive|no_longer"
    ) ~ "Retired/inactive",
    stringr::str_detect(
      normalized_x,
      "unresolved|unknown"
    ) ~ "Unresolved",
    stringr::str_detect(
      normalized_x,
      "invalid|wrong|disconnect"
    ) ~ "Other invalid",
    TRUE ~ "Other/unknown"
  )

  base::factor(
    status_text,
    levels = base::c(
      "Location-confirmed",
      "Active elsewhere",
      "Retired/inactive",
      "Other invalid",
      "Unresolved",
      "Other/unknown"
    )
  )
}


#' Map audited physician listings by adjudicated status.
#'
#' @param audit_sf Point sf object.
#' @param states_sf State polygon sf object.
#' @param status_col Listing-status column.
#' @param zone_col Optional registry-overlap column.
#' @param facet_by_zone Whether to facet by overlap zone.
#' @param target_crs Projected CRS used for plotting.
#' @param map_title Figure title.
#'
#' @return A ggplot object.
make_audited_status_map <- function(
  audit_sf,
  states_sf,
  status_col = "listing_status",
  zone_col = "overlap_zone",
  facet_by_zone = TRUE,
  target_crs = 5070,
  map_title = "Audited Otolaryngology Listings by Validation Status"
) {
  base::message("Building Map 3: audited listing status.")

  required_cols <- status_col

  if (facet_by_zone) {
    required_cols <- base::c(required_cols, zone_col)
  }

  assert_columns(
    audit_sf,
    required_cols,
    object_name = "audit_sf"
  )

  states_map_sf <- sf::st_transform(states_sf, target_crs)

  audit_map_sf <- audit_sf |>
    sf::st_transform(target_crs) |>
    dplyr::mutate(
      map_status = normalize_listing_status(
        .data[[status_col]]
      )
    )

  if (facet_by_zone) {
    audit_map_sf <- audit_map_sf |>
      dplyr::mutate(
        map_zone = base::as.character(
          .data[[zone_col]]
        )
      )
  }

  status_palette <- base::c(
    "Location-confirmed" = "#1B9E77",
    "Active elsewhere" = "#377EB8",
    "Retired/inactive" = "#D95F02",
    "Other invalid" = "#E7298A",
    "Unresolved" = "#7570B3",
    "Other/unknown" = "#666666"
  )

  status_shapes <- base::c(
    "Location-confirmed" = 16,
    "Active elsewhere" = 17,
    "Retired/inactive" = 4,
    "Other invalid" = 15,
    "Unresolved" = 1,
    "Other/unknown" = 3
  )

  map_plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = states_map_sf,
      fill = "#F7F7F7",
      color = "#BDBDBD",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(
      data = audit_map_sf,
      ggplot2::aes(
        color = .data$map_status,
        shape = .data$map_status
      ),
      size = 1.6,
      alpha = 0.75,
      stroke = 0.7
    ) +
    ggplot2::scale_color_manual(
      values = status_palette,
      drop = FALSE,
      name = "Audit status"
    ) +
    ggplot2::scale_shape_manual(
      values = status_shapes,
      drop = FALSE,
      name = "Audit status"
    ) +
    ggplot2::coord_sf(
      crs = target_crs,
      datum = NA
    ) +
    ggplot2::labs(
      title = map_title,
      subtitle = paste0(
        "Each point represents one audited physician-location ",
        "listing."
      )
    ) +
    ent_map_theme()

  if (facet_by_zone) {
    map_plot <- map_plot +
      ggplot2::facet_wrap(
        ggplot2::vars(.data$map_zone)
      ) +
      ggplot2::theme(
        legend.position = "bottom"
      )
  }

  map_plot
}


# -----------------------------------------------------------------------------
# Map 4: Rural single-point-failure map
# -----------------------------------------------------------------------------

#' Map rural areas dependent on one unconfirmed ENT listing.
#'
#' @param region_sf Rural tract or county polygon sf object.
#' @param rural_col Rural indicator.
#' @param apparent_count_col Count within the threshold before validation.
#' @param validated_count_col Count within the threshold after validation.
#' @param population_col Population column.
#' @param region_name_col Optional region-name column.
#' @param threshold_minutes Travel-time threshold represented by counts.
#' @param label_top_n Number of highest-population failures to label.
#' @param target_crs Projected CRS used for plotting.
#' @param map_title Figure title.
#'
#' @return A ggplot object.
make_rural_single_point_failure_map <- function(
  region_sf,
  rural_col = "rural_flag",
  apparent_count_col = "apparent_ent_count",
  validated_count_col = "validated_ent_count",
  population_col = "population",
  region_name_col = NULL,
  threshold_minutes = 60,
  label_top_n = 8L,
  target_crs = 5070,
  map_title = "Rural Otolaryngology Single-Point Failures"
) {
  base::message("Building Map 4: rural single-point failures.")

  required_cols <- base::c(
    rural_col,
    apparent_count_col,
    validated_count_col,
    population_col
  )

  if (!base::is.null(region_name_col)) {
    required_cols <- base::c(
      required_cols,
      region_name_col
    )
  }

  assert_columns(
    region_sf,
    required_cols,
    object_name = "region_sf"
  )

  failure_sf <- region_sf |>
    sf::st_transform(target_crs) |>
    dplyr::mutate(
      rural_flag_std = as_logical_flag(
        .data[[rural_col]]
      ),
      apparent_ent_count = base::as.numeric(
        .data[[apparent_count_col]]
      ),
      validated_ent_count = base::as.numeric(
        .data[[validated_count_col]]
      ),
      region_population = base::as.numeric(
        .data[[population_col]]
      ),
      single_point_failure =
        .data$rural_flag_std &
        .data$apparent_ent_count == 1 &
        .data$validated_ent_count == 0,
      failure_group = dplyr::case_when(
        .data$single_point_failure ~
          "Rural single-point failure",
        .data$rural_flag_std ~
          "Other rural area",
        TRUE ~
          "Nonrural"
      ),
      failure_group = base::factor(
        .data$failure_group,
        levels = base::c(
          "Nonrural",
          "Other rural area",
          "Rural single-point failure"
        )
      )
    )

  failure_summary <- failure_sf |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      failure_regions = base::sum(
        .data$single_point_failure,
        na.rm = TRUE
      ),
      affected_population = base::sum(
        dplyr::if_else(
          .data$single_point_failure,
          .data$region_population,
          0
        ),
        na.rm = TRUE
      )
    )

  subtitle_text <- base::sprintf(
    paste0(
      "%s rural areas containing %s people appear to depend ",
      "on one ENT listing within %s minutes."
    ),
    scales::comma(failure_summary$failure_regions),
    scales::comma(failure_summary$affected_population),
    scales::comma(threshold_minutes)
  )

  base::message(subtitle_text)

  failure_palette <- base::c(
    "Nonrural" = "#F2F2F2",
    "Other rural area" = "#BFD3E6",
    "Rural single-point failure" = "#D7301F"
  )

  map_plot <- ggplot2::ggplot(failure_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$failure_group),
      color = NA
    ) +
    ggplot2::scale_fill_manual(
      values = failure_palette,
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::coord_sf(
      crs = target_crs,
      datum = NA
    ) +
    ggplot2::labs(
      title = map_title,
      subtitle = subtitle_text,
      caption = paste0(
        "A single-point failure has exactly one apparent ENT ",
        "and no location-confirmed ENT within the threshold."
      )
    ) +
    ent_map_theme() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  if (
    !base::is.null(region_name_col) &&
      label_top_n > 0L
  ) {
    label_sf <- failure_sf |>
      dplyr::filter(.data$single_point_failure) |>
      dplyr::slice_max(
        order_by = .data$region_population,
        n = label_top_n,
        with_ties = FALSE
      ) |>
      sf::st_point_on_surface() |>
      dplyr::mutate(
        map_label = base::as.character(
          .data[[region_name_col]]
        )
      )

    map_plot <- map_plot +
      ggplot2::geom_sf_text(
        data = label_sf,
        ggplot2::aes(label = .data$map_label),
        size = 2.6,
        check_overlap = TRUE
      )
  }

  map_plot
}


# -----------------------------------------------------------------------------
# Map 5: State registry-corroboration map
# -----------------------------------------------------------------------------

#' Map registry corroboration and ENT supply by state.
#'
#' @param states_sf State polygon sf object.
#' @param listing_sf Physician point sf object or sf table.
#' @param state_population_tbl State population table.
#' @param state_geometry_id_col State ID in states_sf.
#' @param listing_state_col State ID in listing_sf.
#' @param population_state_col State ID in population table.
#' @param state_population_col Population in population table.
#' @param in_aboto_col ABOto-presence indicator.
#' @param in_enthealth_col ENTHealth-presence indicator.
#' @param corroboration_mode Use any corroboration or triple match.
#' @param target_crs Projected CRS used for plotting.
#' @param map_title Figure title.
#'
#' @return A ggplot object.
make_state_corroboration_map <- function(
  states_sf,
  listing_sf,
  state_population_tbl,
  state_geometry_id_col = "STUSPS",
  listing_state_col = "state",
  population_state_col = "state",
  state_population_col = "population",
  in_aboto_col = "in_aboto",
  in_enthealth_col = "in_enthealth",
  corroboration_mode = base::c("any", "triple"),
  target_crs = 5070,
  map_title = "Registry Corroboration and Otolaryngology Supply"
) {
  corroboration_mode <- base::match.arg(
    corroboration_mode
  )

  base::message("Building Map 5: state registry corroboration.")

  assert_columns(
    states_sf,
    state_geometry_id_col,
    object_name = "states_sf"
  )

  assert_columns(
    listing_sf,
    base::c(
      listing_state_col,
      in_aboto_col,
      in_enthealth_col
    ),
    object_name = "listing_sf"
  )

  assert_columns(
    state_population_tbl,
    base::c(
      population_state_col,
      state_population_col
    ),
    object_name = "state_population_tbl"
  )

  listing_tbl <- listing_sf |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      state_id = base::as.character(
        .data[[listing_state_col]]
      ),
      in_aboto = as_logical_flag(
        .data[[in_aboto_col]]
      ),
      in_enthealth = as_logical_flag(
        .data[[in_enthealth_col]]
      )
    )

  state_registry_tbl <- listing_tbl |>
    dplyr::group_by(.data$state_id) |>
    dplyr::summarise(
      n_nppes = dplyr::n(),
      n_any_corroborated = base::sum(
        .data$in_aboto | .data$in_enthealth,
        na.rm = TRUE
      ),
      n_triple = base::sum(
        .data$in_aboto & .data$in_enthealth,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  state_pop_tbl <- state_population_tbl |>
    dplyr::transmute(
      state_id = base::as.character(
        .data[[population_state_col]]
      ),
      state_population = base::as.numeric(
        .data[[state_population_col]]
      )
    )

  state_registry_tbl <- state_registry_tbl |>
    dplyr::left_join(
      state_pop_tbl,
      by = "state_id"
    ) |>
    dplyr::mutate(
      corroborated_n = if (corroboration_mode == "triple") {
        .data$n_triple
      } else {
        .data$n_any_corroborated
      },
      corroboration_pct = dplyr::if_else(
        .data$n_nppes > 0,
        .data$corroborated_n / .data$n_nppes,
        NA_real_
      ),
      ent_per_100k = dplyr::if_else(
        .data$state_population > 0,
        100000 * .data$n_nppes /
          .data$state_population,
        NA_real_
      )
    )

  state_map_sf <- states_sf |>
    dplyr::mutate(
      state_id = base::as.character(
        .data[[state_geometry_id_col]]
      )
    ) |>
    dplyr::left_join(
      state_registry_tbl,
      by = "state_id"
    ) |>
    sf::st_transform(target_crs)

  state_point_sf <- state_map_sf |>
    dplyr::filter(!base::is.na(.data$ent_per_100k)) |>
    sf::st_point_on_surface()

  state_summary <- state_registry_tbl |>
    dplyr::summarise(
      mean_pct = base::mean(
        .data$corroboration_pct,
        na.rm = TRUE
      ),
      sd_pct = stats::sd(
        .data$corroboration_pct,
        na.rm = TRUE
      ),
      median_pct = stats::median(
        .data$corroboration_pct,
        na.rm = TRUE
      ),
      p25_pct = stats::quantile(
        .data$corroboration_pct,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      p75_pct = stats::quantile(
        .data$corroboration_pct,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      )
    )

  subtitle_text <- base::sprintf(
    paste0(
      "Mean state corroboration was %.1f%% (SD %.1f%%); ",
      "median %.1f%% (p25 %.1f%%, p75 %.1f%%)."
    ),
    100 * state_summary$mean_pct,
    100 * state_summary$sd_pct,
    100 * state_summary$median_pct,
    100 * state_summary$p25_pct,
    100 * state_summary$p75_pct
  )

  base::message(subtitle_text)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = state_map_sf,
      ggplot2::aes(fill = .data$corroboration_pct),
      color = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_sf(
      data = state_point_sf,
      ggplot2::aes(size = .data$ent_per_100k),
      shape = 21,
      fill = "white",
      color = "black",
      alpha = 0.8,
      stroke = 0.3
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "viridis",
      labels = scales::label_percent(
        accuracy = 1
      ),
      limits = base::c(0, 1),
      na.value = "#E6E6E6",
      name = "Listings corroborated"
    ) +
    ggplot2::scale_size_continuous(
      range = base::c(1.5, 7),
      name = "ENTs per 100,000"
    ) +
    ggplot2::coord_sf(
      crs = target_crs,
      datum = NA
    ) +
    ggplot2::labs(
      title = map_title,
      subtitle = subtitle_text,
      caption = paste0(
        "Polygon color shows registry corroboration; circle ",
        "size shows NPPES-listed otolaryngologists per 100,000."
      )
    ) +
    ent_map_theme()
}


# -----------------------------------------------------------------------------
# Map 6: Physician relocation flow map
# -----------------------------------------------------------------------------

#' Build state centroid coordinates.
#'
#' @param states_sf State polygon sf object.
#' @param state_geometry_id_col State ID column.
#' @param target_crs Projected CRS.
#'
#' @return A tibble of state IDs and centroid coordinates.
build_state_centers <- function(
  states_sf,
  state_geometry_id_col = "STUSPS",
  target_crs = 5070
) {
  state_point_sf <- states_sf |>
    dplyr::mutate(
      state_id = base::as.character(
        .data[[state_geometry_id_col]]
      )
    ) |>
    sf::st_transform(target_crs) |>
    sf::st_point_on_surface()

  center_matrix <- sf::st_coordinates(state_point_sf)

  dplyr::bind_cols(
    sf::st_drop_geometry(state_point_sf) |>
      dplyr::select(.data$state_id),
    tibble::tibble(
      center_x = center_matrix[, "X"],
      center_y = center_matrix[, "Y"]
    )
  )
}


#' Map physician relocation flows between states.
#'
#' @param relocation_tbl Relocation record table.
#' @param states_sf State polygon sf object.
#' @param origin_state_col Origin state.
#' @param destination_state_col Destination state.
#' @param state_geometry_id_col State ID in states_sf.
#' @param minimum_flow_n Minimum interstate flow count to draw.
#' @param target_crs Projected CRS used for plotting.
#' @param map_title Figure title.
#'
#' @return A ggplot object.
make_relocation_flow_map <- function(
  relocation_tbl,
  states_sf,
  origin_state_col = "origin_state",
  destination_state_col = "destination_state",
  state_geometry_id_col = "STUSPS",
  minimum_flow_n = 1L,
  target_crs = 5070,
  map_title = "Otolaryngologist Relocation Flows"
) {
  base::message("Building Map 6: relocation flows.")

  assert_columns(
    relocation_tbl,
    base::c(
      origin_state_col,
      destination_state_col
    ),
    object_name = "relocation_tbl"
  )

  state_centers_tbl <- build_state_centers(
    states_sf = states_sf,
    state_geometry_id_col = state_geometry_id_col,
    target_crs = target_crs
  )

  relocation_clean_tbl <- relocation_tbl |>
    dplyr::transmute(
      origin_state = base::as.character(
        .data[[origin_state_col]]
      ),
      destination_state = base::as.character(
        .data[[destination_state_col]]
      )
    ) |>
    dplyr::filter(
      !base::is.na(.data$origin_state),
      !base::is.na(.data$destination_state)
    )

  within_state_n <- relocation_clean_tbl |>
    dplyr::summarise(
      within_state_n = base::sum(
        .data$origin_state ==
          .data$destination_state
      )
    ) |>
    dplyr::pull(.data$within_state_n)

  interstate_flow_tbl <- relocation_clean_tbl |>
    dplyr::filter(
      .data$origin_state !=
        .data$destination_state
    ) |>
    dplyr::count(
      .data$origin_state,
      .data$destination_state,
      name = "relocation_n"
    ) |>
    dplyr::filter(
      .data$relocation_n >= minimum_flow_n
    )

  origin_centers_tbl <- state_centers_tbl |>
    dplyr::rename(
      origin_state = .data$state_id,
      origin_x = .data$center_x,
      origin_y = .data$center_y
    )

  destination_centers_tbl <- state_centers_tbl |>
    dplyr::rename(
      destination_state = .data$state_id,
      destination_x = .data$center_x,
      destination_y = .data$center_y
    )

  interstate_flow_tbl <- interstate_flow_tbl |>
    dplyr::left_join(
      origin_centers_tbl,
      by = "origin_state"
    ) |>
    dplyr::left_join(
      destination_centers_tbl,
      by = "destination_state"
    ) |>
    dplyr::filter(
      !base::is.na(.data$origin_x),
      !base::is.na(.data$destination_x)
    )

  states_map_sf <- sf::st_transform(
    states_sf,
    target_crs
  )

  subtitle_text <- base::sprintf(
    paste0(
      "%s interstate relocation routes are shown; ",
      "%s relocations remained within the same state."
    ),
    scales::comma(base::nrow(interstate_flow_tbl)),
    scales::comma(within_state_n)
  )

  base::message(subtitle_text)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = states_map_sf,
      fill = "#F7F7F7",
      color = "#BDBDBD",
      linewidth = 0.3
    ) +
    ggplot2::geom_curve(
      data = interstate_flow_tbl,
      ggplot2::aes(
        x = .data$origin_x,
        y = .data$origin_y,
        xend = .data$destination_x,
        yend = .data$destination_y,
        linewidth = .data$relocation_n,
        alpha = .data$relocation_n
      ),
      curvature = 0.18,
      color = "#2C7FB8",
      arrow = grid::arrow(
        length = grid::unit(0.08, "inches"),
        type = "closed"
      ),
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = interstate_flow_tbl |>
        dplyr::distinct(
          .data$origin_state,
          .data$origin_x,
          .data$origin_y
        ),
      ggplot2::aes(
        x = .data$origin_x,
        y = .data$origin_y
      ),
      shape = 21,
      fill = "white",
      color = "#2C7FB8",
      size = 2,
      stroke = 0.5
    ) +
    ggplot2::scale_linewidth_continuous(
      range = base::c(0.35, 3.5),
      name = "Relocations"
    ) +
    ggplot2::scale_alpha_continuous(
      range = base::c(0.15, 0.75),
      guide = "none"
    ) +
    ggplot2::coord_sf(
      crs = target_crs,
      datum = NA
    ) +
    ggplot2::labs(
      title = map_title,
      subtitle = subtitle_text,
      caption = paste0(
        "Routes are aggregated by origin and destination state. ",
        "Line width is proportional to the number of physicians."
      )
    ) +
    ent_map_theme()
}


# -----------------------------------------------------------------------------
# Wrapper: create and save all six maps
# -----------------------------------------------------------------------------

#' Create and save the six ENT registry maps.
#'
#' @param tract_sf Tract polygons with apparent and validated travel times.
#' @param audit_sf Audited physician point sf object.
#' @param rural_region_sf Rural tract or county polygon sf object.
#' @param states_sf State polygon sf object.
#' @param listing_sf NPPES physician point sf object.
#' @param state_population_tbl State population table.
#' @param relocation_tbl Physician relocation records.
#' @param save_dir Directory for saved map files.
#' @param timestamp Optional shared filename timestamp.
#' @param threshold_minutes Access threshold for Maps 2 and 4.
#'
#' @return A list containing plots and saved-file metadata.
create_six_ent_maps <- function(
  tract_sf,
  audit_sf,
  rural_region_sf,
  states_sf,
  listing_sf,
  state_population_tbl,
  relocation_tbl,
  save_dir = "figures",
  timestamp = make_timestamp(),
  threshold_minutes = 60
) {
  base::message("Starting six-map ENT registry workflow.")
  base::message(
    base::sprintf(
      "Shared filename timestamp: %s",
      timestamp
    )
  )

  access_penalty_plot <- make_access_penalty_map(
    tract_sf = tract_sf
  )

  losing_access_plot <- make_population_losing_access_map(
    tract_sf = tract_sf,
    threshold_minutes = threshold_minutes
  )

  audit_status_plot <- make_audited_status_map(
    audit_sf = audit_sf,
    states_sf = states_sf
  )

  single_failure_plot <- make_rural_single_point_failure_map(
    region_sf = rural_region_sf,
    threshold_minutes = threshold_minutes
  )

  corroboration_plot <- make_state_corroboration_map(
    states_sf = states_sf,
    listing_sf = listing_sf,
    state_population_tbl = state_population_tbl
  )

  relocation_plot <- make_relocation_flow_map(
    relocation_tbl = relocation_tbl,
    states_sf = states_sf
  )

  map_artifacts <- dplyr::bind_rows(
    save_map_plot(
      plot_obj = access_penalty_plot,
      file_stem = "map_01_access_penalty",
      save_dir = save_dir,
      timestamp = timestamp,
      width = 11,
      height = 7
    ),
    save_map_plot(
      plot_obj = losing_access_plot,
      file_stem = "map_02_population_losing_access",
      save_dir = save_dir,
      timestamp = timestamp,
      width = 11,
      height = 7
    ),
    save_map_plot(
      plot_obj = audit_status_plot,
      file_stem = "map_03_audited_listing_status",
      save_dir = save_dir,
      timestamp = timestamp,
      width = 12,
      height = 8
    ),
    save_map_plot(
      plot_obj = single_failure_plot,
      file_stem = "map_04_rural_single_point_failure",
      save_dir = save_dir,
      timestamp = timestamp,
      width = 11,
      height = 7
    ),
    save_map_plot(
      plot_obj = corroboration_plot,
      file_stem = "map_05_state_registry_corroboration",
      save_dir = save_dir,
      timestamp = timestamp,
      width = 11,
      height = 7
    ),
    save_map_plot(
      plot_obj = relocation_plot,
      file_stem = "map_06_relocation_flows",
      save_dir = save_dir,
      timestamp = timestamp,
      width = 11,
      height = 7
    )
  )

  base::message("Completed all six ENT registry maps.")
  base::message(
    base::sprintf(
      "Saved %s files.",
      scales::comma(base::nrow(map_artifacts))
    )
  )

  base::list(
    plots = base::list(
      access_penalty = access_penalty_plot,
      population_losing_access = losing_access_plot,
      audited_listing_status = audit_status_plot,
      rural_single_point_failure = single_failure_plot,
      state_registry_corroboration = corroboration_plot,
      relocation_flows = relocation_plot
    ),
    artifacts = map_artifacts
  )
}


# -----------------------------------------------------------------------------
# Expected standardized columns
# -----------------------------------------------------------------------------
#
# tract_sf:
#   apparent_minutes
#   validated_minutes
#   population
#
# audit_sf:
#   listing_status
#   overlap_zone
#
# rural_region_sf:
#   rural_flag
#   apparent_ent_count
#   validated_ent_count
#   population
#
# states_sf:
#   STUSPS
#
# listing_sf:
#   state
#   in_aboto
#   in_enthealth
#
# state_population_tbl:
#   state
#   population
#
# relocation_tbl:
#   origin_state
#   destination_state
#
# Example:
#
# map_bundle <- create_six_ent_maps(
#   tract_sf = ent_tract_access_sf,
#   audit_sf = ent_audit_sf,
#   rural_region_sf = rural_access_sf,
#   states_sf = us_states_sf,
#   listing_sf = all_nppes_ent_sf,
#   state_population_tbl = state_pop_tbl,
#   relocation_tbl = ent_relocation_tbl,
#   save_dir = "figures"
# )
#
# map_bundle$artifacts
