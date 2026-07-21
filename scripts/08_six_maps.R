# =============================================================================
# 08_six_maps.R
# Generates six publication maps for the registry validity manuscript.
#
# Maps produced now (data available):
#   3. Audited listing-status point map
#   5. State registry-corroboration map
#   6. Physician relocation-flow map
#
# Maps deferred (require ENT isochrone travel-time data):
#   1. Access-penalty map         — needs apparent_minutes / validated_minutes
#   2. Population losing access   — needs apparent_minutes / validated_minutes
#   4. Rural single-point-failure — needs apparent_ent_count / validated_ent_count
#
# Run after 01_clean_phase2_call_log.R.
# Output: output/figures/  (PNG 600 DPI + PDF, timestamped)
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(tigris)
library(tidycensus)

source(here("scripts", "ent_registry_six_maps.R"))

options(tigris_use_cache = TRUE)

# =============================================================================
# 1. LOAD CALL LOG
# =============================================================================
f_all <- tail(
  sort(list.files(here("data", "processed"),
                  pattern = "ent_phase2_all", full.names = TRUE)),
  1L
)
calls <- read_csv(f_all, show_col_types = FALSE) |>
  mutate(
    npi = as.character(npi),
    listing_status = case_when(
      taking_new_patients %in% c(
        "Yes",
        "No-practice is full but physician does still see established patients",
        "No-on leave of absence (like parental leave)"
      )                                                        ~ "confirmed_valid",
      taking_new_patients %in% c(
        "No-changed practices",
        "No-retired or no longer practicing medicine"
      )                                                        ~ "confirmed_invalid",
      phone_validity_flag == "invalid_format"                  ~ "confirmed_invalid",
      is.na(office_answered) & is.na(taking_new_patients)     ~ "not_attempted",
      TRUE                                                     ~ "unresolved"
    ),
    # Granular status for Map 3: normalize_listing_status() distinguishes
    # "relocat" → Active elsewhere, "retir" → Retired/inactive,
    # "wrong|invalid" → Other invalid.
    listing_status_detail = case_when(
      taking_new_patients %in% c(
        "Yes",
        "No-practice is full but physician does still see established patients",
        "No-on leave of absence (like parental leave)"
      )                                                        ~ "confirmed_valid",
      taking_new_patients == "No-changed practices"            ~ "relocated",
      taking_new_patients == "No-retired or no longer practicing medicine"
                                                               ~ "retired_inactive",
      phone_validity_flag == "invalid_format"                  ~ "wrong_number",
      is.na(office_answered) & is.na(taking_new_patients)     ~ "not_attempted",
      TRUE                                                     ~ "unresolved"
    ),
    overlap_zone = case_when(
      datasets %in% c("ABO, NPI but not ENT Health",
                      "ABO, NPI but no ENTHealth")            ~ "NPPES + ABOto",
      datasets == "ABO, NPI, ENT Health"                      ~ "Triple match",
      TRUE                                                     ~ NA_character_
    )
  )

message(sprintf("Call log loaded: %d records", nrow(calls)))

# =============================================================================
# 2. LOAD EULER DEMOGRAPHICS
# =============================================================================
euler_path <- path.expand(paste0(
  "~/isochrones/publication_materials/figures/",
  "ent_data_integrity/ent_euler_zone_npi_assignments_with_demographics.csv"
))
euler <- read_csv(euler_path, show_col_types = FALSE) |>
  rename(npi = npi_chr) |>
  mutate(npi = as.character(npi))

# =============================================================================
# 3. LOAD SUPPORTING SPATIAL FILES
# =============================================================================
addr_path <- path.expand(paste0(
  "~/isochrones/publication_materials/figures/",
  "ent_data_integrity/primary_addresses_cohort_n11333.csv"
))
addr <- read_csv(addr_path, show_col_types = FALSE) |>
  mutate(
    npi  = as.character(npi),
    zip5 = str_pad(as.character(primary_zip5), 5, "left", "0")
  )

zcta_path <- path.expand(paste0(
  "~/isochrones/publication_materials/figures/",
  "secondary_locations/.cache_zcta_centroids_2020.csv"
))
zcta_centroids <- read_csv(zcta_path, show_col_types = FALSE) |>
  mutate(zip5 = str_pad(as.character(zip5), 5, "left", "0"))

# =============================================================================
# 4. BUILD STATES SF AND STATE POPULATION TABLE
# =============================================================================
excluded_territories <- c("PR", "VI", "GU", "MP", "AS")

states_sf <- states(cb = TRUE, year = 2020, progress_bar = FALSE) |>
  filter(!STUSPS %in% excluded_territories)

state_pop_raw <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year      = 2020,
  survey    = "acs5",
  progress_bar = FALSE
)

state_pop_tbl <- state_pop_raw |>
  select(NAME, estimate) |>
  left_join(
    states_sf |> st_drop_geometry() |> select(NAME, STUSPS),
    by = "NAME"
  ) |>
  filter(!is.na(STUSPS)) |>
  transmute(state = STUSPS, population = estimate)

message(sprintf("State population table: %d states", nrow(state_pop_tbl)))

# =============================================================================
# 5. BUILD STATE CENTROIDS FOR COORDINATE FALLBACK
# =============================================================================
state_centroid_coords <- states_sf |>
  st_point_on_surface() |>
  st_coordinates() |>
  as_tibble() |>
  rename(state_lon = X, state_lat = Y)

state_centroids_tbl <- states_sf |>
  st_drop_geometry() |>
  select(STUSPS) |>
  bind_cols(state_centroid_coords)

# =============================================================================
# 6. BUILD audit_sf  (Map 3: audited listing-status point map)
# =============================================================================
# State abbreviation → full name lookup (includes DC)
state_name_lut <- c(
  setNames(state.name, state.abb),
  "DC" = "District of Columbia"
)

audit_tbl <- calls |>
  filter(listing_status != "not_attempted") |>
  mutate(zip5 = str_pad(as.character(zip), 5, "left", "0")) |>
  left_join(addr |> select(npi, primary_state), by = "npi") |>
  left_join(zcta_centroids |> select(zip5, longitude, latitude), by = "zip5") |>
  left_join(state_centroids_tbl, by = c("state" = "STUSPS")) |>
  # Join Euler flags for database-level incorrect-listing indicators
  left_join(
    euler |> select(npi, in_board, in_ent),
    by = "npi"
  ) |>
  mutate(
    longitude  = coalesce(longitude, state_lon),
    latitude   = coalesce(latitude,  state_lat),
    state_full = dplyr::recode(state, !!!state_name_lut, .default = state),
    # Is this listing incorrect in each database?
    nppes_incorrect = listing_status_detail %in%
      c("relocated", "retired_inactive", "wrong_number"),
    aboto_incorrect = dplyr::case_when(
      !in_board  ~ "Not in ABOto",
      nppes_incorrect ~ "Yes — listed but unconfirmed at this location",
      TRUE        ~ "No — listing appears valid"
    ),
    enthealth_incorrect = dplyr::case_when(
      !in_ent    ~ "Not in ENTHealth",
      nppes_incorrect ~ "Yes — listed but unconfirmed at this location",
      TRUE        ~ "No — listing appears valid"
    ),
    nppes_incorrect_label = ifelse(
      nppes_incorrect,
      "Yes — address not confirmed",
      "No — address confirmed"
    ),
    relocated_to = ifelse(
      listing_status_detail == "relocated",
      "Destination not captured in call protocol",
      NA_character_
    )
  ) |>
  filter(!is.na(longitude), !is.na(latitude))

audit_sf <- st_as_sf(
  audit_tbl,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)

message(sprintf(
  "audit_sf: %d attempted listings geocoded", nrow(audit_sf)
))

# =============================================================================
# 7. BUILD listing_sf  (Map 5: state corroboration map)
# =============================================================================
listing_tbl <- euler |>
  left_join(
    addr |> select(npi, zip5, primary_state),
    by = "npi"
  ) |>
  left_join(zcta_centroids |> select(zip5, longitude, latitude), by = "zip5") |>
  left_join(
    state_centroids_tbl,
    by = c("state_final" = "STUSPS")
  ) |>
  mutate(
    longitude    = coalesce(longitude, state_lon),
    latitude     = coalesce(latitude,  state_lat),
    state        = coalesce(primary_state, state_final),
    in_aboto     = in_board,
    in_enthealth = in_ent
  ) |>
  filter(!is.na(longitude), !is.na(latitude), !is.na(state))

listing_sf <- st_as_sf(
  listing_tbl,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)

message(sprintf(
  "listing_sf: %d NPPES physicians with coordinates", nrow(listing_sf)
))

# =============================================================================
# 8. BUILD relocation_tbl  (Map 6: relocation-flow map)
# =============================================================================
# Only physicians who said "changed practices" are confirmed to have relocated.
# The call protocol did not capture destination state, so destination is NA.
# Map 6 will render with state nodes but no flow arrows until destination
# data is added.
relocation_tbl <- calls |>
  filter(taking_new_patients == "No-changed practices") |>
  transmute(
    origin_state      = state,
    destination_state = NA_character_
  )

message(sprintf(
  "relocation_tbl: %d relocated physicians (destination state not captured)",
  nrow(relocation_tbl)
))

# =============================================================================
# 9. GENERATE AVAILABLE MAPS (3, 5, 6)
# =============================================================================
save_dir  <- here("output", "figures")
timestamp <- make_timestamp()

message(
  "\nMaps 1, 2, and 4 require precomputed isochrone travel-time data ",
  "(apparent_minutes, validated_minutes, population at tract/county level). ",
  "These will be generated once the ENT isochrone workflow is complete.\n"
)

# --- Map 3: Audited listing-status point map ---------------------------------
# Restrict to contiguous US (exclude AK and HI for a cleaner figure).
conus_states_sf  <- states_sf |> filter(!STUSPS %in% c("AK", "HI"))
conus_audit_sf   <- audit_sf  |> filter(!state %in% c("AK", "HI"))

message("Generating Map 3: audited listing status.")
map3 <- tryCatch(
  {
    status_palette <- c(
      "Location-confirmed" = "#1B9E77",
      "Active elsewhere"   = "#377EB8",
      "Retired/inactive"   = "#D95F02",
      "Other invalid"      = "#E7298A",
      "Unresolved"         = "#7570B3",
      "Other/unknown"      = "#666666"
    )
    status_shapes <- c(
      "Location-confirmed" = 16,
      "Active elsewhere"   = 17,
      "Retired/inactive"   = 18,
      "Other invalid"      = 15,
      "Unresolved"         = 1,
      "Other/unknown"      = 8
    )
    make_audited_status_map(
      audit_sf      = conus_audit_sf,
      states_sf     = conus_states_sf,
      status_col    = "listing_status_detail",
      facet_by_zone = TRUE
    ) +
      # drop=TRUE removes legend entries with zero observations
      ggplot2::scale_color_manual(
        values = status_palette, drop = TRUE, name = "Audit result"
      ) +
      ggplot2::scale_shape_manual(
        values = status_shapes, drop = TRUE, name = "Audit result"
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          title.hjust  = 0,
          override.aes = list(size = 3.5, alpha = 1),
          ncol         = 1
        ),
        shape = ggplot2::guide_legend(
          title.hjust  = 0,
          override.aes = list(size = 3.5, alpha = 1),
          ncol         = 1
        )
      ) +
      ggplot2::theme(
        legend.position   = "right",
        legend.title      = ggplot2::element_text(face = "bold", size = 10),
        legend.text       = ggplot2::element_text(size = 9),
        legend.key.height = ggplot2::unit(0.55, "cm"),
        strip.text        = ggplot2::element_text(face = "bold", size = 11)
      )
  },
  error = function(e) {
    message("Map 3 failed: ", e$message)
    NULL
  }
)
if (!is.null(map3)) {
  save_map_plot(
    plot_obj  = map3,
    file_stem = "map_03_audited_listing_status",
    save_dir  = save_dir,
    timestamp = timestamp,
    width     = 13,
    height    = 7
  )
  message("Map 3 complete.")
}

# --- Map 5: State registry-corroboration map ---------------------------------
# Restrict to contiguous US; relabel bubble legend to clarify denominator.
conus_listing_sf <- listing_sf |> filter(!state %in% c("AK", "HI"))

message("Generating Map 5: state registry corroboration.")
map5 <- tryCatch(
  make_state_corroboration_map(
    states_sf            = conus_states_sf,
    listing_sf           = conus_listing_sf,
    state_population_tbl = state_pop_tbl,
    in_aboto_col         = "in_aboto",
    in_enthealth_col     = "in_enthealth"
  ) +
    ggplot2::scale_size_continuous(
      range = c(1.5, 7),
      name  = "ENTs per\n100,000 residents"
    ),
  error = function(e) {
    message("Map 5 failed: ", e$message)
    NULL
  }
)
if (!is.null(map5)) {
  save_map_plot(
    plot_obj  = map5,
    file_stem = "map_05_state_registry_corroboration",
    save_dir  = save_dir,
    timestamp = timestamp,
    width     = 11,
    height    = 7
  )
  message("Map 5 complete.")
}

# --- Map 6: Relocation-flow map ----------------------------------------------
message("Generating Map 6: relocation flows.")
map6 <- tryCatch(
  make_relocation_flow_map(
    relocation_tbl = relocation_tbl,
    states_sf      = states_sf
  ),
  error = function(e) {
    message("Map 6 failed: ", e$message)
    NULL
  }
)
if (!is.null(map6)) {
  save_map_plot(
    plot_obj  = map6,
    file_stem = "map_06_relocation_flows",
    save_dir  = save_dir,
    timestamp = timestamp,
    width     = 11,
    height    = 7
  )
  message("Map 6 complete.")
}

message("\nAll available maps saved to: ", save_dir)

# =============================================================================
# 10. LEAFLET INTERACTIVE MAP (Map 3 — for debugging and exploration)
# =============================================================================
library(leaflet)
library(htmlwidgets)

message("Building leaflet interactive map.")

status_colors <- c(
  "confirmed_valid"   = "#1B9E77",
  "relocated"         = "#377EB8",
  "retired_inactive"  = "#D95F02",
  "wrong_number"      = "#E7298A",
  "unresolved"        = "#7570B3",
  "not_attempted"     = "#AAAAAA",
  "Other/unknown"     = "#666666"
)

leaflet_data <- audit_sf |>
  sf::st_drop_geometry() |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  mutate(
    color = dplyr::recode(
      listing_status_detail,
      "confirmed_valid"  = "#1B9E77",
      "relocated"        = "#377EB8",
      "retired_inactive" = "#D95F02",
      "wrong_number"     = "#E7298A",
      "unresolved"       = "#7570B3",
      .default           = "#666666"
    ),
    label_text = paste0(
      "<b style='font-size:13px'>",
        coalesce(first_name, ""), " ", coalesce(last_name, ""),
      "</b><br>",
      "<b>NPI:</b> ", npi, "<br>",
      "<b>City, State:</b> ",
        coalesce(city, "—"), ", ", coalesce(state_full, state), "<br>",
      "<hr style='margin:4px 0'>",
      "<b>Audit status:</b> ", listing_status_detail, "<br>",
      "<b>Caller response:</b> ", coalesce(taking_new_patients, "—"), "<br>",
      dplyr::if_else(
        listing_status_detail == "relocated",
        paste0("<b>Relocated to:</b> ", relocated_to, "<br>"),
        ""
      ),
      "<hr style='margin:4px 0'>",
      "<b>Registry overlap zone:</b> ", coalesce(overlap_zone, "—"), "<br>",
      # --- database links --------------------------------------------------
      # NPPES: direct provider-view URL by NPI
      "<b>NPPES incorrect listing?</b> ", nppes_incorrect_label,
      " &nbsp;<a href='https://npiregistry.cms.hhs.gov/provider-view/",
        npi, "' target='_blank'>[View NPPES listing]</a><br>",
      # ABOto: no direct NPI deep-link; link to the verify-certification page
      "<b>American Board of Otolaryngology incorrect listing?</b> ",
        aboto_incorrect,
      dplyr::if_else(
        in_board,
        " &nbsp;<a href='https://www.aboto.org/' target='_blank'>[Verify ABOto certification]</a>",
        ""
      ), "<br>",
      # ENTHealth: no direct NPI deep-link; link to Find-an-ENT search
      "<b>ENTHealth.org incorrect listing?</b> ",
        enthealth_incorrect,
      dplyr::if_else(
        in_ent,
        " &nbsp;<a href='https://www.enthealth.org/find-an-ent/' target='_blank'>[Search ENTHealth directory]</a>",
        ""
      ), "<br>"
    )
  )

# Build legend labels from levels actually present in the data
present_statuses <- sort(unique(leaflet_data$listing_status_detail))
legend_labels <- dplyr::recode(
  present_statuses,
  "confirmed_valid"  = "Location-confirmed",
  "relocated"        = "Active elsewhere (relocated)",
  "retired_inactive" = "Retired / inactive",
  "wrong_number"     = "Wrong / disconnected number",
  "unresolved"       = "Unresolved",
  "not_attempted"    = "Not attempted",
  .default           = "Other/unknown"
)
legend_colors <- dplyr::recode(
  present_statuses,
  "confirmed_valid"  = "#1B9E77",
  "relocated"        = "#377EB8",
  "retired_inactive" = "#D95F02",
  "wrong_number"     = "#E7298A",
  "unresolved"       = "#7570B3",
  "not_attempted"    = "#AAAAAA",
  .default           = "#666666"
)

lmap <- leaflet(leaflet_data) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    color       = ~color,
    fillColor   = ~color,
    fillOpacity = 0.85,
    opacity     = 1,
    radius      = 5,
    stroke      = TRUE,
    weight      = 1,
    popup       = ~label_text,
    label       = ~paste0(
      coalesce(first_name, ""), " ", coalesce(last_name, ""),
      " (", listing_status_detail, ")"
    )
  ) |>
  addLegend(
    position = "bottomright",
    colors   = legend_colors,
    labels   = legend_labels,
    title    = "Audit result",
    opacity  = 1
  )

leaflet_path <- file.path(
  save_dir,
  sprintf("map_03_audited_status_interactive_%s.html", timestamp)
)
saveWidget(lmap, file = leaflet_path, selfcontained = TRUE)
message("Leaflet map saved: ", leaflet_path)
