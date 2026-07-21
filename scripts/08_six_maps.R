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
audit_tbl <- calls |>
  filter(listing_status != "not_attempted") |>
  mutate(zip5 = str_pad(as.character(zip), 5, "left", "0")) |>
  left_join(
    addr |> select(npi, primary_state),
    by = "npi"
  ) |>
  left_join(zcta_centroids |> select(zip5, longitude, latitude), by = "zip5") |>
  left_join(state_centroids_tbl, by = c("state" = "STUSPS")) |>
  mutate(
    longitude = coalesce(longitude, state_lon),
    latitude  = coalesce(latitude,  state_lat)
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
message("Generating Map 3: audited listing status.")
map3 <- tryCatch(
  make_audited_status_map(
    audit_sf   = audit_sf,
    states_sf  = states_sf,
    facet_by_zone = TRUE
  ),
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
    width     = 12,
    height    = 8
  )
  message("Map 3 complete.")
}

# --- Map 5: State registry-corroboration map ---------------------------------
message("Generating Map 5: state registry corroboration.")
map5 <- tryCatch(
  make_state_corroboration_map(
    states_sf          = states_sf,
    listing_sf         = listing_sf,
    state_population_tbl = state_pop_tbl,
    in_aboto_col       = "in_aboto",
    in_enthealth_col   = "in_enthealth"
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
