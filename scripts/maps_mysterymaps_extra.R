#!/usr/bin/env Rscript
# =============================================================================
# Additional mysterymaps outputs for the ENT study:
#   1. AAO-HNS district map  (mysterymaps_map_acog_districts, fed our AAO-HNS
#      crosswalk reshaped into the district CSV format the function expects)
#   2. Physician-location dot map (mysterymaps_map_physicians); ZIP centroids
#      from zipcodeR (free -- no paid geocoding)
#
# Isochrones (mysterymaps_create_isochrones) are NOT built here: they require a
# HERE routing API key (HERE_API_KEY), which is not set. See the console note.
#
# Output: model_output/supp/
# =============================================================================

suppressMessages(devtools::load_all("~/mysterymaps", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
options(stringsAsFactors = FALSE)
supp <- "model_output/supp"; dir.create(supp, showWarnings = FALSE, recursive = TRUE)
num <- function(x) suppressWarnings(as.numeric(x))

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L, 0L)
d$wait_days <- num(d$wait_days_business)
d$zip5      <- sprintf("%05s", gsub("[^0-9]", "", substr(d$zip, 1, 5)))
xw <- read.csv("data/raw/state_to_aao_hns_district.csv")

# ---- 1. AAO-HNS district map ------------------------------------------------
# Reshape the per-state crosswalk into the one-row-per-district format the
# function expects: ACOG_District, Subregion, States, State_Abbreviations.
dist_csv <- file.path(supp, "aao_hns_districts_for_map.csv")
xw %>%
  group_by(AAO_HNS_District, Region) %>%
  summarise(States = paste(State, collapse = ", "),
            State_Abbreviations = paste(State_Abbr, collapse = ", "), .groups = "drop") %>%
  rename(ACOG_District = AAO_HNS_District, Subregion = Region) %>%
  write.csv(dist_csv, row.names = FALSE)

dm <- tryCatch(mysterymaps_map_acog_districts(dist_csv), error = function(e) {cat(" district-map err:", conditionMessage(e), "\n"); NULL})
if (!is.null(dm)) {
  if (inherits(dm, "leaflet")) {
    html <- file.path(supp, "figS5_aao_hns_districts.html")
    htmlwidgets::saveWidget(dm, html, selfcontained = TRUE)
    if (requireNamespace("webshot2", quietly = TRUE))
      try(webshot2::webshot(html, file.path(supp, "figS5_aao_hns_districts.png"), vwidth = 1100, vheight = 700), silent = TRUE)
    cat("wrote figS5_aao_hns_districts.{html,png}\n")
  } else if (inherits(dm, "ggplot")) {
    ggplot2::ggsave(file.path(supp, "figS5_aao_hns_districts.png"), dm, width = 10, height = 6, dpi = 150)
    cat("wrote figS5_aao_hns_districts.png\n")
  } else if (inherits(dm, "sf")) {
    saveRDS(dm, file.path(supp, "figS5_aao_hns_districts_sf.rds"))
    cat("map_acog_districts returned sf; saved geometry.\n")
  }
}

# ---- 2. Physician dot map ---------------------------------------------------
zdb <- zipcodeR::zip_code_db[, c("zipcode","lat","lng")]
phys <- merge(d[, c("zip5","state","city","ent_type","offered","wait_days")],
              zdb, by.x = "zip5", by.y = "zipcode", all.x = TRUE)
phys <- phys[!is.na(phys$lat) & !is.na(phys$lng), ]
phys$long <- phys$lng
region <- setNames(xw$Region, xw$State_Abbr)
phys$ACOG_District <- region[phys$state]
cat(sprintf("physicians with coordinates: %d of %d\n", nrow(phys), nrow(d)))

pm <- tryCatch(mysterymaps_map_physicians(phys, jitter_range = 0.05,
        popup_var = "ent_type", output_dir = supp),
        error = function(e) {cat(" physician-map err:", conditionMessage(e), "\n"); NULL})
if (!is.null(pm)) {
  if (inherits(pm, "leaflet")) {
    html <- file.path(supp, "figS6_physician_locations.html")
    htmlwidgets::saveWidget(pm, html, selfcontained = TRUE)
    if (requireNamespace("webshot2", quietly = TRUE))
      try(webshot2::webshot(html, file.path(supp, "figS6_physician_locations.png"), vwidth = 1100, vheight = 700), silent = TRUE)
    cat("wrote figS6_physician_locations.{html,png}\n")
  }
  cat("physician dot map produced (see", supp, ")\n")
}

# ---- 3. Isochrones (blocked) ------------------------------------------------
if (nzchar(Sys.getenv("HERE_API_KEY"))) {
  cat("HERE_API_KEY present -- isochrones could be built here (not yet implemented in this run).\n")
} else {
  cat("\n[ISOCHRONES SKIPPED] mysterymaps_create_isochrones() needs a HERE routing\n",
      "API key (HERE_API_KEY env var; free at https://www.here.com/developer) and\n",
      "the hereR package (installed). Provide the key and I will build drive-time\n",
      "access isochrones around representative practices.\n")
}
cat("\nDone.\n")
