#!/usr/bin/env Rscript
# =============================================================================
# Geographic maps built with the mysterymaps package (native map layer).
# Registers the Google Maps key from ~/.Renviron so the Google-backed helpers
# (geocoding, leaflet basemaps) are available; the state choropleths themselves
# use mysterymaps' built-in US geometry.
#
#   figS4b_offer_rate_by_state.png   mysterymaps_geographic_map (offer rate)
#   figS4b_median_wait_by_state.png  mysterymaps_geographic_map (median wait)
#
# Output: model_output/supp/
# =============================================================================

suppressMessages(devtools::load_all("~/mysterymaps", quiet = TRUE))
options(stringsAsFactors = FALSE)
supp <- "model_output/supp"; dir.create(supp, showWarnings = FALSE, recursive = TRUE)
num <- function(x) suppressWarnings(as.numeric(x))

# Register the Google Maps API key (from ~/.Renviron) for mysterymaps' Google
# helpers. The key is never printed or written to the repo.
gkey <- Sys.getenv("GGMAP_GOOGLE_API_KEY")
if (nzchar(gkey) && requireNamespace("ggmap", quietly = TRUE)) {
  suppressMessages(ggmap::register_google(key = gkey))
  cat("Google Maps key registered (", nchar(gkey), "chars).\n")
} else cat("GGMAP_GOOGLE_API_KEY not set; choropleths still work (no key needed).\n")

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L, 0L)
d$wait_days <- num(d$wait_days_business)

# mysterymaps_geographic_map is a RATE mapper: pass ROW-LEVEL data with a 0/1
# outcome and it computes the per-state proportion and percent-formats the legend.
dd <- d[nzchar(d$state), ]
dd$offered_bin <- as.integer(dd$offered == 1)
cat("states with data:", length(unique(dd$state)), "\n")

m <- tryCatch(mysterymaps_geographic_map(
  data = dd, state_col = "state", outcome_col = "offered_bin",
  fill_label = "Offer rate", title = "ENT appointment offer rate by state",
  direction = 1, include_alaska_hawaii = TRUE), error = function(e) { cat(" err:", conditionMessage(e), "\n"); NULL })
if (!is.null(m)) {
  ggplot2::ggsave(file.path(supp, "figS4b_offer_rate_by_state.png"), m, width = 9, height = 6, dpi = 150)
  cat("wrote figS4b_offer_rate_by_state.png\n")
}

# per-state table for reference (offer rate as proportion; median wait among offered)
by_state <- do.call(rbind, lapply(split(dd, dd$state), function(g) data.frame(
  state = g$state[1], n = nrow(g),
  offer_rate = round(mean(g$offered_bin), 3),
  median_wait = median(g$wait_days[g$offered == 1], na.rm = TRUE))))
write.csv(by_state, file.path(supp, "state_offer_wait.csv"), row.names = FALSE)
cat("wrote state_offer_wait.csv (median-wait choropleth remains figS4_choropleth_median_wait.png)\n")
