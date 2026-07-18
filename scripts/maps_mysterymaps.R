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
# Sample-size protection: a per-state rate from 1-2 calls is uninterpretable
# (e.g., one success reads 100%), so states with fewer than MIN_STATE_N calls are
# suppressed from the choropleth (shown as no-data); all states, with their
# denominators, appear in the companion table state_offer_wait.csv.
MIN_STATE_N <- 5
dd <- d[nzchar(d$state), ]
dd$offered_bin <- as.integer(dd$offered == 1)
st_n <- table(dd$state); low <- names(st_n)[st_n < MIN_STATE_N]
cat(sprintf("states with data: %d; suppressing %d with n<%d (%s)\n",
            length(st_n), length(low), MIN_STATE_N, paste(low, collapse = ", ")))
dd_map <- dd[!(dd$state %in% low), ]

m <- tryCatch(mysterymaps_geographic_map(
  data = dd_map, state_col = "state", outcome_col = "offered_bin",
  fill_label = "Offer rate", title = "ENT appointment offer rate by state (states with <5 calls suppressed)",
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
