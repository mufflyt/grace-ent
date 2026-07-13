# scrape_healthgrades_locations.R
# Scrape Healthgrades for ENT physician practice locations
#
# Healthgrades provides:
# - Multiple office locations per physician
# - Full addresses with street, city, state, ZIP
# - Latitude/longitude coordinates
# - Structured JSON-LD data (easy to parse)
#
# NOTE: Use reasonable rate limiting to be respectful

library(httr)
library(rvest)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)

# ============================================================================
# Configuration
# ============================================================================

input_file <- "ent_final_with_all_npi.xlsx"
output_file <- "ent_healthgrades_locations.xlsx"
checkpoint_file <- "healthgrades_checkpoint.rds"
log_file <- "healthgrades_scrape_log.txt"

# Rate limiting (seconds between requests)
request_delay <- 2

# Headers to mimic browser
browser_headers <- c(

`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
`Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
`Accept-Language` = "en-US,en;q=0.5",
`Accept-Encoding` = "gzip, deflate",
`Connection` = "keep-alive"
)

# ============================================================================
# Logging
# ============================================================================

# Use canonical logging functions from log_safe.R
source(here::here("R", "log_safe.R"))

# Use generic checkpoint utilities
source(here::here("R", "utils", "generic_checkpoint.R"))

#' Log message with timestamp
#'
#' @description
#' Logs message to console via log_info_safe and appends to log file if
#' log_file exists. Adds timestamp prefix for file logging.
#'
#' @param msg [character]: Message to log.
#' @return NULL (invisible). Side effect: prints message and appends to log file.
#' @keywords internal
log_message <- function(msg) {
  log_info_safe(msg)
  if (exists("log_file") && !is.null(log_file)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    full_msg <- paste0("[", timestamp, "] ", msg)
    cat(full_msg, "\n", file = log_file, append = TRUE)
  }
}

# ============================================================================
# Scraping Functions
# ============================================================================
#' Build Healthgrades search URL for a physician
#'
#' @param first_name Physician first name
#' @param last_name Physician last name
#' @param state State abbreviation (optional)
#' @return Healthgrades search URL
build_search_url <- function(first_name, last_name, state = NULL) {
  # Clean names
  name_query <- paste(first_name, last_name)

  if (!is.null(state) && !is.na(state)) {
    # Search with location
    url <- paste0(
      "https://www.healthgrades.com/usearch?what=",
      URLencode(name_query),
      "%20Otolaryngologist&where=",
      URLencode(state)
    )
  } else {
    url <- paste0(
      "https://www.healthgrades.com/usearch?what=",
      URLencode(name_query),
      "%20Otolaryngologist"
    )
  }

  return(url)
}

#' Search Healthgrades for a physician and get profile URL
#'
#' @param first_name Physician first name
#' @param last_name Physician last name
#' @param state State abbreviation
#' @return Profile URL or NA if not found
search_physician <- function(first_name, last_name, state = NULL) {
  url <- build_search_url(first_name, last_name, state)

  response <- tryCatch({
    GET(url, add_headers(.headers = browser_headers), timeout(30))
  }, error = function(e) {
    log_message(paste("  Search error:", e$message))
    return(NULL)
  })

  if (is.null(response) || status_code(response) != 200) {
    return(NA)
  }

  content <- content(response, "text", encoding = "UTF-8")
  page <- read_html(content)

  # Extract physician profile links
  doc_links <- page %>%
    html_nodes("a[href*='/physician/dr-']") %>%
    html_attr("href")

  if (length(doc_links) == 0) {
    return(NA)
  }

  # Filter to unique profile URLs (not #ratings or #highlights)
  profile_urls <- unique(doc_links[!str_detect(doc_links, "#")])

  if (length(profile_urls) == 0) {
    return(NA)
  }

  # Try to match by name
  last_clean <- tolower(str_replace_all(last_name, "[^a-z]", ""))

  for (url in profile_urls) {
    if (str_detect(tolower(url), last_clean)) {
      return(url)
    }
  }

  # Return first result if no exact match
  return(profile_urls[1])
}

#' Extract locations from a Healthgrades profile page
#'
#' @param profile_url Profile URL (relative or absolute)
#' @return Data frame with locations
extract_profile_locations <- function(profile_url) {
  # Make absolute URL
  if (!str_starts(profile_url, "http")) {
    profile_url <- paste0("https://www.healthgrades.com", profile_url)
  }

  response <- tryCatch({
    GET(profile_url, add_headers(.headers = browser_headers), timeout(30))
  }, error = function(e) {
    log_message(paste("  Profile fetch error:", e$message))
    return(NULL)
  })

  if (is.null(response) || status_code(response) != 200) {
    return(tibble())
  }

  content <- content(response, "text", encoding = "UTF-8")
  page <- read_html(content)

  # Extract JSON-LD blocks
  json_blocks <- page %>%
    html_nodes("script[type='application/ld+json']") %>%
    html_text()

  locations <- list()

  for (block in json_blocks) {
    tryCatch({
      data <- fromJSON(block, simplifyVector = FALSE)

      # Check for address in location
      if (!is.null(data$location) && !is.null(data$location$address)) {
        addr <- data$location$address
        loc <- tibble(
          street = addr$streetAddress %||% NA,
          city = addr$addressLocality %||% NA,
          state = addr$addressRegion %||% NA,
          zip = addr$postalCode %||% NA,
          latitude = data$location$geo$latitude %||% NA,
          longitude = data$location$geo$longitude %||% NA
        )
        locations[[length(locations) + 1]] <- loc
      }

      # Check for direct address
      if (!is.null(data$address) && is.null(data$location)) {
        addr <- data$address
        if (!is.null(addr$streetAddress)) {
          loc <- tibble(
            street = addr$streetAddress %||% NA,
            city = addr$addressLocality %||% NA,
            state = addr$addressRegion %||% NA,
            zip = addr$postalCode %||% NA,
            latitude = NA,
            longitude = NA
          )
          locations[[length(locations) + 1]] <- loc
        }
      }

      # Check for @graph array
      if (!is.null(data$`@graph`)) {
        for (item in data$`@graph`) {
          if (!is.null(item$address) && !is.null(item$address$streetAddress)) {
            addr <- item$address
            loc <- tibble(
              street = addr$streetAddress %||% NA,
              city = addr$addressLocality %||% NA,
              state = addr$addressRegion %||% NA,
              zip = addr$postalCode %||% NA,
              latitude = NA,
              longitude = NA
            )
            locations[[length(locations) + 1]] <- loc
          }
        }
      }

    }, error = function(e) {})
  }

  if (length(locations) == 0) {
    return(tibble())
  }

  # Combine and deduplicate
  result <- bind_rows(locations) %>%
    filter(!is.na(street)) %>%
    distinct(street, city, state, zip, .keep_all = TRUE)

  return(result)
}

#' Scrape Healthgrades for a single physician
#'
#' @param first_name Physician first name
#' @param last_name Physician last name
#' @param npi NPI number
#' @param state State abbreviation
#' @return Data frame with locations
scrape_physician <- function(first_name, last_name, npi, state = NULL) {
  log_message(paste("Searching:", first_name, last_name))

  # Search for profile
  profile_url <- search_physician(first_name, last_name, state)

  if (is.na(profile_url)) {
    log_message("  Not found on Healthgrades")
    return(tibble())
  }

  log_message(paste("  Found profile:", profile_url))

  Sys.sleep(request_delay)

  # Extract locations
  locations <- extract_profile_locations(profile_url)

  if (nrow(locations) == 0) {
    log_message("  No locations extracted")
    return(tibble())
  }

  log_message(paste("  Found", nrow(locations), "location(s)"))

  # Add physician info
  locations <- locations %>%
    mutate(
      npi = npi,
      first_name = first_name,
      last_name = last_name,
      healthgrades_url = profile_url,
      scrape_date = Sys.Date()
    )

  return(locations)
}

# Note: Checkpoint functions moved to R/utils/generic_checkpoint.R

# ============================================================================
# Main Script
# ============================================================================

#' Main healthgrades location scraper
#'
#' @description
#' Loads ENT data, searches Healthgrades for each physician, extracts location
#' data, and saves results to timestamped CSV files. Implements rate limiting
#' and retry logic.
#'
#' @return NULL (invisible). Side effect: writes CSV files to output directory.
#' @keywords internal
main <- function() {
  log_message("========================================")
  log_message("Healthgrades Location Scraper")
  log_message("========================================")

  # Load ENT data
  log_message(paste("Loading ENT data from:", input_file))
  ent_data <- read_excel(input_file)

  # Filter to physicians with names
  ent_to_search <- ent_data %>%
    filter(!is.na(first_name) & !is.na(last_name) & !is.na(npi_final)) %>%
    select(npi = npi_final, first_name, last_name, state) %>%
    distinct(npi, .keep_all = TRUE)

  log_message(paste("ENTs to search:", nrow(ent_to_search)))

  # Load checkpoint
  checkpoint <- load_generic_checkpoint(
    checkpoint_file = checkpoint_file,
    default_structure = list(
      data = tibble(),
      completed_npis = c()
    ),
    verbose = FALSE
  )
  if (length(checkpoint$completed_npis) > 0) {
    log_message(paste("Checkpoint loaded:", length(checkpoint$completed_npis),
                     "physicians already processed"))
  }
  completed_npis <- checkpoint$completed_npis
  all_locations <- checkpoint$data

  # Filter to remaining
  remaining <- ent_to_search %>%
    filter(!(npi %in% completed_npis))

  log_message(paste("Remaining to search:", nrow(remaining)))

  if (nrow(remaining) == 0) {
    log_message("All physicians already processed!")
    return(all_locations)
  }

  # Process physicians
  checkpoint_interval <- 25
  batch_count <- 0

  for (i in seq_len(nrow(remaining))) {
    row <- remaining[i, ]

    # Scrape
    result <- tryCatch({
      scrape_physician(
        first_name = row$first_name,
        last_name = row$last_name,
        npi = row$npi,
        state = row$state
      )
    }, error = function(e) {
      log_message(paste("  Error:", e$message))
      tibble()
    })

    # Record results
    if (nrow(result) > 0) {
      all_locations <- bind_rows(all_locations, result)
    }

    completed_npis <- c(completed_npis, row$npi)
    batch_count <- batch_count + 1

    # Checkpoint
    if (batch_count >= checkpoint_interval) {
      save_generic_checkpoint(
        data = all_locations,
        completed_npis = completed_npis,
        checkpoint_file = checkpoint_file,
        verbose = FALSE
      )
      log_message(paste("Checkpoint saved:", length(completed_npis), "physicians processed"))
      batch_count <- 0
    }

    # Rate limiting
    Sys.sleep(request_delay)

    # Progress
    if (i %% 50 == 0) {
      log_message(paste("Progress:", i, "/", nrow(remaining),
                        "- Found locations for", n_distinct(all_locations$npi), "NPIs"))
    }
  }

  # Final checkpoint
  save_generic_checkpoint(
    data = all_locations,
    completed_npis = completed_npis,
    checkpoint_file = checkpoint_file,
    verbose = FALSE
  )
  log_message(paste("Final checkpoint saved:", length(completed_npis), "physicians processed"))

  # ============================================================================
  # Save Results
  # ============================================================================

  log_message("========================================")
  log_message("Saving Results")
  log_message("========================================")

  if (nrow(all_locations) == 0) {
    log_message("No locations found")
    return(tibble())
  }

  # Summary statistics
  log_message(paste("Total location records:", nrow(all_locations)))
  log_message(paste("NPIs with Healthgrades data:", n_distinct(all_locations$npi)))

  # Multiple locations
  multi_loc <- all_locations %>%
    group_by(npi) %>%
    summarise(n_locations = n()) %>%
    filter(n_locations > 1)

  log_message(paste("NPIs with 2+ locations:", nrow(multi_loc)))

  # Summary by NPI
  npi_summary <- all_locations %>%
    group_by(npi, first_name, last_name) %>%
    summarise(
      n_healthgrades_locations = n(),
      cities = paste(unique(city[!is.na(city)]), collapse = ", "),
      states = paste(unique(state[!is.na(state)]), collapse = ", "),
      zips = paste(unique(zip[!is.na(zip)]), collapse = ", "),
      .groups = "drop"
    )

  # Save
  write_xlsx(
    list(
      "All Locations" = all_locations,
      "Summary by NPI" = npi_summary
    ),
    output_file
  )

  log_message(paste("Saved to:", output_file))

  log_message("========================================")
  log_message("COMPLETE")
  log_message("========================================")

  return(all_locations)
}

# ============================================================================
# Run
# ============================================================================

if (sys.nframe() == 0) {
  main()
}
