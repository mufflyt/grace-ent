# scrape_healthgrades_full.R
# Comprehensive Healthgrades scraper for ENT physicians
#
# Extracts ALL available data:
# - Practice locations (multiple per physician)
# - Education (medical school, residency, fellowship)
# - Board certifications
# - Years in practice / graduation year (for age estimation)
# - Hospital affiliations
# - Specialties and subspecialties
# - Gender
# - Languages spoken
# - Patient ratings
# - Insurance accepted
# - Telehealth availability
# - Accepting new patients status
# - Profile photo URL (can be downloaded separately)
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
output_file <- "ent_healthgrades_full.xlsx"
checkpoint_file <- "healthgrades_full_checkpoint.rds"
log_file <- "healthgrades_full_log.txt"

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

# Wrapper for backward compatibility with file logging
#' Log message with timestamp to console and file
#'
#' @description
#' Wrapper around log_info_safe that also writes to a log file if defined.
#' Provides backward compatibility for legacy scraping code that uses
#' file-based logging.
#'
#' @param msg [character]: message to log
#' @return Invisible NULL
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
# Helper Functions
# ============================================================================

#' Safely extract nested value with default
#'
#' @description
#' Navigates nested list structures to extract a value at a specified path.
#' Returns a default value if any key is missing or NULL. Wraps extraction
#' in tryCatch to handle errors gracefully.
#'
#' @param data Nested list or data structure to extract from
#' @param ... [character]: keys representing the path to navigate (e.g., "name", "given")
#' @param default Default value to return if extraction fails (default: NA)
#' @return Extracted value or default if extraction fails
#' @keywords internal
safe_extract <- function(data, ..., default = NA) {
  tryCatch({
    result <- data
    for (key in list(...)) {
      if (is.null(result) || !key %in% names(result)) {
        return(default)
      }
      result <- result[[key]]
    }
    if (is.null(result)) default else result
  }, error = function(e) default)
}

#' Extract text matching a pattern from page content
#'
#' @description
#' Uses regular expression pattern matching to extract text from HTML content.
#' Returns the specified capture group or NA if no match found.
#'
#' @param content [character]: of HTML content to search
#' @param pattern Regular expression pattern with capture groups
#' @param group [integer]: index of capture group to return (default: 1)
#' @return Character string of matched group or NA if no match
#' @keywords internal
extract_text_pattern <- function(content, pattern, group = 1) {
  match <- str_match(content, pattern)
  if (is.na(match[1])) return(NA)
  return(match[group + 1])
}

# ============================================================================
# Search Functions
# ============================================================================

#' Build Healthgrades search URL for a physician
#'
#' @description
#' Constructs a Healthgrades search URL with physician name and optional state.
#' URL encodes parameters and hardcodes "Otolaryngologist" specialty filter.
#'
#' @param first_name [character]: of physician's first name
#' @param last_name [character]: of physician's last name
#' @param state Optional character string of state abbreviation for location filtering
#' @return Character string of fully formed Healthgrades search URL
#' @keywords internal
build_search_url <- function(first_name, last_name, state = NULL) {
  name_query <- paste(first_name, last_name)

  if (!is.null(state) && !is.na(state)) {
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
#' @description
#' Searches Healthgrades using physician name and optional state, parses
#' search results to find profile links, and returns the most likely match.
#' Prioritizes exact last name matches.
#'
#' @param first_name [character]: of physician's first name
#' @param last_name [character]: of physician's last name
#' @param state Optional character string of state abbreviation
#' @return Character string of profile URL or NA if not found
#' @keywords internal
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

  # Filter to unique profile URLs
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

  return(profile_urls[1])
}

# ============================================================================
# Data Extraction Functions
# ============================================================================

#' Extract all data from a Healthgrades profile page
#'
#' @param profile_url Profile URL (relative or absolute)
#' @return List with physician data and locations
extract_profile_full <- function(profile_url) {
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
    return(list(physician = tibble(), locations = tibble()))
  }

  content <- content(response, "text", encoding = "UTF-8")
  page <- read_html(content)

  # Initialize results
  physician_data <- list()
  locations <- list()

  # ========================================
  # Extract JSON-LD structured data
  # ========================================
  json_blocks <- page %>%
    html_nodes("script[type='application/ld+json']") %>%
    html_text()

  # Lists to collect data from multiple JSON-LD blocks
  education_list <- c()
  hospital_list <- c()
  certification_list <- c()

  for (block in json_blocks) {
    tryCatch({
      data <- fromJSON(block, simplifyVector = FALSE)

      # Check for Physician type
      if (!is.null(data$`@type`) && data$`@type` == "Physician") {
        # Name and credentials
        if (is.null(physician_data$name)) {
          physician_data$name <- data$name %||% NA
        }

        # Photo URL (from JSON-LD image property)
        if (is.null(physician_data$photo_url) && !is.null(data$image)) {
          if (is.character(data$image)) {
            physician_data$photo_url <- data$image
          } else if (is.list(data$image) && !is.null(data$image$url)) {
            physician_data$photo_url <- data$image$url
          }
        }

        # Education - collect from alumni/alumniOf
        if (!is.null(data$alumni) && !is.null(data$alumni$alumniOf)) {
          school_name <- data$alumni$alumniOf$name %||% NA
          if (!is.na(school_name)) education_list <- c(education_list, school_name)
        }
        if (!is.null(data$alumniOf)) {
          if (is.list(data$alumniOf)) {
            if (!is.null(data$alumniOf$name)) {
              education_list <- c(education_list, data$alumniOf$name)
            } else {
              schools <- sapply(data$alumniOf, function(x) x$name %||% NA)
              education_list <- c(education_list, schools[!is.na(schools)])
            }
          } else {
            education_list <- c(education_list, data$alumniOf)
          }
        }

        # Hospital affiliations - collect from each block
        if (!is.null(data$hospitalAffiliation)) {
          if (is.list(data$hospitalAffiliation)) {
            hosp_name <- data$hospitalAffiliation$name %||% NA
            if (!is.na(hosp_name)) hospital_list <- c(hospital_list, hosp_name)
          }
        }

        # Medical specialty
        if (is.null(physician_data$specialties) && !is.null(data$medicalSpecialty)) {
          if (is.list(data$medicalSpecialty)) {
            specs <- sapply(data$medicalSpecialty, function(x) {
              if (is.list(x)) x$name %||% NA else x
            })
            physician_data$specialties <- paste(specs[!is.na(specs)], collapse = "; ")
          } else {
            physician_data$specialties <- data$medicalSpecialty
          }
        }

        # Certifications
        if (!is.null(data$hasCredential)) {
          if (is.list(data$hasCredential)) {
            creds <- sapply(data$hasCredential, function(x) x$name %||% NA)
            certification_list <- c(certification_list, creds[!is.na(creds)])
          } else {
            certification_list <- c(certification_list, data$hasCredential)
          }
        }

        # Ratings
        if (is.null(physician_data$rating) && !is.null(data$aggregateRating)) {
          physician_data$rating <- data$aggregateRating$ratingValue %||% NA
          physician_data$review_count <- data$aggregateRating$reviewCount %||% NA
        }
      }

      # Extract locations from various structures
      # Location in main object
      if (!is.null(data$location) && !is.null(data$location$address)) {
        addr <- data$location$address
        loc <- tibble(
          street = addr$streetAddress %||% NA,
          city = addr$addressLocality %||% NA,
          state = addr$addressRegion %||% NA,
          zip = addr$postalCode %||% NA,
          latitude = safe_extract(data, "location", "geo", "latitude"),
          longitude = safe_extract(data, "location", "geo", "longitude"),
          phone = safe_extract(data, "location", "telephone")
        )
        locations[[length(locations) + 1]] <- loc
      }

      # Direct address
      if (!is.null(data$address) && is.null(data$location)) {
        addr <- data$address
        if (!is.null(addr$streetAddress)) {
          loc <- tibble(
            street = addr$streetAddress %||% NA,
            city = addr$addressLocality %||% NA,
            state = addr$addressRegion %||% NA,
            zip = addr$postalCode %||% NA,
            latitude = NA,
            longitude = NA,
            phone = data$telephone %||% NA
          )
          locations[[length(locations) + 1]] <- loc
        }
      }

      # @graph array
      if (!is.null(data$`@graph`)) {
        for (item in data$`@graph`) {
          if (!is.null(item$address) && !is.null(item$address$streetAddress)) {
            addr <- item$address
            loc <- tibble(
              street = addr$streetAddress %||% NA,
              city = addr$addressLocality %||% NA,
              state = addr$addressRegion %||% NA,
              zip = addr$postalCode %||% NA,
              latitude = safe_extract(item, "geo", "latitude"),
              longitude = safe_extract(item, "geo", "longitude"),
              phone = item$telephone %||% NA
            )
            locations[[length(locations) + 1]] <- loc
          }
        }
      }

    }, error = function(e) {})
  }

  # Combine collected lists into physician_data
  if (length(education_list) > 0) {
    physician_data$education <- paste(unique(education_list), collapse = "; ")
  }
  if (length(hospital_list) > 0) {
    physician_data$hospital_affiliations <- paste(unique(hospital_list), collapse = "; ")
  }
  if (length(certification_list) > 0) {
    physician_data$certifications <- paste(unique(certification_list), collapse = "; ")
  }

  # ========================================
  # Extract HTML-only data
  # ========================================

  # Photo URL from HTML (backup if not in JSON-LD)
  if (is.null(physician_data$photo_url)) {
    photo_node <- page %>%
      html_nodes("img[class*='provider-image'], img[class*='doctor-photo'], img[data-qa='provider-image'], .profile-photo img") %>%
      html_attr("src")

    if (length(photo_node) > 0 && !is.na(photo_node[1])) {
      physician_data$photo_url <- photo_node[1]
    }
  }

  # Fix protocol-relative URLs (//photos.healthgrades.com/...)
  if (!is.null(physician_data$photo_url) && str_starts(physician_data$photo_url, "//")) {
    physician_data$photo_url <- paste0("https:", physician_data$photo_url)
  }

  # ========================================
  # Extract certifications from HTML (backup)
  # ========================================
  if (is.null(physician_data$certifications)) {
    # Look for "American Board of X" mentions
    board_matches <- str_extract_all(content, "American Board of [A-Za-z]+")[[1]]
    if (length(board_matches) > 0) {
      physician_data$certifications <- paste(unique(board_matches), collapse = "; ")
    }
  }

  # ========================================
  # Extract specialty from HTML (backup)
  # ========================================
  if (is.null(physician_data$specialties)) {
    # Check for common ENT specialty terms
    if (str_detect(content, fixed("Otolaryngology"))) {
      physician_data$specialties <- "Otolaryngology"
    } else if (str_detect(content, fixed("Ear, Nose and Throat"))) {
      physician_data$specialties <- "Ear, Nose and Throat"
    }
  }

  # Years in practice (critical for age estimation)
  years_match <- str_match(content, "(\\d+)\\+?\\s*(?:years?\\s+(?:of\\s+)?(?:experience|in practice|practicing))")
  if (!is.na(years_match[1])) {
    physician_data$years_in_practice <- as.integer(years_match[2])
  }

  # Graduation year
  grad_match <- str_match(content, "(?:graduated|graduation|class of)\\s*(\\d{4})")
  if (!is.na(grad_match[1])) {
    physician_data$graduation_year <- as.integer(grad_match[2])
  }

  # Gender
  if (str_detect(content, regex("\\b(she|her|herself)\\b", ignore_case = TRUE))) {
    physician_data$gender <- "Female"
  } else if (str_detect(content, regex("\\b(he|him|himself)\\b", ignore_case = TRUE))) {
    physician_data$gender <- "Male"
  }

  # Languages - extract from Languages section
  # Look for common language names after "Languages" header
  lang_section <- str_match(content, "Languages</h3>.*?</div></div></div>")
  if (!is.na(lang_section[1])) {
    # Extract language names (English, Spanish, etc.)
    common_langs <- c("English", "Spanish", "French", "German", "Chinese", "Mandarin",
                      "Cantonese", "Korean", "Japanese", "Vietnamese", "Arabic", "Hindi",
                      "Portuguese", "Russian", "Italian", "Polish", "Tagalog", "Farsi",
                      "Hebrew", "Greek", "Turkish", "Urdu", "Bengali", "Punjabi")
    found_langs <- c()
    for (lang in common_langs) {
      if (str_detect(lang_section[1], fixed(lang))) {
        found_langs <- c(found_langs, lang)
      }
    }
    if (length(found_langs) > 0) {
      physician_data$languages <- paste(found_langs, collapse = "; ")
    }
  }

  # Accepting new patients
  if (str_detect(content, regex("accepting\\s+new\\s+patients", ignore_case = TRUE))) {
    physician_data$accepting_new_patients <- TRUE
  } else if (str_detect(content, regex("not\\s+accepting\\s+new\\s+patients", ignore_case = TRUE))) {
    physician_data$accepting_new_patients <- FALSE
  }

  # Telehealth
  if (str_detect(content, regex("telehealth|telemedicine|video\\s+visit", ignore_case = TRUE))) {
    physician_data$offers_telehealth <- TRUE
  }

  # ========================================
  # Extract insurance from HTML
  # ========================================
  insurance_section <- page %>%
    html_nodes("[data-qa='insurance-list'], .insurance-list, [class*='insurance']") %>%
    html_text() %>%
    paste(collapse = "; ")

  if (nchar(insurance_section) > 0) {
    physician_data$insurance_raw <- str_trunc(insurance_section, 500)
  }

  # ========================================
  # Extract conditions treated
  # ========================================
  conditions <- page %>%
    html_nodes("[data-qa='conditions-list'] li, .conditions-treated li") %>%
    html_text()

  if (length(conditions) > 0) {
    physician_data$conditions_treated <- paste(conditions, collapse = "; ")
  }

  # ========================================
  # Extract procedures
  # ========================================
  procedures <- page %>%
    html_nodes("[data-qa='procedures-list'] li, .procedures-performed li") %>%
    html_text()

  if (length(procedures) > 0) {
    physician_data$procedures_performed <- paste(procedures, collapse = "; ")
  }

  # ========================================
  # Build results
  # ========================================

  # Calculate estimated age from years in practice (assume start practice at ~30)
  est_age <- NA
  if (!is.null(physician_data$graduation_year) && !is.na(physician_data$graduation_year)) {
    est_age <- 2026 - physician_data$graduation_year + 26  # Med school grad + 26 = approx age
  } else if (!is.null(physician_data$years_in_practice) && !is.na(physician_data$years_in_practice)) {
    est_age <- physician_data$years_in_practice + 30  # Years practicing + 30 = approx age
  }

  # Physician data as tibble - clean column names, logical order
  physician_tibble <- tibble(
    # Identity
    hg_name = physician_data$name %||% NA,
    gender = physician_data$gender %||% NA,

    # Age estimation
    years_practicing = physician_data$years_in_practice %||% NA,
    grad_year = physician_data$graduation_year %||% NA,
    est_age = est_age,

    # Ratings
    rating = physician_data$rating %||% NA,
    reviews = physician_data$review_count %||% NA,

    # Practice info
    telehealth = physician_data$offers_telehealth %||% NA,
    new_patients = physician_data$accepting_new_patients %||% NA,
    languages = physician_data$languages %||% NA,

    # Credentials (often empty from JSON-LD)
    education = physician_data$education %||% NA,
    specialties = physician_data$specialties %||% NA,
    certifications = physician_data$certifications %||% NA,
    hospitals = physician_data$hospital_affiliations %||% NA,

    # Clinical (often empty)
    conditions = physician_data$conditions_treated %||% NA,
    procedures = physician_data$procedures_performed %||% NA,
    insurance = physician_data$insurance_raw %||% NA,

    # URLs at end
    photo_url = physician_data$photo_url %||% NA,
    hg_url = profile_url
  )

  # Locations
  if (length(locations) == 0) {
    locations_tibble <- tibble()
  } else {
    locations_tibble <- bind_rows(locations) %>%
      filter(!is.na(street)) %>%
      distinct(street, city, state, zip, .keep_all = TRUE)
  }

  return(list(
    physician = physician_tibble,
    locations = locations_tibble
  ))
}

#' Scrape Healthgrades for a single physician (full data)
#'
#' @description
#' Orchestrates search and extraction for a single physician. Searches for
#' profile, extracts all available data (demographics, ratings, locations),
#' and adds NPI/name metadata to results.
#'
#' @param first_name [character]: of physician's first name
#' @param last_name [character]: of physician's last name
#' @param npi [character]: of National Provider Identifier
#' @param state Optional character string of state abbreviation
#' @return List with physician tibble and locations tibble (may be empty)
#' @keywords internal
scrape_physician_full <- function(first_name, last_name, npi, state = NULL) {
  log_message(paste("Searching:", first_name, last_name))

  # Search for profile
  profile_url <- search_physician(first_name, last_name, state)

  if (is.na(profile_url)) {
    log_message("  Not found on Healthgrades")
    return(list(physician = tibble(), locations = tibble()))
  }

  log_message(paste("  Found profile:", profile_url))

  Sys.sleep(request_delay)

  # Extract all data
  result <- extract_profile_full(profile_url)

  # Add NPI and names at the BEGINNING for easy identification
  if (nrow(result$physician) > 0) {
    result$physician <- result$physician %>%
      mutate(
        npi = npi,
        first_name = first_name,
        last_name = last_name,
        scrape_date = Sys.Date()
      ) %>%
      select(npi, first_name, last_name, everything())  # NPI and names first
  }

  if (nrow(result$locations) > 0) {
    result$locations <- result$locations %>%
      mutate(
        npi = npi,
        first_name = first_name,
        last_name = last_name
      ) %>%
      select(npi, first_name, last_name, street, city, state, zip, latitude, longitude, phone)
    log_message(paste("  Found", nrow(result$locations), "location(s)"))
  } else {
    log_message("  No locations extracted")
  }

  # Log extracted fields
  if (nrow(result$physician) > 0) {
    p <- result$physician
    fields_found <- c()
    if (!is.na(p$est_age)) fields_found <- c(fields_found, paste0("age~", p$est_age))
    if (!is.na(p$gender)) fields_found <- c(fields_found, p$gender)
    if (!is.na(p$rating)) fields_found <- c(fields_found, paste0("rating:", round(p$rating, 1)))
    if (!is.na(p$photo_url)) fields_found <- c(fields_found, "photo")
    if (!is.na(p$telehealth) && p$telehealth) fields_found <- c(fields_found, "telehealth")

    if (length(fields_found) > 0) {
      log_message(paste("  Data:", paste(fields_found, collapse = ", ")))
    }
  }

  return(result)
}

# Note: Checkpoint functions moved to R/utils/generic_checkpoint.R

# ============================================================================
# Main Script
# ============================================================================

#' Main scraping orchestrator for Healthgrades data
#'
#' @description
#' Loads ENT physician data from Excel, processes through search/scrape,
#' maintains checkpoint for resume capability, and exports results to Excel
#' with physician and location sheets.
#'
#' @param max_physicians Optional integer to limit processing (NULL for all)
#' @return List with physicians tibble and locations tibble
#' @keywords internal
main <- function(max_physicians = NULL) {
  log_message("========================================")
  log_message("Healthgrades FULL Data Scraper")
  log_message("========================================")

  # Load ENT data
  log_message(paste("Loading ENT data from:", input_file))
  ent_data <- read_excel(input_file)


  # Filter to physicians with names
  ent_to_search <- ent_data %>%
    filter(!is.na(first_name) & !is.na(last_name) & !is.na(npi_final)) %>%
    select(npi = npi_final, first_name, last_name, state = state_final) %>%
    distinct(npi, .keep_all = TRUE)

  log_message(paste("ENTs to search:", nrow(ent_to_search)))

  # Load checkpoint
  checkpoint <- load_generic_checkpoint(
    checkpoint_file = checkpoint_file,
    default_structure = list(
      physicians = tibble(),
      locations = tibble(),
      completed_npis = c()
    ),
    verbose = FALSE  # We'll log manually
  )
  if (length(checkpoint$completed_npis) > 0) {
    log_message(paste("Checkpoint loaded:", length(checkpoint$completed_npis),
                     "physicians already processed"))
  }
  completed_npis <- checkpoint$completed_npis
  all_physicians <- checkpoint$physicians
  all_locations <- checkpoint$locations

  # Filter to remaining
  remaining <- ent_to_search %>%
    filter(!(npi %in% completed_npis))

  # Apply max limit if specified
  if (!is.null(max_physicians) && nrow(remaining) > max_physicians) {
    remaining <- remaining %>% slice_head(n = max_physicians)
  }

  log_message(paste("Processing:", nrow(remaining), "physicians"))

  if (nrow(remaining) == 0) {
    log_message("All physicians already processed!")
    return(list(physicians = all_physicians, locations = all_locations))
  }

  # Process physicians
  checkpoint_interval <- 25
  batch_count <- 0

  for (i in seq_len(nrow(remaining))) {
    row <- remaining[i, ]

    # Scrape
    result <- tryCatch({
      scrape_physician_full(
        first_name = row$first_name,
        last_name = row$last_name,
        npi = row$npi,
        state = row$state
      )
    }, error = function(e) {
      log_message(paste("  Error:", e$message))
      list(physician = tibble(), locations = tibble())
    })

    # Record results
    if (nrow(result$physician) > 0) {
      all_physicians <- bind_rows(all_physicians, result$physician)
    }
    if (nrow(result$locations) > 0) {
      all_locations <- bind_rows(all_locations, result$locations)
    }

    completed_npis <- c(completed_npis, row$npi)
    batch_count <- batch_count + 1

    # Checkpoint
    if (batch_count >= checkpoint_interval) {
      save_generic_checkpoint(
        physicians = all_physicians,
        locations = all_locations,
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
    if (i %% 25 == 0) {
      log_message(paste("Progress:", i, "/", nrow(remaining)))
    }
  }

  # Final checkpoint
  save_generic_checkpoint(
    physicians = all_physicians,
    locations = all_locations,
    completed_npis = completed_npis,
    checkpoint_file = checkpoint_file,
    verbose = FALSE
  )
  log_message(paste("Final checkpoint saved:", length(completed_npis), "physicians processed"))

  # ============================================================================
  # Summary and Save
  # ============================================================================

  log_message("========================================")
  log_message("Results Summary")
  log_message("========================================")

  if (nrow(all_physicians) > 0) {
    log_message(paste("Physicians with data:", nrow(all_physicians)))
    log_message(paste("  With est_age:", sum(!is.na(all_physicians$est_age))))
    log_message(paste("  With gender:", sum(!is.na(all_physicians$gender))))
    log_message(paste("  With rating:", sum(!is.na(all_physicians$rating))))
    log_message(paste("  With photo:", sum(!is.na(all_physicians$photo_url))))
    log_message(paste("  Telehealth:", sum(all_physicians$telehealth == TRUE, na.rm = TRUE)))
  }

  if (nrow(all_locations) > 0) {
    log_message(paste("Total locations:", nrow(all_locations)))
    log_message(paste("NPIs with locations:", n_distinct(all_locations$npi)))
  }

  # Remove columns that are entirely NA (cleaner output)
  remove_empty_cols <- function(df) {
    df %>% select(where(~!all(is.na(.))))
  }

  # Add location summary to physicians sheet
  if (nrow(all_physicians) > 0 && nrow(all_locations) > 0) {
    loc_summary <- all_locations %>%
      group_by(npi) %>%
      summarise(
        n_locations = n(),
        locations_summary = paste(
          paste0(city, ", ", state),
          collapse = " | "
        ),
        all_addresses = paste(
          paste0(street, ", ", city, ", ", state, " ", zip),
          collapse = " || "
        ),
        .groups = "drop"
      )

    all_physicians <- all_physicians %>%
      left_join(loc_summary, by = "npi") %>%
      mutate(
        n_locations = ifelse(is.na(n_locations), 0, n_locations)
      ) %>%
      # Reorder to put locations near the beginning
      select(npi, first_name, last_name, n_locations, locations_summary, everything())
  }

  # Save to Excel
  sheets <- list()
  if (nrow(all_physicians) > 0) {
    sheets[["Physicians"]] <- remove_empty_cols(all_physicians)
  }
  if (nrow(all_locations) > 0) {
    sheets[["Locations"]] <- remove_empty_cols(all_locations)
  }

  if (length(sheets) > 0) {
    write_xlsx(sheets, output_file)
    log_message(paste("Saved to:", output_file))
  }

  log_message("========================================")
  log_message("COMPLETE")
  log_message("========================================")

  return(list(physicians = all_physicians, locations = all_locations))
}

# ============================================================================
# Photo Download Function
# ============================================================================

#' Download physician photos to local folder
#'
#' @param physicians_data [tibble]: with photo_url and npi columns
#' @param output_dir Directory to save photos (default: "physician_photos")
#' @param delay Seconds between downloads (default: 1)
#' @return Updated tibble with local_photo_path column
download_photos <- function(physicians_data, output_dir = "physician_photos", delay = 1) {
  if (!"photo_url" %in% names(physicians_data)) {
    log_message("No photo_url column found")
    return(physicians_data)
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    log_message(paste("Created directory:", output_dir))
  }

  # Filter to rows with photo URLs
  has_photo <- physicians_data %>%
    filter(!is.na(photo_url) & photo_url != "")

  log_message(paste("Downloading", nrow(has_photo), "photos"))

  local_paths <- rep(NA_character_, nrow(physicians_data))

  for (i in seq_len(nrow(has_photo))) {
    row <- has_photo[i, ]
    npi <- row$npi

    # Determine file extension
    ext <- ".jpg"  # Default
    if (str_detect(row$photo_url, "\\.png")) ext <- ".png"
    if (str_detect(row$photo_url, "\\.gif")) ext <- ".gif"
    if (str_detect(row$photo_url, "\\.webp")) ext <- ".webp"

    filename <- paste0(npi, ext)
    filepath <- file.path(output_dir, filename)

    # Skip if already exists
    if (file.exists(filepath)) {
      log_message(paste("  Already exists:", filename))
      # Find original row index
      orig_idx <- which(physicians_data$npi == npi)
      if (length(orig_idx) > 0) local_paths[orig_idx[1]] <- filepath
      next
    }

    # Download
    tryCatch({
      response <- GET(
        row$photo_url,
        add_headers(.headers = browser_headers),
        timeout(30),
        write_disk(filepath, overwrite = TRUE)
      )

      if (status_code(response) == 200) {
        log_message(paste("  Downloaded:", filename))
        orig_idx <- which(physicians_data$npi == npi)
        if (length(orig_idx) > 0) local_paths[orig_idx[1]] <- filepath
      } else {
        log_message(paste("  Failed:", filename, "- HTTP", status_code(response)))
      }
    }, error = function(e) {
      log_message(paste("  Error downloading", filename, ":", e$message))
    })

    Sys.sleep(delay)

    if (i %% 25 == 0) {
      log_message(paste("  Progress:", i, "/", nrow(has_photo)))
    }
  }

  physicians_data$local_photo_path <- local_paths
  log_message(paste("Photos saved to:", output_dir))

  return(physicians_data)
}

# ============================================================================
# Test function for small batch
# ============================================================================

#' Test scraper with small batch
#'
#' @description
#' Runs main scraping workflow on a limited number of physicians for testing.
#' Optionally downloads profile photos.
#'
#' @param n [integer]: number of physicians to process (default: 10)
#' @param download_photos_flag [logical]: flag to download photos (default: FALSE)
#' @return List with physicians tibble and locations tibble
#' @keywords internal
test_scrape <- function(n = 10, download_photos_flag = FALSE) {
  log_message(paste("Running test scrape for", n, "physicians"))
  result <- main(max_physicians = n)

  if (download_photos_flag && nrow(result$physicians) > 0) {
    log_message("Downloading photos...")
    result$physicians <- download_photos(result$physicians)
  }

  return(result)
}

# ============================================================================
# Run
# ============================================================================

if (sys.nframe() == 0) {
  main()
}
