#!/usr/bin/env Rscript
# =============================================================================
# Build a ZIP -> county FIPS -> CBSA/MSA crosswalk for the ENT mystery-caller
# study. This is the join backbone that lets ZIP-keyed physicians pick up
# county- and MSA-level covariates:
#   * KFF hospital-market HHI            (MSA / CBSA level)
#   * CDC Social Vulnerability Index     (county roll-up of tract SVI)
#   * CMS county Medicare/Medicaid enrollment
#
# Sources (all free, no API key):
#   1. Census 2020 ZCTA5 -> County relationship file (dominant county by land area)
#   2. Census/OMB CBSA delineation file (county -> CBSA code + title + type)
#
# Output: data/raw/zip_to_county_cbsa.csv  (zip, county_fips, cbsa_code,
#         cbsa_title, cbsa_type)
#
# Run: Rscript scripts/build_geo_crosswalk.R
# =============================================================================

suppressPackageStartupMessages({library(readxl)})
options(stringsAsFactors = FALSE, timeout = 300)

here <- function(...) file.path("data", "raw", ...)
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
cache <- file.path(tempdir(), "geo_src")
dir.create(cache, showWarnings = FALSE)

url_zcta <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt"
url_cbsa <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls"
f_zcta <- file.path(cache, "zcta_county.txt")
f_cbsa <- file.path(cache, "cbsa_list1.xls")
if (!file.exists(f_zcta)) utils::download.file(url_zcta, f_zcta, quiet = TRUE)
if (!file.exists(f_cbsa)) utils::download.file(url_cbsa, f_cbsa, quiet = TRUE)

# 1) ZIP(ZCTA) -> dominant county by land area of the ZCTA-county intersection
z <- read.delim(f_zcta, sep = "|", colClasses = "character",
                check.names = FALSE, quote = "")
z <- z[nzchar(z$GEOID_ZCTA5_20),
       c("GEOID_ZCTA5_20", "GEOID_COUNTY_20", "AREALAND_PART")]
z$AREALAND_PART <- suppressWarnings(as.numeric(z$AREALAND_PART))
z$AREALAND_PART[is.na(z$AREALAND_PART)] <- 0
z <- z[order(z$GEOID_ZCTA5_20, -z$AREALAND_PART), ]
zip2cty <- z[!duplicated(z$GEOID_ZCTA5_20), c("GEOID_ZCTA5_20", "GEOID_COUNTY_20")]
names(zip2cty) <- c("zip", "county_fips")

# Institutional point-ZIPs that are not ZCTAs (single academic med centers).
# Mapped to their host county so 100% of the call log resolves.
patch <- data.frame(
  zip         = c("03756","06030","27710","94143","87131","44195","92357"),
  county_fips = c("33009","09003","37063","06075","35001","39035","06071")
)
zip2cty <- zip2cty[!zip2cty$zip %in% patch$zip, ]
zip2cty <- rbind(zip2cty, patch)

# 2) county -> CBSA (OMB/Census delineation; first two rows are title banner)
cb <- as.data.frame(readxl::read_excel(f_cbsa, skip = 2))
names(cb) <- gsub("[^A-Za-z0-9]+", "_", tolower(names(cb)))
sc <- grep("fips_state",  names(cb), value = TRUE)[1]
cc <- grep("fips_county", names(cb), value = TRUE)[1]
cb <- cb[!is.na(cb[[sc]]) & !is.na(cb[[cc]]), ]
cb$county_fips <- paste0(sprintf("%02s", cb[[sc]]), sprintf("%03s", cb[[cc]]))
cty2cbsa <- data.frame(
  county_fips = cb$county_fips,
  cbsa_code   = cb[[grep("^cbsa_code$", names(cb))[1]]],
  cbsa_title  = cb[[grep("cbsa_title",  names(cb))[1]]],
  cbsa_type   = cb[[grep("metropolitan_micropolitan", names(cb))[1]]]
)

# 3) join and write
xwalk <- merge(zip2cty, cty2cbsa, by = "county_fips", all.x = TRUE)
xwalk <- xwalk[order(xwalk$zip), c("zip","county_fips","cbsa_code","cbsa_title","cbsa_type")]
out <- here("zip_to_county_cbsa.csv")
utils::write.csv(xwalk, out, row.names = FALSE, na = "")
cat(sprintf("Wrote %s: %d ZIPs, %d with a CBSA (%.0f%%)\n",
            out, nrow(xwalk), sum(!is.na(xwalk$cbsa_code)),
            100 * mean(!is.na(xwalk$cbsa_code))))
