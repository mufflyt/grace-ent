#!/usr/bin/env Rscript
# =============================================================================
# Population-weighted ZIP-level CDC/ATSDR Social Vulnerability Index (2022).
#
# SVI is published at the census-tract level. The call log is keyed by ZIP, so
# each ZIP is assigned a population-weighted average of the SVI of every tract
# it overlaps. Weight = tract population apportioned to the ZIP by land-area
# share of the ZCTA-tract intersection (assumes uniform density within a tract):
#     w_zt = E_TOTPOP_t * (AREALAND_PART_zt / AREALAND_TRACT_t)
#
# Sources (free, no key):
#   1. CDC/ATSDR SVI 2022 US tract file (RPL_THEMES overall + 4 theme ranks)
#   2. Census 2020 ZCTA5 -> tract relationship file (area of each intersection)
#
# Output: data/raw/zip_svi_2022.csv
#   zip, svi_overall, svi_socioeconomic, svi_household, svi_minority_language,
#   svi_housing_transport, n_tracts, pop_weighted
#
# Run: Rscript scripts/build_zip_svi.R
# =============================================================================

options(stringsAsFactors = FALSE, timeout = 600)
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
cache <- file.path(tempdir(), "geo_src")
dir.create(cache, showWarnings = FALSE)

url_svi <- "https://svi.cdc.gov/Documents/Data/2022/csv/states/SVI_2022_US.csv"
url_zt  <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_tract20_natl.txt"
f_svi <- file.path(cache, "svi_2022_tract.csv")
f_zt  <- file.path(cache, "zcta_tract.txt")
if (!file.exists(f_svi)) utils::download.file(url_svi, f_svi, quiet = TRUE)
if (!file.exists(f_zt))  utils::download.file(url_zt,  f_zt,  quiet = TRUE)

# 1) SVI tracts. RPL_* are percentile ranks in [0,1]; -999 = suppressed.
svi <- read.csv(f_svi, colClasses = "character")
keep <- c(FIPS = "FIPS", pop = "E_TOTPOP",
          svi_overall = "RPL_THEMES", svi_socioeconomic = "RPL_THEME1",
          svi_household = "RPL_THEME2", svi_minority_language = "RPL_THEME3",
          svi_housing_transport = "RPL_THEME4")
svi <- svi[, keep]; names(svi) <- names(keep)
for (c in c("pop","svi_overall","svi_socioeconomic","svi_household",
            "svi_minority_language","svi_housing_transport")) {
  svi[[c]] <- suppressWarnings(as.numeric(svi[[c]]))
  svi[[c]][svi[[c]] == -999] <- NA
}
svi$FIPS <- sprintf("%011s", svi$FIPS)

# 2) ZCTA -> tract intersections with land area of the part and full tract area
zt <- read.delim(f_zt, sep = "|", colClasses = "character",
                 check.names = FALSE, quote = "")
zt <- zt[nzchar(zt$GEOID_ZCTA5_20),
         c("GEOID_ZCTA5_20","GEOID_TRACT_20","AREALAND_PART","AREALAND_TRACT_20")]
names(zt) <- c("zip","FIPS","area_part","area_tract")
zt$FIPS <- sprintf("%011s", zt$FIPS)
zt$area_part  <- suppressWarnings(as.numeric(zt$area_part))
zt$area_tract <- suppressWarnings(as.numeric(zt$area_tract))

# 3) apportioned-population weight, then weighted mean per ZIP
m <- merge(zt, svi, by = "FIPS")
m$w <- m$pop * ifelse(m$area_tract > 0, m$area_part / m$area_tract, 0)
m <- m[is.finite(m$w) & m$w > 0, ]

wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w) & w > 0
                          if (!any(ok)) NA_real_ else sum(x[ok]*w[ok])/sum(w[ok]) }
zips <- split(seq_len(nrow(m)), m$zip)
themes <- c("svi_overall","svi_socioeconomic","svi_household",
            "svi_minority_language","svi_housing_transport")
out <- do.call(rbind, lapply(names(zips), function(z) {
  r <- m[zips[[z]], ]
  vals <- vapply(themes, function(t) round(wmean(r[[t]], r$w), 4), numeric(1))
  data.frame(zip = z, t(vals), n_tracts = nrow(r),
             pop_weighted = round(sum(r$w)), check.names = FALSE)
}))
out <- out[order(out$zip), ]
utils::write.csv(out, "data/raw/zip_svi_2022.csv", row.names = FALSE, na = "")
cat(sprintf("Wrote data/raw/zip_svi_2022.csv: %d ZIPs (%d with an overall SVI)\n",
            nrow(out), sum(!is.na(out$svi_overall))))
