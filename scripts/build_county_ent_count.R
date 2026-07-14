#!/usr/bin/env Rscript
# =============================================================================
# County-level board-certified ENT supply for the wait-time model.
#
# Counts board-certified ENT physicians per county from the study's own
# ABOto/NPPES universe (ENT-specific -- preferred over AHRF's generic physician
# counts), then adds county population (ACS via the CDC SVI tract file) to
# express supply as a density (ENTs per 100k residents).
#
# Inputs (already in repo):
#   data/raw/board_cert_ent_universe_n11333_zip_aboto.csv   (npi, zip5)
#   data/raw/zip_to_county_cbsa.csv                          (zip -> county_fips)
# Plus the CDC SVI 2022 tract file (county population) -- downloaded if absent.
#
# Output: data/raw/county_ent_count.csv
#   county_fips, county_pop, n_ent, ent_per_100k
#
# Run: Rscript scripts/build_county_ent_count.R
# =============================================================================

options(stringsAsFactors = FALSE, timeout = 600)
cache <- file.path(tempdir(), "geo_src"); dir.create(cache, showWarnings = FALSE)

ent <- read.csv("data/raw/board_cert_ent_universe_n11333_zip_aboto.csv",
                colClasses = "character")
ent$zip5 <- sprintf("%05s", ent$zip5)
xw <- read.csv("data/raw/zip_to_county_cbsa.csv", colClasses = "character")

j <- merge(ent[, c("npi","zip5")], xw[, c("zip","county_fips")],
           by.x = "zip5", by.y = "zip", all.x = TRUE)
unmatched <- sum(is.na(j$county_fips))
cat(sprintf("ENTs: %d | mapped to a county: %d (%.1f%%)\n",
            nrow(j), sum(!is.na(j$county_fips)), 100*mean(!is.na(j$county_fips))))

j <- j[!is.na(j$county_fips), ]
cnt <- as.data.frame(table(county_fips = j$county_fips), responseName = "n_ent")
cnt$county_fips <- as.character(cnt$county_fips)

# County population = sum of tract populations from the CDC SVI 2022 file
f_svi <- file.path(cache, "svi_2022_tract.csv")
if (!file.exists(f_svi)) utils::download.file(
  "https://svi.cdc.gov/Documents/Data/2022/csv/states/SVI_2022_US.csv",
  f_svi, quiet = TRUE)
svi <- read.csv(f_svi, colClasses = "character")[, c("FIPS","E_TOTPOP")]
svi$county_fips <- substr(sprintf("%011s", svi$FIPS), 1, 5)
svi$E_TOTPOP <- suppressWarnings(as.numeric(svi$E_TOTPOP))
svi$E_TOTPOP[svi$E_TOTPOP < 0] <- NA
pop <- aggregate(E_TOTPOP ~ county_fips, svi, sum, na.rm = TRUE)
names(pop)[2] <- "county_pop"

out <- merge(pop, cnt, by = "county_fips", all.x = TRUE)
out$n_ent[is.na(out$n_ent)] <- 0L
out$ent_per_100k <- ifelse(out$county_pop > 0,
                           round(1e5 * out$n_ent / out$county_pop, 2), NA)
out <- out[order(out$county_fips), c("county_fips","county_pop","n_ent","ent_per_100k")]
utils::write.csv(out, "data/raw/county_ent_count.csv", row.names = FALSE, na = "")
cat(sprintf("Wrote data/raw/county_ent_count.csv: %d counties, %d with >=1 ENT\n",
            nrow(out), sum(out$n_ent > 0)))
