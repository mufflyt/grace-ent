#!/usr/bin/env Rscript
# =============================================================================
# County-level CMS enrollment covariates for the ENT wait-time model.
#
# Pulls the CMS "Medicare Monthly Enrollment" public dataset (county geography)
# and keeps the latest annual snapshot:
#   medicare_benes        - TOT_BENES        (total Medicare beneficiaries)
#   dual_medicaid_benes   - DUAL_TOT_BENES   (Medicare-Medicaid dual eligibles)
#   dual_pct              - duals / Medicare  (share also on Medicaid)
#
# NOTE ON MEDICAID: CMS does not publish TOTAL Medicaid enrollment at the county
# level nationally (Medicaid is administered/reported by state). The dual-
# eligible count above is the real, CMS-published county-level Medicaid signal
# (Medicare beneficiaries who ALSO have Medicaid). It excludes non-elderly,
# non-disabled Medicaid enrollees. For full county Medicaid coverage you would
# use ACS table C27007 (needs a Census API key).
#
# The download URL is resolved from the CMS data catalog so this keeps working
# when CMS reposts a newer monthly file.
#
# Output: data/raw/county_cms_enrollment.csv
#   county_fips, snapshot_year, medicare_benes, dual_medicaid_benes, dual_pct
#
# Run: Rscript scripts/build_cms_enrollment.R
# =============================================================================

options(stringsAsFactors = FALSE, timeout = 900)
cache <- file.path(tempdir(), "geo_src"); dir.create(cache, showWarnings = FALSE)

# 1) Resolve the latest Medicare Monthly Enrollment CSV from the CMS catalog
resolve_url <- function() {
  fallback <- paste0("https://data.cms.gov/sites/default/files/2026-06/",
    "38b2bd15-bfb9-421d-9bf7-06b9449a94ef/",
    "Medicare%20Monthly%20Enrollment%20Data_March%202026.csv")
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(fallback)
  cat <- tryCatch(jsonlite::fromJSON("https://data.cms.gov/data.json",
                                     simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(cat)) return(fallback)
  for (d in cat$dataset) {
    if (identical(tolower(d$title), "medicare monthly enrollment")) {
      for (dist in d$distribution) {
        u <- dist$downloadURL
        if (!is.null(u) && grepl("\\.csv$", u)) return(u)
      }
    }
  }
  fallback
}

f_csv <- file.path(cache, "cms_mme.csv")
if (!file.exists(f_csv)) utils::download.file(resolve_url(), f_csv, quiet = TRUE)

# 2) Read only the 6 needed columns (skip the other 54 for speed/memory)
hdr <- strsplit(readLines(f_csv, n = 1), ",")[[1]]
keep <- c("YEAR","MONTH","BENE_GEO_LVL","BENE_FIPS_CD","TOT_BENES","DUAL_TOT_BENES")
cc <- ifelse(hdr %in% keep, "character", "NULL")
df <- utils::read.csv(f_csv, colClasses = cc, check.names = FALSE)

# 3) County geography, latest annual (MONTH == "Year") snapshot
df <- df[df$BENE_GEO_LVL == "County" & df$MONTH == "Year", ]
yr <- max(df$YEAR)
df <- df[df$YEAR == yr, ]

num <- function(x) suppressWarnings(as.numeric(x))   # suppressed cells -> NA
out <- data.frame(
  county_fips         = sprintf("%05s", df$BENE_FIPS_CD),
  snapshot_year       = yr,
  medicare_benes      = num(df$TOT_BENES),
  dual_medicaid_benes = num(df$DUAL_TOT_BENES)
)
out$dual_pct <- ifelse(!is.na(out$medicare_benes) & out$medicare_benes > 0,
                       round(out$dual_medicaid_benes / out$medicare_benes, 4), NA)
out <- out[order(out$county_fips), ]
utils::write.csv(out, "data/raw/county_cms_enrollment.csv", row.names = FALSE, na = "")
cat(sprintf("Wrote data/raw/county_cms_enrollment.csv: %d counties (snapshot %s)\n",
            nrow(out), yr))
