#!/usr/bin/env Rscript
#' @title Specialty Alignment Mapper
#'
#' @description
#' Maps ABOG subspecialties to ABMS board certification specialties.
#' Used for cross-specialty NPI disambiguation in ABMS matching.
#'
#' @family npi-matching
#' @name specialty_alignment_mapper
NULL

# Single source-of-truth alignment map — referenced by both functions below.
.ABOG_TO_ABMS_ALIGNMENT_MAP <- list(
  "Obstetrics & Gynecology" = c(
    "Obstetrics and Gynecology",
    "Maternal-Fetal Medicine",
    "Gynecologic Oncology",
    "Female Pelvic Medicine and Reconstructive Surgery",
    "Reproductive Endocrinology and Infertility",
    "Reproductive Endocrinology",
    "Complex Family Planning",
    "Hospice and Palliative Medicine"
  ),
  "Family Medicine" = c(
    "Family Medicine"
  ),
  "Clinical Genetics" = c(
    "Medical Genetics",
    "Clinical Genetics"
  ),
  "Internal Medicine" = c(
    "Internal Medicine"
  )
)

#' Map ABOG Subspecialty to ABMS Specialty
#'
#' Maps an ABOG subspecialty name to its corresponding ABMS board certification
#' specialty. This mapping is used during NPI disambiguation when a physician
#' has multiple ABMS certifications in different specialties.
#'
#' @param abog_subspecialty `character`: Subspecialty name from ABOG data
#'   (credentials.abog_providers.subspecialty_name). Case-sensitive.
#'
#' @return Character. Aligned ABMS specialty or NA_character_ if no mapping
#'   exists.
#'   Possible return values:
#'   \itemize{
#'     \item \code{"Obstetrics & Gynecology"} - OB/GYN and subspecialties
#'     \item \code{"Family Medicine"} - Family medicine
#'     \item \code{"Clinical Genetics"} - Medical genetics specialties
#'     \item \code{"Internal Medicine"} - Internal medicine
#'     \item \code{NA_character_} - No mapping found
#'   }
#'
#' @details
#' The mapping prioritizes exact ABOG subspecialty matching to ABMS specialty
#' categories. This is critical for disambiguating physicians who hold multiple
#' board certifications (e.g., both OB/GYN and Family Medicine).
#'
#' **Mapping Rules:**
#'
#' \strong{Obstetrics & Gynecology:}
#' \itemize{
#'   \item Obstetrics and Gynecology (general certification)
#'   \item Maternal-Fetal Medicine (MFM)
#'   \item Gynecologic Oncology
#'   \item Female Pelvic Medicine and Reconstructive Surgery (FPMRS)
#'   \item Reproductive Endocrinology and Infertility (REI)
#'   \item Complex Family Planning
#' }
#'
#' \strong{Family Medicine:}
#' \itemize{
#'   \item Family Medicine
#' }
#'
#' \strong{Clinical Genetics:}
#' \itemize{
#'   \item Medical Genetics
#'   \item Clinical Genetics
#' }
#'
#' \strong{Internal Medicine:}
#' \itemize{
#'   \item Internal Medicine
#' }
#'
#' @examples
#' \dontrun{
#' map_abog_to_abms_specialty("Maternal-Fetal Medicine")
#' # Returns: "Obstetrics & Gynecology"
#'
#' map_abog_to_abms_specialty("Family Medicine")
#' # Returns: "Family Medicine"
#'
#' map_abog_to_abms_specialty("Unknown Specialty")
#' # Returns: NA_character_
#' }
#'
#' @seealso
#' \code{\link{disambiguate_cross_specialty_matches}} for using this mapping
#' in NPI disambiguation.
#'
#' @export
map_abog_to_abms_specialty <- function(abog_subspecialty) {
  checkmate::assert_string(abog_subspecialty, na.ok = TRUE)

  # Handle NA input
  if (is.na(abog_subspecialty) || nchar(trimws(abog_subspecialty)) == 0) {
    return(NA_character_)
  }

  # Search for matching ABMS specialty
  for (abms_specialty in names(.ABOG_TO_ABMS_ALIGNMENT_MAP)) {
    if (abog_subspecialty %in% .ABOG_TO_ABMS_ALIGNMENT_MAP[[abms_specialty]]) {
      return(abms_specialty)
    }
  }

  # No mapping found
  return(NA_character_)
}

#' Get All ABOG Subspecialties for an ABMS Specialty
#'
#' Returns the list of ABOG subspecialties that map to a given ABMS specialty.
#' Useful for validation and documentation.
#'
#' @param abms_specialty `character`: ABMS specialty name.
#'
#' @return Character vector. ABOG subspecialties that map to this ABMS
#'   specialty,
#'   or NULL if specialty not recognized.
#'
#' @examples
#' \dontrun{
#' get_abog_subspecialties_for_abms("Obstetrics & Gynecology")
#' # Returns: c("Obstetrics and Gynecology", "Maternal-Fetal Medicine", ...)
#' }
#'
#' @export
# Renamed 2026-06-21 from get_abog_subspecialties() — that name now belongs solely
# to the canonical config-driven getter in R/subspecialty_standardizer.R. This is a
# different function (ABMS specialty -> list of mapped ABOG subspecialty full names),
# a name collision that made get_standard_subspecialties() load-order-dependent.
get_abog_subspecialties_for_abms <- function(abms_specialty) {
  checkmate::assert_string(abms_specialty, na.ok = TRUE)

  if (abms_specialty %in% names(.ABOG_TO_ABMS_ALIGNMENT_MAP)) {
    return(.ABOG_TO_ABMS_ALIGNMENT_MAP[[abms_specialty]])
  } else {
    return(NULL)
  }
}
