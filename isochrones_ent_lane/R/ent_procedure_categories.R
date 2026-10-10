# =============================================================================
# ENT Procedure Categories — YAML loader
# =============================================================================
# Replaces the inline `ent_procedure_cpt_codes` tribble that was previously
# defined in user code.  Reads config/ent_procedure_categories.yml and
# returns a tibble with EXACTLY the same columns as the original tribble:
#
#   procedure_category, cpt_code, procedure_group, inclusive_representative_code
#
# Optional additional columns when keep_descriptions = TRUE:
#   description                       — CPT description for the row's code
#   representative_description        — description of the anchor CPT
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(tibble)
  library(dplyr)
  library(here)
})

#' @title Load ENT Procedure Categories from YAML
#'
#' @description
#' Replaces the inline `ent_procedure_cpt_codes` tribble that was previously
#' defined in user code. Reads config/ent_procedure_categories.yml and
#' returns a tibble with consistent columns.
#' @return tibble with columns: procedure_category, cpt_code,
#'   procedure_group, inclusive_representative_code (+ optional
#'   description columns).
#' @examples
#' \dontrun{
#' ent_procedure_cpt_codes <- load_ent_procedure_categories()
#' }
load_ent_procedure_categories <- function(path = NULL,
                                          keep_descriptions = FALSE) {
  if (is.null(path)) {
    path <- here::here("config", "ent_procedure_categories.yml")
  }
  if (!file.exists(path)) stop("YAML not found: ", path)

  cfg <- yaml::read_yaml(path)
  if (is.null(cfg$categories) || !length(cfg$categories)) {
    stop("YAML has no `categories:` section: ", path)
  }

  rows <- lapply(cfg$categories, function(cat) {
    cpt_rows <- lapply(cat$codes, function(c) {
      tibble::tibble(
        procedure_category            = cat$name,
        cpt_code                      = c$code,
        procedure_group               = cat$procedure_group %||% cat$name,
        inclusive_representative_code = cat$inclusive_representative_code,
        description                   = c$description %||% NA_character_,
        representative_description    = cat$representative_description %||% NA_character_
      )
    })
    dplyr::bind_rows(cpt_rows)
  })
  out <- dplyr::bind_rows(rows)

  # Closure invariants — fail fast if the YAML is malformed
  stopifnot(
    "Every row needs a cpt_code"                      = all(nzchar(out$cpt_code)),
    "Every row needs an inclusive_representative_code" = all(nzchar(out$inclusive_representative_code)),
    "Representative codes must themselves appear in the codes list" =
      all(out$inclusive_representative_code %in% out$cpt_code)
  )

  if (!keep_descriptions) {
    out <- dplyr::select(out, procedure_category, cpt_code,
                         procedure_group, inclusive_representative_code)
  }
  out
}

#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a
