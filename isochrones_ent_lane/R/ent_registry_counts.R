#' @title ENT Registry Counts — Single Source of Truth
#'
#' @description
#' Computes all physician counts for the ENT data-integrity figure suite
#' from four input registries. Every figure script sources this file and
#' calls \code{\link{compute_ent_registry_counts}()} so numbers cannot
#' drift between figures.
#'
#' @details
#' Sourced by:
#' \itemize{
#'   \item \code{scripts/improve_ent_euler.R} — Euler diagram
#'   \item \code{scripts/improve_ent_strobe.R} — STROBE attrition
#'   \item \code{scripts/improve_ent_attrition.R} — CONSORT flow
#'   \item \code{scripts/improve_ent_subspecialty_findability.R} — bar chart
#' }
#'
#' Applies CONUS filter
#' (\code{R/geographic_classification.R::NON_CONTIGUOUS_CODES})
#' and deduplicates by NPI before counting. Returns unique-physician counts
#' (not raw registry rows).
#'
#' @name ent_registry_counts
NULL

suppressPackageStartupMessages({
  library(readr)
  library(arrow)
  library(dplyr)
  library(here)
})

# Lazy-load NON_CONTIGUOUS_CODES from the geographic module without
# re-sourcing if it's already in the env.
if (!exists("NON_CONTIGUOUS_CODES")) {
  source(here::here("R", "geographic_classification.R"))
}

# =============================================================================
# ABOto SUBSPECIALTY PRIORITY — single source of truth for the priority order
# =============================================================================
# When a physician holds multiple ABOto subspecialty certs (~4 physicians in
# the current data hold Pediatric + Sleep), this priority resolves the
# classification deterministically. Earlier-listed = higher priority.
#
# ORDERING RATIONALE: rarest-cert-wins. As of 2026:
#   Pediatric (n=369, 3.3% of cohort)
#   Neurotology (n=399, 3.5%)
#   Sleep Medicine (n=225, 2.0%)   ← actually rarer than Neurotology but lower
#                                    priority because Neurotology was
#                                    historically considered the "harder"
#                                    subspecialty
#   General Otolaryngology (n=10,357, 91.3% — default catch-all)
#
# DO NOT REORDER without explicit user approval — changing this changes
# every subspecialty count and percentage in the published figures.
# Any reorder must be paired with an updated DATA_INTEGRITY_NOTES.md.
ABOTO_SUBSPECIALTY_PRIORITY <- c(
  "Pediatric Otolaryngology",   # Complex Pediatric Otolaryngology cert
  "Neurotology",                # Neurotology cert
  "Sleep Medicine",             # Sleep Medicine cert
  "General Otolaryngology"      # default — only the foundational cert
)

#' Compute ENT Registry Counts (Single Source of Truth)
#'
#' Loads four ENT input registries (ABOto raw certifications, ABOto-NPI
#' matched file, NPPES 207Y* taxonomy extract, ENThealth roster), applies
#' the CONUS geographic filter, deduplicates by NPI, and computes all
#' counts used by the figure suite. All figure scripts call this function
#' so numbers cannot drift between figures.
#'
#' @section Data Pipeline:
#' \preformatted{
#' ABOto Raw Certifications (16,228 rows)
#'   → Deduplicate by abohns_id → 15,171 unique physicians
#'   → CONUS filter (exclude AK, HI, PR, territories) → 14,996
#'   → Remove non-practicing → 11,477 active rows
#'   → Deduplicate by NPI → 11,350 unique active board-certified
#'   → Cross-match with NPPES + ENThealth → 4,578 triple match
#' }
#'
#' @section Subspecialty Classification:
#' Uses ABOto certification records \strong{only} (not NPPES taxonomy).
#' NPPES is self-reported and includes codes (Rhinology, Laryngology,
#' Facial Plastics, Allergy) that are not ABOto-issued subspecialties.
#' Priority for multi-certified physicians:
#' Pediatric > Neurotology > Sleep Medicine > General Otolaryngology.
#'
#' @section Closure Assertions:
#' Eight \code{stopifnot()} assertions verify that every count in the
#' attrition chain and Euler diagram balances exactly. If any assertion
#' fails, the function stops with a descriptive error before returning
#' inconsistent numbers to callers.
#'
#' @param paths Optional named list overriding default input file paths.
#'   Keys: \code{abohns_raw} (ABOto certifications CSV),
#'   \code{abohns_matched} (ABOto-NPI matched CSV),
#'   \code{nppes} (NPPES 207Y* taxonomy parquet),
#'   \code{ent} (ENThealth roster CSV).
#'   Defaults read from \code{data/ent/} and \code{artifacts/ent/}.
#'
#' @return Named list with four sub-lists and top-level convenience scalars:
#'   \describe{
#'     \item{\code{$chain}}{STROBE/CONSORT attrition counts: \code{n_raw},
#'       \code{n_unique}, \code{n_conus}, \code{n_active_rows}, \code{n_board},
#'       \code{drop_dedupe}, \code{drop_conus}, \code{drop_retired},
#'       \code{drop_npi_dup}, \code{drop_cross}, \code{n_conus_drop_by_state}.}
#'     \item{\code{$sets}}{Euler diagram set totals: \code{n_npi_univ}
#'       (NPPES), \code{n_board} (ABOto), \code{n_enthealth}, \code{n_triple}.}
#'     \item{\code{$zones}}{Seven Euler zone counts: \code{nppes_only},
#'       \code{board_only}, \code{ent_only}, \code{nppes_board},
#'       \code{nppes_ent}, \code{board_ent}, \code{triple}. Math closes
#'       on \code{$sets} totals.}
#'     \item{\code{$by_subspecialty}}{Data frame with columns
#'       \code{subspecialty}, \code{n_total}, \code{n_findable},
#'       \code{pct_findable} — ABOto-based discoverability by subspecialty.}
#'   }
#'
#' @examples
#' \dontrun{
#' counts <- compute_ent_registry_counts()
#' # STROBE chain
#' counts$chain$n_board   # 11,350 unique active board-certified
#' # Euler headline
#' counts$sets$n_triple   # 4,578 in all three registries
#' # Subspecialty discoverability
#' counts$by_subspecialty # General 38%, Pediatric 72%, etc.
#' }
#'
#' @seealso
#' \code{scripts/improve_ent_euler.R} — area-proportional Euler diagram
#' \code{scripts/improve_ent_strobe.R} — STROBE attrition figure
#' \code{scripts/improve_ent_attrition.R} — CONSORT flow diagram
#' \code{scripts/improve_ent_subspecialty_findability.R} — subspecialty bar chart
#'
#' @family ent-figures
#' @concept ent-registry
#' @concept data-integrity
#' @export
compute_ent_registry_counts <- function(paths = NULL, physician_only = FALSE) {
  # physician_only = TRUE excludes NPIs whose credential text clearly
  # identifies a non-physician (PA-C, NP, APRN, CRNP, AUD, audiologist,
  # PhD/MS-only, DDS, etc.). Applied to BOTH NPPES (via the `credential`
  # column) and ENThealth (via is_md / is_do / credentials). ABOto is left
  # untouched because it only certifies physicians by construction.
  # NPIs with missing/blank credential text are RETAINED — many board-
  # certified physicians leave that field blank.
  #
  # When physician_only = TRUE the cohort lock check is skipped (the lock
  # pins the unfiltered cohort); re-run scripts/write_ent_cohort_lock.R if
  # you want a physician-only lock.

  default_paths <- list(
    abohns_raw     = here::here("data/ent/abohns_certifications_clean.csv"),
    abohns_matched = here::here("data/ent/abohns_with_npi_master.csv"),
    nppes          = here::here("artifacts/ent/ent_from_nppes_flat_file.parquet"),
    ent            = here::here("data/ent/ent_master_roster_final_20260513.csv")
  )
  paths <- modifyList(default_paths, paths %||% list())
  using_default_paths <- identical(paths, default_paths)

  for (p in paths) if (!file.exists(p)) stop("Missing input: ", p)

  # Compile-time check on the priority vector — protects against
  # accidental reordering or renaming.
  stopifnot(
    "ABOTO_SUBSPECIALTY_PRIORITY must have exactly 4 levels" =
      length(ABOTO_SUBSPECIALTY_PRIORITY) == 4L,
    "ABOTO_SUBSPECIALTY_PRIORITY must end with 'General Otolaryngology'" =
      ABOTO_SUBSPECIALTY_PRIORITY[4] == "General Otolaryngology"
  )

  abohns_raw     <- read_csv(paths$abohns_raw,     show_col_types = FALSE)
  abohns_matched <- read_csv(paths$abohns_matched, show_col_types = FALSE)
  nppes_all      <- read_parquet(paths$nppes)
  ent_all        <- read_csv(paths$ent,            show_col_types = FALSE)

  # ── Optional physician-only filter ──────────────────────────────────────
  if (isTRUE(physician_only)) {
    # 2026-06-02: delegate to the shared, YAML-backed classifier in
    # R/utils/physician_identity_filter.R so ENT and every OB-GYN
    # subspecialty share one implementation. Blank credentials are RETAINED
    # by default (~4% of triple-match physicians have no text).
    source(here::here("R", "utils", "physician_identity_filter.R"))
    .is_physician_text <- function(txt) {
      cls <- classify_physician_credential(txt)
      cls$physician_likely & !cls$non_physician_likely
    }

    n_nppes_before <- nrow(nppes_all)
    nppes_all <- nppes_all %>% filter(.is_physician_text(credential))
    message(sprintf("[ent_registry_counts] physician_only=TRUE: dropped %d non-physician NPIs from NPPES (%d -> %d)",
                    n_nppes_before - nrow(nppes_all), n_nppes_before, nrow(nppes_all)))

    n_ent_before <- nrow(ent_all)
    # ENThealth has is_md / is_do booleans; fall back to credential text
    # only if those columns are missing on a row.
    if (all(c("is_md", "is_do") %in% names(ent_all))) {
      keep_ent <- (!is.na(ent_all$is_md) & ent_all$is_md) |
                  (!is.na(ent_all$is_do) & ent_all$is_do)
      ambiguous <- !keep_ent
      if (any(ambiguous) && "credentials" %in% names(ent_all)) {
        keep_ent[ambiguous] <- .is_physician_text(ent_all$credentials[ambiguous])
      }
      ent_all <- ent_all[keep_ent, , drop = FALSE]
    } else if ("credentials" %in% names(ent_all)) {
      ent_all <- ent_all %>% filter(.is_physician_text(credentials))
    }
    message(sprintf("[ent_registry_counts] physician_only=TRUE: dropped %d non-physician rows from ENThealth (%d -> %d)",
                    n_ent_before - nrow(ent_all), n_ent_before, nrow(ent_all)))
  }

  # state_full -> 2-letter code
  # 2026-05-20 (H4-cleanup): swapped the hand-rolled lookup for the
  # canonical R/utils/state_normalization.R helper. The old lookup
  # covered 50 states + DC + PR only; the helper additionally covers
  # the 4 other inhabited US territories (GU, VI, AS, MP), the freely-
  # associated states (FM, MH, PW), APO/FPO military codes (AA, AE,
  # AP), case-insensitive matching, and `state of X` / `commonwealth
  # of X` prefix-strip fallback. For the rows the old code matched
  # (50 states + DC + PR with exact title-case spelling), the
  # behaviour is identical — see test-identity-cache-hardening-semantic.R
  # for the contract pin. Rows that previously returned NA on a
  # mixed-case input ("california") now return "CA" and survive the
  # NA filter at line 192 below; drop_conus / drop_retired remain
  # correct because the new code matches NPI/state to NON_CONTIGUOUS_CODES
  # the same way the old code did.
  if (!exists("convert_state_name_to_abbrev", mode = "function")) {
    source(here::here("R", "utils", "state_normalization.R"))
  }
  abohns_matched <- abohns_matched %>%
    mutate(state_code = unname(convert_state_name_to_abbrev(state_full)))

  # Enforce CONUS on ENThealth (confirmed CONUS-only today, but this
  # makes the assumption explicit so the math can't silently break on
  # future input refreshes).
  ent_all <- ent_all %>% filter(!state %in% NON_CONTIGUOUS_CODES | is.na(state))

  # ── Chain ───────────────────────────────────────────────────────────────────
  n_raw    <- nrow(abohns_raw)
  n_unique <- length(unique(abohns_raw$abohns_id))

  n_conus_drop_by_state <- abohns_matched %>%
    filter(state_code %in% NON_CONTIGUOUS_CODES) %>%
    count(state_code, name = "n") %>%
    arrange(desc(n))
  drop_conus <- sum(n_conus_drop_by_state$n)
  n_conus    <- n_unique - drop_conus

  # BUG FIX (NA): !NA %in% NON_CONTIGUOUS_CODES = TRUE, so rows with missing state_code
  # would silently pass this filter and be counted as CONUS physicians.
  abohns_conus  <- abohns_matched %>% filter(!is.na(state_code) & !state_code %in% NON_CONTIGUOUS_CODES)
  drop_retired  <- sum(is.na(abohns_conus$practicing_status) | abohns_conus$practicing_status != "Practicing")
  n_active_rows <- n_conus - drop_retired

  abohns_active <- abohns_conus %>%
    filter(practicing_status == "Practicing", !is.na(npi))
  n_board       <- length(unique(as.character(abohns_active$npi)))
  drop_npi_dup  <- n_active_rows - n_board

  # Number of *distinct* NPIs that appear in 2+ ABOHNS rows
  npi_chr <- as.character(abohns_active$npi)
  n_npi_dups_distinct <- length(unique(npi_chr[duplicated(npi_chr)]))

  # ── Per-subspecialty discoverability ───────────────────────────────────────
  # Subspecialty classification comes from ABOto certifications ONLY.
  # The board offers four certs:
  #   - Otolaryngology - Head & Neck Surgery   (foundational general cert)
  #   - Complex Pediatric Otolaryngology       (subspecialty cert)
  #   - Neurotology                            (subspecialty cert)
  #   - Sleep Medicine                         (subspecialty cert)
  # NPPES taxonomy codes are NOT used here — they are self-reported and
  # do not map 1:1 to ABOto's subspecialty certifications.
  #
  # Each physician's subspecialty is the rarest cert they hold; priority:
  #   Pediatric > Neurotology > Sleep Medicine > General Otolaryngology.
  # Physicians with no subspecialty cert (only General) are classified as
  # "General Otolaryngology".
  #
  # IMPORTANT: 117 NPIs appear under multiple abohns_id values in the
  # source data, and 3 of those have conflicting subspecialty across
  # rows (e.g. one abohns_id has only General, another for the SAME NPI
  # has Pediatric). We therefore aggregate the boolean has_* flags AT
  # THE NPI LEVEL (OR across all rows for that NPI) before applying the
  # priority rule. This makes the subspecialty assignment deterministic
  # regardless of row ordering, and assigns each physician to the
  # subspecialty they hold under ANY of their abohns_ids.
  ent_npi_set <- unique(as.character(ent_all$npi[!is.na(ent_all$npi)]))

  abohns_subspec_per_id <- abohns_raw %>%
    group_by(abohns_id) %>%
    summarise(
      has_pediatric   = any(certification == "Complex Pediatric Otolaryngology"),
      has_neurotology = any(certification == "Neurotology"),
      has_sleep       = any(certification == "Sleep Medicine"),
      .groups         = "drop"
    )

  # Aggregate at NPI level: OR the flags across all abohns_ids sharing
  # the same NPI in the active cohort, then apply priority.
  abohns_unique <- abohns_active %>%
    mutate(npi_chr = as.character(npi)) %>%
    left_join(abohns_subspec_per_id, by = "abohns_id") %>%
    group_by(npi_chr) %>%
    summarise(
      has_pediatric   = any(has_pediatric,   na.rm = TRUE),
      has_neurotology = any(has_neurotology, na.rm = TRUE),
      has_sleep       = any(has_sleep,       na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(
      # Use ABOTO_SUBSPECIALTY_PRIORITY (top of file) to resolve the
      # priority. case_when evaluates clauses in order, so the first
      # TRUE wins — which by construction follows the priority vector.
      subspecialty = dplyr::case_when(
        has_pediatric   ~ ABOTO_SUBSPECIALTY_PRIORITY[1],
        has_neurotology ~ ABOTO_SUBSPECIALTY_PRIORITY[2],
        has_sleep       ~ ABOTO_SUBSPECIALTY_PRIORITY[3],
        TRUE            ~ ABOTO_SUBSPECIALTY_PRIORITY[4]
      ),
      subspecialty = factor(subspecialty, levels = ABOTO_SUBSPECIALTY_PRIORITY),
      in_enthealth = npi_chr %in% ent_npi_set
    )

  by_subspec <- abohns_unique %>%
    group_by(subspecialty) %>%
    summarise(
      n_total      = dplyr::n(),
      n_findable   = sum(in_enthealth),
      pct_findable = 100 * n_findable / max(n_total, 1L),
      .groups      = "drop"
    ) %>%
    arrange(desc(n_total))

  # ── Subspecialty triangulation (ABOto vs NPPES vs ENThealth) ────────────────
  # Map NPPES primary_taxonomy and ENThealth specialty_primary down to the
  # four ABOto categories so they can be compared on equal footing.  Any
  # source-specific value that doesn't have an ABOto equivalent maps to
  # "General Otolaryngology" (because ABOto's only foundational cert IS
  # general — Rhinology, Laryngology, Allergy, Facial Plastics, H&N Surgery
  # are all practice areas, not separate ABOto certs).
  nppes_to_aboto <- c(
    "207Y00000X" = "General Otolaryngology",
    "207YX0007X" = "General Otolaryngology",      # Head & Neck Surgery -> General
    "207YX0905X" = "Neurotology",
    "207YP0228X" = "Pediatric Otolaryngology",
    "207YX0901X" = "General Otolaryngology",      # Allergy -> General
    "207YX0602X" = "General Otolaryngology",      # Facial Plastic -> General
    "207YS0012X" = "Sleep Medicine",
    "207YS0123X" = "Sleep Medicine",
    "207YX0400X" = "General Otolaryngology"       # Rhinology -> General
  )
  enthealth_to_aboto <- c(
    "General Otolaryngology"                    = "General Otolaryngology",
    "Pediatric Otolaryngology"                  = "Pediatric Otolaryngology",
    "Neurotology"                               = "Neurotology",
    "Otology/Audiology"                         = "Neurotology",   # closest ABOto map
    "Sleep Medicine"                            = "Sleep Medicine",
    "Head and Neck Surgery"                     = "General Otolaryngology",
    "Facial Plastic and Reconstructive Surgery" = "General Otolaryngology",
    "Rhinology"                                 = "General Otolaryngology",
    "Laryngology"                               = "General Otolaryngology",
    "Allergy"                                   = "General Otolaryngology",
    "Endocrine Surgery"                         = "General Otolaryngology"
  )

  nppes_subspec_by_npi <- nppes_all %>%
    filter(!state %in% NON_CONTIGUOUS_CODES, !is.na(npi)) %>%
    distinct(npi, .keep_all = TRUE) %>%
    mutate(
      npi_chr = as.character(npi),
      nppes_subspec_raw   = primary_taxonomy,
      nppes_subspec_aboto = unname(nppes_to_aboto[primary_taxonomy])
    ) %>%
    select(npi_chr, nppes_subspec_raw, nppes_subspec_aboto)

  ent_subspec_by_npi <- ent_all %>%
    filter(!is.na(npi)) %>%
    distinct(npi, .keep_all = TRUE) %>%
    mutate(
      npi_chr = as.character(npi),
      ent_subspec_raw   = specialty_primary,
      ent_subspec_aboto = unname(enthealth_to_aboto[specialty_primary])
    ) %>%
    select(npi_chr, ent_subspec_raw, ent_subspec_aboto)

  subspec_triangulation <- abohns_unique %>%
    select(npi_chr, aboto_subspecialty = subspecialty) %>%
    left_join(nppes_subspec_by_npi, by = "npi_chr") %>%
    left_join(ent_subspec_by_npi,   by = "npi_chr") %>%
    mutate(
      aboto_subspecialty = as.character(aboto_subspecialty),
      # 4-way agreement flag based on the ABOto-mapped values.
      # Missing-on-source values are reported separately so we don't
      # conflate "actual disagreement" with "no signal from this source".
      subspecialty_agreement = dplyr::case_when(
        is.na(nppes_subspec_aboto) & is.na(ent_subspec_aboto)
          ~ "no_nppes_or_enthealth_signal",
        is.na(nppes_subspec_aboto)
          ~ ifelse(aboto_subspecialty == ent_subspec_aboto,
                   "aboto_enthealth_agree_nppes_missing",
                   "aboto_enthealth_disagree_nppes_missing"),
        is.na(ent_subspec_aboto)
          ~ ifelse(aboto_subspecialty == nppes_subspec_aboto,
                   "aboto_nppes_agree_enthealth_missing",
                   "aboto_nppes_disagree_enthealth_missing"),
        aboto_subspecialty == nppes_subspec_aboto &
        aboto_subspecialty == ent_subspec_aboto
          ~ "all_three_agree",
        aboto_subspecialty == nppes_subspec_aboto
          ~ "aboto_nppes_agree_enthealth_disagree",
        aboto_subspecialty == ent_subspec_aboto
          ~ "aboto_enthealth_agree_nppes_disagree",
        nppes_subspec_aboto == ent_subspec_aboto
          ~ "nppes_enthealth_agree_aboto_disagree",
        TRUE
          ~ "all_three_disagree"
      )
    )

  # ── Set totals (Euler legend) ───────────────────────────────────────────────
  nppes <- nppes_all %>% filter(!state %in% NON_CONTIGUOUS_CODES, !is.na(npi))
  ent   <- ent_all            # already CONUS-filtered at top

  # ── Entity-type check (NPPES individual vs organizational NPIs) ─────────────
  # NPPES Entity Code 1 = individual; Entity Code 2 = organizational. The
  # parquet doesn't expose the explicit code, so we derive it: an individual
  # NPI should have a first_name and no organization_name. Any record where
  # both are populated, or neither, is a data quality flag.
  nppes_entity <- nppes_all %>%
    mutate(
      npi_chr = as.character(npi),
      has_first = !is.na(first_name) & nchar(first_name) > 0,
      has_org   = !is.na(organization_name) & nchar(organization_name) > 0,
      nppes_entity_type = dplyr::case_when(
        has_first & !has_org ~ "individual",
        !has_first & has_org ~ "organization",
        has_first & has_org  ~ "both_populated_unclear",
        TRUE                 ~ "neither_populated_unclear"
      )
    ) %>%
    distinct(npi_chr, .keep_all = TRUE) %>%
    select(npi_chr, nppes_entity_type, nppes_state = state)

  n_nonindividual_in_nppes <- sum(nppes_entity$nppes_entity_type != "individual")
  if (n_nonindividual_in_nppes > 0L) {
    warning(sprintf(
      "[ent_registry_counts] %d NPPES records are not individuals (organization or unclear). Check that ent_from_nppes_flat_file.parquet was built with the right Entity Code filter.",
      n_nonindividual_in_nppes
    ), call. = FALSE)
  }

  # ── Cross-register state-mismatch flag (ABOto vs NPPES) ─────────────────────
  # A physician whose ABOto state differs from their NPPES practice state is
  # one of three real cases: (a) the physician moved between registries'
  # snapshot dates, (b) they have multiple practice locations and the two
  # registries chose different ones, or (c) a data quality error. We surface
  # the flag and let downstream callers decide how to weight it.
  state_mismatch <- abohns_active %>%
    mutate(npi_chr = as.character(npi)) %>%
    distinct(npi_chr, .keep_all = TRUE) %>%
    select(npi_chr, aboto_state = state_code) %>%
    left_join(nppes_entity, by = "npi_chr") %>%
    mutate(state_mismatch_flag = !is.na(aboto_state) & !is.na(nppes_state) &
                                  aboto_state != nppes_state)

  n_state_mismatch_in_board <- sum(state_mismatch$state_mismatch_flag, na.rm = TRUE)

  npi_nppes <- unique(as.character(nppes$npi))
  npi_board <- unique(as.character(abohns_active$npi))
  npi_ent   <- unique(as.character(ent$npi[!is.na(ent$npi)]))
  n_ent_orphan <- sum(is.na(ent$npi))

  n_npi_univ  <- length(npi_nppes)
  n_enthealth <- length(npi_ent) + n_ent_orphan

  # ── Zone counts (NPI joins) ─────────────────────────────────────────────────
  all_npis <- unique(c(npi_nppes, npi_board, npi_ent))
  in_n <- all_npis %in% npi_nppes
  in_b <- all_npis %in% npi_board
  in_e <- all_npis %in% npi_ent

  z_triple        <- sum( in_n &  in_b &  in_e)
  z_nppes_board   <- sum( in_n &  in_b & !in_e)
  z_nppes_ent     <- sum( in_n & !in_b &  in_e)
  z_board_ent     <- sum(!in_n &  in_b &  in_e)
  z_nppes_only    <- sum( in_n & !in_b & !in_e)
  z_board_only    <- sum(!in_n &  in_b & !in_e)
  z_ent_only_npi  <- sum(!in_n & !in_b &  in_e)
  z_ent_only      <- z_ent_only_npi + n_ent_orphan

  drop_cross   <- n_board - z_triple

  # ── Decompose the 712 zone (NPPES ∩ ENThealth, not in our cohort) ───────────
  # Each registry has its OWN state field, and a physician with different
  # state designations across registries (e.g., ABOto says "Hawaii" but
  # NPPES has a CONUS practice address) will appear in CONUS NPPES but be
  # excluded from CONUS Board. Empirically (2026-05-15): of the 712,
  # 1 falls into this state-mismatch bucket; 711 have no ABOto record at all
  # (likely AOA-certified DOs, foreign-trained, or never board-certified).
  abohns_matched_all_npis <- unique(as.character(abohns_matched$npi[!is.na(abohns_matched$npi)]))
  npis_in_712 <- all_npis[in_n & !in_b & in_e]
  z_712_state_mismatch  <- sum(npis_in_712 %in% abohns_matched_all_npis)
  z_712_absent_from_abohns  <- sum(!(npis_in_712 %in% abohns_matched_all_npis))

  stopifnot(
    "712 zone decomposition does not close" =
      z_712_state_mismatch + z_712_absent_from_abohns == z_nppes_ent
  )

  # ── Anomaly rosters (for supplementary tables) ──────────────────────────────
  # 117 distinct NPIs appearing in 2+ ABOto rows
  dup_npi_chars <- unique(npi_chr[duplicated(npi_chr)])
  anomaly_abohns_dup_npis <- abohns_active %>%
    mutate(npi_chr = as.character(npi)) %>%
    filter(npi_chr %in% dup_npi_chars) %>%
    arrange(npi_chr, abohns_id) %>%
    select(npi = npi_chr, abohns_id, first_name, last_name,
           state_full, certification, practicing_status)

  # 12 board-cert physicians with NPIs not in NPPES (z_board_only)
  board_only_npis <- all_npis[!in_n & in_b & !in_e]
  anomaly_board_no_nppes <- abohns_active %>%
    mutate(npi_chr = as.character(npi)) %>%
    filter(npi_chr %in% board_only_npis) %>%
    distinct(npi_chr, .keep_all = TRUE) %>%
    select(npi = npi_chr, abohns_id, first_name, last_name,
           state_full, certification, practicing_status)

  # 247 ENThealth rows with NA NPI
  anomaly_ent_no_npi <- ent_all %>%
    filter(is.na(npi)) %>%
    select(any_of(c("first_name", "last_name", "credentials",
                    "specialty_primary", "state", "city", "phone_norm")))

  # 1 (or N) ENThealth NPI not in NPPES
  ent_npi_not_in_nppes <- npi_ent[!npi_ent %in% npi_nppes]
  anomaly_ent_npi_no_nppes <- ent_all %>%
    mutate(npi_chr = as.character(npi)) %>%
    filter(npi_chr %in% ent_npi_not_in_nppes) %>%
    select(any_of(c("npi" = "npi_chr", "first_name", "last_name",
                    "specialty_primary", "state", "city")))

  # 712 zone — physicians in NPPES + ENThealth but not in our active cohort
  anomaly_712 <- tibble::tibble(npi = npis_in_712) %>%
    mutate(in_abohns_file = npi %in% abohns_matched_all_npis,
           category = ifelse(in_abohns_file,
                             "ABOto-listed with state non-CONUS (state-mismatch vs NPPES)",
                             "Absent from ABOto file (likely never ABOto-certified)"))

  # ── Closure assertions ──────────────────────────────────────────────────────
  stopifnot(
    "NPPES set total does not close on its zones" =
      z_nppes_only + z_nppes_board + z_nppes_ent + z_triple == n_npi_univ,
    "Board set total does not close on its zones" =
      z_board_only + z_nppes_board + z_board_ent + z_triple == n_board,
    "ENThealth set total does not close on its zones" =
      z_ent_only + z_nppes_ent + z_board_ent + z_triple == n_enthealth,
    "Chain dedupe does not balance" =
      n_raw - (n_raw - n_unique) == n_unique,
    "Chain CONUS does not balance" =
      n_unique - drop_conus == n_conus,
    "Chain retirement does not balance" =
      n_conus - drop_retired == n_active_rows,
    "Chain NPI dedup does not balance" =
      n_active_rows - drop_npi_dup == n_board,
    "Cross-registry attrition does not balance" =
      n_board - drop_cross == z_triple
  )

  # ── Cohort lock-file verification (if data/ent/cohort_lock_*.json exists) ──
  # The lock file pins the exact NPI set + canonical counts at manuscript
  # lock time.  If any input file drifts and the cohort no longer matches,
  # we want a loud failure here — NOT a silent slide of figure numbers.
  # Re-run scripts/write_ent_cohort_lock.R to intentionally update the lock.
  lock_path <- if (isTRUE(physician_only)) {
    here::here("data", "ent", "cohort_lock_triple_match_v2_physicians.json")
  } else {
    here::here("data", "ent", "cohort_lock_triple_match_v1.json")
  }
  # Skip lock verification when running against non-default (test) inputs.
  # When physician_only=TRUE we check the v2 lock (run
  # scripts/write_ent_cohort_lock.R to regenerate); when FALSE we check v1.
  if (file.exists(lock_path) && using_default_paths) {
    lock <- jsonlite::read_json(lock_path, simplifyVector = TRUE)
    # Recompute Triple Match NPIs for the assertion (must match lock)
    .triple_npis_now <- sort(intersect(intersect(
      unique(as.character(abohns_active$npi)),
      unique(as.character(nppes$npi))),
      unique(as.character(ent$npi[!is.na(ent$npi)]))
    ))
    .locked_npis <- sort(as.character(lock$triple_match_npis))
    stopifnot(
      "Cohort lock: n_triple_match differs from lock file" =
        lock$expected_counts$n_triple_match == z_triple,
      "Cohort lock: n_board_unique_npi differs from lock file" =
        lock$expected_counts$n_board_unique_npi == n_board,
      "Cohort lock: n_nppes_universe_conus differs from lock file" =
        lock$expected_counts$n_nppes_universe_conus == n_npi_univ,
      "Cohort lock: NPI set differs from lock file (set diff)" =
        identical(.triple_npis_now, .locked_npis)
    )
  }

  list(
    # ── Chain (STROBE + CONSORT) ──────────────────────────────────────────────
    chain = list(
      n_raw         = n_raw,
      n_unique      = n_unique,
      n_conus       = n_conus,
      n_active_rows = n_active_rows,
      n_board       = n_board,
      drop_dedupe   = n_raw - n_unique,
      drop_conus    = drop_conus,
      drop_retired  = drop_retired,
      drop_npi_dup  = drop_npi_dup,
      n_npi_dups_distinct = n_npi_dups_distinct,
      drop_cross    = drop_cross,
      n_conus_drop_by_state = n_conus_drop_by_state
    ),

    # ── Set totals (Euler legend, CONSORT parallel registries) ────────────────
    sets = list(
      n_npi_univ  = n_npi_univ,
      n_board     = n_board,
      n_enthealth = n_enthealth,
      n_triple    = z_triple
    ),

    # ── Zone counts (Euler zones) ─────────────────────────────────────────────
    zones = list(
      nppes_only  = z_nppes_only,
      board_only  = z_board_only,
      ent_only    = z_ent_only,
      ent_only_npi = z_ent_only_npi,
      ent_orphan  = n_ent_orphan,
      nppes_board = z_nppes_board,
      nppes_ent   = z_nppes_ent,
      board_ent   = z_board_ent,
      triple      = z_triple
    ),

    # ── Per-subspecialty discoverability ──────────────────────────────────────
    by_subspecialty = by_subspec,

    # ── 712 zone decomposition (NPPES ∩ ENThealth, not in our cohort) ─────────
    zone_712 = list(
      total                  = z_nppes_ent,
      in_abohns_state_mismatch = z_712_state_mismatch,
      absent_from_abohns     = z_712_absent_from_abohns
    ),

    # ── Anomaly rosters (for supplementary CSVs) ──────────────────────────────
    anomalies = list(
      abohns_dup_npis        = anomaly_abohns_dup_npis,
      board_no_nppes         = anomaly_board_no_nppes,
      ent_no_npi             = anomaly_ent_no_npi,
      ent_npi_not_in_nppes   = anomaly_ent_npi_no_nppes,
      zone_712               = anomaly_712
    ),

    # ── Data-quality flags (state-mismatch, entity-type, triangulation) ──────
    qa = list(
      state_mismatch                = state_mismatch,
      n_state_mismatch_in_board     = n_state_mismatch_in_board,
      nppes_entity                  = nppes_entity,
      n_nonindividual_in_nppes      = n_nonindividual_in_nppes,
      subspecialty_triangulation    = subspec_triangulation
    ),

    # ── Input file paths (for receipt SHA256) ─────────────────────────────────
    input_paths = paths,

    # ── Top-level convenience scalars ─────────────────────────────────────────
    n_raw                 = n_raw,
    n_unique              = n_unique,
    n_conus               = n_conus,
    n_active_rows         = n_active_rows,
    n_board               = n_board,
    n_npi_univ            = n_npi_univ,
    n_enthealth           = n_enthealth,
    n_triple              = z_triple,
    n_ent_orphan          = n_ent_orphan,
    n_conus_drop_by_state = n_conus_drop_by_state
  )
}

#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a
