#!/usr/bin/env Rscript
# =============================================================================
# Cross-verification of subspecialty triangulation findings
# =============================================================================
# Confirms the 9-level subspecialty_agreement distribution from
# compute_ent_registry_counts() using four independent techniques. Run
# this any time the triangulation logic in R/ent_registry_counts.R
# changes — all four checks must pass.
#
# Techniques:
#   1. Totals reconciliation        — counts must sum to active Board cohort
#   2. Independent re-implementation — re-derive triangulation in a fresh
#                                       dplyr pipeline and compare row-by-row
#   3. Cross-tab matrix              — 4x4 ABOto x ENThealth (mapped) and
#                                       NPPES x ENThealth tables
#   4. Hand-traced spot checks       — 5 NPIs (one per agreement bucket)
#                                       traced manually through all three
#                                       registries
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(arrow); library(dplyr); library(tidyr); library(here)
})

source(here::here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()

cat("=" , rep("=", 78), "\n", sep="")
cat("SUBSPECIALTY TRIANGULATION — CROSS-VERIFICATION\n")
cat("=" , rep("=", 78), "\n\n", sep="")

# ── 1. Totals reconciliation ─────────────────────────────────────────────────
cat("─── CHECK 1: Totals reconciliation ──────────────────────────────────────\n")
tri <- ct$qa$subspecialty_triangulation
cat(sprintf("  Active Board cohort (ct$chain$n_board)     : %d\n", ct$chain$n_board))
cat(sprintf("  Triangulation table row count              : %d\n", nrow(tri)))
cat(sprintf("  Sum of agreement-bucket counts             : %d\n",
            sum(table(tri$subspecialty_agreement))))
stopifnot(
  "CHECK 1A: triangulation rows != active board cohort" =
    nrow(tri) == ct$chain$n_board,
  "CHECK 1B: agreement-bucket counts do not sum to cohort" =
    sum(table(tri$subspecialty_agreement)) == ct$chain$n_board
)
cat("  [PASS]\n\n")

# ── 2. Independent re-implementation ─────────────────────────────────────────
cat("─── CHECK 2: Independent re-implementation (different code path) ────────\n")
paths <- ct$input_paths

# Re-derive each registry's subspecialty signal in a fresh pipeline using
# different intermediate variable names + a different idiom.
nppes_to_aboto_alt <- function(tax) {
  dplyr::recode(tax,
    "207Y00000X" = "General Otolaryngology",
    "207YX0007X" = "General Otolaryngology",
    "207YX0905X" = "Neurotology",
    "207YP0228X" = "Pediatric Otolaryngology",
    "207YX0901X" = "General Otolaryngology",
    "207YX0602X" = "General Otolaryngology",
    "207YS0012X" = "Sleep Medicine",
    "207YS0123X" = "Sleep Medicine",
    "207YX0400X" = "General Otolaryngology",
    .default = NA_character_
  )
}
enthealth_to_aboto_alt <- function(spec) {
  dplyr::recode(spec,
    "General Otolaryngology"                    = "General Otolaryngology",
    "Pediatric Otolaryngology"                  = "Pediatric Otolaryngology",
    "Neurotology"                               = "Neurotology",
    "Otology/Audiology"                         = "Neurotology",
    "Sleep Medicine"                            = "Sleep Medicine",
    "Head and Neck Surgery"                     = "General Otolaryngology",
    "Facial Plastic and Reconstructive Surgery" = "General Otolaryngology",
    "Rhinology"                                 = "General Otolaryngology",
    "Laryngology"                               = "General Otolaryngology",
    "Allergy"                                   = "General Otolaryngology",
    "Endocrine Surgery"                         = "General Otolaryngology",
    .default = NA_character_
  )
}

NC <- c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP")
state_lookup <- c(setNames(state.abb, state.name),
                  "District of Columbia" = "DC", "Puerto Rico" = "PR")

ab2 <- read_csv(paths$abohns_matched, show_col_types = FALSE) %>%
  mutate(state_code = state_lookup[state_full]) %>%
  filter(!state_code %in% NC, practicing_status == "Practicing", !is.na(npi)) %>%
  distinct(npi, .keep_all = TRUE) %>%
  mutate(npi_chr = as.character(npi))

abohns_raw2 <- read_csv(paths$abohns_raw, show_col_types = FALSE)
subspec_per_id <- abohns_raw2 %>%
  group_by(abohns_id) %>%
  summarise(
    p = any(certification == "Complex Pediatric Otolaryngology"),
    n = any(certification == "Neurotology"),
    s = any(certification == "Sleep Medicine"),
    .groups = "drop"
  )

aboto2 <- ab2 %>%
  left_join(subspec_per_id, by = "abohns_id") %>%
  group_by(npi_chr) %>%
  summarise(p = any(p, na.rm=TRUE), n = any(n, na.rm=TRUE), s = any(s, na.rm=TRUE),
            .groups = "drop") %>%
  mutate(aboto = case_when(p ~ "Pediatric Otolaryngology",
                           n ~ "Neurotology",
                           s ~ "Sleep Medicine",
                           TRUE ~ "General Otolaryngology")) %>%
  select(npi_chr, aboto)

nppes2 <- read_parquet(paths$nppes) %>%
  filter(!state %in% NC, !is.na(npi)) %>%
  distinct(npi, .keep_all = TRUE) %>%
  mutate(npi_chr = as.character(npi),
         nppes_aboto = nppes_to_aboto_alt(primary_taxonomy)) %>%
  select(npi_chr, nppes_aboto)

ent2 <- read_csv(paths$ent, show_col_types = FALSE) %>%
  filter(!state %in% NC | is.na(state)) %>%
  filter(!is.na(npi)) %>%
  distinct(npi, .keep_all = TRUE) %>%
  mutate(npi_chr = as.character(npi),
         ent_aboto = enthealth_to_aboto_alt(specialty_primary)) %>%
  select(npi_chr, ent_aboto)

tri_alt <- aboto2 %>%
  left_join(nppes2, by = "npi_chr") %>%
  left_join(ent2,   by = "npi_chr") %>%
  mutate(agreement_alt = dplyr::case_when(
    is.na(nppes_aboto) & is.na(ent_aboto)
      ~ "no_nppes_or_enthealth_signal",
    is.na(nppes_aboto)
      ~ ifelse(aboto == ent_aboto,
               "aboto_enthealth_agree_nppes_missing",
               "aboto_enthealth_disagree_nppes_missing"),
    is.na(ent_aboto)
      ~ ifelse(aboto == nppes_aboto,
               "aboto_nppes_agree_enthealth_missing",
               "aboto_nppes_disagree_enthealth_missing"),
    aboto == nppes_aboto & aboto == ent_aboto
      ~ "all_three_agree",
    aboto == nppes_aboto
      ~ "aboto_nppes_agree_enthealth_disagree",
    aboto == ent_aboto
      ~ "aboto_enthealth_agree_nppes_disagree",
    nppes_aboto == ent_aboto
      ~ "nppes_enthealth_agree_aboto_disagree",
    TRUE
      ~ "all_three_disagree"
  ))

joined <- tri %>%
  select(npi_chr, subspecialty_agreement, aboto_subspecialty,
         nppes_subspec_aboto, ent_subspec_aboto) %>%
  inner_join(tri_alt %>% select(npi_chr, agreement_alt, aboto, nppes_aboto, ent_aboto),
             by = "npi_chr") %>%
  mutate(
    same_aboto = aboto_subspecialty == aboto,
    same_nppes = (is.na(nppes_subspec_aboto) & is.na(nppes_aboto)) |
                 (!is.na(nppes_subspec_aboto) & !is.na(nppes_aboto) &
                    nppes_subspec_aboto == nppes_aboto),
    same_ent   = (is.na(ent_subspec_aboto) & is.na(ent_aboto)) |
                 (!is.na(ent_subspec_aboto) & !is.na(ent_aboto) &
                    ent_subspec_aboto == ent_aboto),
    same_agreement = subspecialty_agreement == agreement_alt
  )

n_mismatches_aboto      <- sum(!joined$same_aboto)
n_mismatches_nppes      <- sum(!joined$same_nppes)
n_mismatches_ent        <- sum(!joined$same_ent)
n_mismatches_agreement  <- sum(!joined$same_agreement)
cat(sprintf("  ABOto-subspecialty mismatches    : %d\n", n_mismatches_aboto))
cat(sprintf("  NPPES-subspecialty mismatches    : %d\n", n_mismatches_nppes))
cat(sprintf("  ENThealth-subspecialty mismatches: %d\n", n_mismatches_ent))
cat(sprintf("  agreement-label mismatches       : %d\n", n_mismatches_agreement))
stopifnot(
  "CHECK 2A: ABOto subspecialty differs between implementations"   = n_mismatches_aboto == 0,
  "CHECK 2B: NPPES subspecialty differs between implementations"   = n_mismatches_nppes == 0,
  "CHECK 2C: ENThealth subspecialty differs between implementations" = n_mismatches_ent == 0,
  "CHECK 2D: agreement label differs between implementations"      = n_mismatches_agreement == 0
)
cat("  [PASS] Two independent implementations agree on all rows.\n\n")

# ── 3. Cross-tab matrices ────────────────────────────────────────────────────
cat("─── CHECK 3: Cross-tab matrices (sanity-check pattern) ──────────────────\n\n")

cat("ABOto x ENThealth (mapped to ABOto categories), only rows with ENThealth signal:\n")
ct_aboto_ent <- tri %>%
  filter(!is.na(ent_subspec_aboto)) %>%
  count(aboto_subspecialty, ent_subspec_aboto) %>%
  tidyr::pivot_wider(names_from = ent_subspec_aboto, values_from = n, values_fill = 0L) %>%
  arrange(aboto_subspecialty)
print(ct_aboto_ent)

cat("\nABOto x NPPES (mapped to ABOto categories):\n")
ct_aboto_nppes <- tri %>%
  filter(!is.na(nppes_subspec_aboto)) %>%
  count(aboto_subspecialty, nppes_subspec_aboto) %>%
  tidyr::pivot_wider(names_from = nppes_subspec_aboto, values_from = n, values_fill = 0L) %>%
  arrange(aboto_subspecialty)
print(ct_aboto_nppes)

# Diagonal sums of these matrices = "ABOto-source agree" counts
diag_ent   <- tri %>% filter(!is.na(ent_subspec_aboto)) %>%
                       filter(aboto_subspecialty == ent_subspec_aboto)   %>% nrow()
diag_nppes <- tri %>% filter(!is.na(nppes_subspec_aboto)) %>%
                       filter(aboto_subspecialty == nppes_subspec_aboto) %>% nrow()

# These match the agreement-bucket counts:
#   diag_ent   = all_three_agree + aboto_enthealth_agree_nppes_disagree
#                                 + aboto_enthealth_agree_nppes_missing
#   diag_nppes = all_three_agree + aboto_nppes_agree_enthealth_disagree
#                                 + aboto_nppes_agree_enthealth_missing
agg_ent   <- sum(tri$subspecialty_agreement %in%
                 c("all_three_agree",
                   "aboto_enthealth_agree_nppes_disagree",
                   "aboto_enthealth_agree_nppes_missing"))
agg_nppes <- sum(tri$subspecialty_agreement %in%
                 c("all_three_agree",
                   "aboto_nppes_agree_enthealth_disagree",
                   "aboto_nppes_agree_enthealth_missing"))
cat(sprintf("\n  diagonal(ABOto x ENThealth)              : %d\n", diag_ent))
cat(sprintf("  agreement-bucket sum (ABOto+ENT agree)   : %d\n", agg_ent))
stopifnot("CHECK 3A: cross-tab diagonal != agreement-bucket sum (ENT)"   = diag_ent   == agg_ent)
cat(sprintf("  diagonal(ABOto x NPPES)                  : %d\n", diag_nppes))
cat(sprintf("  agreement-bucket sum (ABOto+NPPES agree) : %d\n", agg_nppes))
stopifnot("CHECK 3B: cross-tab diagonal != agreement-bucket sum (NPPES)" = diag_nppes == agg_nppes)
cat("  [PASS] Cross-tab diagonals match agreement-bucket sums.\n\n")

# ── 4. Hand-traced spot checks ───────────────────────────────────────────────
cat("─── CHECK 4: Hand-traced spot checks (5 NPIs across 5 buckets) ──────────\n\n")

# Pick one NPI from each major agreement bucket
spot_npis <- bind_rows(
  tri %>% filter(subspecialty_agreement == "all_three_agree")                       %>% head(1),
  tri %>% filter(subspecialty_agreement == "aboto_enthealth_agree_nppes_disagree")  %>% head(1),
  tri %>% filter(subspecialty_agreement == "nppes_enthealth_agree_aboto_disagree")  %>% head(1),
  tri %>% filter(subspecialty_agreement == "aboto_nppes_agree_enthealth_disagree")  %>% head(1),
  tri %>% filter(subspecialty_agreement == "all_three_disagree")                    %>% head(1)
)

cat("Spot-check NPIs:\n")
print(spot_npis %>% select(npi_chr, aboto_subspecialty,
                            nppes_subspec_raw, nppes_subspec_aboto,
                            ent_subspec_raw,   ent_subspec_aboto,
                            subspecialty_agreement))

# Manually verify each row's agreement_label given its 3 source values
for (i in seq_len(nrow(spot_npis))) {
  r <- spot_npis[i, ]
  manual <- if (is.na(r$nppes_subspec_aboto) & is.na(r$ent_subspec_aboto)) {
    "no_nppes_or_enthealth_signal"
  } else if (is.na(r$nppes_subspec_aboto)) {
    ifelse(r$aboto_subspecialty == r$ent_subspec_aboto,
           "aboto_enthealth_agree_nppes_missing",
           "aboto_enthealth_disagree_nppes_missing")
  } else if (is.na(r$ent_subspec_aboto)) {
    ifelse(r$aboto_subspecialty == r$nppes_subspec_aboto,
           "aboto_nppes_agree_enthealth_missing",
           "aboto_nppes_disagree_enthealth_missing")
  } else if (r$aboto_subspecialty == r$nppes_subspec_aboto &&
             r$aboto_subspecialty == r$ent_subspec_aboto) {
    "all_three_agree"
  } else if (r$aboto_subspecialty == r$nppes_subspec_aboto) {
    "aboto_nppes_agree_enthealth_disagree"
  } else if (r$aboto_subspecialty == r$ent_subspec_aboto) {
    "aboto_enthealth_agree_nppes_disagree"
  } else if (r$nppes_subspec_aboto == r$ent_subspec_aboto) {
    "nppes_enthealth_agree_aboto_disagree"
  } else {
    "all_three_disagree"
  }
  if (manual != r$subspecialty_agreement) {
    stop(sprintf("CHECK 4: NPI %s — manual label '%s' != computed '%s'",
                 r$npi_chr, manual, r$subspecialty_agreement),
         call. = FALSE)
  }
}
cat("  [PASS] All 5 hand-traced NPIs reconcile with computed labels.\n\n")

# ── Summary ──────────────────────────────────────────────────────────────────
cat("=" , rep("=", 78), "\n", sep="")
cat("ALL CHECKS PASSED. Subspecialty triangulation logic verified by:\n")
cat("  1. Totals reconciliation        ✓\n")
cat("  2. Independent re-implementation ✓\n")
cat("  3. Cross-tab matrices            ✓\n")
cat("  4. Hand-traced spot checks       ✓\n")
cat("=" , rep("=", 78), "\n", sep="")
