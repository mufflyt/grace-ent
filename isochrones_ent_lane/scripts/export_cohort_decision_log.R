#!/usr/bin/env Rscript
# =============================================================================
# Per-physician cohort-decision audit log
# =============================================================================
# Inverse of the STROBE aggregate counts: one row per ABOto physician
# (15,171 abohns_ids) showing exactly which exclusion rule (if any)
# removed them from the Triple Match cohort.  Lets a reviewer spot-check
# individual exclusion decisions and lets us answer questions like
# "show me the 175 non-CONUS physicians by state" without re-running
# the pipeline.
#
# Decision steps (in order applied):
#   step 1  subspecialty_cert_collapse  16,228 raw certs -> 15,171 phys
#                                       (not an exclusion: certs merged)
#   step 2  CONUS_filter                15,171 -> 14,996 (-175 non-CONUS)
#   step 3  practicing_filter           14,996 -> 11,477 (-3,519 retired
#                                                          or no NPI)
#   step 4  NPI_dedup                   11,477 -> 11,350 (-127 dup NPIs
#                                                          collapsed)
#   step 5  cross_registry              11,350 -> 4,578 (-6,772 not in
#                                                         all three sets)
#
# Output: data/ent/cohort_decision_log.csv
#   one row per abohns_id, columns:
#     abohns_id, npi, first_name, last_name, state_full, state_code,
#     practicing_status, n_certs, certs_held,
#     final_decision, dropped_at_step, drop_reason
# =============================================================================

suppressPackageStartupMessages({
  library(readr); library(arrow); library(dplyr); library(here)
})

source(here::here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()
paths <- ct$input_paths

NC <- c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP")
state_lookup <- c(setNames(state.abb, state.name),
                  "District of Columbia" = "DC", "Puerto Rico" = "PR")

# ── Load inputs ──────────────────────────────────────────────────────────────
ab_raw     <- read_csv(paths$abohns_raw, show_col_types = FALSE)
ab_matched <- read_csv(paths$abohns_matched, show_col_types = FALSE) %>%
  mutate(state_code = state_lookup[state_full],
         npi_chr = as.character(npi))
nppes      <- read_parquet(paths$nppes) %>%
  filter(!state %in% NC, !is.na(npi))
ent        <- read_csv(paths$ent, show_col_types = FALSE) %>%
  filter(!state %in% NC | is.na(state))

npi_nppes_set <- unique(as.character(nppes$npi))
npi_ent_set   <- unique(as.character(ent$npi[!is.na(ent$npi)]))

# ── Step 1: collapse raw certs by abohns_id ──────────────────────────────────
certs_per_id <- ab_raw %>%
  group_by(abohns_id) %>%
  summarise(
    n_certs    = dplyr::n(),
    certs_held = paste(sort(unique(certification)), collapse = " + "),
    .groups    = "drop"
  )

# ── Build per-physician decision log ─────────────────────────────────────────
# Start from the matched file (15,171 unique abohns_ids) — same denominator
# as the STROBE attrition.  Each row is one physician.
log <- ab_matched %>%
  left_join(certs_per_id, by = "abohns_id") %>%
  mutate(
    # Step 2: CONUS filter
    non_conus  = state_code %in% NC,
    # Step 3: practicing filter
    not_practicing = !non_conus & (is.na(practicing_status) | practicing_status != "Practicing" | is.na(npi))
  )

# Step 4: NPI dedup — for each duplicated NPI, the FIRST abohns_id
# (lowest numeric) is kept; subsequent ones are flagged.  Same convention
# as compute_ent_registry_counts() uses (distinct + arrange by abohns_id).
log <- log %>%
  arrange(abohns_id) %>%
  group_by(npi_chr) %>%
  mutate(
    npi_dup_rank = ifelse(is.na(npi_chr) | non_conus | not_practicing,
                          NA_integer_,
                          dplyr::row_number())
  ) %>%
  ungroup() %>%
  mutate(
    is_npi_duplicate = !is.na(npi_dup_rank) & npi_dup_rank > 1L,
    kept_npi         = !non_conus & !not_practicing & !is_npi_duplicate
  )

# Step 5: cross-registry
log <- log %>%
  mutate(
    in_nppes_set = !is.na(npi_chr) & npi_chr %in% npi_nppes_set,
    in_ent_set   = !is.na(npi_chr) & npi_chr %in% npi_ent_set,
    in_triple    = kept_npi & in_nppes_set & in_ent_set
  )

# Assign each physician to a final decision bucket
log <- log %>%
  mutate(
    final_decision = dplyr::case_when(
      in_triple        ~ "kept_in_triple_match",
      kept_npi & !(in_nppes_set & in_ent_set)
                       ~ "dropped_step5_cross_registry",
      is_npi_duplicate ~ "dropped_step4_npi_duplicate",
      not_practicing   ~ "dropped_step3_practicing",
      non_conus        ~ "dropped_step2_non_conus",
      TRUE             ~ "unknown"
    ),
    dropped_at_step = dplyr::case_when(
      in_triple        ~ NA_character_,
      kept_npi & !(in_nppes_set & in_ent_set) ~ "5_cross_registry",
      is_npi_duplicate ~ "4_npi_dedup",
      not_practicing   ~ "3_practicing_filter",
      non_conus        ~ "2_CONUS_filter",
      TRUE             ~ "unknown"
    ),
    drop_reason = dplyr::case_when(
      in_triple        ~ NA_character_,
      kept_npi & in_nppes_set & !in_ent_set ~ "not_in_ENThealth",
      kept_npi & !in_nppes_set & in_ent_set ~ "not_in_NPPES",
      kept_npi & !in_nppes_set & !in_ent_set ~ "not_in_NPPES_or_ENThealth",
      is_npi_duplicate ~ sprintf("npi_shared_with_abohns_id_other_than_self"),
      not_practicing   ~ sprintf("practicing_status=%s%s",
                                  practicing_status,
                                  ifelse(is.na(npi), " + no_NPI_match", "")),
      non_conus        ~ sprintf("state_non_CONUS (%s)", state_code),
      TRUE             ~ "unknown"
    )
  ) %>%
  select(abohns_id, npi = npi_chr, first_name, last_name,
         state_full, state_code, practicing_status,
         n_certs, certs_held,
         final_decision, dropped_at_step, drop_reason)

# ── Closure check: the decision-log totals must match the STROBE chain ──────
n_total       <- nrow(log)
n_in_triple   <- sum(log$final_decision == "kept_in_triple_match")
n_step2       <- sum(log$dropped_at_step == "2_CONUS_filter", na.rm = TRUE)
n_step3       <- sum(log$dropped_at_step == "3_practicing_filter", na.rm = TRUE)
n_step4       <- sum(log$dropped_at_step == "4_npi_dedup", na.rm = TRUE)
n_step5       <- sum(log$dropped_at_step == "5_cross_registry", na.rm = TRUE)

stopifnot(
  "Decision log: total must equal n_unique (15,171)" = n_total == ct$chain$n_unique,
  "Decision log: kept must equal n_triple (4,578)"    = n_in_triple == ct$sets$n_triple,
  "Decision log: step 2 must equal drop_conus (175)"  = n_step2 == ct$chain$drop_conus,
  "Decision log: step 4 must equal drop_npi_dup (127)"= n_step4 == ct$chain$drop_npi_dup
)

# ── Write CSV ─────────────────────────────────────────────────────────────────
out_path <- here::here("data", "ent", "cohort_decision_log.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
tmp_out_path <- paste0(out_path, ".tmp")
write_csv(log, tmp_out_path)
file.rename(tmp_out_path, out_path)
cat(sprintf("Wrote: %s\n  (%d rows, %d columns)\n\n",
            out_path, nrow(log), ncol(log)))

cat("=== Final-decision distribution ===\n")
print(log %>% count(final_decision, sort = TRUE) %>%
        mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))))

cat("\n=== State-level breakdown of step-2 (CONUS filter) exclusions ===\n")
print(log %>% filter(dropped_at_step == "2_CONUS_filter") %>%
        count(state_code, sort = TRUE))

cat("\n=== Step-5 cross-registry exclusion reasons ===\n")
print(log %>% filter(dropped_at_step == "5_cross_registry") %>%
        count(drop_reason, sort = TRUE))
