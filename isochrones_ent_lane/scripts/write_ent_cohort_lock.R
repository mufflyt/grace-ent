#!/usr/bin/env Rscript
# =============================================================================
# Cohort lock file generator (one-time publication snapshot)
# =============================================================================
# Pins the exact Triple Match cohort + the surrounding canonical counts to a
# single JSON file at the moment the manuscript is locked. After this script
# runs, every subsequent call to compute_ent_registry_counts() will verify
# that the live-computed cohort still matches the lock — if input files
# drift, the next render will fail loudly instead of silently producing
# different numbers.
#
# Two lock versions are maintained:
#   v1: unfiltered cohort           -> cohort_lock_triple_match_v1.json
#   v2: physicians-only cohort      -> cohort_lock_triple_match_v2_physicians.json
#       (NPPES + ENThealth filtered to exclude PA-C, NP, APRN, AUD, audiology,
#        DDS, PhD/MS-only, etc.; matches the published Euler diagram & Table 1)
#
# Default: writes v2 (physician-only). Set ENT_LOCK_PHYSICIAN_ONLY=0 to
# regenerate v1 instead.
#
# Re-running this script REPLACES the targeted lock. Only do so when the
# manuscript definition of the cohort intentionally changes.
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(jsonlite)
  library(digest)
  library(dplyr)
})

source(here::here("R", "ent_registry_counts.R"))

# Mode selection
physician_only <- !identical(trimws(Sys.getenv("ENT_LOCK_PHYSICIAN_ONLY", "1")), "0")
cat(sprintf("Writing cohort lock: physician_only = %s\n", physician_only))

ct <- compute_ent_registry_counts(physician_only = physician_only)
paths <- ct$input_paths

# Recompute the triple-match NPIs deterministically using the SAME filter
# logic compute_ent_registry_counts() applies internally, so the lock's NPI
# set matches the function's set exactly.
suppressPackageStartupMessages({library(readr); library(arrow)})
NC <- c("HI", "AK", "PR", "GU", "VI", "AS", "MP", "AA", "AE", "AP")
state_lookup <- c(setNames(state.abb, state.name),
                  "District of Columbia" = "DC", "Puerto Rico" = "PR")

ab <- read_csv(paths$abohns_matched, show_col_types = FALSE) %>%
  mutate(state_code = state_lookup[state_full]) %>%
  filter(!state_code %in% NC, practicing_status == "Practicing", !is.na(npi)) %>%
  distinct(npi, .keep_all = TRUE)
nppes <- read_parquet(paths$nppes) %>%
  filter(!state %in% NC, !is.na(npi))
ent_full <- read_csv(paths$ent, show_col_types = FALSE) %>%
  filter(!state %in% NC | is.na(state))

# Mirror the physician-only filter from compute_ent_registry_counts().
# 2026-06-02: delegate to the shared YAML-backed classifier so the lock
# generator and the count function stay structurally in sync.
if (physician_only) {
  source(here::here("R", "utils", "physician_identity_filter.R"))
  .is_physician_text <- function(txt) {
    cls <- classify_physician_credential(txt)
    cls$physician_likely & !cls$non_physician_likely
  }
  nppes <- nppes %>% filter(.is_physician_text(credential))
  if (all(c("is_md", "is_do") %in% names(ent_full))) {
    keep_ent <- (!is.na(ent_full$is_md) & ent_full$is_md) |
                (!is.na(ent_full$is_do) & ent_full$is_do)
    ambiguous <- !keep_ent
    if (any(ambiguous) && "credentials" %in% names(ent_full)) {
      keep_ent[ambiguous] <- .is_physician_text(ent_full$credentials[ambiguous])
    }
    ent_full <- ent_full[keep_ent, , drop = FALSE]
  }
}

ent_npi <- ent_full %>% pull(npi) %>% as.character() %>% .[!is.na(.)]
triple_npis <- intersect(intersect(unique(as.character(ab$npi)),
                                    unique(as.character(nppes$npi))),
                          ent_npi) %>% sort()

stopifnot(length(triple_npis) == ct$sets$n_triple)

sha256 <- function(p) digest::digest(file = p, algo = "sha256")
git_sha <- tryCatch(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE)[1],
  error = function(e) "unknown"
)
git_dirty <- length(tryCatch(
  system2("git", c("status", "--porcelain"), stdout = TRUE),
  error = function(e) character(0))) > 0L

# FIX 2026-05-22 (batch-52 R7): locked_at was wall-clock Sys.time() —
# under REPRODUCIBILITY_MODE=strict the cohort lock embedded a fresh
# timestamp every run, breaking the reproducibility contract (the lock
# file is meant to be the deterministic immutable proof of cohort
# composition). Honor PIPELINE_PINNED_TIMESTAMP for strict mode, and
# fall back to Sys.time() otherwise.
.locked_at_raw <- {
  .pinned_ts <- trimws(Sys.getenv("PIPELINE_PINNED_TIMESTAMP", ""))
  .repro_mode <- trimws(Sys.getenv("REPRODUCIBILITY_MODE", "relaxed"))
  if (nzchar(.pinned_ts)) {
    .pinned_ts
  } else if (.repro_mode == "strict") {
    ## strict mode without a pin: emit a fixed sentinel + warn
    warning("write_ent_cohort_lock: REPRODUCIBILITY_MODE=strict but ",
            "PIPELINE_PINNED_TIMESTAMP unset; using sentinel locked_at",
            call. = FALSE)
    "STRICT_UNPINNED"
  } else {
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  }
}
lock <- list(
  cohort_name        = "triple_match",
  version            = if (physician_only) "v2_physicians" else "v1",
  physician_only     = physician_only,
  locked_at          = .locked_at_raw,
  git_sha            = git_sha,
  git_dirty          = git_dirty,
  r_version          = paste(R.version$major, R.version$minor, sep = "."),
  definition         = paste(
    "Physicians simultaneously present in (a) NPPES 207Y* taxonomy",
    "extract (CONUS, unique NPI), (b) ABOto active board-certified",
    "matched-NPI list (CONUS, practicing_status == 'Practicing', unique",
    "NPI), and (c) AAO-HNS ENThealth public directory (NPI-listed).",
    if (physician_only) paste(
      "Physician-only filter applied: NPPES and ENThealth restricted",
      "to NPIs with MD/DO/MBBS/MBChB/MDCM credentials or blank",
      "credential text; PA-C, NP, APRN, AUD, audiology, DDS, PhD/MS-only",
      "explicitly excluded. ABOto unfiltered (only physicians by",
      "construction)."
    ) else "",
    "Subspecialty assignment by ABOTO_SUBSPECIALTY_PRIORITY:",
    "Pediatric > Neurotology > Sleep > General."
  ),
  input_files        = lapply(paths, function(p) list(
    path   = unbox(p),
    sha256 = unbox(sha256(p)),
    size_bytes = unbox(file.size(p)),
    mtime  = unbox(format(file.mtime(p), "%Y-%m-%dT%H:%M:%S%z"))
  )),
  expected_counts    = list(
    n_raw_certs              = unbox(ct$chain$n_raw),
    n_unique_after_dedup     = unbox(ct$chain$n_unique),
    n_conus                  = unbox(ct$chain$n_conus),
    n_active_rows            = unbox(ct$chain$n_active_rows),
    n_board_unique_npi       = unbox(ct$chain$n_board),
    n_nppes_universe_conus   = unbox(ct$sets$n_npi_univ),
    n_enthealth_unique_phys  = unbox(ct$sets$n_enthealth),
    n_triple_match           = unbox(ct$sets$n_triple)
  ),
  triple_match_npis  = triple_npis
)

out_path <- if (physician_only) {
  here::here("data", "ent", "cohort_lock_triple_match_v2_physicians.json")
} else {
  here::here("data", "ent", "cohort_lock_triple_match_v1.json")
}
out_path_tmp <- paste0(out_path, ".tmp")
jsonlite::write_json(lock, out_path_tmp, pretty = TRUE, auto_unbox = FALSE)
file.rename(out_path_tmp, out_path)

cat(sprintf("Wrote: %s\n", out_path))
cat(sprintf("  cohort_name : %s\n", lock$cohort_name))
cat(sprintf("  version     : %s\n", lock$version))
cat(sprintf("  locked_at   : %s\n", lock$locked_at))
cat(sprintf("  git_sha     : %s%s\n", substr(lock$git_sha, 1L, 12L),
            if (lock$git_dirty) " (DIRTY)" else ""))
cat(sprintf("  n_triple    : %d\n", lock$expected_counts$n_triple_match))
cat(sprintf("  NPI count   : %d\n", length(lock$triple_match_npis)))
cat(sprintf("  File size   : %.1f KB\n", file.size(out_path) / 1024))
