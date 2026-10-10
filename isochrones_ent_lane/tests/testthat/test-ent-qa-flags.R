# =============================================================================
# Regression tests for compute_ent_registry_counts() QA flags
# =============================================================================
# Hand-curated 13-physician fixture that exercises every QA bucket.
# Fixture inputs: tests/fixtures/ent_qa/{abohns_*.csv, nppes.parquet,
# ent_master_roster_final.csv}.  Fixture builder:
# tests/fixtures/ent_qa/build_fixture.R.
#
# Coverage matrix (NPI -> bucket):
#   N001  Triple Match, General Otolaryngology
#   N002  Triple Match, Pediatric Otolaryngology
#   N003  Triple Match, Neurotology
#   N004  Triple Match, Sleep Medicine
#   N005  Triple Match, multi-cert (Pediatric + Sleep -> Pediatric wins)
#   N006  Board only — non-CONUS NPPES, CONUS ABOto (state-mismatch zone)
#   N007  Excluded by CONUS filter (state_full = Hawaii)
#   N008  Excluded by retirement filter (no NPI)
#   N009  NPPES + ENThealth, absent from ABOto (zone_712)
#   N010  NPPES only
#   N011/N012  Two abohns_ids share NPI 1000000011 (dup-NPI dedup)
#   N013  ENThealth NA NPI (orphan)
# =============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(here)
  library(dplyr)
})

source(here::here("R", "ent_registry_counts.R"))

fixture <- here::here("tests", "fixtures", "ent_qa")
ct <- compute_ent_registry_counts(paths = list(
  abohns_raw     = file.path(fixture, "abohns_certifications_clean.csv"),
  abohns_matched = file.path(fixture, "abohns_with_npi_master.csv"),
  nppes          = file.path(fixture, "ent_from_nppes_flat_file.parquet"),
  ent            = file.path(fixture, "ent_master_roster_final.csv")
))

# ── Chain counts ─────────────────────────────────────────────────────────────
test_that("chain counts match the fixture's hand-traced values", {
  # 15 raw cert rows in abohns_raw -> 10 unique abohns_ids
  expect_equal(ct$chain$n_raw,         15L)
  expect_equal(ct$chain$n_unique,      10L)
  expect_equal(ct$chain$drop_dedupe,    5L)  # 5 secondary subspec certs
  # 10 unique - 1 Hawaii (N007) = 9 CONUS
  expect_equal(ct$chain$drop_conus,     1L)
  expect_equal(ct$chain$n_conus,        9L)
  # 9 CONUS - 1 retired (N008, no NPI + Likely Retired/Inactive) = 8 active rows
  expect_equal(ct$chain$drop_retired,   1L)
  expect_equal(ct$chain$n_active_rows,  8L)
  # 8 active rows - 1 dup-NPI (N011 vs N012, same NPI 1000000011) = 7 unique NPIs
  expect_equal(ct$chain$drop_npi_dup,   1L)
  expect_equal(ct$chain$n_board,        7L)
})

test_that("117 dup NPIs counter matches fixture's 1 dup", {
  expect_equal(ct$chain$n_npi_dups_distinct, 1L)
})

# ── Set totals ───────────────────────────────────────────────────────────────
test_that("set totals match the fixture's hand-traced values", {
  # NPPES has 9 rows; CONUS-filtered drops 1 (N006 in HI) = 8 unique
  expect_equal(ct$sets$n_npi_univ,  8L)
  expect_equal(ct$sets$n_board,     7L)
  # ENThealth has 7 rows; 1 has NA NPI (orphan); 6 unique non-NA NPIs
  # n_enthealth = unique NPIs (6) + orphan count (1) = 7
  expect_equal(ct$sets$n_enthealth, 7L)
})

test_that("triple match equals N001-N005 (5 NPIs)", {
  # N001..N005 are all in NPPES + Board + ENThealth
  expect_equal(ct$sets$n_triple, 5L)
})

# ── Subspecialty assignment (priority resolution) ────────────────────────────
test_that("subspecialty assignment uses Pediatric > Neurotology > Sleep > General", {
  by_sub <- ct$by_subspecialty %>% dplyr::arrange(subspecialty)

  # N002 (Pediatric only) + N005 (Pediatric+Sleep -> Pediatric wins) = 2 Peds
  expect_equal(
    by_sub$n_total[by_sub$subspecialty == "Pediatric Otolaryngology"], 2L
  )
  # N003 only
  expect_equal(
    by_sub$n_total[by_sub$subspecialty == "Neurotology"], 1L
  )
  # N004 only (N005 went to Peds because of priority)
  expect_equal(
    by_sub$n_total[by_sub$subspecialty == "Sleep Medicine"], 1L
  )
  # N001 + N011 (deduped from N011/N012) = 2 Generals (N006 also General but
  # N006 is NPPES-non-CONUS so not in active Board cohort)
  # Actually: Board cohort = 7. Subspec breakdown: 2 Peds + 1 Neuro + 1 Sleep
  # + 3 General. The 3 Generals are N001, N006 (CONUS in ABOto, non-CONUS
  # in NPPES — counts in Board), and N011/N012 deduped to one.
  expect_equal(
    by_sub$n_total[by_sub$subspecialty == "General Otolaryngology"], 3L
  )
  expect_equal(sum(by_sub$n_total), ct$chain$n_board)
})

# ── State-mismatch flag ──────────────────────────────────────────────────────
test_that("state-mismatch flag fires for N006 (CA in ABOto, HI in NPPES)", {
  sm <- ct$qa$state_mismatch %>%
    filter(npi_chr == "1000000006") %>% dplyr::pull(state_mismatch_flag)
  expect_true(length(sm) == 1L && sm)
})

test_that("state-mismatch flag does NOT fire for N001-N005 (matching states)", {
  sm <- ct$qa$state_mismatch %>%
    filter(npi_chr %in% paste0("100000000", 1:5)) %>%
    dplyr::pull(state_mismatch_flag)
  expect_equal(length(sm), 5L)
  expect_true(!any(sm))
})

# ── NPPES entity-type defensive check ────────────────────────────────────────
test_that("all NPPES fixture rows are individual (entity check passes)", {
  expect_equal(ct$qa$n_nonindividual_in_nppes, 0L)
  expect_true(all(ct$qa$nppes_entity$nppes_entity_type == "individual"))
})

# ── 712-zone decomposition ───────────────────────────────────────────────────
test_that("zone_712 splits N009 (absent) + N006 (state-mismatch)", {
  # N009 in NPPES + ENThealth, NOT in active Board: absent_from_abohns
  # N006 in NPPES (HI) + ENThealth? N006 isn't in ENThealth in this fixture,
  # so zone_712 = NPPES ∩ ENThealth ∖ Board = just N009 = 1
  expect_equal(ct$zones$nppes_ent, 1L)
  expect_equal(ct$zone_712$absent_from_abohns, 1L)
  expect_equal(ct$zone_712$in_abohns_state_mismatch, 0L)
})

# ── Anomaly rosters reconcile with zone counts ───────────────────────────────
test_that("anomaly rosters reconcile with zone counts", {
  expect_equal(nrow(ct$anomalies$abohns_dup_npis),
               2L)   # N011 + N012 both kept (each counted)
  expect_equal(nrow(ct$anomalies$ent_no_npi),
               ct$n_ent_orphan)  # 1 = N013
  expect_equal(nrow(ct$anomalies$zone_712),
               ct$zones$nppes_ent)
})

# ── Closure assertions inside compute_ent_registry_counts() pass ─────────────
# (compute_ent_registry_counts() will throw if any closure fails — getting
# here means all 8 chain + 4 zone closures + 712 split closure all passed.)
test_that("all closure assertions inside compute_ent_registry_counts pass", {
  expect_no_error(
    compute_ent_registry_counts(paths = list(
      abohns_raw     = file.path(fixture, "abohns_certifications_clean.csv"),
      abohns_matched = file.path(fixture, "abohns_with_npi_master.csv"),
      nppes          = file.path(fixture, "ent_from_nppes_flat_file.parquet"),
      ent            = file.path(fixture, "ent_master_roster_final.csv")
    ))
  )
})

# ── ABOTO_SUBSPECIALTY_PRIORITY constant invariants ──────────────────────────
test_that("ABOTO_SUBSPECIALTY_PRIORITY has 4 levels with General last", {
  expect_equal(length(ABOTO_SUBSPECIALTY_PRIORITY), 4L)
  expect_equal(ABOTO_SUBSPECIALTY_PRIORITY[4], "General Otolaryngology")
})

# ── Subspecialty triangulation flag (ABOto vs NPPES vs ENThealth) ────────────
test_that("subspecialty triangulation table has one row per active Board NPI", {
  tri <- ct$qa$subspecialty_triangulation
  expect_equal(nrow(tri), ct$chain$n_board)  # 7 in the fixture
  expect_true(all(c("aboto_subspecialty", "nppes_subspec_aboto",
                    "ent_subspec_aboto", "subspecialty_agreement") %in% names(tri)))
})

test_that("triangulation: N002 (Pediatric in all three) reads all_three_agree", {
  tri <- ct$qa$subspecialty_triangulation
  row <- tri %>% dplyr::filter(npi_chr == "1000000002")
  expect_equal(row$aboto_subspecialty,    "Pediatric Otolaryngology")
  expect_equal(row$nppes_subspec_aboto,   "Pediatric Otolaryngology")
  expect_equal(row$ent_subspec_aboto,     "Pediatric Otolaryngology")
  expect_equal(row$subspecialty_agreement, "all_three_agree")
})

test_that("triangulation: N011 (General; no ENThealth row) reads missing_enthealth", {
  tri <- ct$qa$subspecialty_triangulation
  row <- tri %>% dplyr::filter(npi_chr == "1000000011")
  expect_equal(row$aboto_subspecialty,  "General Otolaryngology")
  expect_equal(row$nppes_subspec_aboto, "General Otolaryngology")
  expect_true(is.na(row$ent_subspec_aboto))
  expect_equal(row$subspecialty_agreement, "aboto_nppes_agree_enthealth_missing")
})
