# Deferred Demographic Enrichment Specs

**Status:** Historical
**Do not use for operations:** written before the frozen scientific baseline (`stabilization-20260920`, 2026-09-20) and not referenced by CLAUDE.md or the documentation indexes; retained as provenance. Current guidance lives in CLAUDE.md, docs/START_HERE.md and the technical appendix register.


This file documents the demographic-enrichment spec items that were too
large to ship in the active conversation thread. Each one warrants a
dedicated multi-hour session with proper code review.

Status as of 2026-05-22:

## P2#12 — Regional Albers Projection per State

**Issue:** All Step 8 geometry operations project to a single national
EPSG:9311 (US National Atlas Equal Area). For states at the geographic
extremes (Maine, Hawaii, Alaska), this introduces small but non-zero
area distortion (~0.01-0.1%). When summed across many isochrones,
overlap fractions can total above 1.0 ("demographic leak").

**Scope:** Modify `R/step8_modules/step8_load_tracts.R` to compute tract
land area in a per-state projection (SPCS or UTM zone) before
projecting the geometry to the national EPSG:9311 for storage.

**Files:**
- `R/step8_modules/step8_load_tracts.R` — area calculation
- New utility: `R/utils/state_projections.R` mapping state FIPS →
  recommended SPCS/UTM zone

**Risk:** Low if done as an OPTIONAL pass that compares projected-area
vs current-area and only emits warnings when delta > 0.05%. High if
swapped in as the new default without regression testing.

**Estimated effort:** 4-6 hours (writing the FIPS→SPCS map, testing
area-preservation on each state, regression-checking national totals).

---

## P2#14 — `future_map` Worker Environment

**Issue:** The orchestrator sources modules with `local = TRUE` to share
scope. Forked `furrr::future_map` workers run in isolated R sessions
and don't see lexical-scope variables from `main()` unless explicitly
exported.

**Current state:** `R/step8_modules/step8_compute_overlap.R:351-371`
uses `future::future({ ... })` with explicit `thresh_args` passed as
arguments to `process_one_threshold()`. As long as
`process_one_threshold` only references its arguments (not module-
level globals), workers function correctly.

**Risk in current code:** `TRACT_STATE_08` is a module-level list
referenced at `R/step8_modules/overlap_engine.R:367` (commented) and
in `data_loading.R:273`. If any callee inside a future-worker references
`TRACT_STATE_08` by lexical scope, that worker will fail with
"object 'TRACT_STATE_08' not found".

**Scope:** Add an explicit `clusterExport`/`furrr_options(globals = ...)`
to make `TRACT_STATE_08` (and any other module-level state) available
to workers. Add a regression test that runs a 2-worker plan with the
module-level state and asserts no "object not found" errors.

**Estimated effort:** 2-3 hours.

---

## P2#17 — Vintage-Specific Tract Loading + 2010↔2020 Crosswalk

**Issue:** The study period covers 2013-2023, but Census tracts were
re-drawn between the 2010 and 2020 Decennial Census. Loading 2020-vintage
tract boundaries for 2013 data means GEOID strings match while physical
locations may not — boundary shifts get attributed to physician movement.

**Scope:**
1. Modify `R/step8_modules/step8_load_tracts.R` to accept a `data_year`
   argument and load the corresponding tract vintage from tigris.
2. Build `R/utils/acs_tract_crosswalk_2010_2020.R` if not present, sourcing
   the Census-published 2010-to-2020 tract relationship file.
3. For pre-2020 study years, area-weighted-apportion 2010 tract overlaps
   onto 2020 GEOIDs for consistency with downstream 2020-keyed
   aggregations.

**Files:** `step8_load_tracts.R`, new `acs_tract_crosswalk_2010_2020.R`,
plus a config entry for the crosswalk download URL.

**Estimated effort:** 6-8 hours (download / cache the crosswalk,
implement apportionment math, regression-test against a known
2010→2020 boundary change).

---

## P2#18 — County-Level Tiling for Memory-Bound States

**Issue:** Processing California or Texas at high resolution can exceed
the per-worker memory cap. Current `R/step8_modules/overlap_engine.R`
has state-level chunking (`STEP8_CHUNK_BY_STATE`), but a single CA pass
can still exceed 16 GB on the largest isochrones.

**Scope:** Add COUNTYFP-level sub-chunking within each state pass. The
existing state loop at `overlap_engine.R:254-267` becomes a state →
county nested loop. After per-county processing, `bind_rows` aggregates
back to the state-level result.

**Estimated effort:** 4-5 hours including memory-profiling on CA / TX
test runs.

---

## P1#8 — R/11 Cohort-Loop Area Weighting

**Issue:** `R/11-create-access-by-group-with-moe-TRACT.R` (13K lines)
applies spatial weights at the AGGREGATE cohort level, using a single
`pop_fraction` scalar per tract. For per-age-group accessibility, the
spec recommends a long-format demographic table
(Tract × AgeGroup × Population) multiplied by per-tract
`overlap_fraction` before re-aggregation.

**Scope:** Major restructure of the Step 11 aggregation path. The
13K-line file needs careful sectioning before edit — recommend
extracting the per-age-group aggregation into a new helper file
(`R/step11_modules/age_cohort_aggregation.R`) so the diff is
reviewable.

**Estimated effort:** 8-12 hours, with full Step 11 test-suite regression
as the success gate.

---

## Status Snapshot

| Done | Spec |
|------|------|
| Phase 1 #1 — manifest validator |
| Phase 1 #3 — `pmax(inner_term, .Machine$double.eps)` |
| Phase 1 #4 — GEOID `str_pad` + assertion |
| Phase 1 #6 — vintage-aware `get_acs_vars` registry |
| Phase 1 #7 — top-coded MOE imputation |
| Phase 1 #9 — `moe_sum_correlated` |
| Phase 1 #10 — atomic cache write |
| Phase 1 #2 — `summarize_with_moe` |
| Phase 1 #5 — verified water-clip/ALAND in place |
| Phase 2 #11 — adaptive rasterization |
| Phase 2 #13 — `assert_boundary_inclusion` helper |
| Phase 2 #16 — verified zero-area sentinel in place |
| Phase 2 #19 — verified partial-cell coverage in place |

**Deferred (this document):** P2#12, P2#14, P2#17, P2#18, P1#8.
