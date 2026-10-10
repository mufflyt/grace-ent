# ENT QA-flag regression fixture

Hand-curated 13-physician test fixture exercising every QA bucket in
`compute_ent_registry_counts()`. Used by
`tests/testthat/test-ent-qa-flags.R` (33 assertions across 12 tests).

## Files

| File | Rows | Description |
| --- | ---: | --- |
| `build_fixture.R` | — | Deterministic builder. Re-run to regenerate the four input files below. |
| `abohns_certifications_clean.csv` | 15 | Cert-level (mirror of `data/ent/abohns_certifications_clean.csv` schema) |
| `abohns_with_npi_master.csv` | 10 | Physician-level matched file |
| `ent_from_nppes_flat_file.parquet` | 9 | NPPES extract (mirrors `artifacts/ent/ent_from_nppes_flat_file.parquet` schema) |
| `ent_master_roster_final.csv` | 7 | ENThealth roster |

## Coverage matrix (NPI → tested bucket)

| NPI | abohns_id(s) | What it tests |
| --- | --- | --- |
| `1000000001` | 1 | **Triple Match**, General Otolaryngology, CONUS, valid phone, current activity. The canonical happy-path row. |
| `1000000002` | 2 | Triple Match, **Pediatric Otolaryngology** subspecialty (priority test) |
| `1000000003` | 3 | Triple Match, **Neurotology** subspecialty |
| `1000000004` | 4 | Triple Match, **Sleep Medicine** subspecialty |
| `1000000005` | 5 | Triple Match, **multi-cert** (Pediatric + Sleep). Tests `ABOTO_SUBSPECIALTY_PRIORITY` — Pediatric wins. |
| `1000000006` | 6 | **State-mismatch case**: ABOto state = California, NPPES state = Hawaii. Excluded from active Board cohort because NPPES is non-CONUS, but logged in `ct$qa$state_mismatch`. |
| `1000000007` | 7 | **CONUS-filter exclusion**: ABOto state = Hawaii. Drops at step 2. |
| (NA NPI)    | 8 | **Retirement-filter exclusion**: `practicing_status = "Likely Retired/Inactive"` AND no NPI match. Drops at step 3. |
| `1000000009` | (none) | **Zone 712 (absent_from_abohns)**: in NPPES + ENThealth but no ABOto record. |
| `1000000010` | (none) | **NPPES-only zone**: no ABOto record, no ENThealth listing. |
| `1000000011` | 11 + 12 | **Duplicate-NPI dedup**: same NPI mapped to two distinct `abohns_id` values. Should collapse to 1 NPI in the active Board cohort. |
| (NA NPI)    | (N013) | **ENThealth orphan**: ENThealth row with no NPI. Counts as `n_ent_orphan`. |

## Expected derived counts (verified by tests)

| Quantity | Value |
| --- | ---: |
| `chain$n_raw` (raw cert rows) | 15 |
| `chain$n_unique` (distinct abohns_id) | 10 |
| `chain$n_conus` (after CONUS filter) | 9 |
| `chain$n_active_rows` (after retirement filter) | 8 |
| `chain$n_board` (unique NPI, active, CONUS) | 7 |
| `chain$n_npi_dups_distinct` (NPIs in 2+ ABOto rows) | 1 |
| `sets$n_npi_univ` (CONUS NPPES) | 8 |
| `sets$n_enthealth` (unique physicians + orphans) | 7 |
| `sets$n_triple` | 5 |
| `zones$nppes_ent` (zone 712) | 1 |
| `n_ent_orphan` | 1 |

## Subspecialty distribution

| Subspecialty | n | Members |
| --- | ---: | --- |
| General Otolaryngology | 3 | N001, N006, N011 |
| Pediatric Otolaryngology | 2 | N002, N005 (multi-cert priority) |
| Neurotology | 1 | N003 |
| Sleep Medicine | 1 | N004 |

## Rebuild

```bash
Rscript tests/fixtures/ent_qa/build_fixture.R
```

The builder script is idempotent — running it twice produces byte-identical
output files.

## Run tests

`testthat::test_file()` will fail at helper-loading time because
`tests/testthat/helper-abog-npi-matching.R` requires the `sqldf` package
(unrelated to this fixture). Workaround:

```bash
Rscript -e 'suppressPackageStartupMessages({library(testthat); library(here); library(dplyr)})
source("R/ent_registry_counts.R")
testthat::with_reporter("progress", {
  source("tests/testthat/test-ent-qa-flags.R", local = TRUE)
})'
```

Expected output:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 33 ]
```

## Adding new test cases

To extend coverage:

1. Add the NPI to the appropriate input file in `build_fixture.R` with a
   comment explaining what bucket it tests.
2. Re-run the builder.
3. Add an `expect_*()` line to `tests/testthat/test-ent-qa-flags.R`
   asserting the expected QA value.
4. Update this README's coverage matrix.

## Files NOT in the fixture (intentionally)

- **Medicare Part B**: `compute_ent_registry_counts()` does not touch Part B.
  The activity-recency proxy (computed by
  `scripts/export_mystery_caller_cohort.R`) is tested separately if at all.
- **RUCA / phone validation**: also computed by the cohort exporter, not by
  the shared count function.
- **Cohort lock file**: skipped automatically when `paths` arg is non-default
  (the `using_default_paths` guard in `compute_ent_registry_counts()`).
