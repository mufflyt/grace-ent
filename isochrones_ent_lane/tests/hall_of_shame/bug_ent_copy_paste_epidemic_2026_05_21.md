# Hall of Shame Entry: The "ENT Copy-Paste Epidemic" — Otolaryngology Identifiers in an OB/GYN Codebase

**Date:** 2026-05-21 (10+ sites found and fixed in a single day's audit batches)
**Severity:** CRITICAL at the table-name + filename layer; LOW at the variable-name layer. The CRITICAL sites caused 4 medicaid matching strategies (NY, CA, FL, MI) to silently return ZERO matches for the entire run history of this project. The bug was invisible at code review and undetectable without runtime forensics (CloudTrail-equivalent: `dbExistsTable(con, "ca_medicaid_otolaryngology")` returning FALSE, then the matcher returning an empty data.frame without error).
**Status:** FIXED (28 sites fixed today across 4 audit batches; see commit `40a361c27` and predecessors)
**Cumulative wasted runtime:** Unknown but potentially years — these matchers have been "running" since the project began but never producing matches because the tables they read from do not exist.
**Time-to-detection:** Surfaced only when a batch-2 audit query confirmed the underlying DuckDB tables: `ca_medicaid_otolaryngology: exists=FALSE`, `ca_medicaid_obgyn: exists=TRUE`. The matchers were reading the wrong (nonexistent) table.

---

## Symptom

The project is an OB/GYN workforce analysis pipeline. The same author had previously written an analogous pipeline for ENT (Otolaryngology) workforce analysis. When the OB/GYN pipeline was built, large swaths of the ENT codebase were copy-pasted as a starting point, with the **intent** to rename all ENT-specific identifiers to OB/GYN equivalents. The renames were done inconsistently.

The result: **identifiers named `otolaryngology`, `ENT`, `Otolaryngology_*`, etc. appear in 10+ places in the OB/GYN codebase**, where most are still being actively executed against ENT-named resources that don't exist in the OB/GYN database.

### The CRITICAL tier — runtime data loss

```r
# R/match_ca_medicaid_abog.R:120
FROM ca_medicaid_otolaryngology
WHERE legal_name LIKE '%,%'
```

The matcher's SQL reads `FROM ca_medicaid_otolaryngology`. The actual table in
the OB/GYN DuckDB is `ca_medicaid_obgyn`. Result: `dbGetQuery` raises a SQL
error → upstream `tryCatch(error = function(e) data.frame())` swallows it →
the matcher returns 0 matches → strategy16_ca_medicaid contributes 0 NPIs
→ the strategy_helpers method-priority LUT counts it as "no candidates" →
end-to-end matching coverage silently lower than the design specifies.

Identical bug in:
- `R/match_fl_medicaid_abog.R:162` (FL)
- `R/match_ny_medicaid_abog.R:125` (NY)
- `R/match_mi_medicaid_abog.R:65,127` (MI — referenced `mi_medicaid_otolaryngology` when the actual table was `mi_medicaid_providers`)

**Plus** matching `*_otolaryngology` references in the loader scripts that
WRITE these tables (`R/load_ca_medicaid_obgyn.R`, `R/load_ny_medicaid_obgyn.R`,
`R/load_texas_medicaid.R`, `R/load_vermont_medicaid.R`, 4 files in
`R/data_download/A-load_*_medicaid_to_duckdb.R`), AND the canonical pipeline
scripts that orchestrate them (`R/canonical_specialty_npi_pipeline.R` —
4 sites, `R/canonical_abog_npi_pipeline_STABLE.R` — 4 sites).

### The HIGH tier — wrong source files

```r
# R/geocode_missing_subspecialist_addresses.R:11
#   - data/temporal_locations/otolaryngology_locations_2024.csv (addresses)

# R/geocode_missing_subspecialist_addresses.R:57
f <- here(sprintf("data/temporal_locations/otolaryngology_locations_%d.csv", year))
```

A script in the OB/GYN matching path read year-by-year data from a directory
named after ENT specialties. If the OB/GYN equivalent file exists at the
expected path (`obgyn_locations_*.csv`), the ENT-named file does not exist
and the loop body's `if (file.exists(f))` silently returned NULL for every
year. Result: the geocoding script processed 0 addresses.

### The MEDIUM tier — keyword lists

```r
# R/process_connecticut_medicaid_for_abog.R:87
physician_keywords <- c(
  "physician", "doctor", "md", "do",
  "obstetric", "gynecolog", "otolaryngology", "ob/gyn", "Otolaryngology",
  ...
)
```

The CT Medicaid → ABOG matcher had `otolaryngology` and `Otolaryngology`
included as "physician keywords." Including them in this list isn't strictly
wrong (they ARE physician specialties) but expanding the search space to ENT
practitioners in an OB/GYN pipeline pollutes the candidate pool with
false-positive matches. Pure ENT copy-paste — the keyword list was
inherited verbatim from the ENT version of the script.

### The LOW tier — cosmetic naming

10+ variable, CTE, and comment names like:
- `Otolaryngology_TAXONOMY_CODES <- c(...)` containing OB/GYN taxonomy codes (`strategy_01`)
- `deactivated_otolaryngology AS (...)` CTE that queries OB/GYN taxonomy (`strategy_13`)
- `otolaryngology_code_found` column alias containing OB/GYN code matches (`strategy_01`)
- Comment lines documenting "Otolaryngology populations" in OB/GYN scripts

These are cosmetic but they signal that the author never did a single
"otolaryngology" / "ENT" grep+rename sweep after the copy-paste.

---

## Root Cause

A senior researcher who builds workforce-analysis pipelines for multiple
specialties used the ENT pipeline as a starting template for the OB/GYN
pipeline. The mental model was:

1. `cp -R ent_pipeline/ obgyn_pipeline/`
2. Sed-replace `otolaryngology` → `obgyn` "later"
3. Develop OB/GYN-specific features in parallel

Step 2 was never completed comprehensively. Each individual rename was done
as it surfaced (someone fixed a CA medicaid table reference at some point,
but missed the FL/NY/MI siblings). The codebase grew around the residual
ENT-named identifiers, with the result that:

- New code that DOES use the correct `*_obgyn` names succeeds.
- Old code that STILL uses the legacy `*_otolaryngology` names returns
  silently empty results (because the ENT tables don't exist in the OB/GYN
  DuckDB).

The two coexist without conflict — the old code just never produces output,
and downstream consumers treat "no rows" as "no matches" rather than "the
upstream query is broken."

---

## Why Review Didn't Catch It

- **The strings look semantically plausible.** `ca_medicaid_otolaryngology`
  is a syntactically valid R identifier and a syntactically valid SQL table
  name. A reviewer scanning the diff sees "California Medicaid table" and
  moves on. The reviewer would have to know — or check — that the actual
  table is named `ca_medicaid_obgyn`.

- **Errors are swallowed at the strategy boundary.** Every matcher wraps
  its SQL in `tryCatch(error = function(e) data.frame())` for graceful
  degradation when an optional data source is unavailable. The same
  graceful-degradation path silently absorbs "the table I am querying does
  not exist" — which should be a hard error.

- **No data-quality assertion that a strategy that COULD produce matches
  DID produce matches.** A matcher that returns 0 rows is treated as
  "this strategy didn't find anyone" — which is acceptable for strategies
  that legitimately have low yield. But there is no signal that
  distinguishes "real zero" from "the underlying table doesn't exist."

- **The integration tests use a fixture database** where the test fixtures
  happen to be loaded with both `*_obgyn` and `*_otolaryngology` table
  names (because the loader scripts ALSO had the ENT copy-paste bug, so
  they wrote to `*_otolaryngology` table names). The test would
  inadvertently match the broken matcher to the broken loader's output
  and "pass" — a producer/consumer drift that produced consistently wrong
  data on both ends.

- **Cosmetic ENT names in variables are a yellow flag.** A reviewer who
  spots `Otolaryngology_TAXONOMY_CODES` in an OB/GYN script might say
  "weird name, but the contents look right" rather than treating it as
  smoke. The cosmetic LOW-tier bugs are themselves signals of the
  CRITICAL-tier bugs elsewhere — wherever you see one ENT-named
  identifier in OB/GYN code, you should grep for all of them.

---

## The Fix

A bulk `sed` across the project replaced `*_medicaid_otolaryngology` with
`*_medicaid_obgyn` (state-by-state) AND `mi_medicaid_otolaryngology` with
`mi_medicaid_providers` (different naming convention for MI). Files touched:

- 4 matcher files (`R/match_{ca,fl,ny,mi}_medicaid_abog.R`)
- 4 loader files (`R/load_{ca,ny,texas,vermont}_medicaid*.R`)
- 4 data-download loaders (`R/data_download/A-load_{ca,ny,fl,mi}_medicaid_to_duckdb.R`)
- 2 canonical pipeline scripts (8 references total)
- 1 connecticut matcher (keyword list pruned)
- 1 geocode script (primary file path renamed to `obgyn_locations_*.csv`
  with legacy fallback + warning)
- Several strategy files (cosmetic naming carryover documented but not
  renamed yet)

The CRITICAL sites are fully fixed. The LOW-tier cosmetic naming is
deferred to a future hygiene sweep — these names don't affect runtime
behavior, but every one of them is a future-confusion landmine.

---

## Regression Tests

Three layers of defense:

### Layer 1 — Static grep, project-wide

```bash
# tests/lint/no_otolaryngology_in_obgyn_paths.sh
# Catches any FROM clause, dbExistsTable, dbWriteTable, or file path
# in an OB/GYN-context file that references an *_otolaryngology resource.
violations=$(grep -rnE '(FROM|dbExistsTable\(|dbWriteTable\(|locations_)[[:space:]]*["'"'"']*[a-z]+_medicaid_otolaryngology|otolaryngology_locations' \
              R/ --include='*.R' --exclude-dir=archive --exclude-dir=@archive \
              | grep -vE '^[^:]+:[^:]+: *(--|#)' \
              || true)
[[ -z "$violations" ]] || { echo "FAIL: ENT identifiers in OB/GYN paths:"; echo "$violations"; exit 1; }
echo "PASS"
```

### Layer 2 — Behavioral test: every matcher's FROM clause must reference an actual table

```r
test_that("every matcher FROM clause references a table that exists in the OB/GYN DuckDB", {
  skip_if_not(DBI::dbIsValid(con))  # skip if DB not mounted in test env
  matchers <- list.files(here::here("R"), pattern = "^match_.*_abog\\.R$", full.names = TRUE)
  for (m in matchers) {
    src <- paste(readLines(m), collapse = "\n")
    from_clauses <- regmatches(src,
                               gregexpr("FROM\\s+([a-z_][a-z_0-9]+)", src,
                                        ignore.case = TRUE))[[1]]
    tables <- unique(sub("^FROM\\s+", "", from_clauses))
    # Filter out CTE names (those defined in the same file via WITH)
    cte_names <- regmatches(src, gregexpr("WITH\\s+([a-z_][a-z_0-9]+)\\s+AS",
                                            src, ignore.case = TRUE))[[1]]
    cte_names <- unique(sub(".*WITH\\s+([^\\s]+)\\s+AS.*", "\\1", cte_names))
    tables <- setdiff(tables, cte_names)
    for (t in tables) {
      expect_true(DBI::dbExistsTable(con, t),
                  info = sprintf("%s references nonexistent table: %s",
                                 basename(m), t))
    }
  }
})
```

### Layer 3 — Pre-commit hook checking for ENT identifier introduction

A `.git/hooks/pre-commit` script that fails any commit reintroducing an
`*_otolaryngology` identifier into the `R/` tree. Cheap, deterministic,
catches the bug class at the most expensive moment (before it lands in
history).

---

## Generalized Lesson

**A successful copy-paste from one workforce-analysis pipeline to another
is incomplete until a project-wide sed pass renames every specialty-coded
identifier.** Three rules:

1. **Treat any reference to a different specialty's identifier as a
   landmine, even when the surrounding code looks correct.** Grep is your
   friend. Before commit: `grep -rnE
   "<other_specialty>|<OtherSpecialty>" R/` from the project root. If
   you're working in an OB/GYN repo, this should return zero hits.

2. **The boundary between "graceful degradation" and "silent data loss" is
   whether the failing query SHOULD have produced data.** If the underlying
   table is OPTIONAL (e.g., Medicaid data for a state we don't have
   loaded), then `error = function(e) data.frame()` is correct. If the
   table is REQUIRED for the strategy to function, the error must
   propagate. The cleanest rule: any `error = function(e) ...` that
   silently produces an empty result MUST be paired with an upstream
   `dbExistsTable()` check that distinguishes "the table doesn't exist"
   from "the query failed for some other reason." When the table doesn't
   exist, stop loudly with a hint to the loader script that should have
   created it.

3. **Cosmetic identifier carryover (variable / CTE names with the wrong
   specialty in the name, but containing the right specialty's data) is
   smoke from a fire elsewhere.** When you find one, search the codebase
   for ALL instances of that wrong-specialty identifier — at least some
   of the matches will be CRITICAL-tier bugs (table names, file paths)
   that haven't been spotted yet.

---

## How To Avoid This Bug Class

For any project that adopts code from a different-specialty version of the
same pipeline:

- **Codify the rename list before the copy.** Pin a YAML file in the new
  project's `config/` with `specialty: obgyn` and a map of all
  specialty-coded identifiers (taxonomies, table names, CSV file
  prefixes). Then write a `scripts/migrate_from_ent.sh` that does the
  rename in one shot rather than ad-hoc over time.

- **Add a project-wide lint check on every commit.** The pre-commit hook
  above is the minimum bar. CI should also run it on every PR.

- **Loaders and matchers should be tested in lockstep.** Today's discovery
  was that BOTH the loader scripts (`R/load_ca_medicaid_obgyn.R`) AND the
  matcher scripts (`R/match_ca_medicaid_abog.R`) had the ENT name. They
  agreed with each other, but the actual DB had the correct OB/GYN name —
  so the producer/consumer pair was BROKEN in agreement. An end-to-end
  test that round-trips data through (loader → DB → matcher) would have
  surfaced this immediately.

This is the **8th Hall of Shame entry** for 2026-05-21 and the one with
the broadest spread (10+ sites across 14+ files). The day's running
theme remains: "the failure was silent in source but obvious at
runtime, and the runtime path was never exercised against real data."
The fix for this case wasn't a single brilliant insight — it was a 5-line
DuckDB query (`dbExistsTable("ca_medicaid_otolaryngology")` → FALSE).
That query should be a standing part of the matcher's startup self-check.
