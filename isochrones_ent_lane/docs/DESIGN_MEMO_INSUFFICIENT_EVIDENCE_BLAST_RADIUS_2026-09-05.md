# Design memo: blast radius of an `insufficient_evidence` row type

> ## FROZEN AS FUTURE WORK — 2026-09-05
>
> **Decision: do not implement.** Owner ruling. No consumer migration, no schema
> change. The consumer-impact table below stands as the record of what such a
> change would cost, should it ever be revisited.
>
> **Fixture F4 is BLOCKED** pending a PI definition of the candidate universe
> (§1, U1/U2/U3). F1, F2, F3 and F5 remain valid unblocked specifications.
>
> **§0 IS SUPERSEDED.** It describes the 321-row predicate disagreement as
> unresolved. It is resolved, and it is **NOT an admission defect** — all 321
> were correctly admitted by the rule that actually governed the build, and the
> builder I named there (script 16) did not produce the artifact. See
> [`FORENSIC_ADMISSION_RULE_DISCREPANCY_321_2026-09-05.md`](FORENSIC_ADMISSION_RULE_DISCREPANCY_321_2026-09-05.md).
> Read §0 only for the measurement, never for the attribution.
>
> The `04_sensitivity_analysis.R:245` denominator was a genuine independent bug
> and is fixed in PR #716.

**Status:** **MEMO ONLY — NOTHING IMPLEMENTED.** No schema, builder, or artifact modified. `b7403fe8` remains frozen.
**Date:** 2026-09-05
**Companions:** [proposal](DESIGN_PROPOSAL_INSUFFICIENT_EVIDENCE_ROW_TYPE_2026-09-05.md) · [reach diagnosis](RETIREMENT_CONSENSUS_REACH_DIAGNOSIS_2026-09-05.md)

---

## 0. A correction to the reach diagnosis, before anything else

The reach diagnosis attributed admission to one documented clause,
`16_create_consensus_dynamic.R:593-595` (`source_count >= 2 AND
year_source_count >= 1 AND evidence_family_count >= 2`). **That attribution is
not fully correct, and the memo below depends on the distinction.**

What is empirically exact, verified across 1,952,043 NPIs:

| rows in `retirement_signals_unified` | NPIs | admitted to consensus |
|---|---|---|
| exactly 1 | 1,223,507 | **0 (0.000%)** |
| ≥2 | 728,536 | **728,536 (100.000%)** |

What does **not** match the documented clause: **321 admitted rows carry final
`source_count = 1`**, and 313 of them are classified retired. 248 are
`DUAL_FAMILY` with `estimator_n_families = 1`. Tier A is not the bypass — all
321 have `tier_a_retirement_event = false`.

So the **effective** admission rule is "≥2 signal rows", while the **documented**
rule is "≥2 distinct sources and ≥2 evidence families". Those are different
predicates, and 321 rows sit in the gap. Either the columns are recomputed after
admission, or there is a second admission path I have not traced.

**Consequence for this memo:** question 1 (the candidate universe) cannot be
answered from the builder source alone. Any implementation must first establish
which predicate is actually governing, because the row type has to be emitted
for exactly the complement of whatever that predicate is.

---

## 1. Candidate universe: for whom is a row guaranteed?

Three candidate definitions, with very different denominators:

| # | Universe | N | What reach would then mean |
|---|---|---|---|
| **U1** | NPIs with ≥1 row in `retirement_signals_unified` | **1,952,043** | "of physicians showing any retirement signal, how many can we adjudicate" |
| **U2** | All NPIs in the provider universe (NPPES-derived intake) | not measured here | "of all physicians, how many can we adjudicate" |
| **U3** | The analytic cohort only (ABOG-certified OB/GYN subspecialists) | ~60k | "of physicians in the study, how many can we adjudicate" |

**Recommendation: U1.** It is the only one where the guarantee is meaningful
*and* cheap — the signal layer already materialises exactly this set, so the row
type is a `LEFT JOIN` away from it and no new intake is required. U2 inflates the
table with millions of NPIs about whom nothing was ever computed, which is
"never enumerated" wearing an `insufficient_evidence` label. U3 is the most
scientifically useful denominator but is a different table's job — the cohort
gate already knows who is in scope.

**U1 is a floor, not the answer to "can this engine classify any physician."**
A physician with zero signals is outside U1 entirely and still gets no row. That
limitation must be stated wherever reach is published, or 37.3% will be read as
more reassuring than it is.

---

## 2. Semantic distinctions

The six states are not one dimension. They are **three independent axes**, and
conflating them is what produced the current ambiguity.

| state | axis | meaning | `is_retired_final` | in U1? |
|---|---|---|---|---|
| `retired` | outcome | Determined: evidence supports cessation | `TRUE` | yes |
| `active` | outcome | Determined: evidence of continued practice | `FALSE` | yes |
| `insufficient_evidence` | **determinability** | Evaluated; evidence below the floor | **`NULL`** | yes |
| `out_of_scope` | **eligibility** | Real, identified, deliberately not in scope | `NULL` | maybe |
| `identity_unresolved` | **identity** | Cannot confirm which human this NPI is | `NULL` | maybe |
| `deceased` | **competing terminal event** | Died; retirement may never have occurred | `FALSE` or `NULL` | yes |

Four points that matter:

- **`active` is a positive finding, not the absence of retirement.** Today
  `is_retired_final = FALSE` (4,543 rows, 0.62%) comes almost entirely from
  `ACTIVE_OVERRIDE` — a real signal. The proposal must not let
  `insufficient_evidence` decay into `FALSE`, or that meaning is lost.
- **`deceased` must stay separate from `retired`.** Death is a competing
  terminal event. Holdout case HO-003 (practising within weeks of death) exists
  to test exactly this; calling it retired is a false positive.
- **`out_of_scope` and `insufficient_evidence` are orthogonal.** A
  dermatologist with two exit signals is out of scope *and* determinable. Only
  the eligibility axis should exclude them.
- **`identity_unresolved` is upstream of all of it.** Holdout cases HO-020 and
  HO-021 (absent from the live registry) are unresolvable before any evidence
  question arises. Three of my 21 cases are in this state, and it is not the
  same failure as thin evidence.

---

## 3. Would `insufficient_evidence` mean "evaluated but below the floor"?

**Yes — and that is the entire content of the proposal.**

> `insufficient_evidence` = *this NPI entered the candidate universe, the evidence
> was assembled and scored, and the result did not clear the evidence floor.*

It is a **positive statement about a completed evaluation**, not the absence of
a record. That is what distinguishes it from today's silence, which is
indistinguishable from never-enumerated, filtered-upstream, or out-of-scope.

The practical test of whether the distinction is real: **`insufficient_evidence`
rows must carry the counts that caused them** (`source_count`,
`year_source_count`, `evidence_family_count`) and a derived
`insufficient_reason`. A row saying only "insufficient" with no numbers is
silence with extra steps.

---

## 4-5. Consumer impact

**99 files reference `retirement_consensus`.** Of those, 14 join to it and a
further handful use presence or counts as implicit logic. The impact classes:

| class | what it does now | under the proposal | risk |
|---|---|---|---|
| **A. `LEFT JOIN` + `IS NULL` means never-retired** | absent NPI → `NULL` → treated active | present row, `retirement_year_final` still `NULL` → **still treated active** | **none — behaviour identical** |
| **B. `INNER JOIN`** | absent NPI silently dropped from the result | absent NPI now **joins**, with `NULL` outcome | **HIGH — row counts and denominators grow** |
| **C. presence / `NOT IN` / `anti_join`** | absence is the signal | absence disappears; predicate must become `determination_status` | **HIGHEST — silently inverts, no error** |
| **D. `COUNT(*)` on the table** | 728,536 | ~1,952,043 (**+168%**) | **HIGH — freshness and QA thresholds break** |
| **E. reads `source_count` / quality** | operates on gated rows only | now sees ungated rows too | **MEDIUM** |

### Named consumers

| consumer | class | current behaviour | change if every candidate emits a row |
|---|---|---|---|
| `R/rebuild_active_obgyn_by_year.R:163-168` | **A** | `LEFT JOIN` + `rc.retirement_year_final IS NULL OR > Y` — header line 12 states `IS NULL` means never retired | **No behavioural change.** `NULL` year still reads as active. **But this is where the 1.2M silent actives are being minted today.** |
| `R/create_active_obgyn_by_year.R:179` (DEPRECATED) | **A** | same pattern; header line 50: *"retirement_year_consensus IS NULL (never retired)"* | No change. Deprecated; confirm it is genuinely dead before ignoring. |
| `R/strategy_09_retired_filter.R` | **B** | `INNER JOIN` to select retirees | Would now admit `NULL`-outcome rows unless it also filters `is_retired_final`. **Must be audited.** |
| `R/create_physicians_active_by_year_table_ALL_YEARS.R` | **B** | `INNER JOIN` | Denominator grows. **Must be audited.** |
| `R/import_nber_2012_2014.R`, `R/import_nber_2007_2011_and_dec_2024.R` | **B** | `INNER JOIN` | Import row counts change. |
| `scripts/retirement_sources/15_validate_with_confidence_thresholds.R:371` | **C** | `WHERE npi_char NOT IN (SELECT npi FROM retirement_consensus)` | **Silently returns ~0 rows.** The set it computes becomes empty because every NPI is now present. Fails without erroring. |
| `R/check_retirement_table_freshness.R:173-176` | **D** | `COUNT(*)` differenced against another table | Freshness delta jumps by ~1.22M. Threshold logic breaks. |
| `R/create_clean_2013_2023_cohort.R:275` | **D** | `COUNT(*)` sanity check | Any hardcoded expected magnitude breaks. |
| `R/integrate_retirement_into_nber.R:123` | **D** | `COUNT(*)` | Same. |
| `R/validation_tests/04_sensitivity_analysis.R:245` | **E** | denominator `COUNT(*) … WHERE source_count = 1` | **Already latently broken today.** It is computing a percentage over a set the gate was believed to make empty — and which in fact holds exactly **321** rows (see §0). Under the proposal it becomes ~1.22M. Either way this line needs review; it is the clearest evidence that nobody has been reading `source_count` semantics carefully. |
| `scripts/qa/retirement_shadow_harness/lib_classify.R` | **B** | `INNER JOIN` | Shadow-harness comparisons shift. |

**The dangerous class is C**, and there is at least one live instance. Class A —
the most common pattern — is safe, which is the counter-intuitive and reassuring
part: the consumers that look most exposed are the ones that do not change.

---

## 6. Would counts, denominators, or rates change even if classification did not?

**Yes. Materially — and this is the strongest argument for a phased migration.**

Classification is untouched: every NPI retired today stays retired, same year,
same confidence. But:

- **Table cardinality** 728,536 → ~1,952,043 (**+168%**).
- **`INNER JOIN` denominators grow** wherever the consensus is the joining table
  (class B). Any rate computed as *retired ÷ consensus rows* would fall by
  roughly a factor of 2.7 **with no change to any individual determination.**
- **Retirement rates by year could shift** if a denominator anywhere is "NPIs in
  the consensus" rather than "NPIs in the cohort". Whether they *do* depends on
  which denominator each rate uses — **not established here, and it must be
  before adoption.**
- **Presence-based sets collapse to empty** (class C).
- **Freshness/QA deltas break** (class D).

The distinction to hold onto: **the estimand does not change; measured
quantities computed over the table do.** A reader seeing a retirement rate move
by 2.7× would reasonably conclude the science changed. It would not have. That
is a communication hazard as much as a technical one.

---

## 7. Migration and backward-compatibility contract

Five conditions. All five, or do not ship.

1. **`credentials.retirement_consensus_determined`** — a view of
   `determination_status = 'determined'`, **byte-identical in row count and
   content to today's table**. Every existing consumer repoints here first, in a
   separate commit that changes no behaviour and can be verified by a row-count
   and checksum equality test against `b7403fe8`.
2. **Only then** does the base table gain the extra rows. Two commits, never one.
3. **`config/canonical_sources.yml`** must name which of the two is canonical for
   which purpose, with the trap path `forbid`-listed. Two names for one concept
   is exactly the footgun non-negotiable #19 exists for.
4. **Every class B and C consumer audited and migrated before step 2.** Class C
   especially: those fail silently, so a passing test suite is not evidence.
5. **Reach published in the manifest** — numerator, denominator, and the
   universe definition (U1/U2/U3). The current manifest records 728,536 and
   723,993 with no denominator; that omission is what let 37.3% reach go
   unnoticed.

**Not backward compatible under any option:** anything keying on row *presence*.
No view fixes class C, because the whole point is that the row now exists.

---

## 8. Proposed fixtures

Specifications, not implementations. Each states inputs, expected output, and
**what must fail** — a fixture that only asserts the happy path proves nothing.

### F1 — weak evidence → `insufficient_evidence`, never retired

*Input:* one synthetic NPI, exactly one signal row (`cessation`, `2yr_gap`, 2019).
*Expect:* a row exists; `determination_status = 'insufficient_evidence'`;
`is_retired_final IS NULL`; `insufficient_reason` contains `single_source`;
`source_count = 1` surfaced.
*Must fail if:* no row is emitted (today's behaviour), **or** `is_retired_final
= FALSE`. **The `FALSE` assertion is the one that matters** — it is the specific
regression that would silently recreate the current bug while appearing to fix
it.
*Negative control:* add a second independent-family signal → the same NPI must
flip to `determined`. If it does not, the fixture is testing a constant.

### F2 — strong active contradiction → active, not retired

*Input:* NPI with two exit signals **and** a current-activity signal of the kind
that produces `ACTIVE_OVERRIDE`.
*Expect:* `determination_status = 'determined'`; `is_retired_final = FALSE`;
`consensus_quality_final = 'ACTIVE_OVERRIDE'`.
*Must fail if:* `TRUE`, or `NULL`. Active is a **positive finding**; degrading it
to `insufficient_evidence` would be a real loss, since `ACTIVE_OVERRIDE`
produced every correct negative in Phase 4.
*Real-world anchor:* HO-013 Gravett — Professor Emeritus with a live appointment
line, correctly not retired at confidence 0.885.

### F3 — strong retirement evidence → retired, with year

*Input:* NPI with ≥2 signals from ≥2 evidence families, ≥1 dated.
*Expect:* `determined`; `is_retired_final = TRUE`; `retirement_year_final`
non-null and equal to the adjudicated year.
*Must fail if:* the year moves relative to `b7403fe8` for any NPI determined
today. **This is the regression gate for the whole migration**: run it across all
728,536 currently-determined NPIs and require exact equality.

### F4 — out-of-scope clinician → no inappropriate admission

*Input:* an NPI with ≥2 exit signals whose taxonomy is outside OB/GYN.
*Expect:* either no row (if eligibility precedes the row type) or
`determination_status = 'out_of_scope'` — **never** `retired`, and never counted
in a retirement rate.
*Must fail if:* the NPI appears with a retirement determination.
*Note:* this fixture exposes an unresolved design question rather than settling
it — §1's choice between U1, U2 and U3 determines which of the two acceptable
outcomes is correct. **It should not be written until that is decided.**

### F5 — reach invariant (proposed addition)

*Input:* the built table.
*Expect:* `COUNT(determination_status = 'determined') = COUNT(*)` of today's
gated set, and reach reported as a ratio.
*Must fail if:* reach moves by more than a declared tolerance **in either
direction**. Reach collapsing to 0% and jumping to 100% are both build defects —
the same both-directions ratchet used for the renv lockfile and the fixture
identity baseline.

---

## Recommendation, unchanged

**Publish reach as a standalone metric first.** It requires no schema change, no
consumer migration, and no risk to class B/C consumers, and it delivers the
finding that actually matters. The row type is one way to institutionalise that
finding, and it is the expensive way.

Before any implementation, §0 must be resolved: **the effective admission rule
and the documented one disagree on 321 rows**, and you cannot correctly emit
rows for the complement of a predicate you have not identified.
