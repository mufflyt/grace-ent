# Design proposal: an explicit `insufficient_evidence` row type for the retirement consensus

> ## FROZEN AS FUTURE WORK — 2026-09-05
>
> **Decision: do not implement.** This proposal and its five failure tests are
> frozen as future work by owner ruling. No schema change, no builder change, no
> consumer migration is authorised on the basis of this document.
>
> **Fixture F4 (out-of-scope clinician) is BLOCKED** pending a PI definition of
> the candidate universe. F4 encodes an answer to the U1/U2/U3 question rather
> than testing one, so writing it before that definition exists would silently
> settle a scientific choice in a test file. It must not be written first.
>
> F1, F2, F3 and F5 remain valid specifications and are unblocked whenever the
> proposal is revisited.

**Status:** **PROPOSAL — NOT IMPLEMENTED.** No code, schema, or artifact was changed.
**Date:** 2026-09-05
**Motivating evidence:** [`RETIREMENT_CONSENSUS_REACH_DIAGNOSIS_2026-09-05.md`](RETIREMENT_CONSENSUS_REACH_DIAGNOSIS_2026-09-05.md)
**Applies to:** the builder `scripts/retirement_sources/16_create_consensus_dynamic.R`, not to the frozen artifact `b7403fe8`, which stays immutable.

## The problem in one sentence

A physician who fails the corroboration gate produces **no row at all**, so the consensus cannot distinguish *"we looked and the evidence was too thin"* from *"we never heard of this person"* — and a `LEFT JOIN` silently converts both into *"not retired."*

### Measured scale

| | |
|---|---|
| NPIs with exactly 1 retirement signal | **1,223,507** |
| …that reach the consensus | **0 (0.000%)** |
| NPIs with ≥2 signals | 728,536 |
| …that reach the consensus | 728,536 (100.000%) |
| **Consensus reach over signal-bearing NPIs** | **37.3%** |

The gate is `16_create_consensus_dynamic.R:593-595`:

```sql
WHERE source_count >= 2
  AND year_source_count >= 1
  AND evidence_family_count >= 2
```

## What this proposal is not

**It does not change the estimand, the threshold, or what counts as a retirement.**

The `>= 2` corroboration requirement is a scientific choice about how much evidence a retirement claim needs, and the code comment defends it well: *"Two status-only sources cannot manufacture a retirement year."* That judgement is sound and is **not** what this proposal touches.

The change is to the **failure mode**, not the decision rule. Every NPI that is retired today stays retired, with the same year and the same confidence. Nothing that currently passes the gate changes value.

## The proposal

Replace the filtering `WHERE` with a **classifying `CASE`**: emit a row for every candidate NPI, and record *why* a determination could not be made.

### Schema addition

| column | type | meaning |
|---|---|---|
| `determination_status` | `VARCHAR` | `determined` \| `insufficient_evidence` |
| `insufficient_reason` | `VARCHAR` | which gate condition failed; `NULL` when determined |
| `source_count` | `BIGINT` | already computed internally; surfaced |
| `year_source_count` | `BIGINT` | already computed internally; surfaced |
| `evidence_family_count` | `BIGINT` | already computed internally; surfaced |

For `insufficient_evidence` rows, `is_retired_final` is **`NULL`, never `FALSE`**. That distinction is the entire point: `NULL` means *no determination*, `FALSE` means *determined to be active*. Collapsing them is the bug being fixed.

`insufficient_reason` would take values such as `single_source`, `no_dated_source`, `single_evidence_family`, or a combination — derived from the same three conditions, so the reason is generated rather than hand-written.

### Sketch, illustrative only

```sql
SELECT *,
  CASE WHEN source_count >= 2
        AND year_source_count >= 1
        AND evidence_family_count >= 2
       THEN 'determined'
       ELSE 'insufficient_evidence' END        AS determination_status,
  CASE WHEN <gate passes> THEN NULL
       ELSE CONCAT_WS('+',
         IF(source_count < 2,          'single_source',        NULL),
         IF(year_source_count < 1,     'no_dated_source',      NULL),
         IF(evidence_family_count < 2, 'single_evidence_family', NULL))
  END                                          AS insufficient_reason
FROM weighted_calc
-- no WHERE: the gate now labels rather than deletes
```

`is_retired_final` and `retirement_year_final` would be forced `NULL` wherever `determination_status <> 'determined'`.

## What it buys

1. **Reach becomes measurable, and provenance can record it.** Today the manifest records 728,536 rows and 723,993 retired — the numerator with no denominator. With this change, reach is a one-line query and belongs in the manifest beside the counts.
2. **Absence stops being ambiguous.** *Never enumerated*, *out of scope*, and *uncorroborated* become three distinguishable states rather than one silence.
3. **The `LEFT JOIN` hazard becomes visible.** A consumer that treats `NULL` as active is now doing so against a row that explicitly says `insufficient_evidence`, which a guard can assert on. Today there is nothing to assert on — non-negotiable #20's exact pattern: the pipeline computes `source_count = 1` and nothing ever checks it.
4. **Validation gets an unresolved rate the engine reports itself**, instead of one reconstructed after the fact by noticing missing rows, which is how the 44.4% in Phase 4 was found.

## Blast radius — the reason this needs a decision, not a patch

**172 files reference `retirement_consensus`. 14 of them `LEFT JOIN` onto it.**

Row count would rise from 728,536 to roughly **1,952,043** (+168%). Every one of those 14 join sites must be audited before this ships, because each currently receives `NULL` for an absent NPI and would afterwards receive a **present row with `NULL` outcome fields**. Consumers that test `is.na(is_retired_final)` behave the same; consumers that test row presence (`!is.na(npi)` after a join, `anti_join`, `NOT EXISTS`) **would silently change meaning** — and that is the dangerous class, because it fails without erroring.

A `determined`-only view (`credentials.retirement_consensus_determined`) preserving today's exact semantics is the obvious migration path: existing consumers repoint at the view and observe no change, new consumers use the full table. That keeps the change additive at every call site, at the cost of two names for one concept.

## Risks

- **Volume.** 1.95M rows in a staging DuckDB is fine; downstream joins in the pipeline may not be. Needs a timing check before adoption.
- **Two names for one table** invites the wrong one being used, which is the failure mode CLAUDE.md #19 is about. It would need a `config/canonical_sources.yml` entry naming which is canonical for which purpose.
- **A visible `insufficient_evidence` count invites threshold-tuning to shrink it.** That would be optimising the wrong quantity: the number exists to be *reported*, not minimised. Worth stating in the contract.

## Open decisions — owner, not implementer

1. **Ship it at all?** The alternative is leaving the builder alone and publishing reach as a separate metric computed against `retirement_signals_unified`. That is far cheaper and gets benefit (1) and (4) without touching 14 join sites. **This is the cheaper option and I would want it ruled out explicitly before spending the larger change.**
2. **Full table plus `determined` view, or a new column on the existing table?**
3. **Should candidates with *zero* signals also get a row?** That would extend the denominator from the signal-bearing universe to the whole provider universe and change reach's meaning again. My inclination is no — zero-signal NPIs are a different population — but it is a real choice about what reach means.
4. **Does `insufficient_evidence` belong in the pinned 14-label set?** It is not a retirement label, so probably not; but the set-equality intake gate would need to say so explicitly rather than by omission.

## If it is adopted, it needs these guards

Stated now so they are not retrofitted:

- **A negative control**: a fixture NPI with exactly one signal must produce a row with `determination_status = 'insufficient_evidence'` and `is_retired_final IS NULL` — and the test must fail if that row is absent *or* if `is_retired_final` is `FALSE`.
- **An invariant**: `determination_status = 'determined'` ⟺ the three gate conditions hold. Discovered equals declared, both directions.
- **An expected-distribution assertion** on reach, per the standing recommendation: fail loudly if reach moves by an implausible amount between builds, in either direction. Reach collapsing to 0% and reach jumping to 100% are both build defects.
- **A field-reachability entry** (non-negotiable #21b): `determination_status` must be read by something, or declared inert with a rationale.

## Recommendation

Adopt **option 1 first** — publish reach as a standalone metric — and treat the row-type change as a follow-on justified only if consumers actually need per-NPI unresolved status. The reach number is the finding; the schema change is one way of institutionalising it, and it is the expensive way.

**Nothing here is implemented, and `b7403fe8` remains frozen and unmodified.**
