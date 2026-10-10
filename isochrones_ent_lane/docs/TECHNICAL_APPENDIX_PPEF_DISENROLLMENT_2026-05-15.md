# Technical Appendix: PPEF Disenrollment Retirement Signal

**Date:** 2026-05-15
**Status:** Historical (Wired into 27-source consensus (commit `a6b300abd`))
**Do not use for operations:** the state this appendix records is dated; see the register for current records.
**Audit context:** Orphan-signal audit — 4 retirement-signal tables that already
had extractors but had never been wired into the `15_create_weighted_consensus_
24source.R` UNION.

---

## Table

```text
table:      credentials.retirement_signal_ppef_disenrollment
rows:       613,662 NPIs
weight:     0.06  (moderate)
confidence: 0.77  (relatively high — administrative federal filing)
year range: 2017–2024
signal:     physician's last PPEF "enrolled" year before becoming "disenrolled"
```

## Why 613K rows is much larger than expected

PPEF covers all Medicare-enrolled physicians in the U.S., not just OB-GYN. That's why the row count is roughly two orders of magnitude larger than our 7,616-NPI OB-GYN cohort. The `safe_left_join` to the cohort filters it down — only the OB-GYN NPIs that appear in PPEF disenrollment records contribute to the consensus.

## Why it was the highest-value find in the orphan-signal audit

Three reasons PPEF is a better retirement indicator than several signals already in the consensus:

1. **Active provider action.** Unlike Part B billing cessation (which could just mean a quiet practice quarter), disenrollment is a deliberate federal filing — much harder to fake.
2. **Independent of clinical-volume noise.** A physician can keep an active NPI but voluntarily disenroll from Medicare; PPEF catches them when Medicare-claim-gap signals miss them.
3. **Covers revalidation failures.** Many older physicians who stop billing eventually fail revalidation and PPEF surfaces them years before the Medicare-gap detector reaches its 5-year threshold.

## How it fits in the consensus

PPEF is one of the 4 newly-wired signals (along with `goba_retired`, `medicare_lifecycle`, `nppes_temporal`). At weight 0.06 / confidence 0.77, a PPEF signal alone won't fire retirement detection (`source_count >= 2` is required), but it cheaply lifts dual-source NPIs that previously had only weak single-source evidence over the threshold.

## What PPEF does NOT tell you

- **Reason for disenrollment** — CMS doesn't publish whether disenrollment was voluntary, for-cause, revalidation-failure, retirement, or death.
- **Return to practice** — A disenrolled provider can re-enroll later (the `reactivation` signal handles this).
- **Non-Medicare practice** — A physician seeing only commercial-insurance or self-pay patients can disenroll from Medicare while remaining clinically active. This is why PPEF is weighted at 0.06 rather than higher — it's directional, not definitive.

---

## Producer

`scripts/retirement_sources/06c_extract_ppef_disenrollment.R`

## Consumer

`scripts/retirement_sources/15_create_weighted_consensus_24source.R` — added to the UNION on 2026-05-15.

## Tests

`tests/testthat/test-consensus-4-new-signals-2026-05-15.R` — proves the 4-source UNION produces sane row/source counts, `source_count >= 2` filtering still works after the additions, and the weighted-year math is exact to 1e-6.

## Related appendices

- `docs/TECHNICAL_APPENDIX_NPPES_DT_LOST_SIGNAL_2026-05-13.md` — sibling appendix from the same retirement-pipeline audit (NPPES death-signal redaction).
- `docs/TECHNICAL_APPENDIX_FROZEN_ACS_DENOMINATOR_2026-05-13.md` — parent audit document.
