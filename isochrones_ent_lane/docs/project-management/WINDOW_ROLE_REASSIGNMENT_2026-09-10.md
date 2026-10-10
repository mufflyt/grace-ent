# Window role reassignment (2026-09-10)

This handoff was promoted from `scratchpad/WINDOW_ROLE_REASSIGNMENT_2026-09-10.md`
so the coordination record is not lost when scratchpad is cleaned.

Codex hit its usage cap and is off until 10:05 AM. Paste the relevant block into
any window that did not receive the cross-session message.

Overriding rule for everyone: do not start over, and do not create a competing
implementation. Codex stopped at a good point; the dangerous move is a fresh
branch.

## Right: Slice 1B implementation owner

Repo: `/Users/tylermuffly/isochrones`

Codex worktree: `/Users/tylermuffly/provider-evidence-registry-worktree`

Branch: `slice/provider-evidence-registry`

Take over exactly where Codex stopped. First: cd to that worktree, inspect git
status, inspect the entire uncommitted diff, inspect the last 5 commits, and
preserve all Codex WIP unless you can demonstrate a defect. If Codex left a
broken edit, repair it rather than discarding it; before changing its design,
show the exact defect.

Codex had already classified all 21 original semantic mismatches, written
`artifacts/provider_evidence_crosswalk_resolution.csv` and
`artifacts/provider_evidence_semantic_conflicts.csv`, begun adding only
genuinely missing semantic rows plus the normalized state-board evidence layer,
fixed a deterministic-order issue in the registry renderer, and regenerated
`docs/RETIREMENT_SOURCE_EVIDENCE_REGISTRY.md` successfully.

Finish Slice 1B only:

- Canonical-key reconciliation.
- Genuinely missing semantic registry rows.
- Normalized `state_board_evidence` layer.
- Valid handling of `retired_limited_practice`.
- Superseded Florida, Texas, and Washington legacy producer classification.
- Emitted `signal_source`, `signal_type`, and `evidence_class` discovery.
- Governance status JSON.
- Targeted tests.
- Final provider-evidence governance gate.

Central rule: do not change retirement inference merely to make governance
green. If governed semantics conflict with legacy inference, record the conflict
and defer the scientific behavior change.

Targeted tests only. No full regression, no mutation campaign. Commit coherent
checkpoints as soon as they are green.

Stop when all of these are true:

- Unregistered production provider-evidence sources = 0.
- Undeclared live producers = 0.
- Multiple live producers = 0.
- Admitted semantic mappings missing = 0.
- Semantic/direction mismatches = 0.
- Unregistered emitted labels = 0.
- Governance gate = PASS.

Do not start Slice 2.

## 3rd: CI/infrastructure owner only

Do not modify `slice/provider-evidence-registry`. Do not make
provider-evidence edits to fix CI. Finish only already-nearly-complete infra
work. When Right pushes a Slice 1B SHA, verify that exact SHA, trigger and
monitor applicable CI, and distinguish code failures from runner failures.

## 4th: read-only production-data retirement work only

Determine the actual provenance of ABMS retirement years; establish
gold-standard/comparator readiness; inspect production `retirement_consensus`
data needed for the later differential. Do not edit Slice 1B files, do not
change policy. Send facts Right needs as "HANDOFF TO RIGHT:" messages.

## Codex

When its budget resets, do not auto-restart it. If Right is nearly done, let
Right finish; save Codex for Slice 2, canonical evidence observation and ledger
code.

## Left

Nickname frozen. Reconciled audit at `mysterynpi` `8f24d66` on
`slice/nickname-audit-union`. Not touching provider-evidence files.
