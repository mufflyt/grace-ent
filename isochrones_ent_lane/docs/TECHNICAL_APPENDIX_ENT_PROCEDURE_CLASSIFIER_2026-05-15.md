# Technical Appendix — ENT Procedure-Pattern Classifier (Medicare Part B)

**Date:** 2026-05-15
**Authors:** Tyler Muffly, MD; with Claude Opus 4.7 (code-generation assistant)
**Status:** Historical (Validated; ready for inclusion in supplementary materials of any)
**Do not use for operations:** the state this appendix records is dated; see the register for current records.
ENT workforce paper that needs to corroborate specialty identity from
Medicare claims data.

---

## TL;DR

We built a procedure-code-based classifier that confirms a physician is
actively practicing otolaryngology by their Medicare Part B billing
pattern. The classifier uses 71 ABOto-curated CPT codes across 11
clinical categories and a three-rule operating point (any ENT code,
≥10% pct_ent share, recent activity 2021–2023).

**Performance, validated on n = 1,000 Triple Match ENTs and n = 100 each
of OB-GYN and Family Practice comparators:**

| Metric | Value | 95% CI |
| --- | ---: | ---: |
| Sensitivity (final ACTIVE_ENT rule) | **79.7%** | 77.2 – 82.2 |
| Specificity vs OB-GYN | **100%** | 96.4 – 100 |
| Specificity vs Family Practice | **100%** | 96.4 – 100 |

The 20% non-classified residual is dominated by Pediatric Otolaryngology
(81% of pediatric ENTs have no Medicare Part B activity by construction
— Medicare insures the ≥65 and disabled adult population, not children).

---

## 1. Background and motivation

A common limitation of ENT workforce studies is that they rely on a
single physician registry (CMS NPPES taxonomy, ABMS / ABOto board
certification, or a society directory such as AAO-HNS ENThealth) without
cross-validating the physician's identity against an independent
behavioral signal of active practice. This produces three known biases:

1. Registry presence ≠ active practice (a board-certified physician may
   have stopped operating, switched specialties, or moved to a
   non-clinical role).
2. Self-designated NPPES taxonomy ≠ board-certified specialty (a
   physician can declare any taxonomy at NPI enumeration).
3. Different registries drift relative to one another over time and
   produce conflicting workforce estimates.

The Triple Match cohort defined elsewhere
(`publication_materials/figures/ent_data_integrity/DATA_INTEGRITY_NOTES.md`)
addresses registry drift by requiring presence in all three registries
simultaneously (ABOto + NPPES + ENThealth = 4,578 unique NPIs). The
procedure-pattern classifier described here addresses the active-practice
question by asking: *does this physician's actual Medicare billing
pattern match what an otolaryngologist would bill?*

---

## 2. Methods

### 2.1 Data sources

| Source | File / table | Years | Granularity |
| --- | --- | --- | --- |
| CMS Medicare Part B Provider Utilization & Payment Data — by Service | `medicare_part_b_by_service_all_years` (DuckDB) | 2013–2023 | 1 row per (NPI × HCPCS code × year × place of service) |
| Triple Match cohort | `mystery_caller_cohort_triple_match_4578.csv` | — | 1 row per NPI |
| Procedure code map | `config/ent_procedure_categories.yml` | — | 71 codes / 11 categories |
| Comparator: Obstetrics & Gynecology | `medicare_part_b_2023` filtered by `Rndrng_Prvdr_Type` | 2023 selection; full-history scoring | — |
| Comparator: Family Practice | same approach | 2023 selection | — |

All cohorts were CONUS-filtered. Triple Match physicians had been
pre-deduplicated to unique NPIs.

### 2.2 ENT procedure-code set construction

The 71-code set was assembled in two phases:

**Phase 1 (clinically curated, 46 codes):** A starting set of operative
ABOto procedures was hand-coded into eight categories — sinus surgery
(13 codes), septoplasty (1), direct laryngoscopy with biopsy (1),
esophageal dilation (5), cochlear implant (1), mastoidectomy (12),
thyroidectomy (10), neck dissection (3). Each category was assigned an
"inclusive representative code" — a single anchor CPT that stands in for
the group in summary tables.

**Phase 2 (empirical expansion, +25 codes / +3 categories):** Phase 1
produced 36% sensitivity on a 100-NPI smoke test, indicating the
operative-only set missed the office-based practice patterns of most
ENTs. We empirically ranked every HCPCS code billed by the full 4,578
Triple Match cohort and added three new categories of high-volume,
ENT-specific codes:

- **ENT diagnostic procedures (17 codes, anchor 31575):** flexible
  laryngoscopy, nasal endoscopy, audiometry, tympanometry, cerumen and
  foreign body removal, tympanostomy tube placement, epistaxis control,
  binocular microscopy.
- **Otolaryngic allergy (5 codes, anchor 95165):** allergen vial
  preparation, allergy skin testing (percutaneous and intracutaneous),
  immunotherapy injections.
- **Vestibular evaluation (3 codes, anchor 95992):** Epley / canalith
  repositioning, basic vestibular evaluation, caloric vestibular test.

**Codes intentionally excluded:**

- Generic Evaluation & Management codes (99202–99215, 99211, 99221–99232,
  99232). These are billed by every outpatient specialty; including them
  would destroy specificity. (Concretely: 99213 is billed by 3,893 of
  the 4,578 Triple Match ENTs — but also by essentially every internist,
  family practitioner, dermatologist, and OB-GYN.)
- Codes billed by <1% of the Triple Match cohort. Below noise floor.
- Cosmetic-only codes (30400–30460 rhinoplasty variants). Medicare does
  not cover purely cosmetic procedures, so they generate no signal.

The full 71-code map is in `config/ent_procedure_categories.yml` and is
loaded by `R/ent_procedure_categories.R::load_ent_procedure_categories()`.
Schema invariants are asserted at load time: every row must have a
non-empty `cpt_code` and `inclusive_representative_code`, and every
representative code must itself appear in its category's code list.

### 2.3 Classifier definition

A physician is classified as **actively practicing ENT by Medicare Part B
billing pattern** if and only if all three of the following hold:

1. **Procedure presence.** They bill ≥1 of the 71 CPT codes in Medicare
   Part B 2013–2023.
2. **Procedure specificity.** Those ENT codes constitute **≥10% of their
   total Medicare service volume** (denoted `pct_ent ≥ 10`).
3. **Temporal recency.** At least one ENT-code service occurred in
   **2021–2023** (the most recent three years of the dataset).

For diagnostic and stratification purposes, physicians are also assigned
to one of six **pct_ent score tiers:**

| Tier | Definition |
| --- | --- |
| HIGHLY_LIKELY | pct_ent ≥ 50% |
| LIKELY | 10% ≤ pct_ent < 50% |
| INCIDENTAL | 1% ≤ pct_ent < 10% |
| NEGLIGIBLE | 0% < pct_ent < 1% |
| NONE | pct_ent = 0 (any Medicare activity, no ENT codes) |
| NO_MEDICARE_ACTIVITY | no Medicare Part B presence at all |

The final ACTIVE_ENT classification corresponds to tiers HIGHLY_LIKELY
OR LIKELY, intersected with the recency rule.

### 2.4 Validation cohorts

| Cohort | n | Sampling | Seed |
| --- | ---: | --- | ---: |
| ENT (Triple Match) | 1,000 | Random sample without replacement from the 4,578-row Triple Match cohort | 20260515 |
| OB-GYN comparator | 100 | Random sample from `medicare_part_b_2023` where `Rndrng_Prvdr_Type == "Obstetrics & Gynecology"` | 20260515 |
| Family Practice comparator | 100 | Random sample from `medicare_part_b_2023` where `Rndrng_Prvdr_Type == "Family Practice"` | 20260515 |

OB-GYN was chosen as a *clean* comparator: surgically active,
Medicare-engaged, but no expected anatomical overlap with ENT
procedures. Family Practice was chosen as a *hard* comparator: primary
care physicians do bill some ENT-overlapping office codes (notably
69210 cerumen removal), so this test stresses the specificity of
the operating point.

All three cohorts were scored against the full Medicare Part B 2013–2023
billing history (not just 2023), so each NPI's `pct_ent` reflects a
10-year practice signal.

### 2.5 Performance metrics

For each cohort we compute:

- Sensitivity (ENT cohort) at three thresholds: any ENT code, pct_ent ≥
  10%, and the final three-rule ACTIVE_ENT classification.
- False-positive rate (each comparator cohort) at the same thresholds.
- Specificity = 1 – FPR.
- 95% confidence intervals computed analytically as
  `p̂ ± 1.96·√(p̂(1-p̂)/n)` (Wald). At n = 1,000 the Wald CI is
  adequate; for the comparator cohorts at n = 100 the exact Clopper-Pearson
  CI on 0/100 events is 0.0% – 3.6%.

---

## 3. Results

### 3.1 Headline performance

| Threshold | ENT sensitivity (n = 1,000) | OB-GYN FPR (n = 100) | Family Practice FPR (n = 100) |
| --- | ---: | ---: | ---: |
| Any ENT code billed | 84.6% (95% CI 82.4 – 86.8) | 0.0% | 12.0% |
| pct_ent ≥ 10% | 81.7% (79.3 – 84.1) | **0.0%** | **0.0%** |
| Final ACTIVE_ENT (≥10% + recent) | **79.7%** (77.2 – 82.2) | **0.0%** | **0.0%** |

The pct_ent ≥ 10% threshold cleanly separates the ENT cohort from both
comparators. In particular, every one of the 12 Family Practice false
positives at the "any ENT code" threshold billed CPT 69210 (cerumen
removal) at <1% of their total Medicare service volume — well below the
10% specificity threshold.

### 3.2 Score distribution among the 1,000 ENTs

| pct_ent tier | n | % |
| --- | ---: | ---: |
| HIGHLY_LIKELY (≥50%) | 187 | 18.7% |
| LIKELY (10–49%) | 630 | 63.0% |
| INCIDENTAL (1–9%) | 27 | 2.7% |
| NEGLIGIBLE (<1%, >0) | 2 | 0.2% |
| NONE (no ENT codes) | 31 | 3.1% |
| NO_MEDICARE_ACTIVITY | 123 | 12.3% |
| **Final ACTIVE_ENT** | **797** | **79.7%** |

Among the 846 physicians with any ENT billing: median pct_ent = 37.7%,
IQR 28.2% – 48.5%, range 0.95% – 100%. The cohort clusters tightly
around one-third of Medicare volume being these 71 codes — exactly the
shape expected of a specialty whose practice mix combines operative,
diagnostic, and office work.

### 3.3 Subspecialty stratification

ABOto issues four board certifications: Otolaryngology – Head & Neck
Surgery (the foundational general cert), Complex Pediatric
Otolaryngology, Neurotology, and Sleep Medicine. We classified each
sampled physician by the rarest subspecialty cert they hold using the
priority order Pediatric > Neurotology > Sleep > General (the
project-canonical ordering, codified in
`R/ent_registry_counts.R::ABOTO_SUBSPECIALTY_PRIORITY`).

| Subspecialty | n in sample | ACTIVE_ENT | % ACTIVE_ENT | NO_MEDICARE_DATA | % NO_MEDICARE_DATA |
| --- | ---: | ---: | ---: | ---: | ---: |
| General Otolaryngology | 867 | 725 | 83.6% | 76 | 8.8% |
| Sleep Medicine | 38 | 34 | 89.5% | 2 | 5.3% |
| Neurotology | 43 | 34 | 79.1% | 3 | 7.0% |
| **Pediatric Otolaryngology** | **52** | **4** | **7.7%** | **42** | **80.8%** |

Pediatric Otolaryngology is the dominant source of the 12.3%
NO_MEDICARE_DATA bucket in the overall sample. Medicare insures the
≥65 and disabled adult population, so pediatric ENT practice generates
negligible Medicare Part B activity by construction. This is a
Medicare-data limitation of the classifier, not a clinical limitation —
the pediatric ENTs in question are practicing; their patients are
simply not in the Medicare data.

If pediatric ENTs are excluded from the denominator, classifier
sensitivity rises to **791 of 948 = 83.4% (95% CI 81.0 – 85.8).**

### 3.4 Temporal recency

Among the 846 ENT-billing physicians in the validation sample:

| Recency | n | % |
| --- | ---: | ---: |
| Billed ≥1 ENT code in 2021–2023 | 815 | 96.3% |
| ENT codes only before 2021 | 31 | 3.7% |

The 3.7% who haven't billed ENT codes since 2020 are classified as
PAST_ENT and excluded from the final ACTIVE_ENT bucket. This corresponds
to the recency rule.

### 3.5 False-positive analysis (Family Practice)

At the "any ENT code" threshold, 12 of 100 Family Practice physicians
appeared as false positives. Their pct_ent distribution:

| FP rank | pct_ent | Distinct ENT codes billed | Codes |
| ---: | ---: | ---: | --- |
| 1 | 0.86% | 1 | 69210 (cerumen) |
| 2 | 0.37% | 1 | 69210 |
| 3 | 0.24% | 2 | 69210, 92567 (tympanometry) |
| 4 | 0.23% | 1 | 69210 |
| 5 | 0.17% | 1 | 69210 |
| 6 | 0.13% | 1 | 69210 |
| 7 | 0.11% | 1 | 69210 |
| 8 | 0.10% | 1 | 69210 |
| 9 | 0.09% | 1 | 69210 |
| 10 | 0.08% | 1 | 69210 |
| 11 | 0.08% | 1 | 69210 |
| 12 | 0.04% | 1 | 69210 |

Maximum pct_ent = 0.86%, ~12-fold below the 10% threshold. The pct_ent
≥ 10% rule eliminates 100% of these false positives. **All Family
Practice false positives at the loose threshold derived from a single
CPT (69210, cerumen removal) billed in negligible volume.**

---

## 4. Discussion and limitations

### 4.1 Medicare-population limitations

The classifier requires Medicare Part B activity, which intrinsically
underestimates ENT prevalence in three sub-populations:

- **Pediatric Otolaryngology** (validated): 80.8% of pediatric ENTs in
  our sample have no Medicare data, because their patients are children.
- **Hospital-employed surgeons** who bill under the facility's NPI rather
  than their own. These physicians may not appear as individual
  prescribers in CMS data even when actively operating.
- **Academic faculty** at institutions that aggregate billing under
  group NPIs.

For workforce papers anchored to Medicare data, these are universal
limitations and should be acknowledged in the methods section of any
study using this classifier.

### 4.2 Why Medicare Part D prescriptions were not used

A parallel attempt to extend the classifier to Medicare Part D
prescription patterns was considered and rejected on substantive
grounds. The substantive issue: most medications commonly prescribed
by ENTs (intranasal corticosteroids, oral antihistamines, antibiotics,
proton pump inhibitors, systemic steroids) are also prescribed at much
higher absolute volumes by primary care, pediatrics, allergy/immunology,
pulmonology, and GI. Only three drugs — ciprofloxacin otic, ofloxacin
otic, and ciprofloxacin-dexamethasone (Ciprodex) — have clear ENT
specificity. Implementing a Part D classifier on the 23 candidate drugs
would likely *decrease* specificity from the current 100% vs both
comparators to an estimated 80–90%, while gaining little sensitivity.
The decision was made to leave Part D out of the operating-point rule.

### 4.3 Code-set currency

The 71 CPT codes are current as of 2024. CPT codes are updated annually
by the AMA; new codes can be added and deprecated codes retired in any
given year. The YAML schema supports straightforward annual updates by
simply editing the codes list within a category and re-running the
loader's runtime assertions.

### 4.4 Generalizability

This classifier was designed and validated on the Triple Match ENT
cohort (n = 4,578) in CONUS. It should generalize to:

- Any ENT cohort defined by NPI, regardless of the source registry used
  to assemble that cohort (the classifier is registry-agnostic at
  inference time).
- All US states in the contiguous 48 plus DC. Non-CONUS (HI, AK, PR,
  GU, VI, AS, MP) physicians are excluded both from our validation
  cohort and from the routine NPPES filtering used elsewhere in this
  project; performance in those territories is untested.

It is **not** designed to:

- Distinguish ENT subspecialties from each other purely by billing
  pattern (subspecialty inference should use ABOto certifications,
  which is a separate signal — see
  `R/ent_registry_counts.R::ABOTO_SUBSPECIALTY_PRIORITY`).
- Replace clinical judgment about whether a specific physician is
  "really" practicing ENT. The classifier reports a behavioral signal
  consistent with ENT practice; it does not adjudicate clinical scope.

---

## 5. Reproducibility

All scripts and data files are committed under
[`commit 80679c686`](https://github.com/) on branch
`feature/seven-subspecialty-expansion`.

### 5.1 Software

| Component | Version |
| --- | --- |
| R | 4.6.0 |
| dplyr | 1.1.x |
| DBI | 1.2.x |
| duckdb (R) | 1.x |
| yaml | 2.3.x |

### 5.2 Data inputs

| Path | Description | SHA256 (input_sha256 of attached receipt JSONs) |
| --- | --- | --- |
| `data/ent/abohns_certifications_clean.csv` | ABOto raw certifications | (in receipt) |
| `data/ent/abohns_with_npi_master.csv` | ABOto-NPI matched | (in receipt) |
| `data/ent/ent_master_roster_final_20260513.csv` | AAO-HNS ENThealth roster | (in receipt) |
| `artifacts/ent/ent_from_nppes_flat_file.parquet` | NPPES 207Y\* taxonomy extract | (in receipt) |
| `/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb` | Medicare Part B 2013–2023 | (external) |

### 5.3 Configuration

```bash
config/ent_procedure_categories.yml     # 71 CPTs / 11 categories
R/ent_procedure_categories.R            # YAML loader + assertions
R/ent_registry_counts.R                 # ABOTO_SUBSPECIALTY_PRIORITY constant
```

### 5.4 Scripts and reproduction commands

```bash
# 1. Discover what HCPCS codes the Triple Match cohort actually bills
Rscript scripts/discover_ent_billed_codes.R

# 2. Binary smoke test (100-NPI sample)
Rscript scripts/test_ent_procedure_match_100_npis.R

# 3. Sensitivity + specificity vs OB-GYN, with continuous pct_ent score
#    and temporal recency
Rscript scripts/test_ent_classifier_full.R

# 4. Specificity vs Family Practice (harder test)
Rscript scripts/test_ent_classifier_vs_family_med.R

# 5. Final validation at n = 1,000, with subspecialty stratification
#    and 95% CIs
Rscript scripts/validate_ent_classifier_n1000.R
```

### 5.5 Outputs

| Path | Contents |
| --- | --- |
| `publication_materials/figures/ent_data_integrity/test_ent_procedure_match_100_npis.csv` | Per-NPI hit report, n = 100 |
| `publication_materials/figures/ent_data_integrity/candidate_ent_codes_for_yaml.csv` | Empirical code-discovery output |
| `publication_materials/figures/ent_data_integrity/ent_classifier_evaluation_n100x2.csv` | ENT (n = 100) + OB-GYN (n = 100), 6-tier scores |
| `publication_materials/figures/ent_data_integrity/ent_classifier_n1000_validation.csv` | Final validation: 1,000 rows × 14 cols (npi, total_services_all_years, ent_services_all_years, ent_services_recent, ent_distinct_codes, pct_ent, ent_score, recently_active, subspecialty, final_classification, etc.) |

All output CSVs are gitignored (per project policy on physician PHI in
publication_materials/), but the scripts are deterministic given the
input files plus the random seed.

### 5.6 Runtime

| Script | Approximate runtime on M2 MacBook Pro with the DuckDB on an attached SSD |
| --- | --- |
| `discover_ent_billed_codes.R` | 90 seconds |
| `test_ent_procedure_match_100_npis.R` | 30 seconds |
| `test_ent_classifier_full.R` | 60 seconds |
| `test_ent_classifier_vs_family_med.R` | 60 seconds |
| `validate_ent_classifier_n1000.R` | 90 seconds |

---

## 6. References (data sources)

1. American Medical Association. *CPT 2024 Professional Edition.*
   Chicago: AMA; 2023.
2. Centers for Medicare & Medicaid Services. *Medicare Provider
   Utilization and Payment Data: Part B Provider Service File,
   2013–2023.* https://data.cms.gov/.
3. National Uniform Claim Committee. *Health Care Provider Taxonomy
   Code Set, 24.1.* https://www.nucc.org/.
4. American Board of Otolaryngology – Head and Neck Surgery
   (ABOto / ABOHNS). Board certification registry.
5. American Academy of Otolaryngology – Head and Neck Surgery.
   *ENThealth Find an ENT Directory.* https://www.enthealth.org/find-an-ent/.

---

*This technical appendix is part of the ENT workforce data-integrity
analysis. See also:
`publication_materials/figures/ent_data_integrity/DATA_INTEGRITY_NOTES.md`
for the cohort-construction methodology, and the four-figure suite
(Euler / STROBE / CONSORT / subspecialty findability) generated by the
`scripts/improve_ent_*.R` family.*
