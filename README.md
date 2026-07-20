# ENT Physician Registry Validity Study

[![tests](https://github.com/mufflyt/grace-ent/actions/workflows/tests.yml/badge.svg)](https://github.com/mufflyt/grace-ent/actions/workflows/tests.yml)

**Full title:** Validity of National Otolaryngology Workforce Listings: A Stratified Mystery-Caller Study  
**Target journal:** Otolaryngology–Head and Neck Surgery  
**IRB:** ODU non-human-subjects research (protocol 24-12-NH-0285, approved December 2024; updated March 2026)

---

## What this study does

Three national databases enumerate U.S. otolaryngologists but count different numbers of physicians
and have never been validated by direct telephone contact. This study cross-links the three databases
by NPI, then calls a stratified random sample of listed physicians to test whether the named physician
still works at the listed address.

**Central question:** Among NPPES-listed otolaryngologists, what proportion can be confirmed active
at their listed practice address, and does that confirmation rate vary by corroboration across ABOto
and ENTHealth?

**Primary outcome:** Physician confirmed present at the NPPES-listed practice location (location-valid listing).  
**Secondary outcome:** Physician confirmed active in otolaryngology anywhere (workforce-valid listing).

---

## The three registries and four strata

| Registry | What it is | Who's in it |
|---|---|---|
| **NPPES** (207Y*) | National Plan and Provider Enumeration System | Mandatory for Medicare billing; any ENT taxonomy code qualifies |
| **ABOto** | American Board of Otolaryngology active certification | Requires ACGME residency + written/oral exams; excludes osteopathic pathway |
| **ENTHealth** | AAO-HNS member directory | Voluntary membership; self-reported |

Cross-linking all three by NPI produces four mutually exclusive strata (the **Euler zones**):

| Stratum | N | Called? |
|---|---|---|
| NPPES only | 2,756 | Pending (supplemental sample) |
| NPPES + ABOto | 6,748 | Yes — 498 called |
| NPPES + ENTHealth | 711 | Not sampled |
| Triple-match (all three) | 4,573 | Yes — 462 called |

The Euler zone file lives at:  
`~/isochrones/publication_materials/figures/ent_data_integrity/ent_euler_zone_npi_assignments_with_demographics.csv`

---

## Call outcomes (current data: n = 960)

Each called listing is assigned to one of four mutually exclusive categories:

| Category | Meaning | n |
|---|---|---|
| `confirmed_valid` | Office reached; staff confirmed physician at this location | 445 |
| `confirmed_invalid` | Physician relocated, retired, or phone invalid/disconnected | 138 |
| `unresolved` | Call protocol completed; physician-at-location status unclear | 169 |
| `not_attempted` | No call disposition recorded | 208 |

Validity outcomes are derived from the `taking_new_patients` REDCap field — see
`scripts/05_database_accuracy_paper.R` Section 3 for the mapping table.

---

## Repository structure

```
grace_ent/
│
├── data/
│   ├── raw/
│   │   ├── ruca_2020_zip_crosswalk_usda.csv   # USDA zip-to-RUCA crosswalk
│   │   └── board_cert_ent_universe_*.csv       # ABOto certification file
│   └── processed/
│       ├── ent_phase2_all_<timestamp>.csv      # All 960 call records (produced by 01)
│       ├── ent_phase2_complete_<timestamp>.csv # Complete records only
│       └── nppes_only_ent_ruca_<timestamp>.csv # NPPES-only cohort + RUCA (from 04)
│
├── scripts/                                    # Run in numbered order
│   ├── 01_clean_phase2_call_log.R              # Import & clean raw REDCap export → ent_phase2_all
│   ├── 02_redcap_correction_report.R           # QC report: flags data-entry errors
│   ├── 03_analysis.R                           # Access-to-care paper (wait times, acceptance rates)
│   ├── 04_nppes_only_ruca.R                    # Build NPPES-only sampling frame with RUCA codes
│   ├── 05_database_accuracy_paper.R            # Registry validity paper (primary analysis)
│   └── 06_fig2_flow_diagram.R                  # CONSORT flow diagram for Figure 2
│
├── manuscript/
│   ├── database_accuracy_manuscript.Rmd        # Main manuscript (knits to Word)
│   ├── references.bib                          # BibTeX references
│   └── vancouver.csl                           # Vancouver numbered citation style
│
├── output/
│   ├── figures/                                # PNG figures at 300 DPI
│   └── tables/                                 # CSV and Excel tables
│
└── call log data pull 7-6-26 for muffly.xlsb.xlsx   # Raw REDCap export (input to 01)
```

---

## How to reproduce the analysis

Run scripts in order from the project root (set by `here::here()`):

```r
# 1. Clean raw call log → data/processed/ent_phase2_all_*.csv
source("scripts/01_clean_phase2_call_log.R")

# 2. (Optional) QC report
source("scripts/02_redcap_correction_report.R")

# 3. Build NPPES-only sampling frame (used once calls are added)
source("scripts/04_nppes_only_ruca.R")

# 4. Registry validity analysis → output/tables/ and output/figures/
source("scripts/05_database_accuracy_paper.R")

# 5. Render flow diagram (Figure 2)
source("scripts/06_fig2_flow_diagram.R")

# 6. Knit manuscript to Word
rmarkdown::render("manuscript/database_accuracy_manuscript.Rmd",
                  output_format = "word_document")
```

The Euler zone file is produced by a separate project (`~/isochrones/`) and is read
directly from that path. It does not need to be regenerated for this analysis.

---

## Key variable reference

| Variable | Source | Meaning |
|---|---|---|
| `npi` | NPPES | 10-digit National Provider Identifier; join key across all files |
| `overlap_zone` | Derived | NPPES+ABOto or NPPES+ABOto+ENTHealth (maps from raw `datasets` column) |
| `taking_new_patients` | REDCap | Raw caller response; basis for all validity outcome derivations |
| `physician_at_location` | Derived | confirmed_yes / confirmed_no / unknown |
| `physician_active` | Derived | confirmed_yes / confirmed_no / unknown |
| `listing_status` | Derived | confirmed_valid / confirmed_invalid / unresolved / not_attempted |
| `ruca_binary` | USDA 2020 | Rural (RUCA ≥ 7) vs Non-Rural (RUCA 1–6) |
| `bog_region` | Euler file | AAO-HNS Board of Governors region (used as geographic covariate) |
| `billed_any_part_b` | Euler file | TRUE if physician billed Medicare Part B in 2022–2023 |
| `years_since_enum` | Euler file | Years since NPI was first enumerated in NPPES |
| `weight` | Derived | Inverse-probability sampling weight (population N / sampled n per stratum) |

---

## Team

| Name | Role | Institution |
|---|---|---|
| Grace E. Falk, PhD | PI, data acquisition | Old Dominion University |
| Tyler M. Muffly, MD | Co-I, statistical analysis | Denver Health / CU School of Medicine |
| Cristina Cabrera-Muffly, MD | Co-I | University of Colorado Anschutz |
| Eric Dobratz, MD | Co-I | Eastern Virginia Medical School |
| Cristina Baldassari, MD | Co-I | Eastern Virginia Medical School |
| Andrew Tompkins, MD, MBA | Co-I | Ohio ENT & Allergy Physicians |
