# grace-ent

[![tests](https://github.com/mufflyt/grace-ent/actions/workflows/tests.yml/badge.svg)](https://github.com/mufflyt/grace-ent/actions/workflows/tests.yml)

A mystery caller study examining access to care for ENT (Ear, Nose, and Throat) specialists.

Run the test suite locally with `Rscript tests/run_tests.R` (or `Rscript run_all.R test`).

## Overview

This project uses a simulated patient (mystery caller) methodology to assess appointment availability, wait times, and other access-to-care metrics for otolaryngology practices.

## Study Design

- **Method:** Mystery caller / secret shopper telephone audit
- **Specialty:** Otolaryngology (ENT)
- **Outcome measures:** Appointment availability, scheduling wait times, insurance acceptance, and patient access barriers

## Team

- **Grace Falk, PhD** (PI, Old Dominion University) — falkge@odu.edu
- **Tyler Muffly, MD** (Co-I, Denver Health) — tyler.muffly@dhha.org
- **Cristina Cabrera-Muffly, MD** (Co-I, University of Colorado Anschutz)
- **Eric Dobratz, MD** (ODU)
- **Cristina Baldassari, MD** (Co-I)
- **Andrew Tompkins, MD, MBA** (Ohio ENT & Allergy Physicians)

## IRB

Approved as **non-human subjects research** by ODU IRB (original approval December 2024; updated March 2026 to add medical students). UC IRB not required.

## Data Sources

| Source | Description |
|--------|-------------|
| **NPPES** | National Plan and Provider Enumeration System — ENT physicians identified by taxonomy code 207Y* |
| **ABOto** | American Board of Otolaryngology active board-certification file — used to verify board-certified ENTs and cross-reference with NPPES |
| **REDCap call log** | Mystery caller study data — 960 records across ENT physicians; tracks appointment availability, wait times, insurance acceptance, ENT subspecialty type, and RUCA rural/urban classification |
| **RUCA codes** | Rural-Urban Commuting Area codes (2020, zip-code level) — used to classify physician practice locations as rural or urban |
| **Healthgrades** | Scraped for physician practice locations, board certifications, education, and specialty details |
| **ent_data_integrity_for_grace_2026-05-18.zip** | Data integrity summary package: 11,333 ENT physicians (NPPES + ABOto overlap), 791 rural physicians, 9 figures, 2 interactive maps — *pending download from email* |

## Geographic Covariate Pipeline

Market- and area-level predictors for modeling **business days until a new-patient
ENT appointment**. Each physician is enriched from their practice `state` and `zip`,
with ZIP resolved to county and CBSA/MSA through a crosswalk backbone. All sources
are public and require no API key.

| Covariate | File (`data/raw/`) | Geo level → join key | Call-log coverage | Source |
|---|---|---|---|---|
| AAO-HNS Board of Governors district (1–8) + region | `state_to_aao_hns_district.csv` | state | 100% | `mufflyt/tyler` |
| Medicaid-to-Medicare Fee Index (All Services, 2024) | `medicaid_fee_index_state.csv` | state | 98.0%¹ | KFF |
| Hospital-market HHI, 387 MSAs | `kff_hhi_msa_2024.xlsx` | MSA (via crosswalk) | 60.7%² | KFF |
| CDC Social Vulnerability Index 2022 (overall + 4 themes) | `zip_svi_2022.csv` | ZIP (pop-weighted from tracts) | 97.8% | CDC/ATSDR |
| ZIP → county FIPS → CBSA backbone | `zip_to_county_cbsa.csv` | ZIP | 100% | Census ZCTA + OMB |
| Board-certified ENT supply per county (+ per 100k) | `county_ent_count.csv` | county FIPS | 99.3% | ABOto/NPPES universe |
| CMS Medicare + dual-eligible Medicaid enrollment (2025) | `county_cms_enrollment.csv` | county FIPS | 99.3% | CMS Medicare Monthly Enrollment |

¹ Tennessee is `NA` — KFF publishes no fee-for-service Medicaid data for TN.
² KFF covers metropolitan MSAs only; the remainder are micropolitan/rural physicians (structural, not a join failure).

### Reproducing

```sh
# 1. Build the crosswalks (populate data/raw/) — downloads public source files
Rscript scripts/build_geo_crosswalk.R      # ZIP → county → CBSA
Rscript scripts/build_zip_svi.R            # population-weighted ZIP-level CDC SVI
Rscript scripts/build_county_ent_count.R   # county ENT supply from the ABOto universe
Rscript scripts/build_cms_enrollment.R     # CMS county Medicare/dual-Medicaid enrollment

# 2. Assemble the model-ready dataset (960 × 52)
Rscript scripts/enrich_call_log.R          # → data/processed/ent_phase2_enriched.csv
```

`enrich_call_log.R` left-joins every covariate in one pass, preserves all original
columns, guards against row multiplication, and prints per-covariate coverage.

**Join note:** the call log stores ZIPs without leading zeros — always zero-pad to
5 digits before joining (`sprintf("%05d", ...)`), or New England/NJ ZIPs silently miss.

**Caveats:** HHI is metropolitan-only (see ²); the CMS "Medicaid" field is
*dual-eligible* beneficiaries (Medicare ∩ Medicaid), the only county-level Medicaid
signal CMS publishes — total county Medicaid coverage would need ACS table C27007
and a Census API key.

## Repository Structure

```
grace_ent/
├── R/                          # R scripts
│   ├── build_ent_locations_v3.R        # Builds ENT provider location database
│   ├── mystery_caller_power_core.R     # Core NB2 GLMM power simulation functions
│   ├── scrape_healthgrades_full.R      # Healthgrades scraper for ENT physicians
│   ├── scrape_healthgrades_locations.R # Companion scraper for practice locations
│   ├── subspecialty_helpers.R          # Subspecialty utility functions
│   └── subspecialty_standardizer.R     # ENT subspecialty classification
├── inst/shiny/
│   ├── ent_rural_urban_power/          # Power calculator (dev): rural vs urban ENT
│   ├── ent_rural_urban_power_deploy/   # Power calculator (shinyapps.io deploy)
│   └── mystery_caller_power/           # General paired-call power calculator
├── scripts/                    # Power analysis + covariate pipeline scripts
│   ├── build_geo_crosswalk.R           # ZIP → county → CBSA backbone
│   ├── build_zip_svi.R                 # population-weighted ZIP-level CDC SVI
│   ├── build_county_ent_count.R        # county ENT supply from ABOto universe
│   ├── build_cms_enrollment.R          # CMS county Medicare/dual-Medicaid enrollment
│   └── enrich_call_log.R               # joins all covariates → model-ready dataset
├── data/
│   ├── raw/                     # Covariate crosswalks + reference data
│   └── processed/              # Cleaned call log + ent_phase2_enriched.csv
├── config/                     # Subspecialty configuration YAML files
├── docs/                       # Vignettes and documentation
└── call log data pull 7-6-26 for muffly.xlsb.xlsx  # Current call log (n=960)
```
