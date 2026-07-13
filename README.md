# grace-ent

A mystery caller study examining access to care for ENT (Ear, Nose, and Throat) specialists.

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
├── scripts/                    # Power analysis scripts
├── config/                     # Subspecialty configuration YAML files
├── docs/                       # Vignettes and documentation
└── call log data pull 7-6-26 for muffly.xlsb.xlsx  # Current call log (n=960)
```
