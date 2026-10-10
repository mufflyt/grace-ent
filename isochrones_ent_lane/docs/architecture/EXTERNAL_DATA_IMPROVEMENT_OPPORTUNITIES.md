# External Data Sources to Improve Low-Quality Table 1 Variables

**Created:** 2026-02-02
**Purpose:** Identify external data sources to upgrade PROXY/LOW quality variables to MEDIUM/HIGH
**Target Variables:** Practice Arrangement, Academic Affiliation, Safety-Net Practice

---

## Executive Summary

| Variable | Current Quality | Target Quality | Best Data Source | Expected Improvement |
|----------|----------------|----------------|------------------|---------------------|
| **Practice Arrangement** | PROXY (~60% coverage, low accuracy) | MEDIUM-HIGH | Medicare Part B TIN + CMS GPRO | 30% → 75% accuracy |
| **Academic Affiliation** | PROXY (~50% coverage, partial) | HIGH | AAMC Faculty Roster + NIH RePORTER | 50% → 85% coverage |
| **Safety-Net Practice** | PROXY (ZIP3-level) | MEDIUM-HIGH | HRSA UDS + 340B Database | ZIP3 → Individual physician |

---

## 1. PRACTICE ARRANGEMENT (Employed vs Owner vs Hospital-Employed)

### Current Limitations
- **Method:** Inferred from practice address count (proxy)
- **Coverage:** ~60% with classification
- **Accuracy:** LOW - Cannot distinguish:
  * Solo practice owner vs solo employed physician
  * Hospital-employed vs private practice
  * Corporate/private equity ownership
  * Partnership structures

### 🎯 Recommended External Data Sources (Ranked by Impact)

#### TIER 1: High Impact, Publicly Available

**1.1 Medicare Part B Tax ID (TIN) Analysis** ⭐ **HIGHEST IMPACT**

| Attribute | Details |
|-----------|---------|
| **Data Source** | CMS Physician and Other Practitioners (PUF) - Medicare Part B |
| **Access** | Public (annual release) |
| **URL** | https://data.cms.gov/provider-summary-by-type-of-service |
| **Key Fields** | `NPI`, `Tax_ID` (TIN/EIN), `Organization_Name`, `Number_of_Group_Practice_Members` |
| **Coverage** | ~70% of physicians billing Medicare |
| **Cost** | Free |

**How It Improves Classification:**
```r
# TIN-based practice arrangement algorithm
practice_type <- case_when(
  # Solo practice: Unique TIN with 1 physician
  n_physicians_per_tin == 1 ~ "Solo Practice (likely owner)",

  # Small group: 2-10 physicians under same TIN
  n_physicians_per_tin >= 2 & n_physicians_per_tin <= 10 ~ "Small Group Practice",

  # Large group: 11-99 physicians
  n_physicians_per_tin >= 11 & n_physicians_per_tin <= 99 ~ "Large Group Practice",

  # Very large: 100+ physicians (likely hospital system or corporate)
  n_physicians_per_tin >= 100 ~ "Hospital/Large System",

  TRUE ~ "Unknown"
)
```

**Expected Improvement:**
- Current: ~60% coverage, ~30% accuracy
- With TIN: **~70% coverage, ~65% accuracy** for employed vs independent
- Still cannot distinguish: Owner vs employed in same group

**Implementation:**
```r
# File: R/enrich_practice_arrangement_tin.R
add_practice_arrangement_tin <- function(cohort, medicare_pub_path) {

  # Load Medicare Part B PUF with TIN
  medicare_tin <- read_medicare_part_b_tin(medicare_pub_path)

  # Count physicians per TIN
  tin_summary <- medicare_tin %>%
    group_by(tax_id) %>%
    summarize(
      n_physicians = n_distinct(npi),
      organization_name = first(organization_name),
      total_services = sum(total_services, na.rm = TRUE)
    )

  # Join to cohort and classify
  cohort %>%
    left_join(medicare_tin %>% select(npi, tax_id), by = "npi") %>%
    left_join(tin_summary, by = "tax_id") %>%
    mutate(
      practice_arrangement_tin = case_when(
        is.na(tax_id) ~ "Unknown (No Medicare)",
        n_physicians == 1 ~ "Solo",
        n_physicians <= 10 ~ "Small Group (2-10)",
        n_physicians <= 99 ~ "Large Group (11-99)",
        n_physicians >= 100 ~ "Hospital/Large System (100+)",
        TRUE ~ "Unknown"
      ),
      practice_arrangement_confidence = if_else(
        !is.na(tax_id), "High", "Unknown"
      )
    )
}
```

---

**1.2 CMS Group Practice Reporting Option (GPRO)**

| Attribute | Details |
|-----------|---------|
| **Data Source** | CMS MIPS Group Data (Quality Payment Program) |
| **Access** | Public (via MIPS Public Use Files) |
| **URL** | https://qpp.cms.gov/mips/explore-measures/quality-measures |
| **Key Fields** | Group TIN, Group Name, Group Size, Individual NPIs in group |
| **Coverage** | ~40% of physicians (MIPS participants only) |
| **Cost** | Free |

**How It Helps:** Identifies physicians participating in group reporting (strong signal of group employment)

**Expected Improvement:**
- Confirms group practice participation: **~40% coverage, ~80% accuracy**
- Works best combined with TIN analysis

---

**1.3 Hospital Affiliation Databases**

| Data Source | Coverage | Access | Cost | Key Benefit |
|-------------|----------|--------|------|-------------|
| **AHA Annual Survey** | ~6,000 hospitals | Purchase | $500-2,000/year | Hospital-employed physicians list |
| **Definitive Healthcare** | ~9,000 hospitals | Subscription | $5,000-20,000/year | Physician-hospital affiliations, employment status |
| **SK&A Physician Office Database** | ~850,000 physicians | Purchase | $10,000-30,000 | Practice ownership, corporate affiliations |
| **Doximity Hospital Affiliations** | ~1M physicians | Partnership/API | Negotiable | Self-reported hospital affiliations |

**Recommended:** Start with **free Doximity data** (if available via partnership), then upgrade to SK&A for ownership data if budget allows.

**Expected Improvement:**
- Hospital-employed identification: **~60% coverage, ~75% accuracy**

---

#### TIER 2: Moderate Impact, Requires Data Matching

**1.4 Medicare Shared Savings Program (MSSP) ACO Participants**

| Attribute | Details |
|-----------|---------|
| **Data Source** | CMS ACO Provider-Level Research Identifiable File (RIF) |
| **Access** | Restricted (requires CMS DUA) |
| **Key Fields** | NPI, ACO Name, ACO TIN, Participation dates |
| **Coverage** | ~15% of physicians (ACO participants) |
| **Cost** | Free (with DUA application, 6-12 month process) |

**How It Helps:** ACO participation indicates group practice or hospital employment (solo physicians rarely join ACOs)

**Expected Improvement:**
- ACO participants likely employed/group: **~15% coverage, ~70% accuracy**

---

**1.5 HRSA Health Center Program Participant List**

| Attribute | Details |
|-----------|---------|
| **Data Source** | HRSA Uniform Data System (UDS) - FQHC Provider Data |
| **Access** | Public (FQHC provider NPIs available via UDS) |
| **URL** | https://data.hrsa.gov/tools/data-reporting/program-data |
| **Key Fields** | NPI, FQHC Name, Employment status (if available) |
| **Coverage** | ~50,000 physicians (FQHC-employed only) |
| **Cost** | Free |

**How It Helps:** FQHC physicians are definitively employed (not owners)

**Expected Improvement:**
- FQHC physicians = employed: **~5% of cohort, ~95% accuracy**

---

**1.6 Private Equity / Corporate Ownership Databases**

| Data Source | Coverage | Access | Cost | Key Benefit |
|-------------|----------|--------|------|-------------|
| **Physician Advocacy Institute (PAI)** | Corporate ownership trends | Public reports | Free | Aggregate statistics only |
| **PitchBook Healthcare Deals** | PE acquisitions | Subscription | $20,000+/year | Identifies PE-owned practices |
| **Definitive Healthcare M&A** | Practice acquisitions | Subscription | $10,000+/year | Tracks corporate ownership changes |

**Note:** Individual physician-level data **not publicly available** for private equity ownership. Best proxy: Large group practice (100+ physicians) + organization name patterns ("Partners", "Medical Group", "Physician Network")

---

#### TIER 3: Research-Only (High Barrier)

**1.7 State Medical Board Incorporation Records**

| Attribute | Details |
|-----------|---------|
| **Data Source** | Secretary of State business registrations (by state) |
| **Access** | Public but requires state-by-state scraping |
| **Key Fields** | Business name, Owner names, Type (PC, LLC, PLLC), NPIs (if linked) |
| **Coverage** | Varies by state (CA, NY, TX have robust systems) |
| **Cost** | Free per lookup; bulk access varies by state |

**How It Helps:** Identifies practice owners by name (can match to physician names)

**Challenge:**
- Requires entity name → NPI matching (difficult)
- 50-state effort (high overhead)
- Not recommended unless state-specific analysis

---

### 🎯 RECOMMENDED IMPLEMENTATION STRATEGY

**Phase 1: Quick Wins (0-3 months)**
1. **Integrate Medicare Part B TIN data** (free, public)
   - Expected: 30% → 65% accuracy, ~70% coverage
   - Implementation: 2 weeks
   - Cost: $0

2. **Add HRSA UDS FQHC provider list** (free, public)
   - Expected: +5% employed physicians with 95% confidence
   - Implementation: 1 week
   - Cost: $0

**Phase 2: Enhanced Classification (3-6 months)**
3. **Add CMS MIPS Group Practice data** (free, public)
   - Expected: +10% group practice identification
   - Implementation: 2 weeks
   - Cost: $0

4. **Evaluate Doximity hospital affiliation partnership**
   - Expected: +20% hospital-employed identification
   - Implementation: Negotiation + 1 month integration
   - Cost: Partnership terms (possibly free)

**Phase 3: Gold Standard (6-12 months, budget-dependent)**
5. **Purchase SK&A Physician Office Database** (if budget allows)
   - Expected: 70% → 85% accuracy, comprehensive ownership data
   - Implementation: 1 month
   - Cost: $10,000-30,000/year

**Phase 4: Research Frontier (12+ months)**
6. **Apply for CMS ACO RIF DUA** (long timeline)
   - Expected: +15% employed/group classification
   - Implementation: 6-12 month DUA process + 1 month integration
   - Cost: Free (DUA administrative burden)

---

### Expected Overall Improvement

| Metric | Current | Phase 1 | Phase 2 | Phase 3 |
|--------|---------|---------|---------|---------|
| **Coverage** | 60% | 75% | 80% | 90% |
| **Accuracy** | 30% | 65% | 75% | 85% |
| **Quality Rating** | PROXY | MEDIUM | MEDIUM-HIGH | HIGH |
| **Cost** | $0 | $0 | $0 | $10-30K/year |

---

## 2. ACADEMIC AFFILIATION

### Current Limitations
- **Method:** Hospital classification + ACGME program matching (partial)
- **Coverage:** ~50% with classification
- **Accuracy:** Partial - Cannot distinguish:
  * Clinical faculty vs volunteer faculty
  * Full-time vs part-time academic
  * Research-active vs teaching-only
  * Academic rank (assistant/associate/full professor)

### 🎯 Recommended External Data Sources (Ranked by Impact)

#### TIER 1: High Impact

**2.1 AAMC Faculty Roster** ⭐ **GOLD STANDARD**

| Attribute | Details |
|-----------|---------|
| **Data Source** | Association of American Medical Colleges (AAMC) Faculty Roster |
| **Access** | Purchase/License from AAMC |
| **URL** | https://www.aamc.org/data-reports/faculty-institutions/report/faculty-roster-us-medical-school-faculty |
| **Key Fields** | NPI (sometimes), Name, Medical School, Department, Rank, FTE, Research status |
| **Coverage** | ~175,000 full-time faculty at 155 US medical schools |
| **Cost** | $2,000-10,000/year (depends on usage rights) |

**How It Improves Classification:**
```r
# AAMC Faculty Roster integration
academic_affiliation <- case_when(
  aamc_rank %in% c("Professor", "Associate Professor") ~ "Senior Academic Faculty",
  aamc_rank %in% c("Assistant Professor") ~ "Junior Academic Faculty",
  aamc_rank %in% c("Instructor", "Lecturer") ~ "Academic Instructor",
  aamc_research_fte > 0.5 ~ "Research-Intensive Faculty",
  aamc_clinical_fte > 0.5 ~ "Clinical Faculty",
  !is.na(aamc_medical_school) ~ "Academic Affiliated",
  TRUE ~ "Non-Academic"
)
```

**Expected Improvement:**
- Current: ~50% coverage (partial identification)
- With AAMC: **~85% coverage for academic physicians, ~95% accuracy**
- Identifies: Rank, FTE, research vs clinical focus

**Implementation Considerations:**
- NPI not always in AAMC data → requires name + medical school matching
- Full-time faculty only (misses voluntary faculty)
- Annual subscription required

---

**2.2 NIH RePORTER (Research Grant Database)** ⭐ **FREE, HIGH IMPACT**

| Attribute | Details |
|-----------|---------|
| **Data Source** | NIH Research Portfolio Online Reporting Tools (RePORTER) |
| **Access** | Public API |
| **URL** | https://reporter.nih.gov/ |
| **Key Fields** | Principal Investigator name, Organization, Grant amount, Project dates |
| **Coverage** | ~50,000 active NIH-funded PIs |
| **Cost** | Free |

**How It Helps:** NIH grant = strong academic affiliation signal (research-intensive institutions)

**Expected Improvement:**
- NIH PIs: **~10% of physicians, ~95% academic affiliation accuracy**
- Identifies research-active faculty specifically

**Implementation:**
```r
# File: R/enrich_academic_affiliation_nih.R
add_nih_researcher_status <- function(cohort, nih_reporter_path) {

  # Load NIH RePORTER data (downloaded via API)
  nih_pis <- read_nih_reporter(nih_reporter_path)

  # Fuzzy match physician names to PI names
  matched <- cohort %>%
    fuzzyjoin::stringdist_left_join(
      nih_pis,
      by = c("first_name" = "pi_first_name", "last_name" = "pi_last_name"),
      max_dist = 1,
      method = "jw"  # Jaro-Winkler distance
    ) %>%
    filter(distance <= 0.1)  # High confidence matches only

  # Classify academic status
  cohort %>%
    mutate(
      nih_funded = npi %in% matched$npi,
      academic_research_active = if_else(nih_funded, "Yes", "No"),
      total_nih_funding = matched$total_funding[match(npi, matched$npi)],
      nih_grant_years = matched$grant_years[match(npi, matched$npi)]
    )
}
```

**Caveats:**
- Name-only matching (no NPI in NIH data) → fuzzy matching required
- Research-intensive bias (misses clinical educators)
- Underestimates academic affiliation (only funded researchers)

---

**2.3 ClinicalTrials.gov Principal Investigators**

| Attribute | Details |
|-----------|---------|
| **Data Source** | ClinicalTrials.gov database (NIH/NLM) |
| **Access** | Public API |
| **URL** | https://clinicaltrials.gov/data-api/api |
| **Key Fields** | PI name, Affiliation (organization), Trial phase, Specialty |
| **Coverage** | ~100,000 PIs (clinical trials only) |
| **Cost** | Free |

**How It Helps:** Clinical trial PI = academic medical center affiliation (typically)

**Expected Improvement:**
- Clinical trial PIs: **~8% of physicians, ~85% academic accuracy**
- Complements NIH (captures clinical research, not just basic science)

---

**2.4 PubMed Author Affiliations**

| Attribute | Details |
|-----------|---------|
| **Data Source** | PubMed/MEDLINE author affiliation data |
| **Access** | Public API (NCBI E-utilities) |
| **URL** | https://www.ncbi.nlm.nih.gov/home/develop/api/ |
| **Key Fields** | Author name, Affiliation text (institution), Publication count |
| **Coverage** | Millions of authors (variable physician coverage) |
| **Cost** | Free |

**How It Helps:**
- Affiliation text contains "University", "School of Medicine", "Academic Medical Center" → academic signal
- Publication count → research productivity proxy

**Challenges:**
- **CRITICAL:** No NPI, no standardized physician identifier
- Name ambiguity (John Smith problem)
- Affiliation text parsing required (messy)
- Computational intensive (millions of records)

**Recommended:** Use only if name + specialty matching feasible, or as secondary validation

---

**2.5 ACGME Program Director/Faculty Listings**

| Attribute | Details |
|-----------|---------|
| **Data Source** | ACGME Accreditation Data System (ADS) |
| **Access** | Public (program websites) or ACGME partnership |
| **Key Fields** | Program director name, Core faculty names, Program institution |
| **Coverage** | ~15,000 program directors + ~50,000 core faculty |
| **Cost** | Free (web scraping) or partnership negotiation |

**How It Helps:** Program director/core faculty = definitive academic affiliation

**Expected Improvement:**
- Residency faculty: **~5% of physicians, ~98% academic accuracy**

**Implementation:** Web scraping ACGME program websites (intensive but valuable)

---

**2.6 Medicare Graduate Medical Education (GME) Payments**

| Attribute | Details |
|-----------|---------|
| **Data Source** | CMS Hospital Cost Reports (HCRIS) - Worksheet E Part A |
| **Access** | Public (CMS HCRIS Public Use Files) |
| **URL** | https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports |
| **Key Fields** | Hospital NPI, Teaching hospital status, GME payments, Resident FTE |
| **Coverage** | ~1,200 teaching hospitals |
| **Cost** | Free |

**How It Helps:**
- Physicians affiliated with teaching hospitals (via hospital NPI linkage) → likely academic
- GME payment amount → teaching intensity proxy

**Expected Improvement:**
- Teaching hospital affiliation: **~30% of physicians, ~70% academic accuracy** (includes community teaching hospitals)

**Note:** Requires hospital NPI → physician NPI linkage (see hospital affiliation databases above)

---

#### TIER 2: Moderate Impact

**2.7 AAMC Teaching Hospital Membership**

| Attribute | Details |
|-----------|---------|
| **Data Source** | AAMC Council of Teaching Hospitals (COTH) |
| **Access** | Public (membership list) |
| **URL** | https://www.aamc.org/members/coth |
| **Key Fields** | Hospital name, Hospital NPI, Medical school affiliation |
| **Coverage** | ~400 major teaching hospitals |
| **Cost** | Free (list only) |

**How It Helps:** Physicians at COTH hospitals → academic affiliation (via hospital linkage)

**Expected Improvement:**
- COTH hospital affiliation: **~25% of physicians, ~75% academic accuracy**

---

**2.8 Medical School Faculty Directory Web Scraping**

| Attribute | Details |
|-----------|---------|
| **Data Source** | Medical school websites (155 US schools) |
| **Access** | Public (web scraping required) |
| **Key Fields** | Faculty name, Department, Rank (sometimes), Email |
| **Coverage** | Potentially ~200,000 faculty (full-time + voluntary) |
| **Cost** | Free (development time for scraping infrastructure) |

**How It Helps:** Direct faculty identification from source institutions

**Challenges:**
- **High development cost:** 155 schools × different website structures
- No NPI (name matching only)
- Maintenance burden (websites change)
- Legal/ethical considerations (robots.txt compliance, rate limiting)

**Recommended:** Only if AAMC Faculty Roster not accessible

---

### 🎯 RECOMMENDED IMPLEMENTATION STRATEGY

**Phase 1: Free High-Impact Sources (0-3 months)**
1. **NIH RePORTER integration** (free, public)
   - Expected: +10% academic identification (research faculty)
   - Implementation: 3 weeks (API integration + name matching)
   - Cost: $0

2. **ClinicalTrials.gov PIs** (free, public)
   - Expected: +8% academic identification (clinical researchers)
   - Implementation: 2 weeks
   - Cost: $0

3. **Medicare GME teaching hospital linkage** (free, public)
   - Expected: +30% academic signal (teaching hospital physicians)
   - Implementation: 2 weeks (requires hospital affiliation data)
   - Cost: $0

**Phase 2: Purchase Gold Standard (3-6 months, budget-dependent)**
4. **AAMC Faculty Roster license** (paid)
   - Expected: 50% → 85% coverage, ~95% accuracy
   - Implementation: 1 month (name + school matching)
   - Cost: $2,000-10,000/year

**Phase 3: Supplemental Sources (6-12 months)**
5. **ACGME program director/faculty web scraping**
   - Expected: +5% academic faculty identification
   - Implementation: 2-3 months (infrastructure development)
   - Cost: Development time

---

### Expected Overall Improvement

| Metric | Current | Phase 1 | Phase 2 | Phase 3 |
|--------|---------|---------|---------|---------|
| **Coverage** | 50% | 65% | 85% | 90% |
| **Accuracy** | Partial | 75% | 95% | 95% |
| **Quality Rating** | PROXY | MEDIUM | HIGH | HIGH |
| **Cost** | $0 | $0 | $2-10K/year | $2-10K/year |

---

## 3. SAFETY-NET PRACTICE

### Current Limitations
- **Method:** Medicaid/Medicare ratio at ZIP3 level (area proxy)
- **Coverage:** ~60% with proxy classification
- **Accuracy:** Low - ZIP3 level, not individual physician
- **Issues:**
  * Cannot identify individual safety-net physicians
  * ZIP3 too coarse (e.g., all Manhattan physicians same score)
  * Doesn't capture FQHC, free clinic, or sliding-fee practices

### 🎯 Recommended External Data Sources (Ranked by Impact)

#### TIER 1: High Impact, Publicly Available

**3.1 HRSA Uniform Data System (UDS) - FQHC Providers** ⭐ **HIGHEST IMPACT**

| Attribute | Details |
|-----------|---------|
| **Data Source** | HRSA Health Center Program UDS Data |
| **Access** | Public (limited) / HRSA Data Warehouse (application) |
| **URL** | https://data.hrsa.gov/tools/data-reporting/program-data |
| **Key Fields** | FQHC Name, Provider NPIs (if available), Service location, Patient demographics |
| **Coverage** | ~14,000 FQHCs serving 30M patients, ~50,000 physicians |
| **Cost** | Free |

**How It Improves Classification:**
```r
# FQHC provider integration
safety_net_status <- case_when(
  npi %in% fqhc_provider_npis ~ "FQHC (Definitive Safety-Net)",
  hrsa_medically_underserved_area == TRUE ~ "MUA/MUP Practice",
  dsht_hospital_affiliated == TRUE ~ "DSH Hospital-Affiliated",
  sliding_fee_scale == TRUE ~ "Sliding Fee Scale",
  TRUE ~ "Non-Safety-Net"
)
```

**Expected Improvement:**
- Current: ZIP3 proxy
- With HRSA UDS: **~50,000 physicians definitively identified as FQHC, ~98% accuracy**
- Captures: FQHC-employed physicians (gold standard for safety-net)

**Implementation:**
```r
# File: R/enrich_safety_net_hrsa.R
add_fqhc_affiliation <- function(cohort, hrsa_uds_path) {

  # Load HRSA UDS FQHC data
  fqhc_sites <- read_hrsa_uds(hrsa_uds_path)

  # Extract provider NPIs (if available in UDS data)
  fqhc_npis <- fqhc_sites %>%
    select(fqhc_name, npi, service_location) %>%
    filter(!is.na(npi))

  # Join to cohort
  cohort %>%
    left_join(fqhc_npis, by = "npi") %>%
    mutate(
      fqhc_affiliated = !is.na(fqhc_name),
      fqhc_safety_net = if_else(fqhc_affiliated, "FQHC Safety-Net", "Not FQHC"),
      fqhc_confidence = if_else(fqhc_affiliated, "Definitive", NA_character_)
    )
}
```

**Data Availability Note:**
- Individual provider NPIs **may not be in public UDS files**
- May require HRSA Data Warehouse application (approval process)
- Alternative: Match by FQHC organization name + address (less precise)

---

**3.2 340B Drug Pricing Program Participant Database**

| Attribute | Details |
|-----------|---------|
| **Data Source** | HRSA 340B Office of Pharmacy Affairs Information System (OPAIS) |
| **Access** | Public (facility list) |
| **URL** | https://www.hrsa.gov/opa/eligibility-and-registration/covered-entities |
| **Key Fields** | Covered entity name, Address, Entity type, Termination date |
| **Coverage** | ~50,000 covered entities (FQHCs, DSH hospitals, Ryan White clinics) |
| **Cost** | Free |

**How It Helps:** Physicians at 340B entities → safety-net practice (by facility linkage)

**Expected Improvement:**
- 340B entity affiliation: **~20% of physicians, ~80% safety-net accuracy**
- Broader than FQHC (includes DSH hospitals, Ryan White clinics)

**Challenge:** No individual physician NPIs → requires facility → physician linkage (use hospital affiliation data)

---

**3.3 State Medicaid Managed Care Provider Directories**

| Attribute | Details |
|-----------|---------|
| **Data Source** | State Medicaid agency provider directories (by state) |
| **Access** | Public (varies by state) |
| **Coverage** | Medicaid-participating physicians by state |
| **Cost** | Free |

**How It Helps:** High Medicaid patient panel → safety-net practice signal

**Example States with Good Data:**
- **California:** Medi-Cal Provider Directory (public)
- **New York:** Medicaid Provider Search (public)
- **Texas:** TMHP Provider Search (public)
- **Florida:** Medicaid Provider Portal (public)

**Expected Improvement:**
- Medicaid participation: **~40% of physicians, ~60% safety-net correlation**
- Best combined with Medicaid claim counts (if available)

**Challenge:**
- 50-state effort (high overhead)
- Medicaid participation ≠ high Medicaid volume (many accept some Medicaid)
- Better proxy: Medicaid claim counts (see Tier 2)

---

**3.4 Ryan White HIV/AIDS Program Provider Data**

| Attribute | Details |
|-----------|---------|
| **Data Source** | HRSA Ryan White HIV/AIDS Program Services Report (RSR) |
| **Access** | Limited (aggregate data public, provider-level requires application) |
| **URL** | https://ryanwhite.hrsa.gov/data |
| **Key Fields** | Provider organization, Service location, Provider NPIs (maybe) |
| **Coverage** | ~2,500 Ryan White clinics, ~10,000 providers |
| **Cost** | Free |

**How It Helps:** Ryan White providers → safety-net (HIV care safety-net system)

**Expected Improvement:**
- Ryan White providers: **~10,000 physicians, ~95% safety-net accuracy**

---

**3.5 Disproportionate Share Hospital (DSH) Affiliations**

| Attribute | Details |
|-----------|---------|
| **Data Source** | CMS Disproportionate Share Hospital data |
| **Access** | Public (via Medicare Cost Reports or HCRIS) |
| **URL** | https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/AcuteInpatientPPS/dsh |
| **Key Fields** | Hospital NPI, DSH percentage, Medicaid inpatient days, SSI percentage |
| **Coverage** | ~1,500 DSH hospitals |
| **Cost** | Free |

**How It Helps:** Physicians at DSH hospitals → safety-net practice (via hospital linkage)

**Expected Improvement:**
- DSH hospital affiliation: **~25% of physicians, ~70% safety-net accuracy**

**Challenge:** Requires hospital NPI → physician NPI linkage (use hospital affiliation databases)

---

#### TIER 2: High Impact, Restricted Access

**3.6 State Medicaid Claims (Individual Physician-Level)** ⭐ **GOLD STANDARD**

| Attribute | Details |
|-----------|---------|
| **Data Source** | State Medicaid agencies (Medicaid Analytic eXtract - MAX files) |
| **Access** | Restricted (requires state DUA) |
| **Key Fields** | Provider NPI, Medicaid beneficiary count, Total claims, Service type |
| **Coverage** | All Medicaid-billing physicians by state |
| **Cost** | Free (but DUA process cost = time) |

**How It Helps:** Direct Medicaid patient count at individual physician level

**Expected Improvement:**
- Medicaid claim volume: **~50% of physicians, ~85% safety-net accuracy**
- Can calculate: % Medicaid patients, Medicaid dependency ratio

**Challenge:**
- **Requires Data Use Agreement (DUA)** with each state (6-12 month process per state)
- IRB approval may be required
- State-by-state effort (50 states)
- Not feasible for most projects unless already have DUA access

**Recommendation:** Only pursue if state-specific analysis and existing relationship with state Medicaid

---

**3.7 CMS Dual-Eligible Beneficiary Data**

| Attribute | Details |
|-----------|---------|
| **Data Source** | CMS Medicare-Medicaid Dual Eligible Beneficiaries |
| **Access** | Restricted (requires CMS DUA) |
| **Key Fields** | Provider NPI, Dual-eligible beneficiary count, Services |
| **Coverage** | ~12M dual-eligible beneficiaries |
| **Cost** | Free (DUA process) |

**How It Helps:** High dual-eligible patient panel → safety-net signal (low-income seniors)

**Expected Improvement:**
- Dual-eligible service: **~30% of physicians, ~75% safety-net correlation**

**Challenge:** Requires CMS DUA (6-12 month process)

---

#### TIER 3: Supplemental (Lower Impact)

**3.8 Area Deprivation Index (ADI) at Practice Location**

| Attribute | Details |
|-----------|---------|
| **Data Source** | University of Wisconsin Neighborhood Atlas (ADI) |
| **Access** | Free (public) |
| **URL** | https://www.neighborhoodatlas.medicine.wisc.edu/ |
| **Key Fields** | Census block group, ADI national percentile, ADI state decile |
| **Coverage** | All US census block groups |
| **Cost** | Free |

**How It Helps:** Physicians practicing in high ADI (deprived) neighborhoods → safety-net proxy

**Expected Improvement:**
- Practice location ADI: **~90% coverage, ~60% safety-net correlation**
- Better than ZIP3 (block group more granular)

**Limitation:** Still area-level (not individual physician), but better than current ZIP3

---

**3.9 National Association of Community Health Centers (NACHC) Directory**

| Attribute | Details |
|-----------|---------|
| **Data Source** | NACHC Community Health Center Locator |
| **Access** | Public (health center list) |
| **URL** | https://www.nachc.org/health-center-issues/health-center-locator/ |
| **Coverage** | Overlaps with HRSA UDS (FQHCs) |
| **Cost** | Free |

**How It Helps:** Same as HRSA UDS (use HRSA instead - more authoritative)

---

### 🎯 RECOMMENDED IMPLEMENTATION STRATEGY

**Phase 1: Definitive Safety-Net Identification (0-3 months)**
1. **HRSA UDS FQHC provider list** (free, public)
   - Expected: ~50,000 physicians definitively identified as FQHC
   - Implementation: 2 weeks (if provider NPIs available in UDS)
   - Cost: $0
   - Quality upgrade: PROXY → **HIGH** for FQHC physicians

2. **340B Covered Entity linkage** (free, public)
   - Expected: +20% safety-net identification (DSH hospitals, Ryan White clinics)
   - Implementation: 3 weeks (requires facility → physician linkage)
   - Cost: $0

3. **Area Deprivation Index (ADI) at practice location** (free, public)
   - Expected: ZIP3 → Census block group (better granularity)
   - Implementation: 1 week
   - Cost: $0
   - Quality upgrade: PROXY → **MEDIUM-PROXY** (still area-level but better)

**Phase 2: Enhanced Medicaid Identification (3-6 months)**
4. **State Medicaid managed care directories** (free, select states)
   - Expected: +40% Medicaid participation flag (not volume)
   - Implementation: 2 months (3-5 high-priority states: CA, NY, TX, FL)
   - Cost: $0

5. **Ryan White HIV/AIDS provider list** (free, if available)
   - Expected: +10,000 providers (specialized safety-net)
   - Implementation: 2 weeks
   - Cost: $0

**Phase 3: Gold Standard (12+ months, DUA-dependent)**
6. **State Medicaid claims DUA** (if state-specific analysis)
   - Expected: Individual physician Medicaid volume → 85% safety-net accuracy
   - Implementation: 6-12 month DUA process per state + 1 month integration
   - Cost: Free (DUA administrative burden)
   - **Only if:** State-specific project, existing state relationship

---

### Expected Overall Improvement

| Metric | Current | Phase 1 | Phase 2 | Phase 3 |
|--------|---------|---------|---------|---------|
| **Coverage** | 60% (ZIP3 proxy) | 80% | 85% | 90% |
| **Granularity** | ZIP3 (coarse) | Individual + Block group | Individual + Medicaid flag | Individual + claims volume |
| **Accuracy** | Low (area proxy) | 65% | 75% | 85% |
| **Quality Rating** | PROXY | MEDIUM | MEDIUM-HIGH | HIGH |
| **Cost** | $0 | $0 | $0 | DUA burden |

---

## SUMMARY: PRIORITIZED IMPLEMENTATION ROADMAP

### 🚀 Quick Wins (0-3 months, $0 cost)

| Variable | External Data Source | Expected Improvement | Implementation |
|----------|---------------------|---------------------|----------------|
| **Practice Arrangement** | Medicare Part B TIN | 30% → 65% accuracy | 2 weeks |
| **Practice Arrangement** | HRSA UDS FQHC list | +5% employed (FQHC) | 1 week |
| **Academic Affiliation** | NIH RePORTER | +10% research faculty | 3 weeks |
| **Academic Affiliation** | ClinicalTrials.gov | +8% clinical researchers | 2 weeks |
| **Academic Affiliation** | Medicare GME teaching hospitals | +30% academic signal | 2 weeks |
| **Safety-Net Practice** | HRSA UDS FQHC providers | 50K definitive safety-net | 2 weeks |
| **Safety-Net Practice** | 340B Covered Entity list | +20% safety-net | 3 weeks |
| **Safety-Net Practice** | ADI (block group) | ZIP3 → block group | 1 week |

**Total Phase 1 Time:** ~3 months (if done sequentially); ~1.5 months (if parallelized)
**Total Phase 1 Cost:** $0

---

### 💰 Budget-Dependent Upgrades (3-12 months)

| Variable | External Data Source | Cost/Year | Expected Improvement |
|----------|---------------------|-----------|---------------------|
| **Practice Arrangement** | SK&A Physician Database | $10-30K | 65% → 85% accuracy |
| **Academic Affiliation** | AAMC Faculty Roster | $2-10K | 65% → 95% accuracy |

**Recommendation:** Start with AAMC Faculty Roster ($2-10K) - higher ROI than SK&A for academic research

---

### 🔬 Research Frontier (12+ months, DUA barrier)

| Variable | External Data Source | Barrier | Expected Improvement |
|----------|---------------------|---------|---------------------|
| **Practice Arrangement** | CMS ACO RIF | 6-12 mo DUA | +15% group/employed |
| **Safety-Net Practice** | State Medicaid claims | 6-12 mo DUA per state | 75% → 85% accuracy |

**Recommendation:** Only pursue if long-term research project with institutional DUA support

---

## IMPLEMENTATION PRIORITY MATRIX

```
High Impact, Low Barrier (DO FIRST)
┌─────────────────────────────────────────────────────┐
│ 1. Medicare Part B TIN (Practice Arrangement)      │
│ 2. HRSA UDS FQHC (Safety-Net)                      │
│ 3. NIH RePORTER (Academic Affiliation)             │
│ 4. 340B Database (Safety-Net)                      │
│ 5. ADI Block Group (Safety-Net - better proxy)     │
└─────────────────────────────────────────────────────┘

High Impact, Moderate Barrier (BUDGET-DEPENDENT)
┌─────────────────────────────────────────────────────┐
│ 6. AAMC Faculty Roster (Academic - $2-10K/year)    │
│ 7. SK&A Physician DB (Practice - $10-30K/year)     │
└─────────────────────────────────────────────────────┘

Moderate Impact, Low Barrier (FILL GAPS)
┌─────────────────────────────────────────────────────┐
│ 8. ClinicalTrials.gov (Academic)                    │
│ 9. MIPS Group Practice (Practice Arrangement)       │
│ 10. State Medicaid directories (Safety-Net)         │
└─────────────────────────────────────────────────────┘

Lower Priority (RESEARCH ONLY)
┌─────────────────────────────────────────────────────┐
│ 11. CMS ACO RIF (Practice - DUA required)          │
│ 12. State Medicaid claims (Safety-Net - DUA)       │
│ 13. Medical school web scraping (Academic - effort) │
└─────────────────────────────────────────────────────┘
```

---

## NEXT STEPS

1. **Review and approve** quick wins (Phase 1: $0 cost)
2. **Download free datasets:**
   - Medicare Part B PUF (TIN data)
   - HRSA UDS FQHC list
   - NIH RePORTER
   - ClinicalTrials.gov
   - 340B Covered Entity list
   - ADI block group data
3. **Implement integration functions** (3 months)
4. **Validate improvements** (run QA analysis)
5. **Update quality documentation** (upgrade from PROXY to MEDIUM/HIGH)
6. **Consider budget allocation** for AAMC Faculty Roster ($2-10K) if academic analysis critical

---

**Questions? See related documentation:**
- `docs/TABLE1_VARIABLE_QUALITY_REFERENCE.md` - Current quality ratings
- `docs/PHYSICIAN_CHARACTERISTICS_ANALYSIS.md` - Variable catalog
