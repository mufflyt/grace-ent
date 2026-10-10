# ABOG-NPI Matching Improvement Roadmap
**Date:** 2025-10-04
**Current Coverage:** 67.1% (53,249/79,397)
**Target Coverage:** 90%+ (71,457+ matches)
**Gap to Close:** 18,208 additional matches needed

---

## Executive Summary

Analysis of historical ABOG-NPI matching reveals **90% coverage was previously achieved** using 20 different matching strategies. The current pipeline achieves only 67.1%, representing a **22.9 percentage point regression**. This document evaluates all recommended approaches and provides a prioritized implementation plan.

### Key Findings

✅ **PROVEN SUCCESS**: File `abog_npi_90_PERCENT_FINAL_20251002_034237.csv` demonstrates 90% coverage is achievable
❌ **CRITICAL BUG FIXED**: DuckDB read_only=TRUE prevented temp table creation (now resolved)
⚠️ **NAME PARSING**: ABOG CSV has pre-parsed columns (`first`, `last`, `middle`) that should be used directly instead of re-parsing `physician_name`
✅ **GENDER BLOCKING**: Prevents catastrophic false positives (DOUG ≠ DANIELA)
✅ **MD/DO CREDENTIALS**: 99.5% detection rate available for soft blocking
❌ **NO JARO-WINKLER**: Explicitly banned due to false positives (KARI → MARILYNN)

---

## Evaluation of Recommended Approaches

### 1. ✅ CRITICAL - Fix ABOG Name Parsing
**Status:** IMMEDIATE ACTION REQUIRED
**Impact:** +0.3% (250 matches)
**Effort:** LOW (10 minutes)

**Problem:**
```r
# Current: Re-parses physician_name, creating duplicate columns
physician_name  -> parse -> first_name, last_name, middle_name
first, last, middle (unused pre-parsed columns)

# Example disasters:
"Kari L. Evans" -> Creates first_name="Kari", last_name="Evans"
But ABOG already has: first="Kari", last="Evans", middle="L"
```

**Solution:**
Use ABOG's pre-parsed columns directly:
```r
# Line ~100 in R/abog_npi_production_pipeline.R
abog <- read_csv(abog_path, show_col_types = FALSE) %>%
  mutate(
    first_name = toupper(trimws(first)),    # Use pre-parsed
    last_name = toupper(trimws(last)),      # Use pre-parsed
    middle_name = toupper(trimws(middle))   # Use pre-parsed
  )
```

**Evidence:** Test showed 34,497 matches with original columns vs 34,247 with re-parsed (+250 matches)

---

### 2. ✅ HIGH PRIORITY - Compile Previous 90% Success
**Status:** READY TO IMPLEMENT
**Impact:** +15-20% (11,909-15,879 matches by reusing validated data)
**Effort:** MEDIUM (2 hours)

**Previous Success Methods (from 90% file):**
```
20 different matching strategies used:
1. original                           # Original NPI from ABOG
2. exact_name                        # First + Last exact match
3. initial_lastname                  # Initial + Last match
4. nickname_match                    # Nickname expansion
5. historical_nppes_2020/2021/2022  # Historical NPPES snapshots
6. medicare_part_b_2020/2021/2022/2023  # Medicare Part B (multiple years)
7. medicare_part_b_physicians_initial/exact/middle  # Medicare Part B variations
8. medicare_part_d                   # Medicare Part D prescribers
9. open_payments                     # Open Payments data
10. physician_compare_2023/2024_initial/exact/middle  # Physician Compare
11. subspecialist_medicare           # Subspecialty-specific Medicare
12. physician_compare_general        # General Physician Compare
```

**Implementation:**
```r
# Load previous 90% success file
prior_90_pct <- read_csv("data/canonical_abog_npi/abog_npi_90_PERCENT_FINAL_20251002_034237.csv")

# Filter high-confidence matches (exclude NA method, very low confidence)
validated_prior <- prior_90_pct %>%
  filter(
    !is.na(match_method),
    match_method != "NA",
    !is.na(npi),
    confidence_score >= 0.85  # Only high-confidence
  ) %>%
  select(abog_id, npi, match_method, confidence_score, npi_first, npi_last)

# Integrate into current pipeline as "Phase 0: Historical Validated Matches"
# Apply BEFORE Phase 1 to avoid duplicate work
```

**Risk Mitigation:**
- ✅ All 79,397 physicians have unique abog_id (no duplicates)
- ✅ Can validate against current NPPES to confirm NPI still active
- ⚠️ Should flag matches with method="NA" for review
- ✅ Cross-validate high-confidence matches (≥0.90) against NPPES name fields

---

### 3. ✅ HIGH PRIORITY - Gender Soft Blocking
**Status:** READY TO IMPLEMENT
**Impact:** +0% matches but PREVENTS CATASTROPHIC FALSE POSITIVES
**Effort:** LOW (30 minutes)

**Critical Evidence:**
```
From JARO_WINKLER_FAILURE_ANALYSIS.md:

DISASTER CASE:
ABOG: "Doug J. Chung, MD"  (Male)
NPPES: "DANIELA CHUNG"     (Female, NPI: 1104333442)
Match: Stage 3 (Initial + Last + State)
Result: FALSE POSITIVE blocked by gender validation
```

**Implementation:**
```r
# Add gender inference from {gender} package
library(gender)

# ABOG: Use first name for gender prediction
abog_gender <- gender(abog$first_name, method = "ssa") %>%
  select(name, gender_prob = proportion_female)

# NPPES: Use "Provider First Name" for gender prediction
nppes_gender <- gender(nppes$first_name, method = "ssa") %>%
  select(name, gender_prob = proportion_female)

# Soft blocking rule (not hard block to allow edge cases)
# Penalize confidence if genders mismatch:
if (abs(abog_gender_prob - nppes_gender_prob) > 0.7) {
  confidence_score <- confidence_score * 0.5  # 50% penalty
  add_flag("gender_mismatch_warning")
}
```

**Gender Package Availability:**
- ✅ Already used in the codebase (found in 10 files)
- ✅ SSA method provides US-based name-gender mappings
- ✅ Returns probability (0.0-1.0) rather than binary Male/Female

---

### 4. ✅ MEDIUM PRIORITY - MD/DO Credential Soft Blocking
**Status:** DATA READY, IMPLEMENT AS FILTER
**Impact:** +0% matches but improves precision (reduces false positives)
**Effort:** LOW (20 minutes)

**Available Data:**
```
From ABOG_CREDENTIAL_DETECTION_ANALYSIS.md:
- 99.5% detection rate (70,487/70,848 physicians)
- 84.2% MD holders (66,855)
- 4.6% DO holders (3,632)
- 361 PhD holders (non-medical)
- 49 International MDs (MBBS variants)

From data/config/suffix_degree_map.csv:
MD,MD
M.D.,MD
DO,DO
D.O.,DO
[Comprehensive normalization table]
```

**Implementation:**
```r
# Load suffix map
suffix_map <- read_csv("data/config/suffix_degree_map.csv")

# Normalize ABOG credentials
abog$credential_clean <- suffix_map$canon[match(abog$suffix, suffix_map$raw)]

# Normalize NPPES credentials (from "Provider Credential Text")
nppes$credential_clean <- suffix_map$canon[match(nppes$credential, suffix_map$raw)]

# Soft blocking rule (not hard block - some physicians switch or don't report)
if (!is.na(abog_credential) && !is.na(nppes_credential)) {
  if (abog_credential != nppes_credential) {
    confidence_score <- confidence_score * 0.8  # 20% penalty
    add_flag("credential_mismatch")
  }
}
```

---

### 5. ⚠️ MEDIUM PRIORITY - Middle Name Matching
**Status:** PARTIALLY IMPLEMENTED (Phase 1 Stage 2 uses initials)
**Impact:** +1-2% (estimated 794-1,588 matches)
**Effort:** LOW (modify existing SQL)

**Current Implementation:**
```sql
-- Stage 2: Exact Last + Exact First Initial + State (line 359)
WHERE a.last_name = n.last_name
  AND SUBSTRING(a.first_name, 1, 1) = SUBSTRING(n.first_name, 1, 1)
  AND a.state = n.practice_state
```

**Enhanced Implementation:**
```sql
-- Stage 2A: Exact Last + First Initial + Middle Initial + State
WHERE a.last_name = n.last_name
  AND SUBSTRING(a.first_name, 1, 1) = SUBSTRING(n.first_name, 1, 1)
  AND (
    a.middle_name IS NULL OR n.middle_name IS NULL OR
    SUBSTRING(a.middle_name, 1, 1) = SUBSTRING(n.middle_name, 1, 1)
  )
  AND a.state = n.practice_state

-- Stage 2B: Exact Last + Exact First + Middle Initial
WHERE a.last_name = n.last_name
  AND a.first_name = n.first_name
  AND (
    a.middle_name IS NULL OR n.middle_name IS NULL OR
    SUBSTRING(a.middle_name, 1, 1) = SUBSTRING(n.middle_name, 1, 1)
  )

-- Stage 2C: Full middle name match (highest confidence)
WHERE a.last_name = n.last_name
  AND a.first_name = n.first_name
  AND a.middle_name = n.middle_name
  AND a.middle_name IS NOT NULL
```

**Evidence:** Prior 90% file includes methods `medicare_part_b_middle` and `physician_compare_2024_middle`

---

### 6. ✅ MEDIUM PRIORITY - Expand Nickname Dictionary
**Status:** READY (R/nickname_system.R exists)
**Impact:** +0.5-1% (397-794 matches)
**Effort:** LOW (already implemented, needs integration)

**Current Implementation:**
```r
# Line 138: KARI = KAREN (WRONG!)
nickname_dict <- list(
  "KARI" = c("KAREN"),  # ❌ INCORRECT
  ...
)
```

**Enhanced from R/nickname_system.R:**
```r
source("R/nickname_system.R")
nickname_dict <- create_nickname_dictionary(verbose = TRUE)

# Provides comprehensive mappings:
# ROBERT -> BOB, BOBBY, ROB, ROBBIE, BERT
# WILLIAM -> BILL, BILLY, WILL, WILLY, LIAM
# ELIZABETH -> LIZ, BETH, BETSY, BETTY, LISA
# + 50+ more common physician names
```

**Integration:**
Replace Phase 1 Stage 2 (line 354-381) with expanded nickname lookups

---

### 7. ✅ HIGH PRIORITY - Use Joe Guido Multi-Stage Matching
**Status:** CODE EXISTS (R/02.3-nber_nppes_joe_guido.R)
**Impact:** +2-3% (1,588-2,382 matches from name variations)
**Effort:** MEDIUM (2 hours to integrate)

**Joe Guido Methodology:**
```r
# Handles hyphenated last names
last_name_1 = "SMITH-JONES"  # Full hyphenated
last_name_2 = "JONES"        # Second part
last_name_3 = "SMITH"        # First part

# Handles multiple first names / middle names
first_name_1 = "JOHN"        # Primary first name
first_name_2 = "MICHAEL"     # First part of middle name
first_name_3 = "ROBERT"      # Second part of middle name
first_name_4 = "JACK"        # Nickname expansion
first_name_5 = ""            # Reserved for additional

# Creates multiple matching permutations:
# JOHN MICHAEL SMITH-JONES can match:
# - JOHN SMITH-JONES
# - JOHN JONES
# - JOHN SMITH
# - JACK SMITH-JONES (nickname)
# - MICHAEL JONES (middle name as first)
```

**Implementation:**
```r
source("R/02.3-nber_nppes_joe_guido.R")
# Integrate standardize_names_for_matching_jg() function
# Apply in Phase 1 for name variation handling
```

---

### 8. ❌ LOW PRIORITY - State License Cross-Reference
**Status:** NOT READY
**Impact:** +2-3% (estimated 1,588-2,382 matches)
**Effort:** HIGH (requires external data acquisition)

**Files Found:**
- R/pipeline/pipeline_cli.R
- R/washington_license_retirement_pipeline.R

**Issues:**
- ❌ State medical board data not integrated into matching pipeline
- ❌ Would require downloading from 50+ state medical boards
- ❌ Data formats inconsistent across states
- ✅ State standardization available (R/03.4-state_standardization_module.R)

**Recommendation:** DEFER until basic matching improvements exhausted

---

### 9. ⚠️ MEDIUM PRIORITY - NPI Registry API
**Status:** POSSIBLE FOR UNMATCHED ONLY
**Impact:** +1-2% (794-1,588 matches)
**Effort:** HIGH (API rate limits, cost)

**Considerations:**
- ✅ Can query NPI Registry API for exact name matches
- ❌ Rate limited (requires delays between requests)
- ⚠️ Should only use for HIGH-VALUE unmatched physicians
- ✅ Good for final cleanup after all other methods

**Implementation Strategy:**
```r
# After all phases complete, for unmatched physicians:
# 1. Filter to high-value (recent graduates, subspecialists)
# 2. Query NPI Registry API with exact name + state
# 3. Apply strict validation (require 3+ matching fields)
# 4. Mark as "npi_api_lookup" method with confidence 0.85
```

---

### 10. ❌ BANNED - Jaro-Winkler Similarity
**Status:** NEVER USE AGAIN
**Impact:** -23% coverage loss + CATASTROPHIC FALSE POSITIVES
**Evidence:** docs/JARO_WINKLER_FAILURE_ANALYSIS.md

**Disasters:**
```
KARI L. EVANS -> MARILYNN EVANS (78.3% confidence - WRONG!)
DOUG J. CHUNG -> DANIELA CHUNG (gender mismatch - WRONG!)
```

**Mandate:** All matching MUST use exact hierarchies only

---

### 11. ❌ NOT AVAILABLE - Name Frequency Tables
**Status:** TABLE NOT FOUND
**Impact:** Cannot implement without data
**Evidence:**
```bash
$ duckdb nber_my_duckdb.duckdb "SELECT COUNT(*) FROM credentials.name_freq"
Error: Table does not exist
```

**Recommendation:**
- ❌ User mentioned 70,194 entries in `credentials.name_freq` but table doesn't exist
- ⚠️ Could build from scratch using SSA baby names data, but LOW PRIORITY
- ✅ Rare name matching can be achieved with other methods (exact + middle name)

---

### 12. ✅ MEDIUM PRIORITY - ABMS Certification
**Status:** DATA AVAILABLE IN ABOG CSV
**Impact:** +0% matches but improves confidence scoring
**Effort:** LOW (20 minutes)

**Available Fields:**
```
certStatus: "Non-Expiring", "Expired", "Active"
mocStatus: "Not required at this time", "Current", "Expired"
sub1certStatus: Subspecialty certification status
```

**Implementation:**
```r
# Use certification as confidence booster
if (!is.na(abog$certStatus) && abog$certStatus == "Non-Expiring") {
  confidence_score <- confidence_score * 1.05  # 5% boost for lifetime cert
}

# Validate subspecialty matches against certification
if (match_method == "subspecialist_medicare") {
  if (is.na(abog$sub1certStatus) || abog$sub1certStatus == "Expired") {
    confidence_score <- confidence_score * 0.9  # 10% penalty
    add_flag("subspecialty_cert_missing")
  }
}
```

---

## Prioritized Implementation Plan

### Phase A: Quick Wins (1 day) - Target: +3-5% coverage

**Priority 1: Fix Name Parsing (10 minutes)**
- Use pre-parsed `first`, `last`, `middle` columns
- Estimated impact: +0.3% (250 matches)

**Priority 2: Integrate 90% Historical File (2 hours)**
- Load `abog_npi_90_PERCENT_FINAL_20251002_034237.csv`
- Filter confidence ≥ 0.85
- Validate against current NPPES
- Estimated impact: +15-20% (11,909-15,879 matches)

**Priority 3: Add Gender Soft Blocking (30 minutes)**
- Use {gender} package for first name gender inference
- Penalize confidence for gender mismatches
- Estimated impact: +0% matches, prevents false positives

**Priority 4: Add MD/DO Soft Blocking (20 minutes)**
- Use `data/config/suffix_degree_map.csv`
- Penalize credential mismatches
- Estimated impact: +0% matches, improves precision

**Expected Result:** 67.1% → 83-90% coverage

---

### Phase B: Name Variation Expansion (1 day) - Target: +2-4% coverage

**Priority 5: Expand Nickname Dictionary (1 hour)**
- Integrate R/nickname_system.R
- Replace hardcoded nicknames
- Estimated impact: +0.5-1% (397-794 matches)

**Priority 6: Add Middle Name Matching (2 hours)**
- Implement 3 new stages (initial, exact+initial, exact+full)
- Estimated impact: +1-2% (794-1,588 matches)

**Priority 7: Integrate Joe Guido Multi-Stage (2 hours)**
- Hyphenated last names (SMITH-JONES)
- Multiple first names
- Estimated impact: +2-3% (1,588-2,382 matches)

**Expected Result:** 83-90% → 86-95% coverage

---

### Phase C: API Cleanup (optional) - Target: +1-2% coverage

**Priority 8: NPI Registry API for Unmatched (4 hours)**
- Query high-value unmatched only
- Strict validation required
- Estimated impact: +1-2% (794-1,588 matches)

**Expected Result:** 86-95% → 88-97% coverage

---

## Implementation Code Template

```r
#!/usr/bin/env Rscript
# ABOG-NPI Enhanced Matching Pipeline v2.0
# Implements all Quick Win improvements

library(dplyr)
library(readr)
library(DBI)
library(duckdb)
library(gender)

# PHASE 0: Load Historical Validated Matches (NEW!)
historical_matches <- read_csv("data/canonical_abog_npi/abog_npi_90_PERCENT_FINAL_20251002_034237.csv") %>%
  filter(
    !is.na(match_method),
    match_method != "NA",
    !is.na(npi),
    confidence_score >= 0.85
  ) %>%
  select(abog_id, npi, match_method, confidence_score_historical = confidence_score)

# Load ABOG with CORRECTED name parsing
abog <- read_csv(abog_path) %>%
  mutate(
    # FIX: Use pre-parsed columns directly!
    first_name = toupper(trimws(first)),
    last_name = toupper(trimws(last)),
    middle_name = toupper(trimws(middle)),
    # Clean state
    state = toupper(trimws(state))
  )

# Add gender inference (soft blocking)
abog_gender <- gender(unique(abog$first_name), method = "ssa") %>%
  select(name, gender_abog = gender, prob_abog = proportion_female)

abog <- abog %>%
  left_join(abog_gender, by = c("first_name" = "name"))

# Add credential normalization (soft blocking)
suffix_map <- read_csv("data/config/suffix_degree_map.csv")
abog$credential_clean <- suffix_map$canon[match(abog$suffix, suffix_map$raw)]

# Connect to NBER DuckDB
con <- dbConnect(duckdb(),
                 dbdir = "/Volumes/MufflySamsung 1/nber_my_duckdb.duckdb",
                 read_only = FALSE)  # MUST be FALSE for temp tables!

# Create temp table
dbWriteTable(con, "temp_abog", abog, overwrite = TRUE, temporary = TRUE)

# PHASE 1: Enhanced exact matching with soft blocking
phase1_sql <- "
WITH nppes_with_gender AS (
  SELECT
    npi,
    UPPER(TRIM(\"Provider First Name\")) AS first_name,
    UPPER(TRIM(\"Provider Last Name (Legal Name)\")) AS last_name,
    UPPER(TRIM(\"Provider Middle Name\")) AS middle_name,
    UPPER(TRIM(\"Provider Business Practice Location Address State Name\")) AS practice_state,
    UPPER(TRIM(\"Provider Credential Text\")) AS credential
  FROM nppes_data
  WHERE \"Entity Type Code\" = '1'
),
matches AS (
  -- Stage 1: Exact first + last + state
  SELECT
    a.abog_id,
    n.npi,
    'exact_with_state' as method,
    0.98 as base_confidence,
    a.gender_abog,
    n.first_name as nppes_first
  FROM temp_abog a
  INNER JOIN nppes_with_gender n
    ON a.first_name = n.first_name
    AND a.last_name = n.last_name
    AND a.state = n.practice_state

  UNION ALL

  -- Stage 2: Exact first + last (no state)
  SELECT
    a.abog_id,
    n.npi,
    'exact_no_state' as method,
    0.95 as base_confidence,
    a.gender_abog,
    n.first_name as nppes_first
  FROM temp_abog a
  INNER JOIN nppes_with_gender n
    ON a.first_name = n.first_name
    AND a.last_name = n.last_name

  -- Add more stages here...
)
SELECT * FROM matches
"

matches_phase1 <- dbGetQuery(con, phase1_sql)

# Apply soft blocking adjustments in R
matches_phase1 <- matches_phase1 %>%
  # Gender soft blocking
  left_join(
    gender(unique(matches_phase1$nppes_first), method = "ssa") %>%
      select(name, gender_nppes = gender, prob_nppes = proportion_female),
    by = c("nppes_first" = "name")
  ) %>%
  mutate(
    # Gender mismatch penalty
    confidence_score = case_when(
      is.na(gender_abog) | is.na(gender_nppes) ~ base_confidence,
      abs(prob_abog - prob_nppes) > 0.7 ~ base_confidence * 0.5,  # 50% penalty
      TRUE ~ base_confidence
    ),
    # Flag gender mismatches
    warning_flags = case_when(
      !is.na(gender_abog) & !is.na(gender_nppes) & abs(prob_abog - prob_nppes) > 0.7 ~ "GENDER_MISMATCH",
      TRUE ~ NA_character_
    )
  )

# Continue with Phase 2, 3, 4, 5...

# Final deduplication and merging with historical
final_results <- bind_rows(
  historical_matches %>% mutate(source = "historical_validated"),
  matches_phase1 %>% mutate(source = "current_pipeline"),
  matches_phase2 %>% mutate(source = "current_pipeline"),
  # ... other phases
) %>%
  group_by(abog_id) %>%
  arrange(desc(confidence_score)) %>%
  slice(1) %>%
  ungroup()

# Write output
write_csv(final_results, sprintf("data/canonical_abog_npi/abog_npi_enhanced_%s.csv",
                                format(Sys.time(), "%Y%m%d_%H%M%S")))

cat(sprintf("
Final Results:
--------------
Total matches: %s (%.1f%%)
From historical: %s
From current pipeline: %s
Average confidence: %.3f
",
format(nrow(final_results), big.mark = ","),
100 * nrow(final_results) / nrow(abog),
sum(final_results$source == "historical_validated"),
sum(final_results$source == "current_pipeline"),
mean(final_results$confidence_score)
))
```

---

## Expected Outcomes

### Conservative Estimate (Phase A only)
- **Starting:** 67.1% (53,249 matches)
- **After Phase A:** 83% (65,900 matches)
- **Gain:** +15.9% (+12,651 matches)

### Moderate Estimate (Phase A + B)
- **Starting:** 67.1% (53,249 matches)
- **After Phase A+B:** 90% (71,457 matches)
- **Gain:** +22.9% (+18,208 matches)

### Optimistic Estimate (Phase A + B + C)
- **Starting:** 67.1% (53,249 matches)
- **After all phases:** 95% (75,427 matches)
- **Gain:** +27.9% (+22,178 matches)

---

## Risk Mitigation

### Validation Requirements

1. **Historical Match Validation:**
   - Cross-check all historical NPIs against current NPPES
   - Flag NPIs that are now deactivated
   - Verify name fields still match

2. **Confidence Score Calibration:**
   - Historical confidence scores may use different methodology
   - Recalibrate to current scale if needed
   - Document any score adjustments

3. **Gender Blocking Exceptions:**
   - Allow manual override for known edge cases
   - Log all gender mismatch warnings for review
   - Consider unisex names (e.g., "Pat", "Chris")

4. **Credential Mismatch Handling:**
   - Some physicians switch between MD/DO in different records
   - International physicians may have multiple credential formats
   - Apply as soft penalty, not hard block

---

## Next Steps

1. ✅ Implement Phase A Quick Wins (1 day)
2. ✅ Validate results against known test cases
3. ✅ Generate comparison report (before/after coverage)
4. ⚠️ If Phase A achieves <85% coverage, proceed to Phase B
5. ⚠️ If Phase A+B achieves <90% coverage, proceed to Phase C
6. ✅ Document final methodology for reproducibility

---

## References

- Previous 90% success: `data/canonical_abog_npi/abog_npi_90_PERCENT_FINAL_20251002_034237.csv`
- Joe Guido method: `R/02.3-nber_nppes_joe_guido.R`
- Enhanced matching: `R/match_abog_to_npi_enhanced.R`
- Jaro-Winkler failure analysis: `docs/JARO_WINKLER_FAILURE_ANALYSIS.md`
- Credential detection: `docs/ABOG_CREDENTIAL_DETECTION_ANALYSIS.md`
- Nickname system: `R/nickname_system.R`
- State standardization: `R/03.4-state_standardization_module.R`
- Matching utilities: `R/matching_utils.R`
