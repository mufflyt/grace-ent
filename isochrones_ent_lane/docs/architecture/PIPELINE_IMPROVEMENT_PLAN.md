# Pipeline Improvement & Quality Assurance Plan
**Date:** 2025-10-06
**Status:** Comprehensive Analysis of User Questions

---

## 📋 User Questions & Action Items

### **1. SHA-256 Hashing for Output Caching**
**Question:** "Will this output of the pipeline data be cached in a SHA-hashed file so it does not need to be rerun again?"

**Current Status:** ❌ NOT IMPLEMENTED
- Pipeline controller *mentions* SHA-256 hashes in documentation
- No actual implementation in production pipeline
- No input change detection
- No automatic cache invalidation

**Recommendation: Implement Content-Addressable Caching**

```r
#' Generate SHA-256 hash for pipeline inputs
#' @param abog_data ABOG physician data
#' @param config Pipeline configuration
#' @return SHA-256 hash string
generate_pipeline_hash <- function(abog_data, config) {
  # Combine all inputs that affect output
  input_fingerprint <- list(
    abog_hash = digest::digest(abog_data, algo = "sha256"),
    config_hash = digest::digest(config, algo = "sha256"),
    nppes_version = DBI::dbGetQuery(con, "SELECT MAX(enumeration_date) FROM nppes_registry")[[1]],
    medicare_version = DBI::dbGetQuery(con, "SELECT MAX(year) FROM medicare_part_b")[[1]]
  )

  digest::digest(input_fingerprint, algo = "sha256")
}

#' Check if cached output exists for current inputs
#' @param input_hash SHA-256 hash of inputs
#' @return Path to cached file or NULL
find_cached_output <- function(input_hash) {
  cache_dir <- here::here("data", "cache")
  cache_file <- file.path(cache_dir, paste0("pipeline_", input_hash, ".csv"))

  if (file.exists(cache_file)) {
    cat(sprintf("✅ Found cached results: %s\n", basename(cache_file)))
    return(cache_file)
  }
  return(NULL)
}
```

**Benefits:**
- Skip entire pipeline if inputs haven't changed
- Automatic cache invalidation when data/config updates
- Reproducibility: same inputs always produce same hash
- Easy to debug: hash identifies exact input state

---

### **2. NPI Luhn Check Validation**
**Question:** "npi is a 10-digit string that must pass the Luhn check. Are we testing the final output of the pipeline for legitimate NPI numbers with a Luhn check? We should."

**Current Status:** ❌ NOT IMPLEMENTED
- No Luhn algorithm validation
- No NPI format validation
- No check for invalid/corrupted NPIs

**Recommendation: Add NPI Validation Function**

```r
#' Validate NPI using Luhn algorithm
#' @param npi Character vector of NPI numbers
#' @return Logical vector indicating valid NPIs
validate_npi_luhn <- function(npi) {
  sapply(npi, function(x) {
    # Check format: exactly 10 digits
    if (is.na(x) || nchar(x) != 10 || !grepl("^[0-9]{10}$", x)) {
      return(FALSE)
    }

    # Luhn algorithm
    digits <- as.integer(strsplit(x, "")[[1]])

    # Double every second digit from right
    for (i in seq(9, 1, -2)) {
      digits[i] <- digits[i] * 2
      if (digits[i] > 9) digits[i] <- digits[i] - 9
    }

    # Sum all digits
    total <- sum(digits)

    # Valid if sum is divisible by 10
    return(total %% 10 == 0)
  })
}

#' Validate final pipeline output NPIs
#' @param final_matches Pipeline output dataframe
#' @return Dataframe with validation results
validate_final_npis <- function(final_matches) {
  cat("🔍 Validating NPI numbers with Luhn algorithm...\n")

  final_matches <- final_matches %>%
    mutate(
      npi_format_valid = nchar(npi) == 10 & grepl("^[0-9]{10}$", npi),
      npi_luhn_valid = validate_npi_luhn(npi),
      npi_valid = npi_format_valid & npi_luhn_valid
    )

  # Report validation results
  n_total <- nrow(final_matches)
  n_valid <- sum(final_matches$npi_valid, na.rm = TRUE)
  n_invalid <- sum(!final_matches$npi_valid, na.rm = TRUE)

  cat(sprintf("  Total NPIs: %s\n", format(n_total, big.mark = ",")))
  cat(sprintf("  ✅ Valid: %s (%.1f%%)\n",
              format(n_valid, big.mark = ","),
              100 * n_valid / n_total))

  if (n_invalid > 0) {
    cat(sprintf("  ❌ Invalid: %s (%.1f%%)\n",
                format(n_invalid, big.mark = ","),
                100 * n_invalid / n_total))

    # Show examples of invalid NPIs
    invalid_examples <- final_matches %>%
      filter(!npi_valid) %>%
      select(abog_id, physician_name, npi, match_method) %>%
      head(10)

    cat("\n  Examples of invalid NPIs:\n")
    print(invalid_examples)
  }

  return(final_matches)
}
```

**Implementation:**
1. Add Luhn validation before writing final output
2. Flag invalid NPIs for manual review
3. Report validation statistics in pipeline summary
4. Consider rejecting matches with invalid NPIs (configurable)

---

### **3. Assumptions to Validate with Data**
**Question:** "What assumptions are we making that should be tested with data now that we have enough data to do so?"

**Current Assumptions Requiring Validation:**

#### **A. Name Matching Assumptions**
```r
# ASSUMPTION: Middle names improve match accuracy
# TEST: Compare match precision with/without middle names
validate_middle_name_assumption <- function() {
  matches_with_middle <- phase1_matches %>%
    filter(!is.na(middle) & middle != "")

  matches_without_middle <- phase1_matches %>%
    filter(is.na(middle) | middle == "")

  # Compare false positive rates
  # Manual review of random samples from each group
}

# ASSUMPTION: Recent graduates (<5 years) are easier to match
# TEST: Compare match rates by career stage
validate_career_stage_assumption <- function() {
  match_rates_by_stage <- abog_data %>%
    left_join(final_matches, by = "abog_id") %>%
    mutate(
      years_since_cert = as.integer(format(Sys.Date(), "%Y")) - cert_year,
      career_stage = case_when(
        years_since_cert < 5 ~ "Recent (0-5y)",
        years_since_cert < 15 ~ "Mid-career (5-15y)",
        TRUE ~ "Senior (15+y)"
      )
    ) %>%
    group_by(career_stage) %>%
    summarise(
      total = n(),
      matched = sum(!is.na(npi)),
      match_rate = matched / total
    )

  print(match_rates_by_stage)
}
```

#### **B. Geographic Assumptions**
```r
# ASSUMPTION: Physicians practice in same state as certification
# TEST: Compare cert state to practice state
validate_geographic_stability <- function() {
  geographic_mobility <- final_matches %>%
    inner_join(
      nppes_registry %>% select(npi, provider_state),
      by = "npi"
    ) %>%
    mutate(
      same_state = (cert_state == provider_state)
    ) %>%
    summarise(
      same_state_pct = mean(same_state, na.rm = TRUE),
      moved_states = sum(!same_state, na.rm = TRUE)
    )

  print(geographic_mobility)
}

# ASSUMPTION: Urban physicians easier to match than rural
# TEST: Compare match rates by RUCC codes
validate_urban_rural_assumption <- function() {
  # Join with RUCC (Rural-Urban Continuum Codes)
  # Compare match rates across continuum
}
```

#### **C. Data Quality Assumptions**
```r
# ASSUMPTION: Medicare Part B data is more reliable than Part D
# TEST: Compare match precision from each source
validate_medicare_source_reliability <- function() {
  medicare_comparison <- final_matches %>%
    filter(match_method %in% c("phase2_medicare_partb", "phase2_medicare_partd")) %>%
    group_by(match_method) %>%
    summarise(
      count = n(),
      avg_confidence = mean(confidence, na.rm = TRUE),
      # Add manual validation sample
    )

  print(medicare_comparison)
}

# ASSUMPTION: Subspecialty codes correctly identify FPMRS/GO/MFM/etc
# TEST: Cross-validate subspecialty classifications
validate_subspecialty_classification <- function() {
  subspecialty_accuracy <- final_matches %>%
    group_by(subspecialty_name) %>%
    summarise(
      count = n(),
      has_matching_taxonomy = sum(taxonomy_matches_subspecialty) / n(),
      has_matching_procedures = sum(procedures_match_subspecialty) / n()
    )

  print(subspecialty_accuracy)
}
```

#### **D. Matching Strategy Assumptions**
```r
# ASSUMPTION: Ensemble matching improves precision over single-source
# TEST: Compare false positive rates
validate_ensemble_improvement <- function() {
  single_source <- phase2_matches  # Medicare only
  ensemble <- phase3_matches       # Multi-source

  # Compare precision on manually validated sample
}

# ASSUMPTION: Blocking filters improve precision without losing recall
# TEST: Measure recall loss from credential blocking
validate_blocking_strategy <- function() {
  blocked_matches <- all_matches %>%
    filter(credential_blocked == TRUE)

  # Manual review: how many blocked matches were actually correct?
}
```

#### **E. Temporal Assumptions**
```r
# ASSUMPTION: Practice patterns change over time
# TEST: Compare match characteristics by practice year
validate_temporal_patterns <- function() {
  temporal_trends <- medicare_claims %>%
    group_by(year) %>%
    summarise(
      avg_claim_count = mean(claim_count),
      providers_active = n_distinct(npi),
      avg_specialty_concentration = mean(subspecialty_pct)
    )

  # Look for trends that affect matching strategy
}
```

**Recommended Testing Framework:**
1. Create `R/validation/assumption_tests.R` with all validation functions
2. Generate validation report showing which assumptions hold
3. Update matching strategy based on empirical findings
4. Re-run validation quarterly as data grows

---

### **4. Code Cleanup & Readability**
**Question:** "How can we improve the pipeline code to make it cleaner and more understandable to someone who is new to the team?"

**Current Issues:**
- 1,599 line monolithic file
- Mixed concerns (matching logic + SQL + config)
- Limited inline documentation
- Magic numbers scattered throughout
- No clear module boundaries

**Recommended Refactoring:**

#### **A. Modular Architecture**
```
R/pipeline/
├── 00_pipeline_controller.R          # Main orchestrator
├── 01_data_loading.R                 # ABOG + database connection
├── 02_phase_orchestrator.R           # Phase execution logic
├── phases/
│   ├── phase0_historical.R
│   ├── phase1_exact_matching.R
│   ├── phase2_medicare.R
│   ├── phase3_ensemble.R
│   ├── phase4_subspecialty.R
│   ├── phase5_temporal.R
│   ├── phase6_npi_registry.R
│   ├── phase7_historical_timeline.R
│   └── phase8_validation.R
├── utils/
│   ├── safe_bind_matches.R          # Schema utilities
│   ├── npi_validation.R             # Luhn check, format
│   ├── confidence_scoring.R         # Match confidence
│   ├── blocking_filters.R           # Credential/gender blocking
│   └── logging_helpers.R            # Progress logging
├── sql/
│   ├── phase2_medicare.sql
│   ├── phase3_ensemble.sql
│   └── phase4_subspecialty.sql
└── validation/
    ├── assumption_tests.R
    ├── npi_luhn_check.R
    └── output_quality_checks.R
```

#### **B. Self-Documenting Code Patterns**
```r
# BEFORE: Magic numbers
filter(confidence >= 0.85)
WHERE years_since_cert < 5

# AFTER: Named constants with rationale
MIN_CONFIDENCE_THRESHOLD <- 0.85  # Validated: 95% precision above this threshold
RECENT_GRADUATE_YEARS <- 5        # ABOG definition of early career

filter(confidence >= MIN_CONFIDENCE_THRESHOLD)
WHERE years_since_cert < !!RECENT_GRADUATE_YEARS
```

#### **C. Function Contracts (Roxygen2)**
```r
#' Phase 2: Medicare Part B Integration
#'
#' Matches ABOG physicians to NPIs using Medicare Part B billing data.
#' Uses fuzzy name matching with OB/GYN taxonomy filtering and credential blocking.
#'
#' @param abog_data Dataframe with columns: abog_id, physician_name, subspecialty_name
#' @param existing_matches Dataframe with previously matched physicians (for deduplication)
#' @param con DuckDB connection with medicare_part_b_* tables
#'
#' @return Dataframe with columns:
#'   - abog_id: ABOG physician ID
#'   - npi: Matched NPI (character, 10 digits, Luhn-valid)
#'   - match_method: "phase2_medicare_partb"
#'   - confidence: 0.0-1.0 score
#'   - [additional context fields]
#'
#' @details
#' Matching strategy:
#'   1. Parse ABOG names (first, middle, last)
#'   2. Search Medicare Part B 2017-2023 with fuzzy string similarity
#'   3. Filter to OB/GYN taxonomy codes (207V%)
#'   4. Block credential mismatches (MD≠DO)
#'   5. Calculate confidence from name similarity + taxonomy match
#'
#' Performance:
#'   - Expected runtime: ~17 minutes for 79K physicians
#'   - Expected matches: ~12K (15% of unmatched)
#'
#' @seealso
#'   - SQL query: R/pipeline/sql/phase2_medicare.sql
#'   - Blocking filters: R/pipeline/utils/blocking_filters.R
#'
#' @export
run_phase2_medicare_integration <- function(abog_data, existing_matches, con) {
  # Implementation
}
```

#### **D. Clear Error Messages**
```r
# BEFORE: Cryptic error
if (nrow(result) == 0) stop("No matches")

# AFTER: Actionable error with context
if (nrow(result) == 0) {
  stop(sprintf(
    "Phase 2 returned 0 matches. Diagnostics:
    - Unmatched physicians: %d
    - Medicare Part B years available: %s
    - OB/GYN providers in NPPES: %d

    Possible causes:
    1. Medicare tables empty (check data/0-Download/)
    2. All physicians already matched in Phase 1
    3. Taxonomy codes not loading (check NPPES registry)

    Debug: Run validate_medicare_tables() for detailed report",
    nrow(abog_unmatched),
    paste(available_years, collapse = ", "),
    obgyn_count
  ))
}
```

#### **E. Configuration-Driven Design**
```yaml
# config/pipeline_matching.yaml
phases:
  phase2_medicare:
    enabled: true
    years_to_search: [2017, 2018, 2019, 2020, 2021, 2022, 2023]
    taxonomy_codes: ["207V%", "208D%"]  # OB/GYN, Urology
    min_name_similarity: 0.75
    apply_credential_blocking: true
    min_confidence: 0.60

  phase3_ensemble:
    enabled: true
    min_sources: 2          # Require ≥2 sources to agree
    min_total_score: 50     # Confidence threshold
    weight_medicare: 0.30
    weight_open_payments: 0.20
    weight_physician_compare: 0.15
```

```r
# Load config-driven parameters
config <- yaml::read_yaml("config/pipeline_matching.yaml")

if (config$phases$phase2_medicare$enabled) {
  phase2_matches <- run_phase2(
    years = config$phases$phase2_medicare$years_to_search,
    taxonomy_codes = config$phases$phase2_medicare$taxonomy_codes,
    min_similarity = config$phases$phase2_medicare$min_name_similarity
  )
}
```

#### **F. Comprehensive Logging**
```r
# BEFORE: Silent operations
medicare_matches <- dbGetQuery(con, query)

# AFTER: Narrated pipeline
cat(sprintf("
🔍 PHASE 2: Medicare Part B Integration

  📊 Input State:
    • Physicians to match: %s
    • Medicare years: %s
    • Taxonomy codes: %s

  ⚙️  Matching Strategy:
    • Fuzzy name matching (min similarity: %.2f)
    • Credential blocking: %s
    • Min confidence: %.2f

  ⏱️  Starting search...\n",
  format(nrow(abog_unmatched), big.mark = ","),
  paste(config$years, collapse = ", "),
  paste(config$taxonomy_codes, collapse = ", "),
  config$min_similarity,
  ifelse(config$credential_blocking, "ENABLED", "disabled"),
  config$min_confidence
))

start_time <- Sys.time()
medicare_matches <- dbGetQuery(con, query)
elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

cat(sprintf("
  ✅ Search complete: %.1f minutes

  📈 Results:
    • Raw matches: %s
    • After blocking: %s (%.1f%% retained)
    • Avg confidence: %.3f

",
  elapsed,
  format(nrow(medicare_raw), big.mark = ","),
  format(nrow(medicare_matches), big.mark = ","),
  100 * nrow(medicare_matches) / nrow(medicare_raw),
  mean(medicare_matches$confidence)
))
```

---

## 🎯 Implementation Priority

### **Phase 1: Critical Validations (Week 1)**
- [ ] Add NPI Luhn validation to final output
- [ ] Implement SHA-256 caching system
- [ ] Create assumption validation suite
- [ ] Generate baseline metrics report

### **Phase 2: Code Refactoring (Week 2)**
- [ ] Extract phases into separate modules
- [ ] Move SQL to external files
- [ ] Add comprehensive Roxygen2 docs
- [ ] Create configuration YAML

### **Phase 3: Testing & Documentation (Week 3)**
- [ ] Write unit tests for each phase
- [ ] Create onboarding guide for new developers
- [ ] Add architecture diagrams
- [ ] Publish internal API documentation

---

## 📊 Success Metrics

**Quality:**
- 100% of NPIs pass Luhn validation
- All assumptions empirically validated
- <1% false positive rate (validated on sample)

**Maintainability:**
- New developer can understand pipeline in <2 hours
- Each phase <200 lines of code
- 100% of functions documented with Roxygen2

**Performance:**
- Cache hit rate >80% for repeated runs
- No duplicate API calls (SHA-based dedup)
- Pipeline runs in <30 minutes end-to-end

---

*Pipeline Improvement Plan: 2025-10-06*
*Next Review: After Phase 1 Implementation*
