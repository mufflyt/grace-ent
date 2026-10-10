# Race/Ethnicity Ordering Enforcement Summary

**Date**: 2025-10-10
**Status**: ✅ **COMPLETE** - Standard ordering enforced across manuscript

## What Was Done

### 1. Canonical Function Created
**File**: `manuscript/R/acs_race_ethnicity_standard_order.R`

Three functions enforce standard Census Bureau / OMB ordering:
- `get_acs_race_standard_order()` - Returns ordered vector
- `apply_acs_race_order(data)` - Applies to data frames
- `validate_acs_race_categories(data)` - Validates completeness

### 2. Manuscript Methods Updated
**File**: `manuscript/R/09_manuscript_text.R` (lines 515-520)

Added explicit statement in **Demographic Analysis** section:

> "Race and ethnicity were classified according to U.S. Census Bureau standards following the B01001A–I table presentation order. **All tables and figures present race/ethnicity categories in standard order**: (1) White alone, (2) Black or African American alone, (3) American Indian and Alaska Native alone, (4) Asian alone, (5) Native Hawaiian and Other Pacific Islander alone, (6) Some other race alone, (7) Two or more races, (8) White alone not Hispanic or Latino, and (9) Hispanic or Latino (any race)."

### 3. Figure Functions Updated
**Files**:
- `manuscript/R/create_female_race_figure_with_moe.R`
- `manuscript/R/get_acs_female_race.R`
- `manuscript/R/compute_female_race_shares.R`

All now use `apply_acs_race_order()` to enforce standard ordering.

### 4. Documentation Created
**Files**:
- `manuscript/docs/ACS_RACE_ETHNICITY_STANDARD_ORDER.md` - Full usage guide
- `manuscript/docs/RACE_ETHNICITY_ORDERING_ENFORCEMENT_SUMMARY.md` - This file

## Standard Order (B01001A–I)

1. **White alone** (B01001A)
2. **Black or African American alone** (B01001B)
3. **American Indian and Alaska Native alone** (B01001C)
4. **Asian alone** (B01001D)
5. **Native Hawaiian and Other Pacific Islander alone** (B01001E)
6. **Some other race alone** (B01001F)
7. **Two or more races** (B01001G)
8. **White alone, not Hispanic or Latino** (B01001H)
9. **Hispanic or Latino** (B01001I)

## Files Modified

### Manuscript Text
- ✅ `manuscript/R/09_manuscript_text.R` - Added standard ordering statement
- ✅ `manuscript/sections/methods_section.txt` - Regenerated
- ✅ `manuscript/sections/methods_section.Rmd` - Regenerated
- ✅ `manuscript/sections/methods_section_summary.md` - Updated stats

### R Functions
- ✅ `manuscript/R/acs_race_ethnicity_standard_order.R` - NEW canonical function
- ✅ `manuscript/R/create_female_race_figure_with_moe.R` - Uses canonical ordering
- ✅ `manuscript/R/get_acs_female_race.R` - Returns data in standard order
- ✅ `manuscript/R/compute_female_race_shares.R` - Maintains standard order

### Documentation
- ✅ `manuscript/docs/ACS_RACE_ETHNICITY_STANDARD_ORDER.md` - Full guide
- ✅ `manuscript/docs/RACE_ETHNICITY_ORDERING_ENFORCEMENT_SUMMARY.md` - This summary

## How to Use in Future Code

### For Figures

```r
# Source the canonical function
source("manuscript/R/acs_race_ethnicity_standard_order.R")

# Apply to your data
library(dplyr)
data <- data %>%
  apply_acs_race_order()  # Enforces standard ordering

# Create figure - legend will be in correct order
ggplot(data, aes(x = year, y = value, color = race_group)) +
  geom_line()
```

### For Tables

```r
# Apply ordering before creating table
table_data <- table_data %>%
  apply_acs_race_order()

# Use gt, kable, or other table package
library(gt)
gt(table_data) %>%
  # Rows will be in standard Census Bureau order
  tab_header(
    title = "Female Population by Race/Ethnicity"
  )
```

### Validation

```r
# Check if your data has all 9 categories
result <- validate_acs_race_categories(data)

if (!result$all_valid) {
  warning("Missing categories: ", paste(result$missing, collapse = ", "))
}
```

## Testing

```r
# Run tests
source("manuscript/R/acs_race_ethnicity_standard_order.R")

# Should return 9 categories in correct order
order <- get_acs_race_standard_order()
stopifnot(length(order) == 9)
stopifnot(order[1] == "White alone")
stopifnot(order[9] == "Hispanic or Latino")

# Test with data
library(dplyr)
library(tibble)

test_data <- tibble(
  race_group = c("Asian alone", "White alone", "Hispanic or Latino"),
  value = c(10, 20, 15)
)

test_data <- apply_acs_race_order(test_data)
stopifnot(is.factor(test_data$race_group))
stopifnot(levels(test_data$race_group) == order)

cat("✅ All tests passed!\n")
```

## Compliance Checklist

Use this checklist when creating new figures/tables with race/ethnicity data:

- [ ] Source `manuscript/R/acs_race_ethnicity_standard_order.R`
- [ ] Apply `apply_acs_race_order()` to data before plotting/tabling
- [ ] Run `validate_acs_race_categories()` to check completeness
- [ ] Verify figure legend shows all 9 categories in order
- [ ] Verify table rows are in standard order
- [ ] Add note: "Categories ordered following standard Census Bureau / OMB presentation order (B01001A–I)"

## Why This Matters

✅ **Federal compliance**: Matches OMB SPD-15 (pre-2024)
✅ **ACS consistency**: Follows B01001A–I table order
✅ **Research standards**: Aligns with Census publications
✅ **Reviewer expectations**: Immediately recognizable
✅ **Cross-study comparability**: Enables direct comparison
✅ **Manuscript coherence**: Same order in Methods, Results, Figures, Tables

## References

- U.S. Census Bureau. "Understanding and Using American Community Survey Data: What All Data Users Need to Know."
- Office of Management and Budget. "Revisions to OMB's Statistical Policy Directive No. 15" (March 2024)
- ACS Summary Tables B01001A through B01001I documentation

## Contact

Questions? See:
- `manuscript/docs/ACS_RACE_ETHNICITY_STANDARD_ORDER.md` (full guide)
- `manuscript/docs/ACS_METHODS_ENHANCEMENT_SUMMARY.md` (ACS best practices)
- Tyler Muffly

---

**✅ ENFORCEMENT**: This ordering is **MANDATORY** for all manuscript tables and figures displaying race/ethnicity data.
