# Oxygen Template Improvement Plan (50 Functions)

This plan applies the roxygen template captured in `R/read_csv_utf8.R` (title, description, details, params, return, side effects, logging, examples, seealso, family) to 50 targeted functions so their contracts, assumptions, and operational impacts are explicit and consistent across the pipeline.

## 1) Matching accuracy evaluation (`R/evaluate_npi_matching_accuracy.R`)

1. **create_nber_gold_standard** (line 23)
   - Add `@details` describing sampling strategy, taxonomy filtering, and any randomness/seed expectations.
   - Add `@param` notes for `sample_size` bounds and `taxonomy_filter` behavior (NULL vs specific specialty).
   - Add `@return` structure (columns produced, row counts) and `@section Side effects` for any file I/O.
   - Add `@examples` showing a minimal gold-standard build with a fixed seed for reproducibility.

2. **format_nber_for_testing** (line 72)
   - Document expected input schema and normalization steps in `@details`.
   - Specify required columns in `@param` and output tibble columns in `@return`.
   - Add `@section Logging` to note any `message()` usage and `@examples` showing a small input subset.

3. **evaluate_parsing_accuracy** (line 98)
   - Add `@details` explaining scoring metrics and thresholds.
   - Clarify `@param` expected fields (true vs parsed values) and `@return` summary metrics.
   - Add `@examples` with a small synthetic gold standard.

4. **print_accuracy_report** (line 160)
   - Add `@details` with report sections and formatting conventions.
   - Add `@section Side effects` for console output and `@section Logging` for verbosity.
   - Add `@examples` showing printing from a sample evaluation result.

5. **run_npi_accuracy_evaluation** (line 204)
   - Add `@details` outlining end-to-end steps (sample, format, evaluate, report).
   - Specify dependencies and file/data sources in `@section Side effects`.
   - Include `@examples` with optional taxonomy filters.

## 2) Valhalla isochrone generation (`R/valhalla_isochrone_generator.R`)

6. **generate_valhalla_isochrone** (line 38)
   - Add `@details` for request payload, contour units, and response assumptions.
   - Document `@param` units for minutes/distance, expected CRS, and timeouts.
   - Add `@section Side effects` for network calls and `@section Logging` for progress messages.
   - Provide `@examples` with a small coordinate and a mocked Valhalla URL.

7. **generate_valhalla_isochrones_all_bands** (line 257)
   - Add `@details` describing multi-band loop order and error handling.
   - Clarify `@param` for contour list ordering and parallel options (if any).
   - Add `@return` structure (list of sf/tibble outputs) and `@examples`.

8. **test_valhalla_connection** (line 338)
   - Document the health check steps in `@details` and expected status codes.
   - Add `@return` truthy/falsey value specification and `@section Logging` for status output.

## 3) Retired filter strategy (`R/strategy_09_retired_filter.R`)

9. **strategy_09_retired_filter** (line 27)
   - Add `@details` describing retirement heuristics and reference tables used.
   - Specify required `abog_data` schema and `config` keys in `@param`.
   - Add `@return` (filtered tibble + audit columns) and `@examples` with minimal data.

## 4) Physician compare materialization (`R/setup_pc_materialized.R`)

10. **materialize_physician_compare** (line 16)
    - Add `@details` outlining table creation steps and dependencies.
    - Document `@section Side effects` for DuckDB writes and temp tables.
    - Add `@return` for table names created and `@examples` with a sample connection.

11. **drop_materialized_pc** (line 268)
    - Add `@details` on which tables get dropped and safety checks.
    - Add `@section Side effects` for destructive DB operations.

12. **check_pc_status** (line 290)
    - Specify `@return` structure (status tibble/flags) and `@examples` for a running DB.

## 5) PPE ingestion pipeline (`R/ppe_ingest_pipeline.R`)

13. **%||%** (line 14)
    - Add `@details` explaining null/empty handling semantics.
    - Document return behavior for empty strings vs NULL.

14. **load_ppe_config** (line 19)
    - Add `@details` for config precedence rules and defaults.
    - Add `@section Side effects` for YAML reads and environment access.

15. **http_head_ok** (line 74)
    - Clarify `@param` timeouts/user agent and `@return` boolean semantics.
    - Add `@section Side effects` for network calls and `@examples` with a test URL.

16. **fetch_cms_catalog** (line 92)
    - Add `@details` for CMS API endpoint structure and error handling.
    - Include `@section Side effects` (HTTP requests) and logging expectations.

17. **find_ppe_datasets** (line 111)
    - Document regex matching for CMS titles and expected catalog fields.
    - Add `@return` structure (list of dataset descriptors).

18. **select_quarters** (line 162)
    - Add `@details` on priority order and fallback logic.
    - Provide `@examples` with a minimal dataset list.

19. **download_ppe_file** (line 194)
    - Add `@details` covering file naming, retry logic, and size checks.
    - Document `@section Side effects` for file downloads and disk writes.

20. **ensure_duckdb_table** (line 234)
    - Specify table schema expectations and unique key behavior.
    - Add `@section Side effects` for table creation/alteration.

21. **already_loaded** (line 254)
    - Document `@return` criteria for loaded status and required columns.

22. **validate_year** (line 263)
    - Add `@details` about year presence thresholds and failure modes.
    - Document any warnings/errors raised.

23. **ingest_ppe_to_duckdb** (line 301)
    - Add `@details` describing CSV parsing, type coercion, and dedup logic.
    - Include `@section Side effects` for DB writes and `@section Logging` for progress.

24. **compare_years** (line 395)
    - Document comparison metrics (e.g., NPI overlap) and output schema.
    - Add `@examples` for comparing two years.

25. **flag_exits** (line 439)
    - Add `@details` for exit flag criteria and multi-year logic.
    - Document output columns in `@return`.

26. **ppe_run_all_from_yaml** (line 492)
    - Add `@details` outlining the end-to-end pipeline steps.
    - Include `@section Side effects` for network, disk, and DB operations.

## 6) Age-stratified populations (`R/utils_age_stratified_populations.R`)

27. **load_subspecialty_config** (line 22)
    - Document expected YAML schema and defaults in `@details`.
    - Add `@return` structure (list of subspecialty settings).

28. **get_acs_variables_for_subspecialty** (line 31)
    - Add `@details` about ACS variable naming and year sensitivity.
    - Specify `@param` accepted subspecialty labels.

29. **get_subspecialty_population** (line 49)
    - Clarify the aggregation logic and geography level in `@details`.
    - Add `@return` with columns and units.

30. **get_all_subspecialty_populations** (line 121)
    - Document default subspecialty list resolution and caching behavior.

31. **calculate_subspecialty_access** (line 159)
    - Add `@details` about coverage calculation and MOE handling.
    - Specify required columns in `isochrone_coverage` and `population` inputs.

32. **create_subspecialty_comparison_table** (line 187)
    - Add `@details` describing join keys and output columns.
    - Include `@examples` with a minimal access results tibble.

## 7) Practice classifier (`R/the_one_practice_classifier_enhanced.R`)

33. **the_one_practice_classifier_enhanced** (line 53)
    - Add `@details` for classification rules and precedence order.
    - Document required columns and any lookup tables.
    - Add `@section Side effects` if it reads/writes to disk or DB.

34. **get_enhanced_classification_summary** (line 557)
    - Specify output summary columns and grouping rules in `@return`.
    - Add `@examples` for summary creation from classified data.

## 8) Cohort inclusion/exclusion (`R/apply_cohort_inclusion_exclusion_criteria.R`)

35. **%||%** (line 6)
    - Add `@details` for defaulting semantics specific to cohort processing.

36. **apply_cohort_inclusion_exclusion_criteria** (line 76)
    - Add `@details` outlining pipeline order, audit tracking, and config usage.
    - Document `@param` for config options and required data columns.
    - Add `@return` structure (final cohort + audit log) and `@section Side effects` for file outputs.

37. **.run_exclusion_step** (line 255)
    - Add `@details` describing how exclusion steps are sequenced and tracked.
    - Add `@return` with expected tracking columns.

38. **.track_exclusion** (line 449)
    - Document tracking flags and audit columns in `@details`.

39. **.build_audit_all_reasons_long** (line 522)
    - Add `@details` explaining long-format audit output and expected column types.

40. **.validate_inputs** (line 609)
    - Document required columns and error conditions in `@details`.

41. **.load_cohort_config** (line 646)
    - Add `@details` covering config path resolution and overrides.

42. **.load_geographic_exclusions_config** (line 695)
    - Document config schema and default fallback behavior.

43. **.define_exclusion_patterns** (line 719)
    - Add `@details` on pattern construction rules and matching precedence.

44. **.initialize_tracking_data** (line 876)
    - Specify tracking columns, default values, and types.

45. **.initialize_exclusion_flow** (line 905)
    - Add `@details` on the flow metadata structure and how it’s consumed later.

46. **.safe_extract_year** (line 931)
    - Document parsing behavior and NA handling.

47. **.exclude_by_npi_type** (line 1029)
    - Add `@details` for NPI type rules and exclusions applied.

48. **.exclude_by_missing_npi** (line 1048)
    - Document how missingness is defined and what is recorded in audit columns.

49. **.exclude_by_npi_enumeration_date** (line 1079)
    - Add `@details` for date cutoff logic and config keys.

50. **.exclude_by_geography** (line 1128)
    - Document geography patterns, match fields, and output tracking flags.

## Oxygen template reference

- Use the standardized roxygen template in `R/read_csv_utf8.R` to ensure every function adds `@title`, `@description`, `@details`, `@param`, `@return`, `@section Side effects`, `@section Logging`, `@examples`, `@seealso`, and `@family` sections where applicable.
