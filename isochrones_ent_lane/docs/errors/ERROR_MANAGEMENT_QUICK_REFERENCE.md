# Error Management System - Quick Reference

**Date**: 2026-01-17
**Version**: 1.0

---

## 30-Second Setup

```r
# 1. Source error manager
source(here::here("R", "utils", "error_manager.R"))

# 2. Wrap your main logic
main_logic <- function() {
  # Your code here
  result <- process_data()
  return(result)
}

# 3. Execute with error context
result <- error_context(
  script_name = "my_script",
  main_logic = main_logic,
  checkpoint_dir = "artifacts/checkpoints/my_script",
  error_output_path = "artifacts/errors/my_script_error.json"
)

# 4. Check result
if (result$status == "success") {
  data <- result$result
}
```

---

## Common Patterns

### Safe File Check
```r
safe_file_check(
  file_path = input_file,
  description = "input data",
  required = TRUE
)
```

### Safe Database Connection
```r
con <- safe_db_connect(
  db_path = database_file,
  read_only = TRUE,
  description = "physician database"
)
```

### Safe Database Write
```r
safe_db_write(
  con = con,
  table_name = "results",
  data = my_data,
  description = "analysis results",
  overwrite = TRUE
)
```

### Safe API Call with Retry
```r
result <- safe_api_call(
  api_function = function() my_api_call(params),
  max_retries = 3,
  retry_delay = 5,
  description = "API service name"
)
```

### Throw Custom Error
```r
throw_error(
  code = "MY_ERROR_CODE",
  message = "Descriptive error message",
  severity = "ERROR",  # FATAL, ERROR, WARNING, INFO
  context = list(
    key1 = value1,
    key2 = value2
  ),
  recoverable = TRUE,
  recovery_action = "What to do next"
)
```

### Save Checkpoint
```r
save_checkpoint(
  checkpoint_dir = checkpoint_dir,
  checkpoint_name = "phase_name",
  checkpoint_data = list(
    key1 = value1,
    key2 = value2,
    timestamp = Sys.time()
  )
)
```

### Load Checkpoint
```r
checkpoint_data <- load_latest_checkpoint(
  checkpoint_dir = checkpoint_dir,
  checkpoint_name = "phase_name"
)

if (!is.null(checkpoint_data)) {
  # Resume from checkpoint
}
```

---

## Error Code Ranges

| Range | System | Examples |
|-------|--------|----------|
| 1000-1999 | File I/O | `FILE_NOT_FOUND`, `FILE_READ_FAILED` |
| 2000-2999 | Database | `DB_CONNECTION_FAILED`, `DB_QUERY_FAILED` |
| 3000-3999 | Matching | `STRATEGY_NO_MATCHES`, `MATCHING_FAILED` |
| 4000-4999 | API/Network | `API_CALL_FAILED`, `API_TIMEOUT` |
| 5000-5999 | Validation | `VALIDATION_FAILED`, `SCHEMA_MISMATCH` |
| 6000-6999 | Configuration | `CONFIG_LOAD_FAILED`, `CONFIG_INVALID` |
| 9000-9999 | Unknown | `UNHANDLED_ERROR`, `UNEXPECTED_STATE` |

---

## Severity Levels

| Severity | When to Use | Pipeline Behavior |
|----------|-------------|-------------------|
| **FATAL** | Cannot continue | Immediate termination |
| **ERROR** | Major failure | May retry or fail gracefully |
| **WARNING** | Unexpected but handled | Continue with caution |
| **INFO** | Informational only | Continue normally |

---

## Error Context Wrapper

```r
result <- error_context(
  script_name = "my_script",
  main_logic = function() {
    # Phase 1: Validate
    safe_file_check(input_file, "input", required = TRUE)
    save_checkpoint(checkpoint_dir, "validated", list(...))

    # Phase 2: Process
    result <- process_data()
    save_checkpoint(checkpoint_dir, "processed", list(...))

    return(result)
  },
  checkpoint_dir = "path/to/checkpoints",
  error_output_path = "path/to/error.json",
  cleanup_on_error = function() {
    # Cleanup code (close connections, remove temp files, etc.)
  }
)

# Check result
if (result$status == "success") {
  data <- result$result
} else {
  cat("Processing failed\n")
}
```

---

## Complete Example

```r
#!/usr/bin/env Rscript
library(here)
source(here("R", "utils", "error_manager.R"))

# Define main logic
main_logic <- function() {
  # Validate inputs
  safe_file_check(input_file, "input data", required = TRUE)
  save_checkpoint(checkpoint_dir, "inputs_validated", list(
    input_file = input_file
  ))

  # Load data
  data <- tryCatch({
    readRDS(input_file)
  }, error = function(e) {
    throw_error(
      code = "DATA_LOAD_FAILED",
      message = sprintf("Failed to load data: %s", e$message),
      severity = "FATAL",
      context = list(input_file = input_file, error = e$message),
      recoverable = FALSE
    )
  })

  save_checkpoint(checkpoint_dir, "data_loaded", list(
    rows = nrow(data)
  ))

  # Process data
  result <- process_data(data)

  if (nrow(result) == 0) {
    throw_error(
      code = "PROCESSING_NO_RESULTS",
      message = "Processing returned zero rows",
      severity = "ERROR",
      context = list(input_rows = nrow(data)),
      recoverable = TRUE,
      recovery_action = "Check processing filters"
    )
  }

  save_checkpoint(checkpoint_dir, "processing_complete", list(
    result_rows = nrow(result)
  ))

  return(result)
}

# Execute with error management
result <- error_context(
  script_name = "my_script",
  main_logic = main_logic,
  checkpoint_dir = "artifacts/checkpoints/my_script",
  error_output_path = "artifacts/errors/my_script_error.json"
)

# Handle result
if (result$status == "success") {
  cat("✓ Processing completed\n")
} else {
  cat("✗ Processing failed\n")
  quit(status = 1)
}
```

---

## Error JSON Output Format

When an error occurs, a JSON file is written:

```json
{
  "code": "PROCESSING_NO_RESULTS",
  "message": "Processing returned zero rows",
  "severity": "ERROR",
  "timestamp": "2026-01-17 14:30:00",
  "script": "my_script",
  "context": {
    "input_rows": 1000,
    "filters_applied": ["age > 18", "status == 'active'"]
  },
  "recoverable": true,
  "recovery_action": "Check processing filters",
  "checkpoint_dir": "artifacts/checkpoints/my_script"
}
```

---

## Checkpoint File Structure

Checkpoints are saved as timestamped RDS files:

```
artifacts/checkpoints/my_script/
├── inputs_validated_20260117_143000.rds
├── data_loaded_20260117_143030.rds
├── processing_complete_20260117_143100.rds
└── ...
```

Each checkpoint contains:
```r
list(
  # Your checkpoint data
  input_file = "/path/to/file.csv",
  rows = 1000,
  timestamp = "2026-01-17 14:30:00"
)
```

---

## Recovery After Failure

```r
# 1. Check error JSON for details
error_json <- jsonlite::read_json("artifacts/errors/my_script_error.json")
cat(sprintf("Error: %s\n", error_json$code))
cat(sprintf("Recovery: %s\n", error_json$recovery_action))

# 2. List available checkpoints
checkpoint_dir <- "artifacts/checkpoints/my_script"
checkpoints <- list.files(checkpoint_dir, pattern = "\\.rds$")
cat("Available checkpoints:\n")
print(checkpoints)

# 3. Load last successful checkpoint
checkpoint_data <- load_latest_checkpoint(checkpoint_dir, "data_loaded")

# 4. Resume from that point
if (!is.null(checkpoint_data)) {
  data <- readRDS(checkpoint_data$input_file)
  # Continue processing from here...
}
```

---

## Testing Error Handling

```r
# Test that function throws expected error
test_that("Function throws FILE_NOT_FOUND error", {
  expect_error(
    my_function("/nonexistent/file.csv"),
    "Pipeline error: FILE_NOT_FOUND"
  )
})

# Test that error has correct fields
test_that("Error has structured fields", {
  err <- tryCatch(
    my_function("/nonexistent/file.csv"),
    error = function(e) e
  )

  expect_equal(err$code, "FILE_NOT_FOUND")
  expect_equal(err$severity, "FATAL")
  expect_false(err$recoverable)
  expect_true(!is.null(err$context))
})

# Test that checkpoint is created
test_that("Checkpoint is created after phase", {
  temp_checkpoint_dir <- tempfile()

  my_function_with_checkpoints(
    checkpoint_dir = temp_checkpoint_dir
  )

  checkpoints <- list.files(temp_checkpoint_dir, pattern = "\\.rds$")
  expect_true(length(checkpoints) > 0)

  unlink(temp_checkpoint_dir, recursive = TRUE)
})
```

---

## Common Mistakes to Avoid

### ❌ DON'T: Use stop() directly
```r
if (invalid) stop("Something wrong")
```

### ✅ DO: Use throw_error()
```r
if (invalid) throw_error(code = "VALIDATION_FAILED", ...)
```

---

### ❌ DON'T: Re-throw with new message
```r
tryCatch({
  throw_error(code = "MY_ERROR", ...)
}, error = function(e) {
  stop(sprintf("Failed: %s", e$code))  # Loses error object
})
```

### ✅ DO: Re-throw original error
```r
tryCatch({
  throw_error(code = "MY_ERROR", ...)
}, error = function(e) {
  stop(e)  # Preserves error object
})
```

---

### ❌ DON'T: Checkpoint trivial operations
```r
x <- 1 + 1
save_checkpoint(checkpoint_dir, "addition", x)
```

### ✅ DO: Checkpoint expensive operations
```r
data <- load_10gb_dataset()  # Expensive
save_checkpoint(checkpoint_dir, "data_loaded", metadata)
```

---

### ❌ DON'T: Swallow errors silently
```r
tryCatch({
  risky_operation()
}, error = function(e) {
  # Silent failure
})
```

### ✅ DO: Log and handle explicitly
```r
tryCatch({
  risky_operation()
}, error = function(e) {
  cat(sprintf("[WARN] Operation failed: %s\n", e$message))
  throw_error(code = "OPERATION_FAILED", ...)
})
```

---

## Integration with Data Lineage

Use both systems together:

```r
library(here)
source(here("R", "utils", "error_manager.R"))
source(here("R", "utils", "write_output_with_metadata.R"))

main_logic <- function() {
  # Error management: safe operations
  safe_file_check(input_file, "input", required = TRUE)
  data <- readRDS(input_file)
  save_checkpoint(checkpoint_dir, "data_loaded", list(...))

  result <- process_data(data)
  save_checkpoint(checkpoint_dir, "processed", list(...))

  # Data lineage: track provenance
  write_output_with_metadata(
    data = result,
    file_path = output_file,
    run_id = RUN_ID,
    step_name = "data_processing",
    inputs = list(list(input_path = input_file)),
    parameters = list(...),
    validation_checks = list(...),
    quality_metrics = list(...)
  )

  return(result)
}

result <- error_context(
  script_name = "processor",
  main_logic = main_logic,
  checkpoint_dir = checkpoint_dir,
  error_output_path = error_output_path
)
```

---

## Help & Documentation

- **User Guide**: `docs/error_management_user_guide_20260117.md`
- **Integration Guide**: `docs/error_management_modular_integration_guide_20260117.md`
- **Design Document**: `docs/error_management_system_design_20260117.md`
- **Test Examples**: `tests/testthat/test-error-management.R`

---

**Print this page and keep it handy while coding!**
