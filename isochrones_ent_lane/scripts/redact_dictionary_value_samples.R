#!/usr/bin/env Rscript
# =============================================================================
# Redact enumerated real values from data-dictionary value samples
# =============================================================================
#
# A data dictionary describes a column's FORMAT. It does not need to enumerate
# real values, and when the column holds contact details it should not.
#
# artifacts/ent/ent_data_dictionary_LATEST.csv carried, in its value_labels
# field, the most frequent observed values with their counts:
#
#   phone_norm       top: (215) 955-6784=17; (612) 871-1144=15; ...
#   address          top: 660 White Plains Rd Ste 500=58; ...
#   matched_address  top: 660 WHITE PLAINS RD, TARRYTOWN, NY, 10591=84
#
# Those are real practice addresses and telephone numbers scraped from a
# physician directory, republished as summary statistics. They are published
# business contacts rather than sensitive personal data, which is why this is a
# redaction rather than an incident -- but a dictionary is the wrong place for
# them, and leaving them there means the PII guard must either flag them
# forever or be taught to ignore a data file, which is worse.
#
# board_cert is deliberately left alone: its top values are certification
# categories, which is exactly what a dictionary should show.
#
# Usage: Rscript scripts/redact_dictionary_value_samples.R
# =============================================================================

TARGET <- "artifacts/ent/ent_data_dictionary_LATEST.csv"

# Columns whose observed values are contact details or locations.
REDACT_COLUMNS <- c("phone_norm", "address", "matched_address",
                    "practice_address", "email", "phone")

main <- function() {
  if (!file.exists(TARGET)) {
    cat(sprintf("nothing to do: %s not present\n", TARGET)); return(invisible(TRUE))
  }
  d <- utils::read.csv(TARGET, colClasses = "character", check.names = FALSE)
  if (!all(c("column", "value_labels") %in% names(d))) {
    stop("unexpected dictionary schema: need `column` and `value_labels`", call. = FALSE)
  }
  hit <- d$column %in% REDACT_COLUMNS & grepl("top:", d$value_labels, fixed = TRUE)
  if (!any(hit)) { cat("nothing to redact\n"); return(invisible(TRUE)) }

  n_vals <- vapply(d$value_labels[hit], function(v)
    length(strsplit(v, ";", fixed = TRUE)[[1]]), integer(1))
  d$value_labels[hit] <- sprintf(
    "top values redacted (%d distinct sampled); see the source artifact for values",
    n_vals)

  utils::write.csv(d, TARGET, row.names = FALSE, na = "")
  cat(sprintf("redacted %d dictionary row(s) in %s:\n", sum(hit), TARGET))
  for (cn in d$column[hit]) cat(sprintf("  %s\n", cn))
  invisible(TRUE)
}

if (sys.nframe() == 0L) main()
