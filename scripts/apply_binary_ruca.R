#!/usr/bin/env Rscript
# =============================================================================
# Adopt the binary rurality definition study-wide: urban = RUCA 1-3, rural =
# RUCA 4-10 (suburban folded into rural). Recodes `ruca_category` in the
# enriched dataset to two levels (Urban / Rural); the raw `ruca_code` is left
# intact so the prior 3-level split can always be reconstructed.
#
# Idempotent: re-running on an already-binary file is a no-op.
# =============================================================================

f <- "data/processed/ent_phase2_enriched.csv"
d <- read.csv(f, colClasses = "character")
rc <- suppressWarnings(as.numeric(d$ruca_code))
before <- table(d$ruca_category, useNA = "ifany")
# urban = RUCA 1-3, rural = 4-10; keep blanks blank
d$ruca_category <- ifelse(is.na(rc) | d$ruca_code == "", d$ruca_category,
                   ifelse(rc <= 3, "Urban", "Rural"))
write.csv(d, f, row.names = FALSE, na = "")
cat("Recoded ruca_category to binary (urban = RUCA 1-3, rural = 4-10).\n")
cat("Before:\n"); print(before)
cat("After:\n");  print(table(d$ruca_category, useNA = "ifany"))
