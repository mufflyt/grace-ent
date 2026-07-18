# Shared helpers for the grace-ent test suite.
# testthat runs tests with the working directory set to tests/testthat, so we
# resolve the repository root by walking up until the enriched call log is found.

find_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(d, "data/processed/ent_phase2_enriched.csv"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate the grace-ent repository root from ", start)
}

ROOT <- find_root()

# absolute path within the repo
P <- function(...) file.path(ROOT, ...)

# read a repo-relative CSV as character (for the enriched call log)
rd_chr <- function(rel) read.csv(P(rel), colClasses = "character", check.names = FALSE)

# read a repo-relative CSV with type inference (for numeric model outputs)
rd_num <- function(rel) read.csv(P(rel), check.names = FALSE)

# the analytic sample used throughout the manuscript
analytic <- function() {
  d <- rd_chr("data/processed/ent_phase2_enriched.csv")
  d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
}
