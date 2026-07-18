#!/usr/bin/env Rscript
# =============================================================================
# Word counts for the manuscript (repro/wordcounts.csv), read back into the Rmd.
# Counts the AUTHOR-WRITTEN PROSE from the .Rmd source, which matches the journal
# definition (body text excluding tables, figure legends, references, and the
# supplement): fenced code chunks, the YAML header, and everything from
# "# Supplementary Material" onward are removed; inline `r ...` expressions count
# as one word each. Abstract = "## Abstract" -> "## Introduction"; main text =
# "## Introduction" -> "## Author contributions".
# =============================================================================

rmd <- "manuscript_mystery_caller.Rmd"
L <- readLines(rmd, warn = FALSE)

# drop YAML header (first fenced --- ... ---)
yz <- which(L == "---"); if (length(yz) >= 2) L <- L[-(yz[1]:yz[2])]
# drop the supplement and everything after
supp <- grep("^# Supplementary Material", L); if (length(supp)) L <- L[seq_len(supp[1] - 1)]
# drop fenced code chunks ``` ... ```
fences <- grep("^```", L)
if (length(fences) >= 2) {
  drop <- unlist(Map(seq, fences[seq(1, length(fences), 2)], fences[seq(2, length(fences), 2)]))
  L <- L[-drop]
}
s <- paste(L, collapse = "\n")
s <- gsub("`r [^`]+`", " word ", s)          # inline R -> one token
s <- gsub("[#*_>|]", " ", s)                 # strip md markup
s <- gsub("\\[[^]]*\\]\\([^)]*\\)", " ", s)   # links/images

sect <- function(a, b) {
  i <- regexpr(a, s, fixed = TRUE); j <- regexpr(b, s, fixed = TRUE)
  if (i < 0) return("")
  if (j < 0 || j <= i) j <- nchar(s) + 1
  substr(s, i + nchar(a), j - 1)
}
wc <- function(x) { x <- trimws(gsub("\\s+", " ", x)); if (!nzchar(x)) 0L else length(strsplit(x, " ")[[1]]) }

body     <- sect("Introduction", "Author contributions")

# abstract: count from the rendered HTML if available (inline stats expanded ->
# accurate), else from the Rmd source
abs_wc <- wc(sect("Abstract", "Keywords"))
html <- "manuscript_output/manuscript_mystery_caller.html"
if (file.exists(html)) {
  h <- paste(readLines(html, warn = FALSE), collapse = " ")
  h <- gsub("<[^>]+>", " ", h); h <- gsub("&[a-z]+;|&#[0-9]+;", " ", h)
  i <- regexpr("Objective.", h, fixed = TRUE); j <- regexpr("Keywords:", h, fixed = TRUE)
  if (i > 0 && j > i) abs_wc <- wc(substr(h, i, j - 1))
}

out <- data.frame(abstract = abs_wc, body = wc(body))
dir.create("repro", showWarnings = FALSE)
write.csv(out, "repro/wordcounts.csv", row.names = FALSE)
cat(sprintf("word counts -> abstract %d, main text %d (repro/wordcounts.csv)\n", out$abstract, out$body))
