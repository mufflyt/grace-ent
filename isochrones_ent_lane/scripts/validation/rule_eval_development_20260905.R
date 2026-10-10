#!/usr/bin/env Rscript
# NOTE (post-run hygiene edit, 2026-09-05): as run, the artifact path was a
# hardcoded SSD copy of the 3cbac594 artifact (hash-verified before use).
# Repo hygiene H2 forbids remount-suffixed volume literals, so the path now
# comes from RETIREMENT_V12_ARTIFACT. This is the ONLY post-run change.
# Development-only evaluation of the FROZEN candidate admission rules.
# Substrate: 3cbac594 read-only. References: GS-221 + hard negatives + neg fixture.
suppressMessages({library(DBI); library(duckdb)})
con <- dbConnect(duckdb::duckdb())
.art <- Sys.getenv("RETIREMENT_V12_ARTIFACT")
if (!nzchar(.art)) stop("Set RETIREMENT_V12_ARTIFACT to the hash-verified 3cbac594 artifact path")
dbExecute(con, sprintf("ATTACH '%s' AS a (READ_ONLY)", .art))

FAM <- c(nppes_deactivation="nppes_registry", medicare_part_b="cms_part_b_claims",
  medicare_part_b_hcpcs="cms_part_b_claims", medicare_dme="cms_part_b_claims",
  medicare_part_d="cms_part_d_claims", abms_status="abms_certification",
  abog_explicit_retirement="abog_certification", open_payments="industry_payments",
  medicaid_roster="medicaid_enrollment", pecos_disenrollment="cms_enrollment",
  colorado_medboard="state_licensure", illinois="state_licensure",
  washington="state_licensure", hospital_affiliation="hospital_affiliation")
fam_df <- data.frame(signal_source = names(FAM), fam = unname(FAM))
duckdb::duckdb_register(con, "fam_map", fam_df)

# Per-NPI feature table from unified signals + consensus columns
dbExecute(con, "
CREATE TEMP TABLE feat AS
WITH u AS (
  SELECT CAST(npi AS VARCHAR) AS npi, signal_source, signal_type, retirement_year,
         f.fam
  FROM a.credentials.retirement_signals_unified
  LEFT JOIN fam_map f USING (signal_source)
)
SELECT npi,
  BOOL_OR(signal_source='nppes_deactivation') AS nppes_deact,
  BOOL_OR(signal_source='nppes_deactivation' AND retirement_year <= 2022) AS nppes_deact_lag4,
  BOOL_OR((signal_source='abog_explicit_retirement' AND signal_type='retired_or_retired_diplomate')
       OR (signal_source='abms_status' AND signal_type='retired')) AS explicit_ret,
  BOOL_OR(signal_source='pecos_disenrollment') AS pecos,
  COUNT(DISTINCT fam) AS family_count,
  BOOL_OR(signal_type IN ('deceased_diplomate','license_deceased')) AS any_death,
  BOOL_OR(NOT signal_type IN ('deceased_diplomate','license_deceased')) AS any_nondeath,
  STRING_AGG(DISTINCT signal_source, '+' ORDER BY signal_source) AS combo
FROM u GROUP BY npi")
dbExecute(con, "
CREATE TEMP TABLE pop AS
SELECT f.*, (f.any_death AND NOT f.any_nondeath) AS death_only,
  c.is_retired_final, c.retirement_year_final, c.source_count,
  c.year_source_count, c.signal_family_count, c.total_source_count
FROM feat f
LEFT JOIN a.credentials.retirement_consensus c ON f.npi = CAST(c.npi AS VARCHAR)")

# Candidate predicates (FROZEN doc, sha 0099e882...)
dbExecute(con, "
CREATE TEMP TABLE pred AS
SELECT npi, combo, family_count, retirement_year_final,
  COALESCE(is_retired_final, FALSE) AS C0,
  COALESCE(source_count >= 2 AND year_source_count >= 1 AND signal_family_count >= 2, FALSE) AS C1,
  (NOT death_only) AND (nppes_deact OR explicit_ret OR (pecos AND family_count >= 2)) AS C2,
  (NOT death_only) AND (nppes_deact_lag4 OR explicit_ret) AS C3,
  (NOT death_only) AND (nppes_deact OR explicit_ret) AS C4
FROM pop")

# ---- national effect ----
cat("== NATIONAL EFFECT (population = ", dbGetQuery(con,"SELECT COUNT(*) n FROM pred")$n,
    " NPIs with >= 1 signal) ==\n", sep = "")
for (r in c("C0","C1","C2","C3","C4")) {
  x <- dbGetQuery(con, sprintf("
    SELECT SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS flagged,
           COUNT(*) AS pop,
           ROUND(100.0*SUM(CASE WHEN %s THEN 1 ELSE 0 END)/COUNT(*),1) AS pct
    FROM pred", r, r))
  cat(sprintf("%s: flags %s retired of %s (%.1f%%)\n", r,
      format(x$flagged, big.mark=","), format(x$pop, big.mark=","), x$pct))
}
cat("\n-- top combos per candidate (flagged rows) --\n")
for (r in c("C2","C3","C4")) {
  cat("\n", r, ":\n", sep="")
  print(dbGetQuery(con, sprintf(
    "SELECT combo, COUNT(*) n FROM pred WHERE %s GROUP BY 1 ORDER BY 2 DESC LIMIT 5", r)),
    row.names = FALSE)
}

# ---- development references ----
gs <- read.csv("gs_expansion_221.csv", colClasses = "character")
gs$ref <- ifelse(gs$gold_label %in% c("retired_clinical","retirement_date_uncertain"), "retired",
          ifelse(gs$gold_label == "retirement_like_but_active", "active", "excluded"))
hn <- read.csv("../fixwt/data/reference/retirement_negative_controls/hard_negatives_nonclaims_20260902.csv",
               colClasses = c(npi = "character"))
nf <- read.csv("../fixwt/tests/fixtures/negative_gold_standard_npi.csv", colClasses = "character")
nfcol <- grep("npi", names(nf), value = TRUE)[1]

p <- dbGetQuery(con, "SELECT * FROM pred")
dbDisconnect(con, shutdown = TRUE)
rownames(p) <- p$npi

score <- function(rule, ref_npis, ref_status, label) {
  reached <- ref_npis %in% p$npi
  pr <- rep(NA, length(ref_npis))
  pr[reached] <- p[ref_npis[reached], rule]
  ret <- ref_status == "retired"; act <- ref_status == "active"
  TP <- sum(pr & ret, na.rm=TRUE); FN <- sum(!pr & ret & reached, na.rm=TRUE)
  FP <- sum(pr & act, na.rm=TRUE); TN <- sum(!pr & act & reached, na.rm=TRUE)
  data.frame(set=label, rule=rule, N=length(ref_npis), reached=sum(reached),
    ref_retired=sum(ret), ref_active=sum(act), TP=TP, FN=FN, FP=FP, TN=TN,
    sens=round(TP/(TP+FN),4), spec=round(TN/(TN+FP),4),
    PPV=round(TP/(TP+FP),4), NPV=round(TN/(TN+FN),4),
    fp_ret_rate=round(FP/max(FP+TN,1),4))
}
res <- NULL
gse <- gs[gs$ref != "excluded", ]
for (r in c("C0","C1","C2","C3","C4")) {
  res <- rbind(res,
    score(r, gse$npi, gse$ref, "GS-221"),
    score(r, hn$npi, rep("active", nrow(hn)), "HARD-NEG-72798"),
    score(r, nf[[nfcol]], rep("active", nrow(nf)), "NEG-FIXTURE-16"))
}
cat("\n== DEVELOPMENT SCORES ==\n")
print(res, row.names = FALSE)

# exact-year accuracy on GS retired rows with a year (Stage B unchanged)
gy <- gs[gs$ref == "retired" & gs$retirement_year != "" & !is.na(gs$retirement_year), ]
gy <- gy[gy$npi %in% p$npi, ]
cat("\n== EXACT-YEAR ACCURACY (GS retired with year, admitted by rule; Stage B = artifact year) ==\n")
for (r in c("C0","C1","C2","C3","C4")) {
  adm <- gy[p[gy$npi, r] %in% TRUE, ]
  ok <- sum(as.integer(adm$retirement_year) == p[adm$npi, "retirement_year_final"], na.rm=TRUE)
  cat(sprintf("%s: admitted %d of %d; exact-year %d (%.2f)\n", r, nrow(adm), nrow(gy),
              ok, ifelse(nrow(adm)>0, ok/nrow(adm), NA)))
}
write.csv(res, "rule_comparison_dev.csv", row.names = FALSE)
