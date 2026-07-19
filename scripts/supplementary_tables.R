#!/usr/bin/env Rscript
# =============================================================================
# Supplementary tables for the ENT access study, built with mysterycall
# (loading the fixed source). Writes CSVs to model_output/supp/.
#
#   S1  Full access model (all odds ratios)          mysterycall_model_table
#   S2  Full timeliness model (all rate ratios)       mysterycall_model_table
#   S3  Wait time by subspecialty                     mysterycall_wait_time_by_group
#   S4  Wait time by rurality                         mysterycall_wait_time_by_group
#   S5  Wait time by AAO-HNS region                   mysterycall_wait_time_by_group
#   S6  Offer-rate disparities by AAO-HNS region      mysterycall_disparities_table
#   S7  Access vs timeliness, side by side            mysterycall_multi_model_table
#   +   Bundled supplemental tables (.docx if pandoc) mysterycall_supplemental_tables
# =============================================================================

suppressMessages(devtools::load_all("~/mysterycall", quiet = TRUE))
options(stringsAsFactors = FALSE, width = 105)
supp <- "model_output/supp"; dir.create(supp, showWarnings = FALSE, recursive = TRUE)
num <- function(x) suppressWarnings(as.numeric(x))
wr  <- function(x, f) { write.csv(as.data.frame(x), file.path(supp, f), row.names = FALSE); cat("  wrote", f, "\n") }

# human-readable term labels + a reader-facing model table (no raw z/se columns)
prettyterm <- function(t) {
  map <- c(
    "(Intercept)"                  = "Intercept",
    "ent_typeFacial Plastics"      = "Facial plastic surgery",
    "ent_typeHead and Neck Cancer" = "Head and neck oncology",
    "ent_typeLaryngology"          = "Laryngology",
    "ent_typeOtology/neurotology"  = "Otology/neurotology",
    "ent_typePediatrics"           = "Pediatric otolaryngology",
    "ent_typeRhinology"            = "Rhinology",
    "ent_typeSleep"                = "Sleep medicine",
    "ent_per_100k_z"               = "Otolaryngologist density (per SD)",
    "medicaid_fee_index_z"         = "Medicaid fee index (per SD)",
    "svi_overall_z"                = "Social Vulnerability Index (per SD)",
    "dual_pct_z"                   = "Dual-eligible share (per SD)",
    "ruralRural"                   = "Rural (vs urban)",
    "ruralSuburban"                = "Suburban (vs urban)")
  out <- unname(map[t])
  out[is.na(out) & grepl("^caller_f", t)] <- paste0("Caller: ", sub("^caller_f", "", t[is.na(out) & grepl("^caller_f", t)]))
  ifelse(is.na(out), t, out)
}
clean_model_tab <- function(tab, ratio_col, ratio_name) {
  p <- num(tab$p_value)
  data.frame(
    Term            = prettyterm(tab$term),
    setNames(list(sprintf("%.2f", num(tab[[ratio_col]]))), ratio_name),
    `95% CI`        = sprintf("%.2f-%.2f", num(tab$ci_lower), num(tab$ci_upper)),
    `p-value`       = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    check.names = FALSE, stringsAsFactors = FALSE)
}

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L, 0L)
d$wait_days <- num(d$wait_days_business)
for (v in c("ent_per_100k","medicaid_fee_index","svi_overall","dual_pct"))
  d[[paste0(v,"_z")]] <- { x<-num(d[[v]]); x[is.na(x)]<-median(x,na.rm=TRUE); as.numeric(scale(x)) }
d$ent_type <- relevel(factor(d$ent_type), ref = "General")
d$rural    <- relevel(factor(d$ruca_category), ref = "Urban")
ct <- table(d$caller); d$caller_f <- relevel(factor(ifelse(d$caller %in% names(ct)[ct<15],"Other",d$caller)),
                                              ref = names(sort(ct,decreasing=TRUE))[1])
clust <- d$cbsa_code; clust[!nzchar(clust)] <- d$county_fips[!nzchar(clust)]
clust[!nzchar(clust)] <- paste0("solo_", which(!nzchar(clust))); d$cluster <- clust
preds <- c("ent_type","ent_per_100k_z","medicaid_fee_index_z","svi_overall_z","dual_pct_z","rural","caller_f")

acc  <- mysterycall_logistic_model(d[!is.na(d$offered),], "offered", preds, "cluster")
d2   <- d[which(d$offered==1 & !is.na(d$wait_days)),]
wait <- mysterycall_nb_model(d2, "wait_days", preds, "cluster")

cat("Building supplementary tables ->", supp, "\n")

# S1 / S2 -- full model tables, reader-facing columns (Term, OR/IRR, 95% CI, p)
try(wr(clean_model_tab(acc$or_table,  "or",  "Odds ratio"),        "S1_access_full_model.csv"))
try(wr(clean_model_tab(wait$irr_table, "irr", "Incidence rate ratio"), "S2_timeliness_full_model.csv"))

# S3-S5 -- wait time by group (reader-facing column names)
wait_grp_labels <- c(ent_type = "Subspecialty", rural = "Rurality",
                     aao_hns_region = "AAO-HNS region",
                     median_days = "Median wait (business days)", q1 = "25th percentile (days)",
                     q3 = "75th percentile (days)", n = "Appointments, n")
for (g in list(c("ent_type","S3_wait_by_subspecialty.csv"),
               c("rural","S4_wait_by_rurality.csv"),
               c("aao_hns_region","S5_wait_by_region.csv"))) {
  o <- tryCatch(mysterycall_wait_time_by_group(d2, "wait_days", g[1]), error = function(e) NULL)
  if (!is.null(o)) {
    o <- if (is.data.frame(o)) o else o$table %||% o$summary %||% o
    names(o) <- ifelse(names(o) %in% names(wait_grp_labels), wait_grp_labels[names(o)], names(o))
    wr(o, g[2])
  }
}

# S6 -- offer-rate disparities by AAO-HNS region (reader-facing subset)
o6 <- tryCatch(mysterycall_disparities_table(d, outcome_col = "offered",
        group_col = "aao_hns_region", ref_group = "New England"), error = function(e) {cat("  S6 err:", conditionMessage(e), "\n"); NULL})
if (!is.null(o6)) {
  o6 <- if (is.data.frame(o6)) o6 else o6$table
  s6 <- data.frame(
    "AAO-HNS region"     = o6$group,
    "Calls, n"           = o6$n,
    "Offers, n"          = o6$n_accepted,
    "Offer rate, %"      = sprintf("%.1f", 100 * num(o6$rate)),
    "95% CI, %"          = sprintf("%.1f-%.1f", 100 * num(o6$lower_ci), 100 * num(o6$upper_ci)),
    "Risk difference vs New England, %" = sprintf("%.1f", 100 * num(o6$abs_diff)),
    "Risk ratio"         = ifelse(is.na(num(o6$rel_risk)), "—", sprintf("%.2f", num(o6$rel_risk))),
    "RR 95% CI"          = ifelse(is.na(num(o6$rr_lower)), "—",
                                  sprintf("%.2f-%.2f", num(o6$rr_lower), num(o6$rr_upper))),
    "p-value"            = ifelse(is.na(num(o6$p_value)), "—",
                                  ifelse(num(o6$p_value) < 0.001, "<0.001", sprintf("%.3f", num(o6$p_value)))),
    check.names = FALSE)
  wr(s6, "S6_offer_disparities_by_region.csv")
}

# S7 -- access vs timeliness side by side
o7 <- tryCatch(mysterycall_multi_model_table(list(Access = acc, Timeliness = wait)),
               error = function(e) {cat("  S7 err:", conditionMessage(e), "\n"); NULL})
if (!is.null(o7)) {
  # mysterycall packs "estimate\np=..." into each cell; the embedded newline
  # breaks pandoc pipe tables, so flatten to a single line.
  o7[] <- lapply(o7, function(col) gsub("\n", ", ", as.character(col)))
  wr(o7, "S7_access_vs_timeliness.csv")
}

# Bundled supplemental tables (docx/text)
bundle <- tryCatch(mysterycall_supplemental_tables(
  logistic_fit = acc, poisson_fit = wait, lmm_fit = NULL,
  file = file.path(supp, "supplemental_bundle"), overwrite = TRUE,
  author = "grace-ent"), error = function(e) {cat("  bundle err:", conditionMessage(e), "\n"); NULL})

cat("\nDone. Files in", supp, ":\n"); print(list.files(supp))
