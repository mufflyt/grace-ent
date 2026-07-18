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

# S1 / S2 -- full model tables (model_table is count-model only; access uses or_table)
try(wr(acc$or_table, "S1_access_full_model.csv"))
try(wr(mysterycall_model_table(wait), "S2_timeliness_full_model.csv"))

# S3-S5 -- wait time by group
for (g in list(c("ent_type","S3_wait_by_subspecialty.csv"),
               c("rural","S4_wait_by_rurality.csv"),
               c("aao_hns_region","S5_wait_by_region.csv"))) {
  o <- tryCatch(mysterycall_wait_time_by_group(d2, "wait_days", g[1]), error = function(e) NULL)
  if (!is.null(o)) wr(if (is.data.frame(o)) o else o$table %||% o$summary %||% o, g[2])
}

# S6 -- offer-rate disparities by AAO-HNS region
o6 <- tryCatch(mysterycall_disparities_table(d, outcome_col = "offered",
        group_col = "aao_hns_region", ref_group = "New England"), error = function(e) {cat("  S6 err:", conditionMessage(e), "\n"); NULL})
if (!is.null(o6)) wr(if (is.data.frame(o6)) o6 else o6$table, "S6_offer_disparities_by_region.csv")

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
