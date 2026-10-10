#!/usr/bin/env Rscript
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate departure-algorithm supplement data: leave-one-source-out (LOSO)
# and the per-cohort CONSORT-style derivation flow (peer review).
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOSO: for each departed physician, which of the six sources fired
# (credentials.retirement_signals_pivot has_* flags); 'sole_support' = departures
# that would be lost if that source were removed (it was the only firing signal).
# CONSORT: certified/designated -> NPI-matched -> active-in-practice -> removed
# (deceased / inactive) -> final active baseline, per cohort (URPS both-pathway).
#
# Writes cliff/data/loso_source_contribution.csv and cliff/data/consort_cohort_flow.csv
# so the supplement renders without the external DuckDB.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

suppressPackageStartupMessages({library(DBI); library(duckdb); library(dplyr); library(readr); library(here)})

SUBS <- c(URPS="Female Pelvic Medicine & Reconstructive Surgery", GO="Gynecologic Oncology", MIGS="MIGS")
coh <- read_csv("/Users/tylermuffly/isochrones/manuscript/tables/table1_physician_characteristics.csv",
                show_col_types=FALSE, guess_max=1e5) %>%
  filter(subspecialty %in% unname(SUBS)) %>%
  transmute(npi=as.character(npi), ab=names(SUBS)[match(subspecialty,SUBS)],
            departed=as.logical(is_retired_for_cohorting),
            deceased=as.logical(deceased)) %>%
  distinct(npi,.keep_all=TRUE)

# --- LOSO from the signals pivot --------------------------------------------
db <- NA_character_
for (c in c("/Volumes/MufflySamsung/DuckDB/nber_my_duckdb.duckdb",
            "/Volumes/MufflySamsung 1/DuckDB/nber_my_duckdb.duckdb")) if (file.exists(c)) { db<-c; break }
if (is.na(db)) stop("NBER DuckDB not mounted.")
con <- dbConnect(duckdb::duckdb(), db, read_only=TRUE)
piv <- dbGetQuery(con, "SELECT CAST(npi AS VARCHAR) npi, has_abms_cert_lapse, has_nppes_deactivation,
                        has_open_payments, has_medicare_part_d, has_medicare_part_b,
                        source_count_unified FROM credentials.retirement_signals_pivot")
dbDisconnect(con, shutdown=TRUE)

dep <- coh %>% filter(departed) %>% inner_join(piv, by="npi")
SRC <- c(has_abms_cert_lapse="ABMS MOC lapse (priority 1)",
         has_nppes_deactivation="NPPES deactivation (priority 2)",
         has_open_payments="Open Payments cessation (priority 3)",
         has_medicare_part_d="Medicare Part D cessation (priority 5)",
         has_medicare_part_b="Medicare Part B cessation (priority 6)")
loso <- do.call(rbind, lapply(names(SRC), function(s){
  fired <- sum(dep[[s]] %in% TRUE)
  sole  <- sum(dep[[s]] %in% TRUE & dep$source_count_unified==1)
  data.frame(source=SRC[[s]], n_departures_supported=fired,
             pct_of_departures=round(100*fired/nrow(dep),1),
             sole_support_lost_if_removed=sole,
             pct_lost_if_removed=round(100*sole/nrow(dep),1))
}))
write_csv(loso, here::here("cliff","data","loso_source_contribution.csv"))

# --- CONSORT-style cohort flow ----------------------------------------------
# ABOG pathway per cohort (table1 is already NPI-matched and CONUS-filtered).
flow <- coh %>% group_by(ab) %>% summarise(
  certified_matched = n(),
  removed_deceased = sum(deceased %in% TRUE),
  removed_inactive = sum(departed %in% TRUE & !(deceased %in% TRUE)),
  active_baseline = sum(departed %in% FALSE), .groups="drop")
# URPS both-pathway: add ABU net-new active (270; 264 with a usable certification year)
abu_active_total <- 270L; abu_active_aged <- 264L
flow$abu_net_new_active <- ifelse(flow$ab=="URPS", abu_active_total, 0L)
flow$active_baseline_final <- ifelse(flow$ab=="URPS",
                                     flow$active_baseline + abu_active_aged,  # matches SSOT (age-structured)
                                     flow$active_baseline)
write_csv(flow, here::here("cliff","data","consort_cohort_flow.csv"))

cat("=== Leave-one-source-out (departed physicians) ===\n")
cat("Departed matched in pivot:", nrow(dep), "of", sum(coh$departed),
    "| median sources per departure:", median(dep$source_count_unified, na.rm=TRUE), "\n\n")
print(as.data.frame(loso), row.names=FALSE)
cat("\n=== CONSORT cohort flow ===\n")
print(as.data.frame(flow), row.names=FALSE)
cat("\nSpecialty inactivity thresholds (config/retirement_specialty_thresholds.yml):",
    "default 3 y; Gynecologic Oncology 4 y; MIGS 4 y; URPS -> default 3 y.\n")
cat("Wrote loso_source_contribution.csv and consort_cohort_flow.csv\n")
