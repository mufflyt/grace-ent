#!/usr/bin/env Rscript
# =============================================================================
# ENT Euler Diagram — Table 1 (gt + Word output, publication-polished)
#
# Improvements over the gtsummary draft:
#   1. Replace "0 (NA%)" / "NA (NA, NA)" with em-dash for structurally
#      undefined cells (e.g., ABOto subspecialty in a zone with no ABOto).
#   2. Merge the two ENThealth-only sub-zones (n=1 + n=247) into a single
#      "ENThealth only" column (n=248).
#   3. No redundant cohort-wide N column.
#   4. Row groups by data source (NPPES / ABOto / ENThealth registries).
#   5. Column spanner by number-of-registries (All three / Two / Single).
#   6. Columns ordered to highlight the triple-match -> single-registry story.
#   7. Adds a "Top 5 states" row (geography preserved).
#
# Input :  ent_euler_zone_npi_assignments.csv  (from build_ent_euler_table1.R)
# Output:  ent_euler_zone_table1.docx
#          ent_euler_zone_table1.html
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(gt)
})

dir_out  <- here::here("publication_materials", "figures", "ent_data_integrity")
roster   <- read_csv(file.path(dir_out, "ent_euler_zone_npi_assignments.csv"),
                     show_col_types = FALSE)

# ── BOG region SSOT (sourced into isolated env to avoid side-effects) ───────
.bog_env <- new.env()
sys.source(here::here("R", "create_specialty_table1.R"),
           envir = .bog_env, chdir = TRUE)
AAO_BOG_LOOKUP <- .bog_env$.AAO_BOG_LOOKUP   # tibble: state, bog_region
BOG_LEVELS     <- .bog_env$.BOG_LEVELS       # 9 region labels in display order
stopifnot("AAO-HNS BOG SSOT not loaded" =
            !is.null(AAO_BOG_LOOKUP) && !is.null(BOG_LEVELS),
          "Expected 9 BOG regions" = length(BOG_LEVELS) == 9L)

# ── Pull ABOto state so Board-only-state-mismatch (n=12) gets a region ──────
abohns_matched <- read_csv(here::here("data", "ent", "abohns_with_npi_master.csv"),
                           show_col_types = FALSE)
if (!exists("convert_state_name_to_abbrev", mode = "function")) {
  source(here::here("R", "utils", "state_normalization.R"))
}
aboto_state_by_npi <- abohns_matched %>%
  mutate(npi_chr = as.character(npi),
         aboto_state = unname(convert_state_name_to_abbrev(state_full))) %>%
  filter(!is.na(npi_chr), !is.na(aboto_state)) %>%
  distinct(npi_chr, .keep_all = TRUE) %>%
  select(npi_chr, aboto_state)

roster <- roster %>%
  mutate(npi_chr = as.character(npi_chr)) %>%
  left_join(aboto_state_by_npi, by = "npi_chr") %>%
  mutate(state_final = dplyr::coalesce(state_final, aboto_state)) %>%
  left_join(AAO_BOG_LOOKUP, by = c("state_final" = "state")) %>%
  mutate(bog_region = factor(bog_region, levels = BOG_LEVELS))

# ── ENT procedure categories billed in Medicare Part B (2022 + 2023) ────────
# Loads the 71-CPT, 11-category map from config/ent_procedure_categories.yml
# and flags, per NPI, which categories they billed in either year. See
# R/ent_procedure_categories.R and the technical appendix dated 2026-05-15.
suppressPackageStartupMessages({library(DBI); library(duckdb)})
source(here::here("R", "ent_procedure_categories.R"))
ent_cat <- load_ent_procedure_categories()
PROC_CATEGORIES <- sort(unique(ent_cat$procedure_category))
ent_cpt_set <- unique(ent_cat$cpt_code)

if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13); fixes broken un-suffixed mount
duckdb_path <- load_paths(verbose = FALSE)$nber_duckdb_main
con <- dbConnect(duckdb::duckdb(), duckdb_path, read_only = TRUE)

npi_with_id <- roster %>% filter(!is.na(npi_chr)) %>% pull(npi_chr) %>% unique()
npi_sql <- paste(npi_with_id, collapse = ",")
cpt_sql <- paste(sprintf("'%s'", ent_cpt_set), collapse = ",")

bill_sql <- sprintf('
  SELECT CAST("Rndrng_NPI" AS VARCHAR) AS npi_chr, "HCPCS_Cd" AS cpt_code
  FROM medicare_part_b_by_service_2023
  WHERE "HCPCS_Cd" IN (%s) AND CAST("Rndrng_NPI" AS VARCHAR) IN (%s)
  UNION
  SELECT CAST("Rndrng_NPI" AS VARCHAR) AS npi_chr, "HCPCS_Cd" AS cpt_code
  FROM medicare_part_b_by_service_2022
  WHERE "HCPCS_Cd" IN (%s) AND CAST("Rndrng_NPI" AS VARCHAR) IN (%s)
', cpt_sql, npi_sql, cpt_sql, npi_sql)

ent_bill_pairs <- dbGetQuery(con, bill_sql)
dbDisconnect(con, shutdown = TRUE)

# (NPI, category) pairs -> per-NPI category-billed flags
ent_bill_by_npi <- ent_bill_pairs %>%
  left_join(ent_cat %>% select(cpt_code, procedure_category) %>% distinct(),
            by = "cpt_code") %>%
  distinct(npi_chr, procedure_category) %>%
  mutate(billed = TRUE) %>%
  tidyr::pivot_wider(names_from = procedure_category,
                     values_from = billed, values_fill = FALSE)

# Make sure every category column exists even if no one in the cohort billed
for (cat in PROC_CATEGORIES) {
  if (!cat %in% names(ent_bill_by_npi)) ent_bill_by_npi[[cat]] <- FALSE
}
ent_bill_by_npi <- ent_bill_by_npi %>%
  mutate(n_ent_categories_billed = rowSums(across(all_of(PROC_CATEGORIES))),
         billed_any_ent_proc     = n_ent_categories_billed > 0L)

roster <- roster %>%
  left_join(ent_bill_by_npi, by = "npi_chr") %>%
  mutate(across(all_of(PROC_CATEGORIES),       ~ tidyr::replace_na(., FALSE)),
         n_ent_categories_billed = tidyr::replace_na(n_ent_categories_billed, 0L),
         billed_any_ent_proc     = tidyr::replace_na(billed_any_ent_proc,    FALSE))

# ── Credential text + any-Medicare-Part-B billing (2022 or 2023) ────────────
# Adds three new row groups to the table: credential type (MD/DO/other),
# NPPES enumeration bucket (likely-resident -> senior), and presence of
# any Medicare Part B claim in 2022 or 2023.
con <- dbConnect(duckdb::duckdb(), duckdb_path, read_only = TRUE)
cred_q <- dbGetQuery(con, sprintf('
  SELECT CAST("NPI" AS VARCHAR) AS npi_chr,
         "Provider Credential Text" AS credential_text
  FROM "NPPES_Data_Dissemination_February_2026_npidata_pfile_20050523_20260208_csv"
  WHERE "NPI" IN (%s)
', npi_sql))

any_part_b_q <- dbGetQuery(con, sprintf('
  SELECT DISTINCT CAST("Rndrng_NPI" AS VARCHAR) AS npi_chr
  FROM medicare_part_b_2023
  WHERE CAST("Rndrng_NPI" AS VARCHAR) IN (%s)
  UNION
  SELECT DISTINCT CAST("Rndrng_NPI" AS VARCHAR) AS npi_chr
  FROM medicare_part_b_2022
  WHERE CAST("Rndrng_NPI" AS VARCHAR) IN (%s)
', npi_sql, npi_sql))
dbDisconnect(con, shutdown = TRUE)

classify_credential <- function(txt) {
  # Returns one of: MD, DO, MD + DO, Other (non-MD/DO), Missing
  #
  # MD includes US/international medical degrees: MD, M.D., M. D., MBBS,
  # MB BCH BAO (Irish), MDCM (Canadian), MBChB (UK).
  # DO includes DO, D.O., D. O.
  # Other catches PA-C, NP, APRN, CRNP, AUD, audiologist, PhD-only, MS-only,
  # DDS, DPM, etc.
  if (is.na(txt) || !nzchar(trimws(txt))) return("Missing")
  u <- toupper(txt)
  has_md <- grepl(paste0(
    "(^|[^A-Z])M\\.?\\s?D\\.?([^A-Z]|$)",   # MD, M.D., M. D.
    "|MBBS|MBCHB|MB\\.?\\s?B\\.?CH\\.?",    # MBBS, MBChB, MB BCH
    "|MDCM"),                                # Canadian MD
    u)
  has_do <- grepl("(^|[^A-Z])D\\.?\\s?O\\.?([^A-Z]|$)", u)
  dplyr::case_when(
    has_md & has_do ~ "MD + DO",
    has_do          ~ "DO",
    has_md          ~ "MD",
    TRUE            ~ "Other (non-MD/DO)"
  )
}
classify_credential_v <- Vectorize(classify_credential, USE.NAMES = FALSE)

enum_bucket_fn <- function(yr) {
  if (is.na(yr)) return(NA_character_)
  yrs <- 2026L - as.integer(yr)
  dplyr::case_when(
    yrs <= 5              ~ "<=5 yrs (likely resident/fellow)",
    yrs >= 6  & yrs <= 10 ~ "6-10 yrs (pre-cert window)",
    yrs >= 11 & yrs <= 20 ~ "11-20 yrs (mid-career)",
    yrs > 20              ~ ">20 yrs (senior)",
    TRUE                   ~ NA_character_
  )
}
enum_bucket_v <- Vectorize(enum_bucket_fn, USE.NAMES = FALSE)

ENUM_BUCKET_LEVELS <- c(
  "<=5 yrs (likely resident/fellow)",
  "6-10 yrs (pre-cert window)",
  "11-20 yrs (mid-career)",
  ">20 yrs (senior)"
)
CRED_LEVELS <- c("MD", "DO", "MD + DO", "Other (non-MD/DO)", "Missing")

roster <- roster %>%
  left_join(cred_q,                                            by = "npi_chr") %>%
  mutate(credential_class    = classify_credential_v(credential_text),
         enum_bucket         = enum_bucket_v(nppes_enum_year),
         billed_any_part_b   = npi_chr %in% any_part_b_q$npi_chr)

# ── Physician-only filter (drop PA/NP/audiology with 207Y* taxonomy) ────────
# Rationale: this is a physician supply analysis. Anyone whose NPPES
# credential text clearly identifies a non-physician role (PA, NP, APRN,
# CRNP, AUD/audiologist, PhD/MS-only, DDS, etc.) is excluded. NPIs with
# missing/blank credential text are RETAINED — many board-certified
# physicians simply leave that field blank, and the Missing rate in Triple
# Match (~4%) demonstrates this.
non_physician <- roster %>%
  filter(credential_class == "Other (non-MD/DO)") %>%
  select(npi_chr, zone, credential_text)

write_csv(non_physician,
          file.path(dir_out, "ent_euler_excluded_nonphysicians.csv"), na = "")
cat(sprintf("Identified %d non-physician NPIs (PA/NP/audiology/etc.):\n",
            nrow(non_physician)))
print(non_physician %>% count(zone, name = "n_dropped"))

# Env switch: EULER_TABLE_PHYSICIAN_ONLY=0 produces the unfiltered table;
# default (1) drops non-physicians. Filenames change accordingly.
physician_only <- !identical(trimws(Sys.getenv("EULER_TABLE_PHYSICIAN_ONLY", "1")), "0")
file_suffix <- if (physician_only) "" else "_unfiltered"
cat(sprintf("Build mode: physician_only = %s  (suffix = '%s')\n",
            physician_only, file_suffix))

if (physician_only) {
  roster <- roster %>% filter(credential_class != "Other (non-MD/DO)")
}
cat(sprintf("Roster size for this build: %d rows\n", nrow(roster)))

# Save the per-NPI roster used for THIS build so reviewers can audit.
roster_audit_path <- file.path(
  dir_out, paste0("ent_euler_zone_npi_assignments", file_suffix, "_with_demographics.csv"))
write_csv(roster %>%
            select(npi_chr, zone, in_nppes, in_board, in_ent,
                   gender_final, age_final,
                   credential_text, credential_class,
                   nppes_enum_year, years_since_enum, enum_bucket,
                   aboto_initial_cert_year, years_since_aboto_cert,
                   aboto_subspecialty,
                   ent_years_since_resi, ent_career_stage, ent_specialty,
                   state_final, bog_region,
                   billed_any_part_b, billed_any_ent_proc,
                   n_ent_categories_billed),
          roster_audit_path, na = "")
cat(sprintf("Wrote per-NPI audit CSV: %s\n", roster_audit_path))

# ── Merge ENThealth-only zones; drop empty zone 5 ───────────────────────────
roster <- roster %>%
  mutate(zone = dplyr::recode(zone,
           "6_ent_only_with_npi" = "5_ent_only",
           "7_ent_only_no_npi"   = "5_ent_only")) %>%
  filter(zone != "5_board_ent_no_nppes")

# ── Zone order (display) and registry-count spanner ─────────────────────────
zone_order <- c("triple_match", "nppes_board", "nppes_ent",
                "nppes_only",   "ent_only",    "board_only_state_mismatch")
zone_codes <- c(
  "8_triple_match"              = "triple_match",
  "2_nppes_board"               = "nppes_board",
  "3_nppes_ent"                 = "nppes_ent",
  "1_nppes_only"                = "nppes_only",
  "5_ent_only"                  = "ent_only",
  "4_board_only_state_mismatch" = "board_only_state_mismatch"
)
roster$zone_id <- zone_codes[roster$zone]

zone_label <- c(
  triple_match              = "Triple Match",
  nppes_board               = "NPI & Board",
  nppes_ent                 = "NPI & ENThealth",
  nppes_only                = "NPI Only",
  ent_only                  = "ENThealth Only",
  board_only_state_mismatch = "Board Only*"
)

# Which (zone, source) pairs are *structurally available*?
# Used to render "—" in unavailable cells instead of "0 (NA%)" / "NA (NA, NA)".
src_avail <- tibble::tribble(
  ~zone_id,                    ~nppes, ~aboto, ~enthealth,
  "triple_match",              TRUE,   TRUE,   TRUE,
  "nppes_board",               TRUE,   TRUE,   FALSE,
  "nppes_ent",                 TRUE,   FALSE,  TRUE,
  "nppes_only",                TRUE,   FALSE,  FALSE,
  "ent_only",                  FALSE,  FALSE,  TRUE,
  "board_only_state_mismatch", FALSE,  TRUE,   FALSE   # not in CONUS NPPES universe
)

# ── Stat helpers ────────────────────────────────────────────────────────────
em <- "\u2014"  # em dash

fmt_pct <- function(num, den) {
  if (is.na(den) || den == 0) return(em)
  sprintf("%s (%.1f%%)", format(num, big.mark = ","),
          100 * num / den)
}
fmt_med_iqr <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(em)
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf("%.0f (%.0f, %.0f)", q[2], q[1], q[3])
}
bog_region_row <- function(df_zone, region_label) {
  n_zone   <- nrow(df_zone)
  n_region <- sum(df_zone$bog_region == region_label, na.rm = TRUE)
  fmt_pct(n_region, n_zone)
}
bog_other_row <- function(df_zone) {
  n_zone <- nrow(df_zone)
  n_other <- sum(is.na(df_zone$bog_region))
  fmt_pct(n_other, n_zone)
}

# ── Per-zone stats (long: one row per (statistic, zone)) ────────────────────
build_zone_stats <- function(df_zone, zone_id) {
  avail <- src_avail[src_avail$zone_id == zone_id, ]
  N     <- nrow(df_zone)

  gate <- function(value, src_ok) if (isTRUE(src_ok)) value else em

  # Credential rows: queryable for any NPI; "—" for the ENThealth-orphan
  # subgroup (no NPI to look up).
  n_with_npi <- sum(!is.na(df_zone$npi_chr))
  cred_value <- function(lvl) {
    if (n_with_npi == 0) return(em)
    fmt_pct(sum(df_zone$credential_class == lvl, na.rm = TRUE), N)
  }
  enum_value <- function(lvl) {
    # Tied to NPPES enum availability — same gate as the median (IQR) rows
    gate(fmt_pct(sum(df_zone$enum_bucket == lvl, na.rm = TRUE), N), avail$nppes)
  }
  any_pb_yes <- if (n_with_npi == 0) em else fmt_pct(sum(df_zone$billed_any_part_b),  N)
  any_pb_no  <- if (n_with_npi == 0) em else fmt_pct(sum(!df_zone$billed_any_part_b), N)

  base_rows <- tibble::tribble(
    ~row_group,                          ~statistic,                                   ~value,
    "Header",                            "N",                                          format(N, big.mark = ","),
    "Demographics",                      "Gender: Female",                             fmt_pct(sum(df_zone$gender_final == "F", na.rm = TRUE), N),
    "Demographics",                      "Gender: Male",                               fmt_pct(sum(df_zone$gender_final == "M", na.rm = TRUE), N),
    "Demographics",                      "Gender: Missing",                            fmt_pct(sum(is.na(df_zone$gender_final) | df_zone$gender_final == ""), N),
    "Credential type (NPPES)",           "MD",                                         cred_value("MD"),
    "Credential type (NPPES)",           "DO",                                         cred_value("DO"),
    "Credential type (NPPES)",           "MD + DO",                                    cred_value("MD + DO"),
    "Credential type (NPPES)",           "Missing credential text",                    cred_value("Missing"),
    "Years since NPPES enumeration",     "<=5 yrs (likely resident/fellow)",           enum_value("<=5 yrs (likely resident/fellow)"),
    "Years since NPPES enumeration",     "6-10 yrs (pre-cert window)",                 enum_value("6-10 yrs (pre-cert window)"),
    "Years since NPPES enumeration",     "11-20 yrs (mid-career)",                     enum_value("11-20 yrs (mid-career)"),
    "Years since NPPES enumeration",     ">20 yrs (senior)",                           enum_value(">20 yrs (senior)"),
    "NPPES (Feb 2026 file)",             "Enumeration year, median (IQR)",             gate(fmt_med_iqr(df_zone$nppes_enum_year),            avail$nppes),
    "NPPES (Feb 2026 file)",             "Years since enumeration, median (IQR)",      gate(fmt_med_iqr(df_zone$years_since_enum),           avail$nppes),
    "ABOto certification",               "Years since initial cert, median (IQR)",     gate(fmt_med_iqr(df_zone$years_since_aboto_cert),     avail$aboto),
    "ABOto certification",               "Subspecialty: General Otolaryngology",       gate(fmt_pct(sum(df_zone$aboto_subspecialty == "General Otolaryngology", na.rm = TRUE), N), avail$aboto),
    "ABOto certification",               "Subspecialty: Pediatric Otolaryngology",     gate(fmt_pct(sum(df_zone$aboto_subspecialty == "Pediatric Otolaryngology", na.rm = TRUE), N), avail$aboto),
    "ABOto certification",               "Subspecialty: Neurotology",                  gate(fmt_pct(sum(df_zone$aboto_subspecialty == "Neurotology", na.rm = TRUE), N), avail$aboto),
    "ABOto certification",               "Subspecialty: Sleep Medicine",               gate(fmt_pct(sum(df_zone$aboto_subspecialty == "Sleep Medicine", na.rm = TRUE), N), avail$aboto),
    "ENThealth registry",                "Age, years, median (IQR)",                   gate(fmt_med_iqr(df_zone$age_final),                  avail$enthealth),
    "ENThealth registry",                "Years since residency, median (IQR)",        gate(fmt_med_iqr(df_zone$ent_years_since_resi),       avail$enthealth),
    "ENThealth registry",                "Career stage: Early (<=10 yrs)",             gate(fmt_pct(sum(df_zone$ent_career_stage == "Early (<=10 yrs)", na.rm = TRUE), N), avail$enthealth),
    "ENThealth registry",                "Career stage: Mid (11-25 yrs)",              gate(fmt_pct(sum(df_zone$ent_career_stage == "Mid (11-25 yrs)",  na.rm = TRUE), N), avail$enthealth),
    "ENThealth registry",                "Career stage: Senior (>25 yrs)",             gate(fmt_pct(sum(df_zone$ent_career_stage == "Senior (>25 yrs)", na.rm = TRUE), N), avail$enthealth),
    "Medicare Part B (2022 or 2023)",    "Billed any Medicare Part B claim",           any_pb_yes,
    "Medicare Part B (2022 or 2023)",    "No Medicare Part B claim",                   any_pb_no
  )

  # AAO-HNS BOG region rows (one per region + an Other/Missing bucket)
  bog_rows <- tibble::tibble(
    row_group = "AAO-HNS BOG Region",
    statistic = c(BOG_LEVELS, "Other / Missing state"),
    value     = c(vapply(BOG_LEVELS, bog_region_row, character(1),
                         df_zone = df_zone),
                  bog_other_row(df_zone))
  )

  # ENT procedure categories billed in Medicare Part B (2022 or 2023). The
  # ENThealth-only-no-NPI subgroup is impossible to query (no NPI), so we
  # render "—" if 100% of the zone lacks an NPI; otherwise N is the zone N.
  n_with_npi <- sum(!is.na(df_zone$npi_chr))
  proc_value <- function(cat_name) {
    if (n_with_npi == 0) return(em)
    fmt_pct(sum(df_zone[[cat_name]], na.rm = TRUE), N)
  }
  any_proc <- if (n_with_npi == 0) em else fmt_pct(sum(df_zone$billed_any_ent_proc, na.rm = TRUE), N)
  no_proc  <- if (n_with_npi == 0) em else fmt_pct(sum(!df_zone$billed_any_ent_proc, na.rm = TRUE), N)

  proc_rows <- tibble::tibble(
    row_group = "ENT procedures billed (Medicare Part B 2022 or 2023)",
    statistic = c("Any ENT procedure billed",
                  "No ENT procedure billed",
                  PROC_CATEGORIES),
    value     = c(any_proc, no_proc,
                  vapply(PROC_CATEGORIES, proc_value, character(1)))
  )

  dplyr::bind_rows(base_rows, bog_rows, proc_rows)
}

stats_long <- roster %>%
  group_by(zone_id) %>%
  group_modify(~ build_zone_stats(.x, .y$zone_id)) %>%
  ungroup()

# Pivot wide: rows = (row_group, statistic), cols = zone
stats_wide <- stats_long %>%
  pivot_wider(names_from = zone_id, values_from = value) %>%
  select(row_group, statistic, all_of(zone_order))

# Header row: extract N's so we can append to column labels (and drop the header row)
ns <- stats_wide %>% filter(statistic == "N") %>% select(-row_group, -statistic)
stats_wide <- stats_wide %>% filter(statistic != "N")

# ── Build column labels with n = X ──────────────────────────────────────────
col_labels <- setNames(
  sprintf("%s\nn = %s", zone_label[zone_order], unlist(ns[1, ], use.names = FALSE)),
  zone_order
)

# ── gt rendering ────────────────────────────────────────────────────────────
gt_tbl <- stats_wide %>%
  gt(rowname_col = "statistic", groupname_col = "row_group") %>%
  cols_label(.list = as.list(col_labels)) %>%
  tab_header(
    title = md(sprintf("**Table 1%s.** Demographics of ENT physicians by Euler-diagram zone",
                       if (physician_only) "" else " (UNFILTERED, includes PA-C / NP / AUD)")),
    subtitle = md(if (physician_only) {
      "Cohort partitioned by membership in NPPES 207Y* universe, ABOto active board certification, and AAO-HNS ENThealth public directory (CONUS, individuals). **Physicians only**: NPIs with non-physician credentials (PA-C, NP, APRN, AUD, audiologist, PhD/MS-only, DDS, etc.) excluded; NPIs with blank credential text retained as likely physicians."
    } else {
      "Cohort partitioned by membership in NPPES 207Y* universe, ABOto active board certification, and AAO-HNS ENThealth public directory (CONUS, individuals). **No credential filter applied** — non-physician NPIs (PA-C / NP / APRN / AUD / audiologist / PhD-only) are included in counts."
    })
  ) %>%
  tab_spanner(label = md("**All three registries**"), columns = c("triple_match")) %>%
  tab_spanner(label = md("**Two registries**"),       columns = c("nppes_board", "nppes_ent")) %>%
  tab_spanner(label = md("**Single registry**"),      columns = c("nppes_only", "ent_only", "board_only_state_mismatch")) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = list(cells_column_labels(everything()),
                     cells_row_groups(),
                     cells_title())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = all_of(zone_order))
  ) %>%
  cols_align(align = "center", columns = all_of(zone_order)) %>%
  cols_align(align = "left",   columns = "statistic") %>%
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(2),
    row_group.padding = px(4),
    heading.title.font.size = px(13)
  ) %>%
  tab_footnote(
    footnote = "Median (IQR) for continuous variables; n (%) for categorical. Em dashes indicate the variable is not measured in that zone (e.g., ABOto data is not available for physicians who are not board-certified). Percentages use the zone N as denominator.",
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "*Board Only = active ABOto board-certified physicians whose practice state is non-CONUS in NPPES but CONUS in ABOto (state-mismatch). They are excluded from the CONUS NPPES universe by design.",
    locations = cells_column_labels(columns = "board_only_state_mismatch")
  )

# ── Save outputs ────────────────────────────────────────────────────────────
docx_out <- file.path(dir_out, paste0("ent_euler_zone_table1", file_suffix, ".docx"))
html_out <- file.path(dir_out, paste0("ent_euler_zone_table1", file_suffix, ".html"))

gt::gtsave(gt_tbl, html_out)
cat("Saved HTML preview:\n  ", html_out, "\n")

word_ok <- tryCatch({
  gt::gtsave(gt_tbl, docx_out)
  TRUE
}, error = function(e) {
  cat("gt::gtsave -> .docx failed (", conditionMessage(e),
      ") - falling back to flextable.\n", sep = "")
  FALSE
})

if (!word_ok) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer",   quietly = TRUE)) {
    stop("Neither pandoc nor flextable+officer available; cannot write .docx")
  }
  # Minimal flextable fallback (no spanners, but correct content)
  ft <- flextable::flextable(stats_wide)
  flextable::save_as_docx(ft, path = docx_out)
}
cat("Saved Word table:\n  ", docx_out, "\n")
