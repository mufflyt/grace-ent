#!/usr/bin/env Rscript
# =============================================================================
# ENT Euler Diagram — Table 1 Demographics per Zone
#
# Builds a Table-1-style demographic summary for every zone of the ENT
# registry Euler diagram (publication_materials/figures/ent_data_integrity/
# ent_registry_overlap_euler_v2.png):
#
#   1. NPI Only                     (NPPES 207Y*, not Board, not ENThealth)
#   2. NPI & Board                  (NPPES & Board, not ENThealth)
#   3. NPI & ENThealth              (NPPES & ENThealth, not Board)
#   4. Board (CONUS) — NPPES non-CONUS only  (Board active, not in CONUS NPPES)
#   5. Board & ENThealth (no NPPES) (Board & ENThealth, not in NPPES)
#   6. ENThealth Only — has NPI     (ENThealth with NPI, not in NPPES, not Board)
#   7. ENThealth Only — no NPI      (ENThealth orphans, no NPI at all)
#   8. Triple Match                 (all three registries)
#
# Demographics reported (where available per zone):
#   N, Gender (F/M/missing), Age (mean ± SD), NPPES enumeration year,
#   Years since enumeration, ABOto years since initial cert,
#   Years since residency graduation (ENThealth), Career stage,
#   Top 5 states, Primary subspecialty distribution (ABOto-mapped)
#
# Outputs:
#   publication_materials/figures/ent_data_integrity/
#     ent_euler_zone_table1_long.csv     — tidy long format (one row per stat)
#     ent_euler_zone_table1_wide.csv     — Table-1 layout (rows = stats, cols = zones)
#     ent_euler_zone_npi_assignments.csv — per-NPI zone membership + raw demographics
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(arrow)
  library(DBI)
  library(duckdb)
})

source(here::here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()

# ── Reproduce set-membership logic ──────────────────────────────────────────
abohns_raw     <- read_csv(ct$input_paths$abohns_raw,     show_col_types = FALSE)
abohns_matched <- read_csv(ct$input_paths$abohns_matched, show_col_types = FALSE)
nppes_all      <- read_parquet(ct$input_paths$nppes)
ent_all        <- read_csv(ct$input_paths$ent,            show_col_types = FALSE)

if (!exists("convert_state_name_to_abbrev", mode = "function")) {
  source(here::here("R", "utils", "state_normalization.R"))
}
abohns_matched <- abohns_matched %>%
  mutate(state_code = unname(convert_state_name_to_abbrev(state_full)))

ent_all <- ent_all %>% filter(!state %in% NON_CONTIGUOUS_CODES | is.na(state))
abohns_conus <- abohns_matched %>%
  filter(!is.na(state_code) & !state_code %in% NON_CONTIGUOUS_CODES)
abohns_active <- abohns_conus %>%
  filter(practicing_status == "Practicing", !is.na(npi)) %>%
  mutate(npi_chr = as.character(npi))
nppes <- nppes_all %>%
  filter(!state %in% NON_CONTIGUOUS_CODES, !is.na(npi)) %>%
  mutate(npi_chr = as.character(npi))

# Per-NPI ABOto initial-cert year (earliest across rows with that NPI)
abohns_initial_year <- abohns_active %>%
  left_join(
    abohns_raw %>% select(abohns_id, initial_certification),
    by = "abohns_id"
  ) %>%
  mutate(initial_cert_year = suppressWarnings(as.integer(substr(initial_certification, 7, 10)))) %>%
  group_by(npi_chr) %>%
  summarise(aboto_initial_cert_year = suppressWarnings(min(initial_cert_year, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(aboto_initial_cert_year = ifelse(is.finite(aboto_initial_cert_year),
                                          aboto_initial_cert_year, NA_integer_))

# Subspecialty (ABOto-mapped) per NPI, computed exactly like ent_registry_counts
abohns_subspec_per_id <- abohns_raw %>%
  group_by(abohns_id) %>%
  summarise(
    has_pediatric   = any(certification == "Complex Pediatric Otolaryngology"),
    has_neurotology = any(certification == "Neurotology"),
    has_sleep       = any(certification == "Sleep Medicine"),
    .groups = "drop"
  )

aboto_subspec_per_npi <- abohns_active %>%
  left_join(abohns_subspec_per_id, by = "abohns_id") %>%
  group_by(npi_chr) %>%
  summarise(
    has_pediatric   = any(has_pediatric,   na.rm = TRUE),
    has_neurotology = any(has_neurotology, na.rm = TRUE),
    has_sleep       = any(has_sleep,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    aboto_subspecialty = dplyr::case_when(
      has_pediatric   ~ "Pediatric Otolaryngology",
      has_neurotology ~ "Neurotology",
      has_sleep       ~ "Sleep Medicine",
      TRUE            ~ "General Otolaryngology"
    )
  ) %>%
  select(npi_chr, aboto_subspecialty,
         aboto_inferred_gender = NULL)

# ABOto inferred gender per NPI (first non-missing)
aboto_gender_per_npi <- abohns_active %>%
  filter(!is.na(inferred_gender), inferred_gender != "") %>%
  distinct(npi_chr, .keep_all = TRUE) %>%
  select(npi_chr, aboto_inferred_gender = inferred_gender)

# ── Set membership ──────────────────────────────────────────────────────────
npi_nppes <- unique(nppes$npi_chr)
npi_board <- unique(abohns_active$npi_chr)
npi_ent   <- unique(as.character(ent_all$npi[!is.na(ent_all$npi)]))

all_npis <- unique(c(npi_nppes, npi_board, npi_ent))
zones_df <- tibble::tibble(npi_chr = all_npis) %>%
  mutate(in_nppes = npi_chr %in% npi_nppes,
         in_board = npi_chr %in% npi_board,
         in_ent   = npi_chr %in% npi_ent,
         zone = dplyr::case_when(
           in_nppes &  in_board &  in_ent ~ "8_triple_match",
            in_nppes &  in_board & !in_ent ~ "2_nppes_board",
            in_nppes & !in_board &  in_ent ~ "3_nppes_ent",
            in_nppes & !in_board & !in_ent ~ "1_nppes_only",
           !in_nppes &  in_board &  in_ent ~ "5_board_ent_no_nppes",
           !in_nppes &  in_board & !in_ent ~ "4_board_only_state_mismatch",
           !in_nppes & !in_board &  in_ent ~ "6_ent_only_with_npi",
           TRUE ~ NA_character_
         ))

# Orphan ENThealth (no NPI) — assemble synthetically
ent_orphan <- ent_all %>%
  filter(is.na(npi)) %>%
  mutate(npi_chr = NA_character_,
         in_nppes = FALSE, in_board = FALSE, in_ent = TRUE,
         zone = "7_ent_only_no_npi")

# ── Join all demographics onto zones_df ─────────────────────────────────────
nppes_demo <- nppes %>%
  distinct(npi_chr, .keep_all = TRUE) %>%
  select(npi_chr,
         nppes_state           = state,
         nppes_primary_taxonomy = primary_taxonomy)

ent_demo <- ent_all %>%
  filter(!is.na(npi)) %>%
  mutate(npi_chr = as.character(npi)) %>%
  distinct(npi_chr, .keep_all = TRUE) %>%
  select(npi_chr,
         ent_age              = age_final,
         ent_board_gender     = board_gender,
         ent_state            = state,
         ent_years_since_grad = years_since_grad,
         ent_years_since_resi = years_since_residency,
         ent_career_stage     = career_stage,
         ent_specialty        = specialty_primary)

# Pull full Feb 2026 NPPES demographics for *all* zone-NPI participants that
# have an NPI (covers in_nppes plus z_board_only & z_ent_only_with_npi).
if (!exists("load_paths", mode = "function")) source(here::here("R", "utils", "load_paths.R"))  # SSOT resolver (CLAUDE.md #13); fixes broken un-suffixed mount
duckdb_path <- load_paths(verbose = FALSE)$nber_duckdb_main
npi_for_duckdb <- unique(c(npi_nppes, npi_board, npi_ent))
con <- dbConnect(duckdb::duckdb(), duckdb_path, read_only = TRUE)
npi_sql <- paste(npi_for_duckdb, collapse = ",")
nppes_full <- dbGetQuery(con, sprintf('
  SELECT
    "NPI"::VARCHAR                          AS npi_chr,
    "Provider Sex Code"                     AS nppes_gender,
    "Provider Enumeration Date"             AS nppes_enum_date,
    "NPI Deactivation Date"                 AS nppes_deact_date
  FROM "NPPES_Data_Dissemination_February_2026_npidata_pfile_20050523_20260208_csv"
  WHERE "NPI" IN (%s)
', npi_sql))
dbDisconnect(con, shutdown = TRUE)

nppes_full <- nppes_full %>%
  mutate(
    nppes_enum_year = suppressWarnings(as.integer(substr(nppes_enum_date, 7, 10))),
    years_since_enum = 2026L - nppes_enum_year
  )

# Build per-NPI roster
roster_with_npi <- zones_df %>%
  left_join(nppes_demo,             by = "npi_chr") %>%
  left_join(nppes_full,             by = "npi_chr") %>%
  left_join(aboto_gender_per_npi,   by = "npi_chr") %>%
  left_join(aboto_subspec_per_npi,  by = "npi_chr") %>%
  left_join(abohns_initial_year,    by = "npi_chr") %>%
  left_join(ent_demo,               by = "npi_chr") %>%
  mutate(
    gender_final = dplyr::coalesce(nppes_gender, aboto_inferred_gender, ent_board_gender),
    state_final  = dplyr::coalesce(nppes_state, ent_state),
    age_final    = ent_age,
    years_since_aboto_cert = ifelse(!is.na(aboto_initial_cert_year),
                                    2026L - aboto_initial_cert_year, NA_integer_)
  )

# Append ENThealth orphans (no NPI) — only ENThealth demographics available
roster_orphan <- ent_orphan %>%
  transmute(
    npi_chr        = NA_character_,
    in_nppes, in_board, in_ent, zone,
    nppes_state    = NA_character_, nppes_primary_taxonomy = NA_character_,
    nppes_gender   = NA_character_, nppes_enum_date = NA_character_,
    nppes_deact_date = NA_character_, nppes_enum_year = NA_integer_,
    years_since_enum = NA_integer_,
    aboto_inferred_gender  = NA_character_,
    aboto_subspecialty     = NA_character_,
    has_pediatric = NA, has_neurotology = NA, has_sleep = NA,
    aboto_initial_cert_year = NA_integer_,
    ent_age = age_final, ent_board_gender = board_gender,
    ent_state = state, ent_years_since_grad = years_since_grad,
    ent_years_since_resi = years_since_residency,
    ent_career_stage = career_stage, ent_specialty = specialty_primary,
    gender_final   = board_gender,
    state_final    = state,
    age_final      = age_final,
    years_since_aboto_cert = NA_integer_
  )

# Align column sets
common_cols <- intersect(names(roster_with_npi), names(roster_orphan))
roster_all  <- bind_rows(
  roster_with_npi[, common_cols],
  roster_orphan[,  common_cols]
)

# ── Sanity: zone counts must match Euler diagram ────────────────────────────
zone_counts <- roster_all %>% count(zone, name = "n_actual")
expected <- tibble::tribble(
  ~zone,                            ~n_expected,
  "1_nppes_only",                   ct$zones$nppes_only,
  "2_nppes_board",                  ct$zones$nppes_board,
  "3_nppes_ent",                    ct$zones$nppes_ent,
  "4_board_only_state_mismatch",    ct$zones$board_only,
  "5_board_ent_no_nppes",           ct$zones$board_ent,
  "6_ent_only_with_npi",            ct$zones$ent_only_npi,
  "7_ent_only_no_npi",              ct$zones$ent_orphan,
  "8_triple_match",                 ct$zones$triple
)
check <- expected %>% left_join(zone_counts, by = "zone")
check <- check %>% mutate(n_actual = dplyr::coalesce(n_actual, 0L))
stopifnot("Zone counts do not match Euler diagram" =
            all(check$n_expected == check$n_actual))
cat("Zone counts validated against Euler diagram:\n"); print(check)

# ── Build Table 1 stats per zone ────────────────────────────────────────────
zone_label <- c(
  "1_nppes_only"                = "NPI Only (NPPES 207Y* only)",
  "2_nppes_board"               = "NPI & Board",
  "3_nppes_ent"                 = "NPI & ENThealth",
  "4_board_only_state_mismatch" = "Board only (NPPES non-CONUS)",
  "5_board_ent_no_nppes"        = "Board & ENThealth (no NPPES)",
  "6_ent_only_with_npi"         = "ENThealth only (has NPI, no NPPES)",
  "7_ent_only_no_npi"           = "ENThealth only (no NPI)",
  "8_triple_match"              = "Triple Match"
)

iqr_str <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return("—")
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf("%.0f (%.0f, %.0f)", q[2], q[1], q[3])
}
mean_sd <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return("—")
  sprintf("%.1f +/- %.1f", mean(x), sd(x))
}
pct <- function(n, d) if (d == 0) "—" else sprintf("%d (%.1f%%)", n, 100 * n / d)
top_n_states <- function(states, n = 5) {
  states <- states[!is.na(states) & states != ""]
  if (!length(states)) return("—")
  tab <- sort(table(states), decreasing = TRUE)
  paste(sprintf("%s (%d)", names(tab)[seq_len(min(n, length(tab)))],
                tab[seq_len(min(n, length(tab)))]),
        collapse = ", ")
}

table1 <- roster_all %>%
  group_by(zone) %>%
  summarise(
    `N`                            = dplyr::n(),
    `Gender: Female`               = pct(sum(gender_final == "F", na.rm = TRUE), dplyr::n()),
    `Gender: Male`                 = pct(sum(gender_final == "M", na.rm = TRUE), dplyr::n()),
    `Gender: Missing`              = pct(sum(is.na(gender_final) | gender_final == ""), dplyr::n()),
    `Age (ENThealth), mean +/- SD` = mean_sd(age_final),
    `Age available (n)`            = sum(is.finite(age_final)),
    `NPPES enumeration year, median (IQR)` = iqr_str(nppes_enum_year),
    `Years since NPPES enum, median (IQR)` = iqr_str(years_since_enum),
    `Years since ABOto initial cert, median (IQR)` = iqr_str(years_since_aboto_cert),
    `Years since residency (ENThealth), median (IQR)` = iqr_str(ent_years_since_resi),
    `Subspecialty: General Otolaryngology` = pct(sum(aboto_subspecialty == "General Otolaryngology", na.rm = TRUE), dplyr::n()),
    `Subspecialty: Pediatric`        = pct(sum(aboto_subspecialty == "Pediatric Otolaryngology", na.rm = TRUE), dplyr::n()),
    `Subspecialty: Neurotology`      = pct(sum(aboto_subspecialty == "Neurotology", na.rm = TRUE), dplyr::n()),
    `Subspecialty: Sleep Medicine`   = pct(sum(aboto_subspecialty == "Sleep Medicine", na.rm = TRUE), dplyr::n()),
    `Top 5 states`                   = top_n_states(state_final, 5),
    .groups = "drop"
  ) %>%
  mutate(zone_label = zone_label[zone]) %>%
  arrange(zone) %>%
  select(zone, zone_label, everything()) %>%
  mutate(across(-c(zone, zone_label), as.character))

# ── Long & wide outputs ─────────────────────────────────────────────────────
table1_long <- table1 %>%
  pivot_longer(cols = -c(zone, zone_label),
               names_to = "statistic",
               values_to = "value") %>%
  mutate(value = as.character(value))

# Wide (Table-1 layout: rows = statistics, columns = zones)
table1_wide <- table1 %>%
  select(-zone) %>%
  pivot_longer(-zone_label, names_to = "statistic", values_to = "value") %>%
  mutate(value = as.character(value)) %>%
  pivot_wider(names_from = zone_label, values_from = value)

# ── Write outputs ───────────────────────────────────────────────────────────
out_dir <- here::here("publication_materials", "figures", "ent_data_integrity")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write_csv(table1_long,
          file.path(out_dir, "ent_euler_zone_table1_long.csv"), na = "")
write_csv(table1_wide,
          file.path(out_dir, "ent_euler_zone_table1_wide.csv"), na = "")

# Per-NPI assignment + demographics (for inspection / supplementary)
roster_out <- roster_all %>%
  mutate(zone_label = zone_label[zone]) %>%
  select(npi_chr, zone, zone_label,
         in_nppes, in_board, in_ent,
         gender_final, age_final,
         nppes_gender, aboto_inferred_gender, ent_board_gender,
         nppes_enum_year, years_since_enum,
         aboto_initial_cert_year, years_since_aboto_cert,
         aboto_subspecialty,
         ent_years_since_resi, ent_career_stage, ent_specialty,
         state_final, nppes_primary_taxonomy)
write_csv(roster_out,
          file.path(out_dir, "ent_euler_zone_npi_assignments.csv"), na = "")

cat("\nWrote:\n")
cat("  ", file.path(out_dir, "ent_euler_zone_table1_long.csv"), "\n")
cat("  ", file.path(out_dir, "ent_euler_zone_table1_wide.csv"), "\n")
cat("  ", file.path(out_dir, "ent_euler_zone_npi_assignments.csv"), "\n")

cat("\n--- Table 1 (wide) ---\n")
print(table1_wide, n = 100, width = Inf)
