#!/usr/bin/env Rscript

# ==============================================================================
# ENT BOARD-TO-NPI MATCHING PIPELINE (CANONICAL)
# ==============================================================================
# Purpose: Match ABOHNS (American Board of Otolaryngology – Head and Neck Surgery)
#          certification records to NPI profiles in the NPPES database.
#
# Logic: Uses a 4-pass tiered matching strategy with a "Hard Gender Gate" 
#        to prevent false positive matches for common names.
#
# Passes:
#   1. Strict: First + Last + State + Gender
#   2. Nickname: First 5 characters + Last + State + Gender
#   3. Fuzzy: Jaro-Winkler distance on First Name + Last + State + Gender
#   4. National: Unique Full Name + Gender (catches physicians who moved)
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(arrow)
library(stringdist)
library(here)

# Load robust gender inference system
source(here::here("R/gender_inference_system.R"))

# --- UTILITIES ---

#' Standardize names for joining
#' Removes non-alphabetical characters and converts to uppercase
.strip_special <- function(x) {
  gsub("[^A-Z]", "", toupper(as.character(x)))
}

# --- DATA LOADING ---

cat("Loading datasets...\n")

# ABOHNS Certification Data (The source list)
# Note: This is usually filtered for active/US-based providers in previous steps
abohns_raw <- read_csv(here::here("data/ent/abohns_certifications_clean.csv"), show_col_types = FALSE)

# NPPES ENT Data (The target pool)
# This is a pre-filtered subset of NPPES containing only Otolaryngologists
nppes_raw <- read_parquet(here::here("artifacts/ent/ent_from_nppes_flat_file.parquet"))

# --- PRE-PROCESSING ---

cat("Normalizing names and inferring genders...\n")

# 1. Process ABOHNS
abohns <- abohns_raw %>%
  # Deduplicate to 1 row per physician ID
  # Priority to non-generalist certifications if multiple exist
  arrange(desc(certification != "Otolaryngology - Head & Neck Surgery")) %>%
  distinct(abohns_id, .keep_all = TRUE) %>%
  mutate(
    .join_last    = .strip_special(last_name),
    .join_first   = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state   = state.abb[match(state_full, state.name)],
    .join_state   = case_when(
      state_full == "District of Columbia" ~ "DC", 
      state_full == "Puerto Rico" ~ "PR", 
      TRUE ~ .join_state
    )
  )

# 2. Infer genders for ABOHNS (used as the 'Gate' anchor)
# Vectorized call to gender::gender for the entire distinct set of names
cat("Calculating gender confidence for board records (Vectorized)...\n")
distinct_names <- unique(abohns$first_name)

# Note: try_gender_package in gender_inference_system.R is not vectorized.
# I will use the gender package directly here for speed, then fall back to the robust system for unknowns.
library(gender)
ssa_results <- gender::gender(distinct_names, years = c(1932, 2012), method = "ssa") %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    inferred_gender = ifelse(gender == "male", "M", "F"),
    gender_conf = ifelse(gender == "male", proportion_male * 100, (1 - proportion_male) * 100)
  ) %>%
  select(first_name = name, inferred_gender, gender_conf)

abohns <- abohns %>% left_join(ssa_results, by = "first_name")

# Fallback for names not in SSA data
missing_gender <- abohns %>% filter(is.na(inferred_gender)) %>% distinct(first_name)
if (nrow(missing_gender) > 0) {
  cat(sprintf("Running robust fallback for %d unique names...\n", nrow(missing_gender)))
  fallback_results <- missing_gender %>%
    rowwise() %>%
    mutate(gender_data = list(infer_gender_robust(first_name))) %>%
    ungroup() %>%
    mutate(
      inferred_gender_fb = sapply(gender_data, function(x) x$gender),
      gender_conf_fb     = sapply(gender_data, function(x) x$confidence)
    ) %>%
    select(first_name, inferred_gender_fb, gender_conf_fb)
  
  abohns <- abohns %>% 
    left_join(fallback_results, by = "first_name") %>%
    mutate(
      inferred_gender = coalesce(inferred_gender, inferred_gender_fb),
      gender_conf     = coalesce(gender_conf, gender_conf_fb)
    ) %>%
    select(-inferred_gender_fb, -gender_conf_fb)
}

# 3. Process NPPES
nppes <- nppes_raw %>%
  mutate(
    .join_last    = .strip_special(last_name),
    .join_first   = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state   = toupper(trimws(state))
  )

# Use existing NPPES gender (NPIs usually have this reported)
nppes <- nppes %>%
  mutate(np_gender = case_when(
    gender %in% c("M", "Male") ~ "M",
    gender %in% c("F", "Female") ~ "F",
    TRUE ~ NA_character_
  ))

# --- MATCHING ENGINE ---

working_df <- abohns %>% 
  mutate(
    npi = NA_character_, 
    match_strategy = NA_character_,
    match_gender_conf = NA_real_
  )

#' Generic Matching Wrapper with Gender Gating
#' 
#' @param strategy_name Label for the pass
#' @param ab_df The board dataset (unmatched rows only)
#' @param np_df The NPPES target dataset
#' @param join_cols Columns to join on
#' @param block_gender Logical. If TRUE, requires gender match if both are
#'   known.
run_match_pass <- function(strategy_name, ab_df, np_df, join_cols, block_gender = TRUE) {
  cat(sprintf("Running Pass: %s...\n", strategy_name))
  
  # Ensure target pool is unique for the join keys to avoid m:m expansion
  target <- np_df %>% distinct(across(all_of(join_cols)), .keep_all = TRUE)
  
  matches <- ab_df %>%
    filter(is.na(npi)) %>%
    inner_join(target %>% select(all_of(join_cols), npi_new = npi, np_gender), 
               by = join_cols)
  
  if (block_gender) {
    # GATE LOGIC:
    # 1. Match if both genders are known and identical
    # 2. Match if one gender is unknown (lax mode to prevent missing data drops)
    # 3. BLOCK if genders are known but different
    matches <- matches %>%
      filter(is.na(inferred_gender) | is.na(np_gender) | (inferred_gender == np_gender))
  }
  
  return(matches %>% mutate(npi = as.character(npi_new), match_strategy = strategy_name))
}

# --- EXECUTE PASSES ---

# Pass 1: Strict (Exact Name + State)
m1 <- run_match_pass("Pass 1: Strict (Name+State)", working_df, nppes, c(".join_first", ".join_last", ".join_state"))
working_df <- working_df %>% rows_update(m1 %>% select(abohns_id, npi, match_strategy), by = "abohns_id")

# Pass 2: Moderate (Nickname/First 5 + State)
m2 <- run_match_pass("Pass 2: Moderate (Nickname+State)", working_df, nppes, c(".join_first_5", ".join_last", ".join_state"))
working_df <- working_df %>% rows_update(m2 %>% select(abohns_id, npi, match_strategy), by = "abohns_id")

# Pass 3: Fuzzy (JW Dist < 0.1 + State)
cat("Running Pass: Pass 3: Fuzzy Match (Typos)...\n")
p3_candidates <- working_df %>%
  filter(is.na(npi)) %>%
  inner_join(nppes %>% select(.join_last, .join_state, npi_new = npi, np_first = .join_first, np_gender), 
             by = c(".join_last", ".join_state")) %>%
  mutate(dist = stringdist::stringdist(.join_first, np_first, method = "jw")) %>%
  filter(dist < 0.1) %>%
  # Gender gate for fuzzy matches
  filter(is.na(inferred_gender) | is.na(np_gender) | (inferred_gender == np_gender)) %>%
  distinct(abohns_id, .keep_all = TRUE) %>%
  mutate(npi = as.character(npi_new), match_strategy = "Pass 3: Fuzzy")

working_df <- working_df %>% rows_update(p3_candidates %>% select(abohns_id, npi, match_strategy), by = "abohns_id")

# Pass 4: National (Full Name, No State)
# We only do this if the name is UNIQUE in NPPES to avoid false positives
p4_target <- nppes %>%
  group_by(.join_first, .join_last) %>%
  filter(n() == 1) %>%
  ungroup()

m4 <- run_match_pass("Pass 4: National (Name Only)", working_df, p4_target, c(".join_first", ".join_last"))
working_df <- working_df %>% rows_update(m4 %>% select(abohns_id, npi, match_strategy), by = "abohns_id")

# --- FINAL CLEANUP ---

final_df <- working_df %>%
  mutate(
    practicing_status = ifelse(is.na(npi), "Likely Retired/Inactive", "Practicing")
  ) %>%
  select(
    abohns_id, npi, first_name, last_name, state_full, certification, 
    inferred_gender, gender_conf, match_strategy, practicing_status
  )

# --- EXPORT & SUMMARY ---

output_path <- here::here("data", "ent", "abohns_with_npi_master.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
tmp_output_path <- paste0(output_path, ".tmp")
write_csv(final_df, tmp_output_path)
file.rename(tmp_output_path, output_path)

cat("\n================================================================\n")
cat("MATCHING COMPLETE\n")
cat("================================================================\n")
cat(sprintf("Total ABOHNS Records: %d\n", nrow(final_df)))
cat(sprintf("Total Matches Found:  %d (%.1f%%)\n", sum(!is.na(final_df$npi)), 100 * sum(!is.na(final_df$npi)) / max(nrow(final_df), 1L)))
cat("\n--- Breakdown by Strategy ---\n")
print(table(final_df$match_strategy, useNA = "always"))

cat("\n--- Gender Confidence in Matched Cohort ---\n")
final_df %>% 
  filter(!is.na(npi)) %>% 
  summarize(mean_conf = mean(gender_conf, na.rm=TRUE), low_conf_n = sum(gender_conf < 80, na.rm=TRUE)) %>%
  print()

cat("\nResults saved to: ", output_path, "\n")
