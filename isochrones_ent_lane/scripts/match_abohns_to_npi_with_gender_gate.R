library(dplyr)
library(readr)
library(stringr)
library(arrow)
library(stringdist)
library(gender)
library(here)

# Load existing gender inference system if possible
# source(here::here("R/gender_inference_system.R"))

.strip_special <- function(x) {
  gsub("[^A-Z]", "", toupper(as.character(x)))
}

cat("Loading datasets...\n")
abohns_raw <- read_csv(here::here("data/ent/abohns_certifications_clean.csv"), show_col_types = FALSE)

# Deduplicate ABOHNS
abohns <- abohns_raw %>%
  arrange(desc(certification != "Otolaryngology - Head & Neck Surgery")) %>%
  distinct(abohns_id, .keep_all = TRUE) %>%
  mutate(
    .join_last = .strip_special(last_name),
    .join_first = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state = state.abb[match(state_full, state.name)],
    .join_state = case_when(state_full == "District of Columbia" ~ "DC", state_full == "Puerto Rico" ~ "PR", TRUE ~ .join_state),
    cert_year = as.numeric(str_extract(initial_certification, "[0-9]{4}"))
  )

nppes <- read_parquet(here::here("artifacts/ent/ent_from_nppes_flat_file.parquet")) %>%
  mutate(
    .join_last = .strip_special(last_name),
    .join_first = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state = toupper(trimws(state))
  )

# --- GENDER INFERENCE ---
cat("Inferring genders for blocking...\n")
unique_names <- unique(c(abohns$first_name, nppes$first_name))
# Simple gender inference using the gender package (SSA method)
gender_preds <- gender::gender(unique_names, method = "ssa") %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name, inferred_gender = gender)

abohns <- abohns %>% left_join(gender_preds, by = c("first_name" = "name"))
nppes <- nppes %>% left_join(gender_preds, by = c("first_name" = "name"))

# --- MATCHING WITH GENDER BLOCK ---
working_df <- abohns %>% mutate(npi = NA_character_, match_strategy = NA_character_)

match_with_gate <- function(strategy_name, ab_df, np_df, join_cols, block_gender = TRUE) {
  cat(paste0("Running: ", strategy_name, "...\n"))
  
  target <- np_df %>% distinct(across(all_of(join_cols)), .keep_all = TRUE)
  
  matches <- ab_df %>%
    filter(is.na(npi)) %>%
    inner_join(target %>% select(all_of(join_cols), npi_new = npi, np_gender = inferred_gender), 
               by = join_cols) %>%
    mutate(npi_new = as.character(npi_new))
  
  if (block_gender) {
    # Only allow match if genders are non-NA and match, OR if one is NA (lax mode)
    matches <- matches %>%
      filter(is.na(inferred_gender) | is.na(np_gender) | (inferred_gender == np_gender))
  }
  
  return(matches %>% mutate(match_strategy = strategy_name))
}

# Pass 1: Strict + Gender Gate
m1 <- match_with_gate("Pass 1: Strict (Gender Gate)", working_df, nppes, c(".join_first", ".join_last", ".join_state"))
working_df <- working_df %>% rows_update(m1 %>% select(abohns_id, npi = npi_new, match_strategy), by = "abohns_id")

# Pass 2: Moderate + Gender Gate
m2 <- match_with_gate("Pass 2: Moderate (Gender Gate)", working_df, nppes, c(".join_first_5", ".join_last", ".join_state"))
working_df <- working_df %>% rows_update(m2 %>% select(abohns_id, npi = npi_new, match_strategy), by = "abohns_id")

# Pass 3: National + Gender Gate (STRICTER)
# For national matches, we DEMAND gender matches if both are known
m3 <- match_with_gate("Pass 3: National (Strict Gender Gate)", working_df, nppes, c(".join_first", ".join_last"))
working_df <- working_df %>% rows_update(m3 %>% select(abohns_id, npi = npi_new, match_strategy), by = "abohns_id")

# Final Cleanup and Status
final_df <- working_df %>%
  mutate(practicing_status = ifelse(is.na(npi), "Likely Retired/Inactive", "Practicing"))

# Export
output_path <- here::here("data/ent/abohns_with_npi_gender_gated.csv")
write_csv(final_df, output_path)

cat("\n--- Final Matching Summary (GENDER GATED) ---\n")
print(table(final_df$match_strategy, useNA = "always"))

cat("\n--- Gender Distribution (Matched Cohort) ---\n")
final_df %>% filter(!is.na(npi)) %>% count(inferred_gender) %>% print()

cat("\nResults saved to: ", output_path, "\n")
