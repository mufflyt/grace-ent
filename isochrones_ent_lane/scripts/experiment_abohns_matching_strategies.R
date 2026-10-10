library(dplyr)
library(readr)
library(stringr)
library(arrow)
library(stringdist)
library(here)

.strip_special <- function(x) {
  gsub("[^A-Z]", "", toupper(as.character(x)))
}

cat("Loading datasets...\n")
abohns <- read_csv(here::here("data/ent/abohns_certifications_clean.csv"), show_col_types = FALSE) %>%
  mutate(
    .join_last = .strip_special(last_name),
    .join_first = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state = state.abb[match(state_full, state.name)],
    .join_state = case_when(state_full == "District of Columbia" ~ "DC", state_full == "Puerto Rico" ~ "PR", TRUE ~ .join_state)
  )

nppes <- read_parquet(here::here("artifacts/ent/ent_from_nppes_flat_file.parquet")) %>%
  mutate(
    .join_last = .strip_special(last_name),
    .join_first = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state = toupper(trimws(state))
  )

# Define Strategies
run_match <- function(strategy_name, ab_df, np_df, join_cols) {
  cat(paste0("Running Strategy: ", strategy_name, "...\n"))
  
  # Ensure NP is distinct for the join columns
  np_distinct <- np_df %>%
    distinct(across(all_of(join_cols)), .keep_all = TRUE)
  
  matched <- ab_df %>%
    left_join(np_distinct %>% select(all_of(join_cols), npi), by = join_cols)
  
  match_count <- sum(!is.na(matched$npi))
  return(list(count = match_count, df = matched))
}

# 1. Strategy: Strict (Full First + Last + State)
s1 <- run_match("Strict (Full First + Last + State)", abohns, nppes, c(".join_first", ".join_last", ".join_state"))

# 2. Strategy: Moderate (First 5 + Last + State) - CURRENT
s2 <- run_match("Moderate (First 5 + Last + State)", abohns, nppes, c(".join_first_5", ".join_last", ".join_state"))

# 3. Strategy: National (Full First + Last, No State)
s3 <- run_match("National (Full First + Last, No State)", abohns, nppes, c(".join_first", ".join_last"))

# 4. Strategy: Fuzzy (Last + State, Fuzzy First)
# (Doing this by joining on Last+State then filtering by string distance)
cat("Running Strategy: Fuzzy (Last + State, Jaro-Winkler First)...\n")
fuzzy_candidates <- abohns %>%
  inner_join(nppes %>% select(.join_last, .join_state, npi, nppes_first = .join_first), 
             by = c(".join_last", ".join_state")) %>%
  mutate(dist = stringdist::stringdist(.join_first, nppes_first, method = "jw")) %>%
  filter(dist < 0.15) %>% # Tolerance for typos/variations
  distinct(abohns_id, .keep_all = TRUE)

s4_count <- nrow(fuzzy_candidates)

# --- Summary ---
results_table <- data.frame(
  Strategy = c("Strict", "Moderate (First 5)", "National (No State)", "Fuzzy (JW < 0.15)"),
  Matches = c(s1$count, s2$count, s3$count, s4_count)
)

cat("\n--- Comparison Table ---\n")
print(results_table)

# --- False Positive Audit ---
# Look at things found in 'National' but NOT in 'Moderate'
cat("\n--- National-only Matches (Potential movers or false positives) ---\n")
national_only <- s3$df %>%
  filter(!is.na(npi)) %>%
  anti_join(s2$df %>% filter(!is.na(npi)), by = "abohns_id") %>%
  select(first_name, last_name, location, npi)
print(national_only)

cat("\n--- Fuzzy-only Matches (Potential nicknames/typos) ---\n")
fuzzy_only <- fuzzy_candidates %>%
  anti_join(s2$df %>% filter(!is.na(npi)), by = "abohns_id") %>%
  select(first_name, last_name, nppes_first, dist)
print(fuzzy_only)
