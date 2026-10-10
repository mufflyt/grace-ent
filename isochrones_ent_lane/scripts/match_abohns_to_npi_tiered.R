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
abohns_raw <- read_csv(here::here("data/ent/abohns_certifications_clean.csv"), show_col_types = FALSE)

# Deduplicate ABOHNS to 1 row per physician for NPI matching
abohns <- abohns_raw %>%
  arrange(desc(certification != "Otolaryngology - Head & Neck Surgery")) %>%
  distinct(abohns_id, .keep_all = TRUE) %>%
  mutate(
    .join_last = .strip_special(last_name),
    .join_first = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state = state.abb[match(state_full, state.name)],
    .join_state = case_when(state_full == "District of Columbia" ~ "DC", state_full == "Puerto Rico" ~ "PR", TRUE ~ .join_state)
  )

# Use the full NPPES ENT subset as the match target
nppes <- read_parquet(here::here("artifacts/ent/ent_from_nppes_flat_file.parquet")) %>%
  mutate(
    .join_last = .strip_special(last_name),
    .join_first = .strip_special(first_name),
    .join_first_5 = substr(.join_first, 1, 5),
    .join_state = toupper(trimws(state))
  )

# Prep ABOHNS for matching
working_df <- abohns %>% mutate(npi = NA_character_, match_strategy = NA_character_)

# --- Pass 1: Strict (First + Last + State) ---
cat("Pass 1: Strict Match...\n")
p1_target <- nppes %>% distinct(.join_first, .join_last, .join_state, .keep_all = TRUE)
matches_p1 <- working_df %>%
  filter(is.na(npi)) %>%
  inner_join(p1_target %>% select(.join_first, .join_last, .join_state, npi_new = npi), 
             by = c(".join_first", ".join_last", ".join_state")) %>%
  mutate(npi = as.character(npi_new), match_strategy = "Pass 1: Strict") %>%
  select(abohns_id, npi, match_strategy)

working_df <- working_df %>%
  rows_update(matches_p1, by = "abohns_id")

# --- Pass 2: Moderate (First 5 + Last + State) ---
cat("Pass 2: Moderate Match (Nicknames)...\n")
p2_target <- nppes %>% distinct(.join_first_5, .join_last, .join_state, .keep_all = TRUE)
matches_p2 <- working_df %>%
  filter(is.na(npi)) %>%
  inner_join(p2_target %>% select(.join_first_5, .join_last, .join_state, npi_new = npi), 
             by = c(".join_first_5", ".join_last", ".join_state")) %>%
  mutate(npi = as.character(npi_new), match_strategy = "Pass 2: Moderate") %>%
  select(abohns_id, npi, match_strategy)

working_df <- working_df %>%
  rows_update(matches_p2, by = "abohns_id")

# --- Pass 3: Fuzzy (Same State, JW < 0.1) ---
cat("Pass 3: Fuzzy Match (Typos)...\n")
p3_candidates <- working_df %>%
  filter(is.na(npi)) %>%
  inner_join(nppes %>% select(.join_last, .join_state, npi_new = npi, nppes_first = .join_first), 
             by = c(".join_last", ".join_state")) %>%
  mutate(dist = stringdist::stringdist(.join_first, nppes_first, method = "jw")) %>%
  filter(dist < 0.1) %>%
  distinct(abohns_id, .keep_all = TRUE) %>%
  mutate(npi = as.character(npi_new), match_strategy = "Pass 3: Fuzzy") %>%
  select(abohns_id, npi, match_strategy)

working_df <- working_df %>%
  rows_update(p3_candidates, by = "abohns_id")

# --- Pass 4: National (Full Name, No State) ---
cat("Pass 4: National Match (Movers)...\n")
# Ensure name is unique in NPPES nationwide to avoid false positives
p4_target <- nppes %>% 
  group_by(.join_first, .join_last) %>% 
  filter(n() == 1) %>% 
  ungroup()

matches_p4 <- working_df %>%
  filter(is.na(npi)) %>%
  inner_join(p4_target %>% select(.join_first, .join_last, npi_new = npi), 
             by = c(".join_first", ".join_last")) %>%
  mutate(npi = as.character(npi_new), match_strategy = "Pass 4: National") %>%
  select(abohns_id, npi, match_strategy)

working_df <- working_df %>%
  rows_update(matches_p4, by = "abohns_id")

# Final Cleanup
final_df <- working_df %>%
  mutate(practicing_status = ifelse(is.na(npi), "Likely Retired/Inactive", "Practicing"))

# Export
output_path <- here::here("data/ent/abohns_with_npi_master.csv")
write_csv(final_df, output_path)

cat("\n--- Final Matching Summary ---\n")
print(table(final_df$match_strategy, useNA = "always"))

cat("\n--- Final Status Summary ---\n")
print(table(final_df$practicing_status))

cat("\nResults saved to: ", output_path, "\n")
