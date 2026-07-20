# =============================================================================
# 03_analysis.R
# ENT Mystery Caller Study — Primary Analysis
#
# Input:  data/processed/ent_phase2_complete_*.csv (749 complete records)
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(mysterycall)

# -----------------------------------------------------------------------------
# Load complete records
# -----------------------------------------------------------------------------
f <- tail(sort(list.files(here("data", "processed"),
                          pattern = "ent_phase2_complete", full.names = TRUE)), 1)
message("Loading: ", f)
df <- read_csv(f, show_col_types = FALSE) |>
  mutate(
    ruca_category      = factor(ruca_category, levels = c("Urban", "Suburban", "Rural")),
    ruca_binary        = factor(ruca_binary, levels = c("Non-Rural", "Rural")),
    new_patient_status = factor(new_patient_status),
    ent_type           = factor(ent_type),
    appointment_with_md = appointment_with_physician == "With sampled physician"
  )

message(sprintf("  %d complete records loaded\n", nrow(df)))

# =============================================================================
# 1. COCHRAN SAMPLE SIZE — how many physicians do we need?
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("1. COCHRAN SAMPLE SIZE\n")
cat(strrep("=", 70), "\n")

# Population of ENT physicians: 11,333 (from board_cert_ent_universe file)
cochran   <- mysterycall_cochran_n(N = 11333, margin_of_error = 0.05)
n_required <- cochran$n
cat(sprintf("  Population N = 11,333 ENT physicians\n"))
cat(sprintf("  Required sample (±5%% margin, 95%% CI): n = %d\n", n_required))
cat(sprintf("  Current complete calls: n = %d (%.1f%% of required)\n",
            nrow(df), 100 * nrow(df) / n_required))

# =============================================================================
# 2. ACCEPTANCE RATE — overall and by rural/urban
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("2. ACCEPTANCE RATES\n")
cat(strrep("=", 70), "\n")

# Prepare binary accepted column (TRUE = taking new patients)
df <- df |>
  mutate(accepted = !is.na(taking_new_patients) & taking_new_patients == "Yes")

cat("\n--- Overall acceptance rate ---\n")
overall_acc <- mysterycall_acceptance_rate(
  df,
  accepted_col = "accepted",
  conf_level   = 0.95
)
print(overall_acc)

cat("\n--- Acceptance rate by RUCA (Rural vs Non-Rural) ---\n")
acc_by_ruca <- mysterycall_acceptance_rate(
  df |> filter(!is.na(ruca_binary)),
  accepted_col = "accepted",
  group_by     = "ruca_binary",
  conf_level   = 0.95
)
print(acc_by_ruca)

cat("\n--- Acceptance rate by ENT type ---\n")
acc_by_ent <- mysterycall_acceptance_rate(
  df |> filter(!is.na(ent_type)),
  accepted_col = "accepted",
  group_by     = "ent_type",
  conf_level   = 0.95
)
print(acc_by_ent)

# =============================================================================
# 3. WAIT TIME SUMMARY — overall and by rural/urban
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("3. WAIT TIME SUMMARY (business days)\n")
cat(strrep("=", 70), "\n")

df_wait <- df |> filter(!is.na(wait_days_business))

cat("\n--- Overall wait time ---\n")
wt_overall <- mysterycall_wait_time_summary(
  df_wait,
  wait_col   = "wait_days_business",
  conf_level = 0.95
)
print(wt_overall)

cat("\n--- Wait time by RUCA (Rural vs Non-Rural) ---\n")
wt_ruca <- mysterycall_wait_time_summary(
  df_wait |> filter(!is.na(ruca_binary)),
  wait_col   = "wait_days_business",
  group_by   = "ruca_binary",
  conf_level = 0.95
)
print(wt_ruca)

cat("\n--- Wait time by ENT type ---\n")
wt_ent <- mysterycall_wait_time_summary(
  df_wait |> filter(!is.na(ent_type)),
  wait_col   = "wait_days_business",
  group_by   = "ent_type",
  conf_level = 0.95
)
print(wt_ent)

# =============================================================================
# 4. BOOTSTRAP CONFIDENCE INTERVALS — acceptance rate by RUCA
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("4. BOOTSTRAP CIs — ACCEPTANCE RATE BY RUCA (Rural vs Non-Rural)\n")
cat(strrep("=", 70), "\n")

boot_acc <- mysterycall_bootstrap_ci(
  df |> filter(!is.na(ruca_binary)),
  outcome_col = "accepted",
  group_col   = "ruca_binary",
  n_boot      = 2000L,
  seed        = 42L,
  alpha       = 0.05,
  stat        = "proportion"
)
print(boot_acc)

cat("\n--- Bootstrap CIs — wait time (median) by RUCA (Rural vs Non-Rural) ---\n")
boot_wait <- mysterycall_bootstrap_ci(
  df_wait |> filter(!is.na(ruca_binary)),
  outcome_col = "wait_days_business",
  group_col   = "ruca_binary",
  n_boot      = 2000L,
  seed        = 42L,
  alpha       = 0.05,
  stat        = "median"
)
print(boot_wait)

# =============================================================================
# 5. CALLER RELIABILITY — inter-rater reliability across callers
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("5. INTER-RATER RELIABILITY\n")
cat(strrep("=", 70), "\n")

cat("\n--- Reliability for office_answered ---\n")
rel_answered <- mysterycall_caller_reliability(
  df |> filter(!is.na(caller) & tolower(caller) != "na" & !is.na(office_answered)),
  caller_col  = "caller",
  outcome_col = "office_answered",
  type        = "auto"
)
print(rel_answered)

cat("\n--- Reliability for accepted (taking new patients) ---\n")
rel_accepted <- mysterycall_caller_reliability(
  df |> filter(!is.na(caller) & tolower(caller) != "na" & !is.na(accepted)),
  caller_col  = "caller",
  outcome_col = "accepted",
  type        = "auto"
)
print(rel_accepted)

# =============================================================================
# 6. POISSON MODEL — wait days ~ rural/urban + ENT type
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("6. POISSON MODEL — WAIT DAYS ~ RUCA + ENT TYPE\n")
cat(strrep("=", 70), "\n")

df_model <- df_wait |>
  filter(!is.na(ruca_binary) & !is.na(ent_type) & !is.na(caller) &
         tolower(caller) != "na") |>
  mutate(
    ruca_binary = relevel(factor(ruca_binary), ref = "Non-Rural"),
    ent_type    = relevel(factor(ent_type),    ref = "General")
  )

cat(sprintf("  Model dataset: %d records\n", nrow(df_model)))

poisson_fit <- mysterycall_poisson_model(
  data             = df_model,
  outcome          = "wait_days_business",
  predictors       = c("ruca_binary", "ent_type"),
  random_intercept = "caller",
  conf_level       = 0.95
)
print(poisson_fit)

# =============================================================================
# 7. MARGINAL EFFECTS — rural vs non-rural wait time difference
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("7. MARGINAL EFFECTS — RUCA (Rural vs Non-Rural) ON WAIT DAYS\n")
cat(strrep("=", 70), "\n")

me <- mysterycall:::mysterycall_marginal_effects(
  model = poisson_fit,
  term  = "ruca_binary",
  data  = df_model,
  type  = "response"
)
print(me)

message("\nAnalysis complete.")
