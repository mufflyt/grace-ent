# =============================================================================
# 05_database_accuracy_paper.R
# ENT Physician Registry Validity — Database Accuracy Paper
#
# Central question:
#   Among clinicians identified as otolaryngologists in NPPES, what proportion
#   can be confirmed as active otolaryngologists at the listed practice, and
#   how does confirmation vary by corroboration in ABOto, ENTHealth, and DAC?
#
# Two principal validity outcomes:
#   (1) physician_at_location  — physician currently practices at listed NPPES address
#   (2) physician_active       — physician remains an active otolaryngologist somewhere
#
# Unit of analysis: physician-location listing (one NPPES address per NPI)
#
# Input:  data/processed/ent_phase2_all_*.csv          (960 call log records)
#         ~/isochrones/publication_materials/...        (Euler zone demographics)
#         data/processed/nppes_only_ent_ruca_*.csv      (NPPES-only cohort)
# Output: output/tables/  (CSV + Excel)
#         output/figures/ (PNG 300 DPI)
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(survey)
library(ggplot2)
library(openxlsx)
library(mysterycall)

# Suppress dplyr masking noise
options(dplyr.summarise.inform = FALSE)

OUT_TABLES  <- here("output", "tables")
OUT_FIGURES <- here("output", "figures")
dir.create(OUT_TABLES,  showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_FIGURES, showWarnings = FALSE, recursive = TRUE)

EULER_FILE <- path.expand(
  "~/isochrones/publication_materials/figures/ent_data_integrity/ent_euler_zone_npi_assignments_with_demographics.csv"
)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
message("Loading call log (all 960 records) ...")
f_all <- tail(sort(list.files(here("data", "processed"),
                               pattern = "ent_phase2_all", full.names = TRUE)), 1)
calls <- read_csv(f_all, show_col_types = FALSE) |>
  mutate(npi = as.character(npi))
message(sprintf("  %d records loaded", nrow(calls)))

message("Loading Euler zone demographics ...")
euler <- read_csv(EULER_FILE, show_col_types = FALSE) |>
  rename(npi = npi_chr) |>
  mutate(npi = as.character(npi))
message(sprintf("  %d physicians in Euler zone file", nrow(euler)))

# =============================================================================
# 2. JOIN CALL LOG TO EULER ZONE
# =============================================================================
message("Joining call log to Euler zone demographics ...")

# Normalise datasets column → overlap_zone (4 strata matching Euler zones)
calls <- calls |>
  mutate(
    overlap_zone = case_when(
      datasets %in% c("ABO, NPI but not ENT Health",
                      "ABO, NPI but no ENTHealth")   ~ "NPPES+ABOto",
      datasets == "ABO, NPI, ENT Health"              ~ "NPPES+ABOto+ENTHealth",
      TRUE                                            ~ NA_character_
    )
  )

df <- calls |>
  left_join(
    euler |> select(npi, zone, in_nppes, in_board, in_ent,
                    credential_class, billed_any_part_b, billed_any_ent_proc,
                    nppes_enum_year, years_since_enum, bog_region),
    by = "npi"
  )

message(sprintf("  Matched %d / %d call records to Euler zone file",
                sum(!is.na(df$zone)), nrow(df)))

# =============================================================================
# 3. DEFINE EXPLICIT VALIDITY OUTCOMES FROM CALL DISPOSITIONS
#    DO NOT infer physician presence from non-missing taking_new_patients.
#    DO NOT infer active ENT from sees_chief_complaint.
# =============================================================================
message("Deriving explicit validity outcomes ...")

df <- df |>
  mutate(
    # -----------------------------------------------------------------
    # (1) physician_at_location
    #     Confirmed YES: staff affirmed physician is affiliated with
    #       this practice (even if not taking new patients right now)
    #     Confirmed NO: staff explicitly said physician relocated
    #     Unknown: practice not reached, or disposition ambiguous
    # -----------------------------------------------------------------
    physician_at_location = case_when(
      taking_new_patients %in% c(
        "Yes",
        "No-practice is full but physician does still see established patients",
        "No-on leave of absence (like parental leave)"
      )                                                             ~ "confirmed_yes",
      taking_new_patients == "No-changed practices"                ~ "confirmed_no",
      TRUE                                                          ~ "unknown"
    ),

    # -----------------------------------------------------------------
    # (2) physician_active
    #     Confirmed YES: physician still practices medicine somewhere
    #       (may have moved, may be on leave, may be full — but active)
    #     Confirmed NO: physician explicitly retired, deceased, or
    #       left medicine
    #     Unknown: practice not reached or disposition not captured
    # -----------------------------------------------------------------
    physician_active = case_when(
      taking_new_patients %in% c(
        "Yes",
        "No-practice is full but physician does still see established patients",
        "No-on leave of absence (like parental leave)",
        "No-changed practices"
      )                                                             ~ "confirmed_yes",
      taking_new_patients == "No-retired or no longer practicing medicine" ~ "confirmed_no",
      TRUE                                                          ~ "unknown"
    ),

    # -----------------------------------------------------------------
    # invalid_reason — subcategory when listing is confirmed invalid
    # -----------------------------------------------------------------
    invalid_reason = case_when(
      phone_validity_flag == "invalid_format"                       ~ "wrong_disconnected_number",
      taking_new_patients == "No-changed practices"                 ~ "relocated",
      taking_new_patients == "No-retired or no longer practicing medicine" ~ "retired_inactive",
      office_answered == FALSE & phone_validity_flag == "valid"     ~ "not_reached_valid_phone",
      taking_new_patients == "No-not specified why"                 ~ "not_specified",
      TRUE                                                          ~ NA_character_
    ),

    # -----------------------------------------------------------------
    # listing_status — 4 exhaustive, mutually exclusive categories
    #   confirmed_valid:   physician confirmed at NPPES location
    #   confirmed_invalid: physician relocated, retired, or wrong number
    #   unresolved:        reached but status unclear (flag for ≥3 attempts)
    #   not_attempted:     no disposition recorded at all
    # NOTE: Currently "unresolved" approximated from call data; once
    # the protocol records attempt counts, this will be updated.
    # -----------------------------------------------------------------
    listing_status = case_when(
      physician_at_location == "confirmed_yes"                     ~ "confirmed_valid",
      physician_at_location == "confirmed_no"                      ~ "confirmed_invalid",
      physician_active == "confirmed_no"                           ~ "confirmed_invalid",
      phone_validity_flag == "invalid_format"                      ~ "confirmed_invalid",
      is.na(office_answered) & is.na(taking_new_patients)         ~ "not_attempted",
      TRUE                                                          ~ "unresolved"
    ),
    listing_status = factor(listing_status,
                            levels = c("confirmed_valid", "confirmed_invalid",
                                       "unresolved", "not_attempted"))
  )

cat("\n--- listing_status distribution ---\n")
print(count(df, listing_status))
cat("\n--- physician_at_location ---\n")
print(count(df, physician_at_location))
cat("\n--- physician_active ---\n")
print(count(df, physician_active))

# =============================================================================
# 4. SAMPLING WEIGHTS
#    Population counts from Euler zone file.
#    Sampling fractions: n_sampled / N_population for each overlap stratum.
#    NPPES-only calls not yet added; placeholder weight = 1.
# =============================================================================
message("Computing sampling weights ...")

# Population N from Euler zone data
pop_n <- euler |>
  count(zone) |>
  tibble::deframe()    # named vector: zone → N

cat("\nEuler zone population counts:\n")
print(pop_n)

# Sampled n from call log by overlap_zone
sampled_n <- df |>
  filter(!is.na(overlap_zone)) |>
  count(overlap_zone, name = "n_sampled")

# Map overlap_zone label to Euler zone label (must match zone column in euler file)
zone_map <- c(
  "NPPES+ABOto"           = "2_nppes_board",
  "NPPES+ABOto+ENTHealth" = "8_triple_match"
)

sampled_n <- sampled_n |>
  mutate(
    euler_zone  = zone_map[overlap_zone],
    pop_N       = pop_n[euler_zone],
    samp_frac   = n_sampled / pop_N,
    weight      = pop_N / n_sampled
  )

cat("\nSampling fractions by overlap zone:\n")
print(sampled_n)

# Assign weights back to individual records
df <- df |>
  left_join(sampled_n |> select(overlap_zone, weight), by = "overlap_zone") |>
  mutate(
    weight = case_when(
      !is.na(weight) ~ weight,
      TRUE           ~ 1          # NPPES-only (pending) and unmatched records
    )
  )

# Survey design object (with replacement; simple within-stratum)
svy <- svydesign(
  ids     = ~caller,          # cluster by caller for robust SEs
  strata  = ~overlap_zone,
  weights = ~weight,
  data    = df |> filter(!is.na(overlap_zone)),
  nest    = TRUE
)

# =============================================================================
# 5. INTER-RATER RELIABILITY
#    Assess agreement across callers for the primary call dispositions.
# =============================================================================
message("Computing inter-rater reliability ...")

rel_data <- df |>
  filter(!is.na(caller) & tolower(caller) != "na")

cat("\n--- Reliability: office_answered ---\n")
tryCatch({
  rel_answered <- mysterycall_caller_reliability(
    rel_data |> filter(!is.na(office_answered)),
    caller_col  = "caller",
    outcome_col = "office_answered",
    type        = "auto"
  )
  print(rel_answered)
}, error = function(e) message("  Skipped (insufficient overlapping pairs): ", e$message))

cat("\n--- Reliability: listing_status (confirmed_valid vs other) ---\n")
tryCatch({
  rel_valid <- mysterycall_caller_reliability(
    rel_data |>
      filter(!is.na(listing_status)) |>
      mutate(valid_lgl = listing_status == "confirmed_valid"),
    caller_col  = "caller",
    outcome_col = "valid_lgl",
    type        = "auto"
  )
  print(rel_valid)
}, error = function(e) message("  Skipped (insufficient overlapping pairs): ", e$message))

# =============================================================================
# 6. VALIDATION CASCADE — TABLE 2
#    Stage proportions with 95% CIs via mysterycall_acceptance_rate().
#    Bootstrap CIs (2,000 replicates) by registry-overlap stratum via
#    mysterycall_bootstrap_ci().
# =============================================================================
message("Computing validation cascade ...")

# Binary outcome columns needed by mysterycall functions
df <- df |>
  mutate(
    reached_lgl      = office_answered == TRUE,
    at_location_lgl  = physician_at_location == "confirmed_yes",
    active_lgl       = physician_active == "confirmed_yes" &
                       physician_at_location == "confirmed_yes",
    new_patient_lgl  = taking_new_patients == "Yes",
    confirmed_valid_lgl = listing_status == "confirmed_valid"
  )

# Overall cascade rates
cat("\n--- Overall: practice reached ---\n")
acc_reached <- mysterycall_acceptance_rate(
  df, accepted_col = "reached_lgl", conf_level = 0.95)
print(acc_reached)

cat("\n--- Overall: physician at location (of all sampled) ---\n")
acc_at_loc <- mysterycall_acceptance_rate(
  df, accepted_col = "at_location_lgl", conf_level = 0.95)
print(acc_at_loc)

cat("\n--- Overall: confirmed valid listing ---\n")
acc_valid <- mysterycall_acceptance_rate(
  df, accepted_col = "confirmed_valid_lgl", conf_level = 0.95)
print(acc_valid)

cat("\n--- Overall: accepting new patients ---\n")
acc_new_pt <- mysterycall_acceptance_rate(
  df, accepted_col = "new_patient_lgl", conf_level = 0.95)
print(acc_new_pt)

# Cascade by overlap_zone
cat("\n--- Confirmed valid listing by overlap zone ---\n")
acc_by_zone <- mysterycall_acceptance_rate(
  df |> filter(!is.na(overlap_zone)),
  accepted_col = "confirmed_valid_lgl",
  group_by     = "overlap_zone",
  conf_level   = 0.95
)
print(acc_by_zone)

# Bootstrap CIs by overlap zone (2,000 replicates)
cat("\n--- Bootstrap CIs: confirmed valid by overlap zone ---\n")
boot_valid <- mysterycall_bootstrap_ci(
  df |> filter(!is.na(overlap_zone)),
  outcome_col = "confirmed_valid_lgl",
  group_col   = "overlap_zone",
  n_boot      = 2000L,
  seed        = 42L,
  alpha       = 0.05,
  stat        = "proportion"
)
print(boot_valid)

# Bootstrap CIs by rurality
cat("\n--- Bootstrap CIs: confirmed valid by rurality ---\n")
boot_valid_ruca <- mysterycall_bootstrap_ci(
  df |> filter(!is.na(ruca_binary)),
  outcome_col = "confirmed_valid_lgl",
  group_col   = "ruca_binary",
  n_boot      = 2000L,
  seed        = 42L,
  alpha       = 0.05,
  stat        = "proportion"
)
print(boot_valid_ruca)

# Retain a summary table for export (raw counts + acceptance_rate output)
cascade_by_zone <- df |>
  filter(!is.na(overlap_zone)) |>
  group_by(overlap_zone) |>
  summarise(
    n_total        = n(),
    n_reached      = sum(reached_lgl,     na.rm = TRUE),
    n_at_location  = sum(at_location_lgl, na.rm = TRUE),
    n_active       = sum(active_lgl,      na.rm = TRUE),
    n_new_patients = sum(new_patient_lgl, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct_reached_of_total  = round(100 * n_reached      / n_total, 1),
    pct_at_loc_of_total   = round(100 * n_at_location  / n_total, 1),
    pct_active_of_total   = round(100 * n_active        / n_total, 1),
    pct_new_pt_of_total   = round(100 * n_new_patients  / n_total, 1),
    pct_at_loc_of_reached = round(100 * n_at_location  / n_reached, 1)
  )

cat("\n--- Cascade counts by overlap zone ---\n")
print(cascade_by_zone)

# =============================================================================
# 7. LISTING STATUS DISTRIBUTION — TABLE 3
#    Uses mysterycall_table_proportion() for clean n / % output.
# =============================================================================
message("Computing listing status and invalid reasons ...")

cat("\n--- Table 3a: listing_status ---\n")
table3_status <- mysterycall_table_proportion(df, listing_status)
print(table3_status)

cat("\n--- Table 3b: invalid_reason (among confirmed_invalid) ---\n")
table3_reasons <- mysterycall_table_proportion(
  df |> filter(listing_status == "confirmed_invalid"),
  invalid_reason
)
print(table3_reasons)

# =============================================================================
# 8. SENSITIVITY BOUNDS
#    Best-case: unresolved → valid
#    Worst-case: unresolved → invalid
# =============================================================================
message("Computing sensitivity bounds ...")

n_total      <- nrow(df)
n_valid      <- sum(df$listing_status == "confirmed_valid",    na.rm = TRUE)
n_invalid    <- sum(df$listing_status == "confirmed_invalid",  na.rm = TRUE)
n_unresolved <- sum(df$listing_status == "unresolved",         na.rm = TRUE)
n_not_att    <- sum(df$listing_status == "not_attempted",      na.rm = TRUE)

denom_obs   <- n_valid + n_invalid
denom_range <- n_total - n_not_att

sensitivity <- tibble(
  scenario    = c("Observed (unresolved excluded)",
                  "Best-case (unresolved → valid)",
                  "Worst-case (unresolved → invalid)"),
  denominator = c(denom_obs, denom_range, denom_range),
  n_valid_cnt = c(n_valid,   n_valid + n_unresolved, n_valid),
  pct_valid   = round(100 * c(n_valid / denom_obs,
                               (n_valid + n_unresolved) / denom_range,
                               n_valid / denom_range), 1)
)

cat("\n--- Sensitivity bounds ---\n")
print(sensitivity)

# =============================================================================
# 9. ADJUSTED MODEL — TABLE 4
#    Survey-weighted logistic regression (svyglm, quasibinomial)
#    Outcome: physician_at_location == "confirmed_yes"
#    Predictors: overlap_zone, years_since_enum, billed_any_part_b,
#                ruca_binary, bog_region, credential_class
#    NOTE: overlap_zone already encodes number-of-corroborating-registries;
#          do NOT add a redundant n_registries predictor.
#    DAC/Medicare billing are secondary activity signals, not gold standards.
# =============================================================================
message("Fitting survey-weighted logistic regression ...")

svy_model_data <- df |>
  filter(!is.na(overlap_zone) & listing_status %in% c("confirmed_valid", "confirmed_invalid")) |>
  mutate(
    outcome         = as.integer(physician_at_location == "confirmed_yes"),
    overlap_zone    = factor(overlap_zone, levels = c("NPPES+ABOto", "NPPES+ABOto+ENTHealth")),
    ruca_binary     = factor(ruca_binary,  levels = c("Non-Rural", "Rural")),
    credential_class = factor(credential_class, levels = c("MD", "DO", "Other", "Missing")),
    billed_any_part_b = as.integer(billed_any_part_b),
    years_since_enum  = as.numeric(years_since_enum)
  )

svy_fit <- svydesign(
  ids     = ~caller,
  strata  = ~overlap_zone,
  weights = ~weight,
  data    = svy_model_data,
  nest    = TRUE
)

tryCatch({
  model <- svyglm(
    outcome ~ overlap_zone + years_since_enum + billed_any_part_b +
              ruca_binary + bog_region + credential_class,
    design = svy_fit,
    family = quasibinomial(link = "logit")
  )
  table4 <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    mutate(across(c(estimate, conf.low, conf.high), \(x) round(x, 3)),
           p.value = round(p.value, 3))
  cat("\n--- Table 4: Adjusted ORs ---\n")
  print(table4)
}, error = function(e) {
  message("  Model failed (likely sparse data until NPPES-only calls complete): ", e$message)
  message("  Skipping Table 4 until full dataset available.")
  table4 <- NULL
})

# =============================================================================
# 10. FIGURE 2 — STUDY FLOW DIAGRAM (conventional, not Sankey)
#    Shows: sampled → attempted → reached → adjudicated → unresolved
# =============================================================================
message("Building Figure 2: study flow diagram ...")

flow_data <- tibble(
  stage = c("Sampled\nfrom registry", "Attempted\n(call placed)", "Office\nreached",
            "Disposition\nrecorded", "Confirmed\nvalid", "Confirmed\ninvalid", "Unresolved"),
  n     = c(
    nrow(df),
    sum(!is.na(df$office_answered) | df$phone_validity_flag == "invalid_format"),
    sum(df$office_answered == TRUE, na.rm = TRUE),
    sum(df$listing_status != "not_attempted"),
    n_valid, n_invalid, n_unresolved
  ),
  x = c(1, 2, 3, 4, 5, 5, 5),
  y = c(0, 0, 0, 0, 1, 0, -1)
)

fig2 <- ggplot(flow_data, aes(x = x, y = y)) +
  geom_tile(width = 0.9, height = 0.8, fill = "#E8F4FD", color = "#2F4F4F", linewidth = 0.7) +
  geom_text(aes(label = paste0(stage, "\nn = ", scales::comma(n))),
            size = 3.2, lineheight = 1.2) +
  geom_segment(data = tibble(x1 = c(1.45, 2.45, 3.45, 4.45, 4.45),
                              x2 = c(1.95, 2.95, 3.95, 4.95, 4.95),
                              y1 = c(0, 0, 0, 1, -1),
                              y2 = c(0, 0, 0, 1, -1),
                              xend = c(2.05, 3.05, 4.05, 5.05, 5.05),
                              yend = c(0, 0, 0, 1, -1)),
               aes(x = x1, y = y1, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")), color = "#2F4F4F") +
  scale_x_continuous(limits = c(0.5, 5.6)) +
  scale_y_continuous(limits = c(-1.6, 1.6)) +
  labs(title = "Study Flow: NPPES ENT Listing Validation",
       subtitle = "Mystery Caller Telephone Audit") +
  theme_void() +
  theme(plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"))

ggsave(file.path(OUT_FIGURES, "fig2_study_flow.png"),
       fig2, width = 12, height = 5, dpi = 300)
message("  Saved fig2_study_flow.png")

# =============================================================================
# 11. FIGURE 3 — FOREST PLOT: listing validity by overlap zone
# =============================================================================
message("Building Figure 3: forest plot by overlap zone ...")

forest_data <- cascade_by_zone |>
  rowwise() |>
  mutate(
    prop  = n_at_location / n_total,
    lower = prop.test(n_at_location, n_total)$conf.int[1],
    upper = prop.test(n_at_location, n_total)$conf.int[2]
  ) |>
  ungroup() |>
  mutate(
    zone_label = factor(overlap_zone,
                        levels = rev(c("NPPES only (pending)",
                                       "NPPES+ABOto",
                                       "NPPES+ENTHealth",
                                       "NPPES+ABOto+ENTHealth")))
  )

fig3 <- ggplot(forest_data, aes(x = prop, y = zone_label)) +
  geom_point(size = 3.5, color = "#2F4F4F") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "#2F4F4F") +
  geom_text(aes(label = sprintf("%.1f%% (%d/%d)", 100 * prop, n_at_location, n_total)),
            hjust = -0.15, size = 3.2) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.15),
                     name = "Physician confirmed at listed NPPES location (%)") +
  scale_y_discrete(name = "Registry overlap stratum") +
  labs(title = "Listing Validity by Registry Corroboration Stratum",
       subtitle = "Error bars: 95% CI; NPPES-only pending") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray40", size = 10))

ggsave(file.path(OUT_FIGURES, "fig3_forest_validity.png"),
       fig3, width = 9, height = 5, dpi = 300)
message("  Saved fig3_forest_validity.png")

# =============================================================================
# 12. SAVE TABLES
# =============================================================================
message("Saving output tables ...")

wb <- createWorkbook()
modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")

hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#2F4F4F",
                   halign = "LEFT", textDecoration = "Bold", wrapText = TRUE)

add_sheet <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data)
  addStyle(wb, name, hdr, rows = 1, cols = seq_len(ncol(data)), gridExpand = TRUE)
  setColWidths(wb, name, cols = seq_len(ncol(data)), widths = "auto")
}

add_sheet(wb, "T2_Cascade",          cascade_by_zone)
add_sheet(wb, "T3a_Listing_Status",  table3_status)
add_sheet(wb, "T3b_Invalid_Reason",  table3_reasons)
add_sheet(wb, "Sensitivity_Bounds",  sensitivity)
if (exists("table4") && !is.null(table4)) add_sheet(wb, "T4_Logistic_Model", table4)

ts   <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
xlsx_path <- file.path(OUT_TABLES, paste0("db_accuracy_tables_", ts, ".xlsx"))
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

write_csv(cascade_by_zone,  file.path(OUT_TABLES, "table2_cascade.csv"))
write_csv(table3_status,    file.path(OUT_TABLES, "table3a_listing_status.csv"))
write_csv(table3_reasons,   file.path(OUT_TABLES, "table3b_invalid_reasons.csv"))
write_csv(sensitivity,      file.path(OUT_TABLES, "sensitivity_bounds.csv"))

message(sprintf("Saved Excel workbook: %s", xlsx_path))
message("Done.")

# =============================================================================
# NOTE: When NPPES-only calls are added to REDCap:
#   1. Rerun 01_clean_phase2_call_log.R to regenerate ent_phase2_all_*.csv
#   2. Rerun this script — sampling weights will auto-update to include
#      NPPES-only stratum (overlap_zone == "NPPES only")
#   3. Add CMS DAC linkage at step 2 (join by NPI, add in_dac column)
# =============================================================================
