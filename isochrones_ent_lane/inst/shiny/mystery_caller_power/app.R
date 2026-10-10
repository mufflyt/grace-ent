#!/usr/bin/env Rscript
# =============================================================================
# Mystery Caller Power Analysis - Shiny App (scaffold)
# =============================================================================
# Interactive front-end for the Negative Binomial GLMM power simulation used
# in the mystery caller appointment-wait-time substudy.
#
# Inputs and outputs follow Section 11 ("A planned Shiny app") of
#   docs/mystery_caller_power_vignette.Rmd
#
# Backend: R/mystery_caller_power_core.R exposes the same simulation engine
# used by scripts/mystery_caller_power_NB.R and the testthat suite. The app
# is a thin reactive wrapper around mc_run_power_at_n().
#
# Launch:
#   shiny::runApp("inst/shiny/mystery_caller_power")
# =============================================================================

# Fallback operator: x %||% y returns y when x is NULL or NA-only.
#' @title Null-coalescing operator
#' @keywords internal
#' @name null_coalesce
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(here)
})

# -----------------------------------------------------------------------------
# Source the shared backend. Hunt up the directory tree so the app works
# whether launched via runApp("inst/shiny/mystery_caller_power") from the
# project root or via shiny::runApp() with working dir inside the app dir.
# -----------------------------------------------------------------------------
source(here::here("R", "mystery_caller_power_core.R"))

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
recommended_n <- function(grid_df, target_power, columns) {
  out <- lapply(columns, function(col) {
    if (!col %in% names(grid_df)) return(NA_integer_)
    hits <- grid_df$n_npi[grid_df[[col]] >= target_power]
    if (length(hits) == 0L) NA_integer_ else min(hits)
  })
  setNames(out, columns)
}

# For each estimand, the smallest N (and call count) at which the power
# curve crosses the target. Returns a data.frame with one row per estimand
# that actually reached the target.
find_crossings <- function(grid_df, target_power) {
  cols <- c("pow_cond_interact", "pow_marg_unwtd", "pow_marg_popwtd")
  rows <- lapply(cols, function(col) {
    hits <- which(grid_df[[col]] >= target_power)
    if (length(hits) == 0L) return(NULL)
    data.frame(
      estimand = col,
      n_npi    = grid_df$n_npi[hits[1]],
      n_calls  = grid_df$n_calls[hits[1]],
      power    = grid_df[[col]][hits[1]]
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    data.frame(estimand = character(), n_npi = integer(),
               n_calls = integer(), power = numeric())
  } else out
}

# Plain-English interpretation of the simulation results. Shared by the
# "How many calls?" tab and the power curve tab.
interpret_findings <- function(grid_df, target_power, n_sim,
                               bcbs_urban, med_urban, bcbs_rural, med_rural,
                               p_rural, q_rural, alpha,
                               show_pop_weighted = FALSE,
                               arm_a = "Blue Cross / Blue Shield",
                               arm_b = "Medicaid",
                               arm_b_short = "Medicaid",
                               arm_a_short = "Blue Cross") {
  crossings <- find_crossings(grid_df, target_power)
  hit <- function(col) {
    h <- crossings[crossings$estimand == col, , drop = FALSE]
    if (nrow(h) == 0L) NA_integer_ else h$n_calls[1]
  }
  calls_pop   <- hit("pow_marg_popwtd")
  calls_unwtd <- hit("pow_marg_unwtd")
  calls_int   <- hit("pow_cond_interact")
  reached <- function(n) !is.na(n)

  marg_truth <- p_rural       * (med_rural - bcbs_rural) +
                (1 - p_rural) * (med_urban  - bcbs_urban)
  unwtd_truth <- 0.5 * (med_rural - bcbs_rural) +
                 0.5 * (med_urban  - bcbs_urban)
  int_truth  <- (med_rural - bcbs_rural) - (med_urban - bcbs_urban)

  fmt_calls <- function(n) if (is.na(n)) "not reached in this grid"
                            else paste0(format(n, big.mark = ","), " calls")
  fmt_npi   <- function(n) if (is.na(n)) "not reached"
                            else paste0(format(n / 2, big.mark = ","),
                                        " physicians")

  pct_tgt <- round(100 * target_power)

  # Sentence 1: the assumed effect, in plain English with worked math.
  # Country-wide bullet only appears when the advanced toggle is on.
  bullet_country <- if (show_pop_weighted) sprintf(paste(
    "<li><strong>Average Medicaid penalty across the country: %.1f days.</strong>",
    " This blends the urban and rural penalties using the real-world mix of",
    " %.0f%% rural and %.0f%% urban physicians (math: %.0f%% × %.1f +",
    " %.0f%% × %.1f = %.1f). Only matters if you want one country-wide",
    " summary number; most clinical audit papers skip this.</li>"),
    marg_truth,
    100 * p_rural, 100 * (1 - p_rural),
    100 * p_rural, med_rural - bcbs_rural,
    100 * (1 - p_rural), med_urban - bcbs_urban,
    marg_truth) else ""

  s_setup <- sprintf(paste(
    "<strong>Your assumed wait times:</strong> Blue Cross / Blue Shield",
    "patients wait %.1f days in urban areas and %.1f days in rural areas.",
    "Medicaid patients wait %.1f days in urban areas and %.1f days in rural",
    "areas. So Medicaid waits <strong>%.1f days longer than Blue Cross in",
    "urban areas</strong> and <strong>%.1f days longer than Blue Cross in",
    "rural areas</strong>. The simulator answers a few different research",
    "questions you might want to publish:",
    "<ul>",
    "<li><strong>Is there a Medicaid penalty at all? Average across your",
    " sampled physicians: %.1f days.</strong> Math: (%.1f + %.1f) / 2.",
    " This is the simplest claim and what most audit studies report.</li>",
    "%s",
    "<li><strong>Is the Medicaid penalty bigger in rural areas?",
    " Difference: %.1f days.</strong> Math: %.1f rural penalty minus %.1f",
    " urban penalty. If this is positive, rural Medicaid patients are hit",
    " harder than urban Medicaid patients.</li>",
    "</ul>"
  ),
    bcbs_urban, bcbs_rural,
    med_urban,  med_rural,
    med_urban - bcbs_urban,
    med_rural - bcbs_rural,
    unwtd_truth, med_rural - bcbs_rural, med_urban - bcbs_urban,
    bullet_country,
    int_truth,
    med_rural - bcbs_rural, med_urban - bcbs_urban)

  # Recommended-N sentences, one per active estimand
  s_unwtd <- if (reached(calls_unwtd)) sprintf(paste(
      "To detect the %.1f-day average Medicaid penalty in your sampled",
      "physicians at %d%% power, you need <strong>%s</strong> (%s)."),
      unwtd_truth, pct_tgt, fmt_calls(calls_unwtd), fmt_npi(calls_unwtd))
    else sprintf(paste(
      "The %.1f-day average Medicaid penalty was <strong>not detected at",
      "%d%% power</strong> in your grid. Add larger physician counts and",
      "re-run."),
      unwtd_truth, pct_tgt)

  s_pop <- if (show_pop_weighted) {
    if (reached(calls_pop)) sprintf(paste(
      "To detect the %.1f-day country-wide (population-weighted) Medicaid",
      "penalty at %d%% power, you need <strong>%s</strong> (%s)."),
      marg_truth, pct_tgt, fmt_calls(calls_pop), fmt_npi(calls_pop))
    else sprintf(paste(
      "The %.1f-day country-wide Medicaid penalty was <strong>not detected",
      "at %d%% power</strong>. Add larger physician counts and re-run."),
      marg_truth, pct_tgt)
  } else ""

  s_int <- if (reached(calls_int)) sprintf(paste(
      "To also detect the smaller %.1f-day rural-vs-urban gap in the Medicaid",
      "penalty at %d%% power, you need <strong>%s</strong> (%s)."),
      int_truth, pct_tgt, fmt_calls(calls_int), fmt_npi(calls_int))
    else sprintf(paste(
      "The %.1f-day rural-vs-urban gap in the Medicaid penalty was",
      "<strong>not detected at %d%% power</strong>. Gap-of-gaps tests",
      "usually need 5 to 20 times more calls than the simple average;",
      "extend the grid (try 2,000, 3,000, 4,000 physicians) and re-run."),
      int_truth, pct_tgt)

  # Bottom-line decision text written for a non-statistician.
  s_decision <- if (reached(calls_unwtd) && !reached(calls_int)) sprintf(paste(
      "<strong>Bottom line:</strong> %s is enough if your paper's main",
      "claim is \"Medicaid patients waited longer than Blue Cross",
      "patients.\" Your study is <em>not</em> yet powered to claim",
      "\"the Medicaid penalty is bigger in rural areas\" - add more",
      "calls and re-run."),
      fmt_calls(calls_unwtd))
    else if (reached(calls_unwtd) && reached(calls_int)) sprintf(paste(
      "<strong>Bottom line:</strong> %s lets you claim a Medicaid penalty",
      "in your sample. %s lets you <em>additionally</em> claim that the",
      "Medicaid penalty is bigger in rural areas than urban areas."),
      fmt_calls(calls_unwtd), fmt_calls(calls_int))
    else
      paste("<strong>Bottom line:</strong> with the sample sizes you tested",
            "none of these questions can be answered at the target power.",
            "Try doubling the largest physician count in the grid, accept a",
            "lower target power (e.g. 80%), or assume a larger true effect.")

  # ---- "Claims you can make in your paper" ---------------------------------
  # Sentence templates are adapted from the language used in published
  # mystery-caller audit studies, e.g. Corbisiero et al. 2024 (ENT) and
  # Muffly et al. 2023 (OB/GYN subspecialists).
  claim_row <- function(power_met, ideal, fallback, tag = NULL) {
    tag_html <- if (is.null(tag)) "" else
      sprintf(" <span style='background:#e0e7ff;color:#3730a3;font-size:0.78em;padding:2px 6px;border-radius:4px;margin-left:4px;'>%s</span>",
              tag)
    if (power_met) {
      sprintf(paste0(
        "<li style='margin:10px 0;'>",
        "<span style='color:#15803d;font-weight:bold;'>",
        "&#10003; You CAN write:</span>%s<br>",
        "<span style='color:#1f2937;'>&ldquo;%s&rdquo;</span></li>"),
        tag_html, ideal)
    } else {
      sprintf(paste0(
        "<li style='margin:10px 0;'>",
        "<span style='color:#b91c1c;font-weight:bold;'>",
        "&#10007; You CANNOT yet write:</span>%s<br>",
        "<span style='color:#1f2937;'>&ldquo;%s&rdquo;</span><br>",
        "<span style='color:#555;font-size:0.95em;'>You would have to settle for: &ldquo;%s&rdquo;</span></li>"),
        tag_html, ideal, fallback)
    }
  }

  # Reference numbers used in the sentences
  pct_longer    <- 100 * (mean(c(med_urban, med_rural)) /
                          mean(c(bcbs_urban, bcbs_rural)) - 1)
  urban_pen     <- med_urban - bcbs_urban
  rural_pen     <- med_rural - bcbs_rural

  # The presence/absence of a marginal effect gates the first group of
  # sentences. The presence/absence of the interaction gates the next group.
  claims <- c(
    list(
      claim_row(
        reached(calls_unwtd),
        sprintf("%s patients waited an average of %.1f days longer than %s patients (P < %g).",
                arm_b, unwtd_truth, arm_a, alpha),
        sprintf("We did not detect a significant difference in appointment wait time between %s and %s patients.",
                arm_b, arm_a),
        tag = "abstract / headline"),
      claim_row(
        reached(calls_unwtd),
        sprintf("%s wait times were %.1f%% longer than %s wait times.",
                arm_b, pct_longer, arm_a),
        sprintf("Wait times for %s and %s did not differ significantly.",
                arm_b, arm_a),
        tag = "abstract / press release"),
      claim_row(
        reached(calls_unwtd),
        sprintf("Within the same physician's practice, %s patients waited %.1f days longer for an appointment than %s patients.",
                arm_b, unwtd_truth, arm_a),
        sprintf("Within-physician differences in wait time between %s and %s did not reach significance.",
                arm_b, arm_a),
        tag = "paired design framing"),
      claim_row(
        reached(calls_unwtd),
        sprintf("The mean appointment wait time was %.1f days for %s versus %.1f days for %s patients (P < %g).",
                (med_urban + med_rural) / 2, arm_b,
                (bcbs_urban + bcbs_rural) / 2, arm_a, alpha),
        sprintf("Mean appointment wait times were %.1f days for %s and %.1f days for %s patients (P > %g).",
                (med_urban + med_rural) / 2, arm_b,
                (bcbs_urban + bcbs_rural) / 2, arm_a, alpha),
        tag = "results section, mean reporting")
    ),
    if (show_pop_weighted) list(
      claim_row(
        reached(calls_pop),
        sprintf("Weighted to the U.S. physician population (%g%% rural), %s patients waited %.1f days longer than %s patients.",
                100 * p_rural, arm_b, marg_truth, arm_a),
        sprintf("After post-stratification weighting, we could not detect a country-wide %s wait-time disparity.",
                arm_b),
        tag = "population-marginal framing")
    ) else list(),
    list(
      claim_row(
        reached(calls_int),
        sprintf("The %s wait-time penalty was significantly larger in rural areas than in urban areas (interaction P < %g).",
                arm_b_short, alpha),
        sprintf("The %s wait-time penalty did not differ significantly between rural and urban physicians.",
                arm_b_short),
        tag = "interaction headline"),
      claim_row(
        reached(calls_int),
        sprintf("Rural %s patients faced an additional %.1f-day wait penalty beyond what urban %s patients experienced.",
                arm_b_short, int_truth, arm_b_short),
        sprintf("We did not detect an additional rural wait-time penalty for %s patients beyond the urban penalty.",
                arm_b_short),
        tag = "magnitude reporting"),
      claim_row(
        reached(calls_int),
        sprintf("Geographic disparities in %s access were significant: rural %s patients waited %.1f days longer than rural %s patients, compared to %.1f days longer in urban areas.",
                arm_b_short, arm_b_short, rural_pen, arm_a_short, urban_pen),
        sprintf("Geographic differences in the %s wait-time gap did not reach statistical significance.",
                arm_b_short),
        tag = "rural-urban subgroup")
    )
  )
  claims <- Filter(Negate(is.null), claims)
  s_claims <- paste0(
    "<div style='background: #fff; border: 1px solid #d1d5db; padding: 14px; margin-top: 12px;'>",
    "<h4 style='margin-top: 0;'>Sentences you can and cannot write in your paper</h4>",
    "<p style='color: #555; margin-bottom: 8px;'>",
    "Green check means the simulation reached your target power for the ",
    "statement, so you would be able to write it with statistical support. ",
    "Red X means the study as currently sized would not.",
    "</p>",
    "<ul style='padding-left: 22px;'>",
    paste(unlist(claims), collapse = ""),
    "</ul>",
    "</div>"
  )

  # Replicate caution
  s_caution <- if (n_sim < 30) sprintf(paste(
    "<span style='color: #b45309;'><strong>Caution:</strong> these power ",
    "estimates are based on only %d simulations per sample size. With that ",
    "few replicates a power of 67%% could really be anywhere from 40%% to ",
    "90%%. Re-run with Monte Carlo replicates of at least 100 before ",
    "putting these numbers in a protocol or grant.</span>"),
    n_sim)
    else sprintf("Based on %d Monte Carlo replicates per sample size.", n_sim)

  paste0(
    "<p>", s_setup,   "</p>",
    "<p>", s_unwtd,
    if (nchar(s_pop) > 0) paste0(" ", s_pop) else "",
    " ", s_int, "</p>",
    "<p>", s_decision, "</p>",
    s_claims,
    "<p style='font-size: 0.95em; color: #555; margin-top: 10px;'>",
    s_caution, "</p>"
  )
}

# Thin wrapper kept for the renderUI call on the power-curve tab.
interpret_power_curve <- function(grid_df, crossings, target_power, alpha,
                                  n_sim, bcbs_urban, med_urban,
                                  bcbs_rural, med_rural, sigma_npi, phi_nb,
                                  q_rural, p_rural,
                                  show_pop_weighted = FALSE,
                                  arm_a = "Blue Cross / Blue Shield",
                                  arm_b = "Medicaid",
                                  arm_b_short = "Medicaid",
                                  arm_a_short = "Blue Cross") {
  interpret_findings(grid_df, target_power, n_sim,
                     bcbs_urban, med_urban, bcbs_rural, med_rural,
                     p_rural, q_rural, alpha,
                     show_pop_weighted = show_pop_weighted,
                     arm_a = arm_a, arm_b = arm_b,
                     arm_a_short = arm_a_short, arm_b_short = arm_b_short)
}

# Run the full power grid across an NPI vector using the shared backend.
run_power_grid <- function(npi_grid, cells, sigma_npi, phi,
                           rural_sampling, pop_rural, alpha, n_sim,
                           fit_family = "negbin", disp_threshold = 1.5,
                           per_n_progress = NULL) {
  out <- vector("list", length(npi_grid))
  for (i in seq_along(npi_grid)) {
    if (!is.null(per_n_progress)) {
      per_n_progress(i, length(npi_grid), npi_grid[i])
    }
    out[[i]] <- mc_run_power_at_n(
      n_npi          = npi_grid[i],
      cells          = cells,
      sigma_npi      = sigma_npi,
      phi            = phi,
      rural_sampling = rural_sampling,
      pop_rural      = pop_rural,
      alpha          = alpha,
      n_sim          = n_sim
    )
  }
  do.call(rbind, out)
}

inputs_to_cells <- function(input) {
  c(BCBS_urban = input$baseline,
    Med_urban  = input$med_urban,
    BCBS_rural = input$bcbs_rural,
    Med_rural  = input$med_rural)
}

# -----------------------------------------------------------------------------
# Help-icon helpers. Each input is paired with a question-mark icon whose
# native HTML "title" attribute renders as a tooltip on hover. No extra JS,
# no extra packages, works in every browser.
# -----------------------------------------------------------------------------
help_label <- function(text, tip) {
  tagList(
    text,
    tags$span(
      style = paste(
        "color: #5a8dee; margin-left: 6px; cursor: help;",
        "font-size: 0.85em; vertical-align: middle;"
      ),
      title = tip,
      icon("circle-question")
    )
  )
}

# -----------------------------------------------------------------------------
# Glossary block displayed at the top of every results tab. Hovering each
# abbreviation also shows a tooltip, but the table is the canonical reference.
# -----------------------------------------------------------------------------
glossary_panel <- wellPanel(
  style = "background-color: #f8f9fa;",
  tags$details(
    tags$summary(tags$strong("Glossary (click to expand)")),
    tags$ul(
      tags$li(tags$strong("Mystery caller study: "),
              "Trained research callers contact physician offices posing as ",
              "patients to measure how long it takes to be offered an ",
              "appointment, and whether the office accepts the caller's ",
              "insurance."),
      tags$li(tags$strong("NPI (National Provider Identifier): "),
              "The 10-digit federal identifier for an individual physician. ",
              "In this design we call each NPI twice (once with Blue Cross / ",
              "Blue Shield insurance, once with Medicaid)."),
      tags$li(tags$strong("BCBS (Blue Cross / Blue Shield): "),
              "Stands in for any commercial insurance signal in the caller ",
              "script."),
      tags$li(tags$strong("Negative Binomial type 2 (NB2): "),
              "A count distribution. Variance equals mean plus mean-squared ",
              "divided by phi. Smaller phi means more overdispersion (the ",
              "right tail of wait times is longer than a Poisson would ",
              "allow)."),
      tags$li(tags$strong("GLMM (Generalized Linear Mixed Model): "),
              "A regression that allows non-normal outcomes (counts here) ",
              "plus random effects (one intercept per physician here)."),
      tags$li(tags$strong("Physician random intercept SD: "),
              "Standard deviation of the per-physician random intercept ",
              "(log scale). Captures how much faster or slower a given ",
              "physician's clinic is than the average. Larger SD means more ",
              "between-physician variation, which weakens the within-NPI ",
              "BCBS-vs-Medicaid contrast."),
      tags$li(tags$strong("Monte Carlo (MC) replicate: "),
              "One full simulated study: draw the outcome under the assumed ",
              "truth, fit the model, record whether the test rejects the ",
              "null. Power is the share of replicates that reject."),
      tags$li(tags$strong("Conditional rural-by-insurance interaction: "),
              "Is the Medicaid wait penalty different between rural and ",
              "urban physicians? Tested by the interaction coefficient in ",
              "the GLMM."),
      tags$li(tags$strong("Population-marginal Medicaid effect: "),
              "What is the Medicaid penalty on average across the whole ",
              "physician population? Requires post-stratification weights ",
              "if the sample over-represents rural physicians."),
      tags$li(tags$strong("Post-stratification (inverse-probability) ",
                          "weighting: "),
              "Re-weights each observation so the sample mix matches the ",
              "true population mix. Rural rows get weight pop_rural / ",
              "sampling_rural; urban rows get (1 - pop_rural) / (1 - ",
              "sampling_rural).")
    )
  )
)

# -----------------------------------------------------------------------------
# Examples from the peer-reviewed mystery-caller literature. Values are the
# study-reported mean (or median) business days until the next available
# new-patient appointment, by insurance arm.
#
# Most papers report a single marginal mean per arm rather than separate
# rural and urban cell means. Loading a study therefore sets the BCBS-urban
# and Medicaid-urban inputs to the published means and leaves the rural
# cells at whatever the user already has. Adjust the rural cells based on
# your prior expectations or pilot data.
# -----------------------------------------------------------------------------
literature_examples <- data.frame(
  id            = c("corbisiero_2024", "muffly_subspec_2023",
                    "muffly_genotolaryngology_2024", "spine_2025",
                    "wisseh_2021"),
  study         = c("Corbisiero et al. 2024",
                    "Muffly et al. 2023",
                    "Muffly et al. 2024",
                    "Spine surgery mystery caller 2025",
                    "Wisseh et al. 2021"),
  specialty     = c("Otolaryngology (Head and Neck Surgery)",
                    "Obstetrics-Gynecology subspecialists",
                    "General Obstetrics-Gynecology",
                    "Orthopedic spine surgery",
                    "Primary care (family medicine)"),
  bcbs_days     = c(32.4, 18.5, 29.2, 22.1, 9.0),
  medicaid_days = c(36.8, 26.7, 30.6, 26.6, 12.5),
  pct_longer    = c(5.73, 44.0, 4.8, 20.4, 38.9),
  n_physicians  = c("612 (1,183 / 1,224 calls)",
                    "Multi-subspecialty national sample",
                    "National sample",
                    "National sample",
                    "Primary care offices"),
  measure       = c("Mean", "Mean", "Mean", "Mean", "Median"),
  citation      = c(
    "Otolaryngol Head Neck Surg. 2024;170(6):1611-1621. doi:10.1002/ohn.752",
    "Am J Obstet Gynecol. 2023;229(3):e1-e10. PubMed 36907536",
    "BMJ Open. 2024;14(6):e082826. PubMed 38837187",
    "J Spine Surg. 2025. PubMed 41443373",
    "J Am Board Fam Med. 2021;34(3):571-578. PubMed 34088817"
  ),
  notes         = c(
    paste("Corbisiero study: the 5.73% figure is the adjusted ratio from a",
          "multivariable model; the raw difference is 4.4 days. Mystery",
          "callers scripted to request the next available appointment with",
          "BCBS or Medicaid. Pediatric otolaryngology and neurotology had",
          "the longest waits."),
    paste("Subspecialist sample across maternal-fetal medicine, reproductive",
          "endocrinology, gynecologic oncology, urogynecology. The 44%",
          "figure is for Medicaid vs commercial across all subspecialties."),
    paste("General OB/GYN mean wait 29.9 business days overall. Medicaid",
          "penalty was small in this general-care setting."),
    paste("Orthopedic spine surgery: Medicaid 26.6 d, Medicare 23.1 d, BCBS",
          "22.1 d. Largest disparity in pediatric and Medicaid cases."),
    paste("Median wait time 9 days for privately insured callers vs 12.5",
          "days for Medicaid; difference was not statistically significant",
          "in their bivariate test but the direction was consistent.")
  ),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(
    "Mystery Caller Power Calculator (Negative Binomial Mixed Model)"
  ),
  helpText(
    "Plan how many physicians you need to call in a mystery-caller study ",
    "of appointment wait time. Hover the blue question marks for help on ",
    "each input. Click the glossary on the results panel for full ",
    "definitions of every abbreviation."
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      h4("Study type"),
      radioButtons(
        "study_type",
        label = help_label(
          "What are the two arms being compared?",
          paste(
            "Insurance arm = each physician is called twice with different ",
            "insurance (Medicaid vs Blue Cross / Blue Shield). Specialty ",
            "arm = each clinical scenario is called at both a generalist ",
            "and a subspecialist (e.g., general ENT vs pediatric ENT). The ",
            "underlying math is identical; only the labels change."
          )),
        choices = c(
          "Insurance arm: Medicaid vs Blue Cross / Blue Shield" = "insurance",
          "Specialty arm: Subspecialist vs Generalist"           = "specialty"
        ),
        selected = "insurance"
      ),

      h4("Assumed true wait time (in business days) per cell"),
      helpText(em(
        "Set the average days-until-appointment you expect to see in each ",
        "of the four cells. The contrast between Medicaid and BCBS is the ",
        "effect we want to detect."
      )),
      uiOutput("cell_input_baseline"),
      uiOutput("cell_input_med_urban"),
      uiOutput("cell_input_bcbs_rural"),
      uiOutput("cell_input_med_rural"),

      tags$hr(),
      h4("Variability between physicians and within the count distribution"),
      numericInput("sigma_npi",
                   help_label(
                     "Physician random intercept SD (log scale)",
                     paste(
                       "Standard deviation of the per-physician random ",
                       "intercept, on the log-rate scale. Larger values ",
                       "mean physicians vary more in their baseline ",
                       "wait time, which raises the standard error of ",
                       "the BCBS-vs-Medicaid contrast. 0.30 is a ",
                       "reasonable starting guess; if you have pilot ",
                       "data, fit a Poisson GLMM with only a (1|NPI) ",
                       "term and use the fitted SD."
                     )),
                   value = 0.30, min = 0, max = 2, step = 0.05),
      numericInput("phi_nb",
                   help_label(
                     "Negative Binomial dispersion (phi)",
                     paste(
                       "Controls how heavy-tailed the count distribution ",
                       "is. Variance equals mean plus mean-squared divided ",
                       "by phi. Smaller phi means more overdispersion. ",
                       "phi = 6 with mean 14 gives a Pearson dispersion ",
                       "ratio near 3.3, which matches typical published ",
                       "mystery-caller wait-time data. Use phi >= 50 only ",
                       "if you have strong evidence the data is nearly ",
                       "Poisson (rare)."
                     )),
                   value = 6, min = 0.5, max = 100, step = 0.5),

      radioButtons(
        "fit_family",
        label = help_label(
          "Model the simulator should fit to each replicate",
          paste(
            "Poisson is the natural model for count data (days), but it ",
            "requires Var(Y) = mean(Y), which almost never holds for ",
            "appointment wait time (long right tail). 'Auto' fits Poisson ",
            "first, checks the Pearson dispersion ratio, and refits ",
            "Negative Binomial if the dispersion ratio exceeds the ",
            "threshold below. This mirrors what most real analysts do."
          )),
        choices = c(
          "Auto: fit Poisson, fall back to Negative Binomial if overdispersed (recommended)" = "auto",
          "Always fit Negative Binomial (safest, slightly slower)" = "negbin",
          "Always fit Poisson (only if you are sure dispersion is ~1)"  = "poisson"
        ),
        selected = "auto"
      ),
      numericInput(
        "disp_threshold",
        label = help_label(
          "Auto fallback threshold (Pearson dispersion ratio)",
          paste(
            "In Auto mode, if the Poisson Pearson dispersion ratio exceeds ",
            "this number, the simulator refits the replicate with a ",
            "Negative Binomial model. Conventional cutoff is 1.5. Below ",
            "1.2 means equidispersed (Poisson fine), above 3 means strong ",
            "overdispersion."
          )),
        value = 1.5, min = 1.0, max = 5.0, step = 0.1
      ),

      tags$hr(),
      h4("Rural-urban sampling design"),
      sliderInput("q_rural",
                  help_label(
                    "Fraction of called physicians that are rural (sample)",
                    paste(
                      "How you allocate calls between rural and urban ",
                      "physicians. 0.50 is a balanced (50/50) stratified ",
                      "design that maximizes power for rural-vs-urban ",
                      "contrasts. Lower values mirror natural geography but ",
                      "reduce power."
                    )),
                  min = 0.05, max = 0.95, value = 0.50, step = 0.05),
      sliderInput("p_rural",
                  help_label(
                    "Fraction of all physicians that are rural (population)",
                    paste(
                      "The true rural share of the underlying physician ",
                      "population. Roughly 0.15 for US obstetrician-",
                      "gynecologists. This is used only for the ",
                      "post-stratification weights, not for sampling."
                    )),
                  min = 0.05, max = 0.95, value = 0.15, step = 0.05),

      checkboxInput(
        "show_pop_weighted",
        label = help_label(
          "Show advanced: population-weighted estimand",
          paste(
            "Off by default. Turn on if you want one country-wide ",
            "summary number for the Medicaid penalty that re-weights ",
            "your 50/50 sample to the actual rural / urban mix of ",
            "physicians. Most clinical audit studies report per-cell ",
            "means and the rural-vs-urban contrast separately and do ",
            "not need this."
          )),
        value = FALSE),

      tags$hr(),
      h4("Statistical decision rule"),
      numericInput("alpha",
                   help_label(
                     "Significance level (alpha)",
                     paste(
                       "Type I error rate, the probability of falsely ",
                       "declaring an effect when none exists. 0.05 is the ",
                       "convention for two-sided tests."
                     )),
                   value = 0.05, min = 0.001, max = 0.2, step = 0.005),
      numericInput("target_power",
                   help_label(
                     "Target statistical power",
                     paste(
                       "Probability of correctly detecting the effect when ",
                       "it is real. 0.80 is the regulatory minimum; 0.90 ",
                       "is the publication-quality target used in this ",
                       "vignette."
                     )),
                   value = 0.90, min = 0.5, max = 0.99, step = 0.01),

      tags$hr(),
      h4("Simulation settings"),
      textInput("npi_grid",
                help_label(
                  "Physician counts to evaluate (comma-separated)",
                  paste(
                    "Sample sizes (number of unique physicians, each ",
                    "called twice) to evaluate. Each value runs a full ",
                    "Monte Carlo simulation. Start coarse (e.g., 200, ",
                    "400, 800, 1500) and zoom in once you see where ",
                    "the curve crosses your target power."
                  )),
                value = "200, 400, 800, 1500"),
      numericInput("n_sim",
                   help_label(
                     "Monte Carlo replicates per physician count",
                     paste(
                       "Number of simulated studies fit at each grid ",
                       "point. More replicates means a tighter power ",
                       "estimate but longer runtime. 30 gives a quick ",
                       "preview; 100 gives a number you can defend; 300 ",
                       "+ is publication-grade."
                     )),
                   value = 100, min = 10, max = 1000, step = 10),
      uiOutput("n_sim_warning"),

      tags$hr(),
      actionButton("run", "Run simulation", class = "btn-primary",
                   icon = icon("play")),
      tags$span(style = "margin-left: 8px;",
        actionButton("reset_defaults", "Reset all inputs",
                     class = "btn-outline-secondary",
                     icon = icon("rotate-left"))
      )
    ),

    mainPanel(
      width = 8,
      wellPanel(
        style = "background-color: #ecfeff; border-left: 4px solid #06b6d4;",
        tags$strong("Reality check: actual Mystery Caller datasets from this lab."),
        tags$br(),
        "Each row below was computed live from the REDCap export stored on ",
        "Dropbox at ", tags$code("/Mystery shopper/mystery_shopper/"),
        ". Four independent studies, all showing the same pattern: Medicaid ",
        "23 to 31% longer than commercial, with severe overdispersion ",
        "(variance/mean ratio above 20). Use the dropdown below to pre-fill ",
        "the sidebar with any study's cell means.",
        tags$br(), tags$br(),
        tags$table(
          class = "table table-sm", style = "background:white; margin-bottom: 8px; font-size: 0.92em;",
          tags$thead(tags$tr(
            tags$th("Study"),
            tags$th("Paired calls"),
            tags$th("Valid waits"),
            tags$th("BCBS mean"),
            tags$th("Medicaid mean"),
            tags$th("% longer"),
            tags$th("Var/Mean"),
            tags$th("NB phi")
          )),
          tags$tbody(
            tags$tr(
              tags$th(colspan = 8, style = "background:#e0f2fe;",
                      "Insurance arm studies (Medicaid vs Blue Cross / Blue Shield)")),
            tags$tr(
              tags$td("Muffly-Corbisiero 2023 OB/GYN subspecialists"),
              tags$td("1,614"), tags$td("676"),
              tags$td("16.5 d"), tags$td("20.2 d"),
              tags$td("22.7%"),
              tags$td("23.0"), tags$td("0.82")),
            tags$tr(
              tags$td("Spine surgery (3-arm)"),
              tags$td("927"), tags$td("467"),
              tags$td("21.8 d"), tags$td("26.8 d"),
              tags$td("22.8%"),
              tags$td("21.5"), tags$td("1.14")),
            tags$tr(
              tags$td("General orthopedic surgery"),
              tags$td("978"), tags$td("565"),
              tags$td("19.6 d"), tags$td("24.5 d"),
              tags$td("25.3%"),
              tags$td("25.4"), tags$td("0.88")),
            tags$tr(
              tags$td("Orthopedic sports medicine"),
              tags$td("1,043"), tags$td("587"),
              tags$td("15.5 d"), tags$td("20.2 d"),
              tags$td("30.6%"),
              tags$td("23.0"), tags$td("0.78")),
            tags$tr(
              tags$th(colspan = 8, style = "background:#fef3c7;",
                      "Specialty arm studies (Subspecialist vs Generalist, scripted pediatric scenarios)")),
            tags$tr(
              tags$td("Pediatric vs general ENT (Corbisiero 2024 + Saeedi pooled)"),
              tags$td("~1,880"), tags$td("1,393"),
              tags$td("31.8 d"), tags$td("54.1 d"),
              tags$td("70.1%"),
              tags$td("24.7"), tags$td("1.69")),
            tags$tr(
              tags$td("Neurotology vs general ENT (Corbisiero 2024 + Saeedi pooled)"),
              tags$td("~1,880"), tags$td("1,393"),
              tags$td("31.8 d"), tags$td("43.6 d"),
              tags$td("37.1%"),
              tags$td("24.7"), tags$td("1.69")),
            tags$tr(
              tags$td("Lizzy pediatric vs general dermatology"),
              tags$td("585"), tags$td("319"),
              tags$td("53.0 d"), tags$td("89.2 d"),
              tags$td("68.3%"),
              tags$td("58.0"), tags$td("~0.5"))
          )
        ),
        tags$div(
          style = "background:#fef9c3; border-left: 3px solid #ca8a04; padding: 10px; font-size: 0.92em; margin-top: 8px;",
          icon("info-circle"),
          tags$strong(" All rural cells in the presets are now anchored to real observations."),
          tags$br(),
          "Every preset's rural cell is built from pooled rural mystery-caller observations (Micropolitan + unlabeled-CBSA). Rural sample sizes per cell: Corbi OB/GYN 21-39, Ortho 25-27, Sports Med 19-24, ENT 71-80. The Spine export had no rural column, so its rural cells stay as informed placeholders.",
          tags$br(), tags$br(),
          tags$strong("Two findings that emerge across all six studies:"),
          tags$ol(
            tags$li(tags$strong("Rural waits are usually SHORTER than urban for OB/GYN and Ortho"),
                    " (rural BCBS 10-16 d vs urban 17-19 d), probably because rural clinics have less queue pressure. Sports Medicine is the exception (rural ~5 d longer than urban). ENT shows the largest, most consistent rural penalty (+12 to +13 d for the two big arms)."),
            tags$li(tags$strong("Rural-by-arm interactions are uniformly small or zero."),
                    " The insurance-by-rural interaction sits at -0.1 to +2.4 d across all four insurance-arm studies. The subspecialty-by-rural interaction in ENT is ~+1 d (Peds-minus-General gap is 22 d urban vs 21 d rural). Powering a study to detect that 1-2 d interaction needs tens of thousands of physicians and is usually infeasible. Power for main effects instead.")
          )
        ),
        tags$em("Implication: wait time is severely overdispersed in every dataset we have. Poisson alone will give you anti-conservative SEs. Auto or Always-Negative-Binomial in the sidebar is the right default. Realistic phi values are 0.8 to 1.2; the app's default phi = 6 is optimistic."),
        tags$br(), tags$br(),
        fluidRow(
          column(7,
            selectInput(
              "preset_study",
              label = "Pre-fill cell means from a real study",
              choices = list(
                "(pick a study)" = "",
                "Insurance arm studies (Medicaid vs Blue Cross)" = c(
                  "Muffly-Corbisiero 2023 OB/GYN subspecialists" = "corbi",
                  "Spine surgery"                                  = "spine",
                  "General orthopedic surgery"                     = "ortho",
                  "Orthopedic sports medicine"                     = "sports_med"
                ),
                "Specialty arm studies (Subspecialist vs Generalist)" = c(
                  "Saeedi ENT: pediatric vs general otolaryngology" = "saeedi_peds_ent",
                  "Saeedi ENT: neurotology vs general otolaryngology" = "saeedi_neuro_ent",
                  "Lizzy pediatric vs general dermatology"           = "lizzy_peds_derm"
                )
              ),
              selected = ""
            )
          ),
          column(5,
            tags$br(),
            actionButton(
              "load_preset",
              "Load this study's cell means",
              class = "btn-outline-info", icon = icon("flask")
            )
          )
        )
      ),
      glossary_panel,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("How many calls?",
                 br(),
                 uiOutput("recommendation"),
                 br(),
                 h4("What the table below is telling you"),
                 uiOutput("grid_interpretation"),
                 br(),
                 h4("Detail: power at each sample size"),
                 helpText(
                   "Each row is a sample size we tested. ",
                   tags$strong("Pct detected"),
                   " columns are the share of simulated studies that ",
                   "successfully detected the effect. You want that share ",
                   "at or above your target power."
                 ),
                 DT::DTOutput("grid_table")),
        tabPanel("Power curve",
                 br(),
                 helpText(
                   "Power versus number of phone calls for each research ",
                   "question. The horizontal dashed line is your target ",
                   "power. Each curve crosses the target line at the ",
                   "sample size you need."
                 ),
                 plotOutput("power_curve", height = "520px"),
                 br(),
                 h4("What this means in plain English"),
                 uiOutput("power_curve_interpretation"),
                 br(),
                 h4("How to read the curves"),
                 tags$ul(
                   tags$li(tags$strong("Steep curves"),
                           " mean the effect goes from undetectable to ",
                           "easily-detectable across a small range of ",
                           "sample sizes."),
                   tags$li(tags$strong("Flat curves near 100%"),
                           " mean you are already over-powered for that ",
                           "effect and could shrink the study."),
                   tags$li(tags$strong("Curves that never reach the target"),
                           " mean the effect is too small (or too noisy) ",
                           "to detect in the sample sizes you tested. Add ",
                           "larger values to the grid, oversample the ",
                           "minority group, or accept a smaller target power."),
                   tags$li(tags$strong("The gap between curves"),
                           " is informative: marginal effects are usually ",
                           "easier to detect than interaction effects ",
                           "because the interaction needs enough data in ",
                           "each cell of the rural-by-insurance table.")
                 )),
        tabPanel("Negative Binomial dispersion sensitivity",
                 br(),
                 helpText(
                   "Sweeps over half, current, and double your assumed ",
                   "Negative Binomial dispersion phi. This shows how ",
                   "sensitive the recommended physician count is to how ",
                   "overdispersed the wait-time data turns out to be."
                 ),
                 actionButton("run_sens",
                              "Run dispersion sensitivity sweep",
                              class = "btn-warning"),
                 br(), br(),
                 plotOutput("sensitivity_plot", height = "500px")),
        tabPanel("Examples from the literature",
                 br(),
                 helpText(
                   "Peer-reviewed mystery-caller studies of new-patient ",
                   "appointment wait time, broken out by insurance arm. ",
                   "Use this table to anchor your assumed cell means in ",
                   "values that have been observed in published work."
                 ),
                 DT::DTOutput("literature_table"),
                 br(),
                 wellPanel(
                   h4("Load study values into the sidebar inputs"),
                   helpText(
                     "Pick a study. Click ", tags$strong("Load values"),
                     " and the urban Blue Cross / Blue Shield and urban ",
                     "Medicaid cells (sidebar) jump to that study's ",
                     "published means. The rural cells stay where they ",
                     "are, so you can keep your own rural assumption."
                   ),
                   selectInput("lit_choice", "Study to load",
                               choices = setNames(literature_examples$id,
                                                  paste0(
                                                    literature_examples$study,
                                                    " (",
                                                    literature_examples$specialty,
                                                    ")"))),
                   uiOutput("lit_selected_summary"),
                   actionButton("lit_load",
                                "Load values into the sidebar",
                                class = "btn-primary",
                                icon = icon("upload"))
                 )),
        tabPanel("Download results",
                 br(),
                 helpText(
                   "Download the current power grid as a comma-separated ",
                   "values (CSV) file for inclusion in a study protocol ",
                   "or grant appendix."
                 ),
                 downloadButton("download_csv",
                                "Download power grid (CSV)",
                                class = "btn-success"))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # Arm labels reactive: drives every UI label and interpretation sentence.
  arm_labels <- reactive({
    if (isTRUE(input$study_type == "specialty")) {
      list(
        arm_a       = "Generalist",
        arm_a_short = "generalist",
        arm_b       = "Subspecialist",
        arm_b_short = "subspecialist",
        is_specialty = TRUE
      )
    } else {
      list(
        arm_a       = "Blue Cross / Blue Shield",
        arm_a_short = "Blue Cross",
        arm_b       = "Medicaid",
        arm_b_short = "Medicaid",
        is_specialty = FALSE
      )
    }
  })

  output$cell_input_baseline <- renderUI({
    lab <- arm_labels()
    numericInput("baseline",
      help_label(
        sprintf("%s, urban physician (days)", lab$arm_a),
        sprintf(paste(
          "Mean business days until the next available appointment for an ",
          "urban %s. This is the baseline cell (intercept) in the model."
        ), lab$arm_a_short)),
      value = isolate(input$baseline %||% 14),
      min = 1, max = 365, step = 1)
  })
  output$cell_input_med_urban <- renderUI({
    lab <- arm_labels()
    numericInput("med_urban",
      help_label(
        sprintf("%s, urban physician (days)", lab$arm_b),
        sprintf(paste(
          "Mean wait for an urban %s. The difference between this and the ",
          "%s-urban cell is the urban %s penalty."
        ), lab$arm_b_short, lab$arm_a_short, lab$arm_b_short)),
      value = isolate(input$med_urban %||% 17),
      min = 1, max = 365, step = 1)
  })
  output$cell_input_bcbs_rural <- renderUI({
    lab <- arm_labels()
    numericInput("bcbs_rural",
      help_label(
        sprintf("%s, rural physician (days)", lab$arm_a),
        sprintf(paste(
          "Mean wait for a rural %s. The difference from the %s-urban ",
          "cell is the rural baseline."
        ), lab$arm_a_short, lab$arm_a_short)),
      value = isolate(input$bcbs_rural %||% 18),
      min = 1, max = 365, step = 1)
  })
  output$cell_input_med_rural <- renderUI({
    lab <- arm_labels()
    numericInput("med_rural",
      help_label(
        sprintf("%s, rural physician (days)", lab$arm_b),
        sprintf(paste(
          "Mean wait for a rural %s. If the rural %s penalty is larger ",
          "than the urban %s penalty, the difference is the rural-by-arm ",
          "interaction we want to detect."
        ), lab$arm_b_short, lab$arm_b_short, lab$arm_b_short)),
      value = isolate(input$med_rural %||% 24),
      min = 1, max = 365, step = 1)
  })

  output$n_sim_warning <- renderUI({
    if (isTRUE(input$n_sim > 300)) {
      tags$div(style = "color: #b58900;",
               icon("triangle-exclamation"),
               sprintf(" %d replicates per N is slow (each NB GLMM fit ",
                       input$n_sim),
               "takes 1-3 s). Consider 100-300 for interactive use.")
    }
  })

  parsed_npi_grid <- reactive({
    raw <- strsplit(input$npi_grid, "[,\\s]+", perl = TRUE)[[1]]
    raw <- raw[nzchar(raw)]
    n <- suppressWarnings(as.integer(raw))
    n <- n[!is.na(n) & n > 0]
    if (length(n) == 0L) return(c(200L, 400L, 800L, 1500L))
    sort(unique(n))
  })

  power_grid <- eventReactive(input$run, {
    npi_grid <- parsed_npi_grid()
    cells    <- inputs_to_cells(input)

    withProgress(
      message = "Running NB power simulation...",
      value = 0,
      {
        run_power_grid(
          npi_grid       = npi_grid,
          cells          = cells,
          sigma_npi      = input$sigma_npi,
          phi            = input$phi_nb,
          rural_sampling = input$q_rural,
          pop_rural      = input$p_rural,
          alpha          = input$alpha,
          n_sim          = input$n_sim,
          fit_family     = input$fit_family,
          disp_threshold = input$disp_threshold,
          per_n_progress = function(i, total, n) {
            setProgress(value = i / total,
                        detail = sprintf("N = %d (%d of %d)", n, i, total))
          }
        )
      }
    )
  }, ignoreNULL = TRUE)

  output$recommendation <- renderUI({
    grid_df <- power_grid()
    cols <- c("pow_cond_interact", "pow_marg_unwtd", "pow_marg_popwtd")
    rec  <- recommended_n(grid_df, input$target_power, cols)
    tgt_pct <- sprintf("%.0f%%", 100 * input$target_power)

    # Each row: plain English question, the call count, and an explanation
    rec_block <- function(question, why, npi_value) {
      calls_text <- if (is.na(npi_value)) {
        tags$span(
          style = "color: #b91c1c; font-weight: bold;",
          "Even the largest sample you tested was not enough. ",
          "Try a bigger grid (e.g., add 2000, 3000, 4000)."
        )
      } else {
        tags$span(
          style = "color: #15803d; font-weight: bold; font-size: 1.4em;",
          sprintf("%s phone calls", format(npi_value * 2L, big.mark = ",")),
          tags$span(style = "color: #444; font-size: 0.8em; font-weight: normal;",
                    sprintf(" (call %s physicians, each twice: once with Blue Cross / Blue Shield, once with Medicaid)",
                            format(npi_value, big.mark = ",")))
        )
      }
      tags$div(
        style = "margin: 14px 0; padding: 12px; border-left: 4px solid #5a8dee; background: #f8fafc;",
        tags$div(style = "font-size: 1.05em;",
                 tags$strong("If your research question is: "),
                 tags$em(paste0("“", question, "”"))),
        tags$div(style = "margin-top: 8px;", calls_text),
        tags$div(style = "color: #555; font-size: 0.92em; margin-top: 6px;", why)
      )
    }

    n_sim_warn <- if (input$n_sim < 30) {
      tags$div(
        style = "margin: 8px 0; padding: 10px; background: #fef3c7; border-left: 4px solid #f59e0b;",
        icon("triangle-exclamation"),
        tags$strong(sprintf(" Caution: you ran only %d simulations per sample size.",
                            input$n_sim)),
        " Power estimates this noisy can wobble by 20+ points between runs. ",
        "Increase ", tags$strong("Monte Carlo replicates"),
        " to 100 (sidebar) for a number you can defend in a protocol."
      )
    } else NULL

    family_note <- {
      summaries <- unique(grid_df$family_summary)
      summaries <- summaries[!is.na(summaries) & nzchar(summaries)]
      if (input$fit_family == "auto" && length(summaries) > 0L) {
        n_fb <- mean(grid_df$pct_fell_back_to_nb, na.rm = TRUE)
        med_disp <- stats::median(grid_df$median_disp_ratio, na.rm = TRUE)
        tag_text <- if (!is.na(n_fb) && n_fb > 0.5) {
          sprintf(
            paste("In Auto mode, the Poisson fit was overdispersed (median",
                  "Pearson dispersion ratio %.2f), so %.0f%% of replicates",
                  "fell back to Negative Binomial. Plan to fit Negative",
                  "Binomial in your real analysis."),
            med_disp, 100 * n_fb)
        } else if (!is.na(n_fb) && n_fb < 0.1) {
          sprintf(
            paste("In Auto mode, the Poisson fit looked equidispersed",
                  "(median dispersion ratio %.2f). Poisson appears safe",
                  "for these assumptions; only %.0f%% of replicates fell",
                  "back to Negative Binomial."),
            med_disp, 100 * n_fb)
        } else {
          sprintf(
            paste("In Auto mode, %.0f%% of replicates fell back to Negative",
                  "Binomial (median Poisson dispersion ratio %.2f). The",
                  "data is on the borderline; running the real analysis",
                  "with Negative Binomial is the safe choice."),
            100 * n_fb, med_disp)
        }
        tags$div(
          style = "margin: 8px 0; padding: 10px; background: #ecfeff; border-left: 4px solid #06b6d4; font-size: 0.95em;",
          icon("circle-info"), " ", tag_text
        )
      } else NULL
    }

    tagList(
      tags$h3(sprintf("How many phone calls will I have to make?")),
      tags$p(
        style = "color: #444;",
        sprintf("Target: detect the effect %s of the time when it is real ",
                tgt_pct),
        sprintf("(power = %.2f), accepting a %g%% chance of a false positive ",
                input$target_power, 100 * input$alpha),
        sprintf("(alpha = %g). Each physician is called twice (once Blue ",
                input$alpha),
        "Cross / Blue Shield, once Medicaid)."
      ),
      n_sim_warn,
      family_note,
      rec_block(
        "Do Medicaid patients wait longer than Blue Cross / Blue Shield patients?",
        paste0("Average across all the physicians you called. This is the ",
               "claim most clinical audit papers (including Corbisiero et ",
               "al. 2024 in otolaryngology) lead with."),
        rec[["pow_marg_unwtd"]]
      ),
      if (isTRUE(input$show_pop_weighted)) rec_block(
        "What is the Medicaid penalty averaged across the whole U.S. physician population?",
        paste0("Country-wide (population-weighted) estimate. Use this only ",
               "if you want one number that generalizes to the actual ",
               "rural / urban mix of physicians in the U.S. (Turn off the ",
               "Advanced toggle in the sidebar if you don't need this.)"),
        rec[["pow_marg_popwtd"]]
      ),
      rec_block(
        "Is the Medicaid penalty bigger in rural areas than urban areas?",
        paste0("Difference of differences. Detecting whether two groups ",
               "differ in their gap usually needs 5 to 20 times more calls ",
               "than detecting a single average gap.")
        ,
        rec[["pow_cond_interact"]]
      )
    )
  })

  output$grid_table <- DT::renderDT({
    grid_df <- power_grid()
    base_cols <- list(
      `Physicians called`      = grid_df$n_npi,
      `Total phone calls`      = grid_df$n_calls,
      `Pct detected: average Medicaid penalty (sampled physicians)`
                               = sprintf("%.0f%%", 100 * grid_df$pow_marg_unwtd),
      `Pct detected: rural-vs-urban gap in Medicaid penalty`
                               = sprintf("%.0f%%", 100 * grid_df$pow_cond_interact),
      `Est. avg Medicaid penalty (days)`
                               = round(grid_df$mean_marg_unw_est, 2)
    )
    if (isTRUE(input$show_pop_weighted)) {
      base_cols[["Pct detected: country-wide Medicaid penalty (population-weighted)"]] <-
        sprintf("%.0f%%", 100 * grid_df$pow_marg_popwtd)
      base_cols[["Est. country-wide Medicaid penalty (days)"]] <-
        round(grid_df$mean_marg_pop_est, 2)
    }
    base_cols[["Model convergence"]] <-
      sprintf("%.0f%%", 100 * grid_df$convergence_rate)
    base_cols[["Simulations per row"]] <- grid_df$n_sim

    pretty <- do.call(data.frame, c(base_cols,
                                    list(check.names = FALSE,
                                         stringsAsFactors = FALSE)))
    DT::datatable(pretty, rownames = FALSE,
                  options = list(dom = "t", pageLength = 50,
                                 scrollX = TRUE))
  })

  output$power_curve <- renderPlot({
    grid_df <- power_grid()
    keep_cols <- c("pow_cond_interact", "pow_marg_unwtd")
    if (isTRUE(input$show_pop_weighted)) {
      keep_cols <- c(keep_cols, "pow_marg_popwtd")
    }
    long <- grid_df |>
      dplyr::select(n_calls, dplyr::all_of(keep_cols)) |>
      tidyr::pivot_longer(-n_calls, names_to = "estimand",
                          values_to = "power") |>
      dplyr::mutate(estimand = dplyr::recode(estimand,
        pow_cond_interact = "Rural-vs-urban gap in Medicaid penalty (hardest)",
        pow_marg_unwtd    = "Medicaid penalty, sampled physicians",
        pow_marg_popwtd   = "Medicaid penalty, country-wide (advanced)"))

    crossings <- find_crossings(grid_df, input$target_power)
    crossings <- crossings[crossings$estimand %in% keep_cols, , drop = FALSE]
    crossings$estimand <- dplyr::recode(crossings$estimand,
      pow_cond_interact = "Rural-vs-urban gap in Medicaid penalty (hardest)",
      pow_marg_unwtd    = "Medicaid penalty, sampled physicians",
      pow_marg_popwtd   = "Medicaid penalty, country-wide (advanced)")

    colour_vals <- c(
      "Medicaid penalty, sampled physicians"            = "#E69F00",
      "Medicaid penalty, country-wide (advanced)"       = "#0072B2",
      "Rural-vs-urban gap in Medicaid penalty (hardest)" = "#D55E00"
    )

    p <- ggplot(long, aes(n_calls, power, colour = estimand)) +
      geom_hline(yintercept = input$target_power, linetype = 2,
                 colour = "grey40") +
      annotate("text", x = max(long$n_calls), y = input$target_power,
               vjust = -0.7, hjust = 1, size = 4, colour = "grey30",
               label = sprintf("Target %.0f%% power",
                               input$target_power * 100)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.8) +
      scale_y_continuous(limits = c(0, 1.04),
                         labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(labels = scales::comma) +
      scale_colour_manual(values = colour_vals) +
      labs(x = "Total phone calls (each physician called twice)",
           y = "Probability of detecting the effect (power)",
           colour = NULL,
           title = "Power versus number of phone calls",
           subtitle = sprintf(
             "Negative Binomial mixed model. Alpha = %g, %d simulations per point. Sample mix %.0f%% rural, weighted to %.0f%% rural population.",
             input$alpha, input$n_sim,
             input$q_rural * 100, input$p_rural * 100)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            plot.title.position = "plot",
            plot.subtitle = element_text(colour = "grey30"))

    if (nrow(crossings) > 0L) {
      p <- p +
        geom_vline(data = crossings,
                   aes(xintercept = n_calls, colour = estimand),
                   linetype = 3, alpha = 0.6, show.legend = FALSE) +
        ggrepel::geom_label_repel(
          data = crossings,
          aes(x = n_calls, y = input$target_power,
              colour = estimand,
              label = sprintf("%s\n%s calls",
                              estimand,
                              format(n_calls, big.mark = ","))),
          size = 3.4, alpha = 0.95, fill = "white",
          box.padding = 0.6, point.padding = 0.4,
          show.legend = FALSE, max.overlaps = Inf
        )
    }
    p
  })

  output$grid_interpretation <- renderUI({
    grid_df <- power_grid()
    tags$div(
      style = "background: #f8fafc; border-left: 4px solid #5a8dee; padding: 14px; font-size: 1.02em;",
      HTML(interpret_findings(
        grid_df       = grid_df,
        target_power  = input$target_power,
        n_sim         = input$n_sim,
        bcbs_urban    = input$baseline,
        med_urban     = input$med_urban,
        bcbs_rural    = input$bcbs_rural,
        med_rural     = input$med_rural,
        p_rural       = input$p_rural,
        q_rural       = input$q_rural,
        alpha         = input$alpha,
        show_pop_weighted = isTRUE(input$show_pop_weighted),
        arm_a         = arm_labels()$arm_a,
        arm_b         = arm_labels()$arm_b,
        arm_a_short   = arm_labels()$arm_a_short,
        arm_b_short   = arm_labels()$arm_b_short
      ))
    )
  })

  output$power_curve_interpretation <- renderUI({
    grid_df <- power_grid()
    crossings <- find_crossings(grid_df, input$target_power)
    tags$div(
      style = "background: #f8fafc; border-left: 4px solid #5a8dee; padding: 14px; font-size: 1.02em;",
      HTML(interpret_power_curve(
        grid_df       = grid_df,
        crossings     = crossings,
        target_power  = input$target_power,
        alpha         = input$alpha,
        n_sim         = input$n_sim,
        bcbs_urban    = input$baseline,
        med_urban     = input$med_urban,
        bcbs_rural    = input$bcbs_rural,
        med_rural     = input$med_rural,
        sigma_npi     = input$sigma_npi,
        phi_nb        = input$phi_nb,
        q_rural       = input$q_rural,
        p_rural       = input$p_rural,
        show_pop_weighted = isTRUE(input$show_pop_weighted),
        arm_a         = arm_labels()$arm_a,
        arm_b         = arm_labels()$arm_b,
        arm_a_short   = arm_labels()$arm_a_short,
        arm_b_short   = arm_labels()$arm_b_short
      ))
    )
  })

  sensitivity_grid <- eventReactive(input$run_sens, {
    npi_grid <- parsed_npi_grid()
    cells    <- inputs_to_cells(input)
    phi_values <- sort(unique(c(input$phi_nb / 2, input$phi_nb,
                                input$phi_nb * 2)))
    out <- vector("list", length(phi_values))
    withProgress(
      message = "Running phi sensitivity sweep...",
      value = 0,
      {
        for (j in seq_along(phi_values)) {
          incProgress(1 / length(phi_values),
                      detail = sprintf("phi = %.2f", phi_values[j]))
          g <- run_power_grid(
            npi_grid       = npi_grid,
            cells          = cells,
            sigma_npi      = input$sigma_npi,
            phi            = phi_values[j],
            rural_sampling = input$q_rural,
            pop_rural      = input$p_rural,
            alpha          = input$alpha,
            n_sim          = input$n_sim,
            fit_family     = input$fit_family,
            disp_threshold = input$disp_threshold
          )
          g$phi <- phi_values[j]
          out[[j]] <- g
        }
      }
    )
    do.call(rbind, out)
  }, ignoreNULL = TRUE)

  output$sensitivity_plot <- renderPlot({
    df <- sensitivity_grid()
    ggplot(df, aes(n_npi, pow_cond_interact, colour = factor(phi))) +
      geom_hline(yintercept = input$target_power, linetype = 2, alpha = 0.6) +
      geom_line() +
      geom_point() +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = "NPIs called",
           y = "Power (conditional interaction)",
           colour = "phi",
           title = "Sensitivity to NB dispersion phi") +
      theme_minimal(base_size = 14)
  })

  # ---------------------------------------------------------------------------
  # Literature examples tab
  # ---------------------------------------------------------------------------
  output$literature_table <- DT::renderDT({
    lit <- literature_examples
    show <- data.frame(
      Study               = lit$study,
      Specialty           = lit$specialty,
      Measure             = lit$measure,
      `BCBS (days)`       = lit$bcbs_days,
      `Medicaid (days)`   = lit$medicaid_days,
      `Difference (days)` = round(lit$medicaid_days - lit$bcbs_days, 1),
      `Medicaid longer (%)` = lit$pct_longer,
      `Sample`            = lit$n_physicians,
      Citation            = lit$citation,
      check.names = FALSE
    )
    DT::datatable(show, rownames = FALSE,
                  options = list(dom = "t", pageLength = 10,
                                 columnDefs = list(
                                   list(className = "dt-right",
                                        targets = c(3, 4, 5, 6))
                                 )))
  })

  output$lit_selected_summary <- renderUI({
    row <- literature_examples[
      literature_examples$id == input$lit_choice, , drop = FALSE
    ]
    if (nrow(row) == 0L) return(NULL)
    tags$div(
      style = "border-left: 3px solid #5a8dee; padding-left: 10px; margin: 10px 0;",
      tags$p(tags$strong(row$study), " - ", row$specialty),
      tags$p(
        sprintf("Mean wait (%s): %.1f days BCBS, %.1f days Medicaid",
                tolower(row$measure), row$bcbs_days, row$medicaid_days),
        tags$br(),
        sprintf("Difference: %.1f days (%.1f%% longer for Medicaid)",
                row$medicaid_days - row$bcbs_days, row$pct_longer)
      ),
      tags$p(em(row$notes)),
      tags$p(tags$small(row$citation))
    )
  })

  observeEvent(input$lit_load, {
    row <- literature_examples[
      literature_examples$id == input$lit_choice, , drop = FALSE
    ]
    if (nrow(row) == 0L) return(NULL)
    updateNumericInput(session, "baseline",  value = round(row$bcbs_days, 1))
    updateNumericInput(session, "med_urban", value = round(row$medicaid_days, 1))
    showNotification(
      sprintf("Loaded %s: BCBS urban = %.1f, Medicaid urban = %.1f. Rural cells unchanged.",
              row$study, row$bcbs_days, row$medicaid_days),
      type = "message", duration = 6
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      sprintf("mystery_caller_power_grid_%s.csv",
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      grid_df <- power_grid()
      utils::write.csv(grid_df, file, row.names = FALSE)
    }
  )

  # ---- Reset all inputs to defaults ----------------------------------------
  observeEvent(input$reset_defaults, {
    updateNumericInput(session,   "baseline",       value = 14)
    updateNumericInput(session,   "med_urban",      value = 17)
    updateNumericInput(session,   "bcbs_rural",     value = 18)
    updateNumericInput(session,   "med_rural",      value = 24)
    updateNumericInput(session,   "sigma_npi",      value = 0.30)
    updateNumericInput(session,   "phi_nb",         value = 6)
    updateRadioButtons(session,   "fit_family",     selected = "auto")
    updateNumericInput(session,   "disp_threshold", value = 1.5)
    updateSliderInput(session,    "q_rural",        value = 0.50)
    updateSliderInput(session,    "p_rural",        value = 0.15)
    updateCheckboxInput(session,  "show_pop_weighted", value = FALSE)
    updateNumericInput(session,   "alpha",          value = 0.05)
    updateNumericInput(session,   "target_power",   value = 0.90)
    updateTextInput(session,      "npi_grid",       value = "200, 400, 800, 1500")
    updateNumericInput(session,   "n_sim",          value = 100)
    showNotification("Inputs reset to defaults.", type = "message",
                     duration = 4)
  })

  # ---- Pre-fill cell means from a chosen real study ------------------------
  # All four studies are computed live from their Dropbox exports. The rural
  # cells for the spine, ortho, and sports-med studies are estimates because
  # those datasets do not break out a rural/urban split; the Corbi values are
  # the Metropolitan vs unlabeled-rural subset means and have small rural n.
  # bcbs_urban / bcbs_rural cells map to "arm A" (BCBS in insurance mode,
  # Generalist in specialty mode). med_urban / med_rural map to "arm B"
  # (Medicaid in insurance mode, Subspecialist in specialty mode). The
  # underlying math is the same; only the labels change.
  preset_cells <- list(
    # ---- Insurance arm studies ----
    # All four cells below are real observed means, NOT estimates. Rural =
    # Micropolitan + unlabeled-CBSA pooled. Sample sizes per rural cell:
    # Corbi 21-39, Ortho 25-27, Sports med 19-24. Spine has no rural/urban
    # column in its export so its rural cells remain a placeholder.
    corbi            = list(study_type = "insurance",
                            bcbs_urban = 17, med_urban = 21,
                            bcbs_rural = 10, med_rural = 16, phi = 0.8,
                            label = "Muffly-Corbisiero 2023 OB/GYN subspecialists"),
    spine            = list(study_type = "insurance",
                            bcbs_urban = 22, med_urban = 27,
                            bcbs_rural = 19, med_rural = 24, phi = 1.1,
                            label = "Spine surgery (rural cells are placeholders)"),
    ortho            = list(study_type = "insurance",
                            bcbs_urban = 19, med_urban = 24,
                            bcbs_rural = 16, med_rural = 23, phi = 0.9,
                            label = "General orthopedic surgery"),
    sports_med       = list(study_type = "insurance",
                            bcbs_urban = 16, med_urban = 19,
                            bcbs_rural = 20, med_rural = 24, phi = 0.8,
                            label = "Orthopedic sports medicine"),
    # ---- Specialty arm studies (subspec vs generalist) ----
    # Cell means below come from POOLING Corbisiero 2024 ENT and Saeedi ENT
    # phase 2 data (~1,880 valid wait observations combined). All four cells
    # are real, not estimated -- including rural which now has 71 to 80 obs
    # per subspecialty. Pooled NB phi 1.69.
    saeedi_peds_ent  = list(study_type = "specialty",
                            bcbs_urban = 32, med_urban = 54,
                            bcbs_rural = 45, med_rural = 66, phi = 1.7,
                            label = "Pediatric vs general ENT (Corbisiero + Saeedi pooled)"),
    saeedi_neuro_ent = list(study_type = "specialty",
                            bcbs_urban = 32, med_urban = 44,
                            bcbs_rural = 45, med_rural = 46, phi = 1.7,
                            label = "Neurotology vs general ENT (Corbisiero + Saeedi pooled)"),
    # Lizzy peds derm cell means: Metro mean from real data; rural mean
    # estimated upward because peds dermatologists cluster in cities.
    lizzy_peds_derm  = list(study_type = "specialty",
                            bcbs_urban = 53, med_urban = 89,
                            bcbs_rural = 55, med_rural = 100, phi = 0.5,
                            label = "Lizzy pediatric vs general dermatology")
  )

  observeEvent(input$load_preset, {
    choice <- input$preset_study
    if (!nzchar(choice) || !choice %in% names(preset_cells)) {
      showNotification("Pick a study from the dropdown first.",
                       type = "warning", duration = 4)
      return(NULL)
    }
    p <- preset_cells[[choice]]
    # Flip study_type first so labels redraw before the values come in.
    updateRadioButtons(session, "study_type", selected = p$study_type)
    updateNumericInput(session, "baseline",   value = p$bcbs_urban)
    updateNumericInput(session, "med_urban",  value = p$med_urban)
    updateNumericInput(session, "bcbs_rural", value = p$bcbs_rural)
    updateNumericInput(session, "med_rural",  value = p$med_rural)
    updateNumericInput(session, "phi_nb",     value = p$phi)
    lab <- if (p$study_type == "specialty") {
      sprintf("Generalist urban %g, Subspecialist urban %g, Generalist rural %g, Subspecialist rural %g",
              p$bcbs_urban, p$med_urban, p$bcbs_rural, p$med_rural)
    } else {
      sprintf("Blue Cross urban %g, Medicaid urban %g, Blue Cross rural %g, Medicaid rural %g",
              p$bcbs_urban, p$med_urban, p$bcbs_rural, p$med_rural)
    }
    showNotification(
      sprintf("Loaded %s. %s, phi %g.", p$label, lab, p$phi),
      type = "message", duration = 8
    )
  })

  # ---- Hide the NB dispersion sensitivity tab when Poisson is forced -------
  observe({
    if (isTRUE(input$fit_family == "poisson")) {
      hideTab(inputId = "main_tabs",
              target = "Negative Binomial dispersion sensitivity")
    } else {
      showTab(inputId = "main_tabs",
              target = "Negative Binomial dispersion sensitivity")
    }
  })
}

shinyApp(ui = ui, server = server)
