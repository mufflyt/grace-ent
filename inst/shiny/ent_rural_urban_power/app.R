# =============================================================================
# Grace's ENT Rural-vs-Urban Power Calculator
# =============================================================================
# Design: each physician is called once, no insurance arm. The primary
# contrast is rural physician vs urban physician (between-physician). Two
# outcomes are powered together:
#   1. Whether the office offered any appointment at all (logistic GLM)
#   2. Business days until appointment, given one was offered (NB GLM)
#
# Backend in R/mystery_caller_power_core.R, prefix mc_ru_*.
# Launch:
#   shiny::runApp("inst/shiny/ent_rural_urban_power")
# =============================================================================

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
  # jsonlite is NOT attached because jsonlite::validate masks shiny::validate.
  # All jsonlite calls below use the namespace prefix.
})

source(here::here("R", "mystery_caller_power_core.R"))

# Health check: confirm glmmTMB is functional before letting users run a
# simulation. Returns a list(ok = logical, msg = character).
mc_health_check <- function() {
  ok <- requireNamespace("glmmTMB", quietly = TRUE) &&
        requireNamespace("TMB", quietly = TRUE)
  if (!ok) {
    return(list(ok = FALSE,
                msg = "glmmTMB or TMB is not installed."))
  }
  res <- tryCatch({
    suppressWarnings({
      n <- 40; r <- rbinom(n, 1, 0.5)
      y <- rnbinom(n, mu = ifelse(r == 1, 12, 8), size = 1.5)
      glmmTMB::glmmTMB(y ~ r, family = glmmTMB::nbinom2)
    })
  }, error = function(e) e)
  if (inherits(res, "error")) {
    list(ok = FALSE, msg = paste("glmmTMB fit failed:", conditionMessage(res)))
  } else {
    list(ok = TRUE,
         msg = sprintf("glmmTMB %s + TMB %s loaded; test fit converged.",
                       utils::packageVersion("glmmTMB"),
                       utils::packageVersion("TMB")))
  }
}

# Reusable help-label helper
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

# Pooled ENT defaults from Corbisiero 2024 + Saeedi ENT real data.
# Offer rate = fraction of paired calls that ended with a valid wait date.
# Pooled overall: 1,393 / ~1,880 = 0.74. By Metro vs Rural, the pooled
# offer rates are similar so we set urban = 0.78, rural = 0.65 as a
# reasonable hypothesized gap (offer-rate disparity is the rural penalty
# that mystery-caller studies are usually designed to detect).
DEFAULTS <- list(
  # Urban from pooled real General Otolaryngology data (32 d). Rural set
  # to give an 8-day conservative gap (40 d) instead of the +13 d the
  # pooled data actually shows. This makes the default a smaller, harder
  # to detect effect so the protocol target is conservative; bump rural
  # higher in the sidebar if you want to power for the larger effect.
  wait_urban   = 32,
  wait_rural   = 40,
  phi          = 1.7,
  rural_frac   = 0.50,
  alpha        = 0.05,
  target_power = 0.90,
  npi_grid     = "200, 400, 600, 800, 1200, 1600",
  n_sim        = 50
)

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  titlePanel("ENT Rural-vs-Urban Power Calculator (Grace's study)"),
  helpText(
    "Plan how many ENT physicians you need to call in a single-call ",
    "rural-vs-urban mystery-caller study. Each physician is called once ",
    "with commercial (Blue Cross / Blue Shield) insurance after Phase 1 ",
    "phone-number validation, so every contacted office is assumed to ",
    "offer an appointment. The only outcome modeled here is business ",
    "days until that appointment. Defaults come from pooled real data ",
    "(Corbisiero 2024 ENT + Saeedi ENT, 273 rural observations, negative binomial dispersion phi = 1.7)."
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      h4("Hypothesized rural-vs-urban effects"),
      helpText(em("Enter the mean appointment wait you expect at an urban ENT physician and at a rural ENT physician. Defaults are pooled estimates from prior US ENT mystery-caller studies (Corbisiero 2024 + Saeedi); change them if you are targeting a specific subspecialty or have stronger prior estimates from pilot data.")),

      numericInput("wait_urban",
        help_label("Mean wait at an urban ENT (days)",
          paste("Mean business days until next available appointment for ",
                "the typical urban ENT physician in your sample frame. ",
                "Pooled real ENT data: 32 days for General Otolaryngology, ",
                "54 days for Pediatric ENT.")),
        value = DEFAULTS$wait_urban, min = 1, max = 365, step = 1),

      numericInput("wait_rural",
        help_label("Mean wait at a rural ENT (days)",
          paste("Mean wait for a rural ENT. Pooled real data: ",
                "45 days General, 66 days Pediatric ENT. Rural wait is ",
                "typically 12-13 days longer than urban in ENT.")),
        value = DEFAULTS$wait_rural, min = 1, max = 365, step = 1),

      numericInput("phi",
        help_label("Negative Binomial dispersion (phi)",
          paste("Controls how heavy-tailed the wait-day distribution is. ",
                "Var = mu + mu^2/phi. Pooled ENT real data: phi ~ 1.7. ",
                "Smaller phi means more overdispersion.")),
        value = DEFAULTS$phi, min = 0.3, max = 50, step = 0.1),

      tags$hr(),
      h4("Sampling design"),
      sliderInput("rural_frac",
        help_label("Fraction of called physicians that are rural",
          paste("With a 50/50 stratified design you maximize power for ",
                "rural-vs-urban contrasts. If you instead sample ",
                "proportionally to the actual ENT physician population ",
                "(~15% rural), shift this slider to 0.15 but expect to ",
                "need a larger total sample.")),
        min = 0.10, max = 0.90, value = DEFAULTS$rural_frac, step = 0.05),

      tags$hr(),
      h4("Analysis model details"),
      helpText(em("These control which covariates the simulator bakes into the fitted model. Defaults match the manuscript's planned adjusted analysis.")),
      checkboxInput(
        "use_adjusted_model",
        label = help_label(
          "Use the full adjusted analysis model in the simulation",
          paste(
            "When on, every replicate fits the same Negative Binomial mixed model that will appear in your manuscript: rural + subspecialty + a configurable number of adjustment covariates + (1|state) random intercept. Each fit is slower than the simple rural-only model, so the simulation takes longer but the power number is anchored in the analysis Grace will actually run. When off, the simulator fits rural-only and we add a +20% safety margin to approximate state clustering."
          )),
        value = TRUE),
      conditionalPanel(
        condition = "input.use_adjusted_model == true",
        sliderInput(
          "state_icc",
          help_label(
            "State-level intraclass correlation (ICC)",
            paste(
              "Fraction of total wait-time variance attributable to ",
              "between-state differences after rural and subspecialty are ",
              "accounted for. Empirical estimates fit to prior mystery-",
              "caller datasets: Corbisiero 2024 ENT = 0.076, Saeedi ENT ",
              "phase 2 = 0.083, Corbi OB/GYN = 0.122, General Ortho = ",
              "0.131, Sports Med = 0.163, Spine = 0.185. For an ENT ",
              "study the empirically anchored default is 0.08."
            )),
          min = 0.00, max = 0.30, value = 0.08, step = 0.01),
        numericInput(
          "n_subspecs",
          help_label(
            "Number of subspecialty levels",
            "Subspecialty is fit as a categorical fixed effect with this many levels. Grace's protocol calls for 7 (General, Pediatric, Rhinology, Neurotology, Laryngology, Facial Plastics, Head & Neck)."),
          value = 7, min = 2, max = 10, step = 1),
        numericInput(
          "n_extra_covars",
          help_label(
            "Number of additional adjustment covariates",
            "Gender, degree, group size, fellowship status, board certification, years in practice, day of week, central appointment number, transfers, locations, hold time, academic affiliation. Each adjustment covariate slightly reduces residual variance and may reduce required N. Set to the number of covariates you plan to include in the manuscript's adjusted model."),
          value = 6, min = 0, max = 12, step = 1)
      ),

      tags$hr(),
      h4("Statistical decision rule"),
      numericInput("alpha",
        help_label("Significance level (alpha)",
          "Probability of falsely declaring an effect. Standard is 0.05."),
        value = DEFAULTS$alpha, min = 0.001, max = 0.20, step = 0.005),
      numericInput("target_power",
        help_label("Target statistical power",
          paste("Probability of detecting the effect when it is real. ",
                "0.80 is the regulatory floor; 0.90 is publication grade.")),
        value = DEFAULTS$target_power, min = 0.50, max = 0.99, step = 0.01),

      tags$hr(),
      h4("Simulation settings"),
      textInput("npi_grid",
        help_label("Total physicians to evaluate (comma-separated)",
          paste("Each value is one full Monte Carlo run. Pick a range ",
                "around what you can afford to call. Defaults span ",
                "200 to 1,200 physicians total.")),
        value = DEFAULTS$npi_grid),
      numericInput("n_sim",
        help_label("Monte Carlo replicates per grid point",
          paste("More replicates means tighter power estimates but longer ",
                "runtime. 30 = quick preview, 100 = defensible, 300+ = ",
                "publication grade.")),
        value = DEFAULTS$n_sim, min = 10, max = 500, step = 10),
      numericInput("seed",
        help_label("Random seed for reproducibility",
          paste("Fixed seed used at the start of every simulation run so ",
                "the power grid, Type I check, and minimum-detectable-",
                "effect search are exactly reproducible. Change only if ",
                "you want a fresh Monte Carlo draw. The scenario JSON ",
                "save / load preserves the seed alongside the other inputs.")),
        value = 1978L, min = 1, max = .Machine$integer.max, step = 1L),

      tags$hr(),
      actionButton("run", "Run simulation",
                   class = "btn-primary", icon = icon("play")),
      tags$span(style = "margin-left: 8px;",
        actionButton("reset_defaults", "Reset to ENT defaults",
                     class = "btn-outline-secondary",
                     icon = icon("rotate-left"))
      ),

      tags$hr(),
      h5("Save / load scenario"),
      helpText("Snapshot every sidebar input to a small JSON file so you can come back to this exact scenario later (or share it with your statistician)."),
      downloadButton("save_scenario", "Save current scenario (.json)",
                     class = "btn-outline-secondary", icon = icon("download")),
      tags$br(), tags$br(),
      fileInput("load_scenario", "Load saved scenario",
                accept = c(".json"), buttonLabel = "Browse...")
    ),

    mainPanel(
      width = 8,

      uiOutput("health_banner"),

      wellPanel(
        style = "background-color: #ecfeff; border-left: 4px solid #06b6d4;",
        tags$strong("Reality check: pooled ENT rural-vs-urban wait times."),
        tags$br(),
        "Pooled from Corbisiero 2024 ENT + Saeedi ENT phase 2 (273 rural ",
        "observations across 7 subspecialties).",
        tags$ul(
          tags$li(tags$strong("General Otolaryngology: 31.8 d urban (n=243) vs 45.3 d rural (n=80)"),
                  " - rural penalty +13.4 d."),
          tags$li(tags$strong("Pediatric Otolaryngology: 54.1 d urban (n=180) vs 66.4 d rural (n=71)"),
                  " - rural penalty +12.3 d."),
          tags$li("Neurotology: 43.6 vs 45.6 (+2.0 d). Head & Neck: 27.8 vs 34.5 (+6.8 d). Facial Plastic: 26.9 vs 31.4 (+4.5 d). Rhinology: 23.7 vs 25.0 (+1.3 d)."),
          tags$li(tags$strong("Pooled NB phi = 1.69, variance / mean = 24.7."),
                  " Wait time is severely overdispersed: NB is the right model.")
        ),
        tags$em("Default cell means in the sidebar are set to the General Otolaryngology pooled values. Adjust if your study is targeting a different ENT subspecialty (e.g., Pediatric ENT 54 d urban / 66 d rural)."),
        tags$br(), tags$br(),
        tags$strong("Phase 1 phone validation in this study:"),
        " phone numbers are confirmed and the Phase 2 script uses commercial (Blue Cross / Blue Shield) insurance. Under those conditions, every contacted ENT office is assumed to offer a new-patient appointment, so the only outcome powered here is the wait-time gap."
      ),

      tabsetPanel(
        id = "main_tabs",
        tabPanel("How many physicians do I need to call?",
                 br(),
                 uiOutput("recommendation"),
                 br(),
                 h4("Detail: power at each sample size"),
                 helpText(
                   "Each row is one Monte Carlo grid point. ",
                   tags$strong("Pow: appointment offered"),
                   " is the share of simulated studies that detected a ",
                   "rural-vs-urban difference in the probability of being ",
                   "offered an appointment. ",
                   tags$strong("Pow: wait time"),
                   " is the share that detected a difference in ",
                   "business days until that appointment."
                 ),
                 DT::DTOutput("grid_table")),

        tabPanel("Power curves",
                 br(),
                 helpText("The horizontal dashed line is your target power."),
                 plotOutput("power_curve", height = "520px"),
                 br(),
                 h4("Interpretation"),
                 uiOutput("curve_interpretation")),

        tabPanel("Sentences you can write",
                 br(),
                 helpText("Which sentences in a final paper are supported by the simulation. Green = the study is powered for that statement. Red = it is not."),
                 uiOutput("claims_panel")),

        tabPanel("Fixed-N planning (one-click)",
                 br(),
                 helpText(
                   "You already know how many calls you can afford. ",
                   "Click the button below and the app runs five analyses ",
                   "in sequence at the fixed N you specify: (1) power at ",
                   "your assumed effect, (2) expected confidence-interval ",
                   "half-width on the response scale, (3) minimum ",
                   "detectable effect via binary search, (4) non-response ",
                   "buffer (power at 80, 85, 90% retention), and (5) ",
                   "allocation sensitivity (rural fraction sweep). Takes ",
                   "5 to 10 minutes total."
                 ),
                 fluidRow(
                   column(5,
                     numericInput("fixed_n",
                       help_label("Total calls available",
                         "Fixed budget. Defaults to 800 (Grace's plan)."),
                       value = 800, min = 50, max = 10000, step = 50)
                   ),
                   column(5,
                     numericInput("fixed_n_sim",
                       help_label("Monte Carlo replicates per sub-analysis",
                         "30 is a quick pass; 50+ is defensible."),
                       value = 30, min = 10, max = 200, step = 10)
                   )
                 ),
                 actionButton("run_fixed_n",
                              "Run full fixed-N analysis",
                              class = "btn-primary",
                              icon = icon("bolt")),
                 br(), br(),
                 uiOutput("fixed_n_summary")),

        tabPanel("Methods text for your manuscript",
                 br(),
                 helpText("Copy-paste a publication-ready Methods paragraph generated from the current simulation. The text re-renders any time you change inputs and re-run."),
                 uiOutput("methods_text"),
                 br(),
                 downloadButton("download_methods", "Download Methods text (.txt)",
                                class = "btn-outline-success",
                                icon = icon("file-lines"))),

        tabPanel("Effect-size sensitivity",
                 br(),
                 helpText(
                   "Re-runs the simulation across a range of assumed rural wait values at a fixed total physician count, holding all other inputs constant. Tells you how power degrades if your hypothesized effect turns out to be smaller than expected. Useful for the protocol's robustness section."
                 ),
                 fluidRow(
                   column(5,
                     numericInput("sens_n_total",
                                  help_label(
                                    "Fixed total physicians for the sweep",
                                    "The single sample size at which to evaluate power across a range of rural wait days. Set to whatever your planning target is (usually the bare-minimum N at the target power)."),
                                  value = 600, min = 50, max = 5000, step = 50)
                   ),
                   column(5,
                     numericInput("sens_n_sim",
                                  help_label(
                                    "Monte Carlo replicates per point",
                                    "Replicates per assumed-rural-wait value. 50 is a quick read, 100+ is defensible."),
                                  value = 50, min = 10, max = 500, step = 10)
                   )
                 ),
                 actionButton("run_sens",
                              "Run effect-size sensitivity sweep",
                              class = "btn-warning", icon = icon("chart-line")),
                 br(), br(),
                 plotOutput("sens_plot", height = "440px"),
                 uiOutput("sens_text")),

        tabPanel("Download results",
                 br(),
                 helpText("Download the current power grid as a CSV for protocol or grant appendix."),
                 downloadButton("download_csv",
                                "Download power grid (CSV)",
                                class = "btn-success"))
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  parsed_grid <- reactive({
    raw <- strsplit(input$npi_grid %||% "", "[,\\s]+", perl = TRUE)[[1]]
    raw <- raw[nzchar(raw)]
    n <- suppressWarnings(as.integer(raw))
    n <- n[!is.na(n) & n >= 50]
    if (length(n) == 0L) return(c(200L, 400L, 800L))
    sort(unique(n))
  })

  power_grid <- eventReactive(input$run, {
    grid <- parsed_grid()
    shiny::validate(
      shiny::need(input$wait_urban > 0 && input$wait_rural > 0,
                  "Wait times must be positive."),
      shiny::need(length(grid) >= 1,
                  "Provide at least one physician count >= 50.")
    )
    use_adj <- isTRUE(input$use_adjusted_model)
    # Reproducibility: same inputs + same seed -> identical grid
    set.seed(isolate(as.integer(input$seed %||% 1978L)))
    n_steps <- length(grid)
    msg <- if (use_adj)
      "Running adjusted-model simulation (slower; full GLMM per replicate)..."
    else
      "Running rural-only simulation..."
    withProgress(message = msg, value = 0, {
      rows <- lapply(seq_along(grid), function(i) {
        N <- grid[i]
        incProgress(1 / n_steps,
                    detail = sprintf("N = %d physicians (%d of %d)",
                                     N, i, n_steps))
        if (use_adj) {
          out <- mc_ru_run_power_adjusted(
            n_total        = N,
            rural_frac     = input$rural_frac,
            wait_urban     = input$wait_urban,
            wait_rural     = input$wait_rural,
            phi            = input$phi,
            state_icc      = input$state_icc,
            n_states       = 51,
            n_subspecs     = input$n_subspecs,
            n_extra_covars = input$n_extra_covars,
            alpha          = input$alpha,
            n_sim          = input$n_sim
          )
          # Align column names with the wait-only output so downstream
          # tabs (table, plot, methods text) don't need to branch.
          out$mean_marg_effect <- NA_real_
          out
        } else {
          mc_ru_run_power_wait_only(
            n_total    = N,
            rural_frac = input$rural_frac,
            wait_urban = input$wait_urban,
            wait_rural = input$wait_rural,
            phi        = input$phi,
            alpha      = input$alpha,
            n_sim      = input$n_sim
          )
        }
      })
      do.call(rbind.data.frame, c(rows, list(make.row.names = FALSE)))
    })
  }, ignoreNULL = TRUE)

  # ---- Recommended N panel -------------------------------------------------
  output$recommendation <- renderUI({
    grid <- power_grid()
    tgt <- input$target_power

    first_hit <- function(col) {
      ok <- which(grid[[col]] >= tgt)
      if (length(ok) == 0L) NA_integer_ else grid$n_total[ok[1]]
    }
    n_wait <- first_hit("pow_wait")

    days_diff       <- input$wait_rural - input$wait_urban
    pct_longer_wait <- 100 * (input$wait_rural / input$wait_urban - 1)

    n_sim_warn <- if (input$n_sim < 30) {
      tags$div(
        style = "margin: 8px 0; padding: 10px; background: #fef3c7; border-left: 4px solid #f59e0b;",
        icon("triangle-exclamation"),
        tags$strong(sprintf(" Caution: you ran only %d simulations.",
                            input$n_sim)),
        " Power estimates this noisy can wobble 20+ points between runs. ",
        "Increase ", tags$strong("Monte Carlo replicates"),
        " to at least 100 for a defensible number."
      )
    } else NULL

    # If every model fit failed (e.g., glmmTMB not installed), call it out
    # loudly instead of letting "Not reached" misdirect the user.
    fit_failed <- all(is.na(grid$pow_wait)) || all(grid$convergence_rate < 0.5)
    fit_warn <- if (fit_failed) {
      tags$div(
        style = "margin: 8px 0; padding: 10px; background: #fee2e2; border-left: 4px solid #dc2626;",
        icon("circle-xmark"),
        tags$strong(" Model fits failed for every replicate."),
        tags$br(),
        "This usually means a required package is missing or broken in the R environment. Check that ",
        tags$code("glmmTMB"), " and ", tags$code("TMB"),
        " are installed and load without error. From the project root in a fresh R session:",
        tags$pre("install.packages(c('TMB','glmmTMB'), type='binary')"),
        "Then restart the Shiny app."
      )
    } else NULL

    wait_effect_zero <- abs(days_diff) < 0.5
    zero_warn <- if (wait_effect_zero) {
      tags$div(
        style = "margin: 8px 0; padding: 10px; background: #fee2e2; border-left: 4px solid #dc2626;",
        icon("circle-exclamation"),
        tags$strong(" Heads up: the assumed effect is essentially zero."),
        tags$br(),
        sprintf("Urban and rural waits are both %.1f days, so there is no effect to detect. ",
                input$wait_urban),
        "Open the sidebar and move the rural slider away from the urban value to a realistic gap (the pooled ENT data shows +13 days for General ENT, +12 days for Pediatric ENT)."
      )
    } else NULL

    safety_margin <- 0.20   # +20% to absorb state-level clustering
    n_wait_safe <- if (is.na(n_wait)) NA_integer_ else
                   ceiling(n_wait * (1 + safety_margin))

    n_text <- if (wait_effect_zero) {
      tags$span(style = "color: #b91c1c; font-weight: bold;",
                "The hypothesized effect is essentially zero. ",
                "Set a non-trivial gap between rural and urban wait times in the sidebar.")
    } else if (is.na(n_wait)) {
      tags$span(style = "color: #b91c1c; font-weight: bold;",
                "Not reached in this grid. Add larger physician counts (e.g., 1500, 2000) and re-run.")
    } else {
      tagList(
        tags$div(
          tags$span(style = "color: #444; font-size: 0.9em;",
                    "Bare minimum (no state clustering): "),
          tags$span(
            style = "color: #15803d; font-weight: bold; font-size: 1.2em;",
            sprintf("%s physicians", format(n_wait, big.mark = ",")),
            tags$span(style = "color: #444; font-size: 0.8em; font-weight: normal;",
                      sprintf(" (about %d rural + %d urban)",
                              round(n_wait * input$rural_frac),
                              n_wait - round(n_wait * input$rural_frac))))
        ),
        tags$div(
          style = "margin-top: 6px;",
          tags$span(style = "color: #444; font-size: 0.9em;",
                    sprintf("With +%.0f%% safety margin for state-level clustering (recommended for protocol): ",
                            100 * safety_margin)),
          tags$span(
            style = "color: #15803d; font-weight: bold; font-size: 1.4em;",
            sprintf("%s physicians", format(n_wait_safe, big.mark = ",")),
            tags$span(style = "color: #444; font-size: 0.8em; font-weight: normal;",
                      sprintf(" (about %d rural + %d urban)",
                              round(n_wait_safe * input$rural_frac),
                              n_wait_safe - round(n_wait_safe * input$rural_frac))))
        )
      )
    }

    tagList(
      tags$h3("How many ENT physicians will Grace need to call?"),
      tags$p(
        style = "color: #444;",
        sprintf(
          "Assumed effect: rural ENT waits %.0f days longer than urban (%.1f%% longer at NB phi = %g).",
          days_diff, pct_longer_wait, input$phi
        ),
        sprintf(" Target: %g%% power at alpha = %g.",
                100 * tgt, input$alpha),
        tags$br(),
        tags$em("Phase 1 phone validation + commercial-insurance script means every contacted physician is assumed to offer an appointment, so wait time is the only outcome.")
      ),
      n_sim_warn,
      fit_warn,
      zero_warn,
      tags$div(
        style = "margin: 14px 0; padding: 12px; border-left: 4px solid #5a8dee; background: #f8fafc;",
        tags$div(style = "font-size: 1.05em;",
                 tags$strong("Research question: "),
                 tags$em("\"Do rural ENT physicians have longer wait times for a new-patient appointment than urban ENT physicians?\"")),
        tags$div(style = "margin-top: 8px;", n_text),
        tags$div(style = "color: #555; font-size: 0.92em; margin-top: 6px;",
                 "Negative Binomial regression on business days until appointment. One observation per physician.")
      ),
      # ---- Precision / confidence-interval half-width ----
      uiOutput("ci_width_block"),
      # ---- Type I error self-check (auto after Run) ----
      uiOutput("type_i_block"),
      # ---- Minimum detectable effect (button-triggered) ----
      tags$div(
        style = "margin: 14px 0; padding: 12px; border-left: 4px solid #5a8dee; background: #f8fafc;",
        tags$div(style = "font-size: 1.05em;",
                 tags$strong("Minimum detectable effect at a fixed N. "),
                 tags$em("Given a planned physician count, what is the smallest rural-vs-urban wait gap (in days) that the study would detect at the target power?")),
        fluidRow(
          column(5,
            numericInput("mde_n_total",
              help_label("Planned physician count",
                "Total physicians the study will call. Defaults to the largest value currently in your simulation grid."),
              value = 600, min = 50, max = 10000, step = 50)
          ),
          column(5,
            numericInput("mde_n_sim",
              help_label("Replicates per binary-search iteration",
                "Each iteration of the binary search runs this many Monte Carlo replicates. 30 gives a quick answer; 50+ tightens it."),
              value = 30, min = 10, max = 200, step = 10)
          )
        ),
        actionButton("run_mde",
                     "Find minimum detectable effect",
                     class = "btn-outline-info", icon = icon("magnifying-glass-arrow-right")),
        br(), br(),
        uiOutput("mde_result")
      ),
      tags$div(
        style = "background:#f0f9ff; border-left: 3px solid #0284c7; padding: 10px; font-size: 0.92em; margin-top: 8px;",
        icon("location-dot"),
        tags$strong(" Why the +20% safety margin?"),
        " A study that recruits from every state will have physicians ",
        tags$span(style = "border-bottom:1px dotted #555; cursor:help;",
                  title = "Clustering means physicians in the same group (here, the same state) tend to be more similar to each other than to physicians in other groups. Within-state similarity reduces effective sample size.",
                  "clustered"),
        " within states (state Medicaid policies, scope-of-practice laws, ENT workforce density). The bare-minimum N above assumes physicians are independent. State-level clustering inflates ",
        tags$span(style = "border-bottom:1px dotted #555; cursor:help;",
                  title = "Standard error: the precision of a regression coefficient. When physicians are clustered, the standard error of the rural-vs-urban contrast is larger than the i.i.d. calculation suggests.",
                  "standard errors"),
        " via a ",
        tags$span(style = "border-bottom:1px dotted #555; cursor:help;",
                  title = "Design effect (DEFF) quantifies how much your effective sample size shrinks under clustering. DEFF = 1 + (m - 1) * ICC, where m is the average cluster size.",
                  "design effect"),
        " of roughly 1 + (avg physicians per state - 1) x state ",
        tags$span(style = "border-bottom:1px dotted #555; cursor:help;",
                  title = "Intra-class correlation coefficient (ICC): the fraction of total outcome variance attributable to between-state differences. Higher ICC means more state-level clustering.",
                  "ICC"),
        ". For an ENT study covering all 50 states (around 4 to 6 physicians per state) with a plausible state ICC of 0.05 to 0.10, the inflation lands at 15 to 50%. 20% is a safe middle-of-the-road target; bump up to 30 to 40% if you expect strong state-level clustering."
      )
    )
  })

  # ---- Grid table ----------------------------------------------------------
  output$grid_table <- DT::renderDT({
    grid <- power_grid()
    show <- data.frame(
      `Physicians called`     = grid$n_total,
      `Rural / urban split`   = sprintf("%d / %d", grid$n_rural, grid$n_urban),
      `Pow: wait time`        = sprintf("%.0f%%", 100 * grid$pow_wait),
      `Mean log rural effect` = round(grid$mean_log_effect, 3),
      `Model convergence`     = sprintf("%.0f%%", 100 * grid$convergence_rate),
      `Simulations per row`   = grid$n_sim,
      check.names = FALSE
    )
    DT::datatable(show, rownames = FALSE,
                  options = list(dom = "t", pageLength = 50, scrollX = TRUE))
  })

  # ---- Power curve ---------------------------------------------------------
  output$power_curve <- renderPlot({
    grid <- power_grid()
    ggplot(grid, aes(n_total, pow_wait)) +
      geom_hline(yintercept = input$target_power, linetype = 2,
                 color = "grey40") +
      annotate("text", x = max(grid$n_total), y = input$target_power,
               vjust = -0.7, hjust = 1, size = 4, colour = "grey30",
               label = sprintf("Target %.0f%% power",
                               input$target_power * 100)) +
      geom_line(linewidth = 1.1, color = "#0072B2") +
      geom_point(size = 2.8, color = "#0072B2") +
      scale_y_continuous(limits = c(0, 1.04),
                         labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Total ENT physicians called",
           y = "Probability of detecting the rural-vs-urban wait difference (power)",
           title = "Power vs sample size: rural-vs-urban wait time",
           subtitle = sprintf(
             "Pooled-ENT defaults. Alpha = %g, %d simulations per point. Sample mix %g%% rural.",
             input$alpha, input$n_sim, 100 * input$rural_frac)) +
      theme_minimal(base_size = 14) +
      theme(plot.title.position = "plot")
  })

  output$curve_interpretation <- renderUI({
    grid <- power_grid()
    first_hit <- function(col) {
      ok <- which(grid[[col]] >= input$target_power)
      if (length(ok) == 0L) NA_integer_ else grid$n_total[ok[1]]
    }
    n_wait  <- first_hit("pow_wait")
    pct_tgt <- round(100 * input$target_power)

    tags$div(
      style = "background: #f8fafc; border-left: 4px solid #5a8dee; padding: 14px;",
      tags$p(sprintf(
        "Assumed truth: rural ENT waits are %.0f days longer than urban (%.0f vs %.0f). Negative Binomial dispersion phi = %g (pooled-ENT default 1.7).",
        input$wait_rural - input$wait_urban,
        input$wait_rural, input$wait_urban, input$phi
      )),
      tags$p(if (is.na(n_wait))
               sprintf("At %d%% target power, the wait-time outcome was not reached in this grid. Add larger physician counts (e.g., 1500, 2000) and re-run.", pct_tgt)
             else
               sprintf("The wait-time outcome reaches %d%% power at %s physicians (about %d rural + %d urban).",
                       pct_tgt, format(n_wait, big.mark = ","),
                       round(n_wait * input$rural_frac),
                       n_wait - round(n_wait * input$rural_frac)))
    )
  })

  # ---- Sentences-you-can-write panel --------------------------------------
  output$claims_panel <- renderUI({
    grid <- power_grid()
    first_hit <- function(col) {
      ok <- which(grid[[col]] >= input$target_power)
      if (length(ok) == 0L) NA_integer_ else grid$n_total[ok[1]]
    }
    wait_ok <- !is.na(first_hit("pow_wait"))

    days_diff       <- input$wait_rural - input$wait_urban
    pct_longer_wait <- 100 * (input$wait_rural / input$wait_urban - 1)

    row <- function(met, ideal, fallback, tag) {
      if (met) {
        sprintf(
          paste0("<li style='margin:10px 0;'>",
                 "<span style='color:#15803d;font-weight:bold;'>&#10003; You CAN write:</span> ",
                 "<span style='background:#e0e7ff;color:#3730a3;font-size:0.78em;padding:2px 6px;border-radius:4px;margin-left:4px;'>%s</span>",
                 "<br><span style='color:#1f2937;'>&ldquo;%s&rdquo;</span></li>"),
          tag, ideal)
      } else {
        sprintf(
          paste0("<li style='margin:10px 0;'>",
                 "<span style='color:#b91c1c;font-weight:bold;'>&#10007; You CANNOT yet write:</span> ",
                 "<span style='background:#e0e7ff;color:#3730a3;font-size:0.78em;padding:2px 6px;border-radius:4px;margin-left:4px;'>%s</span>",
                 "<br><span style='color:#1f2937;'>&ldquo;%s&rdquo;</span>",
                 "<br><span style='color:#555;font-size:0.95em;'>You would have to settle for: &ldquo;%s&rdquo;</span></li>"),
          tag, ideal, fallback)
      }
    }

    bullets <- c(
      row(wait_ok,
          sprintf("Rural otolaryngology offices had a significantly longer mean wait for a new-patient appointment than urban offices (%.0f vs %.0f business days, P < %g).",
                  input$wait_rural, input$wait_urban, input$alpha),
          "We did not detect a significant difference in wait time between rural and urban otolaryngology offices.",
          "abstract / headline"),
      row(wait_ok,
          sprintf("Rural otolaryngology wait times were %.0f%% longer than urban wait times.",
                  pct_longer_wait),
          "Rural and urban otolaryngology wait times did not differ significantly.",
          "abstract / press release"),
      row(wait_ok,
          sprintf("Rural otolaryngologists had a mean wait %.0f days longer than urban otolaryngologists for a new-patient appointment.",
                  days_diff),
          "We did not detect a meaningful rural-urban gap in wait time.",
          "magnitude reporting"),
      row(wait_ok,
          sprintf("Mean appointment wait time was %.0f days at rural otolaryngologists versus %.0f days at urban otolaryngologists (P < %g).",
                  input$wait_rural, input$wait_urban, input$alpha),
          sprintf("Mean appointment wait times were %.0f days at rural and %.0f days at urban otolaryngologists (P > %g).",
                  input$wait_rural, input$wait_urban, input$alpha),
          "results section, mean reporting")
    )

    # Sentences that the *design* cannot support, regardless of N. Each
    # has a brief one-liner explaining why the design rules it out.
    design_limits <- list(
      list(
        claim = "Rural location CAUSES longer wait times for ENT appointments.",
        why   = "Observational mystery-caller design. Can establish association but not causation. Use language like \"associated with\" or \"observed to be longer\"."
      ),
      list(
        claim = "Medicaid patients face longer ENT wait times than commercially insured patients.",
        why   = "Phase 2 script uses commercial (Blue Cross / Blue Shield) insurance for every call. No insurance contrast in this study."
      ),
      list(
        claim = "Rural ENT offices are less likely to offer a new-patient appointment than urban offices.",
        why   = "Phase 1 phone validation means every contacted office is assumed to offer an appointment in the simulation. To make this claim you would need a different design that retains physicians who refuse appointments."
      ),
      list(
        claim = "Black, Hispanic, or non-English-speaking patients face longer ENT waits than white English-speaking patients.",
        why   = "Caller does not signal race, ethnicity, or language preference. No racial / linguistic contrast in this design."
      ),
      list(
        claim = "Rural ENT patients have worse clinical outcomes (cancer survival, hearing recovery, etc.) than urban patients.",
        why   = "The study measures appointment access only. No follow-up after appointment offered, no chart review, no patient outcomes."
      ),
      list(
        claim = "Wait times for ENT care have gotten worse (or better) over time.",
        why   = "Cross-sectional design. No temporal comparison. To make a trend claim you need repeated calls across multiple years."
      ),
      list(
        claim = "Quality of the ENT appointment differs between rural and urban physicians.",
        why   = "The caller never attends an appointment. Wait time is the only outcome."
      ),
      list(
        claim = "The rural ENT wait penalty is significantly larger for pediatric (or head-and-neck cancer) patients than for other subspecialties.",
        why   = "This is the rural-by-subspecialty interaction effect, which is roughly +1 day in pooled real data and would require thousands of physicians to detect at the planned NB phi. Reframe to per-subspecialty stratified estimates instead."
      ),
      list(
        claim = "ENT wait times differ by state.",
        why   = "With about 4-6 physicians per state, the study is underpowered for state-level inference. Geographic claims should stay at the rural-vs-urban level, not the state level."
      ),
      list(
        claim = "Advanced Practice Providers (APPs) see rural ENT patients sooner than physicians.",
        why   = "The study records APP-vs-physician routing as a secondary observation, but power was not budgeted for APP-stratified wait-time analysis. APP findings can be reported descriptively only."
      )
    )
    design_html <- paste(vapply(design_limits, function(x) {
      sprintf(
        paste0("<li style='margin:10px 0;'>",
               "<span style='color:#b91c1c;font-weight:bold;'>&#10007; You CANNOT write under this design:</span> ",
               "<span style='background:#fee2e2;color:#7f1d1d;font-size:0.78em;padding:2px 6px;border-radius:4px;margin-left:4px;'>design limit</span>",
               "<br><span style='color:#1f2937;'>&ldquo;%s&rdquo;</span>",
               "<br><span style='color:#555;font-size:0.95em;'><strong>Why not:</strong> %s</span></li>"),
        x$claim, x$why)
    }, character(1)), collapse = "")

    tagList(
      tags$h4("Sentences you can and cannot write in your paper"),
      tags$p(style = "color: #555;",
             "Two kinds of red X exist below. ",
             tags$strong("Power-gated"), " red X means the simulation hasn't yet reached your target power; collecting more physicians might flip it green. ",
             tags$strong("Design-gated"), " red X means the study design itself rules out that claim; no amount of additional sampling fixes it."),
      tags$h5(style = "margin-top: 18px;", "Claims gated by statistical power"),
      tags$ul(style = "padding-left: 22px;",
              HTML(paste(bullets, collapse = ""))),
      tags$h5(style = "margin-top: 24px;",
              "Claims this study design cannot support (no matter how many physicians)"),
      tags$ul(style = "padding-left: 22px;", HTML(design_html))
    )
  })

  # ---- Reset button --------------------------------------------------------
  observeEvent(input$reset_defaults, {
    updateNumericInput(session,  "wait_urban",   value = DEFAULTS$wait_urban)
    updateNumericInput(session,  "wait_rural",   value = DEFAULTS$wait_rural)
    updateNumericInput(session,  "phi",          value = DEFAULTS$phi)
    updateCheckboxInput(session, "use_adjusted_model", value = TRUE)
    updateSliderInput(session,   "state_icc",    value = 0.08)
    updateNumericInput(session,  "n_subspecs",   value = 7)
    updateNumericInput(session,  "n_extra_covars", value = 6)
    updateSliderInput(session,   "rural_frac",   value = DEFAULTS$rural_frac)
    updateNumericInput(session,  "alpha",        value = DEFAULTS$alpha)
    updateNumericInput(session,  "target_power", value = DEFAULTS$target_power)
    updateTextInput(session,     "npi_grid",     value = DEFAULTS$npi_grid)
    updateNumericInput(session,  "n_sim",        value = DEFAULTS$n_sim)
    updateNumericInput(session,  "seed",         value = 1978L)
    showNotification("Reset to ENT pooled-data defaults (ICC = 0.08, seed = 1978).",
                     type = "message", duration = 4)
  })

  # ---- Health-check banner (top of main panel) -----------------------------
  output$health_banner <- renderUI({
    h <- mc_health_check()
    if (h$ok) {
      tags$div(
        style = "background:#dcfce7; border-left:4px solid #16a34a; padding:8px 12px; margin-bottom:10px; font-size:0.92em;",
        icon("circle-check"), " ",
        tags$strong("Simulator ready. "), h$msg
      )
    } else {
      tags$div(
        style = "background:#fee2e2; border-left:4px solid #dc2626; padding:10px 12px; margin-bottom:10px;",
        icon("circle-xmark"),
        tags$strong(" Simulator NOT ready: ", h$msg),
        tags$br(),
        "From a fresh R session at the project root run:",
        tags$pre("install.packages(c('TMB','glmmTMB'), type='binary')"),
        "Then restart the Shiny app."
      )
    }
  })

  # ---- Save / load scenario ------------------------------------------------
  scenario_keys <- c(
    "wait_urban", "wait_rural", "phi",
    "rural_frac", "alpha", "target_power",
    "npi_grid", "n_sim", "seed",
    "use_adjusted_model", "state_icc", "n_subspecs", "n_extra_covars",
    "sens_n_total", "sens_n_sim"
  )

  output$save_scenario <- downloadHandler(
    filename = function() {
      sprintf("ent_power_scenario_%s.json",
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      vals <- sapply(scenario_keys, function(k) input[[k]],
                     simplify = FALSE)
      vals$saved_at <- as.character(Sys.time())
      vals$app      <- "ent_rural_urban_power"
      jsonlite::write_json(vals, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  observeEvent(input$load_scenario, {
    f <- input$load_scenario
    if (is.null(f)) return(NULL)
    vals <- tryCatch(jsonlite::read_json(f$datapath, simplifyVector = TRUE),
                     error = function(e) NULL)
    if (is.null(vals)) {
      showNotification("Could not parse the JSON scenario file.",
                       type = "error", duration = 6)
      return(NULL)
    }
    for (k in intersect(names(vals), scenario_keys)) {
      v <- vals[[k]]
      if (is.null(v) || (is.character(v) && length(v) == 0)) next
      if (k %in% c("rural_frac")) {
        updateSliderInput(session, k, value = v)
      } else if (k %in% c("npi_grid")) {
        updateTextInput(session, k, value = v)
      } else {
        updateNumericInput(session, k, value = v)
      }
    }
    showNotification(sprintf("Loaded scenario from %s.", f$name),
                     type = "message", duration = 5)
  })

  # ---- Methods text for the manuscript -------------------------------------
  methods_paragraph <- reactive({
    grid <- tryCatch(power_grid(), error = function(e) NULL)
    if (is.null(grid)) return("Run the simulation first to generate Methods text.")
    first_hit <- function(col) {
      ok <- which(grid[[col]] >= input$target_power)
      if (length(ok) == 0L) NA_integer_ else grid$n_total[ok[1]]
    }
    n_wait <- first_hit("pow_wait")
    days_diff <- input$wait_rural - input$wait_urban
    pct_longer <- 100 * (input$wait_rural / input$wait_urban - 1)
    pct_tgt <- round(100 * input$target_power)
    safety_factor <- 1.20
    nonresp_factor <- 1.18

    n_with_clust <- if (is.na(n_wait)) NA else ceiling(n_wait * safety_factor)
    n_final      <- if (is.na(n_wait)) NA else ceiling(n_wait * safety_factor * nonresp_factor)

    if (is.na(n_wait)) {
      return(list(
        prose = "Sample size at the target power was not reached in the current grid. Adjust the inputs and re-run the simulation, then revisit this tab.",
        n_wait = NA_integer_
      ))
    }
    use_adj <- isTRUE(input$use_adjusted_model)

    sim_model_clause <- if (use_adj) {
      sprintf(
        "Each Monte Carlo replicate fit the planned analysis model directly: a negative binomial mixed model with rural, subspecialty (%d levels), and %d additional adjustment covariates as fixed effects and a state-level random intercept assumed to carry an intraclass correlation of %.2f.",
        input$n_subspecs, input$n_extra_covars, input$state_icc
      )
    } else {
      "Each Monte Carlo replicate fit a negative binomial generalized linear model of wait days on a rural-vs-urban indicator. To approximate state-level clustering not captured by the bare-bones model, the simulation estimate was inflated by 20 percent."
    }

    final_n_clause <- if (use_adj) {
      sprintf(
        "yielding a recruitment target of %s ENT physicians after an additional 18 percent buffer for non-response and incomplete calls. ",
        format(ceiling(n_wait * nonresp_factor), big.mark = ",")
      )
    } else {
      sprintf(
        "To accommodate state-level clustering, we inflated the simulation estimate by 20 percent to %s physicians, and budgeted an additional 18 percent for non-response and incomplete calls, yielding a final recruitment target of %s ENT physicians. ",
        format(n_with_clust, big.mark = ","), format(n_final, big.mark = ",")
      )
    }

    prose <- paste0(
      "Sample size and power. ",
      "We determined the required sample size for a single-call rural-vs-urban mystery-caller study of otolaryngology (ENT) physicians using Monte Carlo simulation. ",
      "Each sampled physician was assumed to be contacted once with a commercial (Blue Cross / Blue Shield) insurance script following Phase 1 phone-number validation; therefore the primary outcome was business days until the next available new-patient appointment (a non-negative count, modeled with a negative binomial distribution via the glmmTMB::nbinom2 family) and the primary predictor was rural versus urban physician location. ",
      sim_model_clause, " ",
      sprintf(
        "Under the assumed truth that the mean appointment wait would be %.0f business days at urban physicians and %.0f business days at rural physicians (an %.0f-day, %.1f%% rural penalty, conservative relative to a pooled analysis of two prior US ENT mystery-caller datasets (Corbisiero 2024 and Saeedi ENT, n = 273 rural observations pooled)), with negative binomial dispersion phi = %g, two-sided alpha = %g, and a balanced (50/50) rural-urban sampling design, ",
        input$wait_urban, input$wait_rural, days_diff, pct_longer,
        input$phi, input$alpha
      ),
      sprintf(
        "%d Monte Carlo replicates per sample-size grid point indicated that %s ENT physicians (approximately %d rural and %d urban) were required to detect the hypothesized rural-vs-urban difference at %d%% power, ",
        input$n_sim,
        format(n_wait, big.mark = ","),
        round(n_wait * input$rural_frac),
        n_wait - round(n_wait * input$rural_frac),
        pct_tgt
      ),
      final_n_clause,
      "The primary analysis will be a negative binomial mixed model of wait days regressed on a rural-vs-urban indicator, with subspecialty, physician sex, degree, fellowship training, board certification, practice setting, academic affiliation, group practice size, years in practice, day of the week of the call, central appointment number, number of transfers, number of practice locations, and hold time as fixed-effect covariates, and a random intercept for physician practice state. ",
      "Simulation code and the full power grid are available in the project repository."
    )
    list(prose = prose, n_wait = n_wait)
  })

  output$methods_text <- renderUI({
    mp <- methods_paragraph()
    latex_eq <- paste0(
      "$$\\begin{aligned}",
      "Y_{ij} \\mid u_j \\;&\\sim\\; \\mathrm{NB}\\!\\left(\\mu_{ij},\\, \\phi\\right) \\\\",
      "\\log(\\mu_{ij}) \\;&=\\; \\beta_0",
      " + \\beta_{\\mathrm{rural}}\\, \\mathrm{Rural}_{ij}",
      " + \\boldsymbol{\\beta}_{\\mathrm{subspec}}^{\\!\\top} \\mathrm{Subspec}_{ij}",
      " + \\beta_{\\mathrm{female}}\\, \\mathrm{Female}_{ij} \\\\",
      "&\\quad+\\, \\boldsymbol{\\beta}_{\\mathrm{degree}}^{\\!\\top} \\mathrm{Degree}_{ij}",
      " +\\, \\boldsymbol{\\beta}_{\\mathrm{practice}}^{\\!\\top} \\mathrm{PracticeSetting}_{ij}",
      " +\\, \\beta_{\\mathrm{academic}}\\, \\mathrm{Academic}_{ij} \\\\",
      "&\\quad+\\, \\boldsymbol{\\beta}_{\\mathrm{groupsize}}^{\\!\\top} \\mathrm{GroupSize}_{ij}",
      " +\\, \\beta_{\\mathrm{boardcert}}\\, \\mathrm{BoardCert}_{ij}",
      " +\\, \\beta_{\\mathrm{fellowship}}\\, \\mathrm{FellowshipTrained}_{ij} \\\\",
      "&\\quad+\\, \\boldsymbol{\\beta}_{\\mathrm{exp}}^{\\!\\top} \\mathrm{YearsInPractice}_{ij}",
      " +\\, \\boldsymbol{\\beta}_{\\mathrm{dow}}^{\\!\\top} \\mathrm{DayOfWeek}_{ij}",
      " +\\, \\beta_{\\mathrm{central}}\\, \\mathrm{CentralAppt}_{ij} \\\\",
      "&\\quad+\\, \\beta_{\\mathrm{transfers}}\\, \\log(1+\\mathrm{NumTransfers}_{ij})",
      " +\\, \\beta_{\\mathrm{multiloc}}\\, \\log(\\mathrm{NumLocations}_{ij})",
      " +\\, \\beta_{\\mathrm{hold}}\\, \\mathrm{HoldTime}_{ij}",
      " +\\, u_j \\\\",
      "u_j \\;&\\sim\\; \\mathcal{N}\\!\\left(0,\\, \\sigma^{2}_{\\mathrm{state}}\\right)",
      "\\end{aligned}$$"
    )
    withMathJax(
      tags$div(
        style = "background:#f8fafc; border:1px solid #cbd5e1; padding:14px; line-height:1.65; font-size:1.02em;",
        tags$p(mp$prose),
        tags$h5(style = "margin-top: 14px;", "Model equation"),
        tags$p(style = "color: #555; font-size: 0.92em;",
               "The primary analysis is a negative binomial mixed model. ",
               tags$em("Y"),
               tags$sub("ij"),
               " is the wait in business days for the ",
               tags$em("i"), "-th physician practicing in state ",
               tags$em("j"),
               ". Categorical fixed effects (subspecialty, degree, practice setting, group size, years in practice, day of week) are written as vector coefficients ",
               tags$em(HTML("&beta;")),
               " applied to a vector of indicator variables."),
        HTML(latex_eq)
      )
    )
  })

  output$download_methods <- downloadHandler(
    filename = function() {
      sprintf("ent_methods_text_%s.txt",
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      writeLines(methods_paragraph()$prose, file)
    }
  )

  # ---- CI width / precision block (rendered in recommendation panel) ------
  output$ci_width_block <- renderUI({
    grid <- tryCatch(power_grid(), error = function(e) NULL)
    if (is.null(grid) || !"mean_se_rural" %in% names(grid)) return(NULL)
    tgt <- input$target_power
    ok_rows <- grid[!is.na(grid$pow_wait) & grid$pow_wait >= tgt, , drop = FALSE]
    if (nrow(ok_rows) == 0L) return(NULL)
    r <- ok_rows[1, ]
    se <- r$mean_se_rural
    if (is.na(se) || se <= 0) return(NULL)
    # 95% CI half-width on log scale -> rate ratio half-width -> days
    log_half <- 1.96 * se
    rr_low   <- exp(log(input$wait_rural / input$wait_urban) - log_half)
    rr_high  <- exp(log(input$wait_rural / input$wait_urban) + log_half)
    days_low  <- input$wait_urban * (rr_low  - 1)
    days_high <- input$wait_urban * (rr_high - 1)
    days_pt   <- input$wait_rural - input$wait_urban
    half_width_days <- (days_high - days_low) / 2
    tags$div(
      style = "background:#ecfdf5; border-left: 3px solid #10b981; padding: 10px; font-size: 0.95em; margin-top: 10px;",
      icon("ruler-horizontal"),
      tags$strong(" Expected precision at N = ",
                  format(r$n_total, big.mark = ","),
                  ":"),
      tags$br(),
      sprintf("Mean standard error of the rural log-rate coefficient was %.3f across replicates. ",
              se),
      sprintf("Translated to the response (days) scale: ",
              days_pt),
      tags$br(),
      tags$strong(sprintf("Point estimate %.1f d  (95%% CI %.1f to %.1f d; half-width approximately +/- %.1f d).",
                          days_pt, days_low, days_high, half_width_days))
    )
  })

  # ---- Type I error self-check (auto after main Run) ----------------------
  type_i_result <- eventReactive(input$run, {
    grid <- tryCatch(parsed_grid(), error = function(e) NULL)
    if (is.null(grid)) return(NULL)
    # Use the median grid N (or 400 floor) so this finishes quickly
    n_check <- max(200L, stats::median(grid))
    set.seed(isolate(as.integer(input$seed %||% 1978L)) + 1L)  # different stream
    withProgress(message = "Type I error self-check (null scenario)...",
                 value = 0, {
      out <- mc_ru_type_i_check(
        n_total       = n_check,
        rural_frac    = input$rural_frac,
        wait_baseline = input$wait_urban,
        phi           = input$phi,
        alpha         = input$alpha,
        n_sim         = max(50L, input$n_sim),  # need decent precision
        use_adjusted  = isTRUE(input$use_adjusted_model),
        state_icc     = input$state_icc,
        n_subspecs    = input$n_subspecs,
        n_extra_covars = input$n_extra_covars
      )
      out$n_total <- n_check
      out
    })
  }, ignoreNULL = TRUE)

  output$type_i_block <- renderUI({
    ti <- tryCatch(type_i_result(), error = function(e) NULL)
    if (is.null(ti)) return(NULL)
    in_ci <- isTRUE(ti$alpha_in_ci)
    color <- if (in_ci) "#10b981" else "#f59e0b"
    bg    <- if (in_ci) "#ecfdf5" else "#fef3c7"
    head_icon <- if (in_ci) icon("circle-check") else icon("triangle-exclamation")
    tags$div(
      style = sprintf("background:%s; border-left:3px solid %s; padding:10px; font-size:0.95em; margin-top:10px;",
                      bg, color),
      head_icon,
      tags$strong(" Type I error self-check (null scenario):"),
      tags$br(),
      sprintf(
        "Ran a parallel simulation at N = %s with rural = urban = %g days (no true effect). Rejection rate %.1f%% (95%% binomial CI %.1f to %.1f%%, %d replicates).",
        format(ti$n_total, big.mark = ","),
        input$wait_urban,
        100 * ti$rejection_rate,
        100 * ti$ci_low, 100 * ti$ci_high,
        ti$n_sim
      ),
      tags$br(),
      tags$strong(
        if (in_ci)
          sprintf("Verdict: rejection rate is consistent with the nominal alpha of %g. Model is well-calibrated.",
                  input$alpha)
        else
          sprintf("Verdict: rejection rate of %.1f%% is OUTSIDE the expected band; nominal alpha (%g) is not in the binomial CI. Re-check the model specification or increase replicates.",
                  100 * ti$rejection_rate, input$alpha)
      )
    )
  })

  # ---- Minimum detectable effect (button-triggered) -----------------------
  mde_result_data <- eventReactive(input$run_mde, {
    n_total <- as.integer(input$mde_n_total %||% 600L)
    n_sim   <- as.integer(input$mde_n_sim %||% 30L)
    set.seed(isolate(as.integer(input$seed %||% 1978L)) + 2L)
    withProgress(message = "Binary search for minimum detectable effect...",
                 value = 0, {
      mc_ru_find_mde(
        n_total      = n_total,
        rural_frac   = input$rural_frac,
        wait_urban   = input$wait_urban,
        phi          = input$phi,
        target_power = input$target_power,
        alpha        = input$alpha,
        n_sim        = n_sim,
        use_adjusted = isTRUE(input$use_adjusted_model),
        state_icc    = input$state_icc,
        n_subspecs   = input$n_subspecs,
        n_extra_covars = input$n_extra_covars,
        progress_callback = function(it, max_it, mid, pw) {
          incProgress(1 / max_it,
                      detail = sprintf("trying rural=%.1f, power=%.2f", mid, pw))
        }
      )
    })
  }, ignoreNULL = TRUE)

  output$mde_result <- renderUI({
    r <- tryCatch(mde_result_data(), error = function(e) NULL)
    if (is.null(r)) return(tags$em(style = "color:#777;", "Click the button above to run a binary search at the planned N."))
    if (isTRUE(r$reached)) {
      tags$div(
        style = "background:#ecfdf5; border-left:3px solid #10b981; padding:10px; font-size:0.95em;",
        icon("circle-check"),
        tags$strong(sprintf(
          " At N = %s physicians, the smallest rural-vs-urban wait gap detectable at %d%% power is %.1f days.",
          format(input$mde_n_total, big.mark = ","),
          round(100 * input$target_power),
          r$mde_days
        )),
        tags$br(),
        sprintf("That corresponds to a rural mean of about %.0f days vs urban mean of %.0f days. ",
                input$wait_urban + r$mde_days, input$wait_urban),
        tags$em(r$message)
      )
    } else {
      tags$div(
        style = "background:#fee2e2; border-left:3px solid #dc2626; padding:10px; font-size:0.95em;",
        icon("circle-xmark"),
        tags$strong(" Minimum detectable effect not reached. "),
        r$message
      )
    }
  })

  # ---- Fixed-N one-click planning -----------------------------------------
  fixed_n_result <- eventReactive(input$run_fixed_n, {
    set.seed(isolate(as.integer(input$seed %||% 1978L)) + length(CANONICAL_SUBSPECIALTY_CODES))
    N      <- as.integer(input$fixed_n %||% 800L)
    nsim   <- as.integer(input$fixed_n_sim %||% 30L)
    use_adj <- isTRUE(input$use_adjusted_model)

    sim_fn <- function(n_total, rural_frac = input$rural_frac,
                        wait_rural = input$wait_rural,
                        n_sim_use = nsim) {
      args <- list(
        n_total    = n_total,
        rural_frac = rural_frac,
        wait_urban = input$wait_urban,
        wait_rural = wait_rural,
        phi        = input$phi,
        alpha      = input$alpha,
        n_sim      = n_sim_use
      )
      if (use_adj) {
        args$state_icc      <- input$state_icc
        args$n_subspecs     <- input$n_subspecs
        args$n_extra_covars <- input$n_extra_covars
        do.call(mc_ru_run_power_adjusted, args)
      } else {
        do.call(mc_ru_run_power_wait_only, args)
      }
    }

    withProgress(message = "Fixed-N planning analysis...", value = 0, {
      # Step 1: power at planned N under current assumed effect
      incProgress(0.05, detail = "Step 1/5: power at planned N")
      pow <- sim_fn(N, n_sim_use = max(nsim, 50L))

      # Step 2: CI half-width (already computed inside pow as mean_se_rural)
      incProgress(0.05, detail = "Step 2/5: precision / CI half-width")
      se        <- pow$mean_se_rural
      log_half  <- if (is.finite(se)) 1.96 * se else NA_real_
      rr_pt     <- input$wait_rural / input$wait_urban
      ci_low_d  <- if (is.na(log_half)) NA else
                   input$wait_urban * (exp(log(rr_pt) - log_half) - 1)
      ci_high_d <- if (is.na(log_half)) NA else
                   input$wait_urban * (exp(log(rr_pt) + log_half) - 1)
      half_d    <- if (is.na(log_half)) NA else (ci_high_d - ci_low_d) / 2

      # Step 3: MDE binary search
      incProgress(0.30, detail = "Step 3/5: minimum detectable effect")
      mde <- mc_ru_find_mde(
        n_total      = N,
        rural_frac   = input$rural_frac,
        wait_urban   = input$wait_urban,
        phi          = input$phi,
        target_power = input$target_power,
        alpha        = input$alpha,
        n_sim        = nsim,
        use_adjusted = use_adj,
        state_icc    = input$state_icc,
        n_subspecs   = input$n_subspecs,
        n_extra_covars = input$n_extra_covars
      )

      # Step 4: non-response buffer (analyzable N at 80, 85, 90% retention)
      incProgress(0.30, detail = "Step 4/5: non-response buffer")
      retentions <- c(0.80, 0.85, 0.90, 1.00)
      buffer <- do.call(rbind, lapply(retentions, function(ret) {
        n_keep <- round(N * ret)
        r <- sim_fn(n_keep, n_sim_use = nsim)
        data.frame(retention = ret,
                   n_analyzable = n_keep,
                   power = r$pow_wait)
      }))

      # Step 5: allocation sensitivity (rural fraction sweep at N)
      incProgress(0.20, detail = "Step 5/5: rural-fraction sweep")
      alloc_fracs <- c(0.50, 0.35, 0.20, 0.15)
      alloc <- do.call(rbind, lapply(alloc_fracs, function(rf) {
        r <- sim_fn(N, rural_frac = rf, n_sim_use = nsim)
        data.frame(rural_frac = rf,
                   n_rural    = round(N * rf),
                   n_urban    = N - round(N * rf),
                   power      = r$pow_wait)
      }))

      list(
        N         = N,
        power     = pow$pow_wait,
        mean_se   = se,
        ci_low_d  = ci_low_d,
        ci_high_d = ci_high_d,
        half_d    = half_d,
        mde       = mde,
        buffer    = buffer,
        alloc     = alloc,
        n_sim     = nsim
      )
    })
  }, ignoreNULL = TRUE)

  output$fixed_n_summary <- renderUI({
    r <- tryCatch(fixed_n_result(), error = function(e) NULL)
    if (is.null(r)) {
      return(tags$em(style = "color:#777;",
                     "Set N and click 'Run full fixed-N analysis' above."))
    }

    box <- function(title, body, color = "#5a8dee", bg = "#f8fafc") {
      tags$div(
        style = sprintf("margin:14px 0; padding:14px; border-left:4px solid %s; background:%s;",
                        color, bg),
        tags$div(style = "font-size:1.05em; font-weight:bold;", title),
        tags$div(style = "margin-top:8px;", body)
      )
    }

    pct <- function(x) sprintf("%.0f%%", 100 * x)
    n_lbl <- format(r$N, big.mark = ",")

    buf_tbl <- r$buffer
    buf_tbl$Retention      <- sprintf("%.0f%%", 100 * buf_tbl$retention)
    buf_tbl$`Analyzable N` <- format(buf_tbl$n_analyzable, big.mark = ",")
    buf_tbl$Power          <- sprintf("%.0f%%", 100 * buf_tbl$power)
    buf_html <- HTML(paste(
      "<table style='width:100%;font-size:0.95em;'>",
      "<thead><tr><th>Retention</th><th>Analyzable N</th><th>Power</th></tr></thead><tbody>",
      paste(sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>",
                    buf_tbl$Retention, buf_tbl$`Analyzable N`, buf_tbl$Power),
            collapse = ""),
      "</tbody></table>"))

    alloc_tbl <- r$alloc
    alloc_tbl$`Rural %`    <- sprintf("%.0f%%", 100 * alloc_tbl$rural_frac)
    alloc_tbl$`Rural + Urban` <- sprintf("%s + %s",
                                         format(alloc_tbl$n_rural, big.mark = ","),
                                         format(alloc_tbl$n_urban, big.mark = ","))
    alloc_tbl$Power        <- sprintf("%.0f%%", 100 * alloc_tbl$power)
    alloc_html <- HTML(paste(
      "<table style='width:100%;font-size:0.95em;'>",
      "<thead><tr><th>Rural %</th><th>Rural + Urban</th><th>Power</th></tr></thead><tbody>",
      paste(sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>",
                    alloc_tbl$`Rural %`, alloc_tbl$`Rural + Urban`, alloc_tbl$Power),
            collapse = ""),
      "</tbody></table>"))

    per_subspec_n   <- floor(r$N / max(1, input$n_subspecs))
    per_subspec_cell <- floor(per_subspec_n / 2)

    # Build protocol paragraph
    proto <- sprintf(
      paste(
        "We will enroll %s otolaryngology physicians (%s rural and %s urban),",
        "stratified by 2020 RUCA code. Anticipating a 15-20%% non-response or",
        "incomplete-call rate, the projected analyzable sample is approximately",
        "%s physicians. At this analyzable sample size, the study has approximately",
        "%s power to detect the primary hypothesized rural-vs-urban appointment-wait",
        "difference of %.0f business days (urban mean %.0f, rural mean %.0f),",
        "%s power to detect a difference as small as %.1f days,",
        "and is expected to estimate the rural-vs-urban wait difference with a 95%%",
        "confidence interval half-width of approximately +/- %.1f days."
      ),
      format(r$N, big.mark = ","),
      format(round(r$N * input$rural_frac), big.mark = ","),
      format(r$N - round(r$N * input$rural_frac), big.mark = ","),
      format(round(r$N * 0.825), big.mark = ","),
      pct(r$power),
      input$wait_rural - input$wait_urban,
      input$wait_urban, input$wait_rural,
      pct(input$target_power),
      r$mde$mde_days %||% NA,
      r$half_d %||% NA
    )

    tagList(
      tags$h3(sprintf("Fixed-N planning at N = %s", n_lbl)),
      box(
        sprintf("1. Power at your planned effect (N = %s, gap = %d days)",
                n_lbl, as.integer(input$wait_rural - input$wait_urban)),
        tagList(
          sprintf("Power = %s. Mean log-scale SE of the rural coefficient = %.3f.",
                  pct(r$power), r$mean_se),
          tags$br(),
          tags$em(if (r$power >= input$target_power)
                    sprintf("You are at or above the %s target power.",
                            pct(input$target_power))
                  else
                    sprintf("Below the %s target. Consider reducing N or revising assumptions.",
                            pct(input$target_power)))
        ),
        color = "#10b981", bg = "#ecfdf5"
      ),
      box(
        "2. Expected precision (95% CI half-width on the days scale)",
        tagList(
          if (is.na(r$half_d))
            tags$span(style = "color:#777;", "Could not compute precision (mean SE was missing).")
          else
            sprintf("Rural-urban penalty point estimate: %.0f days. 95%% CI approximately %.1f to %.1f days. CI half-width: +/- %.1f days.",
                    input$wait_rural - input$wait_urban,
                    r$ci_low_d, r$ci_high_d, r$half_d)
        )
      ),
      box(
        "3. Minimum detectable effect at this N",
        tagList(
          if (isTRUE(r$mde$reached))
            sprintf("Smallest rural-urban gap detectable at %s power: %.1f days (rural %.1f vs urban %.0f). %s",
                    pct(input$target_power),
                    r$mde$mde_days,
                    input$wait_urban + r$mde$mde_days,
                    input$wait_urban,
                    r$mde$message)
          else
            "Target power not reached even at 3x the urban wait. Consider lowering the target power."
        )
      ),
      box(
        "4. Non-response buffer: power as the analyzable sample shrinks",
        buf_html,
        color = "#a855f7", bg = "#faf5ff"
      ),
      box(
        "5. Allocation sensitivity: rural-fraction sweep at this N",
        tagList(
          alloc_html,
          tags$br(),
          tags$em("50/50 maximizes power. 15/85 mirrors the natural ENT population mix.")
        ),
        color = "#a855f7", bg = "#faf5ff"
      ),
      box(
        "Bonus: per-subspecialty feasibility (descriptive)",
        tagList(
          sprintf("With %d subspecialty levels, N = %s gives about %d physicians per subspecialty (about %d rural and %d urban per subspecialty at 50/50 sampling).",
                  input$n_subspecs, n_lbl, per_subspec_n,
                  per_subspec_cell, per_subspec_n - per_subspec_cell),
          tags$br(),
          tags$em("Rule of thumb: 50+ per cell is enough for stratified estimates of the big subspecialties (General, Pediatric, Neurotology). Smaller subspecialties (Laryngology, Facial Plastics, Rhinology) should be reported descriptively only."),
          tags$br()
        ),
        color = "#f59e0b", bg = "#fefce8"
      ),
      box(
        "Protocol paragraph (drop-in)",
        tagList(
          tags$div(style = "background:#fff; border:1px solid #cbd5e1; padding:12px; font-family:Georgia, serif; line-height:1.6;",
                   proto),
          tags$br(),
          downloadButton("download_fixed_n_protocol",
                         "Download paragraph (.txt)",
                         class = "btn-outline-success",
                         icon = icon("file-lines"))
        ),
        color = "#64748b", bg = "#f1f5f9"
      )
    )
  })

  output$download_fixed_n_protocol <- downloadHandler(
    filename = function() {
      sprintf("ent_fixed_n_protocol_%s.txt",
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      r <- fixed_n_result()
      if (is.null(r)) {
        writeLines("Run the fixed-N analysis first.", file)
        return(invisible())
      }
      pct <- function(x) sprintf("%.0f%%", 100 * x)
      proto <- sprintf(
        paste(
          "We will enroll %s otolaryngology physicians (%s rural and %s urban),",
          "stratified by 2020 RUCA code. Anticipating a 15-20%% non-response or",
          "incomplete-call rate, the projected analyzable sample is approximately",
          "%s physicians. At this analyzable sample size, the study has approximately",
          "%s power to detect the primary hypothesized rural-vs-urban appointment-wait",
          "difference of %.0f business days (urban mean %.0f, rural mean %.0f),",
          "%s power to detect a difference as small as %.1f days,",
          "and is expected to estimate the rural-vs-urban wait difference with a 95%%",
          "confidence interval half-width of approximately +/- %.1f days."
        ),
        format(r$N, big.mark = ","),
        format(round(r$N * input$rural_frac), big.mark = ","),
        format(r$N - round(r$N * input$rural_frac), big.mark = ","),
        format(round(r$N * 0.825), big.mark = ","),
        pct(r$power),
        input$wait_rural - input$wait_urban,
        input$wait_urban, input$wait_rural,
        pct(input$target_power),
        r$mde$mde_days %||% NA,
        r$half_d %||% NA
      )
      writeLines(proto, file)
    }
  )

  # ---- Effect-size sensitivity sweep --------------------------------------
  sens_grid <- eventReactive(input$run_sens, {
    rural_values <- c(input$wait_urban + 2,
                      input$wait_urban + 4,
                      input$wait_urban + 6,
                      input$wait_urban + 8,
                      input$wait_urban + 10,
                      input$wait_urban + 13,
                      input$wait_urban + 16)
    rural_values <- unique(round(rural_values))
    n_steps <- length(rural_values)
    withProgress(message = "Sweeping over rural wait values...", value = 0, {
      rows <- lapply(seq_along(rural_values), function(i) {
        r <- rural_values[i]
        incProgress(1 / n_steps,
                    detail = sprintf("rural mean = %d days (%d of %d)",
                                     r, i, n_steps))
        out <- mc_ru_run_power_wait_only(
          n_total    = input$sens_n_total,
          rural_frac = input$rural_frac,
          wait_urban = input$wait_urban,
          wait_rural = r,
          phi        = input$phi,
          alpha      = input$alpha,
          n_sim      = input$sens_n_sim
        )
        out$rural_value <- r
        out$gap_days    <- r - input$wait_urban
        out
      })
      do.call(rbind, rows)
    })
  }, ignoreNULL = TRUE)

  output$sens_plot <- renderPlot({
    sg <- sens_grid()
    ggplot(sg, aes(gap_days, pow_wait)) +
      geom_hline(yintercept = input$target_power, linetype = 2,
                 color = "grey40") +
      annotate("text", x = max(sg$gap_days), y = input$target_power,
               vjust = -0.7, hjust = 1, size = 4, colour = "grey30",
               label = sprintf("Target %.0f%% power",
                               input$target_power * 100)) +
      geom_line(linewidth = 1.1, color = "#0072B2") +
      geom_point(size = 2.8, color = "#0072B2") +
      geom_vline(xintercept = input$wait_rural - input$wait_urban,
                 linetype = 3, color = "#D55E00", alpha = 0.7) +
      annotate("text", x = input$wait_rural - input$wait_urban,
               y = 0.05, color = "#D55E00", hjust = -0.1, size = 4,
               label = "Your assumed effect") +
      scale_y_continuous(limits = c(0, 1.04),
                         labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Assumed rural-urban wait gap (days)",
           y = sprintf("Power at N = %s physicians",
                       format(input$sens_n_total, big.mark = ",")),
           title = "Power vs assumed rural-urban effect",
           subtitle = sprintf(
             "Fixed N = %s; alpha = %g; %d simulations per point; NB phi = %g",
             format(input$sens_n_total, big.mark = ","),
             input$alpha, input$sens_n_sim, input$phi)) +
      theme_minimal(base_size = 14) +
      theme(plot.title.position = "plot")
  })

  output$sens_text <- renderUI({
    sg <- sens_grid()
    above <- sg[sg$pow_wait >= input$target_power, , drop = FALSE]
    if (nrow(above) == 0L) {
      return(tags$div(
        style = "background:#fee2e2; border-left:3px solid #dc2626; padding:10px; margin-top:8px;",
        icon("circle-info"),
        sprintf(" At N = %s, no assumed rural-urban gap in this sweep reaches %d%% power. Either increase the fixed sample size or accept a smaller power target.",
                format(input$sens_n_total, big.mark = ","),
                round(100 * input$target_power))
      ))
    }
    min_gap_powered <- min(above$gap_days)
    tags$div(
      style = "background:#f0f9ff; border-left:3px solid #0284c7; padding:10px; margin-top:8px;",
      icon("circle-info"),
      sprintf(" At N = %s physicians, the study has at least %d%% power whenever the true rural-urban wait gap is %d days or more. If the true gap turns out to be smaller than %d days, the study is underpowered at this sample size.",
              format(input$sens_n_total, big.mark = ","),
              round(100 * input$target_power),
              min_gap_powered, min_gap_powered)
    )
  })

  # ---- Download CSV --------------------------------------------------------
  output$download_csv <- downloadHandler(
    filename = function() {
      sprintf("ent_rural_urban_power_%s.csv",
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      utils::write.csv(power_grid(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
