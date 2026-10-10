#!/usr/bin/env Rscript
# =============================================================================
# ENT Cohort STROBE Diagram — ABOto-Anchored Attrition Chain
# =============================================================================
#
# Five-stage STROBE-style attrition from raw ABOto certifications to the
# analytic cohort. Every count is computed LIVE from the shared helper
# R/ent_registry_counts.R so this figure stays in sync with the Euler,
# CONSORT, and subspecialty findability figures.
#
# Chain (six stages, five exclusions):
#   1. ABOto Certifications (16,228 rows)
#      → remove duplicate subspecialty certs (one row per physician)
#   2. Unique Board-Certified Physicians (15,171 by abohns_id)
#      → remove non-CONUS (AK, HI, PR, territories)
#   3. Physicians in Contiguous US (14,996)
#      → remove non-practicing / retired
#   4. Active Practice (11,477 rows)
#      → collapse duplicate NPIs to one physician
#   5. Unique Active Board-Certified (11,350 by NPI) [gold-highlighted]
#
# All counts from R/ent_registry_counts.R::compute_ent_registry_counts().
# Output: publication_materials/figures/ent_data_integrity/
#         ent_strobe_abohns_chain_v2.png
#
# Usage: Rscript scripts/improve_ent_strobe.R
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(tibble)
  library(here)
  # Round-40 audit fix (2026-05-21): the script uses dplyr verbs (filter(),
  # %>%) at lines below but only happened to work because
  # `source(here::here("R", "ent_registry_counts.R"))` further down had
  # `library(dplyr)` as a side effect. Refactor the sourced file and this
  # script crashes silently with `filter()` resolving to `stats::filter`.
  # Load dplyr explicitly so the dependency is visible at the top of the
  # file where readers look.
  library(dplyr)
})

source(here::here("R", "ent_registry_counts.R"))
ct <- compute_ent_registry_counts()

output_dir <- here::here("publication_materials/figures/ent_data_integrity")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Local aliases (chain values from the shared function)
n0_certs              <- ct$chain$n_raw
n1_uniq               <- ct$chain$n_unique
n2_conus              <- ct$chain$n_conus
n3_active             <- ct$chain$n_active_rows
n4_unique_npi         <- ct$chain$n_board
drop_dedupe           <- ct$chain$drop_dedupe
drop_conus            <- ct$chain$drop_conus
drop_retired          <- ct$chain$drop_retired
drop_npi_dup          <- ct$chain$drop_npi_dup
drop_cross            <- ct$chain$drop_cross
n_conus_drop_by_state <- ct$chain$n_conus_drop_by_state
z_triple              <- ct$zones$triple

# ── Palette ───────────────────────────────────────────────────────────────────
text_dark   <- "#1a2b3c"
text_dim    <- "#4a6880"
box_fill    <- "#eaf0f6"
box_edge    <- "#2c3e50"
excl_fill   <- "#fdecea"
excl_edge   <- "#c0392b"
triple_fill <- "#fff4cf"
triple_edge <- "#b7791f"
arrow_col   <- "#34495e"

.fmt <- function(x) {
  # Round-55 audit fix (2026-05-21): the previous one-liner was
  # `format(x, big.mark = ",")` which renders the literal text "NA" for
  # NA / Inf / NaN inputs. That string would land in a published STROBE
  # figure box (e.g., "n = NA") — same retraction-class disaster as the
  # rounds 36/38 fixes. Fail loud instead of publishing "NA" silently;
  # callers who genuinely expect missing counts should pre-fill 0L.
  if (length(x) == 0L) {
    stop("[.fmt] empty input would render no text into a CONSORT box",
         call. = FALSE)
  }
  if (any(is.na(x))) {
    stop(sprintf(
      "[.fmt] RETRACTION GUARD: %d NA value(s) would render as 'NA' in the STROBE figure.",
      sum(is.na(x))
    ), call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop(sprintf(
      "[.fmt] RETRACTION GUARD: %d non-finite value(s) (Inf/NaN) would render literally in the STROBE figure.",
      sum(!is.finite(x))
    ), call. = FALSE)
  }
  format(x, big.mark = ",")
}
.lookup_state <- function(df, code) {
  hit <- df$n[df$state_code == code]
  if (length(hit) == 0L) 0L else as.integer(hit[[1]])
}

# ── Geometry ──────────────────────────────────────────────────────────────────
# 5 chain stages, 4 exclusion boxes. Endpoint = Unique Active Physicians
# (by NPI). The Triple Match box was removed per stakeholder request;
# the registry-overlap diagram (ent_registry_overlap_euler_v2.png)
# carries that downstream step.
endpoint_fill <- "#fff4cf"
endpoint_edge <- "#b7791f"

chain <- tribble(
  ~id,       ~y,    ~label,
  "raw",     90,    sprintf("ABOHNS Raw Certifications\nn = %s", .fmt(n0_certs)),
  "uniq",    72,    sprintf("Unique Board-Certified Physicians\nn = %s", .fmt(n1_uniq)),
  "conus",   54,    sprintf("Physicians in the Contiguous United States\nn = %s", .fmt(n2_conus)),
  "active",  36,    sprintf("Active Practice (Contiguous US, NPI matched)\nn = %s", .fmt(n3_active)),
  "uniqnpi", 16,    sprintf("Unique Active Board-Certified Physicians (by NPI)\nn = %s", .fmt(n4_unique_npi))
)

excl <- tribble(
  ~id,           ~y,    ~label,
  "excl_dedupe", 81,    sprintf("Excluded (n = %s)\nDuplicate subspecialty certifications\n(Neurotology / Pediatric / Sleep)\nCollapsed to one row per physician", .fmt(drop_dedupe)),
  "excl_conus",  63,    sprintf("Excluded (n = %s)  outside the Contiguous US\n   Hawaii        n = %d\n   Alaska        n = %d\n   Puerto Rico   n = %d\nR/geographic_classification.R::NON_CONTIGUOUS_CODES",
                                .fmt(drop_conus),
                                .lookup_state(n_conus_drop_by_state, "HI"),
                                .lookup_state(n_conus_drop_by_state, "AK"),
                                .lookup_state(n_conus_drop_by_state, "PR")),
  "excl_retir",  45,    sprintf("Excluded (n = %s)\nLikely retired / inactive\n(no NPI match in NPPES;\nMedicare cessation signal)", .fmt(drop_retired)),
  "excl_npidup", 26,    sprintf("Excluded (n = %s)\nDuplicate NPI rows\n%d NPIs appear in 2+ ABOHNS rows\nCollapsed to unique physicians",
                                .fmt(drop_npi_dup),
                                ct$chain$n_npi_dups_distinct)
)

# Arrows
chain_arrows <- tribble(
  ~xs, ~ys,   ~xe, ~ye,
  30,  85,    30,  77,
  30,  67,    30,  59,
  30,  49,    30,  41,
  30,  31,    30,  22
)
excl_arrows <- tribble(
  ~xs, ~ys,   ~xe, ~ye,
  42,  90,    66,  81,
  42,  72,    66,  63,
  42,  54,    66,  45,
  42,  36,    66,  26
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot() +
  # Chain boxes (non-endpoint)
  geom_rect(data = chain %>% filter(id != "uniqnpi"),
            aes(xmin = 11, xmax = 49, ymin = y - 3.5, ymax = y + 3.5),
            fill = box_fill, color = box_edge, linewidth = 0.7) +
  geom_text(data = chain %>% filter(id != "uniqnpi"),
            aes(x = 30, y = y, label = label),
            color = text_dark, size = 9.5 / .pt, lineheight = 1.1) +

  # Endpoint (Unique Active Physicians) — highlighted
  geom_rect(data = chain %>% filter(id == "uniqnpi"),
            aes(xmin = 8, xmax = 52, ymin = y - 5, ymax = y + 5),
            fill = endpoint_fill, color = endpoint_edge, linewidth = 1.0) +
  geom_text(data = chain %>% filter(id == "uniqnpi"),
            aes(x = 30, y = y, label = label),
            color = text_dark, size = 10.5 / .pt, fontface = "bold", lineheight = 1.15) +

  # Exclusion boxes
  geom_rect(data = excl,
            aes(xmin = 66, xmax = 98, ymin = y - 5.5, ymax = y + 5.5),
            fill = excl_fill, color = excl_edge, linewidth = 0.5, linetype = "dashed") +
  geom_text(data = excl,
            aes(x = 82, y = y, label = label),
            color = excl_edge, size = 7.5 / .pt, lineheight = 1.15) +

  # Chain arrows
  geom_segment(data = chain_arrows,
               aes(x = xs, y = ys, xend = xe, yend = ye),
               color = arrow_col, linewidth = 0.9,
               arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +

  # Exclusion arrows
  geom_segment(data = excl_arrows,
               aes(x = xs, y = ys, xend = xe, yend = ye),
               color = excl_edge, linewidth = 0.4, linetype = "dashed",
               arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +

  coord_fixed(xlim = c(0, 100), ylim = c(10, 95), clip = "off") +
  theme_void() +
  labs(
    title    = "ENT Physician Cohort - STROBE Exclusion Flow",
    subtitle = paste0(
      "Starting from ABOHNS raw certifications (n = ", .fmt(n0_certs),
      ") and walking through every pre-specified exclusion to the\n",
      "unique active board-certified cohort (n = ", .fmt(n4_unique_npi),
      "). All chain counts and exclusion deltas computed LIVE from R/ent_registry_counts.R."
    ),
    caption  = paste0(
      "Geographic exclusion: R/geographic_classification.R::NON_CONTIGUOUS_CODES.  ",
      "Retirement signal: no NPI match in NPPES (R/retirement_filter_utilities.R logic).\n",
      sprintf("NPI dedup: %d NPIs appear in 2+ ABOHNS rows (different abohns_id) - real data-quality flag.  ",
              ct$chain$n_npi_dups_distinct),
      "Cross-registry overlap with NPPES 207Y* + ENThealth shown separately in ent_registry_overlap_euler_v2.png."
    )
  ) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = text_dark, hjust = 0,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.5, color = text_dim, hjust = 0,
                                 margin = margin(b = 14), lineheight = 1.25),
    plot.caption  = element_text(size = 8, color = text_dim, hjust = 0,
                                 margin = margin(t = 14), lineheight = 1.4),
    plot.margin   = margin(14, 16, 14, 16)
  )

out_path <- file.path(output_dir, "ent_cohort_strobe_v2.png")
ggsave(out_path, p, width = 14, height = 12, dpi = 300, bg = "white")
cat("Saved:", out_path, "\n\n")

# Reproducibility receipt
source(here::here("R", "figure_receipt.R"))
write_figure_receipt(
  figure_path = out_path,
  input_paths = ct$input_paths,
  live_counts = ent_receipt_payload(ct),
  script_path = "scripts/improve_ent_strobe.R"
)

# ── Console ledger ────────────────────────────────────────────────────────────
cat("STROBE exclusion ledger (live):\n")
cat(sprintf("  %-50s n = %s\n", "ABOHNS raw certifications",          .fmt(n0_certs)))
cat(sprintf("    - %-46s (n = %s)\n", "duplicate subspecialty certs",  .fmt(drop_dedupe)))
cat(sprintf("  %-50s n = %s\n", "Unique board-certified physicians",   .fmt(n1_uniq)))
cat(sprintf("    - %-46s (n = %s)\n", "non-CONUS (HI / AK / PR)",      .fmt(drop_conus)))
for (i in seq_len(nrow(n_conus_drop_by_state))) {
  cat(sprintf("        %-44s (n = %s)\n",
              n_conus_drop_by_state$state_code[i],
              .fmt(n_conus_drop_by_state$n[i])))
}
cat(sprintf("  %-50s n = %s\n", "CONUS physicians",                    .fmt(n2_conus)))
cat(sprintf("    - %-46s (n = %s)\n", "likely retired / inactive",     .fmt(drop_retired)))
cat(sprintf("  %-50s n = %s\n", "Active practice (CONUS rows)",        .fmt(n3_active)))
cat(sprintf("    - %-46s (n = %s)\n", "duplicate NPI rows collapsed",  .fmt(drop_npi_dup)))
cat(sprintf("  %-50s n = %s\n", "Unique active physicians by NPI",     .fmt(n4_unique_npi)))
