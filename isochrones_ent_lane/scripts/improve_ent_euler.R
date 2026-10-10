#!/usr/bin/env Rscript
# =============================================================================
# ENT Registry Overlap — Area-Proportional Euler Diagram
# =============================================================================
#
# Produces a three-set Euler diagram (NPPES, ABOto Board, ENThealth) using
# eulerr::euler(shape = "ellipse"). All seven zones labeled with physician
# counts. Headline: "Patients can find only 40% of practicing ENT physicians
# through ENThealth."
#
# All counts from R/ent_registry_counts.R::compute_ent_registry_counts().
# Output: publication_materials/figures/ent_data_integrity/
#         ent_registry_overlap_euler_v2.png
#
# Usage: Rscript scripts/improve_ent_euler.R
# =============================================================================

suppressPackageStartupMessages({
  library(eulerr)
  library(grid)
  library(here)
})

source(here::here("R", "ent_registry_counts.R"))
# physician_only = TRUE excludes PA/NP/audiology NPIs that self-coded the
# 207Y* taxonomy. Keeps the figure aligned with Table 1 (also physicians-only).
ct <- compute_ent_registry_counts(physician_only = TRUE)

output_dir <- here::here("publication_materials/figures/ent_data_integrity")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Local aliases for readability
n_nppes        <- ct$sets$n_npi_univ
n_board        <- ct$sets$n_board
n_ent          <- ct$sets$n_enthealth
n_ent_orphan   <- ct$n_ent_orphan
z_nppes_only   <- ct$zones$nppes_only
z_board_only   <- ct$zones$board_only
z_ent_only     <- ct$zones$ent_only
z_nppes_board  <- ct$zones$nppes_board
z_nppes_ent    <- ct$zones$nppes_ent
z_board_ent    <- ct$zones$board_ent
z_triple       <- ct$zones$triple

cat("Euler counts (from R/ent_registry_counts.R):\n")
cat(sprintf("  NPPES 207Y* (CONUS, unique NPI) : %s\n", format(n_nppes, big.mark = ",")))
cat(sprintf("  Board Certified (active)        : %s\n", format(n_board, big.mark = ",")))
cat(sprintf("  ENThealth (unique physicians)   : %s  (incl. %d with no NPI)\n",
            format(n_ent, big.mark = ","), n_ent_orphan))
cat(sprintf("  Triple Match                    : %s\n\n", format(z_triple, big.mark = ",")))

# ── Fit Euler ─────────────────────────────────────────────────────────────────
zones <- c(
  "NPI"                       = z_nppes_only,
  "Board"                     = z_board_only,
  "ENThealth"                 = z_ent_only,
  "NPI&Board"                 = z_nppes_board,
  "NPI&ENThealth"             = z_nppes_ent,
  "Board&ENThealth"           = z_board_ent,
  "NPI&Board&ENThealth"       = z_triple
)

fit <- eulerr::euler(zones, shape = "ellipse")

# Palette
npi_fill   <- "#2ec4b6"
board_fill <- "#3a86ff"
acad_fill  <- "#ff6b6b"
text_dark  <- "#1a2b3c"
text_dim   <- "#4a6880"

.fmt <- function(x) format(x, big.mark = ",")
.lab <- function(label, n) if (n == 0L) "" else sprintf("%s\n%s", label, .fmt(n))

# Per-zone count labels rendered by eulerr (in same order as zones vector).
# Empty zones (n = 0) get blank labels so eulerr does not draw them.
zone_quantities <- c(
  .lab("NPI Only",                     z_nppes_only),  # NPI
  .lab("Board (CONUS)\nNPPES non-CONUS only", z_board_only),  # Board
  .lab("ENThealth Only\n(no NPI)",     z_ent_only),    # ENThealth
  .lab("AAO-HNS Board & NPI",          z_nppes_board), # NPI&Board
  .lab("ENThealth & NPI",              z_nppes_ent),   # NPI&ENThealth
  .lab("Board & ENThealth\n(no NPPES)", z_board_ent),  # Board&ENThealth
  .lab("Triple Match",                 z_triple)       # NPI&Board&ENThealth
)

euler_plot <- plot(
  fit,
  fills = list(
    fill  = c(npi_fill, board_fill, acad_fill),
    alpha = c(0.28, 0.38, 0.45)
  ),
  edges = list(col = c(npi_fill, board_fill, acad_fill), lwd = 2.2),
  labels = FALSE,                       # set names drawn manually as legend
  quantities = list(
    labels   = zone_quantities,
    fontsize = 11,
    fontface = "bold",
    col      = text_dark
  ),
  legend = FALSE                        # we build our own legend below
)

# Manual legend: colored swatches + set name + total — uses live values
legend_df <- data.frame(
  label = c(
    sprintf("NPPES 207Y* Universe   n = %s",  .fmt(n_nppes)),
    sprintf("Board Certified (active)   n = %s", .fmt(n_board)),
    sprintf("ENThealth   n = %s",            .fmt(n_ent))
  ),
  col   = c(npi_fill, board_fill, acad_fill)
)

# Footnote (plain grid.text — gridtext not available)
footnote_lines <- c(
  paste0(
    "NPPES taxonomy codes (207Y* family):  ",
    "207Y00000X General Otolaryngology  \u2022  207YX0007X Head & Neck Surgery  \u2022  ",
    "207YX0905X Neurotology  \u2022  207YP0228X Pediatric Otolaryngology"
  ),
  paste0(
    "207YX0901X Allergy  \u2022  207YX0602X Facial Plastic & Reconstructive Surgery  \u2022  ",
    "207YS0123X (current) / 207YS0012X (legacy) Sleep Medicine  \u2022  ",
    "207YX0400X Rhinology (no self-designations in 2024 NPPES)"
  ),
  paste0(
    "Data sources:  ",
    "NPPES NPI Registry  https://npiregistry.cms.hhs.gov  \u2022  ",
    "ABOto  https://www.aboto.org  \u2022  ",
    "AAO-HNS ENThealth  https://www.enthealth.org/find-an-ent"
  )
)

# Compose page
out_path <- file.path(output_dir, "ent_registry_overlap_euler_v2.png")
png(out_path, width = 16, height = 11, units = "in", res = 300, bg = "white")

grid.newpage()
pushViewport(viewport(layout = grid.layout(
  nrow    = 4, ncol = 1,
  heights = unit(c(0.55, 0.45, 8.6, 1.4), "in")
)))

# Title — patient-discoverability framing
ent_in_board   <- ct$zones$nppes_ent + ct$zones$board_ent + ct$zones$triple +
                  (n_board - ct$zones$nppes_board - ct$zones$triple - ct$zones$board_ent)
findable_pct   <- 100 * z_triple / n_board     # board-cert physicians also on ENThealth
not_findable_n <- n_board - z_triple

pushViewport(viewport(layout.pos.row = 1))
grid.text(
  sprintf("Patients can find only %.0f%% of practicing ENT physicians through ENThealth",
          findable_pct),
  y  = 0.4,
  gp = gpar(fontsize = 19, fontface = "bold", col = text_dark)
)
popViewport()

# Subtitle (live)
subtitle_text <- sprintf(
  "%s of %s active board-certified ENT physicians are listed in the AAO-HNS public directory. The remaining %s appear in NPPES but cannot be located through ENThealth.",
  .fmt(z_triple),
  .fmt(n_board),
  .fmt(not_findable_n)
)
pushViewport(viewport(layout.pos.row = 2))
grid.text(
  subtitle_text,
  y  = 0.5,
  gp = gpar(fontsize = 11, col = text_dim, fontface = "italic")
)
popViewport()

# Euler + custom legend overlay
pushViewport(viewport(layout.pos.row = 3))
print(euler_plot, newpage = FALSE)

# Manual legend (top-left of the euler row): square swatch + label
legend_x   <- unit(0.04, "npc")
legend_y0  <- unit(0.96, "npc")
swatch_sz  <- unit(0.018, "npc")
line_h     <- unit(0.045, "npc")

for (i in seq_len(nrow(legend_df))) {
  y <- unit(0.96, "npc") - (i - 1) * line_h
  grid.rect(
    x = legend_x, y = y,
    width = swatch_sz, height = swatch_sz,
    just = c("left", "center"),
    gp = gpar(fill = legend_df$col[i], col = legend_df$col[i], alpha = 0.7)
  )
  grid.text(
    legend_df$label[i],
    x = legend_x + swatch_sz + unit(0.008, "npc"), y = y,
    just = c("left", "center"),
    gp = gpar(fontsize = 11, fontface = "bold", col = legend_df$col[i])
  )
}
popViewport()

# Footnote (one grid.text per line so we control line spacing precisely)
pushViewport(viewport(layout.pos.row = 4))
y_positions <- c(0.85, 0.60, 0.25)
for (i in seq_along(footnote_lines)) {
  grid.text(
    footnote_lines[i],
    x = unit(0.02, "npc"), y = unit(y_positions[i], "npc"),
    hjust = 0, vjust = 0.5,
    gp = gpar(fontsize = 8, col = text_dim, fontface = if (i == 3) "italic" else "plain")
  )
}
popViewport()

dev.off()

cat("Saved:", out_path, "\n")

# Reproducibility receipt
source(here::here("R", "figure_receipt.R"))
write_figure_receipt(
  figure_path = out_path,
  input_paths = ct$input_paths,
  live_counts = ent_receipt_payload(ct),
  script_path = "scripts/improve_ent_euler.R"
)
