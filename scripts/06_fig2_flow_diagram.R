# =============================================================================
# 06_fig2_flow_diagram.R
# CONSORT-style study flow diagram for database accuracy manuscript
# Output: output/figures/fig2_study_flow.png
# =============================================================================

library(here)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

out_dir <- here("output", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Counts (from ent_phase2_all call log, n = 960)
# NPPES source population from Euler zone demographics (n = 15,048)
# -----------------------------------------------------------------------------
# Source population
n_nppes_source    <- 15048   # All in NPPES 207Y* taxonomy extract
n_nppes_ent_zones <- 15004   # Restricted to NPPES-containing Euler zones

# Current called cohort (overlapping-registry strata)
n_sampled         <- 960     # Sampled from NPPES+ABOto / NPPES+ENTHealth / Triple zones
n_no_phone        <- 0       # Excluded before sampling (already filtered)

# Call dispositions
n_attempted       <- 752     # office_answered TRUE or FALSE (not NA)
n_not_attempted   <- 208     # listing_status == "not_attempted" (no disposition)

n_reached         <- 577     # office_answered == TRUE
n_not_reached     <- 175     # office_answered == FALSE (busy/no answer/voicemail)

# Listing status (mutually exclusive, exhaustive)
n_confirmed_valid   <- 445   # physician confirmed at listed NPPES location
n_confirmed_invalid <- 138   # relocated, retired, wrong/disconnected number
n_unresolved        <- 169   # practice reached but physician status unclear

# Among confirmed valid
n_new_patients      <- 434   # taking_new_patients == "Yes"

# -----------------------------------------------------------------------------
# Build DOT diagram manually for full control of layout
# -----------------------------------------------------------------------------
dot <- sprintf('
digraph study_flow {
  graph [layout=dot, rankdir=TB, fontname=Helvetica, fontsize=11,
         splines=ortho, nodesep=0.5, ranksep=0.6]
  node [fontname=Helvetica, fontsize=10, shape=box, width=3.8, style=filled,
        fillcolor=white, color="#333333"]
  edge [fontname=Helvetica, fontsize=9, color="#333333"]

  // ---- Main column ---------------------------------------------------
  A [label="NPPES otolaryngology listings\\n(207Y* taxonomy)\\nn = %s"]
  B [label="Listings in NPPES-containing\\nEuler zones\\nn = %s"]
  C [label="Sampled for mystery-caller study\\n(NPPES+ABOto, NPPES+ENTHealth,\\nTriple-match strata)\\nn = %s"]
  D [label="Call attempted\\nn = %s"]
  E [label="Practice reached\\n(office answered)\\nn = %s"]
  F [label="Physician confirmed\\nat listed NPPES location\\nn = %s"]
  G [label="Accepting new patients\\nn = %s"]

  A -> B -> C -> D -> E -> F -> G

  // ---- Exclusion side boxes ------------------------------------------
  node [shape=box, style=dashed, width=3.0, fillcolor="#F8F8F8"]

  excAB [label="44 in NPPES-only zone\\n(calls pending)"]
  excCD [label="208 not attempted:\\nno call disposition\\nrecorded"]
  excDE [label="175 not reached after\\n3+ attempts:\\nno answer / busy /\\nvoicemail"]
  excEF [label="132 confirmed invalid:\\n  Relocated (n = 89)\\n  Retired / inactive (n = 31)\\n  Wrong or disconnected\\n  number (n = 12)\\n\\n169 unresolved status\\n(practice reached; physician\\nstatus unclear)"]
  excFG [label="11 not accepting:\\npractice at capacity\\nor on leave"]

  // dashed arrows from main nodes to side boxes
  edge [style=dashed, constraint=false]
  B  -> excAB
  C  -> excCD
  D  -> excDE
  E  -> excEF
  F  -> excFG

  // force side boxes to same rank as their source node
  { rank=same; B;  excAB }
  { rank=same; C;  excCD }
  { rank=same; D;  excDE }
  { rank=same; E;  excEF }
  { rank=same; F;  excFG }
}
',
  format(n_nppes_source,    big.mark = ","),
  format(n_nppes_ent_zones, big.mark = ","),
  format(n_sampled,         big.mark = ","),
  format(n_attempted,       big.mark = ","),
  format(n_reached,         big.mark = ","),
  format(n_confirmed_valid, big.mark = ","),
  format(n_new_patients,    big.mark = ",")
)

# -----------------------------------------------------------------------------
# Render and export to PNG
# -----------------------------------------------------------------------------
svg_text <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot))

out_path <- file.path(out_dir, "fig2_study_flow.png")
rsvg::rsvg_png(svg = charToRaw(svg_text), file = out_path, width = 2400)

message("Saved: ", out_path)
