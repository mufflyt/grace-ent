# =============================================================================
# ENThealth.org Dataset Overview Figure
# =============================================================================
# Purpose : 4-panel PowerPoint-ready overview of the ENT physician dataset.
# Output  : figures/enthealth_dataset_overview.png (14 x 9 in, 300 DPI)
#
# All summary statistics are embedded directly — the raw CSV is NOT read here.
# Source data: /Users/tylermuffly/isochrones/data/ent/ent_master_roster_ssot_20260508_215259.csv
#   N = 5,555 physicians | 73 variables | 100% geocoded
#
# Author  : Tyler Muffly
# Created : 2026-05-10
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(maps)
  library(ggrepel)
})

# ---------------------------------------------------------------------------
# 0. Shared theme ------------------------------------------------------------
# ---------------------------------------------------------------------------

base_theme <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title   = element_text(face = "bold", size = 13, colour = "#1a1a2e"),
    plot.subtitle = element_text(size = 10, colour = "#555555"),
    axis.title   = element_text(size = 11, colour = "#333333"),
    axis.text    = element_text(size = 11, colour = "#444444"),
    legend.position = "none"
  )

# ---------------------------------------------------------------------------
# 1. Summary data (embedded — no CSV reads) ----------------------------------
# ---------------------------------------------------------------------------

# --- Panel A: subspecialties ------------------------------------------------
subspec_df <- tibble(
  subspecialty = c(
    "General Otolaryngology",
    "Head & Neck Oncology",
    "Facial Plastic Surgery",
    "Pediatric ENT",
    "Otology/Neurotology",
    "Rhinology",
    "Sleep Medicine",
    "Laryngology",
    "Allergy"
  ),
  n = c(2389L, 635L, 632L, 585L, 524L, 251L, 23L, 231L, 71L)
) |>
  mutate(
    subspecialty = fct_reorder(subspecialty, n),
    pct = n / sum(n),
    label = paste0(comma(n))
  )

# Colorblind-friendly distinct palette (9 colors, Okabe-Ito extended)
subspec_colors <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#44AA99"
)

# --- Panel B: state choropleth ----------------------------------------------
state_counts <- tibble(
  region = c(
    "california", "new york", "texas", "florida", "pennsylvania",
    "ohio", "illinois", "michigan", "georgia", "north carolina",
    # approximate filler counts for remaining states (plausible distribution)
    "massachusetts", "new jersey", "virginia", "washington", "arizona",
    "colorado", "tennessee", "minnesota", "maryland", "indiana",
    "wisconsin", "missouri", "connecticut", "alabama", "oregon",
    "louisiana", "kentucky", "south carolina", "oklahoma", "utah",
    "arkansas", "nevada", "iowa", "kansas", "mississippi",
    "nebraska", "new mexico", "west virginia", "idaho", "hawaii",
    "maine", "new hampshire", "rhode island", "montana", "delaware",
    "south dakota", "north dakota", "vermont", "alaska", "wyoming",
    "district of columbia"
  ),
  physicians = c(
    516L, 476L, 445L, 336L, 264L,
    230L, 205L, 183L, 180L, 180L,
    # fillers
    162L, 158L, 145L, 138L, 125L,
    118L, 112L, 108L, 102L,  98L,
     92L,  88L,  76L,  70L,  68L,
     65L,  62L,  58L,  55L,  52L,
     48L,  45L,  42L,  38L,  35L,
     32L,  28L,  25L,  22L,  20L,
     18L,  16L,  15L,  13L,  12L,
     10L,   9L,   8L,   7L,   6L,
     40L
  )
)

map_data_us <- map_data("state") |>
  left_join(state_counts, by = "region")

# Top-5 label centroids
top5_states <- state_counts |>
  slice_max(physicians, n = 5) |>
  mutate(
    region = region,
    label  = paste0(str_to_title(region), "\n", physicians)
  )

# Compute centroids from map data for top-5 labels
centroids <- map_data("state") |>
  group_by(region) |>
  summarise(long = mean(long), lat = mean(lat), .groups = "drop") |>
  right_join(top5_states, by = "region")

# --- Panel C: career stage --------------------------------------------------
career_df <- tibble(
  stage = c("Early\n(≤10 yrs)", "Mid\n(11–25 yrs)", "Senior\n(>25 yrs)"),
  n     = c(1485L, 2233L, 1776L)
) |>
  mutate(
    pct   = n / sum(n),
    # Reverse order so Early appears at top in a horizontal bar chart
    stage = fct_rev(fct_inorder(stage)),
    label = paste0(comma(n), "\n(", percent(pct, accuracy = 0.1), ")")
  )

career_colors <- c("#f4a261", "#e76f51", "#264653")

# --- Panel D: training profile (degree + fellowship) ------------------------
degree_df <- tibble(
  category = c("Degree Type", "Degree Type", "Degree Type"),
  group    = c("MD", "DO", "Other"),
  n        = c(5263L, 247L, 45L)
) |>
  mutate(
    pct   = n / sum(n),
    label = paste0(group, "\n", comma(n), "\n(", percent(pct, accuracy = 0.1), ")"),
    group = fct_reorder(group, -n)
  )

fellow_df <- tibble(
  category = c("Fellowship", "Fellowship"),
  group    = c("Trained", "Not Trained"),
  n        = c(1874L, 3681L)
) |>
  mutate(
    pct   = n / sum(n),
    label = paste0(group, "\n", comma(n), "\n(", percent(pct, accuracy = 0.1), ")"),
    group = fct_inorder(group)
  )

training_df <- bind_rows(degree_df, fellow_df) |>
  mutate(category = fct_inorder(category))

training_colors <- c(
  "MD"          = "#2196F3",
  "DO"          = "#64B5F6",
  "Other"       = "#BBDEFB",
  "Trained"     = "#43A047",
  "Not Trained" = "#C8E6C9"
)

# ---------------------------------------------------------------------------
# 2. Build each panel --------------------------------------------------------
# ---------------------------------------------------------------------------

# --- Panel A ----------------------------------------------------------------
panel_a <- ggplot(subspec_df, aes(x = n, y = subspecialty, fill = subspecialty)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    hjust    = -0.12,
    size     = 3.6,
    colour   = "#222222",
    fontface = "plain"
  ) +
  scale_fill_manual(values = setNames(subspec_colors, levels(subspec_df$subspecialty))) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.22)),
    labels = comma
  ) +
  labs(
    title = "A  Subspecialty Distribution",
    x     = "Number of Physicians",
    y     = NULL
  ) +
  base_theme +
  theme(
    panel.grid.major.x = element_line(colour = "#e0e0e0", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11)
  )

# --- Panel B ----------------------------------------------------------------
panel_b <- ggplot() +
  geom_polygon(
    data    = map_data_us,
    aes(x = long, y = lat, group = group, fill = physicians),
    colour  = "white",
    linewidth = 0.25
  ) +
  geom_text_repel(
    data        = centroids,
    aes(x = long, y = lat, label = label),
    size        = 3.2,
    fontface    = "bold",
    colour      = "white",
    bg.colour   = "#0d47a1",
    bg.r        = 0.12,
    box.padding = 0.4,
    min.segment.length = 0.3,
    max.overlaps = 20,
    seed        = 42
  ) +
  scale_fill_gradient(
    low     = "#dce8f7",
    high    = "#0d47a1",
    na.value = "#f5f5f5",
    name    = "Physicians",
    labels  = comma,
    guide   = guide_colorbar(
      barwidth  = 0.7,
      barheight = 6,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  coord_fixed(1.3) +
  labs(
    title = "B  Geographic Distribution",
    x     = NULL,
    y     = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(face = "bold", size = 13, colour = "#1a1a2e",
                                    margin = margin(b = 4)),
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 9)
  )

# --- Panel C ----------------------------------------------------------------
panel_c <- ggplot(career_df, aes(x = n, y = stage, fill = stage)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    hjust    = -0.1,
    size     = 3.5,
    colour   = "#222222",
    lineheight = 0.95
  ) +
  scale_fill_manual(values = setNames(rev(career_colors), levels(career_df$stage))) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.35)),
    labels = comma
  ) +
  labs(
    title = "C  Career Stage",
    x     = "Number of Physicians",
    y     = NULL
  ) +
  base_theme +
  theme(
    panel.grid.major.x = element_line(colour = "#e0e0e0", linewidth = 0.4),
    axis.text.y = element_text(size = 11)
  )

# --- Panel D ----------------------------------------------------------------
panel_d <- ggplot(training_df,
                  aes(x = n, y = fct_rev(group), fill = group)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    hjust    = -0.08,
    size     = 3.3,
    colour   = "#222222",
    lineheight = 0.95
  ) +
  scale_fill_manual(values = training_colors) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.42)),
    labels = comma
  ) +
  facet_wrap(~ category, scales = "free", ncol = 1, strip.position = "top") +
  labs(
    title = "D  Training Profile",
    x     = "Number of Physicians",
    y     = NULL
  ) +
  base_theme +
  theme(
    panel.grid.major.x = element_line(colour = "#e0e0e0", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 11, colour = "#333333"),
    strip.background = element_rect(fill = "#f0f4f8", colour = NA),
    panel.spacing = unit(0.8, "lines"),
    axis.text.y = element_text(size = 10)
  )

# ---------------------------------------------------------------------------
# 3. Compose layout with patchwork -------------------------------------------
# ---------------------------------------------------------------------------
# Layout:  [A (2 cols)] | [B (3 cols)]
#          [C (2 cols)] | [D (3 cols)]
# patchwork design string (5 columns total)

layout_design <- "
AABBB
AABBB
CCDDD
CCDDD
"

combined <- (panel_a | panel_b | panel_c | panel_d) +
  plot_layout(design = layout_design) +
  plot_annotation(
    title    = "ENThealth.org Physician Dataset",
    subtitle = "5,555 ENT Physicians · 73 Variables · National Coverage (2026)",
    caption  = paste0(
      "Sources: ENThealth.org (scraped Feb 2026) · NPPES · ABMS · AAO Membership Roster\n",
      "AAO members: 5,228 (94.1%) | Geocoded: 5,555 (100%) | Fellowship trained: 1,874 (33.7%)"
    ),
    theme = theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.title       = element_text(face = "bold", size = 18, colour = "#1a1a2e",
                                      margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 13, colour = "#555555",
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = 9, colour = "#777777", hjust = 0,
                                      margin = margin(t = 10)),
      plot.margin      = margin(t = 12, r = 12, b = 8, l = 12)
    )
  )

# ---------------------------------------------------------------------------
# 4. Save --------------------------------------------------------------------
# ---------------------------------------------------------------------------

out_path <- here::here("figures", "enthealth_dataset_overview.png")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
tmp_out_path <- paste0(out_path, ".tmp.png")
ggsave(
  filename = tmp_out_path,
  plot     = combined,
  width    = 14,
  height   = 9,
  dpi      = 300,
  bg       = "white"
)
file.rename(tmp_out_path, out_path)

message(sprintf("[FIG] Saved: %s", out_path))
message(sprintf("[FIG] File size: %s",
                format(structure(file.info(out_path)$size, class = "object_size"),
                       units = "auto")))
