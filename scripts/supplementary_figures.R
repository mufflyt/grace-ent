#!/usr/bin/env Rscript
# =============================================================================
# Supplementary figures for the ENT access study.
#   S1  Kaplan-Meier time-to-appointment by subspecialty  (mysterycall_kaplan_meier)
#   S2  Wait-time distribution, faceted by subspecialty    (mysterycall_facet_histogram)
#   S3  Negative-binomial model diagnostics                (mysterycall_plot_residuals)
#   S4  Geographic choropleth: offer rate + median wait by AAO-HNS region
#       (ggplot2 + maps; mysterycall's map layer delegates to the Google-based
#        mysterymaps, so the choropleth is built directly from the state->region
#        crosswalk in data/raw/state_to_aao_hns_district.csv)
#
# Output: model_output/supp/*.png
# =============================================================================

suppressMessages(devtools::load_all("~/mysterycall", quiet = TRUE))
suppressPackageStartupMessages({library(ggplot2)})
options(stringsAsFactors = FALSE)
supp <- "model_output/supp"; dir.create(supp, showWarnings = FALSE, recursive = TRUE)
num <- function(x) suppressWarnings(as.numeric(x))

d <- read.csv("data/processed/ent_phase2_enriched.csv", colClasses = "character")
d <- d[d$complete == "Complete" & d$ent_type != "" & !is.na(d$ent_type), ]
d$offered   <- ifelse(d$appointment_offered == "TRUE", 1L, 0L)
d$wait_days <- num(d$wait_days_business)

# ---- S1: empirical cumulative probability of an appointment -----------------
# NOT a Kaplan-Meier / time-to-event analysis: this is a single simulated call,
# not longitudinal follow-up. We plot the empirical cumulative proportion of ALL
# calls in each subspecialty that had secured an appointment by business-day t,
# with no-offer calls retained as never obtaining one -- so each curve plateaus
# at the subspecialty's offer rate. No censoring assumption, no log-rank test.
HORIZON <- 90
km <- d
grp <- ifelse(km$ent_type %in% c("General","Pediatrics","Laryngology"),
              as.character(km$ent_type), "Other")
lab <- c(General = "General", Pediatrics = "Pediatric", Laryngology = "Laryngology",
         Other = "Other subspecialties")
km$Subspecialty <- factor(lab[grp],
  levels = c("General", "Pediatric", "Laryngology", "Other subspecialties"))
km$wait_e <- ifelse(km$offered == 1 & !is.na(km$wait_days), pmin(km$wait_days, HORIZON), NA_real_)
pal <- c("General" = "#1b9e77", "Pediatric" = "#d95f02",
         "Laryngology" = "#7570b3", "Other subspecialties" = "#386cb0")
days <- 0:HORIZON
cum <- do.call(rbind, lapply(levels(km$Subspecialty), function(g) {
  sg <- km[km$Subspecialty == g, ]; n <- nrow(sg)
  data.frame(Subspecialty = g, day = days,
             p = vapply(days, function(t) sum(!is.na(sg$wait_e) & sg$wait_e <= t) / n, numeric(1)))
}))
cum$Subspecialty <- factor(cum$Subspecialty, levels = levels(km$Subspecialty))
p1 <- ggplot(cum, aes(day, p, colour = Subspecialty)) +
  geom_step(linewidth = 0.9) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.04))) +
  scale_x_continuous(breaks = seq(0, HORIZON, 15)) +
  labs(x = "Business days since call",
       y = "Cumulative proportion of calls with an appointment",
       colour = "Subspecialty",
       title = "Cumulative probability of securing an appointment, by subspecialty",
       subtitle = "Empirical cumulative proportion of all calls; no-offer calls never obtain an appointment, so each curve plateaus at the subspecialty's offer rate") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey40", size = 9, margin = margin(b = 6)),
        legend.position = c(0.99, 0.02), legend.justification = c(1, 0),
        legend.background = element_rect(fill = scales::alpha("white", 0.8), colour = NA),
        panel.grid.minor = element_blank())
ggsave(file.path(supp, "figS1_km_time_to_appointment.png"), p1, width = 9, height = 6, dpi = 150)
cat("wrote figS1_km_time_to_appointment.png (empirical cumulative)\n")

# ---- S2: wait-time distribution faceted by subspecialty ---------------------
# Rebuilt with independent y-axes: a shared scale flattens every panel except
# general otolaryngology (which dominates the counts).
d2 <- d[which(d$offered == 1 & !is.na(d$wait_days)), ]
sub_lab <- c(General = "General otolaryngology", "Facial Plastics" = "Facial plastic surgery",
  "Head and Neck Cancer" = "Head and neck oncology", Pediatrics = "Pediatric otolaryngology",
  Sleep = "Sleep medicine", Laryngology = "Laryngology",
  "Otology/neurotology" = "Otology/neurotology", Rhinology = "Rhinology")
d2$sub <- ifelse(d2$ent_type %in% names(sub_lab), sub_lab[d2$ent_type], d2$ent_type)
ord <- names(sort(tapply(d2$wait_days, d2$sub, median, na.rm = TRUE)))
d2$sub <- factor(d2$sub, levels = ord)
meds <- aggregate(wait_days ~ sub, d2, median)
p2 <- ggplot(d2, aes(wait_days)) +
  geom_histogram(binwidth = 7, boundary = 0, fill = "#4C72B0",
                 colour = "white", linewidth = 0.2) +
  geom_vline(data = meds, aes(xintercept = wait_days), colour = "#b2182b",
             linetype = "dashed", linewidth = 0.5) +
  geom_text(data = meds, aes(x = Inf, y = Inf, label = sprintf("median %d d", round(wait_days))),
            hjust = 1.08, vjust = 1.5, size = 2.7, colour = "#b2182b") +
  facet_wrap(~ sub, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 150, 30)) +
  labs(x = "Business days to appointment", y = "Number of appointments",
       title = "Wait-time distribution by subspecialty",
       subtitle = "Independent y-axes; dashed line marks each subspecialty's median wait") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold", size = 9.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 9.5, margin = margin(b = 6)),
    panel.spacing    = grid::unit(0.9, "lines"))
ggsave(file.path(supp, "figS2_wait_distribution.png"), p2, width = 9, height = 6, dpi = 200)
cat("wrote figS2_wait_distribution.png\n")

# ---- S3: NB model diagnostics -----------------------------------------------
preds <- c("ent_type","rural","caller_f")
d2$rural <- relevel(factor(d2$ruca_category), ref = "Urban")
ct <- table(d2$caller); d2$caller_f <- relevel(factor(ifelse(d2$caller %in% names(ct)[ct<10],"Other",d2$caller)),
                                                ref = names(sort(ct,decreasing=TRUE))[1])
clust <- d2$cbsa_code; clust[!nzchar(clust)] <- d2$county_fips[!nzchar(clust)]
clust[!nzchar(clust)] <- paste0("solo_", which(!nzchar(clust))); d2$cluster <- clust
wait <- mysterycall_nb_model(d2, "wait_days", preds, "cluster")
png(file.path(supp, "figS3_nb_diagnostics.png"), width = 1100, height = 550, res = 120)
dg <- tryCatch(mysterycall_plot_residuals(wait$model, use_dharma = TRUE, plot = TRUE),
               error = function(e) {cat("S3 diag err:", conditionMessage(e), "\n"); NULL})
dev.off()
cat("wrote figS3_nb_diagnostics.png\n")

# ---- S4: geographic choropleth ----------------------------------------------
xw <- read.csv("data/raw/state_to_aao_hns_district.csv")
reg <- setNames(xw$Region, xw$State_Abbr)
d$region <- reg[d$state]
agg <- do.call(rbind, lapply(split(d, d$region), function(g) data.frame(
  region = g$region[1],
  offer_rate = mean(g$offered == 1, na.rm = TRUE),
  median_wait = median(g$wait_days[g$offered == 1], na.rm = TRUE))))
# map states -> region metric
st_region <- setNames(xw$Region, tolower(xw$State))
us <- ggplot2::map_data("state")
us$region_aao <- st_region[us$region]
us <- merge(us, agg, by.x = "region_aao", by.y = "region", all.x = TRUE)
us <- us[order(us$order), ]
# one label per AAO-HNS BoG region, placed at the region's centroid
cent <- aggregate(cbind(long, lat) ~ region_aao, data = us[!is.na(us$region_aao), ], FUN = mean)
# nudge the small New England label up/right into open space off the coast
ne <- cent$region_aao == "New England"
cent$long[ne] <- cent$long[ne] + 6
cent$lat[ne]  <- cent$lat[ne]  + 2
mk <- function(fill, lab, pal, legend_title) {
  ggplot(us, aes(long, lat, group = group, fill = .data[[fill]])) +
    geom_polygon(colour = "white", linewidth = 0.25) +
    # white-pill labels stay legible over any fill (incl. the dark NE region)
    geom_label(data = cent, aes(long, lat, label = region_aao), inherit.aes = FALSE,
               size = 2.6, fontface = "bold", colour = "grey20",
               fill = scales::alpha("white", 0.72), label.size = 0,
               label.padding = grid::unit(0.09, "lines")) +
    coord_map("albers", 25, 50) + pal +
    labs(title = lab,
         subtitle = "States shaded by the value for their AAO-HNS Board of Governors region",
         fill = legend_title) +
    theme_void(base_size = 12) +
    theme(
      plot.title        = element_text(face = "bold", size = 15),
      plot.subtitle     = element_text(colour = "grey40", size = 9.5,
                                        margin = margin(t = 2, b = 8)),
      legend.position   = "bottom",
      legend.title      = element_text(size = 9, face = "bold"),
      legend.text       = element_text(size = 8),
      legend.key.width  = grid::unit(2.4, "lines"),
      legend.key.height = grid::unit(0.45, "lines"),
      plot.margin       = margin(8, 8, 8, 8))
}
gcb <- guide_colourbar(title.position = "top", title.hjust = 0.5, ticks.colour = "white")
g_off <- mk("offer_rate", "Appointment offer rate by AAO-HNS region",
            scale_fill_viridis_c(option = "C", labels = scales::percent,
                                 na.value = "grey92", guide = gcb), "Offer rate")
g_wt  <- mk("median_wait", "Median business-day wait by AAO-HNS region",
            scale_fill_viridis_c(option = "D", direction = -1, na.value = "grey92",
                                 labels = function(x) paste0(x, " d"), guide = gcb),
            "Median wait")
ggsave(file.path(supp, "figS4_choropleth_offer_rate.png"), g_off, width = 8.5, height = 5.6, dpi = 200)
ggsave(file.path(supp, "figS4_choropleth_median_wait.png"), g_wt,  width = 8.5, height = 5.6, dpi = 200)
cat("wrote figS4_choropleth_offer_rate.png + figS4_choropleth_median_wait.png\n")

cat("\nSupplementary figures in", supp, ":\n"); print(grep("^fig", list.files(supp), value = TRUE))
