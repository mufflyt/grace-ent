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

# ---- S1: Kaplan-Meier time-to-appointment -----------------------------------
# Event = appointment secured; offices that never offered are censored at the
# 90-day horizon; offered waits are clamped to 90 for readability.
HORIZON <- 90
km <- d
km$sub4 <- ifelse(km$ent_type %in% c("General","Pediatrics","Laryngology"),
                  as.character(km$ent_type), "Other subspecialties")
km$sub4 <- factor(km$sub4, levels = c("General","Pediatrics","Laryngology","Other subspecialties"))
km$time  <- ifelse(km$offered == 1 & !is.na(km$wait_days), pmin(km$wait_days, HORIZON), HORIZON)
km$event <- ifelse(km$offered == 1 & !is.na(km$wait_days) & km$wait_days <= HORIZON, 1L, 0L)
km_res <- tryCatch(mysterycall_kaplan_meier(
  km, time_col = "time", event_col = "event", group_col = "sub4",
  max_days = HORIZON, plot = TRUE, risk_table = TRUE,
  plot_title = "Time to secured appointment, by subspecialty"),
  error = function(e) {cat("S1 KM err:", conditionMessage(e), "\n"); NULL})
if (!is.null(km_res)) {
  p1 <- if (!is.null(km_res$plot)) km_res$plot else km_res
  ggsave(file.path(supp, "figS1_km_time_to_appointment.png"), p1, width = 9, height = 7, dpi = 150)
  cat("wrote figS1_km_time_to_appointment.png\n")
}

# ---- S2: wait-time distribution faceted by subspecialty ---------------------
d2 <- d[which(d$offered == 1 & !is.na(d$wait_days)), ]
fh <- tryCatch(mysterycall_facet_histogram(
  d2, x_col = "wait_days", facet_col = "ent_type", binwidth = 7,
  x_label = "Business days to appointment", title = "Wait-time distribution by subspecialty",
  output_dir = supp, filename = "figS2_wait_distribution.png"),
  error = function(e) {cat("S2 hist err:", conditionMessage(e), "\n"); NULL})
if (!is.null(fh)) cat("wrote figS2_wait_distribution.png\n")

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
mk <- function(fill, lab, pal) ggplot(us, aes(long, lat, group = group, fill = .data[[fill]])) +
  geom_polygon(color = "white", linewidth = 0.15) +
  coord_map("albers", 25, 50) + pal +
  labs(title = lab, fill = "") + theme_void(base_size = 11) +
  theme(legend.position = "right")
g_off <- mk("offer_rate", "Appointment offer rate by AAO-HNS region",
            scale_fill_viridis_c(option = "C", labels = scales::percent, na.value = "grey90"))
g_wt  <- mk("median_wait", "Median business-day wait by AAO-HNS region",
            scale_fill_viridis_c(option = "D", direction = -1, na.value = "grey90"))
ggsave(file.path(supp, "figS4_choropleth_offer_rate.png"), g_off, width = 8, height = 5, dpi = 150)
ggsave(file.path(supp, "figS4_choropleth_median_wait.png"), g_wt,  width = 8, height = 5, dpi = 150)
cat("wrote figS4_choropleth_offer_rate.png + figS4_choropleth_median_wait.png\n")

cat("\nSupplementary figures in", supp, ":\n"); print(grep("^fig", list.files(supp), value = TRUE))
