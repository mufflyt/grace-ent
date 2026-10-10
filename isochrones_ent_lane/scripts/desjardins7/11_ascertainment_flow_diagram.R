#!/usr/bin/env Rscript
# =============================================================================
# desjardins7/11_ascertainment_flow_diagram.R
# CONSORT-style workforce-ascertainment flow (Supplement Figure S1), from the
# ABOG certification universe to the physicians active, located, and included in
# the 2020 accessibility surface. Counts are surface-independent through the
# "located" box; surface inclusion is finalized from the frozen run.
# =============================================================================
suppressWarnings(suppressMessages({ library(ggplot2) }))
ROOT <- if (requireNamespace("here", quietly=TRUE)) here::here() else normalizePath(".")
OUTD <- file.path(ROOT, "manuscript", "desjardins7_figures"); dir.create(OUTD, showWarnings=FALSE, recursive=TRUE)
fl <- read.csv(file.path(ROOT,"artifacts","desjardins7","workforce_flow_recovered.csv"))
S  <- function(k) sum(fl[[k]])

box <- function(x, y, w, h, label, fill="#eef2f7") list(
  geom = data.frame(xmin=x-w/2, xmax=x+w/2, ymin=y-h/2, ymax=y+h/2, fill=fill),
  lab  = data.frame(x=x, y=y, label=label))

boxes <- list(
  main1 = list(x=3, y=9,  label=sprintf("ABOG board-certified in 1 of 7 fields and active\nat any point 2013-2023 (with a practice record)\nn = %s", format(S("universe_2013_2023"), big.mark=","))),
  main2 = list(x=3, y=7,  label=sprintf("Active in 2020\n(not retired: retirement_year > 2020)\nn = %s", format(S("active_2020"), big.mark=","))),
  main3 = list(x=3, y=5,  label=sprintf("Located to a 2020 practice tract\n(initial geocode %s + recovery %s)\nn = %s", format(S("geocoded_2020"),big.mark=","), format(S("recovered"),big.mark=","), format(S("geocoded_recovered"), big.mark=","))),
  main4 = list(x=3, y=3,  label=sprintf("Included in the 2020 accessibility surface\n(located physicians with a travel-time isochrone)\nfinalized from the frozen run")))
excl <- list(
  e1 = list(x=7.4, y=8, label=sprintf("Excluded: not active in 2020\n(retired, died, or no 2020 signal)\nn = %s", format(S("universe_2013_2023")-S("active_2020"), big.mark=","))),
  e2 = list(x=7.4, y=6, label=sprintf("Excluded: no mappable location\neven after recovery\nn = %s", format(S("ungeocoded_after"), big.mark=","))))

W <- 4.6; H <- 1.35; EW <- 3.6; EH <- 1.2
rects <- do.call(rbind, c(
  lapply(boxes, function(b) data.frame(xmin=b$x-W/2, xmax=b$x+W/2, ymin=b$y-H/2, ymax=b$y+H/2, fill="#e8eef6")),
  lapply(excl,  function(b) data.frame(xmin=b$x-EW/2, xmax=b$x+EW/2, ymin=b$y-EH/2, ymax=b$y+EH/2, fill="#f6ecec"))))
labs <- do.call(rbind, c(
  lapply(boxes, function(b) data.frame(x=b$x, y=b$y, label=b$label, sz=3.2)),
  lapply(excl,  function(b) data.frame(x=b$x, y=b$y, label=b$label, sz=2.8))))

arrows_v <- data.frame(x=3, xend=3, y=c(9,7,5)-H/2, yend=c(7,5,3)+H/2)
arrows_h <- data.frame(x=3, xend=c(7.4,7.4)-EW/2, y=c(8,6), yend=c(8,6))

p <- ggplot() +
  geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill), colour="grey40", linewidth=0.3) +
  scale_fill_identity() +
  geom_segment(data=arrows_v, aes(x=x,xend=xend,y=y,yend=yend), arrow=arrow(length=unit(6,"pt"),type="closed"), linewidth=0.4, colour="grey30") +
  geom_segment(data=arrows_h, aes(x=x,xend=xend,y=y,yend=yend), arrow=arrow(length=unit(5,"pt"),type="closed"), linewidth=0.35, colour="grey45") +
  geom_text(data=labs, aes(x=x, y=y, label=label, size=sz), lineheight=0.95) +
  scale_size_identity() +
  coord_cartesian(xlim=c(0.4,9.4), ylim=c(2,10)) +
  labs(title="Figure S1. Workforce ascertainment flow, 2020") +
  theme_void(base_size=12) +
  theme(plot.title=element_text(face="bold", size=rel(1.0), hjust=0), plot.title.position="plot",
        plot.margin=margin(8,8,8,8))
ggsave(file.path(OUTD,"figS1_ascertainment_flow.png"), p, width=9, height=6.2, dpi=200, bg="white")
cat("wrote figS1_ascertainment_flow.png\n")
