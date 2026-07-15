#!/usr/bin/env Rscript
# =============================================================================
# 5 km-tolerant isochrone REUSE layer -- "never rebuild an isochrone that already
# exists within 5 km." A drive-time polygon is essentially unchanged if the
# origin moves <=5 km (negligible vs a 30-200 km travel radius), so an existing
# isochrone within 5 km (great-circle / Haversine) is a valid stand-in.
#
# Builds a lightweight coordinate INDEX (lat, lon, band, source) from every
# isochrone store the project already has, plus our ENT outputs, and exposes:
#
#   iso_reuse_index()                    -> build/refresh the persistent index
#   iso_plan(points, bands, tol_m=5000)  -> per point x band: reuse (with source
#                                           + distance) or build
#
# The index is small (coords only); geometry is fetched from the named source
# file when a reused isochrone is actually materialized.
#
# Output: model_output/isochrones/isochrone_reuse_index.rds
# =============================================================================

suppressPackageStartupMessages({library(sf); library(dplyr)})
sf::sf_use_s2(TRUE)
ISO_PROJ <- "/Users/tylermuffly/isochrones"
GRACE    <- "/Users/tylermuffly/grace-ent"
outdir   <- file.path(GRACE, "model_output", "isochrones")
index_f  <- file.path(outdir, "isochrone_reuse_index.rds")
TOL_M    <- 5000

# ---- build the reuse index from all known isochrone stores ------------------
# Each source declares how to pull (lat, lon, band) from its rows.
iso_reuse_index <- function() {
  sources <- list(
    list(file = file.path(ISO_PROJ, "data/derived/provider_isochrones.rds"),
         lat = "latitude", lon = "longitude", band = "drive_time_minutes"),
    list(file = file.path(outdir, "ent_isochrones.rds"),
         lat = NA, lon = NA, band = "band_min")   # ENT: coords come from lookup
  )
  ent_coords <- read.csv(file.path(GRACE, "data/processed/ent_unique_locations.csv"))
  idx <- list()
  for (s in sources) {
    if (!file.exists(s$file)) next
    d <- readRDS(s$file); dd <- sf::st_drop_geometry(d)
    if (is.na(s$lat)) {                       # ENT rds: join coords by loc_id
      m <- merge(dd[, c("loc_id", s$band)], ent_coords, by.x = "loc_id", by.y = "id")
      idx[[s$file]] <- data.frame(lat = m$lat, lon = m$long, band = m[[s$band]],
                                  source = basename(s$file), stringsAsFactors = FALSE)
    } else {
      idx[[s$file]] <- data.frame(lat = dd[[s$lat]], lon = dd[[s$lon]],
                                  band = dd[[s$band]], source = basename(s$file),
                                  stringsAsFactors = FALSE)
    }
  }
  index <- dplyr::bind_rows(idx)
  index <- index[is.finite(index$lat) & is.finite(index$lon) & !is.na(index$band), ]
  saveRDS(index, index_f)
  cat(sprintf("Reuse index: %d isochrones across %d sources, bands %s\n",
              nrow(index), length(unique(index$source)),
              paste(sort(unique(index$band)), collapse = "/")))
  invisible(index)
}

# ---- plan a set of points: reuse-within-5km vs build ------------------------
iso_plan <- function(points, bands = c(30, 60, 120, 180), tol_m = TOL_M) {
  index <- if (file.exists(index_f)) readRDS(index_f) else iso_reuse_index()
  pts <- sf::st_as_sf(points, coords = c("long", "lat"), crs = 4326, remove = FALSE)
  out <- list()
  for (b in bands) {
    lib <- index[index$band == b, ]
    if (!nrow(lib)) { out[[as.character(b)]] <-
      data.frame(id = points$id, band = b, action = "build", source = NA, dist_m = NA); next }
    lib_sf <- sf::st_as_sf(lib, coords = c("lon", "lat"), crs = 4326)
    near <- sf::st_nearest_feature(pts, lib_sf)
    dist <- as.numeric(sf::st_distance(pts, lib_sf[near, ], by_element = TRUE))
    reuse <- dist <= tol_m
    out[[as.character(b)]] <- data.frame(
      id = points$id, band = b,
      action = ifelse(reuse, "reuse", "build"),
      source = ifelse(reuse, lib$source[near], NA),
      dist_m = round(dist))
  }
  dplyr::bind_rows(out)
}

# ---- materialize reused geometry (pull the actual polygon from the source) --
# Returns an sf of reused isochrones for the point x band pairs that have an
# existing isochrone within tol_m, tagged loc_id/band_min/reused/source/dist_m.
# Loads source geometries lazily and caches them in .reuse_geom_cache.
.reuse_geom_cache <- new.env(parent = emptyenv())
.reuse_lib <- function() {
  if (!is.null(.reuse_geom_cache$lib)) return(.reuse_geom_cache$lib)
  ent_coords <- read.csv(file.path(GRACE, "data/processed/ent_unique_locations.csv"))
  parts <- list()
  # sources may store geometry in different CRS (provider=4326, our ENT=9311);
  # normalize each to 4326 before combining.
  f1 <- file.path(ISO_PROJ, "data/derived/provider_isochrones.rds")
  if (file.exists(f1)) {
    d <- sf::st_transform(readRDS(f1), 4326)
    parts[[f1]] <- sf::st_sf(lat = d$latitude, lon = d$longitude,
                             band = d$drive_time_minutes, source = basename(f1),
                             geometry = sf::st_geometry(d))
  }
  f2 <- file.path(outdir, "ent_isochrones.rds")
  if (file.exists(f2)) {
    d <- sf::st_transform(readRDS(f2), 4326); dd <- sf::st_drop_geometry(d)
    co <- ent_coords[match(dd$loc_id, ent_coords$id), c("lat", "long")]
    parts[[f2]] <- sf::st_sf(lat = co$lat, lon = co$long, band = dd$band_min,
                             source = basename(f2), geometry = sf::st_geometry(d))
  }
  lib <- do.call(rbind, parts)
  lib <- lib[is.finite(lib$lat) & is.finite(lib$lon) & !is.na(lib$band), ]
  .reuse_geom_cache$lib <- lib
  .reuse_geom_cache$lib
}

iso_materialize_reused <- function(points, bands = c(30, 60, 120, 180), tol_m = TOL_M) {
  lib <- .reuse_lib()
  pts <- sf::st_as_sf(points, coords = c("long", "lat"), crs = 4326, remove = FALSE)
  out <- list()
  for (b in bands) {
    libb <- lib[lib$band == b, ]
    if (!nrow(libb)) next
    # Match on the ORIGIN coordinate (not the polygon geometry): build a point
    # layer from each isochrone's origin lat/lon, find the nearest origin per
    # target, keep it only if within tol_m, then pull that origin's polygon.
    libb_pts <- sf::st_as_sf(data.frame(x = libb$lon, y = libb$lat),
                             coords = c("x", "y"), crs = 4326)
    near <- sf::st_nearest_feature(pts, libb_pts)
    dist <- as.numeric(sf::st_distance(pts, libb_pts[near, ], by_element = TRUE))
    keep <- which(dist <= tol_m)
    if (!length(keep)) next
    g <- libb[near[keep], ]                 # the polygon for the matched origin
    g$loc_id <- points$id[keep]; g$band_min <- b
    g$reused <- TRUE; g$dist_m <- round(dist[keep])
    g$lat <- g$lon <- g$band <- NULL
    out[[as.character(b)]] <- g
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# ---- demonstrate on the 555 ENT locations -----------------------------------
if (sys.nframe() == 0) {
  iso_reuse_index()
  ent <- read.csv(file.path(GRACE, "data/processed/ent_unique_locations.csv"))
  # plan against the STORE ONLY (what we should have checked before building):
  idx_all <- readRDS(index_f)
  saveRDS(idx_all[idx_all$source != "ent_isochrones.rds", ], index_f)  # store-only view
  plan_store <- iso_plan(ent)
  n_pb <- table(plan_store$action)
  cat(sprintf("\nAgainst the pre-existing store only (per point x band, %d total):\n", nrow(plan_store)))
  print(n_pb)
  loc_reuse <- tapply(plan_store$action, plan_store$id, function(a) any(a == "reuse"))
  cat(sprintf("ENT locations with >=1 reusable band within 5 km: %d of %d (%.1f%%)\n",
              sum(loc_reuse), length(loc_reuse), 100*mean(loc_reuse)))
  iso_reuse_index()   # restore full index (now includes our ENT outputs)
  plan_full <- iso_plan(ent)
  cat(sprintf("\nAfter caching our own 555 outputs, a re-run needs %d builds (0 expected).\n",
              sum(plan_full$action == "build")))
  cat("Reuse index saved:", index_f, "\n")
}
