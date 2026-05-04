
# import environmental data


library(terra)


# import spatial covariate rasters 
elev <- rast("./data/environment/elev_25m.tif")

dist_to_slope50 <- rast("./data/environment/dist_to_slope50_25m.tif")

# dist_to_fire <- rast("./data/environment/dist_to_fire_25m.tif")
dist_to_fire <- rast("./data/environment/dist_to_fire_25m_combined_ext.tif")

# center and scale raster data via terra pkg -> makes covariates comparable now, if mapping dont need to
elev_scaled <- terra::scale(elev, center=TRUE, scale=TRUE)
names(elev_scaled) <- "elev_scaled_25m"
varnames(elev_scaled) <- "elev_scaled_25m"
# log the distance raster, biological importance, distance of 1m is more important than distance of 100m
# logging outside of the model, or run into issues with trying to use gratia draw
log_dist_to_slope50 <- log(dist_to_slope50 + 1)
names(log_dist_to_slope50) <- "log_dist_to_slope50_25m"
varnames(log_dist_to_slope50) <- "log_dist_to_slope50_25m"

log_dist_to_fire <- log(dist_to_fire + 1)
varnames(log_dist_to_fire) <- "log_dist_to_fire_25m"


# rm(dist_to_slope50, dist_to_fire)
