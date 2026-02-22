

# load akdes
load("./data/movement/akdes.rda")

# read in shapefile
hr_shp <- st_read(dsn = './data/movement/UD/shp/merged_95_HR_shp/')



#..............................................
# get habitat values ----

library(terra)
library(corrplot)
library(sf)

# extract covariate mean values
# load spatial covariate data (packedspatraster or spatraster object)


dem <- rast( "./data/environment/dem_50m_z10.tif")
esc_distance <- rast("./data/environment/slope50degree_distance_50m.tif")

# check crs and resolution (should be bc albers and 50m)
dem
esc_distance

# dont need to center and scale if not running analyses

# Combine rasters
rstack = c(dem, esc_distance)

#extract all raster values as mean values
r_extract = extract(rstack, hr_shp, fun = 'mean', bind = TRUE, na.rm = TRUE)

# convert extracted mean covariate values into a dataframe
covariate_results <- as.data.frame(r_extract)
head(covariate_results)

#rename columns
names(covariate_results)[1] <- "individual.local.identifier"
names(covariate_results)[2] <- "mean_elev"
names(covariate_results)[3] <- "mean_esc_dist"



#____________________________________________________________________
# 7) corrplot ----


# import movement results
load("./results/movement_results.rda")


# add covariate extracted mean values to hr results df
dat_corr <- cbind(movement_results, covariate_results)

#subset data to be used in corrplot()
dat_corr <- dat_corr[,c("hr_est_km2", 
                        "mean_elev", "mean_esc_dist")]

# rename for visualisation
names(dat_corr) <- c("Mean Home Range (km^2)",
                     "Mean Elevation", "Mean Escape Terrain Distance")
# Use corrplot() to determine variables to include in full model
corrplot(cor(dat_corr), method = 'number')

#save plot
png(file="./figures/corrplot.png",
    width=6.86, height=6, units="in", res=600)
corrplot(cor(dat_corr), method = 'number')
dev.off()

save(dat_corr, file = "./data/corrplot_data.rda")
