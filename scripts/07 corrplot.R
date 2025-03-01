




#____________________________________________________________________
# Extract covariate values from 95% estimated home range shapefile ----
# 06a.load_merge_shapefiles_per_period.r

library(sf)
# 1. Import 95% home range estimate shapefiles
# Set folder path containing the exported subfolders
folder_path <- "data/home_range/fire_goat/shp"
# Load .shp files from subfolders
shp_dir <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
#Read each shapefile into a list
shp_files <- lapply(shp_dir, st_read)
#rename files
names(shp_files) <- names(AKDES)




# 2. Merge all 95% HR estimate shapefiles into single sf object
# Combine all the shapefiles into a single sf object
dat_shp <- do.call(rbind, shp_files)
# Subset 95% est shapefiles only based on the text "95% est" in the name column
dat_shp <- dat_shp[grepl("95% est", dat_shp$name), ]
# project to bc albers crs
bc_albers <- "EPSG:3005"
dat_shp = st_transform(dat_shp, crs = st_crs(bc_albers))
st_crs(dat_shp)

# # Writing 29 features with 1 fields and geometry type Unknown (any).
# geometry_types <- st_geometry_type(dat_shp)
# table(geometry_types)
# # no unknown is showing up

# save merged shapefile sf object
st_write(dat_shp, dsn = 'data/home_range/fire_goat/merged_95_HR_shp', 
         driver = 'ESRI Shapefile', append=FALSE)
# read in shapefile
dat_shp = st_read(dsn = 'data/home_range/fire_goat/merged_95_HR_shp')









#____________________________________________________________________
# 7) corrplot ----


library(terra)
library(corrplot)
library(sf)

load("./data/home_range/fire_goat_covariate_results.rda")
load("./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")


# add covariate extracted mean values to hr results df
dat_corr <- cbind(fg_results, covariate_results)

#subset data to be used in corrplot()
dat_corr <- dat_corr[,c("mean_hr_est_km2", 
                        "mean_elev_25m", "mean_dist_escape_25m",
                        "mean_scl_elev", "mean_scl_dis_escape")]

# rename for visualisation
names(dat_corr) <- c("Mean Home Range (km2)", 
                     "Mean Elevation", "Mean Dist. to Escape Terrain",
                     "Scaled Mean Elevation", "Scaled Mean Dist. to Escape Terrain")
# Use corrplot() to determine variables to include in full model
plot_corrplot <- corrplot(cor(dat_corr), method = 'number')

#save plot
png(file="figures/fire_goat_corrplot.png",
    width=6.86, height=6, units="in", res=600)
plot_corrplot
dev.off()


