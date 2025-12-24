

# load akdes
load("./scripts/working/data/home_range/akdes_20250301.rda")




#____________________________________________________________________
# Extract covariate values from 95% estimated home range shapefile ----
# 06a.load_merge_shapefiles_per_period.r

library(sf)
# 1. Import 95% home range estimate shapefiles
# Set folder path containing the exported subfolders
# folder_path <- "data/home_range/fire_goat/shp"
folder_path <- "./scripts/working/data/home_range/shp"
# Load .shp files from subfolders
shp_dir <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
#Read each shapefile into a list
shp_files <- lapply(shp_dir, st_read)
#rename files
names(shp_files) <- names(AKDES)




# 2. Merge all 95% HR estimate shapefiles into single sf object
# Combine all the shapefiles into a single sf object
hr_shp <- do.call(rbind, shp_files)
# Subset 95% est shapefiles only based on the text "95% est" in the name column
hr_shp <- hr_shp[grepl("95% est", hr_shp$name), ]
# project to bc albers crs
bc_albers <- "EPSG:3005"
hr_shp = st_transform(hr_shp, crs = st_crs(bc_albers))
st_crs(hr_shp)

# # Writing 29 features with 1 fields and geometry type Unknown (any).
# geometry_types <- st_geometry_type(hr_shp)
# table(geometry_types)
# # no unknown is showing up

# save merged shapefile sf object
st_write(hr_shp, dsn = './scripts/working/data/home_range/merged_95_HR_shp', 
         driver = 'ESRI Shapefile', append=FALSE)
# read in shapefile
hr_shp <- st_read(dsn = './scripts/working/data/home_range/merged_95_HR_shp/')





#..............................................
# get habitat values ----

library(terra)
library(corrplot)
library(sf)

# extract covariate mean values
# load spatial covariate data (packedspatraster or spatraster object)
elev_25m <- rast('./scripts/working/data/rasters/elev_25m.tif')
dist_escape_25m <- rast("./scripts/working/data/rasters/dist_escape_25m.tif")

#scale/center raster data via terra pkg
elev_25m_scaled <- scale(elev_25m)
dist_escape_25m_scaled <- scale(dist_escape_25m)

# Combine rasters
rstack = c(elev_25m, dist_escape_25m,
           elev_25m_scaled, dist_escape_25m)

#extract all raster values as mean values
r_extract = extract(rstack, hr_shp, fun = 'mean', bind = TRUE, na.rm = TRUE)

# convert extracted mean covariate values into a dataframe
covariate_results <- as.data.frame(r_extract)
head(covariate_results)

#rename columns
names(covariate_results)[1] <- "individual.local.identifier"
names(covariate_results)[2] <- "mean_elev_25m"
names(covariate_results)[3] <- "mean_dist_escape_25m"
names(covariate_results)[4] <- "mean_elev_25m_scaled"
names(covariate_results)[5] <- "mean_dist_escape_25m_scaled"



#____________________________________________________________________
# 7) corrplot ----


# import movement results
movement_results <- read.csv("./scripts/working/data/combined_data_movement_hr_results_20250505.csv")

# add covariate extracted mean values to hr results df
dat_corr <- cbind(movement_results, covariate_results)

#subset data to be used in corrplot()
dat_corr <- dat_corr[,c("hr_est_km2", 
                        "mean_elev_25m", "mean_dist_escape_25m",
                        "mean_elev_25m_scaled", "mean_dist_escape_25m_scaled")]

# rename for visualisation
names(dat_corr) <- c("Mean Home Range (km2)", 
                     "Mean Elevation", "Mean Dist. to Escape Terrain",
                     "Scaled Mean Elevation", "Scaled Mean Dist. to Escape Terrain")
# Use corrplot() to determine variables to include in full model
plot_corrplot <- corrplot(cor(dat_corr), method = 'number')

#save plot
png(file="./scripts/working/figures/corrplot.png",
    width=6.86, height=6, units="in", res=600)
plot_corrplot
dev.off()

save(dat_corr, file = "./scripts/working/data/corrplot_data.rda")
