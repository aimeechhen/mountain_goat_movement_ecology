
# rsf

# rsf = relative probability of use of a resource (i.e., habitat covariates/parameters) vs the availability of that resource in the environment/habitat

library(ctmm)
library(raster)
library(tictoc)
library(beepr)
library(crayon)
library(terra)


# Import combined collar data (original + cleaned new)
load("./scripts/working/data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# formatting
goat_data$timestamp <- as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date <- as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

#format names to match required for ctmm based on Movebank critera:
# create a column combining collar_id and year to avoid needing to subset by individuals and year, it will create a unique identifier based on the individual and the year of the data and then those will be grouped together as the data for each individual for each year
goat_data$individual.local.identifier <- paste(goat_data$collar_id, goat_data$year, sep = "_")
# format names to match
goat_data <- plyr::rename(goat_data, c('latitude' = 'location.lat', 
                                       'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(goat_data, mark.rm = TRUE)


#[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]


# import AKDES
load("./scripts/working/data/home_range/akdes_20250505.rda")

# import spatial covariate rasters 
elev_25m <- rast('./scripts/working/data/rasters/elev_25m.tif')
dist_escape_25m <- rast('./scripts/working/data/rasters/dist_escape_25m.tif')
# Some rasters are not loaded in RAM and may be slow to process. See help("raster::readAll").

# reproject so its in lat/long
elev_25m <- project(elev_25m, "epsg:4326")
dist_escape_25m <- project(dist_escape_25m, "epsg:4326")

# convert it into from spatraster to raster object needed for ctmm rsf
elev_25m <- raster(elev_25m)
dist_escape_25m <- raster(dist_escape_25m)

# #scale/center raster data via terra pkg
# elev_25m_scaled <- scale(elev_25m)
# dist_escape_25m_scaled <- scale(dist_escape_25m)

# create a list of rasters
r_list <- list(elev_25m = elev_25m, 
               dist_escape_25m  = dist_escape_25m)#,
# elev_25m_scaled = elev_25m_scaled,
# dist_escape_25m_scaled = dist_escape_25m_scaled)

# #test for a single goat
i <- 1
DATA <- tel_data[[i]]
akdes <- AKDES[[i]]
RSF <- rsf.fit(DATA,akdes,R=r_list)



#____________________________________________________________________
# 8) RSF ----

# initialize empty list for storing
RSF <- list()

# Fit RSF models (rasters needs to be raster object and not spatraster)
START_rsf <- Sys.time()
tic(msg = "rsf analysis")

for(i in 1:length(tel_data)){
  tic(msg = "single loop time")
  cat(bgBlue("Currently on animal", i, " of", length(tel_data)), "\n")
  #Extract individual
  DATA <- tel_data[[i]]
  akdes <- AKDES[[i]]
  
  # Fit rsf
  RSF[[i]] <- rsf.fit(DATA,akdes, R=r_list)
  
  toc() # ~10.5, 16, 41 min each
}
names(RSF) <- names(tel_data)

toc() # ~12.55 hrs, combined = 8.8h, final 6.5h
beep(3)
# kittyR::meowR(sound = 3)
END_rsf <- Sys.time()

dir.create("./data/rsf/", recursive = TRUE, showWarnings = TRUE)


save(RSF, file = "./data/rsf/rsf_20250902.rda")
# save(RSF, file = "./data/rsf/rsf_scaled_20250505.rda")
load("./scripts/working/data/rsf/rsf_20250902.rda")

