
# rsf

# rsf = relative probability of use of a resource (i.e., habitat covariates/parameters) vs the availability of that resource in the environment/habitat

# https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_rsf.R

library(ctmm)
library(raster)
library(tictoc)
library(beepr)
library(crayon)
library(terra)


# import data
load("./data/goat/study_period_tel_data.rda") # ctmm 6 goats study period only
load("./data/movement/akdes.rda")

# import spatial covariate rasters 
elev <- rast("./data/environment/dem_50m_z10.tif")
esc_dist <- rast("./data/environment/slope50degree_distance_50m.tif")
# Some rasters are not loaded in RAM and may be slow to process. See help("raster::readAll").

# check crs and resolution (should be bc albers and 50m)
elev
esc_dist

# reproject so its in lat/long because tracking data is lat/long
elev <- project(elev, "epsg:4326")
esc_dist <- project(esc_dist, "epsg:4326")

# center and scale raster data via terra pkg -> makes covariates comparable now, if mapping dont need to
elev_scaled <- terra::scale(elev, center=TRUE, scale=TRUE)
esc_dist_scaled <- terra::scale(esc_dist, center=TRUE, scale=TRUE)

# convert from spatraster to raster object needed for ctmm rsf, ctmm only can only accept raster objects not spatrasters atm
elev_scaled <- raster(elev_scaled)
esc_dist_scaled <- raster(esc_dist_scaled)


# create a list of rasters (raster covariates must be in a named list)
r_list <- list(elev_scaled = elev_scaled,
               esc_dist_scaled = esc_dist_scaled)

rm(elev, esc_dist)

# #test for a single goat
# i <- 1
# DATA <- tel_data[[i]]
# akdes <- AKDES[[i]]
# RSF <- rsf.fit(DATA,akdes,R=r_list)

#____________________________________________________________________
# 8) iRSF ----

#******CONFIRM IF THIS IS TRUE******
# fitting via rsf.fit because i don't need model selection, wanting to use the full model (every covariate in the r_list)
# rsf.select = rsf.fit + model selection (covariates can change in the model)
#********

# initialize empty list for storing
RSF <- list()

# Fit iRSF models

START_rsf <- Sys.time()
tic(msg = "rsf analysis")

for(i in 1:length(tel_data)){
  tic(msg = "single loop time")
  cat(bgBlue("Currently on animal", i, " of", length(tel_data)), "\n")
  #Extract individual
  DATA <- tel_data[[i]]
  akdes <- AKDES[[i]]
  
  # Fit rsf
  RSF[[i]] <- rsf.fit(DATA, UD = akdes, R=r_list, trace = TRUE)
  
  toc() # ~10.5, 16, 41 min each
}
names(RSF) <- names(tel_data)

toc() # ~12.55 hrs, combined = 8.8h, final 6.5h
beep(3)

END_rsf <- Sys.time()


# repeat at using different resolution -> sensitivity analysis of raster resolution
# does the outcome change/differ? possible ecological implications/meaning
# like signs of coefficents, effect size grow/shrink, ci, high selected area changes


# dir.create("./data/habitat_use/rsf", recursive = TRUE, showWarnings = TRUE)


# save(RSF, file = "./data/habitat_use/rsf/rsf_25m.rda")
save(RSF, file = "./data/habitat_use/rsf/rsf.rda") # 50m raster
load("./data/habitat_use/rsf/rsf.rda")

