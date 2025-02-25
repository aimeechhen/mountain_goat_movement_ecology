
# rsf

library(ctmm)
library(raster)
library(tictoc)
library(beepr)


load("data/collar_data/collar_data_20241123.rda")
load("data/home_range/goat_akdes_20241217.rda")

#format names to match required for ctmm based on Movebank critera:
dat = plyr::rename(collar_data, c('goat_name' = 'individual.local.identifier',
                                  'latitude' = 'location.lat', 
                                  'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(dat, mark.rm = TRUE)

# load spatial covariate rasters 
elev_25m = raster('data/rasters/elev_25m.tif')
dist_escape_25m = raster('data/rasters/dist_escape_25m.tif')
# Some rasters are not loaded in RAM and may be slow to process. See help("raster::readAll").

# create a list of rasters
r_list <- list(elevation = elev_25m, 
          dist_escape = dist_escape_25m)

# initialize empty list for storing
rsf <- list()

# Fit RSF models (needs to be raster object and not spatraster)
START <- Sys.time()
tic()

for(i in 1:length(tel_data)){
  cat("Currently on animal", i, " of", length(tel_data))
  #Extract individual
  DATA <- tel_data[[i]]
  AKDE <- AKDES[[i]]
  
  # Fit rsf
  rsf[[i]] <- rsf.fit(DATA,AKDE, R=r_list)
}
names(rsf) <- names(tel_data)

toc() 
beep(3)


save(rsf, file = "./data/rsf/rsf_20241226.rda")
load("./data/rsf/rsf_20241226.rda")