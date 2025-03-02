
# rsf

library(ctmm)
library(raster)
library(tictoc)
library(beepr)
library(crayon)


# load("data/collar_data/collar_data_20241123.rda")
# load("data/home_range/goat_akdes_20241217.rda")

# Import data as ctmm telemetry object ----
source('./scripts/source/import_data_as_ctmm_telemetry_object.R')

# import AKDES
load("data/home_range/akdes_20250301.rda")

# import spatial covariate rasters 
elev = raster('data/rasters/elev_25m.tif')
dist_escape = raster('data/rasters/dist_escape_25m.tif')
# Some rasters are not loaded in RAM and may be slow to process. See help("raster::readAll").

# create a list of rasters
r_list <- list(elev = elev, 
               dist_escape = dist_escape)

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

toc() # ~12.55 hrs
beep(3)
# kittyR::meowR(sound = 3)
END_rsf <- Sys.time()

dir.create("./data/rsf/", recursive = TRUE, showWarnings = TRUE)
# save(rsf, file = "./data/rsf/fire_goat_rsf_20241220.rda")
# load("./data/rsf/fire_goat_rsf_20241220.rda")
# save(rsf, file = "./data/rsf/rsf_20241226.rda")
# load("./data/rsf/rsf_20241226.rda")
# load("./data/rsf/full_fire_goat_rsf_20250220.rda")
save(RSF, file = "./data/rsf/rsf_20250301.rda")
load("./data/rsf/rsf_20250301.rda")



