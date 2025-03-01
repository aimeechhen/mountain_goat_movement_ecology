
# home range estimation

library(ctmm)
library(tictoc)
library(beepr)

# Import collar data as a ctmm telemetry object
source('./scripts/source/import_data_as_ctmm_telemetry_object.R')

# load movement models
load("data/movement_model/fits_20250301.rda")


# Estimate akdes home-range areas
START_hr <- Sys.time()
tic(msg = "home range analysis")

AKDES <- akde(tel_data,FITS,weights=TRUE)

toc() # 14.2 mins
END_hr <- Sys.time()
beep(8)
kittyR::meowR(sound = 2)

# ~10.86min, 14.2 mins


#save rda:
dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)
# save(AKDES,file="data/home_range/goat_akdes_20241217.rda")
# load("data/home_range/goat_akdes_20241217.rda")
save(AKDES,file="data/home_range/akdes_20250301.rda")
load("data/home_range/akdes_20250301.rda")




#......................................................................
# Create akde UDs as rasters and shapefiles ----
#......................................................................
# Fit_Mods.R

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#save UD as raster:
# dir.create("data/home_range/UD", recursive = TRUE)
dir.create("data/home_range/combined_data_fire_period_all_years/UD", recursive = TRUE, showWarnings = TRUE)

#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/combined_data_fire_period_all_years/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}




#save 95% range estimate UD as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
# dir.create("data/home_range/shp", recursive = TRUE)
dir.create("data/home_range/combined_data_fire_period_all_years/shp", recursive = TRUE, showWarnings = TRUE)

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/combined_data_fire_period_all_years/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp.path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }


