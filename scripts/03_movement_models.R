
# movement models and home range estimation

library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)

# load("data/collar_data/collar_data_20241123.rda")
load("data/collar_data/new_collar_data_20250218.rda")
new_collar$timestamp <- as.POSIXct(new_collar$timestamp, origin = "1970-01-01", tz = "America/Vancouver")

#format names to match required for ctmm based on Movebank critera:
# dat = plyr::rename(collar_data, c('goat_name' = 'individual.local.identifier',
#                                   'latitude' = 'location.lat', 
#                                   'longitude' = 'location.long'))

# #format names to match required for ctmm based on Movebank critera:
# dat = plyr::rename(new_collar, c('goat_name' = 'individual.local.identifier',
#                                   'latitude' = 'location.lat', 
#                                   'longitude' = 'location.long'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import combined collar data
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

#format names to match required for ctmm based on Movebank critera:
# create a column 
goat_data$individual.local.identifier <- paste(goat_data$collar_id, goat_data$year, sep = "_")
# format names to match
goat_data <- plyr::rename(goat_data, c('latitude' = 'location.lat', 
                                       'longitude' = 'location.long'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(new_collar, mark.rm = TRUE)

# summary of the gps data
summary(tel_data)

# visualisation of the data
plot(tel_data)



#...........................................................
# Movement models ----
#...........................................................

#create an empty list to store output
FITS <- list()

START <- Sys.time()
tic(msg = "movement models analysis")

for(i in 1:length(tel_data)){
  message(bgWhite(cyan("Currently on animal ", i, " of ", length(tel_data))))
  #Extract individual
  DATA <- tel_data[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process
  # fit movement models and select best fit
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)
  # beep(8)
  
}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h, #9.698333 mins; full = ~12.5min
END <- Sys.time()
kittyR::meowR(sound = 3)


dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)

# save(FITS,file="data/movement_model/goat_fits_20241107.rda")
# load("data/movement_model/goat_fits_20241107.rda")
# save(FITS,file="data/movement_model/fire_goat_fits_20241217.rda")
# load("data/movement_model/fire_goat_fits_20241224.rda")
# save(FITS,file="data/movement_model/full_fire_goat_fits_20250219.rda")
# load("data/movement_model/full_fire_goat_fits_20250219.rda")
# save(FITS,file="data/movement_model/fits_20250225.rda")
load("data/movement_model/fits_20250225.rda")




#...........................................................
# Speed analysis ----
#...........................................................


SPEED_MEAN <- list()
SPEEDS_INSTA <- list()

START <- Sys.time()
tic(msg = "speed analyses")

for(i in 1:length(tel_data)){
  message("Currently on animal ", i, " of ", length(tel_data))
  #Extract individual telemetry and movement model
  DATA <- tel_data[[i]]
  FIT <- FITS[[i]]
  
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true)
  SPEED_MEAN[[i]] <- speed(object = DATA, CTMM = FIT,
                           robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  # estimate instantaneous speed -> Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value
  SPEEDS_INSTA[[i]] <- speeds(object = DATA, CTMM = FIT,
                              robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  
  
}

#rename for convenience
names(SPEED_MEAN) <- names(tel_data)
names(SPEEDS_INSTA) <- names(tel_data)

toc() #13.87517 mins, ~26 min
END <- Sys.time()
# beep(8)
kittyR::meowR(sound = 3)

# 
# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.


save(SPEED_MEAN, file = "./data/movement_model/speed_mean_20250225.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/speeds_insta_20250225.rda")
load(file = "./data/movement_model/speed_mean_20250225.rda")
load(file = "./data/movement_model/speeds_insta_20250225.rda")





#......................................................................
# Create akde UDs as rasters and shapefiles ----
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
  UD_file <- file.path("data/home_range/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}


#save 95% range estimate UD as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
# dir.create("data/home_range/shp", recursive = TRUE)
dir.create("data/home_range/combined_data_fire_period_all_years/shp", recursive = TRUE, showWarnings = TRUE)

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp.path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }

