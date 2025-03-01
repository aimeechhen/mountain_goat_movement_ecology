
# movement models and home range estimation

library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)


# Import data as ctmm telemetry object ----
source('./scripts/source/import_data_as_ctmm_telemetry_object.R')

# summary of the gps data, (i.e., interval, period, long, lat info)
summary(tel_data) # mostly ~6.25h with a few at 5.5h interval

# visualisation of the data
plot(tel_data)



#...........................................................
# 1. Movement models ----
#...........................................................

#create an empty list to store output
FITS <- list()

START_movement <- Sys.time()
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
END_movement <- Sys.time()
kittyR::meowR(sound = 3)


dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)

# save(FITS,file="data/movement_model/goat_fits_20241107.rda")
# load("data/movement_model/goat_fits_20241107.rda")
# save(FITS,file="data/movement_model/fire_goat_fits_20241217.rda")
# load("data/movement_model/fire_goat_fits_20241224.rda")
# save(FITS,file="data/movement_model/full_fire_goat_fits_20250219.rda")
# load("data/movement_model/full_fire_goat_fits_20250219.rda")
save(FITS,file="data/movement_model/fits_20250301.rda")
load("data/movement_model/fits_20250301.rda")




#...........................................................
# 2. Speed analysis ----
#...........................................................


SPEED_MEAN <- list()
SPEEDS_INSTA <- list()

START_speed <- Sys.time()
tic(msg = "speed analyses")

for(i in 1:length(tel_data)){
  message("Currently on animal ", i, " of ", length(tel_data))
  #Extract individual telemetry and movement model
  DATA <- tel_data[[i]]
  fits <- FITS[[i]]
  
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true)
  SPEED_MEAN[[i]] <- speed(object = DATA, CTMM = fits,
                           robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  # estimate instantaneous speed -> Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value
  SPEEDS_INSTA[[i]] <- speeds(object = DATA, CTMM = fits,
                              robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  
  
}

#rename for convenience
names(SPEED_MEAN) <- names(tel_data)
names(SPEEDS_INSTA) <- names(tel_data)

toc() #13.87517 mins, ~26 min
END_speed <- Sys.time()
# beep(8)
kittyR::meowR(sound = 3)

# 
# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.


save(SPEED_MEAN, file = "./data/movement_model/speed_mean_20250301.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/speeds_insta_20250301.rda")
load(file = "./data/movement_model/speed_mean_20250301.rda")
load(file = "./data/movement_model/speeds_insta_20250301.rda")



