
# movement models and home range estimation

library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)

#...........................................................
# Import data ----
#...........................................................

# Import combined collar data (original + cleaned new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
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
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=TRUE),interactive=FALSE) # Error is off for now to speed up the process, error = true will help avoid iid models and push towards ou and ouf models
  # fit movement models and select best fit
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)

  
}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h, #9.698333 mins; full = ~12.5min
END_movement <- Sys.time()
kittyR::meowR(sound = 3)


dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)
save(FITS,file="data/movement_model/fits_20250505.rda")
load("data/movement_model/fits_20250505.rda")



#...........................................................
# 2. Speed analysis ----
#...........................................................


SPEED_MEAN <- list()
SPEEDS_INSTA <- list()

START_speed <- Sys.time()
tic(msg = "speed analyses")

for(i in 1:length(tel_data)){
  tic(msg = "one goat")
  message("Currently on animal ", i, " of ", length(tel_data))
  #Extract individual telemetry and movement model
  DATA <- tel_data[[i]]
  fits <- FITS[[i]]
  
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true), error = TRUE caused issues with no converging
  SPEED_MEAN[[i]] <- speed(object = DATA, CTMM = fits,
                           robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  # estimate instantaneous speed -> Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value
  SPEEDS_INSTA[[i]] <- speeds(object = DATA, CTMM = fits,
                              robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  toc()
  
}

#rename for convenience
names(SPEED_MEAN) <- names(tel_data)
names(SPEEDS_INSTA) <- names(tel_data)

toc() #13.9 mins, ~26 min, combined data = 48 min, combined without error
END_speed <- Sys.time()
# beep(8)
# kittyR::meowR(sound = 3)

# 
# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.


save(SPEED_MEAN, file = "./data/movement_model/speed_mean_20250505.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/speeds_insta_20250505.rda")
load(file = "./data/movement_model/speed_mean_20250505.rda")
load(file = "./data/movement_model/speeds_insta_20250505.rda")



