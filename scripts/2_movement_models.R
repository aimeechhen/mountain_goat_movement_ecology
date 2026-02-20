
# movement models and home range estimation

library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)

#...........................................................
# Import data ----
#...........................................................

load(file = "./data/goat/study_data.rda")

# drop certain columns or will have issues when converting into ctmm object. not including measurement error, refer to previous scripts for explanation and why
study_data <- subset(study_data, select = -c(fix_type, hdop, vdop, pdop, dop, altitude_m))
# because no DOP is included, ctmm is going to assume DOP = 1

# set id_year as individual.local.identifier to avoid needing to subset by individuals and year, it will create a unique identifier based on the individual and the year of the data and then those will be grouped together as the data for each individual for each year
study_data$individual.local.identifier <- study_data$id_year

# convert to ctmm object
tel_data <- as.telemetry(study_data, mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id", "id_year"))
# 35 items 6x6=36-1= 35 CA12 got collared in 2020, missing 1 year = 35 (6 goats x 6 years except 1 has 5 years)

# ensure each df in the list is in the correct order with matching row name
for (i in 1:length(tel_data)) {
  # order rows in each df in the list based on fix_id
  tel_data[[i]] <- tel_data[[i]][order(tel_data[[i]][["fix_id"]]),]
}

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
tic(msg = "movement models")

for(i in 1:length(tel_data)){
  message(bgWhite(cyan("Currently on animal ", i, " of ", length(tel_data))))
  #Extract individual
  DATA <- tel_data[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process, error = true will help avoid iid models and push towards ou and ouf models, keeping error = FALSE because not using any error models
  # fit movement models and select best fit
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)

}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h, #9.698333 mins; full = ~12.5min, 
# combined: error on = 6.37h, error off = 12.8min
END_movement <- Sys.time()



dir.create("./data/movement_model/", recursive = TRUE, showWarnings = TRUE)
save(FITS,file="data/movement_model/fits.rda")
# load("data/movement_model/fits.rda")



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
  
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true), error = TRUE caused issues with no converging, not using error models, error = FALSE
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

toc() #13.9 mins, ~26 min, combined data = 48 min, combined without error = 59min
END_speed <- Sys.time()
# beep(8)
# kittyR::meowR(sound = 3)

# 
# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.


save(SPEED_MEAN, file = "./data/movement_model/speed_mean_20250505.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/speeds_insta_20250505.rda")
# load("./data/movement_model/speed_mean_20250505.rda")
# load("./data/movement_model/speeds_insta_20250505.rda")



#................................................
# determine window size

load("./scripts/working/data/movement_model/fits_20250505.rda")
summary(FITS)
