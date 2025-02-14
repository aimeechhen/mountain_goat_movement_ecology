

# moving window analysis only, no extractions, save outputs, very basic

library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)
library(sf)
library(raster)
library(terra)
library(beepr)
library(crayon)




#...............................................................
# A. Import data ----
#...............................................................

# load("data/collar_data/collar_data_20240703.rda")
load("data/collar_data/collar_data_20241123.rda")

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# subset to fire goats
# goats <- c("kid_rock", "rocky")
# goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
# fire_goats <- collar_data[collar_data$goat_name %in% goats,] # 43941 obs
# fire_start <- '2023-07-22' # doy = 203
# fire_end <- '2023-10-26' # doy = 299
# dat <- fire_goats[fire_goats$date >= fire_start & fire_goats$date <= fire_end, ]
# dat <- plyr::rename(dat, c('goat_name' = 'individual.local.identifier',
#                            'latitude' = 'location.lat',
#                            'longitude' = 'location.long'))
# 
# 
# dat <- plyr::rename(fire_goats, c('goat_name' = 'individual.local.identifier',
#                                    'latitude' = 'location.lat',
#                                    'longitude' = 'location.long'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#format names to match required for ctmm based on Movebank critera:
dat <- plyr::rename(collar_data, c('goat_name' = 'individual.local.identifier',
                                   'latitude' = 'location.lat',
                                   'longitude' = 'location.long'))

# convert to a ctmm telemetry object
tel_data <- as.telemetry(dat)

# Import rasters
elev <- rast('./data/rasters/elev_25m.tif')
dist_escape <- rast('./data/rasters/dist_escape_25m.tif')
# reproject into lat/long crs
elev <- project(elev, "EPSG:4326")
dist_escape <- project(dist_escape, "EPSG:4326")


#create folders
folder_list <- c("fits_20250118", 
                 "akdes_20250118", 
                 "mean_speed_20250118",
                 "insta_speed_20250118",
                 "covariates_20250118")

# set directory path
# dir_path <- "./data/moving_window/fire_goats/fire_period/basic/" # for only during the fire period with the 6 fire goats
# dir_path <- "./data/moving_window/fire_goats/basic/" # for all of the data but only of the 6 fire goats
dir_path <- "./data/moving_window/" # for all goats for all data

# create every folder in the folder list in the directory 
for (folder in folder_list) {
  dir.create(paste0(dir_path, folder), 
             recursive = TRUE, showWarnings = TRUE)
}


#..............................................................
# for loop description

# for each individual
# 1. subset an individual and generate start times of each window for analysis (outer loop)
# 2. subset data within the window segment and run analysis for each window in the goat data (inner loop)
  # i. fit movement models + save outputs
  # ii. fit akdes + save output
  # iii. fit speed + save output

dt <- 1 %#% 'day' #  %#% uses ctmm package to set the units, i.e. days
win <- 3 %#% 'day'


tic()
START <- Sys.time()

# outer for loop ----

for(goat in 1:length(tel_data)){
  # subset an individual out
  DATA <- tel_data[[goat]]
  
  #.......................................................................
  # Set up the window segments
  # Generate start times with a 3-day segment for the individual, using t column instead of timestamp column because of the nature of how this is set up and using ctmm objects
  times <- seq(from = DATA$t[1], # t = Unix timestamp format
               to = DATA$t[nrow(DATA)],  
               by = dt) # shift each segment by 1 day forward
  
  # ensure that they are full days from 00:00 to 23:59, set the timestamps to 00:00 time, since we are looking at 3-day windows and not time specific
  # Convert Unix timestamps to POSIXct
  times <- as.POSIXct(times, origin = "1970-01-01", tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                                                DATA$longitude[1],
                                                                                method = "fast")) # fast method is used because all the data are in the same timezone, adjust if they cross timezone boundaries
  # Set time to midnight (00:00:00) and convert back to Unix format
  times <- as.numeric(as.POSIXct(format(times, "%Y-%m-%d 00:00:00"), 
                                 tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                             DATA$longitude[1],
                                                             method = "fast"))) # fast method is used because all the data are in the same timezone, adjust if they cross timezone boundaries
  
  # set up list to store
  fits <- list()
  # akdes <- list()
  speed_mean <- list()
  speeds_insta <- list()
  # covariates <- data.frame(
  #   goat_name = character(length(times)), 
  #   window_start <- as.POSIXct(rep(NA, length(times)), tz = "America/Vancouver"),
  #   window_end <- as.POSIXct(rep(NA, length(times)), tz = "America/Vancouver"),
  #   n_fixes = numeric(length(times)),      
  #   mean_elev = numeric(length(times)),     
  #   mean_dist_escape = numeric(length(times)) 
  # )
  
  
  #.......................................................................
  # Analysis on the window segment ----
  
  for (i in 1:length(times)) {
    # Extract data within the window segment
    SUBSET <- DATA[times[i] <= DATA$t & DATA$t <= times[i] + win,] # +win means window size (3 days)

    if (nrow(SUBSET) == 0) {
      cat("No data found for window section in iteration", i, "- moving on to the next iteration.\n")
      next
    }
    
    # get subset window start and end based on the recorded collar data
    WINDOW_START <- as.POSIXct(min(SUBSET$t), origin = "1970-01-01", 
                               tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast"))
    WINDOW_END <- as.POSIXct(max(SUBSET$t), origin = "1970-01-01", 
                             tz = lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast"))
    
    # Indicate the iteration and window segment ----
    cat(bgMagenta(paste((i), "of", length(times), "iterations. Window segment:", 
                      WINDOW_START, "to", WINDOW_END, 
                      "for mountain goat:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FITS <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      if (inherits(FITS, "ctmm")) {
        # AKDES <- akde(SUBSET, FITS, weights = TRUE)
        
        message(yellow("SPEED analyses"))
        SPEED_MEAN <- speed(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -1)
        SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -1)

        # store models/UDs in a list, name the entry based on goat name and subset window start date, not the times[i] as that is in unix format
        fits[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FITS
        # akdes[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED_MEAN
        speeds_insta[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEEDS_INSTA
        

        # # habitat variables ----
        # SUBSET_SF <- as.sf(SUBSET)
        # SUBSET_SF <- st_transform(SUBSET_SF, crs = st_crs("epsg:4326"))
        # #convert sf into spatvector object to be able to extract values
        # locations <- vect(SUBSET_SF)
        # # extract mean habitat values for each moving window segment (meters)
        # covariates$goat_name[i] <- DATA@info$identity
        # covariates$window_start[i] <- WINDOW_START    
        # covariates$window_end[i] <- WINDOW_END       
        # covariates$n_fixes[i] <- nrow(SUBSET)                       
        # covariates$mean_elev[i] <- mean(extract(elev, locations)[,2])                
        # covariates$mean_dist_escape[i] <- mean(extract(dist_escape, locations)[,2]) 
        
        # END OF INNER LOOP
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", i, "-", e$message, "\n")
    })
    
    # # Combine the lists into a dataframe and set the column names to match
    # covariates <- setNames(as.data.frame(do.call(cbind, list(goat_name, window_start, window_end, n_fixes, mean_elev, mean_dist_escape))), 
    #                        c("goat_name", "window_start", "window_end", "n_fixes", "mean_elev", "mean_dist_escape"))
    # 
    
  }
  
  # save all the outputs as a rds for future analysis ----
  message(green(paste("saving output for goat", DATA@info[1])))
  # saveRDS(fits, file = paste0(dir_path, "fits_20250118/fits_", DATA@info[1], ".rds"))
  # saveRDS(akdes, file = paste0(dir_path, "akdes_20250118/akdes_", DATA@info[1], ".rds"))
  saveRDS(speed_mean, file = paste0(dir_path, "mean_speed_20250118/mean_speed_", DATA@info[1], ".rds"))
  saveRDS(speeds_insta, file = paste0(dir_path, "insta_speed_20250118/insta_speed_", DATA@info[1], ".rds"))
  # saveRDS(covariates, file = paste0(dir_path, "covariates_20250118/covariates_", DATA@info[1], ".rds")) # remember this is a df and not a list
  
  # clean up environment
  rm(FITS, 
     # AKDES)
  SPEED_MEAN, SPEEDS_INSTA)
  gc() # free up computational resources
  
  # beep(3)
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW GOAT
  
  beep(8)
}


toc() # 5.2 min, 5 min, 5.6min
# 15min, 33min for all fire goats for cc fire period only
# for all data = 15.5h (but was running 2 sessions and took jan 19-24)
# speed for all data = ~17 days, 3 hours, 17 minutes


END <- Sys.time()


