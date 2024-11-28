

# Moving window (version 1d)

# Script description: (Ryan) uses output from previous file to iterate each over collar and calculate home range over a 14 day moving window, advanced by 1 day to determine when each animal becomes range resident 
# Note: Script is based off Ryan's '03.ctmm_moving_window.R' script. The code has been modified to be able to handle missing data within window segments


# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
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

#______________________________________________________
#03.ctmm_moving_window.R

#Loop through windows of X days of locations, 
#Compute HR size for each window

#Mark Bidwell, Chris Fleming (mostly Chris!)

#Modified by Michael Noonan
#Last updated: Feb 14th 2022

#Rewritten June-July 2024 by Aimee Chhen and checked by Dr. Noonan July 2024
#Extract HR size and 95% CIs
#Extract movement metrics
#Extract centroid
#Save outputs


#...............................................................
# Import data ----
#...............................................................

rm(list = ls())

load("data/collar_data/collar_data_20240703.rda")

#format names to match required for ctmm based on Movebank critera:
dat = plyr::rename(collar_data, c('collar_id' = 'individual.local.identifier',
                                   # 'date_time' = 'timestamp', 
                                   'latitude' = 'location.lat', 
                                   'longitude' = 'location.long'))

#duplicated positions?
summary(duplicated(dat))
# 
# # import data from 02_outlier_detection.r
# dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
# dat = dat[dat$year == "2022",]

# tel.data <- dat
tel.data = as.telemetry(dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
# collars = dat %>% distinct(individual.local.identifier)

# tel.data <- tel.data[1:2]
# 
# dt = 1 %#% 'day' # %#% converts it into days (si units)
# win <- 3 %#% 'day'
# # tel.data <- tel.data[[1]]
# i <- 1
# DATA = tel.data

#...........................................
# Habitat data ----

slope = terrain(dem, "slope", units = 'degrees')
# distance to escape was made by first by creating a raster of 0/1 of whether a pixel had a slope greater or less than a cutoff value (like 45°, or something like that). Then a proximity raster was created where each pixel represented a distance to those 1's (> 45°)
# I can't remember how I derived the other one, whether it was some proximity function in terra, or in QGIS. Probably in QGIS since it's very easy.


# Import rasters
elevation <- rast('./data/rasters/elev_25m.tif')
dist_escape <- rast('./data/rasters/dist_escape_25m.tif')
slope <- rast('./data/rasters/slope_25m.tif')
# reproject into lat/long crs
el <- project(elevation, "EPSG:4326")
esc <- project(dist_escape, "EPSG:4326")
slp <- project(slope, "EPSG:4326")


# some collars within each period cause the script to crash if data is missing, this modified function code corrects for it without having to remove the collar
# due to the missing data, plotting section has been taken out and done separately after, plotting code has been modified to handle missing data

#create directories to hold output:
dir.create(paste0("./data/input_data/moving_window/", "/Fits_20240719"), recursive = TRUE)
dir.create(paste0("./data/input_data/moving_window/", "/UDs_20240719"), recursive = TRUE)
# Set results directory
mod_path = paste0(getwd(), "/data/input_data/moving_window/", '/Fits_20240719')
UD_path = paste0(getwd(), "/data/input_data/moving_window/", '/UDs_20240719')

#...............................................................
# window.hr function ----
#...............................................................

window.HR <- function(DATA, dt, win) {
  # DATA = tel.data
  tryCatch({
    # Generate start times for each 3-day segment
    #*************************(check because they may be overlapping if moving forward by a day?)
    # 1. Set up and generate the start time/time intervals window segment (win = 3 days) without overlapping segments, and empty lists to hold the results ----
    times <- seq(from = DATA$t[1], # t = Unix timestamp format
                 to = DATA$t[nrow(DATA)],  
                 by = dt) # shift each segment by 1 day forward
  })
  #initialize arrays (lists) for results
  hr_min = rep(NA, length(times))
  hr_est = rep(NA, length(times))
  hr_max = rep(NA, length(times))
  tau_p_min = rep(NA, length(times))
  tau_p_est = rep(NA, length(times))
  tau_p_max = rep(NA, length(times))
  tau_v_min = rep(NA, length(times))
  tau_v_est = rep(NA, length(times))
  tau_v_max = rep(NA, length(times))
  diffusion_min = rep(NA, length(times))
  diffusion_est = rep(NA, length(times))
  diffusion_max = rep(NA, length(times))
  speed_mean_min = rep(NA, length(times))
  speed_mean_est = rep(NA, length(times))
  speed_mean_max = rep(NA, length(times))
  mean_el = rep(NA, length(times))
  mean_dist_escape = rep(NA, length(times))
  mean_slope = rep(NA, length(times))
  centroid.long = rep(NA, length(times))
  centroid.lat = rep(NA, length(times))
  
  # 2. loop through each window segment ----
  for(i in 1:length(times)) {
    #indicate what iteration the analysis is currently on
    print(paste((i),"of",length(times),"iterations. At window segment", 
                format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
                "for Collar ID:", DATA@info[1]))
    
    # 3. subset data within window segment ----
    SUBSET <- DATA[times[i] <= DATA$t & DATA$t <= times[i] + win,] # +win means window size i.e. 3 days
    
    # Check if there are any data present in the subset for the current window segment
    if (nrow(SUBSET) == 0) {
      cat("No data found or data is missing for window section in iteration", i, "- moving on to the next iteration.\n")
      # Set results to NA
      hr_min[i] <- NA
      hr_est[i] <- NA
      hr_max[i] <- NA
      next # Move to the next iteration if the subset is empty
    }
    
    # 4. If data exist in the subset, proceed with analysis ----
    tryCatch({
      if (nrow(SUBSET) > 0) {
        
        ## Extract habitat values for each window segment ----
        #convert ctmm telemetry into sf object
        SUBSET.SF <- as.sf(SUBSET)
        SUBSET.SF <- st_transform(SUBSET.SF, crs = st_crs("epsg:4326"))
        #convert sf into spatvector object to be able to extract values
        locations <- vect(SUBSET.SF)
        # extract mean habitat values for each moving window segment (meters)
        mean_el[i] <- mean(extract(el, locations)[,2])
        mean_dist_escape[i] <- mean(extract(esc, locations)[,2])
        mean_slope[i] <- mean(extract(slp, locations)[,2])
        
        # fit movement models ----
        GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
        FIT <- try(ctmm.select(SUBSET, GUESS))
        
        # Extract mean location (i.e. centroid) ----
        fit.mu <- as.data.frame(FIT$mu)
        centroid.sf <- st_as_sf(fit.mu, coords = c('x', 'y'), crs = FIT@info$projection) # FIT is in Transverse Polyconic Equal Distance (tpeqd) projection
        centroid.sf <- st_transform(centroid.sf, crs = 4326)
        centroid <- st_coordinates(centroid.sf)
        centroid.long[i] <- centroid[1]
        centroid.lat[i] <- centroid[2]

        
        # inherits() or (class(FIT)=="ctmm") checks if 'FIT' is an object related to a ctmm object, inherits() provides more flexibility allowing any class derived from "ctmm" vs class(FIT)=="ctmm" is more strict and is exactly "ctmm"
        if (inherits(FIT, "ctmm")) {
          # calculate home range area
          AKDE <- akde(SUBSET, FIT)
          # calculate mean speed
          SPEED <- speed(object = SUBSET, CTMM = FIT, robust = TRUE, units = FALSE, cores = 8)
          
          
          # 4b. save analysis outputs ----
          # using the start date of the window segment as the date
          # save fitted model 'date_Fits_animal.rda'
          fits_path <- file.path(mod_path, paste0(date(SUBSET$timestamp[1]), "_Fits_", DATA@info[1], ".rda"))
          saveRDS(FIT, file = fits_path)
          #save AKDE 'date_UD_animal.rda'
          akde_path <- file.path(UD_path, paste0(date(SUBSET$timestamp[1]), "_UD_", DATA@info[1], ".rda"))
          saveRDS(AKDE, file = akde_path)
          
          # 5. Store results ----
          #Extract home range estimation (units = square meters)
          SUMMARY_HR <- summary(AKDE, units = FALSE)
          hr_min[i] <- SUMMARY_HR$CI[[1]]
          hr_est[i] <- SUMMARY_HR$CI[[2]]
          hr_max[i] <- SUMMARY_HR$CI[[3]]
          
          # Extract mean speed values (units = meters/sec) (time-averaged speed, proportional to distance travelled)
          # OU model causes 'Inf' because not enough data to estimate
          speed_mean_min[i] <- SPEED$CI[[1]]
          speed_mean_est[i] <- SPEED$CI[[2]]
          speed_mean_max[i] <- SPEED$CI[[3]]
          
          # Extract movement metrics ----
          SUMMARY_FIT <- summary(FIT, units = FALSE)$CI
          # Diffusion (square meters/second)
          diffusion_min[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 1]
          diffusion_est[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 2]
          diffusion_max[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 3]
          
          # Extract position and velocity (tau p and v)
          # OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)
          # this may not produce any values therefore unable to go to the next line of code i.e. diffusion values extraction not being ran)
          if (nrow(SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), ]) > 0) {
            tau_p_min[i] <- SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), 1]
            tau_p_est[i] <- SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), 2]
            tau_p_max[i] <- SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), 3]
          } else {
            tau_p_min[i] <- NA
            tau_p_est[i] <- NA
            tau_p_max[i] <- NA
          }
          
          if (nrow(SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), ]) > 0) {
            tau_v_min[i] <- SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), 1]
            tau_v_est[i] <- SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), 2]
            tau_v_max[i] <- SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), 3]
          } else {
            tau_v_min[i] <- NA
            tau_v_est[i] <- NA
            tau_v_max[i] <- NA
          }
          
          
        } else {
          # If FIT is not a ctmm object, set results to NA
          hr_min[i] <- NA
          hr_est[i] <- NA
          hr_max[i] <- NA
          
        }
      } else {
        # If SUBSET is empty, set results to NA
        hr_min[i] <- NA
        hr_est[i] <- NA
        hr_max[i] <- NA
        
        cat("No data found or data is missing for window section in iteration", i, "- setting results to NA.\n")
      }
    }, error = function(e) {
      # Set results to NA in case of error
      hr_min[i] <- NA
      hr_est[i] <- NA
      hr_max[i] <- NA
      
    })
  }
  
  
  # 7. Prepare results dataframe ----
  tryCatch({
    TIMES <- as.POSIXct(times, origin="1970-01-01",
                        tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                    DATA$longitude[1]))
    RESULTS <- data.frame(ID = DATA@info$identity,
                          # Times = TIMES,  #update to timestamp if need to be reran
                          timestamp = TIMES,
                          hr_min,
                          hr_est,
                          hr_max,
                          tau_p_min,
                          tau_p_est,
                          tau_p_max,
                          tau_v_min,
                          tau_v_est,
                          tau_v_max,
                          diffusion_min,
                          diffusion_est,
                          diffusion_max,
                          speed_mean_min,
                          speed_mean_est,
                          speed_mean_max,
                          mean_el,
                          mean_dist_escape,
                          mean_slope,
                          centroid.long,
                          centroid.lat
    )
  })
  
  
  
  return(RESULTS)
}


# issue with plotting within the function due to missing data therefore
# Plot outside of the original function and after to be able to account for missing data
# need to import the result data from all prior to plotting or itll just plot the last loop, might have to update the script


#.........................................................................
# Run the function ----
#.........................................................................

# 1a: ~ 12.5 hours (noonan)
# 1b: ~25 hours 
# 1c: ~24.8 hours
# 1d: ~24.5 hours
# 1e: ~24.7 saving outputs

START <- Sys.time()
tic()

RES <- list()
for(i in 1:length(tel.data)){
  
  # RES[[i]] <- window.HR(data[[i]][1:30,],
  RES[[i]] <- window.HR(tel.data[[i]],
                        dt = 1 %#% 'day', # what does %#% 'day' do?
                        win <- 3 %#% 'day')
  
  # save(RES, file = "./data/input_data/moving_window/Sliding_Window_20240705_noonan.Rda")
  # save(RES, file = "./data/input_data/moving_window/moving_window_1b_movement_20240711.rda")
  # save(RES, file = "./data/input_data/moving_window/moving_window_1c_movement_covariates_20240711.rda")
  # save(RES, file = "./data/input_data/moving_window/moving_window_1d_movement_covariates_centroid_20240711.rda")
  
  save(RES, file = "./data/input_data/moving_window/moving_window_1e_save_outputs_20240720.rda")
}

toc()
END <- Sys.time()

# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# END
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~