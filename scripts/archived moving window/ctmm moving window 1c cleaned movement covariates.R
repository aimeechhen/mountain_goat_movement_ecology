

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



#...............................................................
# Import data ----
#...............................................................

rm(list = ls())

dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat = dat[dat$year == "2022",]

# tel.data <- dat
tel.data = as.telemetry(dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
# collars = dat %>% distinct(individual.local.identifier)

tel.data <- tel.data[1:2]

dt = 1 %#% 'day' # %#% converts it into days (si units)
win <- 3 %#% 'day'
tel.data <- tel.data[[1]]
i <- 1
DATA = tel.data

#...........................................
# Habitat data ----

# Import rasters
elevation <- rast('./data/rasters/elev_25m.tif')
dist_escape <- rast('./data/rasters/dist_escape_25m.tif')
slope <- rast('./data/rasters/slope_25m.tif')
# reproject into lat/long crs
el <- project(elevation, "EPSG:4326")
esc <- project(dist_escape, "EPSG:4326")
slp <- project(slope, "EPSG:4326")


#...............................................................
# window.hr function ----
#...............................................................

window.HR <- function(DATA, dt, win) {
  # DATA = tel.data
  tryCatch({
    # 1. Set up the time intervals and empty lists to hold the results ----
    times <- seq(from = DATA$t[1], # t = Unix timestamp format
                 to = DATA$t[nrow(DATA)],  
                 by = dt)
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
  speed_min = rep(NA, length(times))
  speed_est = rep(NA, length(times))
  speed_max = rep(NA, length(times))
  mean_el = rep(NA, length(times))
  mean_dist_escape = rep(NA, length(times))
  mean_slope = rep(NA, length(times))

  
  # 2. loop through each window segment ----
  for(i in 360:length(times)) {
    #indicate what iteration the analysis is currently on
    print(paste((i),"of",length(times),"iterations. At window segment", 
                format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
                "for Collar ID:", DATA@info[1]))
    
    # 3. subset data within window time segment ----
    SUBSET <- DATA[times[i] <= DATA$t & DATA$t <= times[i] + win,] # +win means window size i.e. 3 days
    
    # Check if there is any data in the subset
    if (nrow(SUBSET) == 0) {
      cat("No data found or data is missing for window section in iteration", i, "- moving on to the next iteration.\n")
      
      # Set results to NA
      hr_min[i] <- NA
      hr_est[i] <- NA
      hr_max[i] <- NA
      
      next # Move to the next iteration if the subset is empty
    }
    
    # 4. If data exist in the subset, proceed with analysis and analyze the data in the subset ----
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
        
        # fit movement models
        GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
        FIT <- try(ctmm.select(SUBSET, GUESS))
        
        if (inherits(FIT, "ctmm")) {
          # calculate home range area
          AKDE <- akde(SUBSET, FIT)
          # calculate mean speed
          SPEED <- speed(SUBSET, FIT, robust = TRUE, units = FALSE)
          
          # 5. Store results ----
          SUMMARY_HR <- summary(AKDE, units = FALSE)
          hr_min[i] <- SUMMARY_HR$CI[[1]]
          hr_est[i] <- SUMMARY_HR$CI[[2]]
          hr_max[i] <- SUMMARY_HR$CI[[3]]
          # Extract mean speed values (units = meters/sec) (time-averaged speed, proportional to distance travelled)
          # OU model causes 'Inf' because not enough data to estimate
          speed_min[i] <- SPEED$CI[[1]]
          speed_est[i] <- SPEED$CI[[2]]
          speed_max[i] <- SPEED$CI[[3]]
          
          # extract movement metrics
          SUMMARY_FIT <- summary(FIT, units = FALSE)$CI
          diffusion_min[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 1]
          diffusion_est[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 2]
          diffusion_max[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 3]
          
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
                          Time = TIMES,
                          mean_el,
                          mean_dist_escape,
                          mean_slope,
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
                          speed_min,
                          speed_est,
                          speed_max
    )
  })
  
  
  
  return(RESULTS)
}


#.........................................................................
# Run the function ----
#.........................................................................

#~ 12.5 hours
# 1c: ~24.8 hours

START <- Sys.time()
tic()
RES <- list()
for(i in 1:length(tel.data)){
  
  # RES[[i]] <- window.HR(data[[i]][1:30,],
  RES[[i]] <- window.HR(tel.data[[i]],
                        dt = 1 %#% 'day', # what does %#% 'day' do?
                        win <- 3 %#% 'day')
  
  # save(RES, file = "./data/input_data/moving_window/Sliding_Window_20240705_noonan.Rda")
  # save(RES, file = "./data/input_data/moving_window/moving_window_movement_20240711.rda")
  # save(RES, file = "./data/input_data/moving_window/moving_window_movement_covariates_20240711.rda")
  
}
toc()
END <- Sys.time()

RESULTS <- do.call(rbind, lapply(RES, as.data.frame))


# head(RES[[10]])