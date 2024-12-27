



# Moving window (version 1f)

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
library(beepr)
library(crayon)

#______________________________________________________
#03.ctmm_moving_window.R

#Loop through windows of X days of locations, 
#Compute HR size for each window

#Mark Bidwell, Chris Fleming (mostly Chris!)

#Modified by Michael Noonan
#Last updated: Feb 14th 2022

#Rewritten June-July 2024 by Aimee Chhen and checked by Dr. Noonan July 2024, then again by Aimee Chhen in December 2024 with concerns about units not being consistent in the outputs
#Extract HR size and 95% CIs
#Extract movement metrics
#Extract centroid
#Save outputs


#...............................................................
# A. Import data ----
#...............................................................

rm(list = ls())
# 
# load("data/collar_data/collar_data_20240703.rda")
load("data/collar_data/collar_data_20241123.rda")

# #~~~~~~~~~~~~~~~~~~~~~~~~~~
# # subset to fire goats
# fire_goats <- c("kid_rock", "rocky")
# # fire_goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
# dat <- collar_data[collar_data$goat_name %in% fire_goats,] # 43941 obs
# # ccfire_start <- '2023-07-22' # doy = 203
# ccfire_start <- '2023-09-22' # doy = 203
# ccfire_end <- '2023-10-26' # doy = 299
# dat <- dat[dat$date >= ccfire_start & dat$date <= ccfire_end, ]
# dat <- plyr::rename(dat, c('goat_name' = 'individual.local.identifier',
#                            'latitude' = 'location.lat',
#                            'longitude' = 'location.long'))
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

#create folder
dir.create(paste0("./data/moving_window/fire_goats", "/fits_20241224"), recursive = TRUE, showWarnings = TRUE)
dir.create(paste0("./data/moving_window/fire_goats", "/akdes_20241224"), recursive = TRUE, showWarnings = TRUE)

# Set results directory
fits_dir = paste0(getwd(), "/data/moving_window/fire_goats", '/fits_20241224')
akdes_dir = paste0(getwd(), "/data/moving_window/fire_goats", '/akdes_20241224')



#...............................................................
# B. window function ----
#...............................................................

# # testing the function
# i <- 1
# DATA <- tel_data[1]
# DATA <- DATA[[1]]
# dt <- 1 %#% 'day'#, #  %#% uses ctmm package to set the units, i.e. days
# win <- 3 %#% 'day'


window_HR <- function(DATA, dt, win) {
  
  # 1) Data prep ----
  tryCatch({
    
    ## Generate start times and prep results lists ----
    # Generate start times for each 3-day segment
    # Set up and generate the start time/time intervals window segment (win = 3 days) without overlapping segments, and empty lists to hold the results
    #*************************(check because they may be overlapping if moving forward by a day?)
    
    
    times <- seq(from = DATA$t[1], # t = Unix timestamp format
                 to = DATA$t[nrow(DATA)],  
                 by = dt) # shift each segment by 1 day forward
    
    
    #...................
    ## Prep and build results dataframe ----
    
    TIMESTAMP <- as.POSIXct(times, 
                            origin="1970-01-01", # add if in Unix timestamp format to convert
                            tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                        DATA$longitude[1],
                                                        method = "fast")) # fast method is used because all the data are in the same timezone, adjust if they cross timezone boundaries
    
    RESULTS <- data.frame(individual.local.identifier = DATA@info$identity,
                          timestamp = TIMESTAMP)
    
    # set up each subsection results dataframe
    movement_model <- data.frame()
    hr_size <- data.frame()
    DOF_results <- data.frame()
    diffusion_results <- data.frame()
    tau_p_results <- data.frame()
    tau_v_results <- data.frame()
    dof_results <- data.frame()
    ci_results <- data.frame()
    speed_mean_results <- data.frame()
    mean_elev_results <- data.frame()
    mean_dist_escape_results <- data.frame()
    covariates_results <- data.frame()
    long_results <- data.frame()
    lat_results <- data.frame()
    centroid_results <- data.frame()
    
  })
  
  
  
  #..............................................
  # WINDOW ANALYSIS ----
  
  for(i in 1:length(times)) {
    # extract data that is within window segment
    SUBSET <- DATA[times[i] <= DATA$t & DATA$t <= times[i] + win,] # +win means window size i.e. 3 days
    
    window_start <- as.Date(min(SUBSET$timestamp),
                            lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    window_end <- as.Date(max(SUBSET$timestamp),
                          lutz::tz_lookup_coords(SUBSET$latitude[1], SUBSET$longitude[1], method = "fast")) 
    
    # add into to results df
    RESULTS$window_start[i] <- window_start
    RESULTS$window_end[i] <- window_end
    
    #indicate what iteration the analysis is currently on
    print(paste((i),"of",length(times),"iterations. Window segment:", 
                window_start, "to", window_end, 
                "for mountain goat:", DATA@info[1]))
    
    
    # Check if there are any data extracted for the current window segment
    if (nrow(SUBSET) == 0) {
      cat("No data found or data is missing for window section in iteration", i, "- moving on to the next iteration.\n")
      # if no data, then set results to NA for the current window segment
      RESULTS[i, c("mean_hr_min_km2", "mean_hr_est_km2", "mean_hr_max_km2")] <- NA
      next # Move to the next iteration if the no data is in the window segment
    }
    
    
    
    # If data exist in the subset, proceed with analysis
    tryCatch({
      if (nrow(SUBSET) > 0) {
        
        ## 2) Fit movement models ----
        # create guesstimate non-interactively
        GUESS <- ctmm.guess(SUBSET, interactive = FALSE) 
        # fit movement models and select best fit, cores = -1 means keep one core for other tasks on the computer, cores = 0 means use them all
        FITS <- try(ctmm.select(DATA, GUESS, trace = 3, cores=-1)) 
        
        
        # save fitted model using the start date of the window segment as the date 'date_fits_animal.rds'
        fits_path <- file.path(fits_dir, paste0(date(SUBSET$timestamp[1]), "_fits_", DATA@info[1], ".rds"))
        saveRDS(FITS, file = fits_path)
        
        # 3) Estimate akdes home-range areas ----
        # inherits() or (class(FIT)=="ctmm") checks if 'FIT' is an object related to a ctmm object, inherits() provides more flexibility allowing any class derived from "ctmm" vs class(FIT)=="ctmm" is more strict and is exactly "ctmm"
        if (inherits(FITS, "ctmm")) {
          
          AKDES <- akde(SUBSET, FITS, weights=TRUE)
          
          #save AKDE using the start date of the window segment as the date ' 'date_akdes_animal.rds'
          akdes_path <- file.path(akdes_dir, paste0(date(SUBSET$timestamp[1]), "_akdes_", DATA@info[1], ".rds"))
          saveRDS(AKDES, file = akdes_path)
          
          # 4) Estimate various speed types ----
          
          # calculate the mean speed of the Gaussian movement process
          SPEED_MEAN <- speed(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = 8)
          # estimates instantaneous speeds at a specified array of times t
          SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = 8)
          
          # Warning message:
          # 3: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
          #                    Movement model is fractal.
          # i.e. warning means that the way the animal moves doesn't follow a simple or predictable pattern .-. speed calculations may not function correctly, .-. producing irregular or unusual speed results
          
          
          
          
          
          
          #__________________________________________________________
          # 5) RESULTS ----
          
          cat(green("Extracting results for window segment", 
                    window_start, "to", window_end, 
                    "for mountain goat:", DATA@info[1], "\n"))
          
          ## 1. Get movement model that was best fit ----
          model_type <- as.data.frame(summary(FITS)$name)
          names(model_type)[1] <- "movement_model"
          
          # add to movement_model df
          movement_model <- rbind(movement_model, model_type)
          
          
          #...............................................................................
          ## 2. Get home range size ----
          hr <- as.data.frame(summary(AKDES, units = FALSE)$CI)
          
          # Check units by seeing if the rowname contains "square kilometers"
          if (grepl("square kilometers", rownames(hr))) {
            # Rename columns
            colnames(hr) <- c("hr_min_km2", "hr_est_km2", "hr_max_km2")
            # add to hr_size df
            hr_size <- rbind(hr_size, hr)
            
          } else if (grepl("square meters", rownames(hr))) { # using 'else if' then else because theres more than just if else i.e. more than 2 scenarios
            # if the rownames do not contain "square kilometers" but contains "square meters" then values need to be converted, therefore divide the each value by 1000000 or 1e6
            # convert values
            hr <- hr / 1e6
            # Rename columns
            colnames(hr) <- c("hr_min_km2", "hr_est_km2", "hr_max_km2")
            # add to hr_size df
            hr_size <- rbind(hr_size, hr)
            
            # if either of those two units do not appear in the rownames i.e. hectares then itll just make new columns for whatever else it could be
          } else {
            # extract the unit in the brackets in the rowname
            unit <- gsub(".*\\((.*)\\).*", "\\1", rownames(hr))
            # rename columns accordingly to the extract unit text string
            colnames(hr) <- paste0(c("hr_min_", "hr_est_", "hr_max_"), unit)
            # add to hr_size df
            hr_size <- rbind(hr_size, hr)
          }
          
          rownames(hr_size) <- NULL
          
          #...............................................................................
          ## 3. Get movement models degrees of freedom ----
          
          # convert it into a df and transpose it because the row/columns are flipped
          DOF <- as.data.frame(t(summary(FITS)$DOF))
          names(DOF)[1] <- "DOF_mean"
          names(DOF)[2] <- "DOF_area"
          names(DOF)[3] <- "DOF_diffusion"
          names(DOF)[4] <- "DOF_speed"
          
          # add to DOF_results df
          DOF_results <- rbind(DOF_results, DOF)
          
          
          
          #................................................................................
          ## 4. Get diffusion estimates ---- 
          
          # units are not all the same -> need to correct and convert
          # units = FALSE removes the units that FITS has assigned and uses the default units -> (units = square meters/second)
          
          # Check to see if summary has a rowname containing "diffusion"
          if (grepl("diffusion", rownames(summary(FITS, units = FALSE)$CI))) {
            # Check units by seeing if the rowname contains "square meters"
            # subset to diffusion size row only, convert it into a df and transpose it because the row/columns are flipped
            diffusion <- as.data.frame(t(summary(FITS, units = FALSE)$CI["diffusion (square meters/second)", ]))
            names(diffusion)[1] <- "diffusion_min_m2_s"
            names(diffusion)[2] <- "diffusion_est_m2_s"
            names(diffusion)[3] <- "diffusion_max_m2_s"
            
            # add to diffusion_results df
            diffusion_results <- rbind(diffusion_results, diffusion)
            
            
          } else {
            # create the diffusion df to hold the NA and set values to NA if diffusion is not in the summary
            diffusion <- data.frame(diffusion_min_m2_s = NA, diffusion_est_m2_s = NA, diffusion_max_m2_s = NA)
            # add to diffusion_results df
            diffusion_results <- rbind(diffusion_results, diffusion)
          }
          
          
          
          #........................................................
          ## 5. Get tau p estimates (position) ----
          # OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)
          
          
          # units are not the same, so need to convert them
          # Check to see if summary has a rowname containing tau p data
          
          if (grepl("[position] (days)", rownames(summary(FITS, units = FALSE)$CI)))  {
            #if it does then extract information
            tau_p <- as.data.frame(summary(FITS)$CI)[grepl("[position] (days)", rownames(summary(FITS)$CI)), ]
            # Rename columns directly
            colnames(tau_p) <- c("tau_p_min_days", "tau_p_est_days", "tau_p_max_days")
            # Bind to results
            tau_p_results <- rbind(tau_p_results, tau_p)
            
          } else if (grepl("[position] (hours)", rownames(summary(FITS, units = FALSE)$CI)))  {
            tau_p <- as.data.frame(summary(FITS)$CI)[grepl("[position] (hours)", rownames(summary(FITS)$CI)), ]
            #if units are in hours, then convert them by dividing the each value by 24 
            tau_p <- tau_p / 24
            
            # Rename columns
            colnames(tau_p) <- c("tau_p_min_days", "tau_p_est_days", "tau_p_max_days")
            # Bind to results
            tau_p_results <- rbind(tau_p_results, tau_p)
            
          } else { # if tau_p is not in the summary
            # create the tau_p df to hold the NA and set values to NA
            tau_p <- data.frame(tau_p_min_days = NA, tau_p_est_days = NA, tau_p_max_days = NA)
            # Bind to results
            tau_p_results <- rbind(tau_p_results, tau_p)
          }
          
          #........................................................
          ## 6. Get tau v estimates (velocity) ----
          # OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)
          
          
          # units are not the same, so need to convert them
          #Check if the row name contains "days"
          if (grepl("[velocity] (minutes)", rownames(summary(FITS, units = FALSE)$CI)))  {
            tau_v <- as.data.frame(summary(FITS)$CI)[grepl("[velocity] (minutes)", rownames(summary(FITS)$CI)), ]
            # Rename columns directly
            colnames(tau_v) <- c("tau_v_min_minutes", "tau_v_est_minutes", "tau_v_max_minutes")
            # Bind to results
            tau_v_results <- rbind(tau_v_results, tau_v)
            
          } else if (grepl("[velocity] (hours) ", rownames(summary(FITS, units = FALSE)$CI)))  {
            tau_v <- as.data.frame(summary(FITS)$CI)[grepl("[velocity] (hours)", rownames(summary(FITS)$CI)), ]
            tau_v <- as.data.frame(summary(FITS)$CI)[grepl("[velocity] (hours)", rownames(summary(FITS)$CI)), ]
            # # if the rownames do not contain units in "minutes" but contains units in "hours" then values need to be converted, therefore multiple the each value by 60
            tau_v <- tau_v * 60
            
            # Rename columns
            colnames(tau_v) <- c("tau_v_min_minutes", "tau_v_est_minutes", "tau_v_max_minutes")
            # Bind to results
            tau_v_results <- rbind(tau_v_results, tau_v)
            
          } else { # if tau_v is not in the summary
            # create the tau_v df to hold the NA and set values to NA
            tau_v <- data.frame(tau_v_min_minutes = NA, tau_v_est_minutes = NA, tau_v_max_minutes = NA)
            # Bind to results
            tau_v_results <- rbind(tau_v_results, tau_v)
          }
          
          
          #......................................................
          ## 7. get mean speed ----
          
          dof_df <- as.data.frame(SPEED_MEAN$DOF)
          names(dof_df)[1] <- "speed_mean_DOF"
          dof_results <- rbind(dof_results, dof_df)
          rownames(dof_results) <- NULL
          
          
          ci_df <- as.data.frame(SPEED_MEAN$CI)
          # check to see if (meters/second) is in the row name
          if (grepl("(meters/second)", rownames(ci_df))) {
            # Rename columns directly
            colnames(ci_df) <- c("speed_mean_min_ms", "speed_mean_est_ms", "speed_mean_max_ms")
            ci_results <- rbind(ci_results, ci_df)
            
          } else {
            # extract the unit in the brackets in the rowname
            unit <- gsub(".*\\((.*)\\).*", "\\1", rownames(ci_df))
            # rename columns accordingly to the extract unit text string
            colnames(ci_df) <- paste0(c("speed_mean_min_", "speed_mean_est_", "speed_mean_max_"), unit)
            ci_results <- rbind(ci_results, ci_df)
            
          }
          rownames(ci_results) <- NULL
          
          #......................................................
          ## 8. get insta speed ----
          
          # TBD
          
          
          
          #......................................................
          ## 9. Extract habitat covariates values for each window segment ----
          #convert ctmm telemetry into sf object
          SUBSET_SF <- as.sf(SUBSET)
          SUBSET_SF <- st_transform(SUBSET_SF, crs = st_crs("epsg:4326"))
          #convert sf into spatvector object to be able to extract values
          locations <- vect(SUBSET_SF)
          # extract mean habitat values for each moving window segment (meters)
          mean_elev <- as.data.frame(mean(extract(elev, locations)[,2]))
          names(mean_elev)[1] <- "mean_elev_m"
          mean_dist_escape <- as.data.frame(mean(extract(dist_escape, locations)[,2]))
          names(mean_dist_escape)[1] <- "mean_dist_escape_m"
          
          mean_elev_results <- rbind(mean_elev_results, mean_elev)
          mean_dist_escape_results <- rbind(mean_dist_escape_results, mean_dist_escape)
          
          
          
          #......................................................
          ## 10. Extract mean location (i.e. centroid) ----
          fits_mu <- as.data.frame(FITS$mu)
          centroid_sf <- st_as_sf(fits_mu, coords = c('x', 'y'), crs = FITS@info$projection) # FIT is in Transverse Polyconic Equal Distance (tpeqd) projection
          centroid_sf <- st_transform(centroid_sf, crs = 4326)
          centroid <- st_coordinates(centroid_sf)
          centroid_long <- as.data.frame(centroid[1])
          centroid_lat <- as.data.frame(centroid[2])
          names(centroid_long)[1] <- "centroid_long"
          names(centroid_lat)[1] <- "centroid_lat"
          
          long_results <- rbind(long_results, centroid_long)
          lat_results <- rbind(lat_results, centroid_lat)
          
          
        } else {
          # If FIT is not a ctmm object, set results to NA
          RESULTS[i, c("mean_hr_min_km2", "mean_hr_est_km2", "mean_hr_max_km2")] <- NA
          
        }
      } else {
        cat("No data found or data is missing for window section in iteration", i, "- setting results to NA.\n")
        # If SUBSET is empty, set results to NA
        RESULTS[i, c("mean_hr_min_km2", "mean_hr_est_km2", "mean_hr_max_km2")] <- NA
        
        
      }
    }, error = function(e) {
      # Set results to NA in case of error
      RESULTS[i, c("mean_hr_min_km2", "mean_hr_est_km2", "mean_hr_max_km2")] <- NA
      
    })
  }
  
  
  ## 6) Combine all subsection df into main df ----
  tryCatch({
    # add subsection results df to main results_df
    RESULTS <- cbind(RESULTS, movement_model) #1
    RESULTS <- cbind(RESULTS, hr_size) #2
    RESULTS <- cbind(RESULTS, DOF_results) #3
    # Convert m^2/second into km^2/day and create new columns for them
    # diffusion_results$diffusion_min_km2_day <- diffusion_results$diffusion_min_m2_s * 0.0864
    # diffusion_results$diffusion_est_km2_day <- diffusion_results$diffusion_est_m2_s * 0.0864
    # diffusion_results$diffusion_max_km2_day <- diffusion_results$diffusion_max_m2_s * 0.0864      
    RESULTS <- cbind(RESULTS, diffusion_results) #4
    RESULTS <- cbind(RESULTS, tau_p_results) #5
    RESULTS <- cbind(RESULTS, tau_v_results) #6
    speed_mean_results <- cbind(dof_results, ci_results)
    RESULTS <- cbind(RESULTS, speed_mean_results) #7
    covariates_results <- cbind(mean_elev_results, mean_dist_escape_results)
    RESULTS <- cbind(RESULTS, covariates_results) #9
    centroid_results <- cbind(long_results, lat_results)
    RESULTS <- cbind(RESULTS, centroid_results) #10
    
    
    ### assign as objects to environment ----
    # if need to inspect each results individually -> mainly used for debugging and troubleshooting the function
    assign("TIMESTAMP", as.data.frame(TIMESTAMP), envir = .GlobalEnv)
    assign("RESULTS", as.data.frame(RESULTS), envir = .GlobalEnv)
    assign("movement_model", as.data.frame(movement_model), envir = .GlobalEnv)
    assign("hr_size", as.data.frame(hr_size), envir = .GlobalEnv)
    assign("DOF_results", as.data.frame(DOF_results), envir = .GlobalEnv)
    assign("diffusion_results", as.data.frame(diffusion_results), envir = .GlobalEnv)
    assign("tau_p_results", as.data.frame(tau_p_results), envir = .GlobalEnv)
    assign("tau_v_results", as.data.frame(tau_v_results), envir = .GlobalEnv)
    # assign("dof_results", as.data.frame(dof_results), envir = .GlobalEnv)
    # assign("ci_results", as.data.frame(ci_results), envir = .GlobalEnv)
    assign("speed_mean_results", as.data.frame(speed_mean_results), envir = .GlobalEnv)
    # assign("mean_elev_results", as.data.frame(mean_elev_results), envir = .GlobalEnv)
    # assign("mean_dist_escape_results", as.data.frame(mean_dist_escape_results), envir = .GlobalEnv)
    assign("covariates_results", as.data.frame(covariates_results), envir = .GlobalEnv)
    # assign("long_results", as.data.frame(long_results), envir = .GlobalEnv)
    # assign("lat_results", as.data.frame(lat_results), envir = .GlobalEnv)
    # assign("centroid_results", as.data.frame(centroid_results), envir = .GlobalEnv)
    
    
    
    
  })
  
  return(RESULTS)
  
  
  
  
}






#...............................................................
# C. Run the function ----
#...............................................................


START <- Sys.time()
tic()

# i <- 1

RES <- list()

for(i in 1:length(tel_data)){
  
  # RES[[i]] <- window.HR(data[[i]][1:30,],
  RES[[i]] <- window_HR(tel_data[[i]],
                        dt <- 1 %#% 'day', #  %#% uses ctmm package to set the units, i.e. days
                        win <- 3 %#% 'day')
  
  save(RES, file = "./data/moving_window/fire_goats/moving_window_20241224.rda")
}




toc()
beep(8)

END <- Sys.time()







#________________________________________________________________________________
# debugging code/notes ----

# for troubleshooting and debugging errors


# Error in data.frame(..., check.names = FALSE) : 
#   arguments imply differing number of rows: 74, 0
nrow(movement_model)  # Check number of rows in movement_model
nrow(hr_size)          # Check number of rows in hr_size
nrow(DOF_results)      # Check number of rows in DOF_results
nrow(diffusion_results)  # Check number of rows in diffusion_results
nrow(tau_p_results)    # Check number of rows in tau_p_results
nrow(tau_v_results)    # Check number of rows in tau_v_results
nrow(speed_mean_results)  # Check number of rows in speed_mean_results
nrow(mean_elev_results)  # Check number of rows in mean_elev_results
nrow(mean_dist_escape_results)  # Check number of rows in mean_dist_escape_results
nrow(long_results)  # Check number of rows in centroid_long_results
nrow(lat_results)   # Check number of rows in centroid_lat_results
