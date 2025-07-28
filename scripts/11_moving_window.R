
library(ctmm)
library(raster)
library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tictoc)
library(beepr)
library(lubridate)
library(crayon)
library(terra)
library(sf)


#...............................................................
# A. Import data ----
#...............................................................

# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# formatting
goat_data$goat_name <- as.factor(goat_data$goat_name)



# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~
# # subset to test moving window
# goats <- c("30575", "30613")
# goat_data <- goat_data[goat_data$collar_id %in% goats,]
# 
# 
# fire_start <- '2023-08-01' # doy = 203
# fire_end <- '2023-08-15' # doy = 299
# goat_data <- goat_data[goat_data$date >= fire_start & goat_data$date <= fire_end, ]
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~


#format names to match required for ctmm based on Movebank critera:
dat <- plyr::rename(goat_data, c('collar_id' = 'individual.local.identifier',
                                 'latitude' = 'location.lat',
                                 'longitude' = 'location.long'))

#Convert to telemetry
tel_data <- as.telemetry(dat, mark.rm = TRUE)




# #...............................................................
# # WINDOW DATA CHECK & PREP  ----
# #...............................................................
# 
# # How to check if there are missing dates in your dataframe by counting the number of recordings and the number of days and comparing them, and troubleshoot the missing dates error
# 
# 
# # Convert 'timestamp' to Date format
# goat_data$date <- as.Date(goat_data$timestamp)
# 
# # Get a list of unique dates for each collar ID
# date_counts <- aggregate(date ~ collar_id, goat_data, FUN = function(x) length(unique(x)))
# 
# # Get the total number of unique dates in the dataset for each collar ID
# total_dates <- aggregate(date ~ collar_id, goat_data, FUN = function(x) length(unique(x)))
# 
# # Merge the two data frames to compare if there is at least one timestamp for every day for each collar ID
# result <- merge(date_counts, total_dates, by = "collar_id", suffixes = c("_count", "_total"))
# 
# # Check completeness
# result$complete <- result$date_count == result$date_total
# 
# # Print results
# print(result)
# 
# 
# 
# #...................................................
# # the number of dates counted are not the same for all the collars
# 
# ##How to check dates which are missing/duplicated and for each collar ----
# 
# # Get unique collar IDs
# collar_ids <- unique(goat_data$collar_id)
# 
# # Loop through each collar ID and check for missing or duplicated dates
# for (id in collar_ids) {
#   # Subset data for the current collar ID
#   subset_data <- goat_data[goat_data$collar_id == id, ]
#   
#   # Get unique dates for the current collar ID
#   unique_dates <- unique(subset_data$date)
#   
#   # Check for missing dates
#   missing_dates <- setdiff(seq(min(unique_dates), max(unique_dates), by = "day"), unique_dates)
#   
#   # Convert missing dates back to Date format
#   missing_dates <- as.Date(missing_dates, origin = "1970-01-01")
#   
#   # Check for duplicated dates
#   duplicated_dates <- unique_dates[duplicated(unique_dates)]
#   
#   # Convert duplicated dates back to Date format
#   duplicated_dates <- as.Date(duplicated_dates, origin = "1970-01-01")
#   
#   # Print results
#   cat("Collar ID:", id, "\n")
#   cat("Missing Dates:", ifelse(length(missing_dates) > 0, paste(missing_dates, collapse = ", "), "None"), "\n")
#   cat("Duplicated Dates:", ifelse(length(duplicated_dates) > 0, paste(duplicated_dates, collapse = ", "), "None"), "\n\n")
# }
# 
# # Collar ID: 30613 
# # Missing Dates: 2023-03-12 
# # 
# # Collar ID: 30642 
# # Missing Dates: 2023-09-23, 2023-09-24, 2023-09-25, 2023-09-26, 2023-09-27 
# 
# # combined data
# # Duplicated Dates: None
# 









# ----

#..................................................................
# B. WINDOW PREP ----
#..................................................................

# moving window analysis only, no extractions, save outputs, very basic

# Import habitat rasters as raster for rsf.fit() from ctmm package, spatrasters do not work with the function
# issue with reprojecting the raster, import as spatraster, reproject, then convert to raster as workaround
# elev <- raster('./data/rasters/elev_25m.tif')
# dist_escape <- raster('./data/rasters/dist_escape_25m.tif')
elev <- rast('./data/rasters/elev_25m.tif')
dist_escape <- rast('./data/rasters/dist_escape_25m.tif')

# reproject into lat/long crs
elev <- project(elev, "EPSG:4326")
dist_escape <- project(dist_escape, "EPSG:4326")
# convert into raster
elev <- raster(elev)
dist_escape <- raster(dist_escape)
# scale rasters
elev_scaled <- scale(elev)
dist_escape_scaled <- scale(elev)

# create a list of rasters
r_list <- list(elev = elev, 
               dist_escape = dist_escape,
               elev_scaled = elev_scaled,
               dist_escape_scaled = dist_escape_scaled)


#create folders
saving_date <- "20250728"
folder_list <- c(paste0("fits_", saving_date), 
                 paste0("akdes_", saving_date),
                 paste0("mean_speed_", saving_date),
                 paste0("insta_speed_", saving_date),
                 paste0("covariates_", saving_date),
                 paste0("rsf_", saving_date))

## set directory path ----
dir_path <- "./data/moving_window/combined_data/"



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
# iv. fit rsf + save output




#//////////////////////////////////////////////////////////
# C. MOVING WINDOW ----
#//////////////////////////////////////////////////////////

dt <- 1 %#% 'day' #  %#% uses ctmm package to set the units, i.e. days
win <- 3 %#% 'day'

tic(msg = "moving window")
START_window <- Sys.time()

#.........................................
# outer for loop ----

for(g in 1:length(tel_data)){
  tic(msg = "single goat")
  # subset an individual out
  DATA <- tel_data[[g]]
  
  #.......................................................................
  # Set up the window segments
  # Generate a "list" or vector of all the window start times with a 3-day segment for the individual, using t column instead of timestamp column because of the nature of how this is set up and using ctmm objects
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
  
  # Set up list to store outputs each window results for an individual
  fits <- list()
  akdes <- list()
  speed_mean <- list()
  speeds_insta <- list()
  rsf <- list()
  # set up dataframe to store outputs, for an individual, window results will be filled in accordingly 
  # NOTE: use '=' instead of '<-' when making df, '<-' causes it to run the operation in the column
  covariates <- data.frame( 
    collar_id = character(length(times)),
    window_start = as.POSIXct(rep(NA, length(times)), tz = "America/Vancouver"),
    window_end = as.POSIXct(rep(NA, length(times)), tz = "America/Vancouver"),
    n_fixes = numeric(length(times)),
    mean_elev = numeric(length(times)),
    mean_dist_escape = numeric(length(times)), 
    mean_elev_scaled = numeric(length(times)),
    mean_dist_escape_scaled = numeric(length(times))
    
  )
  

  
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
                        "for mountain goat:", DATA@info[1]), "-", goat_data$goat_name[which(goat_data$collar_id == DATA@info[1])][1], "\n"))
    cat(bgMagenta(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n")))
    
    # Process the subset if data is present
    tryCatch({
      cat(bgBlue("processing movement models","\n"))
      GUESS <- ctmm.guess(SUBSET, CTMM=ctmm(error=FALSE), interactive=FALSE)
      FITS <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -5))
      
      
      if (inherits(FITS, "ctmm")) {
        cat(bgBlue("processing home range","\n"))
        AKDES <- akde(SUBSET, FITS, weights = TRUE)
        
        cat(bold(bgYellow("processing speeds","\n")))
        tic(msg = "speed analysis")
        # estimate mean speed (may be a mix of OU and OUF, so using robust = true)
        SPEED_MEAN <- speed(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -5)
        SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -5)
        toc()
        
        cat(bgGreen("processing rsf","\n"))
        tic(msg = "rsf analysis")
        RSF <- rsf.fit(SUBSET, AKDES, R=r_list)
        toc() #~15min each
        
        
        # store models/UDs in a list, name the entry based on goat name and subset window start date, not the times[i] as that is in unix format
        fits[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FITS
        akdes[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED_MEAN
        speeds_insta[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEEDS_INSTA
        rsf[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        
        # # habitat variables ----
        cat(bgBlue("processing covariates","\n"))
        SUBSET_SF <- as.sf(SUBSET)
        SUBSET_SF <- st_transform(SUBSET_SF, crs = st_crs("epsg:4326")) # project to lat/long crs
        #extract and fill in window data
        covariates$collar_id[i] <- DATA@info$identity
        covariates$window_start[i] <- WINDOW_START
        covariates$window_end[i] <- WINDOW_END
        covariates$n_fixes[i] <- nrow(SUBSET)
        # extract mean habitat values for each moving window segment (meters)
        covariates$mean_elev[i] <- mean(raster::extract(elev, SUBSET_SF))         
        covariates$mean_dist_escape[i] <- mean(raster::extract(dist_escape, SUBSET_SF))
        covariates$mean_elev_scaled[i] <- mean(raster::extract(elev_scaled, SUBSET_SF))         
        covariates$mean_dist_escape_scaled[i] <- mean(raster::extract(dist_escape_scaled, SUBSET_SF))
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", i, "-", e$message, "\n")
    })
    
    toc()
    
  }
  
  # save outputs ----
  # save the lists containing each window output as a rds for future analysis
  cat(bold(bgWhite(magenta(paste("saving output for goat", DATA@info[1], 
                                 goat_data$goat_name[which(goat_data$collar_id == DATA@info[1])][1]))), "\n"))
  saveRDS(fits, file = paste0(dir_path, paste0("fits_", saving_date, "/fits_"), DATA@info[1], ".rds"))
  saveRDS(akdes, file = paste0(dir_path, paste0("akdes_", saving_date, "/akdes_"), DATA@info[1], ".rds"))
  saveRDS(speed_mean, file = paste0(dir_path, paste0("mean_speed_", saving_date, "/mean_speed_"), DATA@info[1], ".rds"))
  saveRDS(speeds_insta, file = paste0(dir_path, paste0("insta_speed_", saving_date, "/insta_speed_"), DATA@info[1], ".rds"))
  saveRDS(rsf, file = paste0(dir_path, paste0("rsf_", saving_date, "/rsf_"), DATA@info[1], ".rds"))
  # save df 
  saveRDS(covariates, file = paste0(dir_path, paste0("covariates_", saving_date, "/covariates_"), DATA@info[1], ".rds")) # remember this is a df and not a list
  
  # clean up environment
  rm(FITS,
     AKDES,
     SPEED_MEAN, SPEEDS_INSTA,
     RSF)
  gc() # free up computational resources
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW INDIVIDUAL
  toc()
}


toc() 

END_window <- Sys.time()



#////////////////////////////////////////////////
# END 
#////////////////////////////////////////////////



