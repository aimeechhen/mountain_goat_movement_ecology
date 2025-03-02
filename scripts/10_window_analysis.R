
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


#...............................................................
# A. Import data ----
#...............................................................

# load("data/collar_data/collar_data_20240703.rda")
# load("data/collar_data/collar_data_20241123.rda")

# Import combined collar data (original + new)
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)




#...............................................................
# WINDOW DATA CHECK & PREP  ----
#...............................................................

# How to check if there are missing dates in your dataframe by counting the number of recordings and the number of days and comparing them, and troubleshoot the missing dates error

# Convert 'timestamp' to Date format
tel.dat$date <- as.Date(tel.dat$timestamp)

# Get a list of unique dates for each collar ID
date_counts <- aggregate(date ~ individual.local.identifier, tel.dat, FUN = function(x) length(unique(x)))

# Get the total number of unique dates in the dataset for each collar ID
total_dates <- aggregate(date ~ individual.local.identifier, tel.dat, FUN = function(x) length(unique(x)))

# Merge the two data frames to compare if there is at least one timestamp for every day for each collar ID
result <- merge(date_counts, total_dates, by = "individual.local.identifier", suffixes = c("_count", "_total"))

# Check completeness
result$complete <- result$date_count == result$date_total

# Print results
print(result)



#...................................................
# the number of dates counted are not the same for all the collars

##How to check dates which are missing/duplicated and for each collar ----

# Get unique collar IDs
collar_ids <- unique(tel.dat$individual.local.identifier)

# Loop through each collar ID and check for missing or duplicated dates
for (id in collar_ids) {
  # Subset data for the current collar ID
  subset_data <- tel.dat[tel.dat$individual.local.identifier == id, ]
  
  # Get unique dates for the current collar ID
  unique_dates <- unique(subset_data$date)
  
  # Check for missing dates
  missing_dates <- setdiff(seq(min(unique_dates), max(unique_dates), by = "day"), unique_dates)
  
  # Convert missing dates back to Date format
  missing_dates <- as.Date(missing_dates, origin = "1970-01-01")
  
  # Check for duplicated dates
  duplicated_dates <- unique_dates[duplicated(unique_dates)]
  
  # Convert duplicated dates back to Date format
  duplicated_dates <- as.Date(duplicated_dates, origin = "1970-01-01")
  
  # Print results
  cat("Collar ID:", id, "\n")
  cat("Missing Dates:", ifelse(length(missing_dates) > 0, paste(missing_dates, collapse = ", "), "None"), "\n")
  cat("Duplicated Dates:", ifelse(length(duplicated_dates) > 0, paste(duplicated_dates, collapse = ", "), "None"), "\n\n")
}

# Collar ID: 30613 
# Missing Dates: 2023-03-12 
# 
# Collar ID: 30642 
# Missing Dates: 2023-09-23, 2023-09-24, 2023-09-25, 2023-09-26, 2023-09-27 










#~~~~~~~~~~~~~~~~~~~~~~~~~~
# subset to test window analysis
# goats <- c("30575", "30613")
# goat_data <- goat_data[goat_data$collar_id %in% goats,]
# fire_start <- '2023-07-22' # doy = 203
# fire_end <- '2023-10-26' # doy = 299
# goat_data <- goat_data[goat_data$date >= fire_start & goat_data$date <= fire_end, ]
# dat <- plyr::rename(goat_data, c('collar_id' = 'individual.local.identifier',
#                                    'latitude' = 'location.lat',
#                                    'longitude' = 'location.long'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~


#format names to match required for ctmm based on Movebank critera:
dat <- plyr::rename(goat_data, c('collar_id' = 'individual.local.identifier',
                                 'latitude' = 'location.lat',
                                 'longitude' = 'location.long'))

#Convert to telemetry
tel_data <- as.telemetry(dat, mark.rm = TRUE)



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


# create a list of rasters
r_list <- list(elev = elev, 
               dist_escape = dist_escape)


#create folders
folder_list <- c("fits_20250301", 
                 "akdes_20250301",
                 # "mean_speed_20250301",
                 # "insta_speed_20250301",
                 # "covariates_20250301",
                 "rsf_20250220")

## set directory path ----
# dir_path <- "./data/window_analysis/fire_goats/fire_period/basic/" # for only during the fire period with the 6 fire goats
# dir_path <- "./data/window_analysis/fire_goats/basic/" # for all of the data but only of the 6 fire goats
# dir_path <- "./data/window_analysis/" # for all goats for all data
# dir_path <- "./data/window_analysis/fire_goats/full_data/" # for full data for the 6 fire goats
dir_path <- "./data/window_analysis/combined_data/" 
# dir_path <- "./data/window_analysis/test/" # for full data, fire goats 



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
# C. WINDOW ANALYSIS ----
#//////////////////////////////////////////////////////////


# test dates
# fire_start <- '2023-07-01' # doy = 203
# fire_end <- '2023-07-31' # doy = 299
# test_dat <- goat_data[goat_data$date >= fire_start & goat_data$date <= fire_end, ]
# tel_data <- as.telemetry(test_dat)
# DATA <- tel_data[[1]]
# i <- 1
# tel_data <- tel_data[1:2]

dt <- 1 %#% 'day' #  %#% uses ctmm package to set the units, i.e. days
win <- 3 %#% 'day'

tic(msg = "window analysis")
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
  
  # Set up list to store, running certain analyses at a time, commenting out the rest
  fits <- list()
  akdes <- list()
  # speed_mean <- list()
  # speeds_insta <- list()
  # covariates <- data.frame(
  #   collar_id = character(length(times)),
  #   window_start <- as.POSIXct(rep(NA, length(times)), tz = "America/Vancouver"),
  #   window_end <- as.POSIXct(rep(NA, length(times)), tz = "America/Vancouver"),
  #   n_fixes = numeric(length(times)),
  #   mean_elev = numeric(length(times)),
  #   mean_dist_escape = numeric(length(times))
  # )
  rsf <- list()
  
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
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FITS <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      
      if (inherits(FITS, "ctmm")) {
        cat(bgBlue("processing home range","\n"))
        AKDES <- akde(SUBSET, FITS, weights = TRUE)
        
        # cat(bgBlue("movement models","\n"))
        # tic(msg = "speed analysis")
        # SPEED_MEAN <- speed(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -1)
        # SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -1)
        # toc()
        
        cat(bgGreen("processing rsf","\n"))
        tic(msg = "rsf analysis")
        RSF <- rsf.fit(SUBSET, AKDES, R=r_list)
        toc() #~15min each
        
        
        
        # store models/UDs in a list, name the entry based on goat name and subset window start date, not the times[i] as that is in unix format
        fits[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- FITS
        akdes[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- AKDES
        # speed_mean[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEED_MEAN
        # speeds_insta[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- SPEEDS_INSTA
        rsf[[paste0(DATA@info[1], "_", as.character(WINDOW_START))]] <- RSF
        
        # # # habitat variables ----
        # SUBSET_SF <- as.sf(SUBSET)
        # SUBSET_SF <- st_transform(SUBSET_SF, crs = st_crs("epsg:4326"))
        # # #convert sf into spatvector object to be able to extract values, not needed if working with rasterlayer
        # # locations <- vect(SUBSET_SF)
        # # extract mean habitat values for each moving window segment (meters)
        # covariates$collar_id[i] <- DATA@info$identity
        # covariates$window_start[i] <- WINDOW_START
        # covariates$window_end[i] <- WINDOW_END
        # covariates$n_fixes[i] <- nrow(SUBSET)
        # # covariates$mean_elev[i] <- mean(raster::extract(elev, locations)[,2])
        # # covariates$mean_dist_escape[i] <- mean(raster::extract(dist_escape, locations)[,2])
        # covariates$mean_elev[i] <- mean(raster::extract(elev, SUBSET_SF))         # changing locations to SUBSET_SF because of rasterlayer object and indexing isnt necessary
        # covariates$mean_dist_escape[i] <- mean(raster::extract(dist_escape, SUBSET_SF))


        # END OF INNER LOOP
        
        
      }
    }, error = function(e) {
      cat("Error during processing for window segment:", i, "-", e$message, "\n")
    })
    
    # Combine the lists into a dataframe and set the column names to match
    # covariates <- setNames(as.data.frame(do.call(cbind, list(collar_id, window_start, window_end, n_fixes, mean_elev, mean_dist_escape))),
    #                        c("collar_id", "window_start", "window_end", "n_fixes", "mean_elev", "mean_dist_escape"))
    
    
  }
  
  # save all the outputs as a rds for future analysis ----
  cat(bgWhite(magenta(paste("saving output for goat", DATA@info[1], 
                            goat_data$goat_name[which(goat_data$collar_id == DATA@info[1])][1]))), "\n")
  saveRDS(fits, file = paste0(dir_path, "fits_20250301/fits_", DATA@info[1], ".rds"))
  saveRDS(akdes, file = paste0(dir_path, "akdes_20250301/akdes_", DATA@info[1], ".rds"))
  # saveRDS(speed_mean, file = paste0(dir_path, "mean_speed_20250301/mean_speed_", DATA@info[1], ".rds"))
  # saveRDS(speeds_insta, file = paste0(dir_path, "insta_speed_20250301/insta_speed_", DATA@info[1], ".rds"))
  # saveRDS(covariates, file = paste0(dir_path, "covariates_20250301/covariates_", DATA@info[1], ".rds")) # remember this is a df and not a list
  saveRDS(rsf, file = paste0(dir_path, "rsf_20250301/rsf_", DATA@info[1], ".rds"))
  
  
  
  # clean up environment
  rm(FITS,
     AKDES,
     # SPEED_MEAN, SPEEDS_INSTA,
     RSF)
  gc() # free up computational resources
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW GOAT
  toc()
  beep(8)
}


toc() # 5.2 min, 5 min, 5.6min
# 15min, 33min for all fire goats for cc fire period only
# for all data = 15.5h (but was running 2 sessions and took jan 19-24)
# speed for all data = ~17 days, 3 hours, 17 minutes
# new  data, no speed, no rsf = 
kittyR::meowR(sound = 3)

END_window <- Sys.time()



#////////////////////////////////////////////////
# END WINDOW ANALYSIS
#////////////////////////////////////////////////



