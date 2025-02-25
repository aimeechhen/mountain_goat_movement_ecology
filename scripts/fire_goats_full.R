
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

# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")

#.........................................................................
# DATA PREP ----

# collar data
load("data/collar_data/collar_data_20241123.rda")
# specify which dataset its from
collar_data$data_type <- "original_data"

# update dataset to reflect the name changes 2025-02
collar_data$goat_name[collar_data$goat_name == "kid_rock" & collar_data$collar_id == "30561"] <- "selena_goatmez"
collar_data$goat_name[collar_data$goat_name == "rocky" & collar_data$collar_id == "30613" ] <- "goatileo"
collar_data$goat_name[collar_data$goat_name == "vertigoat" & collar_data$collar_id == "30567" ] <- "goatbriella"
collar_data$goat_name[collar_data$goat_name == "billy" & collar_data$collar_id == "30636" ] <- "ryan_goatsling"



dat <- collar_data[collar_data$goat_name %in% goats,]
dat$goat_name <- as.factor(dat$goat_name)


#.........................................................................
# new collar data
load("data/collar_data/new_collar_data_20250218.rda")
new_collar$collar_id <- as.factor(new_collar$collar_id)
# drop columns that isn't needed 
new_collar <- subset(new_collar, select = c(collar_id, timestamp, latitude, longitude, data_type))

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# remove cliff
goat_info <- goat_info[goat_info$goat_name %in% goats,]
new_collar <- merge(new_collar, goat_info[, c("goat_name", "goat_id", "collar_id")], by = "collar_id", all.x = TRUE)

# str(new_collar)
new_collar$goat_name <- as.factor(new_collar$goat_name)
# new_collar$timestamp <- as.POSIXct(new_collar$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
new_collar$date <- as.Date(new_collar$timestamp)
new_collar$year <- year(new_collar$timestamp)
new_collar$month <- month(new_collar$timestamp, label = FALSE) #label = false for numerical month
new_collar$day <- day(new_collar$timestamp)
new_collar$month_day <- format(new_collar$timestamp, "%m-%d")
new_collar$doy <- yday(new_collar$timestamp) #day of the year

# subset to new data and avoid overlap
# new_collar <- new_collar[new_collar$date >= "2023-10-01",]

# combine data for full data
# combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
full_data <- bind_rows(dat, new_collar)

# check for duplicates in the combined df, there probably (and should be) is because they have ~ month overlapping 
# full_data[duplicated(full_data[, c("collar_id", "timestamp", "latitude", "longitude")]), ]
# no duplicates, but the timestamps are off by like ~45 minutes and the position is fairly close

#clean up environment
rm(collar_data, goat_info, dat)


#...........................................................................
## fire period ----
#...........................................................................



# reassigning back to fire_goats for workflow, this now contains original and new fire goat collar data
fire_goats <- full_data
# fire_goats <- new_collar
#sort data by goat and timestamp
fire_goats <- fire_goats[order(fire_goats$collar_id, fire_goats$timestamp), ]

# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
fire_goats <- fire_goats[fire_goats$month_day >= fire_start & fire_goats$month_day <= fire_end, ] #21892 obs

#format names to match required for ctmm based on Movebank critera:
# create a column 
fire_goats$individual.local.identifier <- paste(fire_goats$collar_id, fire_goats$year, sep = "_")
# format names to match
fire_goats <- plyr::rename(fire_goats, c('latitude' = 'location.lat', 
                                         'longitude' = 'location.long'))




#Convert to telemetry
tel_data <- as.telemetry(fire_goats, mark.rm = TRUE)



#/////////////////////////////////////////////////////////////
# ANALYSES ----
#/////////////////////////////////////////////////////////////


# 2) Fit movement models ----
#create an empty list to store output
FITS <- list()

START <- Sys.time()
tic(msg = "starting movement models")

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
kittyR::meowR(sound = 3)
toc() #9.698333 mins; full = ~12.5min
END <- Sys.time()

dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)
# save(FITS,file="data/movement_model/fire_goat_fits_20241217.rda")
# load("data/movement_model/fire_goat_fits_20241224.rda")
save(FITS,file="data/movement_model/full_fire_goat_fits_20250219.rda")
# load("data/movement_model/full_fire_goat_fits_20250219.rda")



#____________________________________________________________
# 3) akdes home-range areas ----
# ~10.86min
START <- Sys.time()
tic(msg = "starting home range analysis")

AKDES <- akde(tel_data,FITS,weights=TRUE)

toc()
# beep(3)
kittyR::meowR(sound = 3)

dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)

#save rda:
# save(AKDES,file="data/home_range/fire_goat_akdes_20241217.rda")
# load("data/home_range/fire_goat_akdes_20241217.rda")
save(AKDES,file="data/home_range/full_fire_goat_akdes_20250219.rda")
load("data/home_range/full_fire_goat_akdes_20250219.rda")





#............................................................
## 3b) Create akde UDs as rasters and shapefiles ----

# Fit_Mods.R
# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#save UD as raster:
# dir.create("data/home_range/fire_goat/UD", recursive = TRUE, showWarnings = TRUE)
dir.create("data/home_range/full_fire_goat/UD", recursive = TRUE, showWarnings = TRUE)


#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/full_fire_goat/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}

#save 95% range estimate as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
dir.create("data/home_range/full_fire_goat/shp", recursive = TRUE, showWarnings = TRUE)

for (name in names(AKDES)) {
  shp_path <- file.path("data/home_range/full_fire_goat/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp_path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("data/home_range/fire_goat/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }
kittyR::meowR(sound = 3)



#___________________________________________________________________________
# Speed analysis ----

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
                           robust = TRUE, units = FALSE)#, 
  # trace = TRUE, cores = 8)
  # estimate instantaneous speed
  SPEEDS_INSTA[[i]] <- speeds(object = DATA, CTMM = FIT,
                              robust = TRUE, units = FALSE)#,
  # trace = TRUE, cores = 8)
  
  
}

#rename for convenience
names(SPEED_MEAN) <- names(tel_data)
names(SPEEDS_INSTA) <- names(tel_data)

toc() #~26 min
# beep(6)
kittyR::meowR(sound = 3)
# 
# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.


# save(SPEED_MEAN, file = "./data/movement_model/fire_goat_speed_mean_20241217.rda")
# save(SPEEDS_INSTA, file = "./data/movement_model/fire_goat_speeds_insta_20241217.rda")
# load(file = "./data/movement_model/fire_goat_speed_mean_20241217.rda")
# load(file = "./data/movement_model/fire_goat_speeds_insta_20241217.rda")
# Warning messages:
#   1: In as.POSIXlt.POSIXct(x, tz) : unknown timezone 'PDT'


save(SPEED_MEAN, file = "./data/movement_model/full_fire_goat_speed_mean_20250219.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/full_fire_goat_speeds_insta_20250219.rda")
load(file = "./data/movement_model/full_fire_goat_speed_mean_20250219.rda")
load(file = "./data/movement_model/full_fire_goat_speeds_insta_20250219.rda")





#____________________________________________________________________
# 5) rsf ----

library(raster)
# import raster as a raster object for rsf.fit, won't work with a spatraster
elev_25m = raster('data/rasters/elev_25m.tif')
dist_escape_25m = raster('data/rasters/dist_escape_25m.tif')
# Some rasters are not loaded in RAM and may be slow to process. See help("raster::readAll").

# create a list of rasters
r_list <- list(elevation = elev_25m, 
               dist_escape = dist_escape_25m)

# initialize empty list for storing
rsf <- list()

# Fit RSF models (needs to be raster object and not spatraster)
START <- Sys.time()
tic(msg = "rsf analysis")

for(i in 1:length(tel_data)){
  cat(bgBlue("Currently on animal", i, " of", length(tel_data)), "\n")
  #Extract individual
  DATA <- tel_data[[i]]
  AKDE <- AKDES[[i]]
  
  # Fit rsf
  rsf[[i]] <- rsf.fit(DATA,AKDE, R=r_list)
}
names(rsf) <- names(tel_data)

toc() # ~12.55 hrs
# beep(3)
kittyR::meowR(sound = 3)

dir.create("./data/rsf/", recursive = TRUE, showWarnings = TRUE)
# save(rsf, file = "./data/rsf/fire_goat_rsf_20241220.rda")
# load("./data/rsf/fire_goat_rsf_20241220.rda")
save(rsf, file = "./data/rsf/full_fire_goat_rsf_20250219.rda")
# load("./data/rsf/full_fire_goat_rsf_20250219.rda")





#..................................................................
# WINDOW PREP ----
#..................................................................

# moving window analysis only, no extractions, save outputs, very basic on full data fire goats

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


# Run data prep for full data first, do not run fire_goats, that is for only during the fire period across all years
# using full data for 6 goats for window analysis
full_data <- new_collar
#sort data by goat and timestamp
full_data <- full_data[order(full_data$collar_id, full_data$timestamp), ]




# convert to a ctmm telemetry object (remember, this tel_data differs from the one above!!)
tel_data <- as.telemetry(full_data)

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
r_list <- list(elevation = elev, 
               dist_escape = dist_escape)


#create folders
folder_list <- c("fits_20250219", 
                 "akdes_20250219",
                 "mean_speed_20250219",
                 "insta_speed_20250219",
                 "covariates_20250219",
                 "rsf_20250220")

## set directory path ----
# dir_path <- "./data/window_analysis/fire_goats/fire_period/basic/" # for only during the fire period with the 6 fire goats
# dir_path <- "./data/window_analysis/fire_goats/basic/" # for all of the data but only of the 6 fire goats
# dir_path <- "./data/window_analysis/" # for all goats for all data
# dir_path <- "./data/window_analysis/fire_goats/full_data/" # for full data for the 6 fire goats
dir_path <- "./data/window_analysis/fire_goats/new_data/" 
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







#[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
# 7) window analysis ----
#[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

# test dates
# fire_start <- '2023-07-01' # doy = 203
# fire_end <- '2023-07-31' # doy = 299
# test_dat <- full_data[full_data$date >= fire_start & full_data$date <= fire_end, ]
# tel_data <- as.telemetry(test_dat)
# DATA <- tel_data[[1]]
# i <- 1
# tel_data <- tel_data[1:2]

dt <- 1 %#% 'day' #  %#% uses ctmm package to set the units, i.e. days
win <- 3 %#% 'day'

tic(msg = "window analysis")
START_ANALYSIS <- Sys.time()

#.........................................
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
  
  # Set up list to store
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
                        "for mountain goat:", DATA@info[1]), "\n"))
    cat(paste0("Number of fixes in window segment subset: ", nrow(SUBSET), "\n"))
    
    # Process the subset if data is present
    tryCatch({
      message(green("movement models"))
      GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
      FITS <- try(ctmm.select(SUBSET, GUESS, trace = 3, cores = -1))
      
      
      if (inherits(FITS, "ctmm")) {
        message(green("home range analyses"))
        AKDES <- akde(SUBSET, FITS, weights = TRUE)
        
        # message(yellow("SPEED analyses"))
        # tic(msg = "speed analysis")
        # SPEED_MEAN <- speed(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -1)
        # SPEEDS_INSTA <- speeds(object = SUBSET, CTMM = FITS, robust = TRUE, units = FALSE, cores = -1)
        # toc()
        
        message(green("rsf analyses"))
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
        # 
        
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
  message(cyan(bgWhite(paste("saving output for goat", DATA@info[1]))))
  # saveRDS(fits, file = paste0(dir_path, "fits_20250219/fits_", DATA@info[1], ".rds"))
  # saveRDS(akdes, file = paste0(dir_path, "akdes_20250219/akdes_", DATA@info[1], ".rds"))
  # saveRDS(speed_mean, file = paste0(dir_path, "mean_speed_20250219/mean_speed_", DATA@info[1], ".rds"))
  # saveRDS(speeds_insta, file = paste0(dir_path, "insta_speed_20250219/insta_speed_", DATA@info[1], ".rds"))
  # saveRDS(covariates, file = paste0(dir_path, "covariates_20250219/covariates_", DATA@info[1], ".rds")) # remember this is a df and not a list
  saveRDS(rsf, file = paste0(dir_path, "rsf_20250219/rsf_", DATA@info[1], ".rds"))
  
  
  
  # clean up environment
  # rm(FITS, 
  #    AKDES)#,#)
  #    # SPEED_MEAN, SPEEDS_INSTA)
  #    # RSF)
  gc() # free up computational resources
  
  # END OF OUTER LOOP, START AT TOP WITH A NEW GOAT
  
  beep(8)
}


toc() # 5.2 min, 5 min, 5.6min
# 15min, 33min for all fire goats for cc fire period only
# for all data = 15.5h (but was running 2 sessions and took jan 19-24)
# speed for all data = ~17 days, 3 hours, 17 minutes
# new  data, no speed, no rsf = 


END_ANALYSIS <- Sys.time()



#////////////////////////////////////////////////
# END WINDOW ANALYSIS ----
#////////////////////////////////////////////////



# RESULTS ----

## Set directory path ----
dir_path <- "./data/window_analysis/fire_goats/new_data/" 

folder_path <- paste0(dir_path, "akdes_20250219")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
AKDE <- rds_dat

# Set the folder path
folder_path <- paste0(dir_path, "fits_20250219")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
FIT <- rds_dat


folder_path <- paste0(dir_path, "covariates_20250219")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
covariates <- rds_dat


# Set the folder path
folder_path <- paste0(dir_path, "rsf_20250220")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
RSF <- rds_dat


#clean up environment
rm(rds_dat, rds_list, rds_files, folder_path)





#______________________________________________________________________________
# Check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)


# check fitted models units
summary_outputs <- data.frame()
for (i in seq_along(FIT)) {
  summary <- summary(FIT[[i]], units = FALSE)$CI # using SI units
  summary_outputs <- rbind(summary_outputs, 
                           data.frame(Var1 = names(table(rownames(summary))), 
                                      Freq = as.integer(table(rownames(summary)))))
}

summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
summary_outputs

# Var1 Freq
# 1             area (square meters) 2986
# 2 diffusion (square meters/second) 1488
# 3            speed (meters/second)  973
# 4                      τ (seconds)  962
# 5               τ[decay] (seconds)   11
# 6              τ[period] (seconds)   11
# 7            τ[position] (seconds)  515




#........................................................................
# Check UD units

hr_size <- data.frame()
for (i in 1:length(AKDE)) {
  cat("Processing AKDE element:", i, "\n")  # Print current loop iteration
  #extract the home range area statistics summary
  tryCatch({
    summary <- as.data.frame(summary(AKDE[[i]])$CI, units = FALSE) # default is square meters
    summary$identifier <- names(AKDE[i])
    #bind the summary to the dataframe
    hr_size <- rbind(hr_size, summary)
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}
# inspect if all the units are the same (units = km²), units = FALSE, default units are m^2, but in the for loop with units = FALSE, it changed it to km² instead of keeping it in m²
hr_size

table(sub(".*\\((.*)\\).*", "\\1", rownames(hr_size)))
# hectares square kilometers     square meters 
# 5986              6471               371 

# units are still mixed despite units = FALSE, therefore need to account for it



#____________________________________________________________________________
# create dataframe for results ----

win_results <- data.frame(goat_name = character(length(AKDE)))

for (i in 1:length(AKDE)) {
  cat("Processing AKDE element:", i, "\n")
  tryCatch({
    # extract item from list
    win_results$collar_id[i] <- AKDE[[i]]@info$identity
    # get timestamp from the list element name
    win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(AKDE)[i])
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}

library(lubridate)
win_results$window_start <- as.POSIXct(win_results$window_start, format = "%Y-%m-%d %H:%M:%S")
win_results$date <- as.Date(win_results$window_start)
win_results$year <- year(win_results$window_start)
win_results$month <- month(win_results$window_start, label = FALSE) #label = false for numerical month
win_results$day <- day(win_results$window_start)
win_results$month_day <- format(win_results$window_start, "%m-%d")
win_results$doy <- yday(win_results$window_start) #day of the year

# i <- 1

#.............................................................
# home range results (units = km^2) ----
# units are not consistent, therefore need to account for it
for (i in 1:length(AKDE)) {
  cat("Processing AKDE element:", i, "\n")
  
  tryCatch({
    
    # subset akde results for one window
    akdesum <- as.data.frame(summary(AKDE[[i]], units = FALSE)$CI)
    
    # using km^2 as default SI units, convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
    if ("area (square kilometers)" %in% rownames(akdesum)) {
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- akdesum[c("low", "est", "high")]
      
    } else if ("area (square meters)" %in% rownames(akdesum)) {
      # convert m^2 to km^2 .-. divide by 1e6
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- akdesum[c("low", "est", "high")] / 1e6    
      
    } else if ("area (hectares)" %in% rownames(akdesum)) { 
      # convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- akdesum[c("low", "est", "high")] / 100
      
    } else { 
      cat("no entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- NA
    }
    
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}





#___________________________________________________________________________
# movement results ----

i <- 3
FIT <- FITS[1:100]


# win_results <- data.frame(goat_name = character(length(FIT)))

tic()
for (i in 1:length(FIT)) {
  # for (i in 1:min(100, length(FIT))) { # testing with subset

  # extract item from list
  win_results$goat_name[i] <- FIT[[i]]@info$identity
  # get timestamp from the list element name
  win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(FIT)[[i]])
  
  
  win_results$movement_model[i] <- summary(FIT[[i]], units = FALSE)$name
  win_results[i, c("DOF_mean", "DOF_area", "DOF_diffusion", "DOF_speed")] <- summary(FIT[[i]], units = FALSE)$DOF[c("mean", "area", "diffusion", "speed")]
  
  
  #..............................................
  # diffusion ----
  
  # si units has it m^2/sec, convert to km^2/day 0.0864 or 8.64e-2
  if ("diffusion (square meters/second)" %in% rownames(summary(FIT[[i]], units = FALSE)$CI))  {
    # Update the corresponding row in the data frame
    win_results[i, c("diffusion_min_km2_day",
                     "diffusion_est_km2_day",
                     "diffusion_max_km2_day")] <- summary(FIT[[i]], units = FALSE)$CI["diffusion (square meters/second)",
                                                                                      c("low", "est", "high")] * 0.0864
    
    
  } else {
    cat("no diffusion entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("diffusion_min_km2_day", "diffusion_est_km2_day", "diffusion_max_km2_day")] <- NA
  }
  
  
  
  
  #..............................................
  # tau_p ----
  # si units are seconds, convert into days
  if ("τ[position] (seconds)" %in% rownames(summary(FIT[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_p_min_s",
                     "tau_p_est_s",
                     "tau_p_max_s")] <- summary(FIT[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                              c("low", "est", "high")]
    
    #convert to seconds to days
    win_results[i, c("tau_p_min_day",
                     "tau_p_est_day",
                     "tau_p_max_day")] <- summary(FIT[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                              c("low", "est", "high")] / 86400
    
    
    
  } else {
    cat("no tau p entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_p_min_s", "tau_p_est_s", "tau_p_max_s")] <- NA
    win_results[i, c("tau_p_min_day", "tau_p_est_day", "tau_p_max_day")] <- NA
  }
  
  
  
  #..............................................
  # tau_v ----
  # si units are seconds, convert to minutes
  if ("τ[velocity] (seconds)" %in% rownames(summary(FIT[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_v_min_s",
                     "tau_v_est_s",
                     "tau_v_max_s")] <- summary(FIT[[i]], units = FALSE)$CI["τ[velocity] (seconds)",
                                                                            c("low", "est", "high")]
    #convert to seconds to minutes
    win_results[i, c("tau_v_min_min", 
                     "tau_v_est_min", 
                     "tau_v_max_min")] <- summary(FIT[[i]], units = FALSE)$CI["τ[velocity] (seconds)", 
                                                                              c("low", "est", "high")] / 60
    
  } else { 
    cat("no tau v entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_v_min_s", "tau_v_est_s", "tau_v_max_s")] <- NA
    win_results[i, c("tau_v_min_min", "tau_v_est_min", "tau_v_max_min")] <- NA
  }
  
  
  fit_mu <- as.data.frame(FIT[[i]]$mu)
  centroid_sf <- st_as_sf(fit_mu, coords = c('x', 'y'), crs = FIT[[i]]@info$projection)
  centroid_sf <- st_coordinates(st_transform(centroid_sf, crs = 4326)) # reproject and grab the coordinates
  win_results[i, c("centroid_long", "centroid_lat")] <- centroid_sf[1, c("X", "Y")]
  
  
  # bls ----

  # If tau_p and tau_v have values, then calculate BLS
  if (!is.na(win_results$tau_p_est_s[i]) && !is.na(win_results$tau_v_est_s[i])) {
    sigma_p <- ctmm:::area.covm(FIT[[i]]$sigma)
    
    bls_min <- sqrt((tau_v_min_s/tau_p_min_s) *  sigma_p)
    bls_est <- sqrt((tau_v_est_s/tau_p_est_s) *  sigma_p)
    bls_max <- sqrt((tau_v_max_s/tau_p_max_s) *  sigma_p)
    
  } else {
    cat("no bls entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("bls_min", "bls_est", "bls_max")] <- NA
  }
  
  
  # end of loop
  
}

toc() #~4min





