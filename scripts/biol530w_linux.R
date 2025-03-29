




library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)
library(lubridate)

#...........................................................
# Import data ----
#...........................................................

# import original collar data
load("data/collar_data/collar_data_20241123.rda")
# specify which dataset its from
collar_data$data_type <- "original_data"

# update dataset to reflect the name changes 2025-02
collar_data$goat_name[collar_data$goat_name == "kid_rock" & collar_data$collar_id == "30561"] <- "selena_goatmez"
collar_data$goat_name[collar_data$goat_name == "rocky" & collar_data$collar_id == "30613" ] <- "goatileo"
collar_data$goat_name[collar_data$goat_name == "vertigoat" & collar_data$collar_id == "30567" ] <- "goatbriella"
collar_data$goat_name[collar_data$goat_name == "billy" & collar_data$collar_id == "30636" ] <- "ryan_goatsling"

# import cleaned new collar data ----
load("data/collar_data/new_collar_data_20250218.rda")
new_collar$collar_id <- as.factor(new_collar$collar_id)
# drop columns that isn't needed 
new_collar <- subset(new_collar, select = c(collar_id, timestamp, latitude, longitude, data_type))

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)

new_collar <- merge(new_collar, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
new_collar$goat_name <- as.factor(new_collar$goat_name)
# new_collar$timestamp <- as.POSIXct(new_collar$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
new_collar$date <- as.Date(new_collar$timestamp)
new_collar$year <- year(new_collar$timestamp)
new_collar$month <- month(new_collar$timestamp, label = FALSE) #label = false for numerical month
new_collar$day <- day(new_collar$timestamp)
new_collar$month_day <- format(new_collar$timestamp, "%m-%d")
new_collar$doy <- yday(new_collar$timestamp) #day of the year

# combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
goat_data <- dplyr::bind_rows(collar_data, new_collar)


goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")



# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)

#format names to match required for ctmm based on Movebank critera:
goat_data <- plyr::rename(goat_data, c('collar_id' = 'individual.local.identifier',
                                       'latitude' = 'location.lat', 
                                       'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(goat_data, mark.rm = TRUE)


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
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=50)
   # beep(8)
  
}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h, #9.698333 mins; full = ~12.5min
END_movement <- Sys.time()


dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)
save(FITS,file="data/movement_model/fits_biol530w.rda")


#...........................................................
# 2. Home range ----
#...........................................................

load("data/movement_model/fits_biol530w.rda")

# Estimate akdes home-range areas
START_hr <- Sys.time()
tic(msg = "home range analysis")

AKDES <- akde(tel_data,FITS,weights=TRUE)

toc() # 14.2 mins
END_hr <- Sys.time()

#save rda:
dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)
save(AKDES,file="data/home_range/akdes_biol530w.rda")
load("data/home_range/akdes_biol530w.rda")






