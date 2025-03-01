


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



#.........................................................................
# DATA PREP ----
#.........................................................................

# new collar data ----
load("data/collar_data/new_collar_data_20250218.rda")
new_collar$collar_id <- as.factor(new_collar$collar_id)
# drop columns that isn't needed 
new_collar <- subset(new_collar, select = c(collar_id, timestamp, latitude, longitude, data_type))

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
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



#.........................................................................
# original collar data ----
load("data/collar_data/collar_data_20241123.rda")
# specify which dataset its from
collar_data$data_type <- "original_data"

# update dataset to reflect the name changes 2025-02
collar_data$goat_name[collar_data$goat_name == "kid_rock" & collar_data$collar_id == "30561"] <- "selena_goatmez"
collar_data$goat_name[collar_data$goat_name == "rocky" & collar_data$collar_id == "30613" ] <- "goatileo"
collar_data$goat_name[collar_data$goat_name == "vertigoat" & collar_data$collar_id == "30567" ] <- "goatbriella"
collar_data$goat_name[collar_data$goat_name == "billy" & collar_data$collar_id == "30636" ] <- "ryan_goatsling"


# subset to the 6 goats of interest
collar_data <- collar_data[collar_data$goat_name %in% goats,]
collar_data$goat_name <- as.factor(collar_data$goat_name)


# combine data for full data
# combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
combined_data <- bind_rows(collar_data, new_collar)

# check for duplicates in the combined df, there probably (and should be) is because they have ~ month overlapping 
# full_data[duplicated(full_data[, c("collar_id", "timestamp", "latitude", "longitude")]), ]
# no duplicates, but the timestamps are off by like ~45 minutes and the position is fairly close

#clean up environment
rm(collar_data, goat_info, dat)



#...........................................................................
## fire period ----
#...........................................................................

# reassigning back to fire_goats for workflow, this now contains original and new fire goat collar data
# fire_goats <- new_collar
#sort data by goat and timestamp
combined_data <- combined_data[order(combined_data$collar_id, combined_data$timestamp), ]

# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
goat_data <- combined_data[combined_data$month_day >= fire_start & combined_data$month_day <= fire_end, ] #21892 obs

# check for NAs
goat_data[complete.cases(goat_data$longitude, goat_data$latitude, goat_data$timestamp, goat_data$goat_name), ] #97
# remove the rows that are full NA rows
goat_data <- goat_data[complete.cases(goat_data$longitude, goat_data$latitude, goat_data$timestamp, goat_data$goat_name), ]

write.csv(goat_data, file = "./data/combined_goat_data_fire_period_all_years.csv", row.names = FALSE)
