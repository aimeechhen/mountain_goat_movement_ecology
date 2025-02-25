

library(lubridate)
library(dplyr)
# library(devtools)
# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)
library(tidyr)

rm(list = ls())

# Mountain goat GPS collar data and supplementary information was provided by BC Parks
# Data cleaning for the collar data was completed by Stefano Mezzini
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/00-compile-goat-tracking-data.R
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/01-compile-tracking-data.R
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/01a-compile-goat-calibration-data.R
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/02-remove-outliers.R




#____________________________________________________________
# Data import and pre-processing ----


# Import clean collar data (takes a few mins to load)
# This rds file contains cleaned telemetry data (already calibrated), variograms, movement models and akde home ranges for mountain goats for the full dataset
dat <- readRDS("data/collar_data/movement-models-akdes-goats-2024-06-07.rds")

#extract the telemetry data from the tibble
tel_data <- dat$tel
#rename the list with the animal id
names(tel_data) <- dat$animal

#convert the list into a dataframe and include the animal id as a column
collar_data <- do.call(rbind, lapply(names(tel_data), function(id) {
  df <- as.data.frame(tel_data[[id]])
  df$animal <- id
  return(df)
}))

# # visualise collar data
# plot(x = collar_data$longitude, y = collar_data$latitude)

# names(collar_data)[9] <- "goat_id"
names(collar_data)[names(collar_data) == 'animal'] <- "goat_id"
# collar_data <- rename(collar_data, goat_id = animal)



#.......................................
# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")

# Add goat_name and collar_id to the dataframe
collar_data <- merge(collar_data, goat_info[, c("goat_name", "goat_id", "collar_id")], by = "goat_id", all.x = TRUE)
collar_data <- relocate(collar_data, c(goat_name, collar_id), .after = goat_id)
collar_data$collar_id = as.factor(collar_data$collar_id)

# for temporal attributes
collar_data$timestamp <- as.POSIXct(collar_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
# collar_data$date_time = as.POSIXct(collar_data$timestamp, format = '%Y-%m-%d %H:%M', tz='UTC')
collar_data$date <- as.Date(collar_data$timestamp)
collar_data$year <- year(collar_data$timestamp)
collar_data$month <- month(collar_data$timestamp, label = FALSE) #label = false for numerical month
collar_data$day <- day(collar_data$timestamp)
collar_data$month_day <- format(collar_data$timestamp, "%m-%d")
collar_data$doy <- yday(collar_data$timestamp) #day of the year

# Total fixes: 65452
# write.csv(collar_data, file = './data/collar_data/extracted_clean_collar_data_20240703.csv', row.names = FALSE)
write.csv(collar_data, file = './data/collar_data/extracted_clean_collar_data_20241123.csv', row.names = FALSE)




# #_____________________________________________________________________________________
# # Identify and remove pre-collaring fixes ----
# 
# # Note: This would have been completed during data cleaning processing. 
# # Code here is for reference and double checking
# 
# # Identify fixes that occurred prior to collaring (collaring started June 24, 2019)
# precollar <- collar_data[collar_data$date < "2019-06-24",] #568 fixes
# # Remove fixes that occurred pre-collaring
# collar_data <- collar_data[!(collar_data$date < "2019-06-24"),] #65810 fixes
# 
# # Check if all pre-collaring fixes have been removed
# pre01 <- collar_data[collar_data$collar_id == "30561" & collar_data$date < "2019-06-26",]
# pre02 <- collar_data[collar_data$collar_id == "30599" & collar_data$date < "2019-06-25",]
# pre03 <- collar_data[collar_data$collar_id == "30613" & collar_data$date < "2019-06-26",]
# pre04 <- collar_data[collar_data$collar_id == "30642" & collar_data$date < "2019-06-26",]
# pre07 <- collar_data[collar_data$collar_id == "30551" & collar_data$date < "2019-06-26",]
# pre08 <- collar_data[collar_data$collar_id == "30548" & collar_data$date < "2019-06-26",]
# pre09 <- collar_data[collar_data$collar_id == "30575" & collar_data$date < "2019-07-09",]
# pre10 <- collar_data[collar_data$collar_id == "30567" & collar_data$date < "2019-06-25",]
# pre11 <- collar_data[collar_data$collar_id == "30648" & collar_data$date < "2019-06-24",]
# pre18 <- collar_data[collar_data$collar_id == "30636" & collar_data$date < "2019-07-09",]
# 
# # Remove the remaining pre-collaring fixes
# collar_data <- collar_data[!(collar_data$collar_id == "30548" & collar_data$date < "2019-06-26"),]
# 
# # Collar ID 30613 was reused on goat_12 (collared on July 21, 2020)
# # Identify fixes that occurred prior to collaring
# pre12 <- collar_data[collar_data$collar_id == "30613" & collar_data$date >= "2019-08-25" & collar_data$date <= "2020-07-20", ]
# #12 fixes
# # Remove fixes that occurred pre-collaring for goat_12 (aka post-mortality fixes for goat_03 or 'post03' [for naming, see below])
# collar_data <- collar_data[!(collar_data$collar_id == "30613" & 
#                                collar_data$date >= "2019-08-25" & collar_data$date <= "2020-07-20"),]
# 
# #clean up environment
# rm(precollar,
#    pre01,
#    pre02,
#    pre03,
#    pre04,
#    pre07,
#    pre08,
#    pre09,
#    pre10,
#    pre11,
#    pre12,
#    pre18)
# 
# 
# 
# #....................................................................
# # Identify and remove post-mortality fixes ----
# #....................................................................
# 
# # Identify post-mortality signal fixes
# 
# #goat_07 cause of death: avalanche
# post07 <- collar_data[collar_data$collar_id == "30551" & collar_data$date > "2023-03-23",] #180 fixes
# 
# #goat_10 cause of death: predation via presumed cougar
# post10 <- collar_data[collar_data$collar_id == "30567" & collar_data$date > "2022-07-01",] #33 fixes
# 
# #goat_10 cause of death: emaciated, starvation likely cause
# post18 <- collar_data[collar_data$collar_id == "30636" & collar_data$date > "2021-03-16",] #0 fixes
# 
# # Remove post-mortality signal fixes
# collar_data <- collar_data[!(collar_data$collar_id == "30551" & collar_data$date > "2023-03-23"),]
# collar_data <- collar_data[!(collar_data$collar_id == "30567" & collar_data$date > "2022-07-01"),]
# 
# #clean up environment
# rm(post07, post10, post18)
# 
# 
# # visualise cleaned up collar data
# plot(x = collar_data$location.long, y = collar_data$location.lat)
# 
# #final dataset: 65582 fixes
# #final dataset with cleaning processing: 65452 fixes
# 
# 
# #save dataset, all the pre-collaring and post-mortality signal fixes has been removed from the dataset
# save(collar_data, file = "data/collar_data/collar_data.rda")






#....................................................................
# Drop goat_03 from dataset ----
#....................................................................

#goat_03 cause of death: fall?; collar-id was reused (collar id: 30613)
# goat_03 <- collar_data[collar_data$goat_name == "cliff" & collar_data$date <= "2019-08-24",] #224 fixes 223

# Calculate the number of days goat03 was tracked for (2019-06-26 to 2019-08-24)
collar_data[collar_data$date == "2019-06-26",] #day 177
collar_data[collar_data$date == "2019-08-24",] #day 236
236-177 
#59 days with 224 fixes

# Drop goat_03 from dataset
collar_data <- collar_data[!(collar_data$goat_name == "cliff" & collar_data$date <= "2019-08-24"),] #65358 fixes

#without goat_03 final dataset: 65358 fixes
#without goat_03 final dataset with cleaning processing: 65229 fixes

# visualise collar data without goat_03
plot(x = collar_data$longitude, y = collar_data$latitude)

#check if the date range is correct:
min(collar_data$date)
max(collar_data$date)

# save(collar_data, file = "data/collar_data/collar_data_20240703.rda")
# load("data/collar_data/collar_data_20240703.rda")
# save(collar_data, file = "data/collar_data/collar_data_20241123.rda")
load("data/collar_data/collar_data_20241123.rda")




#///////////////////////////////////////////
# END ----
#///////////////////////////////////////////





# Window analysis data prep ----

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





#///////////////////////////////////////////
# END ----
#///////////////////////////////////////////

