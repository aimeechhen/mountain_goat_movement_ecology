
# Import data as ctmm telemetry object

library(ctmm)
# source('./scripts/source/import_data_as_ctmm_telemetry_object.r')



# load("data/collar_data/collar_data_20241123.rda")
# load("data/collar_data/new_collar_data_20250218.rda")
# new_collar$timestamp <- as.POSIXct(new_collar$timestamp, origin = "1970-01-01", tz = "America/Vancouver")

#format names to match required for ctmm based on Movebank critera:
# dat = plyr::rename(collar_data, c('goat_name' = 'individual.local.identifier',
#                                   'latitude' = 'location.lat', 
#                                   'longitude' = 'location.long'))

# #format names to match required for ctmm based on Movebank critera:
# dat = plyr::rename(new_collar, c('goat_name' = 'individual.local.identifier',
#                                   'latitude' = 'location.lat', 
#                                   'longitude' = 'location.long'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import combined collar data (original + new)
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

#format names to match required for ctmm based on Movebank critera:
# create a column combining collar_id and year to avoid needing to subset by individuals and year, it will create a unique identifier based on the individual and the year of the data and then those will be grouped together as the data for each individual for each year
goat_data$individual.local.identifier <- paste(goat_data$collar_id, goat_data$year, sep = "_")
# format names to match
goat_data <- plyr::rename(goat_data, c('latitude' = 'location.lat', 
                                       'longitude' = 'location.long'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(goat_data, mark.rm = TRUE)