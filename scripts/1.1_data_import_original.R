

library(sf)
library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)

# create a folder to store all the preprocessing intermediate stages of the tracking data
dir.create(path = "./data/goat/prep/", showWarnings = TRUE)

# inspect a tracking data file & layers
st_layers('./data/goat/raw/original/GPS_Collar30548_20231005121804.gpx')
# use `track_points` layer
raw <- st_read("./data/goat/raw/original/GPS_Collar30548_20231005121804.gpx", layer = 'track_points')


# list all the gps tracking files
files <- list.files('./data/goat/raw/original/', full.names = TRUE)

# create a list to store all the extract
file_list <- list()

# import all the files into a list
for (i in seq_along(files)) {
  # extract the file path for the the data
  filepath <- files[i]
  # import the tracking data
  data <- st_read(dsn = files[i], layer = "track_points", quiet = TRUE)
  # extract the collar_id from the file name (extract the 5 digits after the text string)
  collar_id <- sub(".*GPS_Collar(\\d{5}).*", "\\1", files[i])
  # add the id
  data$collar_id <- collar_id
  # store in the list and assign the item as their collar_id
  file_list[[collar_id]] <- data
  
}

# convert into a single df
raw_original <- do.call(rbind, file_list)

# check rows with rownames that contain NA.## and NA values across all columns
any(raw_original[grepl("NA", rownames(raw_original)), ])

# extract coordinates
coords <- st_coordinates(raw_original)

# add coordinates
raw_original$longitude <- coords[,1] #X
raw_original$latitude <- coords[,2] #Y

# drop geometry
raw_original <- st_drop_geometry(raw_original)

# subset the columns of interest
raw_original <- subset(raw_original, select = c(collar_id, time, longitude, latitude, ele, fix, hdop, vdop, pdop))

# indicate data origin/source, what dataset did they come from
# number indicates the order that the dataset were given (original = 1, batch 2/new = 2, etc)
raw_original$data_source <- 1

#check the timezone
head(raw_original$time) # PDT tz = "America/Vancouver"
tz(raw_original$time) # none, assign it
tz(raw_original$time) <- "America/Vancouver"
tz(raw_original$time)

# rename to indicate time type
raw_original <- rename(raw_original, timestamp_local = time)
tz(raw_original$timestamp_local)
# add date column
raw_original$date_local <- as.POSIXct(format(raw_original$timestamp_local, "%Y-%m-%d"), tz = "America/Vancouver")
tz(raw_original$date_local)

# add utc time
raw_original$timestamp_utc <- as.POSIXct(raw_original$timestamp_local, tz = "UTC")
tz(raw_original$timestamp_utc)

# plot raw_original data 
ggplot(raw_original) +
  facet_wrap(~ collar_id) +
  geom_count(aes(longitude, latitude)) +
  geom_vline(xintercept = 0, color = 'red')
# there are fixes that are super far from the bulk of the points

# see where the fixes are on a map (Esri.WorldImagery for satellite; Esri.WorldTopoMap for elevation)
leaflet(raw_original) %>% 
  addProviderTiles("OpenStreetMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = "red",
             fillOpacity = 0.9)
# some of them are in europe, need to do some recorded fixes screening

# check timestamp/date range for each collar
by(raw_original$timestamp_local, raw_original$collar_id, range)
by(raw_original$date_local, raw_original$collar_id, range)

# check for duplicates
raw_original[duplicated(raw_original[, c("timestamp_local", "longitude", "latitude", "collar_id")]), ] # none
raw_original[duplicated(raw_original[, c("timestamp_local", "collar_id")]), ] # none


# format object types
raw_original$collar_id <- as.factor(raw_original$collar_id)
raw_original$data_source <- as.factor(raw_original$data_source)

save(raw_original, file = "./data/goat/prep/raw_original.rda")
load("./data/goat/prep/raw_original.rda")

# clean environment
rm(list = ls())
gc()







