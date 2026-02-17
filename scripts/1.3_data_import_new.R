
# inspect second data batch

library(sf)
library(ggplot2)
library(basemaps)
library(ctmm)
library(dplyr)
library(lubridate)
library(leaflet)



# Import data ----
raw_new <- read.csv("./data/goat/raw/Cathedral Goat locations Sept 2023 through Feb 10 2025.csv") %>%
  janitor::clean_names() #remove unnecessary characters ("[", " ", etc.) and replace with "_" 
#inspect data
names(raw_new)
head(raw_new)
str(raw_new)
# acq_time_utc = GPS fix acquisition time (actual recorded fix time)
# scts_utc = data is sent or received to Iridium satellite network (origin column), i.e., sent to system .-. after acquisition time

# drop animal column or error will occur when trying to convert to a ctmm telemetry object, and drop columns that provide duplicate information such as collar number
raw_new <- subset(raw_new, select = -c(animal, collar_name, scts_utc))

# indicate data origin
raw_new$data_source <- 2

#check the timezone
tz(raw_new$acq_time_utc)
# convert to timestamp format, currently in character
raw_new$acq_time_utc <- as.POSIXct(raw_new$acq_time_utc, format = "%Y-%m-%d %H:%M", tz = "UTC")
# add a local time
raw_new$timestamp_local <- with_tz(raw_new$acq_time_utc, tzone = "America/Vancouver")
tz(raw_new$timestamp_local)

# add a local date column
raw_new$date_local <- as.POSIXct(format(raw_new$timestamp_local, "%Y-%m-%d"), tz = "America/Vancouver")

# rename to match generic naming
raw_new <- rename(raw_new, c(timestamp_utc = acq_time_utc,
                               longitude = longitude_deg,
                               latitude = latitude_deg))

# plot raw_new data 
ggplot(raw_new) +
  facet_wrap(~ collar_id) +
  geom_count(aes(longitude, latitude))
# there are fixes that are super far from the bulk of the points

# see where the fixes are on a map (Esri.WorldImagery for satellite; Esri.WorldTopoMap for elevation)
leaflet(raw_new) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = "red",
             fillOpacity = 0.9)
# theres definitely some offshoots of points

# check timestamp/date range for each collar
by(raw_new$timestamp_local, raw_new$collar_id, range)
by(raw_new$date_local, raw_new$collar_id, range)

# check for duplicates
raw_new[duplicated(raw_new[, c("timestamp_utc", "longitude", "latitude", "collar_id")]), ] # none
raw_new[duplicated(raw_new[, c("timestamp_utc", "collar_id")]), ] # none

# format object types
raw_new$collar_id <- as.factor(raw_new$collar_id)
raw_new$data_source <- as.factor(raw_new$data_source)

# save as rda to retain formatting
save(raw_new, file = "./data/goat/prep/raw_new.rda")
load("./data/goat/prep/raw_new.rda")

# clean environment
rm(list = ls())
gc()




#.....................................................................
# prep data
#.....................................................................


load("./data/goat/prep/raw_new.rda")
load("./data/goat/goat_info.rda")
str(raw_new)


#....................................................................
# add goat info ----
#....................................................................

# drop goat_03 or going to have merging issues due to two goats one collar dilemma
goat_info <- goat_info[goat_info$goat_id != "CA03",]
# drop the goat 03 factor level
goat_info$goat_id <- droplevels(goat_info$goat_id)
goat_info$goat_name <- droplevels(goat_info$goat_name)
goat_info$collar_id <- droplevels(goat_info$collar_id)

# check for same column names to avoid duplication, add goat info to the raw data
intersect(colnames(raw_new), colnames(goat_info))

# add goat info to df
raw_new <- merge(raw_new, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
raw_new <- relocate(raw_new, c(goat_id, goat_name), .before = collar_id)
raw_new <- relocate(raw_new, timestamp_utc, .after = collar_id)

# reorder
raw_new <- raw_new[order(raw_new$goat_id, raw_new$timestamp_utc),]
rownames(raw_new) <- NULL

# add an outlier column
raw_new$outlier <- 0

# create new columns to format names to match required for ctmm based on Movebank critera:
raw_new$individual.local.identifier <- raw_new$goat_id
raw_new$location.lat <- raw_new$latitude
raw_new$location.long <- raw_new$longitude
raw_new$timestamp <- raw_new$timestamp_utc



save(raw_new, file = "./data/goat/prep/raw_new_prepped.rda")
load("./data/goat/prep/raw_new_prepped.rda")

# clean environment
rm(list = ls())
gc()



names(raw_new)
unique(raw_new$goat_id)
