
# screening fixes for non-animal points
# check if there are mortality signals or non-moving points to be used as calibration data
# prepare df for outlie()


library(sf)
library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)


load("./data/goat/prep/raw_original.rda")
# import additional information on the goats and dates (e.g., capture, mortality etc)
load("./data/goat/goat_info.rda")

str(raw_original)
tz(raw_original$timestamp_utc)
tz(raw_original$timestamp_local)
tz(raw_original$date_local)


# plot raw_original 
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
# some of them are in europe


#..................................................................
# identify fixes that don't belong to the animals, screening of raw_original data

# predeployment = precapture, before collaring
# active = after capture and collaring, when gps is live and has been deployed on the animal
# mortality = dead, or got a mortality signal
# offline = equipment error, collar went offline

# create a column for signal status
raw_original$signal_status <- NA

# create an outlier column for recording of outliers and for these kind of points
raw_original$outlier <- NA

# create a column as non-moving fixes to be used for calibration, mortality, fixes in the office
raw_original$calibration_data <- NA

# create a column to assign cluster id for each clumping of fixes as identifers when fitting the uere.fit()
raw_original$cluster_id <- NA

#..................................................................
# identify the europe fixes
# check where longitude is over 0 (i.e., not even in north america)
leaflet(raw_original[raw_original$longitude > 0, ]) %>% 
  addProviderTiles("OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# 2018-08-06 18:00:10 outliers not using for calibration
# 2018-08-06 18:00:24 outliers not using for calibration

# update european fixes
raw_original$signal_status <- ifelse(raw_original$longitude > 0, "Europe", raw_original$signal_status)
raw_original$outlier <- ifelse(raw_original$longitude > 0, 1, raw_original$outlier)
raw_original$calibration_data <- ifelse(raw_original$longitude > 0, 1, raw_original$calibration_data)
raw_original$cluster_id <- ifelse(raw_original$longitude > 0, paste0("1_europe_", raw_original$collar_id), raw_original$cluster_id)
# update calibration data not to include this outlier point
raw_original$calibration_data[raw_original$timestamp_local == "2018-08-06 18:00:10"] <- NA
raw_original$calibration_data[raw_original$timestamp_local == "2018-08-06 18:00:24"] <- NA


#..................................................................
# Identify fixes that occurred prior to collaring for each individual collars and not in europe (refer to the capture dates for each collar)
prechecks <- list(raw_original[raw_original$collar_id == "30561" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30599" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30613" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30642" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30551" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0 & raw_original$longitude > -119.6 ,],  
                  raw_original[raw_original$collar_id == "30548" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30575" & raw_original$date_local < "2019-07-09" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30567" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30648" & raw_original$date_local < "2019-06-24" & raw_original$longitude < 0,], 
                  raw_original[raw_original$collar_id == "30636" & raw_original$date_local < "2019-07-08" & raw_original$longitude < 0 & raw_original$longitude > -120,])

# cycle through the list of them to check if they occur in the office by changing the number inside [[]] 1 through 10
leaflet(prechecks[[10]]) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# clustering all in the same location
# 2018-09-10 17:02:25 for #1 30561, a bit too far from the cluster, dropping it as calibration data
# 2018-09-10 19:02:22 for #2 30599
# 2019-06-05 17:08:10 for #2 30599
# 2018-09-11 04:00:30 for #3 30613
# 2018-09-10 21:59:24 for #3 30613
# 2019-06-06 06:18:35 for #4 30642
# 2018-09-11 06:21:21 for #5 30551 same
# 2018-09-10 20:32:31 for #6 30548 same
# 2019-06-19 23:37:02 for #9 30648 same
# 2019-06-19 23:07:03 for #9 30648 same
# 2019-06-05 17:08:28 for #10 30636

# update their status
raw_original$signal_status[
  (raw_original$collar_id == "30561" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30599" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30613" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30642" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30551" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0 & raw_original$longitude > -119.6) |
    (raw_original$collar_id == "30548" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30575" & raw_original$date_local < "2019-07-09" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30567" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30648" & raw_original$date_local < "2019-06-24" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30636" & raw_original$date_local < "2019-07-08" & raw_original$longitude < 0 & raw_original$longitude > -120)
] <- "precollaring"

raw_original$outlier[
  (raw_original$collar_id == "30561" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30599" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30613" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30642" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30551" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0 & raw_original$longitude > -119.6) |
    (raw_original$collar_id == "30548" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30575" & raw_original$date_local < "2019-07-09" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30567" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30648" & raw_original$date_local < "2019-06-24" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30636" & raw_original$date_local < "2019-07-08" & raw_original$longitude < 0 & raw_original$longitude > -120)
] <- 1

raw_original$calibration_data[
  (raw_original$collar_id == "30561" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30599" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30613" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30642" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30551" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0 & raw_original$longitude > -119.6) |
    (raw_original$collar_id == "30548" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30575" & raw_original$date_local < "2019-07-09" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30567" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30648" & raw_original$date_local < "2019-06-24" & raw_original$longitude < 0) |
    (raw_original$collar_id == "30636" & raw_original$date_local < "2019-07-08" & raw_original$longitude < 0 & raw_original$longitude > -120)
] <- 1


raw_original$cluster_id <- ifelse(raw_original$collar_id == "30561" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30599" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30613" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30642" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30551" & raw_original$date_local < "2019-06-26" & raw_original$longitude < 0 & raw_original$longitude > -119.6, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30548" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30575" & raw_original$date_local < "2019-07-09" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30567" & raw_original$date_local < "2019-06-25" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30648" & raw_original$date_local < "2019-06-24" & raw_original$longitude < 0, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)
raw_original$cluster_id <- ifelse(raw_original$collar_id == "30636" & raw_original$date_local < "2019-07-08" & raw_original$longitude < 0 & raw_original$longitude > -120, paste0("2_precollar_", raw_original$collar_id), raw_original$cluster_id)


# 2018-09-10 17:02:25 for #1 30561, a bit too far from the cluster, dropping it as calibration data
# 2018-09-10 19:02:22 for #2 30599
# 2019-06-05 17:08:10 for #2 30599
# 2018-09-11 04:00:30 for #3 30613
# 2018-09-10 21:59:24 for #3 30613
# 2019-06-06 06:18:35 for #4 30642
# 2018-09-11 06:21:21 for #5 30551 same
# 2018-09-10 20:32:31 for #6 30548 same
# 2019-06-19 23:37:02 for #9 30648 same
# 2019-06-19 23:07:03 for #9 30648 same
# 2019-06-05 17:08:28 for #10 30636

# update to not include these points in the calibration data, rerun after changing status to check if they were all updated
raw_original[raw_original$collar_id == "30561" & raw_original$timestamp_local == "2018-09-10 17:02:25", ]
raw_original[raw_original$collar_id == "30599" & raw_original$timestamp_local == "2018-09-10 19:02:22", ]
raw_original[raw_original$collar_id == "30599" & raw_original$timestamp_local == "2019-06-05 17:08:10", ]
raw_original[raw_original$collar_id == "30613" & raw_original$timestamp_local == "2018-09-11 04:00:30", ]
raw_original[raw_original$collar_id == "30613" & raw_original$timestamp_local == "2018-09-10 21:59:24", ]
raw_original[raw_original$collar_id == "30642" & raw_original$timestamp_local == "2019-06-06 06:18:35", ]
raw_original[raw_original$collar_id == "30551" & raw_original$timestamp_local == "2018-09-11 06:21:21", ]
raw_original[raw_original$collar_id == "30548" & raw_original$timestamp_local == "2018-09-10 20:32:31", ]
raw_original[raw_original$collar_id == "30648" & raw_original$timestamp_local == "2019-06-19 23:37:02", ]
raw_original[raw_original$collar_id == "30648" & raw_original$timestamp_local == "2019-06-19 23:07:03", ]
raw_original[raw_original$collar_id == "30636" & raw_original$timestamp_local == "2019-06-05 17:08:28", ]

raw_original$calibration_data[raw_original$collar_id == "30561" & raw_original$timestamp_local == "2018-09-10 17:02:25"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30599" & raw_original$timestamp_local == "2018-09-10 19:02:22"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30599" & raw_original$timestamp_local == "2019-06-05 17:08:10"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30613" & raw_original$timestamp_local == "2018-09-11 04:00:30"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30613" & raw_original$timestamp_local == "2018-09-10 21:59:24"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30642" & raw_original$timestamp_local == "2019-06-06 06:18:35"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30551" & raw_original$timestamp_local == "2018-09-11 06:21:21"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30548" & raw_original$timestamp_local == "2018-09-10 20:32:31"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30648" & raw_original$timestamp_local == "2019-06-19 23:37:02"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30648" & raw_original$timestamp_local == "2019-06-19 23:07:03"] <- NA
raw_original$calibration_data[raw_original$collar_id == "30636" & raw_original$timestamp_local == "2019-06-05 17:08:28"] <- NA

raw_original$cluster_id[raw_original$collar_id == "30561" & raw_original$timestamp_local == "2018-09-10 17:02:25"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30599" & raw_original$timestamp_local == "2018-09-10 19:02:22"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30599" & raw_original$timestamp_local == "2019-06-05 17:08:10"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30613" & raw_original$timestamp_local == "2018-09-11 04:00:30"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30613" & raw_original$timestamp_local == "2018-09-10 21:59:24"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30642" & raw_original$timestamp_local == "2019-06-06 06:18:35"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30551" & raw_original$timestamp_local == "2018-09-11 06:21:21"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30548" & raw_original$timestamp_local == "2018-09-10 20:32:31"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30648" & raw_original$timestamp_local == "2019-06-19 23:37:02"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30648" & raw_original$timestamp_local == "2019-06-19 23:07:03"] <- NA
raw_original$cluster_id[raw_original$collar_id == "30636" & raw_original$timestamp_local == "2019-06-05 17:08:28"] <- NA

sum(raw_original$outlier, na.rm = TRUE) #566






#....................................................................
# Post-mortality/equipment failure fixes have been flagged ----
#....................................................................

# Identify post-mortality/equipment failure signal fixes, look at fixes after the indicated dates from goat info

#goat_02 collar went offline Sept 7, 2021
# check if there are any points recorded after it went offline, goat_02 was observed july 14, 2022
raw_original[raw_original$collar_id == "30599" & raw_original$date_local > "2021-09-07", ] # none

# look at points a month before it went offline to see if there were any signals that may be recorded due to equipment failure
leaflet(raw_original[raw_original$collar_id == "30599" & raw_original$date_local > "2021-08-07", ]) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = "red",
             fillOpacity = 0.9)
# all within the study area
# no update required


#............................................................
# goat_03 cause of death: fall?; indicated mortality date 2019-08-24
# Collar ID 30613 was reused on goat_12, capture date 2020-07-21

# check for after mortality date (goat_03) and precapture date (goat_12)
goat_03 <- raw_original[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-24" & raw_original$date_local < "2020-07-21", ] # 12 fixes
leaflet(raw_original[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-24" & raw_original$date_local < "2020-07-21", ] ) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# only at the office, july 13 and 14, 2020, no mortality signal, add as calibration data
# 2020-07-14 03:36:15 fix is an outlier

# update the office dates as precollaring signals
raw_original$signal_status[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-24" & raw_original$date_local < "2020-07-21"] <- "precollaring"
raw_original$outlier[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-24" & raw_original$date_local < "2020-07-21"] <- 1
raw_original$calibration_data[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-24" & raw_original$date_local < "2020-07-21"] <- 1
# update calibration data not to include this outlier point
raw_original$calibration_data[raw_original$timestamp_local == "2020-07-14 03:36:15"] <- NA
#update the cluster_id
raw_original$cluster_id[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-24" & raw_original$date_local < "2020-07-21"] <- "3_30613"
raw_original$cluster_id[raw_original$timestamp_local == "2020-07-14 03:36:15"] <- NA

sum(raw_original$outlier, na.rm = TRUE) #578

# look at a few days before mortality date for potential mortality signals
raw_original[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-20" & raw_original$date_local < "2019-08-25", ] 
leaflet(raw_original[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-20" & raw_original$date_local < "2019-08-25", ] ) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
#none

# check for a clustering
ggplot(raw_original[raw_original$collar_id == "30613" & raw_original$date_local > "2019-08-20" & raw_original$date_local < "2019-08-25", ] ) +
  geom_count(aes(longitude, latitude)) +
  geom_vline(xintercept = 0, color = 'red')
# no clustering

# no more updates, next




#............................................................
#goat_07 cause of death: avalanche; mortality signal date recorded: 2023-03-23
# collar collected april 1/2
# look at the data after mortality date
post07 <- raw_original[raw_original$collar_id == "30551" & raw_original$date_local > "2023-03-23",] #180 fixes

leaflet(raw_original[raw_original$collar_id == "30551" & raw_original$date_local > "2023-03-23",]) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = "red",
             fillOpacity = 0.9)
# points outside of the study area after mortality

# look at the the points before collar collection and time leading up to death
leaflet(raw_original[raw_original$collar_id == "30551" & raw_original$date_local > "2023-02-15" & raw_original$date_local < "2023-04-01",]) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# movement up til march 20th then looks like clumping

# look at points between mortality signal date and before retrieval (april 1 or 2)
leaflet(raw_original[raw_original$collar_id == "30551" & raw_original$date_local >= "2023-03-23" & raw_original$date_local <= "2023-04-01",]) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# clustering, mortality signal, non-moving fixes use for calibration
# 2023-03-24 04:45:00 looks like an outlier
# 2023-03-24 03:44:39 also looks like an outlier

# update status
raw_original$signal_status[raw_original$collar_id == "30551" & raw_original$date_local >= "2023-03-23" & raw_original$date_local <= "2023-04-01"] <- "mortality"
raw_original$outlier[raw_original$collar_id == "30551" & raw_original$date_local >= "2023-03-23" & raw_original$date_local <= "2023-04-01"] <- 1
raw_original$calibration_data[raw_original$collar_id == "30551" & raw_original$date_local >= "2023-03-23" & raw_original$date_local <= "2023-04-01"] <- 1
# change the 2023-03-24 04:45:00 fix as not a calibration point
raw_original[raw_original$timestamp_local == "2023-03-24 04:45:00",] 
raw_original$calibration_data[raw_original$timestamp_local == "2023-03-24 04:45:00"] <- NA
raw_original[raw_original$timestamp_local == "2023-03-24 03:44:39",] 
raw_original$calibration_data[raw_original$timestamp_local == "2023-03-24 04:45:00"] <- NA
#update cluster_id
raw_original$cluster_id[raw_original$collar_id == "30551" & raw_original$date_local >= "2023-03-23" & raw_original$date_local <= "2023-04-01"] <- "4_30551"
raw_original$cluster_id[raw_original$timestamp_local == "2023-03-24 04:45:00"] <- NA
raw_original$cluster_id[raw_original$timestamp_local == "2023-03-24 04:45:00"] <- NA


# look at after April 1 as retrieval
leaflet(raw_original[raw_original$collar_id == "30551" & raw_original$date_local > "2023-04-01",]) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# moving far distances -> human travel, returning the collar to the office

# update as retrieval and outlier
raw_original$signal_status[raw_original$collar_id == "30551" & raw_original$date_local > "2023-04-01"] <- "retrieval"
raw_original$outlier[raw_original$collar_id == "30551" & raw_original$date_local > "2023-04-01"] <- 1

sum(raw_original$calibration_data, na.rm = TRUE)# 622 -> 611 dropped a bunch earlier


#.............................
#goat_10 cause of death: predation via presumed cougar; mortality signal date: 2022-07-01

# look at the data after mortality date
post10 <- raw_original[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01",] #33 fixes
leaflet(raw_original[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01",]) %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# points outside of the study area after mortality

# identify points in the study area after the reported mortality signal, not in the office
leaflet(raw_original[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude < -120,]) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# clustering, mortality signal, non-moving fixes use for calibration
# 2022-07-04 04:00:23 looks like an outlier in the non-moving, not going to use it just in case

# update status
raw_original$signal_status[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude < -120] <- "mortality"
raw_original$outlier[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude < -120] <- 1
raw_original$calibration_data[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude < -120] <- 1
# change the 2022-07-04 04:00:23 fix as not a calibration point
raw_original[raw_original$timestamp_local == "2022-07-04 04:00:23",] 
raw_original$calibration_data[raw_original$timestamp_local == "2022-07-04 04:00:23"] <- NA
# update cluster_id
raw_original$cluster_id[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude < -120] <- "5_30567"
raw_original$cluster_id[raw_original$timestamp_local == "2022-07-04 04:00:23"] <- NA


# identify the points at the office
leaflet(raw_original[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude > -120,]) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# all points in the office, non-moving but too far apart, not going to use for calibration


# update status
raw_original$signal_status[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude > -120] <- "retrieval"
raw_original$outlier[raw_original$collar_id == "30567" & raw_original$date_local > "2022-07-01" & raw_original$longitude > -120] <- 1


sum(raw_original$calibration_data, na.rm = TRUE) # 654 -> 628 dropped the office points


#.....................................
#goat_18 cause of death: emaciated, starvation likely cause; mortality date March 16, 2021
post18 <- raw_original[raw_original$collar_id == "30636" & raw_original$date_local > "2021-03-16",] #0

# look at the the points leading up to death
leaflet(raw_original[raw_original$collar_id == "30636" & raw_original$date_local > "2021-03-14",]) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# movement up til march 15th
# 2021-03-15 11:31:50 looks like an outlier, but not going to update it, letting ctmm outlie check it

# look at march 16th for clustering no-movement signal for calibration
leaflet(raw_original[raw_original$collar_id == "30636" & raw_original$date_local >= "2021-03-16",]) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# not using just to be safe

# no update


sum(raw_original$outlier, na.rm = TRUE) #795
sum(raw_original$calibration_data, na.rm = TRUE) #656, 654?? -> 628 dropped a bunch of calibration data when triple checking



#....................................................................
# Drop goat_03 from dataset ----
#....................................................................

#goat_03 cause of death: fall?; collar was reused on goat_12 (collar id: 30613)

# tracked for 59 days with 279 fixes, dropping goat_03 from dataset
raw_original <- raw_original[!(raw_original$collar_id == "30613" & raw_original$date_local <= "2019-08-24"),] #78020 -> 77741
78020-77741

# check to see any data wasnt dropped
raw_original[raw_original$collar_id == "30613" & raw_original$date <= "2019-08-24",] #none

sum(raw_original$outlier, na.rm = TRUE) # 740


#....................................................................
# Check to see if any points were missed during screening ----
#....................................................................

# change the remaining points from NA to 0, til they get identified, theyre all correct points, not an outlier, and update it during outlie() checks, true points til proven guilty!
raw_original$outlier[is.na(raw_original$outlier)] <- 0
table(raw_original$outlier)

# see if any of the points prior to capture were missed (first capture date was 2019-06-24)
raw_original[raw_original$outlier == 0 & raw_original$date_local < "2019-06-24",] # 2

# set as outliers
raw_original$outlier[raw_original$date_local < "2019-06-24"] <- 1

# check if all the non-goat fixes have been identified and cleaned up
leaflet(raw_original[raw_original$outlier == 0,]) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)

# 2019-12-28 18:47:22 looks like an outlier 

# investigate the fix
raw_original[raw_original$timestamp_local == "2019-12-28 18:47:22",] # 30561, see if it gets flagged during outlie() check


# save raw_original of fixes that have been screened for non-animal fixes
save(raw_original, file = "./data/goat/prep/raw_original_screened.rda")
load("./data/goat/prep/raw_original_screened.rda")

# clean environment
rm(list = ls())
gc()


#///////////////////////////////////////////////////////////////////////
# data prep ----

load("./data/goat/prep/raw_original_screened.rda")
# import goat details
load("./data/goat/goat_info.rda")


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
intersect(colnames(raw_original), colnames(goat_info))

# add goat info to df
raw_original <- merge(raw_original, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
raw_original <- relocate(raw_original, c(goat_id, goat_name), .before = collar_id)
raw_original <- relocate(raw_original, timestamp_utc, .after = collar_id)


#............................................................
# dop checks
#............................................................

# check for NA pdop values
raw_original[is.na(raw_original$pdop),]
test <- raw_original[is.na(raw_original$pdop),] #56, theyre all hdop values

# Fleming et al 2021, S3.1 DOP Proxies, Co-opting DOP values
# In lieu of HDOP values, some GPS devices only record "position DOP" (PDOP), "geometric DOP" (GDOP), or ambiguous DOP values. PDOP values combine HDOP and VDOP; GDOP values combine PDOP and "time DOP" (TDOP) values. As HDOP and VDOP values are both a function of satellite number and spread, they are correlated. Therefore, it is reasonable to co-opt or appropriate related DOP values in the absence of proper HDOP and VDOP values.
# ...assuming the data are not of a special variety (Argos Doppler-shift or e-obs), 
# then the as.telemetry() will first look for an HDOP value. If HDOP values are not found, then as.telemetry() will look for ambiguous DOP values, followed by PDOP and then GDOP values. 
# If no DOP values are found, then as.telemetry() will look for the reported number of satellites, and apply model (S3.1) to approximate DOP values 

# substitute hdop values for missing pdop 
raw_original$pdop <- ifelse(is.na(raw_original$pdop & !is.na(raw_original$hdop)), raw_original$hdop, raw_original$pdop)
# need to do this separately from the new data because new data contains only dop value and when combining the two datasets and having dop and pdop, you will run into issues of getting hdop values of all 100, and vdop all at Inf. Even if you drop fix (location class) column but you want to keep fix type because 
# By default, ctmm's as.telemetry() function will assume that any GPS x-type column is meaningful, and create a corresponding location class column in the output telemetry data object. Model selection can then determine whether or not extra location classes are supported by the calibration data. as.telemetry() also checks if some location estimates lack corresponding speed, altitude, and/or DOP values, in which case location classes will be created to distinguish the level of missingness, as this often corresponds to different location estimation algorithms.

# check again for NA pdop values
raw_original[is.na(raw_original$pdop),] #none, all pdop values now

# drop hdop, vdop columns
raw_original <- subset(raw_original, select = -c(hdop, vdop))

# inspect fix types
unique(raw_original$fix) # Note 3D =/= 3D Valid
raw_original <- rename(raw_original, fix_type = fix)

str(raw_original)
# reorder based on timestamp and goat_id, using utc time hence forth
raw_original <- raw_original[order(raw_original$goat_id, raw_original$timestamp_utc),]
# reset the rownames
rownames(raw_original) <- NULL

# create a column for fix id assign them a fix_id number to keep track of the gps fixes for future use (based on rowname number when sorted based on goat_id and timestamp_utc as noted above)
raw_original$fix_id <- rownames(raw_original)
raw_original <- relocate(raw_original, fix_id, .before = goat_id)
raw_original$fix_id <- as.numeric(raw_original$fix_id)

save(raw_original, file = "./data/goat/prep/raw_original_screened_prepped.rda")
load("./data/goat/prep/raw_original_screened_prepped.rda")


# clean environment
rm(list = ls())
gc()




