
# screening fixes for non-animal points
# check if there are mortality signals or non-moving points to be used as calibration data


library(sf)
library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)


load("./data/goat/prep/combined_data.rda")
# import additional information on the goats and dates (e.g., capture, mortality etc)
load("./data/goat/goat_info.rda")

str(combined_data)
tz(combined_data$timestamp_utc)
tz(combined_data$timestamp_local)
tz(combined_data$date_local)


# plot combined_data 
ggplot(combined_data) +
  facet_wrap(~ collar_id) +
  geom_count(aes(longitude, latitude)) +
  geom_vline(xintercept = 0, color = 'red')
# there are fixes that are super far from the bulk of the points

# see where the fixes are on a map (Esri.WorldImagery for satellite; Esri.WorldTopoMap for elevation)
leaflet(combined_data) %>% 
  addProviderTiles("OpenStreetMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = "red",
             fillOpacity = 0.9)
# some of them are in europe


#..................................................................
# identify fixes that don't belong to the animals, screening of combined_data data

# predeployment = precapture, before collaring
# active = after capture and collaring, when gps is live and has been deployed on the animal
# mortality = dead, or got a mortality signal
# offline = equipment error, collar went offline

# create a column for signal status
combined_data$signal_status <- NA

# create an outlier column for recording of outliers and for these kind of points
combined_data$outlier <- NA

# create a column as non-moving fixes to be used for calibration, mortality, fixes in the office
combined_data$calibration_data <- NA

# create a column to assign cluster id for each clumping of fixes as identifers when fitting the uere.fit()
combined_data$cluster_id <- NA

#..................................................................
# identify the europe fixes
# check where longitude is over 0 (i.e., not even in north america)
leaflet(combined_data[combined_data$longitude > 0, ]) %>% 
  addProviderTiles("OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# 2018-08-06 18:00:10 outliers not using for calibration
# 2018-08-06 18:00:24 outliers not using for calibration

# update european fixes
combined_data$signal_status <- ifelse(combined_data$longitude > 0, "Europe", combined_data$signal_status)
combined_data$outlier <- ifelse(combined_data$longitude > 0, 1, combined_data$outlier)
combined_data$calibration_data <- ifelse(combined_data$longitude > 0, 1, combined_data$calibration_data)
combined_data$cluster_id <- ifelse(combined_data$longitude > 0, paste0("1_europe_", combined_data$collar_id), combined_data$cluster_id)
# update calibration data not to include this outlier point
combined_data$calibration_data[combined_data$timestamp_local == "2018-08-06 18:00:10"] <- NA
combined_data$calibration_data[combined_data$timestamp_local == "2018-08-06 18:00:24"] <- NA


#..................................................................
# Identify fixes that occurred prior to collaring for each individual collars and not in europe (refer to the capture dates for each collar)
prechecks <- list(combined_data[combined_data$collar_id == "30561" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30599" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30613" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30642" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30551" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0 & combined_data$longitude > -119.6 ,],  
                  combined_data[combined_data$collar_id == "30548" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30575" & combined_data$date_local < "2019-07-09" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30567" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30648" & combined_data$date_local < "2019-06-24" & combined_data$longitude < 0,], 
                  combined_data[combined_data$collar_id == "30636" & combined_data$date_local < "2019-07-08" & combined_data$longitude < 0 & combined_data$longitude > -120,])

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
combined_data$signal_status[
  (combined_data$collar_id == "30561" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30599" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30613" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30642" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30551" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0 & combined_data$longitude > -119.6) |
    (combined_data$collar_id == "30548" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30575" & combined_data$date_local < "2019-07-09" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30567" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30648" & combined_data$date_local < "2019-06-24" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30636" & combined_data$date_local < "2019-07-08" & combined_data$longitude < 0 & combined_data$longitude > -120)
] <- "precollaring"

combined_data$outlier[
  (combined_data$collar_id == "30561" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30599" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30613" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30642" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30551" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0 & combined_data$longitude > -119.6) |
    (combined_data$collar_id == "30548" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30575" & combined_data$date_local < "2019-07-09" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30567" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30648" & combined_data$date_local < "2019-06-24" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30636" & combined_data$date_local < "2019-07-08" & combined_data$longitude < 0 & combined_data$longitude > -120)
] <- 1

combined_data$calibration_data[
  (combined_data$collar_id == "30561" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30599" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30613" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30642" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30551" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0 & combined_data$longitude > -119.6) |
    (combined_data$collar_id == "30548" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30575" & combined_data$date_local < "2019-07-09" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30567" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30648" & combined_data$date_local < "2019-06-24" & combined_data$longitude < 0) |
    (combined_data$collar_id == "30636" & combined_data$date_local < "2019-07-08" & combined_data$longitude < 0 & combined_data$longitude > -120)
] <- 1


combined_data$cluster_id <- ifelse(combined_data$collar_id == "30561" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30599" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30613" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30642" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30551" & combined_data$date_local < "2019-06-26" & combined_data$longitude < 0 & combined_data$longitude > -119.6, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30548" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30575" & combined_data$date_local < "2019-07-09" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30567" & combined_data$date_local < "2019-06-25" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30648" & combined_data$date_local < "2019-06-24" & combined_data$longitude < 0, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)
combined_data$cluster_id <- ifelse(combined_data$collar_id == "30636" & combined_data$date_local < "2019-07-08" & combined_data$longitude < 0 & combined_data$longitude > -120, paste0("2_precollar_", combined_data$collar_id), combined_data$cluster_id)


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
combined_data[combined_data$collar_id == "30561" & combined_data$timestamp_local == "2018-09-10 17:02:25", ]
combined_data[combined_data$collar_id == "30599" & combined_data$timestamp_local == "2018-09-10 19:02:22", ]
combined_data[combined_data$collar_id == "30599" & combined_data$timestamp_local == "2019-06-05 17:08:10", ]
combined_data[combined_data$collar_id == "30613" & combined_data$timestamp_local == "2018-09-11 04:00:30", ]
combined_data[combined_data$collar_id == "30613" & combined_data$timestamp_local == "2018-09-10 21:59:24", ]
combined_data[combined_data$collar_id == "30642" & combined_data$timestamp_local == "2019-06-06 06:18:35", ]
combined_data[combined_data$collar_id == "30551" & combined_data$timestamp_local == "2018-09-11 06:21:21", ]
combined_data[combined_data$collar_id == "30548" & combined_data$timestamp_local == "2018-09-10 20:32:31", ]
combined_data[combined_data$collar_id == "30648" & combined_data$timestamp_local == "2019-06-19 23:37:02", ]
combined_data[combined_data$collar_id == "30648" & combined_data$timestamp_local == "2019-06-19 23:07:03", ]
combined_data[combined_data$collar_id == "30636" & combined_data$timestamp_local == "2019-06-05 17:08:28", ]

combined_data$calibration_data[combined_data$collar_id == "30561" & combined_data$timestamp_local == "2018-09-10 17:02:25"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30599" & combined_data$timestamp_local == "2018-09-10 19:02:22"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30599" & combined_data$timestamp_local == "2019-06-05 17:08:10"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30613" & combined_data$timestamp_local == "2018-09-11 04:00:30"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30613" & combined_data$timestamp_local == "2018-09-10 21:59:24"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30642" & combined_data$timestamp_local == "2019-06-06 06:18:35"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30551" & combined_data$timestamp_local == "2018-09-11 06:21:21"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30548" & combined_data$timestamp_local == "2018-09-10 20:32:31"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30648" & combined_data$timestamp_local == "2019-06-19 23:37:02"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30648" & combined_data$timestamp_local == "2019-06-19 23:07:03"] <- NA
combined_data$calibration_data[combined_data$collar_id == "30636" & combined_data$timestamp_local == "2019-06-05 17:08:28"] <- NA

combined_data$cluster_id[combined_data$collar_id == "30561" & combined_data$timestamp_local == "2018-09-10 17:02:25"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30599" & combined_data$timestamp_local == "2018-09-10 19:02:22"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30599" & combined_data$timestamp_local == "2019-06-05 17:08:10"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30613" & combined_data$timestamp_local == "2018-09-11 04:00:30"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30613" & combined_data$timestamp_local == "2018-09-10 21:59:24"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30642" & combined_data$timestamp_local == "2019-06-06 06:18:35"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30551" & combined_data$timestamp_local == "2018-09-11 06:21:21"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30548" & combined_data$timestamp_local == "2018-09-10 20:32:31"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30648" & combined_data$timestamp_local == "2019-06-19 23:37:02"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30648" & combined_data$timestamp_local == "2019-06-19 23:07:03"] <- NA
combined_data$cluster_id[combined_data$collar_id == "30636" & combined_data$timestamp_local == "2019-06-05 17:08:28"] <- NA

sum(combined_data$outlier, na.rm = TRUE) #566






#....................................................................
# Post-mortality/equipment failure fixes have been flagged ----
#....................................................................

# Identify post-mortality/equipment failure signal fixes, look at fixes after the indicated dates from goat info

#goat_02 collar went offline Sept 7, 2021
# check if there are any points recorded after it went offline, goat_02 was observed july 14, 2022
combined_data[combined_data$collar_id == "30599" & combined_data$date_local > "2021-09-07", ] # none

# look at points a month before it went offline to see if there were any signals that may be recorded due to equipment failure
leaflet(combined_data[combined_data$collar_id == "30599" & combined_data$date_local > "2021-08-07", ]) %>% 
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
goat_03 <- combined_data[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-24" & combined_data$date_local < "2020-07-21", ] # 12 fixes
leaflet(combined_data[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-24" & combined_data$date_local < "2020-07-21", ] ) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# only at the office, july 13 and 14, 2020, no mortality signal, add as calibration data
# 2020-07-14 03:36:15 fix is an outlier

# update the office dates as precollaring signals
combined_data$signal_status[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-24" & combined_data$date_local < "2020-07-21"] <- "precollaring"
combined_data$outlier[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-24" & combined_data$date_local < "2020-07-21"] <- 1
combined_data$calibration_data[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-24" & combined_data$date_local < "2020-07-21"] <- 1
# update calibration data not to include this outlier point
combined_data$calibration_data[combined_data$timestamp_local == "2020-07-14 03:36:15"] <- NA
#update the cluster_id
combined_data$cluster_id[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-24" & combined_data$date_local < "2020-07-21"] <- "3_30613"
combined_data$cluster_id[combined_data$timestamp_local == "2020-07-14 03:36:15"] <- NA

sum(combined_data$outlier, na.rm = TRUE) #578

# look at a few days before mortality date for potential mortality signals
combined_data[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-20" & combined_data$date_local < "2019-08-25", ] 
leaflet(combined_data[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-20" & combined_data$date_local < "2019-08-25", ] ) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
#none

# check for a clustering
ggplot(combined_data[combined_data$collar_id == "30613" & combined_data$date_local > "2019-08-20" & combined_data$date_local < "2019-08-25", ] ) +
  geom_count(aes(longitude, latitude)) +
  geom_vline(xintercept = 0, color = 'red')
# no clustering

# no more updates, next




#............................................................
#goat_07 cause of death: avalanche; mortality signal date recorded: 2023-03-23
# collar collected april 1/2
# look at the data after mortality date
post07 <- combined_data[combined_data$collar_id == "30551" & combined_data$date_local > "2023-03-23",] #180 fixes

leaflet(combined_data[combined_data$collar_id == "30551" & combined_data$date_local > "2023-03-23",]) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = "red",
             fillOpacity = 0.9)
# points outside of the study area after mortality

# look at the the points before collar collection and time leading up to death
leaflet(combined_data[combined_data$collar_id == "30551" & combined_data$date_local > "2023-02-15" & combined_data$date_local < "2023-04-01",]) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# movement up til march 20th then looks like clumping

# look at points between mortality signal date and before retrieval (april 1 or 2)
leaflet(combined_data[combined_data$collar_id == "30551" & combined_data$date_local >= "2023-03-23" & combined_data$date_local <= "2023-04-01",]) %>% 
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
combined_data$signal_status[combined_data$collar_id == "30551" & combined_data$date_local >= "2023-03-23" & combined_data$date_local <= "2023-04-01"] <- "mortality"
combined_data$outlier[combined_data$collar_id == "30551" & combined_data$date_local >= "2023-03-23" & combined_data$date_local <= "2023-04-01"] <- 1
combined_data$calibration_data[combined_data$collar_id == "30551" & combined_data$date_local >= "2023-03-23" & combined_data$date_local <= "2023-04-01"] <- 1
# change the 2023-03-24 04:45:00 fix as not a calibration point
combined_data[combined_data$timestamp_local == "2023-03-24 04:45:00",] 
combined_data$calibration_data[combined_data$timestamp_local == "2023-03-24 04:45:00"] <- NA
combined_data[combined_data$timestamp_local == "2023-03-24 03:44:39",] 
combined_data$calibration_data[combined_data$timestamp_local == "2023-03-24 04:45:00"] <- NA
#update cluster_id
combined_data$cluster_id[combined_data$collar_id == "30551" & combined_data$date_local >= "2023-03-23" & combined_data$date_local <= "2023-04-01"] <- "4_30551"
combined_data$cluster_id[combined_data$timestamp_local == "2023-03-24 04:45:00"] <- NA
combined_data$cluster_id[combined_data$timestamp_local == "2023-03-24 04:45:00"] <- NA


# look at after April 1 as retrieval
leaflet(combined_data[combined_data$collar_id == "30551" & combined_data$date_local > "2023-04-01",]) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# moving far distances -> human travel, returning the collar to the office

# update as retrieval and outlier
combined_data$signal_status[combined_data$collar_id == "30551" & combined_data$date_local > "2023-04-01"] <- "retrieval"
combined_data$outlier[combined_data$collar_id == "30551" & combined_data$date_local > "2023-04-01"] <- 1

sum(combined_data$calibration_data, na.rm = TRUE)# 622 -> 611 dropped a bunch earlier


#.............................
#goat_10 cause of death: predation via presumed cougar; mortality signal date: 2022-07-01

# look at the data after mortality date
post10 <- combined_data[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01",] #33 fixes
leaflet(combined_data[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01",]) %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# points outside of the study area after mortality

# identify points in the study area after the reported mortality signal, not in the office
leaflet(combined_data[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude < -120,]) %>% 
  addProviderTiles("Esri.WorldTopoMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# clustering, mortality signal, non-moving fixes use for calibration
# 2022-07-04 04:00:23 looks like an outlier in the non-moving, not going to use it just in case

# update status
combined_data$signal_status[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude < -120] <- "mortality"
combined_data$outlier[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude < -120] <- 1
combined_data$calibration_data[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude < -120] <- 1
# change the 2022-07-04 04:00:23 fix as not a calibration point
combined_data[combined_data$timestamp_local == "2022-07-04 04:00:23",] 
combined_data$calibration_data[combined_data$timestamp_local == "2022-07-04 04:00:23"] <- NA
# update cluster_id
combined_data$cluster_id[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude < -120] <- "5_30567"
combined_data$cluster_id[combined_data$timestamp_local == "2022-07-04 04:00:23"] <- NA


# identify the points at the office
leaflet(combined_data[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude > -120,]) %>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)
# all points in the office, non-moving but too far apart, not going to use for calibration


# update status
combined_data$signal_status[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude > -120] <- "retrieval"
combined_data$outlier[combined_data$collar_id == "30567" & combined_data$date_local > "2022-07-01" & combined_data$longitude > -120] <- 1


sum(combined_data$calibration_data, na.rm = TRUE) # 654 -> 628 dropped the office points


#.....................................
#goat_18 cause of death: emaciated, starvation likely cause; mortality date March 16, 2021
post18 <- combined_data[combined_data$collar_id == "30636" & combined_data$date_local > "2021-03-16",] #0

# look at the the points leading up to death
leaflet(combined_data[combined_data$collar_id == "30636" & combined_data$date_local > "2021-03-14",]) %>% 
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
leaflet(combined_data[combined_data$collar_id == "30636" & combined_data$date_local >= "2021-03-16",]) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 5,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local) %>% 
  addPolylines(lng = ~longitude, lat = ~latitude)
# not using just to be safe

# no update


sum(combined_data$outlier, na.rm = TRUE) #795
sum(combined_data$calibration_data, na.rm = TRUE) #656, 654?? -> 628 dropped a bunch of calibration data when triple checking



#....................................................................
# Drop goat_03 from dataset ----
#....................................................................

#goat_03 cause of death: fall?; collar was reused on goat_12 (collar id: 30613)

# tracked for 59 days with 279 fixes, dropping goat_03 from dataset
combined_data <- combined_data[!(combined_data$collar_id == "30613" & combined_data$date_local <= "2019-08-24"),] #78020 -> 77741
78020-77741

# check to see any data wasnt dropped
combined_data[combined_data$collar_id == "30613" & combined_data$date <= "2019-08-24",] #none

sum(combined_data$outlier, na.rm = TRUE) # 740


#....................................................................
# Check to see if any points were missed during screening ----
#....................................................................

# change the remaining points from NA to 0, til they get identified, theyre all correct points, not an outlier, and update it during outlie() checks, true points til proven guilty!
combined_data$outlier[is.na(combined_data$outlier)] <- 0
table(combined_data$outlier)

# see if any of the points prior to capture were missed (first capture date was 2019-06-24)
combined_data[combined_data$outlier == 0 & combined_data$date_local < "2019-06-24",] # 2

# set as outliers
combined_data$outlier[combined_data$date_local < "2019-06-24"] <- 1

# check if all the non-goat fixes have been identified and cleaned up
leaflet(combined_data[combined_data$outlier == 0,]) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   radius = 2,  # Circle radius in meters
                   color = "red",
                   fillOpacity = 0.9,
                   label = ~timestamp_local)

# 2019-12-28 18:47:22 looks like an outlier 

# investigate the fix
combined_data[combined_data$timestamp_local == "2019-12-28 18:47:22",] # 30561, see if it gets flagged during outlie() check


# save combined_data of fixes that have been screened for non-animal fixes
save(combined_data, file = "./data/goat/prep/combined_data_screened.rda")
load("./data/goat/prep/combined_data_screened.rda")

# clean environment
rm(list = ls())
gc()

