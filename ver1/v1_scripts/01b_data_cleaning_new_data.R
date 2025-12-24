
# 2025-04-30

library(tidyverse)
library(ctmm)
library(lubridate)
library(dplyr)
library(leaflet)
library(mapview)


# vertical position dilution of precision factor (VDOP) quantifies the effect of changes in the size and spatial configuration of the available satellite network on the precision of GPS records (https://doi.org/10.1186/s40317-020-00194-z)


# Mountain goat GPS collar data and supplementary information was provided by BC Parks

# collar data from 2019 to before sept 2023 was completed by Stefano Mezzini
# refer to those scripts here:
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/00-compile-goat-tracking-data.R
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/01-compile-tracking-data.R
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/01a-compile-goat-calibration-data.R
# https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/analysis/02-remove-outliers.R


#.........................................................................
# steps on cleaning collar data from sept 2023 to feb 2025 is completed below
# data cleaning methods based on: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf
# refer to: https://www.biorxiv.org/content/biorxiv/early/2021/07/24/2020.06.12.130195.full.pdf

# Basic steps (Part 1: auto checks via outlier() then Part 2: manual checks)
# 1. Check data
# 2. Flag points
# 3. Drop points and re-check data
# 4. Add points to flagged points list
# 5. Manual checks -> repeat 1-4 til cleaned

#................................................
# Import data ----
raw_data <- read.csv("data/collar_data/raw_collar/Cathedral Goat locations Sept 2023 through Feb 10 2025.csv") %>%
  janitor::clean_names() #remove unnecessary characters ("[", " ", etc.) and replace with "_" 
#inspect data
names(raw_data)
head(raw_data)
str(raw_data)
# acq_time_utc = GPS fix acquisition time (actual recorded fix time)
# scts_utc = data is sent or received to Iridium satellite network (origin column), i.e., sent to system .-. after acquisition time

# drop animal column or error will occur when trying to convert to a ctmm telemetry object, and drop columns that provide duplicate information such as collar number
raw_data <- subset(raw_data, select = -c(animal, collar_name, scts_utc))
raw_data$acq_time_utc <- as.POSIXct(raw_data$acq_time_utc)
raw_data$collar_id <- as.factor(raw_data$collar_id)
raw_data$data_type <- "new_collar"

# check for duplicates
raw_data[duplicated(raw_data[, c("acq_time_utc", "latitude_deg", "longitude_deg", "collar_id")]), ] # none
# cannot add goat_info data or get warnings of duplicates

# create new columns to format names to match required for ctmm based on Movebank critera:
raw_data$individual.local.identifier <- raw_data$collar_id
raw_data$timestamp <- raw_data$acq_time_utc
raw_data$location.lat <- raw_data$latitude_deg
raw_data$location.long <- raw_data$longitude_deg

# extract row number and create a column
raw_data$row_number <- rownames(raw_data)

# Convert collar data to ctmm telemetry object
tel_data <- as.telemetry(raw_data) # average sampling time is 6.25h

# if no calibration data then assign RMS UERE value,  i.e. add User Equivalent Range Error or a default error of 10 m
uere(tel_data) <- 10
# calibration data = like if the collar is not moving i.e., morality signal
# hdop is unitless but if you need to put some kind of units its like hdop=1 is like 10 mins from that stationary signal


#........................................................
# Data prep ----
#........................................................

# create a outlie dataframe

# create a df based on the ctmm telemetry object
tel_df <- do.call(rbind, lapply(names(tel_data), function(id) {
  df <- tel_data[[id]]  # Extract each dataframe
  df$collar_id <- id    # Assign list name as collar_id
  df$row_number <- rownames(df)  # Add row names as a column
  return(df)
}))

# obtain the outlie outputs for all individuals
outlie_results <- outlie(data = tel_data, plot = FALSE) # output is a list

# convert outlie output into a dataframe
outlie_df <- do.call(rbind, lapply(outlie_results, function(df) {
  df$row_number <- rownames(df)  # Add row names as a column
  return(df)
}))

# combine all dfs together by rownames (i.e. adding columns of df2 to df1 and matching based on row names)
dat <- merge(raw_data, tel_df, by = c("row_number", "timestamp"), all = TRUE)
outlie_data <- merge(dat, outlie_df, by = c("row_number", "t"), all = TRUE)
outlie_data <- relocate(outlie_data, t, .after = "latitude")


# set rownames back to original
rownames(outlie_data) <- outlie_data$row_number
#create a flagged outlier column and assign all to 0 to be flagged below
outlie_data$flag_outlier <- 0

#clean environment
rm(dat, outlie_df, outlie_results, tel_df)


#_____________________________________________________________
# custom functions ----

# load custom functions
#Note: code for the function has been modified and was originally written by Stefano Mezzini, refer to script for more information and source)
source('scripts/functions/1_find_angle_outlier_plots.R') 
# Using custom made function, it calculates the turn angle and then plot 6 plots to check data for outliers via visualization (give it a minute to execute)
# Plot description:
#1: gps points
#2: gps points connected
#3: outlie() output of distance and speed, note: "out <- outlie(telemetry, units = FALSE)", 'out' is the output of outlier within the custom function
#4: outlie() data i.e., min speed vs distance
#5: turn angles of points
#6: time between points i.e., the intervals, if point B was missed then point A to C will be considered the interval time


# take a closer look
# load function, function to plot points near potential flagged points for further inspection 
# Note: code for the function has been modified and originally written by Stefano Mezzini, refer to script for more information and source
source('scripts/functions/2_plot_adj_modified.R') 
# the function has options of inputs of:
# location, bounding box of the point (xmin, xmax, ymin, ymax)
# speed (speed_min, speed_max)
# turn angle (angle_min, angle_max)
# number of points to buffer before/after flagged points of interest, buffer = 0 if you dont want any points
# the function also saved the adj_points where they are the points before/after points of interest, and the bad points


# custom function to plot gps points over terrain map, you can hover over the points on the map to get details of that point
# Other maps to consider using: 'USGS.USTopo'
# to add a raster layer to the map, refer to https://rstudio.github.io/leaflet/articles/raster.html
movement_map <- function(data, map_type = 'Esri.WorldImagery') {
  leaflet(data = data) %>%
    addProviderTiles(provider = map_type) %>%
    # movement path
    addPolylines(
      lng = data$longitude,
      lat = data$latitude,
      color = "red",
      weight = 2,
      opacity = 1
    ) %>%
    # gps points
    addCircleMarkers(
      lng = data$longitude,
      lat = data$latitude,
      color = "red",
      radius = 4,
      fillOpacity = 0.8,
      stroke = FALSE,
      label = ~as.character(data$timestamp)
    )
}







#.....................................................................................
# 30548 goatzilla ----
#.....................................................................................

# create a running list to store all the row numbers of flagged points 
outlier_rows <- vector()

# subset an individual
animal <- tel_data[[1]]
# add a column called collar_id (required for custom functions to work)
animal$collar_id <- names(tel_data)[1]
collar_id <- names(tel_data)[1]

# Step 1: Inspect the data 
png(file = paste0("figures/outlie_checks/raw_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
outlie_check <- outlier_plots(animal, return = TRUE) #outlier_plot(x), x is a single individual telemetry ctmm data
dev.off()
#overlay tracking data on a map
raw_tracks <- movement_map(animal)
# view map
raw_tracks
# save an image of the plot
mapshot(raw_tracks, file = paste0("./figures/outlie_checks/raw_tracks_", collar_id, ".png"))

# Step 2: flag points -> points above 0.1 m/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
outlie_check$flag_outlier <- ifelse(outlie_check$speed >= 0.10, 1, 0) 
# number of outliers flagged 
sum(outlie_check$flag_outlier) #20
# extract the row numbers of flagged points 
flagged <- rownames(outlie_check[outlie_check$flag_outlier == 1,])

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% flagged), ]
# re-check the data with the dropped flagged points, compare to the map plot in Viewer to see what the removal looks like
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

# Step 4: add flagged rows to list and change those rows to 1 in the main df
# add to list
outlier_rows <- c(outlier_rows, flagged)

# update dataframe
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)




#.................................................................
## goatzilla Manual checks ----
#.................................................................

# separate the two so you dont override if you make an error
part1 <- outlier_rows


#......................................................
### point 1 ----

# take a closer look
# location points -> set the bounding box around the point
point_check <- plot_adj(flag_check,
                        xmin = 4500,
                        xmax = 5100,
                        ymin = -6000,
                        ymax = -5000,              
                        buffer = 10)
movement_map(flag_check)

# points seem reasonable, nothing to update, moving onto next individual, save plot
png(file = paste0("figures/outlie_checks/clean_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 
dev.off()
#overlay tracking data on a map
clean_tracks <- movement_map(flag_check)
# view map
clean_tracks
# save an image of the plot
mapshot(clean_tracks, file = paste0("./figures/outlie_checks/clean_tracks_", collar_id, ".png"))


#clean up environment and clear plots 
rm(animal, flag_check, outlie_check, point_check, recheck, flagged, collar_id, outlier_rows, part1, part2, raw_tracks)
dev.off(dev.list()["RStudioGD"])




#.....................................................................................
# 30561 selena_goatmez ----
#.....................................................................................

# create a running list to store all the row numbers of flagged points 
outlier_rows <- vector()

# subset an individual
animal <- tel_data[[2]]
# add a column called collar_id (required for custom functions to work)
animal$collar_id <- names(tel_data)[2]
collar_id <- names(tel_data)[2]

# Step 1: Inspect the data 
png(file = paste0("figures/outlie_checks/raw_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
outlie_check <- outlier_plots(animal, return = TRUE) #outlier_plot(x), x is a single individual telemetry ctmm data
dev.off()
#overlay tracking data on a map
raw_tracks <- movement_map(animal)
raw_tracks
# save an image of the plot
mapshot(raw_tracks, file = paste0("./figures/outlie_checks/raw_tracks_", collar_id, ".png"))

# Step 2: flag points -> points above 0.1 m/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
outlie_check$flag_outlier <- ifelse(outlie_check$speed >= 0.10, 1, 0) 
# number of outliers flagged 
sum(outlie_check$flag_outlier) #20
# extract the row numbers of flagged points 
flagged <- rownames(outlie_check[outlie_check$flag_outlier == 1,])

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% flagged), ]
# re-check the data with the dropped flagged points, compare to the map plot in Viewer to see what the removal looks like
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

# Step 4: change those rows to 1 and add flagged rows to running list
# add to list
outlier_rows <- c(outlier_rows, flagged)

# update dataframe
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)


#.................................................................
## Manual checks ----
#.................................................................

# separate the two so you dont override if you make an error
part1 <- outlier_rows
part2 <- vector()

#......................................................
### point 1 ----

# take a closer look
# location points -> set the bounding box around the point, off shot near the middle
point_check <- plot_adj(flag_check,
                        xmin = -2200,
                        xmax = -1800,
                        ymin = -500,
                        ymax = 0,              
                        buffer = 10)
movement_map(flag_check)

# extract adjacent points
adj_points <- point_check$adj_points
# extract the point to double check
bad_point <- point_check$bad_point
plot(bad_point)

# Step 4: flag point and add flagged rows to running list
# flag point
flagged <- rownames(bad_point)
# add this row number to running list
part2 <- c(part2, flagged)

rm(point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 2 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reinspect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# take a closer look
# location points -> set the bounding box around the point, left side on the edge, looks like an offshoot
point_check <- plot_adj(flag_check,
                        xmin = -5000,
                        xmax = -4900,
                        ymin = -1800,
                        ymax = -1500,              
                        buffer = 10)

# points seem reasonable, .-. add manual flagged (part2) to outlier list
outlier_rows <- c(outlier_rows, part2)

# then take all the points that have been flagged and add them to the outlie data and assign them a 1 if its in the list and leave them as is if they are not
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)

# moving onto next individual, save plot
png(file = paste0("figures/outlie_checks/clean_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 
dev.off()
#overlay tracking data on a map
clean_tracks <- movement_map(flag_check)
clean_tracks
# save an image of the plot
mapshot(clean_tracks, file = paste0("./figures/outlie_checks/clean_tracks_", collar_id, ".png"))

#clean up environment and clear plots 
rm(animal, flag_check, outlie_check, point_check, recheck, flagged, collar_id, outlier_rows, part1, part2, raw_tracks)
dev.off(dev.list()["RStudioGD"])






#.....................................................................................
# 30575 the_goatmother ----
#.....................................................................................

# create a running list to store all the row numbers of flagged points 
outlier_rows <- vector()

# subset an individual
animal <- tel_data[[3]]
# add a column called collar_id (required for custom functions to work)
animal$collar_id <- names(tel_data)[3]
collar_id <- names(tel_data)[3]

# Step 1: Inspect the data 
png(file = paste0("figures/outlie_checks/raw_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
outlie_check <- outlier_plots(animal, return = TRUE) #outlier_plot(x), x is a single individual telemetry ctmm data
dev.off()
#overlay tracking data on a map
raw_tracks <- movement_map(animal)
raw_tracks
# save an image of the plot
mapshot(raw_tracks, file = paste0("./figures/outlie_checks/raw_tracks_", collar_id, ".png"))

# Step 2: flag points -> points above 0.1 m/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
outlie_check$flag_outlier <- ifelse(outlie_check$speed >= 0.10, 1, 0) 
# number of outliers flagged 
sum(outlie_check$flag_outlier) #11
# extract the row numbers of flagged points 
flagged <- rownames(outlie_check[outlie_check$flag_outlier == 1,])

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% flagged), ]
# re-check the data with the dropped flagged points
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

# Step 4: change those rows to 1 and add flagged rows to running list
# add to list
outlier_rows <- c(outlier_rows, flagged)

# update dataframe
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)


#.................................................................
## the_goatmother Manual checks ----
#.................................................................

# separate the two so you dont override if you make an error
part1 <- outlier_rows
part2 <- vector()

#......................................................
### point 1 ----

# take a closer look
# location points -> set the bounding box around the point, near the middle, top-right of that cluster
point_check <- plot_adj(flag_check,
                        xmin = -1000,
                        xmax = 0,
                        ymin = 5000,
                        ymax = 6000,              
                        buffer = 10)
movement_map(flag_check)
# looks like an outlier, extract adjacent points
adj_points <- point_check$adj_points
# extract the point to double check
bad_point <- point_check$bad_point
plot(bad_point)

# Step 4: flag point and add flagged rows to running list
# flag point
flagged <- rownames(bad_point)
# add this row number to running list
part2 <- c(part2, flagged)

rm(outlie_check, point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 2 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# take a closer look
# location points -> set the bounding box around the point, very bottom left corner
point_check <- plot_adj(flag_check,
                        xmin = -4500,
                        xmax = -4000,
                        ymin = -2000,
                        ymax = -1500,              
                        buffer = 10)

# looks like an outlier, extract adjacent points however, another goat has this too (goatileo), leaving it in for now
# adj_points <- point_check$adj_points
# # extract the point to double check
# bad_point <- point_check$bad_point
# plot(bad_point)
# 
# # Step 4: flag point and add flagged rows to running list
# # flag point
# flagged <- rownames(bad_point)
# # add this row number to running list
# part2 <- c(part2, flagged)

rm(point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 3 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# points seem reasonable, .-. add manual flagged (part2) to outlier list
outlier_rows <- c(outlier_rows, part2)

# then take all the points that have been flagged and add them to the outlie data and assign them a 1 if its in the list and leave them as is if they are not
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)

# moving onto next individual, save plot
png(file = paste0("figures/outlie_checks/clean_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 
dev.off()
#overlay tracking data on a map
clean_tracks <- movement_map(flag_check)
clean_tracks
# save an image of the plot
mapshot(clean_tracks, file = paste0("./figures/outlie_checks/clean_tracks_", collar_id, ".png"))

#clean up environment and clear plots 
rm(animal, flag_check, outlie_check, point_check, recheck, flagged, collar_id, outlier_rows, part1, part2, raw_tracks)
dev.off(dev.list()["RStudioGD"])





#.....................................................................................
# 30613 goatileo ----
#.....................................................................................

# create a running list to store all the row numbers of flagged points 
outlier_rows <- vector()

# subset an individual
animal <- tel_data[[4]]
# add a column called collar_id (required for custom functions to work)
animal$collar_id <- names(tel_data)[4]
collar_id <- names(tel_data)[4]

# Step 1: Inspect the data 
png(file = paste0("figures/outlie_checks/raw_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
outlie_check <- outlier_plots(animal, return = TRUE) #outlier_plot(x), x is a single individual telemetry ctmm data
dev.off()
#overlay tracking data on a map
raw_tracks <- movement_map(animal)
raw_tracks
# save an image of the plot
mapshot(raw_tracks, file = paste0("./figures/outlie_checks/raw_tracks_", collar_id, ".png"))

# Step 2: flag points -> points above 0.1 m/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
outlie_check$flag_outlier <- ifelse(outlie_check$speed >= 0.10, 1, 0) 
# number of outliers flagged 
sum(outlie_check$flag_outlier) #22
# extract the row numbers of flagged points 
flagged <- rownames(outlie_check[outlie_check$flag_outlier == 1,])

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% flagged), ]
# re-check the data with the dropped flagged points
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

# Step 4: change those rows to 1 and add flagged rows to running list
# add to list
outlier_rows <- c(outlier_rows, flagged)

# update dataframe
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)


#.................................................................
## goatileo Manual checks ----
#.................................................................

# separate the two so you dont override if you make an error
part1 <- outlier_rows
part2 <- vector()

#......................................................
### point 1 ----

# take a closer look
# location points -> set the bounding box around the point, big ass offshot in the middle
point_check <- plot_adj(flag_check,
                        xmin = -4000,
                        xmax = -3000,
                        ymin = 4000,
                        ymax = 6000,              
                        buffer = 10)

# looks like an outlier, extract adjacent points
adj_points <- point_check$adj_points
# extract the point to double check
bad_point <- point_check$bad_point
plot(bad_point)

# Step 4: flag point and add flagged rows to running list
# flag point
flagged <- rownames(bad_point)
# add this row number to running list
part2 <- c(part2, flagged)

rm(outlie_check, point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 2 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# take a closer look
# location points -> set the bounding box around the point, very bottom left corner, offshoot
point_check <- plot_adj(flag_check,
                        xmin = -4500,
                        xmax = -4000,
                        ymin = -2000,
                        ymax = -1500,              
                        buffer = 10)

# looks like an outlier, extract adjacent points but similar point as the goatmother, leaving it in for now
# adj_points <- point_check$adj_points
# # extract the point to double check
# bad_point <- point_check$bad_point
# plot(bad_point)
# 
# # Step 4: flag point and add flagged rows to running list
# # flag point
# flagged <- rownames(bad_point)
# # add this row number to running list
# part2 <- c(part2, flagged)

rm(point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 3 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# take a closer look
# location points -> set the bounding box around the point, looks like its in down in the valley, another individual (toats_mcgoats) went down into the valley too, not removing for now
# point_check <- plot_adj(flag_check,
#                         xmin = -2600,
#                         xmax = -2000,
#                         ymin = 1500,
#                         ymax = 2000,              
#                         buffer = 10)
# 
# # looks like an outlier, extract adjacent points
# adj_points <- point_check$adj_points
# # extract the point to double check
# bad_point <- point_check$bad_point
# plot(bad_point)
# 
# # Step 4: flag point and add flagged rows to running list
# # flag point
# flagged <- rownames(bad_point)
# # add this row number to running list
# part2 <- c(part2, flagged)

rm(outlie_check, point_check, flag_check, adj_points, bad_point, recheck)





#......................................................
### point 4 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# take a closer look
# location points -> set the bounding box around the point
point_check <- plot_adj(flag_check,
                        xmin = -2600,
                        xmax = -2000,
                        ymin = 1500,
                        ymax = 2000,              
                        buffer = 10)

# points seem reasonable, .-. add manual flagged (part2) to outlier list
outlier_rows <- c(outlier_rows, part2)

# then take all the points that have been flagged and add them to the outlie data and assign them a 1 if its in the list and leave them as is if they are not
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)

# moving onto next individual, save plot
png(file = paste0("figures/outlie_checks/clean_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 
dev.off()
#overlay tracking data on a map
clean_tracks <- movement_map(flag_check)
clean_tracks
# save an image of the plot
mapshot(clean_tracks, file = paste0("./figures/outlie_checks/clean_tracks_", collar_id, ".png"))

#clean up environment and clear plots 
rm(animal, flag_check, outlie_check, point_check, recheck, flagged, collar_id, outlier_rows, part1, part2, raw_tracks)
dev.off(dev.list()["RStudioGD"])






#.....................................................................................
# 30642 toats_mcgoats ----
#.....................................................................................

# create a running list to store all the row numbers of flagged points 
outlier_rows <- vector()

# subset an individual
animal <- tel_data[[5]]
# add a column called collar_id (required for custom functions to work)
animal$collar_id <- names(tel_data)[5]
collar_id <- names(tel_data)[5]

# Step 1: Inspect the data 
png(file = paste0("figures/outlie_checks/raw_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
outlie_check <- outlier_plots(animal, return = TRUE) #outlier_plot(x), x is a single individual telemetry ctmm data
dev.off()
#overlay tracking data on a map
raw_tracks <- movement_map(animal)
raw_tracks
# save an image of the plot
mapshot(raw_tracks, file = paste0("./figures/outlie_checks/raw_tracks_", collar_id, ".png"))

# Step 2: flag points -> points above 0.1 m/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
outlie_check$flag_outlier <- ifelse(outlie_check$speed >= 0.10, 1, 0) 
# number of outliers flagged 
sum(outlie_check$flag_outlier) #19
# extract the row numbers of flagged points 
flagged <- rownames(outlie_check[outlie_check$flag_outlier == 1,])

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% flagged), ]
# re-check the data with the dropped flagged points
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

# Step 4: change those rows to 1 and add flagged rows to running list
# add to list
outlier_rows <- c(outlier_rows, flagged)

# update dataframe
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)


#.................................................................
## toats_mcgoats Manual checks ----
#.................................................................

# separate the two so you dont override if you make an error
part1 <- outlier_rows
part2 <- vector()

#......................................................
### point 1 ----

# take a closer look
# location points -> set the bounding box around the point, middle of map, offshoot
point_check <- plot_adj(flag_check,
                        xmin = -1000,
                        xmax = 0,
                        ymin = 3000,
                        ymax = 4000,              
                        buffer = 10)
movement_map(flag_check)

# seems okay, looks like it went down the cliff then came back up

# # looks like an outlier, extract adjacent points
# adj_points <- point_check$adj_points
# # extract the point to double check
# bad_point <- point_check$bad_point
# plot(bad_point)
# 
# # Step 4: flag point and add flagged rows to running list
# # flag point
# flagged <- rownames(bad_point)
# # add this row number to running list
# part2 <- c(part2, flagged)

rm(outlie_check, point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 2 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# points seem reasonable, .-. add manual flagged (part2) to outlier list
outlier_rows <- c(outlier_rows, part2)

# then take all the points that have been flagged and add them to the outlie data and assign them a 1 if its in the list and leave them as is if they are not
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)

# moving onto next individual, save plot
png(file = paste0("figures/outlie_checks/clean_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

#overlay tracking data on a map
clean_tracks <- movement_map(flag_check)
clean_tracks
# save an image of the plot
mapshot(clean_tracks, file = paste0("./figures/outlie_checks/clean_tracks_", collar_id, ".png"))

#clean up environment and clear plots 
rm(animal, flag_check, outlie_check, point_check, recheck, flagged, collar_id, outlier_rows, part1, part2, raw_tracks)
dev.off(dev.list()["RStudioGD"])







#.....................................................................................
# 30648 vincent_van_goat ----
#.....................................................................................

# create a running list to store all the row numbers of flagged points 
outlier_rows <- vector()

# subset an individual
animal <- tel_data[[6]]
# add a column called collar_id (required for custom functions to work)
animal$collar_id <- names(tel_data)[6]
collar_id <- names(tel_data)[6]

# Step 1: Inspect the data 
png(file = paste0("figures/outlie_checks/raw_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
outlie_check <- outlier_plots(animal, return = TRUE) #outlier_plot(x), x is a single individual telemetry ctmm data
dev.off()
#overlay tracking data on a map
raw_tracks <- movement_map(animal)
raw_tracks
# save an image of the plot
mapshot(raw_tracks, file = paste0("./figures/outlie_checks/raw_tracks_", collar_id, ".png"))

# Step 2: flag points -> points above 0.1 m/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
outlie_check$flag_outlier <- ifelse(outlie_check$speed >= 0.10, 1, 0) 
# number of outliers flagged 
sum(outlie_check$flag_outlier) #30
# extract the row numbers of flagged points 
flagged <- rownames(outlie_check[outlie_check$flag_outlier == 1,])

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% flagged), ]
# re-check the data with the dropped flagged points
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 

# Step 4: change those rows to 1 and add flagged rows to running list
# add to list
outlier_rows <- c(outlier_rows, flagged)

# update dataframe
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)


#.................................................................
## toats_mcgoats Manual checks ----
#.................................................................

# separate the two so you dont override if you make an error
part1 <- outlier_rows
part2 <- vector()

#......................................................
### point 1 ----

# take a closer look
# location points -> set the bounding box around the point, top right of the map, offshoot
point_check <- plot_adj(flag_check,
                        xmin = 2000,
                        xmax = 3000,
                        ymin = 5000,
                        ymax = 6000,              
                        buffer = 10)
movement_map(flag_check)
# looks like an outlier, extract adjacent points
adj_points <- point_check$adj_points
# extract the point to double check
bad_point <- point_check$bad_point
plot(bad_point)

# Step 4: flag point and add flagged rows to running list
# flag point
flagged <- rownames(bad_point)
# add this row number to running list
part2 <- c(part2, flagged)

rm(outlie_check, point_check, flag_check, adj_points, bad_point, recheck)

#......................................................
### point 2 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)
# take a closer look
# location points -> set the bounding box around the point, middle bottom, offshoot
point_check <- plot_adj(flag_check,
                        xmin = -2000,
                        xmax = -1000,
                        ymin = 0,
                        ymax = 500,              
                        buffer = 10)

# looks like an outlier, extract adjacent points
adj_points <- point_check$adj_points
# extract the point to double check
bad_point <- point_check$bad_point
plot(bad_point)

# Step 4: flag point and add flagged rows to running list
# flag point
flagged <- rownames(bad_point)
# add this row number to running list
part2 <- c(part2, flagged)

rm(point_check, flag_check, adj_points, bad_point, recheck)





#......................................................
### point 3 ----

# Step 3: Drop flagged points and re-check (check 2) data with flagged points
# exclude all the points that are flagged
flag_check <- animal[!(rownames(animal) %in% c(part1, part2)), ]
# reoinpsect data again with point dropped
recheck <- outlier_plots(flag_check)
movement_map(flag_check)

# points seem reasonable, .-. add manual flagged (part2) to outlier list
outlier_rows <- c(outlier_rows, part2)

# then take all the points that have been flagged and add them to the outlie data and assign them a 1 if its in the list and leave them as is if they are not
outlie_data$flag_outlier <- ifelse(rownames(outlie_data) %in% outlier_rows, 1, outlie_data$flag_outlier)

# moving onto next individual, save plot
png(file = paste0("figures/outlie_checks/clean_check_", collar_id, ".png"), width = 14, height = 6, units = "in", res = 600)
recheck <- outlier_plots(flag_check) #outlier_plot(x), x is a single individual telemetry ctmm data 
dev.off()
#overlay tracking data on a map
clean_tracks <- movement_map(flag_check)
clean_tracks
# save an image of the plot
mapshot(clean_tracks, file = paste0("./figures/outlie_checks/clean_tracks_", collar_id, ".png"))

#clean up environment and clear plots 
rm(animal, flag_check, outlie_check, point_check, recheck, flagged, collar_id, outlier_rows, part1, part2, raw_tracks)
dev.off(dev.list()["RStudioGD"])


#////////////////////////////////////////////////////////////////// 


# sum up all the points that have been flagged
sum(outlie_data$flag_outlier) #127 in total



# save the dataframe with flagged outliers
save(outlie_data, file = "./data/collar_data/outlie_data_20250505.rda")
load("./working/data/collar_data/outlie_data_20250505.rda")



