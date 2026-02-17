
# 2025-04-30
# 2025-11-22
# 2025-12-10

# Mountain goat GPS collar data and supplementary information was provided by BC Parks

library(tidyverse)
library(ctmm)
library(lubridate)
library(dplyr)
library(leaflet)
library(mapview)
library(sf)

# webshot::install_phantomjs() # to save interactive leaflet map as an image

# data cleaning methods based on: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf
# refer to: https://www.biorxiv.org/content/biorxiv/early/2021/07/24/2020.06.12.130195.full.pdf

#import data
# load("./data/goat/prep/raw_data_screened_prepped.rda") #66099
# load("./data/goat/prep/raw_new_prepped.rda")
load("./data/goat/prep/combined_data.rda")
# load("./data/goat/prep/tel_data_merged.rda")
# load("./data/goat/prep/uere.rda")

# subset data to the version/source youre working 
# raw_data <- combined_data[combined_data$data_source == 1,] # original
# raw_data <- combined_data[combined_data$data_source == 2,] # new

# need to drop because not including measurement error
raw_data <- subset(combined_data, select = -c(fix_type, hdop, vdop, pdop, dop, altitude_m))


# Convert collar data to ctmm telemetry object
# must separate the datasets or will have convert issues with hdop and vdop values because one dataset has pdop and the other dop, also if you combine the tel_data into one list and merge the df, youll have issues as well when trying to apply uere. just best to keep them separated overall and check each by itself
tel_data <- as.telemetry(raw_data,
# tel_data <- as.telemetry(combined_data, 
                         mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id", "data_source"))

# set the folder to store images during data cleaning process
# outlie_folder <- "./figures/outlie_filtering/20251210/old/"
# outlie_folder <- "./figures/outlie_filtering/20251210/new/"
outlie_folder <- "./figures/outlie_filtering/20251230/"


# create folder if it doesnt exist
# dir.create(outlie_folder, recursive = TRUE, showWarnings = TRUE)

# create an empty list to store all the outlie data
out_data_good_loc_list <- list()
# out_data_potential_list <- list()
# create an empty vector to record potential outliers
# potential_outliers <- vector()

# ensure each df in the list is in the correct order with matching row name
for (i in 1:length(tel_data)) {
  # order the df based on fix_id
  tel_data[[i]] <- tel_data[[i]][order(tel_data[[i]][["fix_id"]]),]
  # set the rowname to match the fix_id
  rownames(tel_data[[i]] ) <- tel_data[[i]]$fix_id
}

rm(raw_data)


# cannot calibrate the fixes due to calibration data is only of 1 fix type, cannot apply error models without a lot of additional work
# therefore, not including location error (i.e., fix type, dop and UERE)



#_____________________________________________________________
# custom functions ----

# load custom function

# Note: code for this function has been modified and was originally written by Stefano Mezzini, refer to those scripts (https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/tree/main/functions) for more information

# plot_outlie
# Using custom made function, plot 4 panels to check data for outliers via visualization (give it a minute to execute)
# Plot panel description:
#1: gps points
#2: gps points connected
#3: outlie() output of distance and speed, note: "out <- outlie(telemetry, units = FALSE)", 'out' is the output of outlier within the custom function
#4: outlie() data i.e., min speed vs distance


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# output of outlie(), probably should move to outlie2() function

# distance column = `core deviation' denotes distances from the median longitude & latitude
# the speed column = `minimum speed' denotes the minimum speed required to explain the location estimate's displacement as straight-line motion
# NOTE: The speed estimates here are tailored for outlier detection and have poor statistical efficiency.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# plot_range()
# take a closer look at the points
# custom function, function to plot points near potential flagged points for further inspection 
# Note: code for this function has been modified and was originally written by Stefano Mezzini, refer to link above and plot_adj.r script for more information
# the function has options of inputs of:
# location, bounding box of the point (xmin, xmax, ymin, ymax)
# speed (speed_min, speed_max)
# number of points to buffer before/after flagged points of interest, set argument buffer = 0 if you don't want any points
# the function also saved the where they are the points before/after points of interest, and the bad points

# plot_movement_map()
# custom function to plot gps points over terrain (or some whatever leaflet has available) map
# additionally using addCircleMarker allows you to hover over the points on the map to get details of that point
# to add a raster layer to the map, refer to https://rstudio.github.io/leaflet/articles/raster.html
# Other maps to consider using: 'USGS.USTopo'
# Esri.WorldTopoMap for elevation, Esri.WorldImagery for regular map image


# expands the outlie() function to include columns being kept in as.telemetry(keep =) because outlie() does not have this option available, therefore you get the outlie() output data and grabs the columns that was indicated in as.telemetry() which contains fix_id to flag outlier points based on its fix_id

# source('scripts/functions/plot_outlie.R') 
source('scripts/functions/plot_outlie_w_map.R') 
source('scripts/functions/plot_range.R')
source('scripts/functions/plot_movement_map.R')
source('scripts/functions/outlie2.R') 





#...................................................................................
# Basic steps of outlier filtering
#...................................................................................

# based on ctmm_error.R https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_error.R

# 1. Inspect data -> look for outliers, plot & get outlie data (via plot_outlie(), movement_map() and outlier2())
# 2. Identify good/bad location estimates, flag bad location estimates (biological threshold), get fix_id of bad location fixes
# 3. Drop bad location estimates -> take only good location estimates 
# 4. re-check location estimates (via plot_outlie() and movement_map()), 
# 4b. update flag_outlier column and save out_data to good location list

# 5. Manual checks -> repeat 1-4 til cleaned (via plot_range(), plot_outlie, movement_map())
# 5b. update flag_outlier column and save out_data to potential list


# refer to the outlier_filtering folder for the script for each individual

# Average daily movement is about 1.7km/day as per Kim Poole convo, keep in mind that they may do bursts so don't set it too low


#.................................
# stages of outlier filtering
# 1. take only good location estimates (first of original, then new data)
# 2. reviewed all the saved plots of plot_outlie() of good location estimate checks to identify any potential outliers
# 3. revisualize the good locations to compare the difference and as a reference to see if there are any potential outliers or are those points where others have gone as well across all data
# 4. investigate those potential outliers manually via plot_range()




# and saved all the plots and maps then i went back in again reviewed and circled all potential outliers from the saved images to then ask help from Dr. Noonan if they are potential outliers then if they are, continue with point_checks plot_range()


