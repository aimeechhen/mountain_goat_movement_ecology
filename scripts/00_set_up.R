
# Set up

#............................................................
# Load packages ----
#............................................................

library(lubridate)
library(dplyr)
# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)
library(tidyr)
library(lutz)
library(stringr)
library(ggplot2)
library(sf)



fire_goats$goat_color <- factor(fire_goats$goat_name, levels = goats) 
col_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )
goat_palette <- c("#4477AA", "#fa9fb5", "#41ab5d", "#fed976", "#41b6c4", "#88419d", "black" )


#____________________________________________________________
# Data ----

# gps data, combined, cleaned, fire period only across all years
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")

# OUTPUTS 
load("data/movement_model/fits_20250505.rda")
load("data/home_range/akdes_20250505.rda")
load("./data/movement_model/speed_mean_20250505.rda")
load("./data/movement_model/speeds_insta_20250505.rda")
load("./data/rsf/rsf_20250505.rda")

#results
RESULTS <- read.csv("./data/combined_data_movement_hr_results_20250301.csv")



# load spatial covariate data (packedspatraster or spatraster object)
elev_25m = rast('data/rasters/elev_25m.tif')
dist_escape_25m = rast('data/rasters/dist_escape_25m.tif')




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# as ctmm telemetry object ----
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end







#...........................................

# add supplementary information ----

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
goat_data <- merge(goat_data, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)

#..........................................end






# formatting ----
collar_data$timestamp = as.POSIXct(collar_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
collar_data$month_day = as.POSIXct(collar_data$month_day, format = "%m-%d")
collar_data$date = as.Date(collar_data$date, "%Y-%m-%d")


# identify each goat and year identifier
goat_identifier <- unique(fire_goats$individual.local.identifier)

#filepaths
"data/home_range/UD"
"data/home_range/shp"


# file paths, folders
shp_path <- file.path("data/home_range/fire_goat/shp", paste0(name, ".shp"))
UD_file <- file.path("data/home_range/fire_goat/UD", paste0(names(AKDES)[i], ".tif"))
folder_path <- "data/home_range/fire_goat/shp"
dat_shp = st_read(dsn = './data/home_range/merged_95_HR_shp')





# #............................................................
# ## moving window ----
# #............................................................
# 
# # moving window raw results
# load("./data/input_data/moving_window/moving_window_1e_save_outputs_20240720.rda")
# 
# # moving window results dataframe
# mw_dat <- read.csv('./data/moving_window/moving_window_results_20240715.csv')
# 
# # mw.dat <- read.csv('./data/input_data/moving_window/moving_window_results_20240715.csv')
# 
# 




#............................................................
# FIRMS fire data cropped ----
#............................................................

modis <- st_read("C:/Users/achhen/Desktop/nasa_fire/modis_cropped/modis_cropped.shp")
viirs_suomi <- st_read("C:/Users/achhen/Desktop/nasa_fire/viirs_suomi_cropped/viirs_suomi_cropped.shp")
viirs_noaa <- st_read("C:/Users/achhen/Desktop/nasa_fire/viirs_noaa_cropped/viirs_noaa_cropped.shp")







#_________________________________________________________________________________
# Spatial ----


# load cathedral park shapefile
source('./scripts/plot/cathedral_park_shapefile.r')

# Study area bounding box: xmin: -120.4929, ymin: 48.83183, xmax: -119.435, ymax: 49.47324

# Cathedral Provincial Park
# Import Provincial park polygons
bc_parks <- st_read("./data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# Convert CRS to lat/long projection
bc_parks <- st_transform(bc_parks, crs = st_crs(4326))
st_crs(bc_parks)
#subset to cathedral park
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
cathedral #spatial information


# dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
# dat_sf <- st_as_sf(dat, coords = c("location.long", "location.lat"), crs = 4326)
# dat_sf #spatial information
# bbox <- st_bbox(dat_sf)
# bbox_polygon <- st_as_sfc(st_bbox(dat_sf))
# bbox_centroid <- st_centroid(bbox_polygon)
# st_coordinates(bbox_centroid)



