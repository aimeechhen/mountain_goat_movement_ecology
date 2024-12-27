
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



#____________________________________________________________
# Data ----

# extracted clean collar data
# extracted_collar <- read.csv('./data/collar_data/extracted_clean_collar_data_20240703.csv')

# collar 
# load("data/collar_data/collar_data_20240703.rda")
load("data/collar_data/collar_data_20241123.rda")

collar_data$timestamp = as.POSIXct(collar_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
collar_data$month_day = as.POSIXct(collar_data$month_day, format = "%m-%d")
collar_data$date = as.Date(collar_data$date, "%Y-%m-%d")

# add goat name
goat_info <- read.csv("data/goat_info.csv")


# full data
load("data/movement_model/goat_fits_20241107.rda")
load("data/home_range/goat_akdes_20241217.rda")
load("./data/movement_model/goat_speed_mean_20241226.rda")
load("./data/movement_model/goat_speeds_insta_20241226.rda")

#results
load("data/home_range/hr_size_20241226.rda")
load("./data/movement_model/goat_fits_summary_20241226.rda")

#filepaths
"data/home_range/UD"
("data/home_range/shp"


# fire goats
load("data/movement_model/fire_goat_fits_20241217.rda")
load("data/home_range/fire_goat_akdes_20241217.rda")
load(file = "./data/movement_model/fire_goat_speed_mean_20241217.rda")
load(file = "./data/movement_model/fire_goat_speeds_insta_20241217.rda")

#results
load("./data/home_range/fire_goat_hr_movement_results_df_20241219.rda")
dat_shp = st_read(dsn = './data/home_range/merged_95_HR_shp')
load("./data/rsf/fire_goats_rsf_20241220.rda")

# file paths
shp_path <- file.path("data/home_range/fire_goat/shp", paste0(name, ".shp"))
UD_file <- file.path("data/home_range/fire_goat/UD", paste0(names(AKDES)[i], ".tif"))
folder_path <- "data/home_range/fire_goat/shp"


# load spatial covariate data (packedspatraster or spatraster object)
elev_25m = rast('data/rasters/elev_25m.tif')
dist_escape_25m = rast('data/rasters/dist_escape_25m.tif')



# load cathedral park shapefile
source('./scripts/plot/cathedral_park_shapefile.r')


#............................................................
## moving window ----
#............................................................

# moving window raw results
load("./data/input_data/moving_window/moving_window_1e_save_outputs_20240720.rda")

# moving window results dataframe
mw_dat <- read.csv('./data/moving_window/moving_window_results_20240715.csv')

# mw.dat <- read.csv('./data/input_data/moving_window/moving_window_results_20240715.csv')






#............................................................
# NASA fire data cropped ----
#............................................................

modis <- st_read("C:/Users/achhen/Desktop/nasa_fire/modis_cropped/modis_cropped.shp")
viirs_suomi <- st_read("C:/Users/achhen/Desktop/nasa_fire/viirs_suomi_cropped/viirs_suomi_cropped.shp")
viirs_noaa <- st_read("C:/Users/achhen/Desktop/nasa_fire/viirs_noaa_cropped/viirs_noaa_cropped.shp")



#_________________________________________________________________________________
# Spatial ----

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



# helicopter
load("C:/Users/achhen/OneDrive - UBC/Github/Mountain_Goat/data/helicopter/heli_gps.rda")
dat_sf <- st_as_sf(heli_gps, coords = c("long", "lat"), crs = 4326)
dat_sf #spatial information
bbox <- st_bbox(dat_sf)
bbox_polygon <- st_as_sfc(st_bbox(dat_sf))
bbox_centroid <- st_centroid(bbox_polygon)
st_coordinates(bbox_centroid)


