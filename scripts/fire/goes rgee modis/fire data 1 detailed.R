
library(rgee)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(stringr)

rgee::ee_Initialize(user = 'katchhen@gmail.com', drive = TRUE, gcs = FALSE)

rm(list = ls())




#....................................................................
# Cathedral Park
#....................................................................

#check the if area of interest is correct wrt to cathedral park
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
bc_parks <- st_transform(bc_parks, crs = st_crs(4326))
#subset to cathedral park
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
# Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")

# convert sf to gee object
cathedral_gee <- sf_as_ee(cathedral$geometry)
# geojson_string <- st_write(cathedral, driver = "GeoJSON", quiet = TRUE)
# cathedral_gee <- ee$FeatureCollection(geojson_string)
# check if it converted
# cathedral_gee$getInfo()



#....................................................................
# Area of interest
#....................................................................


# set parameters
cratercreek <- data.frame(
  longitude = -120.2528,
  latitude = 48.95914,
  start = '2023-08-21',
  end = '2023-08-22'
)

# Let's set our area of interest
# // Region of interest.
radius_of_interest_meters <- 40000

# Create a geometry based on parameters as GEE object
area_of_interest = ee$Geometry$Point(c(cratercreek$longitude, cratercreek$latitude))$
  buffer(radius_of_interest_meters)



# #........................................................
# # Map
# #........................................................
# 
# # Center map focused on the area of interest
# Map$centerObject(eeObject = area_of_interest,
#                  zoom = 8)
# 
# # plot park
# map_park <-
#   Map$addLayer(
#     eeObject = cathedral_gee,
#     visParams = list(color = 'grey'),
#     name = 'Cathedral Park',
#     shown = TRUE,
#     opacity = 0.2)
# 
# # plot area of interest
# map_aoi <-
#   Map$addLayer(
#     eeObject = area_of_interest,
#     visParams = list(color = 'green'),
#     name = "Area of Interest",
#     shown = TRUE,
#     opacity = 0.2)
# 
# 
# map_park + map_aoi


#....................................................................
# GOES sat data ----
#....................................................................



# // Load GOES-18 Image Collection, filter date (image = raster; featurecollection = vector)
goes_18_data = ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate(cratercreek$start, cratercreek$end)$
  filterBounds(area_of_interest)
# goes_16_data = ee$ImageCollection('NOAA/GOES/16/FDCF')$
#   filterDate(cratercreek$start, cratercreek$end)$
#   filterBounds(area_of_interest)

# MIGHT NEED TO ADD AND COMBINE GOES-16
# goes_data <- c(goes_18_data, goes_16_data)


#check data details
# ee_print(goes_18_data)
# If there are a lot of data, test a subset of it
ee_print(goes_18_data)$limit(5)
ee_print(goes_18_data)$limit(5)$getInfo()

#select the first image
image <- goes_18_data$first()

#View image information
ee_print(image)

#........................................................................
# Inspect image properties ----

# Get the band names (variables) from the first image
image$bandNames()$getInfo()
# Get the properties of the image
image$propertyNames()$getInfo()
# Inspect raw metadata
ee$Image(image)$getInfo()

#Retrieve raw metadata
ee$Image(image)$getInfo()$id #first 13 digits format as year, day of the year, time (HHMMSS), then last 4 sequence id number???
# ee$Image(image)$getInfo()$system:time_start # gives an output of -1, which doesnt make sense??





# Area: The area of detected fire pixels.
# Temp: Brightness temperature (BT) in kelvins, specifically for the middle-infrared channel.
# Mask: Fire mask (layer) indicating the presence of fire, typically with specific codes for different fire confidence levels.
# Power: Fire radiative power.
# DQF: Data Quality Flags indicating the quality of the data for each pixel.


# // Select variables of image.
dqf = image$select('DQF')
# area = image$select('Area')
# temp = image$select('Temp')
mask <- image$select('Mask')
# power <- image$select('Power')


#.....................................
# DQF ----
#...................................

DQFVis <- list(
  min = 0,
  max =  5,
  palette = c('blanchedalmond',  #// Good quality fire pixel
              'darkgreen',       #// Good quality fire free land
              'darkcyan',        #// Opaque cloud, Bad surface type, sunglint, LZA threshold exceeded,
              'darkslateblue',   #// off Earth, or missing input data
              'lemonchiffon',    #// Bad input data
              'burlywood'))      #// Algorithm failure

# Plot DQF
# map_dqf <- 
Map$addLayer(eeObject = dqf, 
             visParams = DQFVis, 
             name = 'Fire Data Quality Flags', 
             shown = TRUE, 
             opacity = 0.7)



#.....................................
# Mask (layer) ----
#...................................

#mask codes (10–15 and 30–35) indicate that the pixel is believed to cover a wildfire, with a varying degree of confidence.
# 10: processed fire pixel
# 11: saturated fire pixel 
# 12: cloud contaminated fire pixel 
# 13: high probability fire pixel 
# 14: medium probability fire pixel 
# 15: low probability fire pixel

# “temporally filtered” code values: 
# 30: temporally filtered process fire pixel
# 31: temporally filtered saturated fire pixel

# 10 or 30	Good quality (highest confidence) fire detections**
#   11 or 31	Saturated fire pixel**

# // Filter to show only pixels that signify a high likelihood of fire and assigning (reclassifying) them a value of 1 (fire) vs 0 = no fire/low probability
# remaps the specified values specified by map codes that indicates a high likelihood of fire to 1.
mask_remapped <- mask$remap(c(10, 11, 12, 13, 14, 30, 31, 32, 33, 34),
                            c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
# Filter pixels that signify a high likelihood of fire.
onlyfire <- mask_remapped$eq(1)

#MIGHT NEED TO CLIP (CROP) THE IMAGE

# map_onlyfire <-
Map$addLayer(eeObject = onlyfire$visualize(
  palette = c('red'), min = 1, max = 1), #image (raster) needs to use 'palette = ' while 'visParams = list(color = 'red')' is for feature (vectors)
  name ='High likelihood of fire pixel', 
  shown = TRUE, 
  opacity = 0.7)



# #........................................................................
# # Export details/image properties ----
# # extract image id (i.e. file path)
# image_full_id <- ee$Image(image)$getInfo()$id 
# # extract id number portion (i.e. filename)
# image_id_number <- basename(image_full_id)
# 
# # Temporal attributes
# # Extract the year using stringr package
# image_year <- str_sub(image_id_number, 1,4)
# # Extract day of year
# image_doy <- str_sub(image_id_number, 5, 7)
# # Extract time of day
# image_time <- str_sub(image_id_number, 8, 13)
# #convert HHMMSS to HH:MM:SS by adding :
# image_time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", image_time)
# 
# imagefolder_path <- file.path(pixel_dir, paste0("fire_pixel_", image_id_number))
# if (!dir.exists(imagefolder_path)) {
#   dir.create(imagefolder_path, recursive = TRUE)
# }




# #........................................................................
# # 1) Convert ee object into spatraster object ----
# 
# # Convert ee into spatraster object
# ee_as_rast(
#   onlyfire,
#   region = area_of_interest,
#   # dsn = file.path(pixel_dir, paste0("fire_pixel_",i,".tif")),
#   dsn = file.path(pixel_dir, paste0("fire_pixel_", image_id_number,".tif")),
#   # via = "drive",
#   container = "google_earth_engine_export/fire_pixel",
#   crs = "EPSG:4326",
#   scale = 200)
# 
# fire_pixel_spat <- rast('C:/Users/achhen/Desktop/goes_data/fire_pixels/fire_pixel_2023233000020400000.tif')
# plot(fire_pixel_spat)
# # REMEMBER: you want remapped values only = 1 because we remapped them and converted them to a scale of 0 and 1 with 1 = high likelihood of fire, see above
# fire_pixel_spat_filtered <- fire_pixel_spat
# fire_pixel_spat_filtered[fire_pixel_spat != 1] <- NA
# plot(fire_pixel_spat_filtered)
# 
# # Extract coordinates
# fire_pixel_df <- as.data.frame(fire_pixel_spat, xy = TRUE)
# 
# # REMEMBER: you want remapped values only = 1 because we remapped them and converted them to a scale of 0 and 1 with 1 = high likelihood of fire, see above
# ggplot(fire_pixel_df[fire_pixel_df$remapped == 1,], 
#        aes(x = x, y = y)) +
#   geom_tile()



#........................................................................
# 2) Convert ee into sf object ----
#........................................................................

# Need to convert image (i.e. pixel) into features (geometric, i.e. points, lines, or polygon) to be able to convert into a sf object

# / Apply a neighborhood reduction to calculate the mean value of neighboring pixels
# // around each pixel within the specified kernel. This operation smooths the image
# // and groups contiguous pixels based on their mean values.


# Convert into polygon representing contiguous areas of high fire likelihood, i.e. Groups contiguous (neighbouring/right beside) pixels with the same value into larger polygons
# this comment most likely is meant for $reduceNeighborhood()

# No reducer then the default is ee$Reducer$countEvery()
# pixel_grouped <- onlyfire$reduceToVectors(scale = 200,  #// 200 m/pixel
#                                           maxPixels = 1e10,
#                                           geometry = area_of_interest,
#                                           crs = 'EPSG:4326')
# Map$addLayer(eeObject = pixel_grouped,
#              visParams = list(color = '#3D2A6A'),
#              name = "Pixels grouped",
#              shown = TRUE,
#              opacity = 0.3)




##...................................................................
# Convert into polygons treating each pixel independently .-. creates points for each individual pixel i.e points of small polygons (more detailed) via countEvery() reducer
# with reducer
pixel_separated <- onlyfire$reduceToVectors(
  reducer = ee$Reducer$countEvery(),  # create points for every pixel that meets the condition (i.e., value = 1)
  scale = 200,
  maxPixels = 1e10,
  geometry = area_of_interest,
  crs = 'EPSG:4326'
)

Map$addLayer(eeObject = pixel_separated,
             visParams = list(color = 'red'),
             name = "Pixels separated",
             shown = TRUE,
             opacity = 0.3)



# message("Checking for fire pixels within aoi")
# # Check if there are any fire pixels located within the area of interest by obtain the actual count value of the pixels that meet the condition (e.g., pixels with a value of 1) within the specified region
# pixel_count <- onlyfire$reduceRegion(
#   reducer = ee$Reducer$count(),
#   geometry = area_of_interest,
#   scale = 200)
# 
# # Identify how many pixels are located within the specified region 
# total_aoi_pixels <- pixel_count$getInfo()
# 
# if (total_aoi_pixels == 0) {
#   message("No fire pixels found within area of interest for image ", basename(ee$Image(image)$getInfo()$id), " Moving onto the next image")
#   # If no fire pixels are found within the area of interest, move on to the next image
#   next # Move to the next image if pixel count is empty
# }



# extract FeatureCollection object information into a list 
pixel_point_info <- pixel_point$getInfo()
# extract the number of pixels within the area of interest
pixel_point_count <- pixel_point_info$features[[1]]$properties$count





# Convert ee object into sf object
pixel_separated_sf <- ee_as_sf(pixel_separated)

ggplot() +
  geom_sf(data = pixel_separated_sf)



# Convert ee object into sf object and save shapefile
ee_as_sf(
  pixel_separated,
  # dsn = file.path(pixel_dir, paste0("fire_pixel_",i,".shp")),
  dsn = file.path(imagefolder_path, paste0("fire_pixel_", image_id_number, ".shp")),
  via = "drive",
  container = "google_earth_engine_export/fire_pixel",
  crs = "EPSG:4326",
  overwrite = TRUE)

# Read in recently exported sf file
shapefile_path <- file.path(imagefolder_path, paste0("fire_pixel_", image_id_number, ".shp"))
pixel_separated_sf <- st_read(shapefile_path)



#___________________________________________________________
# Calculate distance to fire

# Extract coordinates
pixel_coords <- st_coordinates(pixel_separated_sf)

# Create a data frame from the coordinates
fire_data <- data.frame(
  count = rep(pixel_separated_sf$count, each = nrow(pixel_coords) / nrow(pixel_separated_sf)),
  label = rep(pixel_separated_sf$label, each = nrow(pixel_coords) / nrow(pixel_separated_sf)),
  X = pixel_coords[, "X"],
  Y = pixel_coords[, "Y"]
)

# Rename columns to lat/lon
names(fire_data) <- c("count", "label", "longitude", "latitude")


# Extract the year using stringr package
fire_data$year <- as.numeric(str_sub(image_id_number, 1,4))
# Extract day of year
fire_data$doy <- as.numeric(str_sub(image_id_number, 5, 7))
# Convert day of year into date 
fire_data$date <- as.Date(paste(fire_data$year, fire_data$doy, sep = "-"), format = "%Y-%j") #%j = day of year
# Extract time of day
fire_data$time <- str_sub(image_id_number, 8, 13)
#convert HHMMSS to HH:MM:SS by adding :
fire_data$time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", fire_data$time)
# Combine date and time into timestamp
fire_data$timestamp <- as.POSIXct(paste(fire_data$date, fire_data$time), format = "%Y-%m-%d %H:%M:%S")

# Convert df to sf object
fire_data_sf <- st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)

library(sf)
library(lubridate)
library(ctmm)
library(ggplot2)

#.....................................................................
# Collar data ----
#.....................................................................

# import data from 02_outlier_detection.r
dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
dat <- dat[dat$year == '2023',]
dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat$date = as.Date(dat$date, "%Y-%m-%d")

# convert data into sf object
dat_sf <- st_as_sf(dat, coords = c('location.long', 'location.lat'))
# assign crs to collar data
st_crs(dat_sf) <- 4326

#set crs for collar data
dat_sf <- st_transform(dat_sf, st_crs(4326))





library(ctmm)


pixel_dir = 'C:/Users/achhen/Desktop/goes_data/fire_pixel'

# extract image id (i.e. file path)
image_full_id <- ee$Image(image)$getInfo()$id 
# extract id number portion (i.e. filename)
image_id_number <- basename(image_full_id)

# Temporal attributes
# Extract the year using stringr package
image_year <- str_sub(image_id_number, 1,4)
# Extract day of year
image_doy <- str_sub(image_id_number, 5, 7)
# Extract time of day
image_time <- str_sub(image_id_number, 8, 13)
#convert HHMMSS to HH:MM:SS by adding :
image_time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", image_time)


imagefolder_path <- file.path(pixel_dir, paste0("fire_pixel_", image_id_number))
if (!dir.exists(imagefolder_path)) {
  dir.create(imagefolder_path, recursive = TRUE)
}


# Import in shapefile
shapefile_path <- file.path(imagefolder_path, paste0("fire_pixel_", image_id_number, ".shp"))
pixel_separated_sf <- st_read(shapefile_path)

# Check crs
st_crs(pixel_separated_sf)

# Extract coordinates
pixel_coords <- st_coordinates(pixel_separated_sf)

# Create a data frame from the coordinates
fire_data <- data.frame(
  count = rep(pixel_separated_sf$count, each = nrow(pixel_coords) / nrow(pixel_polygon_sf)),
  label = rep(pixel_separated_sf$label, each = nrow(pixel_coords) / nrow(pixel_polygon_sf)),
  X = pixel_coords[, "X"],
  Y = pixel_coords[, "Y"]
)

# Rename columns to lat/lon
names(fire_data) <- c("count", "label", "longitude", "latitude")

# Extract the year using stringr package
fire_data$year <- as.numeric(str_sub(image_id_number, 1,4))
# Extract day of year
fire_data$doy <- as.numeric(str_sub(image_id_number, 5, 7))
# Convert day of year into date 
fire_data$date <- as.Date(paste(fire_data$year, fire_data$doy, sep = "-"), format = "%Y-%j") #%j = day of year
# Extract time of day
fire_data$time <- str_sub(image_id_number, 8, 13)
#convert HHMMSS to HH:MM:SS by adding :
fire_data$time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", fire_data$time)
# Combine date and time into timestamp
fire_data$timestamp <- as.POSIXct(paste(fire_data$date, fire_data$time), format = "%Y-%m-%d %H:%M:%S")




#.....................................................................
# Collar data ----
#.....................................................................


library(sf)
library(lubridate)
library(ctmm)
library(ggplot2)

rm(list = ls())

# import data from 02_outlier_detection.r
dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
dat <- dat[dat$year == '2023',]
dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat$date = as.Date(dat$date, "%Y-%m-%d")

# convert data into sf object
dat_sf <- st_as_sf(dat, coords = c('location.long', 'location.lat'))
# assign crs to collar data
st_crs(dat_sf) <- 4326

#set crs for collar data
dat_sf <- st_transform(dat_sf, st_crs(fire_data))

# Define the time window for collar fix with +/- 1 hour
# time_window <- 3600 #seconds (1 hour) #manual input
time_window <- 1 %#% 'hour' #ctmm package


#...............................................................
# Calculate distance ----
#...............................................................

# initialise columns to store results
dat$fire_event <- "no"
dat$n_fire_points <- NA
dat$dist_to_fire <- NA

dat_sf <- dat_sf[dat_sf$year == 2023,]
dat_sf <- dat_sf[dat_sf$month == 08,]

i <- 1
# i <- dat_sf[dat_sf$date == "2023-08-16",][8,]
# i <- dat_sf[dat_sf$timestamp == "2023-08-18 20:16:08",]


#loop through every collar point
for (i in 1:nrow(dat_sf)) {
  # extract a collar point from data
  collar_point <- dat_sf[i,]
  # extract timestamp of that collar point
  collar_timestamp <- collar_point$timestamp
  # message("Processing GPS collar point ", i, " with timestamp ", collar_timestamp)
  
  # Define time range +/- 1 hour around the collar point timestamp
  start_time <- collar_timestamp - time_window
  end_time <- collar_timestamp + time_window
  
  # extract fire point event within the time window
  fire_point <- fire_data[fire_data$timestamp >= start_time & fire_data$timestamp <= end_time,]
  # message("Number of fire points for ", collar_timestamp, " time window: ", nrow(fire_point))
  dat$n_fire_points[i] <- as.numeric(nrow(fire_point))
  
  # Check if there are any fire points within the time window
  if (nrow(fire_point) == 0) {
    # cat("No data fire points found in window", i, "- moving on to the next collar point.\n")
    # Set results to NA
    dat$dist_to_fire[i] <- NA
    
    next # Move to the next gps collar point if no fire is present
  }
  
  # If fire points exist in the time window, proceed with calculation
  tryCatch({
    if (nrow(fire_point) > 0) {
      dat$fire_event[i] <- "yes"
      # calculate the distance between the collar point and the fire point, output comes out as a matrix
      dist_matrix <- st_distance(collar_point, fire_point)
      #store results, extracting the min. distance between collar point and [multiple] fire point(s)
      dat$dist_to_fire[i] <- min(dist_matrix) # using min value instead of including all distances for all fire points, also causes issues when storing results due to multiple values
      
    } else {
      # If time window is empty, set results to NA
      dat$dist_to_fire[i] <- NA
      
    }
  }, error = function(e) {
    dat$dist_to_fire[i] <- NA
  })
}

str(dat)

# write.csv(dat, "./data/input_data/20240729_dist_to_fire_dat.csv")











#....................................................................
# Calculate distance to fire ----


# dist_matrix <- st_distance(collar_point, fire_point)
dist_matrix <- st_distance(dat_sf, fire_data_sf)
dist_to_fire <- min(dist_matrix)
dist_to_fire







#........................................................................
# 3) Convert polygons into points based on centroid of the each polygon

# Convert the polygons to points via calculating their centroids
centroid_point <- pixel_separated$map(function(feature) {
  centroid = feature$geometry()$centroid()
  return(feature$setGeometry(centroid))
})


Map$addLayer(eeObject = centroid_point,
             visParams = list(color = 'red'),
             name = "Polygon centroid point",
             shown = TRUE,
             opacity = 0.3)





#_____________________________________________________________________________
# Cross check multiple fire data sources ----


# Cross check if  fire data sources are detecting similar results by layering them over each other to see if they fall within the same area or not
modis_data <- st_read("C:/Users/achhen/Desktop/nasa_fire/cathedral/modis_nrt/modis_nrt.shp")
# format time from HHMM into to HH:MM:SS
modis_data$ACQ_TIME <- format(strptime(modis_data$ACQ_TIME, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
modis_data$timestamp <- as.POSIXct(paste(modis_data$ACQ_DATE, modis_data$ACQ_TIME), format = "%Y-%m-%d %H:%M:%S")
#subset data to the same date
modis_points <- modis_data[modis_data$ACQ_DATE == cratercreek$start,]


ggplot() +
  geom_spatraster(data = fire_pixel) +
  geom_point(data = modis_data[modis_data$ACQ_DATE == cratercreek$start,],
             aes(x = LONGITUDE, y = LATITUDE), color = "red")






#_____________________________________________________________________________
# Code to categories ----

# NOTE: these values are arbitrary and for illustrative purposes???????? DOUBLE CHECK IF THESE ARE TOO

# // Remap Mask flags into fire categories. (based on quality/probability of a fire pixel)
fire_categories <- mask$remap(c(0,   10,  11,  12,  13,  14,  15,  30,  31,  32,  33,  34,    #mask flag number
                                35,  40,  50,  60,  100, 120, 121, 123, 124, 125, 126, 127,
                                150, 151, 152, 153, 170, 180, 182, 185, 186, 187, 188, 200,
                                201, 205, 210, 215, 220, 225, 230, 240, 245),          
                              c(7,  1,  2,  3,  4,  5,  6,  1,  2,  3,  4,  5,  6,  7,  7,    # fire category number (ranges 0-11)
                                7,  0,  7,  7,  8,  8,  8,  8,  8,  9,  9,  9,  9,  10, 10,
                                10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11))


# // Create color palette to correspond with fire categories 
maskVisParam <- list(min = 0,
                     max = 11,
                     palette = c('#8BC34A',  # // 0: Good quality fire-free pixel
                                 '#FF5722',  # // 1: Good quality fire pixel
                                 '#FF1493',  # // 2: Saturated fire pixel
                                 '#9C27B0',  # // 3: Cloud contaminated fire pixel
                                 '#FF9800',  # // 4: High probability fire pixel
                                 '#FFC107',  # // 5: Medium probability fire pixel
                                 '#FFEB3B',  # // 6: Low probability fire pixel
                                 '#F5DEB3',  # // 7: Invalid pixel due to sunglint, LZA threshold exceeded, off earth, missing input data, or unprocessed
                                 '#A9A9A9',  # // 8: Invalid pixel due to bad input data
                                 '#2196F3',  # // 9: Invalid pixel due to to surface type (water or desert)
                                 '#C5CAE9',  # // 10: Invalid pixel due to algorithm failure
                                 '#B3E5FC')) # // 11: Invalid pixel due to opaque cloud


# map_categories <- 
Map$addLayer(eeObject = fire_categories, 
             visParams = maskVisParam, 
             name ='Fire Mask based on categories', 
             shown = TRUE, 
             opacity = 0.7)




#_____________________________________________________________________________
# Mask code to confidence values ----

# NOTE: these values are arbitrary and for illustrative purposes!!!

#mask codes (10–15 and 30–35) indicate that the pixel is believed to cover a wildfire, with a varying degree of confidence.
# 10: processed fire pixel
# 11: saturated fire pixel 
# 12: cloud contaminated fire pixel 
# 13: high probability fire pixel 
# 14: medium probability fire pixel 
# 15: low probability fire pixel

# “temporally filtered” code values: 
# 30: temporally filtered process fire pixel
# 31: temporally filtered saturated fire pixel

# 10 or 30	Good quality (highest confidence) fire detections**
#   11 or 31	Saturated fire pixel**


# // Conversion (reclassify) from mask codes to confidence values

fire_mask_codes = c(10, 30, 11, 31, 12, 32, 13, 33, 14, 34, 15, 35)
confidence_values = c(1.0, 1.0, 0.9, 0.9, 0.8, 0.8, 0.5, 0.5, 0.3, 0.3, 0.1, 0.1)
default_confidence_value = 0

code_to_conf = function(image) {
  image$
    clip(area_of_interest)$
    remap(fire_mask_codes, confidence_values, default_confidence_value)
}

fire_conf_values = collection$
  select('Mask')$
  map(code_to_conf)

# Combine all the values in that region of interest into a single new value using $reduce() based on maximum value (i.e. max conf value)
# i.e summarize the time series with a single value: the maximum confidence that was assigned to each pixel over the wildfire time range.
fire_max_conf = fire_conf_values$
  reduce(ee$Reducer$max())

# Check the remapped data 
fire_max_conf$getInfo()



# Define colours (Color scales from yellow to purple as confidence increases from 0 to 1)
affected_area_palette = c('white', 'yellow', 'orange', 'red', 'purple')


Map$addLayer(
  eeObject = collection_max_conf,
  visParams = list(opacity = 0.3,
                   min = 0, max = 1,
                   palette = affected_area_palette),
  name = "GOES-18 Maximum confidence")







#............................
# Fire radiative power (intensity) ----
#............................



# // Create color palette to convey fire radiative power (intensity in megawatts).
powerVisParam <- list(min = 0, 
                      max = 1500, 
                      palette = c('yellow', 'orange', 'red'))

map_power <-
  Map$addLayer(eeObject = power, 
               visParams = powerVisParam, 
               name = 'Fire Radiative Power', 
               shown = TRUE)


