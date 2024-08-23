

# tutorial fire reclassification

# https://github.com/google/earthengine-community/blob/master/datasets/scripts/GOES-16-17_FireReclassification.js

library(rgee)
library(sf)


cratercreek <- data.frame(
  longitude = -120.1,
  latitude = 49.0,
  start = '2023-08-21',
  end = '2023-08-22'
)




# // Load GOES-16 Image Collection, filter date and select first image.
collection <- ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate(cratercreek$start, cratercreek$end)
ee_print(collection)
image <- collection$first()

# Map$setOptions(mapTypeId = "HYBRID")    # For hybrid view, combines satellite imagery with labels (if supported,)
# Map$setOptions(mapTypeId = "SATELLITE") # For satellite view
# Map$setOptions(mapTypeId = "ROADMAP")   # For standard map view
# Map$setOptions(mapTypeId = "TERRAIN")   # For terrain view

# // Select property of image.
mask <- image$select('Mask')
power <- image$select('Power')

# // Define United States region and center map.
USgeometry <- ee$Geometry$Rectangle(
  coords = c(-127, 24, -64, 50),
  proj = 'EPSG:4326')

Map$centerObject(eeObject = USgeometry,
                 zoom = 4)




# // Filter to show only pixels that signify a high likelihood of fire.
allFire <- mask$remap(c(10, 11, 12, 13, 14, 30, 31, 32, 33, 34),
                      c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
# Subset pixels that signify a high likelihood of fire.
onlyFire <- allFire$eq(1)




# // Buffer fires to make them stand out.
fireBuffer <- onlyFire$reduceToVectors(
  scale = 2000,
  geometryType = 'centroid',
  labelProperty = 'buffer',
  maxPixels = 1e10)

buffered_fire <- fireBuffer$map(function(feature){
  feature$buffer(50000)})

# // Remap Mask flags into fire categories.
classifiedImage <- mask$remap(c(0,   10,  11,  12,  13,  14,  15,  30,  31,  32,  33,  34,
                                35,  40,  50,  60,  100, 120, 121, 123, 124, 125, 126, 127,
                                150, 151, 152, 153, 170, 180, 182, 185, 186, 187, 188, 200,
                                201, 205, 210, 215, 220, 225, 230, 240, 245),
                              c(7,  1,  2,  3,  4,  5,  6,  1,  2,  3,  4,  5,  6,  7,  7,
                                7,  0,  7,  7,  8,  8,  8,  8,  8,  9,  9,  9,  9,  10, 10,
                                10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11))

# // Create color palette to correspond with fire categories.
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


# // Create color palette to convey fire radiative power (intensity in megawatts).
powerVisParam <- list(min = 0, 
                      max = 1500, 
                      palette = c('yellow', 'orange', 'red'))

# // Add Layers to Map.
# map1 <-
Map$addLayer(eeObject = classifiedImage, 
             visParams = maskVisParam, 
             name ='Fire Mask', 
             shown = TRUE, 
             opacity = 0.7)
# map2 <-
Map$addLayer(eeObject = power, 
             visParams = powerVisParam, 
             name = 'Fire Radiative Power', 
             shown = TRUE);
# map3 <-
Map$addLayer(eeObject = fireBuffer, 
             visParams = list(color = '#FF00FF'), 
             name = 'Buffer', 
             shown = TRUE)

map1 +
map2 +
map3 +
  map4



# ....................................END OF TUTORIAL..........................
#..............................................................................

#create directories to hold output:
dir.create("./data/fire/test")
dir.create(paste0("./data/input_data/moving_window/", "/Fits_20240719"), recursive = TRUE)

# Set results directory
test_path = "./data/fire/test"

# "./fire/goes/fire_pixel"



# Convert ee image into rast object (fire pixels that signify a high likelihood of fire)
# NOTE: object gets downloaded into Google Drive then moved to Local,
fire_pixel_raster <- ee_as_rast(
  image = onlyFire,
  region = area_of_interest,
  # region = USgeometry,
  # dsn = file.path(test_path),
  # crs = "EPSG:4326"
  via = "drive"
)


only_fire_sf <- ee_as_sf(onlyFire)

test <- rast("./data/fire/test/test.tif")


#....................................

# Convert into vector
fire_vec <- onlyFire$reduceToVectors(
  scale = 2000,
  geometryType = 'centroid',
  maxPixels = 1e10)


map4 <- 
  Map$addLayer(eeObject = fire_vec,
               visParams = list(color = 'red'), 
               name = 'Fire points', 
               shown = TRUE)



# Extract coordinates
fire_points = fire_vec$map(function(feature) {
  coords <- feature$geometry()$coordinates()
  feature$set('longitude', coords$get(0))$set('latitude', coords$get(1))
})








# # // Export the fire points as a CSV file to Google Drive to local
# 
# fire_export <- ee_table_to_drive(
#   collection = fire_points,
#   folder = google_earth_engine_export,
#   description = 'Fire_Points_Export',
#   fileFormat = 'CSV'
# )
# 
# fire_export$start()
# ee_monitoring(fire_export)
# 
# # Move results from Drive to local
# csv <- ee_drive_to_local(task = fire_export)
# 
# 
# 
# 
# 
# # Convert into sf object
# fire_points_sf <- ee_as_sf(fire_points)
# 
# # Convert into dataframe
# fire_points_df <- as.data.frame(fire_points_sf)
# 
# 
# ggplot() +
#   geom_sf(data = fire_points_sf, fill = NA, color = "red", size = 1)
