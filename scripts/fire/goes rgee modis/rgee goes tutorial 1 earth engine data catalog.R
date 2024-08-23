


# tutorial earth engine data catalog
# https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_18_FDCF#description

library(rgee)
library(sf)

# / Time and location of the fire.
# Define parameters
cratercreek <- data.frame(
  longitude = -120.1,
  latitude = 49.0,
  start = '2023-08-21',
  end = '2023-08-25'
)

# // Region of interest.
radius_of_interest_meters <- 40000
area_of_interest = ee$Geometry$Point(c(cratercreek$longitude, cratercreek$latitude))$
  buffer(radius_of_interest_meters)
# convert GEE object to sf object
area_of_interest_sf <- ee_as_sf(area_of_interest)

Map$setCenter(lon = cratercreek$longitude,
              lat = cratercreek$latitude,
              zoom = 3)

#____________________________________________________________________________

collection <- ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate(cratercreek$start, cratercreek$end)$
  filterBounds(area_of_interest)
image <- collection$first()

# // NOAA GOES-18 full disk fire product for a single time slice.

# // TODO(schwehr): Find an asset with some fires.
image = ee$Image('NOAA/GOES/18/FDCF/2022341230020300000')
area = image$select('Area')
temp = image$select('Temp')
dqf = image$select('DQF')

# // On station as GOES-W
xmin <- -205
xmax <- xmin + 135
ymin <- -65
ymax <- 65

Map$setCenter(lon = (xmin + xmax) / 2, 
              lat = (ymin + ymax) / 2, 
              zoom = 3)
geometry = ee$Geometry$Rectangle(coords = c(xmin, ymin, xmax, ymax), 
                                 proj = 'EPSG:4326')

DQFVis <- list(
  min = 0,
  max =  5,
  palette = c('blanchedalmond',  #// Good quality fire pixel
              'darkgreen',       #// Good quality fire free land
              'darkcyan',        #// Opaque cloud, Bad surface type, sunglint, LZA threshold exceeded,
              'darkslateblue',   #// off Earth, or missing input data
              'lemonchiffon',    #// Bad input data
              'burlywood'))      #// Algorithm failure

Map$centerObject(area_of_interest, zoom = 8)

Map$addLayer(eeObject = dqf, 
             visParams = DQFVis, 
             name = 'DQF')




# // Fires are small enough that they are difficult to see at the scale of
# // an entire GOES image.  Buffer fires based on area to make them stand out.
area_vec <- area$reduceToVectors(
  # geometry = geometry,
  geometry = area_of_interest,
  # scale = 2000,
  scale = 200,
  geometryType = 'centroid',
  labelProperty = 'area',
  maxPixels = 1e10)

buffered_area <- area_vec$map(function(feature) {
  buffer_distance <- ee$Number(feature$get('area'))$add(1)$pow(1.5)
  feature$buffer(buffer_distance)
})


Map$addLayer(eeObject = area_vec, 
             visParams = list(color = 'orange'),
             name = 'Area')




# // Buffer fires based on temperature to make them stand out.
temp_vec <- temp$reduceToVectors(
  geometry = geometry,
  # geometry = area_of_interest,
  scale = 2000,
  # scale = 200,
  # geometryType = 'centroid',
  labelProperty = 'temp',
  maxPixels = 1e10)


buffered_temp <- temp_vec$map(function(feature) {
  buffer_distance <- ee$Number(feature$get('temp'))$add(1)$pow(1.5)
  feature$buffer(buffer_distance)
})


Map$addLayer(eeObject = temp_vec, 
             visParams = list(color = 'red'),
             name = 'temp')




