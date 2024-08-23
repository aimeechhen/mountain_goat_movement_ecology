
# tutorial fire detection

# https://github.com/google/earthengine-community/blob/master/datasets/scripts/GOES-16-17_FireDetection.js

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
image = ee$Image('NOAA/GOES/18/FDCF/2023233224020500000')
# Map$setOptions(mapTypeId = "HYBRID")    # For hybrid view, combines satellite imagery with labels (if supported,)
Map$setOptions(mapTypeId = "SATELLITE") # For satellite view
# Map$setOptions(mapTypeId = "ROADMAP")   # For standard map view
# Map$setOptions(mapTypeId = "TERRAIN")   # For terrain view


# // Select property of image.
DQF <- image$select('DQF')

# // Define United States region and center map.
USgeometry <- ee$Geometry$Rectangle(
  coords = c(-127, 24, -64, 50),
  proj = 'EPSG:4326')

Map$centerObject(eeObject = USgeometry,
                 zoom = 4)

# // Create color palette to correspond with Data Quality Flag categories.
DQFVisParam <- list(min = 0,
                    max = 5,
                    palette = c('#FFFF00',   # // 0:  Good quality fire pixel (yellow)
                                '#8BC34A',   # // 1:  Good quality fire-free pixel (light green)
                                '#B3E5FC',   # // 2:  Invalid pixel due to opaque cloud (light blue)
                                '#2196F3',   # // 3:  Invalid pixel due to to surface type, sunglint, LZA threshold exceeded, off earth, or missing input data (blue)
                                '#B2EBF2',   # // 4:  Invalid pixel due to bad input data (pale sky blue)
                                '#C5CAE9'))  # // 5:  Invalid pixel due to algorithm failure (light lavender)


# // Filter image to show only good quality fire-pixels (DQF = 0).
onlyFire <- DQF$eq(0)

# // Buffer fires to make them stand out.
fireBuffer <- onlyFire$reduceToVectors(
  scale = 2000,
  geometryType = 'centroid',
  labelProperty = 'buffer',
  maxPixels = 1e10)

buffered_fire <- fireBuffer$map(function(feature){
  feature$buffer(50000)})

# // Add Layers to Map.
Map$addLayer(eeObject = DQF, 
             visParams = DQFVisParam, 
             name = 'Fire Data Quality Flags', 
             shown = TRUE, 
             opacity = 0.7)

Map$addLayer(eeObject = fireBuffer, 
             visParams = list(color = 'red'),
             name = 'Buffer',
             shown = TRUE)


