

library(rgee)
library(sf)
library(ggplot2)
# library(jsonlite)

rgee::ee_Initialize(user = 'katchhen@gmail.com', drive = TRUE, gcs = FALSE)

# Check that everything is in order to be able to use rgee i.e. users, credentials, path 
rgee::ee_users()
rgee::ee_get_earthengine_path()
rgee::ee_check()
rgee::ee_check_python()
rgee::ee_check_credentials()
rgee::ee_check_python_packages()


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
cathedral_gee$getInfo()


# Okay now we can do some rgee-ing

# / Time and location of the fire.
# Define parameters
cratercreek <- data.frame(
  longitude = -120.1,
  latitude = 49.0,
  start = '2023-08-21',
  end = '2023-08-25'
)



# Let's set our area of interest
# // Region of interest.
radius_of_interest_meters <- 40000

# # as sf object
# area_of_interest <- st_buffer(st_sfc(st_point(c(crater_creek$longitude, crater_creek$latitude)), crs = 4326), 
#                               dist = radius_of_interest_meters)
# # Convert to sf to GEE geometry
# area_of_interest_gee <- sf_as_ee(area_of_interest)


# as GEE object
area_of_interest = ee$Geometry$Point(c(cratercreek$longitude, cratercreek$latitude))$
  buffer(radius_of_interest_meters)
# convert GEE object to sf object
area_of_interest_sf <- ee_as_sf(area_of_interest)



# visually check with coordinates
ggplot() +
  geom_sf(data = area_of_interest_sf) +
  geom_sf(data = cathedral) +
  coord_sf() +
  theme_minimal()

# Create a ee map
Map$centerObject(area_of_interest, zoom = 9)
m0 <-
  Map$addLayer(
    eeObject = cathedral_gee,
    visParams = list(color = 'grey'),
    name = 'm0 Cathedral Park',
    shown = TRUE,
    opacity = 0.2)

m1 <-
  Map$addLayer(
    eeObject = area_of_interest,
    visParams = list(color = 'green'),
    name = "m1 Area of Interest",
    shown = TRUE,
    opacity = 0.2)

m0+m1
# Okay so now we know where we are on the planet, lets get some satellite data based on the parameters we set earlier
# GEE terminology
# Image (raster) 
# FeatureCollection (vector)

#// Satellite data.
goes_18_data = ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate(cratercreek$start, cratercreek$end)$
  filterBounds(area_of_interest)
goes_16_data = ee$ImageCollection('NOAA/GOES/16/FDCF')$
  filterDate(cratercreek$start, cratercreek$end)$
  filterBounds(area_of_interest)



# Check extracted the data
# goes_18_data$getInfo()
# If there are a lot of data, test a subset of it
# sample_images <- goes_18_data$limit(5)$getInfo()
# sample_images2 <- goes_16_data$limit(5)$getInfo()

# // Conversion from mask codes to confidence values.
# NOTE: these values are arbitrary and for illustrative purposes
fire_mask_codes = c(10, 30, 11, 31, 12, 32, 13, 33, 14, 34, 15, 35)
confidence_values = c(1.0, 1.0, 0.9, 0.9, 0.8, 0.8, 0.5, 0.5, 0.3, 0.3, 0.1, 0.1)
default_confidence_value = 0

#map_from_mask_codes_to_confidence_values
remap_mask_to_conf = function(image) {
  image$
    clip(area_of_interest)$
    remap(fire_mask_codes, confidence_values, default_confidence_value)
}


#Summarize the time series with a single value: the maximum confidence that was assigned to each pixel over the wildfire time range via reduce operation via max reducer operator
# NOTE: that other operators would give different insights into the temporal evolution of the wildfire

# Combine all the values in that region of interest into a single new value using $reduce() based on maximum value (i.e. max conf value)
# i.e summarize the time series with a single value: the maximum confidence that was assigned to each pixel over the wildfire time range.
goes_18_confidence = goes_18_data$
  select('Mask')$
  map(remap_mask_to_conf)
goes_18_max_confidence = goes_18_confidence$
  reduce(ee$Reducer$max()) # this line is only needed if values vary but if values are only 0,1 then it isnt needed

goes_16_confidence = goes_16_data$
  select('Mask')$
  map(remap_mask_to_conf)
goes_16_max_confidence = goes_16_confidence$
  reduce(ee$Reducer$max())

# Check the remapped data 
goes_18_max_confidence$getInfo()
goes_16_max_confidence$getInfo()





# visualize that initial data processing step

# Define colours (Color scales from yellow to purple as confidence increases from 0 to 1)
affected_area_palette = c('white', 'yellow', 'orange', 'red', 'purple')

m2 <-
  Map$addLayer(
    eeObject = goes_18_max_confidence,
    visParams = list(opacity = 0.3,
                     min = 0, max = 1,
                     palette = affected_area_palette),
    name = "m2 GOES-18 Maximum confidence")
m2_16 <-
  Map$addLayer(
    eeObject = goes_16_max_confidence,
    visParams = list(opacity = 0.3,
                     min = 0, max = 1,
                     palette = affected_area_palette),
    name = "m2_16 GOES-16 Maximum confidence")




# Using GOES WEST AND GOES EAST would increases the spatial resolution of the combined data
# Therefore reduce the two numbers per pixel to a single one. In this post we keep the minimum value — the confidence of a wildfire being in a fragmented pixel is the lowest value measured from GOES
# NOTE other operators would give different insights

combined_confidence = ee$ImageCollection(c(goes_18_max_confidence,
                                           goes_16_max_confidence))$
  reduce(ee$Reducer$min())

m3 <- 
  Map$addLayer(eeObject = combined_confidence,
               visParams = list(opacity = 0.3,
                                min = 0, max = 1,
                                palette = affected_area_palette),
               name = 'm3 Combined max confidence')



#...............................................................
# Smoothing pixels and outline ----
#...............................................................



# Smoothing the satellite pixels

#smooth both the polygonal corners and the step-like variations of confidence in a single operation, ee.Image.reduceNeighborhood
#It requires choosing a shape, known as a kernel, which will define a region of interest around each pixel, and a reducer operator, which will combine all the values in that region of interest into a single new value.
# Earth Engine offers a range of built-in kernels (such as ee.Kernel.circle) and reducers (such as ee.Reducer.mean). 
#NOTE: Each kernel shape and size and each reducer will produce a different smoothed image


kernel = ee$Kernel$square(
  radius = 2000, units = 'meters') # normalized = TRUE was dropped due to error Unrecognized arguments {'normalized'} to function: Kernel.square


smoothed_confidence = goes_18_max_confidence$reduceNeighborhood(reducer = ee$Reducer$mean(),
                                                                # reducer = ee$Reducer$stdDev(),
                                                                kernel = kernel,
                                                                optimization = 'boxcar')

# Check the data
smoothed_confidence$getInfo()

# confidence of the wildfire-affected area (from yellow to purple) after smoothing
m4 <-
  Map$addLayer(eeObject = smoothed_confidence,
               visParams = list(opacity = 0.3,
                                min = 0, max = 1,
                                palette = affected_area_palette),
               name = "m4 Smoothed confidence")


#...............................................................
# Converting to an outline
#smoothing operation above, the processed data is still an image of confidence values. A straightforward approach is to use the ee.Image.gt operation to keep the parts of the image having confidence above a chosen threshold value.

#confidence of the wildfire-affected area after thresholding the smoothed values
high_confidence = smoothed_confidence$
  gt(0.6) # this converts it into a binary integer
m5 <- 
  Map$addLayer(eeObject = high_confidence,
               visParams = list(opacity = 0.3,
                                min = 0, max = 1,
                                palette = affected_area_palette),
               name = "m5 High confidence")




#next step is to convert the resulting binary image into an outline by generating a multi-polygon in case there are multiple regions in the image above the threshold via ee.Image.reduceToVectors operation
#  reduceToVectors operation typically generates outlines with staircase-like edges

affected_areas = high_confidence$reduceToVectors(scale = 200,  #// 200 m/pixel
                                                 maxPixels = 1e10,
                                                 geometry = area_of_interest,
                                                 crs = 'EPSG:4326')$
  filter(ee$Filter$eq('label', 1))

affected_areas_outline = ee$Image()$byte()$
  paint(featureCollection = affected_areas,
        width = 2)

# outline of the wildfire-affected area around the thresholded confidence values.
m6 <-
  Map$addLayer(eeObject = affected_areas_outline,
               visParams = list(palette = 'red'),
               name = "m6 Affected Areas Outline",
               shown = TRUE,
               opacity = 0.3)




# Smooth the edges
# One way to smooth such edges is to use Earth Engine ee.Feature.simplify operation, specifying an acceptable error margin — the greater the error margin, the simpler the shape


smooth = function(feature) {
  max_error_meters = 500
  ee$Feature(feature)$simplify(max_error_meters)
}


affected_areas_smoothed = ee$FeatureCollection(affected_areas)$
  map(smooth)


affected_areas_smoothed_outline = ee$Image()$byte()$
  paint(featureCollection= affected_areas_smoothed,
        width = 2)

# Final outline of the wildfire-affected area smoothed out
m7 <-
  Map$addLayer(affected_areas_smoothed_outline,
               visParams = list(palette = 'purple'),
               name = 'm7 Smoothed affected areas', 
               shown = TRUE, 
               opacity = 0.3)



# combine all image layers onto one map plot
m0 +
  m1 +
  m2 +
  # m2_16 +
  # m3 +
  m4 +
  m5 +
  m6 +
  m7


































#________________________________________________________________________
# TUTORIAL ANIMATE IT! ----

#https://github.com/r-spatial/rgee?tab=readme-ov-file

# define RGB visualization parameters first.
visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

# Create RGB visualization images for use as animation frames.
rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>%
    ee$Image$clip(mask)
})


# animate this. Define GIF visualization parameters.
gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)

# Get month names
dates_modis_mabbr <- distinctDOY %>%
  ee_get_date_ic %>% # Get Image Collection dates
  '[['("time_start") %>% # Select time_start column
  lubridate::month() %>% # Get the month component of the datetime
  '['(month.abb, .) # subset around month abbreviations


# use ee_utils_gif_* functions to render the GIF animation and add some texts.
animation <- ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")
animation %>%
  ee_utils_gif_annotate(
    text = "NDVI: MODIS/006/MOD13A2",
    size = 15, color = "white",
    location = "+10+10"
  ) %>%
  ee_utils_gif_annotate(
    text = dates_modis_mabbr,
    size = 30,
    location = "+290+350",
    color = "white",
    font = "arial",
    boxcolor = "#000000"
  ) # -> animation_wtxt

# ee_utils_gif_save(animation_wtxt, path = "raster_as_ee.gif")















