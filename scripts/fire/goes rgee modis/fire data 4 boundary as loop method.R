

# Map and extract the fire boundary


library(rgee)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(stringr)

library(tictoc)
library(crayon)

rgee::ee_Initialize(user = 'katchhen@gmail.com', drive = TRUE, gcs = FALSE)


rm(list = ls())




# BASED ON THE CODE HERE: https://medium.com/google-earth/how-to-generate-wildfire-boundary-maps-with-earth-engine-b38eadc97a38

#....................................................................
# Area of interest (aoi) ----
#....................................................................

# center point (centroid) of the collar data
longitude = -120.2528
latitude = 48.95914
# Define the radius around the center point
radius_of_interest_meters <- 50000
# Create a geometry for the area of interest as GEE object, defining the area of interest based on the center of the collar data and radius around it
area_of_interest = ee$Geometry$Point(c(longitude, latitude))$
  buffer(radius_of_interest_meters)

start_date <- "2023-08-19"
end_date <- "2023-09-30"



Map$centerObject(area_of_interest, zoom = 10)



#....................................................................
# GOES sat data ----
#....................................................................


# // Load GOES-18 Image Collection, filter date (image = raster; featurecollection = vector)
goes_18_data = ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate(start_date, end_date)$
  filterBounds(area_of_interest)

# #test image
# # navigate to extract an image in the collection
# img_list <- goes_18_data$toList(count = goes_18_data$size())
# image <- ee$Image(img_list$get(9))
# ee$Image(image)$getInfo()$properties$`system:index`
# image <- ee$Image('NOAA/GOES/18/FDCF/2023233013020400000')

# Get a list of all the images for 2023
image_list <- goes_18_data$toList(goes_18_data$size())
# Find out how many images there are in total
total_images <- goes_18_data$size()$getInfo() #13081 images





#...............................................................
# Fire boundary extraction  ----
#...............................................................

# Task: Obtain coordinates of fire boundaries
# Therefore determine locations of fire pixels, convert them into boundaries and extract 

2023 232 2140 20400000
# Set directory to which the files can be saved to
pixel_dir = 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_boundary/2023'
# pixel_dir = 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_boundary/test'
pixel_dir_outline = 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_boundary_outline/2023'

START <- Sys.time()
tic()

# Loop through every image in the imagecollection list
for (i in seq_len(total_images)) {
  # Extract image from the image list
  image <- ee$Image(image_list$get(i - 1))
  
  # image_clip <- image$clip(area_of_interest)
  # Extract mask band
  mask <- image$select('Mask')
  # Recode the mask to values {0,1} based on fire pixel probability
  mask_remapped <- mask$remap(c(10, 11, 12, 13, 14, 30, 31, 32, 33, 34), # mask codes
                              c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)) # confidence value
  onlyfire <- mask_remapped$eq(1) # only wanting values of 1
  
  
  #...............................................................
  # 1) Smoothing pixels ----
  #...............................................................
  
  
  kernel = ee$Kernel$square(
    radius = 2000, units = 'meters') # normalized = TRUE was dropped due to error Unrecognized arguments {'normalized'} to function: Kernel.square
  
  #Calculates the mean of pixel values in a neighborhood to smoothed pixel values (image object)
  smoothed_pixel <- onlyfire$reduceNeighborhood(reducer = ee$Reducer$mean(),
                                                # reducer = ee$Reducer$stdDev(),
                                                kernel = kernel,
                                                optimization = 'boxcar')$
    toInt() # convert values to integer (i.e for whole numbers)

  #...............................................................
  # 2) Into unsmoothed vectors (feature) ----

  # #Converts raster pixels to vector (geometry) features, counting the number of pixels that meet the condition, i.e generate a vector layer where each feature represents a cluster of 'fire pixels' and their count
  # image to feature object
  vector_area_unsmoothed <- smoothed_pixel$reduceToVectors(scale = 200,  #// 200 m/pixel
                                              maxPixels = 1e6,
                                              geometry = area_of_interest,
                                              crs = 'EPSG:4326')
  
  
  #...............................................................
  # 3) Smoothing vector area ----

  # NOTE: USING SMOOTHED PIXELS THEN SMOOTHING THE VECTORS...double smoothing occurring....????
  
    area_smoother = function(feature) {
    max_error_meters = 500 # defines the maximum allowable deviation between the simplified and original geometries.
    ee$Feature(feature)$simplify(max_error_meters) # reduce the number of vertices in the shape
  }
  
  # smooth over the vector area
  vector_area_smoothed = ee$FeatureCollection(vector_area_unsmoothed)$
    map(area_smoother)
  
    #...............................................................
  # 4) vector area outline ----
    
    # draw the fire outline, this also converts the object back into an image object
   area_outline_unsmoothed = ee$Image()$byte()$
      paint(featureCollection = vector_area_unsmoothed,
            width = 2)

    
  #...............................................................
  # 5) smoothed vector area outline ----
  #...............................................................
    
    
    # draw smoothed outline
    area_outline_smoothed <- ee$Image()$byte()$
      paint(featureCollection= vector_area_smoothed,
            width = 2)
    
  #...............................................................
  # 6) smoothed vector area outline to vector ----
  #...............................................................
  
  area_outline_smoothed_vector <- area_outline_smoothed$reduceToVectors(scale = 200,  #// 200 m/pixel
                                                           maxPixels = 1e6,
                                                           geometry = area_of_interest,
                                                           crs = 'EPSG:4326')
  
  
  
  
  #...............................................................
  # Extract and save ----
  #...............................................................
  


  # extract FeatureCollection object information into a list 
  vector_area_info <- vector_area_unsmoothed$getInfo()
  # determine how many features (polygons) are found within the area of interest i.e. fire areas for each image 
  n_feature <- length(vector_area_info$features)
  
  # if there are no features then there are no pixels then set count to 0
  pixel_point_count <- 0
  
  # if there are features (polygons) then sum the pixel point counts for each feature for a total count
  if (n_feature > 0) {
    for (i in 1:n_feature) {
      pixel_point_count <- pixel_point_count + vector_area_info$features[[i]]$properties$count
    }
  }
  
  # extract id number (i.e. file name)
  image_id_number <- ee$Image(image)$getInfo()$properties$`system:index`
  message(cyan("Image id number:", image_id_number, "There are", n_feature, "geometric feature(s) and", pixel_point_count, "fire pixel(s) found within area of interest"))
  
  
  # Extract day of year
  image_doy <- str_sub(image_id_number, 5, 7)
  
  # Set folder path
  imagefolder_path <- file.path(pixel_dir, paste0(image_doy))
  imagefolder_path_outline <- file.path(pixel_dir_outline, paste0(image_doy))
  # Create day of the year subfolder path where the files are going to be exported to
  if (!dir.exists(imagefolder_path)) {
    dir.create(imagefolder_path, recursive = TRUE)
  }
    if (!dir.exists(imagefolder_path_outline)) {
    dir.create(imagefolder_path_outline, recursive = TRUE)
  }
    
    # Check if there are any data present in the subset for the current window segment
    if (pixel_point_count == 0) {
      message(cyan("Image id number:", image_id_number, "No fire pixels detected in image", i, "of", total_images))  
      message(green("Image id number:", image_id_number, "complete, moving on to the next image"))
      next # Move to the next image if no fire pixels are found in the area
    }
    
  # if data is present, then convert and save
  # message(cyan("Image id number:", image_id_number, "Fire pixels found in image", i, "of", total_images, "Converting ee into rast object and saving file."))
    # saving as raster because its an image with the areas outlined
    # tryCatch({
    #   if (pixel_point_count > 0) {
    #     # affected_areas_sf <- 
    #     ee_as_rast(area_outline_smoothed,
    #                scale = 200,
    #                region = area_of_interest,
    #                dsn = file.path(imagefolder_path, paste0(image_id_number, ".tif")),
    #                via = "drive",
    #                container = "google_earth_engine_export/fire_pixel_boundary",
    #                crs = "EPSG:4326")
    #   }
      
  message(cyan("Image id number:", image_id_number, "Fire pixels found in image", i, "of", total_images, "Converting ee into sf object and saving file."))
     
   tryCatch({
        if (pixel_point_count > 0) {

          # only create file if data exists, Create day of the year subfolder path where the files are going to be exported to
          if (!dir.exists(imagefolder_path)) {
            dir.create(imagefolder_path, recursive = TRUE)
          }
          if (!dir.exists(imagefolder_path_outline)) {
            dir.create(imagefolder_path_outline, recursive = TRUE)
          }
          
          ee_as_sf(
            vector_area_smoothed,
            dsn = file.path(imagefolder_path, paste0(image_id_number, ".shp")),
            via = "drive",
            container = "google_earth_engine_export/fire_pixel_boundary",
            crs = "EPSG:4326",
            overwrite = TRUE
          )
          message(cyan("vector area saved"))
          
          ee_as_sf(
            area_outline_smoothed_vector,
            dsn = file.path(imagefolder_path_outline, paste0(image_id_number, ".shp")),
            via = "drive",
            container = "google_earth_engine_export/fire_pixel_boundary_outline",
            crs = "EPSG:4326",
            overwrite = TRUE
          )
          
          message(cyan("vector area outline saved"))
        }
      
        

       
      message(green("Image id number:", image_id_number, "saved, moving on to the next image"))
      
    }, error = function(e) {
      cat("Error exporting shapefile for image", image_id_number, ":", e$message, "\n")
      # If unable to export then record error
      # image_info$file_saved[i] <- "error_exporting"
    }
    )
    
}


END <- Sys.time()
toc()



#...............................................................
#...............................................................

# NOTE
# I did not see much of a difference between pixel_point and the smoothed pixels (affected area outline). They both were rough and jagged
# however the smoothed outline there was a difference, the outline was smoothed out

pixel_point <- onlyfire$reduceToVectors(
  reducer = ee$Reducer$countEvery(),  # create points for every pixel that meets the condition (i.e., value = 1)
  scale = 200,
  maxPixels = 1e10,
  geometry = area_of_interest,
  crs = 'EPSG:4326')

map_point <- 
  Map$addLayer(eeObject = pixel_point,
               visParams = list(palette = 'red'),
               name = "point",
               shown = TRUE,
               opacity = 0.3)


# map_affected_areas_outline <-
#   Map$addLayer(eeObject = affected_areas_outline,
#                visParams = list(palette = 'red'),
#                name = "Affected Areas Outline",
#                shown = TRUE,
#                opacity = 0.3)


# Final outline of the wildfire-affected area smoothed out
# map_affected_areas_smoothed_outline <-
#   Map$addLayer(affected_areas_smoothed_outline,
#                visParams = list(palette = 'purple'),
#                name = 'm7 Smoothed affected areas', 
#                shown = TRUE, 
#                opacity = 0.3)

map_point +
  map_affected_areas_outline +
  map_affected_areas_smoothed_outline



#____________________________explanation as per AI 
# Option 1: Skip smoothing of pixels i.e. $reduceNeighborhood

# 1 Remap the Mask Values: By remapping the mask values to 0 and 1, you're isolating the pixels that represent fire.
# 2 Filter with $eq(1): This keeps only the pixels that represent fire.
# 3 Reduce to Vectors: By reducing the pixels to vectors (reduceToVectors), you can directly generate polygons that represent the fire boundaries.
# 4  Smooth the Vectors: You can then smooth the resulting vectors if needed to reduce the complexity of the boundary.
# This approach works well if your primary concern is getting a clean boundary for the fire, without worrying too much about noise or small artifacts.




# Option 2: Apply smoothing to pixels  i.e. $reduceNeighborhood
# 
# If you want to reduce noise and small artifacts in the fire boundary:
#   
# 1 Smoothing via $reduceNeighborhood: This would smooth the pixel values, reducing small, isolated pixels that might be noise.
# 2 Reduce to Vectors: After smoothing, reducing to vectors will give you a cleaner boundary with potentially fewer small, irrelevant shapes.
# 3 Smooth the Vectors: You can still smooth the vectors to reduce boundary complexity.
# This method might be preferable if the fire boundaries have a lot of noise or irregularities that you want to smooth out before vectorization.

# Conclusion
# If your data is already clean or you're confident that noise won't significantly affect your results, you can skip the pixel smoothing step and go directly to vectorization. If the data is noisy, smoothing first could give you a cleaner boundary.