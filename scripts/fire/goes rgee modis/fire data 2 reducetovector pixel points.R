

# GOES data prep


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


start_date <- "2023-08-21"
end_date <- "2023-08-23"


#....................................................................
# GOES sat data ----
#....................................................................


# // Load GOES-18 Image Collection, filter date (image = raster; featurecollection = vector)
goes_18_data = ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate('2023-07-28', '2023-09-30')$
  filterBounds(area_of_interest)

goes_18_data = ee$ImageCollection('NOAA/GOES/18/FDCF')$
  filterDate(start_date, end_date)$
  filterBounds(area_of_interest)

# #test image
# # navigate to extract an image in the collection
# img_list <- goes_18_data$toList(count = goes_18_data$size())
# image <- ee$Image(img_list$get(9))
# ee$Image(image)$getInfo()$properties$`system:index`

# MIGHT NEED TO ADD AND COMBINE GOES-16

# Get a list of all the images for 2023
image_list <- goes_18_data$toList(goes_18_data$size())
# Find out how many images there are in total
total_images <- goes_18_data$size()$getInfo() #52,326 images

# #create a df to store the info
# image_info <- data.frame(image_id_number = rep(NA, total_images),
#                          n_features = rep(NA, total_images),
#                          pixel_point_count = rep(NA, total_images),
#                          file_saved = rep(NA, total_images))

# i <- 33502
# i <- 2289


#...............................................................
# Fire pixel point extraction  ----
#...............................................................

# Task: Obtain coordinates of fires
# Therefore determine locations of fire pixels, convert them into points and extract 

# Set directory to which the files can be saved to
# pixel_dir = 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/2023'
pixel_dir = 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/test'

#initialize vectors to store results
# image_id_number = rep(NA, total_images)
# n_features = rep(0, total_images)
# pixel_point_count = rep(0, total_images)
# file_saved = rep(NA, total_images)


image <- ee$Image('NOAA/GOES/18/FDCF/2023233013020400000')

START <- Sys.time()
tic()

#~33.05456 hours

# Loop through every image in the imagecollection list
for (i in seq_len(total_images)) {
  # Extract image from the image list
  image <- ee$Image(image_list$get(i - 1))
  
  # Extract mask band
  mask <- image$select('Mask')
  # Remap mask codes to values of [0,1], where 1 = high likelihood of fire and 0 = no fire
  mask_remapped <- mask$remap(c(10, 11, 12, 13, 14, 30, 31, 32, 33, 34),
                              c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  # Then filter pixels that signify a high likelihood of fire (i.e. value = 1 only)
  onlyfire <- mask_remapped$eq(1)
  
  
  #...........................
  message(blue("Converting pixels to points"))
  
  # Convert into polygons treating each pixel independently .-. creates points for each individual pixel i.e points of small polygons (more detailed) using a reducer, countEvery(), this produces a featurecollection object so essnetially image -> featurecollection (raster -> vector)
  
#Converts raster pixels to vector (geometry) features, counting the number of pixels that meet the condition, i.e generate a vector layer where each feature represents a cluster of 'fire pixels' and their count
  
  #Reduce to Vectors: Reducing the image to vectors will convert these filtered pixels into vector points. Each point represents the location of a fire pixel.
  
  
  pixel_point <- onlyfire$reduceToVectors(
    reducer = ee$Reducer$countEvery(),  # create points for every pixel that meets the condition (i.e., value = 1)
    scale = 200,
    maxPixels = 1e10,
    geometry = area_of_interest,
    crs = 'EPSG:4326')
  Map$centerObject(eeObject = area_of_interest,
                   zoom = 9)
  Map$addLayer(eeObject = pixel_point,
               visParams = list(color = 'red'),
               name = "Pixels separated",
               shown = TRUE,
               opacity = 0.3)
  
  
  # extract FeatureCollection object information into a list 
  pixel_point_info <- pixel_point$getInfo()
  
  # determine how many features (polygons) are found within the area of interest
  n_features <- length(pixel_point_info$features)
  
  # if there are no features then there are no pixels then set count to 0
  pixel_point_count <- 0
  
  # if there are features (polygons) then sum the pixel point counts for each feature for a total count
  if (n_features > 0) {
    for (i in 1:n_features) {
      pixel_point_count <- pixel_point_count + pixel_point_info$features[[i]]$properties$count
    }
  }
  
  message(cyan("There are", n_features, "geometric feature(s) and", pixel_point_count, "fire pixel(s) found within area of interest. Extracting image info"))
  
  # extract id number (i.e. file name)
  image_id_number <- ee$Image(image)$getInfo()$properties$`system:index`
  # Extract day of year
  image_doy <- str_sub(image_id_number, 5, 7)
  
  # # # store results
  # image_id_number <- image_id_number
  # # image_info$pixel_point_count[i] <- pixel_point_count
  
  # Set folder path
  imagefolder_path <- file.path(pixel_dir, paste0(image_doy))
  # Create day of the year subfolder path where the files are going to be exported to
  if (!dir.exists(imagefolder_path)) {
    dir.create(imagefolder_path, recursive = TRUE)
  }
  message(cyan("for image id number", image_id_number))
  
  # Check if there are any data present in the subset for the current window segment
  if (pixel_point_count == 0) {
    
    # Save results in a CSV file
    # write.csv(image_info, "C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/image_info_partial_20240816.csv", row.names = FALSE)
    
    message(cyan("Image", i, "of", total_images, "info extracted, moving on to the next image"))
    next # Move to the next iteration if no fire pixels are found in the area
  }
  
  

  
  
  #.......................
  message(magenta("Fire pixels found, image info extracted, converting ee object into sf object and saving shapefile"))
  # Convert ee object into sf object and save shapefile
  tryCatch({
    if (pixel_point_count > 0) {
      ee_as_sf(
        pixel_point,
        dsn = file.path(imagefolder_path, paste0(image_id_number, ".shp")),
        via = "drive",
        container = "google_earth_engine_export/fire_pixel",
        crs = "EPSG:4326",
        overwrite = TRUE
      )
    }
    # If able to be saved then record completion
    # file_saved[i] <- "completed"
    
    # Save results in a CSV file
    # write.csv(image_info, "C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel/image_info_partial_20240816.csv", row.names = FALSE)
    
    # message(cyan("Image", i, "of", total_images, "saved, moving on to the next image"))
    
  }, error = function(e) {
    cat("Error exporting shapefile for image", image_id_number, ":", e$message, "\n")
    # If unable to export then record error
    # file_saved[i] <- "error_exporting"
  }
  )
}







# saveRDS(image_info, "C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel/fire_pixel_image_info_20240816.rds")
# save(image_info, file = "C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel/fire_pixel_image_info_20240816.rda")

# write.csv(image_info, "C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/image_info_partial_20240816_final.csv", row.names = FALSE)
# save(image_info, file = "C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/image_info_partial_20240816.rda")

toc()
END <- Sys.time()




# 
# #.................................................for individual image
# # Read in recently exported sf file
# shapefile_path <- file.path(imagefolder_path, paste0("fire_pixel_", image_id_number, ".shp"))
# pixel_separated_sf <- st_read(shapefile_path)
# 
# 
# 
# 





#____________________________________________________________
# testing the difference between making the outline using rgee vs
# making the outline using sf
# do they produce the same results?
# unsmoothed versions


# outline in rgee

area_outline_unsmoothed = ee$Image()$byte()$
  paint(featureCollection = pixel_point,
        width = 2)

m1 <-
Map$addLayer(eeObject = pixel_point,
             visParams = list(color = 'red'),
             name = "point",
             shown = TRUE,
             opacity = 0.3)
m2 <-
Map$addLayer(eeObject = area_outline_unsmoothed,
             visParams = list(palette = 'blue'),
             name = "point",
             shown = TRUE,
             opacity = 0.3)

m1 + m2

# outline another way via rgee
pixel_point_geometry <- ee$Geometry$MultiPolygon(pixel_point)
error_margin <- 10  # Error margin in meters
convex_hull <- pixel_point$geometry()$convexHull(error_margin)


# Have to apply to every feature or it will group them together, so using the map()
feature_geometry = function(feature) {
  error_margin <- 10 
  ee$Feature(feature)$geometry()$convexHull(error_margin)
}


pixel_point_geometry <- ee$FeatureCollection(pixel_point)$
  map(feature_geometry)


m3 <-
  Map$addLayer(eeObject = pixel_point_geometry,
               visParams = list(color = 'blue'),
               name = "point",
               shown = TRUE,
               opacity = 0.3)

m1 + m3


# outline in sf
pixel_point_sf <- st_read('C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/test/233/2023233013020400000.shp')
pixel_point_sf <- st_read("C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_boundary/test/233/2023233013020400000v2.shp")
# create a geometry that represents the min. convex geometry, i.e. draw an outline-ish
area_outline_unsmoothed_sf <- st_convex_hull(pixel_point_sf)


plot(st_geometry(pixel_point_sf), col = 'red', pch = 20, main = "Fire Points and Boundary")
plot(st_geometry(area_outline_unsmoothed_sf), add = TRUE, border = 'blue', lwd = 2)


plot(pixel_point_sf[1], col = 'red', pch = 20, main = "Fire Points and Boundary")
plot(area_outline_unsmoothed_sf[1], add = TRUE, border = 'blue', lwd = 2)














#...........................
library(googledrive)

# tryCatch({
#   if (pixel_point_count > 0) {
#    export_task <- ee$batch$Export$table$toDrive(
#       collection = pixel_point,
#       description = paste0('export_ee_format_of_shp_', image_id_number),
#       fileNamePrefix = paste0('fire_pixel_point_', image_id_number),
#       folder = "google_earth_engine_export/fire_pixel_point",
#       fileFormat = 'shp',
#     )
#    
#    export_task$start()
#    ee_monitoring(export_task)
#    # drive_auth()
#    google_path <- "google_earth_engine_export/fire_pixel_point"
#    local_file_path <- file.path(imagefolder_path, paste0(image_id_number, ".shp"))
#    file_to_download <- drive_ls(path = "google_earth_engine_export/fire_pixel_point",
#                                 pattern = "shp")
#    
#    # Download the file
#    drive_download(file_to_download, path = local_file_path, overwrite = TRUE)
#    
#    file.remove(google_path)
#    cat("Completed transfering file image ID:", image_id_number, "\n")
#   }
# 
#   
# }, error = function(e) {
#   cat("Error transferring shapefile for image", image_id_number, ":", e$message, "\n")
# 
# }
# )




