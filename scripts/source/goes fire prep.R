

library(sf)
library(stringr)


# source('./scripts/goes fire prep.r')

#...............................................................
# goes ----
#...............................................................


# Set folder path where the files are located
# folder_path <- 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/2023'
folder_path <- 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_boundary/2023b'
# folder_path <- 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_boundary_outline/2023/'
# Load .shp, .tif etc files within a folder including all the subfolders
shp_files <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
# Import/read all the files into a list
shp_list <- lapply(shp_files, st_read)
# Extract file names and assign names to the list
names(shp_list) <- gsub("\\.shp$", "", basename(shp_files))
# Combine shapefiles
goes <- do.call(rbind, shp_list)

# Extract rownames and put them into a new column
goes$image_id_number <- rownames(goes)
# # count = number of pixels within each polygon?
# label = identifier
# Extract the year using stringr package
goes$year <- as.numeric(str_sub(goes$image_id_number, 1,4))
# Extract day of year
goes$doy <- as.numeric(str_sub(goes$image_id_number, 5, 7))
# Convert day of year into date
goes$date <- as.Date(paste(goes$year, goes$doy, sep = "-"), format = "%Y-%j") #%j = day of year
# Extract time of day
goes$time <- str_sub(goes$image_id_number, 8, 13)
#convert HHMMSS to HH:MM:SS by adding :
goes$time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", goes$time)
# Combine date and time into timestamp
goes$timestamp <- as.POSIXct(paste(goes$date, goes$time), format = "%Y-%m-%d %H:%M:%S")



# clean up environment so these objects arent added when being source() in
rm(folder_path, shp_files, shp_list)




# 
# #____________________________________________________________________
# # for animation, code not used
# 
# 
# # Extract coordinates
# # # Convert row names to a vector for later use
# # row_names <- rownames(goes)
# # # Due to the nature of the geometry type being a polygon, there will multiple points that make up this polygon, therefore multiple coordinates
# # Initialize an empty list to store the results
# coords_list <- vector("list", nrow(goes))
# 
# # Iterate over each feature in the Simple Feature collection
# for (i in seq_len(nrow(goes))) {
#   # Extract the geometry for the current feature
#   polygon_geometry <- goes$geometry[i]
#   
#   # Extract the coordinates from the geometry
#   coords <- st_coordinates(polygon_geometry)
#   
#   # Extract the image filename, label, and count
#   image_id_number <- rownames(goes)[i]
#   count <- goes$count[i]
#   label <- goes$label[i]
#   
#   # Check if `coords` has more than one row and handle it accordingly
#   if (nrow(coords) > 0) {
#     # Combine into a data frame
#     coords_df <- data.frame(
#       image_id_number = image_id_number,
#       count = count,
#       label = label,
#       longitude = coords[, 1],
#       latitude = coords[, 2]
#     )
#     
#     # Append to the list
#     coords_list[[i]] <- coords_df
#   } else {
#     # If no coordinates, create an empty dataframe and add to the list
#     coords_list[[i]] <- data.frame(
#       image_id_number = image_id_number,
#       count = count,
#       label = label,
#       longitude = numeric(0),
#       latitude = numeric(0)
#     )
#   }
# }
# 
# # Combine all data frames in the list into a single data frame
# fire_data <- do.call(rbind, coords_list)
# # Extract the year using stringr package
# fire_data$year <- as.numeric(str_sub(fire_data$image_id_number, 1,4))
# # Extract day of year
# fire_data$doy <- as.numeric(str_sub(fire_data$image_id_number, 5, 7))
# # Convert day of year into date
# fire_data$date <- as.Date(paste(fire_data$year, fire_data$doy, sep = "-"), format = "%Y-%j") #%j = day of year
# # Extract time of day
# fire_data$time <- str_sub(image_id_number, 8, 13)
# #convert HHMMSS to HH:MM:SS by adding :
# fire_data$time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", fire_data$time)
# # Combine date and time into timestamp
# fire_data$timestamp <- as.POSIXct(paste(fire_data$date, fire_data$time), format = "%Y-%m-%d %H:%M:%S")
# 
# # Convert df to sf object
# fire_data_sf <- st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)
# st_crs(fire_data_sf)