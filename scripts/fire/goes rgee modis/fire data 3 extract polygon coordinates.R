

library(sf)
library(stringr)
library(lubridate)
library(ctmm)
library(ggplot2)
# library(dplyr)

rm(list = ls())



# Set folder path where the files are located
folder_path <- 'C:/Users/achhen/Desktop/NOAA/GOES/18/FDCF/fire_pixel_point/2023'

# Load .shp, .tif etc files within a folder including all the subfolders
shp_files <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

# Import/read all the files into a list
shp_list <- lapply(shp_files, st_read)

# Extract file names and assign names to the list
names(shp_list) <- gsub("\\.shp$", "", basename(shp_files))

# Combine shapefiles
shp_dat <- do.call(rbind, shp_list)

# Extract rownames and put them into a new column
shp_dat$image_id_number <- rownames(shp_dat)
# # count = number of pixels within each polygon?
# label = identifier


# Extract the year using stringr package
shp_dat$year <- as.numeric(str_sub(shp_dat$image_id_number, 1,4))
# Extract day of year
shp_dat$doy <- as.numeric(str_sub(shp_dat$image_id_number, 5, 7))
# Convert day of year into date
shp_dat$date <- as.Date(paste(shp_dat$year, shp_dat$doy, sep = "-"), format = "%Y-%j") #%j = day of year
# Extract time of day
shp_dat$time <- str_sub(shp_dat$image_id_number, 8, 13)
#convert HHMMSS to HH:MM:SS by adding :
shp_dat$time <- sub("(.{2})(.{2})(.{2})", "\\1:\\2:\\3", shp_dat$time)
# Combine date and time into timestamp
shp_dat$timestamp <- as.POSIXct(paste(shp_dat$date, shp_dat$time), format = "%Y-%m-%d %H:%M:%S")




#....................................................................

# # Calculate the centroid of each geometry and extract the coordinates
# coords <- st_coordinates(st_centroid(shp_dat))
# # Warning message: st_centroid assumes attributes are constant over geometries 
# 
# # Add centroid coordinates of the polygon to the df
# shp_dat$lon <- coords[, "X"]
# shp_dat$lat <- coords[, "Y"]

#....................................................................



#....................................................................
#....................................................................
#....................................................................

# CODE BELOW WORKS -> however, not necessary

# 
# library(sf)
# 
# # # Convert row names to a vector for later use
# # row_names <- rownames(shp_dat)
# # # Due to the nature of the geometry type being a polygon, there will multiple points that make up this polygon, therefore multiple coordinates
# # Initialize an empty list to store the results
# coords_list <- vector("list", nrow(shp_dat))
# 
# # Iterate over each feature in the Simple Feature collection
# for (i in seq_len(nrow(shp_dat))) {
#   # Extract the geometry for the current feature
#   polygon_geometry <- shp_dat$geometry[i]
#   
#   # Extract the coordinates from the geometry
#   coords <- st_coordinates(polygon_geometry)
#   
#   # Extract the image filename, label, and count
#   image_id_number <- rownames(shp_dat)[i]
#   count <- shp_dat$count[i]
#   label <- shp_dat$label[i]
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
# fire_data$year <- as.numeric(str_sub(image_id_number, 1,4))
# # Extract day of year
# fire_data$doy <- as.numeric(str_sub(image_id_number, 5, 7))
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

#...............................................................
# Collar data ----
#...............................................................


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
dat_sf <- st_transform(dat_sf, st_crs(shp_dat))


#...............................................................
# Calculate distance ----
#...............................................................


# Define the time window for collar fix with +/- 1 hour
# time_window <- 3600 #seconds (1 hour) #manual input
time_window <- 1 %#% 'hour' #ctmm package


# initialise columns to store results
dat$fire_event <- "no"
# dat$n_fire_point <- NA
dat$dist_to_fire <- NA


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
  fire_point <- shp_dat[shp_dat$timestamp >= start_time & shp_dat$timestamp <= end_time,]
  # message("Number of fire points for ", collar_timestamp, " time window: ", nrow(fire_point))
  # dat$n_fire_point[i] <- as.numeric(nrow(fire_point))
  
  # Check if there are any fire points within the time window
  if (nrow(fire_point) == 0) {
    # cat("No data fire points found in window", i, "- moving on to the next collar point.\n")
    # Set results to NA
    dat$dist_to_fire[i] <- NA
    
    next # Move to the next gps collar point if no fire is present
  }
  
  # Convert collar_point and fire_point to sf objects
  collar_point_sf <- st_as_sf(collar_point, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  fire_point_sf <- st_as_sf(fire_point, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  # Match crs
  st_crs(collar_point_sf) <- st_crs(fire_point_sf)
  
  
  # If fire points exist in the time window, proceed with calculation
  tryCatch({
    if (nrow(fire_point) > 0) {
      dat$fire_event[i] <- "yes"
      # calculate the distance between the collar point and the fire point, output comes out as a matrix
      dist_matrix <- st_distance(collar_point_sf, fire_point_sf)
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

write.csv(dat, "./data/input_data/20240819_dist_to_fire_dat_goes18.csv")


dat <- read.csv("./data/input_data/20240819_dist_to_fire_dat_goes18.csv")




#.....................................................................
# Plot ----
#.....................................................................

dat <- read.csv("./data/input_data/20240729_dist_to_fire_dat.csv")


dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat$date = as.Date(dat$date, "%Y-%m-%d")


## Distance to fire over time ----
dat2 <- dat[dat$fire_event == "yes",]

ggplot(data = dat2) +
  geom_line(aes(x = date, y = dist_to_fire)) +
  geom_point(aes(x = date, y = dist_to_fire)) +
  facet_wrap(~ individual.local.identifier, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
             ncol = 2, nrow = 5) +  
  labs(x = 'Date',
       y = 'Distance to fire (m)') +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#convert m into km
dat2$dist_to_fire_km <- (dat2$dist_to_fire)/1000

ggplot(data = dat2) +
  geom_line(aes(x = date, y = dist_to_fire_km)) +
  facet_wrap(~ individual.local.identifier, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
             ncol = 2, nrow = 5) +  
  labs(x = 'Date',
       y = 'Distance to fire (km)') +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# 
# 
# # Distance to fire and elevation
# 
# ggplot(data = dat2) +
#   geom_point(aes(x = elevation, y = dist_to_fire)) +
#   facet_wrap(~ individual.local.identifier, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
#              ncol = 2, nrow = 5) +  
#   labs(x = 'Elevation',
#        y = 'Distance to fire (m)')# +
# # scale_x_date(date_breaks = "1 day", date_labels = "%b-%d") +
# theme_bw() +
#   theme(panel.grid.major = element_blank(), #removes horizontal gridlines
#         panel.grid.minor = element_blank(), #removes vertical gridlines
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
