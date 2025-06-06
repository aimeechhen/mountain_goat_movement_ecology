
# Calculate the distance to fire

library(sf)
library(lubridate)
library(ctmm)
library(ggplot2)
library(dplyr)

rm(list = ls())

# import data from 02_outlier_detection.r
# dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')

# load collar data and shapefile
source('./scripts/source/collar_data_and_shapefile.r')
dat <- collar_data_sf
dat <- dat[dat$year == '2023',]
dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat$date = as.Date(dat$date, "%Y-%m-%d")


goat_info <- read.csv("data/goat_info.csv")
goat_info <- goat_info[!goat_info$goat_name == "cliff",]
dat <- merge(dat, goat_info[, c("goat_name", "goat_id")], by = "goat_id", all.x = TRUE)
dat <- relocate(dat, c("goat_name", "goat_id"), .before = collar_id)
dat <- dat[dat$date > "2023-07-21",]

# import cropped fire data
source('./scripts/source/nasa fire prep.r')
nasa_fire <- rbind(modis, viirs_suomi)
nasa_fire <- rbind(nasa_fire, viirs_noaa)





#...............................................................
# Calculate distance ----
#...............................................................

# Define the time window for collar fix with +/- 1 hour
# time_window <- 3600 #seconds (1 hour) #manual input
time_window <- 1 %#% 'hour' #ctmm package


# initialise columns to store results
dat$fire_event <- "no"
dat$n_fire_points <- NA
dat$dist_to_fire <- NA

# i <- 1
# i <- dat_sf[dat_sf$date == "2023-08-16",][8,]
# i <- dat_sf[dat_sf$timestamp == "2023-08-18 20:16:08",]


#loop through every collar point
for (i in 1:nrow(dat)) {
  # extract a collar point from data
  collar_point <- dat[i,]
  # extract timestamp of that collar point
  collar_timestamp <- collar_point$timestamp
  # message("Processing GPS collar point ", i, " with timestamp ", collar_timestamp)
  
  # Define time range +/- 1 hour around the collar point timestamp
  start_time <- collar_timestamp - time_window
  end_time <- collar_timestamp + time_window
  
  # extract fire point event within the time window
  fire_point <- nasa_fire[nasa_fire$timestamp >= start_time & nasa_fire$timestamp <= end_time,]
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

# write.csv(dat, "./data/input_data/20240729_dist_to_fire_dat_modis.csv")




#.....................................................................
# Plot ----
#.....................................................................

# dat <- read.csv("./data/input_data/20240729_dist_to_fire_dat.csv")


dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat$date = as.Date(dat$date, "%Y-%m-%d")


## Distance to fire over time ----
dat2 <- dat[dat$fire_event == "yes",]

ggplot(data = dat2) +
  geom_line(aes(x = date, y = dist_to_fire)) +
  geom_point(aes(x = date, y = dist_to_fire)) +
  facet_wrap(~ goat_name, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
             ncol = 1, nrow = 6) +  
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
  geom_point(aes(x = date, y = dist_to_fire_km)) +
  facet_wrap(~ goat_name, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
             ncol = 1, nrow = 6) +  
  labs(x = 'Date',
       y = 'Distance to fire (km)') +
  scale_x_date(date_breaks = "1 day", date_labels = "%b-%d") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


ggsave(last_plot(), filename = "./figures/dist_to_fire_boundary_area_nasa.png", bg = "transparent",
       width = 12, height = 9, units = "in", dpi = 600)
# 
# Distance to fire and elevation
# 
# ggplot(data = dat2) +
#   geom_point(aes(x = elevation, y = dist_to_fire)) +
#   facet_wrap(~ goat_name, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot
#              ncol = 2, nrow = 5) +
#   labs(x = 'Elevation',
#        y = 'Distance to fire (m)')# +
#   # scale_x_date(date_breaks = "1 day", date_labels = "%b-%d") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), #removes horizontal gridlines
#         panel.grid.minor = element_blank(), #removes vertical gridlines
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
