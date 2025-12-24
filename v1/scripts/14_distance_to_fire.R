
# Calculate distance to fire (not as a raster)

library(lubridate)
library(ctmm)
library(sf)
library(ggplot2)
library(ggh4x) # to fill in facet wrap title boxes


#..................................................................
# Import data ----
#..................................................................

# Import combined collar data (original + new)
load("./scripts/working/data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

# convert df -> sf
dat_sf <- st_as_sf(goat_data, coords = c("longitude", "latitude"), crs = 4326)
# set crs as lat/long system
st_crs(dat_sf) <- 4326
# convert crs
dat_sf <- st_transform(dat_sf, crs = 4326)


library(geodata)
library(terra)
regions <- gadm(country="Canada", level=2, path = tempdir())
bc <- regions[regions$NAME_1 %in% "British Columbia", ]
ok <- bc[bc$NAME_2 == "Okanagan-Similkameen",]
ok <- project(ok, "EPSG:4326")

#...................................................
# cathedral park shapefile----
#...................................................

# Import shapefile
bc_parks <- st_read("./scripts/working/data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
cathedral <- st_transform(cathedral, crs = 4326)



#...................................................
# fire data ----
#...................................................


load('./scripts/working/data/fire/foippa.rda')
load('./scripts/working/data/fire/firms.rda')
load('./scripts/working/data/fire/cwfis.rda')
fire <- dplyr::bind_rows(FIRMS, foippa, cwfis)
fire$date <- as.Date(fire$date, format = '%Y-%m-%d')
#combine polygons
aoi <- c(st_geometry(dat_sf), st_geometry(cathedral), st_geometry(fire))
# get bbox of all the polygons
aoi_bbox <- st_bbox(aoi)





plot(ok)
plot(cathedral[17], add = TRUE, col = NA)
plot(cwfis[6], add = TRUE)
plot(foippa[9], add = TRUE)
plot(foippa[9])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~based on timestamp

#______________________________________________________________________________________________
# create a new timestamp column for each year where all the date and time is the same but the year is different to see how it would look compared to other years of the same time and place
fire$timestamp_2019 <- fire$timestamp
fire$timestamp_2020 <- fire$timestamp
fire$timestamp_2021 <- fire$timestamp
fire$timestamp_2022 <- fire$timestamp
fire$timestamp_2023 <- fire$timestamp
fire$timestamp_2024 <- fire$timestamp
# set the dummy year to compare
year(fire$timestamp_2019) <- 2019
year(fire$timestamp_2020) <- 2020
year(fire$timestamp_2021) <- 2021
year(fire$timestamp_2022) <- 2022
year(fire$timestamp_2023) <- 2023
year(fire$timestamp_2024) <- 2024





#/////////////////////////////////////////////////////////////////////////
# Calculate distance to fire ----
#/////////////////////////////////////////////////////////////////////////

library(ctmm)


# Define the time window for collar fix with +/- 1 hour
# time_window <- 3600 #seconds (1 hour) #manual input
time_window <- 1 %#% 'hour' #ctmm package

# initialise columns to store results
dat_sf$fire_event <- "no"
dat_sf$n_fire_points <- NA
dat_sf$dist_to_fire <- NA


#loop through every collar point
for (i in 1:nrow(dat_sf)) {
  # extract a collar point from data
  collar_point <- dat_sf[i,]
  
  # create a custom timestamp of that collar point 
  collar_timestamp <- collar_point$timestamp
  
  # message("Processing GPS collar point ", i, " with timestamp ", collar_timestamp)
  
  # Define time range +/- 1 hour around the collar point timestamp
  start_time <- collar_timestamp - time_window
  end_time <- collar_timestamp + time_window
  
  # extract fire point event within the time window
  # based on the year and using the appropriate timestamp year column
  # extract the year and paste it after the _
  timestamp_year_start <- paste0("timestamp_", year(start_time))
  timestamp_year_end <- paste0("timestamp_", year(end_time))
  
  # extract fire points based on the year-specific timestamp_YYYY column
  fire_point <- fire[fire[[timestamp_year_start]] >= start_time & fire[[timestamp_year_end]] <= end_time,]
  message("Number of fire points for time window: ", collar_timestamp, " ", nrow(fire_point))
  dat_sf$n_fire_points[i] <- as.numeric(nrow(fire_point))
  
  # Check if there are any fire points within the time window
  if (nrow(fire_point) == 0) {
    # cat("No data fire points found in window", i, "- moving on to the next collar point.\n")
    # Set results to NA
    dat_sf$dist_to_fire[i] <- NA
    
    next # Move to the next gps collar point if no fire is present
  }
  
  # If fire points exist in the time window, proceed with calculation
  tryCatch({
    if (nrow(fire_point) > 0) {
      dat_sf$fire_event[i] <- "yes"
      # calculate the distance between the collar point and the fire point, output comes out as a matrix
      dist_matrix <- st_distance(collar_point, fire_point)
      #store results, extracting the min. distance between collar point and [multiple] fire point(s)
      dat_sf$dist_to_fire[i] <- min(dist_matrix) # using min value instead of including all distances for all fire points, also causes issues when storing results due to multiple values
      
    } else {
      # If time window is empty, set results to NA
      dat_sf$dist_to_fire[i] <- NA
      
    }
  }, error = function(e) {
    dat_sf$dist_to_fire[i] <- NA
  })
}






table(dat_sf$year, dat_sf$fire_event) # total = 2010 (not incl. 2023)
table(dat_sf$fire_event) # 2477 in 2023
# 2019  345
# 2020  400
# 2021  408
# 2022  427
# 2023  467
# 2024  430


## results distance to fire over time ----
dat2 <- dat_sf[dat_sf$fire_event == "yes",]


dat2$timestamp <- as.POSIXct(dat2$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat2$date <- as.Date(dat2$date, "%Y-%m-%d")
dat2$month_day <- as.Date(dat2$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting
dat2$year <- as.character(dat2$year)

#convert m into km
dat2$dist_to_fire_km <- (dat2$dist_to_fire)/1000

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~based on timestamp


#/////////////////////////////////////////////////////////// based on date
# distance to fire (date) ----



fire$date_2019 <- fire$date
fire$date_2020 <- fire$date
fire$date_2021 <- fire$date
fire$date_2022 <- fire$date
fire$date_2023 <- fire$date
fire$date_2024 <- fire$date
# set the dummy year
year(fire$date_2019) <- 2019
year(fire$date_2020) <- 2020
year(fire$date_2021) <- 2021
year(fire$date_2022) <- 2022
year(fire$date_2023) <- 2023
year(fire$date_2024) <- 2024



# initialise columns to store results
dat_sf$fire_event <- "no"
dat_sf$n_fire_points <- NA
dat_sf$dist_to_fire <- NA

# i <- 8400

tictoc::tic("loop")
for (i in 1:nrow(dat_sf)) {
  # extract a collar point from data
  collar_point <- dat_sf[i,]
  
  # create a custom timestamp of that collar point 
  collar_date <- collar_point$date
  
  # Define time range +/- 1 hour around the collar point timestamp
  start_time <- collar_point$date
  end_time <- collar_point$date
  
  # extract fire point event within the time window
  # based on the year and using the appropriate timestamp year column
  # extract the year and paste it after the _
  date_start <- paste0("date_", year(start_time))
  date_end <- paste0("date_", year(end_time))
  
  # extract fire points based on the year-specific timestamp_YYYY column
  fire_point <- fire[fire[[date_start]] >= start_time & fire[[date_end]] <= end_time,]
  message(collar_point$goat_name, " ", collar_point$collar_id, " Date: ", collar_point$date, ", Number of fire points: ", nrow(fire_point))
  dat_sf$n_fire_points[i] <- as.numeric(nrow(fire_point))
  
  # Check if there are any fire points within the time window
  if (nrow(fire_point) == 0) {
    # cat("No data fire points found in window", i, "- moving on to the next collar point.\n")
    # Set results to NA
    dat_sf$dist_to_fire[i] <- NA
    
    next # Move to the next gps collar point if no fire is present
  }
  
  # If fire points exist in the time window, proceed with calculation
  tryCatch({
    if (nrow(fire_point) > 0) {
      dat_sf$fire_event[i] <- "yes"
      # calculate the distance between the collar point and the fire point, output comes out as a matrix
      dist_matrix <- st_distance(collar_point, fire_point)
      #store results, extracting the min. distance between collar point and [multiple] fire point(s)
      dat_sf$dist_to_fire[i] <- min(dist_matrix) # using min value instead of including all distances for all fire points, also causes issues when storing results due to multiple values
      
    } else {
      # If time window is empty, set results to NA
      dat_sf$dist_to_fire[i] <- NA
      
    }
  }, error = function(e) {
    dat_sf$dist_to_fire[i] <- NA
  })
}
tictoc::toc() # 3mins
beepr::beep(3)


table(dat_sf$year, dat_sf$fire_event) # total = 2010 (not incl. 2023)
table(dat_sf$fire_event) # 2477 in 2023

#convert m into km
dat_sf$dist_to_fire_km <- (dat_sf$dist_to_fire)/1000

#/////////////////////////////////////////////////////////// based on date end




## results distance to fire over time ----
dat2 <- dat_sf[dat_sf$fire_event == "yes",]


dat2$timestamp <- as.POSIXct(dat2$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat2$date <- as.Date(dat2$date, "%Y-%m-%d")
dat2$month_day <- as.Date(dat2$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting
dat2$year <- as.character(dat2$year)

#convert m into km
dat2$dist_to_fire_km <- (dat2$dist_to_fire)/1000

# Plot ----


goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
dat2$goat_color <- factor(dat2$collar_id, levels = goats) 
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )  # colours correspond with the goat order above

# set the strip colours in the facet wrap
strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))

ggplot(data = subset(dat2, dist_to_fire_km < 10)) +
  geom_line(aes(x = month_day, y = dist_to_fire_km, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  geom_point(aes(x = month_day, y = dist_to_fire_km,
                 colour = ifelse(year == 2023, "2023", "other"),
                 size = ifelse(year == 2023, 2, 1),
                 alpha = ifelse(year != 2023, 0.2, 1))) +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot
              ncol = 2, nrow = 3, strip = strip_col,
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
  

  scale_size_identity() +  # Keep sizes as defined
  scale_alpha_identity() +
  guides(size = "none", alpha = "none", colour = "none") + 
  labs(x = "", 
       y = 'Distance to fire (km)') +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


# ggsave(last_plot(), filename = "./figures/distance_to_fire/combined_data_distance_to_fire.png", 
#        bg = "transparent",
#        width = 12, height = 6, units = "in", dpi = 600)


# ggsave(last_plot(), filename = "./figures/living_labs/figure9.png",
ggsave(last_plot(), filename = "./figures/figures (old)/rmga/figure4.png",
       bg = "transparent",
       width = 12, height = 6, units = "in", dpi = 600)





#///////////////////////////////////////////////////////////


# assign fire boundary numbers ----
# data chunks based on each boundary, using distance to fire rasters 

load('./data/fire/foippa.rda')
dist_to_fire <- rast("./data/fire/dist_to_fire_raster.tif")
load('./data/fire/foippa.rda')
# fire <- foippa
load('./data/fire/cwfis.rda')
# fire <- cwfis
fire <- dplyr::bind_rows(foippa, cwfis)

# assign each perimeter a number
fire$boundary_num <- seq(from = 1, to = nrow(fire), by = 1)

# subset data chunks based on the boundary dates

# data chunks for distance to fire based on time, so theyre separated based on the boundary dates
# the chunks are divided from boundary date to the next the date because the next boundary date is what we know for sure what it is and we dont know when it was like that, but only that on that date it is like that so we cant like buffer it

#  1 -> "2023-07-23" = june 22-aug 2
#  2 -> "2023-08-03" = aug 3-4
#  3 -> "2023-08-05" = aug 5-9
#  4 -> "2023-08-10" = aug 10-11
#  5 -> "2023-08-12" = aug 12-13
#  6 -> "2023-08-14" =  aug 14
#  7 -> "2023-08-15" = aug 15
#  8 -> "2023-08-16" = aug 16
#  9 -> "2023-08-17"  = aug 17-18
#  10 -> "2023-08-19"  = aug 19-20
#  11 -> "2023-08-21"  = aug 21
#  12 -> "2023-08-22"  = aug 22-28
#  13 -> "2023-08-29"  = aug 29-oct 26

load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")

# set up the column
goat_data$boundary_num <- NA_integer_
# assign the boundary number based on the date range
for (i in 1:nrow(goat_data)) {
  # for every row
  data_row <- goat_data$date[i]
  # assign the boundary number
  if (data_row >= as.Date("2023-06-22") & data_row <= as.Date("2023-08-02")) {
    goat_data$boundary_num[i] <- 1
  } else if (data_row >= as.Date("2023-08-03") & data_row <= as.Date("2023-08-04")) {
    goat_data$boundary_num[i] <- 2
  } else if (data_row >= as.Date("2023-08-05") & data_row <= as.Date("2023-08-09")) {
    goat_data$boundary_num[i] <- 3
  } else if (data_row >= as.Date("2023-08-10") & data_row <= as.Date("2023-08-11")) {
    goat_data$boundary_num[i] <- 4
  } else if (data_row >= as.Date("2023-08-12") & data_row <= as.Date("2023-08-13")) {
    goat_data$boundary_num[i] <- 5
  } else if (data_row == as.Date("2023-08-14")) {
    goat_data$boundary_num[i] <- 6
  } else if (data_row == as.Date("2023-08-15")) {
    goat_data$boundary_num[i] <- 7
  } else if (data_row == as.Date("2023-08-16")) {
    goat_data$boundary_num[i] <- 8
  } else if (data_row >= as.Date("2023-08-17") & data_row <= as.Date("2023-08-18")) {
    goat_data$boundary_num[i] <- 9
  } else if (data_row >= as.Date("2023-08-19") & data_row <= as.Date("2023-08-20")) {
    goat_data$boundary_num[i] <- 10
  } else if (data_row == as.Date("2023-08-21")) {
    goat_data$boundary_num[i] <- 11
  } else if (data_row >= as.Date("2023-08-22") & data_row <= as.Date("2023-08-28")) {
    goat_data$boundary_num[i] <- 12
  } else if (data_row >= as.Date("2023-08-29") & data_row <= as.Date("2023-10-26")) {
    goat_data$boundary_num[i] <- 13
  }
  
}



