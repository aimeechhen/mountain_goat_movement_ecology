

library(ctmm)
library(raster)
library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tictoc)
library(beepr)
library(lubridate)
library(crayon)
library(moveVis)
# library(move)
# devtools::install_git('https://gitlab.com/bartk/move2.git')
library(move2)
library(sf)
library(gganimate)
library(ggplot2)
library(tidyterra)
library(maptiles)



# subset to only the fire goats
goats <- c("goatileo", "goatzilla", "selena_goatmez", "the_goatmother", "toats_mcgoats", "vincent_van_goat")

#.........................................................................
# DATA PREP ----

# collar data
load("data/collar_data/collar_data_20241123.rda")
# specify which dataset its from
collar_data$data_type <- "original_data"

# update dataset to reflect the name changes 2025-02
collar_data$goat_name[collar_data$goat_name == "kid_rock" & collar_data$collar_id == "30561"] <- "selena_goatmez"
collar_data$goat_name[collar_data$goat_name == "rocky" & collar_data$collar_id == "30613" ] <- "goatileo"
collar_data$goat_name[collar_data$goat_name == "vertigoat" & collar_data$collar_id == "30567" ] <- "goatbriella"
collar_data$goat_name[collar_data$goat_name == "billy" & collar_data$collar_id == "30636" ] <- "ryan_goatsling"



dat <- collar_data[collar_data$goat_name %in% goats,]
dat$goat_name <- as.factor(dat$goat_name)


#.........................................................................
# new collar data
load("data/collar_data/new_collar_data_20250218.rda")
new_collar$collar_id <- as.factor(new_collar$collar_id)
# drop columns that isn't needed 
new_collar <- subset(new_collar, select = c(collar_id, timestamp, latitude, longitude, data_type))

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# remove cliff
goat_info <- goat_info[goat_info$goat_name %in% goats,]
new_collar <- merge(new_collar, goat_info[, c("goat_name", "goat_id", "collar_id")], by = "collar_id", all.x = TRUE)

# str(new_collar)
new_collar$goat_name <- as.factor(new_collar$goat_name)
# new_collar$timestamp <- as.POSIXct(new_collar$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
new_collar$date <- as.Date(new_collar$timestamp)
new_collar$year <- year(new_collar$timestamp)
new_collar$month <- month(new_collar$timestamp, label = FALSE) #label = false for numerical month
new_collar$day <- day(new_collar$timestamp)
new_collar$month_day <- format(new_collar$timestamp, "%m-%d")
new_collar$doy <- yday(new_collar$timestamp) #day of the year

# combine data for full data
# combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
full_data <- bind_rows(dat, new_collar)

full_data <- full_data[complete.cases(full_data$longitude, full_data$latitude, full_data$timestamp, full_data$goat_name), ]

#............................................................
# fire goat data ----

# reassigning back to fire_goats for workflow, this now contains original and new fire goat collar data
fire_goats <- full_data
# fire_goats <- new_collar
#sort data by goat and timestamp
fire_goats <- fire_goats[order(fire_goats$collar_id, fire_goats$timestamp), ]

# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "2023-07-22"
fire_end <- "2023-10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
fire_goats <- fire_goats[fire_goats$date >= fire_start & fire_goats$date <= fire_end, ] #21892 obs

# #check for na in the df
# any(is.na(fire_goats))
# NA are showing up in the df for some reason n=97 with NA across every column
fire_goats <- fire_goats[complete.cases(fire_goats$longitude, fire_goats$latitude, fire_goats$timestamp, fire_goats$goat_name), ]

# khroma package, bright palette
goat_palette <- c("#4477AA", "#fa9fb5", "#41ab5d", "#fed976", "#41b6c4", "#88419d" )


#.....................................................................

# what is the extent of the goat data
dat_sf <- st_as_sf(fire_goats, coords = c("longitude", "latitude"), crs = 4326)
# get extent of the data aka bounding box
st_bbox(dat_sf)
# set crs as lat/long system
st_crs(dat_sf) <- 4326

# cathedral ----
# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
# # Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")
st_crs(cathedral) <- 4326


# Extract the coordinates
coords <- st_coordinates(cathedral)
# Combine coordinates with the attributes of the sf object
cathedral_coords <- cbind(st_drop_geometry(cathedral), coords)
# create df so it can be used in movevis
cathedral_df <- data.frame(x = cathedral_coords$X,
                           y = cathedral_coords$Y)


# calculate the center point of the collar data based on all the data points. st_union is used due to multiple points
centroid <- st_centroid(st_union(dat_sf))
centroid <- st_centroid(cathedral)
# defining the area of interest based on the center of the collar data/park and radius around it (in meters)
area_of_interest <- st_buffer(centroid, dist = 15000)
# Create a geometry for the area of interest
area_of_interest <- st_as_sfc(st_bbox(area_of_interest))
st_crs(area_of_interest) <- 4326 
# extract aoi
aoi_bbox <- as.numeric(st_bbox(area_of_interest)[c("xmin", "xmax", "ymin", "ymax")])

plot(area_of_interest)
plot(cathedral, add = TRUE)


#.............................................
# foippa fire data ----

FOIPPA <- st_read('data/fire/bc_gov_FOIPPA/crater_boundaries/23 K52125 Perimeter History Jun17.shp')
FOIPPA <- st_transform(FOIPPA, crs = 4326)

colnames(FOIPPA)[colnames(FOIPPA) == 'CaptureDat'] <- 'date'
colnames(FOIPPA)[colnames(FOIPPA) == 'CaptureTim'] <- 'time'
FOIPPA$date <- as.Date(FOIPPA$date, format = '%Y-%m-%d')
FOIPPA$time <- format(strptime(FOIPPA$time, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
FOIPPA$timestamp <- ymd_hms(paste(FOIPPA$date, FOIPPA$time))


# Extract the coordinates
COORDS <- st_coordinates(FOIPPA)
# Combine coordinates with the attributes of the sf object
foippa_coords <-  cbind( FOIPPA[COORDS[, "L1"], "timestamp", drop = FALSE], COORDS)
# create df so it can be used in movevis
foippa_df <- data.frame(longitude = foippa_coords$X,
                        latitude = foippa_coords$Y,
                        timestamp = as.POSIXct(foippa_coords$timestamp),
                        goat_name = "wildfire")



#clean up environment
rm(bc_parks, centroid, collar_data, dat, goat_info, new_collar)




#_______________________________________________________________
# moveVis ----





# Convert dataframe collar data into a move object
move_data <- df2move(fire_goats, 
                     proj = "epsg:4326",
                     x = "longitude", y = "latitude", time = "timestamp",
                     track_id = "goat_name")

move_data <- sp::spTransform(move_data, crs("+init=epsg:4326"))
# move_data <- st_transform(move_data, crs = st_crs("epsg:4326"), aoi = aoi_bbox)
# move_data <- sp::spTransform(move_data, crs("+init=epsg:3857"))
ext <- extent(-120.3819 , -119.9626 , 48.93508 , 49.20846 )

# check the crs of the move_data object
st_crs(move_data)

# look at the data points
# view_spatial(move_data)
# subset_move_data <- subset_move(move_data, from = "2023-07-22 00:00:00", to = "2023-10-27 00:00:00")

# align move_data to a uniform time scale (may take a while)
m <- align_move(move_data, res = 120, unit = "mins") # lower the res = , longer it takes
# m <- align_move(subset_move_data, res = 120, unit = "mins") # lower the res = , longer it takes
# st_crs(m)

# #You can also use a custom extent (and use the pipe to customize frames):
# ext <- extent(-120.3819 , -119.9626 , 48.93508 , 49.20846 )
# # changing extent doesnt work....

# use_multicore() n_cores =

# create spatial frames with a OpenStreetMap watercolour map (basemap is used by default)
tic()
frames <- 
  frames_spatial(m, path_colours = goat_palette, 
                 # tail_colour = "white", tail_size = 0.8, #how to include tail
                 ext = ext, equidistant = F) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress() %>% 
  
  # let's draw a polygon on frames:
  # cannot expand the map...even tried modifying the extent, doesnt change??
  # add_gg(gg = expr(
  # geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1, linetype = "dashed")), data = cathedral) %>% 
  add_gg(gg = expr(
    geom_path(aes(x = x, y = y), data = cathedral_df,
              colour = "black", linetype = "dashed", linewidth = 1.5)), data = cathedral_df) %>%
  add_gg(expr(theme(legend.title = element_blank()))) %>% 
  add_gg(expr(guides(linetype = "none"))) %>% 
  add_text("Cathedral Provincial Park Boundary", 
           x = -120.07, y = 49.003,
           colour = "black", size =2)



# add_gg(gg = expr(
#   geom_path(aes(x = x, y = y), data = foippa_df,
#             colour = "red", linetype = "solid", linewidth = 1.5)), data = foippa_df)
# add_text("---", 
#          x = -120.06, y = 49.005,
#          colour = "black", size = 3)

toc()
# 1162 frames, target = 30s clip

# frames[[100]] +# preview one of the frames, e.g. the 100th frame
#      geom_path(aes(x = x, y = y), data = foippa_df,
#               colour = "red", linetype = "solid", linewidth = 1.5) # test additional layers

animate_frames(frames, out_file = tempfile(fileext = ".gif"), fps = 40, overwrite = TRUE)

# animate frames (may take a while, like hours, for a lot of data)
tic()
animate_frames(frames, out_file = "figures/moveVis_during_fire.mp4", 
               fps = 40, overwrite = TRUE)
# Approx animation duration = ~750.25s (~12.5 min) @ 25 fps for 18757 frames
toc()



#________________________________________
# gganimate ----

bbox <- st_bbox(c(
  xmin = -120.33383,
  ymin = 48.95037,
  xmax = -119.81255,
  ymax = 49.25434
), crs = st_crs(cathedral)) 

bbox <- st_bbox(cathedral)

basemaptile <- get_tiles(bbox, provider = "OpenStreetMap", zoom = 13)

fire_goats$goat_color <- factor(fire_goats$goat_name, levels = goats) 
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )

# goat_palette <- c("#4477AA", "#fa9fb5", "#41ab5d", "#fed976", "#41b6c4", "#88419d", "black" )

# p <-
  ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = cathedral, aes(color = "Cathedral Provincial Park"), fill = NA, linewidth = 1) +
  geom_sf(data = FOIPPA, color = "red", fill = NA, lwd = 0.7) +
  geom_point(data = fire_goats, aes(x = longitude, y = latitude, color = goat_color), size = 3) +
  # scale_color_manual(values = setNames(goat_palette, goats)) +
  scale_colour_manual(values = goat_palette,
                      labels=c('Goatzilla',
                               'Selena Goatmez',
                               'The Goatmother',
                               'Goatileo',
                               'Toats McGoats',
                               'Vincent Van Goat',
                               'Cathedral Provincial Park')) +
  labs(title = "{frame_time}") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_sf(xlim = c(-120.33383, -119.81255),
           ylim = c(48.95037, 49.25434)) +
  transition_time(date)  +  # Animate based on the 'date'
  ease_aes('linear')  # Smooth transition



# animate the plot
anim <- animate(p, nframes = 100, fps = 10)
# display animation
anim

anim_save("figures/goats_during_fire.mp4", anim)
anim_save("figures/goats_during_fire.gif", anim)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DAG ----
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# set parameters of dag, colour, text etc

dag_code <- "
digraph DAG {
  # Set background color and font size
  graph [rankdir=LR, bgcolor = transparent, fontsize=14, fontname = 'Arial']   
  
  # Customize node appearance for each node
  node [shape = circle, style = filled, fontname = 'Arial', fontsize = 14, fontcolor = black, width = 1.5]   
  
  A [label = 'Wildfire', fillcolor = '#cc3311', fontcolor = white]      # Wildfire node as red
  B [label = 'Mountain Goats', fillcolor = lightblue]  # Mountain Goats node as light blue
  C [label = 'Home range', fillcolor = '#ccddaa', fontcolor = black]   # Other nodes as light green
  D [label = 'Habitat use', fillcolor = '#ccddaa', fontcolor = black]   
  E [label = 'Movement', fillcolor = '#ccddaa', fontcolor = black]   

  A -> B [color = white, penwidth = 3] #arrow thickness
  B -> C [color = white, penwidth = 1]
  B -> D [color = white, penwidth = 1]
  B -> E [color = white, penwidth = 1]
  
  # Custom node positions (adjust for asymmetry)
  A [pos='1,2!']
  B [pos='2,3!']
  C [pos='3,4!']
  D [pos='4,2!']
  E [pos='4,5!']
}
"
dag_plot <-
  grViz(dag_code)


# Convert to SVG, then save as png
dag_plot = DiagrammeRsvg::export_svg(dag_plot)
dag_plot = charToRaw(dag_plot) # flatten
rsvg::rsvg_png(dag_plot, "figures/dag_plot.png") # saved graph as png in current working directory













#/////////////////////////////////////////////////////////////////////////
# Calculate distance to fire ----
#/////////////////////////////////////////////////////////////////////////



# import cropped fire data
source('./scripts/source/nasa fire prep.r')
firms <- rbind(modis, viirs_suomi)
firms <- rbind(firms, viirs_noaa)
fire <- bind_rows(firms, FOIPPA)

fire <- st_transform(fire, crs = st_crs(dat_sf))


# Define the time window for collar fix with +/- 1 hour
# time_window <- 3600 #seconds (1 hour) #manual input
time_window <- 1 %#% 'hour' #ctmm package

# initialise columns to store results
dat_sf$fire_event <- "no"
dat_sf$n_fire_points <- NA
dat_sf$dist_to_fire <- NA

# i <- 1
# i <- dat_sf[dat_sf$date == "2023-08-16",][8,]
# i <- dat_sf[dat_sf$timestamp == "2023-08-18 20:16:08",]


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
  fire_point <- fire[fire$timestamp >= start_time & fire$timestamp <= end_time,]
  message("Number of fire points for ", collar_timestamp, " time window: ", nrow(fire_point))
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







## results distance to fire over time ----
dat2 <- dat_sf[dat_sf$fire_event == "yes",]
table(dat_sf$fire_event) #461

dat2$timestamp = as.POSIXct(dat2$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat2$date = as.Date(dat2$date, "%Y-%m-%d") # put in dummy year to be able to plot on the same panel

#.....................................................................
# Plot distance to fire ----
#.....................................................................

# dat <- read.csv("./data/input_data/20240729_dist_to_fire_dat.csv")





ggplot(data = dat2) +
  geom_line(aes(x = date, y = dist_to_fire)) +
  geom_point(aes(x = date, y = dist_to_fire)) +
  facet_wrap(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
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
  facet_wrap(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
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



#..................................................................
# points under 20, 10 etc km
# 
# library(ggh4x) # to fill in facet wrap title boxes
# strip <- strip_themed(background_x = 
#                         elem_list_rect(fill = goat_palette))
# # palette is out of order
# 
# ggplot(data = subset(dat2, dist_to_fire_km < 20)) +
#   geom_line(aes(x = date, y = dist_to_fire_km, color = goat_name)) +
#   geom_point(aes(x = date, y = dist_to_fire_km, color = goat_name)) +
#   facet_wrap2(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
#               ncol = 2, nrow = 3, strip = strip) +  
#   labs(x = 'Date',
#        y = 'Distance to fire (km)') +
#   scale_x_date(date_breaks = "1 day", date_labels = "%b-%d") +
#   scale_color_manual(values = goat_palette) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), #removes horizontal gridlines
#         panel.grid.minor = element_blank(), #removes vertical gridlines
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         legend.position = "none",
#         strip.text = element_text(color = "white", face = "bold"))
# 
# 
# ggsave(last_plot(), filename = "./figures/distance_to_fire/20250220_dist_to_fire_h.png", bg = "transparent",
#        width = 12, height = 6, units = "in", dpi = 600)
# 
# 
# 





#...........................................................................
# all years fire period ----
#...........................................................................

# data during the fire period across all years

# reassigning back to fire_goats for workflow, this now contains original and new fire goat collar data
fire_goats <- full_data
# fire_goats <- new_collar
#sort data by goat and timestamp
fire_goats <- fire_goats[order(fire_goats$collar_id, fire_goats$timestamp), ]

# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
fire_goats_all <- fire_goats[fire_goats$month_day >= fire_start & fire_goats$month_day <= fire_end, ] #21892 obs

# # data carpentry
# fire_goats_all$time <- format(fire_goats_all$timestamp, format = "%H:%M:%S")

# convert to sf object
fire_goats_all_sf <- st_as_sf(fire_goats_all, coords = c("longitude", "latitude"), crs = 4326)
# get extent of the data aka bounding box
# st_bbox(fire_goats_all_sf)
# set crs as lat/long system
st_crs(fire_goats_all_sf) <- 4326


# create a new timestamp column for each year where all the date and time is the same but the year is different to see how it would look compared to other years of the same time and place
fire$timestamp_2019 <- fire$timestamp
fire$timestamp_2020 <- fire$timestamp
fire$timestamp_2021 <- fire$timestamp
fire$timestamp_2022 <- fire$timestamp
fire$timestamp_2023 <- fire$timestamp
fire$timestamp_2024 <- fire$timestamp
year(fire$timestamp_2019) <- 2019
year(fire$timestamp_2020) <- 2020
year(fire$timestamp_2021) <- 2021
year(fire$timestamp_2022) <- 2022
year(fire$timestamp_2023) <- 2023
year(fire$timestamp_2024) <- 2024

# extract collar data for the fire period across all years
# plot distance to fire across all years to compare behaviour

# recalculate distance to fire across all years

# Define the time window for collar fix with +/- 1 hour
time_window <- 1 %#% 'hour' #ctmm package

dat_sf <- fire_goats_all_sf

# initialise columns to store results
dat_sf$fire_event <- "no"
dat_sf$n_fire_points <- NA
dat_sf$dist_to_fire <- NA

# i <- 1
# i <- dat_sf[dat_sf$date == "2023-08-16",][8,]
# i <- dat_sf[dat_sf$timestamp == "2023-08-18 20:16:08",]


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
  message("Number of fire points for ", collar_timestamp, " time window: ", nrow(fire_point))
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






#.....................................................................
# Plot distance to fire ----
#.....................................................................

# library(ggstar)

## results distance to fire over time ----
dat2 <- dat_sf[dat_sf$fire_event == "yes",]

table(dat_sf$year, dat_sf$fire_event) # total = 2010 (not incl. 2023)
table(dat_sf$fire_event) # 2477 with 2023
# 2019  345
# 2020  400
# 2021  408
# 2022  427
# 2023  467
# 2024  430


dat2$timestamp <- as.POSIXct(dat2$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat2$date <- as.Date(dat2$date, "%Y-%m-%d")
dat2$month_day <- as.Date(dat2$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting
dat2$year <- as.character(dat2$year)

#convert m into km
dat2$dist_to_fire_km <- (dat2$dist_to_fire)/1000




# point_shapes <- c("2019" = "22", # square
#   "2020" = 24, # triangle
#   "2021" = 3, # X
#   "2022" = "\u2605", # star
#   "2024" = "\u2b27" # skinny diamond
# )

fire_goats$goat_color <- factor(fire_goats$goat_name, levels = goats) 
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )

# goat_palette <- c("#4477AA", "#fa9fb5", "#41ab5d", "#fed976", "#41b6c4", "#88419d", "black" )

labels=c('Goatzilla',
         'Selena Goatmez',
         'The Goatmother',
         'Goatileo',
         'Toats McGoats',
         'Vincent Van Goat')


library(ggh4x) # to fill in facet wrap title boxes
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
  labs(x = 'Date',
       y = 'Distance to fire (km)') +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


ggsave(last_plot(), filename = "./figures/distance_to_fire/20250222_dist_to_fire_all.png", 
       bg = "transparent",
       width = 12, height = 6, units = "in", dpi = 600)





#...............................................................................
# static plot goat fire basemap ----



fire_goats$goat_color <- factor(fire_goats$goat_name, levels = goats) 

bbox <- st_bbox(c(
  xmin = -120.33383,
  ymin = 48.95037,
  xmax = -119.81255,
  ymax = 49.25434
), crs = st_crs(cathedral)) 


basemaptile <- get_tiles(bbox, provider = "OpenStreetMap", zoom = 13)
crs(basemaptile)

FOIPPA$date_group <- as.factor(FOIPPA$date)
# colourblind friendly
red_gradient <- c(
  "#FEE5D9",   
  "#FCBBA1",  
  "#FC9272",  
  "#FB6A4A",  
  "#EF3B2C",    
  "#CB181D",
  "#a50f15"
)

#generate the palette
red_gradient_palette <- colorRampPalette(red_gradient)(15)
# library(RColorBrewer)
# red_gradient_palette <- colorRampPalette(brewer.pal(15, "Reds"))(length(unique(FOIPPA$date)))

ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = cathedral, aes(color = "Cathedral Provincial Park"), fill = NA, linewidth = 1, alpha = 0.3) +
  geom_point(data = fire_goats, aes(x = longitude, y = latitude, color = goat_color), size = 2, alpha = 0.2) +
  geom_sf(data = FOIPPA, color = red_gradient_palette, fill = NA, linewidth = 1.2)  +
  scale_colour_manual(values = goat_palette,
                      labels=c('Goatzilla',
                               'Selena Goatmez',
                               'The Goatmother',
                               'Goatileo',
                               'Toats McGoats',
                               'Vincent Van Goat',
                               'Cathedral Provincial Park')) +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white")) 



ggsave(last_plot(), filename = "./figures/data_map.png", 
       bg = "transparent",
       width = 6, height = 6.23, units = "in", dpi = 600)


