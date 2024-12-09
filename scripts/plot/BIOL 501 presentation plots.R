
# BIOL 501 presentation figures



# Plotting collar data for visualization
#https://movevis.org/

library(moveVis)
library(move)



# Load collar data
load("data/collar_data/collar_data_20241123.rda")

# subset to when the fire was only
fire_goats <- collar_data[collar_data$timestamp > "2023-07-01",]

# khroma package, bright palette
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377" )

# longitude <- -120.1724
# latitude <- 49.07157
# # Identify the goats with data that are still alive
# surviving_goats <- unique(collar_data$goat_name[collar_data$timestamp > "2023-07-21"])
# subset them
# fire_goats <- collar_data[collar_data$goat_name %in% surviving_goats, ]

# Convert dataframe collar data into a move object
move_data <- df2move(fire_goats, 
                     proj = "epsg:4326",
                     x = "longitude", y = "latitude", time = "timestamp",
                     track_id = "goat_name")

move_data <- sp::spTransform(move_data, crs("+init=epsg:4326"))

# align move_data to a uniform time scale (may take a while)
m <- align_move(move_data, res = 120, unit = "mins")

ext <- extent(m)
ext@xmin <- -120.4929 
ext@xmax <- -120.0126 
ext@ymin <- 48.83183 
ext@ymax <- 49.08616

ext(m) <- ext

# create spatial frames with a OpenStreetMap watercolour map (basemap is used by default)
frames <- frames_spatial(m, path_colours = goat_palette,
                         # map_service = "osm", map_type = "streets", 
                         map_res = 0.8, 
                         # equidistant = F,
                         ext = ext) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  # add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()
  


frames[[100]] # preview one of the frames, e.g. the 100th frame

animate_frames(frames, out_file = "figures/moveVis_fire_goats.gif", width = 700, height = 500, res = 60)







#...........................................................................
# plot fire boundaries
#...........................................................................

library(sf)
library(ggplot2)
library(ctmm)
library(gganimate)
library(lubridate)


# load collar data and shapefile
source('./scripts/source/collar_data_and_shapefile.r')

# calculate the center point of the collar data based on all the data points. st_union is used due to multiple points
centroid <- st_centroid(st_union(collar_data_sf))
# defining the area of interest based on the center of the collar data and radius around it (in meters)
area_of_interest <- st_buffer(centroid, dist = 50000)
# Create a geometry for the area of interest
area_of_interest <- st_as_sfc(st_bbox(area_of_interest))

# load cathedral park shapefile
source('./scripts/source/cathedral_park_shapefile.r')

# # Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")


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

ggplot() +
  # geom_point(data = collar_data, aes(x = longitude, y = latitude), color = "black") +
  geom_sf(data = FOIPPA, aes(color = as.factor(date)), fill = NA, lwd = 0.7) +
  scale_color_manual(name = "Fire boundary", values = rainbow(length(unique(FOIPPA$date)))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(last_plot(),  file="figures/foippa_coloured.png",
       width = 6, height = 6.23, units = "in", dpi = 600, bg = "transparent")





fire_start <- '2023-07-22'
fire_end <- '2023-10-26'





# Animate FOIPPA only
p <- ggplot() +
  geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = FOIPPA, aes(color = as.factor(date)), fill = NA, lwd = 0.7) +
  scale_color_manual(name = "Date", values = rainbow(length(unique(FOIPPA$date)))) +
  labs(title = "Crater Creek fire boundaries",
       subtitle = "Date: {closest_state}",
       # fill = "Shape Area",
       x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  transition_states(date, wrap = TRUE) + # Animate based on the CaptureDat
  ease_aes('linear') # Smooth transition


# animate the plot
anim <- animate(p, nframes = 100, fps = 10)
# display animation
anim

anim_save("figures/foippa_animation_coloured.gif", anim)



#.....................................................................
# park + fire + goat ----

# subset to when the fire was only
fire_goats <- collar_data[collar_data$timestamp > "2023-07-01",]

# khroma package, bright palette
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377" )


p <- ggplot() +
  geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = FOIPPA, aes(color = as.factor(date)), fill = NA, lwd = 0.7) +
  geom_point(data = fire_goats, aes(x = longitude, y = latitude), color = "grey") +
  scale_color_manual(name = "fire boundary", values = rainbow(length(unique(FOIPPA$date)))) +
  labs(title = "Mountain goats and fire boundaries around Cathedral Provincial Park",
       subtitle = "Date: {frame_time}",
       x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  transition_time(date) +  # Animate based on the 'date'
  ease_aes('linear')  # Smooth transition


# animate the plot
anim <- animate(p, nframes = 100, fps = 10)
# display animation
anim

anim_save("figures/goat_foippa_animation_coloured.gif", anim)
