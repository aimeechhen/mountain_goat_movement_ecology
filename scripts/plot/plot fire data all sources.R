
# plot collar data, multiple fire data sources

library(sf)
library(ggplot2)
library(ctmm)
library(gganimate)
library(lubridate)




# load collar data and shapefile
source('./scripts/collar_data_and_shapefile.r')

# calculate the center point of the collar data based on all the data points. st_union is used due to multiple points
centroid <- st_centroid(st_union(collar_data_sf))
# defining the area of interest based on the center of the collar data and radius around it (in meters)
area_of_interest <- st_buffer(centroid, dist = 50000)
# Create a geometry for the area of interest
area_of_interest <- st_as_sfc(st_bbox(area_of_interest))



ggplot() +
  geom_point(data = collar_data, aes(x = longitude, y = latitude), shape = 6)

# load cathedral park shapefile
source('./scripts/cathedral_park_shapefile.r')

# # Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")

ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1, lwd = 0.5) +
  geom_sf(data = area_of_interest, fill = NA, color = "purple", size = 1, lwd = 0.5)




#.............................................
# nasa fire data ----
source('./scripts/nasa fire prep.r')

# combine all 3 nasa satellite instruments into one df
nasa_fire <- rbind(modis, viirs_suomi)
nasa_fire <- rbind(nasa_fire, viirs_noaa)

# plot data
ggplot() +
  geom_sf(data = nasa_fire)

# ggplot() +
#   geom_sf(data = modis, aes(color = "MODIS")) +
#   geom_sf(data = viirs_suomi, aes(color = "VIIRS Suomi")) +
#   geom_sf(data = viirs_noaa, aes(color = "VIIRS NOAA")) +
#   scale_color_manual(values = c("MODIS" = "red", "VIIRS Suomi" = "purple", "VIIRS NOAA" = "orange"))

ggplot() +
  geom_sf(data = nasa_fire, aes(color = "NASA")) +
  scale_color_manual(values = c("NASA" = "red"))

#.............................................
# goes fire data ----

# source('./scripts/goes fire prep.r')
# goes <- st_transform(goes, "EPSG:3005")
# 
# Draw outline i.e outline_unsmoothed
# goes$geometry <- st_convex_hull(goes$geometry)


# ggplot() +
#   geom_sf(data = goes, aes(color = "Goes"), fill = NA) +
#   scale_color_manual(values = c("Goes" = "darkred"))




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

# Remove perimeter that has a same date as another
# FOIPPA <- FOIPPA[!FOIPPA$Source == 9,]


# plot all polygons
ggplot() +
  geom_sf(data = FOIPPA, aes(color = "FOIPPA"), fill = NA, lwd = 0.5) +
  scale_color_manual(values = c("FOIPPA" = "blue")) + 
  labs(title = "FOIPPA Fire Polygons for Crater Creek",
       x = "Longitude", y = "Latitude")

# plot each perimeter/dat
ggplot() +
  geom_sf(data = FOIPPA, aes(color = date), fill = NA, lwd = 0.5) +
  scale_color_manual(name = "date", values = rainbow(length(unique(FOIPPA$date)))) +
  labs(title = "FOIPPA Fire Polygons for Crater Creek",
       x = "Longitude", y = "Latitude")

ggplot() +
  geom_sf(data = FOIPPA, aes(color = date), fill = NA, lwd = 0.5) +
  facet_wrap(~ date) +  
  labs(title = "FOIPPA Fire Polygons for Crater Creek",
       x = "Longitude", y = "Latitude")


#...............................................
# Plot static all fire sources ----

ggplot() +
  geom_sf(data = cathedral, color = "black", fill = NA, size = 1, linetype = 'dashed') +
  geom_point(data = collar_data, aes(x = longitude, y = latitude, color = "Mountain Goats"), fill = "darkgrey", shape = 25) + 
  # geom_sf(data = modis, aes(color = "MODIS")) +
  # geom_sf(data = viirs_suomi, aes(color = "VIIRS Suomi")) +
  # geom_sf(data = viirs_noaa, aes(color = "VIIRS NOAA")) +
  geom_sf(data = nasa_fire, aes(color = "NASA")) +
  geom_sf(data = FOIPPA, aes(color = "FOIPPA"), fill = NA, lwd = 0.5) +
  # geom_sf(data = goes, aes(color = "Goes"), fill = NA) +
  scale_color_manual(values = c("Mountain Goats" = "darkgrey",
                                "NASA" = "red", 
                                # "VIIRS Suomi" = "purple", "VIIRS NOAA" = "orange",
                                # "Goes" = "darkred",
                                "FOIPPA" = "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())



ggsave(last_plot(), filename = "./figures/fire_data_5_goat_nasa_foippa.png",
       width = 6.86, height = 6, units = "in", dpi = 600)



#___________________________________________________________________
# animate

fire_start <- '2023-07-15'
fire_end <- '2023-09-30'


# modis <- subset(modis, 
#                 modis$date >= min(goes$date) & modis$date <= max(goes$date))


nasa_fire <- subset(nasa_fire,
                    nasa_fire$date >= fire_start & nasa_fire$date <= fire_end)

collar_data <- subset(collar_data, 
                      collar_data$date >= fire_start & collar_data$date <= fire_end)

# animate
p <- ggplot() +
  geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1) +
  geom_point(data = collar_data, aes(x = longitude, y = latitude, color = "Mountain Goats"), shape = 25) + 
  # geom_sf(data = nasa_fire, aes(color = "NASA", fill = Shape_Area)) +
  geom_sf(data = nasa_fire, aes(color = "NASA")) +
  # geom_sf(data = goes, color = "GOES", linewidth = 1.5, alpha = 0.3) +
  geom_sf(data = FOIPPA, aes(color = "FOIPPA"), fill = NA, linewidth = 1.5) +
  scale_color_manual(values = c("Mountain Goats" = "darkgrey",
                                "NASA" = "red", 
                                # "GOES" = "purple",
                                "FOIPPA" = "blue")) +
  labs(title = "Fire data for Crater Creek",
       # fill = "Shape Area",
       subtitle = "Date: {frame_time}",
       x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()) #+
  # # add animation
  # transition_time(date) + # Animate based on date
  # ease_aes('linear') # Smooth transition


# Animate the plot
# p_animated <- animate(p, nframes = 100, fps = 5)
p_animated <- p + 
  transition_time(timestamp) + # Animate based on the timestamp
  ease_aes('linear') # Smooth transition


# Display the animated plot
p_animated


# Set animation parameters to make it slower
# animation <- animate(p_animated, nframes = length(unique(dat$date)) * 10, fps = 10)
# animation <- animate(p_animated, nframes = length(unique(goes$timestamp)) * 2, fps = 10)
animation <- animate(p_animated, nframes = length(unique(collar_data$timestamp)), fps = 10, 
                     width = 600, height = 600)
# Display the animated plot
animation


anim_save("fire_data_5_goat_nasa_foippa_animation.gif", 
          animation = p_animated, path = "figures")

anim_save("fire_data_5_goat_nasa_foippa_animation_slowed.gif", 
          animation = animation, path = "figures")





# Animate FOIPPA only
p <- ggplot() +
  geom_sf(data = FOIPPA, aes(fill = Shape_Area), color = "black", lwd = 0.2) +
  # scale_fill_gradientn(colors = c("darkred", "red", "orange", "yellow")) +
  labs(title = "FOI Fire Polygons for Crater Creek",
       subtitle = "Date: {frame_time}",
       # fill = "Shape Area",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  transition_time(date) + # Animate based on the CaptureDat
  ease_aes('linear') # Smooth transition

# animate the plot
anim <- animate(p, nframes = 100, fps = 5)
# display animation
anim

# save animation
# anim_save("figures/foi_fire_polygons_animation.gif", anim)
