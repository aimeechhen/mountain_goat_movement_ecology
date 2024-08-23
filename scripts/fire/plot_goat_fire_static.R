


modis_nrt <- st_read("spatial/data/nasa_fire/cathedral/modis_nrt/modis_nrt.shp")
viirs_suomi <- st_read("spatial/data/nasa_fire/cathedral/viirs_suomi/viirs_suomi.shp")
viirs_noaa <- st_read("spatial/data/nasa_fire/cathedral/viirs_noaa/viirs_noaa.shp")


# Create a time column, data carpentry
# Add leading zeros and colon ":" to the time strings to match the format "HH:MM"
fire_data$time <- str_pad(fire_data$ACQ_TIME, width = 4, side = "left", pad = "0")
fire_data$time <- paste0(substr(fire_data$time, 1, 2), ":", substr(fire_data$time, 3, 4))
# Combine date and time columns into a single POSIXct column
fire_data$timestamp <- as.POSIXct(paste(fire_data$ACQ_DATE, fire_data$time))


# Plot fire data
ggplot() +
  geom_sf(data = fire_data, aes(color = "red"))

#.........................................................

# Import Provincial park polygons
bc_parks <- st_read("spatial/data/shapefiles/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
bc_parks <- st_transform(bc_parks, crs = st_crs(modis_nrt))

#subset to cathedral park
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
# Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")


# Plot Cathedral Park
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1)



# Plot Cathedral Park and fire data
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_sf(data = fire_data, aes(color = "red"))


#.........................................................


# Add mountain goat data
load("data/goat/goat_data.rda")

#only goat data
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        # legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank())

#
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat)) + 
  geom_sf(data = fire_data, aes(color = "red")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank())
        
ggsave(last_plot(), file="spatial/figures/fire_goat_static.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")

ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat)) + 
  geom_sf(data = fire_data, aes(color = "red")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank())

ggsave(last_plot(), file="spatial/figures/fire_goat_static.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")
