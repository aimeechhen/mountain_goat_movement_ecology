
# Fire data

# nasa fire data ----
source('./scripts/source/nasa fire prep.r')

# combine all 3 nasa satellite instruments into one df
firms <- rbind(modis, viirs_suomi)
firms <- rbind(firms, viirs_noaa)

# goes fire data ----
source('./scripts/source/goes fire prep.r')

# foippa fire data ----
source('scripts/foippa fire prep.r')




#............................................................................
# CWFIS 
#............................................................................


library(lubridate)
library(sf)


# https://cwfis.cfs.nrcan.gc.ca/downloads/hotspots/archive/ for the perimeters
# 2023_progression

# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
cathedral <- st_transform(cathedral, crs = 4326)

# import fire data
FOIPPA <- st_read('data/fire/bc_gov_FOIPPA/crater_boundaries/23 K52125 Perimeter History Jun17.shp')
FOIPPA <- st_transform(FOIPPA, crs = 4326)
colnames(FOIPPA)[colnames(FOIPPA) == 'CaptureDat'] <- 'date'
colnames(FOIPPA)[colnames(FOIPPA) == 'CaptureTim'] <- 'time'
FOIPPA$date <- as.Date(FOIPPA$date, format = '%Y-%m-%d')
FOIPPA$time <- format(strptime(FOIPPA$time, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
FOIPPA$timestamp <- ymd_hms(paste(FOIPPA$date, FOIPPA$time))
FOIPPA$date_group <- as.factor(FOIPPA$date)
st_bbox(FOIPPA)

library(geodata)
regions <- gadm(country="Canada", level=2, path = tempdir())
bc <- regions[regions$NAME_1 %in% "British Columbia", ]
ok <- bc[bc$NAME_2 == "Okanagan-Similkameen",]
ok <- project(ok, "EPSG:4326")

library(canadianmaps)
library(tidyverse)
bc_shape <- st_as_sf(PROV) %>% 
  filter(PRENAME == 'British Columbia') %>% 
  st_geometry() %>% 
  st_transform(crs = 4326)

shp <- st_read("C:/Users/achhen/Downloads/2023_progression/m3_progression_2023.shp")
shp$date <- as.Date(as.character(shp$date), format = "%Y%m%d")
shp <- shp[format(shp$date, "%m") == "08",]
shp <- st_transform(shp, crs = 4326)

#combine polygons to get aoi
aoi <- c(st_geometry(cathedral), st_geometry(FOIPPA))
aoi <- st_bbox(aoi)
shp <- st_crop(shp, aoi)

shp <- shp[order(shp$date),]

plot(ok)
plot(cathedral[17], add = TRUE, col = NA)
plot(shp[6], add = TRUE)
plot(FOIPPA[9], add = TRUE, col = "red")


library(maptiles)
library(ggplot2)
library(gganimate)
library(tidyterra)
basemaptile <- get_tiles(aoi, provider = "OpenStreetMap", zoom = 13)


p <-
ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = shp, color = "red", fill = NA, lwd = 0.7) +
  # geom_point(data = fire_goats, aes(x = longitude, y = latitude, color = goat_color), size = 3) +
  # geom_point(data = goat_data, aes(x = longitude, y = latitude, color = goat_color), size = 3) +
  # scale_color_manual(values = setNames(goat_palette, goats)) +
  # scale_colour_manual(values = goat_palette,
  #                     labels=c('Goatzilla',
  #                              'Selena Goatmez',
  #                              'The Goatmother',
  #                              'Goatileo',
  #                              'Toats McGoats',
  #                              'Vincent Van Goat',
  #                              'Cathedral Provincial Park')) +
labs(title = "{frame_time}") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  # coord_sf(xlim = c(-120.33383, -119.81255),
  #          ylim = c(48.95037, 49.25434)) +
  transition_time(date)  +  # Animate based on the 'date'
  ease_aes('linear')  # Smooth transition



# animate the plot
anim <- animate(p, nframes = 100, fps = 10)
# display animation
anim

anim_save("figures/cwfis_perimeters.gif", anim)




#....................................................................
#....................................................................
#....................................................................


library(maptiles)
library(ggplot2)
library(gganimate)
library(tidyterra)
library(geodata)
canada <- gadm(country="Canada", level=2, path = tempdir())
bc <- canada[canada$NAME_1 %in% "British Columbia", ]
unique(bc$NAME_2)
# region <- bc[bc$NAME_2 == "Central Okanagan",]
region <- bc[bc$NAME_2 %in% c("Columbia-Shuswap", "Thompson-Nicola"), ]
region <- project(region, "EPSG:4326")
plot(region)


# trying to get the mcdougall fire perimeters

shp <- st_read("C:/Users/achhen/Downloads/2023_progression/m3_progression_2023.shp")
shp$date <- as.Date(as.character(shp$date), format = "%Y%m%d")
# shp <- shp[format(shp$date, "%m") == "08",]
shp <- st_transform(shp, crs = 4326)
shp <- st_crop(shp, region)

# shp <- shp[order(shp$date),]

plot(region)
plot(shp[6], add = TRUE)

basemaptile <- get_tiles(st_bbox(region), 
                         project = TRUE,
                         provider = "OpenStreetMap", zoom = 13)

plot(basemaptile)


p <-
  ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = shp, color = "red", fill = NA, lwd = 0.7) +
labs(title = "{frame_time}") +
  theme_void() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  transition_time(date)  +  # Animate based on the 'date'
  ease_aes('linear')  # Smooth transition

# animate the plot
anim <- animate(p, nframes = 300, fps = 10)
# display animation
anim

# anim_save("figures/mcdougall_fire.gif", anim)

anim_save("figures/thompson_shuswap_fires.gif", anim)


