
# Fire data

library(lubridate)
library(sf)


# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
cathedral <- st_transform(cathedral, crs = 4326)

#...........................................................
# basemap ----
#...........................................................

library(geodata)
library(terra)
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


#...........................................................
# foippa fire data ----
#...........................................................


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

save(FOIPPA, file = "./data/fire/foippa.rda")



#...................................................
# firms cropped fire data ----
#...................................................

modis <- st_read("data/fire/firms/modis_cropped/modis_cropped.shp")
viirs_suomi <- st_read("data/fire/firms/viirs_suomi_cropped/viirs_suomi_cropped.shp")
viirs_noaa <- st_read("data/fire/firms/viirs_noaa_cropped/viirs_noaa_cropped.shp")
firms <- rbind(modis, viirs_suomi)
firms <- rbind(firms, viirs_noaa)
# match the crs
firms <- st_transform(firms, crs = 4326)


# data carpentry
# format time from HHMM into to HH:MM:SS
firms$ACQ_TIME <- format(strptime(firms$ACQ_TIME, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
firms$timestamp <- as.POSIXct(paste(firms$ACQ_DATE, firms$ACQ_TIME), format = "%Y-%m-%d %H:%M:%S")
names(firms)[names(firms) == "ACQ_DATE"] <- "date"
names(firms)[names(firms) == "ACQ_TIME"] <- "time"


save(firms, file = './data/fire/firms.rda')



#............................................................................
# CWFIS ----
#............................................................................


# https://cwfis.cfs.nrcan.gc.ca/downloads/hotspots/archive/ for the perimeters
# 2023_progression

# check out the datamart?

CWFIS <- st_read("./data/fire/CWFIS/2023_progression/m3_progression_2023.shp") #13238 items
CWFIS$date <- as.Date(as.character(CWFIS$date), format = "%Y%m%d")

# CWFIS <- CWFIS[format(CWFIS$date, "%m") == "08",]
CWFIS <- st_transform(CWFIS, crs = 4326)

#combine polygons
aoi <- c(st_geometry(cathedral), st_geometry(FOIPPA), st_geometry(firms))
# get bbox of all the polygons
aoi_bbox <- st_bbox(aoi)

CWFIS <- st_crop(CWFIS, aoi_bbox) #63 items in the area
# sort by date
CWFIS <- CWFIS[order(CWFIS$date),]


save(CWFIS, file = './data/fire/cwfis.rda')


plot(ok)
plot(cathedral[17], add = TRUE, col = NA)
plot(CWFIS[6], add = TRUE)
plot(FOIPPA[9], add = TRUE)








#///////////////////////////////////////////////////////////////////////



library(maptiles)
library(ggplot2)
library(gganimate)
library(tidyterra)
basemaptile <- get_tiles(aoi, provider = "OpenStreetMap", zoom = 13)


p <-
ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = CWFIS, color = "red", fill = NA, lwd = 0.7) +
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

canada <- gadm(country="Canada", level=2, path = tempdir())
bc <- canada[canada$NAME_1 %in% "British Columbia", ]
unique(bc$NAME_2)
# region <- bc[bc$NAME_2 == "Central Okanagan",]
region <- bc[bc$NAME_2 %in% "Central Okanagan", ]
region <- project(region, "EPSG:4326")
plot(region)


# trying to get the mcdougall fire perimeters

CWFIS <- st_read("C:/Users/achhen/Downloads/2023_progression/m3_progression_2023.shp")
CWFIS$date <- as.Date(as.character(CWFIS$date), format = "%Y%m%d")
# CWFIS <- CWFIS[format(CWFIS$date, "%m") == "08",]
CWFIS <- st_transform(CWFIS, crs = 4326)
CWFIS <- st_crop(CWFIS, region)

# CWFIS <- CWFIS[order(CWFIS$date),]

plot(region)
plot(CWFIS[6], add = TRUE)

basemaptile <- get_tiles(st_bbox(region), 
                         project = TRUE,
                         provider = "OpenStreetMap", zoom = 13)

plot(basemaptile)


p <-
  ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = CWFIS, color = "red", fill = NA, lwd = 0.7) +
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

anim_save("figures/map/mcdougall_fire.gif", anim)





library(leaflet)
library(mapview)
m <-
leaflet(data = CWFIS) %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  addPolylines(data = CWFIS, color = "red",  # for outline, for fill use addPolygons()
               # dashArray = "9,9",  # dashed outline
               stroke = 1, opacity = 0.5) %>% 
addMiniMap(width = 150, height = 150)

mapshot(m, file = "figures/map/mcdougall_fire.png")


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

CWFIS <- st_read("C:/Users/achhen/Downloads/2023_progression/m3_progression_2023.shp")
CWFIS$date <- as.Date(as.character(CWFIS$date), format = "%Y%m%d")
# CWFIS <- CWFIS[format(CWFIS$date, "%m") == "08",]
CWFIS <- st_transform(CWFIS, crs = 4326)
CWFIS <- st_crop(CWFIS, region)

# CWFIS <- CWFIS[order(CWFIS$date),]

plot(region)
plot(CWFIS[6], add = TRUE)

basemaptile <- get_tiles(st_bbox(region), 
                         project = TRUE,
                         provider = "OpenStreetMap", zoom = 13)

plot(basemaptile)


p <-
  ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  geom_sf(data = CWFIS, color = "red", fill = NA, lwd = 0.7) +
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


