
# Fire data

library(lubridate)
library(sf)


# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
st_crs(cathedral) # bc albers, NAD83
cathedral <- st_transform(cathedral, crs = 4326)
# Extract the bounding box of the sf object
cathedral_bbox <- st_bbox(cathedral)

#...........................................................
# basemap ----
#...........................................................

# library(geodata)
# library(terra)
# regions <- gadm(country="Canada", level=2, path = tempdir())
# bc <- regions[regions$NAME_1 %in% "British Columbia", ]
# ok <- bc[bc$NAME_2 == "Okanagan-Similkameen",]
# ok <- project(ok, "EPSG:4326")


library(canadianmaps)
# Download shapefile of Canada
# Convert to spatial features (sf) object
provinces <- st_as_sf(PROV)
# Extract BC
bc_shape <- provinces[provinces$PRENAME == 'British Columbia',]
# Extract boundaries only
bc_shape <- st_geometry(bc_shape)
# Extract the bounding box of the sf object
bc_bbox <- st_bbox(bc_shape)
rm(provinces)


#...........................................................
# foippa fire data ----
#...........................................................


foippa <- st_read('data/fire/bc_gov_foippa/crater_boundaries/23 K52125 Perimeter History Jun17.shp')
foippa$CaptureTim <- format(strptime(foippa$CaptureTim, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
foippa$timestamp <- ymd_hms(paste(foippa$CaptureDat, foippa$CaptureTim))
# colnames(foippa)[colnames(foippa) == 'CaptureDat'] <- 'date'
# colnames(foippa)[colnames(foippa) == 'CaptureTim'] <- 'time'
foippa$date <- foippa$CaptureDat
foippa$time <- foippa$CaptureTim
# foippa$date_group <- as.factor(foippa$date)
st_crs(foippa) #bc albers epsg:3005, NAD83
foippa <- st_transform(foippa, crs = 4326)
st_bbox(foippa)

save(foippa, file = "./data/fire/foippa.rda")
load("./data/fire/foippa.rda")











#...................................................
# firms (already cropped) ----
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



#...............................................................
# goes ----
#...............................................................


# Set folder path where the files are located

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

#clean up environment
rm(folder_path, shp_files, shp_list)

save(goes, file = './data/fire/goes.rda')



#............................................................................
# CWFIS ----
#............................................................................


# https://cwfis.cfs.nrcan.gc.ca/downloads/hotspots/archive/ for the perimeters
# 2023_progression

# check out the datamart?

cwfis <- st_read("./data/fire/CWFIS/2023_progression/m3_progression_2023.shp") #13238 items
cwfis$date <- as.Date(as.character(cwfis$date), format = "%Y%m%d")
st_crs(cwfis) #canada atlas lambert, NAD83
# cwfis <- cwfis[format(cwfis$date, "%m") == "08",]
cwfis <- st_transform(cwfis, crs = 4326)

#combine polygons
# aoi <- c(st_geometry(cathedral), st_geometry(foippa), st_geometry(firms))
aoi <- c(st_geometry(cathedral), st_geometry(foippa))
# get bbox of all the polygons
aoi_bbox <- st_bbox(aoi)

cwfis <- st_crop(cwfis, aoi_bbox) #63 items in the area
# sort by date
cwfis <- cwfis[order(cwfis$date),]


save(cwfis, file = './data/fire/cwfis.rda')
load('./data/fire/cwfis.rda')



# REVIEW THIS SHAPE -> only 1 fire perimeter listed for cathedral
# Read in perimeter data
fire_perimeter <- st_read("data/fire/CWFIS/2023_perimeters/cc_apt_buf.shp")
# Convert crs from NAD83 / Canada Atlas Lambert to WGS84 Geographic projection (i.e. "latitude/longitude projection")
fire_perimeter <- st_transform(fire_perimeter, crs = st_crs(bc_shape))
#Crop the raster to the extent of the BC
bc_fire_perimeter <- st_crop(fire_perimeter, bc_bbox)
#Crop the raster to the extent of the park
park_fire_perimeter <- st_crop(fire_perimeter, cathedral_bbox)

plot(bc_fire_perimeter)
plot(park_fire_perimeter)
library(ggplot2)
ggplot() +
  geom_sf(data = bc_shape, fill = NA, color = "darkgrey", size = 1) +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_sf(data = bc_fire_perimeter, fill = NA, color = "red", size = 1) +
  theme_bw()


ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_sf(data = park_fire_perimeter, fill = NA, color = "red", size = 1) +
  theme_bw()




# plot(ok)
plot(cathedral[17])
#, add = TRUE, col = NA)
plot(cwfis[6], add = TRUE, col = "red")
plot(foippa[2], add = TRUE, col = "blue")




#////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////// ----
#////////////////////////////////////////////////////////////////////////////////////////

# Create a distance to fire raster ----
library(sf)
# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# convert df to sf object
goat_sf <- st_as_sf(goat_data, coords = c("longitude","latitude"))
goat_sf <- st_set_crs(goat_sf, 4326)
goat_sf <- st_transform(goat_sf, crs = 5070)
# add a buffer around the goat data making the area of interest larger
goat_buffered <- st_buffer(goat_sf, dist = 25000)
goat_buffered <- st_transform(goat_buffered, crs = 4326)

load('./data/fire/foippa.rda')
# fire <- foippa
load('./data/fire/cwfis.rda')
# fire <- cwfis
fire <- dplyr::bind_rows(foippa, cwfis)
which(st_is_empty(fire)) # check for empty geometries
ext(fire)
ext(goat_buffered)

# # merge rows with same dates together, not this does not merge polygons together
# merged_date <- aggregate(fire, by = list(fire$date), FUN = function(x) paste(unique(x), collapse = ", "))

# merge the geometries with the same dates, other columns not necessary
# foippa <- aggregate(foippa["geometry"], by = list(date = foippa$date), FUN = st_union)
fire <- aggregate(fire["geometry"], by = list(date = fire$date), FUN = st_union)


r_list <- list()
# Generate a distance to coast raster
for (i in 1:nrow(fire)) {
  message("loop ", i)
  # extract a perimeter
  perimeter <- fire[i,]
  # create an empty spatraster object (template essentially)
  r <- rast(ext = ext(fire))
  # convert vector object into a raster
  r <- terra::rasterize(perimeter, r) # set field as 1?
  # set the extent based on the goat data area but enlargened
  ext(r) <- ext(st_bbox(goat_buffered))
  # calculate the euclidean distance (from each cell to nearest non-NA cell)
  dist_to_fire <- distance(r)
  #store in a list
  r_list[[i]] <- dist_to_fire
}

#convert into a raster stack (as a single raster)
rstack <- rast(r_list)
# name all the layers in the raster stack & format to ensure the 00:00:00 shows up in the name
# names(rstack) <- format(foippa$timestamp, "%Y-%m-%d %H:%M:%S")
names(rstack) <- fire$date

#visualize rasters
plot(rstack[[1]]) # plot a single raster
plot(rstack) # plot all the rasters in a grid

rm(r)

# writeRaster(rstack, "./data/fire/dist_to_fire_raster.tif", overwrite = TRUE)
# dist_to_fire <- rast("./data/fire/dist_to_fire_raster.tif")
writeRaster(rstack, "./data/fire/dist_to_fire_raster_enlarged.tif", overwrite = TRUE)
dist_to_fire <- rast("./data/fire/dist_to_fire_raster_enlarged.tif")




#////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////// ----
#////////////////////////////////////////////////////////////////////////////////////////

# Plot ----

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




#.......................................................................
# bc data  ----

library(bcdata)
# https://catalogue.data.gov.bc.ca/



# https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-current
# https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical
# map of bc wildfire services and regional boundaries
# https://www.for.gov.bc.ca/ftp/HPR/external/!publish/Maps_and_Data/Maps/Organizational_Overview/

## historical fires ----
historical_fires <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_INCIDENTS_SP") %>% # search for data
  # filter(FIRE_NUMBER == "K52125") %>% # filter based on fire id number
  filter(FIRE_YEAR == "2023") %>% 
  collect() # download data

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical/resource/6db589c4-e45e-4ae9-a7b5-775cbfec6037")

#...........................................................
## perimeter area (polygons of the fire) ----

#polygon
fire_polygons <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP") %>% 
  filter(FIRE_NUMBER == "K52125") %>%
  filter(FIRE_YEAR == "2023") %>% 
  collect()

plot(fire_polygons$geometry)

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical/resource/c899578d-0738-4166-9b65-0588464f42ee")


#...........................................................
## fire burn severity map ----
library(bcdata)
burn_severity <- bcdc_query_geodata("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP") %>%
  # filter(FIRE_NUMBER == "K52125") %>%
  # filter(FIRE_YEAR == "2023") %>%
  collect()
plot(burn_severity$geometry)
burn_severity <- burn_severity[burn_severity$BURN_SEVERITY_RATING != "Unburned",] # 5204 -> 4476

burn_severity_all <- burn_severity
burn_severity <- burn_severity_all[burn_severity_all$FIRE_YEAR == 2023,]

library(sf)
# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# convert df to sf object
goat_sf <- st_as_sf(goat_data, coords = c("longitude","latitude"))
goat_sf <- st_set_crs(goat_sf, 4326)
goat_sf <- st_transform(goat_sf, crs = st_crs(burn_severity))
# goat_buffered <- st_buffer(goat_sf, dist = 25000)
# # clip the fire polygons
# severity_clipped <- st_intersection(burn_severity, goat_buffered)
# plot(severity_clipped$geometry)

goat_sf <- goat_sf[goat_sf$year == 2023,]

library(ggplot2)
# colour the burn severity by rating
ggplot() +
  geom_sf(data = burn_severity, aes(fill = BURN_SEVERITY_RATING), alpha = 0.5) +
  # geom_sf(data = goat_sf) +
  scale_fill_manual(values = c("High" = "red",
                               "Medium" = "orange",
                               "Low" = "yellow")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-historical
bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-historical/resource/714536b6-9957-42ca-8c3a-a3f853d807e6")


#...........................................................
## Fuel types ----

fuel_types <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_FUEL_TYPE_SP") %>%
  collect()

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-fuel-types-public/resource/a0d25dd1-e906-4b9f-994e-bf99098621d0")


#...........................................................
## fire zones ----

gov_fire_zones <- bcdc_query_geodata("WHSE_LEGAL_ADMIN_BOUNDARIES.DRP_MOF_FIRE_ZONES_SP") %>%
  collect()

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bec-map/resource/46ceb84a-3f6d-436c-b4c1-c89beb72d11a")