
library(ggplot2)
library(sf)
library(lubridate)
library(leaflet)
library(mapview)




# Import combined collar data (original + new) ----
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)




#............................................................
# Plot
#............................................................



goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") 
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# create a goat colour column and assign the proper colour based on the goat, in collar_id order
goat_data$goat_colour <- goat_palette[match(goat_data$goat_name, goats)]

# goat data only using goat name in legend
# m <-
leaflet(data = goat_data) %>%
  # # add different provider tiles and a layers control so that users can switch between the different
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  # setView(lng = -120.1761, lat = 49.09, zoom = 11) %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = ~goat_colour,
             fillOpacity = 0.9) %>%  # colour by goat
  addLegend("topleft", 
            pal = colorFactor(goat_palette, goats),
            values = ~goat_name,
            title = "Mountain Goats") %>%  
  addMiniMap(width = 150, height = 150,,
             position = "bottomleft")


mapshot(m, file = "figures/leaflet_map_goat_data.png")








#////////////////////////////////////////////////////////////////////
# map goat data and fire data ----
#////////////////////////////////////////////////////////////////////

goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") 
collar_id <- c("30548", "30561", "30575", "30613", "30642", "30648")
goat_labels <- c("Goatzilla", "Selena Goatmez", "The Goatmother", "Goatileo", "Toats Mcgoats", "Vincent Van Goat")
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# add a goat colour column and assign them the proper colour
goat_data$goat_colour <- goat_palette[match(goat_data$goat_name, goats)]



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


# colourblind friendly palette
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
red_gradient_palette
FOIPPA$map_colour <- red_gradient_palette



# # using goat names in legend
# m2 <-
#   leaflet(data = goat_data %>% 
#             # modify the legend text by modifying the items in the df in leaflet() only
#             dplyr::mutate(goat_name = case_when(goat_name == "goatzilla" ~ "Goatzilla", 
#                                                 goat_name == "selena_goatmez" ~ "Selena Goatmez", 
#                                                 goat_name == "the_goatmother" ~ "The Goatmother", 
#                                                 goat_name == "goatileo" ~ "Goatileo", 
#                                                 goat_name == "toats_mcgoats" ~ "Toats Mcgoats", 
#                                                 goat_name == "vincent_van_goat" ~ "Vincent Van Goat"))) %>% 
#   # # add different provider tiles and a layers control so that users can switch between the different
#   addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
#   setView(lng = -120.1, lat = 49.11, zoom = 11) %>%
#   addCircles(lng = ~longitude, lat = ~latitude,
#              radius = 2,  # Circle radius in meters
#              color = ~goat_colour,
#              fillOpacity = 0.9) %>%  # colour by goat
#   addPolygons(data = FOIPPA, color = ~red_gradient_palette, 
#               fillOpacity = 0, weight = 5) %>%
#   addLegend("topleft", 
#             pal = colorFactor(goat_palette, goat_labels),
#             values = ~goat_name,
#             title = "Mountain Goats") %>%  
#   addMiniMap(width = 150, height = 150,
#              position = "bottomleft")

# mapshot(m2, file = "figures/leaflet_map_goat_data_foippa.png")




#..................................................................
# using collar id in legend ----
#..................................................................



goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") 
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# add a goat colour column and assign them the proper colour
goat_data$goat_colour <- goat_palette[match(goat_data$goat_name, goats)]


m3 <-
leaflet(data = goat_data) %>% 
  # # add different provider tiles and a layers control so that users can switch between the different
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  setView(lng = -120.1, lat = 49.11, zoom = 11) %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = goat_data$goat_colour,
             fillOpacity = 0.9) %>%  # colour by goat
  addPolygons(data = FOIPPA, color = ~red_gradient_palette, 
              fillOpacity = 0, weight = 5) %>%
  addLegend("topleft", 
            pal = colorFactor(goat_data$goat_colour, domain = goat_data$collar_id), # uses the colour column for the colour, and matches it with the collar_id for the legend
            values = goat_data$collar_id,
            title = "Mountain Goats") %>%  
  addLegend("topleft", 
            pal = colorFactor(FOIPPA$map_colour, domain = FOIPPA$date), # uses the palette object for the colour, and matches it with the date for the legend
            values = FOIPPA$date,
            title = "Fire perimeter") %>%  
  addMiniMap(width = 150, height = 150,
             position = "bottomleft")

mapshot(m3, file = "figures/living_labs/figure1.png")



#/////////////////////////////////////////////////////////
# ggplot plotting + maptiles package, use leaflet instead ----

# 
# library(sf)
# library(terra)
# library(tidyterra)
# library(maptiles)


# cathedral
# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
# # Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")
st_crs(cathedral) <- 4326


goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )  # colours correspond with the goat order above


# 
# # get extent of cathedral
# bbox <- st_bbox(cathedral)
# 
# # get base map
# basemaptile <- get_tiles(bbox, provider = "OpenStreetMap", zoom = 13)
# # crs(basemaptile)
# # ext(basemaptile)
# 
# 
# ggplot() +
#   geom_spatraster_rgb(data = basemaptile) +
#   geom_sf(data = cathedral, aes(color = "Cathedral Provincial Park"), fill = NA, linewidth = 1, alpha = 0.3) +
#   geom_point(data = goat_data, aes(x = longitude, y = latitude, color = goat_name), size = 2, alpha = 0.2) +
#   scale_colour_manual(values = goat_palette,
#                       labels=c('Goatzilla',
#                                'Selena Goatmez',
#                                'The Goatmother',
#                                'Goatileo',
#                                'Toats McGoats',
#                                'Vincent Van Goat',
#                                'Cathedral Provincial Park')) +
#   theme_void() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.title = element_blank(),
#         legend.frame = element_blank())
# 
# ggsave(last_plot(), filename = "./figures/map_goat_data.png", 
#        bg = "transparent",
#        width = 6, height = 6.23, units = "in", dpi = 600)


#....................................................................
# adding foippa polygons
# use EPSG:3857 to get sharper basemap
# goat_data_sf <- st_as_sf(goat_data, coords = c("longitude", "latitude"), crs = 4326)
# goat_data_sf <- st_transform(goat_data_sf, crs = 3857)
# cathedral <- st_transform(cathedral, crs = 3857)
# 
# bbox2 <- st_bbox(c(
#   xmin = -120.33383,
#   ymin = 48.95037,
#   xmax = -119.81255,
#   ymax = 49.25434
# ), crs = st_crs(cathedral))
# basemaptile2 <- get_tiles(bbox2, provider = "OpenStreetMap", zoom = 12)
# ggplot() +
#   geom_spatraster_rgb(data = basemaptile2) +
#   geom_sf(data = cathedral, aes(color = "Cathedral Provincial Park"), fill = NA, linewidth = 1, alpha = 0.3) +
#   geom_point(data = goat_data, aes(x = longitude, y = latitude, color = goat_name), size = 2, alpha = 0.2) +
#   # geom_sf(data = goat_data_sf, aes(color = goat_name), size = 2, alpha = 0.2) +
#   geom_sf(data = FOIPPA, color = red_gradient_palette, fill = NA, linewidth = 1.2)  +
#   scale_colour_manual(values = goat_palette,
#                       labels=c('Goatzilla',
#                                'Selena Goatmez',
#                                'The Goatmother',
#                                'Goatileo',
#                                'Toats McGoats',
#                                'Vincent Van Goat',
#                                'Cathedral Provincial Park')) +
#   theme_void() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "top",
#         legend.justification = "center",
#         legend.direction = "horizontal",
#         legend.background = element_rect(fill = "white")) 

