
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
goat_data$goat_color <- goat_palette[match(goat_data$goat_name, goats)]



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
goat_data$goat_color <- goat_palette[match(goat_data$goat_name, goats)]
# collar_data$colours <- goat_palette[match(collar_data$goat_name, goats)]

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



#..............................................................
# moveVis ----
#..............................................................

# devtools::install_github("16EAGLE/moveVis")
library(moveVis)
library(move2)
library(tictoc)

# load("data/collar_data/collar_data_20241123.rda")
# collar_data <- collar_data[collar_data$year == 2023,]

goat_data <- readRDS("./data/collar_data/full_combined_data_20250309.rds")
# goat_data <- goat_data[goat_data$year == 2024,]
goat_data <- goat_data[goat_data$year == 2023,]
goat_data <- goat_data[,-c(7:11)] # "t", "HDOP", "x", "y", "VAR.xy"
goat_data <- goat_data[complete.cases(goat_data),]

# Convert dataframe collar data into a move object
# move_data <- df2move(goat_data, 
move_data <- df2move(collar_data,                      
                     proj = "epsg:3857",
                     x = "longitude", y = "latitude", time = "timestamp",
                     track_id = "goat_name")

# move_data <- sp::spTransform(move_data, crs("+init=epsg:4326"))
# move_data <- st_transform(move_data, crs = st_crs("epsg:4326"), aoi = aoi_bbox)
# move_data <- sp::spTransform(move_data, crs("+init=epsg:3857"))
ext <- extent(-120.3819 , -119.9626 , 48.93508 , 49.20846)

# check the crs of the move_data object
st_crs(move_data)

# look at the data points
# view_spatial(move_data)
# subset_move_data <- subset_move(move_data, from = "2023-07-22 00:00:00", to = "2023-10-27 00:00:00")

# align move_data to a uniform time scale (may take a while)
m <- align_move(move_data, res = 240, unit = "mins") # lower the res = , longer it takes
# m <- align_move(subset_move_data, res = 120, unit = "mins") # lower the res = , longer it takes
# st_crs(m)

# #You can also use a custom extent (and use the pipe to customize frames):
# ext <- extent(-120.3819 , -119.9626 , 48.93508 , 49.20846 )
# # changing extent doesnt work....

# use_multicore() n_cores =

# create spatial frames with a OpenStreetMap watercolour map (basemap is used by default)
tic()
frames <-
  frames_spatial(m, 
                 # path_colours = goat_palette, 
                 path_colours = NA, 
                 # tail_colour = "white", tail_size = 0.8, #how to include tail
                 # ext = ext, 
                 equidistant = F) %>%
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress() #%>% 

frames[[50]]

# let's draw a polygon on frames:
# cannot expand the map...even tried modifying the extent, doesnt change??
# add_gg(gg = expr(
# geom_sf(data = cathedral, color = "black", fill = NA, linewidth = 1, linetype = "dashed")), data = cathedral) %>% 
# add_gg(gg = expr(
#   geom_path(aes(x = x, y = y), data = cathedral_df,
#             colour = "black", linetype = "dashed", linewidth = 1.5)), data = cathedral_df) %>%
# add_gg(expr(theme(legend.title = element_blank()))) %>% 
# add_gg(expr(guides(linetype = "none"))) %>% 
# add_text("Cathedral Provincial Park Boundary", 
#          x = -120.07, y = 49.003,
#          colour = "black", size =2)



# add_gg(gg = expr(
#   geom_path(aes(x = x, y = y), data = foippa_df,
#             colour = "red", linetype = "solid", linewidth = 1.5)), data = foippa_df)
# add_text("---", 
#          x = -120.06, y = 49.005,
#          colour = "black", size = 3)

toc()
# 1162 frames, target = 30s clip

frames[[100]] +# preview one of the frames, e.g. the 100th frame
     geom_path(aes(x = x, y = y), data = foippa_df,
              colour = "red", linetype = "solid", linewidth = 1.5) # test additional layers

# set to 30s clip
# Calculate FPS for a 30-sec animation
total_frames <- length(frames)
calculated_fps <- total_frames / 30  # Adjust fps dynamically

tic()
animate_frames(frames, out_file = tempfile(fileext = ".mp4"), fps = calculated_fps, overwrite = TRUE)
toc()

# animate frames (may take a while, like hours, for a lot of data)
tic()
animate_frames(frames, out_file = "figures/moveVis_goats_2024.mp4", 
               fps = 40, overwrite = TRUE)
# Approx animation duration = ~750.25s (~12.5 min) @ 25 fps for 18757 frames
toc()



#..............................................................
# gganimate ----
#..............................................................

library(maptiles)
library(tidyterra)
library(gganimate)
library(sf)

# cathedral ----
# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
# # Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")
st_crs(cathedral) <- 4326


goat_data <- readRDS("./data/collar_data/full_combined_data_20250309.rds")
# goat_data <- goat_data[goat_data$year == 2024,]
goat_data <- goat_data[goat_data$year == 2023,]
goat_data <- goat_data[,-c(7:11)]
goat_data <- goat_data[complete.cases(goat_data),]

# for fire period only
# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"
#subset goat data based on the date range of the crater creek wildfire across all years
goat_data <- goat_data[goat_data$month_day >= fire_start & goat_data$month_day <= fire_end, ] # 13884 fixes


# goat_data_sf <- st_as_sf(goat_data, coords = c("longitude", "latitude"), crs = 4326)
# bbox <- st_bbox(cathedral)

hull <- goat_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_convex_hull()
bbox <- st_bbox(st_union(cathedral[17], hull))


basemaptile <- get_tiles(bbox, provider = "OpenStreetMap", zoom = 13)


p <-
ggplot() +
  geom_spatraster_rgb(data = basemaptile) +
  # geom_sf(data = cathedral, aes(color = "Cathedral Provincial Park"), fill = NA, linewidth = 1) +
  # geom_sf(data = FOIPPA, color = "red", fill = NA, lwd = 0.7) +
  geom_point(data = goat_data, aes(x = longitude, y = latitude, color = goat_color), size = 3) +
  scale_colour_manual(values = goat_palette,
                      labels=c('Goatzilla',
                               'Selena Goatmez',
                               'The Goatmother',
                               'Goatileo',
                               'Toats McGoats',
                               'Vincent Van Goat')) +
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

