
library(leaflet)
library(RColorBrewer) 
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 6, name = "Set2")

goat_palette <- brewer.pal(n = 6, name = "Set2")
goat_palette <- brewer.pal(n = 6, name = "Dark2")
goat_palette
goat_palette <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F") #manual
goat_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A" ,"#66A61E", "#E6AB02") #manual



library(scales)
show_col(goat_palette)

source('./scripts/source/cathedral_park_shapefile.r')
# convert to lat lot
cathedral <- st_transform(cathedral, crs = 4326)

load("data/collar_data/collar_data_20241123.rda")
# identify the goats that were tracked during the wildfire
goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
# subset to fire goats
fire_goats <- collar_data[collar_data$goat_name %in% goats,] # 43941 obs

# Define the period based onCrater Creek wildfire date range (July 22 to October 26)
fire_start <- "07-22"
fire_end <- "10-26"
#subset goat data based on the date range of the crater creek wildfire across all years
fire_goats <- fire_goats[fire_goats$month_day >= fire_start & fire_goats$month_day <= fire_end, ] #10376 obs

fire_goats <- fire_goats[fire_goats$year == "2023", ]
# fire_goats$goat_name <- as.factor(fire_goats$goat_name)

dat_sf <- st_as_sf(fire_goats, coords = c('longitude', 'latitude'))
st_centroid(st_union(dat_sf)) # find the center of the collar data




m <-
leaflet(data = fire_goats) %>%
  # # add different provider tiles and a layers control so that users can switch between the different
  # addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  # addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%
  # addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
  # addProviderTiles("Esri.NatGeoWorldMap", group = "Esri.NatGeoWorldMap") %>%
  # addProviderTiles("USGS.USTopo", group = "USGS.USTopo") %>%
  # # add a layers control
  # addLayersControl(baseGroups = c(
  #   "OpenStreetMap", "OpenTopoMap",
  #   "Esri.WorldTopoMap","Esri.WorldImagery", "Esri.NatGeoWorldMap",
  #     "USGS.USTopo"),
  # position = "topleft") %>%
  setView(lng = -120.1761, lat = 49.03446, zoom = 11) %>%
  addPolylines(data = cathedral, color = "red",  # for outline, for fill use addPolygons()
               # dashArray = "9,9",  # dashed outline
               stroke = 1, opacity = 0.5) %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,  # Circle radius in meters
             color = goat_palette,
             fillOpacity = 0.9) %>%  # colour by goat
  addLegend("topright", 
            pal = colorFactor(goat_palette, unique(fire_goats$goat_name)), 
            values = ~goat_name,
            title = "Mountain Goats") %>%  # Legend for goat color mapping
  addMiniMap(width = 150, height = 150)


#...........................................
library(mapview)
mapshot(m, file = "figures/map/fire_goat_leaflet_map_OpenStreetMap.png")
mapshot(m, file = "figures/map/fire_goat_leaflet_map_Esri_NatGeoWorldMap.png")
mapshot(m, file = "figures/map/fire_goat_leaflet_map_Esri_WorldTopoMap.png")

