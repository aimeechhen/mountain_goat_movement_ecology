

# Map


library(rgee)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(stringr)

library(tictoc)
library(crayon)

rgee::ee_Initialize(user = 'katchhen@gmail.com', drive = TRUE, gcs = FALSE,
                    credentials = "persistent")


rm(list = ls())


#check the if area of interest is correct wrt to cathedral park
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
bc_parks <- st_transform(bc_parks, crs = st_crs(4326))
#subset to cathedral park
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95
# Convert crs from BC Albers to WGS84 Geographic projection (i.e. "latitude/longitude projection")
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")

# convert sf to gee object
cathedral_gee <- sf_as_ee(cathedral$geometry)

#___________________________________________________

# BASED ON THE CODE HERE: https://medium.com/google-earth/how-to-generate-wildfire-boundary-maps-with-earth-engine-b38eadc97a38

#....................................................................
# Area of interest (aoi) ----
#....................................................................

# center point (centroid) of the collar data
longitude = -120.17
latitude = 49.05
# Define the radius around the center point
radius_of_interest_meters <- 30000
# Create a geometry for the area of interest as GEE object, defining the area of interest based on the center of the collar data and radius around it
area_of_interest = ee$Geometry$Point(c(longitude, latitude))$
  buffer(radius_of_interest_meters)

Map$centerObject(eeObject = area_of_interest,
                 zoom = 12)
# m0 <-
#   Map$addLayer(
#     eeObject = area_of_interest,
#     visParams = list(color = 'green'),
#     name = 'm0 Area of interest',
#     shown = TRUE,
#     opacity = 0.2)

# m00 <-
Map$addLayer(
  eeObject = cathedral_gee,
  visParams = list(
    color = 'white',        # Outline color
    # fillColor = 'transparent', # Transparent fill
    strokeWidth = 2         # Outline thickness
  ),
  name = 'm00 cathedral',
  shown = TRUE,
  opacity = 0.7
)

m0 + m00




#_____________________________________________

library(leaflet)
library(dplyr)


longitude <- -120.17
latitude <- 49.05

m <- leaflet()
m <- setView(m, lng=longitude, lat=latitude, zoom = 12)
m <- addTiles(m)
m


longitude <- -120.17
latitude <- 49.05
m <- leaflet() %>%
  setView(lng=longitude, lat=latitude, zoom = 12) %>% 
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Stamen.Toner",
    group = "Stamen.Toner"
  ) %>%
  addProviderTiles(
    "Stamen.Terrain",
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "Wikimedia",
    group = "Wikimedia"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft",
    layersControlOptions(collapsed = FALSE)
  )

m


longitude <- -120.17
latitude <- 49.05

m <-
leaflet() %>%
  addTiles() %>%
  setView(lng = -120.17, lat = 49.05, zoom = 11) %>%
  # addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolylines(data = cathedral, color = "red",  
               # dashArray = "9,9", 
               stroke = 1, opacity = 0.5) %>%
  addMiniMap(width = 150, height = 150)

# save as a static plot
library(mapview)
# webshot::install_phantomjs()
mapshot(m, file = "figures/leaflet_map_default.png")



leaflet() %>%
  addTiles(group = "StreetMap") %>%
  addProviderTiles(providers$Stamen.Watercolor, group = "Stamen") %>%
  setView(lng = longitude, lat = latitude, zoom = 12) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>% 
addPolygons(data = cathedral, color = "white", stroke = 1, opacity = 0.8) %>% 
  addLayersControl(baseGroups = c("Stamen", "Esri.WorldImagery"),
                   position = "bottomleft")

