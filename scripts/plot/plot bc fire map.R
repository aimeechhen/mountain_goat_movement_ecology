
# bc government fire data

library(bcdata)
library(ggplot2)
library(leaflet)
library(mapview)
library(sf)

# fire info

# historical fires
WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_INCIDENTS_SP <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_INCIDENTS_SP") %>% # search for data
  filter(FIRE_NUMBER == "K52125") %>% # filter based on fire id number
  collect() # download data

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical/resource/6db589c4-e45e-4ae9-a7b5-775cbfec6037")





#polygon
fire_polygon <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP") %>% 
  # filter(FIRE_NUMBER == "K52125") %>% 
  filter(FIRE_YEAR == "2023") %>% 
  collect()
# bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical/resource/c899578d-0738-4166-9b65-0588464f42ee")
bcdc_get_citation("22c7cb44-1463-48f7-8e47-88857f207702")


plot(fire_polygon$geometry)

fire_polygon <- st_transform(fire_polygon, crs = "epsg:4326" )


library(canadianmaps) # to download a shapefile of BC
bc_shape <- st_as_sf(PROV) %>%  # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only
plot(bc_shape)
# centroid of BC lat = 53.7, long -124.6


# load cathedral park shapefile
source('./scripts/source/cathedral_park_shapefile.r')
cathedral <- st_transform(cathedral, "+proj=longlat +datum=WGS84")

m <-
leaflet() %>%
  addTiles() %>%
  setView(lng = -124.6, lat = 53.7, zoom = 5) %>% #  center point of BC
    # addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolygons(data = fire_polygon, 
              color = "#cc3311",       # Border color
              fillColor = "#cc3311",   # Solid fill color
              fillOpacity = 0.8,       # Adjust fill opacity
              weight = 2,              # Border thickness
              opacity = 1) %>%         # Border opacity
  addPolylines(data = bc_shape, 
               color = "black",        # Boundary color
               weight = 0.7,           # boundary line thickness
               opacity = 1)            # Full opacity


# save
mapshot(m, file = "figures/leaflet_2023_bc_fire_map.png")
