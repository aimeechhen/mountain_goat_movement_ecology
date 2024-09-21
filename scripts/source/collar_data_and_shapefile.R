

library(sf)

# source('./scripts/collar_data_and_shapefile.r')

# import collar data
load("data/collar_data/collar_data_20240703.rda")
# convert data into sf object
collar_data_sf <- st_as_sf(collar_data, coords = c('longitude', 'latitude'))
# set crs as lat/long system
st_crs(collar_data_sf) <- 4326
