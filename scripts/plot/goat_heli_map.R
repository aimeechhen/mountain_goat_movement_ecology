


#Map the data point locations

#import collar data
gps_data <- read.csv("data/goat-data.csv")

library(dplyr)        # for data wrangling
library(canadianmaps) # to download a shapefile of BC
library(elevatr)      # to download digital elevation models
library(purrr)        # for functional programming (map_***(), etc.)
library(sp)           # for spatial data, SpatialPoints()
library(sf)           # for spatial data
library(terra)        # for raster data
library(progress)     # for elevation, get_elev_raster()
#library(ggplot2)

# #import a shapefile of British Columbia
bc_shape <- st_as_sf(PROV) %>%  # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

#subset the coordinates
gps_coordinates <- gps_data[,6:7]

#convert coordinates to spatial data points
gps_sf <- SpatialPoints(select(gps_coordinates, location.long, location.lat))

#do not load the the ctmm package for this or you will run into errors
ctmm::projection(gps_sf) <- '+proj=longlat' 

#plot shapefile with fixes
plot(bc_shape)
sp::plot(gps_sf, add = TRUE, col = 'red', pch = 19)
        # , pch = 19, cex = 0.5) 

#.......................................

#Looks like some data points are in the USA

library(geodata)
#download shapefile
#level 0 = country; level 1 = province/state; level 2 = counties
provinces <- gadm(country="Canada", level=1, path = "data/geodata/Canada")
states <- gadm(country="USA", level=1, path = "data/geodata/USA")

#plot both shape files, layered, then layer on the data points
plot(provinces)
plot(states, add = TRUE)
sp::plot(goat_data_sf, add = TRUE, col = 'red', pch = 19, cex = 0.5) 

saveRDS(states, file = "rds/shapefile_USA.rds")
saveRDS(provinces, file = "rds/shapefile_CAN.rds")

#plot shapefile with fixes
plot(bc_shape)
sp::plot(gps_location, add = TRUE, col = 'red', pch = 19)
points(x = data_helicopter$location.long,
       y = data_helicopter$location.lat,
       col = "blue")

