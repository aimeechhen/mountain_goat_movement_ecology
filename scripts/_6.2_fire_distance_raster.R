

# calculate distance to fire perimeter and create a distance to fire raster

library(sf)
library(lubridate)
library(terra)



# # import foippa and cwfis fire perimeters
load("./data/fire/fire_perimeter_by_date.rda")
fire_by_date
fire_perimeter <- fire_by_date

bc_albers <- "epsg:3005"

# set spatial properties of the raster by defining the extent and crs as a template for the raster
ext_value <- ext(fire_perimeter)
crs_value <- bc_albers
res_value <- 50 # in meters

# create a template for the spatial properties of the raster   
raster_template = rast(ext_value,
                       crs = crs_value,
                       res = res_value)



# calculate distance to fire perimeter and create a distance to fire raster
r_list <- list()

# this will take time to run depending on how many perimeters you have
for (i in 1:nrow(fire_perimeter)) {
  message("perimeter ", i)
  # extract a perimeter
  perimeter <- fire_perimeter[i,]
  # used the raster template
  r <- raster_template
  # convert vector object into a raster
  r <- terra::rasterize(perimeter, r, field = 1) # set field as 1?
  # calculate the euclidean distance in meters (from each NA cell to nearest non-NA cell, i.e, inside permeter = perimeter cell(value of 1 ) ) 
  fire_distance <- distance(r)
  #store in a list
  r_list[[i]] <- fire_distance
}

# convert the list of rasters into a raster stack (i.e. as a single raster)
rstack <- rast(r_list)

# name all the layers in the raster stack & format to ensure the 00:00:00 shows up in the name
names(rstack) <- fire_perimeter$date

writeRaster(rstack, "./data/fire/fire_distance.tif")


