# This script is obtaining environment data

library(elevatr) 
library(sf)
library(terra)

# use all their gps data for raster size for the 6 study goats
load("./data/goat/goat_data_no_outliers.rda") # 6 goats all data
goat_data <- goat_data[goat_data$within_window == 1,] # 13071
# convert to sf object
goat_data_sf <- st_as_sf(goat_data, coords = c("longitude", "latitude"), crs = 4326)
# convert to bc albers crs
goat_data_sf <- st_transform(goat_data_sf, crs = 3005)


# plot(vect(fire_perimeter))
# plot(ext(vect(st_buffer(st_as_sfc(st_bbox(goat_data_sf)), 5000))), add = TRUE)
# # plot(ext(vect(goat_data_sf)), add = TRUE, border = "green")
# plot(vect(goat_data_sf), add = TRUE)
# 
# goat_ext <- ext(vect(st_buffer(st_as_sfc(st_bbox(goat_data_sf)), 3000)))
# # 1416677.21593339, 1441627.90078138, 453541.588541201, 474060.471117106 (xmin, xmax, ymin, ymax)
# fire_ext <- ext(vect(fire_perimeter))
# # SpatExtent : 1422293.00857, 1452498.5073, 455651.5322, 488730.6013925 (xmin, xmax, ymin, ymax)
# 
# # left (goat)   xmin = 1416677.21593339 
# # right (fire)  xmax = 1452498.5073 
# # bottom (goat) ymin = 453541.588541201 
# # top (fire)    ymax = 488730.6013925
# 
# combined_ext <- ext(xmin(goat_ext),  
#                     xmax(fire_ext),  
#                     ymin(goat_ext), 
#                     ymax(fire_ext)) + 500
# 
# plot(combined_ext)
# plot(vect(fire_perimeter), add = TRUE)
# plot(ext(vect(st_buffer(st_as_sfc(st_bbox(goat_data_sf)), 3000))), add = TRUE)
# plot(vect(goat_data_sf), add = TRUE)
# 

#.....................................................................
# Download elevation data
#.....................................................................

# ryan's original elev raster elev_25m sent back in 2023
# originally the USGS is in WGS84, so decimal degrees. Transforming it to a metric projection (BC Albers) it shows as 17x32m resolution. I would have resampled to 25x25 to match any other raster inputs, like landcover.

# download dem data
# buffer of 5km around the goat data as the bounding box i.e., raster size
dem <- get_elev_raster(locations = goat_data_sf, prj = st_crs(goat_data_sf),
                       # dem <- get_elev_raster(locations = study_area_sf,
                       z = 11, # 14 = ~10m; 13 = ~20m; 12 = ~30m; 11 = ~50m (based on latitude)
                       # clip = "locations", # clipping to region instead of bounding box or tile
                       clip = "bbox", # clipping to the bounding box of the data
                       expand = 8000) # adding a buffer around goat data = raster size to get (20km buffer)

# combined_ext <- st_as_sf(vect(combined_ext)) # turn into a spatvector
# st_crs(combined_ext) <- "epsg:3005" 
# dem <- get_elev_raster(locations = combined_ext, 
#                        prj = st_crs(goat_data_sf),
#                        z = 11, # 14 = ~10m; 13 = ~20m; 12 = ~30m; 11 = ~50m (based on latitude)
#                        clip = "locations")

# renaming layer name so when importing it, its correct and explicit
names(dem) <- "elev_25m" 

elev <- rast(dem)
elev <- project(elev, "epsg:3005")

plot(elev)

writeRaster(elev, "./data/environment/elev_25m.tif", overwrite = TRUE) # z = 11, expand = 5000
writeRaster(elev, "./data/environment/elev_25m_enlargened.tif", overwrite = TRUE) # z = 11, expand = 8000
writeRaster(elev, "./data/environment/elev_25m_manual_ext.tif", overwrite = TRUE) # z = 11


elev <- rast("./data/environment/elev_25m.tif")
elev2 <- rast("./data/environment/elev_25m_manual_ext.tif")



#.....................................................................
# Slope data
#.....................................................................

# import dem data
elev <- rast("./data/environment/elev_25m.tif")
plot(elev)
# calculate slope of the terrain (in degrees) based off of dem
slope <- terrain(elev, v = "slope", unit = "degrees")
plot(slope)
# set to numerical categories (i.e, set 50+ degree slopes as 1 and below 50 as 0)
# slopes over 50 = a proxy-ish for escape terrain, but does not account for ruggedness
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)
# renaming layer name, so its properly indicated and explicit and not shown as "slope", for reference to avoid confusion
names(slope50) <- "slope50_25m" 
varnames(slope50) <- "slope50_25m" 



#.............................

# Distance to escape terrain as per Ryan:
# I opened 'escape' in QGIS and calculated a proximity raster. There's a tool in terra that does this, but 
# I haven't had much luck with it.
# distance to escape was made by first by creating a raster of 0/1 of whether a pixel had a slope greater or less than a cutoff value (like 45°, or something like that). Then a proximity raster was created where each pixel represented a distance to those 1's (> 45°)
# I can't remember how I derived the other one, whether it was some proximity function in terra, or in QGIS. Probably in QGIS since it's very easy.



# calculate distance to slopes that are over 50 degree
dist_to_slope50 <- terra::distance(slope50, target = 1) # only calculate for cells = 1
dist_to_slope50

plot(dist_to_slope50)
# renaming layer name, so its properly indicated and explicit, for reference to avoid confusion
names(dist_to_slope50) <- "dist_to_slope50_25m"
varnames(dist_to_slope50) <- "dist_to_slope50_25m"

writeRaster(dist_to_slope50, file = "./data/environment/dist_to_slope50_25m.tif", overwrite = TRUE) # expand = 5000
writeRaster(dist_to_slope50, file = "./data/environment/dist_to_slope50_25m_enlargened.tif", overwrite = TRUE) # expand = 8000

rm(dem, slope, slope50)




#.............................
# check environment data

elev <- rast("./data/environment/elev_25m.tif")
dist_to_slope50 <- rast("./data/environment/dist_to_slope50_25m.tif")
dist_to_fire <- rast("./data/environment/dist_to_fire_25m.tif")


plot(elev)
plot(dist_to_slope50)
plot(dist_to_fire)


plot(ext(elev2))
plot(ext(dist_to_fire), add = TRUE, col = "red")
plot(ext(elev), add = TRUE, col = "green")
plot(st_geometry(goat_data_sf), add = TRUE, col = "black")
plot(ext(dist_to_fire[[1]]), add = TRUE, col = "blue")



#..................................................................
# 50m sensitivity analysis i.e., other resolution ----

elev <- rast("./data/environment/elev_25m.tif")

# as per dr. noonan
# test <- rast(dem)
# test <- project(test, "EPSG:3005")
plot(elev)
res(elev)
elev_50m <- terra::aggregate(elev, 2) # made it more coarse
res(elev_50m)
# [1] 50.19045 50.19045
# theres your ~50m resolution
writeRaster(elev_50m, "./data/environment/elev_50m.tif", overwrite = TRUE) # z = 11

# calculate slope of the terrain (in degrees) based off of dem
slope <- terrain(elev_50m, v = "slope", unit = "degrees")
plot(slope)
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)
names(slope50) <- "slope50_50m" 
varnames(slope50) <- "slope50_50m" 
# calculate distance to slopes that are over 50 degree
dist_to_slope50 <- terra::distance(slope50, target = 1) # only calculate for cells = 1
dist_to_slope50
plot(dist_to_slope50)
# renaming layer name, so its properly indicated and explicit, for reference to avoid confusion
names(dist_to_slope50) <- "dist_to_slope50_50m"
varnames(dist_to_slope50) <- "dist_to_slope50_50m"

writeRaster(dist_to_slope50, file = "./data/environment/dist_to_slope50_50m.tif", overwrite = TRUE)




#.................................................
elev <- rast("./data/environment/elev_25m.tif")

# as per dr. noonan
# test <- rast(dem)
# test <- project(test, "EPSG:3005")
plot(elev)
res(elev)
elev_75m <- terra::aggregate(elev, 3) # made it more coarse
res(elev_75m)
# [1] 75.27645 75.27645
# theres your ~50m resolution
plot(elev_75m)
writeRaster(elev_75m, "./data/environment/elev_75m.tif", overwrite = TRUE) # z = 11

# calculate slope of the terrain (in degrees) based off of dem
slope <- terrain(elev_75m, v = "slope", unit = "degrees")
plot(slope)
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)
names(slope50) <- "slope50_75m" 
varnames(slope50) <- "slope50_75m" 
# calculate distance to slopes that are over 50 degree
dist_to_slope50 <- terra::distance(slope50, target = 1) # only calculate for cells = 1
dist_to_slope50
plot(dist_to_slope50)
# renaming layer name, so its properly indicated and explicit, for reference to avoid confusion
names(dist_to_slope50) <- "dist_to_slope50_75m"
varnames(dist_to_slope50) <- "dist_to_slope50_750m"

writeRaster(dist_to_slope50, file = "./data/environment/dist_to_slope50_75m.tif", overwrite = TRUE)


#.................................................
elev <- rast("./data/environment/elev_25m.tif")

# as per dr. noonan
# test <- rast(dem)
# test <- project(test, "EPSG:3005")
plot(elev)
res(elev)
elev_100m <- terra::aggregate(elev, 4) # made it more coarse
res(elev_100m)
# [1] 100.3686 100.3686
# theres your ~50m resolution
plot(elev_100m)
writeRaster(elev_100m, "./data/environment/elev_100m.tif", overwrite = TRUE) # z = 11

# calculate slope of the terrain (in degrees) based off of dem
slope <- terrain(elev_100m, v = "slope", unit = "degrees")
plot(slope)
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)
names(slope50) <- "slope50_100m" 
varnames(slope50) <- "slope50_100m" 
# calculate distance to slopes that are over 50 degree
dist_to_slope50 <- terra::distance(slope50, target = 1) # only calculate for cells = 1
dist_to_slope50
plot(dist_to_slope50)
# renaming layer name, so its properly indicated and explicit, for reference to avoid confusion
names(dist_to_slope50) <- "dist_to_slope50_100m"
varnames(dist_to_slope50) <- "dist_to_slope50_100m"

writeRaster(dist_to_slope50, file = "./data/environment/dist_to_slope50_100m.tif", overwrite = TRUE)
