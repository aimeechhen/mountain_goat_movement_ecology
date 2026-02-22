
# This script is obtaining elevation data

library(elevatr) 
library(sf)
library(terra)

# buffer of 5km around the goat data as the bounding box i.e., raster size
load("./data/goat/goat_data_sf.rda")

# The ground elevation is measured at a few select locations, but it is interpolated between them. The result of the interpolation is then rasterized at a set resolution, and the result is the DEM. This process can be quite imprecise [41, 42]. At a cliff, for example, the ground elevation may drop by several hundred meters within a single pixel of the DEM.  (https://doi.org/10.1186/s40317-020-00194-z)

# to determine z value i.e., resolution level, go to -> https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
# Ground resolution per tile pixel varies per zoom level, the given pixel cell's latitude, and input data source.
# This formula generates the following table:
# ground_resolution = (cos(latitude * pi/180) * 2 * pi * 6378137 meters) / (256 * 2^zoom_level pixels)
# latitude is about 48-49, level 11 zoom = ~50m
# latitude <- 49
# zoom_level <- 11
# native resolution
# ground_resolution = (cos(latitude * pi/180) * 2 * pi * 6378137 meters) / (256 * 2^zoom_level pixels) / 30.87 meters per arc second
# (cos(latitude * pi/180) * 2 * pi * 6378137)  / (256 * 2^zoom_level)  / 30.87 # 1.624464 arc seconds/pixel?

# https://www.esri.com/arcgis-blog/products/product/mapping/how-can-you-tell-what-map-scales-are-shown-for-online-maps?rmedium=redirect&rsource=blogs.esri.com/esri/arcgis/2009/03/19/how-can-you-tell-what-map-scales-are-shown-for-online-maps

# check downloaded raster to confirm


#.....................................................................
# Download elevation data
#.....................................................................

# elevation raster (digital elevation model, dem)

# geodata package has resolution/region limitation :()
# bcmaps package cuts off at the CA/US border :(

# download dem data
dem <- get_elev_raster(locations = goat_data_sf, prj = st_crs(goat_data_sf),
                       # dem <- get_elev_raster(locations = study_area_sf,
                       z = 10, # 14 = ~10m; 13 = ~20m; 12 = ~30m; 11 = ~50m (based on latitude)
                       # clip = "locations", # clipping to region instead of bounding box or tile
                       clip = "bbox", # clipping to the bounding box of the data
                       expand = 5000) # adding a buffer around goat data = raster size to get
# Mosaicing & Projecting
# Clipping DEM to bbox
# Note: Elevation units are in meters.

# check resolution scale, for sensitivity analysis, only use one and then aggregate(fun = mean) to coarser resolution, because if you use different z values, you're getting different tiles, so more things are changing rather than just the resolution
dem
# z14 = resolution : 3.13559, 3.13559  (x, y)
# z13 = resolution : 6.270171, 6.270171  (x, y)
# z12 = resolution : 12.54654, 12.54654  (x, y)
# z11 = resolution : 25.09523, 25.09523  (x, y)
# z10 = esolution : 50.24815, 50.24815  (x, y)


# plot raster
plot(dem)
plot(study_bbox, add = TRUE)
plot(st_geometry(goat_data_sf), add = TRUE)

dem_50m <- dem
# writeRaster(dem, "./data/environment/dem_10m.tif")
# writeRaster(dem, "./data/environment/dem_3m_z14.tif", overwrite = TRUE)
# writeRaster(dem, "./data/environment/dem_6m_z13.tif", overwrite = TRUE)
# writeRaster(dem, "./data/environment/dem_12.5m_z12.tif", overwrite = TRUE)
# writeRaster(dem, "./data/environment/dem_25m_z11.tif", overwrite = TRUE)
writeRaster(dem, "./data/environment/dem_50m_z10.tif", overwrite = TRUE)
# writeRaster(dem, "./data/environment/dem_z11_locs.tif", overwrite = TRUE)


#............................................................
# change raster resolution ----
#............................................................

# dem <- rast("./data/environment/dem_10m.tif")
# dem <- rast("./data/environment/dem_25m_z11.tif")
dem <- rast("./data/environment/dem_50m_z10.tif")
# dem <- rast("./data/environment/dem_z11_locs.tif")
plot(dem)
test <- dem

# as per dr. noonan
# test <- rast(dem)
# test <- project(test, "EPSG:3005")
plot(test)
res(test)
test2 <- terra::aggregate(test, 2) # made it more coarse
res(test2)
# [1] 50.19045 50.19045
# theres your ~50m resolution

# for sensitivity analysis
dem_100m <- terra::aggregate(test, 2) 
res(dem_100m)
#100.4963 100.4963
dem_150m <- terra::aggregate(test, 3) # made it more coarse
res(dem_150m)
# 150.7445 150.7445


# writeRaster(dem_50m, "./data/environment/dem_50m.tif", overwrite = TRUE)
writeRaster(dem_100m, "./data/environment/dem_100m.tif", overwrite = TRUE)
writeRaster(dem_150m, "./data/environment/dem_150m.tif", overwrite = TRUE)
# dem_50m <- rast("./data/environment/dem_50m.tif")
dem_100m <- rast("./data/environment/dem_100m.tif")
dem_150m <- rast("./data/environment/dem_150m.tif")

#.................................................

# how to confirm that your raster is actually is correct resolution (based on the ground resolution calculations)? different crs gives different resolution
res(dem)
crs(dem)

# convert to meter projection to get resolution details
dem <- project(dem, "epsg:3005") #resolution  : 23.53822, 23.53822  (x, y)
dem <- project(dem, "epsg:3857") #resolution  : 35.92367, 35.92367  (x, y)
