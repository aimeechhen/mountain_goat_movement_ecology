

# slope

library(terra)

# steep slopes being used as proxy-ish for escape terrain, but does not account for ruggedness

# sources for determining slope angle value to set as threshold (wrt escape terrain)
# 60+ Sarmento & berger 2020 tested http://dx.doi.org/10.7717/peerj.9296 
# 50+ White et al 2017 did not explain why they used 50degrees? https://doi.org/10.2981/wlb.00277
# 45+ poole & heard 2003 
# 40+ poole 2009 based on poole & heard 2003; gross et al 2002, decided on 40 https://cdnsciencepub-com.eu1.proxy.openathens.net/doi/10.1139/z09-009
# 40+ White and Gregovich 2018 doesnt say where/how they selected it
# 40+ wolf et al 2022 https://doi.org/10.1002/jwmg.22229 ; 
# 40+ shafer et al 2012, but doesnt say how they picked 40 degrees? https://esajournals.onlinelibrary.wiley.com/doi/10.1890/11-0815.1
# 40+ lowrey et al 2017 https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.1769
# richard & cote based on poole 2009; shafer et al 2012
# 35+ wells et al 2011 based on others https://doi.org/10.3390/rs3030435
# 33+ gross et al 2002 based on others and decided on 33 GIS-based habitat models for mountain goats
# flesch & belt 2017 used 33+ based on gross et al



# dem <- rast("./data/environment/dem_10m.tif")
# dem <- rast("./data/environment/dem_z11_bbox.tif")
# dem <- rast("./data/environment/dem_25m_z11.tif")
dem <- rast("./data/environment/dem_50m_z10.tif")

# calculate slope of the terrain (in degrees)
slope <- terrain(dem, v = "slope", unit = "degrees")
plot(slope)

# set 50+ degree slopes as 1 and below 50 as 0
# slopes over 50 = a proxy-ish for escape terrain, but does not account for ruggedness
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)
names(slope50) <- "slope50degree_50m" #50m resolution, renaming layer name so when importing it, its correct and not shown as "slope" for reference (avoid confusion)
slope50

# writeRaster(slope50, file = "./data/environment/slope50degree_25m.tif", overwrite = TRUE)
writeRaster(slope50, file = "./data/environment/slope50degree_50m.tif", overwrite = TRUE)

# calculate 50+ degree slopes distances
slope50_distance <- distance(slope50, target = 1) # only calculate for cells = 1
plot(slope50_distance)
names(slope50_distance) <- "slope50degree_distance_50m" #50m resolution, renaming layer name so when importing it, its correct, its indicated for reference (avoid confusion)
slope50_distance

# writeRaster(d2slope50, file = "./data/environment/dist_to_slope_50degree_25m.tif", overwrite = TRUE)
writeRaster(slope50_distance, file = "./data/environment/slope50degree_distance_50m.tif", overwrite = TRUE)

rm(dem, slope, slope50, slope50_distance)

#....................................................................
# other resolutions ----

# other resolutions to be used for sensitivity analysis of the rasters 
# for sensitivity analysis, dont aggregate() the slope rasters, get slope for the dem raster that has been aggregate(), only using 1 dem raster but aggregating at different level, refer to the elevation_data.R script for more details

# 100m resolution
dem <- rast("./data/environment/dem_100m.tif")
# calculate slope of the terrain (in degrees)
slope <- terrain(dem, v = "slope", unit = "degrees")
plot(slope)

# set 50+ degree slopes as 1 and below 50 as 0
# slopes over 50 = a proxy-ish for escape terrain, but does not account for ruggedness
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)

# calculate 50+ degree slopes distances
slope50_distance <- distance(slope50, target = 1) # only calculate for cells = 1
plot(slope50_distance)
names(slope50_distance) <- "slope50degree_distance_100m" #100m resolution, renaming layer name so when importing it, its correct, its indicated for reference (avoid confusion)
slope50_distance

# writeRaster(d2slope50, file = "./data/environment/dist_to_slope_50degree_25m.tif", overwrite = TRUE)
writeRaster(slope50_distance, file = "./data/environment/slope50degree_distance_100m.tif", overwrite = TRUE)

rm(dem, slope, slope50, slope50_distance)

#............................
# 150m resolution

dem <- rast("./data/environment/dem_150m.tif")
# calculate slope of the terrain (in degrees)
slope <- terrain(dem, v = "slope", unit = "degrees")
plot(slope)

# set 50+ degree slopes as 1 and below 50 as 0
# slopes over 50 = a proxy-ish for escape terrain, but does not account for ruggedness
slope50 <- ifel(slope >= 50, 1, 0) # ifel() specific to terra package
plot(slope50)
# no points at all!!!!



