
# home range estimation

library(ctmm)
library(tictoc)
library(beepr)


#...........................................................
# Import data ----
#...........................................................

# Import combined collar data (original + new)
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

#format names to match required for ctmm based on Movebank critera:
# create a column combining collar_id and year to avoid needing to subset by individuals and year, it will create a unique identifier based on the individual and the year of the data and then those will be grouped together as the data for each individual for each year
goat_data$individual.local.identifier <- paste(goat_data$collar_id, goat_data$year, sep = "_")
# format names to match
goat_data <- plyr::rename(goat_data, c('latitude' = 'location.lat', 
                                       'longitude' = 'location.long'))

# load movement models
load("data/movement_model/fits_20250301.rda")


#...........................................................


# Estimate akdes home-range areas
START_hr <- Sys.time()
tic(msg = "home range analysis")

AKDES <- akde(tel_data,FITS,weights=TRUE)

toc() # 14.2 mins
END_hr <- Sys.time()
beep(8)
kittyR::meowR(sound = 2)

# ~10.86min, 14.2 mins


#save rda:
dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)
# save(AKDES,file="data/home_range/goat_akdes_20241217.rda")
# load("data/home_range/goat_akdes_20241217.rda")
save(AKDES,file="data/home_range/akdes_20250301.rda")
load("data/home_range/akdes_20250301.rda")




#......................................................................
# Create akde UDs as rasters and shapefiles ----
#......................................................................
# Fit_Mods.R

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#save UD as raster:
# dir.create("data/home_range/UD", recursive = TRUE)
dir.create("data/home_range/combined_data_fire_period_all_years/UD", recursive = TRUE, showWarnings = TRUE)

#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/combined_data_fire_period_all_years/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}




#save 95% range estimate UD as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
# dir.create("data/home_range/shp", recursive = TRUE)
dir.create("data/home_range/combined_data_fire_period_all_years/shp", recursive = TRUE, showWarnings = TRUE)

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/combined_data_fire_period_all_years/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp.path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }


