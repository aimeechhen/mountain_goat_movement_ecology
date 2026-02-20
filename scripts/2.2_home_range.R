
# home range estimation

library(ctmm)
library(tictoc)
library(beepr)


#...........................................................
# Import data ----
#...........................................................

load(file = "./data/goat/study_period_tel_data.rda")

# summary of the gps data, (i.e., interval, period, long, lat info), check the period to ensure that its the study period length
summary(tel_data) # all 6.25h, 3.27-3.28 months

# visualisation of the data
plot(tel_data)

# load movement models
load("./data/movement/fits.rda")



#...........................................................
# Home range ----
#...........................................................

# home range = total area used (where)
# Utilization distribution (UD) = probability density function (PDF) or probability distribution
# UD = gives weight of usage within the home-range (where and how much)
# kernel density estimation = used to estimate probability density function


# Estimate akdes UDs (home-range areas)
START_hr <- Sys.time()
tic(msg = "home range analysis")

AKDES <- akde(tel_data,FITS,weights=TRUE)

toc() # 13.8min
END_hr <- Sys.time()
beep(8)


#save rda:
save(AKDES, file = "./data/movement/akdes.rda")
load("./data/movement/akdes.rda")


#......................................................................
# Create akde UDs as rasters and shapefiles ----
#......................................................................

load("./data/movement/akdes.rda")

# Fit_Mods.R

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#save utilization distribution (UD) as raster:
dir.create("./data/movement/UD/raster", recursive = TRUE, showWarnings = TRUE)

#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("./data/movement/UD/raster", paste0(names(AKDES)[i], "_UD_raster.tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}



#save 95% range estimate utilization distribution (UD) as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12, now it does 2026-02-17

dir.create("./data/movement/UD/shp", recursive = TRUE, showWarnings = TRUE)

for (i in 1:length(AKDES)) {
  shp.path <- file.path("./data/movement/UD/shp", paste0(names(AKDES)[i], "_UD_shapefile.shp"))
  writeVector(AKDES[[i]], shp.path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("./data/movement/UD/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }





#____________________________________________________________________
# Extract covariate values from 95% estimated home range shapefile ----
# 06a.load_merge_shapefiles_per_period.r

library(sf)
# 1. Import 95% home range estimate shapefiles
# Set folder path containing the exported subfolders
# folder_path <- "data/home_range/fire_goat/shp"
folder_path <- "./data/movement/UD/shp"
# Load .shp files from subfolders
shp_dir <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
#Read each shapefile into a list
shp_files <- lapply(shp_dir, st_read)
#rename files
names(shp_files) <- names(AKDES)

# 2. Merge all 95% HR estimate shapefiles into single sf object
# Combine all the shapefiles into a single sf object
hr_shp <- do.call(rbind, shp_files)
# Subset 95% est shapefiles only based on the text "95% est" in the name column
hr_shp <- hr_shp[grepl("95% est", hr_shp$name), ]
# project to bc albers crs
bc_albers <- "EPSG:3005"
hr_shp = st_transform(hr_shp, crs = st_crs(bc_albers))
st_crs(hr_shp)

# # Writing 29 features with 1 fields and geometry type Unknown (any).
# geometry_types <- st_geometry_type(hr_shp)
# table(geometry_types)
# # no unknown is showing up

# save merged shapefile sf object
st_write(hr_shp, dsn = './data/movement/UD/shp/merged_95_HR_shp', 
         driver = 'ESRI Shapefile', append=FALSE)
# read in shapefile
hr_shp <- st_read(dsn = './data/movement/UD/shp/merged_95_HR_shp/')


