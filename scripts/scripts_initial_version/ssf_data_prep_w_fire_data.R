
library(mgcv)
library(gratia)
library(ggplot2)
library(wesanderson)
library(dplyr)
library(terra)
library(cowplot)
library(amt)
library(tidyterra)
library(sf)
library(hmmSSF)





#..................................................................
## Data preparation ----

# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# convert collar_id to character object so only 6 show up instead of 10 levels when as.factor object, issues with extracting outputs
goat_data$collar_id <- as.character(goat_data$collar_id)
unique(goat_data$collar_id)

# The data have columns for individual `ID`, `time`, longitude/latitude coordinates (`lon` and `lat`) and projected coordinates (`x` and `y`).
# format to match workflow & hmmSSF package requirements
goat_data$time <- goat_data$timestamp
goat_data$ID <- goat_data$collar_id

# need projected coordinates (`x` and `y`) for hmmSSF package for generating random points
# convert df to sf object
goat_sf <- st_as_sf(goat_data, coords = c("longitude","latitude"))
goat_sf <- st_set_crs(goat_sf, 4326) 
# reproject
goat_sf <- st_transform(goat_sf, "epsg:5070") # NAD83, Conus Albers
# extract reprojected coordinates
goat_coords <- st_coordinates(goat_sf)
# add to df
goat_data$x <- goat_coords[,"X"] # lon_reproj
goat_data$y <- goat_coords[,"Y"] # lat_reproj

# clean up environment
rm(goat_coords, goat_sf)



#////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////// ----
#////////////////////////////////////////////////////////////////////

# in a gam-like framework, you dont used vs available steps, instead you model the likelihood of use in space/time (poisson process)
# model estimates a smooth function over spatial location — i.e., where animals go — without needing step start/end.
# refer to the sim data workflow to see the translation from amt/glmm to mcvg/gam to understand a bit better


#..................................................................
# Generating random points ----

#***smooth covariate effects and non-parametric movement kernel***  petrel.rmd example https://github.com/NJKlappstein/smoothSSF/blob/main/examples/petrel.Rmd
#
# Model a non-parametric movement kernel. 
# .-. need to sample spatially uniform points on a disc with radius R (rather than sampling from a parametric distribution of step lengths and/or turning angles). 
# can't do it via amt package
# .-. use hmmSSF package (can write code manually yourself too) via get_controls function
# output = step lengths and turning angles

# The general steps are:
# 1. define R as the maximum observed step length (or something similar)
# 2. sample each turning angle from Unif(-pi, pi)
# 3. sample each step length as the square root of a random draw of Unif(0, R^2)

# Note: uniform sampling needs a lot of random points for model fitting, and so we will use 50 random points in this example.
# get_controls() output = df with new columns 
# `stratum` = identifier for each observed location and its associated random points, 
# `obs` = binary variable to identify whether the location is observed or random; `obs = 1` if observed, and `obs = 0` if random, 
# `step` = the step length, and 
# `angle` = the turning angle. 

# get control/random locations
# tictoc::tic()
ssf_data <- hmmSSF::get_controls(obs = goat_data,
                                 n_controls = 50, # number of random points
                                 distr = "uniform")
# tictoc::toc() #1min



# remove weights column returned by get_controls (unneeded here)
# w = importance weights (used in the fitting functions of `hmmSSF` and not applicable here). 
ssf_data$w <- NULL

# data carpentry
ssf_data$date <- as.Date(ssf_data$time)
ssf_data$year <- lubridate::year(ssf_data$time)
# combine collar_id and year into combined info id format
ssf_data$id_year <- paste(ssf_data$ID, ssf_data$year, sep = "_")
# ID (id_year) must be a factor or you'll run into errors for random slope and random smooth models
ssf_data$id_year <- as.factor(ssf_data$id_year)


head(ssf_data)




# plot histograms of observed step lengths and turning angles
obs <- subset(ssf_data, obs == 1)
hist(obs$step, 80)
hist(obs$angle, 80)


#.....................................................................................
## Replace zero step length to very small number ##
# sl with 0 value, i.e., 0 length

# because it's overkill to use a zero-inflated distribution just for one zero
# check for step with a value of 0
wh_zero <- which(ssf_data$step == 0) # 3 items
ssf_data$step[wh_zero] <- runif(length(wh_zero),
                                min = 0, 
                                max = min(ssf_data$step[-wh_zero], na.rm = TRUE))







#.....................................................................
# load data ----
#.....................................................................

# load spatial covariate data (packedspatraster or spatraster object), reproject, and scale/center goat-habitat covariate raster data via terra pkg
# scale rasters so they can be compared to as the same relative mean,  additionally, if not scaled, the model will produce NA values in the model summary later on
elev_25m <- rast('data/rasters/elev_25m.tif')
elev_25m <- project(elev_25m, "epsg:5070")
elev_25m_scaled <- scale(elev_25m)
# rename layer as the scaled raster shows up as the unscaled name
names(elev_25m_scaled) <- "elev_25m_scaled"

dist_escape_25m <- rast('data/rasters/dist_escape_25m.tif')
dist_escape_25m <- project(dist_escape_25m,"epsg:5070")
# dist_escape_25m_scaled <- scale(dist_escape_25m)
# # rename layer as the scaled raster shows up as the unscaled name
# names(dist_escape_25m_scaled) <- "dist_escape_25m_scaled"

# load fire data (fire data was converted into a raster first, refer to fire data prep script)
# dist_to_fire <- rast("./data/fire/dist_to_fire_raster.tif")
dist_to_fire <- rast("./data/fire/dist_to_fire_raster_enlarged.tif")
dist_to_fire <- project(dist_to_fire,"epsg:5070")
# dist_to_fire_scaled <- scale(dist_to_fire)



#.....................................................................................
## Extract covariates values (and other data preparation) ----

# We already have step lengths and turning angles, but we want to assess selection for "distance to colony" as well (as described in Michelot et al. 2023). We can derive this covariate using the code below. We also set a single observation with a step length of zero to a small number so we don't have to bother with zero inflation.

# instead of distance to colony, extract covariate data
# check the output of extraction before adding it to the df
head(terra::extract(elev_25m, ssf_data[, c("x", "y")]))
head(terra::extract(elev_25m_scaled, ssf_data[, c("x", "y")])) # properly renamed
# extract the 2nd column
ssf_data$elev_25m <- terra::extract(elev_25m, ssf_data[, c("x", "y")])[, "elev_25m"]
# ssf_data$elev_25m_scaled <- terra::extract(elev_25m_scaled, ssf_data[, c("x", "y")])[, "elev_25m_scaled"]

head(terra::extract(dist_escape_25m, ssf_data[, c("x", "y")]))
head(terra::extract(dist_escape_25m_scaled, ssf_data[, c("x", "y")])) # properly renamed
# extract the 2nd column
ssf_data$dist_escape_25m <- terra::extract(dist_escape_25m, ssf_data[, c("x", "y")])[, "dist_escape_25m"]
# ssf_data$dist_escape_25m_scaled <- terra::extract(dist_escape_25m_scaled, ssf_data[, c("x", "y")])[, "dist_escape_25m_scaled"]

# check to see if it extracted correctly, review the numbers, do they look reasonable/realistic?
head(ssf_data)



# The response needs to be a combination of a column of `times` (all same value) and stratum ID (as a factor). 
# "times" is defined as a constant variable, .-. add dummy time column as a single value for all
ssf_data$times <- 1
# Since we sampled spatially uniform points, we can fit a non-parametric movement kernel by specifying smooth terms for step lengths and turning angles. 
# For step lengths, we can use the default thin plate regression splines (TPRS). 
# Turning angles are a circular distribution, and so we should use a cyclic spline.
# We also include a smooth effect of distance to colony, using TPRS. 
# We specify `family = cox.ph` (identity link function) and 
# use the `weights` argument to specify whether each row is an observed or random location (`obs`). 
# We will use the default basis dimension for now.



# dir.create("./data/ssf/")
# save(ssf_data, file = "./data/ssf/ssf_data_no_fire.rda")
load("./data/ssf/ssf_data_no_fire.rda")




#.......................................................
### Calculate distance to fire ----

# set the date range and their perimeter number for each perimeter
# fire discovered July 22, 2023 and declared out Oct 26, 2023 https://wildfiresituation.nrs.gov.bc.ca/incidents?fireYear=2023&incidentNumber=K52125

# make a column to indicate if it was the fire year or not
ssf_data$fire_year <- ifelse(ssf_data$year == 2023, "TRUE", "FALSE")

# set up the columns
ssf_data$dist_to_fire <- rep(NA_integer_, nrow(ssf_data))
ssf_data$dist_to_fire_scaled <- rep(NA_integer_, nrow(ssf_data))
ssf_data$boundary_num <- rep(NA_integer_, nrow(ssf_data))

# data chunks for distance to fire based on time, so theyre separated based on the boundary dates
# the chunks are divided from boundary date to the next the date because the next boundary date is what we know for sure what it is and we dont know when it was like that, but only that on that date it is like that so we cant like buffer it

# assign a perimeter number based on date range
# first boundary was july 23 (i.e., no data for July 22 first day of the fire) .-. first fire boundary polygon is for after the fire has been discovered
# from = date of the boundary (i.e.,  the boundary data dates we have)
# to = day before of next boundary 
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-07-23") & ssf_data$date <= as.Date("2023-08-01") ] <-  1 
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-02") & ssf_data$date <= as.Date("2023-08-02") ] <-  2
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-03") & ssf_data$date <= as.Date("2023-08-03") ] <-  3
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-04") & ssf_data$date <= as.Date("2023-08-04") ] <-  4
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-05") & ssf_data$date <= as.Date("2023-08-05") ] <-  5
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-06") & ssf_data$date <= as.Date("2023-08-06") ] <-  6
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-07") & ssf_data$date <= as.Date("2023-08-07") ] <-  7
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-08") & ssf_data$date <= as.Date("2023-08-08") ] <-  8
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-09") & ssf_data$date <= as.Date("2023-08-09") ] <-  9
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-10") & ssf_data$date <= as.Date("2023-08-10") ] <-  10
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-11") & ssf_data$date <= as.Date("2023-08-11") ] <-  11
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-12") & ssf_data$date <= as.Date("2023-08-12") ] <-  12
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-13") & ssf_data$date <= as.Date("2023-08-13") ] <-  13
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-14") & ssf_data$date <= as.Date("2023-08-14") ] <-  14
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-15") & ssf_data$date <= as.Date("2023-08-15") ] <-  15
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-16") & ssf_data$date <= as.Date("2023-08-16") ] <-  16
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-17") & ssf_data$date <= as.Date("2023-08-17") ] <-  17
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-18") & ssf_data$date <= as.Date("2023-08-18") ] <-  18
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-19") & ssf_data$date <= as.Date("2023-08-19") ] <-  19
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-20") & ssf_data$date <= as.Date("2023-08-20") ] <-  20
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-21") & ssf_data$date <= as.Date("2023-08-21") ] <-  21
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-22") & ssf_data$date <= as.Date("2023-08-25") ] <-  22
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-26") & ssf_data$date <= as.Date("2023-08-26") ] <-  23
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-27") & ssf_data$date <= as.Date("2023-08-27") ] <-  24
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-28") & ssf_data$date <= as.Date("2023-08-28") ] <-  25
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-08-29") & ssf_data$date <= as.Date("2023-09-09") ] <-  26
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-09-10") & ssf_data$date <= as.Date("2023-09-14") ] <-  27
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-09-15") & ssf_data$date <= as.Date("2023-09-15") ] <-  28
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-09-16") & ssf_data$date <= as.Date("2023-09-17") ] <-  29
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-09-18") & ssf_data$date <= as.Date("2023-09-18") ] <-  30
ssf_data$boundary_num[ssf_data$date >= as.Date("2023-09-19") & ssf_data$date <= as.Date("2023-10-26") ] <-  31

# check if it worked
table(ssf_data$boundary_num)

# note: the boundary number = raster layer number
# therefore, based on the boundary number, extract covariate values using the same number for the raster layer



# i <- 80000

tictoc::tic()
for (i in seq_len(nrow(ssf_data))) {
  # print a message every 10000 iterations to track progress
  if (i %% 10000 == 0) {
    cat("completed", i, "iterations \n")
  }
  
  # check the year, if it isn't 2023 aka the fire year, then move to next row
  if (ssf_data$year[i] != 2023) {
    next
  }
  
  # if the boundary number has a number and not NA then continue, if it doesn't have a value, assign NA to the distance to fire
  if (!is.na(ssf_data$boundary_num[i])) {
    # get perimeter number
    perimeter_number <- ssf_data$boundary_num[i]
    # get the coordinates for this point
    pt_coord <- cbind(ssf_data$x[i], ssf_data$y[i])
    # extract covariate
    output <- try(terra::extract(dist_to_fire[[perimeter_number]], pt_coord))[1,1]
    output_scaled <- try(terra::extract(dist_to_fire_scaled[[perimeter_number]], pt_coord))[1,1] # added [1,1] because it extracts into a df, so [1,1] just grabs the vector value, used method = "bilinear" to buffers the point and interpolates instead of exact cell match but only reduced the NA values by 16 pts
    # update the column
    ssf_data$dist_to_fire[i] <- output
    ssf_data$dist_to_fire_scaled[i] <- output_scaled
  } else {
    ssf_data$dist_to_fire[i] <- NA
    ssf_data$dist_to_fire_scaled[i] <- NA
  }
  
}

tictoc::toc() # 31 min
beepr::beep(4)
# convert m to km
ssf_data$dist_to_fire_km <- ssf_data$dist_to_fire / 1000

head(ssf_data)

# save(ssf_data, file = "./data/ssf/ssf_data_w_fire.rda")
save(ssf_data, file = "./data/ssf/ssf_data_w_fire_enlarged.rda")
load("./data/ssf/ssf_data_w_fire_enlarged.rda")





