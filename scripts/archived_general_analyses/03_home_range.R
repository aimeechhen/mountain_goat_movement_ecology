
# home range estimation

library(ctmm)
library(tictoc)
library(beepr)



library(ctmm)

load("data/collar_data/collar_data_20241123.rda")

#format names to match required for ctmm based on Movebank critera:
dat = plyr::rename(collar_data, c('goat_name' = 'individual.local.identifier',
                                  'latitude' = 'location.lat', 
                                  'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(dat, mark.rm = TRUE)


#____________________________________________________________
# Estimate akdes home-range areas ----
# 14.2 mins
START <- Sys.time()
tic()
AKDES <- akde(tel_data,FITS,weights=TRUE)
toc()
END <- Sys.time()
beep(8)


#save rda:
# dir.create("data/home_range/", recursive = TRUE)
# save(AKDES,file="data/home_range/goat_akdes_20241217.rda")
load("data/home_range/goat_akdes_20241217.rda")


#......................................................................
# Fit_Mods.R

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#save UD as raster:
dir.create("data/home_range/UD", recursive = TRUE)
#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}




#save 95% range estimate UD as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
dir.create("data/home_range/shp", recursive = TRUE)

for (name in names(AKDES)) {
  shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp.path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("data/home_range/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }





#__________________________________________________________

# get mean home range size (km^2) ----
#using the meta() from ctmm 

hr_size <- data.frame()

for (i in 1:length(AKDES)) {
  # get hr size 
  hr <- as.data.frame(ctmm::meta(AKDES[i]))
  #hr <- as.data.frame(ctmm::meta(AKDES[i]), units = FALSE)
  # subset to hr size row only
  hr <- hr[1,]
  # add to hr_size df
  hr_size <- rbind(hr_size, hr)
}

# inspect if all the units are the same (units = km²), units = FALSE, default units are m^2, but in the for loop with units = FALSE, it changed it to km² instead of keeping it in m²
hr_size
# if theyre all the same then rename the columns and drop the rowname
names(hr_size)[1] <- "mean_hr_min_km2"
names(hr_size)[2] <- "mean_hr_est_km2"
names(hr_size)[3] <- "mean_hr_max_km2"
rownames(hr_size) <- NULL



# save(hr_size, file = "data/home_range/hr_size_20241226.rda")
load("data/home_range/hr_size_20241226.rda")



#.............................
# alternatively, get hr values from the summary
# 
# #create a dataframe to store home range area statistics from the AKDE
# hr_size <- data.frame()
# 
# #loop through each object in the AKDE list
# for (i in 1:length(AKDES)) {
#   #extract the home range area statistics summary
#   summary <- as.data.frame(summary(AKDES[[i]])$CI) # default is square meters
#   summary$collar_id <- names(AKDES[i])
#   #bind the summary to the dataframe
#   hr_size <- rbind(hr_size, summary)
# }
# 
# row.names(hr_size) <- NULL
# names(hr_size)[7] <- "hr_low"
# names(hr_size)[8] <- "hr_est"
# names(hr_size)[9] <- "hr_high"
# 
# #save home range size results
# # save(hr_size, file = "data/home_range/hr_size.rda")
# load("data/home_range/hr_size.rda")
