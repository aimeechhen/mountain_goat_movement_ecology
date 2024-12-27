
# movement models and home range estimation

library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)

load("data/collar_data/collar_data_20241123.rda")

#format names to match required for ctmm based on Movebank critera:
dat = plyr::rename(collar_data, c('goat_name' = 'individual.local.identifier',
                                  'latitude' = 'location.lat', 
                                  'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(dat, mark.rm = TRUE)

# summary of the gps data
summary(tel_data)

# visualisation of the data
plot(tel_data)

#...........................................................
# Movement models ----

#create an empty list to store output
FITS <- list()

START <- Sys.time()
tic()
for(i in 1:length(tel_data)){
  message("Currently on animal ", i, " of ", length(tel_data))
  #Extract individual
  DATA <- tel_data[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process
  # fit movement models and select best fit
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)
  
}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h
END <- Sys.time()



# save(FITS,file="data/movement_model/goat_fits_20241107.rda")
load("data/movement_model/goat_fits_20241107.rda")

#____________________________________________________________
# Estimate various speed types ----

SPEED_MEAN <- list()
SPEEDS_INSTA <- list()

START <- Sys.time()
tic()

for(i in 1:length(tel_data)){
  message("Currently on animal ", i, " of ", length(tel_data))
  #Extract individual telemetry and movement model
  DATA <- tel_data[[i]]
  FIT <- FITS[[i]]
  
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true)
  SPEED_MEAN[[i]] <- speed(object = DATA, CTMM = FIT,
                           robust = TRUE, units = FALSE, trace = TRUE, cores = 8)
  # estimate instantaneous speed -> Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value
  SPEEDS_INSTA[[i]] <- speeds(object = DATA, CTMM = FIT,
                              robust = TRUE, units = FALSE, trace = TRUE, cores = 8)
  
  
}

#rename for convenience
names(SPEED_MEAN) <- names(tel_data)
names(SPEEDS_INSTA) <- names(tel_data)
#  13.87517 mins
toc()
END <- Sys.time()
beep(8)
save(SPEED_MEAN, file = "./data/movement_model/goat_speed_mean_20241226.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/goat_speeds_insta_20241226.rda")


load("./data/movement_model/goat_speed_mean_20241226.rda")
load("./data/movement_model/goat_speeds_insta_20241226.rda")



#_______________________________________________________________________________
# prep and build results dataframe ----

#build results dataframe
SUMMARY_FITS <- data.frame(goat_name = names(FITS))
SUMMARY_FITS$collar_id <- collar_data$collar_id[match(SUMMARY_FITS$goat_name, collar_data$goat_name)]
SUMMARY_FITS$goat_id <- collar_data$goat_id[match(SUMMARY_FITS$goat_name, collar_data$goat_name)]
SUMMARY_FITS <- SUMMARY_FITS[, c("goat_name", "goat_id", "collar_id")]





#__________________________________________________________
# RESULTS ----

# inspect one summary output of movement model
fitsum <- summary(FITS[[1]])
fitsum
# CI units:
# area (square kilometers)
# τ[position] (months)       
# τ[velocity] (minutes)   
# speed (kilometers/day)  
# diffusion (hectares/day)


#........................................
## check units to see if theyre all the same ----
summary_outputs <- data.frame()

for (i in seq_along(FITS)) {
  summary <- summary(FITS[[i]])$CI
  summary_outputs <- rbind(summary_outputs, data.frame(Var1 = names(table(rownames(summary))), Freq = as.integer(table(rownames(summary)))))
}
# get the counts be combining all the entries and summing them up to get the total
summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
summary_outputs

# Var1 Freq
# 1 area (square kilometers)   10
# 2 diffusion (hectares/day)   10
# 3   speed (kilometers/day)   10
# 4       τ[position] (days)    8
# 5     τ[position] (months)    2
# 6      τ[velocity] (hours)    1
# 7    τ[velocity] (minutes)    9



# they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections





#..................................................
# Get movement model that was best fit ----
model_type <- data.frame()


#get movement model that was best fit
model_type <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$name
  model_type <- rbind(model_type, summary)
}
names(model_type)[1] <- "movement_model"

SUMMARY_FITS <- cbind(SUMMARY_FITS, model_type)




#...............................................................................
# Get movement models degrees of freedom ----

DOF_results <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$DOF
  DOF_results <- rbind(DOF_results, summary)
}
names(DOF_results)[1] <- "DOF_mean"
names(DOF_results)[2] <- "DOF_area"
names(DOF_results)[3] <- "DOF_diffusion"
names(DOF_results)[4] <- "DOF_speed"

SUMMARY_FITS <- cbind(SUMMARY_FITS, DOF_results)


#................................................................................
#Get diffusion estimates ----

diffusion_results <- data.frame(
  diffusion_min_m2_s = numeric(length(FITS)),
  diffusion_est_m2_s = numeric(length(FITS)),
  diffusion_max_m2_s = numeric(length(FITS))
)

# #Get diffusion values (units = square meters/second)
# units = FALSE removes the units that FITS has assigned and then by default it is in m^2/second
for (i in seq_along(FITS)) {
  # Check if "diffusion (square meters/second)" is present in the row names
  if ("diffusion (square meters/second)" %in% row.names(summary(FITS[[i]], units = FALSE)$CI)) {
    # Update the corresponding row in the data frame
    diffusion_results[i, "diffusion_min_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 1]
    diffusion_results[i, "diffusion_est_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 2]
    diffusion_results[i, "diffusion_max_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 3]
  }
}

# # Convert m^2/second into km^2/day
diffusion_results$diffusion_min_km2_day <- diffusion_results$diffusion_min_m2_s * 0.0864
diffusion_results$diffusion_est_km2_day <- diffusion_results$diffusion_est_m2_s * 0.0864
diffusion_results$diffusion_max_km2_day <- diffusion_results$diffusion_max_m2_s * 0.0864

SUMMARY_FITS <- cbind(SUMMARY_FITS, diffusion_results)

#.........................................................
# Get position and velocity (tau p and tau v) ----
# OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)

## Get tau p estimates ----
tau_p_results <- data.frame()

for (i in seq_along(FITS)) {
  # SPEED_MEAN[i]
  summary <- as.data.frame(summary(FITS[[i]])$CI)
  results <- summary[grepl("position", row.names(summary)),]
  tau_p_results <- rbind(tau_p_results, results)
}
# inspect if all the units are the same 
tau_p_results
# some are in months and mostly in days, so convert the ones with months units into day units
tau_p_results[grep("months", rownames(tau_p_results)), ] <- tau_p_results[grep("months", rownames(tau_p_results)), ] * 30
# reinspect
tau_p_results
# looks much better

# if theyre all the same then rename the columns and drop the rowname
names(tau_p_results)[1] <- "tau_p_min_days"
names(tau_p_results)[2] <- "tau_p_est_days"
names(tau_p_results)[3] <- "tau_p_max_days"
rownames(tau_p_results) <- NULL

SUMMARY_FITS <- cbind(SUMMARY_FITS, tau_p_results)



#............................................
## Get tau v estimates ----
tau_v_results <- data.frame()
# summary_outputs <- data.frame()

for (i in seq_along(FITS)) {
  summary <- as.data.frame(summary(FITS[[i]])$CI)
  results <- summary[grepl("velocity", row.names(summary)),]
  # check if there are results, if not, then insert NA so then everything is recorded and not skipped
  if (nrow(results) > 0) {
    tau_v_results <- rbind(tau_v_results, results)
  } else {
    tau_v_results <- rbind(tau_v_results, NA) 
  }
  # summary_outputs <- rbind(summary_outputs, data.frame(Var1 = rownames(summary[grepl("velocity", rownames(summary)), ]), Freq = as.integer(table(rownames(summary[grepl("velocity", rownames(summary)), ])))))
  
}
# summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
# summary_outputs

# inspect if all the units are the same 
tau_v_results
# some are in hours and minutes, so convert the ones with hour units into minute units
tau_v_results[grep("hours", rownames(tau_v_results)), ] <- tau_v_results[grep("hours", rownames(tau_v_results)), ] * 60
# reinspect
tau_v_results
# looks much better

# if theyre all the same units now then rename the columns and drop the rowname
names(tau_v_results)[1] <- "tau_v_min_minutes"
names(tau_v_results)[2] <- "tau_v_est_minutes"
names(tau_v_results)[3] <- "tau_v_max_minutes"
rownames(tau_v_results) <- NULL

SUMMARY_FITS <- cbind(SUMMARY_FITS, tau_v_results)





#......................................................
# get mean speed ----
# Extract mean speed values (units = meters/sec) (time-averaged speed, proportional to distance travelled)
# OU model causes 'Inf' because not enough data to estimate
dof_results <- data.frame()
ci_results <- data.frame()

for (i in 1:length(SPEED_MEAN)) {
  dof_df <- as.data.frame(SPEED_MEAN[[i]]$DOF)
  dof_results <- rbind(dof_results, dof_df)
  ci_df <- as.data.frame(SPEED_MEAN[[i]]$CI)
  ci_results <- rbind(ci_results, ci_df)
  
}

# inspect if all the units are the same 
dof_results # (units = unitless?)
ci_results #(units = meters/second)

# if theyre all the same then rename the columns and drop the rowname
names(dof_results)[1] <- "speed_mean_DOF"
names(ci_results)[1] <- "speed_mean_min_ms"
names(ci_results)[2] <- "speed_mean_est_ms"
names(ci_results)[3] <- "speed_mean_max_ms"
rownames(dof_results) <- NULL
rownames(ci_results) <- NULL

# combine the two
speed_mean_results <- cbind(dof_results, ci_results)

SUMMARY_FITS <- cbind(SUMMARY_FITS, speed_mean_results)


#save df
save(SUMMARY_FITS, file = "./data/movement_model/goat_fits_summary_20241226.rda")
# load("./data/movement_model/goat_fits_summary_20241226.rda")

#..........................................
# get insta speed ----
# Extract insta speed values ****(units = meters/sec) (time-averaged speed, proportional to distance travelled)
# OU model causes 'Inf' because not enough data to estimate**** check

speed_insta_results <- data.frame(
  speed_insta_min = numeric(length(FITS)),
  speed_insta_est = numeric(length(FITS)),
  speed_insta_max = numeric(length(FITS))
)


# SPEEDS_INSTA <- readRDS(file = "./data/movement_model/goat_speeds_insta_20241108.rda")
# SPEEDS_INSTA # Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value 






#____________________________________________________________
# Estimate akdes home-range areas ----
# 14.2 mins
START <- Sys.time()
tic()
AKDES <- akde(tel_data,FITS,weights=TRUE)
toc()
END <- Sys.time()
beep(8)

#......................................................................
# Fit_Mods.R

# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#And save
dir.create("data/home_range/", recursive = TRUE)

#save rda:
# save(AKDES,file="data/home_range/goat_akdes_20241217.rda")
load("data/home_range/goat_akdes_20241217.rda")

#save UD as raster:
dir.create("data/home_range/UD", recursive = TRUE)
#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}




#save 95% range estimate as shapefile:
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
  # subset to hr size row only
  hr <- hr[1,]
  # add to hr_size df
  hr_size <- rbind(hr_size, hr)
}

# inspect if all the units are the same (units = km²)
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
# HR_size <- data.frame()
# 
# #loop through each object in the AKDE list
# for (i in 1:length(AKDES)) {
#   #extract the home range area statistics summary
#   summary <- as.data.frame(summary(AKDES[[i]])$CI)
#   summary$collar_id <- names(AKDES[i])
#   #bind the summary to the dataframe
#   HR_size <- rbind(HR_size, summary)
# }
# 
# row.names(HR_size) <- NULL
# names(HR_size)[7] <- "HR_low"
# names(HR_size)[8] <- "HR_est"
# names(HR_size)[9] <- "HR_high"
# 
# #save home range size results
# # save(HR_size, file = "data/home_range/HR_size.rda")
# load("data/home_range/HR_size.rda")
