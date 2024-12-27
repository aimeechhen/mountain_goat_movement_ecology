
library(ctmm)
library(raster)
library(lme4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tictoc)
library(beepr)


# 1) Data prep ----

# collar data
load("data/collar_data/collar_data_20241123.rda")

# subset to fire goats
goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
fire_goats <- collar_data[collar_data$goat_name %in% goats,] # 43941 obs

# Define the wildfire date range
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

# Crater creek wildfire time period
# ccfire_start <- '2023-07-22' # doy = 203
# ccfire_end <- '2023-10-26' # doy = 299

#subset goat data based on the date range of the crater creek wildfire across all years
fire_goats <- fire_goats[fire_goats$month_day >= fire_start & fire_goats$month_day <= fire_end, ] #10376 obs

#format names to match required for ctmm based on Movebank critera:
# create a column 
fire_goats$individual.local.identifier <- paste(fire_goats$goat_name, fire_goats$year, sep = "_")
# format names to match
fire_goats <- plyr::rename(fire_goats, c('latitude' = 'location.lat', 
                           'longitude' = 'location.long'))

#Convert to telemetry
tel_data <- as.telemetry(fire_goats, mark.rm = TRUE)



#____________________________________________________________
# 2) Fit movement models ----
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

toc() #9.698333 mins
END <- Sys.time()

dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)
# save(FITS,file="data/movement_model/fire_goat_fits_20241217.rda")
load("data/movement_model/fire_goat_fits_20241217.rda")





#____________________________________________________________
# 3) Estimate akdes home-range areas ----
# ~10.86min
START <- Sys.time()
tic()
AKDES <- akde(tel_data,FITS,weights=TRUE)
toc()
beep(3)


dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)

#save rda:
# save(AKDES,file="data/home_range/fire_goat_akdes_20241217.rda")
load("data/home_range/fire_goat_akdes_20241217.rda")



#............................................................
## 3b) Create akde UDs as rasters and shapefiles ----

# Fit_Mods.R
# in ctmm you can create 2 things:
# PMF: a raster of the probabilities of where the animal could be in the HR
# AKDE shapefile: a boundary of some given quantile

#save UD as raster:
dir.create("data/home_range/fire_goat/UD", recursive = TRUE, showWarnings = TRUE)

#Note: includes distribution function = probability mass function
#QUESTION: for the PMF you don't specify the level.UD because it's a PMF rather than a contour
for (i in 1:length(AKDES)) {
  UD_file <- file.path("data/home_range/fire_goat/UD", paste0(names(AKDES)[i], ".tif"))
  writeRaster(AKDES[[i]], filename = UD_file, format = 'GTiff', DF = "PMF",
              overwrite = TRUE)
}

#save 95% range estimate as shapefile:
# DF = "PMF" is not possible in a shp file, shp is only for points or boundaries
#Note: this sometimes works and sometimes doesnt because it says writeShapefile() doesnt exist, writeVector() does work as of recent update, now it doesn't, currently using writeShapefile() as of 2024-03-12
dir.create("data/home_range/fire_goat/shp", recursive = TRUE, showWarnings = TRUE)

for (name in names(AKDES)) {
  shp_path <- file.path("data/home_range/fire_goat/shp", paste0(name, ".shp"))
  writeVector(AKDES[[name]], shp_path,
              level.UD=0.95, level=0.95, overwrite = TRUE)
}

# for (name in names(AKDES)) {
#   shp.path <- file.path("data/home_range/fire_goat/shp", paste0(name, ".shp"))
#   writeShapefile(AKDES[[name]], shp.path,
#                  level.UD=0.95, level=0.95, overwrite = TRUE)
# }





#.....................................................
# 4) Estimate various speed types ----

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

toc()
beep(6)

# 
# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.


# save(SPEED_MEAN, file = "./data/movement_model/fire_goat_speed_mean_20241217.rda")
# save(SPEEDS_INSTA, file = "./data/movement_model/fire_goat_speeds_insta_20241217.rda")

load(file = "./data/movement_model/fire_goat_speed_mean_20241217.rda")
load(file = "./data/movement_model/fire_goat_speeds_insta_20241217.rda")
# Warning messages:
#   1: In as.POSIXlt.POSIXct(x, tz) : unknown timezone 'PDT'





#_______________________________________________________________________________
# prep and build results dataframe ----

hr_results <- data.frame(individual.local.identifier = names(FITS))
# extract the goat name from individual.local.identifier, i.e. drop the "_year" portion
hr_results$goat_name <- gsub("_[0-9]{4}$", "", hr_results$individual.local.identifier)
# extract the year, i.e. last 4 digits after the _
hr_results$year <- gsub(".*_([0-9]{4})$", "\\1", hr_results$individual.local.identifier)
# add collar_id column, match them based on goat_name
hr_results$collar_id <- collar_data$collar_id[match(hr_results$goat_name, collar_data$goat_name)]
hr_results$goat_id <- collar_data$goat_id[match(hr_results$goat_name, collar_data$goat_name)]
hr_results <- hr_results[, c("goat_name", "goat_id", "collar_id", "year", "individual.local.identifier")]



#__________________________________________________________
# RESULTS ----

# inspect one summary output of movement model
fitsum <- summary(FITS[[1]])
fitsum
# units:
# τ[position] (days)       
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

# they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections

# Var1 Freq
#         area (square kilometers)   29
#         diffusion (hectares/day)   12
# diffusion (square kilometers/day)   17
#           speed (kilometers/day)   13
#               τ[position] (days)   27
#              τ[position] (hours)    2
#              τ[velocity] (hours)    6
#            τ[velocity] (minutes)    7



#..................................................
## 1. Get movement model that was best fit ----
model_type <- data.frame()

for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$name
  model_type <- rbind(model_type, summary)
}
names(model_type)[1] <- "movement_model"

# add to results df
hr_results <- cbind(hr_results, model_type)






# ..................................................
## 2. get mean home range size ----
# i.e. extract AKDE UD values using the meta() from ctmm 

i <- 1

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

# add to results df
hr_results <- cbind(hr_results, hr_size)



#...........................................................
## 2b. get yearly mean home range size (km^2) ----

# Compare between years, based on year only 

# set years
data_years <- seq(from = 2019, to = 2023, by = 1)
hr_size_yearly <- data.frame()

for (i in data_years) {
  # check if data is being subsetted correctly
  message("on year ", i, " subsetting data to: ", paste(names(AKDES)[grepl(i, names(AKDES))], collapse = " "))
  # subset data
  dat <- AKDES[grepl(i, names(AKDES))]
  hr <- as.data.frame(ctmm::meta(dat))
  # hr <- as.data.frame(ctmm::meta(AKDES[grepl(i, names(AKDES))])) #full length, using one line, for reference purposes
  # subset to hr size row only
  hr <- hr[1,]
  hr$year <- i
  hr_size_yearly <- rbind(hr_size_yearly, hr)
}

# inspect if all the units are the same
hr_size_yearly
# if theyre all the same then rename the columns and drop the rowname
names(hr_size_yearly)[1] <- "mean_hr_min_km2"
names(hr_size_yearly)[2] <- "mean_hr_est_km2"
names(hr_size_yearly)[3] <- "mean_hr_max_km2"
rownames(hr_size_yearly) <- NULL


save(hr_size_yearly, file = "data/home_range/fire_goat_hr_size_yearly_20241217.rda")





#.............................
# alternatively, get hr values from the summary

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


#...............................................................................
## 3. Get movement models degrees of freedom ----

DOF_results <- data.frame()

for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$DOF
  DOF_results <- rbind(DOF_results, summary)
}
names(DOF_results)[1] <- "DOF_mean"
names(DOF_results)[2] <- "DOF_area"
names(DOF_results)[3] <- "DOF_diffusion"
names(DOF_results)[4] <- "DOF_speed"

# add to results df
hr_results <- cbind(hr_results, DOF_results)




#................................................................................
## 4. Get diffusion estimates ----

diffusion <- data.frame()

for (i in seq_along(FITS)) {
  summary <- as.data.frame(summary(FITS[[i]])$CI)
  results <- summary[grepl("diffusion", row.names(summary)),]
  diffusion <- rbind(diffusion, results)
}
# inspect if all the units are the same 
diffusion

# theyre not all the same -> need to correct and convert
# units = FALSE removes the units that FITS has assigned and then by default it is in m^2/second 
#Get diffusion values (units = square meters/second)
diffusion_results <- data.frame(
  diffusion_min_m2_s = numeric(length(FITS)),
  diffusion_est_m2_s = numeric(length(FITS)),
  diffusion_max_m2_s = numeric(length(FITS))
)

for (i in seq_along(FITS)) {
  # Check if "diffusion (square meters/second)" is present in the row names
  if ("diffusion (square meters/second)" %in% row.names(summary(FITS[[i]], units = FALSE)$CI)) {
    # Update the corresponding row in the data frame
    diffusion_results[i, "diffusion_min_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 1]
    diffusion_results[i, "diffusion_est_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 2]
    diffusion_results[i, "diffusion_max_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 3]
  }
}

# Convert m^2/second into km^2/day and create new columns for them
diffusion_results$diffusion_min_km2_day <- diffusion_results$diffusion_min_m2_s * 0.0864
diffusion_results$diffusion_est_km2_day <- diffusion_results$diffusion_est_m2_s * 0.0864
diffusion_results$diffusion_max_km2_day <- diffusion_results$diffusion_max_m2_s * 0.0864

# add to results df
hr_results <- cbind(hr_results, diffusion_results)




#........................................................
## 5. Get tau p estimates (position) ----
# OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)

### 4. Get tau p estimates----
tau_p <- data.frame()

for (i in seq_along(FITS)) {
  # SPEED_MEAN[i]
  summary <- as.data.frame(summary(FITS[[i]])$CI)
  results <- summary[grepl("position", row.names(summary)),]
  tau_p <- rbind(tau_p, results)
}
# inspect if all the units are the same 
tau_p
# some are in hours and mostly in days, so convert the ones with hour units into day units
tau_p[grep("hours", rownames(tau_p)), ] <- tau_p[grep("hours", rownames(tau_p)), ] / 24
# reinspect
tau_p
# looks much better

# if theyre all the same then rename the columns and drop the rowname
names(tau_p)[1] <- "tau_p_min_days"
names(tau_p)[2] <- "tau_p_est_days"
names(tau_p)[3] <- "tau_p_max_days"
rownames(tau_p) <- NULL

# add to results df
hr_results <- cbind(hr_results, tau_p)






#........................................................
## 6. Get tau v estimates (velocity) ----
# OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)

tau_v <- data.frame()
# summary_outputs <- data.frame()

for (i in seq_along(FITS)) {
  summary <- as.data.frame(summary(FITS[[i]])$CI)
  results <- summary[grepl("velocity", row.names(summary)),]
  # check if there are results, if not, then insert NA so then everything is recorded and not skipped
  if (nrow(results) > 0) {
    tau_v <- rbind(tau_v, results)
  } else {
    tau_v <- rbind(tau_v, NA) 
  }
  # summary_outputs <- rbind(summary_outputs, data.frame(Var1 = rownames(summary[grepl("velocity", rownames(summary)), ]), Freq = as.integer(table(rownames(summary[grepl("velocity", rownames(summary)), ])))))
  
}
# summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
# summary_outputs

# inspect if all the units are the same 
tau_v
# some are in hours and minutes, so convert the ones with hour units into minute units
tau_v[grep("hours", rownames(tau_v)), ] <- tau_v[grep("hours", rownames(tau_v)), ] * 60
# reinspect
tau_v
# looks much better

# if theyre all the same units now then rename the columns and drop the rowname
names(tau_v)[1] <- "tau_v_min_minutes"
names(tau_v)[2] <- "tau_v_est_minutes"
names(tau_v)[3] <- "tau_v_max_minutes"
rownames(tau_v) <- NULL

# add to results df
hr_results <- cbind(hr_results, tau_v)





#......................................................
## 7. get mean speed ----
# Extract mean speed values (time-averaged speed, proportional to distance travelled)
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

# add to results df
hr_results <- cbind(hr_results, speed_mean_results)






#..........................................
## 8. get insta speed ----
# Extract insta speed values ****(units = meters/sec) (time-averaged speed, proportional to distance travelled)
# OU model causes 'Inf' because not enough data to estimate**** check

# speed_insta_results <- data.frame(
#   speed_insta_min = numeric(length(FITS)),
#   speed_insta_est = numeric(length(FITS)),
#   speed_insta_max = numeric(length(FITS))
# )


# SPEEDS_INSTA <- readRDS(file = "./data/movement_model/goat_speeds_insta_20241217.rda")
SPEEDS_INSTA # Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value 

# there are several entries, so like many insta speeds with timestamps

# SPEED_INSTA RESULTS NOT INCLUDED


#save results df
# save(hr_results, file = "./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")
load("./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")

# clean up environment
rm(model_type,
   summary,
   hr,
   hr_size,
   hr_size_yearly,
   ci_df,
   dof_df,
   DOF_results,
   diffusion,
   diffusion_results,
   tau_p,
   tau_v,
   results,
   ci_results,
   dof_results,
   speed_mean_results)



#________________________________________________________________
# 5) hr regression analysis  ----

library(lme4)

# use goat_name instead of "individual.local.identifier" because individual.local.identifier contains year and goat_name and when using this, it accounts for all of that junk instead of just goat_name hence using ONLY goat_name
#moreover when using individual.local.identifier in the model, its being set up as a 2 way interaction between year and goat and thats a no no

#..............................................
## hr model ----

# Build full model based on 95% home range area estimate, weighted regression
#dof mean is the effective sample size for estimating the center of the animal's home range.


m1 <- glmer(mean_hr_est_km2 ~ year + 
              (1|goat_name),
            family = Gamma('log'),
            weights = DOF_area,
            data = hr_results,
            na.action = "na.fail")

summary(m1)


#export and save summary output to a textfile
sink("data/home_range/fire_goat_m_hr_summary_20241225.txt")
print(summary(m1))
cat("\n") #enter blank line
# calculate the CI values
print("CI Values (lower, upper)")
# estimate + upper & lower z-score * std err
print(paste("year2020:", paste(-0.18335 + c(-1.96, 1.96) * 0.03170, collapse = " ")))
print(paste("year2021:", paste(-0.40621 + c(-1.96, 1.96) * 0.03300, collapse = " ")))
print(paste("year2022:", paste(-0.08494 + c(-1.96, 1.96) * 0.03241, collapse = " ")))
print(paste("year2023:", paste(-0.12407 + c(-1.96, 1.96) * 0.03215, collapse = " ")))
sink() #terminate output exporting connection/process (multiple functions can be exported)




#..............................................
#test for significance using likelihood ratio test (Note: this can be achieved by just running summary on the model and the "Pr(>|z|)" produces the same value)
m2 <- glmer(mean_hr_est_km2 ~ 1 + (1|goat_name),
            family = Gamma('log'),
            weights = DOF_area,
            data = hr_results,
            na.action = "na.fail")
test_results <- anova(m1, m2)
round(test_results$`Pr(>Chisq)`[2], 2) #p = 0 







#.......................................................
## diffusion model ----

#build full model based on diffusion rates
md <- glmer(diffusion_est_km2_day ~ year + 
              (1|goat_name),
            family = Gamma('log'),
            weights = DOF_diffusion,
            data = hr_results,
            na.action = "na.fail")

summary(md)



#export and save summary output to a textfile
sink("./data/home_range/fire_goat_m_diffusion_summary_20241225.txt")
print(summary(md))
cat("\n") #enter blank line
# calculate the CI values
print("CI Values (lower, upper)")
# estimate + upper & lower z-score * std err
print(paste("year2020:", paste(-0.23305 + c(-1.96, 1.96) * 0.01008, collapse = " ")))
print(paste("year2021:", paste(-0.02558 + c(-1.96, 1.96) * 0.01051, collapse = " ")))
print(paste("year2022:", paste(-0.03114 + c(-1.96, 1.96) * 0.01014, collapse = " ")))
print(paste("year2023:", paste(-0.17369 + c(-1.96, 1.96) * 0.01085, collapse = " ")))
sink() #terminate output exporting connection/process (multiple functions can be exported)





#____________________________________________________________________
# 6) Extract covariate values from 95% estimated home range shapefile ----
# 06a.load_merge_shapefiles_per_period.r

library(sf)
# 1. Import 95% home range estimate shapefiles
# Set folder path containing the exported subfolders
folder_path <- "data/home_range/fire_goat/shp"
# Load .shp files from subfolders
shp_dir <- list.files(path = folder_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
#Read each shapefile into a list
shp_files <- lapply(shp_dir, st_read)
#rename files
names(shp_files) <- names(AKDES)




# 2. Merge all 95% HR estimate shapefiles into single sf object
# Combine all the shapefiles into a single sf object
dat_shp <- do.call(rbind, shp_files)
# Subset 95% est shapefiles only based on the text "95% est" in the name column
dat_shp <- dat_shp[grepl("95% est", dat_shp$name), ]
# project to bc albers crs
bc_albers <- "EPSG:3005"
dat_shp = st_transform(dat_shp, crs = st_crs(bc_albers))
st_crs(dat_shp)

# # Writing 29 features with 1 fields and geometry type Unknown (any).
# geometry_types <- st_geometry_type(dat_shp)
# table(geometry_types)
# # no unknown is showing up

# save merged shapefile sf object
st_write(dat_shp, dsn = 'data/home_range/fire_goat/merged_95_HR_shp', 
         driver = 'ESRI Shapefile', append=FALSE)
# read in shapefile
dat_shp = st_read(dsn = 'data/home_range/fire_goat/merged_95_HR_shp')




# 3. extract covariate mean values
# load spatial covariate data (packedspatraster or spatraster object)
elev_25m <- rast('data/rasters/elev_25m.tif')
dist_escape_25m <- rast('data/rasters/dist_escape_25m.tif')

#scale/center raster data via terra pkg
scl_elev <- scale(elev_25m)
scl_dis_escape <- scale(dist_escape_25m)

# Combine rasters
rstack = c(elev_25m, dist_escape_25m,
           scl_elev, scl_dis_escape)

#extract all raster values as mean values
r_extract = extract(rstack, dat_shp, fun = 'mean', bind = TRUE, na.rm = TRUE)

# convert extracted mean covariate values into a dataframe
covariate_results <- as.data.frame(r_extract)

#rename columns
names(covariate_results)[1] <- "individual.local.identifier"
names(covariate_results)[2] <- "mean_elev_25m"
names(covariate_results)[3] <- "mean_dist_escape_25m"
names(covariate_results)[4] <- "mean_scl_elev"
names(covariate_results)[5] <- "mean_scl_dis_escape"


save(covariate_results, file = "./data/home_range/fire_goat_covariate_results.rda")
load("./data/home_range/covariate_results.rda")

#____________________________________________________________________
# 7) corrplot ----


library(terra)
library(corrplot)
library(sf)

load("./data/home_range/fire_goat_covariate_results.rda")
load("./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")


# add covariate extracted mean values to hr results df
dat_corr <- cbind(hr_results, covariate_results)

#subset data to be used in corrplot()
dat_corr <- dat_corr[,c("mean_hr_est_km2", 
                  "mean_elev_25m", "mean_dist_escape_25m",
                  "mean_scl_elev", "mean_scl_dis_escape")]

# rename for visualisation
names(dat_corr) <- c("Mean Home Range (km2)", 
                    "Mean Elevation", "Mean Dist. to Escape Terrain",
                    "Scaled Mean Elevation", "Scaled Mean Dist. to Escape Terrain")
# Use corrplot() to determine variables to include in full model
plot_corrplot <- corrplot(cor(dat_corr), method = 'number')

#save plot
png(file="figures/fire_goat_corrplot.png",
    width=6.86, height=6, units="in", res=600)
plot_corrplot
dev.off()






#____________________________________________________________________
# 8) RSF ----

library(raster)
elev_25m = raster('data/rasters/elev_25m.tif')
dist_escape_25m = raster('data/rasters/dist_escape_25m.tif')
# Some rasters are not loaded in RAM and may be slow to process. See help("raster::readAll").

R <- list(elevation = elev_25m, 
          dist_escape = dist_escape_25m)

rsf <- list()

# Fit RSF models (needs to be raster object and not spatraster)
START <- Sys.time()
tic()

for(i in 1:length(tel_data)){
  cat("Currently on animal", i, " of", length(tel_data))
  #Extract individual
  DATA <- tel_data[[i]]
  AKDE <- AKDES[[i]]
  
  # Fit rsf
  rsf[[i]] <- rsf.fit(DATA,AKDE, R=R)
}
names(rsf) <- names(tel_data)

toc() # ~12.55 hrs
beep(3)

dir.create("./data/rsf/", recursive = TRUE, showWarnings = TRUE)
# save(rsf, file = "./data/rsf/fire_goat_rsf_20241220.rda")
load("./data/rsf/fire_goat_rsf_20241220.rda")




#..................................................................
## Extract rsf coefficients ----

rsf_list <- list()

i = 1
summary(rsf[[i]])

for(i in 1:length(rsf)){
  
  #create and transpose (flip the rows/columns) a dataframe
  elev <- data.frame(t(summary(rsf[[i]])$CI["elevation (1/elevation)",]))
  elev_cov = rsf[[i]]$COV['elevation','elevation']
  dist_escape <- data.frame(t(summary(rsf[[i]])$CI["dist_escape (1/dist_escape)",])) 
  dist_escape_cov = rsf[[i]]$COV['dist_escape','dist_escape']
  
  c(elev, elev_cov, dist_escape, dist_escape_cov)
  
  res <- cbind(elev, elev_cov, dist_escape, dist_escape_cov)
  # rename the columns
  names(res) <- c("rsf_elev_min",
                  "rsf_elev_est",
                  "rsf_elev_max", 
                  "rsf_elev_cov",
                  "rsf_dist_escape_min",
                  "rsf_dist_escape_est",
                  "rsf_dist_escape_max", 
                  "rsf_dist_escape_cov")
  
  res$individual.local.identifier <- rsf[[i]]@info$identity
  
  rsf_list[[i]] <- res
}

# convert list into a dataframe
rsf_results <- do.call(rbind, rsf_list)

rsf_results <- relocate(rsf_results, individual.local.identifier, .before = rsf_elev_min)

save(rsf_results, file = "./data/rsf/fire_goat_rsf_results_20241225.rda")
load("./data/rsf/fire_goat_rsf_results_20241225.rda")


# Values near 0 = no preference
# Values below 0 = selected for, to be closer to escape terrain
# values above 0 = selected against





#______________________________________________________________________________
## Combine all results ----


# combine hr results, mean hr covariate results, rsf results into a single dataframe

# remove duplicate columns before adding
covariate_results <- subset(covariate_results, select = -individual.local.identifier)
rsf_results <- subset(rsf_results, select = -individual.local.identifier)

# combine
results_df <- cbind(hr_results, covariate_results)
results_df <- cbind(results_df, rsf_results)


#...............................................................................
## yearly rsf values i.e. mean value per year ----
# set years
results_df$year <- as.numeric(results_df$year)
data_years <- seq(from = 2019, to = 2023, by = 1)
rsf_yearly <- data.frame()

# calculate mean values for each year
for (i in 1:length(data_years)) {
  rsf_mean <- colMeans(results_df[results_df$year == data_years[i], 34:41], na.rm = TRUE)
  rsf_mean <- data.frame(year = data_years[i], t(rsf_mean)) # transpose the df as the columns/rows are flipped
  rsf_yearly <- rbind(rsf_yearly, rsf_mean)
}

# write.csv(rsf_yearly,  "./data/rsf/fire_goat_rsf_yearly_covariates.csv", row.names = FALSE)
rsf_yearly <- read.csv( "./data/rsf/fire_goat_rsf_yearly_covariates.csv")





#____________________________________________________________________
# HSF ----

library(amt)
library(terra)
library(dplyr)
library(forcats) # fct_collapse() collapse factor levels into manually defined groups
library(ggplot2)
library(broom) #tidy()

load("data/collar_data/collar_data_20241123.rda")

# load spatial covariate data (packedspatraster or spatraster object)
elev_25m = rast('data/rasters/elev_25m.tif')
dist_escape_25m = rast('data/rasters/dist_escape_25m.tif')
# reproject to lat/long crs
elev_25m <- project(elev_25m, "epsg:4326")
dist_escape_25m <-  project(dist_escape_25m, "epsg:4326")

# subset to fire goats
fire_goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
fisher <- collar_data[collar_data$goat_name %in% fire_goats,]

# convert to  track_xyz object
fisher <- make_track(fisher, 
                     .x = longitude,             # x-coordinates (projected)
                     .y = latitude,             # y-coordinates (projected)
                     .t = timestamp,     # timestamp column in POSIXct format
                     crs = 4326,        # Assuming UTM Zone 10N (adjust if necessary)
                     all_cols = TRUE     # Retain all other columns
)

# subset to single animal
dat <- fisher %>% 
  filter(goat_name == "kid_rock") %>% # subset to 1 animal
  arrange(t_) ## reorder in ascending order

summarize_sampling_rate(dat) 





#...............................................
## Generate random available points ----

# prepare rsf data and df (this takes some time to generate)
# Each row is a single point with all of its associated attributes
# 'case_' = indicator variable is equal to TRUE for the used location, & FALSE for random locations
tic()
rsf_dat <- dat %>% 
  random_points() %>% #generate random points
  extract_covariates(elev_25m) %>% # extract covariate values 
  extract_covariates(dist_escape_25m) %>% 
  mutate(elev_25m = scale(elev_25m)[, 1], # scales covariate to standardise them (ie. center them)
         dist_escape_25m = scale(dist_escape_25m)[, 1],
         weight = ifelse(case_, 1, 1e3) #add weight to background points
  )
toc()
beepr(3)





#.....................................................
## Explore sensitivity of HSF coefficients to the number of available random points ----

#Check covariate influence based on number of available random points
# i.e. find out how strongly a covariate influences animal selection when the number of random points are adjusted
# evaluate how the beta coefficent for the covariates changes (increases from 1 to 100) as number of available points per used location
n.frac <- c(1, 5, 10, 50, 100)
n.pts <- ceiling(nrow(dat) * n.frac)
n.rep <- 20


#*************I THINK THIS IS WHAT IS GOING ON WITH THIS CHUNK OF CODE, CONFIRM?
## calculate min. convex polygon around animal's location via random_points(), then
# samples x random location for every used location within this polygon
# .-. these locations are meant to capture what is assumed to be available to the animal
res1 <- tibble(
  n.pts = rep(n.pts, n.rep), 
  frac = rep(n.frac, n.rep), 
  res = map(
    n.pts, ~
      dat %>% random_points(n = .x) %>% 
      extract_covariates(elev_25m) %>% # extract covariate values 
      extract_covariates(dist_escape_25m) %>% 
      mutate(elev_25m = scale(elev_25m), # scale covariates
             dist_escape_25m = scale(dist_escape_25m),
             w = ifelse(case_, 1, 5000)) %>% # add weights
      # fit glm to predict case_ i.e. used (TRUE) or random (FALSE) locations using scaled covariates
      glm(case_ ~ elev_25m + dist_escape_25m, 
          weight = w, data = ., family = binomial()) %>% 
      tidy())) #convert to tiny format


# visualise number of points based on covariates
res1 %>% 
  unnest(cols = res) %>% #unnest the nested column i.e. list column to get at the data
  # rename
  mutate(terHSF.Lupe1 = recode(term, "(Intercept)"="Intercept",
                               elev_25m = "elevation", 
                               dist_escape_25m = "distance to escape terrain")) %>% 
  # plot
  ggplot(aes(factor(frac), y = estimate)) +
  geom_boxplot() + facet_wrap(~ terHSF.Lupe1, scale  ="free") + # distribution of estimates
  geom_jitter(alpha = 0.2) + 
  labs(x = "Number of available points (multiplier of no. of used locations)", y = "Estimate") +
  theme_light()

#****************************** DO I HAVE ENOUGH POINTS BASED ON THIS VISUALISATION?! how do i know?!?!
# ELEVATION is not decreasing like dist to esc and the intercept...at 10 pts it jumps up




# if above points are correct....then 
# Use the largest sample size here for the rest of the paper ?????
# #***************SHOULD I BE USING [,1]????
Lupe.dat <- dat %>%
  random_points(n = max(n.pts)) %>%
  extract_covariates(elev_25m) %>% # extract covariate values
  extract_covariates(dist_escape_25m) %>%
  mutate(elev_25m = scale(elev_25m),
         dist_escape_25m = scale(dist_escape_25m))
#   mutate(el_scaled = scale(elev_25m)[, 1], # Scaled elevation
#          esc_scaled = scale(dist_escape_25m)[, 1])



# visualise
#Combine observed with available points
# elevation is numerical, proportion wont work, therefore, it needs to be changed
# .-. response changed to count and changed to geom_line
Lupe.dat %>% 
  group_by(elev_25m, case_) %>% 
  summarize(n = n(), .groups = 'drop') %>% 
  ggplot(aes(x = elev_25m, y = n, color = case_, group = case_)) +
  geom_line(size = 1) +
  labs(x = "Elevation", y = "Count", color = "case_") +
  scale_color_brewer(palette = "Paired", name = "case_", 
                     breaks = c("FALSE", "TRUE"), labels = c("Available", "Used")) +
  theme_light()




#___________________________________________________________________________
# Section 3: Fitting and Interpetting Parameters in a Habitat-Selection Function (HSF)
Lupe.dat$w <- ifelse(Lupe.dat$case_, 1, 5000) # used to assign weights
# fit model
HSF.Lupe1 <- glm(case_ ~ elev_25m + dist_escape_25m, 
                 data = Lupe.dat, weight = w,
                 family = binomial(link = "logit"))

summary(HSF.Lupe1)

#************************HOW DO I COMPARE THIS TO THE CTMM RSF FIT???????
#ran summary on both and idk how to read the outputs



# Compare the relative intensity of rate of use of the two locations that differ by 1 unit of the explanatory variable but otherwise equivalent -> i.e. they should be equally accessible and have identical values for all other explanatory variables 
#******What the fuck does that even mean?! Not very helpful for those who are using this as a how to guide who don't know how to!

# modifiying for numerical values
# Availability of elevation and distance to escape terrain
#************availability of what?
a.elevation <- with(Lupe.dat[Lupe.dat$case_ == 0, ], mean(elev_25m))  # Average elevation within MCP
a.distance <- with(Lupe.dat[Lupe.dat$case_ == 0, ], mean(dist_escape_25m))  # Average distance within MCP

# Calculate relative use, adjusting for availability
exp(coef(HSF.Lupe1)["elev_25m"])*a.elevation / a.distance

# Or, comparing dist to esc to elevation (instead of elevation to dist to esc)
1/(exp(coef(HSF.Lupe1)["elev_25m"])*a.elevation/a.distance)
#-1.020324 

# 1/0.9800811 # 1.020324



#______________________________________________
# Section 4: Interactions

ggplot(Lupe.dat, aes(elev_25m, dist_escape_25m, fill=case_))+
  geom_boxplot() +    
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used"))+ 
  theme_light() 

# HSF.Lupe2 is in Section 3c: Categorical Predictors ( does not apply?)


HSF.Lupe3 <- glm(
  case_ ~ elev_25m + dist_escape_25m + elev_25m:dist_escape_25m,
  data = Lupe.dat, 
  weight=w,
  family = binomial)
summary(HSF.Lupe3)