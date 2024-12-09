
# movement models and home range estimation

library(ctmm)
library(tictoc)

load("data/collar_data/collar_data_20240703.rda")

#format names to match required for ctmm based on Movebank critera:
collar_data = plyr::rename(collar_data, c('collar_id' = 'individual.local.identifier',
                                          'latitude' = 'location.lat', 
                                          'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(collar_data, mark.rm = TRUE)

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



# save(FITS,file="data/goat_fits_20241107.rda")
load("data/goat_fits_20241107.rda")

#...................................................................
# Estimate various speed types

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
END <- Sys.time()

saveRDS(SPEED_MEAN, file = "./data/goat_speed_mean_20241108.rda")
saveRDS(SPEEDS_INSTA, file = "./data/goat_speeds_insta_20241108.rda")








#.......................................................
# Extract FITS movement model summary ----

# inspect one summary output of movement model
fitsum <- summary(FITS[[1]])
# units:
# τ[position] (days)       
# τ[velocity] (minutes)   
# speed (kilometers/day)  
# diffusion (hectares/day)


#build results dataframe
SUMMARY_FIT <- data.frame()
collar_id <- names(FITS)

#get movement model that was best fit
model_type <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$name
  model_type <- rbind(model_type, summary)
}
names(model_type)[1] <- "movement_model"

SUMMARY_FIT <- cbind(collar_id, model_type)



#Get degrees of freedom
DOF_results <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$DOF
  DOF_results <- rbind(DOF_results, summary)
}
names(DOF_results)[1] <- "DOF_mean"
names(DOF_results)[2] <- "DOF_area"
names(DOF_results)[3] <- "DOF_diffusion"
names(DOF_results)[4] <- "DOF_speed"

SUMMARY_FIT <- cbind(SUMMARY_FIT, DOF_results)



#Get diffusion estimates ----
diffusion_results <- data.frame(
  diffusion_min = numeric(length(FITS)),
  diffusion_est = numeric(length(FITS)),
  diffusion_max = numeric(length(FITS))
)

# #Get diffusion values (units = square meters/second)
# units = FALSE removes the units that FITS has assigned and then by default it is in m^2/second
for (i in seq_along(FITS)) {
  # Check if "diffusion (square meters/second)" is present in the row names
  if ("diffusion (square meters/second)" %in% row.names(summary(FITS[[i]], units = FALSE)$CI)) {
    # Update the corresponding row in the data frame
    diffusion_results[i, "diffusion_min"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 1]
    diffusion_results[i, "diffusion_est"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 2]
    diffusion_results[i, "diffusion_max"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 3]
  }
}

# # Convert m^2/second into km^2/day
diffusion_results$diffusion_min_km2_day <- diffusion_results$diffusion_min * 0.0864
diffusion_results$diffusion_est_km2_day <- diffusion_results$diffusion_est * 0.0864
diffusion_results$diffusion_max_km2_day <- diffusion_results$diffusion_max * 0.0864

SUMMARY_FIT <- cbind(SUMMARY_FIT, diffusion_results)

#.........................................................
# Extract position and velocity (tau p and tau v) ----
# OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)

#Get tau p estimates
tau_p_results <- data.frame(
  tau_p_min = numeric(length(FITS)),
  tau_p_est = numeric(length(FITS)),
  tau_p_max = numeric(length(FITS))
)

#τ[position] (days) 
for (i in seq_along(FITS)) {
  summary <- summary(FITS[[i]])$CI
  tau_p_results$tau_p_min[i] <- summary[grepl("position", row.names(summary)), 1]  
  tau_p_results$tau_p_est[i] <- summary[grepl("position", row.names(summary)), 2]
  tau_p_results$tau_p_max[i] <- summary[grepl("position", row.names(summary)), 3]
}

SUMMARY_FIT <- cbind(SUMMARY_FIT, tau_p_results)


#Get tau v estimates
tau_v_results <- data.frame(
  tau_v_min = numeric(length(FITS)),
  tau_v_est = numeric(length(FITS)),
  tau_v_max = numeric(length(FITS))
)

#τ[velocity] (minutes) 
for (i in seq_along(FITS)) {
  summary <- summary(FITS[[i]])$CI
  tau_v_results$tau_v_min[i] <- summary[grepl("velocity", row.names(summary)), 1] 
  tau_v_results$tau_v_est[i] <- summary[grepl("velocity", row.names(summary)), 2]
  tau_v_results$tau_v_max[i] <- summary[grepl("velocity", row.names(summary)), 3]
}

SUMMARY_FIT <- cbind(SUMMARY_FIT, tau_v_results)





#......................................................
# get mean speed
# Extract mean speed values (units = meters/sec) (time-averaged speed, proportional to distance travelled)
# OU model causes 'Inf' because not enough data to estimate
speed_mean_results <- data.frame(
  speed_mean_min = numeric(length(FITS)),
  speed_mean_est = numeric(length(FITS)),
  speed_mean_max = numeric(length(FITS))
)
for (i in 1:length(SPEED_MEAN)) {
  speed_mean_results$speed_mean_min[i] <- SPEED_MEAN[[i]]$CI[[1]]
  speed_mean_results$speed_mean_est[i] <- SPEED_MEAN[[i]]$CI[[2]]
  speed_mean_results$speed_mean_max[i] <- SPEED_MEAN[[i]]$CI[[3]]
  
}

SUMMARY_FIT <- cbind(SUMMARY_FIT, speed_mean_results)


#save df
save(SUMMARY_FIT, file = "./data/goat_fits_summary_20241108.rda")


#..........................................
#..........................................

# get insta speed
# Extract insta speed values ****(units = meters/sec) (time-averaged speed, proportional to distance travelled)
# OU model causes 'Inf' because not enough data to estimate**** check

speed_insta_results <- data.frame(
  speed_insta_min = numeric(length(FITS)),
  speed_insta_est = numeric(length(FITS)),
  speed_insta_max = numeric(length(FITS))
)

SPEEDS_INSTA # Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value 

