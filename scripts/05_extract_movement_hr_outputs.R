
# Extract movement and home range results

library(ctmm)
library(tictoc)


# data import ----
load("./data/movement_model/fits_20250505.rda")
load("./data/movement_model/speed_mean_20250505.rda")
load("./data/movement_model/speeds_insta_20250505.rda")
load("./data/home_range/akdes_20250505.rda")



#______________________________________________________________________________
# check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)

# inspect one summary output of movement model (using concise units)
fitsum <- summary(FITS[[1]])
fitsum
# CI units:
# area (square kilometers)
# τ[position] (months)       
# τ[velocity] (minutes)   
# speed (kilometers/day)  
# diffusion (hectares/day)

fitsum <- summary(FITS[[1]], units = FALSE) # units = FALSE, using SI units
fitsum <- summary(FITS[[1]], units = FALSE)$CI
fitsum
# CI units:
# area (square meters)
# τ[position] (seconds)       
# τ[velocity] (seconds)   
# speed (meters/second)  
# diffusion (square meters/second)


#........................................
## check units to see if theyre all the same

i <- 1


# check fitted models units
summary_outputs <- data.frame()
for (i in seq_along(FITS)) {
  summary <- summary(FITS[[i]], units = FALSE)$CI # using SI units
  summary_outputs <- rbind(summary_outputs, 
                           data.frame(Var1 = names(table(rownames(summary))), 
                                      Freq = as.integer(table(rownames(summary)))))
}

summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
summary_outputs

# Var1 Freq
# 1             area (square meters)   35
# 2 diffusion (square meters/second)   35
# 3            speed (meters/second)   12
# 4            τ[position] (seconds)   35
# 5            τ[velocity] (seconds)   12



# if they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections

#clean environment
rm(summary, summary_outputs)



#........................................................................
# Check UD units

hr_size <- data.frame()
for (i in 1:length(AKDES)) {
  cat("Processing AKDE element:", i, "\n")  # Print current loop iteration
  #extract the home range area statistics summary
  tryCatch({
    summary <- as.data.frame(summary(AKDES[[i]])$CI, units = FALSE) # default is square meters
    summary$identifier <- names(AKDES[i])
    #bind the summary to the dataframe
    hr_size <- rbind(hr_size, summary)
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}
# inspect if all the units are the same (units = km²), units = FALSE, default units are m^2, but in the for loop with units = FALSE, it changed it to km² instead of keeping it in m²
hr_size

# inspect if any of them have different values in the brackets and create a table summing them
table(sub(".*\\((.*)\\).*", "\\1", rownames(hr_size)))
# square kilometers 
# 35 



#...................................................................
# prep results dataframe ----
#...................................................................

#build results dataframe
# RESULTS <- data.frame(goat_name = names(FITS))
# RESULTS$collar_id <- collar_data$collar_id[match(RESULTS$goat_name, collar_data$goat_name)]
# RESULTS$goat_id <- collar_data$goat_id[match(RESULTS$goat_name, collar_data$goat_name)]
# RESULTS <- RESULTS[, c("goat_name", "goat_id", "collar_id")]

RESULTS <- data.frame(collar_id = character(length(FITS)), 
                      year = character(length(FITS)))

# create skeleton df to be filled in
for (i in 1:length(FITS)) {
  cat("Processing FITS element:", i, "\n")
  tryCatch({
    # Extract collar_id and year from names(FITS)
    RESULTS$collar_id[i] <- substr(names(FITS)[i], 1, 5)
    RESULTS$year[i] <- gsub(".*_", "", names(FITS)[i])
    
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}

# # Import supplementary mountain goat info 
# goat_info <- read.csv("data/goat_info.csv")
# goat_info$collar_id <- as.factor(goat_info$collar_id)
# # subset to only the fire goats
# goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
# goat_info <- goat_info[goat_info$goat_name %in% goats,]







#///////////////////////////////////////////////////////////
# Extract results ----
#///////////////////////////////////////////////////////////

# Note: Using meta() from ctmm is meant for population level, group level purposes, not individual animals
# variables can only be “area”, “diffusion”, “speed”, “tauposition”, “tauvelocity”, “distance”
# for individual, get their results from their individual summary


#.............................................................
## home range results (units = km^2) ----
#.............................................................

# if units are not consistent, therefore need to account for it
for (i in 1:length(AKDES)) {
  cat("Processing AKDE element:", i, "\n")
  
  tryCatch({
    
    # subset akde results for one window
    akdesum <- as.data.frame(summary(AKDES[[i]], units = FALSE)$CI)
    
    # using km^2 as default SI units, convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
    if ("area (square kilometers)" %in% rownames(akdesum)) {
      RESULTS[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- akdesum[c("low", "est", "high")]
      
    } else if ("area (square meters)" %in% rownames(akdesum)) {
      # convert m^2 to km^2 .-. divide by 1e6
      RESULTS[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- akdesum[c("low", "est", "high")] / 1e6    
      
    } else if ("area (hectares)" %in% rownames(akdesum)) { 
      # convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
      RESULTS[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")]<- akdesum[c("low", "est", "high")] / 100
      
    } else { 
      cat("no home range entry found for", "\n")
      RESULTS[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- NA
    }
    
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using summary()
# extract then check method
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





#...........................................................
# movement models results ----
#...........................................................

# extracting movement results all at once instead of sections independently
# adding results to the already built results df from above

tic(msg = "movement results")
for (i in 1:length(FITS)) {
  # for (i in 1:min(100, length(FIT))) { # testing with subset
  
  RESULTS$movement_model[i] <- summary(FITS[[i]], units = FALSE)$name
  RESULTS[i, c("DOF_mean", "DOF_area", "DOF_diffusion", "DOF_speed")] <- summary(FITS[[i]], units = FALSE)$DOF[c("mean", "area", "diffusion", "speed")]
  
  
  #..............................................
  ## diffusion
  
  # si units has it m^2/sec, convert to km^2/day 0.0864 or 8.64e-2
  if ("diffusion (square meters/second)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI))  {
    # Update the corresponding row in the data frame
    RESULTS[i, c("diffusion_low_km2_day",
                 "diffusion_est_km2_day",
                 "diffusion_high_km2_day")] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)",
                                                                                   c("low", "est", "high")] * 0.0864
    
    
  } else {
    cat("no diffusion entry found", "\n")
    RESULTS[i, c("diffusion_low_km2_day", "diffusion_est_km2_day", "diffusion_high_km2_day")] <- NA
  }
  
  
  
  
  #..............................................
  ## tau_p
  # tau-p is the ~time it takes to cross your home range
  # si units are seconds, convert into days
  if ("τ[position] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    RESULTS[i, c("tau_p_low_s",
                 "tau_p_est_s",
                 "tau_p_high_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                         c("low", "est", "high")]
    
    #convert to seconds to days
    RESULTS[i, c("tau_p_low_day",
                 "tau_p_est_day",
                 "tau_p_high_day")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                           c("low", "est", "high")] / 86400
    
    
    
  } else {
    cat("no tau p entry", "\n")
    RESULTS[i, c("tau_p_low_s", "tau_p_est_s", "tau_p_high_s")] <- NA
    RESULTS[i, c("tau_p_low_day", "tau_p_est_day", "tau_p_high_day")] <- NA
  }
  
  
  
  #..............................................
  ## tau_v
  # tau-p is the ~time it takes to cross your home range
  # si units are seconds, convert to minutes
  if ("τ[velocity] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    RESULTS[i, c("tau_v_low_s",
                 "tau_v_est_s",
                 "tau_v_high_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)",
                                                                         c("low", "est", "high")]
    #convert to seconds to minutes
    RESULTS[i, c("tau_v_low_min", 
                 "tau_v_est_min", 
                 "tau_v_high_min")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)", 
                                                                           c("low", "est", "high")] / 60
    
  } else { 
    cat("no tau v entry", "\n")
    RESULTS[i, c("tau_v_low_s", "tau_v_est_s", "tau_v_high_s")] <- NA
    RESULTS[i, c("tau_v_low_min", "tau_v_est_min", "tau_v_high_min")] <- NA
  }
  
  
  
  
  #..............................................
  ## bls
  
  # If tau_p and tau_v have values, then calculate BLS
  if (!is.na(RESULTS$tau_p_est_s[i]) && !is.na(RESULTS$tau_v_est_s[i])) {
    sigma_p <- ctmm:::area.covm(FITS[[i]]$sigma)
    
    bls_low <- sqrt((RESULTS$tau_v_low_s[i] / RESULTS$tau_p_low_s[i]) * sigma_p)
    bls_est <- sqrt((RESULTS$tau_v_est_s[i] / RESULTS$tau_p_est_s[i]) * sigma_p)
    bls_high <- sqrt((RESULTS$tau_v_high_s[i] / RESULTS$tau_p_high_s[i]) * sigma_p)
    
    RESULTS[i, c("bls_low", "bls_est", "bls_high")] <- c(bls_low, bls_est, bls_high)
  } else {
    cat("no bls entry found", "\n")
    RESULTS[i, c("bls_low", "bls_est", "bls_high")] <- NA
  }
  
  
  # end of loop
  
}

toc() #~4min



#....................................................
# Supplementary mountain goat information ----
#....................................................


# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
# add supplementary info to df
RESULTS <- merge(RESULTS, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
RESULTS <- dplyr::relocate(RESULTS, c("collar_id","goat_name", "goat_id"), .before = year)



# SAVE RESULTS ----
write.csv(RESULTS, file = "./data/combined_data_movement_hr_results_20250505.csv", row.names = FALSE)
RESULTS <- read.csv("./data/combined_data_movement_hr_results_20250505.csv")








#/////////////////////////////////////////////////////////////////////////
# Single results ----
#/////////////////////////////////////////////////////////////////////////



#get movement model type that was best fit
model_type <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$name
  model_type <- rbind(model_type, summary)
}
names(model_type)[1] <- "movement_model"

model_type




#...........................................
# Get movement models degrees of freedom

DOF_results <- data.frame()
for (i in 1:length(FITS)) {
  summary <- summary(FITS[[i]])$DOF
  DOF_results <- rbind(DOF_results, summary)
}
names(DOF_results)[1] <- "DOF_mean"
names(DOF_results)[2] <- "DOF_area"
names(DOF_results)[3] <- "DOF_diffusion"
names(DOF_results)[4] <- "DOF_speed"

DOF_results



#...........................................
#Get diffusion estimates

diffusion_results <- data.frame(
  diffusion_low_m2_s = numeric(length(FITS)),
  diffusion_est_m2_s = numeric(length(FITS)),
  diffusion_high_m2_s = numeric(length(FITS))
)

# #Get diffusion values (units = square meters/second)
# units = FALSE removes the units that FITS has assigned and then by default it is in m^2/second
for (i in seq_along(FITS)) {
  # Check if "diffusion (square meters/second)" is present in the row names
  if ("diffusion (square meters/second)" %in% row.names(summary(FITS[[i]], units = FALSE)$CI)) {
    # Update the corresponding row in the data frame
    diffusion_results[i, "diffusion_low_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 1]
    diffusion_results[i, "diffusion_est_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 2]
    diffusion_results[i, "diffusion_high_m2_s"] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)", 3]
  }
}

# # Convert m^2/second into km^2/day
diffusion_results$diffusion_low_km2_day <- diffusion_results$diffusion_low_m2_s * 0.0864
diffusion_results$diffusion_est_km2_day <- diffusion_results$diffusion_est_m2_s * 0.0864
diffusion_results$diffusion_high_km2_day <- diffusion_results$diffusion_high_m2_s * 0.0864

diffusion_results


#.........................................................
# Get position and velocity (tau p and tau v)
# OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)

# tau-p is the ~time it takes to cross your home range

## Get tau p estimates
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
names(tau_p_results)[1] <- "tau_p_low_days"
names(tau_p_results)[2] <- "tau_p_est_days"
names(tau_p_results)[3] <- "tau_p_high_days"
rownames(tau_p_results) <- NULL

tau_p_results



#............................................
## Get tau v estimates

# tau-v is how long (in time, not distance) you're going in a straight line before turning, for far you're going before turning is ballistic length scales (bls) which uses tau-v in its calculation
# high tau-v value = higher the time spent going in a straight light

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
names(tau_v_results)[1] <- "tau_v_low_minutes"
names(tau_v_results)[2] <- "tau_v_est_minutes"
names(tau_v_results)[3] <- "tau_v_high_minutes"
rownames(tau_v_results) <- NULL

tau_v_results





#......................................................
# get mean speed
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
names(ci_results)[1] <- "speed_mean_low_ms"
names(ci_results)[2] <- "speed_mean_est_ms"
names(ci_results)[3] <- "speed_mean_high_ms"
rownames(dof_results) <- NULL
rownames(ci_results) <- NULL

# combine the two
speed_mean_results <- cbind(dof_results, ci_results)
speed_mean_results




#..........................................
# get insta speed
# Extract insta speed values ****(units = meters/sec) (time-averaged speed, proportional to distance travelled)
# OU model causes 'Inf' because not enough data to estimate**** check

speed_insta_results <- data.frame(
  speed_insta_low = numeric(length(FITS)),
  speed_insta_est = numeric(length(FITS)),
  speed_insta_high = numeric(length(FITS))
)


# SPEEDS_INSTA <- readRDS(file = "./data/movement_model/goat_speeds_insta_20241108.rda")
# SPEEDS_INSTA # Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value 



#..........................................
# combine each component together into a df ----


#build results dataframe
fits_results <- data.frame(goat_name = names(FITS))
fits_results$collar_id <- collar_data$collar_id[match(fits_results$goat_name, collar_data$goat_name)]
fits_results$goat_id <- collar_data$goat_id[match(fits_results$goat_name, collar_data$goat_name)]
fits_results <- fits_results[, c("goat_name", "goat_id", "collar_id")]

# combine parts
fits_results <- cbind(fits_results, model_type)
fits_results <- cbind(fits_results, DOF_results)
fits_results <- cbind(fits_results, diffusion_results)
fits_results <- cbind(fits_results, tau_p_results)
fits_results <- cbind(fits_results, tau_v_results)
fits_results <- cbind(fits_results, speed_mean_results)


#save df
save(fits_results, file = "./data/movement_model/goat_fits_summary_20241226.rda")
# load("./data/movement_model/goat_fits_summary_20241226.rda")







#/////////////////////////////////////////
# Using the meta() ----
#/////////////////////////////////////////

#using the meta() from ctmm
# meant for population level, group level purposes, not individual animals
# variables can only be “area”, “diffusion”, “speed”, “tauposition”, “tauvelocity”, “distance”

# get mean home range size (km^2)

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
names(hr_size)[1] <- "mean_hr_low_km2"
names(hr_size)[2] <- "mean_hr_est_km2"
names(hr_size)[3] <- "mean_hr_high_km2"
rownames(hr_size) <- NULL




#............
# using meta()
diffusion_results <- data.frame()

for (i in 1:length(FITS)) {
  # get hr size 
  res <- as.data.frame(ctmm::meta(FITS[i], units = FALSE, variable = "diffusion"))
  #hr <- as.data.frame(ctmm::meta(AKDES[i]), units = FALSE)
  # subset to first row
  res <- res[1,]
  # add to hr_size df
  diffusion_results <- rbind(diffusion_results, res)
}





#............
# using meta()
tau_p_results <- data.frame()

for (i in 1:length(FITS)) {
  # get hr size 
  res <- as.data.frame(ctmm::meta(FITS[i], units = FALSE, variable = "tauposition"))
  #hr <- as.data.frame(ctmm::meta(AKDES[i]), units = FALSE)
  # subset to first row
  res <- res[1,]
  # add to hr_size df
  tau_p_results <- rbind(tau_p_results, res)
}





#............
# using meta()
tau_v_results <- data.frame()

for (i in 1:length(FITS)) {
  # get hr size 
  res <- as.data.frame(ctmm::meta(FITS[i], units = FALSE, variable = "tauvelocity"))
  #hr <- as.data.frame(ctmm::meta(AKDES[i]), units = FALSE)
  # subset to first row
  res <- res[1,]
  # add to hr_size df
  tau_v_results <- rbind(tau_v_results, res)
}

