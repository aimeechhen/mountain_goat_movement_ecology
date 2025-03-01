# window analysis results


# WINDOW RESULTS ----

## Set directory path ----
dir_path <- "./data/window_analysis/fire_goats/new_data/" 

folder_path <- paste0(dir_path, "akdes_20250219")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
AKDE <- rds_dat

# Set the folder path
folder_path <- paste0(dir_path, "fits_20250219")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
FIT <- rds_dat


folder_path <- paste0(dir_path, "covariates_20250219")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
covariates <- rds_dat


# Set the folder path
folder_path <- paste0(dir_path, "rsf_20250220")
# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
RSF <- rds_dat


#clean up environment
rm(rds_dat, rds_list, rds_files, folder_path)





#______________________________________________________________________________
# Check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)


# check fitted models units
summary_outputs <- data.frame()
for (i in seq_along(FIT)) {
  summary <- summary(FIT[[i]], units = FALSE)$CI # using SI units
  summary_outputs <- rbind(summary_outputs, 
                           data.frame(Var1 = names(table(rownames(summary))), 
                                      Freq = as.integer(table(rownames(summary)))))
}

summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
summary_outputs

# Var1 Freq
# 1             area (square meters) 2986
# 2 diffusion (square meters/second) 1488
# 3            speed (meters/second)  973
# 4                      τ (seconds)  962
# 5               τ[decay] (seconds)   11
# 6              τ[period] (seconds)   11
# 7            τ[position] (seconds)  515




#........................................................................
# Check UD units

hr_size <- data.frame()
for (i in 1:length(AKDE)) {
  cat("Processing AKDE element:", i, "\n")  # Print current loop iteration
  #extract the home range area statistics summary
  tryCatch({
    summary <- as.data.frame(summary(AKDE[[i]])$CI, units = FALSE) # default is square meters
    summary$identifier <- names(AKDE[i])
    #bind the summary to the dataframe
    hr_size <- rbind(hr_size, summary)
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}
# inspect if all the units are the same (units = km²), units = FALSE, default units are m^2, but in the for loop with units = FALSE, it changed it to km² instead of keeping it in m²
hr_size

table(sub(".*\\((.*)\\).*", "\\1", rownames(hr_size)))
# hectares square kilometers     square meters 
# 5986              6471               371 

# units are still mixed despite units = FALSE, therefore need to account for it



#____________________________________________________________________________
# create dataframe for results ----

win_results <- data.frame(goat_name = character(length(AKDE)))

for (i in 1:length(AKDE)) {
  cat("Processing AKDE element:", i, "\n")
  tryCatch({
    # extract item from list
    win_results$collar_id[i] <- AKDE[[i]]@info$identity
    # get timestamp from the list element name
    win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(AKDE)[i])
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}

library(lubridate)
win_results$window_start <- as.POSIXct(win_results$window_start, format = "%Y-%m-%d %H:%M:%S")
win_results$date <- as.Date(win_results$window_start)
win_results$year <- year(win_results$window_start)
win_results$month <- month(win_results$window_start, label = FALSE) #label = false for numerical month
win_results$day <- day(win_results$window_start)
win_results$month_day <- format(win_results$window_start, "%m-%d")
win_results$doy <- yday(win_results$window_start) #day of the year

# i <- 1

#.............................................................
# home range results (units = km^2) ----
# units are not consistent, therefore need to account for it
for (i in 1:length(AKDE)) {
  cat("Processing AKDE element:", i, "\n")
  
  tryCatch({
    
    # subset akde results for one window
    akdesum <- as.data.frame(summary(AKDE[[i]], units = FALSE)$CI)
    
    # using km^2 as default SI units, convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
    if ("area (square kilometers)" %in% rownames(akdesum)) {
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- akdesum[c("low", "est", "high")]
      
    } else if ("area (square meters)" %in% rownames(akdesum)) {
      # convert m^2 to km^2 .-. divide by 1e6
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- akdesum[c("low", "est", "high")] / 1e6    
      
    } else if ("area (hectares)" %in% rownames(akdesum)) { 
      # convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- akdesum[c("low", "est", "high")] / 100
      
    } else { 
      cat("no entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
      win_results[i, c("hr_min", "hr_est", "hr_max")] <- NA
    }
    
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}





#___________________________________________________________________________
# movement results ----

i <- 3
FIT <- FITS[1:100]


# win_results <- data.frame(goat_name = character(length(FIT)))

tic()
for (i in 1:length(FIT)) {
  # for (i in 1:min(100, length(FIT))) { # testing with subset
  
  # extract item from list
  win_results$goat_name[i] <- FIT[[i]]@info$identity
  # get timestamp from the list element name
  win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(FIT)[[i]])
  
  
  win_results$movement_model[i] <- summary(FIT[[i]], units = FALSE)$name
  win_results[i, c("DOF_mean", "DOF_area", "DOF_diffusion", "DOF_speed")] <- summary(FIT[[i]], units = FALSE)$DOF[c("mean", "area", "diffusion", "speed")]
  
  
  #..............................................
  # diffusion ----
  
  # si units has it m^2/sec, convert to km^2/day 0.0864 or 8.64e-2
  if ("diffusion (square meters/second)" %in% rownames(summary(FIT[[i]], units = FALSE)$CI))  {
    # Update the corresponding row in the data frame
    win_results[i, c("diffusion_min_km2_day",
                     "diffusion_est_km2_day",
                     "diffusion_max_km2_day")] <- summary(FIT[[i]], units = FALSE)$CI["diffusion (square meters/second)",
                                                                                      c("low", "est", "high")] * 0.0864
    
    
  } else {
    cat("no diffusion entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("diffusion_min_km2_day", "diffusion_est_km2_day", "diffusion_max_km2_day")] <- NA
  }
  
  
  
  
  #..............................................
  # tau_p ----
  # si units are seconds, convert into days
  if ("τ[position] (seconds)" %in% rownames(summary(FIT[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_p_min_s",
                     "tau_p_est_s",
                     "tau_p_max_s")] <- summary(FIT[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                            c("low", "est", "high")]
    
    #convert to seconds to days
    win_results[i, c("tau_p_min_day",
                     "tau_p_est_day",
                     "tau_p_max_day")] <- summary(FIT[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                              c("low", "est", "high")] / 86400
    
    
    
  } else {
    cat("no tau p entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_p_min_s", "tau_p_est_s", "tau_p_max_s")] <- NA
    win_results[i, c("tau_p_min_day", "tau_p_est_day", "tau_p_max_day")] <- NA
  }
  
  
  
  #..............................................
  # tau_v ----
  # si units are seconds, convert to minutes
  if ("τ[velocity] (seconds)" %in% rownames(summary(FIT[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_v_min_s",
                     "tau_v_est_s",
                     "tau_v_max_s")] <- summary(FIT[[i]], units = FALSE)$CI["τ[velocity] (seconds)",
                                                                            c("low", "est", "high")]
    #convert to seconds to minutes
    win_results[i, c("tau_v_min_min", 
                     "tau_v_est_min", 
                     "tau_v_max_min")] <- summary(FIT[[i]], units = FALSE)$CI["τ[velocity] (seconds)", 
                                                                              c("low", "est", "high")] / 60
    
  } else { 
    cat("no tau v entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_v_min_s", "tau_v_est_s", "tau_v_max_s")] <- NA
    win_results[i, c("tau_v_min_min", "tau_v_est_min", "tau_v_max_min")] <- NA
  }
  
  
  fit_mu <- as.data.frame(FIT[[i]]$mu)
  centroid_sf <- st_as_sf(fit_mu, coords = c('x', 'y'), crs = FIT[[i]]@info$projection)
  centroid_sf <- st_coordinates(st_transform(centroid_sf, crs = 4326)) # reproject and grab the coordinates
  win_results[i, c("centroid_long", "centroid_lat")] <- centroid_sf[1, c("X", "Y")]
  
  
  # bls ----
  
  # If tau_p and tau_v have values, then calculate BLS
  if (!is.na(win_results$tau_p_est_s[i]) && !is.na(win_results$tau_v_est_s[i])) {
    sigma_p <- ctmm:::area.covm(FITS[[i]]$sigma)
    
    bls_min <- sqrt((win_results$tau_v_min_s[i] / win_results$tau_p_min_s[i]) * sigma_p)
    bls_est <- sqrt((win_results$tau_v_est_s[i] / win_results$tau_p_est_s[i]) * sigma_p)
    bls_max <- sqrt((win_results$tau_v_max_s[i] / win_results$tau_p_max_s[i]) * sigma_p)
    
    win_results[i, c("bls_min", "bls_est", "bls_max")] <- c(bls_min, bls_est, bls_max)
    
  } else {
    cat("no bls entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("bls_min", "bls_est", "bls_max")] <- NA
  }
  
  
  # end of loop
  
}

toc() #~4min




