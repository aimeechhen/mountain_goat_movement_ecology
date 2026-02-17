# window analysis results

library(ctmm)
library(sf)
library(ggh4x)
library(gridExtra)
library(lubridate)
library(tictoc)

# WINDOW RESULTS ----

## Set directory path ----
# dir_path <- "./data/window_analysis/original_data/" 
# dir_path <- "./data/window_analysis/new_data/"
# dir_path <- "./data/window_analysis/full_6_combined_data/"
# dir_path <- "./data/window_analysis/fire_period/"
# dir_path <- "./data/window_analysis/fire_period_all_years/"
dir_path <- "./data/moving_window/"
dir_path <- "./data/window_analysis (old, do not use)/original_data/covariates_20250118/"


# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "akdes.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
AKDES <- rds_dat

# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "fits.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
FITS <- rds_dat


# Load .shp, .tif etc files within a folder including all the subfolders, remember these are saved as df
rds_files <- list.files(dir_path, pattern = "covariates.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# # combine together as one df
rds_dat <- do.call(rbind, rds_list)
covariates <- rds_dat

# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "rsf.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
RSF <- rds_dat

# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "mean_speed.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
mean_speed <- rds_dat

# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "insta_speed.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
insta_speed <- rds_dat


#clean up environment
rm(rds_dat, rds_list, rds_files)

#______________________________________________________________________________
# Check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)


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

# speed = distance travelled over time (from a to b)
# diffusion = area travelled over time (area/space when going from a to b)

# Var1 Freq
# 1             area (square meters) 3350
# 2 diffusion (square meters/second) 1825
# 3            speed (meters/second) 1278
# 4                      τ (seconds) 1250
# 5               τ[decay] (seconds)   26
# 6              τ[period] (seconds)   26
# 7            τ[position] (seconds)  549
# 8            τ[velocity] (seconds)    2




#........................................................................
# Check UD units

hr_size <- data.frame()
for (i in 1:length(AKDES)) {
  # cat("Processing AKDE element:", i, "\n")  # Print current loop iteration
  #extract the home range area statistics summary
  tryCatch({
    summary <- as.data.frame(summary(AKDES[[i]], units = FALSE)$CI) # default is square meters
    summary$identifier <- names(AKDES[i])
    #bind the summary to the dataframe
    hr_size <- rbind(hr_size, summary)
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}
# inspect if all the units are the same (units = km²), 
#units = FALSE, default units are m^2, 
# units = TRUE -> km² instead of keeping it in m²
hr_size

table(sub(".*\\((.*)\\).*", "\\1", rownames(hr_size)))
# square meters 
# 3350 


#____________________________________________________________________________
# create dataframe for results ----

#prepare df
win_results <- data.frame(collar_id = rep(NA_character_, length(FITS)),
                          window_start = as.POSIXct(rep(NA, length(FITS))),
                          date = as.Date(rep(NA, length(FITS))),
                          year = rep(NA_integer_, length(FITS)),
                          month = rep(NA_integer_, length(FITS)),
                          day = rep(NA_integer_, length(FITS)),
                          month_day = rep(NA_character_, length(FITS)),
                          doy = rep(NA_integer_, length(FITS)))

tic()
for (i in 1:length(FITS)) {
  # for (i in 1:min(100, length(FIT))) { # testing with subset
  
  # track progress every 500 iterations
  if (i %% 500 == 0) {
    cat("completed", i, "iterations \n")
  }
  
  # fill in basic info into the df before extracting results
  
  # extract item from list
  win_results$collar_id[i] <- FITS[[i]]@info$identity
  
  # extract timestamp from the list's element name
  win_timestamp <- sub(".*_(\\d{4}-\\d{2}-\\d{2}(?: \\d{2}:\\d{2}:\\d{2})?)", "\\1", names(FITS)[i])
  
  # some may only have date and not date and time so
  if (grepl("\\d{2}:\\d{2}:\\d{2}", win_timestamp)) {
    # if it contains both then format it as a timestamp
    win_results$window_start[i] <- as.POSIXct(win_timestamp, format = "%Y-%m-%d %H:%M:%S")
  } else {
    # if it only contains date, then format it as date
    win_results$window_start[i] <- as.POSIXct(win_timestamp, format = "%Y-%m-%d")
  }
  
  # data carpentry
  win_results$date[i] <- as.Date(win_results$window_start[i])
  win_results$year[i] <- year(win_results$window_start[i])
  win_results$month[i] <- month(win_results$window_start[i], label = FALSE)
  win_results$day[i] <- day(win_results$window_start[i])
  win_results$month_day[i] <- format(win_results$window_start[i], "%m-%d")
  win_results$doy[i] <- yday(win_results$window_start[i])
  
  
  #.............................................................
  #/// home range results (units = m^2) ----
  
  # check if they match first
  if (!is.null(AKDES[[i]]) && FITS[[i]]@info$identity == AKDES[[i]]@info$identity) {
    
  #if they match then extract home range results
    tryCatch({
      akdesum <- as.data.frame(summary(AKDES[[i]], units = FALSE)$CI)
      
      if ("area (square kilometers)" %in% rownames(akdesum)) {
        win_results[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- akdesum[c("low", "est", "high")]
        
      } else if ("area (square meters)" %in% rownames(akdesum)) {
        win_results[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- akdesum[c("low", "est", "high")] / 1e6
        
      } else if ("area (hectares)" %in% rownames(akdesum)) {
        win_results[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- akdesum[c("low", "est", "high")] / 100
        
      } else {
        cat("no area entry found for", win_results$collar_id[i], win_results$window_start[i], "\n")
        win_results[i, c("hr_low_km2", "hr_est_km2", "hr_high_km2")] <- NA
      }
    }, error = function(e) {
      cat("Error extracting AKDE for index", i, ":", conditionMessage(e), "\n")
    })
  }
  
  #.............................................................
  #/// movement metrics results ----
  
    win_results$movement_model[i] <- summary(FITS[[i]], units = FALSE)$name
  win_results[i, c("DOF_mean", "DOF_area", "DOF_diffusion", "DOF_speed")] <- summary(FITS[[i]], units = FALSE)$DOF[c("mean", "area", "diffusion", "speed")]
  
  
  #..............................................
  ## diffusion ----
  
  # si units has it m^2/sec, convert to km^2/day 0.0864 or 8.64e-2
  if ("diffusion (square meters/second)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI))  {
    # Update the corresponding row in the data frame
    win_results[i, c("diffusion_low_km2_day",
                     "diffusion_est_km2_day",
                     "diffusion_high_km2_day")] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)",
                                                                                        c("low", "est", "high")] * 0.0864
    
  } else {
    # cat("no diffusion entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("diffusion_low_km2_day", "diffusion_est_km2_day", "diffusion_high_km2_day")] <- NA
  }
  
  
  
  
  #..............................................
  # tau_p ----
  # si units are seconds, convert into days
  if ("τ[position] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_p_low_s",
                     "tau_p_est_s",
                     "tau_p_high_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                              c("low", "est", "high")]
    
    #convert to seconds to days
    win_results[i, c("tau_p_low_day",
                     "tau_p_est_day",
                     "tau_p_high_day")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                                c("low", "est", "high")] / 86400
    
    
    
  } else {
    # cat("no tau p entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_p_low_s", "tau_p_est_s", "tau_p_high_s")] <- NA
    win_results[i, c("tau_p_low_day", "tau_p_est_day", "tau_p_high_day")] <- NA
  }
  
  
  
  #..............................................
  # tau_v ----
  # si units are seconds, convert to minutes
  if ("τ[velocity] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_v_low_s",
                     "tau_v_est_s",
                     "tau_v_high_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)",
                                                                              c("low", "est", "high")]
    #convert to seconds to minutes
    win_results[i, c("tau_v_low_min", 
                     "tau_v_est_min", 
                     "tau_v_high_min")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)", 
                                                                                c("low", "est", "high")] / 60
    
  } else { 
    # cat("no tau v entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_v_low_s", "tau_v_est_s", "tau_v_high_s")] <- NA
    win_results[i, c("tau_v_low_min", "tau_v_est_min", "tau_v_high_min")] <- NA
  }
  
  
  fit_mu <- as.data.frame(FITS[[i]]$mu)
  centroid_sf <- st_as_sf(fit_mu, coords = c('x', 'y'), crs = FITS[[i]]@info$projection)
  centroid_sf <- st_coordinates(st_transform(centroid_sf, crs = 4326)) # reproject and grab the coordinates
  win_results[i, c("centroid_long", "centroid_lat")] <- centroid_sf[1, c("X", "Y")]
  
  
  # bls ----
  
  # If tau_p and tau_v have values, then calculate BLS
  if (!is.na(win_results$tau_p_est_s[i]) && !is.na(win_results$tau_v_est_s[i])) {
    sigma_p <- ctmm:::area.covm(FITS[[i]]$sigma) # area.covm sigma  = get geometric area/volume
    
    bls_low <- sqrt((win_results$tau_v_low_s[i] / win_results$tau_p_low_s[i]) * sigma_p)
    bls_est <- sqrt((win_results$tau_v_est_s[i] / win_results$tau_p_est_s[i]) * sigma_p)
    bls_high <- sqrt((win_results$tau_v_high_s[i] / win_results$tau_p_high_s[i]) * sigma_p)
    
    win_results[i, c("bls_low", "bls_est", "bls_high")] <- c(bls_low, bls_est, bls_high)
    
  } else {
    # cat("no bls entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("bls_low", "bls_est", "bls_high")] <- NA
  }
  
  
  # end of loop
  
}

toc() #~44secs


# check for NAs
sapply(win_results, function(x) sum(is.na(x))) 


# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.character(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
win_results <- merge(win_results, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
win_results <- dplyr::relocate(win_results, c("collar_id","goat_name", "goat_id"), .before = window_start)


#.........................................................................
#/// covariates results ----

# check for NA values
sapply(covariates, function(x) sum(is.na(x))) 

#data carpentry
covariates$collar_id <- as.character(covariates$collar_id)
unique(covariates$collar_id)
#  "30548" ""      "30561" "30575" "30613" "30642" "30648"
# check how many of them have ""
sum(covariates$collar_id == "", na.rm = TRUE) # 11178 - 7773 = 3405
# [1] 7773
# drop those blank rows
covariates <- covariates[covariates$collar_id != "", ]
# recheck for NAs
sapply(covariates, function(x) sum(is.na(x))) 
# those NAs were dropped as well
# there isnt a FITS/AKDES for every covariate row, only want covariates that had fitted movement models and akdes in the window

# add covariate data to results df
win_results <- merge(win_results, covariates, by = c("collar_id", "window_start"),
             all.x = TRUE)
# check for NAs
sapply(win_results, function(x) sum(is.na(x))) 





write.csv(win_results, file = "./data/moving_window/combined_data_window_results_20250902.csv", row.names = FALSE)
win_results <- read.csv("./data/moving_window/combined_data_window_results_20250902.csv")
