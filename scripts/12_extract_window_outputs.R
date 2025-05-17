# window analysis results

library(ctmm)
library(sf)
library(ggh4x)
library(gridExtra)
library(lubridate)

# WINDOW RESULTS ----

## Set directory path ----
# dir_path <- "./data/window_analysis/original_data/" 
# dir_path <- "./data/window_analysis/new_data/"
# dir_path <- "./data/window_analysis/full_6_combined_data/"
dir_path <- "./data/window_analysis/fire_period/"
# dir_path <- "./data/window_analysis/fire_period_all_years/"



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

# # Load .shp, .tif etc files within a folder including all the subfolders
# rds_files <- list.files(dir_path, pattern = "rsf.*\\.rds$", full.names = TRUE, recursive = TRUE)
# # Import/read all the files into a list
# rds_list <- lapply(rds_files, readRDS)
# # combine together as one list
# rds_dat <- do.call(c, rds_list)
# RSF <- rds_dat


#clean up environment
rm(rds_dat, rds_list, rds_files)
rm(AKDES, FITS, covariates, original)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~for merged extracted windows
# combining original and new data window analysis

original <- AKDES
original <- FITS
original <- covariates
# original <- RSF

# drop the 4 goats not used in this study
original <- original[!grepl("alpine_pacino", names(original))]
original <- original[!grepl("billy", names(original))]
original <- original[!grepl("great_goatsby", names(original))]
original <- original[!grepl("vertigoat", names(original))]

# rename list entries
# names(original) <- gsub("alpine_pacino", "30551", names(original))
# names(original) <- gsub("billy", "30636", names(original))
names(original) <- gsub("goatzilla", "30548", names(original))
# names(original) <- gsub("great_goatsby", "30599", names(original))
names(original) <- gsub("kid_rock", "30561", names(original))
names(original) <- gsub("rocky", "30613", names(original))
names(original) <- gsub("the_goatmother", "30575", names(original))
names(original) <- gsub("toats_mcgoats", "30642", names(original))
# names(original) <- gsub("vertigoat", "30567", names(original))
names(original) <- gsub("vincent_van_goat", "30648", names(original))


original_akdes <- original
original_fits <- original
original_covariates <- original

new_akdes <- AKDES
new_fits <- FITS
new_covariates <- covariates


names(original_covariates)
names(new_covariates)
# drop column "window_start....as.POSIXct.rep.NA..length.times....tz....America.Vancouver.." and  "window_end....as.POSIXct.rep.NA..length.times....tz....America.Vancouver.." 
original_covariates <- original_covariates[,c(1,4:8)]
new_covariates <- new_covariates[,c(1,4:8)]

# combine original and new data window outputs
covariates <- rbind(original_covariates, new_covariates)
AKDES <- c(original_akdes, new_akdes)
FITS <- c(original_fits, new_fits)


saveRDS(covariates, file = "data/window_analysis/covariates_extracted_combined_data_20250317.rds")
saveRDS(AKDES, file = "data/window_analysis/akdes_extracted_combined_data_20250317.rds")
saveRDS(FITS, file = "data/window_analysis/fits_extracted_combined_data_20250317.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end



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
# hectares square kilometers     square meters 
# 5986              6471               371 

# units are still mixed despite units = FALSE, therefore need to account for it



#____________________________________________________________________________
# create dataframe for results ----

win_results <- data.frame(collar_id = character(length(AKDES)))

for (i in 1:length(AKDES)) {
  # cat("Processing AKDE element:", i, "\n")
  tryCatch({
    # extract item from list
    win_results$collar_id[i] <- AKDES[[i]]@info$identity
    # get timestamp from the list element name
    win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(AKDES)[i])
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}


win_results$window_start <- as.POSIXct(win_results$window_start, format = "%Y-%m-%d %H:%M:%S")
win_results$date <- as.Date(win_results$window_start)
win_results$year <- year(win_results$window_start)
win_results$month <- month(win_results$window_start, label = FALSE) #label = false for numerical month
win_results$day <- day(win_results$window_start)
win_results$month_day <- format(win_results$window_start, "%m-%d")
win_results$doy <- yday(win_results$window_start) #day of the year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ for merged extracted windows
# rename entries
win_results$collar_id <- gsub("goatzilla", "30548", win_results$collar_id)
win_results$collar_id <- gsub("kid_rock", "30561", win_results$collar_id)
win_results$collar_id <- gsub("rocky", "30613", win_results$collar_id)
win_results$collar_id <- gsub("the_goatmother", "30575", win_results$collar_id)
win_results$collar_id <- gsub("toats_mcgoats", "30642", win_results$collar_id)
win_results$collar_id <- gsub("vincent_van_goat", "30648", win_results$collar_id)
unique(win_results$collar_id)
# drop the empty window
win_results <- win_results[-10940,]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end



# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
win_results <- merge(win_results, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
win_results <- dplyr::relocate(win_results, c("collar_id","goat_name", "goat_id"), .before = window_start)


# i <- 1








#.............................................................
# home range results (units = km^2) ----
# units are not consistent, therefore need to account for it
for (i in 1:length(AKDES)) {
  # cat("Processing AKDE element:", i, "\n")
  
  tryCatch({
    
    # subset akde results for one window
    akdesum <- as.data.frame(summary(AKDES[[i]], units = FALSE)$CI)
    
    # using km^2 as default SI units, convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
    if ("area (square kilometers)" %in% rownames(akdesum)) {
      win_results[i, c("hr_min_km2", "hr_est_km2", "hr_max_km2")] <- akdesum[c("low", "est", "high")]
      
    } else if ("area (square meters)" %in% rownames(akdesum)) {
      # convert m^2 to km^2 .-. divide by 1e6
      win_results[i, c("hr_min_km2", "hr_est_km2", "hr_max_km2")] <- akdesum[c("low", "est", "high")] / 1e6    
      
    } else if ("area (hectares)" %in% rownames(akdesum)) { 
      # convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
      win_results[i, c("hr_min_km2", "hr_est", "hr_max_km2")] <- akdesum[c("low", "est", "high")] / 100
      
    } else { 
      cat("no entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
      win_results[i, c("hr_min_km2", "hr_est_km2", "hr_max_km2")] <- NA
    }
    
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}





#___________________________________________________________________________
# movement results ----

# i <- 3
# FIT <- FITS[1:100]


# win_results <- data.frame(goat_name = character(length(FIT)))

tic()
for (i in 1:length(FITS)) {
  # for (i in 1:min(100, length(FIT))) { # testing with subset
  
  # extract item from list
  win_results$collar_id[i] <- FITS[[i]]@info$identity
  # get timestamp from the list element name
  win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(FITS)[[i]])
  
  
  win_results$movement_model[i] <- summary(FITS[[i]], units = FALSE)$name
  win_results[i, c("DOF_mean", "DOF_area", "DOF_diffusion", "DOF_speed")] <- summary(FITS[[i]], units = FALSE)$DOF[c("mean", "area", "diffusion", "speed")]
  
  
  #..............................................
  ## diffusion ----
  
  # si units has it m^2/sec, convert to km^2/day 0.0864 or 8.64e-2
  if ("diffusion (square meters/second)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI))  {
    # Update the corresponding row in the data frame
    win_results[i, c("diffusion_min_km2_day",
                     "diffusion_est_km2_day",
                     "diffusion_max_km2_day")] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)",
                                                                                       c("low", "est", "high")] * 0.0864
    
  } else {
    cat("no diffusion entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("diffusion_min_km2_day", "diffusion_est_km2_day", "diffusion_max_km2_day")] <- NA
  }
  
  
  
  
  #..............................................
  # tau_p ----
  # si units are seconds, convert into days
  if ("τ[position] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_p_min_s",
                     "tau_p_est_s",
                     "tau_p_max_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                             c("low", "est", "high")]
    
    #convert to seconds to days
    win_results[i, c("tau_p_min_day",
                     "tau_p_est_day",
                     "tau_p_max_day")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                               c("low", "est", "high")] / 86400
    
    
    
  } else {
    cat("no tau p entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_p_min_s", "tau_p_est_s", "tau_p_max_s")] <- NA
    win_results[i, c("tau_p_min_day", "tau_p_est_day", "tau_p_max_day")] <- NA
  }
  
  
  
  #..............................................
  # tau_v ----
  # si units are seconds, convert to minutes
  if ("τ[velocity] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_v_min_s",
                     "tau_v_est_s",
                     "tau_v_max_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)",
                                                                             c("low", "est", "high")]
    #convert to seconds to minutes
    win_results[i, c("tau_v_min_min", 
                     "tau_v_est_min", 
                     "tau_v_max_min")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)", 
                                                                               c("low", "est", "high")] / 60
    
  } else { 
    # cat("no tau v entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_v_min_s", "tau_v_est_s", "tau_v_max_s")] <- NA
    win_results[i, c("tau_v_min_min", "tau_v_est_min", "tau_v_max_min")] <- NA
  }
  
  
  fit_mu <- as.data.frame(FITS[[i]]$mu)
  centroid_sf <- st_as_sf(fit_mu, coords = c('x', 'y'), crs = FITS[[i]]@info$projection)
  centroid_sf <- st_coordinates(st_transform(centroid_sf, crs = 4326)) # reproject and grab the coordinates
  win_results[i, c("centroid_long", "centroid_lat")] <- centroid_sf[1, c("X", "Y")]
  
  
  # bls ----
  
  # If tau_p and tau_v have values, then calculate BLS
  if (!is.na(win_results$tau_p_est_s[i]) && !is.na(win_results$tau_v_est_s[i])) {
    sigma_p <- ctmm:::area.covm(FITS[[i]]$sigma) # area.covm sigma  = get geometric area/volume
    
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ for merged extracted windows
# rename entries
win_results$collar_id <- gsub("goatzilla", "30548", win_results$collar_id)
win_results$collar_id <- gsub("kid_rock", "30561", win_results$collar_id)
win_results$collar_id <- gsub("rocky", "30613", win_results$collar_id)
win_results$collar_id <- gsub("the_goatmother", "30575", win_results$collar_id)
win_results$collar_id <- gsub("toats_mcgoats", "30642", win_results$collar_id)
win_results$collar_id <- gsub("vincent_van_goat", "30648", win_results$collar_id)
win_results <- win_results[complete.cases(win_results$goat_name), ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end


# covariates results ----

# covar <- covariates
covariates <- covar

covariates$collar_id <- as.factor(covariates$collar_id)
covariates$window_start <- as.POSIXct(covariates$window_start, format = "%Y-%m-%d %H:%M:%S")
covariates$date <- as.Date(covariates$window_start)
covariates$year <- year(covariates$window_start)
covariates$month <- month(covariates$window_start, label = FALSE) #label = false for numerical month
covariates$month_day <- format(covariates$window_start, "%m-%d")
covariates$day <- day(covariates$window_start)
covariates$month_day <- as.Date(covariates$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting
covariates$doy <- yday(covariates$window_start) #day of the year




# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
covariates <- merge(covariates, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
covariates <- dplyr::relocate(covariates, c("collar_id","goat_name", "goat_id"), .before = n_fixes)

#11178 rows
#check for NAS
test <- covariates[!complete.cases(covariates$collar_id, covariates$window_start, covariates$window_end, covariates$n_fixes), ] #7725
# drop rows where every item are NA across the rows
covariates <- covariates[complete.cases(covariates$collar_id, covariates$window_start, covariates$window_end, covariates$n_fixes), ] #3453, 5970 for combined window sets, full data = 11916


# covariates <- merge(covariates, win_results[, c("collar_id","goat_name", "goat_id")], by = c("collar_id", "window_start"), all.x = TRUE)



# 
# write.csv(win_results, file = "./data/window_analysis/combined_data_window_partial_results_20250313.csv", row.names = FALSE)
# win_results <- read.csv("./data/window_analysis/combined_data_window_partial_results_20250313.csv")


# full data
write.csv(win_results, file = "./data/window_analysis/merged_windows_extracted_combined_data_20250317.rds", row.names = FALSE)
win_results <- read.csv("./data/window_analysis/merged_windows_extracted_combined_data_20250317.rds")
write.csv(win_results, file = "./data/window_analysis/merged_windows_extracted_covariates_combined_data_20250317.rds", row.names = FALSE)
