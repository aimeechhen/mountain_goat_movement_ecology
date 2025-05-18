
# Integrated step selection analysis (ISSA)

# based on the framework/how to guide Appendix B https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441


library(amt)
library(terra)
library(dplyr)
library(forcats) # fct_collapse() collapse factor levels into manually defined groups
library(ggplot2)
library(broom) #tidy()
library(tidyverse)
library(beepr)
library(gridExtra)
library(grid)
library(sf)
library(tictoc)
library(crayon)
library(lubridate)



#..................................................................
# Import data ----
#..................................................................

# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# convert collar_id to character object so only 6 show up instead of 10 levels when as.factor object, issues with extracting outputs
goat_data$collar_id <- as.character(goat_data$collar_id)

# goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
# goat_data <- goat_data[goat_data$goat_name %in% goats,]
unique(goat_data$collar_id) # 10 are showing up
# goat_data$collar_id <- droplevels(goat_data$collar_id)

# need to convert lat/long into a different project and get the coordinates via that crs for generating ssf_dat covariate values
# convert df to sf object
goat_coords <- st_as_sf(goat_data, coords = c("longitude","latitude"))
goat_coords <- st_set_crs(goat_coords, 4326) 
# reproject
goat_coords <- st_transform(goat_coords, "epsg:5070")
# extract reprojected coordinates
goat_coords <- st_coordinates(goat_coords)
# add to df
goat_data$lon_reproj <- goat_coords[,"X"]
goat_data$lat_reproj <- goat_coords[,"Y"]

# load spatial covariate data (packedspatraster or spatraster object) and reproject
elev_25m <- rast('data/rasters/elev_25m.tif')
elev_25m <- project(elev_25m, "epsg:5070")
dist_escape_25m <- rast('data/rasters/dist_escape_25m.tif')
dist_escape_25m <- project(dist_escape_25m,"epsg:5070")

# goat_data$timestamp <- as.POSIXct(goat_data$timestamp, origin = "1970-01-01", tz = "UTC")
goat_data$individual.local.identifier <- paste(goat_data$collar_id, goat_data$year, sep = "_")

# convert to track_xyz object
goat_track <- make_track(goat_data, 
                         .x = lon_reproj,             # x-coordinates (projected)
                         .y = lat_reproj,             # y-coordinates (projected)
                         .t = timestamp,     # timestamp column in POSIXct format
                         crs = "epsg:5070",        # Assuming UTM Zone 10N (adjust if necessary)
                         all_cols = TRUE     # Retain all other columns
)

goats <- unique(goat_data$individual.local.identifier)
# years <- unique(goat_track$year)

ssf_dat_list <- vector("list", length(goats))
issa_model <- vector("list", length(goats))

rss_data <- data.frame(collar_id = character(length(goats)),
                       goat_name = character(length(goats)),
                       year = rep(NA, length(goats)),
                       like50 = rep(NA, length(goats)),
                       like100 = rep(NA, length(goats)),
                       rss = rep(NA, length(goats)))

rm(goat_coords)



#.................................................................
# for loop ----

for (i in seq_along(goats)) {
    #subset an individual now
    current_goat <- goats[i]
  # subset a goat
  dat <- goat_track[goat_track$individual.local.identifier == current_goat,]
  # reorder in ascending order
  dat <- dat[order(dat$t_),]
  
  cat(bgMagenta(paste0("on iteration ", i, " | mountain goat and year: ", current_goat)), "\n")
  cat(bgMagenta(paste0("Number of fixes in subset: ", nrow(dat), "\n")))
  
  study_year <- unique(as.character(dat$year))
  collar_id <- unique(as.character(dat$collar_id))
  goat_name <- unique(as.character(dat$goat_name))
  goat_id <- unique(as.character(dat$goat_id))
  individual.local.identifier <- unique(as.character(dat$individual.local.identifier))
  
  # Section 2b: Format and Generate Random Steps ----
  ssf_dat <- tryCatch({
    dat %>% 
    track_resample(rate = minutes(375), tolerance = minutes(60)) %>%  #resample the track to get regular time steps based on summarize_sampling_rate
    steps_by_burst() %>% # convert from points to steps
    # add_count(burst_) %>%  # Count steps per burst
    # filter(n >= 3) %>% # Keep only bursts with >= 3 steps
    # select(-n) %>% # remove counter column
    random_steps() %>%  #  generate random available steps, NOTE: Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length
    extract_covariates(elev_25m, where = "both") %>% 
    extract_covariates(dist_escape_25m , where = "both") %>% 
    mutate(elev_25m_start = scale(elev_25m_start), # scale and center our covariates and add the additional movement covariates
           elev_25m_end = scale(elev_25m_end),
           dist_escape_25m_start = scale(dist_escape_25m_start),      # Scale distance to escape at start
           dist_escape_25m_end = scale(dist_escape_25m_end),          # Scale distance to escape at end
           cos_ta_ = cos(ta_), 
           log_sl_ = log(sl_)
    ) %>% 
    filter(!is.na(ta_))
  
  }, error = function(e) {
    # tried troubleshooting, dont know how to fix the random step error
    # Error in rle(unlist(sapply(bursts, "[[", "burst_"))) : 
    #   'x' must be a vector of an atomic type
    message(paste("Error in processing goat", current_goat, ":", e$message))
    message(paste("problem with generating random steps, moving onto the next goat"))
    # Return NULL if error occurs
    return(NULL)
  })
  
# fill null is returned then fill in NA values, skip the rest of the analysis because issue with data and move onto the next individual
  if (is.null(ssf_dat)) {
    rss_data$individual.local.identifier[i] <- individual.local.identifier
    rss_data$collar_id[i] <- collar_id
    rss_data$goat_name[i] <- goat_name
    rss_data$goat_id[i] <- goat_id
    rss_data$year[i] <- study_year
    rss_data$like50[i] <- NA
    rss_data$like100[i] <- NA
    rss_data$rss[i] <- NA
    ssf_dat_list[[i]] <- NA
    issa_model[[i]] <- NA
    next
  }
  
   # fit issa ----
  m4 <- ssf_dat %>% 
    fit_issf(case_ ~ elev_25m_end +  I(elev_25m_end^2) + dist_escape_25m_end +   
               sl_ + log_sl_ + cos_ta_ + 
               elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
               dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + # added dist to esc
               strata(step_id_), model = TRUE)
  
  # save the summary output
  sink(paste0("data/issa/model_summary/", unique(as.character(dat$individual.local.identifier)), "_issa_model_summary.txt"))
  cat("\n") #enter blank line
  print(paste0("ISSA model for collar ID: ", unique(as.character(dat$collar_id)), " | ", unique(as.character(dat$goat_name))))
  cat("\n") #enter blank line
  print(summary(m4))
  sink() #terminate output exporting connection/process
  
  
  #______________________________________________________
  # Section 3g:  Update Step-Length Distribution ----
  
  cat(bgBlue("update step-length distribution","\n"))
  
  # Update the distribution
  updated_sl4 <- update_sl_distr(m4)
  
  # Check the class
  class(updated_sl4)
  print(updated_sl4)
  
  # data.frame for plotting
  plot_sl4 <- data.frame(x = rep(NA, 100))
  
  # x-axis is sequence of possible step lengths
  plot_sl4$x <- seq(from = 0, to = 400, length.out = 100)
  
  # y-axis is the probability density under the given gamma distribution
  # For the tentative distribution
  plot_sl4$tentative <- dgamma(
    x = plot_sl4$x,
    shape = m4$sl_$params$shape,
    scale = m4$sl_$params$scale)
  
  # For the updated distribution
  plot_sl4$updated <- dgamma(
    x = plot_sl4$x,
    shape = updated_sl4$params$shape,
    scale = updated_sl4$params$scale)
  
  # Pivot from wide data to long data
  plot_sl4 <- plot_sl4 %>%
    pivot_longer(cols = -x)
  
  #...............................................................
  # Section 3h: Update Turn-Angle Distribution ----
  
  cat(bgBlue("update turn angle distribution","\n"))
  
  tryCatch({
  updated_ta4 <- update_ta_distr(m4)
  
  # Check the class
  class(updated_ta4)
  print(updated_ta4)
  
  # compare tentative and updated distributions to se how accounting for habitat-selection changes estimates of selection-free turn-angle distribution
  
  # data.frame for plotting
  plot_ta4 <- data.frame(x = rep(NA, 100))
  
  # x-axis is sequence of possible step lengths
  plot_ta4$x <- seq(from = -1 * pi, to = pi, length.out = 100)
  
  # y-axis is the probability density under the given von Mises distribution
  # For the tentative distribution
  plot_ta4$tentative <- circular::dvonmises(
    x = plot_ta4$x,
    mu = m4$ta_$params$mu,
    kappa = m4$ta_$params$kappa)
  
  
  # For the updated distribution
  plot_ta4$updated <- circular::dvonmises(
    x = plot_ta4$x,
    mu = updated_ta4$params$mu,
    kappa = updated_ta4$params$kappa)
  
  
  # Pivot from wide data to long data
  plot_ta4 <- plot_ta4 %>%
    pivot_longer(cols = -x)
  
  }, error = function(e) {
    # Log the error and continue to the next line of code
    message(paste("Error with goat", current_goat, ":", e$message))
 
  })
  
  
  
  cat(bgBlue("Calculating RSS","\n"))
  
  #.....................................................................
  # Section 3i: Calculating RSS for Movements ----
  
  # note: using step length
  like50 <- dgamma(50,
                   shape = updated_sl4$params$shape,
                   scale = updated_sl4$params$scale)
  # = 0
  like100 <- dgamma(100,
                    shape = updated_sl4$params$shape,
                    scale = updated_sl4$params$scale)
  # = 0
  rss <- like50/like100 #1.23
  
   # save rss values to dataframe
  rss_data$individual.local.identifier[i] <- individual.local.identifier
  rss_data$collar_id[i] <- collar_id
  rss_data$goat_name[i] <- goat_name
  rss_data$goat_id[i] <- goat_id
  rss_data$year[i] <- study_year
  rss_data$like50[i] <- like50
  rss_data$like100[i] <- like100
  rss_data$rss[i] <- rss
  
  ssf_dat$individual.local.identifier <- individual.local.identifier
  ssf_dat$collar_id <- collar_id
  ssf_dat$goat_name <- goat_name
  ssf_dat$goat_id <- goat_id
  
  ssf_dat_list[[i]] <- ssf_dat
  issa_model[[i]] <- m4
  

  
}

 

# name each item based on their id
names(ssf_dat_list) <- goats
names(issa_model) <- goats

#................................................................
# issa data ----
#................................................................

# convert list into a dataframe
issa_data <- do.call(rbind, ssf_dat_list)

issa_data <- relocate(issa_data, c(individual.local.identifier, collar_id, goat_name, goat_id), .before = burst_)

save(issa_data, file = "data/issa/issa_data_20250505.rda")

load("data/issa/issa_data_20250505.rda")

