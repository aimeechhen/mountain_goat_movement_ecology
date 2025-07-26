# Integrated step selection analysis (ISSA)

# based on the framework/how to guide Appendix B https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441
# additional annotations from https://issa-guild.github.io/book/index.html

# remotes::install_github("jmsigner/amt")
library(amt) # dev version needed: remotes::install_github("jmsigner/amt")
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
library(ggpubr)
library(ggh4x)
library(ctmm)
# install.packages('TMB', type = 'source')
library(glmmTMB)
library(cowplot)
library(MASS)
# devtools::install_github("smthfrmn/mixedSSA", build_vignettes = TRUE, force = TRUE)
library(mixedSSA)


# general workflow of issa

# pre-prep gps data and habitat rasters
# 1. format and generate steps data
# 2. fit issf
# 3. update sl and ta distribution based on model predictions
# 4. plot movement parameters
# 5. extract outputs/results:
# get CI values
# calculate rss between 2 locations?
# calculate rss for movement

#~~~~~~~~~~~~~

# Integrated step selection analysis (ISSA)
# basic framework

# DATA PREP
# load data
# check data for duplicates
# convert gps data into amt track_xyz object
# Visualization of the tracks

# format and generate steps (ssf data)
# check data for duplicates
# visualize ssf data
# plot movement data of raw ssf data
# plot movement parameters of raw ssf data (i.e., tentative distribution)

# fit issa model (or issa mixed effects model)

# additional steps with mixed effects model
# i) config the model before fitting
# ii) need to fix variance of intercept first then fix df of the first random term (1|step_id) component 
# iii) then fit the model

# update movement distributions
# plot updated distribution

# extract updated movement parameters (shape, scale, kappa)
# extract model coefficients from model summary
# extract se (variance) and ci values of the model




#..................................................................
# Import data ----
#..................................................................

load('./data/fire/foippa.rda')
load('./data/fire/firms.rda')
load('./data/fire/cwfis.rda')
fire <- dplyr::bind_rows(FIRMS, FOIPPA, CWFIS)
fire$date <- as.Date(fire$date, format = '%Y-%m-%d')
which(st_is_empty(fire)) # check for empty geometries
rm(FIRMS, FOIPPA, CWFIS)

# load spatial covariate data (packedspatraster or spatraster object), reproject, and scale/center goat-habitat covariate raster data via terra pkg
# scale rasters so they can be compared to as the same relative mean,  additionally, if not scaled, the model will produce NA values in the model summary later on
elev <- rast('data/rasters/elev_25m.tif')
elev <- project(elev, "epsg:5070")
elev_scaled <- scale(elev)

dist_escape <- rast('data/rasters/dist_escape_25m.tif')
dist_escape <- project(dist_escape,"epsg:5070")
dist_escape_scaled <- scale(dist_escape)

# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# convert collar_id to character object so only 6 show up instead of 10 levels when as.factor object, issues with extracting outputs
goat_data$collar_id <- as.character(goat_data$collar_id)
unique(goat_data$collar_id)

# need to convert lat/long into a different project and get the coordinates via that crs for generating ssf_dat covariate values, using lat/long epsg:4326 produced issues later on
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

#clean up environment
rm(goat_coords)

# combine collar_id and year into combined info id format
goat_data$id_year <- paste(goat_data$collar_id, goat_data$year, sep = "_")

# interested in differences in-between individuals over different years
goats <- unique(goat_data$id_year)

# convert to amt track_xyz object
goat_track <- make_track(goat_data, 
                         .x = lon_reproj,             # x-coordinates (projected)
                         .y = lat_reproj,             # y-coordinates (projected)
                         .t = timestamp,     # timestamp column in POSIXct format
                         crs = "epsg:5070",        # Assuming UTM Zone 10N (adjust if necessary)
                         all_cols = TRUE)     # Retain all other columns



# # inspect sampling rate 
# summarize_sampling_rate(goat_track)
# 
# # inspect sampling rates as per id_year
# sampling_rates <- vector("list", length(goats))
# 
# for (i in seq_along(goats)) {
#   current_goat <- goats[i]
#   subset_goat <- goat_track[goat_track$id_year == current_goat, ]
#   subset_goat <- subset_goat[order(subset_goat$t_), ]
#   rate <- summarize_sampling_rate(subset_goat)
#   rate$id_year <- current_goat
#   sampling_rates[[i]] <- rate
# }
# # combine into df
# sampling_rate_df <- do.call(rbind, sampling_rates)
# range(sampling_rate_df$mean) # 4.638970 6.568744
# mean(sampling_rate_df$mean) # 5.965427
# # buffer of 5.965427-4.638970 = 1.326457
# # 5.965427h = 357.9256min
# # 1.326457h = 79.58742min
# 
# rm(rate, sampling_rate_df, sampling_rates, subset_goat)

# # Visualization of the tracks:
# ggplot() + 
#   geom_spatraster(data = elev_scaled, show.legend = FALSE)  +
#   geom_path(data = goat_track, aes(x_, y_),  col= "black") +
#   facet_wrap(~ collar_id, nrow = 2) + 
#   scale_fill_terrain_c() +
#   theme_bw()


#.........................................................
## set up ----
#.........................................................

# create foloder for saving outputs
dir.create("./data/issa/single/", recursive = TRUE, showWarnings = FALSE)

# # create folders for the model summaries
# lapply(c("m1", "m2", "m4"), function(folder) {
#   dir.create(file.path("./data/issa/single/model_summary", folder), recursive = TRUE, showWarnings = FALSE)
# })

# create folders for plots
# lapply(c("sl_plot", "ta_plot"), function(folder) {
#   dir.create(file.path("./figures/issa/single/plot", folder), recursive = TRUE, showWarnings = FALSE)
# })



#////////////////////////////////////////////////////////////////
# Single animal (subset data) ----
#////////////////////////////////////////////////////////////////


# single animal data is subsetted workflow approach subsets the data for each animal and each animal is modelled/analyzed separately

# prepare objects for saving
ssf_dat_list <- vector("list", length(goats))
m1_list <- vector("list", length(goats))
m2_list <- vector("list", length(goats))
m4_list <- vector("list", length(goats))
sl_param_list <- vector("list", length(goats))
ta_param_list <- vector("list", length(goats))
sl_list <- vector("list", length(goats))
ta_list <- vector("list", length(goats))
m4_coeff_list <- vector("list", length(goats))





#.................................................................
# START LOOP ----
i <- 1

for (i in seq_along(goats)) {
  # indicate goat 
  current_goat <- goats[i]
  # subset a goat
  subset_goat <- goat_track[goat_track$id_year == current_goat,]
  # reorder in ascending order
  subset_goat <- subset_goat[order(subset_goat$t_),]
  
  cat(bgMagenta(paste0("on iteration ", i, " | mountain goat and year: ", current_goat)), "\n")
  cat(bgMagenta(paste0("Number of fixes in subset: ", nrow(subset_goat), "\n")))
  
  study_year <- unique(as.character(subset_goat$year))
  collar_id <- unique(as.character(subset_goat$collar_id))
  goat_name <- unique(as.character(subset_goat$goat_name))
  id_year <- unique(as.character(subset_goat$id_year))
  
  # name the objects in the list
  names(ssf_dat_list)[i] <- current_goat
  # names(m1_list)[i] <- current_goat
  # names(m2_list)[i] <- current_goat
  names(m4_list)[i] <- current_goat
  names(sl_list)[i] <- current_goat
  names(ta_list)[i] <- current_goat
  names(sl_param_list)[i] <- current_goat
  names(ta_param_list)[i] <- current_goat
  names(m4_coeff_list)[i] <- current_goat
  
  
  # 1. Format and Generate Random Steps ----
  ssf_dat <- tryCatch({
    subset_goat %>% 
      track_resample(rate = minutes(375), tolerance = minutes(80)) %>%  #resample the track to get regular time steps based on summarize_sampling_rate
      steps_by_burst(keep_cols = 'start') %>% # convert from points to steps
      #~~~~~~~~~~~~~~~~~~~~~~~~ to check the number of steps in a burst
      # add_count(burst_) %>%  # Count steps per burst
      # filter(n >= 3) %>% # Keep bursts with 3+ steps only
      # select(-n) %>% # remove counter column
      #~~~~~~~~~~~~~~~~~~~~~~~~~
      random_steps() %>% # generate random available steps
      #NOTE: Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length
      extract_covariates(elev_scaled, where = "both") %>%   # extract covariates for the random steps generated
      extract_covariates(dist_escape_scaled, where = "both") %>%   # extract covariates for the random steps generated
      amt::filter(!is.na(ta_)) %>%  # drop na values in the ta_ column
      mutate(cos_ta_ = cos(ta_), # transform ta_ and create a new column
             log_sl_ = log(sl_)) # transform sl_ and create a new column
    
  }, error = function(e) {
    message(paste("Error problem with generating random steps, moving onto the next goat", id_year, ":", e$message))
    # tried troubleshooting, dont know how to fix the random step error
    # Error in rle(unlist(sapply(bursts, "[[", "burst_"))) : 
    #   'x' must be a vector of an atomic type
    
    # Return NULL if error occurs
    return(NULL)
  })
  head(ssf_dat)
  
  # if null is returned, then fill in NA values, skip the rest of the analysis because issue with data and move onto the next individual
  if (is.null(ssf_dat)) {
    # create a dummy dataframe and fill in accordingly with NA or with known info
    ssf_dat <- data.frame(matrix(nrow = 1, ncol = 0))  
    ssf_dat[1, c("x1_", "x2_", "y1_", "y2_", "sl_", "ta_", 
                 "collar_id", "goat_name",  "goat_id", "longitude", "latitude", "HDOP", 
                 "data_type", "date", "year", "month", "day", "month_day", "doy", "id_year", 
                 "burst_", "t1_", "t2_", "dt_", "case_", "step_id_", 
                 "elev_25m_start", "elev_25m_end", "dist_escape_25m_start", "dist_escape_25m_end", 
                 "cos_ta_",
                 "log_sl_")] <- NA
    ssf_dat$id_year <- id_year
    ssf_dat$collar_id <- collar_id
    ssf_dat$goat_name <- goat_name
    ssf_dat$year <- study_year
    
    plot_sl <- data.frame(
      step_length = seq(from = 0, to = 400, length.out = 300),
      prob_density = NA_real_,
      elev_level = NA_character_,
      id_year = id_year,
      collar_id = collar_id,
      goat_name = goat_name,
      year = study_year)
    plot_ta <- data.frame(
      turn_angle = seq(from = -pi, to = pi, length.out = 300),
      prob_density = NA_real_,
      elev_level = NA_character_,
      id_year = id_year,
      collar_id = collar_id,
      goat_name = goat_name,
      year = study_year)
    
    ssf_dat_list[[i]] <- ssf_dat
    m1_list[[i]] <- NA
    m2_list[[i]] <- NA
    m4_list[[i]] <- NA
    sl_list[[i]] <- plot_sl
    ta_list[[i]] <- plot_ta
    coeff_list[[i]] <- NA
    
    next
  }
  
  
  # #...........................................................
  # ## calculate distance to fire for step_start ----
  # # Define the time window with +/- 1 hour buffer
  # time_window <- 1 %#% 'hour' #ctmm package
  # # initialise columns to store results
  # if (any(ssf_dat$year == "2023")) {
  #   ssf_dat$fire_event <- "no"
  #   ssf_dat$n_fire_points <- NA
  #   ssf_dat$dist_to_fire_start <- NA
  #   
  #   #loop through every collar point
  #   for (j in 1:nrow(ssf_dat)) {
  #     # extract a collar point from data
  #     collar_point <- ssf_dat[j,]
  #     # create a custom timestamp of that collar point 
  #     collar_timestamp <- collar_point$t1_
  #     # Define time range +/- 1 hour around the collar point timestamp
  #     start_time <- collar_timestamp - time_window
  #     end_time <- collar_timestamp + time_window
  #     
  #     # extract fire point events within the time window
  #     fire_point <- fire[fire$timestamp >= start_time & fire$timestamp <= end_time,]
  #     # drop empty geometries
  #     fire_point <- fire_point[!st_is_empty(fire_point), ]
  #     # record number of fire points in the time window
  #     ssf_dat$n_fire_points[j] <- as.numeric(nrow(fire_point))
  #     
  #     # Check if there are any fire points within the time window
  #     if (nrow(fire_point) == 0) {
  #       # cat("No data fire points found in window", i, "- moving on to the next collar point.\n")
  #       # Set results to NA
  #       ssf_dat$dist_to_fire_start[j] <- NA
  #       
  #       next # Move to the next gps collar point if no fire is present
  #     }
  #     
  #     # If fire points exist in the time window, proceed with calculation
  #     tryCatch({
  #       if (nrow(fire_point) > 0) {
  #         ssf_dat$fire_event[j] <- "yes"
  #         # calculate the distance between the collar point and the fire point, output comes out as a matrix
  #         dist_matrix <- st_distance(collar_point, fire_point)
  #         #store results, extracting the min. distance between collar point and [multiple] fire point(s)
  #         ssf_dat$dist_to_fire_start[j] <- min(dist_matrix) # using min value instead of including all distances for all fire points, also causes issues when storing results due to multiple values
  #         
  #       } else {
  #         # If time window is empty, set results to NA
  #         ssf_dat$dist_to_fire_start[j] <- NA
  #         
  #       }
  #     }, error = function(e) {
  #       ssf_dat$dist_to_fire_start[j] <- NA
  #     })
  #   }
  # } else {
  #   message("not a fire year")
  # }
  
  #..............................................................
  ## check original distributions (tentative, i.e. resampled raw data) ----
  #................................................................
  sl_distr(ssf_dat)
  ta_distr(ssf_dat)
  print(paste0("tenantive shape = ", round(sl_distr(ssf_dat)$params$shape, 5)))
  print(paste0("tenantive scale = ", round(sl_distr(ssf_dat)$params$scale, 5)))
  print(paste0("tenantive kappa = ", round(ta_distr(ssf_dat)$params$kappa, 5)))
  
  
  #............................................................
  # 2. Fit issf ----
  #............................................................
  
  # start of step = the movement process, the influences of habitat on the movement with turn angle or step length
  # end of the step = the selection process (i.e., where they selected/decided to be/end up)
  
  # model summary:
  # look sl, log_sl, cos_ta p-values, are they significant or not?
  # coefficient for covariate indicates + = selection or - = avoidance
  # distance to a feature (such as distance to water, fire etc) can be tricky, + = selection for areas further from feature which means feature itself is avoided
  # movement parameters
  # Coefficient for log(Step Length) modifies shape parameter indicating + = more longer steps, - = fewer longer steps 
  # Coefficient for Step Length modifies the scale parameter indicating + = longer steps, - = shorter steps
  # cos[TA] converts the circular measure (turning angle of 360 degrees) to linear; moving backwards = -1, random walk = 0, moving forward = 1 (DR. NOONAN SAYS THIS IS WRONG when reviewing the issa book source)
  # note: cos[TA] can break the model, use check_model() or model_performance() 
  # kappa = directionality of movement, i.e. increase in kappa = forward movement, note: kappa = concentration parameter of the vonMises distributions
  
  
  ## model 1: basic model ----
  # #covariates not interacting with any movement parameters, i.e. step length, turn angles)
  # cat(bgBlue("fit issf model 1","\n"))
  # m1 <- fit_issf(case_ ~ 
  #                  sl_ + log_sl_ + cos_ta_ + 
  #                  elev_25m_end + dist_escape_25m_end +
  #                  strata(step_id_), 
  #                data = ssf_dat, model = TRUE)
  # summary(m1)
  # #extract model coeffs
  # m1_coeff <- data.frame(term = names(m1$model$coefficients),
  #                        m1_coeff = m1$model$coefficients)
  # # print(performance::check_model(m1$model, residual_type = "normal"))
  # # print(performance::model_performance(m1$model))
  # 
  # #......................................................................
  ## model 2: basic + quadratic term (x^2) ----
  # # non-linear effect/relationship (e.g. what if the animal selects for intermediate elevations and avoids locations with extremely low or high elevations?), can use splines or polynomials to capture this non-linear effect. 
  # 
  # cat(bgBlue("fit issf model 2","\n"))
  # m2 <- fit_issf(case_ ~ 
  #                  sl_ + log_sl_ + cos_ta_ + 
  #                  elev_25m_end + dist_escape_25m_end + 
  #                  I(elev_25m_end^2) + 
  #                  strata(step_id_), 
  #                data = ssf_dat, model = TRUE)
  # summary(m2)
  # #extract model coeffs
  # m2_coeff <- data.frame(term = names(m2$model$coefficients),
  #                        m2_coeff = m2$model$coefficients)
  # 
  # #......................................................................
  ## model 3: interactions terms with categorical variables ----
  
  
  
  
  
  #......................................................................
  ## model 4: basic + quadratic term + interaction terms ----
  
  # interaction terms with movement characteristics and continuous explanatory variables capturing environmental characteristics
  # e.g. what if the animal moves differently based on habitat feature that is at the start of the movement step? 
  
  # cat(bgBlue("fit issf model 4","\n"))
  m4 <- fit_issf(case_ ~ 
                   sl_ + log_sl_ + cos_ta_ + 
                   elev_25m_end + 
                   # dist_escape_25m_end + 
                   I(elev_25m_end^2) +   
                   elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
                   # dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + 
                   strata(step_id_), 
                 data = ssf_dat, model = TRUE)
  summary(m4)
  
  # extract model coeff
  m4_coeff <- data.frame(term = names(m4$model$coefficients),
                         m4_coeff = m4$model$coefficients)
  
  # # save the summary output
  # sink(paste0("./data/issa/single/model_summary/m4/", unique(as.character(subset_goat$id_year)), "m4_summary_single.txt"))
  # cat("\n") #enter blank line
  # print(paste0("ISSA model for collar ID: ", unique(as.character(subset_goat$collar_id)), " | ", unique(as.character(subset_goat$goat_name))))
  # cat("\n") #enter blank line
  # print(summary(m4))
  # sink() #terminate output exporting connection/process
  
  
  #......................................................................
  ## model 5: basic + quadratic term + interaction terms + distance to fire ----
  # m5 <- fit_issf(case_ ~
  #                  sl_ + log_sl_ + cos_ta_ +
  #                  elev_25m_end + dist_escape_25m_end +
  #                  I(elev_25m_end^2) +
  #                  elev_25m_start:(sl_ + log_sl_ + cos_ta_) +
  #                  dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) +
  #                  dist_to_fire +
  #                  strata(step_id_),
  #                data = ssf_dat, model = TRUE)
  # summary(m5)
  
  
  #......................................................................
  ## model 6: basic + quadratic term + interaction terms + distance to fire + fire/covariate interaction terms ----
  # m6 <-  fit_issf(case_ ~ 
  #                   sl_ + log_sl_ + cos_ta_ + 
  #                   elev_25m_end + dist_escape_25m_end + 
  #                   I(elev_25m_end^2) +   
  #                   elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
  #                   dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + 
  #                   dist_to_fire +
  #                   elev_25m_start:dist_to_fire + 
  #                   dist_escape_25m_end:dist_to_fire +
  #                   strata(step_id_),
  #                 data = ssf_dat, model = TRUE)
  # summary(m6)
  # 
  
  
  
  
  
  #...........................................................
  # 3. update distribution ----
  #...........................................................
  
  cat(bgBlue("update distributions (model 4)","\n"))
  
  # Low elevation step-length distribution
  low_sl <- update_gamma(dist = m4$sl_,
                         beta_sl = m4$model$coefficients["sl_"] + -2 * m4$model$coefficients["sl_:elev_25m_start"],
                         beta_log_sl = m4$model$coefficients["log_sl_"] + -2 * m4$model$coefficients["log_sl_:elev_25m_start"])
  # Medium elevation step-length distribution
  med_sl <- update_gamma(dist = m4$sl_,
                         beta_sl = m4$model$coefficients["sl_"] + 0 * m4$model$coefficients["sl_:elev_25m_start"],
                         beta_log_sl = m4$model$coefficients["log_sl_"] + 0 * m4$model$coefficients["log_sl_:elev_25m_start"])
  # high step-length distribution
  hi_sl <- update_gamma(dist = m4$sl_,
                        beta_sl = m4$model$coefficients["sl_"] + 2 * m4$model$coefficients["sl_:elev_25m_start"],
                        beta_log_sl = m4$model$coefficients["log_sl_"] + 2 * m4$model$coefficients["log_sl_:elev_25m_start"])
  
  sl_param <- data.frame(grouping = c("low", "med", "high"),
                         collar_id = rep(collar_id, 3),
                         beta_sl = rep(NA, 3),
                         beta_log_sl = rep(NA, 3),
                         shape = rep(NA, 3),
                         scale = rep(NA, 3))
  
  # extract the information based on grouping/category and fill in the df accordingly
  # if the row = category, column = to be filled in with
  sl_param[sl_param$grouping == "low", c("beta_sl", "beta_log_sl", "shape", "scale")] <- c(low_sl$beta_sl, 
                                                                                           low_sl$beta_log_sl, 
                                                                                           low_sl$params$shape, 
                                                                                           low_sl$params$scale)
  
  sl_param[sl_param$grouping == "med", c("beta_sl", "beta_log_sl", "shape", "scale")] <- c(med_sl$beta_sl, 
                                                                                           med_sl$beta_log_sl, 
                                                                                           med_sl$params$shape, 
                                                                                           med_sl$params$scale)
  
  sl_param[sl_param$grouping == "high", c("beta_sl", "beta_log_sl", "shape", "scale")] <- c(hi_sl$beta_sl, 
                                                                                            hi_sl$beta_log_sl, 
                                                                                            hi_sl$params$shape,
                                                                                            hi_sl$params$scale)
  
  if (any(sl_param$shape < 0)) { 
    cat(red("Problem: shape below 0\n")); print(sl_param[sl_param$shape < 0, ]) 
    }
  
  head(sl_param)
  
 
  
  
  #.............................
  # Creating turn angle values -> low, med, high
  
  # low turn-angle distribution
  low_ta <- update_vonmises(dist = m4$ta_,
                            beta_cos_ta = m4$model$coefficients["cos_ta_"] + -2 * m4$model$coefficients["cos_ta_:elev_25m_start"])
  # med turn-angle distribution
  med_ta <- update_vonmises(dist = m4$ta_,
                            beta_cos_ta = m4$model$coefficients["cos_ta_"] + 0 * m4$model$coefficients["cos_ta_:elev_25m_start"])
  # hi turn-angle distribution
  hi_ta <- update_vonmises(dist = m4$ta_,
                           beta_cos_ta = m4$model$coefficients["cos_ta_"] + 2 * m4$model$coefficients["cos_ta_:elev_25m_start"])

  # create a df to store the extracted parameters
  ta_param <- data.frame(grouping = c("low", "med", "high"),
                         collar_id = rep(collar_id, 3),
                         beta_cos_ta = rep(NA, 3),
                         kappa = rep(NA, 3),
                         mu = rep(0, 3))
  
  
  # extract the information based on grouping/category and fill in the df accordingly
  # if the row = category, column = to be filled in with
  ta_param[ta_param$grouping == "low", c("beta_cos_ta", "kappa")] <- c(low_ta$beta_cos_ta, 
                                                                       low_ta$params$kappa)
  
  ta_param[ta_param$grouping == "med", c("beta_cos_ta", "kappa")]  <- c(med_ta$beta_cos_ta, 
                                                                        med_ta$params$kappa)
  
  ta_param[ta_param$grouping == "high", c("beta_cos_ta", "kappa")]  <- c(hi_ta$beta_cos_ta, 
                                                                         hi_ta$params$kappa)
  
  head(ta_param)
  
  
  
  #.......................................................
  ## plot updated distribution ----
  #.......................................................
  
  # check distribution by plotting
  
  cat(bgBlue("plot m4 update distribution","\n"))
  # 
  # # step length distribution 
  # #create a data.frame for plotting
  # plot_sl <- data.frame(step_length = rep(NA, 100))
  # # x-axis is sequence of possible step lengths
  # plot_sl$step_length <- seq(from = 0, to = 400, length.out = 100)
  # # y-axis is the probability density under the given gamma distribution
  # plot_sl$low <- dgamma(x = plot_sl$step_length, 
  #                       shape = low_sl$params$shape,
  #                       scale = low_sl$params$scale)
  # plot_sl$medium <- dgamma(x = plot_sl$step_length, 
  #                          shape = med_sl$params$shape,
  #                          scale = med_sl$params$scale)
  # plot_sl$high <- dgamma(x = plot_sl$step_length, 
  #                        shape = hi_sl$params$shape,
  #                        scale = hi_sl$params$scale)
  # # data carpentry
  # plot_sl <- pivot_longer(plot_sl, cols = -step_length) # flip the df
  # plot_sl <- rename(plot_sl, c(prob_density = value,
  #                              elev_level = name))
  # plot_sl$id_year <- id_year
  # plot_sl$collar_id <- collar_id
  # plot_sl$goat_name <- goat_name
  # plot_sl$year <- study_year
  # 
  # head(plot_sl)
  
  plot_data <- list()
  #create a df for plotting
  for (i in 1:nrow(sl_param)) {
    # x-axis is sequence of possible step lengths
    step_length <- seq(from = 0, to = 400, length.out = 100)
    # extract the data
    grouping <- sl_param[i, "grouping"] # note: the column name is 'grouping' and not 'category' as indicated in the Chatterjee et al workflow and its for the quantile value, theres no such column as quantile
    # extract collar id
    collar_id <- sl_param[i, "collar_id"] # there is no column called "id", I think they mean is the random effects column which is id
    # extract the shape and scale of the sl movement parameters
    shape <- sl_param[i, "shape"]
    scale <- sl_param[i, "scale"] # in chatterjee et al, they forced all values to be positive via the ab() function, i.e., absolute values
    
    # y-axis is the probability density under the given gamma distribution
    prob_density <- dgamma(x = step_length,
                           shape = sl_param[i, "shape"], # if shape not above 0, theres a problem
                           # scale = abs(sl_param_typical[i, "scale"]) # making all values positive if they arent already
                           scale = sl_param[i, "scale"])
    
    # create a df with all the extracted values
    plot_data[[i]] <- data.frame(step_length = step_length,
                                 prob_density = prob_density,
                                 grouping = grouping,
                                 collar_id = collar_id,
                                 shape = shape,
                                 scale = scale)
    
  }
  
  plot_sl <- do.call(rbind, plot_data)
  plot_sl$grouping <- as.factor(plot_sl$grouping)
  head(plot_sl)
  # clean up environment
  rm(plot_data)
  
  
  
  p1 <- 
    ggplot(plot_sl, aes(x = step_length, y = prob_density, color = factor(grouping))) +
    geom_line(linewidth = 1) +
    scale_color_manual(name = "Elevation",
                       breaks = c("low", "medium", "high"),
                       values = c("navyblue", "gray50", "firebrick")) +
    xlab("Step Length (m)") +
    ylab("Probability Density") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), #removes horizontal gridlines
          panel.grid.minor = element_blank()) #removes vertical gridlines
  # print(p1)
  
  #.....................................
  # turn angles
  
  plot_ta <- data.frame(turn_angle = rep(NA, 100))
  # x-axis is sequence of possible step lengths
  plot_ta$turn_angle <- seq(from = -pi, to = pi, length.out = 100)
  
  # y-axis is the probability density under the given gamma distribution
  # trycatch is at the function instead of the front of the code chunk because, if there is an error then its as if the code never happened, but if its on the outside, it still running and executing the function and what to do with an error
  # low
  plot_ta$low <-  tryCatch({
    circular::dvonmises(x = plot_ta$turn_angle, 
                        # kappa = low_ta$params$kappa,
                        kappa = abs(low_ta$params$kappa), # making all values positive if they arent already, to fix the error, based off of chatterjee et al
                        mu = 0)
    # issue with plotting low -> kappa value is negative (and cannot be negative(?))
  }, error = function(e) {
    cat(yellow("Error m4 low turn angle data (plot):", id_year, "-", e$message, "\n"))
    # if an error occurs, then fill the column with NA values
    rep(NA_real_, nrow(plot_ta))
  })
  
  
  # med
  plot_ta$medium <-   tryCatch({
    circular::dvonmises(x = plot_ta$turn_angle, 
                        # kappa = med_ta$params$kappa,
                        kappa = abs(med_ta$params$kappa), # making all values positive if they arent already, to fix the error, based off of chatterjee et al
                        mu = 0)
  }, error = function(e) {
    cat(yellow("Error m4 med turn angle data (plot):", id_year, "-", e$message, "\n"))
    # if an error occurs, then fill the column with NA values
    rep(NA_real_, nrow(plot_ta))
  })
  
  # hi
  plot_ta$high <-   tryCatch({
    circular::dvonmises(x = plot_ta$turn_angle, 
                        # kappa = hi_ta$params$kappa,
                        kappa = abs(hi_ta$params$kappa), # making all values positive if they arent already, to fix the error, based off of chatterjee et al
                        mu = 0)
  }, error = function(e) {
    cat(yellow("Error m4 high turn angle data (plot):", id_year, "-", e$message, "\n"))
    # if an error occurs, then fill the column with NA values
    rep(NA_real_, nrow(plot_ta))
  })
  
  # data carpentry
  plot_ta <- pivot_longer(plot_ta, cols = -turn_angle) # flip the df
  plot_ta <- rename(plot_ta, c(prob_density = value,
                               elev_level = name))
  plot_ta$id_year <- id_year
  plot_ta$collar_id <- collar_id
  plot_ta$goat_name <- goat_name
  plot_ta$year <- study_year
  
  # Plot
  p2 <- ggplot(plot_ta, aes(x = turn_angle, y = prob_density, color = factor(elev_level))) +
    geom_line(linewidth = 1) +
    scale_color_manual(name = "Elevation",
                       breaks = c("low", "medium", "high"),
                       values = c("navyblue", "gray50", "firebrick")) +
    scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                       labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
    xlab("Turn Angle (radians)") +
    ylab("Probability Density") +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  # print(p2)
  
  
  #.......................................................
  ## Plot Multi-panel ----
  #.......................................................
  
  # Combine and save plot
  if (exists("p2")) {
    plot_elevation <- ggarrange(p1, p2,
                                ncol = 2,
                                common.legend = TRUE,
                                legend = "top")
  } else {
    plot_elevation <- ggarrange(p1,
                                ncol = 1,
                                common.legend = TRUE,
                                legend = "top")
  }
  
  # add plot title
  plot_elevation <- annotate_figure(plot_elevation,
                                    top = text_grob(paste("Model 4 ISSA |", id_year), 
                                                    hjust = 0.5))
  # print(plot_elevation)
  # Save the plot
  ggsave(plot_elevation,
         file = paste0("./figures/issa/single/m4/m4_", id_year, ".png"),
         width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")
  
  
  # store outputs
  m4_coeff$id_year <- id_year
  m4_coeff$collar_id <- collar_id
  m4_coeff$goat_name <- goat_name
  m4_coeff$year <- study_year
  
  m4_coeff_list[[i]] <- m4_coeff
  
  # store models
  ssf_dat_list[[i]] <- as.data.frame(ssf_dat)
  # m1_list[[i]] <- m1
  # m2_list[[i]] <- m2
  m4_list[[i]] <- m4
  # m5_list[[i]] <- m5
  # m6_list[[i]] <- m6
  sl_param_list[[i]] <- sl_param
  ta_param_list[[i]] <- ta_param
  sl_list[[i]] <- as.data.frame(plot_sl)
  ta_list[[i]] <- as.data.frame(plot_ta)
  
} # close loop



## END LOOP ----
#///////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////





#...........................................................
## save ----
#...........................................................

# convert list dataframe
m4_coeff_data <- do.call(rbind, m4_coeff_list)
m4_coeff_data <- relocate(m4_coeff_data, c(id_year, collar_id, goat_name, year), .before = term)
save(m4_coeff_data, file = "./data/issa/single/m4_coeff_data.rda")
load("./data/issa/single/m4_coeff_data.rda")

# convert list into a dataframe
issa_data <- do.call(rbind, ssf_dat_list)
issa_data <- relocate(issa_data, c(id_year, collar_id, goat_name, year), .before = burst_)
save(issa_data, file = "./data/issa/single/issa_data.rda")
load("./data/issa/single/issa_data.rda")


sl_data <- do.call(rbind, sl_list)
ta_data <- do.call(rbind, ta_list)
save(sl_data, file = "./data/issa/single/sl_data.rds")
save(ta_data, file = "./data/issa/single/ta_data.rds")







