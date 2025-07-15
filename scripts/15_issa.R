
# Integrated step selection analysis (ISSA)

# based on the framework/how to guide Appendix B https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441
# additional annotations from https://issa-guild.github.io/book/index.html

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
library(ggpubr)
library(ggh4x)





#..................................................................
# Import data ----
#..................................................................

# Import combined collar data (original + new)
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
# convert collar_id to character object so only 6 show up instead of 10 levels when as.factor object, issues with extracting outputs
goat_data$collar_id <- as.character(goat_data$collar_id)
unique(goat_data$collar_id)

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

# combine collar_id and year into combined info id format
goat_data$individual.local.identifier <- paste(goat_data$collar_id, goat_data$year, sep = "_")

# convert to amt track_xyz object
goat_track <- make_track(goat_data, 
                         .x = lon_reproj,             # x-coordinates (projected)
                         .y = lat_reproj,             # y-coordinates (projected)
                         .t = timestamp,     # timestamp column in POSIXct format
                         crs = "epsg:5070",        # Assuming UTM Zone 10N (adjust if necessary)
                         all_cols = TRUE     # Retain all other columns
)

goats <- unique(goat_data$individual.local.identifier)

# prepare objects for saving
ssf_dat_list <- vector("list", length(goats))
issa_m1 <- vector("list", length(goats))
issa_m2 <- vector("list", length(goats))
issa_m4 <- vector("list", length(goats))
sl_list <- vector("list", length(goats))
ta_list <- vector("list", length(goats))
coeff_list <- vector("list", length(goats))
rss_data <- data.frame(individual.local.identifier = rep(NA, length(goats)),
                       collar_id = rep(NA, length(goats)),
                       goat_name = rep(NA, length(goats)),
                       year = rep(NA, length(goats)),
                       like50 = rep(NA, length(goats)),
                       like100 = rep(NA, length(goats)),
                       rss_m1 = rep(NA, length(goats)),
                       like50_lo = rep(NA, length(goats)),
                       like100_lo = rep(NA, length(goats)),
                       like50_med = rep(NA, length(goats)),
                       like100_med = rep(NA, length(goats)),
                       like50_hi = rep(NA, length(goats)),
                       like100_hi = rep(NA, length(goats)),
                       rss_lo = rep(NA, length(goats)),
                       rss_med = rep(NA, length(goats)),
                       rss_hi = rep(NA, length(goats)))
#clean up environment
rm(goat_coords)

if (!dir.exists("data/issa/model_summary/")) {
  dir.create("data/issa/model_summary/", recursive = TRUE)
}




#.................................................................
# START LOOP ----

tic(msg = "issa")
for (i in seq_along(goats)) {
  #subset an individual now
  current_goat <- goats[i]
  # subset a goat
  subset_goat <- goat_track[goat_track$individual.local.identifier == current_goat,]
  # reorder in ascending order
  subset_goat <- subset_goat[order(subset_goat$t_),]
  cat(bgMagenta(paste0("on iteration ", i, " | mountain goat and year: ", current_goat)), "\n")
  cat(bgMagenta(paste0("Number of fixes in subset: ", nrow(subset_goat), "\n")))
  
  study_year <- unique(as.character(subset_goat$year))
  collar_id <- unique(as.character(subset_goat$collar_id))
  goat_name <- unique(as.character(subset_goat$goat_name))
  individual.local.identifier <- unique(as.character(subset_goat$individual.local.identifier))
  # name the objects in the list
  names(ssf_dat_list)[i] <- individual.local.identifier
  names(issa_m1)[i] <- individual.local.identifier
  names(issa_m2)[i] <- individual.local.identifier
  names(issa_m4)[i] <- individual.local.identifier
  names(sl_list)[i] <- individual.local.identifier
  names(ta_list)[i] <- individual.local.identifier
  names(coeff_list)[i] <- individual.local.identifier
  
  
  # 1. Format and Generate Random Steps ----
  ssf_dat <- tryCatch({
    subset_goat %>% 
      track_resample(rate = minutes(375), tolerance = minutes(60)) %>%  #resample the track to get regular time steps based on summarize_sampling_rate
      steps_by_burst() %>% # convert from points to steps
      #~~~~~~ to check the number of steps in a burst
      # add_count(burst_) %>%  # Count steps per burst
      # filter(n >= 3) %>% # Keep bursts with 3+ steps only
      # select(-n) %>% # remove counter column
      #~~~~~~
      random_steps() %>%  # generate random available steps, number of random steps can be set using 'n ='
      #NOTE: Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length
      # set the # of steps, n = n_random_steps
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
    message(paste("Error problem with generating random steps, moving onto the next goat", individual.local.identifier, ":", e$message))
    
    # Return NULL if error occurs
    return(NULL)
  })
  
  # if null is returned, then fill in NA values, skip the rest of the analysis because issue with data and move onto the next individual
  if (is.null(ssf_dat)) {
    # create a dummy dataframe and fill in accordingly with NA or with known info
    ssf_dat <- data.frame(matrix(nrow = 1, ncol = 0))  
    ssf_dat[1, c("burst_", "x1_", "x2_", "y1_", "y2_", "sl_", "ta_", "t1_", "t2_", "dt_",
                 "case_", "step_id_", "elev_25m_start", "elev_25m_end", "dist_escape_25m_start",
                 "dist_escape_25m_end", "cos_ta_", "log_sl_", "individual.local.identifier",
                 "collar_id", "goat_name", "year")] <- NA
    ssf_dat$individual.local.identifier <- individual.local.identifier
    ssf_dat$collar_id <- collar_id
    ssf_dat$goat_name <- goat_name
    ssf_dat$year <- study_year
    
    rss_data$individual.local.identifier[i] <- individual.local.identifier
    rss_data$collar_id[i] <- collar_id
    rss_data$goat_name[i] <- goat_name
    rss_data$year[i] <- study_year
    rss_data[i, c("like50", "like100", "rss_m1",
                  "like50_lo", "like100_lo",
                  "like50_med", "like100_med",
                  "like50_hi", "like100_hi",
                  "rss_lo", "rss_med", "rss_hi")] <- NA
    
    plot_sl <- data.frame(
      step_length = seq(from = 0, to = 400, length.out = 300),
      prob_density = NA_real_,
      elev_level = NA_character_,
      individual.local.identifier = individual.local.identifier,
      collar_id = collar_id,
      goat_name = goat_name,
      year = study_year
    )
    plot_ta <- data.frame(
      turn_angle = seq(from = -pi, to = pi, length.out = 300),
      prob_density = NA_real_,
      elev_level = NA_character_,
      individual.local.identifier = individual.local.identifier,
      collar_id = collar_id,
      goat_name = goat_name,
      year = study_year
    )
    
    
    ssf_dat_list[[i]] <- ssf_dat
    issa_m1[[i]] <- NA
    issa_m2[[i]] <- NA
    issa_m4[[i]] <- NA
    sl_list[[i]] <- plot_sl
    ta_list[[i]] <- plot_ta
    coeff_list[[i]] <- NA
    next
  }
  
  #..............................................................
  ## check original distributions (tentative, i.e. resampled raw data) ----
  #................................................................
  sl_distr(ssf_dat)
  ta_distr(ssf_dat)
  print(paste0("original kappa = ", ta_distr(ssf_dat)$params$kappa))
  
  
  
  
  #//////////////////////////////////////////////////////////////////////
  #//////// start modelling ////////////////////////////////////// ----
  #//////////////////////////////////////////////////////////////////////
  
  #............................................................
  # 2. Fit issf ----
  #............................................................
  
  # start of step is the movement process, the influences of habitat on the movement with turn angle or step length
  
  # model 1: basic model (covariates not interacting with any movement parameters, i.e. step length, turn angles)
  cat(bgBlue("fit issf model 1","\n"))
  m1 <- ssf_dat %>% 
    fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +
               sl_ + log_sl_ + cos_ta_ + 
               strata(step_id_), model = TRUE)
  
  # look sl, log_sl, cos_ta p-values, are they significant or not?
  # coefficient for covariate indicates + = selection or - = avoidance
  # distance to a feature (such as distance to water, fire etc) can be tricky, + = selection for areas further from feature which means feature itself is avoided
  # movement parameters
  # Coefficient for log(Step Length) modifies shape parameter indicating + = more longer steps, - = fewer longer steps 
  # Coefficient for Step Length modifies the scale parameter indicating + = longer steps, - = shorter steps
  # cos[TA] converts the circular measure (turning angle of 360 degrees) to linear
  # moving backwards = -1, random walk = 0, moving forward = 1
  # note: cos[TA] can break the model, use check_model() or model_performance() 
  # kappa = directionality of movement, i.e. increase in kappa = forward movement, note: kappa = concentration parameter of the vonMises distributions
  # end of the step is selection process (i.e., where they selected/decided to be/end up)
  summary(m1)
  
  print(performance::check_model(m1$model, residual_type = "normal"))
  print(performance::model_performance(m1$model))
  
  #extract model coeffs
  m1_coeff <- data.frame(term = names(m1$model$coefficients),
                         m1_coeff = m1$model$coefficients)
  
  # save the summary output
  sink(paste0("data/issa/model_summary/m1/", unique(as.character(subset_goat$individual.local.identifier)), "_issa_m1_summary.txt"))
  cat("\n") #enter blank line
  print(paste0("ISSA model for collar ID: ", unique(as.character(subset_goat$collar_id)), " | ", unique(as.character(subset_goat$goat_name))))
  cat("\n") #enter blank line
  print(summary(m1))
  sink() #terminate output exporting connection/process
  
  
  #............................................................
  # 3. RSS for two locations without ci values ----
  #............................................................
  
  # log_rss with no ci values
  
  ## unit change distribution (i.e. single unit change in elevation for comparison), keeping dist_escape constant ----
  # s1: elev = 3, dist_escape = 1.5
  # s2: elev = 2, dist_escape = 1.5
  # create 2 dataframes to calculate log-RSS
  s1 <- data.frame(
    elev_25m_end = 3,
    dist_escape_25m_end = 1.5,
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  s2 <- data.frame(
    elev_25m_end = 2,
    dist_escape_25m_end = 1.5,
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  
  # calculate log-RSS
  lr1 <- log_rss(m1, x1 = s1, x2 = s2)
  lr1$df
  
  
  
  
  #.............................................
  ##range distribution (i.e. changing the elevation into unit range of -2 to +2) ----
  # create 2 dataframes using elevation values as a range to calculate log-RSS
  s1 <- data.frame(
    elev_25m_end = seq(from = -2, to = 2, length.out = 200),
    dist_escape_25m_end = 1.5,
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  s2 <- data.frame(
    elev_25m_end = 0, # mean of elev, since we scaled and centered
    dist_escape_25m_end = 1.5,
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  
  # Calculate log-RSS
  lr2 <- log_rss(m1, s1, s2)
  lr2$df
  
  #log_rss
  lr2_df_log_plot <- 
    ggplot(lr2$df, aes(x = elev_25m_end_x1 , y = log_rss)) + 
    geom_line(size = 1) + 
    geom_hline(yintercept = 0 , linetype =  "dashed", color = "gray30") + 
    xlab("Elevation (SD)") + 
    ylab( "log-RSS vs Mean Elevation") + 
    theme_bw()
  lr2_df_log_plot
  
  # rss plot
  lr2_df_plot <- ggplot(lr2$df, aes(x = elev_25m_end_x1 , y = exp(log_rss))) + 
    geom_line(size = 1) + 
    geom_hline(yintercept = 0 , linetype =  "dashed", color = "gray30") + 
    xlab("Elevation (SD)") + 
    ylab( "RSS vs Mean Elevation") + 
    theme_bw()
  lr2_df_plot
  
  
  #............................................................
  # 4. get log_rss with CI values for model uncertainty ----
  #............................................................
  
  # log_rss with ci values
  
  # using large-sample CI approach instead of bootstrapping due to sample size to avoid computing time for calculating bootstrap CI
  # use standard errors from fitted model to estimate the CI based on normal approximation of the sampling distribution of beta hat
  
  lr2_ci_se <- log_rss(m1, s1, s2, ci = "se", ci_level = 0.95)
  head(lr2_ci_se$df)
  
  lr2_ci_se_plot <- ggplot(lr2_ci_se$df, aes(x = elev_25m_end_x1, y = log_rss)) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), linetype = "dashed", color = "black", fill = "gray80", alpha = 0.5  ) + 
    geom_line(size = 1 ) + 
    geom_hline(yintercept = 0 , linetype = "dashed" , color = "gray30") + 
    ggtitle("log rss with ci values via standard error from fitted model") +
    xlab( "Elevation (SD)") + 
    ylab("log-RSS vs Mean Elevation") + 
    theme_bw()
  print(lr2_ci_se_plot)
  ggsave(lr2_ci_se_plot, file = paste0("figures/issa/log_rss_ci/m1_", individual.local.identifier, ".png"),
         create.dir = TRUE)
  
  #......................................................................
  # 5. update m1 distributions ----
  #......................................................................
  
  cat(bgBlue("update distributions (model 1)","\n"))
  
  # after fitting issf model, the tentative (original, i.e. resampled raw data) distribution needs to be updated with the fitted values (the betas) from the model to produce the new parameters for the selection-free distribution via amt functions
  updated_sl <- update_sl_distr(m1)
  updated_ta <- update_ta_distr(m1)
  
  # check object class and output
  class(updated_sl)
  class(updated_ta)
  updated_sl
  updated_ta
  # look at the kappa value in ta, is it positive or negative? needs to be positive
  print(paste0("m1 updated kappa = ", updated_ta$params$kappa))
  
  
  
  #......................................................................
  # 6. Calculate RSS for Movements ----
  #......................................................................
  
  # likelihood of animal taking a 50m vs 100m step from their starting location. Use updated pdf of sl distribution
  # how much more likely are they to take a shorter (i.e., 50m) step vs longer step (i.e., 100m), the output is the likeliness of 50m instead of 100m
  
  # calculate likelihood under selection-free sl distribution
  like50 <- dgamma(50, 
                   shape = updated_sl$params$shape,
                   scale = updated_sl$params$scale)
  like100 <- dgamma(100, 
                    shape = updated_sl$params$shape,
                    scale = updated_sl$params$scale)
  rss_m1 <- like50/like100
  
  
  
  
  #//////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////// ----
  #//////////////////////////////////////////////////////////////////////
  
  
  
  #......................................................................
  # 2. Fit issf MODEL 2 ----
  #......................................................................
  
  # model 2: quadratic term (x^2), 
  # non-linear effect/relationship (e.g. what if the animal selects for intermediate elevations and avoids locations with extremely low or high elevations?), can use splines or polynomials to capture this non-linear effect. 
  cat(bgBlue("fit issf model 2","\n"))
  m2 <- ssf_dat %>% 
    fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end + I(elev_25m_end^2) + 
               sl_ + log_sl_ + cos_ta_ + 
               strata(step_id_), model = TRUE)
  summary(m2)
  performance::check_model(m2$model, residual_type = "normal")
  performance::model_performance(m2$model)
  
  #extract model coeffs
  m2_coeff <- data.frame(term = names(m2$model$coefficients),
                         m2_coeff = m2$model$coefficients)
  
  # save the summary output
  sink(paste0("data/issa/model_summary/m2/", unique(as.character(subset_goat$individual.local.identifier)), "_issa_m2_summary.txt"))
  cat("\n") #enter blank line
  print(paste0("ISSA model for collar ID: ", unique(as.character(subset_goat$collar_id)), " | ", unique(as.character(subset_goat$goat_name))))
  cat("\n") #enter blank line
  print(summary(m2))
  sink() #terminate output exporting connection/process
  
  
  
  #............................................................
  # 3. RSS for two locations without ci values (model 2) ----
  #............................................................
  
  # by adding quadratic term, log_rss for 1 unit change is no longer constant (compared to m1).
  cat(bgBlue("RSS for two locations without ci values (model 2)","\n"))
  
  ## unit change distribution (i.e. single unit change in elevation for comparison), keeping dist_escape constant ----
  # compare log-rss with 1 unit change for in the 2 models (m1 linear elevation vs m2 quadratic elevation)
  
  # log_rss with no ci values
  # differ by unit value (i.e. 1 unit comparison), keeping dist_escape constant
  # s1: elev = 3, dist_escape = 1.5
  # s2: elev = 2, dist_escape = 1.5
  # s3: elev = -1, dist_escape = 1.5
  # s4: elev = -2, dist_escape = 1.5
  
  
  # create 4 dataframes to compare rss values between model 1 and 2
  s1 <- data.frame(
    elev_25m_end = 3, # mean of elev, since we scaled and centered
    dist_escape_25m_end = 1.5,
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  s2 <- data.frame(
    elev_25m_end = 2, # mean of elev, since we scaled and centered
    dist_escape_25m_end = 1.5,
    # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  
  s3 <- data.frame(
    elev_25m_end = -1, # mean of elev, since we scaled and centered
    dist_escape_25m_end = 1.5,
    # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  s4 <- data.frame(
    elev_25m_end = -2, # mean of elev, since we scaled and centered
    dist_escape_25m_end = 1.5,
    # landuseC_end = factor("wet",  levels = levels(ssf_dat$landuseC_end)),
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  
  #.....................................................
  # compare the two sets of locations
  # Calculate log-RSS under m1
  lr_m1_s1s2 <- log_rss(m1, s1, s2)
  lr_m1_s3s4 <- log_rss(m1, s3, s4)
  
  # Calculate log-RSS under m2
  lr_m2_s1s2 <- log_rss(m2, s1, s2)
  lr_m2_s3s4 <- log_rss(m2, s3, s4)
  
  print(data.frame(
    "model" = c("linear", "quadratic"),
    "logRSS_s1_s2" = round(c(lr_m1_s1s2$df$log_rss, lr_m2_s1s2$df$log_rss), 3),
    "logRSS_s3_s4" = round(c(lr_m1_s3s4$df$log_rss, lr_m2_s3s4$df$log_rss), 3)))
  
  
  #.............................................
  ##range distribution (i.e. changing the elevation into unit range of -2 to +2) ----
  
  # Make a new data.frame for s1 and s2
  s1 <- data.frame(
    elev_25m_end = seq(from = -2, to = 2, length.out = 100),
    dist_escape_25m_end = 1.5,
    # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  s2 <- data.frame(
    elev_25m_end = 0, # mean of elev, since we scaled and centered
    dist_escape_25m_end = 1.5,
    # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
    sl_ = 100,
    log_sl_ = log(100),
    cos_ta_ = 1)
  
  # Calculate log-RSS
  lr_m2 <- log_rss(m2, s1, s2)
  lr_m2$df
  
  
  # Plot using ggplot2
  lr_m2_plot <- ggplot(lr_m2$df, aes(x = elev_25m_end_x1, y = log_rss)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    xlab("Elevation (SD)") +
    ylab("log-RSS vs Mean Elevation") +
    theme_bw()
  print(lr_m2_plot)
  ggsave(lr_m2_plot, file = paste0("figures/issa/log_rss_range/m1_elev_", individual.local.identifier, ".png"),
         create.dir = TRUE)
  
  #//////////////////////////////////////////////////////////////////////
  #////////////////////////////////////////////////////////////////////// ----
  #//////////////////////////////////////////////////////////////////////
  
  
  
  #......................................................................
  # 2. Fit issf MODEL 4 ----
  #......................................................................
  
  # model 4: interaction terms with movement characteristics and continuous explanatory variables capturing environmental characteristics
  # e.g. what if the animal moves differently based on habitat feature that is at the start of the movement step? 
  
  # seeing how covariates influences their decision making on where to go (end step)??????
  cat(bgBlue("fit issf model 4","\n"))
  
  m4 <- ssf_dat %>% 
    fit_issf(case_ ~ elev_25m_end +  I(elev_25m_end^2) + dist_escape_25m_end +   
               sl_ + log_sl_ + cos_ta_ + 
               elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
               dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + # added dist to esc
               strata(step_id_), model = TRUE)
  summary(m4)
  
  print(performance::check_model(m4$model, residual_type = "normal"))
  print(performance::model_performance(m4$model))
  
  # extract m4 coeff
  m4_coeff <- data.frame(term = names(m4$model$coefficients),
                         m4_coeff = m4$model$coefficients)
  
  # save the summary output
  sink(paste0("data/issa/model_summary/m4", unique(as.character(subset_goat$individual.local.identifier)), "_issa_m4_summary.txt"))
  cat("\n") #enter blank line
  print(paste0("ISSA model for collar ID: ", unique(as.character(subset_goat$collar_id)), " | ", unique(as.character(subset_goat$goat_name))))
  cat("\n") #enter blank line
  print(summary(m4))
  sink() #terminate output exporting connection/process
  
  
  
  #......................................................................
  # 5. update m4 distributions ----
  #......................................................................
  
  ##range distribution (i.e. changing the elevation into unit range of -2 to +2) ----
  
  # Creating elevation values -> low, med, high
  # continuous to categorical -> based on values that corresponded to mean and +/- 2 standard deviates from the mean (because the covariates were scaled and centered)
  # after fitting issf model, distribution needs to be updated for the three values of elevation
  # manually update distribution from the iSSF coefficients using amt functions, update_gamma(dist, beta_sl, beta_log_sl) and update_vonmises(dist, beta_cos_ta)
  cat(bgBlue("update distributions (model 4)","\n"))
  
  # Low elevation step-length distribution
  low_sl <- update_gamma(
    dist = m4$sl_,
    beta_sl = m4$model$coefficients["sl_"] +
      -2 * m4$model$coefficients["sl_:elev_25m_start"],
    beta_log_sl = m4$model$coefficients["log_sl_"] +
      -2 * m4$model$coefficients["log_sl_:elev_25m_start"])
  # Medium elevation step-length distribution
  med_sl <- update_gamma(
    dist = m4$sl_,
    beta_sl = m4$model$coefficients["sl_"] +
      0 * m4$model$coefficients["sl_:elev_25m_start"],
    beta_log_sl = m4$model$coefficients["log_sl_"] +
      0 * m4$model$coefficients["log_sl_:elev_25m_start"])
  # high step-length distribution
  hi_sl <- update_gamma(
    dist = m4$sl_,
    beta_sl = m4$model$coefficients["sl_"] +
      2 * m4$model$coefficients["sl_:elev_25m_start"],
    beta_log_sl = m4$model$coefficients["log_sl_"] +
      2 * m4$model$coefficients["log_sl_:elev_25m_start"])
  
  
  #.............................
  # Creating turn angle values -> low, med, high
  
  # low turn-angle distribution
  low_ta <- update_vonmises(
    dist = m4$ta_,
    beta_cos_ta = m4$model$coefficients["cos_ta_"] + 
      -2 * m4$model$coefficients["cos_ta_:elev_25m_start"])
  print(paste0("m4 low ta kappa = ", low_ta$params$kappa))
  # med turn-angle distribution
  med_ta <- update_vonmises(
    dist = m4$ta_,
    beta_cos_ta = m4$model$coefficients["cos_ta_"] +
      0 * m4$model$coefficients["cos_ta_:elev_25m_start"])
  print(paste0("m4 med ta kappa = ", med_ta$params$kappa))
  # hi turn-angle distribution
  hi_ta <- update_vonmises(
    dist = m4$ta_,
    beta_cos_ta = m4$model$coefficients["cos_ta_"] +
      2 * m4$model$coefficients["cos_ta_:elev_25m_start"])
  print(paste0("m4 high ta kappa = ", hi_ta$params$kappa))
  
  #~~~~~~~~~~~~~~~~~~~~~~
  # checking where beta_cos_ta becomes a negative value over the range of elev_25m_start values
  
  # extract model coeffs
  coeffs <- m4$model$coefficients
  # create a sequence of values ranging min and max values of elev_25m_start to plot
  elev_range <- data.frame(elev = seq(min(ssf_dat$elev_25m_start), max(ssf_dat$elev_25m_start), length.out = 100))
  # calculate the effect of elevation on turning angle (i.e., effect of turning angle and how elevation changes it, shows how straight the movement is or not based on elevation)
  elev_range$beta_cos_ta <- coeffs["cos_ta_"] + coeffs["cos_ta_:elev_25m_start"] * elev_range$elev
  
  elev_range_plot <- ggplot(data = elev_range, aes(x = elev, y = beta_cos_ta)) +
    geom_line() + 
    geom_hline(yintercept = 0, color = "red")
  print(elev_range_plot)
  # beta_cos_ta is negative through all of it
  # cos_ta_:elev_25m_start term is a small number compared to cos_ta (main effect), therefore its producing a negative kappa
  # cos_ta_ is a highly significant p value but cos_ta_:elev_25m_start is not significant
  # .-. turns are more likely than going straight across all elevation ranges??????
  # or elevation range is too small to get meaningful variation in the turning behaviour??????
  # if kappa is always negative, then the animal is favouring turning over going in a straight line (i.e. avoidance of straight behaviour) or elevation doesnt affect turning behaviour?????
  # when using actual elevation in meters and not scaled, then replotting, it still shows the same thing as if it was on a -2 to 2 range scale
  # may want to use unscaled elevation to get actual values for elevation metric of preference instead of what is low med high??????
  
  #.......................................................
  ## check distribution by plotting ----
  #.......................................................
  
  cat(bgBlue("check m4 distributions by plotting","\n"))
  
  # step lengh distribution 
  #create a data.frame for plotting
  plot_sl <- data.frame(step_length = rep(NA, 100))
  # x-axis is sequence of possible step lengths
  plot_sl$step_length <- seq(from = 0, to = 400, length.out = 100)
  # y-axis is the probability density under the given gamma distribution
  plot_sl$low <- dgamma(x = plot_sl$step_length, 
                        shape = low_sl$params$shape,
                        scale = low_sl$params$scale)
  plot_sl$medium <- dgamma(x = plot_sl$step_length, 
                           shape = med_sl$params$shape,
                           scale = med_sl$params$scale)
  plot_sl$high <- dgamma(x = plot_sl$step_length, 
                         shape = hi_sl$params$shape,
                         scale = hi_sl$params$scale)
  # data carpentry
  plot_sl <- plot_sl %>% 
    pivot_longer(cols = -step_length)
  plot_sl <- rename(plot_sl, c(prob_density = value,
                               elev_level = name))
  plot_sl$individual.local.identifier <- individual.local.identifier
  plot_sl$collar_id <- collar_id
  plot_sl$goat_name <- goat_name
  plot_sl$year <- study_year
  
  p1 <- ggplot(plot_sl, aes(x = step_length, y = prob_density, color = factor(elev_level))) +
    geom_line(size = 1) +
    scale_color_manual(name = "Elevation",
                       breaks = c("low", "medium", "high"),
                       values = c("navyblue", "gray50", "firebrick")) +
    xlab("Step Length (m)") +
    ylab("Probability Density") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), #removes horizontal gridlines
          panel.grid.minor = element_blank()) #removes vertical gridlines
  print(p1)
  
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
                        kappa = low_ta$params$kappa,
                        mu = 0)
    # issue with plotting low -> kappa value is negative (and cannot be negative(?))
  }, error = function(e) {
    cat(yellow("Error m4 low turn angle data (plot):", individual.local.identifier, "-", e$message, "\n"))
    # if an error occurs, then fill the column with NA values
    rep(NA_real_, nrow(plot_ta))
  })
  
  
  # med
  plot_ta$medium <-   tryCatch({
    circular::dvonmises(x = plot_ta$turn_angle, 
                        kappa = med_ta$params$kappa,
                        mu = 0)
  }, error = function(e) {
    cat(yellow("Error m4 med turn angle data (plot):", individual.local.identifier, "-", e$message, "\n"))
    # if an error occurs, then fill the column with NA values
    rep(NA_real_, nrow(plot_ta))
  })
  
  # hi
  plot_ta$high <-   tryCatch({
    circular::dvonmises(x = plot_ta$turn_angle, 
                        kappa = hi_ta$params$kappa,
                        mu = 0)
  }, error = function(e) {
    cat(yellow("Error m4 high turn angle data (plot):", individual.local.identifier, "-", e$message, "\n"))
    # if an error occurs, then fill the column with NA values
    rep(NA_real_, nrow(plot_ta))
  })
  
  # data carpentry
  plot_ta <- plot_ta %>% 
    pivot_longer(cols = -turn_angle)
  plot_ta <- rename(plot_ta, c(prob_density = value,
                               elev_level = name))
  plot_ta$individual.local.identifier <- individual.local.identifier
  plot_ta$collar_id <- collar_id
  plot_ta$goat_name <- goat_name
  plot_ta$year <- study_year
  
  # Plot
  p2 <- ggplot(plot_ta, aes(x = turn_angle, y = prob_density, color = factor(elev_level))) +
    geom_line(size = 1) +
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
  print(p2)
  
  
  #.......................................................
  ## Plot Multi-panel ----
  #.......................................................
  
  # Combine and save plot
  if (exists("p2")) {
    plot_elevation <- ggarrange(
      p1, p2,
      ncol = 2,
      common.legend = TRUE,
      legend = "top"
    )
  } else {
    plot_elevation <- ggarrange(
      p1,
      ncol = 1,
      common.legend = TRUE,
      legend = "top"
    )
  }
  
  # add plot title
  plot_elevation <- annotate_figure(plot_elevation,
                                    top = text_grob(paste("Model 4 ISSA |", individual.local.identifier), 
                                                    hjust = 0.5))
  print(plot_elevation)
  # Save the plot
  ggsave(plot_elevation,
         file = paste0("figures/issa/issa_elevation_", individual.local.identifier, ".png"),
         width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")
  
  
  #......................................................................
  # 6. Calculate RSS for Movements ----
  #......................................................................
  cat(bgBlue("Calculating Movement RSS sl for model 4","\n"))
  
  # note: using step length
  # calculate likelihood under selection-free sl distribution
  # we dont want to use log_rss() because the fitted movement parameters in issf represented updates to the original sl distribution and not to the distribution inself, therefore use the updated pdf of the sl distribution directly
  # to calculate likelihood under selection-free sl distribution
  # is this log_rss or rss value?
  #low sl
  like50_lo <- dgamma(50, 
                      shape = low_sl$params$shape,
                      scale = low_sl$params$scale)
  like100_lo <- dgamma(1:200, 
                       shape = low_sl$params$shape,
                       scale = low_sl$params$scale)
  rss_lo <- like50_lo/like100_lo
  # medium sl
  like50_med <- dgamma(50, 
                       shape = med_sl$params$shape,
                       scale = med_sl$params$scale)
  like100_med <- dgamma(1:200, 
                        shape = med_sl$params$shape,
                        scale = med_sl$params$scale)
  rss_med <- like50_med/like100_med
  # high sl
  like50_hi <- dgamma(50, 
                      shape = hi_sl$params$shape,
                      scale = hi_sl$params$scale)
  like100_hi <- dgamma(1:200, 
                       shape = hi_sl$params$shape,
                       scale = hi_sl$params$scale)
  rss_hi <- like50_hi/like100_hi
  
  # 
  # plot(rss_hi, type = "l", col = "red")
  # lines(rss_med, type = "l", col = "grey20")
  # lines(rss_lo, type = "l", col = "blue")
  # abline(h = 1, col = "grey", lty = "dashed")
  # 
  
  #.......................................................
  # Store outputs ----
  #.......................................................
  
  ssf_dat$individual.local.identifier <- individual.local.identifier
  ssf_dat$collar_id <- collar_id
  ssf_dat$goat_name <- goat_name
  ssf_dat$year <- study_year
  
  rss_data$individual.local.identifier[i] <- individual.local.identifier
  rss_data$collar_id[i] <- collar_id
  rss_data$goat_name[i] <- goat_name
  rss_data$year[i] <- study_year
  rss_data$like50[i] <- like50
  rss_data$like100[i] <- like100
  rss_data$rss_m1[i] <- rss_m1
  rss_data$like50_lo[i] <- like50_lo
  rss_data$like100_lo[i] <- like100_lo
  rss_data$like50_med[i] <- like50_med
  rss_data$like100_med[i] <- like100_med
  rss_data$like50_hi[i] <- like50_hi
  rss_data$like100_hi[i] <- like100_hi
  rss_data$rss_lo[i] <- rss_lo
  rss_data$rss_med[i] <- rss_med
  rss_data$rss_hi[i] <- rss_hi
  
  m12_coeffs <- merge(m1_coeff, m2_coeff, all = TRUE)
  m_coeffs <- merge(m12_coeffs, m4_coeff, all = TRUE)
  m_coeffs$individual.local.identifier <- individual.local.identifier
  m_coeffs$collar_id <- collar_id
  m_coeffs$goat_name <- goat_name
  m_coeffs$year <- study_year
  
  coeff_list[[i]] <- m_coeffs
  ssf_dat_list[[i]] <- as.data.frame(ssf_dat)
  issa_m1[[i]] <- m1
  issa_m2[[i]] <- m2
  issa_m4[[i]] <- m4
  sl_list[[i]] <- as.data.frame(plot_sl)
  ta_list[[i]] <- as.data.frame(plot_ta)
  
}

beepr::beep(3)
toc()

# END LOOP ----
#///////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////


#...........................................................
# save outputs ----
#...........................................................

# convert list dataframe
m_coeff_data <- do.call(rbind, coeff_list)
m_coeff_data <- relocate(m_coeff_data, c(individual.local.identifier, collar_id, goat_name), .before = term)
save(m_coeff_data, file = "data/issa/m_coeff_20250616.rda")
load("data/issa/m_coeff_20250616.rda")

# convert list into a dataframe
issa_data <- do.call(rbind, ssf_dat_list)
issa_data <- relocate(issa_data, c(individual.local.identifier, collar_id, goat_name), .before = burst_)
save(issa_data, file = "data/issa/issa_data_20250616.rda")
load("data/issa/issa_data_20250616.rda")

save(rss_data, file = "data/issa/rss_data_20250616.rda")
load("data/issa/rss_data_20250616.rda")
write.csv(rss_data, file = "data/issa/rss_data_20250616.csv")

sl_data <- do.call(rbind, sl_list)
ta_data <- do.call(rbind, ta_list)
save(sl_data, file = "./data/issa/sl_data_20250616.rds")
save(ta_data, file = "./data/issa/ta_data_20250616.rds")


#...........................................................
# model coeff averages ----
#...........................................................

# group data based on coeff and year for each model and get the mean
mean_m1 <- aggregate(m1_coeff ~ term + year, data = m_coeff_data, FUN = mean, na.rm = TRUE)
mean_m2 <- aggregate(m2_coeff ~ term + year, data = m_coeff_data, FUN = mean, na.rm = TRUE)
mean_m4 <- aggregate(m4_coeff ~ term + year, data = m_coeff_data, FUN = mean, na.rm = TRUE)

# plot the coeff based on year and term
ggplot(mean_m4, aes(x = year, y = m4_coeff, color = term, group = term)) +
  geom_line() +
  geom_point() +
  theme_bw()

# reorganize the df for easier reading but not used for plotting, i.e. organize columns by year (names_from), and fill in with m1_coeff data (values_from)
mean_m1 <- mean_m1 %>%
  pivot_wider(names_from = year, values_from = m1_coeff)
mean_m2 <- mean_m2 %>%
  pivot_wider(names_from = year, values_from = m2_coeff)
mean_m4 <- mean_m4 %>%
  pivot_wider(names_from = year, values_from = m4_coeff)
mean_m4 <- as.data.frame(mean_m4)


#...........................................................
# rss data 
#...........................................................

# likelihood of animal taking a 50m vs 100m step from their starting location. Use updated pdf of sl distribution
# how much more likely are they to take a shorter (i.e., 50m) step vs longer step (i.e., 100m), the output is the likeliness of 50m instead of 100m

# average rss value per year, since lo, med and hi are the same for each year, only taking one of them and plotting
rss_mean <- aggregate(rss_med ~ year, data = rss_data, FUN = mean, na.rm = TRUE)
write.csv(rss_mean, file = "./data/issa/rss_mean_m4.csv")

ggplot(data = rss_mean, aes(x = year, y = rss_med, group = 1)) + 
  geom_line()


log(rss_mean$rss_med)



#...........................................................
# plot issa sl and ta results ----
#...........................................................


goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377")
strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))

# all years and individuals, facet wrap
# sl_plot_facetwrap <- 
ggplot(data = sl_data, aes(x = step_length, y = prob_density, color = factor(elev_level))) +
  geom_line(size = 1) +
  facet_wrap2(~ collar_id + year, 
              # scales = "fixed",
              ncol = 5, nrow = 7) +
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  # ggtitle(as.character(yr)) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        legend.position = "bottom",
        legend.justification = "center",
        strip.text = element_text(color = "white", face = "bold"))
# ggsave(sl_plot_facetwrap, file = "figures/issa/sl_plot_facetwrap.png")

# as a grid, each row is a year and each column is an animal
sl_plot_grid <-
  ggplot(data = sl_data, aes(x = step_length, y = prob_density, color = factor(elev_level))) +
  geom_line(size = 1) +
  facet_grid(year ~ collar_id, scales = "free_x") + 
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        strip.text = element_text(color = "white", face = "bold"))
ggsave(sl_plot_grid, file = "figures/issa/sl_plot/sl_plot_grid.png")


# as per year for all goats
goat_years <- unique(sl_data$year)
for (i in goat_years) {
  sl_plot <- ggplot(data = subset(sl_data, year == i),
                    aes(x = step_length, y = prob_density, color = factor(elev_level))) +
    geom_line(size = 1) +
    facet_wrap2(~ collar_id, ncol = 2, nrow = 3, strip = strip_col) +  # facets by collar_id only
    scale_color_manual(name = "Elevation",
                       breaks = c("low", "medium", "high"),
                       values = c("navyblue", "gray50", "firebrick")) +
    xlab("Step Length (m)") +
    ylab("Probability Density") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.justification = "center",
          strip.text = element_text(color = "white", face = "bold")) +
    ggtitle(i)
  
  print(sl_plot)
  
  ggsave(sl_plot, file = paste0("./figures/issa/sl_plot/sl_plot_", i, ".png"))
}


#..............................................
# ta plots

# as a grid, each row is a year and each column is an animal
ta_plot_grid <-
  ggplot(data = ta_data, aes(x = turn_angle, y = prob_density, color = factor(elev_level))) +
  geom_line(size = 1) +
  facet_grid(year ~ collar_id, scales = "free_x") + 
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  xlab("Turn Angle") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        strip.text = element_text(color = "white", face = "bold"))
ggsave(ta_plot_grid, file = "./figures/issa/ta_plot_grid.png")

# as per year for all goats
goat_years <- unique(ta_data$year)
for (i in goat_years) {
  ta_plot <- ggplot(data = subset(ta_data, year == i),
                    aes(x = turn_angle, y = prob_density, color = factor(elev_level))) +
    geom_line(size = 1) +
    facet_wrap2(~ collar_id, ncol = 2, nrow = 3, strip = strip_col) +  # facets by collar_id only
    scale_color_manual(name = "Elevation",
                       breaks = c("low", "medium", "high"),
                       values = c("navyblue", "gray50", "firebrick")) +
    xlab("Turn angle)") +
    ylab("Probability Density") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.justification = "center",
          strip.text = element_text(color = "white", face = "bold")) +
    ggtitle(i)
  
  print(ta_plot)
  
  ggsave(ta_plot, file = paste0("./figures/issa/ta_plot/ta_plot_", i, ".png"))
}



