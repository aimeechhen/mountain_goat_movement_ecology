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




# general workflow

# pre-prep gps data and habitat rasters
# 1. format and generate steps data
# 2. fit issf
# 3. update sl and ta distribution based on model predictions
# 4. plot movement parameters
# 5. extract outputs/results:
# get CI values
# calculate rss between 2 locations?
# calculate rss for movement


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


# # Visualization of the tracks:
# ggplot() + 
#   geom_spatraster(data = elev_scaled, show.legend = FALSE)  +
#   geom_path(data = goat_track, aes(x_, y_),  col= "black") +
#   facet_wrap(~ collar_id, nrow = 2) + 
#   scale_fill_terrain_c() +
#   theme_bw()


#.........................................................
# output set up ----
#.........................................................

# create foloder for saving outputs
dir.create("./data/issa/individual/", recursive = TRUE, showWarnings = FALSE)

# create folders for the model summaries
lapply(c("m1", "m2", "m4"), function(folder) {
  dir.create(file.path("./data/issa/individual/model_summary", folder), recursive = TRUE, showWarnings = FALSE)
})
# create folders for plots
lapply(c("sl_plot", "ta_plot"), function(folder) {
  dir.create(file.path("./figures/issa/individual/plot", folder), recursive = TRUE, showWarnings = FALSE)
})


ssf_dat_list <- vector("list", length(goats))
m1_list <- vector("list", length(goats))
m2_list <- vector("list", length(goats))
m4_list <- vector("list", length(goats))
# m5_list <- vector("list", length(goats))
# m6_list <- vector("list", length(goats))
# m7_list <- vector("list", length(goats))




# inspect sampling rate 
summarize_sampling_rate(goat_track)

# inspect sampling rates as per id_year
sampling_rates <- vector("list", length(goats))

for (i in seq_along(goats)) {
  current_goat <- goats[i]
  subset_goat <- goat_track[goat_track$id_year == current_goat, ]
  subset_goat <- subset_goat[order(subset_goat$t_), ]
  rate <- summarize_sampling_rate(subset_goat)
  rate$id_year <- current_goat
  sampling_rates[[i]] <- rate
}
# combine into df
sampling_rate_df <- do.call(rbind, sampling_rates)
range(sampling_rate_df$mean) # 4.638970 6.568744
mean(sampling_rate_df$mean) # 5.965427
# buffer of 5.965427-4.638970 = 1.326457
# 5.965427h = 357.9256min
# 1.326457h = 79.58742min


#////////////////////////////////////////////////////////////////
# UNPOOLED DATA (subset data) ----
#////////////////////////////////////////////////////////////////

# unpooled data workflow approach subsets the data for each animal and each animal is modelled/analyzed separately

tic(msg = "issa")

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
  names(ssf_dat_list)[i] <- current_goat
  names(m1_list)[i] <- current_goat
  names(m2_list)[i] <- current_goat
  names(m4_list)[i] <- current_goat
  names(m5_list)[i] <- current_goat
  names(m6_list)[i] <- current_goat
  
  # 1. Format and Generate Random Steps ----
  ssf_dat <- tryCatch({
    subset_goat %>% 
      track_resample(rate = minutes(375), tolerance = minutes(80)) %>%  #resample the track to get regular time steps based on summarize_sampling_rate
      steps_by_burst(keep_cols = 'start') %>% # convert from points to steps
      random_steps() %>% # generate random available steps
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
    
    ssf_dat_list[[i]] <- ssf_dat
    m1_list[[i]] <- NA
    m2_list[[i]] <- NA
    m4_list[[i]] <- NA
    # m5_list[[i]] <- NA
    # m6_list[[i]] <- NA
    
    next
  }
  
  
  #.............................................
  # calculate distance to fire for step_start----
  # Define the time window with +/- 1 hour buffer
  time_window <- 1 %#% 'hour' #ctmm package
  # initialise columns to store results
  if (any(ssf_dat$year == "2023")) {
    ssf_dat$fire_event <- "no"
    ssf_dat$n_fire_points <- NA
    ssf_dat$dist_to_fire_start <- NA
    
    #loop through every collar point
    for (j in 1:nrow(ssf_dat)) {
      # extract a collar point from data
      collar_point <- ssf_dat[j,]
      # create a custom timestamp of that collar point 
      collar_timestamp <- collar_point$t1_
      # Define time range +/- 1 hour around the collar point timestamp
      start_time <- collar_timestamp - time_window
      end_time <- collar_timestamp + time_window
      
      # extract fire point events within the time window
      fire_point <- fire[fire$timestamp >= start_time & fire$timestamp <= end_time,]
      # drop empty geometries
      fire_point <- fire_point[!st_is_empty(fire_point), ]
      # record number of fire points in the time window
      ssf_dat$n_fire_points[j] <- as.numeric(nrow(fire_point))
      
      # Check if there are any fire points within the time window
      if (nrow(fire_point) == 0) {
        # cat("No data fire points found in window", i, "- moving on to the next collar point.\n")
        # Set results to NA
        ssf_dat$dist_to_fire_start[j] <- NA
        
        next # Move to the next gps collar point if no fire is present
      }
      
      # If fire points exist in the time window, proceed with calculation
      tryCatch({
        if (nrow(fire_point) > 0) {
          ssf_dat$fire_event[j] <- "yes"
          # calculate the distance between the collar point and the fire point, output comes out as a matrix
          dist_matrix <- st_distance(collar_point, fire_point)
          #store results, extracting the min. distance between collar point and [multiple] fire point(s)
          ssf_dat$dist_to_fire_start[j] <- min(dist_matrix) # using min value instead of including all distances for all fire points, also causes issues when storing results due to multiple values
          
        } else {
          # If time window is empty, set results to NA
          ssf_dat$dist_to_fire_start[j] <- NA
          
        }
      }, error = function(e) {
        ssf_dat$dist_to_fire_start[j] <- NA
      })
    }
  } else {
    message("not a fire year")
  }
  
  #..............................................................
  ## check original distributions (tentative, i.e. resampled raw data) ----
  #................................................................
  sl_distr(ssf_dat)
  ta_distr(ssf_dat)
  print(paste0("original kappa = ", ta_distr(ssf_dat)$params$kappa))
  
  
  #............................................................
  # 2. Fit issf ----
  #............................................................
  
  # look sl, log_sl, cos_ta p-values, are they significant or not?
  # coefficient for covariate indicates + = selection or - = avoidance
  # distance to a feature (such as distance to water, fire etc) can be tricky, + = selection for areas further from feature which means feature itself is avoided
  # movement parameters
  # Coefficient for log(Step Length) modifies shape parameter indicating + = more longer steps, - = fewer longer steps 
  # Coefficient for Step Length modifies the scale parameter indicating + = longer steps, - = shorter steps
  # cos[TA] converts the circular measure (turning angle of 360 degrees) to linear; moving backwards = -1, random walk = 0, moving forward = 1 (DR. NOONAN SAYS THIS IS WRONG when reviewing the issa book source)
  # note: cos[TA] can break the model, use check_model() or model_performance() 
  # kappa = directionality of movement, i.e. increase in kappa = forward movement, note: kappa = concentration parameter of the vonMises distributions
  # end of the step is selection process (i.e., where they selected/decided to be/end up)
  
  ## model 1: basic model ----
  #covariates not interacting with any movement parameters, i.e. step length, turn angles)
  cat(bgBlue("fit issf model 1","\n"))
  m1 <- fit_issf(case_ ~ 
                   sl_ + log_sl_ + cos_ta_ + 
                   elev_25m_end + dist_escape_25m_end +
                   strata(step_id_), 
                 data = ssf_dat, model = TRUE)
  summary(m1)
  
  
  #......................................................................
  ## model 2: basic + quadratic term (x^2) ----
  # non-linear effect/relationship (e.g. what if the animal selects for intermediate elevations and avoids locations with extremely low or high elevations?), can use splines or polynomials to capture this non-linear effect. 
  
  cat(bgBlue("fit issf model 2","\n"))
  m2 <- fit_issf(case_ ~ 
                   sl_ + log_sl_ + cos_ta_ + 
                   elev_25m_end + dist_escape_25m_end + 
                   I(elev_25m_end^2) + 
                   strata(step_id_), 
                 data = ssf_dat, model = TRUE)
  summary(m2)
  
  #......................................................................
  ## model 3: interactions terms with categorical variables ----
  
  
  
  
  
  #......................................................................
  ## model 4: basic + quadratic term + interaction terms ----
  # interaction terms with movement characteristics and continuous explanatory variables capturing environmental characteristics
  # e.g. what if the animal moves differently based on habitat feature that is at the start of the movement step? 
  cat(bgBlue("fit issf model 4","\n"))
  m4 <- fit_issf(case_ ~ 
                   sl_ + log_sl_ + cos_ta_ + 
                   elev_25m_end + dist_escape_25m_end + 
                   I(elev_25m_end^2) +   
                   elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
                   dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + 
                   strata(step_id_), 
                 data = ssf_dat, model = TRUE)
  summary(m4)
  
  
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
  
  # store models
  ssf_dat_list[[i]] <- ssf_dat
  m1_list[[i]] <- m1
  m2_list[[i]] <- m2
  m4_list[[i]] <- m4
  # m5_list[[i]] <- m5
  # m6_list[[i]] <- m6
  
  
  
} # close loop



# END LOOP ----
#///////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////




#...........................................................
# 3. update distribution ----
#...........................................................










#...........................................................
# save outputs ----
#...........................................................


model_list_indiv <- c(m1_list,
                      m2_list,
                      m3_list,
                      m4_list,
                      m5_list,
                      m6_list)

save(model_list_indiv,  file = "./data/issa/model_list_individual_level.rda")







#////////////////////////////////////////////////////////////////
# POOLED DATA (all data) ----
#////////////////////////////////////////////////////////////////


# pooled data workflow approach uses data from all the animals i.e., not subsetted

# ISSA with mixed effects

# foundational framework is based on fieberg's how to issa at the individual level
# and expanding to include mixed effects for population level

# based on Chatterjee et al 2024 https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14321
# including Muff et al 2019 which they use the approach from Muff et al 2019 for the mixed effects for habitat selection but Chatterjee et al 2024 integrated movement into habitat selection and created the mixedssa package that updates distributions for movement parameters (i.e. step length (gamma) and turn angle (Von Mises))
# Note: the original Chatterjee et al 2024 workflow does not work, you must refer to the correction article https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.70050

# Kingdon et al 2025 combines the above into a more accessible language
# Note kingdon et al 2025 does not update distributions after the model has been fitted?

# amt breakdown https://cran.r-project.org/web/packages/amt/vignettes

# Resampling
# Question: I should resample my data to get regular time steps because issa assumes regular sampling intervals i.e. the step length/turn angles depend on the time intervals between fixes? right? because if i dont have regular time steps then it violates the issa model assumptions right? or I shouldnt resample the data because then i could be losing data?
# Noonan: I would recommend regularizing the data, yes. But you can do that in ctmm as well using predict() and the timestamps you want to predict for. That way you don’t lose any data. TBC

# Points to steps
# steps(): Takes as an input a track, converts the track to step and calculating some derived quantities (e.g., step lengths, turning angles). The function steps() expects a track with regular sampling rates.
# steps_by_burst(): Takes as an input a resampled track (i.e., a track with several bursts) and will calculate derived quantities per burst.

# inspect sampling rate to determine resampling rate
# summarize_sampling_rate(goat_track)
# min    q1 median   mean    q3      max      sd     n unit 
# <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>   <dbl> <int> <chr>
#   1     0     0      1 12049.    60 23265000 441844. 13793 sec  


# 1. Format and Generate Random Steps ----
# ssf_dat <- goat_track %>%
#   nest(data = -collar_id) %>% # separate data based on id (subsetting)
#   # nest(data = -collar_id_year) %>% # separate data based on id (subsetting)
#   mutate(data = map(data,  ~ .x %>%
#                       steps(keep_cols = 'start'))) %>%   # convert from points to steps (note: Chatterjee et al uses 'steps()')
#                       # steps())) %>%
#   unnest(cols = data) %>% # stitch it back together into one (unsubsetting)
#   random_steps() %>%
#   extract_covariates(elev_scaled, where = "both") %>%
#   #extract_covariates(dist_escape_scaled, where = "both"))) %>%
#   amt::filter(!is.na(ta_)) %>%   # drop NA values in turn angle column
mutate(cos_ta_ = cos(ta_), # transform ta_ and create a new column
       log_sl_ = log(sl_)) # transform sl_ and create a new column


# resample, convert points to steps, generate random steps, extract covariates
ssf_dat <- goat_track %>%
  #resample the track to get regular time steps based on summarize_sampling_rate
  track_resample(rate = minutes(375), tolerance = minutes(60)) %>% # to be continued!
  # exclude groups of consecutive steps (i.e., bursts) that have fewer than the min. number of points within sampling interval
  filter_min_n_burst(min_n = 3) %>% 
  # convert from points to steps (note: Chatterjee et al uses 'steps()')
  steps_by_burst(keep_cols = 'start') %>%
  # generate random available steps, number of control steps paired with each observed step (***ADD INFO*** not sure why, refer to issa guild, theres mention of it)
  random_steps() %>%
  #NOTE generating random steps produced this message:
  #Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length
  # tried adding fit_distr lines code below to see if it fixes the message but doesnt
  # sl_distr = fit_distr(steps_track$sl_, "gamma"), # fit a distribution to the data?
  # ta_distr = fit_distr(steps_track$ta_, "vonmises")) # fit a distribution to the data?
  
  # extract covariates for the random steps generated
  extract_covariates(elev_scaled, where = "both") %>%
  extract_covariates(dist_escape_scaled, where = "both") %>%
  # drop na values in the ta_ column
  amt::filter(!is.na(ta_)) %>% 
  mutate(cos_ta_ = cos(ta_), # transform ta_ and create a new column
         log_sl_ = log(sl_)) # transform sl_ and create a new column

# kingdon et al converted and added the column log_sl = log(sl_ + 1)) as log_sl, why added +1?

# # view/inspect df
# glimpse(ssf_dat)

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # check for duplicates in data
# which(diff(ssf_dat$sl_) == 0)
# which(ssf_dat$sl_ == 0)
# hist(ssf_dat$sl_)
# range(ssf_dat$sl_)
# # check the the rows that are duplicated
# ssf_dat[which(duplicated(ssf_dat[, "sl_"]) | duplicated(ssf_dat[, "sl_"], fromLast = TRUE)), ]
# # check all rows and all columns
# ssf_dat[which(duplicated(ssf_dat) | duplicated(ssf_dat, fromLast = TRUE)), ] 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check for NA, remove?
# test <- ssf_dat[!complete.cases(ssf_dat),] # not sure why 1 elev_end has NA values
# all(complete.cases(ssf_dat))



#..............................................................
## check original distributions (tentative, i.e. resampled raw data) ----
#................................................................

# refer to Chatterjee et al 2024 workflow to plot the tentative distribution



#..........................................................
# 2. Fit issf ----
#..........................................................

# create another column for step id but with the collar id with it
ssf_dat$step_id <- with(ssf_dat, paste(collar_id, step_id_, sep = "_"))
#check for collinearity, convergence issues
cor(ssf_dat[, c("sl_", "log_sl_", "cos_ta_", "elev_25m_end", "dist_escape_25m_end")], use = "complete.obs")
# corr matrix indicates the correlation between variables

# troubleshooting -> reduced the number of random effects, dropped having sl and log_sl together due to collinearity

# sl_ + log(sl_)+ cos(ta_)+ 
#   x1_end   +  
#   sl_:x1_start + 
#   log(sl_):x1_start + 
#   cos(ta_):x1_start +
#   (1|step_id)+ (0+ sl_ +(log(sl_)) + cos(ta_)|id) +(0+ x1_end|id), 

## model 7: basic + quadratic term + interaction terms + basic mixed effects ----
#NOTE IVE EDITED THE MODEL SO IT DOESNT MATCH/BUILD FROM THE MODELS ABOVE
model_config <- glmmTMB(case_ ~ 
                          sl_ + log_sl_ + cos_ta_ + 
                          elev_25m_end + 
                          sl_:elev_25m_start + 
                          log_sl_:elev_25m_start + 
                          cos_ta_:elev_25m_start +
                          I(elev_25m_end^2) +   
                          (log_sl_ + cos_ta_):elev_25m_start + # dropped the sl_ term here
                          # dist_escape_25m_start:(log_sl_ + cos_ta_) + # dropped the sl_ term here
                          #random effects
                          (1 | step_id) +
                          (0 + log_sl_+ cos_ta_ | collar_id) + 
                          (0 + elev_25m_end | collar_id),
                        # (0 + dist_escape_25m_end | collar_id), 
                        REML = TRUE, 
                        family=poisson(), 
                        data = ssf_dat,  
                        doFit=FALSE)

model_config$parameters$theta[1] <- log(1e3)
nvarparam <- length(model_config$parameters$theta)
model_config$mapArg <- list(theta=factor(c(NA, 1:(nvarparam-1))))

m7 <- glmmTMB:::fitTMB(model_config)


summary(m7)
# now update the distribution -> go to section 3

#......................................................................
## model 8: basic + quadratic term + interaction terms + basic mixed effects + random interscept and slope for ID and adding year as a fixed effect  (1 | collar_id) + distance to fire + fire/covariate interaction terms ----
model_config <- glmmTMB(case_ ~ 
                          sl_ + log_sl_ + cos_ta_ + 
                          elev_25m_end + dist_escape_25m_end + 
                          I(elev_25m_end^2) +   
                          elev_25m_start:(sl_ + log_sl_ + cos_ta_) +
                          dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + 
                          year + 
                          (1 | step_id) +
                          (0 + log_sl_ | collar_id) + 
                          (0 + log_sl_:cos_ta_ | collar_id) +
                          (0 + sl_ + log_sl_+ cos_ta_ | collar_id) + 
                          (0 + elev_25m_end | collar_id) +
                          (0 + dist_escape_25m_end | collar_id), 
                        REML = TRUE, 
                        family=poisson(), 
                        data = ssf_dat,  
                        doFit=FALSE)

model_config$parameters$theta[1] <- log(1e3)
nvarparam <- length(model_config$parameters$theta)
model_config$mapArg <- list(theta=factor(c(NA, 1:(nvarparam-1))))

m8 <- glmmTMB:::fitTMB(model_config)

#......................................................................
## model 9: basic + quadratic term + interaction terms +  basic mixed effects + random interscept and slope for ID and adding year as a fixed effect  (1 | collar_id)  + distance to fire +  ***+ fire mixed effects???*** ----

# dist_to_fire +   
#   elev_25m_end:dist_to_fire +
#   dist_escape_25m_end:dist_to_fire +

#......................................................................
## model 10: basic + quadratic term + interaction terms + basic mixed effects ----

#......................................................................
## model 11: basic + quadratic term + interaction terms + basic mixed effects + random interscept and slope for ID and adding year as a fixed effect  (1 | collar_id)  + distance to fire ----





#...........................................................
# 3. update distribution ----
#...........................................................

## update sl distribution ----

# marginal model or population average, no random effects (random effects set to 0), fixed-effects-only (what a typical/average individual looks like)
updated_sl_param_typical <- mixedSSA::update_dist(model = m7,
                                                  dist_name = "gamma", # name of the step-length distribution assumed
                                                  beta_sl =  "sl_", # the name of the step lengths coefficient in our model
                                                  beta_log_sl = "log_sl_", # the name of the log(sl) coefficient in our model
                                                  interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                                  quantiles = c(0.05, 0.5, 0.95)) # for continuous interaction variables
# check the distribution type
updated_sl_param_typical@distribution_name
# check the parameters i.e. step length, shape & scale values, values of the coefficients that were used to calculate the new parameter values
# in the grouping column -> the category (for categorical interaction variables) or quantile (for numerical interaction variables)
updated_sl_param_typical@updated_parameters
# check the step length (m) data (this is the movement data (the raw step lengths) that were used to fit the tentative distribution as per Chatterjee et al 2024 )
updated_sl_param_typical@movement_data


#...................................................................
## Plot updated sl distributions ----
#...................................................................

sl_param_typical <- updated_sl_param_typical@updated_parameters

plots_data <- list()
#create a df for plotting
for (i in 1:nrow(sl_param_typical)) {
  # x-axis is sequence of possible step lengths
  step_length <- seq(from = 0, to = 400, length.out = 100)
  # extract the data
  quantile <- sl_param_typical[i, "grouping"] # note: the column name is 'grouping' and not 'category' as indicated in the Chatterjee et al workflow and its for the quantile value, theres no such column as quantile
  # extract collar id
  collar_id <- sl_param_typical[i, "random_effect"] # there is no column called "id", I think they mean is the random effects column which is id
  # extract the shape and scale of the sl movement parameters
  # extract the shape and scale of the sl movement parameters
  shape <- sl_param_typical[i, "shape"]
  scale <- sl_param_typical[i, "scale"] # in chatterjee et al, they forced all values to be positive via the ab() function, i.e., absolute values
  
  # y-axis is the probability density under the given gamma distribution
  prob_density <- dgamma(x = step_length,
                         shape = sl_typical[i, "shape"],
                         # scale = abs(sl_typical[i, "scale"]) # making all values positive if they arent already
                         scale = sl_typical[i, "scale"])
  
  # create a df with all the extracted values
  plots_data[[i]] <- data.frame(step_length = step_length,
                                prob_density = prob_density,
                                quantile = quantile,
                                collar_id = collar_id,
                                shape = shape,
                                scale = scale)
  
}

plot_sl_typical <- do.call(rbind, plots_data)
plot_sl_typical$quantile <- as.factor(plot_sl_typical$quantile)
head(plot_sl_typical)


ggplot(plot_sl_typical, aes(x = step_length, y = prob_density, color = quantile)) +
  geom_line(size = 1) +
  # scale_color_manual(name = "Elevation",
  #                    breaks = c("low", "medium", "high"),
  #                    values = c("navyblue", "gray50", "firebrick")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines



#..................................................................
# now add the random effects -> conditional model that allows for individual variation/differences
# updated_sl_param_variant? updated_sl_param_conditional?
updated_sl_param_random_effect <- mixedSSA::update_dist(model = m7,
                                                        dist_name = "gamma", # name of the step-length distribution assumed
                                                        beta_sl =  "sl_", # the name of the step lengths coefficient in our model
                                                        beta_log_sl = "log_sl_", # the name of the log(sl) coefficient in our model
                                                        interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                                        random_effects_var_name = "collar_id", # for individual variation/differences
                                                        quantiles = c(0.05, 0.5, 0.95)) # for continuous interaction variables

# check the distribution type
updated_sl_param_random_effect@distribution_name
# check the parameters
updated_sl_param_random_effect@updated_parameters
updated_sl_param_random_effect@interaction_var # elevation
updated_sl_param_random_effect@grouping_type # quantile
updated_sl_param_random_effect@model

# extract the update parameters values from the updated distribution for population
sl_params_random <- updated_sl_param_random_effect@updated_parameters
head(sl_params_random)




# get plot data for the individual distributions
plot_data <- list()
#create a df for plotting
for (i in 1:nrow(sl_params_random)) {
  # x-axis is sequence of possible step lengths
  step_length <- seq(from = 0, to = 400, length.out = 100)
  # extract the data
  quantile <- sl_params_random[i, "grouping"] # note: the column name is 'grouping' and not 'category' as indicated in the Chatterjee et al workflow and its for the quantile value, theres no such column as quantile
  # extract collar id
  collar_id <- sl_params_random[i, "random_effect"] # there is no column called "id", I think they mean is the random effects column which is id
  # extract the shape and scale of the sl movement parameters
  shape <- sl_params_random[i, "shape"]
  scale <- sl_params_random[i, "scale"] # in chatterjee et al, they forced all values to be positive via the ab() function, i.e., absolute values
  
  # y-axis is the probability density under the given gamma distribution
  prob_density <- dgamma(x = step_length,
                         shape = sl_params_random[i, "shape"],
                         # scale = abs(sl_params_random[i, "scale"]) # making all values positive if they arent already
                         scale = sl_params_random[i, "scale"])
  
  # create a df with all the extracted values
  plot_data[[i]] <- data.frame(step_length = step_length,
                               prob_density = prob_density,
                               quantile = quantile,
                               collar_id = collar_id,
                               shape = shape,
                               scale = scale)
  
}

plot_sl_random <- do.call(rbind, plot_data)
plot_sl_random$quantile <- as.factor(plot_sl_random$quantile)
head(plot_sl_random)

unique(plot_sl_random$collar_id)
plot_sl_random[is.na(plot_sl_random$collar_id), ]
# drop na
plot_sl_random <- plot_sl_random[!is.na(plot_sl_random$collar_id), ]


# compare to the “typical” individual, where all random effects are set to 0
# look at how these individual parameters differ
sl_plots_data_combined <- rbind(plot_sl_random, plot_sl_typical)
head(sl_plots_data_combined)

# subset data based on quantile of interest
sl_plots_data_combined <- sl_plots_data_combined[sl_plots_data_combined$quantile == 0.05, ] # adjust the quantile accordingly to what youre interested in
#create a new column based on collar_id
sl_plots_data_combined$type <- ifelse(sl_plots_data_combined$collar_id == "typical", "typical", "individual")

# check for NA/Inf and drop them
any(is.na(sl_plots_data_combined))
sapply(sl_plots_data_combined, function(x) sum(is.na(x))) # checking across all columns
sapply(sl_plots_data_combined, function(x) sum(is.infinite(x))) # checking across all columns
#dropping
sl_plots_data <- subset(sl_plots_data_combined, !is.na(type))
sl_plots_data <- subset(sl_plots_data, !is.infinite(prob_density))

# sl_plots_data <- subset(sl_plots_data_combined, !is.na(prob_density) & !is.infinite(prob_density))
head(sl_plots_data)

p1 <- 
  ggplot(sl_plots_data, aes(x = step_length, y = prob_density, group = collar_id, color = type, alpha = type)) +
  geom_line(size = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  scale_color_discrete(name = "Type") +
  scale_alpha_manual(name = "Type", values = c("typical" = 1, "individual" = 0.7)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines

# the prob density values are really small, why???


#/////////////////////////////////////////////////////////////////----
## update ta distribution ----

# marginal model or population average, no random effects (random effects set to 0), fixed-effects-only (what a typical/average individual looks like)
updated_ta_param_typical <- mixedSSA::update_dist(model = m7,
                                                  dist_name = "vonmises", # name of the turn angle distribution assumed
                                                  beta_cos_ta  = "cos_ta_", # the name of the cos(ta) coefficient in our model
                                                  interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                                  quantiles = c(0.05, 0.5, 0.95), # for continuous interaction variables
                                                  tentative_dist = amt::fit_distr(ssf_dat$ta_[ssf_dat$case_ == "TRUE"], 
                                                                                  dist_name = "vonmises", na.rm = TRUE)) # this line seems like its required for ta distribution
#*****not sure why tentative_dist needs to be indicated for TA but not for SL******
# check the distribution type
updated_ta_param_typical@distribution_name
# check the parameters i.e. step length, shape & scale values, values of the coefficients that wre used to calculate the new parameter values
# in the grouping column -> the category (for categorical interaction variables) or quantile (for numerical interaction variables)
updated_ta_param_typical@updated_parameters



#...................................................................
## Plot updated ta distributions ----
#...................................................................

# extract parameters
ta_param_typical <- updated_ta_param_typical@updated_parameters
head(ta_param_typical)

plot_data <- list()
#create a df for plotting
for (i in 1:nrow(ta_param_typical)) {
  # x-axis is sequence of possible turn angles
  turn_angle <- seq(from = -pi, to = pi, length.out = 100)
  # extract the data
  quantile <- ta_param_typical[i, "grouping"] # note: the column name is 'grouping' and not 'category' as indicated in the Chatterjee et al workflow and its for the quantile value, theres no such column as quantile
  # extract collar id
  collar_id <- ta_params_random[i, "random_effect"] # there is no column called "id", I think they mean is the random effects column which is id
  # extract kappa and mu of the ta movement parameters
  kappa <- ta_param_typical[i, "kappa"]
  mu <- ta_param_typical[i, "mu"] # in chatterjee et al, they forced all values to be positive via the ab() function, i.e., absolute values
  # y-axis is the probability density under the given gamma distribution
  prob_density <- circular::dvonmises(x = turn_angle,
                                      # kappa = ta_param_typical[i, "kappa"],
                                      kappa = abs(ta_param_typical[i, "kappa"]), # making all values positive if they arent already, to fix the error
                                      # mu = circular(pi))
                                      mu = mu)
  # Error in circular::dvonmises(x = turn_angle, kappa = ta_param_typical[i,  : 
  #                                                                         the concentration parameter 'kappa' must be non negative
  # Warning messages:
  #   1: In as.circular(x) :
  #   an object is coerced to the class 'circular' using default value for the following components:
  #   type: 'angles'
  # units: 'radians'
  # template: 'none'
  # modulo: 'asis'
  # zero: 0
  # rotation: 'counter'
  # conversion.circularxradians0counter
  
  # create a df with all the extracted values
  plot_data[[i]] <- data.frame(turn_angle = turn_angle,
                               prob_density = prob_density,
                               quantile = quantile,
                               collar_id = collar_id,
                               kappa = kappa,
                               mu = mu)
  
}

plot_ta_typical <- do.call(rbind, plot_data)
plot_ta_typical$quantile <- as.factor(plot_ta_typical$quantile)
head(plot_ta_typical)


ggplot(plot_ta_typical, aes(x = turn_angle, y = prob_density, color = quantile)) +
  geom_line(size = 1) +
  # scale_color_manual(name = "Elevation",
  #                    breaks = c("low", "medium", "high"),
  #                    values = c("navyblue", "gray50", "firebrick")) +
  xlab("Turn Angle") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines



#..................................................................
# now add the random effects -> conditional model that allows for individual variation/differences
# updated_ta_param_variant? updated_ta_param_conditional?
updated_ta_param_random_effect <- mixedSSA::update_dist(model = m7,
                                                        dist_name = "vonmises", # name of the turn angle distribution assumed
                                                        beta_cos_ta  = "cos_ta_", # the name of the cos(ta) coefficient in our model
                                                        interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                                        random_effects_var_name = "collar_id", # for individual variation/differences
                                                        quantiles = c(0.05, 0.5, 0.95), # for continuous interaction variables
                                                        tentative_dist = amt::fit_distr(ssf_dat$ta_[ssf_dat$case_ == "TRUE"], 
                                                                                        dist_name = "vonmises", na.rm = TRUE)) # this line seems like its required for ta distribution

# inspect the slot components
slotNames(updated_ta_param_random_effect)
# check the distribution type
updated_ta_param_random_effect@distribution_name
# check the parameters
updated_ta_param_random_effect@updated_parameters
updated_ta_param_random_effect@interaction_var # elevation
updated_ta_param_random_effect@grouping_type # quantile

# extract the update parameters values from the updated distribution for population
ta_params_random <- updated_ta_param_random_effect@updated_parameters
head(ta_params_random)

# get plot data for the individual distributions
plot_data <- list()
#create a df for plotting
for (i in 1:nrow(ta_params_random)) {
  # x-axis is sequence of possible turn angles
  turn_angle <- seq(from = -pi, to = pi, length.out = 100)
  # extract the data
  quantile <- ta_params_random[i, "grouping"] # note: the column name is 'grouping' and not 'category' as indicated in the Chatterjee et al workflow and its for the quantile value, theres no such column as quantile
  # extract collar id
  collar_id <- ta_params_random[i, "random_effect"] # there is no column called "id", I think they mean is the random effects column which is id
  # extract kappa and mu of the ta movement parameters
  kappa <- ta_params_random[i, "kappa"]
  mu <- ta_params_random[i, "mu"] # in chatterjee et al, they forced all values to be positive via the ab() function, i.e., absolute values
  # y-axis is the probability density under the given gamma distribution
  prob_density <- circular::dvonmises(x = turn_angle,
                                      # kappa = ta_params_random[i, "kappa"],
                                      kappa = abs(ta_params_random[i, "kappa"]), # making all values positive if they arent already, to fix the error
                                      # mu = circular(pi))
                                      mu = mu)
  # Error in circular::dvonmises(x = turn_angle, kappa = ta_params_random[i,  : 
  #                                                                         the concentration parameter 'kappa' must be non negative
  # Warning messages:
  #   1: In as.circular(x) :
  #   an object is coerced to the class 'circular' using default value for the following components:
  #   type: 'angles'
  # units: 'radians'
  # template: 'none'
  # modulo: 'asis'
  # zero: 0
  # rotation: 'counter'
  # conversion.circularxradians0counter
  
  # create a df with all the extracted values
  plot_data[[i]] <- data.frame(turn_angle = turn_angle,
                               prob_density = prob_density,
                               quantile = quantile,
                               collar_id = collar_id,
                               kappa = kappa,
                               mu = mu)
  
}

plot_ta_random <- do.call(rbind, plot_data)
plot_ta_random$quantile <- as.factor(plot_ta_random$quantile)
head(plot_ta_random)

unique(plot_ta_random$collar_id)
#check for na
plot_ta_random[is.na(plot_ta_random$collar_id), ]
# drop na
plot_ta_random <- plot_ta_random[!is.na(plot_ta_random$collar_id), ]


# compare to the “typical” individual, where all random effects are set to 0
# look at how these individual parameters differ
ta_plots_data_combined <- rbind(plot_ta_random, plot_ta_typical)
head(ta_plots_data_combined)

# subset data based on quantile of interest
ta_plots_data_combined <- ta_plots_data_combined[ta_plots_data_combined$quantile == 0.05, ] # adjust the quantile accordingly to what youre interested in
#create a new column based on collar_id
ta_plots_data_combined$type <- ifelse(ta_plots_data_combined$collar_id == "typical", "typical", "individual")

# check for NA/Inf and drop them
any(is.na(ta_plots_data_combined))
sapply(ta_plots_data_combined, function(x) sum(is.na(x))) # checking across all columns
sapply(ta_plots_data_combined, function(x) sum(is.infinite(x))) # checking across all columns
#dropping
ta_plots_data <- subset(ta_plots_data_combined, !is.na(type))
head(ta_plots_data)

p2 <- 
  ggplot(ta_plots_data, aes(x = turn_angle, y = prob_density, group = collar_id, color = type, alpha = type)) +
  geom_line(size = 1) +
  xlab("Turn Angle") +
  ylab("Probability Density") +
  scale_color_discrete(name = "Type") +
  scale_alpha_manual(name = "Type", values = c("typical" = 1, "individual" = 0.7)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines




plot_elevation <- ggarrange(p1, p2,
                            ncol = 2,
                            common.legend = TRUE,
                            legend = "bottom")

# plot_elevation <- 
  annotate_figure(plot_elevation,
                                  top = text_grob(paste("Model 7 ISSA mixed effects")))
