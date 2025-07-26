



# ISSA with mixed effects at population level

# foundational framework is based on fieberg's how to issa at the individual level
# and expanding to include mixed effects for population level

# based on Chatterjee et al 2024 https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14321
# including Muff et al 2019 which they use the approach from Muff et al 2019 for the mixed effects for habitat selection but Chatterjee et al 2024 integrated movement into habitat selection and created the mixedssa package that updates distributions for movement parameters (i.e. step length (gamma) and turn angle (Von Mises))
# Note: the original Chatterjee et al 2024 workflow does not work, you must refer to the correction article https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.70050

# Kingdon et al 2025 combines the above into a more accessible language
# Note kingdon et al 2025 does not update distributions after the model has been fitted?


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

# clean up environment
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



# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# test2 <- diff(goat_track$longitude)
# range(test2)
# which(test2 == 0)
# which(diff(goat_track$latitude) == 0)
# which(diff(goat_track$x_) == 0)
# 
# #~~~~~~~~~~~~~~~~~~~~~~



# # Visualization of the tracks:
# ggplot() + 
#   geom_spatraster(data = elev_scaled, show.legend = FALSE)  +
#   geom_path(data = goat_track, aes(x_, y_),  col= "black") +
#   facet_wrap(~ collar_id, nrow = 2) + 
#   scale_fill_terrain_c() +
#   theme_bw()




#////////////////////////////////////////////////////////////////
# population (all data) ----
#////////////////////////////////////////////////////////////////


# population workflow approach uses data from all the animals i.e., not subsetted

# ISSA with mixed effects model

# foundational framework is based on fieberg's how to issa at the individual level
# and expanding to include mixed effects
# based on Chatterjee et al 2024 https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14321
# including Muff et al 2019 which they use the approach from Muff et al 2019 for the mixed effects for habitat selection but Chatterjee et al 2024 integrated movement into habitat selection and created the mixedssa package that updates distributions for movement parameters (i.e. step length (gamma) and turn angle (Von Mises))
# Note: the original Chatterjee et al 2024 article workflow does not work, you must refer to the correction article https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.70050
# Note: could not get certain mixedssa r package functions to work, workflow is based on their work but taken their functions apart to make it more flexible and customization for other data and easier to troubleshoot
# includes Kingdon et al 2025 which combines the above into a more accessible language and uses base R instead of tidyverse which amt is based on :(

# amt breakdown https://cran.r-project.org/web/packages/amt/vignettes

# Resampling
# Question: I should resample my data to get regular time steps because issa assumes regular sampling intervals i.e. the step length/turn angles depend on the time intervals between fixes? right? because if i dont have regular time steps then it violates the issa model assumptions right? or I shouldnt resample the data because then i could be losing data?
# Noonan: I would recommend regularizing the data, yes. But you can do that in ctmm as well using predict() and the timestamps you want to predict for. That way you don’t lose any data. ****TBC*****

# Points to steps
# steps(): Takes as an input a track, converts the track to step and calculating some derived quantities (e.g., step lengths, turning angles). The function steps() expects a track with regular sampling rates.
# steps_by_burst(): Takes as an input a resampled track (i.e., a track with several bursts) and will calculate derived quantities per burst.

# inspect sampling rate to determine resampling rate
# summarize_sampling_rate(goat_track)
# min    q1 median   mean    q3      max      sd     n unit 
# <dbl> <dbl>  <dbl>  <dbl> <dbl>    <dbl>   <dbl> <int> <chr>
#   1     0     0      1 12049.    60 23265000 441844. 13793 sec  




#...........................................................
# 1. Format and Generate Random Steps ----
#...........................................................

# resample, convert points to steps, generate random steps, extract covariates
ssf_dat <- goat_track %>%
  #resample the track to get regular time steps based on summarize_sampling_rate
  track_resample(rate = minutes(375), tolerance = minutes(80)) %>% # to be continued!
  # exclude groups of consecutive steps (i.e., bursts) that have fewer than the min. number of points within sampling interval
  filter_min_n_burst(min_n = 3) %>% 
  # convert from points to steps (note: Chatterjee et al uses 'steps()')
  steps_by_burst(keep_cols = 'start') %>%
  # generate random available steps, number of control steps paired with each observed step (***ADD INFO*** not sure why, refer to issa guild, theres mention of it)
  random_steps() %>%
  # NOTE generating random steps produced this message:
  # Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length *************WHY???***************
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

# kingdon et al converted and added the column log_sl = log(sl_ + 1)) as log_sl, why added +1?************

# view/inspect df
glimpse(ssf_dat) #like str(x)

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

# create another column for step id but with the collar id with it (REVIEW FOR REASONING)
ssf_dat$step_id <- with(ssf_dat, paste(collar_id, step_id_, sep = "_"))
#check for collinearity, convergence issues
cor(ssf_dat[, c("sl_", "log_sl_", "cos_ta_", "elev_25m_end", "dist_escape_25m_end")], use = "complete.obs")
# corr matrix indicates the correlation between variables

# troubleshooting problems with model -> reduced the number of random effects, dropped having sl and log_sl together due to collinearity

## model 7: basic + quadratic term + interaction terms + basic mixed effects ----
#REMINDER ***** IVE EDITED THE MODEL SO IT DOESNT MATCH/BUILD ON/FROM THE MODELS ABOVE ******

# fit mixed model via glmmTMB based on Muff et al 2019, code repository at https://conservancy.umn.edu/handle/11299/204737
# based on the workflow, assuming 'x1' is referring to rasters/habitat/covariates, therefore, adjusted accordingly,
# info is provided near the end of the workflow (update distribution) indicting that x1_start is the name of the habitat variable layer

# set up the model without fitting the model, as per muff et al 2019
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
# # troubleshooting attempts:
# # if grouped by collar_id produces NA values conditional model & warnings: Model convergence problem; singular convergence (7). See vignette('troubleshooting'), help('diagnose')
# # if using fixed year and grouped by collar_id causes same issues
# # if grouped by collar_id and steps as collar_id_year + step_id_ -> produces NA values in conditional model summary & warning  Model convergence problem; singular convergence (7)
# # if grouped by collar_id + year, causes problems when fitting the model, produces NA values in conditional model summary 
# # if grouped by collar_id_year and step_id as collar_id + step_id_ -> produces na values in  AIC, BIC, logLik, -2*log(L) only but the rest are populated
# # and warnings of Model convergence problem; non-positive-definite Hessian matrix.
# # if grouped by collar_id_year and step_id as collar_id_year + step_id_ -> produces na values in  AIC, BIC, logLik, -2*log(L) only but the rest are populated
# # and warnings of 1 Model convergence problem; non-positive-definite Hessian matrix. See vignette('troubleshooting')
# # and 2 Model convergence problem; false convergence (8). See vignette('troubleshooting'), help('diagnose')




## we have to manually fix the variance of the intercept first and then fix the 
## standard deviation of the first random term, which is the (1|step_id) component 
## in the above model equation that is fixed as 1e3 here. 
# this section can be done within the model config similar to kongdon et al approach as a 1-step workflow but not as flexible in terms of configuration
# doing 2-step workflow as per muff et al and Chatterjee et al also allows for more complex configuration
model_config$parameters$theta[1] <- log(1e3)
## We need to tell glmmTMB not to change the variance by setting it to NA:
nvarparam <- length(model_config$parameters$theta)
model_config$mapArg <- list(theta=factor(c(NA, 1:(nvarparam-1)))) # fixes the first variance parameter i.e., the first theta is fixed and wont be estimated, the rest can be freely estimated
# the first variance is fixed in glmmtmb because if we were using clogit() then we would have strata(step_id) in the model (like muff et al), however theres no strata() in glmmtmb so we included the random intercept (1 | step_id), one per stratum, fixing the variance is mimicking the strata(step_id) to be able to use glmmtmb, i.e. mirrors the strata(step_id) behaviour in the clogit() function
# in summary, glmmtmb = need to model stratum-specific intercepts via random effects instead because not using strata() to get the correct ssf behaviour (must prevent strinkage via fixing variance)
# for more info, refer to muff et al paper

# # from correction article workflow
# nthetas <- length(model_config$parameters$theta)
# model_config$mapArg <- list(theta = factor(c(1:nthetas))) # did not set it to NA
# this approach is incorrect for ssf (for our approach) because youre allowing shrinkage to occur and breaks the within-stratum matching logic
# stratum/strata = grouping unit and in our case each step is a stratum, each used (case) step you generate available (control) steps
# strinkage = group-specific estimates that are pulled towards the population average instead of/rather than being estimated independently

# these two will produce different outcomes in the model summary because one was set to na and one was not


## Then fit the model and look at the results:
m7 <- glmmTMB:::fitTMB(model_config)

summary(m7)
# now update the distribution! -> go to section 3

#......................................................................
## model 8: basic + quadratic term + interaction terms + basic mixed effects + random interscept and slope for ID and adding year as a fixed effect  (1 | collar_id) + distance to fire + fire/covariate interaction terms ----

# model_config <- glmmTMB(case_ ~ 
#                           sl_ + log_sl_ + cos_ta_ + 
#                           elev_25m_end + dist_escape_25m_end + 
#                           I(elev_25m_end^2) +   
#                           elev_25m_start:(sl_ + log_sl_ + cos_ta_) +
#                           dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + 
#                           year + 
#                           (1 | step_id) +
#                           (0 + log_sl_ | collar_id) + 
#                           (0 + log_sl_:cos_ta_ | collar_id) +
#                           (0 + sl_ + log_sl_+ cos_ta_ | collar_id) + 
#                           (0 + elev_25m_end | collar_id) +
#                           (0 + dist_escape_25m_end | collar_id), 
#                         REML = TRUE, 
#                         family=poisson(), 
#                         data = ssf_dat,  
#                         doFit=FALSE)
# 
# model_config$parameters$theta[1] <- log(1e3)
# nvarparam <- length(model_config$parameters$theta)
# model_config$mapArg <- list(theta=factor(c(NA, 1:(nvarparam-1))))
# 
# m8 <- glmmTMB:::fitTMB(model_config)
# # now update the distribution -> go to section 3

#......................................................................
## model 9: basic + quadratic term + interaction terms +  basic mixed effects + random interscept and slope for ID and adding year as a fixed effect  (1 | collar_id)  + distance to fire +  ***+ fire mixed effects???*** ----

# dist_to_fire +   
#   elev_25m_end:dist_to_fire +
#   dist_escape_25m_end:dist_to_fire +


# TO ADD


#......................................................................
## model 10: basic + quadratic term + interaction terms + basic mixed effects ----

# TO ADD


#......................................................................
## model 11: basic + quadratic term + interaction terms + basic mixed effects + random interscept and slope for ID and adding year as a fixed effect  (1 | collar_id)  + distance to fire ----


# TO ADD





# END OF MODELS




#...........................................................
# 3. update distribution & plot ----
#...........................................................



### sl distribution typical ----

# After fitting the mixed-effect model using the glmmTMB package, we get the model output into the mixedSSA package for updating the movement parameters. First, we update the parameters in the step-length distribution and assume they follow a gamma distribution.

# marginal model or population average, no random effects (random effects set to 0), fixed-effects-only (what a typical/average individual looks like)
updated_sl_param_typical <- mixedSSA::update_dist(model = m7,
                                                  dist_name = "gamma", # name of the step-length distribution assumed
                                                  beta_sl =  "sl_", # the name of the step lengths coefficient in our model
                                                  beta_log_sl = "log_sl_", # the name of the log(sl) coefficient in our model
                                                  interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                                  quantiles = c(0.05, 0.5, 0.95)) # for continuous interaction variables, set to false if categorical?
# check the distribution type
updated_sl_param_typical@distribution_name
# check the parameters i.e. step length, shape & scale values, values of the coefficients that were used to calculate the new parameter values
# in the grouping column -> the category (for categorical interaction variables) or quantile (for numerical interaction variables)
updated_sl_param_typical@updated_parameters # there wont be any random effects therefore it will show as NA
# check the step length (m) data (this is the movement data (the raw step lengths) that were used to fit the tentative distribution as per Chatterjee et al 2024 )
updated_sl_param_typical@movement_data

# extract values
sl_param_typical <- updated_sl_param_typical@updated_parameters


# this part was adopted from the get_sl_plot_data() function from mixedssa a function for getting all the step length plot lines from the updated_parameters tibble, the get_sl_plot_data() gave a lot of issues, so it was deconstructed and based off of fieberg and Chatterjee & manually plotted instead of using the plot_updated_dist() as that also gave issues 
plot_data <- list()
#create a df for plotting
for (i in 1:nrow(sl_param_typical)) {
  # x-axis is sequence of possible step lengths
  step_length <- seq(from = 0, to = 400, length.out = 100)
  # extract the data
  quantile <- sl_param_typical[i, "grouping"] # note: the column name is 'grouping' and not 'category' as indicated in the Chatterjee et al workflow and its for the quantile value, theres no such column as quantile
  # extract collar id
  collar_id <- sl_param_typical[i, "random_effect"] # there is no column called "id", I think they mean is the random effects column which is id
  # extract the shape and scale of the sl movement parameters
  shape <- sl_param_typical[i, "shape"]
  scale <- sl_param_typical[i, "scale"] # in chatterjee et al, they forced all values to be positive via the ab() function, i.e., absolute values
  
  # y-axis is the probability density under the given gamma distribution
  prob_density <- dgamma(x = step_length,
                         shape = sl_param_typical[i, "shape"],
                         # scale = abs(sl_param_typical[i, "scale"]) # making all values positive if they arent already
                         scale = sl_param_typical[i, "scale"])
  
  # create a df with all the extracted values
  plot_data[[i]] <- data.frame(step_length = step_length,
                               prob_density = prob_density,
                               quantile = quantile,
                               collar_id = collar_id,
                               shape = shape,
                               scale = scale)
  
}

plot_sl_typical <- do.call(rbind, plot_data)
plot_sl_typical$quantile <- as.factor(plot_sl_typical$quantile)
head(plot_sl_typical)
# clean up environment
rm(plot_data)

p1_typical <-
  ggplot(plot_sl_typical, aes(x = step_length, y = prob_density, color = quantile)) +
  geom_line(linewidth = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines



#..................................................................
### sl distribution random ----

# now add the random effects -> conditional model that allows for individual variation/differences
# updated_sl_param_variant? updated_sl_param_conditional?
updated_sl_param_random_effect <- mixedSSA::update_dist(model = m7,
                                                        dist_name = "gamma", # name of the step-length distribution assumed
                                                        beta_sl =  "sl_", # the name of the step lengths coefficient in our model
                                                        beta_log_sl = "log_sl_", # the name of the log(sl) coefficient in our model
                                                        interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                                        random_effects_var_name = "collar_id", # for individual variation/differences
                                                        quantiles = c(0.05, 0.5, 0.95)) # for continuous interaction variables
slotNames(updated_sl_param_random_effect)
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

rm(plot_data)

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

p1_random <- 
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
#/////////////////////////////////////////////////////////////////

## ta distribution typical ----
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
  collar_id <- ta_param_typical[i, "random_effect"] # there is no column called "id", I think they mean is the random effects column which is id
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


p2_typical <- 
  ggplot(plot_ta_typical, aes(x = turn_angle, y = prob_density, color = quantile)) +
  geom_line(size = 1) +
  xlab("Turn Angle") +
  ylab("Probability Density") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines



#..................................................................
## ta distribution random effects ----
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

p2_random <- 
  ggplot(ta_plots_data, aes(x = turn_angle, y = prob_density, group = collar_id, color = type, alpha = type)) +
  geom_line(size = 1) +
  xlab("Turn Angle") +
  ylab("Probability Density") +
  scale_color_discrete(name = "Type") +
  scale_alpha_manual(name = "Type", values = c("typical" = 1, "individual" = 0.7)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank()) #removes vertical gridlines




#......................................................................
# Multi-panel plot ----

# plot population average/typical individual
plot_elevation_typical <- ggarrange(p1_typical, p2_typical,
                                    ncol = 2,
                                    common.legend = TRUE,
                                    legend = "bottom")

plot_elevation_typical <-
  annotate_figure(plot_elevation_typical,
                  top = text_grob(paste("Model 7 ISSA elevation - population average/typical individual" )))
plot_elevation_typical


# plot random effects that allow for individual variation
plot_elevation_random <- ggarrange(p1_random, p2_random,
                                   ncol = 2,
                                   common.legend = TRUE,
                                   legend = "bottom")

plot_elevation_random <-
  annotate_figure(plot_elevation_random,
                  top = text_grob(paste("Model 7 ISSA elevation - random effect for individual")))
plot_elevation_random





ggsave(plot_elevation_typical,
       file = "./figures/issa/issa_elevation_typical.png",
       width = 8, height = 6, units = "in", dpi = 600, bg = "transparent")

ggsave(plot_elevation_random,
       file = "./figures/issa/issa_elevation_random.png",
       width = 8, height = 6, units = "in", dpi = 600, bg = "transparent")


#save the model
saveRDS(m7, "./issa/model/m7_population.rds")


#................................................................
# 5. extract outputs/results ----
# get CI values
# calculate rss between 2 locations?
# calculate rss for movement
#................................................................

# via fieberg, chatterjee, kongdon, or combination








#................................................................
# Delta method for SE calculation of mean speed ----
#................................................................

# from corrections article
# This will follow the same process as before, but we will calculate the mean speed (and its SE) for different quantiles of distance from reefedge. The function of the model parameters that we want to estimate (i.e., ~ (tentative_shape + x3 + x8 * distanceq) * (1 / (1 / tentative_scale - (x2 + x7 * distanceq)))) in the delta method calculation uses equations 17 and 18 in the updated manuscript version, which were originally mis-specified in Appendix 1.


### Estimated coefficients and their var/cov matrix
contmean <- summary(m_mixedssa)$coefficients$cond[, "Estimate"]
contvar <- vcov(m_mixedssa)$cond

### tentative distribution parameters
cont_sl_dist <- updated_sl_parameters@updated_parameters
tentative_shape <- cont_sl_dist[1, "shape"]
tentative_scale <- cont_sl_dist[1, "scale"]

# filter out "tentative", we won't calculate a SE for the tentative parameters
cont_sl_dist <- cont_sl_dist |>
  dplyr::filter(grouping != "tentative") |>
  # get the actual values of quantiles for delta method
  mutate(dist_quant = quantile(ssf_dat$elev_start, # not sure if its supposed to be start or end as the corrected workflow/paper doesnt say, it just has it as a distance to whatever not based on start or end of the step
                               na.rm = T,
                               probs = as.numeric(grouping)))


for (i in 1:nrow(cont_sl_dist)) {
  distanceq <- cont_sl_dist$dist_quant[i] # pull of the distance value
  cont_sl_dist$se[i] <- sqrt(msm::deltamethod( ~ (tentative_shape + x3 + x8 * distanceq) * (1 / (1 / tentative_scale - (x2 + x7 * distanceq))),
                                               contmean, contvar))
}


# check output
cont_sl_dist

plot_data <- cont_sl_dist |>
  mutate(mean = shape * scale,
         ci_lwr = mean - 1.96 * se,
         ci_upr = mean + 1.96 * se)

### Check results for mean speed
plot_data


### plotting the results
plot_delta_sd <-
  ggplot(plot_data) +
  geom_point(aes(x = dist_quant, y = mean), size = 2.5, col = "blue") +
  geom_linerange(aes(x = dist_quant, ymin = ci_lwr, ymax = ci_upr), linewidth = 1) +
  labs(x = "Elevation", y = "Mean step length") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10))


paper_figure <- 
  cowplot::plot_grid(
    plot_sl_cont, plot_delta_sd, plot_ta_cont,
    ncol = 3, labels = c("(a)", "(b)", "(c)"),
    hjust = -0.1)





sessionInfo()







## kongdon et al ----

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>from kongdon et al

model <- m7


# Population mean coefficient from mixed model
pop.model <- as.data.frame(summary(model)$coef$cond[-1, "Estimate"])
pop.model$term <- rownames(pop.model)
pop.model <- data.table::as.data.table(pop.model)
colnames(pop.model) <- c("estimate", "term")

# Variance of coefficients from mixed model
se.model <- as.data.frame(summary(model)$coef$cond[-1, "Std. Error"])
se.model$term <- rownames(se.model)
se.model <- data.table::as.data.table(se.model)
colnames(se.model) <- c("std.error", "term")

model.comb <- merge(pop.model, se.model, by = "term")

indivs.model <- coef(model)$cond$id_year[ , -1] %>% 
  tibble::rownames_to_column("id_year") %>% 
  pivot_longer(-id_year, names_to = "term", values_to = "estimate") %>% 
  mutate(method = "ME")

ran_vals.model <-broom.mixed::tidy(model, effect= 'ran_vals') #### in reference to the population estimate
indivs.se.model <- data.table::setDT(ran_vals.model)[group=='id_year', .(id_year =level, term ,se=std.error)]
indivs_model <- merge(data.table::setDT(indivs.model), indivs.se.model, by = c('id_year', 'term'))

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


