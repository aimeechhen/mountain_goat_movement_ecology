



# mixedssa



library(lubridate)
library(terra)
library(raster)
library(tidyverse)
library(tidyterra)
library(conflicted)
library(ggplot2)
# install.packages('TMB', type = 'source')
library(glmmTMB)
# remotes::install_github("jmsigner/amt")
library(amt) # dev version needed: remotes::install_github("jmsigner/amt")
library(cowplot)
library(MASS)
# devtools::install_github("smthfrmn/mixedSSA", build_vignettes = TRUE, force = TRUE)
library(mixedSSA)
library(sf)
library(tictoc)





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

# load spatial covariate data (packedspatraster or spatraster object), reproject, and scale/center goat-habitat covariate raster data via terra pkg
elev_25m <- rast('data/rasters/elev_25m.tif')
elev_25m <- project(elev_25m, "epsg:5070")
elev_25m <- scale(elev_25m)

dist_escape_25m <- rast('data/rasters/dist_escape_25m.tif')
dist_escape_25m <- project(dist_escape_25m,"epsg:5070")
dist_escape_25m <- scale(dist_escape_25m)



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

#~~~~~~~~~~~~~~~~~~~~~~
# check for duplicates
test2 <- diff(goat_track$longitude)
range(test2)
which(test2 == 0)
which(diff(goat_track$latitude) == 0)
which(diff(goat_track$x_) == 0)
duptest <- goat_track[duplicated(goat_track$longitude) & duplicated(goat_track$latitude), ] 
#~~~~~~~~~~~~~~~~~~~~~~

# create a parameter value df (movement parameter outputs)
param_value <- data.frame(individual.local.identifier = rep(NA, length(goats)),
                          shape_tentative = rep(NA, length(goats)),
                          scale_tentative = rep(NA, length(goats)),
                          kappa_tentative = rep(NA, length(goats)))


# Visualization of the tracks:
ggplot() + 
  geom_spatraster(data = elev_25m, show.legend = FALSE)  +
  geom_path(data= goat_track, aes(x_, y_),  col= "black") +
  facet_wrap(~ collar_id, nrow =2) + 
  scale_fill_terrain_c() +
  theme_bw()



#////////////////////////////////////////////////////////////////
# individual-level ----
#////////////////////////////////////////////////////////////////

# based on the framework/how to guide Appendix B for Integrated step selection analysis (ISSA) https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441 
# additional annotations from https://issa-guild.github.io/book/index.html


# Christina Prokopenko: a quick follow up - Tal mentioned 
# 1. not to scale log_sl, 
# 2. not to scale and centre within a model, instead do it manually and save your mean, standard error etc in a file so you know what was done. Finally 
# 3. doing an interaction between two scaled and centred variables can make things go a bit wonky, so make sure not to do that.




for (i in seq_along(goats)) {
  # indicate the individual
  current_goat <- goats[i]
  # subset individual data
  subset_goat <- goat_track[goat_track$individual.local.identifier == current_goat,]
  ## 1. Format and Generate Random Steps ----
  ssf_dat_indiv <- subset_goat %>% 
    steps(keep_cols = 'both') %>%
    random_steps() %>%
    extract_covariates(elev_25m, where = "both") %>%
    extract_covariates(dist_escape_25m , where = "both") 
  # scaled raster at the start, not rescaling and centering covariates because then thats double scaling?????
  # %>% 
  #   mutate(elev_25m_start = scale(elev_25m_start), # scale and center our covariates and add the additional movement covariates
  #          elev_25m_end = scale(elev_25m_end),
  #          dist_escape_25m_start = scale(dist_escape_25m_start),      # Scale distance to escape at start
  #          dist_escape_25m_end = scale(dist_escape_25m_end))
  
  # Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length, which is: 3.1248381079509
  # this occurred with all individuals
  
  glimpse(ssf_dat_indiv)
  
  #..............................................................
  ### inspect distributions (tentative, i.e. resampled raw data) ----
  #................................................................
  
  #as per issa guild zulip group, tal:
  # we do not know what the 'true selection-free' step-length and turn-angle distributions are - all we have are these tentative distributions that we will later adjsut based on the iSSA results.
  # the true distributions are what you get after adjusting the 'tentative' distributions using the appropriate iSSF coefficients. For example, if you have sampled step lengths from a tentative gamma distribution with shape a and scale b, but only included log_sl as a covariate in your iSSA, you have assumed that the tentative scale parameter (a) is the true scale parameter (otherwise you would need to include sl_ as a covariate) , but you allowed the shape parameter to be adjusted (b + the iSSF coefficient for log_sl)
  
  # Christina Prokopenko: The only time you would want to update and report them [the distributions] is if you want to say something about movement results specifically - i.e. the average step length was x and turn angle was y sort of thing. It is also a good check to make sure your models aren't predicting negative speeds.
  
  sl_distr_tentative <- sl_distr(ssf_dat_indiv)
  ta_distr_tentative <- ta_distr(ssf_dat_indiv)
  
  # update param_value df
  param_value$individual.local.identifier[i] <- current_goat
  param_value$shape_tentative[i] <- sl_distr_tentative$params$shape
  param_value$scale_tentative[i] <- sl_distr_tentative$params$scale
  param_value$kappa_tentative[i] <- ta_distr_tentative$params$kappa
  
  
  #..........................................................
  ## 2. fit issa ----
  
  m5 <- ssf_dat_indiv %>% 
    fit_issf(case_ ~ elev_25m_end +
               sl_ + log(sl_) + cos(ta_) + 
               sl_:elev_25m_start + 
               log(sl_):elev_25m_start + 
               cos(ta_):elev_25m_start+
               strata(step_id_))
  
  #..........................................................
  ## 5. update distribution ----
  
  sl_fit <- amt::fit_distr(ssf_dat_indiv$sl_[ssf_dat_indiv$case_=="TRUE"], "gamma", na.rm = T)
  ta_fit <- amt::fit_distr(ssf_dat_indiv$ta_[ssf_dat_indiv$case_=="TRUE"], "vonmises", na.rm = T)
  
  #..........................................................
  ## extract model coeffs and update param df ----
  
  # get model coeffs
  # m5_coeff <- data.frame(term = names(m5$model$coefficients),
  #                        m5_coeff = m5$model$coefficients)
  
  # extract model coeff values from summary table
  model_coeff <- broom::tidy(m5$model)
  # add model summary output to param df
  param_value[i, 5:6] <- as.data.frame(update_gamma(dist= sl_fit, 
                                                    beta_sl = model_coeff$estimate[model_coeff$term == "sl_"] ,
                                                    beta_log_sl = model_coeff$estimate[model_coeff$term == "log(sl_)"])$params)[1,]
  
  
  param_value[i, 7:8] <- as.data.frame(update_vonmises(dist= ta_fit, 
                                                       beta_cos_ta = model_coeff$estimate[model_coeff$term == "cos(ta_)"])$params)[1,]
  
  
  
} # close loop

#update column names
names(param_value)[5:8] <-c("shape_indv", "scale_indv", "kappa_indv", "mu_indv") 









#////////////////////////////////////////////////////////////////
# population-level ----
#////////////////////////////////////////////////////////////////

## 1. Format and Generate Random Steps ----
cat("mixed ssa population level")

ssf_dat <- goat_track %>% 
  nest(data = -individual.local.identifier) %>% # separate data based on id (subsetting)
  mutate(data = map(data,  ~ .x %>% 
                      # steps(keep_cols = 'both'))) %>%
                      steps())) %>%
  unnest(cols = data) %>% # stitch it back together into one (unsubsetting)
  random_steps() %>%
  extract_covariates(elev_25m, where = "both") %>% 
  #extract_covariates(dist_escape_25m , where = "both"))) %>% 
  amt::filter(!is.na(ta_))   # drop NA values in turn angle column

# message displayed after formating and generating steps
# Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length, which is: 0.596785143982828


# create another column for step id but with the collar id with it (like individual.local.identifier for ctmm/movebank but instead of it being for the animal id, its for the step)
ssf_dat$step_id <- with(ssf_dat, paste(individual.local.identifier, step_id_, sep = "_"))
# it will look like collarid_year_stepid



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check for duplicates in data
which(diff(ssf_dat$sl_) == 0)
which(ssf_dat$sl_ == 0)
hist(ssf_dat$sl_)
range(ssf_dat$sl_)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # create a column for log_sl_ or you'll run into issues later on
# ssf_dat$log_sl_ <- log(ssf_dat$sl_)


# view/inspect df
glimpse(ssf_dat)

# check and remove NA
ssf_dat[!complete.cases(ssf_dat),]


#..........................................................
## 2. fit issa ----

# fit mixed model via glmmTMB based on Muff et al 2020, code repository at https://conservancy.umn.edu/handle/11299/204737
# based on the workflow, assuming 'x1' is referring to rasters/habitat/covariates, therefore, adjusted accordingly,
# info is provided near the end of the workflow (update distribution) indicting that x1_start is the name of the habitat variable layer
# as per muff et al 2019, set up the model without fitting the model
model_config <- glmmTMB(case_ ~ 
                          sl_ + log(sl_)+
                          # sl_ + log_sl_+ 
                     cos(ta_)+ elev_25m_end   +  
                     sl_:elev_25m_start + 
                     log(sl_):elev_25m_start +
                       # log_sl_:elev_25m_start +
                     cos(ta_):elev_25m_start +
                     (1 | step_id) + # random intercept | grouped by step_id
                     (0 + sl_ +(log(sl_)) + cos(ta_) | individual.local.identifier) + # random slopes sl_ +(log(sl_)) + cos(ta_) | grouped by individual.local.identifier
                       # (0 + sl_ +(log_sl_) + cos(ta_) | individual.local.identifier) + 
                     (0 + elev_25m_end | individual.local.identifier), # random slope elev_25m_end | grouped by individual.local.identifier
                   REML = TRUE, 
                   family=poisson(), 
                   data = ssf_dat,  
                   doFit=FALSE)



## we have to manually fix the variance of the intercept first and then fix the 
## standard deviation of the first random term, which is the (1|step_id) component 
## in the above model equation that is fixed as 1e3 here.
model_config$parameters$theta[1] <- log(1e3)

## We need to tell glmmTMB not to change the variance by setting it to NA:
# nvarparm7 <- length(model_config$parameters$theta)
# model_config$mapArg <- list(theta=factor(c(NA, 1:(nvarparm7-1))))

# from corrected workflow
nthetas <- length(model_config$parameters$theta)
model_config$mapArg <- list(theta = factor(c(1:nthetas))) # did not set it to NA


## Then fit the model and look at the results:
tic()
m_mixedssa <- glmmTMB:::fitTMB(model_config)
toc() # 2.51min
beepr::beep(4)
# Warning message:
#   In finalizeTMB(TMBStruc, obj, fit, h, data.tmb.old) :
#   Model convergence problem; singular convergence (7). See vignette('troubleshooting'), help('diagnose')

# # message did not show up when started with a fresh r session
# # Warning message: 
# #   In (function (start, objective, gradient = NULL, hessian = NULL,  :
# #                   NA/NaN function evaluation
# diagnose(m_mixedssa)
# # Unusually large Z-statistics (|x|>5):
# #   
# #   theta_0+sl_+(log_sl_)+cos(ta_)|individual.local.identifier.1 theta_0+sl_+(log_sl_)+cos(ta_)|individual.local.identifier.2 theta_0+sl_+(log_sl_)+cos(ta_)|individual.local.identifier.3 
# # -54.79969                                                    -21.45939                                                    -11.59663 
# # 
# # Large Z-statistics (estimate/std err) suggest a *possible* failure of the Wald approximation - often also associated with parameters that are at or near the edge of
# # their range (e.g. random-effects standard deviations approaching 0).  (Alternately, they may simply represent very well-estimated parameters; intercepts of
# #                                                                        non-centered models may fall in this category.) While the Wald p-values and standard errors listed in summary() may be unreliable, profile confidence intervals (see
# #                                                                                                                                                                                                                                         ?confint.glmmTMB) and likelihood ratio test p-values derived by comparing models (e.g. ?drop1) are probably still OK.  (Note that the LRT is conservative when the
# #                                                                                                                                                                                                                                                                                                                                                                 null value is on the boundary, e.g. a variance or zero-inflation value of 0 (Self and Liang 1987; Stram and Lee 1994; Goldman and Whelan 2000); in simple cases these
# #                                                                                                                                                                                                                                                                                                                                                                 p-values are approximately twice as large as they should be.)
# 
# # if there are convergence issues, may be due to a scaling + centering factor -issa guild zulip group

summary(m_mixedssa)

#....................................................................
## 5. Update distributions ----
#....................................................................

# We will need to pass the tentative sl distribution (from corrected workflow)
tentative_sl_dist <- amt::fit_distr(ssf_dat$sl_[ssf_dat$case_ == "TRUE"], dist_name = "gamma", na.rm = TRUE)

### The quantiles we will update the parameters with (from corrected workflow)
quantiles <- c(0.05, 0.5, 0.95)

# After fitting the mixed-effect model using the glmmTMB package, we get the model output into the mixedSSA package for updating the movement parameters. First, we update the parameters in the step-length distribution and assume they follow a gamma distribution.
updated_sl_parameters <- mixedSSA::update_dist(model = m_mixedssa,
                                               dist_name = "gamma", # name of the step-length distribution assumed
                                               beta_sl =  "sl_", # the name of the step lengths coefficient in our model
                                               # beta_log_sl = "log_sl_", # the name of the log(sl) coefficient in our model
                                               beta_log_sl = "log(sl_)", # the name of the log(sl) coefficient in our model
                                               interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                               random_effects_var_name = "individual.local.identifier", # for individual update
                                               quantiles = quantiles, # for continuous interaction variables, updated from corrected workflow
                                               tentative_dist = tentative_sl_dist) # updated from corrected workflow

# # Extract the updated movement parameters for the reference habitat class and 
# # not include the first row of tentative movement parameters
# param_value[,9:10] <- updated_sl_parameters@updated_parameters[-1,5:6] # create new columns in the df and add the outputs
# # Warning messages:
# #   1: In `[<-.data.frame`(`*tmp*`, , 9:10, value = list(shape = c(0.567049,  :
# #                                                                    replacement element 1 has 36 rows to replace 35 rows
# #                                                                  2: In `[<-.data.frame`(`*tmp*`, , 9:10, value = list(shape = c(0.567049,  :
# #                                                                                                                                   replacement element 2 has 36 rows to replace 35 rows
# 



#.........................................................................

# We will need to pass the tentative ta distribution (from corrected workflow)
tentative_ta_dist <- amt::fit_distr(ssf_dat$ta_[ssf_dat$case_ == "TRUE"], dist_name = "vonmises", na.rm = TRUE)

updated_ta_parameters <- mixedSSA::update_dist(model = m_mixedssa,
                                               dist_name = "vonmises", # name of the turn-angle distribution assumed
                                               beta_cos_ta =  "cos(ta_)", # the name of the cos(ta) coefficient in our model
                                               interaction_var_name = "elev_25m_start", # name of the habitat variable layer
                                               random_effects_var_name = "individual.local.identifier", # for individual update
                                               # quantiles = FALSE, # for continuous interaction variables
                                               quantiles = quantiles, #updated from corrected workflow
                                               tentative_dist = tentative_ta_dist #updated from corrected workflow
                                               # tentative_dist = fit_distr(ssf_dat$ta_[ssf_dat$case_=="TRUE"], dist_name = "vonmises", na.rm = TRUE) # added this line to give it a distribution? due to the error, see below
)
# Error in validate_tentative_distribution(args) : 
#   arg 'tentative_dist' must not be null for distribution vonmises. See amt::fit_distr.

# tentative_dist <- fit_distr(ssf_dat, dist_name = "vonmises", na.rm = TRUE)
# # Error in `vec_c()`:
# #   ! Can't combine `collar_id` <character> and `x1_` <double>.
# 
# # specifying to the ta_ column and only when cases are true
# tentative_dist <- fit_distr(ssf_dat$ta_[ssf_dat$case_=="TRUE"], dist_name = "vonmises", na.rm = TRUE)

# # troubleshooting: drop all the unnecessary columns that are not doubles or convert them to double type. date types dont work either, dttm
# names(ssf_dat)
# drop_col <- c("goat_name_start", "goat_id_start", "data_type_start", "month_day_start",
#               "individual.local.identifier_start", "goat_name_end", "goat_id_end",  "data_type_end", "month_day_end", 
#               "individual.local.identifier_end", "date_start", "date_end")
# 
# # test <- subset(ssf_dat, select = -drop_col) # doesnt work, conflict with amt package??
# test <- ssf_dat[ , !names(ssf_dat) %in% drop_col]
# # now try to fit distribution


# pop_model <- broom::tidy(m_mixedssa$model)
# Extract the updated movement parameters for the reference habitat class and 
# not include the first row of tentative movement parameters
param_value[,11:12] <- updated_ta_parameters@updated_parameters[-1,4:5] # create new columns in the df and add the outputs
# Warning messages:
#   1: In `[<-.data.frame`(`*tmp*`, , 9:10, value = list(shape = c(0.567049,  :
#                                                                    replacement element 1 has 36 rows to replace 35 rows
#                                                                  2: In `[<-.data.frame`(`*tmp*`, , 9:10, value = list(shape = c(0.567049,  :
#                                                                                                                                   replacement element 2 has 36 rows to replace 35 rows

names(param_value)[9:12] <-c("shape_all", "scale_all", "kappa_all", "mu_all") 






#//////////////////////////////////////////////////////////////////////////////
# PLOT ----
#//////////////////////////////////////////////////////////////////////////////

# param_value1 <- param_value |> mutate(indv_shape_bias = shape_tentative - shape_indv,
#                                       mixed_shape_bias = shape_tentative - shape_all,
#                                       indv_scale_bias = scale_tentative - scale_indv,
#                                       mixed_scale_bias = scale_tentative - scale_all,
#                                       indv_kappa_bias = kappa_tentative - kappa_indv,
#                                       mixed_kappa_bias = kappa_tentative - kappa_all) 
# 
# 
# shp_bias <- param_value1 |> 
#   pivot_longer(cols = c(indv_shape_bias, mixed_shape_bias), names_to = "shapediff", values_to = "shape_value")|>
#   ggplot()+ geom_boxplot(aes(y= shape_value, x = shapediff, fill = shapediff), outlier.shape = NA)+
#   geom_jitter(aes(y= shape_value, x = shapediff), size = 0.6)+
#   scale_x_discrete(labels=c('Individual\nModel', 'Mixed-effect\nModel'))+
#   scale_fill_manual(breaks = c("indv_shape_bias", "mixed_shape_bias"),
#                     values = c("indv_shape_bias" = "#ab1489",
#                                "mixed_shape_bias"= "#69b3a2"))+
#   labs(y= "Shape Parameter deviation", x= "Model type")+  theme_bw()+
#   theme(legend.position = "none", 
#         axis.title = element_text(size=16)) 
# 
# scl_bias <- param_value1 |> 
#   pivot_longer(cols = c(indv_scale_bias, mixed_scale_bias), names_to = "scalediff", values_to = "scale_value")|>
#   ggplot()+ geom_boxplot(aes(y= scale_value, x = scalediff, fill = scalediff), outlier.shape = NA)+
#   geom_jitter(aes(y= scale_value, x = scalediff), size = 0.6)+
#   scale_x_discrete(labels=c('Individual\nModel', 'Mixed-effect\nModel'))+
#   scale_fill_manual(breaks = c("indv_scale_bias", "mixed_scale_bias"),
#                     values = c("indv_scale_bias" = "#ab1489",
#                                "mixed_scale_bias"= "#69b3a2"))+
#   labs(y= "Scale Parameter deviation", x= "Model type")+ theme_bw()+ 
#   theme(legend.position = "none", 
#         axis.title = element_text(size=16))
# 
# kap_bias <- param_value1 |>
#   pivot_longer(cols = c(indv_kappa_bias, mixed_kappa_bias), names_to = "kappadiff", values_to = "kappa_value") |>
#   ggplot()+ geom_boxplot(aes(y= kappa_value, x = kappadiff, fill = kappadiff), outlier.shape = NA)+
#   geom_jitter(aes(y= kappa_value, x = kappadiff), size = 0.6)+
#   scale_x_discrete(labels=c('Individual\nModel', 'Mixed-effect\nModel'))+
#   scale_fill_manual(breaks = c("indv_kappa_bias", "mixed_kappa_bias"),
#                     values = c("indv_kappa_bias" = "#ab1489",
#                                "mixed_kappa_bias"= "#69b3a2"))+
#   labs(y= "Kappa Parameter deviation", x= "Model type")+ theme_bw()+ 
#   theme(legend.position = "none", 
#         axis.title = element_text(size=16))
# 
# plot_grid(shp_bias, scl_bias, kap_bias, ncol=3, labels = c("A", "B", "C"))
# 
# 
# sessionInfo()



#//////////////////////////////////////////////////////////////////

# plot from corrected workflow
plot_sl_cont <- mixedSSA::plot_updated_dist(
  updated_dist_params_obj = updated_sl_parameters,
  include_tentative = FALSE,
  print_plot = FALSE,
  xlim = 100
) +
  scale_color_manual(
    name = "Elevation",
    labels = c("low", "Medium", "high"),
    values = c("orange", "red", "brown")
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )


plot_ta_cont <- mixedSSA::plot_updated_dist(
  updated_dist_params_obj = updated_ta_parameters,
  vonmises_mu = pi,
  include_tentative = FALSE,
  print_plot = FALSE
) +
  scale_color_manual(
    name = "Elevation",
    labels = c("low", "Medium", "high"),
    values = c("orange", "red", "brown")
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10)
  )



#////////////////////////////////////////////////////////////////////
# Delta method for SE calculation of mean speed ----
# from corrected workflow
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
  dplyr::filter(
    grouping != "tentative"
  ) |>
  mutate(
    # get the actual values of quantiles for delta method
    dist_quant = quantile(ssf_dat$elev_25m_start, # not sure if its supposed to be start or end as the corrected workflow/paper doesnt say, it just has it as a distance to whatever not based on start or end of the step
                          na.rm = T,
                          probs = as.numeric(grouping)
    )
  )


for (i in 1:nrow(cont_sl_dist)) {
  distanceq <- cont_sl_dist$dist_quant[i] # pull of the distance value
  cont_sl_dist$se[i] <- sqrt(
    msm::deltamethod(
      ~ (tentative_shape + x3 + x8 * distanceq) * (1 / (1 / tentative_scale - (x2 + x7 * distanceq))),
      contmean,
      contvar
    )
  )
}


# check output
cont_sl_dist

plot_data <- cont_sl_dist |>
  mutate(
    mean = shape * scale,
    ci_lwr = mean - 1.96 * se,
    ci_upr = mean + 1.96 * se
  )

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
    axis.text = element_text(size = 10)
  )


paper_figure <- 
  cowplot::plot_grid(
  # plot_sl_cat, plot_delta_cat, plot_ta_cat,
  plot_sl_cont, plot_delta_sd, plot_ta_cont,
  # ncol = 3, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  ncol = 3, labels = c("(a)", "(b)", "(c)"),
  hjust = -0.1)
