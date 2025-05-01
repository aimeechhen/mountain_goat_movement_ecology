



# SSF ----

# based on the framework/how to guide Appendix B https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441


library(amt)
library(terra)
library(dplyr)
library(forcats) # fct_collapse() collapse factor levels into manually defined groups
library(ggplot2)
library(broom) #tidy()
library(tidyverse)
library(beepr)



# Import combined collar data (original + new)
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

# load spatial covariate data (packedspatraster or spatraster object) and reproject
elev_25m <- rast('data/rasters/elev_25m.tif') %>% 
  project("epsg:4326")
dist_escape_25m <- rast('data/rasters/dist_escape_25m.tif') %>% 
  project("epsg:4326")

# convert to track_xyz object
fisher <- make_track(goat_data, 
                     .x = longitude,             # x-coordinates (projected)
                     .y = latitude,             # y-coordinates (projected)
                     .t = timestamp,     # timestamp column in POSIXct format
                     crs = 4326,        # Assuming UTM Zone 10N (adjust if necessary)
                     all_cols = TRUE     # Retain all other columns
)


#format numbers in scientific notation with 10^n for plotting
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# subset to single animal
dat <- fisher %>% 
  # filter(goat_name == "kid_rock") %>% # subset to 1 animal
  filter(goat_name == "selena_goatmez") %>%
  arrange(t_) ## reorder in ascending order

# inspect sampling rate of data
summarize_sampling_rate(dat)
# 6.25 h


#.................................................................
## Section 2b: Format and Generate Random Steps ----

#****************WHAT SHOULD MY TOLERANCE BE?****************
ssf_dat <- dat %>% 
  track_resample(rate = minutes(375), tolerance = minutes(60)) %>%  #resample the track to get regular time steps based on summarize_sampling_rate
  steps_by_burst() %>% # convert from points to steps
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

print(ssf_dat, n = 3, width = Inf)


# #..........................................
# ## Section 2c: Tentative movement parameters ----
# 
# #fit a tentative step-length and turn-angle distributions
# # then samples from these distributions to generate random steps
# 
# 
# # view the parameters of the tentative step-length distribution
# sl_distr(ssf_dat)
# # access the parameters of the tentative turn-angle distribution 
# ta_distr(ssf_dat)



#_______________________________________________
# Section 3a: Fit iSSF ----


# fit basic iSSF with habitat covariates (no interaction terms)

m1 <- ssf_dat %>% 
  fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

# inspect model summary
summary(m1)


# 
# #...................................
# ## Section 3c: Calculating Relative Selection Strength (RSS) for Two Locations ----
# 
# # create 2 dataframes to calculate log-RSS
# 
# # plot relative selection strength
# # Make a new data.frame for s1
# s1 <- data.frame(
#   elev_25m_end = seq(from = -2, to = 2, length.out = 200),
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s2
# s2 <- data.frame(
#   elev_25m_end = 0, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # Calculate log-RSS
# lr2 <- log_rss(m1, s1, s2)
# # lr2$df
# 
# ggplot(lr2$df, aes(x = elev_25m_end_x1, y = log_rss)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#   xlab("Elevation (SD)") +
#   ylab("log-RSS vs Mean Elevation") +
#   theme_bw()
# 
# 


#...................................
# ## Section 3e: Large-Sample Confidence Intervals ----
# 
# #** skipped bootstrapping section, may need to do it for checks**
# 
# # to avoid computing time for calculating bootstrap CI
# lr2_ci_se <- log_rss(m1, s1, s2, ci = "se", ci_level = 0.95)
# 
# # Plot
# ggplot(lr2_ci_se, aes(x = elev_25m_end_x1, y = log_rss, 
#                         group = factor(type), 
#                         fill = factor(type))) +
#   geom_ribbon(aes(ymin = lwr, ymax = upr), 
#               linetype = "dashed", color = "black", alpha = 0.25) +
#   geom_line(size = 1, color = "black") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#   xlab("Elevation (SD)") +
#   ylab("log-RSS vs Mean Elevation") +
#   theme_bw()



## Update model with interaction terms ----
# 
# #...........................................................
# ## Section 3g:  Update Step-Length Distribution
# 
# 
# # Update the distribution
# updated_sl <- update_sl_distr(m1)
# 
# 
# # Check the class
# class(updated_sl)
# print(updated_sl)
# 
# 
# # data.frame for plotting
# plot_sl <- data.frame(x = rep(NA, 100))
# 
# # x-axis is sequence of possible step lengths
# plot_sl$x <- seq(from = 0, to = 400, length.out = 100)
# 
# # y-axis is the probability density under the given gamma distribution
# # For the tentative distribution
# plot_sl$tentative <- dgamma(
#   x = plot_sl$x, 
#   shape = m1$sl_$params$shape,
#   scale = m1$sl_$params$scale)
# 
# # For the updated distribution
# plot_sl$updated <- dgamma(
#   x = plot_sl$x,
#   shape = updated_sl$params$shape,
#   scale = updated_sl$params$scale)
# 
# # Pivot from wide data to long data
# plot_sl <- plot_sl %>% 
#   pivot_longer(cols = -x)
# 
# 
# # Plot 
# #*************************DOES NOT LOOK RIGHT**************************  issue with the data?!?!?!?
# # probability density (yaxis) is like -1.37^-195 to -2.88^-194
# #also its not a curve -> issue with the data?
# ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
#   geom_line(size = 1) +
#   xlab("Step Length (m)") +
#   ylab("Probability Density") +
#   scale_color_manual(name = "Distribution", 
#                      breaks = c("tentative", "updated"),
#                      values = c("blue", "orange")) +
#   theme_bw()
# 
# # the estimates do not change very much because the sl_ and log_sl_ were not stat significant in fitted model (m1)

# 
# #...............................................................
# ## Section 3h: Update Turn-Angle Distribution
# updated_ta <- update_ta_distr(m1)
# 
# # Check the class
# class(updated_ta)
# print(updated_ta)
# 
# # compare tentative and updated distributions to se how accounting for habitat-selection changes estimates of selection-free turn-angle distribution
# 
# # data.frame for plotting
# plot_ta <- data.frame(x = rep(NA, 100))
# 
# # x-axis is sequence of possible step lengths
# plot_ta$x <- seq(from = -1 * pi, to = pi, length.out = 100)
# 
# # y-axis is the probability density under the given von Mises distribution
# # For the tentative distribution
# plot_ta$tentative <- circular::dvonmises(
#   x = plot_ta$x, 
#   mu = m1$ta_$params$mu,
#   kappa = m1$ta_$params$kappa)
# 
# 
# # For the updated distribution 
# plot_ta$updated <- circular::dvonmises(
#   x = plot_ta$x, 
#   mu = updated_ta$params$mu,
#   kappa = updated_ta$params$kappa)
# 
# 
# 
# # Pivot from wide data to long data
# plot_ta <- plot_ta %>% 
#   pivot_longer(cols = -x)
# 
# # Plot
# ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
#   geom_line(size = 1) +
#   coord_cartesian(ylim = c(0, 0.25)) +
#   xlab("Relative Turn Angle (radians)") +
#   ylab("Probability Density") +
#   scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
#                      labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
#   scale_color_manual(name = "Distribution", 
#                      breaks = c("tentative", "updated"),
#                      values = c("blue", "orange")) +
#   theme_bw()
# 
# 
# # again, estimates do not change very much, cos_ta_ was not stat. significant in fitted model (m1) p=0.44
# 
# 
# 
# 
# #.....................................................................
# ### Section 3i: Calculating RSS for Movements ----
# like50 <- dgamma(50, 
#                  shape = updated_sl$params$shape,
#                  scale = updated_sl$params$scale)
# # = 0
# like100 <- dgamma(100, 
#                   shape = updated_sl$params$shape,
#                   scale = updated_sl$params$scale)
# # = 0
# like50/like100
# # .-. 0/0 = NaN
# #************ISSUE***************
# # [1] NaN
# 
# 



#________________________________________________
# Section 4: More Complex Models ----

# 
# 
# ## Section 4a: Non-Linear Relationships: 
# 
# #Adding Quadratic Terms ----
# 
# # adding quadratic term for elevation (could use splines or polynomials to capture non-linear effect_)
# # example, if the the animal select/avoids low, med, or high elevation 
# 
# 
# 
# m2 <- ssf_dat %>% 
#   fit_issf(case_ ~ dist_escape_25m_end + elev_25m_end + I(elev_25m_end^2) + 
#              sl_ + log_sl_ + cos_ta_ + 
#              strata(step_id_), model = TRUE)
# 
# 
# 
# # data.frame for s1
# s1 <- data.frame(
#   elev_25m_end = 3, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s2
# s2 <- data.frame(
#   elev_25m_end = 2, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s3
# s3 <- data.frame(
#   elev_25m_end = -1, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s4
# s4 <- data.frame(
#   elev_25m_end = -2, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   # landuseC_end = factor("wet",  levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # Calculate log-RSS under m1
# lr_m1_s1s2 <- log_rss(m1, s1, s2)
# lr_m1_s3s4 <- log_rss(m1, s3, s4)
# 
# # Calculate log-RSS under m2
# lr_m2_s1s2 <- log_rss(m2, s1, s2)
# lr_m2_s3s4 <- log_rss(m2, s3, s4)
# 
# # Compare
# data.frame(
#   "model" = c("linear", "quadratic"),
#   "logRSS_s1_s2" = round(c(lr_m1_s1s2$df$log_rss, lr_m2_s1s2$df$log_rss), 3),
#   "logRSS_s3_s4" = round(c(lr_m1_s3s4$df$log_rss, lr_m2_s3s4$df$log_rss), 3))
# 
# 
# 
# 
# # Range of log-RSS predictions
# 
# # Make a new data.frame for s1
# s1 <- data.frame(
#   elev_25m_end = seq(from = -2, to = 2, length.out = 100),
#   dist_escape_25m_end = 1.5,
#   # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s2
# s2 <- data.frame(
#   elev_25m_end = 0, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # Calculate log-RSS
# lr_m2 <- log_rss(m2, s1, s2)
# lr_m2$df
# 
# 
# # Plot using ggplot2
# ggplot(lr_m2$df, aes(x = elev_25m_end_x1, y = log_rss)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#   xlab("Elevation (SD)") +
#   ylab("log-RSS vs Mean Elevation") +
#   theme_bw()
# 
# 
# #........................................................................
# ## Section 4c: Categorical covariates ----
# #........................................................................
# 
# # no categorical covariates, skipping model 3
# 






#........................................................................
## Section 4d: Continuous Movement Predictors ----
#........................................................................

# fit issa with covariates and interaction terms

m4 <- ssf_dat %>% 
  fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +   
             sl_ + log_sl_ + cos_ta_ + 
             elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
             dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + # added dist to esc
             strata(step_id_), model = TRUE)

summary(m4)



#...................................................................
### elevation ----

# Creating elevation values -> low, med, high
# continuous to categorical -> based on values that corresponded to mean and +/- 2 standard deviates from the mean (because the covariates were scaled and centered)


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

# hi elevation step-length distribution
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

# med turn-angle distribution
med_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    0 * m4$model$coefficients["cos_ta_:elev_25m_start"])

# hi turn-angle distribution
hi_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    2 * m4$model$coefficients["cos_ta_:elev_25m_start"])




#.....................................
# plot results for elevation

#create data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
plot_sl$low <- dgamma(x = plot_sl$x, 
                      shape = low_sl$params$shape,
                      scale = low_sl$params$scale)

plot_sl$medium <- dgamma(x = plot_sl$x, 
                         shape = med_sl$params$shape,
                         scale = med_sl$params$scale)

plot_sl$high <- dgamma(x = plot_sl$x, 
                       shape = hi_sl$params$shape,
                       scale = hi_sl$params$scale)

# Pivot from wide to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)


# Plot
#format numbers in scientific notation with 10^n
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
p1 <-
ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  scale_y_continuous(labels = scientific_10) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() 
# prob density number seems very wrong its 10^-197

#create data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# low
plot_ta$low <- circular::dvonmises(x = plot_ta$x, 
                                   kappa = low_ta$params$kappa,
                                   mu = 0)
# issue with plotting low -> kappa value is negative (and cannot be negative(?))

plot_ta$medium <- circular::dvonmises(x = plot_ta$x, 
                                      kappa = med_ta$params$kappa,
                                      mu = 0)

plot_ta$high <- circular::dvonmises(x = plot_ta$x, 
                                    kappa = hi_ta$params$kappa,
                                    mu = 0)

# Pivot from wide to long data
plot_ta <- plot_ta %>% 
  pivot_longer(cols = -x)

# Plot
p2 <- ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  theme_bw()


#.......................................................
# Multi-panel
#.......................................................
library(gridExtra)

plot_elevation <- grid.arrange(p1,
                               p2,
                               ncol = 2)

ggsave(plot_elevation, 
       file="figures/issa/issa_elevation.png",
       width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")




#...................................................................
### dist to escape----

# Creating dist to escape values -> low, med, high

# continuous to categorical -> based on values that corresponded to mean and +/- 2 standard deviates from the mean (because the covariates were scaled and centered)

# Low step-length distribution
low_sl2 <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] + 
    -2 * m4$model$coefficients["sl_:dist_escape_25m_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    -2 * m4$model$coefficients["log_sl_:dist_escape_25m_start"])

# Medium step-length distribution
med_sl2 <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    0 * m4$model$coefficients["sl_:dist_escape_25m_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    0 * m4$model$coefficients["log_sl_:dist_escape_25m_start"])

# hi step-length distribution
hi_sl2 <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    2 * m4$model$coefficients["sl_:dist_escape_25m_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    2 * m4$model$coefficients["log_sl_:dist_escape_25m_start"])


#.............................
# Creating turn angle values -> low, med, high


# low turn-angle distribution
low_ta2 <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    -2 * m4$model$coefficients["cos_ta_:dist_escape_25m_start"])

# med turn-angle distribution
med_ta2 <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    0 * m4$model$coefficients["cos_ta_:dist_escape_25m_start"])

# hi turn-angle distribution
hi_ta2 <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    2 * m4$model$coefficients["cos_ta_:dist_escape_25m_start"])





#.....................................
# plot results for elevation

#create data.frame for plotting
plot_sl2 <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl2$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
plot_sl2$low <- dgamma(x = plot_sl2$x, 
                      shape = low_sl2$params$shape,
                      scale = low_sl2$params$scale)

plot_sl2$medium <- dgamma(x = plot_sl2$x, 
                         shape = med_sl2$params$shape,
                         scale = med_sl2$params$scale)

plot_sl2$high <- dgamma(x = plot_sl2$x, 
                       shape = hi_sl2$params$shape,
                       scale = hi_sl2$params$scale)

# Pivot from wide to long data
plot_sl2 <- plot_sl2 %>% 
  pivot_longer(cols = -x)


# Plot

p3 <-
  ggplot(plot_sl2, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Distance to escape terrain",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  scale_y_continuous(labels = scientific_10) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() 
# again issue with prob density values


#create data.frame for plotting
plot_ta2 <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta2$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# low
plot_ta2$low <- circular::dvonmises(x = plot_ta2$x, 
                                   kappa = low_ta2$params$kappa,
                                   mu = 0)

plot_ta2$medium <- circular::dvonmises(x = plot_ta2$x, 
                                      kappa = med_ta2$params$kappa,
                                      mu = 0)

plot_ta2$high <- circular::dvonmises(x = plot_ta2$x, 
                                    kappa = hi_ta2$params$kappa,
                                    mu = 0)
# issue with value, kappa is negative when it cannot be

# Pivot from wide to long data
plot_ta2 <- plot_ta2 %>% 
  pivot_longer(cols = -x)

# Plot
p4 <- ggplot(plot_ta2, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Distance to escape terrain",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  theme_bw()



#.......................................................
# Multi-panel
#.......................................................


plot_dist_esc <- grid.arrange(p3,
                              p4,
                              ncol = 2)

ggsave(plot_dist_esc, 
       file="figures/issa/issa_dist_escape.png",
       width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")


#.......................................................
### Multi-panel plot ----
#.......................................................



plot_issa <- grid.arrange(plot_elevation,
                          plot_dist_esc,
                          nrow = 2)

ggsave(plot_dist_esc, 
       file="figures/issa/plot_issa.png",
       width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")