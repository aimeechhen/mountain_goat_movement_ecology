

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

#..................................................................
# Import data ----
#..................................................................

# Import combined collar data (original + new)
goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")
# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)

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


# convert to track_xyz object
fisher <- make_track(goat_data, 
                     .x = lon_reproj,             # x-coordinates (projected)
                     .y = lat_reproj,             # y-coordinates (projected)
                     .t = timestamp,     # timestamp column in POSIXct format
                     crs = "epsg:5070",        # Assuming UTM Zone 10N (adjust if necessary)
                     all_cols = TRUE     # Retain all other columns
)


#format numbers in scientific notation with 10^n
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#..................................................
# Subset animal ----
#..................................................

# subset to single animal
dat <- fisher[fisher$collar_id == unique(fisher$collar_id)[2],]# change the number to switch to the next one
# reorder in ascending order
dat <- dat[order(dat$t_),]

# # subset to single animal
# dat <- fisher %>% 
#   filter(collar_id == unique(collar_id)[2]) %>% # change the number to switch to the next one
#   arrange(t_) ## reorder in ascending order

# inspect sampling rate of data
summarize_sampling_rate(dat)

## Section 2b: Format and Generate Random Steps ----

#****************WHAT SHOULD MY TOLERANCE BE?****************
ssf_dat <- dat %>% 
  track_resample(rate = minutes(375), tolerance = minutes(60)) %>%  #resample the track to get regular time steps based on summarize_sampling_rate
  steps_by_burst() %>% # convert from points to steps
  random_steps() %>%  #  generate random available steps, NOTE: Steps with length 0 are present. This will lead to an error when fitting a gamma distribution. 0 step lengths are replaced with the smallest non zero step length
  extract_covariates(elev_25m, where = "both") %>% 
  extract_covariates(dist_escape_25m , where = "both") %>% 
  mutate(collar_id = collar_id,
         elev_25m_start = scale(elev_25m_start), # scale and center our covariates and add the additional movement covariates
         elev_25m_end = scale(elev_25m_end),
         dist_escape_25m_start = scale(dist_escape_25m_start),      # Scale distance to escape at start
         dist_escape_25m_end = scale(dist_escape_25m_end),          # Scale distance to escape at end
         cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_)
  ) %>% 
  filter(!is.na(ta_)) %>% 
  # include these data
  mutate(
    collar_id = dat$collar_id[1],
    goat_name = dat$goat_name[1]
  ) %>% 
  select(collar_id, goat_name, everything()) # reorganize columns

#check if covariates got values
print(ssf_dat, n = 3, width = Inf)



#..........................................
## Section 2c: Tentative movement parameters ----

#fit a tentative step-length and turn-angle distributions
# then samples from these distributions to generate random steps

# Check parameters
# view the parameters of the tentative step-length distribution
sl_distr(ssf_dat) # shape and scale parameters
# access the parameters of the tentative turn-angle distribution
ta_distr(ssf_dat) # kappa and mu parameters



#........................................................
# Section 3: Fit issa ----
#........................................................

# ##  Section 3b: fit basic iSSF with habitat covariates (no interaction terms) ----
# 
# ## MODEL 1 ----
# 
# m1 <- ssf_dat %>%
#   fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +
#              sl_ + log_sl_ + cos_ta_ +
#              strata(step_id_), model = TRUE)
# 
# # inspect model summary
# summary(m1)
# 
# 
# 
# #_______________________________________________________
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
# lr2$df

# ggplot(lr2$df, aes(x = elev_25m_end_x1, y = log_rss)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#   xlab("Elevation (SD)") +
#   ylab("log-RSS vs Mean Elevation") +
#   theme_bw()
# 
# 
# #...................................
# ## Section 3e: Large-Sample Confidence Intervals ----
# 
# #** skipped bootstrapping section, may need to do it for checks**
# 
# # # to avoid computing time for calculating bootstrap CI
# # lr2_ci_se <- log_rss(m1, s1, s2, ci = "se", ci_level = 0.95)
# # 
# # # Plot
# # ggplot(lr2_ci_se, aes(x = elev_25m_end_x1, y = log_rss,
# #                         group = factor(type),
# #                         fill = factor(type))) +
# #   geom_ribbon(aes(ymin = lwr, ymax = upr),
# #               linetype = "dashed", color = "black", alpha = 0.25) +
# #   geom_line(size = 1, color = "black") +
# #   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
# #   xlab("Elevation (SD)") +
# #   ylab("log-RSS vs Mean Elevation") +
# #   theme_bw()
# 
# 
# #______________________________________________________
# ## Section 3g:  Update Step-Length Distribution ----
# 
# # Update the distribution
# updated_sl <- update_sl_distr(m1)
# 
# # Check the class
# class(updated_sl)
# print(updated_sl)
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
# 
# 
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
# ## Section 3i: Calculating RSS for Movements ----
# like50 <- dgamma(50,
#                  shape = updated_sl$params$shape,
#                  scale = updated_sl$params$scale)
# # = 0
# like100 <- dgamma(100,
#                   shape = updated_sl$params$shape,
#                   scale = updated_sl$params$scale)
# # = 0
# like50/like100 #1.25
# 
# 
# 
# 
# 
# 
# #________________________________________________
# # Section 4: More Complex Models ----
# 
# ## Section 4a: Non-Linear Relationships:
# 
# #Adding Quadratic Terms
# 
# # adding quadratic term for elevation (could use splines or polynomials to capture non-linear effect_)
# # example, if the the animal select/avoids low, med, or high elevation
# 
# ##MODEL 2 ----
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
# #......................................................
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


#........................................................................
## Section 4c: Categorical covariates ----
#........................................................................

# no categorical covariates, skipping model 3






#........................................................................
## Section 4d: Continuous Movement Predictors ----
#........................................................................

# fit issa with covariates and interaction terms

## MODEL 4 ----

m4 <- ssf_dat %>% 
  fit_issf(case_ ~ elev_25m_end +  I(elev_25m_end^2) + dist_escape_25m_end +   
             sl_ + log_sl_ + cos_ta_ + 
             elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
             dist_escape_25m_start:(sl_ + log_sl_ + cos_ta_) + # added dist to esc
             strata(step_id_), model = TRUE)

# save the summary output
sink(paste0("data/issa/", as.character(unique(dat$collar_id)), "_issa_model_summary.txt"))
cat("\n") #enter blank line
print(paste0("ISSA model for collar ID: ", as.character(unique(dat$collar_id)), " | ", as.character(unique(dat$goat_name))))
cat("\n") #enter blank line
print(summary(m4))
sink() #terminate output exporting connection/process



#_______________________________________________________
### Section 3c: Calculating Relative Selection Strength (RSS) for Two Locations ----

# create 2 dataframes to calculate log-RSS

# plot relative selection strength
# # Make a new data.frame for s1
# s1 <- data.frame(
#   elev_25m_start = seq(from = -2, to = 2, length.out = 200),
#   elev_25m_end = seq(from = -2, to = 2, length.out = 200),
#   dist_escape_25m_start = 1.5,
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s2
# s2 <- data.frame(
#   elev_25m_start = 0, # mean of elev, since we scaled and centered
#   elev_25m_end = 0, # mean of elev, since we scaled and centered
#   dist_escape_25m_end = 1.5,
#   dist_escape_25m_start = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s3
# s3 <- data.frame(
#   elev_25m_start = -1, 
#   elev_25m_end = -1, # mean of elev, since we scaled and centered
#   dist_escape_25m_start = 1.5,
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s4
# s4 <- data.frame(
#   elev_25m_start = -2,
#   elev_25m_end = -2, # mean of elev, since we scaled and centered
#   dist_escape_25m_start = 1.5,
#   dist_escape_25m_end = 1.5,
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

# # Calculate log-RSS under m4
# lr_m4_s1s2 <- log_rss(m4, s1, s2) # no elev_25m_start issue
# lr_m4_s3s4 <- log_rss(m4, s3, s4)
# 
# 
# # Compare
# data.frame(
#   "model" = c("linear", "quadratic"),
#   "logRSS_s1_s2" = round(c(lr_m1_s1s2$df$log_rss, lr_m4_s1s2$df$log_rss), 3),
#   "logRSS_s3_s4" = round(c(lr_m1_s3s4$df$log_rss, lr_m4_s3s4$df$log_rss), 3))
# 
# 
# 
# #..........................................................
# ### Range of log-RSS predictions ----
# 
# # Make a new data.frame for s1
# s1 <- data.frame(
#   elev_25m_start = seq(from = -2, to = 2, length.out = 100),
#     elev_25m_end = seq(from = -2, to = 2, length.out = 100),
#   dist_escape_25m_start = 1.5,
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#   log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# # data.frame for s2
# s2 <- data.frame(
#   elev_25m_start = 0,
#   elev_25m_end = 0, # mean of elev, since we scaled and centered
#   dist_escape_25m_start = 1.5,
#   dist_escape_25m_end = 1.5,
#   sl_ = 100,
#    log_sl_ = log(100),
#   cos_ta_ = 1)
# 
# 
# # Calculate log-RSS
# lr_m4 <- log_rss(m4, s1, s2)
# lr_m4$df
# 
# ggplot(lr_m4$df, aes(x = elev_25m_end_x1, y = log_rss)) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
#   xlab("Elevation (SD)") +
#   ylab("log-RSS vs Mean Elevation") +
#   theme_bw()









#______________________________________________________
### Section 3g:  Update Step-Length Distribution ----

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


# Plot
ggplot(plot_sl4, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  scale_color_manual(name = "Distribution",
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()

# the estimates do not change very much because the sl_ and log_sl_ were not stat significant in fitted model (m1)




#...............................................................
### Section 3h: Update Turn-Angle Distribution
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

# Plot
ggplot(plot_ta4, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 0.25)) +
  xlab("Relative Turn Angle (radians)") +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  scale_color_manual(name = "Distribution",
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()


#  again, estimates do not change very much, cos_ta_ was not stat. significant in fitted model (m1) p=0.44




#.....................................................................
### Section 3i: Calculating RSS for Movements ----
like50 <- dgamma(50,
                 shape = updated_sl4$params$shape,
                 scale = updated_sl4$params$scale)
# = 0
like100 <- dgamma(100,
                  shape = updated_sl4$params$shape,
                  scale = updated_sl4$params$scale)
# = 0
like50/like100 #1.23


#create a dataframe to store the rss
rss_data <- data.frame(collar_id = as.character(unique(goat_data$collar_id)),
                       goat_name = as.character(unique(goat_data$goat_name)),
                       # year = numeric(),
                       like50 = NA,
                       like100 = NA,
                       rss = NA)




#....................................................................
# issa dataframe ----
#....................................................................


# data carpentry
ssf_dat$collar_id <- unique(dat$collar_id)
ssf_dat$goat_name <- unique(dat$goat_name)
ssf_dat$goat_id <- unique(dat$goat_id)

# issa_data <- data.frame()


# add to issa df
issa_data <- rbind(issa_data, ssf_dat)

# restart with another individual




write.csv(issa_data, file = "./data/issa/issa_data.csv")















#..................................................
# PLOT ----
#..................................................


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




#...................................................
#### plot sl df for elevation ----
#...................................................

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

#...................................................
#### plot ta df for elevation ----
#...................................................


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

# Combine and save plot

plot_elevation <- grid.arrange(
  p1, 
  p2, 
  ncol = 2,
  top = textGrob(paste("ISSA plot for", as.character(unique(dat$collar_id)), " | ", as.character(unique(dat$goat_name)))))

# # Save the plot
# ggsave(plot_elevation, 
#        file = paste0("figures/issa/", as.character(unique(dat$collar_id)), "_issa_elevation.png"),
#        width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")

# ggsave(last_plot(), 
#        file = paste0("figures/issa/", as.character(unique(dat$collar_id)), "_issa_elevation.png"),
#        width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")


#...................................................................
### dist to escape ----
#...................................................................

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



#...................................................
#### plot sl df for dist to esc ----
#...................................................


# plot results for dist to esc

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
  scale_color_manual(name = "dis esc",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  scale_y_continuous(labels = scientific_10) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() 
# again issue with prob density values


#...................................................
#### plot ta df for dist to esc ----
#...................................................

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
  scale_color_manual(name = "dis esc",
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
                              ncol = 2,
                              top = textGrob(paste("ISSA plot for", as.character(unique(dat$collar_id)), " | ", as.character(unique(dat$goat_name)))))

# # Save the plot
# ggsave(plot_dist_esc, 
#        file = paste0("figures/issa/", as.character(unique(dat$collar_id)), "_issa_dist_escape.png"),
#        width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")


# # Save the plot
# ggsave(last_plot(), 
#        file = paste0("figures/issa/", as.character(unique(dat$collar_id)), "_issa_dist_escape.png"),
#        width = 6.23, height = 6, units = "in", dpi = 600, bg = "transparent")


#.......................................................
### Multi-panel plot ----
#.......................................................



plot_issa <- grid.arrange(plot_elevation,
                          plot_dist_esc,
                          nrow = 2)


ggsave(plot_issa, 
       file = paste0("figures/issa/", as.character(unique(dat$collar_id)), "_issa_combined.png"),
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")







#clean up environment
rm(dat,
   low_sl, med_sl, hi_sl,
   low_ta, med_ta, hi_ta,
   low_sl2, med_sl2, hi_sl2,
   low_ta2, med_ta2, hi_ta2,
   m4,
   p1, p2, p3, p4,
   plot_sl, plot_ta,
   plot_sl2, plot_ta2,
   plot_elevation, plot_dist_esc, plot_issa,
   plot_sl4, plot_ta4, updated_sl4, updated_ta4,
   like50, like100)

