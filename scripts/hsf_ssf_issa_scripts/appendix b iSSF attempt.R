
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

# Section 1: Overview ----
# how to fit and interpret parameters and outputs for iSSA

# Step-selection functions (SSF) -> model transition or "steps" connecting sequenced/sequential locations (Δt units apart) in geographical space
# i.e. SSF analyse animal movement focusing on paths or "steps" it takes between 2 continuous or sequential location over a set interval  (Δt)

# conditional probability of finding an individual at a location at a time give it was at that location and time +
# selection-independent movement kernel -> describes how the naimal would move in a homogeneous habitat or in absence of habitat selection 
# (i.e.absence of habitat selection  = ssf is consistant for all locations)

#.............................................................
## iSSA Workflow summary
# Integrated step-selection analyses steps:
# 1. Estimate a tentative selection-free movement kernel, using observed step-lengths and turn angles
# 2. Generate time-dependent available locations by simulating potential movements from the previously observed location
# 3. Estimate β (regression/parameter coefficients) using conditional logistic regression, with strata formed by combining time-dependent used and available locations.
#  4. Re-estimate the movement parameters in using regression coefficients associated with movement characteristics (e.g., step-length, log step-length, and cosine of the turn angle). This step adjusts the parameters in the movement model to account for the effect of habitat selection when estimating the movement kernel

# Appendix section
# step 1-2: Format and Generate Random Steps (section 2b) and Tentative Movement Parameters (section 2c)
# step 3: Fit iSSF (section 3a)
# step 4: Interpreting Movement Parameters *section 3b)

# simplified
# step 1: estimate movement kernel via observed step-lengths and turn angles  
# step 2: simulate potential movement from observed location to generate available locations
# step 3: combine used and available locations -> do a conditional logistic regression to estimate beta
# step 4: use regression coefficient [associated with movement characteristics (step-length, log step-length, cosine of turn angle)] -> to re-estimate movement parameters (i.e. re-adjust parameters to account for effect of habitat selection when estimating movement kernel)



# Section 2: Data prep ----

# collar 
load("data/collar_data/collar_data_20241123.rda")

# subset to fire goats
fire_goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "caprini", "vincent_van_goat", "rocky")
fisher <- collar_data[collar_data$goat_name %in% fire_goats,]

# load spatial covariate data (packedspatraster or spatraster object)
elev_25m = rast('data/rasters/elev_25m.tif')
dist_escape_25m = rast('data/rasters/dist_escape_25m.tif')
# reproject to lat/long crs
elev_25m <- project(elev_25m, "epsg:4326")
dist_escape_25m <-  project(dist_escape_25m, "epsg:4326")

# convert to  track_xyz object
fisher <- make_track(fisher, 
                     .x = longitude,             # x-coordinates (projected)
                     .y = latitude,             # y-coordinates (projected)
                     .t = timestamp,     # timestamp column in POSIXct format
                     crs = 4326,        # Assuming UTM Zone 10N (adjust if necessary)
                     all_cols = TRUE     # Retain all other columns
)

# subset to single animal
dat <- fisher %>% 
  # filter(goat_name == "kid_rock") %>% # subset to 1 animal
  filter(goat_name == "vincent_van_goat") %>%
  arrange(t_) ## reorder in ascending order

# # group landcover types from numerical values into categorical groups
# reclass_landuse <- function(x) {
#   fct_collapse(factor(x),
#                forest = c("30","40","50","60", "70","80", "90","100"),
#                grass = c("120", "140"),
#                wet = c("160"))
# }

# inspect sampling rate of data
summarize_sampling_rate(dat)
# rate of 6.25h and a deviation of +/- 2 minutes? of the sampling rate


#.................................................................
## Section 2b: Format and Generate Random Steps ----

# using data for 1 individual
#workflow:
# 1. resample the track to get regular time steps (track_resample()),
# 2. convert from points to steps (steps_by_burst()), 
# 3. generate random available steps (random_steps()), and 
# 4. attach the environmental covariates to both the observed and available steps (extract_covariates())
# 5. scale and center our covariates and add the additional movement covariates that we will need to conduct an integrated step-selection analysis.

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


# #TROUBLESHOOTING
# # Resample the track to get regular time steps
# ssf_dat1 <- track_resample(dat, rate = minutes(375), tolerance = minutes(15))
# # Convert the resampled track from points to steps
# ssf_dat2 <- steps_by_burst(ssf_dat1)
# # Generate random available steps
# ssf_dat3 <- random_steps(ssf_dat2)
# # Extract elevation covariates for both start and end points of the steps
# ssf_dat4 <- extract_covariates(ssf_dat3, elev_25m, where = "both")
# # Extract escape distance covariates for both start and end points of the steps
# ssf_dat5 <- extract_covariates(ssf_dat4, dist_escape_25m , where = "both")
# # Scale and center covariates, and calculate additional movement covariates
# ssf_dat6 <- mutate(
#   ssf_dat5,
#   elev_25m_start = scale(elev_25m_start),  # Scale elevation at step start
#   elev_25m_end = scale(elev_25m_end),      # Scale elevation at step end
#   dist_escape_25m_start = scale(dist_escape_25m_start),      # Scale distance to escape at start
#   dist_escape_25m_end = scale(dist_escape_25m_end),          # Scale distance to escape at end
#   cos_ta_ = cos(ta_),                        # Cosine of turn angle
#   log_sl_ = log(sl_)                         # Logarithm of step length
# )
# # check for NA values
# any(is.na(ssf_dat1))
# any(is.na(ssf_dat2))
# ssf_dat2[!complete.cases(ssf_dat2),]
# print(ssf_dat2[!complete.cases(ssf_dat2),], n = 174)
# any(ssf_dat2$sl_ == 0)
# ssf_dat2[ssf_dat2$sl_ == 0,]
# any(is.na(ssf_dat5))
# ssf_dat5[!complete.cases(ssf_dat5),]



print(ssf_dat, n = 3, width = Inf)



#..........................................
## Section 2c: Tentative movement parameters ----

#fit a tentative step-length and turn-angle distributions and 
# then samples from these distributions to generate random steps
# view the parameters of the tentative step-length distribution
sl_distr(ssf_dat)
# for kid_rock -> shape = 0.555; scale = 0.007
# for vincent_van_goat -> shape = 0.537; scale = 0.006


# access the parameters of the tentative turn-angle distribution 
ta_distr(ssf_dat)
# for kid_rock -> kappa = 0.153; mu = 0
# for vincent_van_goat -> kappa = 0.208; mu = 0


# the values are very small


#_______________________________________________
# Section 3: Basic iSSF ----

# section summary:
# fit basic iSSF
# how to interpret habitat effects and movement parameters


# ...................................
## Section 3a: Fit iSSF ----
# fit iSSF with  habitat covariates
# If habitat selection at the step scale depends on the covariates, and if, in the absence of habitat selection, step lengths and turn angles follow a constant distribution (i.e., the selection-free movement kernel does not depend on covariates, then this model can be fitted

m1 <- ssf_dat %>% 
  fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)



#...................................
## Section 3b: Interpreting Habitat-Selection Parameters ----
# explore coefficient of fitted model
# interpretation of the parameters is very similar to the RSF interpretation but at a local scale with habitat availability determined by the selection-free movement kernel
# Exponentiating habitat-selection parameters in a fitted step-selection model allows for comparing the relative rates of use between two locations. These locations differ by 1 unit in an explanatory variable, but are otherwise identical/equivalent. The locations should be equally accessible and have the same values for all other explanatory variables.
summary(m1)


# all values are shown as significant.....


#...................................
## Section 3c: Calculating Relative Selection Strength (RSS) for Two Locations ----

# create 2 dataframes to calculate log-RSS

# data.frame for s1
s1 <- data.frame(
  elev_25m_end = 3,
  dist_escape_25m_end = 1.5,
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elev_25m_end = 2,
  dist_escape_25m_end = 1.5,
   sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)


# calculate log-RSS
lr1 <- log_rss(m1, x1 = s1, x2 = s2)
lr1$df



# plot relative selection strength
# Make a new data.frame for s1
s1 <- data.frame(
  elev_25m_end = seq(from = -2, to = 2, length.out = 200),
  dist_escape_25m_end = 1.5,
   sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elev_25m_end = 0, # mean of elev, since we scaled and centered
  dist_escape_25m_end = 1.5,
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr2 <- log_rss(m1, s1, s2)

# Plot using ggplot2
ggplot(lr2$df, aes(x = elev_25m_end_x1, y = log_rss)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()

# Plot using ggplot2
ggplot(lr2$df, aes(x = elev_25m_end_x1, y = exp(log_rss))) +
  geom_line(size = 1) +
  geom_hline(yintercept = exp(0), linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("RSS vs Mean Elevation") +
  theme_bw()



#...................................
## Section 3d: Bootstrap Confidence Intervals ----

# Calculate log-RSS (takes a while to run)
lr2_ci_boot <- log_rss(m1, s1, s2, ci = "boot", ci_level = 0.95, n_boot = 1000) # 19.32 mins
beep(2)

# Check the header of the data.frame
head(lr2_ci_boot$df)

# plot with confidence intervals
ggplot(lr2_ci_boot$df, aes(x = elev_25m_end_x1, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              linetype = "dashed", 
              color = "black", fill = "gray80", alpha = 0.5) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()


#...................................
## Section 3e: Large-Sample Confidence Intervals ----

# to avoid computing time for calculating bootstrap CI
lr2_ci_se <- log_rss(m1, s1, s2, ci = "se", ci_level = 0.95)

# Check the header of the data.frame
head(lr2_ci_se$df)


# Add "type" label to both predictions data.frames
lr2_ci_boot$df$type <- "Bootstrap"
lr2_ci_se$df$type <- "SE"

# Combine two prediction data.frames
lr2_ci_both <- rbind(lr2_ci_boot$df, lr2_ci_se$df)

# Plot
ggplot(lr2_ci_both, aes(x = elev_25m_end_x1, y = log_rss, 
                        group = factor(type), 
                        fill = factor(type))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              linetype = "dashed", color = "black", alpha = 0.25) +
  geom_line(size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_fill_manual(name = "CI Type", breaks = c("Bootstrap", "SE"),
                    values = c("blue", "orange")) +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()




#.................................................
## Section 3f: Interpreting Movement Parameters ----



#...........................................................
## Section 3g:  Update Step-Length Distribution

# Update the distribution
updated_sl <- update_sl_distr(m1)


# Check the class
class(updated_sl)
print(updated_sl)


# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# For the tentative distribution
plot_sl$tentative <- dgamma(
  x = plot_sl$x, 
  shape = m1$sl_$params$shape,
  scale = m1$sl_$params$scale)

# For the updated distribution
plot_sl$updated <- dgamma(
  x = plot_sl$x,
  shape = updated_sl$params$shape,
  scale = updated_sl$params$scale)

# Pivot from wide data to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)

# Plot 
#*************************DOES NOT LOOK RIGHT**************************  issue with the data?!?!?!?
# probability density (yaxis) is like -1.37^-195 to -2.88^-194
#also its not a curve -> issue with the data?
ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  scale_color_manual(name = "Distribution", 
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()


#...............................................................
## Section 3h: Update Turn-Angle Distribution ----
# Update the distribution
updated_ta <- update_ta_distr(m1)

# Check the class
class(updated_ta)
print(updated_ta)

# data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -1 * pi, to = pi, length.out = 100)

# y-axis is the probability density under the given von Mises distribution
# For the tentative distribution
plot_ta$tentative <- circular::dvonmises(
  x = plot_ta$x, 
  mu = m1$ta_$params$mu,
  kappa = m1$ta_$params$kappa)
# Warning message:
#   In as.circular(x) :
#   an object is coerced to the class 'circular' using default value for the following components:
#   type: 'angles'
# units: 'radians'
# template: 'none'
# modulo: 'asis'
# zero: 0
# rotation: 'counter'
# conversion.circularxradians0counter


# For the updated distribution 
#****************ISSUE WITH KAPPA???**************** PROBLEM WITH THE DATA?????
plot_ta$updated <- circular::dvonmises(
  x = plot_ta$x, 
  mu = updated_ta$params$mu,
  kappa = updated_ta$params$kappa)

# 
# Error in circular::dvonmises(x = plot_ta$x, mu = updated_ta$params$mu,  : 
#                                the concentration parameter 'kappa' must be non negative
#                              In addition: Warning message:
#                                In as.circular(x) :
#                                an object is coerced to the class 'circular' using default value for the following components:
#                                type: 'angles'
#                              units: 'radians'
#                              template: 'none'
#                              modulo: 'asis'
#                              zero: 0
#                              rotation: 'counter'
#                              conversion.circularxradians0counter



# Pivot from wide data to long data
plot_ta <- plot_ta %>% 
  pivot_longer(cols = -x)

# Plot
ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
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


#.....................................................................
### Section 3i: Calculating RSS for Movements ----
like50 <- dgamma(50, 
                 shape = updated_sl$params$shape,
                 scale = updated_sl$params$scale)
like100 <- dgamma(100, 
                  shape = updated_sl$params$shape,
                  scale = updated_sl$params$scale)
like50/like100
#************ISSUE***************
# [1] NaN

#________________________________________________
# Section 4: More Complex Models ----



## Section 4a: Non-Linear Relationships: Adding Quadratic Terms ----

m2 <- ssf_dat %>% 
  fit_issf(case_ ~ dist_escape_25m_end + elev_25m_end + I(elev_25m_end^2) + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)



# data.frame for s1
s1 <- data.frame(
  elev_25m_end = 3, # mean of elev, since we scaled and centered
  dist_escape_25m_end = 1.5,
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elev_25m_end = 2, # mean of elev, since we scaled and centered
  dist_escape_25m_end = 1.5,
  # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s3
s3 <- data.frame(
  elev_25m_end = -1, # mean of elev, since we scaled and centered
  dist_escape_25m_end = 1.5,
  # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s4
s4 <- data.frame(
  elev_25m_end = -2, # mean of elev, since we scaled and centered
  dist_escape_25m_end = 1.5,
  # landuseC_end = factor("wet",  levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS under m1
lr_m1_s1s2 <- log_rss(m1, s1, s2)
lr_m1_s3s4 <- log_rss(m1, s3, s4)

# Calculate log-RSS under m2
lr_m2_s1s2 <- log_rss(m2, s1, s2)
lr_m2_s3s4 <- log_rss(m2, s3, s4)

# Compare
data.frame(
  "model" = c("linear", "quadratic"),
  "logRSS_s1_s2" = round(c(lr_m1_s1s2$df$log_rss, lr_m2_s1s2$df$log_rss), 3),
  "logRSS_s3_s4" = round(c(lr_m1_s3s4$df$log_rss, lr_m2_s3s4$df$log_rss), 3))






# Make a new data.frame for s1
s1 <- data.frame(
  elev_25m_end = seq(from = -2, to = 2, length.out = 100),
  dist_escape_25m_end = 1.5,
  # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elev_25m_end = 0, # mean of elev, since we scaled and centered
  dist_escape_25m_end = 1.5,
  # landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr_m2 <- log_rss(m2, s1, s2)

# Plot using ggplot2
#*******************ERROR*************
ggplot(lr_m2$df, aes(x = elev_25m_end, y = log_rss)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()




# Movement and Habitat Interactions

# Interactions Between Movement Characteristics and Categorical Variables

m3 <- ssf_dat %>% 
  fit_issf(case_ ~ dist_escape_25m_end + elev_25m_end +  
             # landuseC_end + 
             sl_ + log_sl_ + cos_ta_ + 
             # landuseC_start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)


summary(m3)




# elevation step-length distribution
elev_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"],
  beta_log_sl = m3$model$coefficients["log_sl_"])
# 
# dist to escapestep-length distribution
dist_esc_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"] +
    m3$model$coefficients["sl_:dist_escape_25m_start"],
  beta_log_sl = m3$model$coefficients["log_sl_"] +
    m3$model$coefficients["log_sl_:dist_escape_25m_start"])
# 
# # Wet step-length distribution
# wet_sl <- update_gamma(
#   dist = m3$sl_,
#   beta_sl = m3$model$coefficients["sl_"] +
#     m3$model$coefficients["sl_:landuseC_startwet"],
#   beta_log_sl = m3$model$coefficients["log_sl_"] + 
#     m3$model$coefficients["log_sl_:landuseC_startwet"])



# Elev turn-angle distribution
elev_ta <- update_vonmises(
  dist = m3$ta_, beta_cos_ta = m3$model$coefficients["cos_ta_"])
# 
# Dist esc turn-angle distribution
dist_esc_ta <- update_vonmises(
  dist = m3$ta_,
  beta_cos_ta = m3$model$coefficients["cos_ta_"] +
    m3$model$coefficients["cos_ta_:dist_escape_25m_start"])




# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_sl$elev <- dgamma(x = plot_sl$x, 
                         shape = elev_sl$params$shape,
                         scale = elev_sl$params$scale)
# Grass
plot_sl$dist_esc <- dgamma(x = plot_sl$x, 
                        shape = dist_esc_sl$params$shape,
                        scale = dist_esc_sl$params$scale)


# Pivot from wide to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)

# Plot
p1<-ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(#name = "Land-use",
                     breaks = c("elev", "dist_esc"),
                     values = c("blue", "purple")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw()


#data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_ta$elev <- circular::dvonmises(x = plot_ta$x, 
                                      kappa = elev_ta$params$kappa,
                                      mu = 0)
# Grass
plot_ta$dist_esc <- circular::dvonmises(x = plot_ta$x, 
                                     kappa = dist_esc_ta$params$kappa,
                                     mu = 0)


# Pivot from wide to long data
plot_ta <- plot_ta %>% 
  pivot_longer(cols = -x)

# Plot
p2 <- ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(#name = "Land-use",
                     breaks = c("elev", "dist"),
                     values = c("blue", "purple")) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  theme_bw()

combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")





#..........................
# Continuous Movement Predictors

m4 <- ssf_dat %>% 
  fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +   
             elev_25m_end + 
             sl_ + log_sl_ + cos_ta_ + 
             elev_25m_start:(sl_ + log_sl_ + cos_ta_) + 
             strata(step_id_), model = TRUE)

summary(m4)


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

# Wet step-length distribution
hi_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    2 * m4$model$coefficients["sl_:elev_25m_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    2 * m4$model$coefficients["log_sl_:elev_25m_start"])



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


#data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_sl$low <- dgamma(x = plot_sl$x, 
                      shape = low_sl$params$shape,
                      scale = low_sl$params$scale)
# Grass
plot_sl$medium <- dgamma(x = plot_sl$x, 
                         shape = med_sl$params$shape,
                         scale = med_sl$params$scale)
# Wet
plot_sl$high <- dgamma(x = plot_sl$x, 
                       shape = hi_sl$params$shape,
                       scale = hi_sl$params$scale)

# Pivot from wide to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)

# Plot
p1 <- ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Elevation",
                     breaks = c("low", "medium", "high"),
                     values = c("navyblue", "gray50", "firebrick")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw() 

#data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# low
plot_ta$low <- circular::dvonmises(x = plot_ta$x, 
                                   kappa = low_ta$params$kappa,
                                   mu = 0)
# med
plot_ta$medium <- circular::dvonmises(x = plot_ta$x, 
                                      kappa = med_ta$params$kappa,
                                      mu = 0)
# hi
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
combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")



