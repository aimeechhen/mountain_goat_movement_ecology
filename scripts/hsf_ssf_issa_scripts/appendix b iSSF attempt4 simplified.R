





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

#..................................................................
# Import data ----
#..................................................................

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


#format numbers in scientific notation with 10^n
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#..................................................
# Subset animal ----
#..................................................

# subset to single animal
dat <- fisher %>% 
  filter(collar_id == unique(collar_id)[6]) %>% # change the number to switch to the next one
  arrange(t_) ## reorder in ascending order

unique(dat$goat_name)

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
  mutate(elev_25m_start = scale(elev_25m_start), # scale and center our covariates and add the additional movement covariates
         elev_25m_end = scale(elev_25m_end),
         dist_escape_25m_start = scale(dist_escape_25m_start),      # Scale distance to escape at start
         dist_escape_25m_end = scale(dist_escape_25m_end),          # Scale distance to escape at end
         cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_)
  ) %>% 
  filter(!is.na(ta_))

print(ssf_dat, n = 3, width = Inf)


# Check parameters
sl_distr(ssf_dat) # shape and scale parameters
ta_distr(ssf_dat) # kappa and mu parameters


#........................................................
# Fit issa ----
#........................................................
# fit issa with covariates and interaction terms

m4 <- ssf_dat %>% 
  fit_issf(case_ ~ elev_25m_end + dist_escape_25m_end +   
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





#...................................................................
## elevation ----

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
### plot sl df for elevation ----
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
### plot ta df for elevation ----
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
## dist to escape ----
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
### plot sl df for dist to esc ----
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
### plot ta df for dist to esc ----
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
                              # p4, 
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
## Multi-panel plot ----
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
   plot_elevation, plot_dist_esc, plot_issa)

