
# HSF / SSF tutorial

# based on the framework/how to guide https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441

library(amt)
library(terra)
library(dplyr)
library(forcats) # fct_collapse() collapse factor levels into manually defined groups
library(ggplot2)
library(broom) #tidy()

# load fisher location (amt_fisher is a track_xyt object) 
fisher <- amt_fisher

# load spatial covariate data (packedspatraster or spatraster object)
landuse <- amt_fisher_covar$landuse
elevation <- amt_fisher_covar$elevation
popden <- amt_fisher_covar$popden

# if packedspatraster object, they need to be unpacked
landuse <- raster(unwrap(landuse))
elevation <- raster(unwrap(elevation))
popden <- raster(unwrap(popden))



# using 1 animal
dat <- fisher[fisher$name == "Lupe",]
# reorder in ascending order
dat <- dat[order(dat$t_),]

dat <- fisher %>% 
  filter(name == "Lupe") %>% 
  arrange(t_)

# group landcover types from numerical values into categorical groups
reclass_landuse <- function(x) {
  fct_collapse(factor(x),
               forest = c("30","40","50","60", "70","80", "90","100"),
               grass = c("120", "140"),
               wet = c("160"))
}

# inspect sampling rate of data
summarize_sampling_rate(dat)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HSF ----

# data preparation
rsf_dat <- dat %>% 
  random_points() %>% #generate random points
  extract_covariates(landuse) %>% # extract covariate values 
  extract_covariates(elevation) %>% 
  extract_covariates(popden) %>% 
  mutate(elevation = scale(elevation)[, 1], # scales covariate to standardise them (ie. center them)
         popden = scale(popden)[, 1],
         landuseC = reclass_landuse(landuse), # reclassify as a new variable
         forest = landuseC == "forest", # create new column 
         weight = ifelse(case_, 1, 1e3) #add weight to background points
  )

# Each row is a single point with all of its associated attributes
# 'case_' = indicator variable is equal to TRUE for the used location, & FALSE for random locations
print(rsf_dat, n = 3, width = Inf)



# Explore sensitivity of HSF coefficients to the number of available points
# calculate a minimum convex polygon around Lupe's location
# then samples 10 random location for every used location within this polygon. these locationms are meant to capture what is assumed to be available to the animal (not the only method)
# other method = you can samoke from within an outer counter of a kernel density estimate applied to Lupe's location or to sample using a regular grid (refer to vignette("rsf") for more options in amt)


# How many available points do I need to sample? available points are used to estimate the integral in the denominator of the use-availability likelihood .-. evaluate how β^ changes as we increase the number of available locations from 1 available location per used location to 100 available locations per used location
n.frac <- c(1, 5, 10, 50, 100)
n.pts <- ceiling(nrow(dat) * n.frac)
n.rep <- 20

res1 <- tibble(
  n.pts = rep(n.pts, n.rep), 
  frac = rep(n.frac, n.rep), 
  res = map(
    n.pts, ~
      dat %>% random_points(n = .x) %>% 
      extract_covariates(landuse) %>% 
      extract_covariates(elevation) %>% 
      extract_covariates(popden) %>% 
      mutate(landuseC = reclass_landuse(landuse), 
             elevation = scale(elevation), 
             popden = scale(popden),
             w = ifelse(case_, 1, 5000)) %>% 
      glm(case_ ~ elevation + popden + landuseC, 
          weight = w, data = ., family = binomial()) %>% 
      tidy()))

res1 %>% unnest(cols = res) %>% 
  mutate(terHSF.Lupe1 = recode(term, "(Intercept)"="Intercept",
                               popden = "human population density", 
                               landuseCgrass = "landuse == grass", 
                               landuseCother = "landuse == other", 
                               landuseCshrub = "landuse == shrub", 
                               landuseCwet = "landuse == wet", 
  )) %>% 
  ggplot(aes(factor(frac), y = estimate)) +
  geom_boxplot() + facet_wrap(~ terHSF.Lupe1, scale  ="free") +
  geom_jitter(alpha = 0.2) + 
  labs(x = "Number of available points (multiplier of no. of used locations)", y = "Estimate") +
  theme_light()


# as intercept decrease -> we increase # of available points (as it is rougly proportional to the log difference between #s used and available points
# but the slope parameter estimates, on average do not change much once we include 10+ available points per used point
# .-. for this case, 10 available points per used point is enough for interpreting the slope coefficient
# using more available points reduces Monte Carlo error, so we will use 100 available points per used point to minimize Monte Carlo error

# Using a dataset that combines observed location with 100 available points per used location, the proportion of used and available location in each landuseC class can be examined

#' Use the largest sample size here for the rest of the paper
Lupe.dat <- dat %>% 
  random_points(n = max(n.pts)) %>% 
  extract_covariates(landuse) %>% 
  extract_covariates(elevation) %>% 
  extract_covariates(popden) %>% 
  mutate(landuseC = reclass_landuse(landuse), 
         elevation = scale(elevation), 
         popden = scale(popden))

Lupe.dat %>% 
  group_by(case_, landuseC) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), 
         label = paste0(round(prop * 100, 1), "%")) %>% 
  ggplot(aes(landuseC, prop, fill = case_, group=case_,label = label)) + 
  geom_col(position = position_dodge2()) +
  geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Land use class", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()

#_____________________________________________________________
# Fitting and Interpetting Parameters in a Habitat-Selection Function (HSF) 

#******add description


Lupe.dat$w <- ifelse(Lupe.dat$case_, 1, 5000)
HSF.Lupe1 <- glm(case_ ~ elevation + popden + landuseC, 
                 data = Lupe.dat, weight = w,
                 family = binomial(link = "logit"))

# Interpreting Habitat-Selection Parameters in Terms of Relative Intensities or Rates of Use
summary(HSF.Lupe1)

# Quantitative Predictors
exp(3 * coef(HSF.Lupe1)["elevation"] + 
      1.5 * coef(HSF.Lupe1)["popden"] + coef(HSF.Lupe1)["landuseCwet"]) /
  exp(2 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
        coef(HSF.Lupe1)["landuseCwet"])

exp(coef(HSF.Lupe1)["elevation"])

exp(11 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
      coef(HSF.Lupe1)["landuseCwet"]) /
  exp(10 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
        coef(HSF.Lupe1)["landuseCwet"])

exp(111 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
      coef(HSF.Lupe1)["landuseCwet"]) /
  exp(110 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
        coef(HSF.Lupe1)["landuseCwet"])

exp(-4 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
      coef(HSF.Lupe1)["landuseCwet"]) /
  exp(-5 * coef(HSF.Lupe1)["elevation"] + 1.5 * coef(HSF.Lupe1)["popden"] + 
        coef(HSF.Lupe1)["landuseCwet"])



# Categorical Predictors
exp(coef(HSF.Lupe1)["landuseCwet"])

Lupe.dat <- mutate(Lupe.dat, landuseC1 = fct_relevel(landuseC, "wet"))
levels(Lupe.dat$landuseC)

levels(Lupe.dat$landuseC1)       

HSF.Lupe2 <- glm(case_ ~ elevation + popden + landuseC1, 
                 data = Lupe.dat, weight = w, family = binomial(link = "logit"))
summary(HSF.Lupe2)



#................
# Adjusting for Differences in Habitat Availability

# Availability of forest and wet within Lupe's MCP
a.forest <- with(Lupe.dat[Lupe.dat$case_ == 0, ], sum(landuseC == "forest")) 
a.wet <- with(Lupe.dat[Lupe.dat$case_ == 0, ], sum(landuseC == "wet"))
# Multiply by the ratio of availabilities
exp(coef(HSF.Lupe1)["landuseCwet"])*a.wet/a.forest

# Or, comparing wet to forest (instead of forest to wet)
1/(exp(coef(HSF.Lupe1)["landuseCwet"])*a.wet/a.forest)


#............
# Adjusting using Integrated Spatial Intensities

# Filter to include only available locations, then integrate over landuse categories
available.locs <- filter(Lupe.dat, case_ == 0)
available.locs$wx <- exp(predict(HSF.Lupe1, available.locs))
with(available.locs, sum(wx[landuseC == "forest"]) / sum(wx[landuseC == "wet"]))

# Now, compare to the observed ratio of Lupe's use of forest and and wet habitat 
with(Lupe.dat[Lupe.dat$case_ == 1, ], 
     sum(landuseC == "forest") / sum(landuseC == "wet"))

# No adjustment
exp(coef(HSF.Lupe1)["landuseCwet"]) / exp(coef(HSF.Lupe1)["landuseCgrass"])

# Naive adjustment
a.grass <- with(Lupe.dat[Lupe.dat$case_ == 0, ], 
                sum(landuseC == "grass")) 
(exp(coef(HSF.Lupe1)["landuseCwet"]) / 
    exp(coef(HSF.Lupe1)["landuseCgrass"])) * a.wet / a.grass

# Integrated Spatial Intensities 
# Filter to include only available locations, then integrate over landuse categories
with(available.locs, sum(wx[landuseC == "wet"]) / sum(wx[landuseC == "grass"]))


# Now, compare to the observed ratio in the data set
with(Lupe.dat[Lupe.dat$case_==1, ], sum(landuseC == "wet") / 
       sum(landuseC == "grass"))



#______________________________________________
# Interactions

ggplot(Lupe.dat, aes(landuseC, elevation, fill=case_))+
  geom_boxplot() +    
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used"))+ 
  theme_light() 

HSF.Lupe3 <- glm(
  case_ ~ elevation + popden + landuseC + elevation:landuseC,
  data = Lupe.dat, 
  weight=w,
  family = binomial)
summary(HSF.Lupe3)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SSF ----



# SSF model transition or "steps" connecting sequential locations (Δt units apart) in geographical space
# i.e. SSF analyse animal movement focusing on paths or "steps" it takes between 2 continuous or sequential location over a set interval  (Δt)



# how to fit and interpret parameters and outputs for iSSA

# Integrated step-selection analyses steps:

# Section: Format and Generate Random Steps and Tentative Movement Parameters section
# 1. Estimate a tentative selection-free movement kernel,  using observed step-lengths and turn angles
# 2. Generate time-dependent available locations by simulating potential movements from the previously observed location

# Section: Fit iSSF
# 3. Estimate β (regression/parameter coefficients) using conditional logistic regression, with strata formed by combining time-dependent used and available locations.

# Section: Interpreting Movement Parameters 
#  4. Re-estimate the movement parameters in using regression coefficients associated with movement characteristics (e.g., step-length, log step-length, and cosine of the turn angle). This step adjusts the parameters in the movement model to account for the effect of habitat selection when estimating the movement kernel


# step 1-2: Format and Generate Random Steps and Tentative Movement Parameters section
# step 3: Fit iSSF section
# step 4: Interpreting Movement Parameters section 



# Section: Format and Generate Random Steps

# data preparation
ssf_dat <- dat %>% 
  track_resample(rate = minutes(2), tolerance = seconds(20)) %>%  #resample the track to get regular time steps
  steps_by_burst() %>% # convert from points to steps
  random_steps() %>%  #  generate random available steps
  extract_covariates(landuse, where = "both") %>%  # extract covariate values
  extract_covariates(elevation, where = "both") %>% 
  extract_covariates(popden, where = "both") %>% 
  mutate(elevation_start = scale(elevation_start), # scale and center our covariates and add the additional movement covariates
         elevation_end = scale(elevation_end),
         popden_start = scale(popden_start),
         popden_end = scale(popden_end),
         landuseC_start = reclass_landuse(landuse_start), 
         landuseC_end = reclass_landuse(landuse_end), 
         forest_start = landuseC_start == "forest",
         forest_end = landuseC_end == "forest",
         cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_)
  ) %>% 
  filter(!is.na(ta_))

print(ssf_dat, n = 3, width = Inf)

#..........................................
# Tenative movement parameters
#fit a tentative step-length and turn-angle distributions and then samples from these distributions to generate random steps
# view the parameters of the tentative step-length distribution
sl_distr(ssf_dat)

# access the parameters of the tentative turn-angle distribution 
ta_distr(ssf_dat)


#_______________________________________________
## Basic iSSF ----

# fit iSSF with  habitat covariates
# If habitat selection at the step scale depends on the covariates, and if, in the absence of habitat selection, step lengths and turn angles follow a constant distribution (i.e., the selection-free movement kernel does not depend on covariates, then this model can be fitted

m1 <- ssf_dat %>% 
  fit_issf(case_ ~ popden_end + elevation_end + landuseC_end + 
             sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

#............
# Interpreting Habitat-Selection Parameters
# explore coefficient of fitted model
# interpretation of the parameters is very similar to the RSF interpretation but at a local scale with habitat availability determined by the selection-free movement kernel
# Exponentiating habitat-selection parameters in a fitted step-selection model allows for comparing the relative rates of use between two locations. These locations differ by 1 unit in an explanatory variable, but are otherwise identical/equivalent. The locations should be equally accessible and have the same values for all other explanatory variables.
summary(m1)



# Calculating Relative Selection Strength (RSS) for Two Locations
# data.frame for s1
s1 <- data.frame(
  elevation_end = 3,
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 2,
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

lr1 <- log_rss(m1, x1 = s1, x2 = s2)
lr1$df

# Make a new data.frame for s1
s1 <- data.frame(
  elevation_end = seq(from = -2, to = 2, length.out = 200),
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 0, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr2 <- log_rss(m1, s1, s2)

# Plot using ggplot2
ggplot(lr2$df, aes(x = elevation_end_x1, y = log_rss)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()

# Plot using ggplot2
ggplot(lr2$df, aes(x = elevation_end_x1, y = exp(log_rss))) +
  geom_line(size = 1) +
  geom_hline(yintercept = exp(0), linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("RSS vs Mean Elevation") +
  theme_bw()



#......
# Bootstrap Confidence Intervals

# Calculate log-RSS (takes a while to run)
lr2_ci_boot <- log_rss(m1, s1, s2, ci = "boot", ci_level = 0.95, n_boot = 1000)

# Check the header of the data.frame
head(lr2_ci_boot$df)

ggplot(lr2_ci_boot$df, aes(x = elevation_end_x1, y = log_rss)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              linetype = "dashed", 
              color = "black", fill = "gray80", alpha = 0.5) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()



# Large-Sample Confidence Intervals
lr2_ci_se <- log_rss(m1, s1, s2, ci = "se", ci_level = 0.95)

# Check the header of the data.frame
head(lr2_ci_se$df)


# Add "type" label to both predictions data.frames
lr2_ci_boot$df$type <- "Bootstrap"
lr2_ci_se$df$type <- "SE"

# Combine two prediction data.frames
lr2_ci_both <- rbind(lr2_ci_boot$df, lr2_ci_se$df)

# Plot
ggplot(lr2_ci_both, aes(x = elevation_end_x1, y = log_rss, 
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
# Interpreting Movement Parameters

# Update Step-Length Distribution

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
ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  scale_color_manual(name = "Distribution", 
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()


# Update Turn-Angle Distribution
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

# For the updated distribution
plot_ta$updated <- circular::dvonmises(
  x = plot_ta$x, 
  mu = updated_ta$params$mu,
  kappa = updated_ta$params$kappa)

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



# Calculating RSS for Movements
like50 <- dgamma(50, 
                 shape = updated_sl$params$shape,
                 scale = updated_sl$params$scale)
like100 <- dgamma(100, 
                  shape = updated_sl$params$shape,
                  scale = updated_sl$params$scale)
like50/like100



#________________________________________________
# More Complex Models
# Non-Linear Relationships: Adding Quadratic Terms

m2 <- ssf_dat %>% 
  fit_issf(case_ ~ popden_end + elevation_end + I(elevation_end^2) + 
             landuseC_end + sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)



# data.frame for s1
s1 <- data.frame(
  elevation_end = 3, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 2, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s3
s3 <- data.frame(
  elevation_end = -1, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s4
s4 <- data.frame(
  elevation_end = -2, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet",  levels = levels(ssf_dat$landuseC_end)),
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
  elevation_end = seq(from = -2, to = 2, length.out = 100),
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# data.frame for s2
s2 <- data.frame(
  elevation_end = 0, # mean of elev, since we scaled and centered
  popden_end = 1.5,
  landuseC_end = factor("wet", levels = levels(ssf_dat$landuseC_end)),
  sl_ = 100,
  log_sl_ = log(100),
  cos_ta_ = 1)

# Calculate log-RSS
lr_m2 <- log_rss(m2, s1, s2)

# Plot using ggplot2
ggplot(lr_m2$df, aes(x = elevation_end_x1, y = log_rss)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  xlab("Elevation (SD)") +
  ylab("log-RSS vs Mean Elevation") +
  theme_bw()




# Movement and Habitat Interactions

# Interactions Between Movement Characteristics and Categorical Variables

m3 <- ssf_dat %>% 
  fit_issf(case_ ~ popden_end + elevation_end +  
             landuseC_end + 
             sl_ + log_sl_ + cos_ta_ + 
             landuseC_start:(sl_ + log_sl_ + cos_ta_) +
             strata(step_id_), model = TRUE)


summary(m3)




# Forest step-length distribution
forest_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"],
  beta_log_sl = m3$model$coefficients["log_sl_"])

# Grass step-length distribution
grass_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"] +
    m3$model$coefficients["sl_:landuseC_startgrass"],
  beta_log_sl = m3$model$coefficients["log_sl_"] +
    m3$model$coefficients["log_sl_:landuseC_startgrass"])

# Wet step-length distribution
wet_sl <- update_gamma(
  dist = m3$sl_,
  beta_sl = m3$model$coefficients["sl_"] +
    m3$model$coefficients["sl_:landuseC_startwet"],
  beta_log_sl = m3$model$coefficients["log_sl_"] + 
    m3$model$coefficients["log_sl_:landuseC_startwet"])



# Forest turn-angle distribution
forest_ta <- update_vonmises(
  dist = m3$ta_, beta_cos_ta = m3$model$coefficients["cos_ta_"])

# Grass turn-angle distribution
grass_ta <- update_vonmises(
  dist = m3$ta_, 
  beta_cos_ta = m3$model$coefficients["cos_ta_"] +
    m3$model$coefficients["cos_ta_:landuseC_startgrass"])

# Wet turn-angle distribution
wet_ta <- update_vonmises(
  dist = m3$ta_,
  beta_cos_ta = m3$model$coefficients["cos_ta_"] +
    m3$model$coefficients["cos_ta_:landuseC_startwet"])


# data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 400, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_sl$forest <- dgamma(x = plot_sl$x, 
                         shape = forest_sl$params$shape,
                         scale = forest_sl$params$scale)
# Grass
plot_sl$grass <- dgamma(x = plot_sl$x, 
                        shape = grass_sl$params$shape,
                        scale = grass_sl$params$scale)
# Wet
plot_sl$wet <- dgamma(x = plot_sl$x, 
                      shape = wet_sl$params$shape,
                      scale = wet_sl$params$scale)

# Pivot from wide to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)

# Plot
p1<-ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Land-use",
                     breaks = c("forest", "grass", "wet"),
                     values = c("forestgreen", "wheat", "blue")) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw()


#data.frame for plotting
plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -pi, to = pi, length.out = 100)

# y-axis is the probability density under the given gamma distribution
# Forest
plot_ta$forest <- circular::dvonmises(x = plot_ta$x, 
                                      kappa = forest_ta$params$kappa,
                                      mu = 0)
# Grass
plot_ta$grass <- circular::dvonmises(x = plot_ta$x, 
                                     kappa = grass_ta$params$kappa,
                                     mu = 0)
# Wet
plot_ta$wet <- circular::dvonmises(x = plot_ta$x, 
                                   kappa = wet_ta$params$kappa,
                                   mu = 0)

# Pivot from wide to long data
plot_ta <- plot_ta %>% 
  pivot_longer(cols = -x)

# Plot
p2 <- ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  scale_color_manual(name = "Land-use",
                     breaks = c("forest", "grass", "wet"),
                     values = c("forestgreen", "wheat", "blue")) +
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
  fit_issf(case_ ~ popden_end + landuseC_end +   
             elevation_end + 
             sl_ + log_sl_ + cos_ta_ + 
             elevation_start:(sl_ + log_sl_ + cos_ta_) + 
             strata(step_id_), model = TRUE)

summary(m4)


# Low elevation step-length distribution
low_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    -2 * m4$model$coefficients["sl_:elevation_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    -2 * m4$model$coefficients["log_sl_:elevation_start"])

# Medium elevation step-length distribution
med_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    0 * m4$model$coefficients["sl_:elevation_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    0 * m4$model$coefficients["log_sl_:elevation_start"])

# Wet step-length distribution
hi_sl <- update_gamma(
  dist = m4$sl_,
  beta_sl = m4$model$coefficients["sl_"] +
    2 * m4$model$coefficients["sl_:elevation_start"],
  beta_log_sl = m4$model$coefficients["log_sl_"] +
    2 * m4$model$coefficients["log_sl_:elevation_start"])



# low turn-angle distribution
low_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    -2 * m4$model$coefficients["cos_ta_:elevation_start"])

# med turn-angle distribution
med_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    0 * m4$model$coefficients["cos_ta_:elevation_start"])

# hi turn-angle distribution
hi_ta <- update_vonmises(
  dist = m4$ta_,
  beta_cos_ta = m4$model$coefficients["cos_ta_"] +
    2 * m4$model$coefficients["cos_ta_:elevation_start"])


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




