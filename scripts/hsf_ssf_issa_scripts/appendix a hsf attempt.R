



# HSF ----

# based on the framework/how to guide Appendix A https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13441

library(amt)
library(terra)
library(dplyr)
library(forcats) # fct_collapse() collapse factor levels into manually defined groups
library(ggplot2)
library(broom) #tidy()


# Section 1: Overview ----

# how to fit and interpret habitat-selection functions (HSF) using 
# logistic regression, 
# inhomogeneous Poisson Point Process (IPP) models and 
# weighted distribution theory

# 1a) inhomogeneous Poisson Point Process (IPP) models

# 1b) weighted distribution theory


# Section 2: Data prep ----

# collar 
load("data/collar_data/collar_data_20241123.rda")

# subset to fire goats
fire_goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
fisher <- collar_data[collar_data$goat_name %in% fire_goats,]

# load spatial covariate data (packedspatraster or spatraster object)
elev_25m = rast('data/rasters/elev_25m.tif')
dist_escape_25m = rast('data/rasters/dist_escape_25m.tif')
# reproject to lat/long crs
elev_25m <- project(elev_25m, "epsg:4326")
dist_escape_25m <-  project(dist_escape_25m, "epsg:4326")

# check data type of covariates
# datatype(elev_25m) #FLT4S: 32-bit float (single precision) .-. numerical
#inspect values and check range of value
range(values(elev_25m), na.rm = TRUE)

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
  filter(goat_name == "kid_rock") %>% # subset to 1 animal
  arrange(t_) ## reorder in ascending order
# # using 1 animal
# dat <- fisher[fisher$goat_name == "kid_rock",]
# # reorder in ascending order
# dat <- dat[order(dat$t_),]


# # group landcover types from numerical values into categorical groups
# reclass_landuse <- function(x) {
#   fct_collapse(factor(x),
#                forest = c("30","40","50","60", "70","80", "90","100"),
#                grass = c("120", "140"),
#                wet = c("160"))
# }

# inspect sampling rate of data
summarize_sampling_rate(dat) 



#.........................................................
## Section 2b: Generate random available points ----
# using data for 1 individual
# workflow:
# 1. generate available points (random_points()), and
# 2. attach the environmental covariates to both the observed and available points (extract_covariates())
# 3. scale and center our covariates and add a weight to the background points.

# data preparation
rsf_dat <- dat %>% 
  random_points() %>% #generate random points
  extract_covariates(elev_25m) %>% # extract covariate values 
  extract_covariates(dist_escape_25m) %>% 
  mutate(elev_25m = scale(elev_25m)[, 1], # scales covariate to standardise them (ie. center them)
         dist_escape_25m = scale(dist_escape_25m)[, 1],
         weight = ifelse(case_, 1, 1e3) #add weight to background points
  )


# Each row is a single point with all of its associated attributes
# 'case_' = indicator variable is equal to TRUE for the used location, & FALSE for random locations
print(rsf_dat, n = 3, width = Inf)

#.....................................................................................  
## Section 2c: Availability Domain and Number of Available Points ----
#workflow:
# calculate a minimum convex polygon around Lupe's location
# then samples 10 random location for every used location within this polygon. these locations are meant to capture what is assumed to be available to the animal (not the only method)
# other method = you can sample from within an outer counter of a kernel density estimate applied to Lupe's location or to sample using a regular grid (refer to vignette("rsf") for more options in amt)

# How many available points do I need to sample? available points are used to estimate the integral in the denominator of the use-availability likelihood .-. evaluate how β^ changes as we increase the number of available locations from 1 available location per used location to 100 available locations per used location


# Explore sensitivity of HSF coefficients to the number of available points
n.frac <- c(1, 5, 10, 50, 100)
n.pts <- ceiling(nrow(dat) * n.frac)
n.rep <- 20

#*************I THINK THIS IS WHAT IS GOING ON WITH THIS CHUNK OF CODE, CONFIRM?
# calculate min. convex polygon around animal's location via random_points(), then
# samples x random location for every used location within this polygon
# .-. these locations are meant to capture what is assumed to be available to the animal
res1 <- tibble(
  n.pts = rep(n.pts, n.rep), 
  frac = rep(n.frac, n.rep), 
  res = map(
    n.pts, ~
      dat %>% random_points(n = .x) %>% 
      extract_covariates(elev_25m) %>% # extract covariate values 
      extract_covariates(dist_escape_25m) %>% 
      mutate(elev_25m = scale(elev_25m), # scale covariates
             dist_escape_25m = scale(dist_escape_25m),
             w = ifelse(case_, 1, 5000)) %>% # add weights
      # fit glm to predict case_ using scaled covariates
            glm(case_ ~ elev_25m + dist_escape_25m, 
          weight = w, data = ., family = binomial()) %>% 
      tidy() #convert to tiny format
    )
  ) 

# visualise sensitivity
res1 %>% 
  unnest(cols = res) %>% #unnest the nested column i.e. list column to get at the data
  # rename
  mutate(terHSF.Lupe1 = recode(term, "(Intercept)"="Intercept",
                               elev_25m = "elevation", 
                               dist_escape_25m = "distance to escape terrain")) %>% 
  # plot
  ggplot(aes(factor(frac), y = estimate)) +
  geom_boxplot() + facet_wrap(~ terHSF.Lupe1, scale  ="free") + # distribution of estimates
  geom_jitter(alpha = 0.2) + 
  labs(x = "Number of available points (multiplier of no. of used locations)", y = "Estimate") +
  theme_light()


# as intercept decrease -> we increase # of available points (as it is roughly proportional to the log difference between #s used and available points
# but the slope parameter estimates, on average do not change much once we include 10+ available points per used point
# .-. for this case, 10 available points per used point is enough for interpreting the slope coefficient
# using more available points reduces Monte Carlo error, so we will use 100 available points per used point to minimize Monte Carlo error

#...........................
# proportion of used and available locations in for each covariate
# Look at proportion of used and available locations in each covariate class using a data set created by combining our observed locations with 100 available pts per used location i.e. using a dataset that combines observed location with 100 available points per used location, the proportion of used and available location in each covariate class can be examined

# Use the largest sample size here for the rest of the paper
Lupe.dat <- dat %>%
  random_points(n = max(n.pts)) %>%
  extract_covariates(elev_25m) %>% # extract covariate values
  extract_covariates(dist_escape_25m) %>%
  mutate(elev_25m = scale(elev_25m),
         dist_escape_25m = scale(dist_escape_25m))


# #***************SHOULD I BE USING [,1]????
# Lupe.dat <- dat %>% 
#   random_points(n = max(n.pts)) %>% 
#   extract_covariates(el) %>% # extract covariate values 
#   extract_covariates(esc) %>% 
#   mutate(el_scaled = scale(elev_25m)[, 1], # Scaled elevation
#          esc_scaled = scale(dist_escape_25m)[, 1])
# 
#

# visualise the Combine observed with available points
#*****************************issue with plotting because elevation or dis to esc isnt categorical but numerical
Lupe.dat %>%
  group_by(case_, elev_25m) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n),
         label = paste0(round(prop * 100, 1), "%")) %>%
  ggplot(aes(elev_25m, prop, fill = case_, group=case_,label = label)) +
  geom_col(position = position_dodge2()) +
  # geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Elevation", y = "Proportion", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_",
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()
# beep(4)

# visualise, combine observed with available points -> proportion of used and available location
# elevation is numerical, proportion wont work, therefore, it needs to be changed
# .-. response changed to count and changed to geom_line
Lupe.dat %>% 
  group_by(elev_25m, case_) %>% 
  summarize(n = n(), .groups = 'drop') %>% 
  ggplot(aes(x = elev_25m, y = n, color = case_, group = case_)) +
  geom_line(size = 1) +
  labs(x = "Elevation", y = "Count", color = "case_") +
  scale_color_brewer(palette = "Paired", name = "case_", 
                     breaks = c("FALSE", "TRUE"), labels = c("Available", "Used")) +
  theme_light()

Lupe.dat %>% 
  group_by(dist_escape_25m, case_) %>% 
  summarize(n = n(), .groups = 'drop') %>% 
  ggplot(aes(x = dist_escape_25m, y = n, color = case_, group = case_)) +
  geom_line(size = 1) +
  labs(x = "Distance to escape terrain", y = "Count", color = "case_") +
  scale_color_brewer(palette = "Paired", name = "case_", 
                     breaks = c("FALSE", "TRUE"), labels = c("Available", "Used")) +
  theme_light()


# due to the nature of elevation data being continuous and elevation data not being binned, a density estimation approach was used (specifically kernel density estimation)

Lupe.dat %>%
  ggplot(aes(elev_25m, prop, fill = case_, group=case_,label = label)) +
geom_density(alpha = 0.4) +
  # geom_text(size = 4, vjust = -0.25, position = position_dodge(width = 1)) +
  labs(x = "Elevation", y = "Density", fill = "case_")+
  scale_fill_brewer(palette = "Paired", name="case_",
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used")) +
  theme_light()




#_____________________________________________________________
# Section 3: Fitting and Interpetting Parameters in a Habitat-Selection Function (HSF) ----

#assigning "inifinte weights" to available points ensure that logistic reress estimators of slope coeffiicnets converge to the of the IPP (inhomogeneous poisson point process) model
#assign a large weight (1000+) to each available location and weight = 1 to all observed locations (larger values can be used to verify that results are robust to this choice) when fitting regression models to use-available data
# assign weight then fit weighted logistic regression models with same habitat covariates.
# Note: they used same model structure when evaluating the number of available points needed to accurately estimate the integral in the denominator of the likelihood function (see Availability Domain and Number of Available Points).

Lupe.dat$w <- ifelse(Lupe.dat$case_, 1, 5000) # used to assign weights
# fit model
HSF.Lupe1 <- glm(case_ ~ elev_25m + dist_escape_25m, 
                 data = Lupe.dat, weight = w,
                 family = binomial(link = "logit"))


#......................................................................
## Section 3a: Interpreting Habitat-Selection Parameters in Terms of Relative Intensities or Rates of Use ----


#log relative intensity and log relative selective strength = same thing
#calculations of relative intensity

summary(HSF.Lupe1)
# we can exponentiate habitat-selection parameters to compare the relative intensity of rate of use of the two locations that differ by 1 unit of the explanatory variable but otherwise equivalent -> i.e. they should be equally accessible and have identical values for all other explanatory variables

#******What the fuck does that even mean?! Not very helpful for those who are using this as a how to guide who don't know how to!

#summary output:
# The coefficient for elev_25m is 1.581 × 10^−3
# The coefficient for dist_escape_25m is −5.416 × 10^−3
# .-. These coefficients indicate how a 1-unit change in elev_25m or dist_escape_25m translates into a change in the log-odds of the response variable (case_) for this logistic regression.


# Location s1: elev_25m = 3, dist_escape_25m = 3
# Location s2: elev_25m = 2, dist_escape_25m = 2


#....................................................
## Section 3b: Quantitative Predictors ----

# Only one predictor changes and it differs by 1 unit, since they were scaled, then it implies that the 2 observation differ by 1 sd in the original elevation unit
exp(3 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"]) /
  exp(2 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"])

# exp(coef(HSF.Lupe1)["elev_25m"])
# 
# exp(11 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"]) /
#   exp(10 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"])
# 
# exp(111 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"]) /
#   exp(110 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"])
# 
# exp(-4 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"]) /
#   exp(-5 * coef(HSF.Lupe1)["elev_25m"] + 1.5 * coef(HSF.Lupe1)["dist_escape_25m"])

exp(3 * coef(HSF.Lupe1)["dist_escape_25m"] + 1.5 * coef(HSF.Lupe1)["elev_25m"]) /
  exp(2 * coef(HSF.Lupe1)["dist_escape_25m"] + 1.5 * coef(HSF.Lupe1)["elev_25m"])

exp(1.5 * coef(HSF.Lupe1)["elev_25m"] + 3 * coef(HSF.Lupe1)["dist_escape_25m"]) /
  exp(1.5 * coef(HSF.Lupe1)["elev_25m"] + 2 * coef(HSF.Lupe1)["dist_escape_25m"])


#..................................................
## Section 3c: Categorical Predictors (does not apply? because theyre both quantitative values? i.e. numerical and not categorical?) ----
# exp(coef(HSF.Lupe1)["landuseCwet"])
# 
# Lupe.dat <- mutate(Lupe.dat, landuseC1 = fct_relevel(landuseC, "wet"))
# levels(Lupe.dat$landuseC)
# 
# levels(Lupe.dat$landuseC1)       
# 
# ## this cannot be done because fct_relevel() is for categorical
# HSF.Lupe2 <- glm(case_ ~ elevation + popden + landuseC1, 
#                  data = Lupe.dat, weight = w, family = binomial(link = "logit"))
# summary(HSF.Lupe2)



#..................................................
## Section 3d:  Adjusting for Differences in Habitat Availability ----

# these are categorical

# # Availability of forest and wet within Lupe's MCP
# a.forest <- with(Lupe.dat[Lupe.dat$case_ == 0, ], sum(landuseC == "forest")) 
# a.wet <- with(Lupe.dat[Lupe.dat$case_ == 0, ], sum(landuseC == "wet"))
# # Multiply by the ratio of availabilities
# exp(coef(HSF.Lupe1)["landuseCwet"])*a.wet/a.forest
# 
# # Or, comparing wet to forest (instead of forest to wet)
# 1/(exp(coef(HSF.Lupe1)["landuseCwet"])*a.wet/a.forest)



# modifiying for numerical values
# Availability of elevation and distance to escape terrain
a.elevation <- with(Lupe.dat[Lupe.dat$case_ == 0, ], mean(elev_25m))  # Average elevation within MCP
a.distance <- with(Lupe.dat[Lupe.dat$case_ == 0, ], mean(dist_escape_25m))  # Average distance to escape within MCP

# Calculate relative use, adjusting for availability
exp(coef(HSF.Lupe1)["elev_25m"])*a.elevation / a.distance
# -0.9800811 
# 'exp(coef(HSF.Lupe1)["elev_25m"])' indicates the effect of elevation on the relative use of habitats
# a negative value = means that as elevation increases, the likelihood of habitat use decreases, relative to availability.


# Or, comparing dist to esc to elevation (instead of elevation to dist to esc)
1/(exp(coef(HSF.Lupe1)["elev_25m"])*a.elevation/a.distance)
#-1.020324 

1/0.9800811 # 1.020324

# #............
# # Adjusting using Integrated Spatial Intensities
# 
# # Filter to include only available locations, then integrate over landuse categories
# available.locs <- filter(Lupe.dat, case_ == 0)
# available.locs$wx <- exp(predict(HSF.Lupe1, available.locs))
# with(available.locs, sum(wx[landuseC == "forest"]) / sum(wx[landuseC == "wet"]))
# 
# # Now, compare to the observed ratio of Lupe's use of forest and and wet habitat 
# with(Lupe.dat[Lupe.dat$case_ == 1, ], 
#      sum(landuseC == "forest") / sum(landuseC == "wet"))
# 
# # No adjustment
# exp(coef(HSF.Lupe1)["landuseCwet"]) / exp(coef(HSF.Lupe1)["landuseCgrass"])
# 
# # Naive adjustment
# a.grass <- with(Lupe.dat[Lupe.dat$case_ == 0, ], 
#                 sum(landuseC == "grass")) 
# (exp(coef(HSF.Lupe1)["landuseCwet"]) / 
#     exp(coef(HSF.Lupe1)["landuseCgrass"])) * a.wet / a.grass
# 
# # Integrated Spatial Intensities 
# # Filter to include only available locations, then integrate over landuse categories
# with(available.locs, sum(wx[landuseC == "wet"]) / sum(wx[landuseC == "grass"]))
# 
# 
# # Now, compare to the observed ratio in the data set
# with(Lupe.dat[Lupe.dat$case_==1, ], sum(landuseC == "wet") / 
#        sum(landuseC == "grass"))



#______________________________________________
# Section 4: Interactions ----

ggplot(Lupe.dat, aes(elev_25m, dist_escape_25m, fill=case_))+
  geom_boxplot() +    
  scale_fill_brewer(palette = "Paired", name="case_", 
                    breaks=c("FALSE", "TRUE"), labels=c("Available", "Used"))+ 
  theme_light() 

HSF.Lupe3 <- glm(
  case_ ~ elev_25m + dist_escape_25m + elev_25m:dist_escape_25m,
  data = Lupe.dat, 
  weight=w,
  family = binomial)
summary(HSF.Lupe3)






