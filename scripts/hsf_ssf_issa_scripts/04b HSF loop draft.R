
# HSF loop for fire goats
library(amt)
library(terra)
library(dplyr)
library(forcats) # fct_collapse() collapse factor levels into manually defined groups
library(ggplot2)
library(broom) #tidy()
library(tictoc)
library(beepr)
library(gridExtra)


# Section 2: Data prep ----

# load spatial covariate data (packedspatraster or spatraster object)
elev_25m = rast('data/rasters/elev_25m.tif')
dist_escape_25m = rast('data/rasters/dist_escape_25m.tif')
# reproject to lat/long crs
elev_25m <- project(elev_25m, "epsg:4326")
dist_escape_25m <-  project(dist_escape_25m, "epsg:4326")

# collar data
load("data/collar_data/collar_data_20241123.rda")
# identify the goats that were tracked during the wildfire
goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
# subset to fire goats
fire_goats <- collar_data[collar_data$goat_name %in% goats,] # 43941 obs
# format goat identifier name based on year
fire_goats$individual.local.identifier <- paste(fire_goats$goat_name, fire_goats$year, sep = "_")
# identify each goat and year
goat_identifier <- unique(fire_goats$individual.local.identifier)


# Define the Crater Creek wildfire date range
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
fisher <- fire_goats[fire_goats$month_day >= fire_start & fire_goats$month_day <= fire_end, ] #10376 obs


# convert to track_xyz object (amt object)
fisher <- make_track(fisher, 
                     .x = longitude,             # x-coordinates (projected)
                     .y = latitude,             # y-coordinates (projected)
                     .t = timestamp,     # timestamp column in POSIXct format
                     crs = 4326,        # Assuming UTM Zone 10N (adjust if necessary)
                     all_cols = TRUE     # Retain all other columns
)


#.....................................................
#Outputs:
# HSF1_results = list of basic HSF models
# HSF3_results = list of HSF models with interaction terms
# relative_use_results = list of relative use values
# rsf_data_list = list of rsf data

#create folder for plots
dir.create(paste0("./figures/hsf/fire_goats", "/sensitivity_20241228"), recursive = TRUE, showWarnings = TRUE)
dir.create(paste0("./figures/hsf/fire_goats", "/density_20241228"), recursive = TRUE, showWarnings = TRUE)
dir.create(paste0("./figures/hsf/fire_goats", "/interaction_20241228"), recursive = TRUE, showWarnings = TRUE)

# Set results directory for plots
sensitivity_dir = paste0(getwd(), "/figures/hsf/fire_goats", '/sensitivity_20241228')
density_dir = paste0(getwd(), "/figures/hsf/fire_goats", '/density_20241228')
interaction_dir = paste0(getwd(), "/figures/hsf/fire_goats", '/interaction_20241228')



#.....................................................
# HSF loop ----


START <- Sys.time()
tic()
# Initialize separate lists for each type of output
HSF1_models <- list()
HSF3_models <- list()
relative_use_results <- list()
hsf_rsf_list <- list()


for (i in goat_identifier) {
  message("Currently on ", i)
  # subset to a single goat
  dat <- fisher %>%
    filter(individual.local.identifier == i) %>%
    arrange(t_) # Reorder in ascending order
  
  # inspect sampling rate of data
  print(summarize_sampling_rate(dat))
  
  ## Section 2b: Generate random available points ----
  # workflow:
  # 1. generate available points (random_points()), and
  # 2. attach the environmental covariates to both the observed and available points (extract_covariates())
  # 3. scale and center our covariates and add a weight to the background points.
  rsf_dat <- dat %>% 
    random_points() %>% #generate random points
    extract_covariates(elev_25m) %>% # extract covariate values 
    extract_covariates(dist_escape_25m) %>% 
    mutate(elev_25m = scale(elev_25m)[, 1], # scales covariate to standardise them (ie. center them)
           dist_escape_25m = scale(dist_escape_25m)[, 1],
           weight = ifelse(case_, 1, 1e3) #add weight to background points
    )
  
  # Store hsf data in list
  hsf_rsf_list[[i]] <- rsf_dat
  
  
  #.....................................................................................  
  # Section 2c: Availability Domain and Number of Available Points ----
  # Explore sensitivity of HSF coefficients to the number of available points
  n.frac <- c(1, 5, 10, 50, 100)
  n.pts <- ceiling(nrow(dat) * n.frac)
  n.rep <- 20
  
  message("Currently on section 2c")
  
  # takes a few minutes
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
        tidy()  #convert to tiny format
    )
  )
  
  # visualise sensitivity
  plot_sensitivity <-
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
    # ggtitle(paste0(goat)) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), #removes horizontal gridlines
      panel.grid.minor = element_blank()) #removes vertical gridlines
  
  #save sensitivity plots
  sensitivity_path <- file.path(sensitivity_dir, paste0(i, ".png"))
  ggsave(plot_sensitivity, filename = sensitivity_path,
         width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")
  
  
  
  
  #.........................................................
  ## Proportion of used and available location ----
  # Use the largest sample size for HSF model
  Lupe.dat <- dat %>%
    random_points(n = max(n.pts)) %>%
    extract_covariates(elev_25m) %>%
    extract_covariates(dist_escape_25m) %>%
    mutate(
      elev_25m = scale(elev_25m),
      dist_escape_25m = scale(dist_escape_25m))
  
  # visualise, combine observed with available points -> proportion of used and available location
  # due to the nature of elevation data being continuous and elevation is not being binned, a density estimation approach was used (specifically kernel density estimation)
  
  
  library(tidyr)
  # Reshape data to long format to be able to facet wrap them to have axis same scale
  Lupe.dat.long <- Lupe.dat %>%
    pivot_longer(
      cols = c(elev_25m, dist_escape_25m), 
      names_to = "Variable", 
      values_to = "Value"
    )
  
  
  plot_density <-
    ggplot(Lupe.dat.long, aes(x = Value, fill = case_, group = case_)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~Variable, scales = "fixed", # 
               labeller = as_labeller(c(elev_25m = "Elevation",
                                        dist_escape_25m = "Distance to escape terrain"
               ))) +
    labs(x = "", y = "Density", fill = "Case") +
    scale_fill_brewer(palette = "Paired", name = "Case",
                      breaks = c("FALSE", "TRUE"), labels = c("Available", "Used")) +
    scale_x_continuous(limits = c(-max(abs(Lupe.dat.long$Value), na.rm = TRUE), 
                                  max(abs(Lupe.dat.long$Value), na.rm = TRUE))) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  
  #save density plots
  density_path <- file.path(density_dir, paste0(i, ".png"))
  ggsave(plot_density, filename = density_path,
         width = 6.86, height = 3.23, units = "in", dpi = 600, bg = "transparent")
  
  #_____________________________________________________________
  # Section 3: Fitting and Interpetting Parameters in a Habitat-Selection Function (HSF) ----
  
  message("Currently on fitting HSF")
  
  # Fit the HSF models
  Lupe.dat$w <- ifelse(Lupe.dat$case_, 1, 5000)
  
  # 1st model
  HSF_m1 <- glm(case_ ~ elev_25m + dist_escape_25m, 
                data = Lupe.dat, 
                weight = w, 
                family = binomial(link = "logit"))
  # store models
  HSF1_models[[i]] <- HSF_m1
  
  # inspect model summary
    summary(HSF_m1)
  
  
  #..................................................
  # Section 3d: Adjusting for Differences in Habitat Availability ----
  
  # Availability of elevation and distance to escape terrain
  a.elevation <- with(Lupe.dat[Lupe.dat$case_ == 0, ], mean(elev_25m))  # Average elevation within MCP
  a.distance <- with(Lupe.dat[Lupe.dat$case_ == 0, ], mean(dist_escape_25m))  # Average distance to escape within MCP
  
  ## Calculate relative use, adjusting for availability ----
  relative_use <- exp(coef(HSF_m1)["elev_25m"]) * a.elevation / a.distance
  # 'exp(coef(HSF.Lupe1)["elev_25m"])' indicates the effect of elevation on the relative use of habitats
  # a negative value = means that as elevation increases, the likelihood of habitat use decreases, relative to availability???????
  
  # store relative use
  relative_use_results[[i]] <- relative_use
  
  
  # #............
  # 3e. Adjusting using Integrated Spatial Intensities????
  
  
  #______________________________________________
  # Section 4: Interactions ----
  
  ## 3rd model: Adding interaction term ----
  HSF_m3 <- glm(case_ ~ elev_25m + dist_escape_25m + elev_25m:dist_escape_25m, 
                data = Lupe.dat, 
                weight = w, 
                family = binomial(link = "logit"))
  # store models
  HSF3_models[[i]] <- HSF_m3
  
  # inspect model summary
  summary(HSF_m3)
  
  
  # visualisation
  plot_interaction <- 
    ggplot(Lupe.dat, aes(elev_25m, dist_escape_25m, fill = case_)) +
    geom_boxplot() +
    scale_fill_brewer(palette = "Paired", name = "case_", 
                      breaks = c("FALSE", "TRUE"), labels = c("Available", "Used")) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), #removes horizontal gridlines
      panel.grid.minor = element_blank()) #removes vertical gridlines
  
  #save density plots
  interaction_path <- file.path(interaction_dir, paste0(i, ".png"))
  ggsave(plot_interaction, filename = interaction_path,
         width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")
  
  
  
}

# save outputs ----
dir.create("./data/hsf/", showWarnings = TRUE)
save(HSF1_models , file = "./data/hsf/fire_goat_hsf_basic_model_20241228.rda")
save(HSF3_models , file = "./data/hsf/fire_goat_hsf_interaction_model_20241228.rda")
save(relative_use_results , file = "./data/hsf/fire_goat_hsf_relative_use_results_20241228.rda")
save(hsf_rsf_list , file = "./data/hsf/fire_goat_hsf_rsf_20241228.rda")





toc() # 20 mins with fire goats only
beepr::beep(3)

#~46.33 min
#~51.3 min

# Warning messages:
#   1: glm.fit: fitted probabilities numerically 0 or 1 occurred


# extract m1 models summary
# summary(HSF1_models[[1]])
