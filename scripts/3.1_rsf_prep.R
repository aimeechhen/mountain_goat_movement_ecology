# Load in any of the necessary packages
library(ggplot2)
library(metafor)
library(scico)
library(mgcv)
library(tidyterra)
library(ggspatial)
library(ctmm)
library(raster)
library(tictoc)
library(beepr)
library(crayon)
library(terra)
library(sf)
library(gratia)
options(scipen = 999)

# rsf = relative probability of use of a resource (i.e., habitat covariates/parameters) vs the availability of that resource in the environment/habitat

# https://github.com/ctmm-initiative/ctmmlearn/blob/main/ctmm_rsf.R


#...........................................................
# Import data ----
#...........................................................

load("./data/goat/goat_data_no_outliers.rda")
load("./results/goat_akdes.rda") # needed for weights

# subset to data within the window timeframe
goat_data <- goat_data[goat_data$within_window == 1,] # 13071
# make sure its in the right order
goat_data <- goat_data[order(goat_data$fix_id), ]
# naming as required by ctmm
goat_data$timestamp <- goat_data$timestamp_utc
goat_data$individual.local.identifier <- goat_data$id_year

# convert to ctmm object
tel_data <- as.telemetry(goat_data, mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id"))

#Reproject to preserve north = up
ctmm::projection(tel_data) <- median(tel_data)


# import spatial covariate rasters 
source("./scripts/data_import_environmental.R")

# Reproject to match the projection of the tracking data
elev_scaled <- project(elev_scaled, tel_data[[1]]@info$projection, method = "near")
log_dist_to_slope50 <- project(log_dist_to_slope50, tel_data[[1]]@info$projection, method = "near")
# NOTE: BEST PRACTICE, SHOULD REPROJECT THE GPS POINTS TO THE RASTER AND NOT THE RASTER TO THE GPS POINT (point are just moving around vs changing the entire landscape of the raster and you get "ghosting crinkles" every time you reproject these kinds of rasters)




#...................................................................

# First build dataset of observed locations
rsf_df <- list()
# rsf_list <- list()

tictoc::tic(msg = "observed locations")
for(i in 1:length(tel_data)){
  message("Currently building df for: ", tel_data[[i]]@info$identity)
  #First build a dataframe with coordinates and individual ID 
  rsf_df_i <- data.frame(id_year = tel_data[[i]]@info$identity,
                         x = tel_data[[i]]$x,
                         y = tel_data[[i]]$y)
  
  #Then get habitat information
  cilla_sf <- as.sf(tel_data[[i]])
  # rsf_df_i$elev_scaled <- extract(elev_scaled, cilla_sf)[,"elev_scaled_25m"] # can use [,2] instead of putting in the column name
  rsf_df_i$elev_scaled <- terra::extract(elev_scaled, cilla_sf)[,2] # using [,2] to accommodate other resolutions
  rsf_df_i$log_dist_to_slope50 <- terra::extract(log_dist_to_slope50, cilla_sf)[,2]
  rsf_df_i$weight <- AKDES[[i]]$weights
  rsf_df_i$detection <- 1
  
  #Then build an individual specific dataset of quadrature points
  quadrature_i <- terra::as.data.frame(elev_scaled, xy = TRUE)[,c("x","y")]
  quadrature_i$elev_scaled <- terra::extract(elev_scaled, quadrature_i[,c("x","y")])[,2] 
  # quadrature_i$dist_to_slope50 <- extract(dist_to_slope50, quadrature_i[,c("x","y")])[,2]
  quadrature_i$log_dist_to_slope50 <- terra::extract(log_dist_to_slope50, quadrature_i[,c("x","y")])[,2]
  quadrature_i$id_year <- tel_data[[i]]@info$identity
  quadrature_i$weight <- 1
  quadrature_i$detection <- 0
  quadrature_i <- quadrature_i[,names(rsf_df_i)]
  
  #Combine and store in main list
  rsf_df_i <- rbind(rsf_df_i, quadrature_i)
  rsf_df[[i]] <- rsf_df_i
}

tictoc::toc() #1.2 min
beepr::beep(3)


#Some final data carpentry
rsf_data <- do.call(rbind, rsf_df) #33,330,201
rsf_data$id_year <- as.factor(rsf_data$id_year)
# extract goat ID
rsf_data$ID <- as.factor(sub("_.*", "", rsf_data$id_year)) 
# extract year
rsf_data$year = as.factor(sub(".*_", "", rsf_data$id_year))
rsf_data <- na.omit(rsf_data)
rsf_data <- dplyr::relocate(rsf_data, c("ID", "year", "id_year", "x", "y"), .before = elev_scaled)

# save(rsf_data, file = "./data/rsf/rsf_data.rda") # raster projected to gps points (not points projected to raster)
load("./data/rsf/rsf_data.rda")



rm(quadrature_i, rsf_df_i, cilla_sf, rsf_df, AKDES, elev, dist_to_fire, dist_to_slope50, i)


test <- rsf_data[rsf_data$detection == 1,]

# where in parameter space do the data occur?
plot(x = test$elev_scaled, y = test$log_dist_to_slope50)






# 
# 
# #............................................................
# 
# # GO TO SEPARATE INDIVIDUAL SCRIPTS NOW
# 
# 
# load("./data/rsf/rsf_data2.rda") # 2 = projected to raster
# 
# # load(data_path)
# 
# # 3 rsf models to answer these:
# # wanting to know their overall resource selection then fit all data
# 
# # wanting to know what their resource selection was like during the fire then fit only data during the fire
# # rsf_data <- rsf_data[rsf_data$year == 2023, ] # 5,713,669
# # rsf_data$id_year <- droplevels(rsf_data$id_year)
# # wanting to know what their resource selection when there isnt a fire then fit data that are non fire
# # rsf_data <- rsf_data[rsf_data$year != 2023, ] # 27,616,532
# # rsf_data$id_year <- droplevels(rsf_data$id_year)
# # wanting to know how their resource selection changed (if it did) then get the difference (via mapping)
# 
# # what to do when not enough unique values (i.e., knots) are available in the raster?
# # when running those rsfs with different scale of rasters, everything is supposed to be the same except for the raster resolution. what if the raster doesnt have enough unique values for the number of knots that were assigned? e.g., the rsf gam k = 5 but at 50m resolution, there are only 4
# 
# 
# 
# # wanting to know what their resource selection was like during the fire then fit only data during the fire
# rsf_data_fire <- rsf_data[rsf_data$year == 2023, ] # 5,713,669
# rsf_data_fire$id_year <- droplevels(rsf_data_fire$id_year)
# 
# #Fit the RSF
# tictoc::tic()
# rsf_fire <-
#   bam(detection ~
#         s(ID, bs = 're') + # ID = goat_id
#         s(elev_scaled, k = 5) + # not using 'by = year', because the affect of x isnt different each year
#         s(log_dist_to_slope50, k = 5) +
#         ti(log_dist_to_slope50, elev_scaled) + # interaction between two continuous variables therefore ti() is used instead of smooth s(), if the variables were continuous and categorical, then s() with bs = "sz" can be used
#       s(x, by = ID) + s(y, by = ID) + s(I((x^2 + y^2)/2), by = ID), # spatial smooth, random effect of ID
#       family = poisson(link = 'log'), 
#       data = rsf_data_fire,
#       weights = weight,
#       method = 'fREML',
#       discrete = TRUE,
#       control = gam.control(nthreads = 14, trace = TRUE))
# tictoc::toc() # fire = 5.5 min
# beepr::beep(4)
# 
# 
# # save(RSF, file = "./results/rsf_gam.rda")
# # save(rsf_fire, file = "./results/rsf_gam_fire.rda")
# # save(rsf_fire, file = "./results/rsf_gam_fire_not_reproj.rda")
# 
# # model_path <- paste0("results/rsf/", resolution, "/rsf_gam_",
# #                      if (nzchar(scenario)) paste0(scenario, "_") else "", resolution, ".rda")
# # save(RSF, file = model_path)
# #................................................
# 
# 
# # wanting to know what their resource selection when there isnt a fire then fit data that are non fire
# rsf_data_normal <- rsf_data[rsf_data$year != 2023, ] # 27,616,532
# rsf_data_normal$id_year <- droplevels(rsf_data_normal$id_year)
# 
# #Fit the RSF
# tictoc::tic()
# rsf_normal <-
#   bam(detection ~
#         s(ID, bs = 're') + # ID = goat_id not id_year
#         s(year, bs = 're') + # random effect of year because not each year is the same
#         s(ID, year, bs="re") + # individual-specific yearly deviations
#         s(elev_scaled, k = 5) + # not using 'by = year', because the affect of x isnt different each year
#         s(log_dist_to_slope50, k = 5) +
#         ti(log_dist_to_slope50, elev_scaled) + # interaction between two continuous variables therefore ti() is used instead of smooth s(), if the variables were continuous and categorical, then s() with bs = "sz" can be used
#       s(x, by = ID) + s(y, by = ID) + s(I((x^2 + y^2)/2), by = ID), # spatial smooth, x is being affected by ID differently
#       family = poisson(link = 'log'), 
#       data = rsf_data_normal,
#       weights = weight,
#       method = 'fREML',
#       discrete = TRUE,
#       control = gam.control(nthreads = 14, trace = TRUE))
# tictoc::toc() # all = 3.4 min, fire = 35 sec, no fire = 2.7 min
# 
# # save(rsf_normal, file = "./results/rsf_gam_normal.rda")
# 
# 
# 
# RSF <- rsf_fire
# 
# summary(RSF)
# # plot(RSF, pages = 1)
# # gratia::draw(RSF, rug = FALSE) # this is occuring inside of the model rathre than done outside x^2 + y^2, error will occur like needing to log and scale raster data prior
# k.check(RSF)
# # for all years
# # k'            edf   k-index p-value
# # s(id_year)                          35 0.000002273109        NA      NA
# # s(elev_scaled)                       4 0.999999769138 0.8808904   0.000
# # s(log_dist_to_slope50)               4 0.999992125203 0.9190636   0.155
# # ti(log_dist_to_slope50,elev_scaled) 16 1.000012609432 1.1738199   0.795
# 
# # model_plot_path <- paste0("results/rsf/", resolution, "/rsf_model_",
# #                      if (nzchar(scenario)) paste0(scenario, "_") else "", resolution, ".png")
# # ggsave(last_plot(),
# #        width = 6*1.5, height = 6, units = "in",
# #        dpi = 600,
# #        bg = "transparent",
# #        # file="results/rsf/rsf_model.png") # overall
# #        # file="results/rsf/rsf_model_fire.png")
# #        file="results/rsf/rsf_model_nofire.png")
# # file = model_plot_path)
# 
# 
# 
# 
# #...........................................................................
# 
# # load("./results/rsf_gam.rda")
# load("./results/rsf_gam_fire.rda")
# # load("./results/rsf_gam_nofire.rda")
# 
# 
# 
# #Then build a dataset for the model to use to generate predictions
# pred_df <- terra::as.data.frame(elev_scaled, xy = TRUE)[,c("x","y")]
# pred_df$elev_scaled <- terra::extract(elev_scaled, pred_df[,c("x","y")])[,2]
# # pred_df$dist_to_slope50 <- terra::extract(dist_to_slope50, pred_df[,c("x","y")])[,2]
# pred_df$log_dist_to_slope50 <- terra::extract(log_dist_to_slope50, pred_df[,c("x","y")])[,2]
# pred_df$year <- as.factor(2020)
# # factor any ID, needed for random effect -> needed for model because model has ID random effects, but in predictions, individual variation is ignored
# pred_df$ID <- as.factor("CA01")
# pred_df$id_year <- as.factor("CA01_2023")
# 
# # reorder columns
# pred_df <- dplyr::relocate(pred_df, c("ID", "year", "id_year"), .before = x)
# 
# # generate predictions
# pred_df$lambda <- pred <- predict(rsf_fire, newdata = pred_df,
#                                   exclude = c("(Intercept)","s(ID)"),
#                                   # exclude = c("(Intercept)","s(ID), s(year)"),
#                                   type = "response")
# 
# # get lambda values (i.e., intensity of use/selection)
# pred_df$lambda[pred_df$lambda > quantile(pred_df$lambda, .99)] <- quantile(pred_df$lambda, .99)
# pred_df$lambda[pred_df$lambda < quantile(pred_df$lambda, .01)] <- quantile(pred_df$lambda, .01)
# pred_df$lambda <- pred_df$lambda/max(pred_df$lambda)
# 
# # convert to raster
# rsf_map <- rast(pred_df[c("x","y","lambda")], type="xyz")
# 
# plot(rsf_map)
# 
# # dir.create("./results/rsf/")
# # writeRaster(rsf_map, file = "./results/rsf/rsf_map.tif")
# writeRaster(rsf_map, file = "./results/rsf/rsf_map_fire_not_reproj.tif", overwrite = TRUE)
# writeRaster(rsf_map, file = "./results/rsf/rsf_map_fire2.tif", overwrite = TRUE)
# writeRaster(rsf_map, file = "./results/rsf/rsf_map_nofire.tif", overwrite = TRUE)
# 
# 
# # raster_path <- paste0("results/rsf/", resolution, "/rsf_map_",
# #                      if (nzchar(scenario)) paste0(scenario, "_") else "", resolution, ".tif")
# # writeRaster(rsf_map, file = raster_path)
# 
# 
# rm(rsf_map, pred_df, rsf_df, RSF, pred)
# 
# 
# 
# 
# 
# 
# # # wanting to know their overall resource selection
# # rsf_map <- rast("./results/rsf/rsf_map.tif")
# # # wanting to know what their resource selection was like during the fire 
# # rsf_map_fire <- rast("./results/rsf/rsf_map_fire.tif")
# # # wanting to know what their resource selection when there isnt a fire
# # rsf_map_nofire <- rast("./results/rsf/rsf_map_nofire.tif")
# # # wanting to know how their resource selection changed (if it did) then get the difference (via mapping)
# # # what the non fire (norm) is minus fire and how much fire differed compared to the norm
# # rsf_map_diff <- rsf_map_nofire - rsf_map_fire
# 
# 
# #.............................
# # other resolutions
# # "50m" or "75m" or "100m"
# # resolution <- "50m"
# 
# # wanting to know their overall resource selection
# # rsf_map <- rast(paste0("results/rsf/", resolution, "/rsf_map_", resolution, ".tif"))
# # rsf_map_fire <- rast(paste0("results/rsf/", resolution, "/rsf_map_fire_", resolution, ".tif"))
# # rsf_map_nofire <- rast(paste0("results/rsf/", resolution, "/rsf_map_nofire_", resolution, ".tif"))
# # rsf_map_diff <- rsf_map_nofire - rsf_map_fire
# 
# 
# 
# # plot(rsf_map)
# plot(rsf_map_fire)
# plot(rsf_map_nofire)
# plot(rsf_map_diff) #above 0 = increased selection?
# 
# 
# 



options(scipen = 999)

# wanting to know what their resource selection was like during the fire then fit only data during the fire
rsf_data_fire <- rsf_data[rsf_data$year == 2023, ] # 5,713,669
rsf_data_fire$id_year <- droplevels(rsf_data_fire$id_year)
rsf_data_fire$year <- droplevels(rsf_data_fire$year)

sum(rsf_data$detection == 1)
sum(rsf_data_fire$detection == 1)

ggplot() +
  geom_histogram(data = rsf_data_fire[rsf_data_fire$detection ==1, ], aes(x = elev_scaled)) +
  # geom_density(data = rsf_data_fire, aes(x = elev_scaled))
  ggtitle("fire year") 

ggplot() +
  geom_histogram(data = rsf_data[rsf_data$detection ==1, ], aes(x = elev_scaled)) +
  ggtitle("normal years")


