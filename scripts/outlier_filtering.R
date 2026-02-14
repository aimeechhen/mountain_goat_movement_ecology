
# 2025-04-30
# 2025-11-22
# 2025-12-10
# 2026-01-22

# Mountain goat GPS collar data and supplementary information was provided by BC Parks

library(tidyverse)
library(ctmm)
library(lubridate)
library(dplyr)
library(leaflet)
library(mapview)
library(sf)
library(basemaps)
library(crayon)
library(tictoc)
library(paletteer)
library(beepr)
options(scipen = 999)


# webshot::install_phantomjs() # to save interactive leaflet map as an image

# data cleaning methods based on: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf
# refer to: https://www.biorxiv.org/content/biorxiv/early/2021/07/24/2020.06.12.130195.full.pdf

# see function scripts for description of what each function does
source('scripts/functions/plot_outlie_w_map.R')
source('scripts/functions/outlie2.R') 
source('scripts/functions/plot_movement_map.R')
source('scripts/functions/plot_range.R')
source('scripts/functions/turn_angle.R')
source('scripts/functions/fix_interval.R')

# set the palette for plotting (plot_range())
colour_pal <- paletteer_c("grDevices::rainbow", 30)

# import data
load("./data/goat/prep/combined_data.rda")

# cannot calibrate the fixes due to calibration data is only of 1 fix type, cannot apply error models without a lot of additional work
# therefore, not including location error (i.e., fix type, dop and UERE)
# need to drop certain columns because not including measurement error, if they are included you will run into issues when converting into ctmm object
raw_data <- subset(combined_data, select = -c(fix_type, hdop, vdop, pdop, dop, altitude_m))

# convert to ctmm object
tel_data <- as.telemetry(raw_data, 
                         mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id", "data_source"))


# ensure each df in the list is in the correct order with matching row name
for (i in 1:length(tel_data)) {
  # order the df based on fix_id
  tel_data[[i]] <- tel_data[[i]][order(tel_data[[i]][["fix_id"]]),]
  # set the rowname to match the fix_id
  rownames(tel_data[[i]] ) <- tel_data[[i]]$fix_id
}


# clean up environment
rm(raw_data, i)

# set the folder to store images during data cleaning process
outlie_folder <- "./figures/outlie_filtering/20260213/"
# set the folder to store data during data cleaning process
in_progress_folder <- "./data/goat/outlie_filtering/in_progress/20260213/"
cleaned_folder <- "./data/goat/outlie_filtering/cleaned/20260213/"

# create folder if it doesnt exist
dir.create(outlie_folder, recursive = TRUE, showWarnings = TRUE)
dir.create(in_progress_folder, recursive = TRUE, showWarnings = TRUE)
dir.create(cleaned_folder, recursive = TRUE, showWarnings = TRUE)

# # create folder to store in progress cleaning data
# dir.create("./data/goat/outlie_filtering/in_progress/", recursive = TRUE, showWarnings = TRUE)
# dir.create("./data/goat/outlie_filtering/cleaned/", recursive = TRUE, showWarnings = TRUE)




#...................................................................................
# outlier filtering
#.....................................................................................

# run through the whole workflow then start at the top again with a new individual, repeat process

# set individual
goat_id <- "CA02"
# extract data for individual
goat <- tel_data[[goat_id]]
# add a column called individual.local.identifier (required for custom functions to work)
goat$individual.local.identifier <- goat_id


#..........................................................

# PART 1: outlie() checks ----

cat(green("1. look for outliers, plot & get outlie data \n"))
# 1. look for outliers, plot & get outlie data
png(file = paste0(outlie_folder, goat$individual.local.identifier[1], "_1a_raw_check.png"), width = 6.23*2, height = 6*1.5, units = "in", res = 600)
out <- plot_outlie(goat) #plot_outlie(x), x is a single individual telemetry ctmm data
dev.off()
# #overlay tracking data on a map for better representation to investigate fixes that may be outliers 
((raw_tracks <- movement_map(goat)))
# # save an image of the map
# mapview::mapshot2(raw_tracks, vwidth = 1600, vheight = 900,
#                   file = paste0(outlie_folder, goat$individual.local.identifier[1], "_1b_map_raw_tracks.png"))
# get the outlie data, have to do outlier2() after plot_outlie or plot_outlie wont plot in R
out_data <- outlie2(goat, id = "fix_id") #outlie2(x), includes keep items from as.telemetry()
beep(4)

#..........................................................
cat(green("2. good location estimates, flag bad location estimates (biological threshold), get fix_id of bad location fixes \n"))
# 2. good location estimates, flag bad location estimates (biological threshold), get fix_id of bad location fixes
#points above where speed vs distance points tend to plateau out, consider the value and does it make biological sense for the animal to be moving at that speed and distance; Management Plan for the Mountain Goat (2010) p.17 100m/h = ~0.028m/s = 2.4km/day, value of 1m/s = ~8.64km/day, so a speed value of 0.5m/s is a conservative speed as a speed burst
# create an outlier column to flag, if units are above the plateau, give them a 1 as flagged, or 0 as innocent
out_data$flag_outlier <- ifelse(out_data$speed >= 0.5, 1, 0)
# how many fixes were found above the plateau?
sum(out_data$flag_outlier) #1
# extract fix_id of bad location estimates
bad_locations <- out_data[out_data$flag_outlier == 1, "fix_id"]

#..............................................
cat(green("3. take only good location estimates \n"))
# 3. take only good location estimates (drop the flagged points), needs to be telemetry object because plot_outlie needs it to be, cant be an outlie object (i.e. out_data)
# subset data that do not include any of the bad location estimates to see what the data looks like 
good_locations <- goat[!(goat$fix_id %in% bad_locations), ]

#..............................................
cat(green("4. re-check (good location estimates) \n"))
# 4. re-check (good location estimates)
# compare to the map in Viewer to see what the removal looks like, add flagged rows to list 
png(paste0(outlie_folder, goat$individual.local.identifier[1], "_2a_good_location_check.png"), width = 6.23*2, height = 6*1.5, units = "in", res = 600)
recheck <- plot_outlie(good_locations)
dev.off()
((good_tracks <- movement_map(good_locations, 
                              map_type = "Esri.WorldImagery")))
# map_type = "Esri.WorldTopoMap")))
# map_type = "Stadia.StamenTerrain")))
mapview::mapshot2(good_tracks, vwidth = 1600, vheight = 900,
                  file = paste0(outlie_folder, goat$individual.local.identifier[1], "_2b_map_good_loc_tracks.png"))
beep(3)

# looks better without the bad location estimates

#............................................................

cat(cyan("5. save good_location (tel data with bad locations dropped) and save outlie data with bad locations identified \n"))
# save progress to be imported later and combined with other
saveRDS(good_locations, file = paste0(in_progress_folder, goat_id, "_good_locations.rds"))
saveRDS(out_data, file = paste0(in_progress_folder, goat_id, "_out_data.rds"))


#/////////////////////////////////////////////////////////////
# PART 2: Manual checks ----
#/////////////////////////////////////////////////////////////

# # set individual
# goat_id <- "CA11"
# goat <- tel_data[[goat_id]]
# goat$individual.local.identifier <- goat_id
# 
# # # import data if not doing all at once
# good_locations <- readRDS(file = paste0(in_progress_folder, goat_id, "_good_locations.rds"))
# out_data <- readRDS(file = paste0(in_progress_folder, goat_id, "_out_data.rds"))
# # extract fix_id of bad location estimates
# bad_locations <- out_data[out_data$flag_outlier == 1, "fix_id"]
movement_map(good_locations, map_type = "Esri.WorldImagery")

cat(yellow(bold("PART 2: Manual checks \n")))
# create an empty vector to record potential outliers
potential_outliers <- vector()

#.....................................................................................
## CA01 selena_goatmez ----

#point 1: 631 -> bad, 2.25h 2.5km
#point 2: 1282 -> bad
#point 3: 1278 -> bad
#point 4: 2847 -> bad
#point 5: 346 -> bad
#point 6: 3207 -> bad
#point 7: 2483 -> leaving it, 2.5km in 6+ hrs even though this is generally how much they move in an entire day
# 611 -> leaving it
# 2978 -> ok, keep, high turn angle but low speed/dist
# 3865 -> leaving it
# potential_outliers <- c(631, 1282, 1278, 2847, 346, 3207)

#............................
## CA04 toats_mcgoats ----

#point 1: 17645 -> bad
#point 2: 15595 -> bad
#point 3: 17621 -> bad
#point 4: 14931 -> bad
#point 5: 18862 -> bad
#point 6: 21381 -> keep, high turn angle/dist but low speed, points not really clustered
#point 7: 17659 -> keep, high turn angle, dist is ~1100m but trailing distance among points and lowish speed
#point 8: 17562 -> bad
#point 9: 18920 -> bad
#point 10: 16280 -> okay? at lot of night time moving, keeping
#point 11: 15170 -> keeping, a lot of early movement
# 15628 -> bad
# 15169 -> keep, next point looks questionable as well but dont want to overclean
# potential_outliers <- c(17645, 15595, 17621, 14931, 18862, 17562, 18920, 15628)

#............................
## CA08 goatzilla ----

#point 1: 41176 -> bad, ~180 turn, high dist but low speed 0.13
#point 2: 34480 -> bad, spike off cluster, ~180
#point 3: 33878 -> bad, high speed/distance, spike off cluster
# 34286 -> keep
# 34253 -> keep
# potential_outliers <- c(41176, 34480, 33878)

#............................
## CA09 the_goatmother ----

#point 1: 46066 -> bad
#point 2: 43218 -> bad
#point 3: 48828 -> bad, spike off cluster, far dist
#point 4: 46278 -> bad
#point 5: 44100 -> bad 
#point 6: 41785 -> bad
#point 7: 43185 -> bad
#point 8: 45783 -> bad
#point 9: 41795 -> bad
#point 10: 43831 -> bad
#point 11: 45768 -> leaving it, on the edge of flagging
#point 12: 43927 -> bad
#point 13: 45390 -> leaving it, on the edge of flagging
#point 14: 49466 -> leaving it
#point 15: 43563 -> leaving it
#point 16: 44594 -> leaving it
#point 17: 44605 -> okay
# 41530 -> okay
# 44007 -> keep
# potential_outliers <- c(46066, 43218, 48828, 46278, 44100, 41785, 43185, 45783, 41795, 43831, 43927)

#............................
# CA11 vincent_van_goat ----

#point 1: 58955 -> bad
#point 2: 61848 -> bad
#point 3: 64178 -> bad
#point 4: 58528 -> bad
#point 5: 58097 -> bad
#point 6: 58124 -> bad
#point 7: 60973 -> bad
#point 8: 60778 -> bad
#point 9: 60772 -> bad
#point 10: 66041 -> bad
#point 11: 60946 -> bad
#point 12: 58104 -> bad
#point 13: 60695 -> bad
#point 14: 65323 -> bad
#point 15: 59818 -> bad
#point 16: 63581 -> bad
#point 17: 64820 -> bad
#point 18: 64819 -> bad
#point 19: 60742 -> bad
# potential_outliers <- c(58955, 61848, 64178, 58528, 58097, 58124, 60973, 60778, 60772, 66041, 60946, 58104, 60695, 65323, 59818, 63581, 64820, 64819, 60742)

#............................
# CA12 goatileo  ----

#point 1: 72042 -> bad
#point 2: 68537 -> bad
#point 3: 69108 -> bad
#point 4: 67847 -> bad
#point 5: 72788 -> bad
#point 6: 68346 -> bad
#point 7: 67166 -> leaving it, speed/distance not TOO high but still, 0.1 & 800m, dont want to over clean
#point 8: 69127 -> bad
#point 9: 69092 -> leaving it
#point 10: 71413 -> okay
#point 11: 67169 -> okay
#point 12: 67838 -> okay
# potential_outliers <- c(72042, 68537, 69108, 67847, 72788, 68346, 69127)



#.....................................................................................
# 1b. look for outliers, plot & get outlie data
# Examine potential outlier ----
poi_id <- 10599
point_check <- plot_range(good_locations, fix_id = poi_id, limit = 10, palette = colour_pal) 
# point_check <- plot_range(updated_locations, fix_id = poi_id, limit = 10, palette = colour_pal)
# plot section
movement_map(point_check$data_section, map_type = "Esri.WorldImagery")
# movement_map(point_check$data_section, map_type = "Esri.WorldTopoMap")
# movement_map(point_check$data_section, map_type = "Stadia.StamenTerrain")

# inspect data section and details of the point -> look at what the distance is to next step and speed, if its far and high speed with a spike shape, as well as if the spike is coming off a cluster of points and returns to that cluster
data_section <- point_check$data_section
# get turning angle of spike, if its ~180 that means they turned around and went back to where they came from suggesting an error spike. note, animals could do intentional backtracking but if its combine with high speed + high distance, and in plot 3 the point is far off top right corner and the other point are clustering, strong indicator of a spike as well
turn_angle(data_section, poi_id)
# what was their travel time? and does it seem reasonable to be moving that far in that time length?
fix_interval(data_section, poi_id)

# check to see what it would look like without the point, it looks good, go to next step, if it doesnt then on to the next point to check
test <- good_locations[!(good_locations$fix_id %in% c(potential_outliers, poi_id)), ]
test2 <- plot_range(test, fix_id = (poi_id - 1), limit = 10, palette = colour_pal)
movement_map(test2$data_section, map_type = "Esri.WorldImagery")

#...................
# 2b. if bad point, confirming identified point to be flagged
# movement_map(point_check$POI)
# record the fix_id of potential bad location estimate and add it to the list
potential_outliers <- c(potential_outliers, poi_id)
# 3b. take only updated locations (good locations + manual check) (drop potential outliers)
updated_locations <- good_locations[!(good_locations$fix_id %in% potential_outliers), ]
# 4b. recheck (updated location estimates)
recheck <- plot_outlie(updated_locations)
movement_map(updated_locations, map_type = "Esri.WorldImagery")
# movement_map(updated_locations, map_type = "Esri.WorldTopoMap")
# movement_map(updated_locations, map_type = "Stadia.StamenTerrain")
beep(4)

# no longer any indication of clear outliers in the data
# everything looks good, can move onto step 6


#..............................................................
# 6. update flag_outlier column in the main df with the bad location estimates and potential outliers
# out_data$flag_outlier <- ifelse(out_data$fix_id %in% bad_locations, 1, out_data$flag_outlier)
out_data$flag_outlier <- ifelse(out_data$fix_id %in% c(bad_locations, potential_outliers), 1, out_data$flag_outlier)

sum(out_data$flag_outlier)


#.......................................................................
cat(cyan("7. save cleaned data (tel data with bad locations dropped) and save outlie data with bad locations identified \n"))
#save 
png(paste0(outlie_folder, goat$individual.local.identifier[1], "_5_cleaned_check.png"), width = 6.23*2, height = 6*1.5, units = "in", res = 600)
final_check <- plot_outlie(updated_locations)
dev.off()
((cleaned_tracks <- movement_map(updated_locations, map_type = "Esri.WorldImagery")))
mapview::mapshot2(cleaned_tracks, vwidth = 1600, vheight = 900,
                  file = paste0(outlie_folder, goat_id, "_3_map_manual_cleaned_tracks.png"))

# save progress to be imported later and combined with other
saveRDS(updated_locations, file = paste0(cleaned_folder, goat_id, "_updated_locations.rds"))
saveRDS(out_data, file = paste0(cleaned_folder, goat_id, "_out_data_manual_checks.rds"))
beep(4)

# move onto next goat, restart the process
rm(cleaned_tracks, data_section, final_check, goat, good_locations, good_tracks, out, out_data, point_check, raw_tracks, recheck, updated_locations, bad_locations, poi_id, potential_outliers, test, test2)
gc()
dev.off(dev.list()["RStudioGD"])







#............................
# CA

#point 1:  -> 
#point 2:  -> 
#point 3:  -> 
#point 4:  -> 
#point 5:  -> 
#point 6:  -> 
#point 7:  -> 
#point 8:  -> 
#point 9:  -> 
#point 10:  -> 
#point 11:  -> 
#point 12:  -> 
#point 13:  -> 
#point 14:  -> 
#point 15:  -> 
#point 16:  -> 
#point 17:  -> 




#~~~~~~~~~~~~~~~~~~~~~~~ ----
# not core 6 goats ----
# CA02 great goatsby ----
#point 1: 10837 -> bad
#point 2: 10208 -> bad
#point 3: 10373 -> bad
#point 4: 12505 -> bad
#point 5: 11078 -> bad
#point 6: 11074 -> bad
#point 7: 12149 -> bad
#point 8: 13251 -> bad
#point 9: 13813 -> leaving it
# 10731 -> bad
# 10728 -> bad
# 10445 -> leaving it
# 12083 -> leaving it
# 12109 -> leaving it
# 14036 -> bad 
# potential_outliers <- c(10837, 10208, 10373, 12505, 11078, 11074, 12149, 13251, 10731, 10728)

# CA07 alpine_pacino ----
# clean, nothing additional

# CA10 goatbriella ----
#point 1: 51662 -> 
#point 2: 56466 -> 
#point 3: 53814 -> 
#point 4: 51824 -> 
#point 5: 54107 -> okay
#point 6: 51537 -> 
#point 7:  -> 
#point 8:  -> 
#point 9:  -> 

#~~~~~~~~~~~~~~~~~~~~~~~~