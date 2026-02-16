
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
# dir.create(outlie_folder, recursive = TRUE, showWarnings = TRUE)
# dir.create(in_progress_folder, recursive = TRUE, showWarnings = TRUE)
# dir.create(cleaned_folder, recursive = TRUE, showWarnings = TRUE)
# dir.create("./data/goat/outlie_filtering/in_progress/", recursive = TRUE, showWarnings = TRUE)
# dir.create("./data/goat/outlie_filtering/cleaned/", recursive = TRUE, showWarnings = TRUE)

# # create a list to store all of the potential outliers (only once)
# identified_list <- vector("list", 10)
# # assign names to each item
# names(identified_list) <- names(tel_data)
# save(identified_list, file = paste0(in_progress_folder, "identified_list.rda"))


#...................................................................................
# outlier filtering
#...................................................................................

# run through the whole workflow then start at the top again with a new individual, repeat process

# set individual
goat_id <- "CA01"
# extract data for individual
goat <- tel_data[[goat_id]]
# add a column called individual.local.identifier (required for custom functions to work)
goat$individual.local.identifier <- goat_id


#..........................................................
# PART 1: outlie() checks ----
#..........................................................

cat(green("1. look for outliers, plot & get outlie data \n"))
# 1. look for outliers, plot & get outlie data
png(file = paste0(outlie_folder, goat$individual.local.identifier[1], "_1a_raw_check.png"), width = 6.23*2, height = 6*1.5, units = "in", res = 600)
out <- plot_outlie(goat) #plot_outlie(x), x is a single individual telemetry ctmm data
dev.off()
# #overlay tracking data on a map for better representation to investigate fixes that may be outliers 
# ((raw_tracks <- movement_map(goat)))
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

# set individual
goat_id <- "CA01"
goat <- tel_data[[goat_id]]
goat$individual.local.identifier <- goat_id

# # import data if not doing all at once
good_locations <- readRDS(file = paste0(in_progress_folder, goat_id, "_good_locations.rds"))
out_data <- readRDS(file = paste0(in_progress_folder, goat_id, "_out_data.rds"))
# extract fix_id of bad location estimates
bad_locations <- out_data[out_data$flag_outlier == 1, "fix_id"]
movement_map(good_locations, map_type = "Esri.WorldImagery")

cat(yellow(bold("PART 2: Manual checks \n")))
# create an empty vector to record potential outliers
potential_outliers <- vector()

#........................................
## CA01 selena_goatmez ----

# obvious ones
# 631 -> bad, 2.25h 2.5km
# 1282 -> bad
# 2847 -> bad
# 1278 -> bad
# 2483 -> okay
# 346 -> bad
#less obvious
# 3207 -> bad
# 3865 -> leaving it
potential_outliers <- c(631, 1282, 2847, 1278, 346, 3207)


#........................................
## CA04 toats_mcgoats ----

# starburst
# 17516 -> 2021-02-09
# 15466 -> 2020-02-16
# 17492 -> 2021-02-06
# 17530 -> 2021-02-10
# 17433 -> 2021-01-31
# 18791 -> 2021-06-11
# 18733 -> 2021-06-06


#........................................
## CA08 goatzilla ----

# 40796
# 34232
# 33630 -> leaving it
# potential_outliers <- c(40796, 34232)

#........................................
## CA09 the_goatmother ----

# 42838 -> bad
# 48448 -> bad
# 45898 -> bad
# 45686 -> bad, really far
# starburst cluster spikes
# 45388 -> 2021-06-07, leaving it
# 43720 -> 2020-12-01, bad
# 41405 -> 2019-10-29, bad, 1.5h for ~2km to get back to the cluster, near 180 turn
# 42805 -> 2020-06-04, bad
# 45403 -> 2021-06-09, bad, 9 hours to point, might be missing a fix but not going to assume, removing
# 41415 -> 2019-10-30, bad, even though its 1 of the spike off a cluster (41405 day before), dist/speed really high
# lesser obvious
# 43451 -> 2020-10-26 leaving it, ca11 has similar fix, could be both bad or actually went
# potential_outliers <- c(42838, 48448, 45898, 45686, 43720, 41405, 42805, 45403, 41415)

#........................................
## CA11 vincent_van_goat ----

# obvious outliers
# 63676 -> bad
# 58026 -> bad
# 57595 -> bad
# 57622 -> bad
# 60471 -> bad
# 65413 -> bad
# 60444 -> bad
# 64695 -> leaving it, 12h interval, high dist but low speed, 179 turn
# 60193 -> bad
# 58453 -> bad
# lesser obvious
# 60276 -> leaving it
# 57602 -> leaving it
# 63079 -> leaving it, 6 hr and 12.5 hr, then returns to cluster, low speed
# 59316 -> leaving it, ca09 had similar fix location, could be both bad or actual fix, avoiding overcleaning
# 64255 -> leaving it, 12.5 and 6.25h might be missing a fix inbetween, low speed
# 61346 -> okay
# potential_outliers <- c(63676, 58026, 57595, 57622, 60471, 65413, 60444, 60193, 58453)

#............................
# CA12 goatileo  ----

# obvious ones
# 71414 -> leaving it, high dist, low speed 0.17m/s, 6.25 hr
# 67909 -> bad
# 68480 -> bad
# 68499 -> leaving it
# 67219 -> bad
# 67718 -> bad
# 72039 -> leaving it, high dist, but low speed with 18.75 interval
# lesser obvious
# 67040 -> leaving it
# potential_outliers <- c(67909, 68480, 67219, 67718)

#.....................................................................................
# Examine potential outlier ----

# 1b. look for outliers, plot & get outlie data
poi_id <- 346
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

# update to the running list of potential_outliers
load(identified_list, file = paste0(in_progress_folder, "identified_list.rda"))
identified_list[[goat_id]] <- c(identified_list, bad_locations, potential_outliers)
# save updated list for next goat
save(identified_list, file = paste0(in_progress_folder, "identified_list.rda"))

# move onto next goat, restart the process
rm(cleaned_tracks, data_section, final_check, goat, good_locations, good_tracks, out, out_data, point_check, raw_tracks, recheck, updated_locations, bad_locations, poi_id, potential_outliers, test, test2, identified_list)
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
# other goats ----

#........................................
# CA02 great goatsby ----

# obvious ones
# 10708 -> bad
# 10949 -> bad
# 10079 -> bad
# 10244 -> bad
# 12376 -> bad
# 13122 -> bad
# 13684 -> leaving it
# lesser obvious
# 13907 -> bad
# 10602 -> leaving it, 18h to next fix, these 2 may be behavioural
# 10599 -> leaving it, these 2 may be behavioural
# 10224 -> okay
# 11809 -> ok
# 12020 -> leaving it, these 4 may be behavioural, over a short period, not random dates
# 11980 -> leaving it, these 4 may be behavioural, over a short period, not random dates
# 11954 -> leaving it, these 4 may be behavioural, over a short period, not random dates
# 10316 -> leaving it, these 4 may be behavioural, over a short period, not random dates (except this one, occurred way earlier
# potential_outliers <- c(10708, 10949, 10079, 10244, 12376, 13122, 13907)

#........................................
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