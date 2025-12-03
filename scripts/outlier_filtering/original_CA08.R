

source("scripts/1.7_outlier_filtering.R")
library(paletteer)
# check if we're saving to the right folder
outlie_folder

load("./data/goat/cleaning/in_progress/out_data_good_loc_list.rda")

#.....................................................................................
# CA08 goatzilla -----
#.....................................................................................


# extract data for individual
goat <- tel_data[["CA08"]]
# add a column called individual.local.identifier (required for custom functions to work)
goat$individual.local.identifier <- "CA08"


#..................................................................
# outlier filtering

# 1. look for outliers, plot & get outlie data
# png(file = paste0(outlie_folder, goat$individual.local.identifier[1], "_1_raw_check.png"), width = 6.23*2, height = 6*1.5, units = "in", res = 600)
out <- plot_outlie(goat) #plot_outlie(x), x is a single individual telemetry ctmm data
# dev.off()
#overlay tracking data on a map for better representation to investigate fixes that may be outliers 
((raw_tracks <- movement_map(goat)))
# save an image of the map
# mapview::mapshot2(raw_tracks, vwidth = 1600, vheight = 900,
#                   file = paste0(outlie_folder, goat$individual.local.identifier[1], "_2_map_raw_tracks.png"))
# get the outlie data, have to do outlier2() after plot_outlie or plot_outlie wont plot in R
out_data <- outlie2(goat, id = "fix_id") #outlie2(x), includes keep items from as.telemetry()


# 2. good location estimates, flag bad location estimates (biological threshold), get fix_id of bad location fixes
#points above where speed vs distance points tend to plateau out, consider the value and does it make biological sense for the animal to be moving at that speed and distance
# create an outlier column to flag, if units are above the plateau, give them a 1 as flagged, or 0 as innocent
out_data$flag_outlier <- ifelse(out_data$speed >= 0.10, 1, 0) 
# how many fixes were found above the plateau?
sum(out_data$flag_outlier) #0.2 = 34, 0.1 = 125
# extract fix_id of bad location estimates
bad_locations <- out_data[out_data$flag_outlier == 1, "fix_id"]


# 3. take only good location estimates (drop the flagged points), needs to be telemetry object because plot_outlie needs it to be, cant be an outlie object (i.e. out_data)
# subset data that do not include any of the bad location estimates to see what the data looks like 
good_locations <- goat[!(goat$fix_id %in% bad_locations), ]

# 4. re-check (good location estimates)
# compare to the map in Viewer to see what the removal looks like, add flagged rows to list
png(paste0(outlie_folder, goat$individual.local.identifier[1], "_3_good_location_check.png"), width = 6.23*2, height = 6*1.5, units = "in", res = 600)
recheck <- plot_outlie(good_locations)
dev.off()
((good_tracks <- movement_map(good_locations)))
mapview::mapshot2(good_tracks, vwidth = 1600, vheight = 900,
                  file = paste0(outlie_folder, goat$individual.local.identifier[1], "_4_map_good_tracks.png"))


#save outlie data with bad locations identified
out_data$flag_outlier <- ifelse(out_data$fix_id %in% bad_locations, 1, out_data$flag_outlier)
out_data_good_loc_list[[goat$individual.local.identifier[1]]] <- out_data

# save progress
save(out_data_good_loc_list, file = "./data/goat/cleaning/in_progress/out_data_good_loc_list.rda")
# clean environment
rm(list = ls())
gc()

# but still looks like there might be some outliers
# unclear, need to ask Dr. Noonan's help



# 
# #/////////////////////////////////////////////////////////////////////////////
# 
# 
# # no longer any indication of clear outliers in the filtered data
# # everything looks good, can skip step 5
# 
# # 6. update flag_outlier column in the main df with the bad location estimates and potential outliers
# # out_data$flag_outlier <- ifelse(out_data$fix_id %in% bad_locations, 1, out_data$flag_outlier)
# out_data$flag_outlier <- ifelse(out_data$fix_id %in% potential_outliers, 1, out_data$flag_outlier)
# 
# # store updated out_data in list
# out_data_good_loc_list[[4]] <- out_data
# 
# # move onto next goat
# 
# # save list and move on to next goat
# save(out_data_good_loc_list, file = "./data/goat/cleaning/out_data_good_loc_list.rda")
# 
# # clean environment and clear plots and viewer pane 
# rm(list = ls())
# gc()
# dev.off(dev.list()["RStudioGD"])
