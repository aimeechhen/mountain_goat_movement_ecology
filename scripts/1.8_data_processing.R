


# import outlie data
# load("./data/goat/cleaning/in_progress/out_data_good_loc_list_original_final.rda")
load("./data/goat/cleaning/out_data_good_loc_list.rda")

# convert into a df
outlie_data <- do.call(rbind, out_data_good_loc_list)
# reorder based on fix_id
outlie_data <- outlie_data[order(outlie_data$fix_id),]


# load("./data/goat/cleaning/in_progress/out_data_good_loc_list_new_final.rda")
# outlie_new <- do.call(rbind, out_data_good_loc_list)
# outlie_new <- outlie_new[order(outlie_new$fix_id),]
# 
# # check for same column names to avoid duplication,
# intersect(colnames(outlie_original), colnames(outlie_new))
# 
# # merge the two df
# outlie_data <- merge(outlie_original, outlie_new, 
#                      by = c("timestamp", "longitude", "latitude", "class", "HDOP", 
#                             "fix_id", "goat_id", "goat_name", "collar_id", "data_source", 
#                             "x", "y","VAR.xy", "individual.local.identifier", "t", 
#                             "distance", "VAR.distance", "speed", "VAR.speed", "row_n","flag_outlier"), 
#                      all = TRUE)

# outlie_data <- outlie_data[order(outlie_data$fix_id),]
# reset rownames based on fix_id
rownames(outlie_data) <- outlie_data$fix_id

# how many were detected as bad locations?
sum(outlie_data$flag_outlier) # 1178, 0.5 = 13
# outlie_data[outlie_data$flag_outlier == 1,]
save(outlie_data, file = "./data/goat/prep/outlie_data.rda")

load("./data/goat/prep/outlie_data.rda")

# import data
load("./data/goat/prep/combined_data.rda")

# check for same column names to avoid duplication,
intersect(colnames(combined_data), colnames(outlie_data))

# combine dfs by fix_id without duplicating the columns when using all = TRUE, cannot list all the same columns in the by= argument, it comes with 154k+ obs.
collar_data <- merge(combined_data, outlie_data[, !names(outlie_data) %in% names(combined_data)[names(combined_data) != "fix_id"]],
  by = "fix_id", all = TRUE)


collar_data <- collar_data[order(collar_data$fix_id),]
rownames(collar_data) <- collar_data$fix_id

str(collar_data)
# check to see if the flag_outlier transferred over
# how many were detected as bad locations?
sum(collar_data$flag_outlier, na.rm = TRUE) #13
table(collar_data$flag_outlier) # 1178
range(collar_data$flag_outlier, na.rm = TRUE)
# update the column, if it contains NA then give it a 0
collar_data$flag_outlier <- ifelse(is.na(collar_data$flag_outlier), 0, collar_data$flag_outlier)

# how many currently 
sum(collar_data$outlier) # 742

# update outlier column
collar_data$outlier <- ifelse(collar_data$flag_outlier == 1, 1, collar_data$outlier)
sum(collar_data$outlier) #1920, 755
# 1178+742 #matches
742+13




save(collar_data, file = "./data/goat/collar_data.rda")
# go to stage 3. revisualize the good locations to compare the difference and as a reference to see if there are any potential outliers or are those points where others have gone as well across all data

# clean environment
rm(list = ls())
gc()







#////////////////////////////////////////////////////////////////////////////////////////////
# data prep for analysis

library(lubridate)

load("./data/goat/collar_data.rda")

# add temporal attributes
collar_data$year_local <- year(collar_data$timestamp_local)
collar_data$month_local <- month(collar_data$timestamp_local, label = FALSE) #label = false for numerical month
collar_data$day_local <- day(collar_data$timestamp_local)
collar_data$month_day_local <- format(collar_data$timestamp_local, "%m-%d")
collar_data$doy_local <- yday(collar_data$timestamp_local) #day of the year

# create a column to be used for period specific analyses
collar_data$id_year <-  paste(collar_data$goat_id, collar_data$year_local, sep = "_")

# make a list of goats that were alive during the fire
goats <- c("CA01", "CA04", "CA08", "CA09", "CA11", "CA12")

# indicate that they are part of the fire study
collar_data$fire_study <- ifelse(collar_data$goat_id %in% goats, 1, 0)

# Define the dates for wildfire period as per bc wildfire dates
# July 22 to October 26, 2023
fire_start <- "2023-07-22"
fire_end <- "2023-10-26"

# Define the the study period based on the fire period across all years (same days across all years)
# July 22 to October 26
period_start <- "07-22"
period_end <- "10-26"

# create a column to indicate if the fix fall within the fire period, 1 = yes, 0 = no
collar_data$fire_period <- ifelse(collar_data$date_local >= fire_start & collar_data$date_local <= fire_end, 1, 0)

# create a column to indicate if the fix fall within the study period, 1 = yes, 0 = no
# study period = the fire period across all years
collar_data$study_period <- ifelse(collar_data$month_day_local >= period_start & collar_data$month_day_local <= period_end, 1, 0)

#subset to the 6 fire goats
goat_data <- collar_data[collar_data$goat_id %in% goats,]

# update goat id, name and collar id, individual.local.identifier to only contain 6 levels
goat_data$goat_id <- droplevels(goat_data$goat_id)
goat_data$goat_name <- droplevels(goat_data$goat_name)
goat_data$collar_id <- droplevels(goat_data$collar_id)
goat_data$individual.local.identifier <- droplevels(goat_data$individual.local.identifier)

# make sure its in the right order
goat_data <- goat_data[order(goat_data$fix_id), ]


save(goat_data, file = "./data/goat/goat_data.rda")
