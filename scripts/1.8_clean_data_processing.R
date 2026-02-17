
library(ctmm)
options(scipen = 999)

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


in_progress_folder <- "./data/goat/outlie_filtering/in_progress/20260213/"
cleaned_folder <- "./data/goat/outlie_filtering/cleaned/20260213/"


#...................................................
# Check clean data ----

# Load all outlie data
rds_files <- list.files(cleaned_folder, pattern = ".*out_data.*\\.rds$", full.names = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine into df 
outlie_df <- do.call(rbind, rds_list)

# dont need this actually, updated the flag outlier column during the filtering process
# load(file = paste0(in_progress_folder, "identified_list.rda"))
# flag_fix_id <- do.call(c, identified_list) #29

# go to visualize_data.R script to plot to check if anything was missed

# save outlie data
save(outlie_df, file = paste0(cleaned_folder, "outlie_df.rda"))

# clean up environment
rm(rds_files, rds_list)

#............................................................
# update tracking data ----
#............................................................

load(file = paste0(cleaned_folder, "outlie_df.rda"))

# check for same column names to avoid duplication
intersect(colnames(combined_data), colnames(outlie_df))

# combine df
clean_data <- merge(combined_data, outlie_df, 
                    by = c("fix_id", "goat_id", "goat_name", "collar_id", "longitude", "latitude", 
                           "data_source","individual.local.identifier", "timestamp"), 
                    all.x = TRUE)

# how many currently outlier from screening process
sum(clean_data$outlier) #742
# how many were detected as bad locations and manual checks
sum(clean_data$flag_outlier, na.rm = TRUE) #29
# update the column, if it contains NA then give it a 0
clean_data$flag_outlier <- ifelse(is.na(clean_data$flag_outlier), 0, clean_data$flag_outlier)
# update the outlier column with bad locations and manual checked outliers
clean_data$outlier <- ifelse(clean_data$flag_outlier == 1, 1, clean_data$outlier)
sum(clean_data$outlier) #771
# 742+29

# reorganize the df based on combined_data structure
# check what columns match from the main df and order it in that way and what columns dont match are the leftovers and placed at the back/end of the df
clean_data <- clean_data[, c(intersect(names(combined_data), names(clean_data)), 
                             setdiff(names(clean_data), names(combined_data)))]

# ensure the df ordered properly
clean_data <- clean_data[order(clean_data$fix_id),]
# reset the row number to proper default
rownames(clean_data) <- NULL

# make a list of goats that were alive during the fire
goats <- c("CA01", "CA04", "CA08", "CA09", "CA11", "CA12")

# indicate that they are part of the fire study
clean_data$fire_study <- ifelse(clean_data$goat_id %in% goats, 1, 0)



save(clean_data, file = "./data/goat/clean_data.rda")

rm(outlie_df)
