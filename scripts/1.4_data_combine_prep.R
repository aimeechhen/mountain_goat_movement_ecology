# prep data for outlier filtering
library(dplyr)


#import data
load("./data/goat/prep/raw_original_screened_prepped.rda") #66099
load("./data/goat/prep/raw_new_prepped.rda")

# check end of original data
aggregate(timestamp_utc ~ goat_id, data = raw_original, max)

# goat_id       timestamp_utc
# 1     CA01 2023-10-05 06:30:38 *
# 2     CA02 2021-09-07 11:00:38
# 3     CA04 2023-10-03 04:30:38 *
# 4     CA07 2023-04-19 08:57:57
# 5     CA08 2023-10-05 06:30:38 *
# 6     CA09 2023-10-04 05:30:38 *
# 7     CA10 2022-07-07 15:05:20
# 8     CA11 2023-10-05 06:30:38 *
# 9     CA12 2023-10-03 10:45:37 *
# 10    CA18 2021-03-16 23:45:44

# check start of new data
aggregate(timestamp_utc ~ goat_id, data = raw_new, min)
# goat_id       timestamp_utc
# 1    CA01 2023-09-01 03:45:00
# 2    CA04 2023-09-01 16:16:00
# 3    CA08 2023-09-01 03:45:00
# 4    CA09 2023-09-01 03:47:00
# 5    CA11 2023-09-01 03:45:00
# 6    CA12 2023-09-01 03:45:00
# ~ 1 month of overlap

# do not include any fixes that overlap with the original data (i.e., chop off some of the start part of the new data) to avoid duplicates
# check the data before chopping, these will be duplicated fixes
dupe_CA01 <- raw_new[raw_new$goat_id == "CA01" & raw_new$timestamp < "2023-10-05 06:30:38", ] #129
dupe_CA04 <- raw_new[raw_new$goat_id == "CA04" & raw_new$timestamp < "2023-10-03 04:30:38", ] #119
dupe_CA08 <- raw_new[raw_new$goat_id == "CA08" & raw_new$timestamp < "2023-10-05 06:30:38", ] #132
dupe_CA09 <- raw_new[raw_new$goat_id == "CA09" & raw_new$timestamp < "2023-10-04 05:30:38", ] #122
dupe_CA11 <- raw_new[raw_new$goat_id == "CA11" & raw_new$timestamp < "2023-10-05 06:30:38", ] #126
dupe_CA12 <- raw_new[raw_new$goat_id == "CA12" & raw_new$timestamp < "2023-10-03 10:45:37", ] #121
# 129 + 119 + 132 + 122 + 126 + 121 #= 749

# chop the data, subset the data not to include 
raw_new <- raw_new[!(
  (raw_new$goat_id == "CA01" & raw_new$timestamp < "2023-10-05 06:30:38") |
    (raw_new$goat_id == "CA04" & raw_new$timestamp < "2023-10-03 04:30:38") |
    (raw_new$goat_id == "CA08" & raw_new$timestamp < "2023-10-05 06:30:38") |
    (raw_new$goat_id == "CA09" & raw_new$timestamp < "2023-10-04 05:30:38") |
    (raw_new$goat_id == "CA11" & raw_new$timestamp < "2023-10-05 06:30:38") |
    (raw_new$goat_id == "CA12" & raw_new$timestamp < "2023-10-03 10:45:37")
),]
# 11628-10879 #= 749
# 10879 fixes


# check for duplicates to see if they were dropped correctly or if other duplicated fixes exist (some dupes are the same but with a difference of ~38-40secs in timestamp between the fixes)
dupes <- raw_new[paste(raw_new$collar_id, raw_new$timestamp_utc) %in% 
                   paste(raw_original$collar_id, raw_original$timestamp_utc), ] #14 without chopping, 0 when chopped

# # drop duplicated rows
# raw_new <- raw_new[!(paste(raw_new$collar_id, raw_new$timestamp_utc) %in%
#                        paste(raw_original$collar_id, raw_original$timestamp_utc)),]

# clean up environment
rm(dupe_CA01, dupe_CA04, dupe_CA08, dupe_CA09, dupe_CA11, dupe_CA12, dupes)

#........................................................

# check for same column names to avoid duplication
intersect(colnames(raw_original), colnames(raw_new))

# combine datasets
combined_data <- merge(raw_original, raw_new, 
                       by = c("goat_id", "goat_name", "collar_id", 
                              "timestamp_utc", "timestamp_local", "longitude", "latitude", 
                              "fix_type", "data_source", "date_local", "outlier",
                              "individual.local.identifier", "location.lat", "location.long", "timestamp"),
                       all = TRUE)

#reset fix_id to account for new data
rownames(combined_data) <- NULL
combined_data$fix_id <- as.numeric(rownames(combined_data))
combined_data <- relocate(combined_data, fix_id, .before = goat_id)

# create a column to indicate what kind of dop type the fix had
combined_data$dop_type <- as.factor(ifelse(!is.na(combined_data$dop), "dop",
                                           ifelse(!is.na(combined_data$hdop), "hdop",
                                                  ifelse(!is.na(combined_data$vdop), "vdop",
                                                         ifelse(!is.na(combined_data$pdop), "pdop",
                                                                NA)))))
# organized the dops together
combined_data <- relocate(combined_data, c(dop, dop_type), .after = pdop)
combined_data <- relocate(combined_data, c(hdop, vdop, pdop, dop, dop_type), .after = fix_type)
combined_data$fix_type <- as.factor(combined_data$fix_type)
str(combined_data)

# total raw dataset (10 animals) = 76978
# indicate if they are part of this study or not
combined_data$study_goat <- ifelse(combined_data$goat_id %in% c("CA01", "CA04", "CA08", "CA09", "CA11", "CA12"), 1, 0)
sum(combined_data$study_goat) # 55194 fixes for the 6 goats

save(combined_data, file = "./data/goat/prep/combined_data.rda")
# load("./data/goat/prep/combined_data_w_dupes.rda") # previous version before chopping ~38-40sec timestamp difference but same everything else, refer to the screenshot of the df
load("./data/goat/prep/combined_data.rda")



# clean environment
rm(list = ls())
gc()





