
# preparing raw df for outlier inspection

library(dplyr)
library(ctmm)
library(lubridate)
options(scipen = 999)

load("./data/goat/prep/combined_data_screened.rda")
# import goat details
load("./data/goat/goat_info.rda")

raw_data <- combined_data

# check for duplicated data
combined_data[duplicated(combined_data[c("collar_id", "timestamp_utc")]) | 
           duplicated(combined_data[c("collar_id", "timestamp_utc")], fromLast = TRUE), ] # yes

# check for duplicates because the datasets have been combined and two datasets had an overlapping period i.e. same data twice
combined_data[duplicated(combined_data[c("collar_id", "timestamp_utc", "latitude", "longitude")]) | 
           duplicated(combined_data[c("collar_id", "timestamp_utc", "latitude", "longitude")], fromLast = TRUE), ] # none

# investigate why they're not being flagged when coordinates are included, check duplicate coordinates
all.equal(combined_data$latitude[7820], combined_data$latitude[7821]) # "Mean relative difference: 0.0000000346921"
# minor difference, so thats why its showing up
all.equal(combined_data$longitude[7820], combined_data$longitude[7821]) # "Mean relative difference: 0.00000003162507"
# same

# extract all the duplicates flagged without coordinates to inspect further
dupes <- combined_data[duplicated(combined_data[c("collar_id", "timestamp_utc")]) | 
                         duplicated(combined_data[c("collar_id", "timestamp_utc")], fromLast = TRUE), ] #28

# drop duplicated rows because its the same data, from two datasets
combined_data <- combined_data[!duplicated(combined_data[c("collar_id", "timestamp_utc")]) | 
                   duplicated(combined_data[c("collar_id", "timestamp_utc")], fromLast = TRUE), ] # 77741 -> 77727
77741-77727 #14

# double check that they were dropped
combined_data[duplicated(combined_data[c("collar_id", "timestamp_utc")]) | 
           duplicated(combined_data[c("collar_id", "timestamp_utc")], fromLast = TRUE), ] #none



#....................................................................
# add goat info ----
#....................................................................

# drop goat_03 or going to have merging issues due to two goats one collar dilemma
goat_info <- goat_info[goat_info$goat_id != "CA03",]
# drop the goat 03 factor level
goat_info$goat_id <- droplevels(goat_info$goat_id)
goat_info$goat_name <- droplevels(goat_info$goat_name)
goat_info$collar_id <- droplevels(goat_info$collar_id)

# check for same column names to avoid duplication, add goat info to the raw data
intersect(colnames(combined_data), colnames(goat_info))

# add goat info to df
raw_data <- merge(combined_data, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
raw_data <- relocate(raw_data, c(goat_id, goat_name), .before = collar_id)
str(raw_data)



#.........................................................................
# prep data ----
#.........................................................................

# reorder based on timestamp and goat_id, using utc time hence forth
raw_data <- raw_data[order(raw_data$goat_id, raw_data$timestamp_utc),]
# reset the rownames
rownames(raw_data) <- NULL

# create a row number column to keep track
raw_data$row_number <- rownames(raw_data)




# save df for outlier checks
save(raw_data, file = "./data/goat/prep/raw_data_prepped.rda")
load("./data/goat/prep/raw_data_prepped.rda")


# clean environment
rm(list = ls())
gc()





# create new columns to format names to match required for ctmm based on Movebank critera:
raw_data$individual.local.identifier <- raw_data$goat_id # must use collar_id and not goat_id
raw_data$location.lat <- raw_data$latitude
raw_data$location.long <- raw_data$longitude
raw_data$timestamp <- raw_data$timestamp_utc

tel_data <- as.telemetry(raw_data)

# no duplicate warning


















#....................................................................
# prep df for cleaning  ----
#....................................................................



# DO THIS DURING MAYBE...cleaning


# create new columns to format names to match required for ctmm based on Movebank critera:
raw_data$individual.local.identifier <- raw_data$collar_id # must use collar_id and not goat_id
raw_data$location.lat <- raw_data$latitude
raw_data$location.long <- raw_data$longitude
raw_data$timestamp <- raw_data$timestamp_utc

# rename to match ctmm formatting
raw_data <- rename(raw_data, DOP = dop,
                   HDOP = hdop,
                   VDOP = vdop,
                   PDOP = pdop)




# backup <- raw_data
raw_data <- backup

#convert to telemetry object
tel_data <- as.telemetry(raw_data)

# obtain the outlie outputs for all individuals
outlie_data <- outlie(data = tel_data, plot = FALSE) # output is a list

# assign the id for each item in the list
names(outlie_data) <- names(tel_data)

# convert outlie output list into a df for cleaning
outlie_df <- do.call(rbind, lapply(names(outlie_data), function(x) {
  # extract df
  df  <- outlie_data[[x]]
  # record collar_id
  df$collar_id <- x
  return(df)
}))


# create a telemetry df for ctmm telemetry data
telemetry_df <- do.call(rbind, lapply(names(tel_data), function(id) {
  df <- tel_data[[id]]  # extract each df
  df$collar_id <- id    # assign list name as collar_id
  return(df)
}))



# check for same column names to avoid duplication, add telemetry data to goats
intersect(colnames(raw_data), colnames(telemetry_df))
# combine all dfs together by rownames (i.e. adding columns of df2 to df1 and matching based on row_number)
raw_data <- merge(raw_data, telemetry_df, by = c("collar_id", "longitude", "latitude",  "timestamp"), all = TRUE)

# check for same column names to avoid duplication, add the outlie results
intersect(colnames(raw_data), colnames(outlie_df))
raw_data <- merge(raw_data, outlie_df, by = c("collar_id", "t"), all = TRUE)




# save df for outlier checks
save(raw_data, file = "./data/goat/prep/raw_data_prepped.rda")
load("./data/goat/prep/raw_data_prepped.rda")

# clean environment
rm(list = ls())
gc()
