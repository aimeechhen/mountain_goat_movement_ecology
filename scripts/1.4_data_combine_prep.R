# prep data for outlier filtering
library(ctmm)
library(dplyr)


#import data
load("./data/goat/prep/raw_original_screened_prepped.rda") #66099
load("./data/goat/prep/raw_new_prepped.rda")
load("./data/goat/prep/uere.rda")

# check for same column names to avoid duplication, add goat info to the raw data
intersect(colnames(raw_original), colnames(raw_new))

# combine datasets
combined_data <- merge(raw_original, raw_new, 
                       by = c("goat_id", "goat_name", "collar_id", 
                              "timestamp_utc", "timestamp_local", "longitude", "latitude", 
                              "fix_type", "data_source", "date_local", "outlier"),
                       all = TRUE)

#reset fix_id to account for new data
rownames(combined_data) <- NULL
combined_data$fix_id <- as.numeric(rownames(combined_data))

str(combined_data)

# create new columns to format names to match required for ctmm based on Movebank critera:
combined_data$individual.local.identifier <- combined_data$goat_id
combined_data$location.lat <- combined_data$latitude
combined_data$location.long <- combined_data$longitude
combined_data$timestamp <- combined_data$timestamp_utc


#................................................

# Convert collar data to ctmm telemetry object, and dont include screened outliers
# must separate the datasets or will have convert issues with hdop and vdop values because one dataset has pdop and the other dop
tel_data <- as.telemetry(combined_data[combined_data$data_source == 1,], mark.rm = FALSE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id", "data_source", "outlier"))


# convert ctmm telemetry output list into a df
tel_df <- do.call(rbind, tel_data)



# Convert collar data to ctmm telemetry object, and dont include screened outliers
tel_data2 <- as.telemetry(combined_data[combined_data$data_source == 2,], mark.rm = FALSE,
                          keep = c("fix_id", "goat_id", "goat_name", "collar_id", "data_source", "outlier"))

# convert ctmm telemetry output list into a df
tel_df2 <- do.call(rbind, tel_data2)



#............................................................
# combine telemetry data together and sort by fix_id

telemetry_df <- bind_rows(tel_df, tel_df2)
telemetry_df <- telemetry_df[order(telemetry_df$fix_id),]


#............................................................
# merge main df with telemetry data

# check for same column names to avoid duplication,
intersect(colnames(combined_data), colnames(telemetry_df))

# add telemetry data to main df
combined_data <- merge(combined_data, telemetry_df, by = c("fix_id", "goat_id", "goat_name", "collar_id", "longitude",  "latitude", "timestamp", "data_source", "outlier"), all = TRUE)
str(combined_data)

#data carpentry
combined_data <- combined_data[order(combined_data$fix_id),]
combined_data$fix_type <- as.factor(combined_data$fix_type)

str(combined_data)

save(combined_data, file = "./data/goat/prep/combined_data.rda")
load("./data/goat/prep/combined_data.rda")





#............................................................
# merge tel_data together for a single list to be used in outlie filtering

# check which goats are the same in both lists
intersect(names(tel_data), names(tel_data2))

# i.e., extract the df from list2, merge it to the df in list1
tel_data[["CA01"]] <- merge(tel_data[["CA01"]], tel_data2[["CA01"]], all = TRUE)
tel_data[["CA04"]] <- merge(tel_data[["CA04"]], tel_data2[["CA04"]], all = TRUE)
tel_data[["CA08"]] <- merge(tel_data[["CA08"]], tel_data2[["CA08"]], all = TRUE)
tel_data[["CA09"]] <- merge(tel_data[["CA09"]], tel_data2[["CA09"]], all = TRUE)
tel_data[["CA11"]] <- merge(tel_data[["CA11"]], tel_data2[["CA11"]], all = TRUE)
tel_data[["CA12"]] <- merge(tel_data[["CA12"]], tel_data2[["CA12"]], all = TRUE)

# then order the df by fix_id
tel_data[["CA01"]] <- tel_data[["CA01"]] [order(tel_data[["CA01"]][["fix_id"]]),]
tel_data[["CA04"]] <- tel_data[["CA04"]] [order(tel_data[["CA04"]][["fix_id"]]),]
tel_data[["CA08"]] <- tel_data[["CA08"]] [order(tel_data[["CA08"]][["fix_id"]]),]
tel_data[["CA09"]] <- tel_data[["CA09"]] [order(tel_data[["CA09"]][["fix_id"]]),]
tel_data[["CA11"]] <- tel_data[["CA11"]] [order(tel_data[["CA11"]][["fix_id"]]),]
tel_data[["CA12"]] <- tel_data[["CA12"]] [order(tel_data[["CA12"]][["fix_id"]]),]

str(tel_data[["CA08"]])


save(tel_data, file = "./data/goat/prep/tel_data_merged.rda")
load("./data/goat/prep/tel_data_merged.rda")



# clean environment
rm(list = ls())
gc()






