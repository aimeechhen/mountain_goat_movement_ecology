
# combine raw datasets

library(dplyr)

load("./data/goat/prep/raw_original.rda")
load("./data/goat/prep/raw_new.rda")

str(raw_original)
str(raw_new)

# combine original and new data together
combined_data <- bind_rows(raw_original, raw_new)

str(combined_data)

# reorganize df
combined_data <- relocate(combined_data, c(timestamp_utc, timestamp_local, date_local), .after = collar_id)
combined_data <- relocate(combined_data, c(altitude_m, temp_c), .after = ele)
combined_data <- relocate(combined_data, dop, .before = hdop)

#sort by utc time and collar id
combined_data <- combined_data[order(combined_data$collar_id, combined_data$timestamp_utc), ]

# drop the rownames and reset it
rownames(combined_data) <- NULL

save(combined_data, file = "./data/goat/prep/combined_data.rda")
load("./data/goat/prep/combined_data.rda")


# clean environment
rm(list = ls())
gc()
