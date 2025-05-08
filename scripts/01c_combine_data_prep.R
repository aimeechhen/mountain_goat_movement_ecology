
library(dplyr)
library(lubridate)


#.....................................................
# Data prep ----
#.....................................................

#load original data
load("data/collar_data/collar_data_20250505.rda")
#drop extra columns
collar_data <- subset(collar_data, select = c(collar_id, goat_name, goat_id, timestamp, longitude, latitude, HDOP, data_type))

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]

# load cleaned new collar data
load("data/collar_data/cleaned_new_collar_data_20250505.rda")
# add supp data
new_collar <- merge(new_collar, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
new_collar <- relocate(new_collar, c(goat_name, goat_id), .after = collar_id)
#drop extra columns
new_collar <- subset(new_collar, select = c(collar_id, goat_name, goat_id, timestamp, longitude, latitude, HDOP, data_type))



#______________________________________________________________________
# Combine original and cleaned new data ----
#______________________________________________________________________

combined_data <- rbind(collar_data, new_collar)

# combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
combined_data <- dplyr::bind_rows(collar_data, new_collar)
any(is.na(combined_data))

save(combined_data, file = "./data/collar_data/full_combined_data_20250505.rda")
load("./data/collar_data/full_combined_data_20250505.rda")


#....................................................................
# fire goat data ----
#....................................................................


#subset to the 6 fire goats
goat_data <- combined_data[combined_data$goat_name %in% goats,]
# for temporal attributes
goat_data$timestamp <- as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date <- as.Date(goat_data$timestamp)
goat_data$year <- year(goat_data$timestamp)
goat_data$month <- month(goat_data$timestamp, label = FALSE) #label = false for numerical month
goat_data$day <- day(goat_data$timestamp)
goat_data$month_day <- format(goat_data$timestamp, "%m-%d")
goat_data$doy <- yday(goat_data$timestamp) #day of the year

# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
goat_data <- goat_data[goat_data$month_day >= fire_start & goat_data$month_day <= fire_end, ] # 13794 fixes


# fire period total dataset: 13,787 fixes
# yearly fixes total:
goat_data_2019 <- goat_data[goat_data$year == "2019", ] # 1972
goat_data_2020 <- goat_data[goat_data$year == "2020", ] # 2353
goat_data_2021 <- goat_data[goat_data$year == "2021", ] # 2201
goat_data_2022 <- goat_data[goat_data$year == "2022", ] # 2200
goat_data_2023 <- goat_data[goat_data$year == "2023", ] # 2905
goat_data_2024 <- goat_data[goat_data$year == "2024", ] # 2163



# #re-order the data based on collar_id and timestamp
# test <- goat_data[order(goat_data$collar_id, goat_data$timestamp),]
# test <- goat_data[order(as.numeric(rownames(goat_data))),]
goat_data <- goat_data[order(goat_data$collar_id, goat_data$timestamp), ]



#save combined data cleaned fire period, all years
save(goat_data, file = "data/collar_data/fire_period_all_years_combined_data_20250505.rda")
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")



