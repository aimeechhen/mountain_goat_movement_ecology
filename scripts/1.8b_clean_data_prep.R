
#.....................................................................
# data prep for analysis ----
#.....................................................................

library(lubridate)

load(file = "./data/goat/clean_data.rda")

str(clean_data)

#subset to the 6 fire goats
goat_data <- clean_data[clean_data$fire_study == 1,] # 55194

# update goat id, name and collar id, individual.local.identifier to only contain 6 levels
goat_data$goat_id <- droplevels(goat_data$goat_id)
goat_data$goat_name <- droplevels(goat_data$goat_name)
goat_data$collar_id <- droplevels(goat_data$collar_id)
goat_data$individual.local.identifier <- droplevels(goat_data$individual.local.identifier)

str(goat_data)

# make sure its in the right order
goat_data <- goat_data[order(goat_data$fix_id), ]

# how many outliers within this dataset?
sum(goat_data$outlier) #333

# add temporal attributes
goat_data$year_local <- year(goat_data$timestamp_local)
goat_data$month_local <- month(goat_data$timestamp_local, label = FALSE) #label = false for numerical month
goat_data$day_local <- day(goat_data$timestamp_local)
goat_data$month_day_local <- format(goat_data$timestamp_local, "%m-%d")
goat_data$doy_local <- yday(goat_data$timestamp_local) #day of the year

# create a column to be used for period specific analyses
goat_data$id_year <-  paste(goat_data$goat_id, goat_data$year_local, sep = "_")


#.....................................................................
# Wildfire details ----
#.....................................................................

# Define the dates for wildfire period as per bc wildfire dates
# July 22 to October 26, 2023
fire_start <- "2023-07-22"
fire_end <- "2023-10-26"

# Define the the study period based on the fire period across all years (same days across all years)
# July 22 to October 26
period_start <- "07-22"
period_end <- "10-26"

# create a column to indicate if the fix fall within the fire period, 1 = yes, 0 = no
goat_data$fire_period <- ifelse(goat_data$date_local >= fire_start & goat_data$date_local <= fire_end, 1, 0)

# create a column to indicate if the fix fall within the study period, 1 = yes, 0 = no
# study period = the fire period across all years
goat_data$study_period <- ifelse(goat_data$month_day_local >= period_start & goat_data$month_day_local <= period_end, 1, 0)

str(goat_data)

save(goat_data, file = "./data/goat/goat_data.rda")



#..................................................................................
# goat data df details

# print column number and column name side by side
data.frame(colnames(goat_data))
# sources of data
# column 1-31 tracking data + goat info
# column 32-40 outlie() data
# column 41-49 analyses/fire study prep


library(ctmm)

load(file = "./data/goat/goat_data.rda")

# drop certain columns or will have issues when converting into ctmm object. not including measurement error, refer to previous scripts for explanation and why
goat_data <- subset(goat_data, select = -c(fix_type, hdop, vdop, pdop, dop, altitude_m))

# convert to ctmm object
tel_data <- as.telemetry(raw_data, mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id", "id_year"))

# ensure each df in the list is in the correct order with matching row name
for (i in 1:length(tel_data)) {
  # order the df based on fix_id
  tel_data[[i]] <- tel_data[[i]][order(tel_data[[i]][["fix_id"]]),]
  # set the rowname to match the fix_id
  rownames(tel_data[[i]] ) <- tel_data[[i]]$fix_id
}
