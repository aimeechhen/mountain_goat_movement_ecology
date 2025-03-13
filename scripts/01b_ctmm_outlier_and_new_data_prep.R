
library(tidyverse)
library(ctmm)
library(lubridate)

# refer to: https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf
# refer to: https://www.biorxiv.org/content/biorxiv/early/2021/07/24/2020.06.12.130195.full.pdf




# LONG VERSION OF DATA CLEANING

# import data ----
new_collar <- read.csv("data/collar_data/raw_collar/Cathedral Goat locations Sept 2023 through Feb 10 2025.csv") %>%
  janitor::clean_names() #%>% #remove unnecessary characters ("[", " ", etc.) and replace with "_" 
  # as.data.frame()

# acq_time_utc = GPS fix acquisition time (actual recorded fix time)
# scts_utc = data is sent or received to Iridium satellite network (origin column), i.e., sent to system .-. after acquisition time

# drop animal column or error will occur when trying to convert to a ctmm telemetry object, and drop columns that provide duplicate information such as collar number
new_collar <- subset(new_collar, select = -c(animal, collar_name, scts_utc))

new_collar$acq_time_utc <- as.POSIXct(new_collar$acq_time_utc)
new_collar$collar_id <- as.factor(new_collar$collar_id)
new_collar$data_type <- "new_data"

# check for duplicates
new_collar[duplicated(new_collar[, c("acq_time_utc", "latitude_deg", "longitude_deg", "collar_id")]), ] # none
# cannot add goat_info data or get warnings of duplicates


#format names to match required for ctmm based on Movebank critera:
dat = plyr::rename(new_collar, c('collar_id' = 'individual.local.identifier',
                                 'acq_time_utc' = 'timestamp',
                                 'latitude_deg' = 'location.lat', 
                                 'longitude_deg' = 'location.long'))

# Convert collar data to telemetry object
tel_data <- as.telemetry(dat)

# if no calibration data then assign RMS UERE
uere(tel_data) <- 10
# calibration data = like if the collar is not moving i.e., morality signal
# hdop is unitless but if you need to put some kind of units its like hdop=1 is like 10 mins from that stationary signal


outliers_rowname <- data.frame()

#.................................................................................
# 30548, goatzilla ----
#Extract an example individual from the raw dataset
goat <- tel_data[[1]]
#Checking for outliers
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(goat)
#Plot a check for outliers
OUTLIERS_RAW <- outlie(goat, family="serif") # units: speed m/s, distance m
plot(OUTLIERS_RAW, family="serif") # inspect plot units
dev.off()

# units from the plot are cm/s but in the output is in m/s
# # create a speed cm/s to m/s to match the plot output numerical values for easier interpretation when flagging
# OUTLIERS_RAW$speed_cm_s <- OUTLIERS_RAW$speed * 100
# # convert distance m to km to match the plot units
# OUTLIERS_RAW$distance_km <- OUTLIERS_RAW$distance / 1000
# flag points above 10 cm/s, create an outlier column to flag, if units are above 10, give them a 1 as flagged, or 0 as unflagged
OUTLIERS_RAW$flag_outlier <- ifelse(OUTLIERS_RAW$speed >= 0.10, 1, 0)

# display all the flagged points
OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,] # this includes the point below
# how many points are there? 
nrow(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])
# list all the rownames
flagged <- rownames(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])

# location checks, point 485 is included from above
# plot(goat[-10:10 + which(goat$x >= 4000 & goat$x <= 6000 &
#                            goat$y <= -5000), ])
# test <- goat[-10:10 + which(goat$x >= 4000 & goat$x <= 6000 &
#                               goat$y <= -5000), ] #NOT SURE WHAT OT MAKE OF THIS, NEED EXTRA HELP
# 
# 
# plot(goat[-10:10 + which(goat$x >= 6000 &
#                            goat$y >= -1000), ])
# test <- goat[-10:10 + which(goat$x >= 6000 &
#                               goat$y >= -1000), ]
# rowname 485

# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]

#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")

# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)

# # do i keep checking until no more points at above 10?
# OUTLIERS_CHECK$flag_outlier <- ifelse(OUTLIERS_CHECK$speed >= 0.10, 1, 0)
# 
# # display all the flagged points
# OUTLIERS_CHECK[OUTLIERS_CHECK$flag_outlier == 1,] # this includes the point below
# # how many points are there? 
# nrow(OUTLIERS_CHECK[OUTLIERS_CHECK$flag_outlier == 1,])
# # list all the rownames
# flagged <- rownames(OUTLIERS_CHECK[OUTLIERS_CHECK$flag_outlier == 1,])
# test <- test[!(rownames(test) %in% flagged), ]
# #Checking for outliers after they have been removed
# par(mfrow=c(1,3))
# #Visualisation of the individual tracking data
# plot(test)
# #Plot a check for outliers
# OUTLIERS_CHECK3 <- outlie(test, family="serif")
# plot(OUTLIERS_CHECK3, family="serif")




#.................................................................................
# 30561, selena_goatmez ----
#Extract an example individual from the raw dataset
goat <- tel_data[[2]]
#Checking for outliers
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(goat)
#Plot a check for outliers
OUTLIERS_RAW <- outlie(goat, family="serif") # units: speed m/s, distance m
plot(OUTLIERS_RAW, family="serif") # inspect plot units
dev.off()


OUTLIERS_RAW$flag_outlier <- ifelse(OUTLIERS_RAW$speed >= 0.10, 1, 0)

# display all the flagged points
OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,] # this includes the point below
# how many points are there? 
nrow(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])
# list all the rownames
flagged <- rownames(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])

# location check
# plot(goat[-10:10 + which(goat$x >= 4000 & goat$x <= 6000 &
#                            goat$y <= -5000), ])


# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]

#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")

# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)






#.................................................................................
# 30575, goatmother ----
#Extract an example individual from the raw dataset
goat <- tel_data[[3]]
#Checking for outliers
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(goat)
#Plot a check for outliers
OUTLIERS_RAW <- outlie(goat, family="serif") # units: speed m/s, distance m
plot(OUTLIERS_RAW, family="serif") # inspect plot units
dev.off()
# look for that tail
plot(OUTLIERS_RAW, family="serif") # inspect plot units
# still 10~
OUTLIERS_RAW$flag_outlier <- ifelse(OUTLIERS_RAW$speed >= 0.10, 1, 0)

# display all the flagged points
OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,] # this includes the point below
# how many points are there? 
nrow(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])
# list all the rownames
flagged <- rownames(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])

# location check
plot(goat[-10:10 + which(goat$x <= 0 & 
                           goat$y >= 5000), ])
test <- goat[-10:10 + which(goat$x <= 0 & 
                              goat$y >= 5000), ]
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")
# test <- goat[!(rownames(goat) %in% 9072), ] 
# add this row number to flagged
flagged <- rownames(goat[goat$x <= 0 & 
                           goat$y >= 5000, ])

# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]

#replot to check without flagged outlier
#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")



# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)




#.................................................................................
# 30613, goatileo ----
#Extract an example individual from the raw dataset
goat <- tel_data[[4]]
#Checking for outliers
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(goat)
#Plot a check for outliers
OUTLIERS_RAW <- outlie(goat, family="serif") # units: speed m/s, distance m
plot(OUTLIERS_RAW, family="serif") # inspect plot units
dev.off()
# take a closer look for that "tail"
plot(OUTLIERS_RAW, family="serif")
# there isnt really a tail so going to use ~10 again
OUTLIERS_RAW$flag_outlier <- ifelse(OUTLIERS_RAW$speed >= 0.10, 1, 0)

# display all the flagged points
OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,] # this includes the point below
# how many points are there? 
nrow(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])
# list all the rownames
flagged <- rownames(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])

# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]


#replot to check without flagged outlier
#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")
dev.off()


# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)


# location check
# goatileo
plot(goat[-10:10 + which(goat$x >= -4000 & goat$x <= -3000 &
                           goat$y >= 5000), ])
test <- goat[-10:10 + which(goat$x >= -4000 & goat$x <= -3000 &
                              goat$y >= 5000), ]

# check to see what it looks lik without the outlier
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")

# add this row number to flagged
flagged <- rownames(goat[goat$x >= -4000 & goat$x <= -3000 &
                           goat$y >= 5000, ])


# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]

#replot to check without flagged outlier
#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")

# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)






#.................................................................................
# 30642, toats_mcgoats ----
#Extract an example individual from the raw dataset
goat <- tel_data[[5]]
#Checking for outliers
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(goat)
#Plot a check for outliers
OUTLIERS_RAW <- outlie(goat, family="serif") # units: speed m/s, distance m
plot(OUTLIERS_RAW, family="serif") # inspect plot units
dev.off()
# take a closer look for that "tail"
plot(OUTLIERS_RAW, family="serif")
# there isnt really a tail so going to use ~10 again
OUTLIERS_RAW$flag_outlier <- ifelse(OUTLIERS_RAW$speed >= 0.10, 1, 0)

# display all the flagged points
OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,] # this includes the point below
# how many points are there? 
nrow(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])
# list all the rownames
flagged <- rownames(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])

# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]


#replot to check without flagged outlier
#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")
dev.off()



# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)




#.................................................................................
# 30648, vincent_van_goat ----
#Extract an example individual from the raw dataset
goat <- tel_data[[6]]
#Checking for outliers
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(goat)
#Plot a check for outliers
OUTLIERS_RAW <- outlie(goat, family="serif") # units: speed m/s, distance m
plot(OUTLIERS_RAW, family="serif") # inspect plot units
dev.off()
# take a closer look for that "tail"
plot(OUTLIERS_RAW, family="serif")
# there isnt really a tail so going to use ~10 again
OUTLIERS_RAW$flag_outlier <- ifelse(OUTLIERS_RAW$speed >= 0.10, 1, 0)

# display all the flagged points
OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,] # this includes the point below
# how many points are there? 
nrow(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])
# list all the rownames
flagged <- rownames(OUTLIERS_RAW[OUTLIERS_RAW$flag_outlier == 1,])

# exclude all the points that are flagged
test <- goat[!(rownames(goat) %in% flagged), ]


#replot to check without flagged outlier
#Checking for outliers after they have been removed
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")
dev.off()


# location check
# plot the points [-#:#] before and after the criteria i'm looking for
# speed over 13 cm/s and distance under 2 km
# test <- OUTLIERS_RAW[OUTLIERS_RAW$speed_cm_s >= 13 & OUTLIERS_RAW$distance_km <= 2,]
# test <- OUTLIERS_RAW[OUTLIERS_RAW$speed_cm_s >= 15,]

#remove the points based on rowname
# test <- goat[!(rownames(goat) %in% c("1491", "9565")), ] # circle 1, speed over 20 cm/s
# test <- goat[!(rownames(goat) %in% c("1512", "1542")), ] # circle 2, distance over 12 km
# test <- goat[!(rownames(goat) %in% c("3255", "1820")), ] # circle 3, speed over 13 cm/s and distance under 2 km
# test <- goat[!(rownames(goat) %in% 9819), ] # speed over 17 cm/s
# test <- goat[!(rownames(goat) %in% c("9674", "1873")), ] # speed over 16 cm/s
# test <- goat[!(rownames(goat) %in% c("7259", "1563")), ] # speed over 15 cm/s
# 
# test <- goat[-10:10 + which(rownames(goat) %in% c("1512", "1542")), ] # circle 2, distance over 12 km

# vincent van goat
# point 1
dev.off()
plot(goat[-10:10 + which(goat$x >= -2000 & goat$y <= -4000), ])
test <- goat[-10:10 + which(goat$x >= -2000 & goat$y <= -4000), ]


# check to see what it looks lik without the outlier
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")

# add this row number to flagged
flagged <- rownames(goat[goat$x >= -2000 & goat$y <= -4000, ])
# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)


# point 2
plot(goat[goat$x >= 2000 & goat$x <= 4000 &
            goat$y >= 5000 & goat$y <= 6000, ])
plot(goat[-10:10 + which(goat$x >= 2000 & goat$x <= 4000 &
                           goat$y >= 5000 & goat$y <= 6000), ])
test <- goat[-10:10 + which(goat$x >= 2000 & goat$x <= 4000 &
                              goat$y >= 5000 & goat$y <= 6000), ]
# check to see what it looks lik without the outlier
par(mfrow=c(1,3))
#Visualisation of the individual tracking data
plot(test)
#Plot a check for outliers
OUTLIERS_CHECK <- outlie(test, family="serif")
plot(OUTLIERS_CHECK, family="serif")

# add this row number to flagged
flagged <- rownames(goat[goat$x >= 2000 & goat$x <= 4000 &
                           goat$y >= 5000 & goat$y <= 6000, ])

# add to flagged list for main df
outliers_rowname <- rbind(outliers_rowname, flagged)









#/////////////////////////////////////////////////////////////////////
# SHORT VERSION OF DATA CLEANING ----
#/////////////////////////////////////////////////////////////////////


library(dplyr)
library(ctmm)


new_collar <- read.csv("data/collar_data/raw_collar/Cathedral Goat locations Sept 2023 through Feb 10 2025.csv") %>%
  janitor::clean_names() #%>% #remove unnecessary characters ("[", " ", etc.) and replace with "_" 
# as.data.frame()

# acq_time_utc = GPS fix acquisition time (actual recorded fix time)
# scts_utc = data is sent or received to Iridium satellite network (origin column), i.e., sent to system .-. after acquisition time

# drop animal column or error will occur when trying to convert to a ctmm telemetry object, and drop columns that provide duplicate information such as collar number
new_collar <- subset(new_collar, select = -c(animal, collar_name))
new_collar$acq_time_utc <- as.POSIXct(new_collar$acq_time_utc)
# new_collar$acq_time_utc <- as.POSIXct(new_collar$acq_time_utc, format = "%Y-%m-%d %H:%M")
new_collar$collar_id <- as.factor(new_collar$collar_id)
new_collar$data_type <- "new_data"
# check for duplicates
new_collar[duplicated(new_collar[, c("acq_time_utc", "latitude_deg", "longitude_deg", "collar_id")]), ] # none
# cannot add goat_info data or get warnings of duplicates


#format names to match required for ctmm based on Movebank critera:
dat = plyr::rename(new_collar, c('collar_id' = 'individual.local.identifier',
                                 'acq_time_utc' = 'timestamp',
                                 'latitude_deg' = 'location.lat', 
                                 'longitude_deg' = 'location.long'))

# duplicates <- dat[duplicated(dat[c("timestamp", "location.lat", "location.long")]), ]

# Convert collar data to telemetry object
tel_data <- as.telemetry(dat)

# if no calibration data then assign RMS UERE
uere(tel_data) <- 10
# calibration data = like if the collar is not moving i.e., morality signal
# hdop is unitless but if you need to put some kind of units its like hdop=1 is like 10 mins from that stationary signal

tel_df <- do.call(rbind, lapply(names(tel_data), function(id) {
  df <- tel_data[[id]]  # Extract each dataframe
  df$collar_id <- id    # Assign list name as collar_id
  return(df)
}))


telem.out = outlie(data = tel_data, plot = TRUE)
telem.x = plyr::ldply(telem.out, rbind)
telemetry.check = cbind(dat, telem.x)

# telem.out = outlie(data = tel_data, plot = TRUE)
# names(telem.out) <- names(tel_data)
# # Combine data frames using do.call and add a column based on the list names
# telem.x <- do.call(rbind, lapply(names(telem.out), function(x) {
#   df <- telem.out[[x]]
#   # df$goat_name <- x  # Add the name of the goat as a new column
#   return(df)
# }))

telemetry.check = cbind(dat, telem.x)

# since i did the checks above and using 10 for all of them, clean this up and do this
telemetry.check$flag_outlier <- ifelse(telemetry.check$speed >= 0.10, 1, 0)




#...................................................................................
## locations check ----
goat <- tel_data[[3]]
flagged <- rownames(goat[goat$x <= 0 & 
                           goat$y >= 5000, ]) #9072
telemetry.check$flag_outlier[rownames(telemetry.check) %in% flagged] <- 1


goat <- tel_data[[4]]
flagged <- rownames(goat[goat$x >= -4000 & goat$x <= -3000 &
                           goat$y >= 5000, ]) #8003
telemetry.check$flag_outlier[rownames(telemetry.check) %in% flagged] <- 1


goat <- tel_data[[6]]
flagged <- rownames(goat[goat$x >= -2000 & goat$y <= -4000, ]) #4404
telemetry.check$flag_outlier[rownames(telemetry.check) %in% flagged] <- 1

flagged <- rownames(goat[goat$x >= 2000 & goat$x <= 4000 &
                           goat$y >= 5000 & goat$y <= 6000, ]) # 8599
telemetry.check$flag_outlier[rownames(telemetry.check) %in% flagged] <- 1

sum(telemetry.check$flag_outlier) #126 flagged
write.csv(telemetry.check, file = "data/collar_data/flagged_new_collar_data_20250218.csv", row.names = FALSE)



#...................................................................................

# import flagged outlier data

telemetry.check <- read.csv(file = "data/collar_data/flagged_new_collar_data_20250218.csv")
telemetry.check$timestamp <- as.POSIXct(telemetry.check$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")

# dropped all flagged outliers from the dataset
new_collar <- telemetry.check[telemetry.check$flag_outlier == 0,] # 126 fixes not included

# revert back to normal column names
new_collar = plyr::rename(new_collar, c('individual.local.identifier' = 'collar_id',
                                      # 'timestamp' = 'acq_time_utc',
                                      'location.lat' = 'latitude', 
                                      'location.long' = 'longitude'))


#save dataset, dropped all flagged outliers from the dataset
save(new_collar, file = "data/collar_data/new_collar_data_20250218.rda")
load("data/collar_data/new_collar_data_20250218.rda")


#/////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////


# CHECK VARIOGRAMS ON NEW DATA









#______________________________________________________________________
# Combine original and new cleaned data ----
#______________________________________________________________________

# cleaned new data prep
# import cleaned new collar data ----
load("data/collar_data/new_collar_data_20250218.rda")
new_collar$collar_id <- as.factor(new_collar$collar_id)
# drop columns that isn't needed 
new_collar <- subset(new_collar, select = c(collar_id, timestamp, latitude, longitude, data_type))

# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
new_collar <- merge(new_collar, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)

# str(new_collar)
new_collar$goat_name <- as.factor(new_collar$goat_name)
# new_collar$timestamp <- as.POSIXct(new_collar$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
new_collar$date <- as.Date(new_collar$timestamp)
new_collar$year <- year(new_collar$timestamp)
new_collar$month <- month(new_collar$timestamp, label = FALSE) #label = false for numerical month
new_collar$day <- day(new_collar$timestamp)
new_collar$month_day <- format(new_collar$timestamp, "%m-%d")
new_collar$doy <- yday(new_collar$timestamp) #day of the year



#.........................................................................
# original collar data ----
#.........................................................................

# import original collar data
load("data/collar_data/collar_data_20241123.rda")
# specify which dataset its from
collar_data$data_type <- "original_data"

# update dataset to reflect the name changes 2025-02
collar_data$goat_name[collar_data$goat_name == "kid_rock" & collar_data$collar_id == "30561"] <- "selena_goatmez"
collar_data$goat_name[collar_data$goat_name == "rocky" & collar_data$collar_id == "30613" ] <- "goatileo"
collar_data$goat_name[collar_data$goat_name == "vertigoat" & collar_data$collar_id == "30567" ] <- "goatbriella"
collar_data$goat_name[collar_data$goat_name == "billy" & collar_data$collar_id == "30636" ] <- "ryan_goatsling"


# subset to the 6 goats of interest
collar_data <- collar_data[collar_data$goat_name %in% goats,]
collar_data$goat_name <- as.factor(collar_data$goat_name)


#.........................................................................
# combine data for full data ----
#.........................................................................

# combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
combined_data <- dplyr::bind_rows(collar_data, new_collar) # 55,457 fixes for 6 mountain goats

# check for duplicates in the combined df, there probably (and should be) is because they have ~ month overlapping 
# full_data[duplicated(full_data[, c("collar_id", "timestamp", "latitude", "longitude")]), ]
# no duplicates, but the timestamps are off by like ~45 minutes and the position is fairly close

#clean up environment
rm(collar_data, goat_info, dat)


# reassigning back to fire_goats for workflow, this now contains original and new fire goat collar data
# fire_goats <- new_collar
#sort data by goat and timestamp
combined_data <- combined_data[order(combined_data$collar_id, combined_data$timestamp), ]

saveRDS(combined_data, file = "./data/collar_data/full_combined_data_20250309.rds")



#...........................................................................
## fire period ----
#...........................................................................

# import full combined dataset
combined_data <- readRDS("./data/collar_data/full_combined_data_20250309.rds")

# Define the wildfire date range as per bc wildfire dates
# July 22 to October 26
fire_start <- "07-22"
fire_end <- "10-26"

#subset goat data based on the date range of the crater creek wildfire across all years
goat_data <- combined_data[combined_data$month_day >= fire_start & combined_data$month_day <= fire_end, ] # 13884 fixes

# check for NAs
goat_data[!complete.cases(goat_data$longitude, goat_data$latitude, goat_data$timestamp, goat_data$goat_name), ] #97
# remove the rows that are full NA rows
goat_data <- goat_data[complete.cases(goat_data$longitude, goat_data$latitude, goat_data$timestamp, goat_data$goat_name), ]

# fire period total dataset: 13,787 fixes
# yearly fixes total:
goat_data_2019 <- goat_data[goat_data$year == "2019", ] # 1972
goat_data_2020 <- goat_data[goat_data$year == "2020", ] # 2353
goat_data_2021 <- goat_data[goat_data$year == "2021", ] # 2201
goat_data_2022 <- goat_data[goat_data$year == "2022", ] # 2200
goat_data_2023 <- goat_data[goat_data$year == "2023", ] # 2898
goat_data_2024 <- goat_data[goat_data$year == "2024", ] # 2163





#organize columns
goat_data <- relocate(goat_data, c(collar_id, goat_name, goat_id), .before = timestamp)

write.csv(goat_data, file = "./data/combined_goat_data_fire_period_all_years.csv", row.names = FALSE)



#///////////////////////////////////////////////////////////////////////
# Plot ----
#///////////////////////////////////////////////////////////////////////


# Plotting collar data for visualization
#https://movevis.org/