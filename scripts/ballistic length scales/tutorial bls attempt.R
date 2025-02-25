#This file contains the code to calculate the ballistic length scale across a chosen window size for an individual
#Results presented for coyote pair PEC068 and PEC088 in Fig. 1E

#load libraries
library(tidyverse)
library(dplyr)
library(iterators)
library(parallel)
library(foreach)
library(ctmm)
library(tidyr)
library(reshape2)
# library(rowr)
library(lubridate)

#-------------------------------------------------------
#-- function: get_bls
#This function calculates the bls for a specified window

#You need to input the following:
#data: movement data from movebank

# import data from 02_outlier_detection.r
dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat = dat[c(dat$year == "2022" & dat$individual.local.identifier == "30548"),]



get_bls <- function(data) {
  
  # data <- bind_rows(data)
  
  #resample data to correct for shortest time interval 
  # Create a new timestamp column -> round down to the nearest minute and drop the seconds in the timestamp
  # bin the timestamps into 1-minute intervals via cut() 
  dat <- dat %>% 
    # Create a new column 'timestamp_rs' by resampling the 'timestamp' column to 1-minute intervals
    # `cut` bins the timestamps into 1-minute intervals
    mutate(timestamp_rs = sapply(ymd_hms(timestamp), cut, 
                                 breaks = "1 min")) %>% #special feature in lubrdiate pkg!
    
    # Remove duplicate rows based on the new 'timestamp_rs' column
    # .keep_all = "TRUE" ensures that all other columns are kept in the resulting dataframe
    distinct(timestamp_rs, .keep_all = "TRUE") %>% #collapse to individual resampled time points
    
    # Drop the original 'timestamp' column
    dplyr::select(-c("timestamp")) #%>%
    
    # # Rename 'timestamp_rs' to 'timestamp' to match the original column name
    # rename("timestamp" = timestamp_rs)
  
  # combine_data=as.telemetry(bind_rows(dat))
  tel.dat=as.telemetry(dat)
  combine_data <- tel.dat
  
  
  
  start <- combine_data$timestamp[1]
  stop <- tail(combine_data,1)$timestamp
  
  guess <- ctmm.guess(combine_data, interactive = FALSE)
  fit <- ctmm.select(combine_data, guess, trace = 2, cores = 2)
  fitsum <- summary(fit, units=FALSE) #summary of fit object
  
  #extract values to calculate bls from the model object
  low_tau_p <- fitsum$CI[2,][[1]]
  est_tau_p <- fitsum$CI[2,][[2]]
  high_tau_p <- fitsum$CI[2,][[3]]
  
  low_tau_v <- fitsum$CI[3,][[1]]
  est_tau_v <- fitsum$CI[3,][[2]]
  high_tau_v <- fitsum$CI[3,][[3]]
  
  sigma_p <- ctmm:::area.covm(fit$sigma)
  
  # ballistic length scale: sqrt(tau_v/tau_p * sigm_p)
  
  bls_low <- sqrt((low_tau_v/low_tau_p) *  sigma_p)
  bls_mid <- sqrt((est_tau_v/est_tau_p) *  sigma_p)
  bls_high <- sqrt((high_tau_v/high_tau_p) *  sigma_p)
  
  print(cbind(start, stop, low_tau_p, est_tau_p, high_tau_p, low_tau_v, est_tau_v, high_tau_v,
              sigma_p, bls_low, bls_mid, bls_high))
  
  return(cbind(start, stop, low_tau_p, est_tau_p, high_tau_p, low_tau_v, est_tau_v, high_tau_v,
               sigma_p, bls_low, bls_mid, bls_high))
  
}

#-----------------------------------------------------------------------------------------
#Example of calclating BLS

name1 = "30548" #enter name of your individual here
ind1 <- dat %>%
  filter(individual.local.identifier==name1)

ind1_day=split(combine_data, cut(strptime(paste(ind1$timestamp), format="%Y-%m-%d %H:%M:%S"),"days"),
               drop=TRUE) #separate the data into each days; this allows for applying bls over windows of certain duration 

#calculate the bls in 60 day frames over the time series (can vary window size)
# bls <- rollApply(data=ind1_day, window = 60, #choose the window size 
#                  fun = get_bls, align = "left", minimum=60) 
# rowr package doesnt exist anymore. using zoo package

bls <- rollApply(data=ind1_day, window = 60, #choose the window size 
                 fun = get_bls, align = "left", fill = NA, by.column = FALSE, partial = FALSE)



# Install and load the zoo package if you haven't already
install.packages("zoo")
library(zoo)

# Assuming ind1_day is your dataframe and get_bls is your function
bls <- rollapply(data = ind1_day, width = 60, FUN = get_bls, align = "left", fill = NA, by.column = FALSE, partial = FALSE)


#shape and tidy dataframe
bls_dat <- bls %>%
  t() %>%
  as.data.frame() %>%
  rename("start"=V1, "stop"=V2, 
         "low_tau_p"=V3, "est_tau_p"=V4, "high_tau_p"=V5, 
         "low_tau_v"=V6, "est_tau_v"=V7, "high_tau_v"=V8,
         "sigma_p"=V9, "bls_low"=V10, "bls_mid"=V11, "bls_high"=V12) %>%
  mutate(start=as.POSIXct(as.numeric(start), origin="1970-01-01", tz="UTC"), #convert 10 digit datetime back to ymd_hms
         stop=as.POSIXct(as.numeric(stop), origin="1970-01-01", tz="UTC")) #convert 10 digit datetime back to ymd_hms













# the rest only applies if there are duplicate timestamps
#%>% #special feature in lubrdiate pkg!
# distinct(timestamp_rs, .keep_all = "TRUE") %>% #collapse to individual resampled time points
# dplyr::select(-c("timestamp")) %>%
# rename("timestamp" = timestamp_rs)
#Create a new timestamp column -> round down to the nearest minute via format() and drop the seconds in the timestamp
# DATA$timestamp_rs <- as.POSIXct(format(DATA$timestamp, "%Y-%m-%d %H:%M"))



#_______________________________________________________________
# ATTEMPT 2 ----

# Based on the script by:
# https://github.com/anagkrish/encounter_homerangeshift/blob/v1.0.0/encounter_bls.R

#This file contains the code to calculate the ballistic length scale across a chosen window size for an individual
#Results presented for coyote pair PEC068 and PEC088 in Fig. 1E

#load libraries
library(tidyverse)
library(dplyr)
library(iterators)
library(parallel)
library(foreach)
library(ctmm)
library(tidyr)
library(reshape2)
# library(rowr) # removed from CRAN
library(lubridate)
library(beepr)



#.........................................................................
# collar data
load("data/collar_data/collar_data_20241123.rda")

# identify the goats that were tracked during the wildfire
goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
# subset to fire goats
fire_goats <- collar_data[collar_data$goat_name %in% goats,] # 43941 obs

# format names to match required for ctmm based on Movebank critera:
fire_goats$individual.local.identifier <- paste(fire_goats$goat_name, fire_goats$year, sep = "_") # format name that includes year
fire_goats = plyr::rename(fire_goats, c('latitude' = 'location.lat',
                                        'longitude' = 'location.long'))

# Define the Crater Creek wildfire date range (July 22 to October 26)
fire_start <- "07-22"
fire_end <- "10-26"
#subset goat data based on the date range of the crater creek wildfire across all years
fire_goats <- fire_goats[fire_goats$month_day >= fire_start & fire_goats$month_day <= fire_end, ] #10376 obs

# goat and year identifier
goat_identifier <- unique(fire_goats$individual.local.identifier)



#.........................................................................
# function: get_bls ----
#.........................................................................

#This function calculates the bls for a specified window

#You need to input the following:
#data: movement data from movebank

# name1 = "kid_rock_2019"
# ind1 <- fire_goats %>%
#   filter(individual.local.identifier==name1)
# 
# DATA <- ind1

get_bls <- function(DATA) {
  
  DATA <- DATA  %>% 
  mutate(timestamp_rs = sapply(ymd_hms(timestamp), cut, 
                               breaks = "1 min")) %>% #special feature in lubrdiate pkg!
    distinct(timestamp_rs, .keep_all = "TRUE") %>% #collapse to individual resampled time points
    dplyr::select(-c("timestamp")) %>%
    rename("timestamp" = timestamp_rs)

  combine_data <- as.telemetry(data) 

    # combine multiple df/lists and convert into ctmm telemetry object
    # combine_data=as.telemetry(bind_rows(DATA))
    # combine_data <- as.telemetry(data) # did not combine data from separate df for different individuals
    
  start <- combine_data$timestamp[1]
  stop <- tail(combine_data,1)$timestamp
  
  guess <- ctmm.guess(combine_data, interactive = FALSE)
  fit <- ctmm.select(combine_data, guess, trace = 2, cores = 2)
  fitsum <- summary(fit, units=FALSE) #summary of fit object
  
    #extract values to calculate bls from the model object
    low_tau_p <- fitsum$CI[2,][[1]]
    est_tau_p <- fitsum$CI[2,][[2]]
    high_tau_p <- fitsum$CI[2,][[3]]
    
    low_tau_v <- fitsum$CI[3,][[1]]
    est_tau_v <- fitsum$CI[3,][[2]]
    high_tau_v <- fitsum$CI[3,][[3]]
    
    sigma_p <- ctmm:::area.covm(fit$sigma)
    
    # ballistic length scale: sqrt(tau_v/tau_p * sigm_p)
    
    b_l_s_low <- sqrt((low_tau_v/low_tau_p) *  sigma_p)
    b_l_s_mid <- sqrt((est_tau_v/est_tau_p) *  sigma_p)
    b_l_s_high <- sqrt((high_tau_v/high_tau_p) *  sigma_p)
    
    print(cbind(start, stop, low_tau_p, est_tau_p, high_tau_p, low_tau_v, est_tau_v, high_tau_v,
                sigma_p, b_l_s_low, b_l_s_mid, b_l_s_high))
    
    return(cbind(start, stop, low_tau_p, est_tau_p, high_tau_p, low_tau_v, est_tau_v, high_tau_v,
                 sigma_p, b_l_s_low, b_l_s_mid, b_l_s_high))
    
}







#.........................................................................
# #Example of calculating BLS ----
#.........................................................................


# name1 = "kid_rock" #enter name of your individual here
name1 = "kid_rock_2019"

ind1 <- fire_goats %>%
  filter(individual.local.identifier==name1)

#separate the data into each days; this allows for applying bls over windows of certain durations 
ind1_day <- split(ind1, cut(strptime(paste(ind1$timestamp), format="%Y-%m-%d %H:%M:%S"),"days"),
               drop=TRUE)

#calculate the bls in 60 day frames over the time series (can vary window size)
bls <- rollApply(data=ind1_day, window = 60, #choose the window size
                 fun = get_bls, align = "left", minimum=60)

# rowr package doesnt exist anymore .-. using zoo package as a workaround
bls <- rollapply(data = ind1_day, width = 3,  #choose the window size 
                 FUN = get_bls, align = "left", 
                 fill = NA, by.column = FALSE, partial = FALSE)



#choose the window size




