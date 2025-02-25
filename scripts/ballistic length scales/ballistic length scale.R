
# Based on the script by:
# https://github.com/anagkrish/encounter_homerangeshift/blob/v1.0.0/encounter_bls.R

# import data from 02_outlier_detection.r
# dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
load("data/collar_data/collar_data_20241123.rda")
collar_data$timestamp = as.POSIXct(collar_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
collar_data$month_day = as.POSIXct(collar_data$month_day, format = "%m-%d")
collar_data$date = as.Date(collar_data$date, "%Y-%m-%d")



dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
dat = dat[dat$year == "2022",]

dat = dat[dat$individual.local.identifier == '30548',]

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
library(rowr)
library(lubridate)

#-------------------------------------------------------
#-- function: get_bls
#This function calculates the bls for a specified window

#You need to input the following:
#data: movement data from movebank

get_bls <- function(dat) {
  
  # DATA <- bind_rows(DATA)
  
  #resample data to correct for shortest time interval 
  dat <- dat %>% 

    mutate(timestamp_rs = sapply(ymd_hms(timestamp), cut, 
                                 breaks = "1 min")) %>% #special feature in lubrdiate pkg!
    distinct(timestamp_rs, .keep_all = "TRUE") %>% #collapse to individual resampled time points
    dplyr::select(-c("timestamp")) %>%
    rename("timestamp" = timestamp_rs)
  
  combine_data=as.telemetry(bind_rows(dat))
  
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

#-----------------------------------------------------------------------------------------
#Example of calclating BLS

name1 = "30548" #enter name of your individual here
ind1 <- dat %>%
  filter(individual.local.identifier==name1)

ind1_day=split(Pair1, cut(strptime(paste(ind1$timestamp), format="%Y-%m-%d %H:%M:%S"),"days"),
               drop=TRUE) #separate the data into each days; this allows for applying bls over windows of certain durations 

#calculate the bls in 60 day frames over the time series (can vary window size)
bls <- rollApply(data=ind1_day, window = 60, #choose the window size 
                 fun = get_bls, align = "left", minimum=60)

#shape and tidy dataframe
bls_dat <- bls %>%
  t() %>%
  as.data.frame() %>%
  rename("start"=V1, "stop"=V2, 
         "low_tau_p"=V3, "est_tau_p"=V4, "high_tau_p"=V5, 
         "low_tau_v"=V6, "est_tau_v"=V7, "high_tau_v"=V8,
         "sigma_p"=V9, "b_l_s_low"=V10, "b_l_s_mid"=V11, "b_l_s_high"=V12) %>%
  mutate(start=as.POSIXct(as.numeric(start), origin="1970-01-01", tz="UTC"), #convert 10 digit datetime back to ymd_hms
         stop=as.POSIXct(as.numeric(stop), origin="1970-01-01", tz="UTC")) #convert 10 digit datetime back to ymd_hms