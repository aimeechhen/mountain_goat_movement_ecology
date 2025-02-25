# sliding window v3 with window gap fix

# Script description: (Ryan) uses output from previous file to iterate each over collar and calculate home range over a 14 day moving window, advanced by 1 day to determine when each animal becomes range resident 


# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)


#______________________________________________________
#03.ctmm_moving_window.R

#Loop through windows of X days of locations, compute HR size for each window
#Plot ML point estimate of HR size and 95% CIs
#Mark Bidwell, Chris Fleming (mostly Chris!)

#Modified by Michael Noonan

#Last updated: Feb 14th 2022

rm(list = ls())
# gc()
# 
# 
# getwd()

#read in data from 02.outlier.detection.r to establish when the arrive on winter range:
dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")


# dat3 <- dat2[dat2$during == 1,]

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #calculating home ranges reveals a couple of extreme outliers in the HR during data not captured by their DOP values. 
# #Collar 44095 has 2 points 60km and 205km away from main points over 24 hrs with an outlier vaue of 3.
# #these were removed in 02, but double check
# dat = dat[!(dat$id %in% c(32597, 32598)),] 
# 
# #,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
# #what are the monitoring periods for each animal for each year:
# #mon.times = dat %>%
# #  group_by(individual.local.identifier) %>%
# #  dplyr::summarise(first.date = min(timestamp), last.date = max(timestamp))
# #mon.times$win = as.numeric(floor(difftime(mon.times$last.date, mon.times$first.date, units = c("days"))))
# ## #,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
# #dat.collars.d = dat %>%
# #  group_by(herd, individual.local.identifier) %>%
# #  distinct(individual.local.identifier)
# ## #,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,




#.............................CHANGE PERIOD.............................
#change this to each period and run: normal, during, post
# period = 'during'
# tel.dat = droplevels(dat[dat$during == 1,])
# period = 'normal'
# tel.dat = droplevels(dat[dat$normal == 1,])

period = 'all'
# tel.dat <- dat
tel.dat = dat[dat$year == "2022",]
tel.dat <- tel.dat[tel.dat$month_day >= '07-01' & tel.dat$month_day <= '07-31', ]

data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
collars = tel.dat %>% distinct(individual.local.identifier)

data <- data[5:6]

#create directories to hold output:
dir.create(paste0("./test/data/input_data/moving_window/", period, "/Fits"), recursive = TRUE)
dir.create(paste0("./test/data/input_data/moving_window/", period, "/UDs"), recursive = TRUE)



# some collars within each period cause the script to crash if data is missing, this modified function code corrects for it without having to remove the collar
# due to the missing data, plotting section has been taken out and done separately after, plotting code has been modified to handle missing data

#...............................................................
# window.hr function ----
#...............................................................

window.HR <- function(data, dt, win, Fig_Path, Result_Path, UD_path) {
  DATA = data
  
  # SECTION: Set up the time intervals and empty lists to hold the results
  tryCatch({
    
    times <- seq(from = DATA$t[1], 
                 to = DATA$t[nrow(DATA)], 
                 by = win) # should this be by dt or win?
    # chop off end of times 
    # times <- times[-((length(times)-win/dt+1):length(times))]

  #   #print the range of timestamps in the data
  #   print(paste("Data timestamps range from", as.POSIXct(DATA$t[1], origin="1970-01-01", tz="UTC"), "to", as.POSIXct(DATA$t[nrow(DATA)], origin="1970-01-01", tz="UTC")))
  #   
  #   # Adjust the slicing to ensure complete windows
  #   times <- times[times <= (DATA$t[nrow(DATA)] - win)]
  #   print(paste("Time intervals set from", as.POSIXct(times[1], origin="1970-01-01", tz="UTC"), "to", as.POSIXct(times[length(times)], origin="1970-01-01", tz="UTC")))
  })
  
  
  #initialize arrays (lists) for results
  AREAS_lo <- rep(NA,length(times))
  AREAS_ml <- rep(NA,length(times))
  AREAS_hi <- rep(NA,length(times))
  
  # SECTION: iteration of time intervals for window section
  for(i in 1:length(times)) {
    
 #indicate what iteration the analysis is currently on
    # print(paste((i),"of",length(times),"iterations", "- Collar ID:", DATA@info[1]))
    print(paste((i),"of",length(times),"iterations. At window segment", 
                format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
                "for Collar ID:", DATA@info[1]))
    
    # subset for the time segment
    SUBSET <- DATA[times[i]<=DATA$t & DATA$t<=times[i]+win,] # should I be adding win or dt here or neither?
    
    # Check if there is any data in the subset
    if (nrow(SUBSET) == 0) {
      cat("No data found or data is missing for window section in iteration", i, "- moving on to the next iteration.\n")

      # Set results to NA
      AREAS_lo[i] <- NA
      AREAS_ml[i] <- NA
      AREAS_hi[i] <- NA
      
      next # Move to the next iteration if the subset is empty
    }
    
    tryCatch({
      if (nrow(SUBSET) > 0) {
        GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
        FIT <- try(ctmm.select(SUBSET, GUESS))
        
        # inherits() or (class(FIT)=="ctmm") checks if 'FIT' is an object related to a ctmm object, inherits() provides more flexibility allowing any class derived from "ctmm" vs class(FIT)=="ctmm" is more strict and is exactly "ctmm"
        if (inherits(FIT, "ctmm")) {
          AKDE <- akde(SUBSET, FIT)
          
          #to not overwrite each iteration, need to add the date of each window as file name:
          filename <- format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
          # fname = DATA[1]$timestamp
          # filename = lubridate::date(fname[i])
          
          #save AKDE 'date_animal_period_UD'
          akde.path <- file.path(UD_path, paste0(filename, '_', period, "_UD_", DATA@info[1], ".rda"))
          save(AKDE, file = akde.path)
          
          #save fitted model 'date_animal_period_Fits.rda'
          mod_path <- file.path(Result_Path, paste0(filename, '_', period,"_Fits_", DATA@info[1], ".rda"))
          save(FIT, file = mod_path)
          
          # Store results
          SUMMARY <- summary(AKDE, units = FALSE)
          AREAS_lo[i] <- SUMMARY$CI[[1]]
          AREAS_ml[i] <- SUMMARY$CI[[2]]
          AREAS_hi[i] <- SUMMARY$CI[[3]]
        } else {
          # If FIT is not a ctmm object, set results to NA
          AREAS_lo[i] <- NA
          AREAS_ml[i] <- NA
          AREAS_hi[i] <- NA
        }
      } else {
        # If SUBSET is empty, set results to NA
        AREAS_lo[i] <- NA
        AREAS_ml[i] <- NA
        AREAS_hi[i] <- NA
        
        cat("No data found or data is missing for window section in iteration", i, "- setting results to NA.\n")
      }
    }, error = function(e) {
      # Set results to NA in case of error
      AREAS_lo[i] <- NA
      AREAS_ml[i] <- NA
      AREAS_hi[i] <- NA
    })
  }
  
  # Prepare results dataframe
  tryCatch({
    TIMES <- as.POSIXct(times, origin="1970-01-01",
                        tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                    DATA$longitude[1]))
    RESULTS <- data.frame(TIMES,
                          AREAS_lo,
                          AREAS_ml,
                          AREAS_hi)
    names(RESULTS)[1] <- "Time"
  })
  
  return(RESULTS)
}


# NOTE: might be an error with the filename date being saved, the dates go until august however, when plotting, its the proper date range



#.........................................................................
# Run the function ----
#.........................................................................

START <- Sys.time()
tic()

# t =  Unix timestamp, needs to be converted into regular format?

# Define dt and win in seconds ?
# dt <- 1 * 24 * 60 * 60  # 1 day in seconds,
# win <- 3 * 24 * 60 * 60 # 3 days in seconds, window segments

results = lapply(data,
                 window.HR,
                 dt = 1 %#% 'day', # what does %#% 'day' do?
                 win <- 3 %#% 'day',
                 Fig_Path = paste0(getwd(),"/test/data/input_data/moving_window/", period),
                 Result_Path = paste0(getwd(),"/test/data/input_data/moving_window/", period,'/Fits'),
                 UD_path = paste0(getwd(),"/test/data/input_data/moving_window/", period,'/UDs'))

toc()
END <- Sys.time()

save(results, file = "./data/input_data/moving_window/20240628_moving_window_results_all_dat_2023_v2.rda")

load("./data/input_data/moving_window/20240628_moving_window_results_all_dat.rda")


#'''''''''''''''''''''''''''''''''''''''''''''''''''
#Plot all the results and save them as a png ----
#'''''''''''''''''''''''''''''''''''''''''''''''''''

# issue with plotting within the function due to missing data therefore
# Plot outside of the original function and after to be able to account for missing data
# need to import the result data from all prior to plotting or itll just plot the last loop, might have to update the script

# if window.hr function works and able to handle data gaps, then the plotting may be able to be included

plot_results <- function(results, Fig_Path, data, period) {
  TIMES <- results[[1]]$Time
  AREAS_lo <- results[[1]]$AREAS_lo
  AREAS_ml <- results[[1]]$AREAS_ml
  AREAS_hi <- results[[1]]$AREAS_hi
  
  fig.path <- file.path(Fig_Path, paste0(period, "_", data@info[1], ".png"))
  
  png(file=fig.path, type="cairo", units = "in", width = 6.81, height = 3, pointsize = 10, res = 600)
  
  par(mfrow=c(1,2), mgp = c(1.5, 0.5, 0), oma=c(0,0,0,0), mar=c(3,3,2,2), cex.lab=1.2, cex.main = 1, family = "serif")
  
  rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
  data$Col <- rbPal(nrow(data))[as.numeric(cut(data$t,breaks = nrow(data)))]
  plot(data, col.grid = NA, pch = 20, cex = 0.2, 
       # col.DF = "#669543", 
       col.UD = "#669543", 
       col = data$Col, labels=FALSE)
  title(main = "a)", adj = 0)
  
  plot(AREAS_ml ~ TIMES, pch=19, cex=1.25, ylim=c(0, max(AREAS_hi, na.rm = TRUE)), ylab = "Home Range Area", xlab = "Date", xaxt = "n")
  
  arrows(TIMES, AREAS_lo, TIMES, AREAS_hi, length = 0.05, angle = 90, code = 3)
  title(main = "b)", adj = 0)
  
  # Adjust x-axis with month labels
  axis(1, at = seq(from = min(TIMES, na.rm = TRUE), to = max(TIMES, na.rm = TRUE), by = "month"), labels = format(seq(from = min(TIMES, na.rm = TRUE), to = max(TIMES, na.rm = TRUE), by = "month"), "%b"))
  
  dev.off()
}



# Plot the results and save the png
for (i in 1:length(data)) {
  plot_results(results[i], paste0(getwd(), "./data/input_data/moving_window/", period), data[[i]], period)
}

# Warning messages:
# 1: In arrows(TIMES, AREAS_lo, TIMES, AREAS_hi, length = 0.05,  ... :
#                zero-length arrow is of indeterminate angle and so skipped






#________________________________________________________________________
# Import window.hr result files ----

# 20240619:
# UDs are missing? only goes up to August 11, 2023. Where is the rest of August and Sept?
# checked the data prep, all data are there up to last day of data
# CURRENTLY: rerunning sliding window (during period) to see if it has to do with the modified code
# prepping part 2 of sliding window while rerunning code check
# might be an error with the filename date being saved, the dates go until august however, when plotting, its the proper date range (for during, normal still rerunning)


#load rda:
# in version 4, files are corrupted, cannot read them in
setwd(paste0(getwd(),'/test/data/input_data/moving_window/',period,'/UDs'))

ud.period = list.files(pattern="*.rda", all.files=TRUE, 
                       full.names=FALSE)
ud.period.uds <- list()

for(i in 1:length(ud.period)){
  load(ud.period[[i]])
  ud.period.uds[[i]] <- AKDE
}



#................................................
# Create dataframe and extract results
#................................................

# NOTE: moved the following files out of the folder because it contains no data from the folder, its causing an error
# 2019-08-24_all_data_UD_30648.rda
# 2019-08-24_all_UD_30648.rda
# 2019-08-24_normal_UD_30648.rda
# 2023-03-13_all_data_2023_UD_30642.rda


hr.list = list()

for(i in 1:length(ud.period.uds)){
  #i = 10
  hr <- as.data.frame(period)
  hr$ID = ud.period.uds[[i]]@info$identity
  hr$start = str_sub(print(ud.period[[i]]),1,10)
  hr$hr_low = summary(ud.period.uds[[i]], units = FALSE)$CI[1,"low"]/1000000
  hr$hr_est = summary(ud.period.uds[[i]], units = FALSE)$CI[1,"est"]/1000000
  hr$hr_high = summary(ud.period.uds[[i]], units = FALSE)$CI[1,"high"]/1000000
  hr.list[[i]] = hr
  
}

ud.period.window = do.call(rbind, hr.list)


getwd()
setwd("C:/Users/achhen/OneDrive - UBC/Github/mountain_goat_movement_ecology/data/input_data/moving_window/")
write.csv(ud.period.window, paste0(period,'_20240628_moving_window_UDs.csv'), row.names = FALSE)
setwd("C:/Users/achhen/OneDrive - UBC/Github/mountain_goat_movement_ecology/")


results <- read.csv('./data/input_data/moving_window/all_data_20240628_moving_window_UDs.csv')



#________________________________________________________
# Plot results ----


# rm(list = ls())
# gc()

library(tidyr)
library(lubridate)
library(ggplot2)

ggplot(ud.period.window) +
  geom_point(aes(x = start, y = hr_est))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#......................................................................
# Plot results on calendar scale ----
#......................................................................

mw.dat <- ud.period.window
mw.dat <- read.csv("./data/input_data2/moving_window/all_data_20240703_moving_window_UDs")

mw.dat = mw.dat %>%
  pivot_longer(!c(period, ID, start), names_to = "HR_type", values_to = "95_estimate")

mw.dat$start = as.Date(mw.dat$start)
mw.dat$period = as.factor(mw.dat$period)
mw.dat$ID = as.factor(mw.dat$ID)
mw.dat$year = year(mw.dat$start)
mw.dat$month = month(mw.dat$start)
mw.dat$day <- day(mw.dat$start)
mw.dat$doy <- yday(mw.dat$start)
mw.dat$HR_type = as.factor(mw.dat$HR_type)


# Define the range and the extended range for the x-axis
start_doy <- 203 # July 22 = day 203
end_doy <- 273   # Sept 30 = day 273
extend_days <- 5 # Extend the range by a few days on each end

# scale_color_manual(name = "Year", values = c("2019" = "#bae4b3",
#                                              "2020" = "#74c476",
#                                              "2021" = "#31a354",
#                                              "2022" = "#006d2c",
#                                              "2023" = 'black')) +
# 
# scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                              "2020" = "#ddaa33",
#                                              "2021" = "#006d2c",
#                                              "2022" = "#33bbee",
#                                              "2023" = 'black')) +


#........................................................................
## Plot single goat ----
#........................................................................

ggplot(data = mw.dat[mw.dat$HR_type == "hr_est" & mw.dat$ID == "30548",]) +
  geom_smooth(aes(x = doy, y = `95_estimate`, color = as.factor(year), group = as.factor(year), linetype = 'solid')) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  ggtitle("30548") +
  scale_y_continuous(name = '95% Home Range Estimate') +
  scale_x_continuous(name = 'Month',
                     limits = c(-5, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) + # Use month abbreviations
  scale_color_manual(name = "Year", values = c("2019" = "#332288",
                                               "2020" = "#ddaa33",
                                               "2021" = "#006d2c",
                                               "2022" = "#33bbee",
                                               "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))

#........................................................................
## Plot all goats figure via facet wrap ----
#........................................................................

ggplot(data = mw.dat[mw.dat$HR_type == "hr_est",]) +
  geom_smooth(aes(x = doy, y = `95_estimate`, color = as.factor(year), group = as.factor(year), linetype = 'solid')) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, scales = "free", ncol = 3, nrow = 4) +  # Facet by ID, adjust ncol as needed
  # ggtitle("Home Range Estimates for ID: {facet_var}") +  # Dynamic title using facet variable
  scale_y_continuous(name = '95% Home Range Estimate') +
  scale_x_continuous(name = 'Month',
                     limits = c(-5, 340), expand = c(0, 1), # so the plot extends a bit after december
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, "")) + # Use month abbreviations
  scale_color_manual(name = "Year", values = c("2019" = "#332288",
                                               "2020" = "#ddaa33",
                                               "2021" = "#006d2c",
                                               "2022" = "#33bbee",
                                               "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))






#~~~~~~~~~~~~~ BELOW plotting based on within period range and herd formatting ----


dat = dat3 %>%
  pivot_longer(!c(period, ID, start, year), names_to = "HR_type", values_to = "95_estimate")
#dat$start = as.POSIXct(dat$start,  format = '%Y-%m-%d %H:%M', tz='UTC')
dat$week = week(dat$start)
dat$doy <- yday(dat$start)
dat$HR_type = as.factor(dat$HR_type)

# determine the week number
# July 22
dat[dat$start == "2023-07-22",] # week 29
# Sept 30 =

dat[dat$start == "2023-09-30",] # week ?? should be week 39


# fire lifespan
dat$fire.week = ifelse(dat$week == 29, 1, 
                       ifelse(dat$week == 30, 2,
                              ifelse(dat$week == 31, 3,
                                     ifelse(dat$week == 32, 4,
                                            ifelse(dat$week == 33, 5, 
                                                   ifelse(dat$week == 34, 6,
                                                          ifelse(dat$week == 35, 7,
                                                                 ifelse(dat$week == 36, 8,
                                                                        ifelse(dat$week == 37, 9, 
                                                                               ifelse(dat$week == 38, 10, 
                                                                                      ifelse(dat$week == 39, 11, 
                                                                                             dat$week + 11)))))))))))


# #............................................................................
# # Plot
# #............................................................................
# 
# 
# #1)
ggplot() +
  geom_smooth(dat = dat[dat$HR_type == "hr_est",], aes(x = fire.week, y = `95_estimate`, color = period), linetype = 'solid') +
  scale_x_discrete(name ="Weeks from July 22", limits = c(1:30)) +
  scale_y_continuous(name = '95% Home Range Estimate') #+
  # geom_vline(xintercept = c(9,25), linetype = 'dashed') #week 9 = Dec 24, week 25 = Apr 8
# ggsave("data/moving_window/test/home_range.jpg")
# 
# #2)
# ggplot() +
#   geom_smooth(dat = dat[dat$HR_type == "hr_est",], aes(x = fire.week, y = `95_estimate`, color = period), linetype = 'solid') +
#   scale_x_discrete(name ="Weeks from July 22", limits = c(1:30)) +
#   scale_y_continuous(name = '95% Home Range Estimate') +
#   geom_vline(xintercept = c(9,25), linetype = 'dashed') #+  #week 9 = Dec 24, week 25 = Apr 8
#   # facet_wrap(period~herd, ncol = 2)

#-------------------------------------------------------------------------------

#average home range for period, herd and start time:
dat1 = data %>% 
  dplyr::group_by(period, start) %>%
  dplyr::summarise(hr_ave_low = mean(hr_low), hr_ave_estimate = mean(hr_est), hr_ave_high = mean(hr_high))


dat2 = dat1 %>%
  pivot_longer(!c(period, start), names_to = "HR_type", values_to = "HR_estimate")
dat2$week = week(dat2$start)
dat2$HR_type = as.factor(dat2$HR_type)
#calculate weeks from October 31 (winter.week)
dat2$fire.week = ifelse(dat2$week == 29, 1, 
                        ifelse(dat2$week == 30, 2,
                               ifelse(dat2$week == 31, 3,
                                      ifelse(dat2$week == 32, 4,
                                             ifelse(dat2$week == 33, 5, 
                                                    ifelse(dat2$week == 34, 6,
                                                           ifelse(dat2$week == 35, 7,
                                                                  ifelse(dat2$week == 36, 8,
                                                                         ifelse(dat2$week == 37, 9, 
                                                                                ifelse(dat2$week == 38, 10, 
                                                                                       ifelse(dat2$week == 39, 11, 
                                                                                              dat2$week + 11)))))))))))


#............................................................................
# NEED TO UPDATE BELOW...looks like theyre all herd related
#............................................................................



#update period so we have years instead of descriptors:
dat2$period = as.character(dat2$period)
# dat2[which(dat2$period == "during"),"period"] <- "2019"
dat2[which(dat2$period == "normal"),"period"] <- "normal"
dat2[which(dat2$period == "during"),"period"] <- "2023"
# dat2[which(dat2$period == "after"),"period"] <- "2022"
dat2$period = as.factor(dat2$period)

#3)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == 'hr_ave_estimate',], aes(x = fire.week, y = HR_estimate, color = period), linetype = 'solid') +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_low",], aes(x = fire.week, y = HR_estimate, color = period), linetype = 'dashed', se = FALSE) +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_high",], aes(x = fire.week, y = HR_estimate, color = period), linetype = 'dashed', se = FALSE) +
  scale_x_discrete(name ="Weeks from July 22", limits = c(1:30)) +
  scale_y_continuous(name = 'Log-scaled 95% Home Range Estimate', trans = 'log10') +
  geom_vline(xintercept = c(4,25), linetype = 'dashed') #week 9 = Dec 24, week 25 = Apr 8

#4)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_estimate",], aes(x = winter.week, y = HR_estimate, color = period), linetype = 'solid') +
  scale_x_discrete(name ="Weeks from July 22", limits = c(1:30)) +
  scale_y_continuous(name = 'Log Scaled 95% Home Range Estimate', trans = 'log10') +
  geom_vline(xintercept = c(11,22), linetype = 'dashed', size = 1) +  #week 9 = Dec 24, week 25 = Apr 8 / 11-22 is original Jan 20 - Mar 25
  geom_vline(xintercept = c(6,15), linetype = 'dashed', color = '#F8766D', size = 0.75) + #hr during
  geom_vline(xintercept = c(6,15), linetype = 'dashed', color = '#00BA38', size = 0.75) + #hr prior1
  geom_vline(xintercept = c(6,15), linetype = 'dashed', color = '#619CFF', size = 0.75)   #hr prior2
#facet_wrap(~herd, ncol = 1)

#-------------------------------------------------------------------------------
#check each herd's home range settlement period for each period:

#5)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_estimate",], aes(x = fire.week, y = HR_estimate, color = period),span = 0.1, linetype = 'solid') +
  geom_point(data = dat2[dat2$HR_type == "hr_ave_estimate",], aes(x = fire.week, y = HR_estimate, color = period), alpha = 0.5) +
  scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = 'Log Scaled 95% Home Range Estimate', trans = 'log10') #+
  facet_wrap(period~herd, ncol = 2, scales = 'free')
ggsave("C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/presentation/home_range_by_period_herd_log_scaled_2022-06-03.jpg", width = 10, height = 12)
#-------------------------------------------------------------------------------
#5)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == 'cn',], aes(x = fire.week, y = HR_estimate, color = period), span = 0.5, linetype = 'solid') +
  geom_point(data = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == 'cn',], aes(x = fire.week, y = HR_estimate, color = period), alpha = 0.5) +
  scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = 'Log Scaled 95% Home Range Estimate', trans = 'log10') #+
  ggtitle("Columbia North") +
  facet_wrap(~period, ncol = 2, scales = 'free')
ggsave("C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/presentation/home_range_CN_by_period_herd_log_scaled_2022-06-03.jpg", width = 10, height = 12)

#Estimated breakpoints (week from October 31) from figure 5
#HR Prior1: 8-15 (2019-12-17 to 2020-02-04)
#HR Prior2: 7-13 (2018-12-10 to 2019-01-21)
#HR During: 7-17 (2020-12-09 to 2021-02-18)
#HR After: 14-18 (2022-01-22 to 2022-02-25)

#CN P1: 13-23 (2020-01-17 to 2020-03-26)
#CN P2: 18-23 (2019-02-19 to 2019-04-01)
#CN DU: 15-22 (2021-01-29 to 2021-03-25)
#CN AF: 12-24 (2022-01-08 to 2022-04-08)

#CS P1: 10-20 (2019-12-31 to 2020-03-10)
#CS P2: 19-24 (2019-02-26 to 2019-04-08)
#CS DU: 10-24 (2020-12-30 to 2021-04-08)
#CS AF: 18-24 (2022-02-19 to 2022-04-08)

target = dat %>% 
  group_by(period, fire.week) %>%
  dplyr::summarise(min.week.date = min(start), max.week.date = max(start))

#6) recreate previous figure with v_lines for each period defined from above lines
#in excel I created a file that reflects those break dates:
dat_migr = read.csv('./data/input_data/moving_window/moving_window_weeks_220604.csv')
dat_migr$herd = as.factor(dat_migr$herd)
#change period values to be years, instead of descriptors:
dat_migr$period = as.character(dat_migr$period)
dat_migr[which(dat_migr$period == "prior2"),"period"] <- "2019"
dat_migr[which(dat_migr$period == "prior1"),"period"] <- "2020"
dat_migr[which(dat_migr$period == "during"),"period"] <- "2021"
dat_migr[which(dat_migr$period == "after"),"period"] <- "2022"
dat_migr$period = as.factor(dat_migr$period)

dat_migr$week = as.numeric(dat_migr$week)

herd = 'hr'
titl = 'Hart Ranges'
#set the data:
ggdat = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == herd,]
ggmigr= dat_migr[dat_migr$herd == herd,]

blue1 = "#3A8EB7"
blue2 = "#03AAFC"
blueA = "#BCD5EE"
red = "#FF0000"

mp = ggplot() +
  geom_smooth(data = ggdat, 
              aes(x = winter.week, y = HR_estimate, color = period), span = 0.5, linetype = 'solid') +
  geom_point(data = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == herd,], 
             aes(x = winter.week, y = HR_estimate, color = period), alpha = 0.7) +
  scale_x_continuous(name ="Weeks from October 31") +
  geom_point(data = ggdat[ggdat$HR_type == "hr_ave_estimate" & ggdat$herd == herd,], 
             aes(x = winter.week, y = HR_estimate), color = 'gray', pch = 21, alpha = 0.8) +
  scale_y_continuous(name = 'Log Scaled 95% Home Range Estimate (km^2)', trans = 'log10') +
  ggtitle(titl) +
  theme(legend.position = 'none') +
  scale_color_manual(values = c(blue2, blue1, red, blueA),
                     labels = c("2021", "2020", "2019", "2022")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~period, ncol = 2, scales = 'free_x')

mp +
  theme(legend.position="none",
        axis.title.y = element_text(size=12, family = "sans"),
        axis.title.x = element_text(size=12, family = "sans"),
        axis.text.y = element_text(size=10, family = "sans"),
        axis.text.x  = element_text(size=10, family = "sans")) +
  geom_vline(data = ggmigr,
             aes(xintercept = week), linetype = 'dashed')


getwd()
ggsave(paste0("./figures/home_range_",herd,"_by_period_herd_log_scaled_2023-01-27.jpg"), 
       units = 'px', width = 2200, height = 1400)

#-------------------------------------------------------------------------------
#subset to windows determined above:
run.dat = read.csv('./data/input_data/220526_moving_window_formatted_for_tele_dat.csv')

#HART RANGES
run.hr = run.dat[run.dat$herd == 'hr',]
#HR prior1: 2019-12-17 to 2020-02-04
run.hr$prior1 = ifelse(run.hr$timestamp >= '2019-12-17' & run.hr$timestamp <= '2020-02-04', 1, 0)

#HR prior2: 2018-12-10 to 2019-01-21
run.hr$prior2 = ifelse(run.hr$timestamp >= '2018-12-10' & run.hr$timestamp <= '2019-01-21', 1, 0)

#HR during: 2020-12-09 to 2021-02-18
run.hr$during = ifelse(run.hr$timestamp >= '2020-12-09' & run.hr$timestamp <= '2021-02-18', 1, 0)

#HR after: 2022-01-22 to 2022-02-25
run.hr$after = ifelse(run.hr$timestamp >= '2022-01-22' & run.hr$timestamp <= '2022-02-25', 1, 0)

#COLUMBIA NORTH
run.cn = run.dat[run.dat$herd == 'cn',]
#CN prior1: 2020-01-17 to 2020-03-26
run.cn$prior1 = ifelse(run.cn$timestamp >= '2020-01-17' & run.cn$timestamp <= '2020-03-26', 1, 0) 

#CN prior2: 2019-02-19 to 2019-04-01
run.cn$prior2 = ifelse(run.cn$timestamp >= '2019-02-19' & run.cn$timestamp <= '2019-04-01', 1, 0)

#CN during: 2021-01-29 to 2021-03-25
run.cn$during = ifelse(run.cn$timestamp >= '2021-01-29' & run.cn$timestamp <= '2021-03-25', 1, 0)

#CN after: 2022-01-08 to 2022-04-08
run.cn$after = ifelse(run.cn$timestamp >= '2022-01-08' & run.cn$timestamp <= '2022-04-08', 1, 0)

#CENTRAL SELKIRKS  
run.cs = run.dat[run.dat$herd == 'cs',]
#CS prior1: 2019-12-31 to 2020-03-10
run.cs$prior1 = ifelse(run.cs$timestamp >= '2019-12-31' & run.cs$timestamp <= '2020-03-10', 1, 0)

#CS prior2: 2019-02-26 to 2019-04-08
run.cs$prior2 = ifelse(run.cs$timestamp >= '2019-02-26' & run.cs$timestamp <= '2019-04-08', 1, 0)

#CS during: 2020-12-30 to 2021-04-08
run.cs$during = ifelse(run.cs$timestamp >= '2020-12-30' & run.cs$timestamp <= '2021-04-08', 1, 0)

#CS after: 2022-02-19 to 2022-04-08
run.cs$after = ifelse(run.cs$timestamp >= '2022-02-19' & run.cs$timestamp <= '2022-04-08', 1, 0)

#subset for just the period within the winter range windows
#each of the dates is subset in 06.Batch_Run.r so these last 6 lines are not required, but added to calculate sample size.:
run.cn$resident = rowSums(run.cn[,c("prior1", "prior2", "during", "after")])
run.cn = run.cn[run.cn$resident == 1,]
run.cs$resident = rowSums(run.cs[,c("prior1", "prior2", "during", "after")])
run.cs = run.cs[run.cs$resident == 1,]
run.hr$resident = rowSums(run.hr[,c("prior1", "prior2", "during", "after")])
run.hr = run.hr[run.hr$resident == 1,]


#write each file separately to retain those dates specific to each herd:
write.csv(run.hr, './data/input_data/home_range/HR_220605_clean_data_formatted_for_tele.csv', row.names = FALSE)
write.csv(run.cn, './data/input_data/home_range/CN_220605_clean_data_formatted_for_tele.csv', row.names = FALSE)
write.csv(run.cs, './data/input_data/home_range/CS_220605_clean_data_formatted_for_tele.csv', row.names = FALSE)


#EOF












