
# Sliding window

# Script description: (Ryan) uses output from previous file to iterate each over collar and calculate home range over a 14 day moving window, advanced by 1 day to determine when each animal becomes range resident 

library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)



#______________________________________________________
#03.ctmm_moving_window.R

#Loop through windows of X days of locations, compute HR size for each window
#Plot ML point estimate of HR size and 95% CIs
#Mark Bidwell, Chris Fleming (mostly Chris!)

#Modified by Michael Noonan

#Last updated: Feb 14th 2022

# rm(list = ls())
# gc()
# 
# 
# getwd()

#read in data from 02.outlier.detection.r to establish when the arrive on winter range:
load("data/goat/20240612_clean_goat_data.rda")
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")

# anyNA(goat_data)
dat <- goat_data

#subset to 2023 year
tel.dat <- dat[dat$year == "2023",]

# # remove collar 30642 due to missing 5 days in a row i.e. no fixes for 5 days and window is set at 3 days, it breaks the function and produces an error code
sort(unique(goat_data$collar_id))
# tel.dat <- tel.dat[!(tel.dat$collar_id == "30642"),]
tel.dat <- tel.dat[!(tel.dat$collar_id %in% c("30548", "30551", "30561", "30567", "30575", "30599", "30613")),]
# tel.dat <- tel.dat[tel.dat$collar_id == "30642",]
sort(unique(tel.dat$collar_id))

data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')

# collars = tel.dat %>% distinct(individual.local.identifier)
collars = tel.dat %>% distinct(collar_id)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# to fix issue with 30642

#include 30642 in data but drop 30551 due to its date range
load("data/goat/20240612_clean_goat_data.rda")
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
tel.dat <- dat[dat$year == "2023",]
# remove collar 30551 due to mortality during 2023
tel.dat <- tel.dat[!(tel.dat$collar_id == "30551"),]
data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')


# Collar ID: 30642 
# Missing Dates: 2023-09-23, 2023-09-24, 2023-09-25, 2023-09-26, 2023-09-27 
tel.dat[tel.dat$date == "2023-09-23",] # day 266 (where it crashes)
tel.dat[tel.dat$date == "2023-09-27",] # day 270
# exclude 30642 for 1:length(times) so all collars are completed then
# include 30642 let it crash then so then 30642 from 1:166 is done then 
# exclude 30551, 271:length(times) in the function code in the for loop for the remainder of 30642 windows and run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#create directories to hold output:
dir.create("data/moving_window/")
dir.create(paste0("data/moving_window/", "/Fits"), recursive = TRUE)
dir.create(paste0("data/moving_window/", "/UDs"), recursive = TRUE)


#dt is how much you advance the window
#win is the length of the window you are looking at
#Ryan: In my case I was calculating an akde for a 14 day window (that was the mean range crossing time), and advancing that window by 1 day (dt) until I found when animals were range resident.
#Dr. Noonan: select a window based on the home range crossing time (tau_p). I would aim for something on the order of 2-3 crossings at minimum. 

window.HR <- function(data, dt, win, Fig_Path, Result_Path, UD_path){
  DATA = data
  
  #........set up
  tryCatch({
    times <- seq(DATA$t[1], DATA$t[nrow(DATA)], by = dt)
    
    # chop off end of times
    times <- times[-((length(times)-win/dt+1):length(times))]
  })
  #initialize arrays for results
  AREAS_lo <- rep(NA,length(times))
  AREAS_ml <- rep(NA,length(times))
  AREAS_hi <- rep(NA,length(times))

  #...........ctmm fit models and akde hr
    for(i in 1:length(times))
  {tryCatch
    ({
      print(paste((i),"of",length(times),"iterations", "- Collar ID:", DATA@info[1]))
      
      # subset times t to t+win
      SUBSET <- DATA[times[i]<=DATA$t & DATA$t<=times[i]+win,]
      
      # analyse subset
      GUESS <- ctmm.guess(SUBSET,interactive=FALSE)
      FIT <- try(ctmm.select(SUBSET,GUESS))
      if(class(FIT)=="ctmm")
      {
        AKDE <- akde(SUBSET,FIT)
        
        #to not overwrite each iteration, need to add the date of each window as file name:
        fname = DATA[1]$timestamp
        f.name = lubridate::date(fname[i])
        
        #save AKDE 'date_animal_UD'
        akde.path <- file.path(UD_path, paste0(f.name, '_', "UD_", DATA@info[1], ".rda"))
        save(AKDE, file = akde.path)
        
        #save fitted model 'date_animal.rda'
        mod_path <- file.path(Result_Path, paste0(f.name, '_', "Fits_", DATA@info[1], ".rda"))
        save(FIT, file = mod_path)
        
        # store results in arrays
        SUMMARY <- summary(AKDE,units=FALSE)
        AREAS_lo[i] <- SUMMARY$CI[[1]]
        AREAS_ml[i] <- SUMMARY$CI[[2]]
        AREAS_hi[i] <- SUMMARY$CI[[3]]
      }
    })
    }
  
  # ........set up for plotting
  # Plot ML point estimate of area and 95% CIs
  tryCatch({
    TIMES <- as.POSIXct(times, origin="1970-01-01",
                        tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                    DATA$longitude[1]))
    RESULTS<-data.frame(TIMES,
                        AREAS_lo,
                        AREAS_ml,
                        AREAS_hi)
    names(RESULTS)[1]<-"Time"
  })


  ############################################
  #Plot all the results and save them as a png
  ############################################

  fig.path <- file.path(Fig_Path,
                        paste(DATA@info[1], ".png", sep = ""))


  #Save the graphic device's output as a png
  png(file=fig.path,
      type="cairo",
      units = "in",
      width = 6.81, height = 3,
      pointsize = 10,
      res = 600) #

  #Set the par to plot all on same screen
  par(mfrow=c(1,2),
      mgp = c(1.5, 0.5, 0),
      oma=c(0,0,0,0),
      mar=c(3,3,2,2),
      cex.lab=1.2,
      cex.main = 1,
      family = "serif")

  #Plot the relocation data, coloured by time
  #Create a function that scales colours between red and blue
  rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
  #Then create a variable that scales from red to blue between the two times
  DATA$Col <- rbPal(nrow(DATA))[as.numeric(cut(DATA$t,breaks = nrow(DATA)))]
  plot(DATA,
       col.grid = NA,
       pch = 20,
       cex = 0.2,
       col.DF = "#669543",
       col = DATA$Col,
       labels=FALSE)
  title(main = "a)", adj = 0)


  #Plot of the range estimates over time
  plot(RESULTS$AREAS_ml~RESULTS$Time,
       pch=19,
       cex=1.25,
       ylim=c(0,max(RESULTS$AREAS_hi,na.rm=TRUE)),
       ylab="Home Range Area",
       xlab="Date")
  arrows(RESULTS$Time,
         RESULTS$AREAS_lo,
         RESULTS$Time,
         RESULTS$AREAS_hi,
         length=0.05,
         angle=90,
         code=3)
  title(main = "b)", adj = 0)

  dev.off()

  
  
  
  return(RESULTS)
}


#.........................................................................
# Run the function ----
#.........................................................................

#Error in if (EOV) { : missing value where TRUE/FALSE needed
#drop supplementary data columns because it has NA values -> did not fix
#updated ctmm to see if it fixes the error -> did not fix
#remove collar 30551 due to mortality during 2023 and run it separately after to fix error
#Dr. Noonan: recommend running the window.HR function line by line on the particular data subset that is causing problems. e need to know the specific piece of code that is causing the error before we can fix anything. We need to know the specific piece of code that is causing the error before we can fix anything.
# issue = 30642 has 5 days gap missing fixes

START <- Sys.time()

results = lapply(data,
                 window.HR,
                 dt = 1 %#% 'day',
                 win <- 3 %#% 'day',
                 Fig_Path = paste0(getwd(),"/data/moving_window/"),
                 Result_Path = paste0(getwd(),"/data/moving_window/", '/Fits'),
                 UD_path = paste0(getwd(),"/data/moving_window/", '/UDs'))

END <- Sys.time()








#......................................................................
# PART 2 of sliding window ----
#......................................................................

#-------------------------------------------------------------------------------
# Find and list all .rda files in the folder
#load rda: 
setwd(paste0(getwd(),'/data/moving_window/','/UDs'))
ud = list.files(pattern="*.rda", all.files=TRUE,
                full.names=FALSE)

# directory <- "data/moving_window/UDs/"
# ud = list.files(directory, pattern="\\.rda$", all.files=TRUE, 
#               full.names=FALSE)

ud.uds <- list()

for(i in 1:length(ud)){
  load(ud[[i]])
  ud.uds[[i]] <- AKDE
}

hr.list = list()

for(i in 1:length(ud.uds)){
  #i = 10
  hr <- data.frame(collar_id = NA, start = NA, hr_low = NA, hr_est = NA, hr_high = NA)
  hr$collar_id = ud.uds[[i]]@info$identity
  hr$start = str_sub(print(ud[[i]]),1,10) 
  hr$hr_low = summary(ud.uds[[i]], units = FALSE)$CI[1,"low"]/1000000
  hr$hr_est = summary(ud.uds[[i]], units = FALSE)$CI[1,"est"]/1000000
  hr$hr_high = summary(ud.uds[[i]], units = FALSE)$CI[1,"high"]/1000000
  hr.list[[i]] = hr
  
}

ud.window = do.call(rbind, hr.list)

write.csv(ud.window, paste0('moving_window_UDs.csv'), row.names = FALSE)

ggplot(ud.window) +
  geom_point(aes(x = start, y = hr_est))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#-------------------------------------------------------------------------------

rm(list = ls())
gc()

library(tidyr)
library(lubridate)
library(ggplot2)

data = read.csv('./data/moving_window/moving_window_UDs.csv')

data$ID = as.factor(data$ID)
data$start = as.Date(data$start)
data$year = year(data$start)
data$year = as.factor(data$year)


dat = data %>%
  pivot_longer(!c(year, ID, start), names_to = "HR_type", values_to = "95_estimate")
#dat$start = as.POSIXct(dat$start,  format = '%Y-%m-%d %H:%M', tz='UTC')
dat$week = week(dat$start)
dat$HR_type = as.factor(dat$HR_type)

#__________________________________________________________________________

#1)
ggplot() +
  geom_smooth(dat = dat[dat$HR_type == "hr_est",], aes(x = start, y = `95_estimate`, color = year), linetype = 'solid') +
  # scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = '95% Home Range Estimate') +
  geom_vline(xintercept = c(9,25), linetype = 'dashed') #week 9 = Dec 24, week 25 = Apr 8
ggsave("C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/presentation/home_range.jpg")

#2)
ggplot() +
  geom_smooth(dat = dat[dat$HR_type == "hr_est",], aes(x = start, y = `95_estimate`, color = year), linetype = 'solid') +
  # scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = '95% Home Range Estimate') +
  geom_vline(xintercept = c(9,25), linetype = 'dashed') #+  #week 9 = Dec 24, week 25 = Apr 8
  # facet_wrap(period~herd, ncol = 2)

#-------------------------------------------------------------------------------

#average home range for year, herd and start time:
dat1 = data %>% 
  dplyr::group_by(year, start) %>%
  dplyr::summarise(hr_ave_low = mean(hr_low), hr_ave_estimate = mean(hr_est), hr_ave_high = mean(hr_high))


dat2 = dat1 %>%
  pivot_longer(!c(year, start), names_to = "HR_type", values_to = "HR_estimate")
dat2$week = week(dat2$start)
dat2$HR_type = as.factor(dat2$HR_type)
#calculate weeks from October 31 (winter.week)
dat2$winter.week = ifelse(dat2$week == 44, 1, 
                          ifelse(dat2$week == 45, 2,
                                 ifelse(dat2$week == 46, 3,
                                        ifelse(dat2$week == 47, 4,
                                               ifelse(dat2$week == 48, 5, 
                                                      ifelse(dat2$week == 49, 6,
                                                             ifelse(dat2$week == 50, 7,
                                                                    ifelse(dat2$week == 51, 8,
                                                                           ifelse(dat2$week == 52, 9, 
                                                                                  ifelse(dat2$week == 53, 10, 
                                                                                         dat2$week + 10))))))))))

#update year so we have years instead of descriptors:
dat2$year = as.character(dat2$year)
dat2[which(dat2$year == "prior2"),"year"] <- "2019"
dat2[which(dat2$year == "prior1"),"year"] <- "2020"
dat2[which(dat2$year == "during"),"year"] <- "2021"
dat2[which(dat2$year == "after"),"year"] <- "2022"
dat2$year = as.factor(dat2$year)

#3)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == 'hr_ave_estimate',], aes(x = start, y = HR_estimate, color = period), linetype = 'solid') +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_low",], aes(x = winter.week, y = HR_estimate, color = period), linetype = 'dashed', se = FALSE) +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_high",], aes(x = winter.week, y = HR_estimate, color = period), linetype = 'dashed', se = FALSE) +
  scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = 'Log-scaled 95% Home Range Estimate', trans = 'log10') +
  geom_vline(xintercept = c(4,25), linetype = 'dashed') #week 9 = Dec 24, week 25 = Apr 8

#4)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == 'hr',], aes(x = winter.week, y = HR_estimate, color = period), linetype = 'solid') +
  scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
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
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_estimate",], aes(x = winter.week, y = HR_estimate, color = period),span = 0.1, linetype = 'solid') +
  geom_point(data = dat2[dat2$HR_type == "hr_ave_estimate",], aes(x = winter.week, y = HR_estimate, color = period), alpha = 0.5) +
  scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = 'Log Scaled 95% Home Range Estimate', trans = 'log10') +
  facet_wrap(period~herd, ncol = 2, scales = 'free')
ggsave("C:/Users/Ryan/OneDrive/ABMI/caribou_anthropause/presentation/home_range_by_period_herd_log_scaled_2022-06-03.jpg", width = 10, height = 12)
#-------------------------------------------------------------------------------
#5)
ggplot() +
  geom_smooth(data = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == 'cn',], aes(x = winter.week, y = HR_estimate, color = period), span = 0.5, linetype = 'solid') +
  geom_point(data = dat2[dat2$HR_type == "hr_ave_estimate" & dat2$herd == 'cn',], aes(x = winter.week, y = HR_estimate, color = period), alpha = 0.5) +
  scale_x_discrete(name ="Weeks from October 31", limits = c(1:30)) +
  scale_y_continuous(name = 'Log Scaled 95% Home Range Estimate', trans = 'log10') +
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
  group_by(period, winter.week) %>%
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