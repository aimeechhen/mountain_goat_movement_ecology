

# Moving window results

library(lubridate)
library(tidyverse)

# load saved results
# load("./data/moving_window/moving_window_1d_movement_covariates_centroid_20240711.rda")
load("./data/moving_window/moving_window_1e_save_outputs_20240720.rda")

# turn a list (of elements/list(s)) into a dataframe then combine them into a single dataframe
mw_dat <- do.call(rbind, lapply(RES, as.data.frame))
colnames(mw_dat)[colnames(mw_dat) == "ID"] <- "collar_id"

# add goat name
goat_info <- read.csv("data/goat_info.csv")
mw_dat <- merge(mw_dat, goat_info[, c("goat_name", "goat_id", "collar_id")], by = "collar_id", all.x = TRUE)
mw_dat <- relocate(mw_dat, c("goat_id", "goat_name"), .before = collar_id)

# data carpentry
mw_dat$date <- as.Date(mw_dat$timestamp)
mw_dat$year <- year(mw_dat$timestamp)
mw_dat$month <-  month(mw_dat$timestamp)
mw_dat$day <- day(mw_dat$timestamp)
mw_dat$doy <- yday(mw_dat$timestamp)
mw_dat$month_day <- format(mw_dat$timestamp, format = "%m-%d")
mw_dat <- relocate(mw_dat, c('date', 'year', 'month', 'day', 'doy', 'month_day'), .after = 'timestamp')
mw_dat$timestamp = as.POSIXct(mw_dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
mw_dat$date = as.Date(mw_dat$date, "%Y-%m-%d")
mw_dat$year <- as.numeric(mw_dat$year)
mw_dat$month <- as.numeric(mw_dat$month)
mw_dat$day <- as.numeric(mw_dat$day)
mw_dat$doy <- as.numeric(mw_dat$doy)



#__________________________________________________________________
# Plot moving window results ----

# 1) Plot home range estimates over time (based on Ryan's code in the window.hr function)

#Set the par to plot all on same screen
par(mgp = c(1.5, 0.5, 0),
    oma=c(0,0,0,0), 
    mar=c(3,3,2,2), 
    cex.lab=1.2, 
    cex.main = 1, 
    family = "serif")

# Plot of the range estimates over time
plot(mw_dat$hr_est ~ mw_dat$date, 
     pch=19, 
     cex=1.25, 
     ylim=c(0, max(mw_dat$hr_est, na.rm = TRUE)), 
     ylab = "Home Range Area", 
     # xlab = "Date",
     xlab = "",
     xaxt = "n") # blocks x axis tick values
# arrows(mw_dat$timestamp, 
#        mw_dat$hr_min, 
#        mw_dat$timestamp, 
#        mw_dat$hr_max, 
#        length = 0.05, angle = 90, code = 3)
# title(main = "b)", adj = 0)

# #Warning messages:
# 1: In arrows(mw_dat$timestamp, mw_dat$hr_min, mw_dat$timestamp,  ... :
#                zero-length arrow is of indeterminate angle and so skipped

# #Warning is due to when hr_min = hr_max .-. causing zero-length arrows, tried filtering them out 
# non_zero_arrows <- mw_dat$hr_min != mw_dat$hr_max
# 
# # and add arrows only for non-zero-length ranges but same warning/issue occurred so skipped it
# arrows(mw_dat$timestamp[non_zero_arrows], 
#        mw_dat$hr_min[non_zero_arrows], 
#        mw_dat$timestamp[non_zero_arrows], 
#        mw_dat$hr_max[non_zero_arrows], 
#        length = 0.05, angle = 90, code = 3)

# Adjust x-axis with custom month labels
axis(1, 
     at = seq(
       from = as.Date(min(mw_dat$timestamp, na.rm = TRUE)), 
       to = as.Date(max(mw_dat$timestamp, na.rm = TRUE)), 
       by = "month"), 
     labels = format(seq(
       from = as.Date(min(mw_dat$timestamp, na.rm = TRUE)), 
       to = as.Date(max(mw_dat$timestamp, na.rm = TRUE)), 
       by = "month"), "%b-%y"), 
     las = 2)  # Rotate labels vertically



#__________________________________________________________________
# Fire dates ----

#fire ignition date (i.e. fire start date): 2023-07-22
#fire extinguished date (i.e. fire end date): unknown; .-. using end of fire season (2023-09-30) as end date as the fire was still burning but contained ('held') via news reports ~Sept 4&7, 2023 (and still burning Sept 17th?)
# as per bc data catalogue end date oct 26, 2023

fire_start <- '2023-07-22' # doy = 203
fire_end <- '2023-10-26' # doy = 299

# create a column based on if the window segment was during the wildfire or not
mw_dat$fire_presence <- ifelse(mw_dat$date >= fire_start & mw_dat$date <= fire_end, 1, 0) # 1 = yes, 0 = no

# identify the period each window segment is in based before, during and post wildfire time period
mw_dat$period <- NA
mw_dat$period <- ifelse(mw_dat$date < fire_start, 'before',
                        ifelse(mw_dat$date >= fire_start & mw_dat$date <= fire_end, 'during',
                               'post'))



#__________________________________________________________________
# MW results overall ----

#check for NA values
anyNA(mw_dat$hr_est)
#locate NA values
na_hr <- mw_dat[!complete.cases(mw_dat$hr_est),] #24

# home range units = square meters
#calculate the mean total home range size across all window segments (convert square m into km^2)
round(mean(mw_dat$hr_est, na.rm = TRUE)/1000000, 2)
#calculate CIs of the mean total home range size across all window segments (convert square m into km^2)
round(mean(mw_dat$hr_min, na.rm = TRUE)/1000000, 2) # ci low
round(mean(mw_dat$hr_max, na.rm = TRUE)/1000000, 2) # ci high

#............................................................
# hr vs fire_presence ----

#number of home range entries in each fire_presence category
table(mw_dat$fire_presence) # contains NAs

# no fire
# calculate the mean total home range based on fire_presence (convert square m into km^2)
round(mean(mw_dat$hr_est[mw_dat$fire_presence == "0"], na.rm = TRUE)/1e6, 2)
#calculate CIs of the mean total home range size across fire window segments (convert square m into km^2)
round(mean(mw_dat$hr_min[mw_dat$fire_presence == "0"], na.rm = TRUE)/1e6, 2)
round(mean(mw_dat$hr_max[mw_dat$fire_presence == "0"], na.rm = TRUE)/1e6, 2)

# with fire
# calculate the mean total home range based on fire_presence (convert square m into km^2)
round(mean(mw_dat$hr_est[mw_dat$fire_presence == "1"], na.rm = TRUE)/1e6, 2)
#calculate CIs of the mean total home range size across fire window segments (convert square m into km^2)
round(mean(mw_dat$hr_min[mw_dat$fire_presence == "1"], na.rm = TRUE)/1e6, 2)
round(mean(mw_dat$hr_max[mw_dat$fire_presence == "1"], na.rm = TRUE)/1e6, 2)


#...........
# min/max value mean home range

# no fire
#calculate min/max home range based on fire presence (convert m to km)
round(min(mw_dat$hr_est[mw_dat$fire_presence == "0"], na.rm = TRUE)/1e6, 2) # smallest home range size
round(max(mw_dat$hr_est[mw_dat$fire_presence == "0"], na.rm = TRUE)/1e6, 2) # largest home range size

# with fire
round(min(mw_dat$hr_est[mw_dat$fire_presence == "1"], na.rm = TRUE)/1e6, 2)# smallest home range size
round(max(mw_dat$hr_est[mw_dat$fire_presence == "1"], na.rm = TRUE)/1e6, 2) # largest home range size


#..............
library(lme4)
# Did home range size differ during a fire?

# drop NA values within hr_est
mw_dat2 <- mw_dat[complete.cases(mw_dat$hr_est),] #24

#test for significance in fire, compare model with and without fire as a variable
# home range size (response variable) = was continuous and strictly positive .-. gamma distribution and log link
fire_test <- glmer(hr_est ~ fire_presence + (1|ID), family = Gamma('log'), 
                   # weights = DOF_area,
                   data = mw_dat2, na.action = "na.fail")

fire_test2 <- glmer(hr_est ~ 1 + (1|ID), family = Gamma('log'), 
                    # weights = DOF_area,
                    data = mw_dat2, na.action = "na.fail")

fire_test_results <- anova(fire_test, fire_test2)
fire_test_pvalue <- round(fire_test_results$`Pr(>Chisq)`[2], 2)
fire_test_pvalue





#............................................................
## elevation vs fire_presence ----
# Did elevation differ based on fire presence?
mw_dat[!complete.cases(mw_dat$mean_el),]
mw_dat2 <- mw_dat[complete.cases(mw_dat$mean_el),] #24

#test for significance in elevation, compare model with and without elevation as a variable
el_test <- glmer(mean_el ~ fire_presence + (1|ID), family = Gamma('log'),
                 # weights = DOF_area,
                 data = mw_dat2, na.action = "na.fail")

el_test2 <- glmer(mean_el ~ 1 + (1|ID), family = Gamma('log'),
                  # weights = DOF_area,
                  data = mw_dat2, na.action = "na.fail")

el_test_results <- anova(el_test, el_test2)
el_test_pvalue <- round(el_test_results$`Pr(>Chisq)`[2], 2)
el_test_pvalue


#............................................................
## dis to esc terrain vs fire presence ----
# Did distance to escape terrain differ based on fire presence?

#test for significance in distance to escape terrain, compare model with and without distance to escape terrain as a variable
escape_test <- glmer(mean_dist_escape ~ fire_presence + (1|ID), family = Gamma('log'),
                     # weights = DOF_area,
                     data = mw_dat2, na.action = "na.fail")

escape_test2 <- glmer(mean_dist_escape ~ 1 + (1|ID), family = Gamma('log'),
                      # weights = DOF_area,
                      data = mw_dat2, na.action = "na.fail")

escape_test_results <- anova(escape_test, escape_test2)
escape_test_pvalue <- round(escape_test_results$`Pr(>Chisq)`[2], 2)
escape_test_pvalue


#............................................................
## slope vs fire presence ----
# Did slope differ based on fire presence?

#test for significance in slope, compare model with and without slope as a variable
slope_test <- glmer(mean_slope ~ fire_presence + (1|ID), family = Gamma('log'),
                    # weights = DOF_area,
                    data = mw_dat2, na.action = "na.fail")

slope_test2 <- glmer(mean_slope ~ 1 + (1|ID), family = Gamma('log'),
                     # weights = DOF_area,
                     data = mw_dat2, na.action = "na.fail")

slope_test_results <- anova(slope_test, slope_test2)
slope_test_pvalue <- round(slope_test_results$`Pr(>Chisq)`[2], 2)
slope_test_pvalue
























#............................................................
# hr vs period ----
# Did home range size differ between periods?


# only 2 periods
# # calculate the mean total home range during the fire (convert square m into km^2)
# round(mean(mw_dat$hr_est[mw_dat$period == "during"], na.rm = TRUE)/1e6, 2)
# 
# #calculate CIs of the mean total home range size across all window segments (convert square m into km^2)
# round(mean(mw_dat$hr_min[mw_dat$period == "during"], na.rm = TRUE)/1e6, 2)
# round(mean(mw_dat$hr_max[mw_dat$period == "during"], na.rm = TRUE)/1e6, 2)

# #test for significance in period, compare model with and without period as a variable
# hr_test <- glmer(hr_est ~ period + (1|ID), family = Gamma('log'), 
#                      # weights = DOF_area,
#                      data = mw_dat2, na.action = "na.fail")
# 
# hr_test2 <- glmer(hr_est ~ 1 + (1|ID), family = Gamma('log'), 
#                  # weights = DOF_area,
#                  data = mw_dat2, na.action = "na.fail")
# 
# hr_test_results <- anova(hr_test, hr_test2)
# hr_test_pvalue <- round(hr_test_results$`Pr(>Chisq)`[2], 2)




# 
# #number of home range entries in each period category
# table(mw_dat2$period)
# 
# #calculate mean home range & home range based on period categories (convert m to km)
# round(mean(mw_dat2$hr_est[mw_dat2$period == "before"])/1e6, 2)
# round(min(mw_dat2$hr_est[mw_dat2$period == "before"])/1e6, 2)
# round(max(mw_dat2$hr_est[mw_dat2$period == "before"])/1e6, 2)
# 
# round(mean(mw_dat2$hr_est[mw_dat2$period == "during"])/1e6, 2)
# round(min(mw_dat2$hr_est[mw_dat2$period == "during"])/1e6, 2)
# round(max(mw_dat2$hr_est[mw_dat2$period == "during"])/1e6, 2)
# 
# round(mean(mw_dat2$hr_est[mw_dat2$period == "post"])/1e6, 2)
# round(min(mw_dat2$hr_est[mw_dat2$period == "post"])/1e6, 2)
# round(max(mw_dat2$hr_est[mw_dat2$period == "post"])/1e6, 2)



# 
# #............................................................
## Did elevation differ between periods? ----
# 
# #test for significance in elevation, compare model with and without elevation as a variable
# el_test <- glmer(mean_el ~ period + (1|ID), family = Gamma('log'), 
#                  # weights = DOF_area,
#                  data = mw_dat2, na.action = "na.fail")
# 
# el_test2 <- glmer(mean_el ~ 1 + (1|ID), family = Gamma('log'), 
#                   # weights = DOF_area,
#                   data = mw_dat2, na.action = "na.fail")
# 
# el_test_results <- anova(el_test, el_test2)
# el_test_pvalue <- round(el_test_results$`Pr(>Chisq)`[2], 2)
# 
# 
# 
# #............................................................
## Did distance to escape terrain differ between periods? ----
# 
# #test for significance in distance to escape terrain, compare model with and without distance to escape terrain as a variable
# escape_test <- glmer(mean_dist_escape ~ period + (1|ID), family = Gamma('log'), 
#                      # weights = DOF_area,
#                      data = mw_dat2, na.action = "na.fail")
# 
# escape_test2 <- glmer(mean_dist_escape ~ 1 + (1|ID), family = Gamma('log'), 
#                       # weights = DOF_area,
#                       data = mw_dat2, na.action = "na.fail")
# 
# escape_test_results <- anova(escape_test, escape_test2)
# escape_test_pvalue <- round(escape_test_results$`Pr(>Chisq)`[2], 2)
# 
# 
# 
# #............................................................
## Did slope differ between periods? ----
# 
# #test for significance in slope, compare model with and without slope as a variable
# slope_test <- glmer(mean_slope ~ period + (1|ID), family = Gamma('log'), 
#                     # weights = DOF_area,
#                     data = mw_dat2, na.action = "na.fail")
# 
# slope_test2 <- glmer(mean_slope ~ 1 + (1|ID), family = Gamma('log'), 
#                      # weights = DOF_area,
#                      data = mw_dat2, na.action = "na.fail")
# 
# slope_test_results <- anova(slope_test, slope_test2)
# slope_test_pvalue <- round(slope_test_results$`Pr(>Chisq)`[2], 2)
