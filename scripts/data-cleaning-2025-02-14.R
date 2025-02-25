# how to data clean panic mode with Stefano
library('dplyr')     # for data wrangling (mutate(), %>%, etc.)
library('tidyr')     # for data wrangling (nest(), unnest(), pivot_*, etc.)
library('purrr')     # for functional programming (map_***(), etc.)
library('ctmm')      # for movement models
library('lubridate') # for working with dates
library('mapview')   # for interactive maps
library('ggplot2')   # for fancy plots
theme_set(theme_bw())

# source custom functions
source('functions/outlier_plots.R') # to plot outlier diagnostic plots, original custom function was modified to include plot titles
source('functions/check_animal.R') # to run diagnostic plots
source('functions/plot_adj.R') # to plot 20 adjacent locations
source('functions/flag_outlier.R') # to mark outliers
source('functions/remove_outlier_flags.R') # to start over with an animal

# preparing the df for processing
# import data, data wrangling
d <- readr::read_csv("data/collar_data/raw_collar/Cathedral Goat locations Sept 2023 through Feb 10 2025.csv") %>%
  janitor::clean_names() %>% # remove unnecessary characters ("[", " ", etc.) and replace with "_" 
  # modify the df and create new columns
  mutate(animal = collar_id,
         timestamp = as.POSIXct(acq_time_utc), 
         outlier = 0, # currently no outliers; > 0 is an outlier
         original_outliers = outlier) %>% # create a copy of the column for backup purposes
  select(! acq_time_utc) %>%
  nest(tel = ! animal) # tel does not exist, create df for creating the telemetry data

cat(paste('#', unique(d$animal)), sep = '\n')

# the end product should 2 columns with one of them being nested

#______________________________________________________________________________________
# Visually inspect data and look for amy outlier points

#**NOTE**: this must be done individual at a time for every individual, there are no shortcuts, dig in, it might take a while, maybe get some food, water, take a nap before 

# Run visual diagnostics using stefano custom function check_animal()
# check_animal() will produce 5 plots
# look for anything weird such as things described below
# the output of check_animal() will be a df, ctmm outlie object

out <- check_animal('x') # x = animal id

#.....................................................
# Breakdown of the 5 plots

# plot 1: plot of telemetry
# note: data not properly projected, the axes are not equivalent, so it should look a little funky

# plot 2: ctmm outlie plot #1
# red dot: larger the dot = further from median telemetry points
# blue line: thicker = higher straight line displacement
# if you see a red dot or a line that doesn't make sense, like a red dot in the middle of no where or a blue line that doesn't look reasonable like the distance travelled the animal in a day vs what the actually distance that animal can travel in a day, or like a line sticking out randomly in la la land no where near any of the others
# for more information refer to the ctmm function outlie() 

# plot 3: ctmm outlie plot #2
# red = x axis
# blue= y axis
# if you have points in the top right corner = you got an animal moving really fast and very far .-. most likely outlier

# plot 4: speed and turning angle (stefano custom made)
# possible see points top left corner -> at high speed & close to 0, but NOT points top right corner = bad i.e., high speeds at 180 degrees! because that means they are running backwards!!

# plot 5: speed and time between gps points (stefano custom made)
# seeing a decrease is okay
# you will see most of the points at the set time interval, but you will also see points after because if a fix was not recorded, it will look like the sampling frequency was twice as long, and if 2 fixes wasn't recorded back to back it'll look like a fix interval 3x the time length etc.
# note: the plot x axis set at 24h, anything longer than that defeats the purpose of looking at what was their daily speed

#................................................................
# once you have identified any potential outliers, then you must check each of those points manually and pluck them out one or a few at a time to see if they are actually outliers or not
# do one point at a time, if you have more than one point, you will see more than 1 red dot
# look at what they animal is doing, like the points and lines are their movements, if it looks behaviourally realistic then you good
# if not, remove these points because they are deemed outliers 

# extract the potential outliers and visualize using plot_adj() (stefano custom function)
# set parameters to target the points
# e.g. speed over 0.15 and turning angle over 135 and pluck those points and run plot_adj()
# to get a better picture of behaviour, you may want to see points before/after the potential outlier
# set n_adj = x, x being the number of GPS points before/after

plot_adj('x',
         max_speed = 0.15, # speeds < 0.15 m/s are ok, > 0.15 m/s are possibly problematic
         max_angle = 140,  # turning angles < 135 are ok, > 135 are possibly problematic
         n_adj = 10)       # number of points before and after the problematic red point


# if they are outliers, flag them and record it in the df using flag_outlier() (custom stefano function)
# put in animal id, the values you just put in for plot_adj()
# and flag that point as in outlier
# flagging outliers as numerical values such as 0 not an outlier, 1 as for sure outlier and some other value like 0.5 or 2 as a MAYBE outlier

flag_outlier(id = 'x', max_speed = 0.15, max_angle = 140, value = 1) # modify values as needed


# Then, remove flagged outlier using remove_outlier_flags() (custom stefano function), the outlier needs to be flagged or it won't work
remove_outlier_flags(id = 'x') 

#__________________________________________________________________

# 30561 (selena_goatmez)
out <- check_animal('30561')
plot_adj('30561',
         max_speed = 0.15, # speeds < 0.15 m/s are ok, > 0.15 m/s are possibly problematic
         max_angle = 140,  # turning angles < 135 are ok, > 135 are possibly problematic
         n_adj = 10)       # number of points before and after the problematic red point

# record flagged outlier and remove outlier point
if(FALSE) {
  sum(d$tel[[which(d$animal == '30561')]]$outlier) # should have no outliers (== 0)
  flag_outlier(id = '30561', max_speed = 0.15, max_angle = 140, value = 1) # to remove a problematic point
  sum(d$tel[[which(d$animal == '30561')]]$outlier) # check to see if it has been recorded properly, should have one outlier (== 1)
  remove_outlier_flags(id = '30561') # to remove outlier labels for this animal
  sum(d$tel[[which(d$animal == '30561')]]$outlier) # check to see if it has been removed, should have no outliers (== 0)
}

# 30575 (the_goatmother)
out <- check_animal('30575')

# 30648 (vincent_van_goat) FLAGGGED
out <- check_animal('30648')
plot_adj('30648', max_speed = 0.15, max_angle = 175, n_adj = 10)
# running backwards for 3km?
plot(tel[-10:10 + which(tel$x < 3e3 & tel$y > 7e3), ])#??????????????



# plot 3 median deviation doesnt work
plot_adj('30648', max_speed = 0.10, median_deviation = 12000, n_adj = 10)
# plot 2 x,y doesnt work
plot_adj('30648', x = 1, y = -1, n_adj = 10)
# how to isolate points to look at them?



# 30548 (goatzilla)
out <- check_animal('30548')
plot_adj('30548', max_speed = 0.13, max_angle = 175, n_adj = 1)


# 30613 (goatileo) maybeeeeee
out <- check_animal('30613')
plot_adj('30613', max_speed = 0.12, max_angle = 170, n_adj = 1)

# 30642 (toats_mcgoats)
out <- check_animal('30642')

