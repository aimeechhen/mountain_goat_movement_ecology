

library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)


#...............................................................
# Import data ----
#...............................................................

dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
period = 'all_data'
tel.dat <- dat
# tel.dat = dat[dat$year == "2022",]
# tel.dat <- tel.dat[tel.dat$month_day >= '07-01' & tel.dat$month_day <= '07-31', ]

data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
collars = tel.dat %>% distinct(individual.local.identifier)

# data <- data[5:6]

#create directories to hold output:
dir.create(paste0("./data/input_data/moving_window/", period, "/Fits"), recursive = TRUE)
dir.create(paste0("./data/input_data/moving_window/", period, "/UDs"), recursive = TRUE)




#...............................................................
# window.hr function ----
#...............................................................

window.HR <- function(DATA, dt, win) {
  #DATA = data
  
  tryCatch({
    # 1. Set up the time intervals and empty lists to hold the results ----
    times <- seq(from = DATA$t[1], # t = Unix timestamp format
                 to = DATA$t[nrow(DATA)],  
                 by = dt) # should this be by dt or win?
  })
    #initialize arrays (lists) for results
    AREAS_lo <- rep(NA,length(times))
    AREAS_ml <- rep(NA,length(times))
    AREAS_hi <- rep(NA,length(times))
    
    # 2. loop through each window segment ----
    for(i in 1:length(times)) {
      #indicate what iteration the analysis is currently on
      print(paste((i),"of",length(times),"iterations. At window segment", 
                  format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"),
                  "for Collar ID:", DATA@info[1]))
      
      # 3. subset data within window time segment ----
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
      
      # 4. If data exist in the subset, proceed with analysis and analyze the data in the subset ----
      tryCatch({
        if (nrow(SUBSET) > 0) {
          GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
          FIT <- try(ctmm.select(SUBSET, GUESS))
          
          if (inherits(FIT, "ctmm")) {
            AKDE <- akde(SUBSET, FIT)
            
            # 5. Saving analysis outputs and storing results ----
            #to not overwrite each iteration, need to add the date of each window as file name:
            filename <- format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d")
            # fname = DATA[1]$timestamp
            # filename = lubridate::date(fname[i])
            
            #save AKDE 'date_animal_period_UD'
            # akde.path <- file.path(UD_path, paste0(filename, '_', period, "_UD_", DATA@info[1], ".rda"))
            # save(AKDE, file = akde.path)
            # 
            # #save fitted model 'date_animal_period_Fits.rda'
            # mod_path <- file.path(Result_Path, paste0(filename, '_', period,"_Fits_", DATA@info[1], ".rda"))
            # save(FIT, file = mod_path)
            
            ## 5b. store results ----
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
    
    # 6. Prepare results dataframe ----
    tryCatch({
      TIMES <- as.POSIXct(times, origin="1970-01-01",
                          tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                      DATA$longitude[1]))
      RESULTS <- data.frame(ID = DATA@info$identity,
                              Time = TIMES,
                            AREAS_lo,
                            AREAS_ml,
                            AREAS_hi)
    })
    
    
    
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # 7. Plot all the results and save them as a png ----
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 
    # fig.path <- file.path(Fig_Path,
    #                       paste(DATA@info[1], ".png", sep = ""))
    # 
    # #Save the graphic device's output as a png
    # png(file=fig.path,
    #     type="cairo",
    #     units = "in",
    #     width = 6.81, height = 3,
    #     pointsize = 10,
    #     res = 600) #
    # 
    # #Set the par to plot all on same screen
    # par(mfrow=c(1,2),
    #     mgp = c(1.5, 0.5, 0),
    #     oma=c(0,0,0,0),
    #     mar=c(3,3,2,2),
    #     cex.lab=1.2,
    #     cex.main = 1,
    #     family = "serif") 
    # 
    # # a) Plot the relocation data, coloured by time (i.e. plot position/location over time, coloured by time)
    # #Create a function that scales colours between red and blue (a gradient to represent over time)
    # rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
    # #Then create a variable that scales from red to blue between the two times (i.e. assign colors to each position based on the time)
    # data$Col <- rbPal(nrow(data))[as.numeric(cut(as.numeric(data$timestamp), breaks = nrow(data)))]
    # plot(data, 
    #      col.grid = NA, 
    #      pch = 20, 
    #      cex = 0.2,
    #      # col.DF = "#669543", 
    #      col.UD = "#669543", 
    #      col = data$Col, 
    #      labels=FALSE)
    # title(main = "a)", adj = 0)
    # 
    # # b) Plot of the range estimates over time
    # plot(AREAS_ml ~ TIMES, 
    #      pch=19, 
    #      cex=1.25, 
    #      ylim=c(0, max(AREAS_hi, na.rm = TRUE)), 
    #      ylab = "Home Range Area", 
    #      xlab = "Date", 
    #      xaxt = "n")
    # arrows(TIMES, 
    #        AREAS_lo, 
    #        TIMES, 
    #        AREAS_hi, 
    #        length = 0.05, angle = 90, code = 3)
    # title(main = "b)", adj = 0)
    # 
    # # Adjust x-axis with month labels
    # axis(1, 
    #      at = seq(
    #        from = min(TIMES, na.rm = TRUE), 
    #        to = max(TIMES, na.rm = TRUE), 
    #        by = "month"), 
    #      labels = format(seq(
    #        from = min(TIMES, na.rm = TRUE), 
    #        to = max(TIMES, na.rm = TRUE), 
    #        by = "month"), "%b"))
    # 
    # dev.off()
    
    return(RESULTS)
}


#.........................................................................
# Run the function ----
#.........................................................................

#~ 12.5 hours mins
tic()
RES <- list()
for(i in 1:length(data)){
  
  RES[[i]] <- window.HR(data[[i]],
                        dt = 1 %#% 'day', # what does %#% 'day' do?
                        win <- 3 %#% 'day')
  
  save(RES, file = "./data/input_data/moving_window/Sliding_Window_20240705_noonan.Rda")
  
  
}
toc()


head(RES[[10]])




# 
# # 
# # results = lapply(data,
# #                  window.HR,
# #                  dt = 1 %#% 'day', # what does %#% 'day' do?
# #                  win <- 3 %#% 'day',
# #                  Fig_Path = paste0(getwd(),"data/input_data/moving_window/", period),
# #                  Result_Path = paste0(getwd(),"data/input_data/moving_window/", period,'/Fits'),
# #                  UD_path = paste0(getwd(),"data/input_data/moving_window/", period,'/UDs'))
# 
# 
# 
# 
# 
# #________________________________________________________________________
# # Import window.hr result files ----
# 
# # UDs are missing? Where is the rest?
# # checked the data prep, all data are there up to last day of data
# # When plotting you can see the gaps
# 
# #load rda:
# # in version 4, files are corrupted, cannot read them in
# getwd()
# setwd(paste0(getwd(),'./data/input_data/moving_window/',period,'/UDs'))
# 
# ud.period = list.files(pattern="*.rda", all.files=TRUE, 
#                        full.names=FALSE)
# ud.period.uds <- list()
# 
# for(i in 1:length(ud.period)){
#   load(ud.period[[i]])
#   ud.period.uds[[i]] <- AKDE
# }
# 
# 
# #................................................
# # Create dataframe and extract results ----
# #................................................
# 
# # NOTE: moved the following files out of the folder because it contains no data from the folder, its causing an error
# # 2019-08-24_all_data_UD_30648.rda
# # 2019-08-24_all_UD_30648.rda
# # 2019-08-24_normal_UD_30648.rda
# # 2023-03-13_all_data_2023_UD_30642.rda
# 
# hr.list = list()
# 
# for(i in 1:length(ud.period.uds)){
#   #i = 10
#   hr <- as.data.frame(period)
#   hr$ID = ud.period.uds[[i]]@info$identity
#   hr$start = str_sub(print(ud.period[[i]]),1,10)
#   hr$hr_low = summary(ud.period.uds[[i]], units = FALSE)$CI[1,"low"]/1000000
#   hr$hr_est = summary(ud.period.uds[[i]], units = FALSE)$CI[1,"est"]/1000000
#   hr$hr_high = summary(ud.period.uds[[i]], units = FALSE)$CI[1,"high"]/1000000
#   hr.list[[i]] = hr
#   
# }
# 
# ud.period.window = do.call(rbind, hr.list)
# 
# 
# # setwd('../../')
# # write.csv(ud.period.window, paste0(period,'_2024XXXX_moving_window.csv'), row.names = FALSE)
# setwd("C:/Users/achhen/OneDrive - UBC/Github/mountain_goat_movement_ecology/data/input_data/moving_window/")
# 
# write.csv(ud.period.window, paste0(period,'_20240628_moving_window_UDs.csv'), row.names = FALSE)
# 
# setwd("C:/Users/achhen/OneDrive - UBC/Github/mountain_goat_movement_ecology/")
# 
# 
# 
# 
# 
# 
# #......................................................................
# # Plot results on calendar scale ----
# #......................................................................
# 
# 
# 
# mw.dat <- ud.period.window
# # mw.dat <- read.csv('./data/input_data/moving_window/all_data_20240628_moving_window_UDs.csv')
# # mw.dat <- read.csv("./data/input_data2/moving_window/all_data_20240703_moving_window_UDs")
# 
# mw.dat = mw.dat %>%
#   pivot_longer(!c(period, ID, start), names_to = "HR_type", values_to = "95_estimate")
# 
# mw.dat$start = as.Date(mw.dat$start)
# mw.dat$period = as.factor(mw.dat$period)
# mw.dat$ID = as.factor(mw.dat$ID)
# mw.dat$year = year(mw.dat$start)
# mw.dat$month = month(mw.dat$start)
# mw.dat$day <- day(mw.dat$start)
# mw.dat$doy <- yday(mw.dat$start)
# mw.dat$HR_type = as.factor(mw.dat$HR_type)
# 
# 
# # Define the range and the extended range for the x-axis
# start_doy <- 203 # July 22 = day 203
# end_doy <- 273   # Sept 30 = day 273
# extend_days <- 5 # Extend the range by a few days on each end
# 
# 
# 
# 
# #........................................................................
# ## Plot single goat ----
# #........................................................................
# 
# ggplot(data = mw.dat[mw.dat$HR_type == "hr_est" & mw.dat$ID == "30548",]) +
#   geom_smooth(aes(x = doy, y = `95_estimate`, color = as.factor(year), group = as.factor(year), linetype = 'solid')) +
#   geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
#   ggtitle("30548") +
#   scale_y_continuous(name = '95% Home Range Estimate') +
#   scale_x_continuous(name = 'Month',
#                      limits = c(-5, 340), expand = c(0, 1), # so the plot extends a bit after december
#                      breaks = seq(0, 365, by = 30),
#                      labels = c(month.abb, "")) + # Use month abbreviations
#   scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                                "2020" = "#ddaa33",
#                                                "2021" = "#006d2c",
#                                                "2022" = "#33bbee",
#                                                "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))
# 
# #........................................................................
# ## Plot all goats figure via facet wrap ----
# #........................................................................
# 
# ggplot(data = mw.dat[mw.dat$HR_type == "hr_est",]) +
#   geom_smooth(aes(x = doy, y = `95_estimate`, color = as.factor(year), group = as.factor(year), linetype = 'solid')) +
#   geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, scales = "free", ncol = 3, nrow = 4) +  # Facet by ID, adjust ncol as needed
#   # ggtitle("Home Range Estimates for ID: {facet_var}") +  # Dynamic title using facet variable
#   scale_y_continuous(name = '95% Home Range Estimate') +
#   scale_x_continuous(name = 'Month',
#                      limits = c(-5, 340), expand = c(0, 1), # so the plot extends a bit after december
#                      breaks = seq(0, 365, by = 30),
#                      labels = c(month.abb, "")) + # Use month abbreviations
#   scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                                "2020" = "#ddaa33",
#                                                "2021" = "#006d2c",
#                                                "2022" = "#33bbee",
#                                                "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))
# 
# 
# 
