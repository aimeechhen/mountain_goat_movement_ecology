
# Moving window rewritten to fix window gap i.e missing data in a window segment

# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)


rm(list = ls())

#...............................................................
# Import data ----
#...............................................................

dat <- read.csv('./data/input_data2/20240703_moving_window_formatted_for_tele_dat.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")

period = 'all_data'

tel.dat <- dat
# tel.dat = dat[dat$year == "2021",]

data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
collars = tel.dat %>% distinct(individual.local.identifier)

#create directories to hold output:
dir.create(paste0("./data/input_data2/moving_window/", period, "/Fits"), recursive = TRUE)
dir.create(paste0("./data/input_data2/moving_window/", period, "/UDs"), recursive = TRUE)


#...............................................................
# Window function ----
#...............................................................

# Define dt and win in seconds
dt <- 1 * 24 * 60 * 60  # 1 day in seconds,
win <- 3 * 24 * 60 * 60 # 3 days in seconds, window segments

# Result_Path = paste0(getwd(),"/data/input_data/moving_window2/", period,'/Fits')
# UD_path = paste0(getwd(),"/data/input_data/moving_window2/", period,'/UDs')
# data <- data[1:2]

window.HR <- function(data, dt, win, Fig_Path, Result_Path, UD_path, 
                      # log_file = "error_log.txt"
) {
  DATA = data
  #initialize vectors for results
  AREAS_lo <- numeric()
  AREAS_ml <- numeric()
  AREAS_hi <- numeric()
  TIMES <- as.POSIXct(character())
  
  # #open a log file to save error messages
  # log_conn <- file(log_file, open = "a")  # "a" for append mode
  
  # 1. List all the window section times available in the dataset in continuous segments of 3 days without overlapping segments ----
  for (i in 1:length(data)) {
    # for (i in seq_along(data)) {
    
    #generate start times for each 3-day segment
    start_times <- seq(
      from = as.POSIXct(format(data[[i]]$timestamp[1], "%Y-%m-%d 00:00:00", tz = "UTC")), 
      to = as.POSIXct(format(data[[i]]$timestamp[nrow(data[[i]])], "%Y-%m-%d 00:00:00", tz = "UTC")) - win, 
      by = win
    )
    
    #create a dataframe to store start and end times
    times <- data.frame(
      start_time = start_times,
      end_time = start_times + win - 1,
      AREAS_lo = NA, # Initialize areas with NA
      AREAS_ml = NA,
      AREAS_hi = NA
    )
    cat("There are", nrow(times), "window segments for Collar ID:", DATA[[i]]@info$identity, "\n")
    
    # 2. loop through each window segment
    for (j in 1:nrow(times)) {
      start_date <- times$start_time[j]
      end_date <- times$end_time[j]
      
      #subset data in the window segment
      SUBSET <- data[[i]][data[[i]]$timestamp >= start_date & data[[i]]$timestamp <= end_date, ]
      
      print(paste0("Processing ", i, " of ", length(DATA), 
                   ". Iteration ", j, " of ", nrow(times), 
                   ". At window segment ", format(start_date, "%Y-%m-%d"), 
                   " for Collar ID: ", DATA[[i]]@info$identity))
      
      #check if subset is empty or if there are data
      if (nrow(SUBSET) == 0) {
        cat("No data found or data is missing for window section on iteration", j, " of ", nrow(times), "- moving on to the next iteration.\n")
        # Set results to NA if no data
        AREAS_lo[j] <- NA
        AREAS_ml[j] <- NA
        AREAS_hi[j] <- NA
        
      } else {
        # 3. analyze subset if data are present
        tryCatch({
          GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
          FIT <- ctmm.select(SUBSET, GUESS)
          AKDE <- akde(SUBSET, FIT)
          
          # 4. save analysis outputs ----
          # save fitted model 'date_period_Fits_animal.rda'
          mod_path <- file.path(UD_path, paste0(format(start_date, "%Y-%m-%d"), DATA[[i]]@info$identity, "_Fits_", DATA[[i]]@info$identity, ".rda"))
          saveRDS(FIT, file = mod_path)
          #save AKDE 'date_period_UD_animal.rda'
          akde_path <- file.path(paste0(format(start_date, "%Y-%m-%d"), "_UD_", DATA[[i]]@info$identity, ".rda"))
          saveRDS(AKDE, file = akde_path)
          
          # store results in arrays
          SUMMARY <- summary(AKDE, units = FALSE)
          AREAS_lo[j] <- SUMMARY$CI[[1]]
          AREAS_ml[j] <- SUMMARY$CI[[2]]
          AREAS_hi[j] <- SUMMARY$CI[[3]]
          
        }, error = function(e) {
          # cat("Error in processing window segment:", format(start_date, "%Y-%m-%d"), "-", format(end_date, "%Y-%m-%d"), "for Collar ID:", data[[i]]@info$identity, "\n")
          # # record error message in log file
          # error_message <- paste("Error in processing window segment: Iteration", j, "of", length(start_times), "-", format(start_date, "%Y-%m-%d"), "-", format(end_date, "%Y-%m-%d"), "for Collar ID:", data[[i]]@info$identity)
          # writeLines(error_message, con = log_conn)
          
          #set error results to NA
          cat("No data found or data is missing for window section in iteration", j, " of ", nrow(times), 
              "- setting results to NA.\n")
          AREAS_lo[j] <- NA
          AREAS_ml[j] <- NA
          AREAS_hi[j] <- NA
        })
      }
      
      TIMES[j] <- start_date  # Store start time in TIMES vector
    }
  }
  
  # # Close the log file connection
  # close(log_conn)
  
  # Store results in a dataframe
  RESULTS <- data.frame(
    TIMES = TIMES,
    AREAS_lo = AREAS_lo,
    AREAS_ml = AREAS_ml,
    AREAS_hi = AREAS_hi
  )
  
  return(RESULTS)
}



#.........................................................................
# Run the function ----
#.........................................................................


results <- window.HR(data,
                     dt = dt,
                     win = win,
                     Fig_Path = paste0(getwd(), "/data/input_data2/moving_window/", period),
                     Result_Path = paste0(getwd(), "/data/input_data2/moving_window/", period, '/Fits'),
                     UD_path = paste0(getwd(), "/data/input_data2/moving_window/", period, '/UDs'),
                     # log_file = paste0(getwd(), "/data/input_data2/moving_window/", period, "_error_log.txt")
)


#________________________________________________________________________
# Import window.hr result files ----


#load rda:
getwd()
setwd(paste0(getwd(),'/data/input_data2/moving_window/',period,'/UDs'))
ud.period = list.files(pattern="*.rda", all.files=TRUE, 
                       full.names=FALSE)

ud.period.uds <- list()

for(i in 1:length(ud.period)){
  load(ud.period[[i]])
  ud.period.uds[[i]] <- AKDE
}



#................................................
# Create dataframe and extract results ----
#................................................
load('20240703_moving_window_results.rda')

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
setwd("C:/Users/achhen/OneDrive - UBC/Github/mountain_goat_movement_ecology/data/input_data2/moving_window/")

write.csv(ud.period.window, paste0(period,'_20240703_moving_window_UDs.csv'), row.names = FALSE)
setwd('../../../')




#______________________________________________________________
#Plot all the results and save them as a png ----

ggplot(ud.period.window) +
  geom_point(aes(x = start, y = hr_est)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# issue with plotting within the function due to missing data therefore
# Plot outside of the original function and after to be able to account for missing data

plot_results <- function(results, Fig_Path, data, period, index) {
  # Extract start times and area estimates from the results
  TIMES <- results$times$start_time
  AREAS_lo <- results$AREAS_lo
  AREAS_ml <- results$AREAS_ml
  AREAS_hi <- results$AREAS_hi
  
  fig.path <- file.path(Fig_Path, paste0(period, "_", data@info$identity, ".png"))
  
  #Save the graphic device's output as a png
  png(file=fig.path, 
      type="cairo", 
      units = "in", 
      width = 6.81, 
      height = 3, 
      pointsize = 10, 
      res = 600)
  
  #Set the par to plot all on same screen
  par(mfrow=c(1,2), 
      mgp = c(1.5, 0.5, 0),
      oma=c(0,0,0,0), 
      mar=c(3,3,2,2), 
      cex.lab=1.2, 
      cex.main = 1, 
      family = "serif")
  
  # a) Plot the relocation data, coloured by time (i.e. plot position/location over time, coloured by time)
  #Create a function that scales colours between red and blue (a gradient to represent over time)
  rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
  #Then create a variable that scales from red to blue between the two times (i.e. assign colors to each position based on the time)
  data$Col <- rbPal(nrow(data))[as.numeric(cut(as.numeric(data$timestamp), breaks = nrow(data)))]
  plot(data, 
       col.grid = NA, 
       pch = 20, 
       cex = 0.2,
       # col.DF = "#669543", 
       col.UD = "#669543", 
       col = data$Col, 
       labels=FALSE)
  title(main = "a)", adj = 0)
  
  # b) Plot of the range estimates over time
  plot(AREAS_ml ~ TIMES, 
       pch=19, 
       cex=1.25, 
       ylim=c(0, max(AREAS_hi, na.rm = TRUE)), 
       ylab = "Home Range Area", 
       xlab = "Date", 
       xaxt = "n")
  arrows(TIMES, 
         AREAS_lo, 
         TIMES, 
         AREAS_hi, 
         length = 0.05, angle = 90, code = 3)
  title(main = "b)", adj = 0)
  
  # Adjust x-axis with month labels
  axis(1, 
       at = seq(
         from = min(TIMES, na.rm = TRUE), 
         to = max(TIMES, na.rm = TRUE), 
         by = "month"), 
       labels = format(seq(
         from = min(TIMES, na.rm = TRUE), 
         to = max(TIMES, na.rm = TRUE), 
         by = "month"), "%b"))
  
  dev.off()
}



# Plot the results and save the png
# for (i in 1:length(DATA)) {
#   plot_results(results[i], paste0(getwd(), "./data/input_data/moving_window2/", period), DATA[[i]], period)
# }

for (i in seq_along(DATA)) {
  plot_results(results[[i]], 
               DATA[[i]], period, i,
               Fig_Path = paste0(getwd(),"/data/input_data2/moving_window/", period))
}

# Warning messages:
# 1: In arrows(TIMES, AREAS_lo, TIMES, AREAS_hi, length = 0.05,  ... :
#                zero-length arrow is of indeterminate angle and so skipped
#(for 16 items)




