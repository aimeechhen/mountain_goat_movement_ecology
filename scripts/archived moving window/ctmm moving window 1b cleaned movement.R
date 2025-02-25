

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

rm(list = ls())

dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")
# period = 'all_data'
tel.dat <- dat
# tel.dat = dat[dat$year == "2022",]
# tel.dat <- tel.dat[tel.dat$month_day >= '07-01' & tel.dat$month_day <= '07-31', ]

data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
collars = tel.dat %>% distinct(individual.local.identifier)

# data <- data[1:2]

# #create directories to hold output:
# dir.create(paste0("./data/input_data/moving_window/", period, "/Fits"), recursive = TRUE)
# dir.create(paste0("./data/input_data/moving_window/", period, "/UDs"), recursive = TRUE)
# 
# 
# dt = 1 %#% 'day' # %#% converts it into days (si units)
# win <- 3 %#% 'day'
# data <- data[[1]]
# i <- 1
# DATA = data

#convert coordinate locations into sf object
dat.sf <- st_as_sf(dat, coords = c('location.long', 'location.lat'))

# set crs
st_crs(dat.sf) <- 4326

#convert sf into spatvector object
locations <- vect(dat.sf)
crs(locations)





#...............................................................
# window.hr function ----
#...............................................................

window.HR <- function(DATA, dt, win) {
  
  tryCatch({
    # 1. Set up the time intervals and empty lists to hold the results ----
    times <- seq(from = DATA$t[1], # t = Unix timestamp format
                 to = DATA$t[nrow(DATA)],  
                 by = dt)
  })
  #initialize arrays (lists) for results
  hr_min = rep(NA, length(times))
  hr_est = rep(NA, length(times))
  hr_max = rep(NA, length(times))
  tau_p_min = rep(NA, length(times))
  tau_p_est = rep(NA, length(times))
  tau_p_max = rep(NA, length(times))
  tau_v_min = rep(NA, length(times))
  tau_v_est = rep(NA, length(times))
  tau_v_max = rep(NA, length(times))
  diffusion_min = rep(NA, length(times))
  diffusion_est = rep(NA, length(times))
  diffusion_max = rep(NA, length(times))
  speed_min = rep(NA, length(times))
  speed_est = rep(NA, length(times))
  speed_max = rep(NA, length(times))
  
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
      hr_min[i] <- NA
      hr_est[i] <- NA
      hr_max[i] <- NA
      # tau_p_min[i] <- NA
      # tau_p_est[i] <- NA
      # tau_p_max[i] <- NA
      # tau_v_min[i] <- NA
      # tau_v_est[i] <- NA
      # tau_v_max[i] <- NA
      # diffusion_min[i] <- NA
      # diffusion_est[i] <- NA
      # diffusion_max[i] <- NA
      # speed_min[i] <- NA
      # speed_est[i] <- NA
      # speed_max[i] <- NA
      
      next # Move to the next iteration if the subset is empty
    }
    
    # 4. If data exist in the subset, proceed with analysis and analyze the data in the subset ----
    tryCatch({
      if (nrow(SUBSET) > 0) {
        GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
        FIT <- try(ctmm.select(SUBSET, GUESS))
        
        if (inherits(FIT, "ctmm")) {
          # calculate home range area
          AKDE <- akde(SUBSET, FIT)
          # calculate mean speed
          SPEED <- speed(SUBSET, FIT, robust = TRUE, units = FALSE)
          
          # 5. Store results ----
          SUMMARY_HR <- summary(AKDE, units = FALSE)
          hr_min[i] <- SUMMARY_HR$CI[[1]]
          hr_est[i] <- SUMMARY_HR$CI[[2]]
          hr_max[i] <- SUMMARY_HR$CI[[3]]
          # Extract mean speed values (units = meters/sec) (time-averaged speed, proportional to distance travelled)
          # OU model causes 'Inf' because not enough data to estimate
          speed_min[i] <- SPEED$CI[[1]]
          speed_est[i] <- SPEED$CI[[2]]
          speed_max[i] <- SPEED$CI[[3]]
          # extract movement metrics
          SUMMARY_FIT <- summary(FIT, units = FALSE)$CI
          # OU-mini f Tau isnt separated into Tau position/tau velocity, it cannot distinguish between velocity and auto-corr (auto-corr position and auto-corr velocity, model is saying they're just going straight)
          tau_p_min[i] <- SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), 1]
          tau_p_est[i] <- SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), 2]
          tau_p_max[i] <- SUMMARY_FIT[grepl("position", row.names(SUMMARY_FIT)), 3]
          tau_v_min[i] <- SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), 1]
          tau_v_est[i] <- SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), 2]
          tau_v_max[i] <- SUMMARY_FIT[grepl("velocity", row.names(SUMMARY_FIT)), 3]
          diffusion_min[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 1]
          diffusion_est[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 2]
          diffusion_max[i] <- SUMMARY_FIT[grepl("diffusion", row.names(SUMMARY_FIT)), 3]
          
        } else {
          # If FIT is not a ctmm object, set results to NA
          hr_min[i] <- NA
          hr_est[i] <- NA
          hr_max[i] <- NA

        }
      } else {
        # If SUBSET is empty, set results to NA
        hr_min[i] <- NA
        hr_est[i] <- NA
        hr_max[i] <- NA

        
        cat("No data found or data is missing for window section in iteration", i, "- setting results to NA.\n")
      }
    }, error = function(e) {
      # Set results to NA in case of error
      hr_min[i] <- NA
      hr_est[i] <- NA
      hr_max[i] <- NA
      # tau_p_min[i] <- NA
      # tau_p_est[i] <- NA
      # tau_p_max[i] <- NA
      # tau_v_min[i] <- NA
      # tau_v_est[i] <- NA
      # tau_v_max[i] <- NA
      # diffusion_min[i] <- NA
      # diffusion_est[i] <- NA
      # diffusion_max[i] <- NA
      # speed_min[i] <- NA
      # speed_est[i] <- NA
      # speed_max[i] <- NA
    })
  }
  
  # 6. Prepare results dataframe ----
  tryCatch({
    TIMES <- as.POSIXct(times, origin="1970-01-01",
                        tz = lutz::tz_lookup_coords(DATA$latitude[1],
                                                    DATA$longitude[1]))
    RESULTS <- data.frame(ID = DATA@info$identity,
                          Time = TIMES,
                          hr_min,
                          hr_est,
                          hr_max,
                          tau_p_min,
                          tau_p_est,
                          tau_p_max,
                          tau_v_min,
                          tau_v_est,
                          tau_v_max,
                          diffusion_min,
                          diffusion_est,
                          diffusion_max,
                          speed_min,
                          speed_est,
                          speed_max
    )
  })
  
  
  
  return(RESULTS)
}


#.........................................................................
# Run the function ----
#.........................................................................

#~ 12.5 hours
# with speed ~23hours
START <- Sys.time()
tic()
RES <- list()
for(i in 1:length(data)){
  
  # RES[[i]] <- window.HR(data[[i]][1:30,],
  RES[[i]] <- window.HR(data[[i]],
                        dt = 1 %#% 'day', # what does %#% 'day' do?
                        win <- 3 %#% 'day')
  
  # save(RES, file = "./data/input_data/moving_window/Sliding_Window_20240705_noonan.Rda")
  save(RES, file = "./data/input_data/moving_window/moving_window_movement_20240711.rda")
  
}
toc()
END <- Sys.time()

RESULTS <- do.call(rbind, lapply(RES, as.data.frame))
# moving_window_movement_20240711.rda: issue with diffusion not being saved, not sure why


# head(RES[[10]])