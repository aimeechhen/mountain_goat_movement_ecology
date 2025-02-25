
library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)

dat3 <- read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
dat <- dat3

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")

period = 'all'
# tel.dat <- dat
dat2 = dat[dat$year == "2023",]
tel.dat <- dat2[dat2$month_day >= '05-01' & dat2$month_day <= '05-31', ]

data = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')
collars = tel.dat %>% distinct(individual.local.identifier)

data <- data[3:4]

#create directories to hold output:
dir.create(paste0("./test/data/input_data/moving_window/", period, "/Fits"), recursive = TRUE)
dir.create(paste0("./test/data/input_data/moving_window/", period, "/UDs"), recursive = TRUE)

i <- 1

#...............................................................
# Window function ----
#...............................................................

window.HR <- function(data, dt, win, Fig_Path, Result_Path, UD_path,  log_file = "error_log.txt") {
  DATA = data
  # #open a log file to save error messages
  # log_conn <- file(log_file, open = "a")  # "a" for append mode
  
  
  # SECTION: Set up the time intervals and empty lists to hold the results
  tryCatch({
    #DATA$t[1]: initial time, final time: DATA$t[nrow(DATA)], via interval time: by = dt
    times <- seq(
      from = tel.dat$t[1], 
      to = tel.dat$t[nrow(tel.dat)], 
      by = win)
    # chop off end of times
    # times <- times[-((length(times)-win/dt+1):length(times))]
    
  }, error = function(e) {
    writeLines(paste("Error in setting up time intervals:", e$message), log_conn)
  })
  
  # Initialize arrays (lists) for results
  AREAS_lo <- rep(NA, length(times))
  AREAS_ml <- rep(NA, length(times))
  AREAS_hi <- rep(NA, length(times))
  
  # times <- data.frame(start_time = start_times,
  #                     end_time = start_times + win - 1)
  
  
  
  # SECTION: iteration of time intervals for window section
  for(i in 1:length(times)) {
    
    #indicate what iteration the analysis is currently on
    # print(paste((i),"of",length(times),"iterations", "- Collar ID:", DATA@info[1]))
    
    # print(paste0("Processing ", i, " of ", length(DATA), 
    #              ". Iteration ", i, " of ", nrow(times), 
    #              ". At window segment ", times[i], 
    #              " for Collar ID: ", DATA[[i]]@info$identity))
    # subset for the time segment
    SUBSET <- DATA[times[i] <= DATA$t & DATA$t <= times[i]+win,]
    
    start_date <- times[i]
    end_date <- start_date + win - 1
    
    print(paste0("Processing ", i, " of ", length(data), 
                 ". Iteration ", i, " of ", tel.dat$t[nrow(tel.dat)], 
                 ". At window segment from ", 
                 format(as.POSIXct(times[i], origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), 
                 " to ", 
                 format(as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), 
                 " for Collar ID: ", 
                 DATA@info[1]))
    
    
    
    
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
        
        if (inherits(FIT, "ctmm")) {
          AKDE <- akde(SUBSET, FIT)
          
          #to not overwrite each iteration, need to add the date of each window as file name:
          # fname = DATA[1]$timestamp
          # filename = lubridate::date(fname[i])
          
          #save AKDE 'date_animal_period_UD'
          akde.path <- file.path(Result_Path, paste0(format(start_date, "%Y-%m-%d"), '_', period, "_UD_", DATA@info[1], ".rda"))
          save(AKDE, file = akde.path)
          
          #save fitted model 'date_animal_period_Fits.rda'
          mod_path <- file.path(UD_path, paste0(format(start_date, "%Y-%m-%d"), '_', period,"_Fits_", DATA@info[1], ".rda"))
          save(FIT, file = mod_path)
          
          # Store results
          SUMMARY <- summary(AKDE, units = FALSE)
          AREAS_lo[i] <- SUMMARY$CI[[1]]
          AREAS_ml[i] <- SUMMARY$CI[[2]]
          AREAS_hi[i] <- SUMMARY$CI[[3]]
        } 
        else {
          # If FIT is not a ctmm object, set results to NA
          AREAS_lo[i] <- NA
          AREAS_ml[i] <- NA
          AREAS_hi[i] <- NA
        }
      }
      else {
        # If SUBSET is empty, set results to NA
        AREAS_lo[i] <- NA
        AREAS_ml[i] <- NA
        AREAS_hi[i] <- NA
        
        cat("No data found or data is missing for window section in iteration", i, "- setting results to NA.\n")
      }
    }, error = function(e) {
      # cat("Error in processing window segment:",
      #     format(as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), 
      #     " to ", 
      #     format(as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), 
      #     " for Collar ID: ", 
      #     DATA@info[1])
      # record error message in log file
      error_message <- paste("Error in processing window segment:",  
                             format(as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), 
                             " to ", 
                             format(as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d"), 
                             " for Collar ID: ", 
                             DATA@info[1])
      writeLines(error_message, con = log_conn)
      
      # Set results to NA in case of error
      cat("No data found or data is missing for window section in iteration", i, " of ", nrow(times), "- setting results to NA.\n")
      AREAS_lo[i] <- NA
      AREAS_ml[i] <- NA
      AREAS_hi[i] <- NA
    })
  }
  
  
  # Close the log file connection
  close(log_conn)
  
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


# NOTE: might be an error with the filename date being saved, the dates go until august however, when plotting, its the proper date range (for during, normal still rerunning)



#.........................................................................
# Run the function ----
#.........................................................................

START <- Sys.time()
tic()

results = lapply(data,
                 window.HR,
                 dt = 1 %#% 'day',
                 win <- 3 %#% 'day',
                 Fig_Path = paste0(getwd(),"/test/data/input_data/moving_window/", period),
                 Result_Path = paste0(getwd(),"/test/data/input_data/moving_window/", period,'/Fits'),
                 UD_path = paste0(getwd(),"/test/data/input_data/moving_window/", period,'/UDs'),
                 log_file = paste0(getwd(), "/data/input_data2/moving_window/", period, "_error_log.txt"))

toc()
END <- Sys.time()
