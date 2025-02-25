
# devtools::install_github("ctmm-initiative/ctmm", force = TRUE)
library(ctmm)
library(lutz)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tictoc)


dat <- read.csv('./data/input_data/20240626_moving_window_formatted_for_tele_dat_all.csv')

dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")

period = 'TEST'
tel.dat = dat[dat$year == "2023",]


collars = tel.dat %>% distinct(individual.local.identifier)
DATA = as.telemetry(tel.dat, timeformat = '%Y-%m-%d %H:%M:%S', timezone = 'UTC')


#dt = 1 move forward by 1 day
#win = 3 day window span

# Define dt and win in seconds
dt <- 1 * 24 * 60 * 60  # 1 day in seconds
win <- 3 * 24 * 60 * 60 # 3 days in seconds

# List all the window section times available in the dataset in continuous segments of 3 days without overlapping segments
#initial (start) time : DATA$t[1] 
#final (end) time: DATA$t[nrow(DATA)] - win
test <- DATA[[1]]
DATA <- test




#create directories to hold output:
dir.create(paste0("./data/input_data/moving_window2/", period, "/Fits"), recursive = TRUE)
dir.create(paste0("./data/input_data/moving_window2/", period, "/UDs"), recursive = TRUE)

DATA <- DATA[1:2]

for (i in 1:length(DATA)) {
    # Generate start times for each 3-day segment
    start_times <- seq(
      as.POSIXct(format(DATA[[i]]$timestamp[1], "%Y-%m-%d 00:00:00", tz = "UTC")), 
      as.POSIXct(format(DATA[[i]]$timestamp[nrow(DATA[[i]])], "%Y-%m-%d 00:00:00", tz = "UTC")) - win, 
      by = win)
    
    # Create a dataframe to store the start and end times of each segment
    times <- data.frame(start_time = start_times,
                        end_time = start_times + win - 1)
    
    cat("There are", length(start_times), "window segments for Collar ID", DATA[[i]]@info$identity, "\n")
    
    #initialize arrays (lists) for results
    AREAS_lo <- rep(NA,length(times))
    AREAS_ml <- rep(NA,length(times))
    AREAS_hi <- rep(NA,length(times))
    }



# i = 1
window.HR <- function(data, Fig_Path, Result_Path, UD_path) {
  
  
  
  
  
  # SECTION: iteration of time intervals for window segment
  for(j in 1:length(times)) {
    
    # Indicate what iteration the analysis is currently on
    print(paste((j), "of", length(times), "iterations", "- Collar ID:", DATA[[i]]@info$identity))
    
    SUBSET <- DATA[[i]][DATA[[i]]$timestamp >= times$start_time[j] & DATA[[i]]$timestamp <= times$end_time[j], ]
    
    # Check if there is any data in the subset
    if (nrow(SUBSET) == 0) {
      cat("No data found or data is missing for window section in iteration", j, "- moving on to the next iteration.\n")
      
      # Set results to NA
      AREAS_lo[j] <- NA
      AREAS_ml[j] <- NA
      AREAS_hi[j] <- NA
      
      next # Move to the next iteration if the subset is empty
    }}
    
    # Analyze subset
    GUESS <- ctmm.guess(SUBSET, interactive = FALSE)
    FIT <- try(ctmm.select(SUBSET, GUESS))
    
    if (class(FIT) == "ctmm") {
      AKDE <- akde(SUBSET, FIT)
      
      # Get start date and add the date of each window segment in the filename when saving
      start_date <- times$start_time[j]
      filename <- format(start_date, "%Y-%m-%d")
      
      # Save AKDE 'date_animal_period_UD'
      akde.path <- file.path(UD_path, paste0(filename, '_', period, "_UD_", DATA[[i]]@info$identity, ".rda"))
      save(AKDE, file = akde.path)
      
      # Save fitted model 'date_animal_period_Fits.rda'
      mod_path <- file.path(Result_Path, paste0(filename, '_', period, "_Fits_", DATA[[i]]@info$identity, ".rda"))
      save(FIT, file = mod_path)
      
      # Store results
      SUMMARY <- summary(AKDE, units = FALSE)
      AREAS_lo[j] <- SUMMARY$CI[[1]]
      AREAS_ml[j] <- SUMMARY$CI[[2]]
      AREAS_hi[j] <- SUMMARY$CI[[3]]
    } else {
      # If FIT is not a ctmm object, set results to NA
      AREAS_lo[j] <- NA
      AREAS_ml[j] <- NA
      AREAS_hi[j] <- NA
    }}})
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


#.........................................................................
# Run the function ----
#.........................................................................

START <- Sys.time()
tic()

results = lapply(data,
                 window.HR,
                 # Fig_Path = paste0(getwd(),"/data/input_data/moving_window2/", period),
                 Result_Path = paste0(getwd(),"/data/input_data/moving_window2/", period,'/Fits'),
                 UD_path = paste0(getwd(),"/data/input_data/moving_window2/", period,'/UDs'))

toc()
END <- Sys.time()

save(results, file = "./data/input_data/moving_window2/test.rda")
# saveRDS(results, "./data/input_data/moving_window/20240624_moving_window_results_during.rds")
# saveRDS(results, "./data/input_data/moving_window/20240624_moving_window_results_normal.rds")

load("./data/input_data/moving_window/20240628_moving_window_results_all_dat.rda")