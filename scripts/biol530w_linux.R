




library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)
library(lubridate)
library(tidyverse)
library(glmmTMB)
library(crayon)


#...........................................................
# Import data ----
#...........................................................

# # import original collar data
# load("data/collar_data/collar_data_20241123.rda")
# # specify which dataset its from
# collar_data$data_type <- "original_data"
# 
# # update dataset to reflect the name changes 2025-02
# collar_data$goat_name[collar_data$goat_name == "kid_rock" & collar_data$collar_id == "30561"] <- "selena_goatmez"
# collar_data$goat_name[collar_data$goat_name == "rocky" & collar_data$collar_id == "30613" ] <- "goatileo"
# collar_data$goat_name[collar_data$goat_name == "vertigoat" & collar_data$collar_id == "30567" ] <- "goatbriella"
# collar_data$goat_name[collar_data$goat_name == "billy" & collar_data$collar_id == "30636" ] <- "ryan_goatsling"
# 
# # import cleaned new collar data ----
# load("data/collar_data/new_collar_data_20250218.rda")
# new_collar$collar_id <- as.factor(new_collar$collar_id)
# # drop columns that isn't needed 
# new_collar <- subset(new_collar, select = c(collar_id, timestamp, latitude, longitude, data_type))
# 
# # Import supplementary mountain goat info 
# goat_info <- read.csv("data/goat_info.csv")
# goat_info$collar_id <- as.factor(goat_info$collar_id)
# 
# new_collar <- merge(new_collar, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
# new_collar$goat_name <- as.factor(new_collar$goat_name)
# # new_collar$timestamp <- as.POSIXct(new_collar$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
# new_collar$date <- as.Date(new_collar$timestamp)
# new_collar$year <- year(new_collar$timestamp)
# new_collar$month <- month(new_collar$timestamp, label = FALSE) #label = false for numerical month
# new_collar$day <- day(new_collar$timestamp)
# new_collar$month_day <- format(new_collar$timestamp, "%m-%d")
# new_collar$doy <- yday(new_collar$timestamp) #day of the year
# 
# # combine two dataframes and match the same column names together and whatever columns are missing, they are just added and given NA values
# goat_data <- dplyr::bind_rows(collar_data, new_collar)


goat_data <- read.csv("./data/combined_goat_data_fire_period_all_years.csv")



# formatting
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)

#format names to match required for ctmm based on Movebank critera:
goat_data <- plyr::rename(goat_data, c('collar_id' = 'individual.local.identifier',
                                       'latitude' = 'location.lat', 
                                       'longitude' = 'location.long'))

# convert data to a ctmm telemetry object
tel_data <- as.telemetry(goat_data, mark.rm = TRUE)


#...........................................................
# 1. Movement models ----
#...........................................................


#create an empty list to store output
FITS <- list()

START_movement <- Sys.time()
tic(msg = "movement models analysis")

for(i in 1:length(tel_data)){
  message(bgWhite(cyan("Currently on animal ", i, " of ", length(tel_data))))
  #Extract individual
  DATA <- tel_data[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process
  # fit movement models and select best fit
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=50)
   # beep(8)
  
}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h, #9.698333 mins; full = ~12.5min
END_movement <- Sys.time()


dir.create("data/movement_model/", recursive = TRUE, showWarnings = TRUE)
save(FITS,file="data/movement_model/fits_biol530w.rda")


#...........................................................
# 2. Home range ----
#...........................................................

load("data/movement_model/fits_biol530w.rda")

# Estimate akdes home-range areas
START_hr <- Sys.time()
tic(msg = "home range analysis")

AKDES <- akde(tel_data,FITS,weights=TRUE)

toc() # 14.2 mins
END_hr <- Sys.time()

#save rda:
dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)
save(AKDES,file="data/home_range/akdes_biol530w.rda")



load("data/home_range/akdes_biol530w.rda")



#...........................................................
# 2b. Overlap ----
#...........................................................

#calculate 95% AKDE home range overlap for a pairwise comparison
hr_overlap <- overlap(AKDES, level = 0.95)

#indicate the matrix layers to be extracted
matrix_level <- c('low', 'est', 'high')
num_matrices <- dim(hr_overlap$CI)[3]
matrix_cube <- list() # to store the 3 matrix layers


# extract overlap values from each matrix layer
for (i in 1:num_matrices) {
  #extract CI matrix from array
  matrix_layer <- hr_overlap$CI[,,i]
  #remove duplicate values of the matrix
  matrix_layer[upper.tri(matrix_layer, diag = TRUE)] <- NA
  #Create a new data frame based on the overlap values
  matrix_layer <- as.data.frame(matrix_layer)
  #extract year from rowname
  matrix_layer$year <-  sub(".*?_(.*)", "\\1", rownames(matrix_layer))
  # Rename columns to remove year component for pivoting
  colnames(matrix_layer) <- sub("^(.*?)_.*", "\\1", colnames(matrix_layer))
  # extract collar_id from rowname
  matrix_layer$goat_A <- sub("^(.*?)_.*", "\\1", rownames(matrix_layer))
  # Rename rows to remove year component for pivoting
  rownames(matrix_layer) <- sub("^(.*?)_.*", "\\1", rownames(matrix_layer))
  overlap_col_name <- paste0('overlap_', matrix_level[i])
  matrix_layer$year <- as.factor(matrix_layer$year)
  #Reshape the matrix from wide format (each goat as a column) to long format (one row per goat pair)
  matrix_layer <- pivot_longer(matrix_layer, cols = -c(goat_A, year),
                               names_to = 'goat_B', values_to = overlap_col_name,
                               values_drop_na = TRUE) 
  matrix_layer <- as.data.frame(matrix_layer)
  # store the matrix layer
  matrix_cube[[i]] <- matrix_layer
}


# Merge all matrices (low, est, high)
overlap_result <- matrix_cube[[1]] # extract one layer to start the df
# add the other two extracted layers to the df
for (i in 2:num_matrices) {
  overlap_result <- left_join(overlap_result, matrix_cube[[i]], 
                              by = c("goat_A", "goat_B", "year"))
}

#create an empty dataframe for the results
overlap_df <- data.frame()
# put the results into the empty dataframe
overlap_df <- rbind(overlap_df, overlap_result)



# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]

# extract goat_info$goat_name value based on matching the two collar_id 
overlap_df$name_A <- goat_info$goat_name[match(overlap_df$goat_A, goat_info$collar_id)]
overlap_df$name_B <- goat_info$goat_name[match(overlap_df$goat_B, goat_info$collar_id)]
overlap_df$sex_A <- goat_info$Sex[match(overlap_df$goat_A, goat_info$collar_id)]
overlap_df$sex_B <- goat_info$Sex[match(overlap_df$goat_B, goat_info$collar_id)]

# assign them a pair id number based on their collar ids
overlap_df$pair_ID <- paste(overlap_df$goat_A, overlap_df$goat_B, sep = "_")
overlap_df <- relocate(overlap_df, pair_ID, .before = goat_A)
#add column to indicate which sexes that are being compared
overlap_df <- mutate(overlap_df,
                     sex_comparison = case_when(paste(sex_A, sex_B) == "F F" ~ "F-F",
                                                paste(sex_A, sex_B) == "M M" ~ "M-M",
                                                paste(sex_A, sex_B) == "F M" ~ "F-M",
                                                paste(sex_A, sex_B) == "M F" ~ "F-M"))

# reorganize df
overlap_df <- relocate(overlap_df, c(name_A, name_B, sex_A, sex_B, sex_comparison), .after = goat_B)


#save
save(object = overlap_df, file = "data/social/overlap_df.rda")





#............................................................
# Home range overlap results ----
#............................................................

#calculate mean total home range overlap 
round(mean(overlap_df$overlap_est), 2)

#calculate range of total home range overlap 
round(min(overlap_df$overlap_est), 2)
round(max(overlap_df$overlap_est), 2)

#............................................................
# Is sex a factor?

#rescale the values
min_val <- min(overlap_df$overlap_est)
max_val <- max(overlap_df$overlap_est)
squeeze_min <- 0.001
squeeze_max <- 0.999
overlap_df$overlap_est_squeezed <- ((overlap_df$overlap_est - min_val) / (max_val - min_val)) * (squeeze_max - squeeze_min) + squeeze_min
overlap_df <- relocate(overlap_df, overlap_est_squeezed, .after = overlap_high)

#test for significance in sex, compare model with and without sex as a variable
HRO_test <- glmmTMB(overlap_est_squeezed ~ sex_comparison + (1|year), family = beta_family(link = "logit"), data = overlap_df)
HRO_test2 <- glmmTMB(overlap_est_squeezed ~ 1 + (1|year), family = beta_family(link = "logit"), data = overlap_df)
HRO_test_results <- anova(HRO_test, HRO_test2)
HRO_test_pvalue <- round(HRO_test_results$`Pr(>Chisq)`[2], 2)

#number of home range overlap in each sex comparison category
table(overlap_df$sex_comparison)

#calculate mean home range overlap & range based on sex comparison categories
round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "F-F"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "F-F"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "F-F"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "M-M"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "M-M"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "M-M"]), 2)

round(mean(overlap_df$overlap_est[overlap_df$sex_comparison == "F-M"]), 2)
round(min(overlap_df$overlap_est[overlap_df$sex_comparison == "F-M"]), 2)
round(max(overlap_df$overlap_est[overlap_df$sex_comparison == "F-M"]), 2)





#............................................................
# 3. Proximity ----
#............................................................

#create empty columns for results to be saved to
overlap_df$proximity_low <- NA
overlap_df$proximity_est <- NA
overlap_df$proximity_high <- NA


#calculate the proximity statistics
for(i in 1:nrow(hr_overlap$CI)){
  # Extract animal indices from columns 'anteater_A' and 'anteater_B'
  ANIMAL_A <- overlap_df[i, 'goat_A']
  ANIMAL_B <- overlap_df[i, 'goat_B']

  # Extract tracking data for the pair of animals
  TEL_DATA <- tel_data[c(ANIMAL_A, ANIMAL_B)] # extract animal by name, has extra layers .-. it doesnt work, that is why
  MODELS <- list(FITS[ANIMAL_A][[1]], FITS[ANIMAL_B][[1]])
  
  # Use tryCatch to handle potential errors during the calculation of the proximity statistic
  prox <- tryCatch(
    {
      # Attempt to calculate the proximity statistic using the proximity function
      PROXIMITY <- proximity(data = TEL_DATA, CTMM = MODELS, GUESS=ctmm(error=FALSE))},
    
    # If an error occurs during the try block, execute the error block
    error=function(err){
      
      # Print an error message indicating that an error occurred (added this line, issues running this code post pre-print version)
      cat("Error occurred at index", i, ": ", conditionMessage(err), "\n")
      
      # If an error occurs, set the proximity values to NA
      PROXIMITY <- c(NA,NA,NA)
      
      # Return the NA values
      return(PROXIMITY)
    }
  )
  
  # Assign the proximity values to the corresponding columns 
  overlap_df[i, c("proximity_low")] <- PROXIMITY[1]
  overlap_df[i, c("proximity_est")] <- PROXIMITY[2]
  overlap_df[i, c("proximity_high")] <- PROXIMITY[3]
  
  # #save results to a csv file
  write.csv(overlap_df, "data/social/proximity_data.csv", row.names = FALSE)
  
  cat("finished index", i, "\n") # see the loop happening in real time
}




