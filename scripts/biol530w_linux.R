




library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)
library(lubridate)
library(tidyverse)
library(glmmTMB)
library(crayon)
library(tictoc)


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

matrix_layer, overlap_result, matrix_cube
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
dir.create("data/home_range/", recursive = TRUE, showWarnings = TRUE)
save(object = overlap_df, file = "data/home_range/overlap_df.rda")


rm(hr_overlap, matrix_layer, overlap_result, matrix_cube, num_matrices, matrix_level, overlap_col_name)


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


#clean up environment
rm(min_val, max_val, squeeze_min, squeeze_max, HRO_test, HRO_test2, HRO_test_results)


#............................................................
# 3. Proximity ----
#............................................................

#create empty columns for results to be saved to
overlap_df$proximity_low <- NA
overlap_df$proximity_est <- NA
overlap_df$proximity_high <- NA

tic(msg = "proximity")
START_TIME <- Sys.time()

#calculate the proximity statistics
for(i in 1:nrow(hr_overlap$CI)){
  tic(msg = "1 proximity loop")
  # Extract animal indices from columns 'anteater_A' and 'anteater_B'
  ANIMAL_A <- overlap_df[i, 'goat_A']
  ANIMAL_B <- overlap_df[i, 'goat_B']

  # Extract tracking data for the pair of animals
  TEL_DATA <- tel_data[c(ANIMAL_A, ANIMAL_B)] # extract animal by name, has extra layers .-. it doesnt work, that is why
  MODELS <- list(FITS[ANIMAL_A][[1]], FITS[ANIMAL_B][[1]])
  
  # Use tryCatch to handle potential errors during the calculation of the proximity statistic
  prox <- tryCatch(
    {
      # Attempt to calculate the proximity statistic using the proximity function, disabling location error for speed
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
    toc()
  )
  
  # Assign the proximity values to the corresponding columns 
  overlap_df[i, c("proximity_low")] <- PROXIMITY[1]
  overlap_df[i, c("proximity_est")] <- PROXIMITY[2]
  overlap_df[i, c("proximity_high")] <- PROXIMITY[3]
  
  # #save results to a csv file
  write.csv(overlap_df, "data/encounter/proximity_data.csv", row.names = FALSE)
  
  cat("finished index", i, "\n") # see the loop happening in real time
}

END_TIME <- Sys.time()
toc()

proximity_df <- overlap_df


#save proximity dataframe
save(proximity_df, file = "data/encounter/proximity_df.rda")
load("data/encounter/proximity_df.rda")

#clean up environment
rm(ANIMAL_A, ANIMAL_B, TEL_DATA, MODELS, prox, PROXIMITY)


#............................................................
# Proximity ratio results and analysis ----
#............................................................

#test for significance in sex, compare model with and without sex as a variable across all 121 dyads
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "log"), data = proximity_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2) #p = 0.13

prox_overlap_test <- glmer(proximity_est ~ overlap_est + (1|site), family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test2 <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test_results <- anova(prox_overlap_test, prox_overlap_test2)
prox_overlap_test_pvalue <- round(prox_overlap_test_results$`Pr(>Chisq)`[2], 2) #p = 0.03

#test for significance in sex, compare model with and without sex as a variable
proximity_test <- glmer(proximity_est ~ sex_comparison + (1|site), 
                        family = Gamma(link = "log"), data = proximity_df)
proximity_test2 <- glmer(proximity_est ~ 1 + (1|site), 
                         family = Gamma(link = "log"), data = proximity_df)
proximity_test_results <- anova(proximity_test, proximity_test2)
proximity_test_results
proximity_test_pvalue <- round(proximity_test_results$`Pr(>Chisq)`[2], 2) #p = 0.13
proximity_test_pvalue

#test for significance in home-range overlap, compare model with and without overlap as a variable
prox_overlap_test <- glmer(proximity_est ~ overlap_est + (1|site), 
                           family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test2 <- glmer(proximity_est ~ 1 + (1|site), 
                            family = Gamma(link = "log"), data = proximity_df)
prox_overlap_test_results <- anova(prox_overlap_test, prox_overlap_test2)
prox_overlap_test_results
prox_overlap_test_pvalue <- round(prox_overlap_test_results$`Pr(>Chisq)`[2], 2) #p = 0.03
prox_overlap_test_pvalue


# clean up environment
rm(proximity_test, proximity_test2, prox_overlap_test, prox_overlap_test2, proximity_test, proximity_test2, proximity_test_results, prox_overlap_test, prox_overlap_test2, prox_overlap_test_results)


#............................................................
# Distance ----
#............................................................

# Script description: calculate the distances between individuals, sensitivity analysis, estimate encounters between individuals. encounter analysis

#Calculate the distance statistics

RES <- list()
tic(msg = "distance analysis")
for (i in 1:nrow(overlap_df)) {
  tic(msg = "distance loop")
  ANIMAL_A <- as.character(overlap_df[i, 'goat_A']) 
  ANIMAL_B <- as.character(overlap_df[i, 'goat_B'])
  TRACKING_DATA <- tel_data[c(ANIMAL_A, ANIMAL_B)]
  MODELS <- list(FITS[[ANIMAL_A]], FITS[[ANIMAL_B]])
  
  DISTANCES_RES <- tryCatch({
    distances_result <- distances(data = TRACKING_DATA, CTMM = MODELS, GUESS = ctmm(error = FALSE))
    data.frame(pair_ID = paste(ANIMAL_A, ANIMAL_B, sep = "_"),
               distance_low = distances_result$low, 
               distance_est = distances_result$est, 
               distance_high = distances_result$high,
               t = distances_result$t,
               timestamp = distances_result$timestamp)
  }, error = function(err) {
    data.frame(pair_ID = paste(ANIMAL_A, ANIMAL_B, sep = "_"),
               distance_low = NA,
               distance_est = NA,
               distance_high = NA,
               t = NA, 
               timestamp = NA)
  }
  )
  toc()
  RES[[i]] <- DISTANCES_RES
  
  #write.csv(RES, "data/DATA_distance.csv", row.names = FALSE)
  cat("finished index", i, "\n")
}
toc()

#Turn the list of list into a data frame
DATA_DISTANCE <- do.call(rbind, RES)
#save distance data
save(DATA_DISTANCE, file = "data/encounter/distance_data.rda")
load("data/encounter/distance_data.rda")



#check for NA values within the dataframe
DATA_DISTANCE[!complete.cases(DATA_DISTANCE), ] 

#add overlap and proximity information to the distance dataframe
distance_df <- merge(DATA_DISTANCE, proximity_df, by = "pair_ID")
distance_df <- relocate(distance_df, c(distance_low, distance_est, distance_high), .after = proximity_high)
distance_df <- relocate(distance_df, c(timestamp, t), .after = sex_comparison)

#save the distance dataframe
save(distance_df, file = "data/encounter/distance_df.rda")
load("data/encounter/distance_df.rda")





#............................................................
# Encounter ----
#............................................................

#set encounter radius
#larger the radius = more encounters can occur within that radius due to more individuals that can be within the radius (measurements are in meters)
enc_radius <- 0:1000
enc_count <- vector("numeric", length(enc_radius))

#calculate the number of encounters occurring within each radius size
for(i in 1:length(enc_radius)){
  enc_count[i] <- sum(distance_df$distance_est < enc_radius[i])
}

#visualization
plot(x = enc_radius, y = enc_count, type = "l")

#...................................................

#sensitivity analysis on female-male encounter significance
encounter_radius_pvalue <- vector("numeric", length(enc_radius))
pair_ID <- unique(overlap_df$pair_ID)

#Loop over encounter radii
for(i in 1:length(enc_radius)){
  
  res <- list()
  
  for (j in pair_ID){
    subset_A <- distance_df[distance_df$pair_ID == j,]
    
    # Count the number of times "distance_est" is below some threshold distance i 
    encounter_count <- sum(subset_A$distance_est < enc_radius[i])
    
    #save results
    res[[j]] <- data.frame(encounter_count = encounter_count,
                           overlap_est = subset_A$overlap_est[1],
                           sex_comparison = subset_A$sex_comparison[1])
  }
  
  res <- do.call(rbind, res)
  
  # test for significance 
    encounter_radius_test <- try(glmer(encounter_count ~ overlap_est + sex_comparison + (1|site),
                                     family = poisson(link = "log"), data = res, subset = res > 0))
  encounter_radius_test2 <- try(glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = res, subset = res > 0))
  encounter_radius_test_results <- try(anova(encounter_radius_test, encounter_radius_test2))
  p_val <- try(encounter_radius_test_results$`Pr(>Chisq)`[2])
  encounter_radius_pvalue[i] <- ifelse(class(p_val) == "try-error", NA, p_val)
  
  cat("finished index", i, "\n")
}

encounter_radius_df <- data.frame(x = enc_radius,
                                  y = encounter_radius_pvalue)

#save dataframe
save(encounter_radius_df, file = "data/encounter/encounter_radius_df.rda")

#visualization
plot(y ~ x,
     data = encounter_radius_df,
     type = "l",
     xlab = "Encounter radius (m)",
     ylab = "p-value")
abline(0.05, 0)


#............................................................
# Estimating encounters ----
#............................................................

#calculate total encounters of all individuals based on sex comparison type
proximity_df$encounter_count <- NA
pair_ID <- unique(proximity_df$pair_ID)

for (i in pair_ID){
  subset_A <- distance_df[distance_df$pair_ID == i,]
  
  # Count the number of times "distance_est" is below 15
  encounter_count <- sum(subset_A$distance_est < 15)
  
  #save results
  proximity_df[proximity_df$pair_ID == i, "encounter_count"] <- encounter_count
  
}

#number of pairs that had 0 encounters
proximity_df[proximity_df$encounter_count == 0,] #78
#number of pairs that had at least 1 encounter
proximity_df[proximity_df$encounter_count != 0,] #43

#calculate the number of encounters based on threshold
sum(proximity_df$encounter_count)
sum(proximity_df$encounter_count[proximity_df$sex_comparison == "male-male"])
sum(proximity_df$encounter_count[proximity_df$sex_comparison == "female-female"])
sum(proximity_df$encounter_count[proximity_df$sex_comparison == "female-male"])

#............................................................
# Encounter results ----
#............................................................

#effect of sex and overlap on encounter rates (model that does not include 0 encounter counts)
encounter_test <- glmer(encounter_count ~ overlap_est + sex_comparison + (1|site), family = poisson(link = "log"), data = proximity_df, subset = encounter_count > 0)
encounter_test2 <- glmer(encounter_count ~ 1 + (1|site), family = poisson(link = "log"), data = proximity_df, subset = encounter_count > 0)
encounter_test_results <- anova(encounter_test, encounter_test2)
encounter_test_pvalue <- round(encounter_test_results$`Pr(>Chisq)`[2], 2)

# amount of home-range overlap and the number of observed encounters (beta (B) = 4.86 ± 0.148, p = 0.00)
summary(encounter_test)




#............................................................
# Proximity ratio deviations ----
#............................................................

# Script description: identify individuals of interest, deviated pairs analysis, caluculate distances between individuals in the deviated pairs, investigate correlative movement between those individuals, calculate mean correlative movement statistics


#identify pairs that did not have a proximity ratio of 1
proximity_above1 <- proximity_df[proximity_df$proximity_low > 1,]
proximity_below1 <- proximity_df[proximity_df$proximity_high < 1,]

#exclude pairs with a HR overlap of 0
proximity_below1[proximity_below1$overlap_est < 0.0001,]
proximity_below1 <- proximity_below1[!(proximity_below1$overlap_est < 0.0001),]

#create a dataframe of the deviated pairs
proximity_identified_pairs_df <- rbind(proximity_above1, proximity_below1)
proximity_identified_pairs_df$pair_ID_number <- seq(from = 1, to = 12, by = 1)
proximity_identified_pairs_df <- relocate(proximity_identified_pairs_df, pair_ID_number, .before = anteater_A)

#correct the sex_comparison output to female-male
proximity_identified_pairs_df <- mutate(proximity_identified_pairs_df,
                                        sex_comparison = case_when(paste(Sex.A, Sex.B) == "Male Male" ~ "male-male",
                                                                   paste(Sex.A, Sex.B) == "Female Female" ~ "female-female",
                                                                   paste(Sex.A, Sex.B) == "Male Female" ~ "female-male",
                                                                   paste(Sex.A, Sex.B) == "Female Male" ~ "female-male"))

#clean up environment
rm(proximity_above1, proximity_below1)

#save identified pairs dataframe
# save(proximity_identified_pairs_df, file = "data/encounter/proximity_identified_pairs_df.rda")
load("data/encounter/proximity_identified_pairs_df.rda")

#............................................................
# Deviated pairs results ----
#............................................................

#number of pairs with a deviated proximity ratio based on sex comparison (ie. a proximity ratio value not equal to 1)
table(proximity_identified_pairs_df$sex_comparison)

# Proximity ratio sex analysis for identified pairs
#test for significance in sex, compare model with and without sex as a variable
proximity_test_pairs <- glmer(proximity_est ~ sex_comparison + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
proximity_test2_pairs <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
proximity_test_results_pairs <- anova(proximity_test_pairs, proximity_test2_pairs)
proximity_test_pvalue_pairs <- round(proximity_test_results_pairs$`Pr(>Chisq)`[2], 2) #0.16

# Proximity and overlap analysis for identified pairs
prox_overlap_test_pairs <- glmer(proximity_est ~ overlap_est + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
prox_overlap_test2_pairs <- glmer(proximity_est ~ 1 + (1|site), family = Gamma(link = "log"), data = proximity_identified_pairs_df)
prox_overlap_test_results_pairs <- anova(prox_overlap_test_pairs, prox_overlap_test2_pairs)
prox_overlap_test_pvalue_pairs <- round(prox_overlap_test_results_pairs$`Pr(>Chisq)`[2], 2) #0.65


#............................................................
# Estimating distances of the deviated pairs ----
#............................................................

#subset telemetry data and fitted model for each pair
