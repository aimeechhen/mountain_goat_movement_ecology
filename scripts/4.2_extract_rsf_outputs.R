# 
# Extract rsf results 

library(ctmm)
library(dplyr)

# data import ----
# load("./data/rsf/rsf_20250301.rda")
load("./data/habitat_use/rsf/rsf.rda")


#______________________________________________________________________________
# check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)


# inspect one summary output of rsf (using concise units)
summary(RSF[[1]])
# CI units:
# esc_dist_scaled (1/esc_dist_scaled) 
# elev_scaled (1/elev_scaled)             
# area (square kilometers)      
# τ[position] (days)            
# τ[velocity] (minutes)         
# speed (kilometers/day)        
# diffusion (square kilometers/day) 

summary(RSF[[1]], units = FALSE)$CI # using SI units
# CI units:
# esc_dist_scaled (1/esc_dist_scaled)      
# elev_scaled (1/elev_scaled)                   
# area (square meters)            
# τ[position] (seconds)       
# τ[velocity] (seconds)            
# speed (meters/second)        
# diffusion (square meters/second)


#........................................
## check units to see if theyre all the same

# i <- 1


# check fitted models units
summary_outputs <- data.frame()
for (i in seq_along(RSF)) {
  summary <- summary(RSF[[i]], units = FALSE)$CI # using SI units
  summary_outputs <- rbind(summary_outputs, 
                           data.frame(Var1 = names(table(rownames(summary))), 
                                      Freq = as.integer(table(rownames(summary)))))
}

summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
summary_outputs

# Var1 Freq
# 1                area (square meters)   35
# 2    diffusion (square meters/second)   35
# 3         elev_scaled (1/elev_scaled)   35
# 4 esc_dist_scaled (1/esc_dist_scaled)   35
# 5               speed (meters/second)   13
# 6               τ[position] (seconds)   35
# 7               τ[velocity] (seconds)   13


# if they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections


#.............................................................
# rsf ----
#.............................................................

# extract rsf results (i.e., the rsf coefficients)

# i = 15
# summary(RSF[[1]], units = FALSE)
# RSF[[i]]@info$identity

rsf_list <- list()

for(i in 1:length(RSF)){
    #create and transpose (flip the rows/columns) a dataframe
  elev <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["elev_scaled (1/elev_scaled)",]))
  elev_cov <- RSF[[i]]$COV['elev_scaled','elev_scaled']
  esc_dist <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["esc_dist_scaled (1/esc_dist_scaled)",])) 
  esc_dist_cov <- RSF[[i]]$COV['esc_dist_scaled','esc_dist_scaled']
  
  c(elev, elev_cov, esc_dist, esc_dist_cov)
  
  res <- cbind(elev, elev_cov, esc_dist, esc_dist_cov)
  # rename the columns based on the order they were extracted
  names(res) <- c("rsf_elev_low",
                  "rsf_elev_est",
                  "rsf_elev_high", 
                  "rsf_elev_cov",
                  "rsf_esc_dist_low",
                  "rsf_esc_dist_est",
                  "rsf_esc_dist_high", 
                  "rsf_esc_dist_cov")
  
  res$AIC <- RSF[[i]]$AIC
  res$BIC <- RSF[[i]]$BIC
  res$AICc <- RSF[[i]]$AICc
  
  
  # extract goat id (text string location 1 to 4)
  res$goat_id <- substr(names(RSF)[i], 1, 4)
  # extract year (text string after the _)
  res$year <- gsub(".*_", "", names(RSF)[i])
  # extract id_year
  res$individual.local.identifier <- RSF[[i]]@info$identity
  # move them to the front of the df
  res <- dplyr::relocate(res, c(goat_id, year, individual.local.identifier), .before = rsf_elev_low)
  
  
  rsf_list[[i]] <- res
}

# convert list into a dataframe
rsf_results <- do.call(rbind, rsf_list)

head(rsf_results)

#....................................................
## Supplementary mountain goat information ----
#....................................................

# Import supplementary mountain goat info 
goat_info <- read.csv("./data/goat/goat_info.csv")
goat_info$goat_id <- as.factor(goat_info$goat_id)

# subset to only the study goats
goats <- c("CA01", "CA04", "CA08", "CA09", "CA11", "CA12")
goat_info <- goat_info[goat_info$goat_id %in% goats,]
# add supplementary info to df
rsf_results <- merge(rsf_results, goat_info[, c("goat_id","goat_name", "collar_id")], by = "goat_id", all.x = TRUE)
rsf_results <- dplyr::relocate(rsf_results, c("goat_id","goat_name", "collar_id"), .before = year)


rsf_results$goat_id <- as.factor(rsf_results$goat_id)
rsf_results$goat_name <- as.factor(rsf_results$goat_name)
rsf_results$collar_id <- as.factor(rsf_results$collar_id)
rsf_results$year <- as.factor(rsf_results$year)
rsf_results$individual.local.identifier <- as.factor(rsf_results$individual.local.identifier)

str(rsf_results)


#.......................................
# save

# dir.create("./results/")
save(rsf_results, file = "./results/rsf_results.rda")
load("./results/rsf_results.rda")

write.csv(rsf_results, file = "./results/rsf_results.csv", row.names = FALSE)
# rsf_results <- read.csv("./results/rsf_results.csv")


# Values near 0 = no preference
# Values below 0 = selected for, to be closer to escape terrain
# values above 0 = selected against





