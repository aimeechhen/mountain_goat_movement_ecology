# 
# Extract rsf results 

library(ctmm)


# data import ----
# load("./data/rsf/rsf_20250301.rda") 
# load("./data/rsf/rsf_20250505.rda")
# load("./data/rsf/rsf_scaled_20250505.rda")
load(".scripts/working/data/rsf/rsf_20250902.rda")


#______________________________________________________________________________
# check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)


summary(RSF[[i]], units = FALSE) 
# inspect one summary output of movement model (using concise units)
rsfsum <- summary(RSF[[1]])
rsfsum
# CI units:
# dist_escape (1/dist_escape_25m) 
# elev (1/elev_25m)        
# area (square kilometers)    
# τ[position] (days)           
# τ[velocity] (hours)          
# speed (kilometers/day)       
# diffusion (hectares/day)  

rsfsum <- summary(RSF[[1]], units = FALSE)$CI # using SI units
rsfsum
# CI units:
# dist_escape (1/dist_escape_25m)      
# elev (1/elev_25)                   
# area (square meters)            
# τ[position] (seconds)       
# τ[velocity] (seconds)            
# speed (meters/second)        
# diffusion (square meters/second)


#........................................
## check units to see if theyre all the same

i <- 1


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
# 3 dist_escape_25m (1/dist_escape_25m)   35
# 4               elev_25m (1/elev_25m)   35
# 5               speed (meters/second)   15
# 6               τ[position] (seconds)   35
# 7               τ[velocity] (seconds)   15



# if they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections



#..................................................................
# rsf results ----
#.............................................................

# extract rsf coefficients from model summary and rsf model info

i = 15
summary(RSF[[i]], units = FALSE)
RSF[[i]]@info$identity


rsf_list <- list()
for(i in 1:length(RSF)){
  #create and transpose (flip the rows/columns) a dataframe
  rsf_elev <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["elev_25m (1/elev_25m)",]))
  # get the cov also
  rsf_elev_cov <- RSF[[i]]$COV['elev_25m','elev_25m']
  rsf_dist_escape <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["dist_escape_25m (1/dist_escape_25m)",])) 
  rsf_dist_escape_cov <- RSF[[i]]$COV['dist_escape_25m','dist_escape_25m']
  
  # elev_scaled <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["elev_25m_scaled (1/elev_25m_scaled)",]))
  # elev_scaled_cov <- RSF[[i]]$COV['elev_25m_scaled','elev_25m_scaled']
  # dist_escape_scaled <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["dist_escape_25m_scaled (1/dist_escape_25m_scaled)",])) 
  # dist_escape_scaled_cov <- RSF[[i]]$COV['dist_escape_25m_scaled','dist_escape_25m_scaled']
  
  #combine all the columns
  # res <- cbind(elev, elev_cov, dist_escape, dist_escape_cov,
  #              elev_scaled, elev_scaled_cov, dist_escape_scaled, dist_escape_scaled_cov)
  res <- cbind(rsf_elev, rsf_elev_cov, rsf_dist_escape, rsf_dist_escape_cov)
             
  
  # rename the columns
  names(res) <- c("rsf_elev_low",
                  "rsf_elev_est",
                  "rsf_elev_high", 
                  "rsf_elev_cov",
                  "rsf_dist_escape_low",
                  "rsf_dist_escape_est",
                  "rsf_dist_escape_high", 
                  "rsf_dist_escape_cov")#,
                  # "rsf_elev_scaled_low",
                  # "rsf_elev_scaled_est",
                  # "rsf_elev_scaled_high", 
                  # "rsf_elev_scaled_cov",
                  # "rsf_dist_escape_scaled_low",
                  # "rsf_dist_escape_scaled_est",
                  # "rsf_dist_escape_scaled_high", 
                  # "rsf_dist_escape_scaled_cov")
  
  res$AIC <- RSF[[i]]$AIC
  res$BIC <- RSF[[i]]$BIC
  res$AICc <- RSF[[i]]$AICc
  res$individual.local.identifier <- RSF[[i]]@info$identity
  #store in a list
  rsf_list[[i]] <- res
}
# Warning messages:
#   1: In chisq.ci(STUFF$D, VAR = STUFF$VAR, level = level) :
#   VAR[Area] = -3.40831047725829 < 0
# 2: In chisq.ci(STUFF$D, VAR = STUFF$VAR, level = level) :
#   VAR[Area] = -3.40831047725829 < 0
# 3: In chisq.ci(STUFF$D, VAR = STUFF$VAR, level = level) :
#   VAR[Area] = -3.40831047725829 < 0
# 4: In chisq.ci(STUFF$D, VAR = STUFF$VAR, level = level) :
#   VAR[Area] = -3.40831047725829 < 0


# convert list into a dataframe
rsf_results <- do.call(rbind, rsf_list)

# data carpentry
# extract collar_id and year from individual.local.identifier
# extract text before first _
rsf_results$collar_id <- sub("_.*", "", rsf_results$individual.local.identifier)
# extract text after first _
rsf_results$year <- sub(".*_", "", rsf_results$individual.local.identifier)



goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
# Add goat_name and collar_id to the dataframe
rsf_results <- merge(rsf_results, goat_info[, c("collar_id", "goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
rsf_results <- dplyr::relocate(rsf_results, c(individual.local.identifier, collar_id, goat_name, goat_id, year), .before = rsf_elev_low)
rsf_results$collar_id <- as.factor(rsf_results$collar_id)

head(rsf_results)

# create a column to indicate fire year or not
rsf_results$fire_year <- ifelse(rsf_results$year == "2023", 1, 0) # 1 = fire year, 0 = not a fire year
rsf_results <- rsf_results2
save(rsf_results, file = "./data/rsf/rsf_results_20250902.rda")
load("./scripts/working/data/rsf/rsf_results_20250902.rda")

