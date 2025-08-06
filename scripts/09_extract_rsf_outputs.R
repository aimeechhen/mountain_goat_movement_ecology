# 
# Extract rsf results 

library(ctmm)


# data import ----
# load("./data/rsf/rsf_20250301.rda") 
load("./data/rsf/rsf_20250505.rda")


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
# dist_escape (1/dist_escape) 
# elev (1/elev)               
# area (square kilometers)    
# τ[position] (days)           
# τ[velocity] (hours)          
# speed (kilometers/day)       
# diffusion (hectares/day)  

rsfsum <- summary(RSF[[1]], units = FALSE)$CI # using SI units
rsfsum
# CI units:
# dist_escape (1/dist_escape)      
# elev (1/elev)                   
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
# 1             area (square meters)   35
# 2 diffusion (square meters/second)   35
# 3      dist_escape (1/dist_escape)   35
# 4                    elev (1/elev)   35
# 5            speed (meters/second)   15
# 6            τ[position] (seconds)   35
# 7            τ[velocity] (seconds)   15



# if they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections



#..................................................................
# rsf ----
#.............................................................

# extract rsf results (i.e., the rsf coefficients)

i = 15
summary(RSF[[i]], units = FALSE)
RSF[[i]]@info$identity


rsf_list <- list()
for(i in 1:length(RSF)){
  #create and transpose (flip the rows/columns) a dataframe
  elev <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["elev (1/elev)",]))
  elev_cov <- RSF[[i]]$COV['elev','elev']
  dist_escape <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["dist_escape (1/dist_escape)",])) 
  dist_escape_cov <- RSF[[i]]$COV['dist_escape','dist_escape']
  
  c(elev, elev_cov, dist_escape, dist_escape_cov)
  
  res <- cbind(elev, elev_cov, dist_escape, dist_escape_cov)
  # rename the columns
  names(res) <- c("rsf_elev_min",
                  "rsf_elev_est",
                  "rsf_elev_max", 
                  "rsf_elev_cov",
                  "rsf_dist_escape_min",
                  "rsf_dist_escape_est",
                  "rsf_dist_escape_max", 
                  "rsf_dist_escape_cov")
  
  res$individual.local.identifier <- RSF[[i]]@info$identity
  
  rsf_list[[i]] <- res
}

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
rsf_results <- dplyr::relocate(rsf_results, c(individual.local.identifier, collar_id, goat_name, goat_id, year), .before = rsf_elev_min)
rsf_results$collar_id = as.factor(rsf_results$collar_id)



save(rsf_results, file = "./data/rsf/rsf_results_20250505.rda")
load("./data/rsf/rsf_results_20250505.rda")





