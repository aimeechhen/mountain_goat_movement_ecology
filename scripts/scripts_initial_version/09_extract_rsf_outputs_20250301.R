# 
# Extract rsf results 

# data import ----
load("./data/rsf/rsf_20250301.rda")



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



#                              Var1 Freq
# 1             area (square meters)    1
# 2 diffusion (square meters/second)    1
# 3      dist_escape (1/dist_escape)    1
# 4                    elev (1/elev)    1
# 5            speed (meters/second)    1
# 6            τ[position] (seconds)    1
# 7            τ[velocity] (seconds)    1

# Var1 Freq
# 1             area (square meters)   35
# 2 diffusion (square meters/second)   35
# 3            speed (meters/second)   12
# 4            τ[position] (seconds)   35
# 5            τ[velocity] (seconds)   12



# if they are not the same units, therefore need to convert to make the units uniform across all individuals for sections that have mismatched units
# i.e. diffusion, tau_p and tau_v sections



#//////////////////////////////////////////////////////////////////
# prep results dataframe ----
#/////////////////////////////////////////////////////////////////

RESULTS <- data.frame(collar_id = character(length(FITS)), 
                      year = character(length(FITS)))


#..................................................................
# rsf ----
#.............................................................

# extract rsf results (i.e., the rsf coefficients)

rsf_list <- list()

i = 15
summary(RSF[[i]], units = FALSE)
RSF[[i]]@info$identity

for(i in 1:length(rsf)){
  
  #create and transpose (flip the rows/columns) a dataframe
  elev <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["elev (1/elev)",]))
  elev_cov < RSF[[i]]$COV['elev','elev']
  dist_escape <- data.frame(t(summary(RSF[[i]], units = FALSE)$CI["dist_escape (1/dist_escape)",])) 
  dist_escape_cov = RSF[[i]]$COV['dist_escape','dist_escape']
  
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

rsf_results <- relocate(rsf_results, individual.local.identifier, .before = rsf_elev_min)

save(rsf_results, file = "./data/rsf/fire_goat_rsf_results_20241225.rda")
load("./data/rsf/fire_goat_rsf_results_20241225.rda")


# Values near 0 = no preference
# Values below 0 = selected for, to be closer to escape terrain
# values above 0 = selected against







#//////////////////////////////////////////////////////
# RESULTS ----
#//////////////////////////////////////////////////////




library(ctmm)
library(ggplot2)
library(gridExtra)
library(dplyr)

# load data ----
# original data
load("./data/collar_data/collar_data_20241123.rda")
load("./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")
load("./data/home_range/fire_goat_covariate_results.rda")
load("./data/rsf/fire_goat_rsf_results_20241225.rda")

load("./data/rsf/rsf_20250301.rda")

# combined data ----
results_df <- read.csv(file = "./data/full_data_results_20250225.csv")
# results_df <- RESULTS

# full data
# results_df <- read.csv(file = "./data/full_data_results_20250225.csv")
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
# Add goat_name and collar_id to the dataframe
results_df <- merge(results_df, goat_info[, c("goat_name", "goat_id", "collar_id")], by = "collar_id", all.x = TRUE)
results_df <- relocate(results_df, c(goat_name, goat_id), .after = collar_id)
results_df$collar_id = as.factor(results_df$collar_id)




# combine hr results, mean hr covariate results, rsf results into a single dataframe

# # remove duplicate columns before adding
# covariate_results <- subset(covariate_results, select = -individual.local.identifier)
# rsf_results <- subset(rsf_results, select = -individual.local.identifier)
# # combine
# results_df <- cbind(hr_results, covariate_results)
# results_df <- cbind(results_df, rsf_results)
# 
# # clean environment
# rm(hr_results, covariate_results, rsf_results)
# 
# # helicoper report palette
# scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
#                   breaks = c('2019', '2020', '2021'))



year_palette <- c("#b3b3b3", "#b3b3b3","#b3b3b3", "#b3b3b3", "#ae2012", "#b3b3b3") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023', '2024'))
#format data
results_df$year <- as.factor(results_df$year)
results_df$goat_name <- as.factor(results_df$goat_name)






#...............................................................................
## yearly rsf values i.e. mean value per year ----
# set years
results_df$year <- as.factor(results_df$year)

data_years <- as.factor(seq(from = 2019, to = 2023, by = 1))

rsf_yearly <- data.frame()

# calculate mean values for each year
for (i in 1:length(data_years)) {
  rsf_mean <- colMeans(results_df[results_df$year == data_years[i], 34:41], na.rm = TRUE)
  rsf_mean <- data.frame(year = data_years[i], t(rsf_mean)) # transpose the df as the columns/rows are flipped
  rsf_yearly <- rbind(rsf_yearly, rsf_mean)
}

# write.csv(rsf_yearly,  "./data/rsf/fire_goat_rsf_yearly_covariates.csv", row.names = FALSE)
rsf_yearly <- read.csv( "./data/rsf/fire_goat_rsf_yearly_covariates.csv")

rsf_yearly





# Values near 0 = no preference
# Values below 0 = selected for, to be closer to escape terrain
# values above 0 = selected against


#...........................................................
# PLOT ----
#...........................................................

# RSF elevation

# Plot elevation for spring across all years

pd = position_dodge(width = 0.5)

plot_rsf_elevation <-
  ggplot(data = results_df) + 
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = goat_name, y = rsf_elev_est, ymin = rsf_elev_min, ymax = rsf_elev_max, 
                      color = year), 
                  size = 0.5, position = pd) +
  ylab("Elevation") +
  ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position = c(0.5, 1.05), #horizontal, vertical
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_blank(), 
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)


ggsave(plot_rsf_elevation,
       file="figures/individual_plot/fire_goat_rsf_elevation.png",
       width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#..........................................................
# Plot RSF distance to escape ----
#..........................................................


# Plot distance to escape across all years

plot_rsf_dist_esc <-
  ggplot(data = results_df) + 
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = goat_name, y = rsf_dist_escape_est, ymin = rsf_dist_escape_min, ymax = rsf_dist_escape_max, color = year), 
                  size = 0.5, position = pd) +
  ylab("Distance to Escape Terrain") +
  ggtitle("b)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.title = element_blank(), 
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)


ggsave(plot_rsf_dist_esc,
       file="figures/individual_plot/fire_goat_rsf_dist_esc.png",
       width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#............................................
# Multi-panel RSF ----
#............................................

plot_rsf <- grid.arrange(plot_rsf_elevation, plot_rsf_dist_esc,
                         ncol = 1)

ggsave(plot_rsf,
       file="figures/rsf/fire_goat_rsf.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")

