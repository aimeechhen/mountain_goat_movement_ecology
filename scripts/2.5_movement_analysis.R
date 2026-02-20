

# movement and hr analysis
library(ctmm)
library(dplyr)
library(lme4)
options(scipen = 999)


# data import ----
load("./data/movement/fits.rda")
load("./data/movement/akdes.rda")
# import extract results
load(file = "./data/results/movement_results.rda")
load(file = "./data/results/movement_meta_results.rda")
load(file = "./data/results/movement_meta2_results.rda")

# indicate if it was the fire year or not
results_df$fire_year <- ifelse(results_df$year == 2023, 1, 0)



#...........................................................
# #check for significance, home range ----
#...........................................................

# Is a fire a factor in home-range size?

hr_test <- glmer(hr_est_km2 ~ fire_year + (1|goat_id), family = Gamma(link = "log"), data = results_df)
hr_test2 <- glmer(hr_est_km2 ~ 1 + (1|goat_id), family = Gamma(link = "log"), data = results_df)
hr_res <- anova(hr_test, hr_test2)
hr_res
paste("p = ", round(rsf_test2$`Pr(>Chisq)`[2],2), sep = "") #0.02


# subset data
fire_year <- AKDES[grepl("2023", names(AKDES))]
no_fire_year <- AKDES[!grepl("2023", names(AKDES))]

#calculate mean home range sizes for fire period
meta(fire_year)
#calculate mean home range sizes for no fire years
meta(no_fire_year)


#test to see significance of year on home range using the meta() function
hr_year_compare <- list(fire = fire_year,
                        no_fire = no_fire_year)
COL_year <- c("grey", "#A50026")
# PAY ATTENTION where fire/ and no_fire/ is located in the matrix, it may flip and might be no_fire then fire/ 
meta(hr_year_compare, col = COL_year, sort = TRUE)

# the fire year/normal year ratio of mean home-range size was estimated as 0.23 (0.01–1.03), which marginally includes 1, implying these differences were not significant.
# fire/no_fire = 0.53 (0.04-2.42)
# no_fire/fire = 0.32 (0.01-1.48) 

# save the analysis results
sink(paste0("./results/", "hr_analysis.txt"))
cat("\n") #enter blank line
print("calculate mean home range sizes for fire period")
print(meta(fire_year))
cat("\n") #enter blank line
print("calculate mean home range sizes for no fire years")
print(meta(no_fire_year))
cat("\n") #enter blank line
print("test to see significance of year on home range using the meta() function, compare fire vs no fire")
print("PAY ATTENTION where fire/ and no_fire/ is located in the matrix, it may flip and might be no_fire then fire/")
hr_year_compare <-  list(fire = fire_year,
                         no_fire = no_fire_year)
COL_year <- c("grey", "#A50026")
print(meta(hr_year_compare, col = COL_year, sort = TRUE))
sink() #terminate output exporting connection/process


#...........................................................
# check for significance, diffusion
#...........................................................


diff_test <- glmer(diffusion_est_km2_day ~ fire_year + (1|goat_id), family = Gamma(link = "log"), data = results_df)
diff_test2 <- glmer(diffusion_est_km2_day ~ 1 + (1|goat_id), family = Gamma(link = "log"), data = results_df)
diff_res <- anova(diff_test, diff_test2)
diff_res
paste("p = ", round(rsf_test$`Pr(>Chisq)`[2],2), sep = "") #p-value 0.97

fire_year2 <- FITS[grepl("2023", names(FITS))]
no_fire_year2 <- FITS[!grepl("2023", names(FITS))]


#calculate mean diffusion for fire period
ctmm::meta(fire_year2, variable = "diffusion")
#calculate mean diffusion for no fire years
ctmm::meta(no_fire_year2, variable = "diffusion")


#test to see significance of year on diffusion using the meta() function
diff_year_compare <-  list(fire = fire_year2,
                           no_fire = no_fire_year2)
COL_year <- c("grey", "#A50026")
# PAY ATTENTION where fire/ and no_fire/ is located in the matrix, it may flip and might be no_fire then fire/
ctmm::meta(diff_year_compare, col = COL_year, sort = TRUE, variable = "diffusion")

# fire year/normal year ratio of mean diffusion rates was estimated as 3.20 (1.80–5.16), which excludes 1, implying a significant difference.
# fire/no_fire = 1.03 (0.46-1.89)
# no_fire/fire = 0.85 (0.39-1.76)


# save the analysis results
sink(paste0("./results/diffusion_analysis.txt"))
cat("\n") #enter blank line
print("calculate mean diffusion for fire period")
print(meta(fire_year2, variable = "diffusion"))
cat("\n") #enter blank line
print("calculate mean diffusion for no fire years")
print(meta(no_fire_year2, variable = "diffusion"))
cat("\n") #enter blank line
print("test to see significance of year on diffusion using the meta() function, compare fire vs no fire")
print("PAY ATTENTION where fire/ and no_fire/ is located in the matrix, it may flip and might be no_fire then fire/")
diff_year_compare <-  list(fire = fire_year2,
                           no_fire = no_fire_year2)
COL_year <- c("grey", "#A50026")
print(meta(diff_year_compare, col = COL_year, sort = TRUE, variable = "diffusion"))
sink() #terminate output exporting connection/process













