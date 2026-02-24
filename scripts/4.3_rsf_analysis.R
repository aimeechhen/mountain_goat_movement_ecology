# rsf analysis

library(lme4)
library(ctmm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggpubr)

options(scipen = 999)

# import data ----
load("./results/rsf_results.rda")

# create a column to indicate fire year or not
rsf_results$fire_year <- ifelse(rsf_results$year == "2023", 1, 0) # 1 = fire year, 0 = not a fire year


# Ryan used RSF scores...where did he get these from and what are they?

#...........................................................
# #check for significance ----
#...........................................................



# #.................................................................
# # does habitat use differ between years?
# rsf_model <- lmer(rsf_elev_est ~ year + (1|collar_id), 
#                    # family =  gaussian("identity"), 
#                    data = rsf_results)
# rsf_null <- lmer(rsf_elev_est ~ 1 + (1|collar_id),  
#                   # family =  gaussian("identity"), 
#                   data = rsf_results)
# rsf_model <- lmer(rsf_elev_scaled_est ~ year + (1|collar_id), 
#                   # family =  gaussian("identity"), 
#                   data = rsf_results)
# rsf_null <- lmer(rsf_elev_scaled_est ~ 1 + (1|collar_id),  
#                  # family =  gaussian("identity"), 
#                  data = rsf_results)
# 
# 
# rsf_test_results <- anova(rsf_model, rsf_null)
# paste("p = ", round(rsf_test_results$`Pr(>Chisq)`[2],2), sep = "") #p-value



#.................................................................
# does resource selection differ between fire vs no fire year?

# check range of data for distribution
range(rsf_results$rsf_elev_est) 

rsf_model <- lmer(rsf_elev_est ~ fire_year + (1|goat_id), data = rsf_results, na.action = "na.fail")
rsf_null <- lmer(rsf_elev_est ~ 1 + (1|goat_id), data = rsf_results, na.action = "na.fail")
rsf_test <- anova(rsf_model, rsf_null) 
rsf_test
paste("p = ", round(rsf_test$`Pr(>Chisq)`[2],2), sep = "") #p-value 0.87

#0.53
#0.82
#0.7
summary(rsf_model)
MuMIn::dredge(rsf_model)



# distance to escape terrain

# check range of data for distribution
range(rsf_results$rsf_esc_dist_est) 
#check for significance
rsf_model2 <- lmer(rsf_esc_dist_est ~ fire_year + (1|goat_id), data = rsf_results, na.action = "na.fail")
rsf_null2 <- lmer(rsf_esc_dist_est ~ 1 + (1|goat_id), data = rsf_results, na.action = "na.fail")
rsf_test2 <- anova(rsf_model2, rsf_null2)
rsf_test2
paste("p = ", round(rsf_test2$`Pr(>Chisq)`[2],2), sep = "") #p-value 0.58
#0.53
summary(rsf_model2)
MuMIn::dredge(rsf_model2)






