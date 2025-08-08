# rsf analysis

library(lme4)
library(ctmm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggpubr)

# import data ----
load("./data/rsf/rsf_results_20250505.rda")

rsf_results$collar_id <- as.factor(rsf_results$collar_id)
rsf_results$goat_name <- as.factor(rsf_results$goat_name) 
rsf_results$year <- as.factor(rsf_results$year) 

# create a column to indicate fire year or not
rsf_results$fire_year <- ifelse(rsf_results$year == "2023", 1, 0) # 1 = fire year, 0 = not a fire year


# Ryan used RSF scores...where did he get these from and what are they?


#.................................................................
# does habitat use differ between years?
rsf_model <- glmer(rsf_elev_est ~ year + (1|collar_id),  family =  gaussian("identity"), data = rsf_results)
rsf_null <- glmer(rsf_elev_est ~ 1 + (1|collar_id),  family =  gaussian("identity"), data = rsf_results)

rsf_test_results <- anova(rsf_model, rsf_null)
paste("p = ", round(rsf_test_results$`Pr(>Chisq)`[2],2), sep = "") #p-value


#.................................................................
# does habitat use differ between fire vs no fire year?
range(rsf_results$rsf_elev_est) 
rsf_model <- glmer(rsf_elev_est ~ fire_year + (1|collar_id), 
                   family = gaussian("identity"), # ranges from -1,1
                   data = rsf_results,
                   na.action = "na.fail")
rsf_null <- glmer(rsf_elev_est ~ 1 + (1|collar_id), 
                  family =  gaussian("identity"), # ranges from -1,1
                  data = rsf_results,
                  na.action = "na.fail")

rsf_test <- anova(rsf_model, rsf_null) #refitting model(s) with ML (instead of REML)
paste("p = ", round(rsf_test$`Pr(>Chisq)`[2],2), sep = "") #p-value

summary(rsf_model)
MuMIn::dredge(rsf_model)



# distance to escape terrain
range(rsf_results$rsf_dist_escape_est) 
rsf_model2 <- glmer(rsf_dist_escape_est ~ fire_year + (1|collar_id), 
                   family =  gaussian("identity"), # ranges from -1,1
                   data = rsf_results,
                   na.action = "na.fail")
rsf_null2 <- glmer(rsf_dist_escape_est ~ 1 + (1|collar_id), 
                  family =  gaussian("identity"), # ranges from -1,1
                  data = rsf_results,
                  na.action = "na.fail")

rsf_test2 <- anova(rsf_model2, rsf_null2) # refitting model(s) with ML (instead of REML)
paste("p = ", round(rsf_test2$`Pr(>Chisq)`[2],2), sep = "") #p-value

summary(rsf_model2)
MuMIn::dredge(rsf_model2)








#...............................................................................
## yearly rsf coefficient values i.e. mean value per year ----

names(rsf_results)
# set columns for mean calculations
rsf_columns <- c("rsf_elev_min", "rsf_elev_est", "rsf_elev_max", "rsf_elev_cov","rsf_dist_escape_min", "rsf_dist_escape_est", "rsf_dist_escape_max", "rsf_dist_escape_cov")

# calculate mean values for each year
rsf_yearly <- aggregate(. ~ year, data = rsf_results[, c("year", rsf_columns)], FUN = mean, na.rm = TRUE)


# rsf coefficient values
# Values near 0 = no preference
# Values below 0 = selected for, to be closer to escape terrain
# values above 0 = selected against



#//////////////////////////////////////////////////////
# PLOT ----
#//////////////////////////////////////////////////////


year_palette <- c("#b3b3b3", "#b3b3b3","#b3b3b3", "#b3b3b3", "#ae2012", "#b3b3b3") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023', '2024'))
# data_years <- as.factor(seq(from = 2019, to = 2024, by = 1)) # does the same thing as the line above, but line above allows you to see the years and the colour to match it explicitly
# set the year order based on data_years
# rsf_results$year <- factor(rsf_results$year, levels = data_years)

#...............................................................................
# RSF elevation ----
# Plot elevation across all years, grouped by individual

pd = position_dodge(width = 0.5)

plot_rsf_elevation <-
  ggplot(data = rsf_results) +
  geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
  geom_pointrange(aes(x = goat_name, y = rsf_elev_est, ymin = rsf_elev_min, ymax = rsf_elev_max,
                      colour = year),
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
        legend.position = "top", #horizontal, vertical
        # legend.position.inside = c(0.5, 1.05),
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = year_palette,
                      breaks = data_years) + 
  # guides(colour = guide_legend(ncol = 1)) # set legend format, if you want it on the side
  guides(colour = guide_legend(nrow = 1)) # set legend format, if you want it top/bot

# 
# ggsave(plot_rsf_elevation,
#        file="figures/rsf/rsf_elevation_individual.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#..........................................................
# Plot RSF distance to escape ----
#..........................................................


# Plot distance to escape across all years, grouped by individual

plot_rsf_dist_esc <-
  ggplot(data = rsf_results) +
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

# 
# ggsave(plot_rsf_dist_esc,
#        file="figures/rsf/rsf_dist_esc.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")



#............................................
# Multi-panel RSF ----
#............................................

# plot_rsf_individual <- grid.arrange(plot_rsf_elevation, plot_rsf_dist_esc,
#                          ncol = 1)

plot_rsf_individual <- ggarrange(plot_rsf_elevation, plot_rsf_dist_esc,
                                 ncol = 1,
                                 common.legend = TRUE,
                                 legend = "top")
plot_rsf_individual

ggsave(plot_rsf_individual, 
       file="figures/rsf/plot_rsf_individual.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")









#..........................................................
# Plot annual RSF elev mean ----
#..........................................................

# population level

plot_rsf_annual_elev <-
  ggplot(data = rsf_yearly) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  geom_pointrange(aes(x = year, y = rsf_elev_est, ymin = rsf_elev_min, ymax = rsf_elev_max, 
                      color = year), 
                  size = 0.5, position = pd) +
  ylab("Elevation") +
  # ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)





#..........................................................
# Plot annual RSF distance to escape mean ----
#..........................................................


plot_rsf_annual_dist_esc <- 
  ggplot(data = rsf_yearly) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") +
  geom_pointrange(aes(x = year, y = rsf_dist_escape_est, ymin = rsf_dist_escape_min, ymax = rsf_dist_escape_max, 
                      color = year), 
                  size = 0.5, position = pd) +
  ylab("Distance to Escape Terrain") +
  # ggtitle("b)") +
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




#............................................
# Multi-panel RSF ----
#............................................

plot_rsf_annual <- grid.arrange(plot_rsf_annual_elev, plot_rsf_annual_dist_esc,
                                nrow = 1)

plot_rsf_annual


ggsave(plot_rsf_annual,
       file="figures/rsf/rsf_annual_mean.png",
       width = 9.86, height = 3.23, units = "in", dpi = 600, bg = "transparent")
