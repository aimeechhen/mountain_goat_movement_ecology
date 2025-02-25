# fire goats plots

library(ctmm)
library(ggplot2)
library(gridExtra)

# load data ----
load("./data/collar_data/collar_data_20241123.rda")
load("./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")
load("./data/home_range/fire_goat_covariate_results.rda")
load("./data/rsf/fire_goat_rsf_results_20241225.rda")

#______________________________________________________________________________
## Combine all results ----
# combine hr results, mean hr covariate results, rsf results into a single dataframe

# remove duplicate columns before adding
covariate_results <- subset(covariate_results, select = -individual.local.identifier)
rsf_results <- subset(rsf_results, select = -individual.local.identifier)
# combine
results_df <- cbind(hr_results, covariate_results)
results_df <- cbind(results_df, rsf_results)

# clean environment
rm(hr_results, covariate_results, rsf_results)

# helicoper report palette
scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                  breaks = c('2019', '2020', '2021'))



year_palette <- c("#4477AA", "#ccbb44","#228833", "#66CCEE", "#ae2012") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023'))
#format data
results_df$year <- as.factor(results_df$year)
results_df$goat_name <- as.factor(results_df$goat_name)


#...................................................................
# Plot hr ----
#...................................................................


# a) -> boxplot of hr sizes 
#dat.hr2$period <- factor(dat.hr2$period, levels = c('before', 'during', 'after'))

# plot_hr <-
  ggplot(data = results_df,
         mapping = aes(x = year, y = mean_hr_est_km2, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) + #*************** WHAT DOES JITTER DO TO THE DATA WHEN PLOTTING?
  labs(y = bquote(bold("Home range area " ~ (km^2))),
       x = "Year") +
  ggtitle("a)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = year_palette, 
                    breaks = data_years) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)


ggsave(plot_hr, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual_plot/fire_goat_hr.png")


#...................................................................
# Plot diffusion ----
#...................................................................


plot_diff <-
  ggplot(data = results_df,
         mapping = aes(x = year, y = diffusion_est_km2_day, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Diffusion rate " ~ (km^2/day))),
       x = "Year") +
  ggtitle("b)") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_fill_manual(values = year_palette, 
                    breaks = data_years) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)

ggsave(plot_diff, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/individual_plot/fire_goat_diffusion.png")




#..........................................................
# Multi-panel ----
#..........................................................

# Vertical
plot_v <- grid.arrange(plot_hr, plot_diff,
                       ncol = 1)
ggsave(plot_v, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/home_range/fire_goat_hr_diff_v.png")


# Horizontal
plot_h <- grid.arrange(plot_hr, plot_diff,
                       ncol = 2)
ggsave(plot_h, width = 6.86, height = 3.23, units = "in", dpi = 600, bg = "transparent",
       file="figures/home_range/fire_goat_hr_diff_h.png")




#..................................................
# Plot RSF elevation ----
#..................................................

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
# Multi-panel RSF spring ----
#............................................

plot_rsf <- grid.arrange(plot_rsf_elevation, plot_rsf_dist_esc,
                         ncol = 1)

ggsave(plot_rsf,
       file="figures/rsf/fire_goat_rsf.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")

