# rsf analysis


library(ggplot2)
library(gridExtra)

# import data ----
rsf_results <- read.csv("./data/combined_data_rsf_results_20250302.csv")
rsf_yearly_mean <- read.csv( "./data/rsf/combined_data_rsf_yearly_mean_results.csv")

rsf_results$collar_id <- as.factor(rsf_results$collar_id)
rsf_results$goat_name <- as.factor(rsf_results$goat_name) 
rsf_results$year <- as.factor(rsf_results$year) 

rsf_yearly_mean$year <- as.factor(rsf_yearly_mean$year) 





#//////////////////////////////////////////////////////
# PLOT ----
#//////////////////////////////////////////////////////


year_palette <- c("#b3b3b3", "#b3b3b3","#b3b3b3", "#b3b3b3", "#ae2012", "#b3b3b3") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023', '2024'))
# data_years <- as.factor(seq(from = 2019, to = 2024, by = 1))

# 
# #...............................................................................
# # RSF elevation ----
# # Plot elevation across all years
# 
# pd = position_dodge(width = 0.5)
# 
# # plot_rsf_elevation <-
# ggplot(data = rsf_results) + 
#   geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
#   geom_pointrange(aes(x = goat_name, y = rsf_elev_est, ymin = rsf_elev_min, ymax = rsf_elev_max, 
#                       colour = year_palette), 
#                   size = 0.5, position = pd) +
#   ylab("Elevation") +
#   ggtitle("a)") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
#         axis.title.y = element_text(size=10, family = "sans", face = "bold"),
#         axis.title.x = element_blank(), 
#         axis.text.y = element_text(size=8, family = "sans"),
#         axis.text.x  = element_text(size=8, family = "sans"),
#         legend.position = c(0.5, 1.05), #horizontal, vertical
#         legend.justification = "center",
#         legend.direction = "horizontal",
#         legend.title = element_blank(), 
#         legend.background = element_blank(),
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA)) +
#   scale_colour_manual(values = year_palette, 
#                       breaks = data_years)
# 
# 
# ggsave(plot_rsf_elevation,
#        file="figures/individual_plot/fire_goat_rsf_elevation.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")
# 
# 
# 
# #..........................................................
# # Plot RSF distance to escape ----
# #..........................................................
# 
# 
# # Plot distance to escape across all years
# 
# plot_rsf_dist_esc <-
#   ggplot(data = results_df) + 
#   geom_hline(yintercept = 0, col = "grey70", linetype = "dashed") +
#   geom_pointrange(aes(x = goat_name, y = rsf_dist_escape_est, ymin = rsf_dist_escape_min, ymax = rsf_dist_escape_max, color = year), 
#                   size = 0.5, position = pd) +
#   ylab("Distance to Escape Terrain") +
#   ggtitle("b)") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
#         axis.title.y = element_text(size=10, family = "sans", face = "bold"),
#         axis.title.x = element_blank(), 
#         axis.text.y = element_text(size=8, family = "sans"),
#         axis.text.x  = element_text(size=8, family = "sans"),
#         legend.position="none",
#         legend.title = element_blank(), 
#         panel.background = element_rect(fill = "transparent"),
#         plot.background = element_rect(fill = "transparent", color = NA)) +
#   scale_colour_manual(values = year_palette, 
#                       breaks = data_years)
# 
# 
# ggsave(plot_rsf_dist_esc,
#        file="figures/individual_plot/fire_goat_rsf_dist_esc.png",
#        width = 6.86, height = 3, units = "in", dpi = 600, bg = "transparent")
# 
# 
# 
# #............................................
# # Multi-panel RSF ----
# #............................................
# 
# plot_rsf <- grid.arrange(plot_rsf_elevation, plot_rsf_dist_esc,
#                          ncol = 1)
# 
# ggsave(plot_rsf,
#        file="figures/rsf/fire_goat_rsf.png",
#        width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")
# 
# 




#..........................................................
# Plot annual RSF elev mean ----
#..........................................................


plot_rsf_annual_elev <- 
ggplot(data = rsf_yearly_mean) +
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
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)



#..........................................................
# Plot annual RSF distance to escape mean ----
#..........................................................


plot_rsf_annual_dist_esc <- 
ggplot(data = rsf_yearly_mean) +
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
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_colour_manual(values = year_palette, 
                      breaks = data_years)




#............................................
# Multi-panel RSF ----
#............................................

plot_rsf_annual <- grid.arrange(plot_rsf_annual_elev, plot_rsf_annual_dist_esc,
                         nrow = 1)

ggsave(plot_rsf_annual,
       file="figures/rsf/rsf_annual_mean.png",
       width = 9.86, height = 3.23, units = "in", dpi = 600)
