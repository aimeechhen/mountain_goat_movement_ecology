


library(ggplot2)
library(gridExtra)


# import results
load(file = "./data/results/movement_results.rda")


year_palette <- c("#b3b3b3", "#b3b3b3","#b3b3b3", "#b3b3b3", "#ae2012", "#b3b3b3") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023', '2024'))


#...................................................................
# Plot hr ----
#...................................................................


# a) boxplot of hr sizes 

plot_hr <-
  ggplot(data = results_df,
         # mapping = aes(x = year, y = mean_hr_est_km2, fill = year)) +
         mapping = aes(x = year, y = hr_est_km2, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) + #*************** jitter spreads the data out so the point arent stacked
  labs(y = bquote(bold("Home range area" ~ (km^2))),
       x = "") +
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
                      breaks = data_years) +
  scale_y_continuous(breaks = seq(from = 0, to = 300, by = 50))
plot_hr

ggsave(plot_hr, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/movement/plot_hr.png")


#...................................................................
# Plot diffusion ----
#...................................................................


plot_diff <-
  ggplot(data = results_df,
         mapping = aes(x = year, y = diffusion_est_km2_day, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Diffusion rate" ~ (km^2/day))),
       # labs(y = bquote(bold("Movement rate " ~ (km^2/day))),
       x = "") +
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
       file="figures/movement/plot_diffusion.png")




#..........................................................
# Multi-panel ----
#..........................................................

# # Vertical
# plot_v <- grid.arrange(plot_hr, plot_diff,
#                        ncol = 1)
# ggsave(plot_v, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
#        file="figures/movement_hr/fire_goat_hr_diff_v.png")


# Horizontal
plot_h <- grid.arrange(plot_hr, plot_diff,
                       ncol = 2)
ggsave(plot_h, width = 6.86*1.5, height = 3.23, units = "in", dpi = 600, bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       # file="figures/tws/hr_diff.png")
       file="figures/movement/hr_diffusion.png")


