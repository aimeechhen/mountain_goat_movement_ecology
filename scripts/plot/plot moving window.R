
# Moving Window Results ----

library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)


# Import moving window results
mw.dat <- read.csv('./data/input_data/moving_window/moving_window_results_20240715.csv')

# mw.dat = mw.dat %>%
#   pivot_longer(!c(ID, timestamp), names_to = "HR_type", values_to = "95_estimate")
# mw.dat$HR_type = as.factor(mw.dat$HR_type)

mw.dat$date <- as.Date(mw.dat$date)

# convert square meters into km^2
mw.dat$hr_min_km <- mw.dat$hr_min/1e6
mw.dat$hr_est_km <- mw.dat$hr_est/1e6
mw.dat$hr_max_km <- mw.dat$hr_max/1e6

# Define the range and the extended range for the x-axis
start_doy <- 203 # July 22 = day 203
end_doy <- 273   # Sept 30 = day 273
extend_days <- 5 # Extend the range by a few days on each end

# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- mw.dat[mw.dat$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks





#__________________________________________________________________
# 1) Home range estimate ----


## Full scale ----
# based on Ryan's code in the window.hr function but using ggplot

# single goat
# ggplot(data = mw.dat[mw.dat$ID == "30548",]) +
#   geom_point(aes(x = date, y = hr_est_km), shape = 19, size = 1.25) +
#   geom_errorbar(aes(x = date, ymin = hr_min_km, ymax = hr_max_km), width = 0.1) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   ggtitle("30548") +
#   labs(x = 'Date') + 
#   scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")"),
#                      limits = c(0, max(mw.dat$hr_max_km, na.rm = TRUE))) +
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# all goats
ggplot(data = mw.dat) +
  geom_point(aes(x = date, y = hr_est_km), shape = 19, size = 1.25) +
  geom_errorbar(aes(x = date, ymin = hr_min_km, ymax = hr_max_km), width = 0.1) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = expression("95% Home Range Area Estimate (km"^2*")")) + 
  # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")"),
  #                    limits = c(0, max(mw.dat$hr_max_km, na.rm = TRUE))) +
  scale_y_log10() +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(last_plot(), file = "figures/moving_window_hr_time.png",
       height = 6.86, width = 14,
       units = "in", dpi = 600,
       bg = "transparent")



#....................................................................
## Calendar scale  ----

# Single goat

# ggplot(data = mw.dat[mw.dat$ID == "30548",]) +
#   # geom_smooth(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year), linetype = 'solid')) +
#   geom_point(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year))) +
#   geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
#   ggtitle("30548") +
#   scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
#   # scale_y_log10(name = expression("95% Home Range Area Estimate (km"^2*")")) +
#   scale_x_continuous(name = 'Month',
#                      limits = c(-5, 340), 
#                      expand = c(0, 0), # Full year
#                      breaks = month_breaks, # Approximate month starts
#                      # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
#                      labels = month.abb) + # Month abbreviations
#   scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                                "2020" = "#ddaa33",
#                                                "2021" = "#006d2c",
#                                                "2022" = "#33bbee",
#                                                "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))



# All goats
ggplot(data = mw.dat) +
  geom_point(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year))) +
  # geom_smooth(aes(x = doy, y = hr_est, color = as.factor(year), group = as.factor(year), linetype = 'solid'),  alpha = 0.5) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  labs(y = expression("95% Home Range Area Estimate (km"^2*")")) + 
  facet_wrap(~ ID, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  # ggtitle("Home Range Estimates for ID: {facet_var}") +  # Dynamic title using facet variable
  # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
  scale_y_log10() +
  scale_x_continuous(name = 'Month',
                     limits = c(-5, 340), 
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(#name = "Year", 
    name = "", 
    values = c("2019" = "#332288",
               "2020" = "#ddaa33",
               "2021" = "#006d2c",
               "2022" = "#33bbee",
               "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center")
ggsave(last_plot(),  file="figures/moving_window_hr_overtime_calendar_scale.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")



#_____________________________________________________________________
# 2) Movement ----

## Speed ----
ggplot(data = mw.dat) +
  geom_point(aes(x = date, y = speed_est)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = 'Mean Speed (m/s)') + 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#inf results (OU)
ggsave(last_plot(),  file="figures/moving_window_speed_overtime.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")



## Diffusion ----
ggplot(data = mw.dat) +
  geom_line(aes(x = date, y = diffusion_est)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = expression('Diffusion (m'^2*'/s)')) + 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(last_plot(),  file="figures/moving_window_diffusion_overtime.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")

# might have to log scale y axis








#_____________________________________________________________________
# 3) Covariates ----

## Elevation ----


# full scale
ggplot(data = mw.dat) +
  geom_line(aes(x = date, y = mean_el)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = 'Mean elevation (m)') + 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(last_plot(),  file="figures/moving_window_elevation_overtime.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")



# calendar scale
ggplot(data = mw.dat) +
  geom_line(aes(x = doy, y = mean_el, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  labs(y = 'Mean elevation (m)',
       x = 'Month') + 
  scale_x_continuous(limits = c(-5, 340), 
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(name = "", 
                     values = c("2019" = "#332288",
                                "2020" = "#ddaa33",
                                "2021" = "#006d2c",
                                "2022" = "#33bbee",
                                "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center")

ggsave(last_plot(),  file="figures/moving_window_elevation_overtime_calendar_scale.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")



## Distance to escape terrain ----
ggplot(data = mw.dat) +
  geom_line(aes(x = date, y = mean_dist_escape)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = 'Mean distance to escape terrain (m)') + 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(last_plot(),  file="figures/moving_window_dist_escape_overtime.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")

# calendar scale
ggplot(data = mw.dat) +
  geom_line(aes(x = doy, y = mean_dist_escape, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  labs(x = 'Month',
       y = 'Mean distance to escape terrain (m)') + 
  scale_x_continuous(limits = c(-5, 340), 
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(name = "", 
                     values = c("2019" = "#332288",
                                "2020" = "#ddaa33",
                                "2021" = "#006d2c",
                                "2022" = "#33bbee",
                                "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center")

ggsave(last_plot(),  file="figures/moving_window_dist_escape_overtime_calendar_scale.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


## Slope ----
ggplot(data = mw.dat) +
  geom_line(aes(x = date, y = mean_slope)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = 'Mean slope (m)') + 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(last_plot(),  file="figures/moving_window_slope_overtime.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


#calendar scale
ggplot(data = mw.dat) +
  geom_line(aes(x = doy, y = mean_slope, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ ID, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  labs(x = 'Month',
       y = 'Slope') + 
  scale_x_continuous(limits = c(-5, 340), 
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(name = "", 
                     values = c("2019" = "#332288",
                                "2020" = "#ddaa33",
                                "2021" = "#006d2c",
                                "2022" = "#33bbee",
                                "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center")


ggsave(last_plot(),  file="figures/moving_window_slope_overtime_calendar_scale.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


#_____________________________________________________________________
# 4) Centroid ----



ggplot(data = mw.dat) +
  geom_point(aes(x = centroid.long, y = centroid.lat, color = as.factor(year))) +
  facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  ggtitle('Centroid') +
  labs(x = 'Longitude',
       y = 'Latitude') + 
  scale_color_manual(name = "Year", values = c("2019" = "#332288",
                                               "2020" = "#ddaa33",
                                               "2021" = "#006d2c",
                                               "2022" = "#33bbee",
                                               "2023" = 'black')) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))

ggsave(last_plot(),  file="figures/moving_window_centroid_location.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
