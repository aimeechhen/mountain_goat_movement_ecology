





win_results <- read.csv("./data/window_analysis/merged_windows_extracted_covariates_combined_data_20250317.rds")



#////////////////////////////////////////////////////////
# PLOT ----
#////////////////////////////////////////////////////////


library(ggplot2)


# win_results <- read.csv("./data/window_analysis/combined_data_window_partial_results_20250313.csv")
win_results <- read.csv("./data/window_analysis/merged_windows_extracted_combined_data_20250317.rds")

# formatting
win_results$window_start = as.POSIXct(win_results$window_start, format = "%Y-%m-%d %H:%M:%S")
win_results$date = as.Date(win_results$date, "%Y-%m-%d")
win_results$year <- as.factor(win_results$year)
win_results$goat_name <- as.factor(win_results$goat_name)
win_results$collar_id <- as.factor(win_results$collar_id)

mw_dat <- win_results


#__________________________________________________________________
# 1) Home range estimate ----



## Full scale ----
# based on Ryan's code in the window.hr function but using ggplot

# single goat
# ggplot(data = mw_dat[mw_dat$ID == "30548",]) +
#   geom_point(aes(x = date, y = hr_est_km), shape = 19, size = 1.25) +
#   geom_errorbar(aes(x = date, ymin = hr_min_km, ymax = hr_max_km), width = 0.1) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   ggtitle("30548") +
#   labs(x = 'Date') + 
#   scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")"),
#                      limits = c(0, max(mw_dat$hr_max_km, na.rm = TRUE))) +
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))






goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
mw_dat <- mw_dat[order(mw_dat$collar_id, mw_dat$window_start), ]
mw_dat$goat_color <- factor(mw_dat$collar_id, levels = goats) 
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# create a goat colour column to assign their colour
mw_dat$goat_color <- goat_palette[match(mw_dat$goat_name, goats)]

# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- mw_dat[mw_dat$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks

library(ggh4x) # to fill in facet wrap title boxes
strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))


# home range
# all goats
# ggplot(data = mw_dat) +
#   geom_point(aes(x = date, y = hr_est_km2), shape = 19, size = 1.25) +
#   geom_errorbar(aes(x = date, ymin = hr_min_km2, ymax = hr_max_km2), width = 0.1) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-10-26')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap2(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
#               ncol = 2, nrow = 3, strip = strip_col,
#               labeller = labeller(collar_id = c('30548' = 'Goatzilla',
#                                                 '30561' = 'Selena Goatmez',
#                                                 '30575' = 'The Goatmother',
#                                                 '30613' = 'Goatileo',
#                                                 '30642' = 'Toats McGoats',
#                                                 '30648' = 'Vincent Van Goat'))) +  
#   scale_size_identity() +  # Keep sizes as defined
#   scale_alpha_identity() +
#   labs(x = "", y = expression("95% Home Range Area Estimate (km"^2*")")) + 
#   # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")"),
#   #                    limits = c(0, max(mw_dat$hr_max_km, na.rm = TRUE))) +
#   scale_y_log10() +
#   # scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         strip.text = element_text(color = "white", face = "bold"))

# ggsave(last_plot(), file = "figures/moving_window_hr_time.png",
#        height = 6.86, width = 14,
#        units = "in", dpi = 600,
#        bg = "transparent")



#....................................................................
## Calendar scale home range ----

# Single goat

# ggplot(data = mw_dat[mw_dat$ID == "30548",]) +
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

# 
# 
# # All goats
# ggplot(data = mw_dat) +
#   # geom_point(aes(x = doy, y = hr_est_km2, color = as.factor(year), group = as.factor(year))) +
#   geom_smooth(aes(x = doy, y = hr_est_km2, color = as.factor(year), group = as.factor(year), linetype = 'solid'),  alpha = 0.5) +
#   geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dashed") +
#   labs(y = expression("95% Home Range Area Estimate (km"^2*")")) +
#   facet_wrap(~ goat_name,
#              ncol = 1, nrow = 6, # sorted by ID
#              scales = "fixed", ) +  #set axis so theyre the same for every plot
#   # ggtitle("Home Range Estimates for ID: {facet_var}") +  # Dynamic title using facet variable
#   # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
#   scale_y_log10() +
#   scale_x_continuous(name = 'Month',
#                      limits = c(-5, 370),
#                      expand = c(0, 0), # Full year
#                      breaks = month_breaks, # Approximate month starts
#                      # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
#                      labels = month.abb) + # Month abbreviations
#   scale_color_manual(#name = "Year",
#     name = "",
#     values = c("2019" = "#332288",
#                "2020" = "#ddaa33",
#                "2021" = "#006d2c",
#                "2022" = "#33bbee",
#                "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         legend.position = "top",
#         legend.direction = "horizontal",
#         legend.justification = "center")
# ggsave(last_plot(),  file="figures/moving_window/fire_goats_moving_window_1_hr_overtime_calendar_scale.png",
#        width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")
# 


#_____________________________________________________________________
# 2) Movement ----
# 
# ## Speed ----
# ggplot(data = mw_dat) +
#   geom_point(aes(x = date, y = speed_est)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   labs(x = 'Date',
#        y = 'Mean Speed (m/s)') + 
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# #inf results (OU)
# ggsave(last_plot(),  file="figures/moving_window_speed_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
# 
# 
# 
# ## Diffusion ----


ggplot(data = mw_dat) +
  geom_line(aes(x = date, y = diffusion_est_km2_day)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-10-26')), color = "#bb5566", linetype = "dotdash") +
  # facet_wrap2(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot
  #             ncol = 2, nrow = 3, strip = strip_col,
  #             labeller = labeller(collar_id = c('30548' = 'Goatzilla',
  #                                               '30561' = 'Selena Goatmez',
  #                                               '30575' = 'The Goatmother',
  #                                               '30613' = 'Goatileo',
  #                                               '30642' = 'Toats McGoats',
  #                                               '30648' = 'Vincent Van Goat'))) +
  scale_color_manual(values = setNames(goat_palette, goats)) +
  labs(x = '',
       y = expression('Diffusion (km'^2*'/day)')) +
  # scale_x_date(date_labels = "%b-%Y", date_breaks = "1 week") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))

# ggsave(last_plot(),  file="figures/moving_window_diffusion_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")

ggsave(last_plot(),  file="figures/window_analysis/window_diffusion_fire_period.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


# might have to log scale y axis


# # colour them by sex
# goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# # goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# sex_palette <- c("#4477AA", "#EE6677", "#EE6677", "#EE6677", "#EE6677", "#4477AA") # colours correspond with the goat order above
# 
# ggplot(data = mw_dat) +
#   geom_line(aes(x = date, y = diffusion_est_km2_day, colour = goat_name)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-10-26')), color = "black", linetype = "dotdash") +
#   labs(x = '',
#        y = expression('Diffusion (km'^2*'/day)')) +
#   scale_color_manual(values = setNames(sex_palette, goats)) +
#   scale_x_date(date_labels = "%b %d", date_breaks = "2 day") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# # check where the data is missing or outside of the scale range 
# mw_dat[is.na(mw_dat$date) | is.na(mw_dat$diffusion_est_km2_day), ] # all goatzilla, IID movement models
# 
# ggsave(last_plot(),  file="figures/window_analysis/window_diffusion_fire_period_sex.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
# 






#_____________________________________________________________________
# 3) Covariates ----

goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
covariates <- covariates[order(covariates$collar_id, covariates$window_start), ]
covariates$goat_color <- factor(covariates$collar_id, levels = goats) 
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# create a goat colour column to assign their colour
covariates$goat_color <- goat_palette[match(covariates$goat_name, goats)]


# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )  # colours correspond with the goat order above
# set the strip colours in the facet wrap
strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))

## Elevation ----


# full scale
ggplot(data = covariates) +
  geom_line(aes(x = date, y = mean_elev)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ collar_id, ncol = 2, nrow = 5, # sorted by ID
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



# # calendar scale
ggplot(data = mw_dat) +
  geom_line(aes(x = doy, y = mean_el, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dashed") +
  facet_wrap(~ goat_name, ncol = 1,
             nrow = 6, # sorted by ID
             scales = "fixed") +  #set axis so theyre the same for every plot
  labs(y = 'Mean elevation (m)',
       x = 'Month') +
  scale_x_continuous(limits = c(-5, 370),
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
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))

# ggsave(last_plot(),  file="figures/moving_window/fire_goats_moving_window_2_elevation_overtime_calendar_scale.png",
#        width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")
# 







# fire period for all years, using collar_id labels
# plot_el <-
# ggplot(data = covariates) +
ggplot(data = mw_dat) +
  geom_line(aes(x = month_day, y = mean_elev, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(y = 'Mean elevation (m)',
       x = '') + 
  ggtitle("a)") +
  scale_x_date(date_breaks = "3 day", date_labels = "%b %d") +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


plot_esc <-
  ggplot(data = covariates) +
  geom_line(aes(x = month_day, y = mean_dist_escape, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(y = 'Distance to escape terrain (m)',
       x = '') + 
  ggtitle("b)") +
  scale_x_date(date_breaks = "3 day", date_labels = "%b %d") +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))




# Horizontal
plot_h <- grid.arrange(plot_el, plot_esc,
                       nrow = 2)
ggsave(plot_h, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/living_labs/figure5.png",)









#..............................
## Distance to escape terrain ----
# ggplot(data = mw_dat) +
#   geom_line(aes(x = date, y = mean_dist_escape)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   labs(x = 'Date',
#        y = 'Mean distance to escape terrain (m)') + 
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# ggsave(last_plot(),  file="figures/moving_window_dist_escape_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")

# calendar scale
ggplot(data = mw_dat) +
  geom_line(aes(x = doy, y = mean_dist_escape, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ goat_name, ncol = 1, 
             # nrow = 6, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Month',
       y = 'Mean distance to escape terrain (m)') + 
  scale_x_continuous(limits = c(-5, 370), 
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
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))


ggsave(last_plot(),  file="figures/moving_window/fire_goats_moving_window_3_dist_escape_overtime_calendar_scale.png",
       width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")

#................................................
## Slope ----
# ggplot(data = mw_dat) +
#   geom_line(aes(x = date, y = mean_slope)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   labs(x = 'Date',
#        y = 'Mean slope (m)') + 
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# ggsave(last_plot(),  file="figures/moving_window_slope_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
# 
# 
# #calendar scale
# ggplot(data = mw_dat) +
#   geom_line(aes(x = doy, y = mean_slope, color = as.factor(year), group = as.factor(year))) +
#   geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 6, # sorted by ID
#              scales = "fixed", ) +  #set axis so theyre the same for every plot
#   labs(x = 'Month',
#        y = 'Slope') + 
#   scale_x_continuous(limits = c(-5, 340), 
#                      expand = c(0, 0), # Full year
#                      breaks = month_breaks, # Approximate month starts
#                      labels = month.abb) + # Month abbreviations
#   scale_color_manual(name = "", 
#                      values = c("2019" = "#332288",
#                                 "2020" = "#ddaa33",
#                                 "2021" = "#006d2c",
#                                 "2022" = "#33bbee",
#                                 "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         legend.position = "top",
#         legend.direction = "horizontal",
#         legend.justification = "center")
# 
# 
# ggsave(last_plot(),  file="figures/moving_window_slope_overtime_calendar_scale.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


#_____________________________________________________________________
# 4) Centroid ----

# 
# 
# ggplot(data = mw_dat) +
#   geom_point(aes(x = centroid.long, y = centroid.lat, color = as.factor(year))) +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   ggtitle('Centroid') +
#   labs(x = 'Longitude',
#        y = 'Latitude') + 
#   scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                                "2020" = "#ddaa33",
#                                                "2021" = "#006d2c",
#                                                "2022" = "#33bbee",
#                                                "2023" = 'black')) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))
# 
# ggsave(last_plot(),  file="figures/moving_window_centroid_location.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")






#......................................................................
# single goat all years ----

# mw_dat2 <- mw_dat[mw_dat$goat_name == "vincent_van_goat",]

# Get unique goat names
goat_names <- unique(mw_dat$goat_name)

# Loop through each unique goat_name
for (goat in goat_names) {
  mw_dat2 <- mw_dat[mw_dat$goat_name == goat, ]
  
  # plot
  p <-
    ggplot(data = mw_dat2) +
    geom_point(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year))) +
    # geom_smooth(aes(x = doy, y = hr_est, color = as.factor(year), group = as.factor(year), linetype = 'solid'),  alpha = 0.5) +
    geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dashed") +
    labs(y = expression("95% Home Range Area Estimate (km"^2*")")) + 
    facet_wrap(~ year, 
               ncol = 1, nrow = 6, # sorted by ID
               scales = "fixed", ) +  #set axis so theyre the same for every plot
    ggtitle(paste(mw_dat2$goat_name)) +
    # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
    scale_y_log10() +
    scale_x_continuous(name = 'Month',
                       limits = c(-5, 370), 
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
          legend.position = "none")
  
  
  
  ggsave(filename = paste0("figures/moving_window/individual/mw_1_hr_overtime_calendar_", goat, ".png"),
         plot = p,
         width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")
}
