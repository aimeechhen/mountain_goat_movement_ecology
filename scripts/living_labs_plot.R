# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- mw_dat[mw_dat$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks


# full data, calendar year
# home range ----
plot_hr1 <-
ggplot(data = mw_dat) +
  geom_line(aes(x = doy, y = hr_est_km2, group = year, 
                  colour = ifelse(year == 2023, "2023", "other"),
                  alpha = ifelse(year != 2023, 0.5, 1))) +
    geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
    scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
  ggtitle("a)") +
    scale_x_continuous(name = '',
                       limits = c(-5, 340),
                       expand = c(0, 0), # Full year
                       breaks = month_breaks, # Approximate month starts
                       # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
                       labels = month.abb) + # Month abbreviations
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))



# diffusion ----
plot_d1 <- 
ggplot(data = mw_dat[!is.na(mw_dat$diffusion_est_km2_day),]) +
  geom_line(aes(x = doy, y = diffusion_est_km2_day, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.5, 1))) +
  geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(x = '',
       y = expression('Diffusion rate (km'^2*'/day)')) +
  ggtitle("b)") +
  scale_x_continuous(name = '',
                     limits = c(-5, 340),
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


plot_multi <- grid.arrange(plot_hr1, plot_d1,
                       nrow = 2)
ggsave(plot_multi, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/living_labs/figure4.png")



#......................................................
# elevation
plot_el <-
ggplot(data = covariates) +
  geom_line(aes(x = doy, y = mean_elev, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.7, 1))) +
  geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(y = 'Mean elevation (m)',
       x = '') + 
  ggtitle("a)") +
  scale_x_continuous(name = '',
                     limits = c(-5, 340),
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


# distance to escape terrain
plot_esc <-
ggplot(data = covariates) +
  geom_line(aes(x = doy, y = mean_dist_escape, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.7, 1))) +
  geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(y = 'Distance to escape terrain (m)',
       x = '') + 
  ggtitle("b)") +
  scale_x_continuous(name = '',
                     limits = c(-5, 340),
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


plot_h <- grid.arrange(plot_el, plot_esc,
                       nrow = 2)
ggsave(plot_h, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/living_labs/figure6.png")





#...........................................
# fire period

# fire period for all years
fire_start <- '07-22' # doy = 203
fire_end <- '10-26' # doy = 299
# subset data to only during the fire period across all years
mw_dat2 <- mw_dat[mw_dat$month_day >= fire_start & mw_dat$month_day <= fire_end, ]
# then reformat back
mw_dat2$month_day <- as.Date(mw_dat2$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting

# format 
covariates$month_day <- format(covariates$window_start, "%m-%d")
covariates2 <- covariates[covariates$month_day >= fire_start & covariates$month_day <= fire_end, ]
# then reformat back
covariates2$month_day <- as.Date(covariates2$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting


# hr

plot_hr2 <-
ggplot(data = mw_dat2) +
  geom_line(aes(x = month_day, y = hr_est_km2, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
  ggtitle("a)") +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  scale_x_date(name = "",
    date_labels = "%b %d", date_breaks = "3 day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


plot_d2 <-
ggplot(data = mw_dat2[!is.na(mw_dat2$diffusion_est_km2_day),]) +
  geom_line(aes(x = month_day, y = diffusion_est_km2_day, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  ggtitle("b)") +
  labs(x = '',
       y = expression('Diffusion rate (km'^2*'/day)')) +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


plot_multi2 <- grid.arrange(plot_hr2, plot_d2,
                           nrow = 2)
ggsave(plot_multi2, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/living_labs/figure5.png")

#...............


plot_el2 <-
ggplot(data = covariates2) +
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


plot_esc2 <-
  ggplot(data = covariates2) +
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
plot_h2 <- grid.arrange(plot_el2, plot_esc2,
                       nrow = 2)
ggsave(plot_h2, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/living_labs/figure7.png")


