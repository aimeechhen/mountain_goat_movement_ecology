

#////////////////////////////////////////////////////////
# PLOT ----
#////////////////////////////////////////////////////////


library(ggplot2)

win_results <- read.csv("./v1/data/window_analysis_20250317/merged_windows_extracted_combined_data_20250317.rds")


library(lubridate)

goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
dir_path <- "./v1/data/window_analysis_20250317/original_data/covariates_20250118/"
rds_files <- list.files(dir_path, pattern = "covariates.*\\.rds$", 
                       full.names = TRUE, recursive = TRUE)
# read file and add a collar_id column before combining
rds_list <- lapply(rds_files, function(f) {
  df <- readRDS(f)
  # extract collar id from object
  df$collar_id <- gsub(".*covariates_(\\d+)\\.rds$", "\\1", f)
  return(df)
})

#combine into a df
covariates <- do.call(rbind, rds_list)

collar_id <- c("30548", "30561", "30575", "30613", "30642", "30648")
covariates <- covariates[covariates$collar_id %in% collar_id,]
covariates <- subset(covariates, select = -goat_name)
covariates <- covariates[,3:8]
# add goat info
goat_info <- read.csv("./v1/data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
goat_info <- goat_info[goat_info$goat_name %in% goats,]
covariates <- merge(covariates, goat_info[, c("collar_id","goat_name")], by = "collar_id", all.x = TRUE)
covariates <- na.omit(covariates)
covar <- covariates

# get new data covariates
dir_path <- "./v1/data/window_analysis_20250317/new_data/covariates_20250219/"
# Load .shp, .tif etc files within a folder including all the subfolders, remember these are saved as df
rds_files <- list.files(dir_path, pattern = "covariates.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# # combine together as one df
rds_dat <- do.call(rbind, rds_list)
covariates2 <- rds_dat
covariates2 <- covariates2[,c(1, 4:8)]
covariates2 <- merge(covariates2, goat_info[, c("collar_id","goat_name")], by = "collar_id", all.x = TRUE)
covariates2 <- na.omit(covariates2)

# combine together because didnt have time to run as one, had to run in chunks
covariates <- rbind(covar, covariates2)

covariates <- na.omit(covariates)

covariates$date <- format(covariates$window_start, "%Y-%m-%d")
covariates$date <- as.Date(covariates$date, "%Y-%m-%d")
covariates$doy <- yday(covariates$date)
covariates$year <- year(covariates$date)





# BASED OFF LIVING LABS PLOTS


library(ggh4x)
library(gridExtra)

mw_dat <- win_results

# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- mw_dat[mw_dat$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks

strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))


# full data, calendar year
# home range ----
plot_hr1 <-
  ggplot(data = mw_dat) +
  geom_line(aes(x = doy, y = hr_est_km2, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.5, 1))) +
  geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
  
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
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
  
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
ggsave(plot_multi, width = 12, height = 8, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       # file="figures/living_labs/figure4.png")
       file="./figures/figures (old)/rmga/S01.png")



#......................................................
# elevation ----



plot_el <-
  ggplot(data = covariates) +
  geom_line(aes(x = doy, y = mean_elev, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.7, 1))) +
  geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  # facet_wrap2(~ collar_id, scales = "fixed",
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
  
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
        strip.text = element_text(size = 10, color = "white", face = "bold"))

# theme(panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position = "none",
#       axis.title.y = element_text(size=10, face = "bold"),
#       axis.title.x = element_text(size=10, face = "bold"),
#       axis.text.y = element_text(size=9),
#       axis.text.x  = element_text(size=9),
#       strip.text = element_text(size = 10, color = "white", face = "bold"),
#       panel.background = element_rect(fill = "transparent", color = NA),
#       plot.background = element_rect(fill = "transparent", color = NA))


# ggsave(plot_el, width = 6*3, height = 6.23, units = "in", dpi = 600, bg = "transparent",
#        # file="figures/home_range/fire_goat_hr_diff_h.png")
#        file="figures/tws/window_elev.png")

# distance to escape terrain ----
plot_esc <-
  ggplot(data = covariates) +
  geom_line(aes(x = doy, y = mean_dist_escape, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.7, 1))) +
  geom_vline(xintercept = c(203, 299), color = "black", linetype = "dashed") +
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
  
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
        strip.text = element_text(size = 10, color = "white", face = "bold"))


plot_h <- grid.arrange(plot_el, plot_esc,
                       nrow = 2)
# ggsave(plot_h, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
ggsave(plot_h, width = 12, height = 8, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       # file="figures/living_labs/figure6.png")
       file="./figures/figures (old)/rmga/S02.png")





#...........................................
# fire period ----

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
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
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
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
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
# ggsave(plot_multi2, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
ggsave(plot_multi2, width = 12, height = 8, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       # file="figures/living_labs/figure5.png")
       file="./figures/figures (old)/rmga/S03.png")

#...............


plot_el2 <-
  ggplot(data = covariates2) +
  geom_line(aes(x = month_day, y = mean_elev, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
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
  # facet_wrap2(~ collar_id, scales = "fixed", 
  #             ncol = 2, nrow = 3, strip = strip_col) +
  
  facet_wrap2(~ collar_id, scales = "fixed",
              ncol = 3, nrow = 2, 
              strip = strip_col,
              # strip = strip_themed(background_x = elem_list_rect(fill = alpha(goat_palette, 0.3))), 
              labeller = labeller(collar_id = c('30548' = 'Goatzilla',
                                                '30561' = 'Selena Goatmez',
                                                '30575' = 'The Goatmother',
                                                '30613' = 'Goatileo',
                                                '30642' = 'Toats McGoats',
                                                '30648' = 'Vincent Van Goat'))) +
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
# ggsave(plot_h2, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
ggsave(plot_h2, width = 12, height = 8, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       # file="figures/living_labs/figure7.png")
       file="./figures/figures (old)/rmga/S04.png")










