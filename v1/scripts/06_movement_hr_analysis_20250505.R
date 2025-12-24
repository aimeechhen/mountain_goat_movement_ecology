

# movement and hr analysis
library(ctmm)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lme4)


# data import ----
load("./scripts/working/data/movement_model/fits_20250505.rda")
load("./scripts/working/data/home_range/akdes_20250505.rda")
# import extract results
results_df <- read.csv("./scripts/working/data/combined_data_movement_hr_results_20250505.csv")
results_df$fire_year <- ifelse(results_df$year == 2023, 1, 0)


# for the other years
meta(AKDES[grepl("2019", names(AKDES))]) # 2019 population level 
meta(AKDES[grepl("2020", names(AKDES))]) # 2020 population level 
meta(AKDES[grepl("2021", names(AKDES))]) # 2021 population level 
meta(AKDES[grepl("2022", names(AKDES))]) # 2022 population level 
meta(AKDES[grepl("2023", names(AKDES))]) # 2023 population level 
meta(AKDES[grepl("2024", names(AKDES))]) # 2024 population level 


meta(AKDES[grepl("2019", names(AKDES))])[1,1]
meta(AKDES[grepl("2019", names(AKDES))])[1,2]
meta(AKDES[grepl("2019", names(AKDES))])[1,3]

meta(FITS[grepl("2024", names(FITS))], variable = "diffusion")
meta(FITS, variable = "tauposition")



# create a df for meta() results (population level)
meta_df <- data.frame(year = factor(),
                      hr_km2_low = numeric(),
                      hr_km2_est = numeric(),
                      hr_km2_high = numeric(),
                      diff_km2_day_low = numeric(),
                      diff_km2_day_est = numeric(),
                      diff_km2_day_high = numeric())


for (i in 2019:2024) {
  hr <- meta(AKDES[grepl(as.character(i), names(AKDES))])
  diff <- meta(FITS[grepl(as.character(i), names(FITS))], variable = "diffusion")
  
  meta_df <- rbind(meta_df, data.frame(year = i,
                                       hr_km2_low  = hr["mean (km²)", "low"],
                                       hr_km2_est  = hr["mean (km²)", "est"],
                                       hr_km2_high = hr["mean (km²)", "high"],
                                       diff_km2_day_low  = diff["mean (km²/day)", "low"],
                                       diff_km2_day_est  = diff["mean (km²/day)", "est"],
                                       diff_km2_day_high = diff["mean (km²/day)", "high"]))
}

write.csv(meta_df, file = "./scripts/working/data/results_meta_df.csv", row.names = FALSE)


meta_df <- data.frame(results = c("hr_km2_low", "hr_km2_est", "hr_km2_high",
                              "diff_km2_day_low", "diff_km2_day_est", "diff_km2_day_high"))

# Loop over years and add each year as a column
for (i in 2019:2024) {
  hr <- meta(AKDES[grepl(as.character(i), names(AKDES))])
  diff <- meta(FITS[grepl(as.character(i), names(FITS))], variable = "diffusion")
  
  res <- c(
    hr["mean (km²)", "low"],
    hr["mean (km²)", "est"],
    hr["mean (km²)", "high"],
    diff["mean (km²/day)", "low"],
    diff["mean (km²/day)", "est"],
    diff["mean (km²/day)", "high"]
  )
  
  meta_df[[as.character(i)]] <- res
}


write.csv(meta_df, file = "./scripts/working/data/results_meta_df_long.csv", row.names = FALSE)







#...........................................................
# #check for significance, home range ----
#...........................................................

# Is a fire a factor in home-range size?

hr_test <- glmer(hr_est_km2 ~ fire_year + (1|collar_id), family = Gamma(link = "log"), data = results_df)
hr_test2 <- glmer(hr_est_km2 ~ 1 + (1|collar_id), family = Gamma(link = "log"), data = results_df)
hr_res <- anova(hr_test, hr_test2)
round(hr_res$`Pr(>Chisq)`[2], 2) #0




# subset data
no_fire_year <- AKDES[!grepl("2023", names(AKDES))]
fire_year <- AKDES[grepl("2023", names(AKDES))]

#calculate mean home range sizes for no fire years
meta(no_fire_year)
#calculate mean home range sizes for fire period
meta(fire_year)

#test to see significance of year on home range using the meta() function
hr_year_compare <- list(no_fire = no_fire_year,
                        fire = fire_year)
COL_year <- c("grey", "#A50026")
meta(hr_year_compare, col = COL_year, sort = TRUE)

# the fire year/normal year ratio of mean home-range size was estimated as 0.23 (0.01–1.03), which marginally includes 1, implying these differences were not significant.

# save the analysis results
sink(paste0("data/", "hr_analysis.txt"))
cat("\n") #enter blank line
print("calculate mean home range sizes for no fire years")
print(meta(no_fire_year))
cat("\n") #enter blank line
print("calculate mean home range sizes for fire period")
print(meta(fire_year))
cat("\n") #enter blank line
print("test to see significance of year on home range using the meta() function, compare fire vs no fire")
hr_year_compare <- list(no_fire = no_fire_year,
                        fire = fire_year)
COL_year <- c("grey", "#A50026")
print(meta(hr_year_compare, col = COL_year, sort = TRUE))
sink() #terminate output exporting connection/process






#...........................................................
# #check for significance, diffusion
#...........................................................


diff_test <- glmer(diffusion_est_km2_day ~ fire_year + (1|collar_id), family = Gamma(link = "log"), data = results_df)
diff_test2 <- glmer(diffusion_est_km2_day ~ 1 + (1|collar_id), family = Gamma(link = "log"), data = results_df)
diff_res <- anova(diff_test, diff_test2)
round(diff_res$`Pr(>Chisq)`[2], 2) #0


no_fire_year2 <- FITS[!grepl("2023", names(FITS))]
fire_year2 <- FITS[grepl("2023", names(FITS))]

#calculate mean diffusion for no fire years
ctmm::meta(no_fire_year2, variable = "diffusion")
#calculate mean diffusion for fire period
ctmm::meta(fire_year2, variable = "diffusion")

#test to see significance of year on diffusion using the meta() function
diff_year_compare <- list(no_fire = no_fire_year2,
                          fire = fire_year2)
COL_year <- c("grey", "#A50026")
ctmm::meta(diff_year_compare, col = COL_year, sort = TRUE, variable = "diffusion")

# fire year/normal year ratio of mean diffusion rates was estimated as 3.20 (1.80–5.16), which excludes 1, implying a significant difference.

# save the analysis results
sink(paste0("./scripts/working/data/", "diffusion_analysis_v2.txt"))
cat("\n") #enter blank line
print("calculate mean diffusion for no fire years")
print(meta(no_fire_year2, variable = "diffusion"))
cat("\n") #enter blank line
print("calculate mean diffusion for fire period")
print(meta(fire_year2, variable = "diffusion"))
cat("\n") #enter blank line
print("test to see significance of year on diffusion using the meta() function, compare fire vs no fire")
diff_year_compare <- list(no_fire = no_fire_year2,
                          fire = fire_year2)
COL_year <- c("grey", "#A50026")
print(meta(diff_year_compare, col = COL_year, sort = TRUE, variable = "diffusion"))
sink() #terminate output exporting connection/process
















#//////////////////////////////////////////////////////
# PLOT ----
#//////////////////////////////////////////////////////





# import extract results
results_df <-read.csv("./data/combined_data_movement_hr_results_20250505.csv")


# load data ----
# original data
# load("./data/collar_data/collar_data_20241123.rda")
# load("./data/home_range/fire_goat_hr_movement_results_df_20241225.rda")
# load("./data/home_range/fire_goat_covariate_results.rda")
# load("./data/rsf/fire_goat_rsf_results_20241225.rda")

# combined data ----
# results_df <- read.csv(file = "./data/full_data_results_20250225.csv")
# results_df <- RESULTS

# full data
# results_df <- read.csv(file = "./data/full_data_results_20250225.csv")
# goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
# # Import supplementary mountain goat info 
# goat_info <- read.csv("data/goat_info.csv")
# goat_info <- goat_info[goat_info$goat_name %in% goats,]
# # Add goat_name and collar_id to the dataframe
# results_df <- merge(results_df, goat_info[, c("goat_name", "goat_id", "collar_id")], by = "collar_id", all.x = TRUE)
results_df <- relocate(results_df, c(goat_name, goat_id), .after = collar_id)
results_df$collar_id = as.factor(results_df$collar_id)




year_palette <- c("#b3b3b3", "#b3b3b3","#b3b3b3", "#b3b3b3", "#ae2012", "#b3b3b3") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023', '2024'))
#format data
results_df$year <- as.factor(results_df$year)
results_df$goat_name <- as.factor(results_df$goat_name)




#...................................................................
# Plot hr ----
#...................................................................


# a) -> boxplot of hr sizes 
#dat.hr2$period <- factor(dat.hr2$period, levels = c('before', 'during', 'after'))

plot_hr <-
  ggplot(data = results_df,
         # mapping = aes(x = year, y = mean_hr_est_km2, fill = year)) +
         mapping = aes(x = year, y = hr_est_km2, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) + #*************** jitter spreads the data out so the point arent stacked
  labs(y = bquote(bold("Home range area " ~ (km^2))),
       x = "") +
  # ggtitle("a)") +
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


ggsave(plot_hr, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/movement_hr/plot_hr_20250225.png")


#...................................................................
# Plot diffusion ----
#...................................................................


plot_diff <-
  ggplot(data = results_df,
         mapping = aes(x = year, y = diffusion_est_km2_day, fill = year)) +
  geom_boxplot(alpha = 0.5, size = 0.3, outlier.size = 0.3) +
  geom_jitter(alpha = 0.9, size = 1, aes(col = year), width = 0.1) +
  labs(y = bquote(bold("Diffusion rate " ~ (km^2/day))),
       # labs(y = bquote(bold("Movement rate " ~ (km^2/day))),
       x = "") +
  # ggtitle("b)") +
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
       file="figures/movement_hr/plot_diff_20250505.png")




#..........................................................
# Multi-panel ----
#..........................................................

# Vertical
plot_v <- grid.arrange(plot_hr, plot_diff,
                       ncol = 1)
ggsave(plot_v, width = 3.23, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/movement_hr/fire_goat_hr_diff_v.png")


# Horizontal
plot_h <- grid.arrange(plot_hr, plot_diff,
                       ncol = 2)
ggsave(plot_h, width = 6.86*1.5, height = 3.23, units = "in", dpi = 600, bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/tws/hr_diff.png")

