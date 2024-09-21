library(ggplot2)

# plot collar data to see sampling fixes over time

dat = read.csv('./data/input_data/20240703_moving_window_formatted_for_tele_dat.csv')
dat$timestamp = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S")

#sort data by goat and timestamp
dat <- dat[order(dat$individual.local.identifier, dat$timestamp), ]

dat$fix_intervals <- NA
# calculate the length of time between fixes (units = hours)
for (i in unique(dat$individual.local.identifier)) {
  # Subset data for the current individual
  ind_data <- subset(dat, individual.local.identifier == i)
  # Calculate the time differences in hours
  ind_data$fix_intervals <- c(NA, diff(as.numeric(ind_data$timestamp)) / 3600)
  
  # Update the main dataframe with calculated intervals
  dat$fix_intervals[dat$individual.local.identifier == i] <- ind_data$fix_intervals
}

dat <- dat[complete.cases(dat),]

test <- dat[dat$fix_intervals >= 36,]
test <- dat[dat$fix_intervals >= 50,]
dat <- dat[!dat$fix_intervals >= 36,]


ggplot(data = dat, aes(y = fix_intervals, x = doy)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  # ggtitle("Sampling") + 
  xlab("Month") +
  ylab("Fix sampling interval (hours)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, family = "sans", face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm")) +
  facet_wrap(~ year, 
             ncol = 2)  # Facet by year with free y scales
