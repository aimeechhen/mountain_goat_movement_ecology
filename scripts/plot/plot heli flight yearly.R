

# import data first from 02 script
library(ctmm)
library(lubridate)
library(ggplot2)
library(gridExtra)

load("data/helicopter/heli_data.rda")

scale_fill_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                  breaks = c('2019', '2020', '2021')) +
  scale_colour_manual(values = c("#005f73", "#ae2012", "#0a9396"), 
                      breaks = c('2019', '2020', '2021'))

#subset based on year to plot
flight_2019 <- heli_data[heli_data$year == "2019",]
flight_2020 <- heli_data[heli_data$year == "2020",]
flight_2021 <- heli_data[heli_data$year == "2021",]


#Convert to individual telemetry objects
flights_2019 <- as.telemetry(heli_data[heli_data$year == "2019",])
projection(flights_2019) <- median(heli_before)
flights_2020 <- as.telemetry(heli_data[heli_data$year == "2020",])
projection(flights_2020) <- median(heli_before)
flights_2021 <- as.telemetry(heli_data[heli_data$year == "2021",])
projection(flights_2021) <- median(heli_before)


#Convert to individual telemetry objects
heli_before <- as.telemetry(heli_dat[heli_dat$period == "before",])
projection(heli_before) <- median(heli_before)
# heli_during <- as.telemetry(heli_dat[heli_dat$period == "during",])
# projection(heli_during) <- median(heli_before)
heli_after <- as.telemetry(heli_dat[heli_dat$period == "after",])
projection(heli_after) <- median(heli_before)



#...............................................................




# Figure of helicopter tracks and flight times per year
png(file="figures/heli_tracks_per_year.png",
    width=3*1.5, height=6*1.5, units="in", res=600)
par(mfrow = c(3,1))


plot(flights_2019, col = viridis::viridis(length(flights_2019)))
title("a)", adj = 0)
mtext("2019", side = 3, line = 1, adj = 0.5)

plot(flights_2020, col = viridis::viridis(length(flights_2020)))
title("b)", adj = 0)
mtext("2020", side = 3, line = 1, adj = 0.5)

plot(flights_2021, col = viridis::viridis(length(flights_2021)))
title("c)", adj = 0)
mtext("2021", side = 3, line = 1, adj = 0.5)

dev.off()


#...............................................................
# Plot based on Flight ID


#plot_2019 <-
  ggplot(data = flight_2019,
         aes(y = ID, x = doy)) +
  geom_point() + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +
  xlab("Month") +
  ylab("Flight ID") +
  ggtitle("2019") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



plot_2020 <-
  ggplot(data = flight_2020,
         aes(y = ID, x = doy)) +
  geom_point() + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +
  xlab("Month") +
  ylab("Flight ID") +
  ggtitle("2020") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



plot_2021 <-
  ggplot(data = flight_2021,
         aes(y = ID, x = doy)) +
  geom_point() + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) + 
  xlab("Month") +
  ylab("Flight ID") +
  ggtitle("2021") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))




flight_plot <- grid.arrange(plot_2019,
                            plot_2020,
                            plot_2021,
                            ncol = 1)
flight_plot
ggsave(flight_plot, filename = "figures/flight_plot.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)





#................................................................




# annual_heli_2019 <-
  ggplot(data = flight_2019,
         aes(y = ID, x = doy)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  ggtitle("Helicopter Activity") + 
  xlab("Month") +
  ylab("2019") +
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
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))



annual_heli_2020 <-
  ggplot(data = flight_2020,
         aes(y = ID, x = doy)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  scale_y_continuous(labels = scales::scientific) +
  xlab("Month") +
  ylab("2020") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))


annual_heli_2021 <-
  ggplot(data = flight_2021,
         aes(y = ID, x = doy)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(limits = c(-2, 370), expand = c(0, 1),
                     breaks = seq(0, 365, by = 30),
                     labels = c(month.abb, month.abb[1])) +  # Use month abbreviations
  scale_y_continuous(labels = scales::scientific) +
  xlab("Month") +
  ylab("2021") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 14, family = "sans", face = "bold"),
        axis.title.y = element_text(size=10, family = "sans", face = "bold"),
        axis.title.x = element_text(size=10, family = "sans", face = "bold"),
        axis.text.y = element_text(size=8, family = "sans"),
        axis.text.x  = element_text(size=8, family = "sans"),
        legend.position="none",
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"))




annual_flight_plot <- grid.arrange(annual_heli_2019,
                                   annual_heli_2020,
                                   annual_heli_2021,
                                   ncol = 1)


annual_flight_plot
ggsave(annual_flight_plot, width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent",
       file="figures/annual_heli_activity.png")
