
# park boundary as a sf object
library(sf)


# Import Provincial park polygons
source('./scripts/cathedral_park_shapefile.r')

library(ggplot2)
library(stringr)
library(lubridate)
library(gganimate)

library(ctmm)
# Add goat tracking data
load("data/goat/goat_data.rda")
load("data/helicopter/heli_data.rda")

# Create the plot with heli data and goat data

# goat data
ggplot() +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat, color = date)) + 
  theme_minimal() +
  theme(legend.position = "none")

# cathedral park
ggplot() +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1)


# helicopter data
ggplot() +
  geom_point(data = heli_data, aes(x = long, y = lat, color = date)) + 
  theme_minimal() +
  theme(legend.position = "none")



# static plot
ggplot() +
  geom_point(data = heli_data, aes(x = long, y = lat, color = as.factor(date))) +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat), color = "black") +  # Add goat data points
  scale_color_manual(name = "date", values = rainbow(length(unique(heli_data$date)))) +
  labs(title = "Spatial Distribution of Mountain Goats and Helicopters Over Time", color = "date") +
  theme_bw() +
  theme(legend.position = "none")




#..........................................................
# Animate the plot ----
#..........................................................


# plot goat vs helicopters
p <- 
ggplot() +
  geom_point(data = heli_data, aes(x = long, y = lat, color = as.factor(date))) +
  geom_sf(data = cathedral, fill = NA, color = "black", size = 1) +
  geom_point(data = goat_data, aes(x = location.long, y = location.lat), color = "black") +  # Add goat data points
  #scale_color_manual(name = "date", values = rainbow(length(unique(heli_data$date)))) +
  #labs(title = "Spatial Distribution of Mountain Goats and Helicopters Over Time", color = "date") +
  theme_bw()



# Animate the plot
p_animated <- p + transition_time(date) +
  labs(title = "Date: {frame_time}") +
  #shadow_mark(alpha = 0.3, size = 1) + #so the dots dont disappear
  # shadow_wake(wake_length = 0.1, alpha = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        # panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA))

# Print the animated plot
print(p_animated,duration = 10)

# save the animation
anim_save("goat_heli_activity_plot_animation.gif", 
          animation = last_animation(), path = "figures", 
          width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")

