
# elevation data based on coordinates

library(sf)
library(elevatr)     # for elevation, get_elev_raster()
library(raster)
library(tidyverse)

load("data/collar_data/collar_data_20240703.rda")
elev_25m = rast('data/rasters/elev_25m.tif')


#Convert gps coordinates to sf object, spatial data points
coord_sf <- st_as_sf(collar_data, coords = c("longitude", "latitude"))
#Retrieve coordinate reference system (CRS) and set the CRS to a geographic coordinate system (e.g., WGS 84)
st_crs(coord_sf) <- st_crs("+proj=longlat +datum=WGS84")
#Get the bounding box of the goat data
bbox <- st_bbox(coord_sf)

#Get elevation raster for the region(s) of interest (units = meters)
dem_raster <- get_elev_raster(locations = coord_sf, 
                              z = 9,
                              clip = 'bbox',
                              expand = 0.2)
plot(dem_raster)

#Extract the Digital Elevation Model (DEM) data from the raster
dem_data <- mutate(collar_data,
                   el = terra::extract(dem_raster, coord_sf),
                   ID1 = 1:n(),
                   ID2 = ID1) %>%
  relocate(ID1:ID2)

# drop ID1,ID2 columns
dem_data <- dem_data[,3:19]


# specify folder to where the plots are going to be saved to
output_dir <- "./figures/elevation/"

# create directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

#plot coordinates for visual
ggplot(dem_data, aes(longitude, latitude, color = el)) +
  geom_point() +
  scale_color_viridis_c()

# plot elevation overtime
ggplot() +
  geom_line(data = dem_data, aes(x = date, y = el)) +
  facet_wrap(~collar_id, ncol = 2, nrow = 6,
             scales = "fixed") +
  labs(y = 'Elevation (m)',
       x = 'Date') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), #removes horizontal gridlines
        panel.grid.minor = element_blank(), #removes vertical gridlines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# ggsave(filename = paste0(output_dir, "elevation", ".png"), 
#        plot = last_plot(), width = 10, height = 8)


# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- dem_data[dem_data$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks


ggplot(data = dem_data) +
  geom_line(aes(x = doy, y = el, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 273), color = "black", linetype = "dotdash") +
  facet_wrap(~ collar_id, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  labs(y = 'Elevation (m)',
       x = 'Month') + 
  scale_x_continuous(limits = c(0, 366), 
                     expand = c(0.01, 0.01), # limits a space before/after data on plot
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  # scale_y_continuous(limits = c(1500, 2600),
  #                    breaks = seq(1500, 2600, by = 300)) + 
  scale_color_manual(name = "", 
                     values = c("2019" = "#332288",
                                "2020" = "#ddaa33",
                                "2021" = "#006d2c",
                                "2022" = "#33bbee",
                                "2023" = 'red')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center")



ggsave(filename = paste0(output_dir, "elevation_calendar", ".png"), 
       plot = last_plot(), width = 10, height = 8)





# for 2023 only (not including 30551)
ggplot(dem_data[dem_data$year == "2023" & dem_data$collar_id != "30551", ]) +
  geom_line(aes(x = doy, y = el, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 273), color = "black", linetype = "dotdash") +
  facet_wrap(~ collar_id, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  labs(y = 'Elevation (m)',
       x = 'Month') + 
  ggtitle("2023") +
  scale_x_continuous(limits = c(0, 366), 
                     expand = c(0.01, 0.01), # limits a space before/after data on plot
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(name = "", 
                     values = c("2023" = 'red')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        legend.position = "none")


ggsave(filename = paste0(output_dir, "elevation_calendar_2023", ".png"), 
       plot = last_plot(), width = 10, height = 8)

#.............................................
# for each individual ----



goat <- unique(dem_data$collar_id)

# # determine the doy number for the 1st of each month for xaxis breaks
# day_1 <- dem_data[dem_data$day == 1, ]
# day_1 <- day_1[!duplicated(day_1$month), ]
# day_1[order(day_1$month),]
# month_breaks <- sort(day_1$doy)
# month_breaks

# collar_id <- 30548

# plot elevation overtime
for (collar_id in goat) {
  # extract individual data
  DATA <- dem_data[dem_data$collar_id == collar_id,]
  
  # over time
  p <-
    ggplot(data = DATA) +
    geom_line(aes(x = date, y = el)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
    labs(y = 'Elevation (m)',
         x = 'Date') + 
    ggtitle(paste(collar_id)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), #removes horizontal gridlines
          panel.grid.minor = element_blank(), #removes vertical gridlines
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  ggsave(filename = paste0(output_dir, "elevation_", collar_id, ".png"), 
         plot = p, width = 10, height = 8)
  
  
  
  
  # calendar plot
  p2 <-
    ggplot(data = DATA) +
    geom_line(aes(x = doy, y = el, color = as.factor(year), group = as.factor(year))) +
    geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
    facet_wrap(~ year, ncol = 1, nrow = 5,
               scales = "fixed", ) +  #set axis so theyre the same for every plot
    labs(y = 'Elevation (m)',
         x = 'Month') + 
    ggtitle(paste(collar_id)) +
    scale_x_continuous(limits = c(-1, 367), 
                       expand = c(0.01, 0.01), # limits a space before/after
                       breaks = month_breaks, # Approximate month starts
                       labels = month.abb) + # Month abbreviations
    scale_y_continuous(limits = c(1500, 2600),
                       breaks = seq(1500, 2600, by = 200)) + 
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
          plot.title = element_text(hjust = 0.5, size = 14, family = "sans"),
          plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
          legend.position="none")
  
  
  ggsave(filename = paste0(output_dir, "elevation_calendar_", collar_id, ".png"), 
         plot = p2, width = 10, height = 8)
  
}










#_______________________________________________________________________
# calculate the elevation change between timestamp points

# Ensure the data is sorted by goat_id and timestamp
dem_data_sorted <- dem_data[order(dem_data$collar_id, dem_data$timestamp), ]

# create column for change in elevation value (i.e. delta value, delta = final - initial)
dem_data_sorted$delta_el <- NA

# Loop through each animal (grouped by collar_id)
for (i in unique(dem_data_sorted$collar_id)) {
  # Subset an individual
  animal <- dem_data_sorted[dem_data_sorted$collar_id == i, ]
  
  # Calculate the elevation change between consecutive points
  for (j in 2:nrow(animal)) {
    animal$delta_el[j] <- animal$el[j] - animal$el[j-1]
  }
  
  # store results
  dem_data_sorted[dem_data_sorted$collar_id == i, ] <- animal
}


# plot changes in elevation over time
ggplot(dem_data_sorted, aes(x = date, y = delta_el, color = collar_id)) +
  geom_line() + 
  facet_wrap(~ collar_id, ncol = 2, scales = "fixed") +
  labs(title = "Elevation Change Over Time",
       x = "Date",
       y = "Elevation Change (m)") 



# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- dem_data_sorted[dem_data_sorted$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks

# calendar year
ggplot(data = dem_data_sorted) +
  geom_line(aes(x = doy, y = delta_el, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ collar_id, ncol = 2, nrow = 6, # sorted by ID
             scales = "fixed", ) +  #set axis so theyre the same for every plot
  labs(y = 'Elevation change (m)',
       x = 'Month') + 
  scale_x_continuous(limits = c(0, 366), 
                     expand = c(0.01, 0.01), # limits a space before/after
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







#...........................................................
# extract using raster ----


load("data/collar_data/collar_data_20240703.rda")
elev_25m = rast('data/rasters/elev_25m.tif')


#Convert gps coordinates to sf object, spatial data points
coord_sf <- st_as_sf(collar_data, coords = c("longitude", "latitude"))
#Retrieve coordinate reference system (CRS) and set the CRS to a geographic coordinate system (e.g., WGS 84)
st_crs(coord_sf) <- st_crs("+proj=longlat +datum=WGS84")
#convert sf into spatvector object to be able to extract values
locations <- vect(coord_sf)

# reproject crs
elev <- project(elev_25m, "EPSG:4326")

# extract values
el_values <- extract(elev, locations)[,2]
# get elevation range based on collar data coordinates
range(el_values)

#Get the bounding box of the goat data
bbox <- st_bbox(coord_sf)