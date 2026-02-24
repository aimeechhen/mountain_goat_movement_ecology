














load("./data/fire/fire_combined.rda")
load("./data/fire/fire_perimeter_by_date.rda")


#........................................................
# plot fire data

# plot fire boundaries
perimeter_dates <- unique(fire_combined$date_local)

i <- 4
# plot boundaries for each day
for(i in seq_along(perimeter_dates)) {
  ggplot(fire_combined[fire_combined$date_local == perimeter_dates[i], ]) +
    geom_sf(aes(fill = data_source), color = "black", alpha = 0.5) +
    scale_fill_manual(values = c("foippa" = "lightblue", "cwfis" = "orange")) +
    ggtitle(as.character(perimeter_dates[i])) +
    theme_minimal()
  
  ggsave(last_plot(), file = paste0("figures/fire/perimeter_check/perimeter_date_", as.character(perimeter_dates[i]), ".png"))
  
}





#........................................................
# plot fire perimeters merged together

date_merged <- unique(fire_by_date$date)
# 
# for(i in seq_along(date_merged)) {
#   ggplot(fire_by_date[fire_by_date$date == date_merged[i], ]) +
#     geom_sf(fill = "orange", color = "black") +
#     ggtitle(as.character(date_merged[i])) +
#     theme_minimal()
#   
#   ggsave(last_plot(), file = paste0("figures/fire/perimeter_merged/perimeter_date_", as.character(date_merged[i]), ".png"))
#   
# }




# #.......................................................................
# # plot fire per date with goat data
# 
# library(ggplot2)
# 
# ggplot() +
#   geom_sf(data = fire, aes(color = as.factor(date)), fill = NA)

library(basemaps)
library(RColorBrewer)

load("./data/goat/study_data.rda")
str(study_data)
study_data$date_local <- as.Date(study_data$date_local)

DATA <- study_data

data_sf <- st_as_sf(DATA, coords = c("longitude", "latitude"), crs = 4326)
data_sf <- st_transform(data_sf, crs = st_crs(3857))
fire_by_date_reproj <- st_transform(fire_by_date, crs = st_crs(3857))

# get a bbox for both fire and goat data, merge their geometries into single geometry
ext <- st_as_sfc(st_bbox(st_union(c(st_geometry(data_sf), st_geometry(fire_by_date_reproj)))))
ext <- st_bbox(st_buffer(st_transform(ext, crs = 3857), dist = 1500))
# extract the coordinates from the geometry and add them back to the df to plot pathway
data_sf <- cbind(data_sf, st_coordinates(data_sf))



# plot all together over a map

ggplot() +
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery", zoom = 16) +
  geom_path(data = data_sf, aes(x = X, y = Y, group = id_year, color = goat_id), alpha = 1.2) +
  geom_point(data = data_sf, aes(x = X, y = Y, color = goat_id), alpha = 1.2) +
  geom_sf(data = fire_by_date_reproj, color = "red", fill = NA) +
  scale_fill_identity() +
  scale_color_manual(values = setNames(brewer.pal(10, "Set1"), unique(data_sf$goat_id)),
                     guide = guide_legend(nrow = 1, byrow = TRUE)) +
  coord_sf(expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank())

#............................................................................


# plot per date with map
for(i in seq_along(date_merged)) {
  # subset data based on the date
  goat_data <- data_sf[data_sf$date_local == date_merged[i], ]
  fire_data <- fire_by_date_reproj[fire_by_date_reproj$date == date_merged[i], ]
  
  # # set bbox around goat data for that day
  ext <- st_as_sfc(st_bbox(st_union(c(st_geometry(data_sf), st_geometry(fire_data)))))
  ext <- st_bbox(st_buffer(st_transform(ext, crs = 3857), dist = 1500))
  
  ggplot() +
    basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery", zoom = 16) +
    geom_path(data = goat_data, aes(x = X, y = Y, group = id_year, color = goat_id), alpha = 1.2) +
    geom_point(data = goat_data, aes(x = X, y = Y, color = goat_id), alpha = 1.2) +
    geom_sf(data = fire_data, color = "red", fill = NA) +
    scale_fill_identity() +
    scale_color_manual(values = setNames(brewer.pal(10, "Set1"), unique(data_sf$goat_id)),
                       guide = guide_legend(nrow = 1, byrow = TRUE)) +
    coord_sf(expand = FALSE) +
    theme_bw() +
    ggtitle(as.character(date_merged[i])) +
    theme(legend.position = "top",
          legend.title = element_blank())
  
  ggsave(last_plot(), file = paste0("figures/fire_goat_by_date/fire_date_", as.character(date_merged[i]), ".png"))
  
}




# plot per date without map


for(i in seq_along(date_merged)) {
  # subset data based on the date
  goat_data <- data_sf[data_sf$date_local == date_merged[i], ]
  fire_data <- fire_by_date_reproj[fire_by_date_reproj$date == date_merged[i], ]
  
  ggplot() +
    geom_path(data = goat_data, aes(x = X, y = Y, group = id_year, color = goat_id), alpha = 1.2) +
    geom_point(data = goat_data, aes(x = X, y = Y, color = goat_id), alpha = 1.2) +
    geom_sf(data = fire_data, color = "red", fill = NA) +
    scale_color_manual(values = setNames(brewer.pal(10, "Set1"), unique(data_sf$goat_id)),
                       guide = guide_legend(nrow = 1, byrow = TRUE)) +
    ggtitle(as.character(date_merged[i])) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.title = element_blank())
  
  ggsave(last_plot(), file = paste0("figures/fire_goat_by_date/no_map/fire_date_", as.character(date_merged[i]), ".png"))
  
}



goat_subset <- data_sf[as.Date(data_sf$date_local) %in% fire_by_date$date, ]
fire_by_date$date_local <- as.Date(fire_by_date$date)  # match the day of goat data

# facet wrap all fire dates
ggplot() +
  geom_path(data = goat_subset, aes(x = X, y = Y, group = id_year, color = goat_id), alpha = 1.2) +
  geom_point(data = goat_subset, aes(x = X, y = Y, color = goat_id), alpha = 1.2) +
  geom_sf(data = fire_by_date, color = "red", fill = NA) +
  scale_color_manual(values = setNames(brewer.pal(10, "Set1"), unique(goat_subset$goat_id)),
                     guide = guide_legend(nrow = 1, byrow = TRUE)) +
  facet_wrap(~ as.Date(date_local)) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())

ggsave(last_plot(), file = "./figures/fire_perimeter_goat_plot_by_date.png",
       width = 8*2, height = 11*2, unit = "in")
