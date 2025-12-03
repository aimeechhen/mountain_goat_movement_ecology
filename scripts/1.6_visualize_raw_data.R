
library(sf)
library(ggplot2)
library(basemaps)
library(leaflet)


#..............................................................
# visualize the raw data ----
#..............................................................


# load("./data/goat/prep/combined_data.rda")
load("./data/goat/collar_data.rda")


# DATA <- combined_data
DATA <- collar_data
# DATA <- DATA[DATA$data_source == 1 & DATA$outlier == 0, ]
DATA <- DATA[DATA$data_source == 2 & DATA$outlier == 0, ]
# DATA <- DATA[DATA$outlier == 0, ]



# convert to sf object for plotting
data_sf <- st_as_sf(DATA, coords = c("longitude", "latitude"), crs = 4326)
data_sf <- st_transform(data_sf, crs = st_crs(3857))
ext <- st_as_sfc(st_bbox(data_sf))
ext <- st_bbox(st_buffer(st_transform(ext, crs = 3857), dist = 5000))
# extract the coordinates from the geometry and add them back to the df to plot pathway
data_sf <- cbind(data_sf, st_coordinates(data_sf))




#.............................................................................................
# pathway ----
#.............................................................................................


ggplot() +
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery", zoom = 16) +
  geom_path(data = data_sf, aes(x = X, y = Y, group = goat_id, color = goat_id), alpha = 0.5) +
  geom_point(data = data_sf, aes(x = X, y = Y, color = goat_id), alpha = 0.5) +
  scale_fill_identity() +
  coord_sf() +
  theme_bw() +
  theme(legend.position = "top")

# ggsave(last_plot(), file = "figures/outlie_filtering/1_pathway_raw_original_screened.png",
# ggsave(last_plot(), file = "figures/outlie_filtering/2_pathway_raw_new.png", 
# ggsave(last_plot(), file = "figures/outlie_filtering/3a_pathway_good_locations_original.png",
ggsave(last_plot(), file = "figures/outlie_filtering/3b_pathway_good_locations_new.png",
       height = 6*1.5, width = 6.23*1.5, units = "in", dpi = 600)




ggplot() +
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery", zoom = 16) +
  geom_path(data = data_sf, aes(x = X, y = Y, group = goat_id, color = goat_id)) +
  geom_point(data = data_sf, aes(x = X, y = Y, color = goat_id), alpha = 0.5, size = 0.9) +
  scale_fill_identity() +
  coord_sf() +
  facet_wrap(~goat_id, ncol = 3) +
  theme_bw() +
  theme(legend.position = "none")


# ggsave(last_plot(), file = "figures/outlie_filtering/1_pathway_raw_original_screened_individual.png", 
# ggsave(last_plot(), file = "figures/outlie_filtering/2_pathway_raw_new_individual.png", 
# ggsave(last_plot(), file = "figures/outlie_filtering/3a_pathway_good_locations_original_individual.png", 
ggsave(last_plot(), file = "figures/outlie_filtering/3b_pathway_good_locations_new_individual.png",
       height = 6*3, width = 6.23*3, units = "in", dpi = 600)

# you can definitely see some potential outliers




#.............................................................................................
# points ----
#.............................................................................................




ggplot() +
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery", zoom = 15) +
  geom_sf(data = data_sf, aes(color = goat_id)) +
  scale_fill_identity() +
  coord_sf() +
  theme_minimal() +
  theme(legend.position = "top")

# save plot to refer to while data cleaning
ggsave(last_plot(), file = "figures/outlie_filtering/2_points_raw_new.png", 
       height = 6*1.5, width = 6.23*1.5, units = "in", dpi = 600)




ggplot() +
  basemap_gglayer(ext, map_service = "esri", map_type = "world_imagery", zoom = 15) +
  geom_sf(data = data_sf, aes(color = goat_id)) +
  scale_fill_identity() +
  coord_sf() +
  facet_wrap(~goat_id) +
  theme_bw() +
  theme(legend.position = "none")

# save plot to refer to while data cleaning
ggsave(last_plot(), file = "figures/outlie_filtering/2_points_raw_new_individual.png", 
       height = 6*1.5, width = 6.23*3, units = "in", dpi = 600)
