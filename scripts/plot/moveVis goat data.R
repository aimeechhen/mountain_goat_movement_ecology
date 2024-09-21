
# Plotting collar data for visualization
#https://movevis.org/


devtools::install_github("16EAGLE/moveVis")
library(moveVis)
library(move)

# Load collar data
load("data/goat/goat_data.rda")

# Convert dataframe collar data into a move object
move_data <- df2move(goat_data, 
                     proj = "epsg:4326",
                     x = "location.long", y = "location.lat", time = "timestamp",
                     track_id = "goat_id")

# align move_data to a uniform time scale (may take a while)
m <- align_move(move_data, res = 120, unit = "mins")

# create spatial frames with a OpenStreetMap watercolour map (basemap is used by default)
frames <- frames_spatial(m, path_colours = rainbow(10)) %>% #,
                         # map_service = "osm_stamen", map_type = "watercolor", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames (may take a while, like hours, for a lot of data)
# animate_frames(frames, out_file = "figures/moveVis.gif")
Start <- Sys.time()
animate_frames(frames, out_file = "figures/moveVis.mp4")
# Approx animation duration = ~750.25s (~12.5 min) @ 25 fps for 18757 frames
End <- Sys.time()
