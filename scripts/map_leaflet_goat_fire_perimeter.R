
library(ggplot2)
library(sf)
library(lubridate)
library(leaflet)
library(mapview)
library(webshot)








# Import goat data
load("data/collar_data/fire_period_all_years_combined_data_20250505.rda")
goat_data$timestamp = as.POSIXct(goat_data$timestamp, format = "%Y-%m-%d %H:%M:%S")
goat_data$date = as.Date(goat_data$date, "%Y-%m-%d")
goat_data$goat_name <- as.factor(goat_data$goat_name)
goat_data$collar_id <- as.factor(goat_data$collar_id)




#............................................................
# Plot
#............................................................


collar_id <- c("30548", "30561", "30575", "30613", "30642", "30648")
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # in collar id order
goat_data$goat_name <- factor(goat_data$goat_name, levels = goats)
goat_labels <- c("Goatzilla", "Selena Goatmez", "The Goatmother", "Goatileo", "Toats Mcgoats", "Vincent Van Goat") # must be in the same order as above
legend_labels <- setNames(goat_labels, goats) # create display labels for the legend
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# add a goat colour column and assign them the proper colour
goat_data$goat_color <- goat_palette[match(goat_data$goat_name, goats)]
# set up palette function and set levels order
pal <- colorFactor(palette = goat_palette,
                   domain = goat_data$goat_name,  
                   levels = goats)


#////////////////////////////////////////////////////////////////////
# map goat data ----
#////////////////////////////////////////////////////////////////////


# goat data only using goat_name in legend
# m <-
leaflet(data = goat_data) %>%
  # # add different provider tiles and a layers control so that users can switch between the different
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  # setView(lng = -120.1761, lat = 49.09, zoom = 11) %>%
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,
             color = ~pal(goat_name),
             fillOpacity = 0.9) %>%
  addLegend("topleft",
            pal = pal,
            values = ~goat_name,
            labFormat = function(type, cuts, p) {  # custom labelling function
              legend_labels[cuts]  # match the original goat_name to display label
            },
            title = "Mountain Goats") %>%
  addMiniMap(width = 150, height = 150, position = "bottomleft")










#////////////////////////////////////////////////////////////////////
# fire data ----
#////////////////////////////////////////////////////////////////////

library(paletteer)

# subset goat data to 2023, fire year
fire_goats <- goat_data[goat_data$year == "2023",]

# import fire boundaries
load('./data/fire/foippa.rda')
# fire <- foippa
load('./data/fire/cwfis.rda')
# fire <- cwfis
fire <- dplyr::bind_rows(foippa, cwfis)
which(st_is_empty(fire)) # check for empty geometries
# combine the polygons together for each date and drop all the other columns
fire <- aggregate(fire["geometry"], by = list(date = fire$date), FUN = st_union)
str(fire)
# set the date order  
fire <- fire[order(fire$date), ]

# set the palette to use
fire_palette <- paletteer_c("ggthemes::Classic Red", 31)
# assign the colour
fire$fire_color <- fire_palette

map <-
  leaflet(data = fire_goats) %>%
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
  # setView(lng = -120.175, lat = 49.02965, zoom = 11) %>% # centered on fire goat data
    addPolygons(data = fire, 
              color = fire$fire_color,
              fillOpacity = 0, weight = 2) %>%
  addLegend("topleft", 
            colors = fire$fire_color,
            labels = as.character(fire$date),
            title = "Fire perimeter") %>%  
  addMiniMap(width = 150, height = 150, position = "bottomright")

# # colours do not save, due to leaflet being an interactive, sometimes not able to handle
# mapview::mapshot(map, file = "figures/map_goat_and_fire_temp.png")

# ALTERNATIVE SAVING
# for the colours to be saved, save as a html first, then save as png
htmlwidgets::saveWidget(map, "figures/temp_map.html", selfcontained = TRUE)
# resave html as a png
webshot2::webshot("figures/temp_map.html",file = "figures/map_goat_and_fire_temp.png",
                  vwidth = 1200, vheight = 800)






#////////////////////////////////////////////////////////////////////
# map goat data and fire data ----
#////////////////////////////////////////////////////////////////////


# convert to sf object
fire_goats_sf <- st_as_sf(fire_goats, coords = c("longitude", "latitude"), crs = 4326)
# extract the center point
centroid_sf <- st_centroid(st_union(fire_goats_sf))  # union ensures single geometry
# display coordinates
st_coordinates(centroid_sf)

# combine both goat and fire together now

map <-
  leaflet(data = fire_goats) %>%
  # addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>% # topography
  # addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>% # satellite

  # goat 
  addCircles(lng = ~longitude, lat = ~latitude,
             radius = 2,
             color = ~pal(goat_name),
             fillOpacity = 1.2) %>%
  addLegend("topleft",
            pal = pal,
            values = ~goat_name,
            labFormat = function(type, cuts, p) {  # custom labelling function
              legend_labels[cuts]  # match the original goat_name to display label
            },
            title = "Mountain Goats") %>%
  
  # fire
  addPolygons(data = fire, 
              color = fire$fire_color,
              fillOpacity = 0, weight = 2) %>%
  addLegend("topright", 
            colors = fire$fire_color,
            labels = as.character(fire$date),
            title = "Fire perimeter") %>%  
  
  addMiniMap(width = 150, height = 150, position = "bottomleft")





# # colours do not save, due to leaflet being an interactive, sometimes not able to handle
# mapview::mapshot(map, file = "figures/map_goat_and_fire.png")

# ALTERNATIVE SAVING
# for the colours to be saved, save as a html first, then save as png
htmlwidgets::saveWidget(map, "figures/temp_map.html", selfcontained = TRUE)
# resave html as a png
webshot2::webshot("figures/temp_map.html",file = "figures/map_goat_and_fire_topo.png",
                  vwidth = 1200, vheight = 800)
