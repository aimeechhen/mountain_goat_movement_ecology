


# Esri.WorldTopoMap for elevation, Esri.WorldImagery for regular map image,
# any other leaflet tiles can be used here, refer to leaflet package for more information

movement_map <- function(data, map_type = 'Esri.WorldImagery') {
  # create an argument to be able to change the map type
  # extract the map source via leaflet provider() function list
  map_source <- leaflet::providers[[map_type]]
  
  # if you entered the wrong type or incorrectly, you'll get an error telling you so
  if (is.null(map_source)) {
    stop(paste("Map provider", map_type, "not found."))
  }
  
  # plot
  leaflet(data = data) %>%
    addProviderTiles(provider = map_type) %>%
    # movement path
    addPolylines(lng = data$longitude,
                 lat = data$latitude,
                 color = "red",
                 weight = 2,
                 opacity = 0.6) %>%
    # gps points
    addCircleMarkers(lng = data$longitude,
                     lat = data$latitude,
                     color = "red",
                     radius = 3,
                     fillOpacity = 0.8,
                     stroke = FALSE,
                     label = ~as.character(data$timestamp)) %>% 
    # add title (Note: when saving the plot, this title does not transfer over when using mapview::mapshot2)
    addControl(data$individual.local.identifier[1],
               position = "topleft")
  
}
