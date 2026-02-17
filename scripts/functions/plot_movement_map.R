





# custom function to plot gps points over terrain (or some whatever leaflet has available) map
# additionally using addCircleMarker() allows you to hover over the points on the map to get details of that point
# to add a raster layer to the map, refer to https://rstudio.github.io/leaflet/articles/raster.html
# Other maps to consider using: 'USGS.USTopo'
# Esri.WorldTopoMap for elevation, Esri.WorldImagery for regular map image
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
                     # label and popup can modified to include whatever column info you want in to show)
                     label = ~paste0("fix_id: ", fix_id, "  |  ", as.character(data$timestamp)),
                     popup = ~paste0("fix_id: ", fix_id,
                                     "  |  timestamp: ", as.character(data$timestamp),
                                     "  |  lon: ", data$longitude,
                                     "  |  lat: ", data$latitude,
                                     "  |  x: ", round(x, 2),
                                     "  |  y: ", round(y, 2))) %>%
                       
                     
    # add title (Note: when saving the plot, this title does not transfer over when using mapview::mapshot2)
    addControl(data$individual.local.identifier[1],
               position = "topleft") %>%
    # add a layers control (ability to click through different map types) ***CURRENTLY DOESNT WORK NEED TO TROUBLESHOOT****, icon shows up but doesnt work
    addLayersControl(baseGroups = c("OpenStreetMap",
      "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery",  "Esri.WorldTerrain", "Esri.OceanBasemap", "Esri.NatGeoWorldMap",
      "CartoDB.Positron", "CartoDB.DarkMatter",
      "USGS.USTopo", "USGS.USImagery", "USGS.USImageryTopo"))

}
