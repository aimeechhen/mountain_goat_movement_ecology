
# Manual outlier inspection

# refer to https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2656.13610 section 5.2 Filtering unrealistic movement
# for those spikes in movement tracks
# inspecting positions before and after spikes or point of interest, incoming and outgoing speeds

# based on the concept of Stefano's plot_adj script (https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/functions/plot_adj.R)

# Purpose:
# take a closer look, function to plot sequential points before and after the flagged point of interest for further inspection via visualization (give it a minute to execute)
# these plots are similar to plot_outlie() except plot_range() you can set the colour palette, the reasoning is to be able to see movement direction via colour
# values are set to null to allow for parameters not need to be included

# Plot 4 panels to check data for outliers via visualization (give it a minute to execute)

# Plot panel description:
#1: telemetry gps points
#2: location and velocities, from the outlie() function
#3: outlie() output of distance and speed, note: "out <- outlie(telemetry, units = FALSE)", 'out' is the output of outlier() within the custom function, plotting that output
#4: telemetry path on top of a map

# limit = how many points you want to look at before/after the point(s) of interest (poi) i.e., setting the range of points, set to = if you dont want to see any of the fixes before/after
plot_range <- function(telemetry, 
                       fix_id = NULL, # if you want to enter fix_id
                       fix_id_col = "fix_id", # indicate the id column name
                       xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, # bounding box extent
                       limit = 0, # number of points before/after to define section/range
                       palette = NULL, # if using gradient palettes and its light to dark, dark will match up with the most recent fixes and lighter ones would be the oldest points
                       reset_layout = TRUE) {
  
  #...........................................................
  # define points of interest (POI), the fix you want to inspect/take a closer look at
  # two approaches in isolating, via fix_id or bounding box
  
  # via fix_id
  if (!is.null(fix_id)) {
    POI <- which(telemetry[[fix_id_col]] %in% fix_id)
    if (length(POI) == 0) {
      stop("fix_id not found in telemetry data.")
    }
  } else {
    # via bounding box
    # set the bounding box around the POI
    bbox <- (is.null(xmin) | telemetry$x >= xmin) &
      (is.null(xmax) | telemetry$x <= xmax) &
      (is.null(ymin) | telemetry$y >= ymin) &
      (is.null(ymax) | telemetry$y <= ymax)
    # find the row number of the points that are within the bbox to inspect if theyre outliers or not
    POI <- which(bbox)
    message("number of points found in bbox as the points of interest (POI): ", length(POI))
  }
  
  # indicate if you entered the wrong values for the bbox or fix_id
  if (length(POI) == 0) {
    warning("No points selected as POI or no points fall inside the specified bounding box. Check bbox extent values or fix_id")
    return(list(data_section = NULL, POI = NULL))
  }
  
  # create an empty vector that will represent the section in the data before/after the POI you want to subset 
  section <- c()  
  for (pt in POI) {
    # get the start and end for each point, if there are more than one point then there will be more than 1 section being defined, if there are multiple sections that overlap, drop the duplicate rows so when you combine them all, you'll only have one section
    section <- unique(as.vector(sapply(POI, function(pt) (pt - limit):(pt + limit))))
  }
  
  # define the section -> get rid of rows that are outside of the section
  section <- section[which(section >= 1 & section <= nrow(telemetry))]
  
  # subset data within the defined section from the telemetry data
  subset_data <- telemetry[section, ]
  
  # how many points are in the section
  n_pts_section <- nrow(subset_data)
  message("number of points of interest: ", length(POI), " and points in the section: ", n_pts_section)
  
  #...................................................
  # set up the palette based on the number of points in this section
  colour_palette <- if (is.function(palette)) {
    # generate the palette based on the n_pts_section
    palette(n_pts_section)                     
  } else {
    rep(palette, length.out = n_pts_section)
  }
  
  # indicate the POI in the defined section
  section_poi <- which(section %in% POI)
  
  #.............................................
  # plot the section of points
  layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)) # set up multi-panel layout 
  par(oma = c(0, 0, 1.5, 0))  # allow a main plot title be added, set outer top margin (lines)
  
  
  # plot telemetry data
  plot(subset_data, units = FALSE)
  title(main = "Plot 1: telemetry data for section")
  
  # plot outlie for the data section (plot of locations and velocities)
  out_section <- outlie(subset_data, units = FALSE, plot = TRUE)
  title(main = "Plot 2: location & velocities for section (outlie() output)")
  
  # min speed vs distance, inspect plot units, units = FALSE to have units plotted as m/s instead of cm/s and m
  ctmm::plot(out_section, units = FALSE)
  title(main = "Plot 3: speed vs distance (outlie object)")
  
  # plot pathway
  # # create an empty plot first because were custom colouring it, the colours come in after
  # plot(subset_data$x, subset_data$y, type = 'n',
  #      main = 'Plot 4: telemetry path for section', xlab = "x (meters)", ylab = "y (meters)")
  # # add the coloured points
  # points(subset_data$x, subset_data$y,
  #        col = colour_palette, cex = 2)
  # 
  # # then, add the connecting lines, n_pt-1 means the lines between the points youll have 5 points but only 4 lines connecting, so thats why its n_pts minus 1
  # for (pt in 1:(n_pts_section - 1)) {
  #   # connecting the dots with a line and colouring the lines from pt to pt + 1 based on the set palette
  #   lines(subset_data$x[pt : (pt + 1)], 
  #         subset_data$y[pt : (pt + 1)], 
  #         col = colour_palette[pt], lwd = 2)
  # }
  # # lastly, add the POI in red
  # points(subset_data$x[section_poi],
  #        subset_data$y[section_poi],
  #        col = "#FF0000", pch = 19, cex = 2.5)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plot telemetry path on map
  coords <- st_coordinates(st_transform(as.sf(subset_data),  "EPSG:3857")) # convert to sf object, transform to 3857 crs, extract coordinates
  basemaps::basemap_plot(ext = st_transform(st_bbox(c(xmin = min(subset_data$longitude) - 0.05,
                                                      ymin = min(subset_data$latitude) - 0.01,
                                                      xmax = max(subset_data$longitude) + 0.05,
                                                      ymax = max(subset_data$latitude) + 0.01), crs = 4326), "EPSG:3857"),
                         map_service = "esri",
                         map_type = "world_imagery")
  # add the coloured points
  points(coords[,1], coords[,2],
         col = colour_palette,cex = 2)
  
  # then, add the connecting lines, n_pt-1 means the lines between the points youll have 5 points but only 4 lines connecting, so thats why its n_pts minus 1
  for (pt in 1:(n_pts_section - 1)) {
    lines(coords[pt:(pt + 1), 1],
          coords[pt:(pt + 1), 2],
          col = colour_palette[pt], lwd = 2)
  }
  
  # lastly, add the POI in red
  points(coords[section_poi, 1],
         coords[section_poi, 2],
         col = "#FF0000", pch = 19, cex = 2.5)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # add a title to the multipanel plot
  mtext(paste0("ID: ", unique(telemetry$individual.local.identifier)), outer = TRUE, adj = 0.5)
  
  # resets the plot layout to default i.e., single plot, not multi-panel, so its like dev.off() without "closing/clearing" the plot
  if(reset_layout) layout(1)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # this section is from outlie2 function
  # combining outlie data with telemetry data
  subset_data$row_n <- rownames(subset_data)
  out_section$row_n <- rownames(out_section)
  # check if the rownames match (rowname is the representation of fix_id since outlie() doesn't have the keep argument to carry the information over, assuming it has been configured properly and before)
  if (!identical(rownames(out_section), rownames(subset_data))) {
    stop("rownames are not the same")
  }
  
  # check if the t value matches in both
  if (!all(out_section$t == subset_data$t)) {
    stop(paste0("t does not match at row(s): ", paste(which(out_section$t != subset_data$t), collapse = ", ")))
  }
  
  # check for duplicate columns
  # intersect(colnames(out), colnames(telemetry))
  
  # if the two checks above are true, then cbind the columns while avoiding duplicating columns
  output <- cbind(subset_data[, setdiff(names(subset_data), names(out_section)), drop = FALSE], out_section)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # make these object outputs available outside of the function, to get the information when running the function
  # output <- plot_range(x)
  # output$POI
  return(list(data_section = output,
              POI = telemetry[POI, ])) # bad points
}


