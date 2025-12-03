

# based on the concept of Stefano's plot_adj script (https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/functions/plot_adj.R)

# take a closer look, function to plot sequential points before and after the flagged point of interest for further inspection
# values are set to null to allow for parameters not need to be included

# limit = how many points you want to look at before/after the point(s) of interest (poi)
plot_range <- function(telemetry, 
                       xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL,
                       limit = 0,
                       palette = NULL, # if using gradient palettes and its light to dark, dark will match up with the most recent fixes and lighter ones would be the oldest points
                       reset_layout = TRUE) {
  
  # set the bounding box based on the function arguments
  bbox <- (is.null(xmin) | telemetry$x >= xmin) &
    (is.null(xmax) | telemetry$x <= xmax) &
    (is.null(ymin) | telemetry$y >= ymin) &
    (is.null(ymax) | telemetry$y <= ymax)
  
  
  #...........................................................
  # find the rownumber of the points that are within the bbox to inspect if theyre outliers or not
  POI <- which(bbox)
  
  message("number of points found in bbox as the points of interest (POI): ", length(POI))
  # indicate if you entered the wrong values for the bbox
  if (length(POI) == 0) {
    warning("No points fall inside the specified bounding box.")
    return(list(data_section = NULL, POI = NULL))
  }
  
  # create an empty vector that will represent the section
  section <- c()  
  for (pt in POI) {
    # get the start and end for each point, if there are more than one point then there will be more than 1 section being defined, if there are multiple sections that overlap, drop the duplicate rows so when you combine them all, you'll only have one section
    section <- unique(as.vector(sapply(POI, function(pt) (pt - limit):(pt + limit))))
  }
  
  # get rid of rows that are outside of the section
  section <- section[which(section >= 1 & section <= nrow(telemetry))]
  
  # fill the section with the actual points (the rows) from the data
  data_section <- telemetry[section, ]
  
  # how many points are in the section
  n_pts_section <- nrow(data_section)
  message("number of points of interest: ", length(POI), " and points in the section: ", n_pts_section)
  
  #...................................................
  # set up the palette based on the number of points in this section
  colour_palette <- if (is.function(palette)) {
    # generate the palette based on the n_pts_section
    palette(n_pts_section)                     
  } else {
    rep(palette, length.out = n_pts_section)
  }
  
  # indicate the POI in the section
  section_poi <- which(section %in% POI)
  
  
  #.............................................
  # plot the section of points
  layout(matrix(1:3, nrow = 1, ncol = 3, byrow = TRUE)) # set up multi-panel layout plots
  par(oma = c(0, 0, 3, 0))  # allow a main plot title be added
  
  # plot telemetry data
  plot(data_section, units = FALSE)
  title(main = "Plot 1: telemetry data for section")
  
  # plot outlie for the data section
  out_section <- outlie(data_section, units = FALSE, plot = TRUE)
  title(main = "Plot 2: outlie output for section")
  
  # plot pathway
  # create an empty plot first because were custom colouring it, the colours come in after
  plot(data_section$x, data_section$y, type = 'n',
       main = 'Plot 3: telemetry path for section', xlab = "x (meters)", ylab = "y (meters)")
  # add the coloured points
  points(data_section$x, data_section$y,
         col = colour_palette, cex = 2)
  
  # then, add the connecting lines, n_pt-1 means the lines between the points youll have 5 points but only 4 lines connecting, so thats why its n_pts minus 1
  for (pt in 1:(n_pts_section - 1)) {
    # connecting the dots with a line and colouring the lines from pt to pt + 1 based on the set palette
    lines(data_section$x[pt : (pt + 1)], 
          data_section$y[pt : (pt + 1)], 
          col = colour_palette[pt], lwd = 2)
  }
  # lastly, add the POI in red
  points(data_section$x[section_poi],
         data_section$y[section_poi],
         col = "#FF0000", pch = 19, cex = 2.5)
  
  # add a title to the multipanel plot
  mtext(paste0("ID: ", unique(telemetry$individual.local.identifier)), outer = TRUE, adj = 0.5)
  
  # resets the plot layout to default i.e., single plot, not multi-panel, so its like dev.off() without "closing/clearing" the plot
  if(reset_layout) layout(1)
  
  # make these object outputs available outside of the function, to get the information when running the function
  # output <- plot_range(x)
  # output$POI
  return(list(data_section = data_section,
              POI = telemetry[POI, ])) # bad points
}


