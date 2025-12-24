
# based on Stefano's code of plot_adj script (https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/functions/plot_adj.R)

# take a closer look, function to plot points near potential flagged points for further inspection
# values are set to null to allow for parameters not need to be included
plot_adj <- function(telemetry, 
                     xmin = NULL, xmax = NULL,
                     ymin = NULL, ymax = NULL,
                     speed_min = NULL, speed_max = NULL,
                     distance_min = NULL, distance_max = NULL,
                     angle_min = NULL, angle_max = NULL,
                     buffer = 0,
                     reset_layout = TRUE) {
  
  
  
  # Create vector to track subset rows of telemetry data of a single individual
  subset_data <- rep(TRUE, nrow(telemetry)) 
  out <- outlie(telemetry)
  falses <- rep(TRUE, nrow(out)) 
  
  # location
  if (!is.null(xmin)) subset_data <- subset_data & telemetry$x >= xmin
  if (!is.null(xmax)) subset_data <- subset_data & telemetry$x <= xmax
  if (!is.null(ymin)) subset_data <- subset_data & telemetry$y >= ymin
  if (!is.null(ymax)) subset_data <- subset_data & telemetry$y <= ymax
  
  # speed
  if (!is.null(speed_min)) falses <- falses & out$speed >= speed_min
  if (!is.null(speed_max)) falses <- falses & out$speed <= speed_max
  
  # distance
  if (!is.null(distance_min)) falses <- falses & out$distance >= distance_min
  if (!is.null(distance_max)) falses <- falses & out$distance <= distance_max
  
  # turn angle
  if (!is.null(angle_min)) falses <- falses & out$angle >= angle_min
  if (!is.null(angle_max)) falses <- falses & out$angle <= angle_max
  
  # Find the points (rows) that match the conditions
  identified_points <- which(c(subset_data & falses))  
  # find the adjacent points 
  buffered_points <- unique(unlist(lapply(identified_points, function(i) (i - buffer):(i + buffer)))) # get rows around the identified points
  buffered_points <- buffered_points[buffered_points >= 1 & buffered_points <= nrow(telemetry)] # drop the rows that are not around the identified points
  
  # subset the data based on the buffered i.e., the adjacent points
  adj_points <- telemetry[buffered_points, ]
  
  # plot the identified points
  layout(matrix(1:3, nrow = 1, ncol = 3, byrow = TRUE)) # set up multi-panel layout plots
  par(oma = c(0, 0, 3, 0))  # allow a main plot title be added
  
  # plot telemetry data
  plot(adj_points, units = FALSE)
  title(main = "Plot 1: telemetry data")
  # plot line connecting the telemetry points with errors
  # plot(adj_points, units = FALSE, error = FALSE, col = 'black',
  #      type = 'l')
  # plot(adj_points, add = TRUE, units = FALSE, col = 'black', transparency.error = 0)
  # title(main = "Plot 2: locations with DOP")
  adj_out <- outlie(adj_points, units = FALSE, plot = TRUE)
  title(main = "Plot 2: outlie output")
  
  # plot based on coordinates, set up an empty plot to fill in
  plot(adj_points$longitude, adj_points$latitude, type = 'n',
       main = 'Plot 3: locations colored by set of points',
       xlab = 'Longitude', ylab = 'Latitude')
  # draw each buffered segment in a different color
  for (i in identified_points) {
    pts <- (i - buffer):(i + buffer)
    pts <- pts[pts >= 1 & pts <= nrow(telemetry)]
    subset_pts <- telemetry[pts, ]
    
    # assign a color (you can use rainbow or something fancier)
    col <- rainbow(length(identified_points))[which(identified_points == i)]
    
    lines(subset_pts$longitude, subset_pts$latitude, col = col, lwd = 2)
    points(subset_pts$longitude, subset_pts$latitude, col = col, pch = 20)
  }
  
  # add a title to the multipanel plot
  mtext(paste0("ID: ", unique(telemetry$collar_id)), outer = TRUE, adj = 0.01) # title left align, on the outside of the plot
  
   # resets the plot layout to default i.e., single plot, not multi-panel, so its like dev.off() without "closing/clearing" the plot
  if(reset_layout) layout(1)
  
  # make these object outputs available outside of the function, to get the information when running the function
  # output <- plot_adj(x)
  # output$identified_points
  return(list(adj_points = adj_points,
              bad_point = telemetry[subset_data, ]))
}

