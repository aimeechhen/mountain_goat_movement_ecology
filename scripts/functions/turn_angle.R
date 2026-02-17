

turn_angle <- function(data,
                  fix_id) {
  
  # get row of the point from the data
  i <- which(data$fix_id == fix_id)
  if(length(i) == 0) stop("poi_id not found in data.")
  if(i == 1 || i == nrow(data)) return("missing point to calculate angle") #if point happens to be in the first or last row of the df
  
  # get coordinates for before, current, after points (corner of the triangle)
  # before
  x0 <- data$x[i - 1] # one row behind
  y0 <- data$y[i - 1]
  # point of interest
  x1 <- data$x[i]
  y1 <- data$y[i]
  # after
  x2 <- data$x[i + 1] # one row ahead
  y2 <- data$y[i + 1]
  
  # get the the sides of the triangle i.e., calculate the distances between points
  side_01 <- sqrt((x1 - x0)^2 + (y1 - y0)^2) # between pt 0 and 1 -> side A
  side_12 <- sqrt((x2 - x1)^2 + (y2 - y1)^2) # between pt 1 and 2 -> side B
  side_02 <- sqrt((x2 - x0)^2 + (y2 - y0)^2) # between pt 0 and 2 -> side C
  
  # get angles, get your geometry on, throw back to law of cosines
  # cos(theta) = (AB^2 + BC^2 - AC^2) / (2 * AB * BC)
  cos_angle <- (side_01^2 + side_12^2 - side_02^2) / (2 * side_01 * side_12)
  
  # convert rad into degrees
  angle_deg <- (pi - acos(cos_angle)) * (180 / pi)
  
  # get output
  return(paste0(round(angle_deg, 2), " degrees"))
}
