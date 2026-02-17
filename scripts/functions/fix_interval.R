fix_interval <- function(data, 
         fix_id,
         time_col = "timestamp") {
  
  # get row of the point from the data
  i <- which(data$fix_id == fix_id)
  if(length(i) == 0) stop("poi_id not found in data.")
  
  # calculate fix interval before and after poi
  time_to <- as.numeric(difftime(data$timestamp[i], data$timestamp[i - 1], units = "mins")) # before
  time_from <- as.numeric(difftime(data$timestamp[i + 1], data$timestamp[i], units = "mins")) # after
  
  # print output
  cat(paste0("fix interval - time to poi: ", round(time_to, 2), " min (", round(time_to/60, 2), " hours)\n"),
      paste0("fix interval - leaving poi: ", round(time_from, 2), " min (", round(time_from/60, 2), " hours)\n"))
}