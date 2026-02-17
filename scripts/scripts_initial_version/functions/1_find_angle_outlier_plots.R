
#...............................................................
# turn angle calculations ----
#...............................................................

# Stefano wrote the turn angle function code source: https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/functions/find_angle.R


# function to calculate angle between three points
find_angle <- function(x, y, radians = FALSE) {
  if(length(x) != length((y))) stop('x and y must be the same length.\n')
  
  # coordinates for current location
  x_1 <- x
  y_1 <- y
  
  # coordinates for previous location
  x_0 <- c(NA, x[-length(x)])
  y_0 <- c(NA, y[-length(y)])
  
  # coordinates for next location
  x_2 <- c(x[-1], NA)
  y_2 <- c(y[-1], NA)
  
  # return NA if any coordinates are missing
  l_01 <- sqrt((x_0 - x_1)^2 + (y_0 - y_1)^2) # segm. to current location
  l_12 <- sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2) # segm. from current location
  l_02 <- sqrt((x_0 - x_2)^2 + (y_0 - y_2)^2) # distance between endpoints
  
  # calculate the angle between segments (in radians) using law of cosines
  # https://stackoverflow.com/questions/1211212/how-to-calculate-an-angle-from-three-points
  # https://www.mathsisfun.com/algebra/trig-cosine-law.html
  angle <- acos((l_01^2 + l_12^2 - l_02^2) / (2 * l_01 * l_12))
  
  # calculate turning angle (in radians)
  angle <- pi - angle
  
  if(radians) {
    message('Returned angles in radians.')
    return(angle)
  } else {
    message('Returned angles in degrees.')
    return(angle / pi * 180)
  }
}


#...............................................................
# test the function ----
#...............................................................

if(FALSE) {
  x <- c(0, 0, 1, 0.5)
  y <- c(1, 0, 0, 0.5)
  plot(x, y, type = 'b', pch = as.character(1:length(x)))
  find_angle(x, y)
  
  # random angles
  x <- runif(8)
  y <- runif(8)
  plot(y ~ x, type = 'b', pch = as.character(1:length(x)))
  find_angle(x = x, y = y)
  
  # estimated distribution of all possible angles
  hist(find_angle(runif(1e6), runif(1e6)), breaks = 250)
}



#.....................................................................................
# outlier plots  ----
#.....................................................................................

# note this has been modified from the original script by Stefano (https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/functions/outlier_plots.R)
# 'telemetry' = the telemetry data of one individual

outlier_plots <- function(telemetry, 
                          units = FALSE, 
                          # ci_level = 0,
                          return = TRUE,  # If TRUE, return the outlier check results instead of just plotting
                          reset_layout = TRUE,  # If TRUE, reset plotting layout to a single plot
                          time_cap_dt = TRUE) {  # If TRUE, cap time-between-fixes at 24 hours for cleaner plots
  # layout(matrix(c(1, 1, 2:5), ncol = 2, byrow = TRUE))
  layout(matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE)) # set up multi-panel layout plots
  par(oma = c(0, 0, 3, 0))  # allow a main plot title be added, # top margin = 3 lines
  
  # plot telemetry data
  plot(telemetry, units = FALSE)
  title(main = "Plot 1")
  
  # plot line connecting the telemetry points 
  plot(telemetry$x, telemetry$y, type = 'l', xlab = 'x (m)',
       ylab = 'y (m)')
  title(main = "Plot 2")
  
  # plot of locations and velocities
  out <- outlie(telemetry, units = FALSE)
  title(main = "Plot 3")
  
  # min speed vs distance, inspect plot units, units = FALSE to have units plotted as m/s instead of cm/s and m
  plot(out, units = FALSE)
  title(main = "Plot 4")
  
  # calculate turning angles in degrees using the function above
  out$angle <- find_angle(telemetry$x, telemetry$y, radians = FALSE)
  # calculate time intervals
  out$dt <- out$t - lag(out$t)
#plot turn angles and speed
  plot(out$angle, out$speed, pch = 19, col = '#00000050', xaxt = 'n',
       xlim = c(0, 180), xlab = 'Turning angle (degrees)',
       ylab = 'Minimum speed (m/s)')
  title(main = "Plot 5")
  axis(side = 1, at = seq(0, 180, by = 45))

  #plot time between fixes, i.e, time interval plot, time between fixes
  # SLD is often not useful because dt is too often clustered (dt is the time interval between fixes)
  if(time_cap_dt) {
    XLIM <- c(0, min(max(out$dt / (1 %#% 'hour'), na.rm = TRUE), 24)) # convert time to hours using ctmm package conversion (i.e., %#%)
    #x axis = time and sets the values to range between 0 to 24hr
    XLAB <- if_else(XLIM[2] == 24 & time_cap_dt,
                    'Time between locations (hours, capped at 24)',
                    'Time between locations (hours)')
  } else {
    XLIM <- c(0, max(out$dt / (1 %#% 'hour'), na.rm = TRUE))
    XLAB <- 'Time between locations (hours)'
  }
  # plot time intervals between points within a day (if point B was missed, so then the interval will be longer because now the interval is point A to C)
  plot(out$dt / (1 %#% 'hours'), out$speed, pch = 19, col = '#00000050',
       xlab = XLAB,
       ylab = 'Minimum speed (m/s)',
       xlim = XLIM)
  title(main = "Plot 6")
  
  mtext(paste0("ID: ", unique(telemetry$collar_id)), outer = TRUE, adj = 0.01) # title left align, on the outside of the plot
  if(reset_layout) layout(1)   # resets the plot layout to default i.e., single plot, not multi-panel, so its like dev.off() without "closing/clearing" the plot
  if(return) return(out)
}


# # add title for entire plot (only works if you modify it within the function and not outside, need to adapt it accordingly, to be completed)
# mtext(paste0("ID: ", names(tel_data)[1]), outer = TRUE, adj = 0.01) # title left align, on the outside of the plot