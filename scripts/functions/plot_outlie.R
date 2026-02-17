
library(ctmm)
library(paletteer)


# Based on Noonan et al 2022 Appendix S2 
# https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf

# added a 4th panel of movement pathway on a map

# Plot 4 panels to check data for outliers via visualization (give it a minute to execute)

# Plot panel description:
#1: telemetry gps points
#2: location and velocities, from the outlie() function
#3: outlie() output of distance and speed, note: "out <- outlie(telemetry, units = FALSE)", 'out' is the output of outlier() within the custom function, plotting that output
#4: telemetry path on top of a map

# Additional information:
# output of outlie()

# distance column = `core deviation' denotes distances from the median longitude & latitude
# the speed column = `minimum speed' denotes the minimum speed required to explain the location estimate's displacement as straight-line motion
# NOTE: The speed estimates here are tailored for outlier detection and have poor statistical efficiency.

#............................................................................
# outlie plots  ----
#............................................................................

# 'telemetry' = the ctmm telemetry data of one individual

plot_outlie <- function(telemetry, 
                        units = FALSE, 
                        return = TRUE,  # If TRUE, return the outlier check results instead of just plotting
                        reset_layout = TRUE) {  # If TRUE, reset plot layout to a single plot
  
  # set up the plot panels
  layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)) # set up multi-panel layout 
  par(oma = c(0, 0, 1.5, 0))  # allow a main plot title be added, set outer top margin (lines)
  
  # plot telemetry data (need to indicate ctmm package when plotting or will get warnings because its using base r plot() function)
  ctmm::plot(telemetry, units = FALSE)
  title(main = "telemetry points")
  
  # plot of locations and velocities
  out <- outlie(telemetry, units = FALSE, plot = TRUE)
  title(main = "location & velocities")
  
  # min speed vs distance, inspect plot units, units = FALSE to have units plotted as m/s instead of cm/s and m
  ctmm::plot(out, units = FALSE)
  title(main = "speed vs distance")
  
  # plot telemetry path without map, uncomment this section and comment out on map section if preferred
  # plot(telemetry$x, telemetry$y, 
  #      type = 'l', xlab = 'x (m)', ylab = 'y (m)')
  # title(main = "telemetry path")
  
  # plot telemetry path on map
  coords <- st_coordinates(st_transform(as.sf(telemetry),  "EPSG:3857")) # convert to sf object, transform to 3857 crs, extract coordinates
  basemaps::basemap_plot(ext = st_transform(st_bbox(c(xmin = min(telemetry$longitude) - 0.01,
                                            ymin = min(telemetry$latitude) - 0.01,
                                            xmax = max(telemetry$longitude) + 0.01,
                                            ymax = max(telemetry$latitude) + 0.01), crs = 4326), "EPSG:3857"),
               map_service = "esri",
               map_type = "world_imagery")
  # add the path
  lines(coords[,1], coords[,2], col = "red", lwd = 2)
  title(main = "telemetry path")
  
  mtext(paste0("ID: ", unique(telemetry$individual.local.identifier)), 
        outer = TRUE, adj = 0.5) # title adj = 0.01 left align, on the outside of the plot, adj = 0.5 is middle
  
  if(reset_layout) layout(1)   # resets the plot layout to default i.e., single plot, not multi-panel, so its like dev.off() without "closing/clearing" the plot
  
  if(return) return(out)
}
