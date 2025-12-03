
library(ctmm)

#............................................................................
# outlie plots  ----
#............................................................................

# based on the concept of Stefano Mezzini's outlier_plots.R script (https://github.com/QuantitativeEcologyLab/bc-mammals-temperature/blob/main/functions/outlier_plots.R)

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
  
  # plot telemetry path 
  plot(telemetry$x, telemetry$y, 
       type = 'l', xlab = 'x (m)', ylab = 'y (m)')
  title(main = "telemetry path")
  
  mtext(paste0("ID: ", unique(telemetry$individual.local.identifier)), 
        outer = TRUE, adj = 0.5) # title adj = 0.01 left align, on the outside of the plot, adj = 0.5 is middle
  
  if(reset_layout) layout(1)   # resets the plot layout to default i.e., single plot, not multi-panel, so its like dev.off() without "closing/clearing" the plot
  
  if(return) return(out)
}
