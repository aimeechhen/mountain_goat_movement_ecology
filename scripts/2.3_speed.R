
library(ctmm)

#...........................................................
# 2. Speed analysis ----
#...........................................................

load("./data/goat/study_period_tel_data.rda")
load("./data/movement/fits.rda")

SPEED_MEAN <- list()
SPEEDS_INSTA <- list()

START_speed <- Sys.time()
tic(msg = "speed analyses")

for(i in 1:length(tel_data)){
  tic(msg = "one goat")
  message("Currently on animal ", i, " of ", length(tel_data))
  #Extract individual telemetry and movement model
  DATA <- tel_data[[i]]
  fits <- FITS[[i]]
  
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true), error = TRUE caused issues with no converging
  SPEED_MEAN[[i]] <- speed(object = DATA, CTMM = fits,
                           robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  # estimate instantaneous speed -> Error in as.POSIXlt.POSIXct(x, tz) : invalid 'tz' value
  SPEEDS_INSTA[[i]] <- speeds(object = DATA, CTMM = fits,
                              robust = TRUE, units = FALSE, trace = TRUE, cores = -1)
  toc()
  
}

#rename for convenience
names(SPEED_MEAN) <- names(tel_data)
names(SPEEDS_INSTA) <- names(tel_data)

toc() #13.9 mins, ~26 min, combined data = 48 min, combined without error = 59min
END_speed <- Sys.time()

# Warning messages:
#   1: In speed.ctmm(CTMM, data = object, t = t, level = level,  ... :
#                      Movement model is fractal.

save(SPEED_MEAN, file = "./data/movement_model/speed_mean.rda")
save(SPEEDS_INSTA, file = "./data/movement_model/speeds_insta.rda")
# load("./data/movement_model/speed_mean.rda")
# load("./data/movement_model/speeds_insta.rda")



#................................................
# determine window size

load("./scripts/working/data/movement_model/fits.rda")
summary(FITS)
