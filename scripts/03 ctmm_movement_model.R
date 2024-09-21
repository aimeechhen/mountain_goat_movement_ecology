

library(ctmm)
library(dplyr)
library(tictoc)

#import the data
collar_data <- read.csv('./data/collar_data/collar_data_basic_20240828.csv')

#format names to match required for ctmm based on Movebank critera:
collar_data = plyr::rename(collar_data, c('collar_id' = 'individual.local.identifier',
                                          'latitude' = 'location.lat', 
                                          'longitude' = 'location.long'))

#Convert to telemetry
goats <- as.telemetry(collar_data, mark.rm = TRUE)

#____________________________________________________________
# Fit movement models

START <- Sys.time()
tic()

FITS <- list()
for(i in 1:length(goats)){
  
  #Exctract individual
  DATA <- goats[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process
  
  # fit models
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)
  
}
#rename for convenience
names(FITS) <- names(goats)

toc()

save(FITS,file="data/ctmm/goat_fits.rda")
# load("data/ctmm/goat_fits.rda")




#...................................................................
# Estimate speed

START <- Sys.time()
tic()

SPEED <- list()
SPEEDS <- list()
for(i in 1:length(goats)){
  message("Currently on", i)
  #Extract individual
  DATA <- goats[[i]]
  FIT <- FITS[[i]]
  # estimate mean speed (may be a mix of OU and OUF, so using robust = true)
  SPEED[[i]] <- speed(FIT, DATA, robust = TRUE, units = FALSE, trace = TRUE)
  # estimate instantaneous speed
  SPEEDS[[i]] <- speed(FIT, DATA, robust = TRUE, units = FALSE, trace = TRUE)
  
}

#rename for convenience
names(SPEED) <- names(goats)
names(SPEEDS) <- names(goats)

toc()
END <- Sys.time()

saveRDS(SPEED, file = "./data/ctmm/goat_speed.rda")
saveRDS(SPEEDS, file = "./data/ctmm/goat_speeds.rda")

# turn list of list/df into a df


