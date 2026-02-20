
# movement models and home range estimation

library(ctmm)
library(tictoc)
library(dplyr)
library(beepr)
library(crayon)

#...........................................................
# Import data ----
#...........................................................

load(file = "./data/goat/study_period_tel_data.rda")

# summary of the gps data, (i.e., interval, period, long, lat info), check the period to ensure that its the study period length
summary(tel_data) # all 6.25h, 3.27-3.28 months

# visualisation of the data
plot(tel_data)

#...........................................................
# 1. Movement models ----
#...........................................................

#create an empty list to store output
FITS <- list()

START_movement <- Sys.time()
tic(msg = "movement models")

for(i in 1:length(tel_data)){
  message(bgWhite(green("Currently on animal ", i, " of ", length(tel_data))))
  #Extract individual
  DATA <- tel_data[[i]]
  
  # create guesstimate non-interactively
  GUESS <- ctmm.guess(DATA,CTMM=ctmm(error=FALSE),interactive=FALSE) # Error is off for now to speed up the process, error = true will help avoid iid models and push towards ou and ouf models, keeping error = FALSE because not using any error models
  # fit movement models and select best fit
  FITS[[i]] <- ctmm.select(DATA, GUESS, trace = 3, cores=-1)

}

#rename for convenience
names(FITS) <- names(tel_data)

toc() #~1.46h, #9.698333 mins; full = ~12.5min, 
# combined: error on = 6.37h, error off = 12.8min
END_movement <- Sys.time()



# dir.create("./data/movement/", recursive = TRUE, showWarnings = TRUE)
save(FITS,file="data/movement/fits.rda")
# load("./data/movement/fits.rda")


