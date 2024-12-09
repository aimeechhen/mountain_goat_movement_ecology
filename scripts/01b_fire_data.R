
# Fire data

# nasa fire data ----
source('./scripts/source/nasa fire prep.r')

# combine all 3 nasa satellite instruments into one df
nasa_fire <- rbind(modis, viirs_suomi)
nasa_fire <- rbind(nasa_fire, viirs_noaa)

# goes fire data ----
source('./scripts/source/goes fire prep.r')

# foippa fire data ----
source('scripts/foippa fire prep.r')


