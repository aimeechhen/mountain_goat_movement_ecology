
# Fire data

# nasa fire data ----
source('./scripts/source/nasa fire prep.r')

# combine all 3 nasa satellite instruments into one df
firms <- rbind(modis, viirs_suomi)
firms <- rbind(firms, viirs_noaa)

# goes fire data ----
source('./scripts/source/goes fire prep.r')

# foippa fire data ----
source('scripts/foippa fire prep.r')


