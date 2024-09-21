

# nasa fire prep
library(sf)
# source('./scripts/nasa fire prep.r')


# import cropped nasa fire data
modis <- st_read("C:/Users/achhen/Desktop/nasa_fire/modis_cropped/modis_cropped.shp")
viirs_suomi <- st_read("C:/Users/achhen/Desktop/nasa_fire/viirs_suomi_cropped/viirs_suomi_cropped.shp")
viirs_noaa <- st_read("C:/Users/achhen/Desktop/nasa_fire/viirs_noaa_cropped/viirs_noaa_cropped.shp")



# format time from HHMM into to HH:MM:SS
modis$ACQ_TIME <- format(strptime(modis$ACQ_TIME, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
modis$timestamp <- as.POSIXct(paste(modis$ACQ_DATE, modis$ACQ_TIME), format = "%Y-%m-%d %H:%M:%S")
names(modis)[names(modis) == "ACQ_DATE"] <- "date"
names(modis)[names(modis) == "ACQ_TIME"] <- "time"


# format time from HHMM into to HH:MM:SS
viirs_suomi$ACQ_TIME <- format(strptime(viirs_suomi$ACQ_TIME, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
viirs_suomi$timestamp <- as.POSIXct(paste(viirs_suomi$ACQ_DATE, viirs_suomi$ACQ_TIME), format = "%Y-%m-%d %H:%M:%S")
names(viirs_suomi)[names(viirs_suomi) == "ACQ_DATE"] <- "date"
names(viirs_suomi)[names(viirs_suomi) == "ACQ_TIME"] <- "time"

# format time from HHMM into to HH:MM:SS
viirs_noaa$ACQ_TIME <- format(strptime(viirs_noaa$ACQ_TIME, format = "%H%M"), format = "%H:%M:%S")
# combine date and time into timestamp
viirs_noaa$timestamp <- as.POSIXct(paste(viirs_noaa$ACQ_DATE, viirs_noaa$ACQ_TIME), format = "%Y-%m-%d %H:%M:%S")
names(viirs_noaa)[names(viirs_noaa) == "ACQ_DATE"] <- "date"
names(viirs_noaa)[names(viirs_noaa) == "ACQ_TIME"] <- "time"