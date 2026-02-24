

# fire data

library(sf)
library(lubridate)
library(dplyr)
library(ggplot2)

# Crater Creek wildfire background information
# https://wildfiresituation.nrs.gov.bc.ca/incidents?fireYear=2023&incidentNumber=K52125
# Gillanders Creek
# https://wildfiresituation.nrs.gov.bc.ca/incidents?fireYear=2023&incidentNumber=K51680


#crater creek wildfire merged with Gillanders Creek fire...change in weather (increased wind)
#Tuesday morning (aug 15) crater creek fire = 697 hectares;  Gillanders Creek fire = 480 hectares. 
# Wednesday afternoon (aug 16) = estimated 1,100 hectares to 10,000
# after merging referred to crater creek fire henceforth https://globalnews.ca/news/9899992/keremeos-b-c-area-fires-merge-10000-hectares/
# major smoke!
# people were trapped at the cathedral lake lodge (where goats are found a lot! refer to map and that little square) https://www.cbc.ca/news/canada/british-columbia/cathedral-lakes-lodge-crater-creek-fire-stranding-1.6938375

# grew almost tenfold overnight from approximately 12 square kilometres on Tuesday evening to 100 square kilometres Wednesday morning. It is now classified as a "wildfire of note," meaning the blaze is highly visible or poses a potential threat to public safety. https://www.cbc.ca/news/canada/british-columbia/cathedral-lakes-lodge-crater-creek-fire-stranding-1.6938375
# placed under evacuation order just five hours after being put on evacuation alert.... says they were unable to leave due to the wildfire blocking the only access road connecting the guesthouse to Ashnola River Road, which leads to Keremeos.
# Bojahra noted there were fires and rockfalls reported on Ashnola River Road on Tuesday evening that the wildfire service deemed too dangerous for its crews to pass through to help evacuate people from the lodge.
# But Chief Keith Crow of the Lower Similkameen Indian Band, with some parts of its reserves under evacuation order, expressed frustration at the decision to leave people stranded in Cathedral Lakes Lodge for nearly 12 hours, citing a similar situation had happened in 2018 due to the Placer Mountain wildfire.


#....................................................................
# foippa data ----
#....................................................................

# fire boundaries were requested and obtained from Freedom of Information and Protection of Privacy Act (FOIPPA)
# https://www2.gov.bc.ca/gov/content/governments/about-the-bc-government/open-government/open-information/freedom-of-information
# submission request https://citz-foi-prod.objectstore.gov.bc.ca/openinfopub/packages/FOR-2024-41389/openinfo/Response_Letter_FOR-2024-41389.pdf
# the data is available to the public here https://www2.gov.bc.ca/enSearch/detail?id=7AFDBC16F15F42E289E9F7DDB0F80C40&recorduid=FOR-2024-41389
# bc gov made it public on Publication Date: 2026-02-05

# Clinton Galloway, the analyst assigned to your request, at 778 974-2251. This number can also be reached toll-free at 1 833 283-8200. Please provide the FOI request number, FOR-2024-41389, in any communications
# Clinton.Galloway@gov.bc.ca
#*****contact foippa dude to see if we can get more boundaries from him?!?!*****

# Source = 0 – unknown, 1 – hand sketch of any type, 3 - non-corrected airborne, 7 –
# Processed IR image , 9 – Derived from satellite imagery

foippa <- st_read('./data/fire/foippa/crater_boundaries/23 K52125 Perimeter History Jun17.shp')
# refer to the "./data/fire/foippa/crater_boundaries/23 K52125 Perimeter History Jun17.shp.xml" file for metadata
str(foippa)
st_crs(foippa) # bc albers

# based on the metadata, assuming ground time = local time therefore bc time
#   <caldate>REQUIRED: The year (and optionally month, or month and day) for which the data set corresponds to the ground.</caldate>

# reformat the time structure into hours and minutes
foippa$CaptureTim <- format(strptime(foippa$CaptureTim, format = "%H%M"), format = "%H:%M:%S")
# combine into date_time but has no timezone
foippa$date_time <- paste(foippa$CaptureDat, foippa$CaptureTim)
# convert into proper timestamp with timezone
foippa$timestamp_local <- as.POSIXct(foippa$date_time, format="%Y-%m-%d %H:%M:%S", tz="America/Vancouver")
tz(foippa$timestamp_local)
# drop the column to avoid confusion
foippa <- subset(foippa, select = -date_time)
#create new columns for convenience purposes
foippa$date_local <- as.Date(foippa$timestamp_local)
foippa$time_local <- format(foippa$timestamp_local, format = "%H:%M:%S")
foippa$data_source <- "foippa"

# save(foippa, file = "./data/fire/foippa.rda")
load("./data/fire/foippa.rda")



#....................................................................
# cwfis data ----
#....................................................................

# fire perimeters were obtained from Canadian Wildland Fire Information System (CWFIS)
# download files: https://cwfis.cfs.nrcan.gc.ca/downloads/hotspots/archive/2023_progression.zip
# https://cwfis.cfs.nrcan.gc.ca/downloads/hotspots/archive/ for the perimeters
# 2023_progression

#*****how to get data from the interactive map? there are daily boundaries on the map when flipping through!!****

cwfis <- st_read("./data/fire/cwfis/2023_progression/m3_progression_2023.shp")
cwfis
str(cwfis)
st_crs(cwfis) # not bc albers
# transform to bc albers crs
cwfis <- bcmaps::transform_bc_albers(cwfis)

# # reformat date structure, because its canadian fire, going to assume its local
cwfis$date <- as.Date(as.character(cwfis$date), format = "%Y%m%d")
# no time zone because no time is provided, only date, so cannot assign tz

# subset the fire perimeters that fall within the crater creek period
cwfis <- cwfis[cwfis$date >= "2023-07-22" & cwfis$date <= "2023-10-26", ] # 7327 fires


load("./data/goat/goat_data_sf.rda")

# get the extent based on bounding box
ext <- st_as_sfc(st_bbox(goat_data_sf))
# add a buffer making the bbox larger
study_bbox <- st_buffer(ext, dist = 20000) 
# subset any fire perimeters that fall occurs in the study area, even if its partial
cwfis <- st_filter(cwfis, study_bbox) # 20k = 42, 15k = 39, 10k = 35, 5k = 12 perimeters
# this contains two fires, crater creek and the Gillanders Creek fire since they merged and 2 became 1


plot(st_geometry(study_bbox))
plot(st_geometry(cwfis), add = TRUE)
plot(st_geometry(goat_data_sf), add = TRUE)

cwfis$data_source <- "cwfis"

save(cwfis, file = './data/fire/cwfis.rda')
load('./data/fire/cwfis.rda')



#.......................................................................
# combine data

# check for same column names to have column names in common and avoid duplication
colnames(foippa)
colnames(cwfis)

cwfis <- dplyr::rename(cwfis, date_local = date)
intersect(colnames(foippa), colnames(cwfis))

# combine datasets
fire_combined <- dplyr::bind_rows(foippa, cwfis)
#  check for empty geometries
which(st_is_empty(fire_combined)) 
# order by date
fire_combined <- fire_combined[order(fire_combined$date_local),]

str(fire_combined)

# drop columns that are redundant or not necessary
fire_combined <- subset(fire_combined, select = -c(Fire_Num, Fire_Name, Label, CaptureDat, CaptureTim,
                                 timestamp_local, time_local, clusterid, consis_id))

# check for duplicated perimeters
fire_combined[duplicated(st_geometry(fire_combined)),]


save(fire_combined, file = "./data/fire/fire_combined.rda")


#................................................
# fire by date


load("./data/fire/fire_combined.rda")


# merge the geometries based on same dates, other columns are not necessary because some have time but most do not or show as 00:00:00
fire_by_date <- aggregate(fire_combined["geometry"], by = list(date = fire_combined$date_local), FUN = st_union)
str(fire_by_date)
tz(fire_by_date$date)
# assume your goat timestamps are local



# save(fire_by_date, file =  "./data/fire/fire_perimeter_by_date.rda")
load("./data/fire/fire_perimeter_by_date.rda")
