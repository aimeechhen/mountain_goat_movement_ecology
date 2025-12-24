
# import cathedral/study area boundary
source("./scripts/environment/cathedral.R")

library(bcdata)
# https://catalogue.data.gov.bc.ca/

bec <- bcdc_query_geodata("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY") %>% 
  collect() 

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bec-map/resource/46ceb84a-3f6d-436c-b4c1-c89beb72d11a")
bcdc_get_citation("f358a53b-ffde-4830-a325-a5a03ff672c3")
bec_crop <- st_crop(bec, st_bbox(st_transform(full_study_area_sf, st_crs(bec))))

names(bec_crop)
plot(bec_crop["ZONE"])
plot(bec_crop["ZONE_NAME"])
plot(bec_crop["MAP_LABEL"])
bec_hot <- bec_crop[grepl("Hot", bec_crop$SUBZONE_NAME), ]

cathedral_sf <- st_transform(cathedral_sf, st_crs(bec))
plot(cathedral_sf)
plot(test, add = TRUE)
