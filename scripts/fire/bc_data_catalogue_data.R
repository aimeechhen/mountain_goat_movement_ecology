
# bc data catalogue
library(bcdata)

# https://www.for.gov.bc.ca/ftp/HTS/external/!publish/DataCatalogue_FAIB_Data/Burn_Severity_Historic_2023/

# historical fires
WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_INCIDENTS_SP <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_INCIDENTS_SP") %>% # search for data
  filter(FIRE_NUMBER == "K52125") %>% # filter based on fire id number
  collect() # download data

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-incident-locations-historical/resource/6db589c4-e45e-4ae9-a7b5-775cbfec6037")


#...........................................................
# perimeter area (polygons of the fire) ----

#polygon
WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP") %>% 
  # filter(FIRE_NUMBER == "K52125") %>% 
  filter(FIRE_YEAR == "2023") %>% 
  collect()


plot(WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP$geometry)

WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP <- st_transform(WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP, crs = "epsg:4326" )

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical/resource/c899578d-0738-4166-9b65-0588464f42ee")
# or ID "22c7cb44-1463-48f7-8e47-88857f207702"







#...........................................................
# Severity ----

# historical
WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP <- bcdc_query_geodata("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP") %>%
  collect()

WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP <- bcdc_query_geodata("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP") %>% 
  filter(FIRE_NUMBER == "K52125") %>% 
  collect()

# # historical are only for 2018-2021, need to refer to current/same year data for more recent
# test <- WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP[WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP$FIRE_NUMBER == "K52125",]
# test <- WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP[WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SP$FIRE_YEAR == "2023",]

# https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-historical
bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-historical/resource/714536b6-9957-42ca-8c3a-a3f853d807e6")

# https://www.for.gov.bc.ca/ftp/HTS/external/!publish/DataCatalogue_FAIB_Data/Burn_Severity_Historic_2023/pdfs/burn_severity_K52125.pdf

# same year
WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP <- bcdc_query_geodata("WHSE_FOREST_VEGETATION.VEG_BURN_SEVERITY_SAME_YR_SP") %>% 
  filter(FIRE_NUMBER == "K52125") %>% 
  collect()

bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-same-year")
# or "https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-same-year/resource/1ff6244d-cea0-4d66-a246-f523aa2efa91"
bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/fire-burn-severity-historical/resource/714536b6-9957-42ca-8c3a-a3f853d807e6")

#...........................................................
# Fuel types ----

WHSE_LAND_AND_NATURAL_RESOURCE.PROT_FUEL_TYPE_SP <- bcdc_query_geodata("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_FUEL_TYPE_SP") %>%
  collect()
bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-fuel-types-public/resource/a0d25dd1-e906-4b9f-994e-bf99098621d0")



