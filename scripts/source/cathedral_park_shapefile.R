
# Cathedral Park shapefile

library(sf)
# source('./scripts/cathedral_park_shapefile.r')

# Import shapefile
bc_parks <- st_read("data/rasters/bc_provincial_parks/TA_PARK_ECORES_PA_SVW/TA_PEP_SVW_polygon.shp")
# subset to CPP
cathedral <- bc_parks[bc_parks$PROT_NAME == "CATHEDRAL PARK", ] #row 95

# clean up environment so these objects arent added when being source() in
rm(bc_parks)