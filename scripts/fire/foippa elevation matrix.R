

# get elevation values for the study area and convert them into matrix form
# based on the spatial properties of the 'foippa time raster matrix.r'

library(terra)

source('scripts/source/foippa fire prep.r')

#...............................................................

# i want a matrix of the elevation values
# they need to be based on the spatial properties of the time matrices
# the time matrices were based on the raster template
# .-. i need to use the raster template as the spatial properties for each perimeter entry

elev_25m <- rast("./data/rasters/elev_25m.tif")

#spatial properties from time matrix
crs_value <- "EPSG:32610"
elev_25m <- project(elev_25m, crs_value)
FOIPPA <- st_transform(FOIPPA, crs_value)

ext_value <- ext(st_bbox(FOIPPA))


# crop
el <- crop(elev_25m, ext_value)
plot(el, main = 'elevation raster')


#matrix

# Extract elevation raster values and convert to a 2D matrix
x_coords <- xFromCol(el, 1:ncol(el))  
y_coords <- yFromRow(el, 1:nrow(el))  

m <- matrix(values(el), 
            nrow = nrow(el), 
            ncol = ncol(el), 
            byrow = TRUE)  # Reshape raster values into a 2D matrix

image(m, main = "elevation matrix")

save(m, file = "./data/fire/20241210_elevation_matrix.rda")
