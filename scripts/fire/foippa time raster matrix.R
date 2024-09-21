

library(terra)
library(tictoc)
library(ggplot2)
source('scripts/foippa fire prep.r')


ggplot() +
  geom_sf(data = FOIPPA, aes(color = date), fill = NA, lwd = 0.5) +
  facet_wrap(~ date) +
  labs(title = "FOIPPA Fire Polygons for Crater Creek",
       x = "Longitude", y = "Latitude")

#crater creek wildfire merged with Gillanders Creek fire...change in weather (incresed wind)
#Tuesday morning (aug 15) crater creek fire = 697 hectares;  Gillanders Creek fire = 480 hectares. 
# Wednesday afternoon (aug 16) = estimated 1,100 hectares to 10,000

# Remove perimeter that has a same date as another (what does source mean?)
# FOIPPA <- FOIPPA[!FOIPPA$Source == 9,]

# convert crs into a meter measurement projection
FOIPPA <- st_transform(FOIPPA, crs = "EPSG:32610")

# set spatial properties of the raster by defining the extent and crs as a template for the raster
ext_value <- ext(st_bbox(FOIPPA))
crs_value <- "EPSG:32610"
res_value <- 25 # 25m resolution based on UTM projection

# create a template for the spatial properties of the raster   
raster_template = rast(ext(ext_value),
                       crs = crs_value,
                       res = res_value)

# create a list to store objects
raster_list <- list()
matrix_list <- list()

for (i in 1:nrow(FOIPPA)) {
  # extract perimeter for the current row
  perimeter <- FOIPPA[i,]
  message("Currently on perimeter ", i, " of ", nrow(FOIPPA))
  # get the outline of the polygons
  outline <- st_boundary(perimeter)
  # raster_perimeter <- rasterize(vect(perimeter), raster_template)
  # convert geometries into vector object (i.e into a spatvector object) 
  lns <- vect(outline)
  # extract the timestamp for the current perimeter
  capture_timestamp <- FOIPPA$timestamp[i]
  # assign the timestamp
  lns$timestamp <- capture_timestamp
  # then convert spatvector into a spatraster object using the spatial properties outlined above and set background cells/pixels in the raster to 0 so the raster contains only values of 0 or 1 (0 = outside the polygon, 1 = inside the polygon) via field and background arguments
  r <- rasterize(lns, raster_template, field = "timestamp", background = 0)
  # Convert raster into a matrix, use wide = TRUE to make sure its in grid format i.e. 2D matrix
  m <- as.matrix(r, wide = TRUE)
  #check if the timestamp was properly entered
  # cat("raster timestamp entered as", format(as.POSIXct(range(values(r)), origin = "1970-01-01", tz = "UTC")[2], "%Y-%m-%d %H:%M:%S"), "\n")
  print(range(values(r))) # output is in Unix format
  print(range(as.vector(m)))  # output is in Unix format
  # store perimeter raster in the list
  raster_list[[i]] <- r
  matrix_list[[i]] <- m 
  #visualise
  plot(r, main = paste(capture_timestamp))
  image(m, main = paste(capture_timestamp))
  
}

#save results
save(raster_list, file = "./data/fire/time_raster_list.rda")
save(matrix_list, file = "./data/fire/time_matrix_list.rda")

# extract(r, lns)


