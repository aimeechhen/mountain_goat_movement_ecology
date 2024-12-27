

library(terra)
library(tictoc)
library(ggplot2)
source('scripts/source/foippa fire prep.r')


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

# # set folder path
# folder_path <- "./data/fire/time_rasters/"
# # create folder if it doesnt exist
# if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)

# i <- 5

for (i in 1:nrow(FOIPPA)) {
  # first create a raster
  # extract perimeter for the current row
  perimeter <- FOIPPA[i,]
  message("Currently on perimeter ", i, " of ", nrow(FOIPPA))
  # get the outline of the polygons
  outline <- st_boundary(perimeter)
  # raster_perimeter <- rasterize(vect(perimeter), raster_template)
  # convert geometries into vector object (i.e into a spatvector object) 
  outline_spatvector <- vect(outline)
  # extract the timestamp for the current perimeter
  capture_timestamp <- FOIPPA$timestamp[i]
  # assign the timestamp
  outline_spatvector$timestamp <- capture_timestamp
  # then convert spatvector into a spatraster object using the spatial properties outlined above and set background cells/pixels in the raster to 0 so the raster contains only values of 0 or 1 (0 = outside the polygon, 1 = inside the polygon) via field and background arguments
  r <- rasterize(outline_spatvector, raster_template, field = "timestamp", background = 0)
  
  #then create a matrix using the raster
  # Convert raster into a matrix, use wide = TRUE to make sure its in grid format i.e. 2D matrix
  m <- as.matrix(r, wide = TRUE)

  print(range(values(r))) # output is in Unix format
  print(range(as.vector(m)))  # output is in Unix format
  
  #visualise
  plot(r, main = paste("Raster -", capture_timestamp))
  image(m, main = paste("Matrix -", capture_timestamp)) 
  
  # store perimeter raster and matrices in a list
  raster_list[[i]] <- r
  matrix_list[[i]] <- m 
  # writeRaster(r, filename = file.path(folder_path, paste0("perimeter_", i, ".tif")), overwrite = TRUE)
  
}

# names(raster_list) <- FOIPPA$timestamp
names(matrix_list) <- FOIPPA$timestamp

# raster_perimeter <- intersect(vect(perimeter), raster_template)[[1]]

#check if the timestamp was properly entered
# cat("raster timestamp entered as", format(as.POSIXct(range(values(r)), origin = "1970-01-01", tz = "UTC")[2], "%Y-%m-%d %H:%M:%S"), "\n")
# # converting to standard format
# r_unix <- range(values(r), na.rm = TRUE)
# r_standard <- as.POSIXct(r_unix, origin = "1970-01-01", tz = "UTC")
# r_time <- r_standard[2]
#   m_unix <- range(as.vector(m), na.rm = TRUE)
# m_standard <- as.POSIXct(m_unix, origin = "1970-01-01", tz = "UTC")
# m_time <- m_standard[2]
# raster_list[[i]] <- r_time
# matrix_list[[i]] <- m_time 
unix_format <- range(values(r), na.rm = TRUE)
standard_format <- as.POSIXct(unix_format, origin = "1970-01-01", tz = "UTC")
arrival_time <- standard_format[2]


#save results as rda
# save(raster_list, file = "./data/fire/20241208_time_raster_list.rda")
saveRDS(raster_list, file = "./data/fire/20241208_time_raster_list.rds")
# save(matrix_list, file = "./data/fire/20240923_time_matrix_list.rda")

# extract(r, lns)

#_____________________________________________
# Conversion back to raster ----

# Set folder path where the files are located
folder_path <- "./data/fire/time_rasters/"
# Load .shp, .tif etc files within a folder including all the subfolders
rast_files <- list.files(path = folder_path, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
# Import/read all the files into a list
rast_list <- lapply(rast_files, rast)

# Loop through each raster in the list
for (i in seq_along(rast_list)) {
  # Convert the raster to a matrix
  m <- as.matrix(rast_list[[i]], wide = TRUE)
    # Store the matrix in the list
  matrix_list[[i]] <- m
}




# convert matrix into raster to test
load("./data/fire/20240923_time_matrix_list.rda")
r_list <- list()
i <- 7
for (i in 1:length(matrix_list)) {
  # extract a matrix from the list
  m <- matrix_list[[i]]
  # convert matrix back into a raster
  r <- rast(m)
  
  plot(r, main = paste("Raster -", names(matrix_list)[i])) 
  image(m, main = paste("Matrix -", names(matrix_list)[i])) 

  # store raster in the list
  r_list[[i]] <- r
}
