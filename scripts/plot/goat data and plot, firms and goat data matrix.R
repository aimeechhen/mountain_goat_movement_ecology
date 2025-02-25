
# Committee Meeting 2025-01-10 
# 4 things needed for Dr. Thompson
# 1. goat data
# 2. plot of goat data, coloured by goat
# 3. goat data in matrix form (save image)
# 4. FIRMS fire points in matrix form (save image)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. goat data ----
# need coordinates, timestamps, goat id for fire goats

load("data/collar_data/collar_data_20241123.rda")

# collar data for wildfire aka the fire goats
# identify the goats that were tracked during the wildfire
goats <- c("kid_rock", "toats_mcgoats", "goatzilla", "the_goatmother", "vincent_van_goat", "rocky")
# subset to fire goats
fire_goats <- collar_data[collar_data$goat_name %in% goats,] # 43941 obs

# Define the Crater Creek wildfire dates
fire_start <- '2023-09-22' # doy = 203
fire_end <- '2023-10-26' # doy = 299
#subset goat data based on the Crater Creek wildfire dates
fire_goats <- fire_goats[fire_goats$date >= fire_start & fire_goats$date <= fire_end, ] #10376 obs

# subset dataframe to coordinates, timestamps, goat id
dat <- fire_goats[,c(2,4:6)]

# save dataset
save(dat, file = "./data/collar_data/fire_goats_gps_data_only.rda")


#________________________________________________________________________________

# 2.  plot of goat data, coloured by goat
library(ggplot2)

ggplot() +
  geom_point(data = dat, aes(x = longitude, y = latitude, color = goat_name)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = -0.05, size = 10, family = "sans"),
        axis.title.y = element_text(size = 10, family = "sans", face = "bold"),
        axis.title.x = element_text(size = 10, family = "sans", face = "bold"),
        axis.text.y = element_text(size = 8, family = "sans"),
        axis.text.x  = element_text(size = 8, family = "sans"),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  guides(color = guide_legend(nrow = 1)) # put legend in one row

ggsave(last_plot(), file = "./figures/fire_goats_collar_data.png",
       width = 6.86, height = 6, units = "in", dpi = 600, bg = "transparent")


#________________________________________________________________________________

# 3. goat data in matrix form (save image)

library(sf)
library(terra)
library(dplyr)

# set spatial properties of the raster by defining the extent and crs as a template for the raster
# extent based on the original matrix of using the FOIPPA data
ext_value <- ext(703241.583763359, 732999.398617109, 5426226.65103667, 5459749.20251924)
crs_value <- "EPSG:32610"
res_value <- 25 # 25m resolution based on UTM projection

# create a template for the spatial properties of the raster   
raster_template = rast(ext(ext_value),
                       crs = crs_value,
                       res = res_value)

# convert to sf object and keep the lat/long columns in the sf object, must use the lat/long crs (because its lat/long?) then reproject
dat_sf <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# set up lists for storage
r_list <- list()
m_list <- list()

# i <- "kid_rock"

# loop through every goat in the 'goats' vector object
for (i in goats) {
  message("Currently on ", i)
  # subset data for a single goat
  goat <- dat_sf[dat_sf$goat_name == i,]
  
  #subset to coordinates and timestamp only (geometry column is automatically included)
  goat <- goat[,c("longitude", "latitude", "timestamp")]
  # reproject crs 
  goat <- st_transform(goat, crs = 32610)
  
  # convert geometries into vector object (i.e into a spatvector object) 
  goat_spatvector <- vect(goat)
  goat_spatvector$timestamp <- goat$timestamp
  # then convert spatvector into a spatraster object using the spatial properties outlined above and set background cells/pixels in the raster to 0 so the raster contains only values of 0 or 1 (0 = outside the polygon, 1 = inside the polygon) via field and background arguments
  r <- rasterize(goat_spatvector, raster_template, field = "timestamp", background = 0)
  # plot the spatraster to check
  plot(r)
  
  # Convert raster into a matrix, use wide = TRUE to make sure its in grid format i.e. 2D matrix
  m <- as.matrix(r, wide = TRUE)
  # view matrix to check
  image(m)
  
  # check timestamp format
  print(range(values(r))) # output is in Unix format
  print(range(as.vector(m)))  # output is in Unix format
  
  # save image
  png(paste0("figures/matrix/collar_data_matrix_", i, ".png"),
             width = 6.86, height = 6, units = "in", res = 600)
  # view matrix
  image(m, main = paste0("collar data matrix for ", i))
  dev.off()
  
  # store perimeter raster and matrices in a list
  r_list[[i]] <- r
  m_list[[i]] <- m 
  
}


save(r_list, file = "./data/collar_data/collar_data_raster_list_20250110.rda")
save(m_list, file = "./data/collar_data/collar_data_matrix_list_20250110.rda")


# convert matrix into raster to test
load("./data/fire/20240923_time_matrix_list.rda")
raster_list <- list()

for (i in 1:length(m_list)) {
  # extract a matrix from the list
  m <- m_list[[i]]
  # convert matrix back into a raster
  r <- rast(m)
  
  plot(r) 
  image(m) 
  
  # store raster in the list
  raster_list[[i]] <- r
}



#________________________________________________________________________________

# 4. FIRMS fire points in matrix form (save image)

source("scripts/source/nasa fire prep.r")

firms <- rbind(modis, viirs_suomi)
firms <- rbind(firms, viirs_noaa)

#subset to coordinates and timestamp only (geometry column is automatically included)
fire <- firms[,c("LONGITUDE", "LATITUDE", "timestamp")]

# reproject crs 
fire <- st_transform(fire, crs = st_crs(32610))

# convert geometries into vector object (i.e into a spatvector object) 
fire_spatvector <- vect(fire)
fire_spatvector$timestamp <- fire$timestamp
# then convert spatvector into a spatraster object using the spatial properties outlined above and set background cells/pixels in the raster to 0 so the raster contains only values of 0 or 1 (0 = outside the polygon, 1 = inside the polygon) via field and background arguments
r <- rasterize(fire_spatvector, raster_template, field = "timestamp", background = 0)

# plot the spatraster
plot(r)
#convert raster to matrix
m <- as.matrix(r, wide = TRUE)
# view matrix
image(m, main = "firms fire points matrix") 

# save image
png("figures/fire/firms_matrix.png", width = 6.86, height = 6, units = "in", res = 600)
# view matrix
image(m, main = "firms fire points matrix") 
dev.off()

# save matrix
save(m, file = "data/fire/firms_matrix.rda")


# 
# 
# as.POSIXct(times, 
#            origin="1970-01-01", # add if in Unix timestamp format to convert
#            tz = lutz::tz_lookup_coords(DATA$location.lat[1],
#                                        DATA$location.long[1],
#                                        method = "fast")) 