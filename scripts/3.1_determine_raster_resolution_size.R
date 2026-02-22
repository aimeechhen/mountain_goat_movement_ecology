

# get raster size and determine raster resolution 

library(bcdata)
library(sf)


#............................................................
# import goat data ----
#............................................................


load("./data/goat/goat_data.rda") # 6 goats all data
goat_data <- goat_data[goat_data$outlier != 1,]

goat_data_sf <- st_as_sf(goat_data, coords = c("longitude", "latitude"), crs = 4326)
# convert to bc albers crs
goat_data_sf <- st_transform(goat_data_sf, crs = 3005)
goat_data_sf <- bcmaps::transform_bc_albers(goat_data_sf)
# get the extent based on bounding box
ext <- st_as_sfc(st_bbox(goat_data_sf))
# add a buffer making the bbox larger
study_bbox <- st_buffer(ext, dist = 5000)


#............................................................
# determine raster size ----
#............................................................

# BC Parks, Ecological Reserves, and Protected Areas data

# access the data from bc data catalogue
bcdata <- bcdc_query_geodata("WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW")
# retrieve the data
bcparks <- collect(bcdata) 
# extract cathedral
cathedral_sf <- subset(bcparks, grepl("CATHEDRAL", PROTECTED_LANDS_NAME))

# get citation for the data (refer to the BC Geographic Warehouse Custom Download link)
# bcdc_get_citation("https://catalogue.data.gov.bc.ca/dataset/bc-parks-ecological-reserves-and-protected-areas/resource/ee6bf892-9ba1-4992-b765-a9c25fd57306")

plot(study_bbox)
plot(st_geometry(cathedral_sf), add = TRUE)
plot(st_geometry(goat_data_sf), add = TRUE)

# a 5km buffer around the goat data as the bbox is a good raster size (it does not include all of cathedral prov park though)

# save(goat_data_sf, file = "./data/goat/goat_data_sf.rda")

rm(bcdata, bcparks)




#............................................................
# determine raster resolution ----
#............................................................

# determine the scale that rasters need to be/get in 
# 1. figure out measurement error before anything else
# 2. Look at the histogram of it and maybe match it to the quantiles, like seventy-five.
# histogram of measurement error (which is measurement error = DOP * UERE)

#2025-11-12 meeting
# you want it to match like the resolution of the data that you can say something about the location.
# how to determine what raster scale to have your rasters in
# its based on how coarse your data are and how much measurement error there is then on it
# look at the average DOP value and the UERE i.e., measurement error = DOP * UERE

# assign the UERE to the data? 
# so give you measurement error kind of in meters when you multiply DOP times UERE.
# So if the average DOP value is, e.g. one, and the UERE is 10, then it'll be 10.
# The average error is going to be 10, and then you also have the range of the DOP values.
# So you can use that to get a sense as to what scale you might want to think of.
# So if the average error is 10, a 20 meter pixel size grid size, will be pretty good, 25 kind of thing.
# Because then they don't risk shooting out of it [the cell] because of measurement error.

# if raster is too fine-scaled
# Because if the error is like 20 meters. And your raster is like 5 meters.
# Most of the error is going to push those locations outside of where the actual, like the animal could have been anywhere in that space.
# So that measurement that you have for the elevation might be wrong.
# But if it's the average elevation over a much bigger area,
# The precise elevation at the location the animal was is not going to be as accurate.
# But there's less of a chance of it being wrong because it'll be correct on average.


#.......................................................................
# # measurement error
#.......................................................................

# refer to the the measurement_error.R script for more information and how the value was obtained
# Note measurement error is not being included in analyses, its only used here as reference for determining the resolution to get the raster in

# measurement error = 18.39591


#.....................................................................
# how to get a histogram of the measurement error? -> referring to dop values (is one part that makes up measurement error)

# Look at the histogram of it and maybe match it to the quantiles, like seventy-five.
# plot a histogram of all the dops
hist(na.omit(combined_data$pdop))
hist(na.omit(combined_data$dop))
# majority of them are under 2

quantile(combined_data$pdop, na.rm = TRUE)
# 0%  25%  50%  75% 100% 
# 0.8  1.4  1.6  1.8 16.2 
quantile(combined_data$dop, na.rm = TRUE)

# at the 95%
quantile(combined_data$pdop, prob = 0.95, na.rm = TRUE)
# 2.4
quantile(combined_data$dop, prob = 0.95, na.rm = TRUE)
# 2.4


# Remember: # So if the average DOP value is, e.g. one, and the UERE is 10, then it'll be 10.
# therefore 2.4 = 24m

# to determine the size...would 25m be good enough?
# So 2.4 would be your HDOP.
# So 95% of the DOP values are going to be below 2.4.
# And 95% of the errors are going to be below 25 meters.
# So if they're like right in the middle
# 25m would push them out of it Because it would be 12 and a half to the edge of the raster, so.
# Yeah, 50m would push it so that 95% of the locations to 24 meters, they're still in the cell, even if they're in the center.

# This is just changing the resolution to match the measurement error that's not actually incorporating measurement error into the analysis.

# *****can it be determined like this too? adding the measurement error and the dop value (in meters, remember dop values are unitless)******
# 18.39591 + 24
# 42.39591

