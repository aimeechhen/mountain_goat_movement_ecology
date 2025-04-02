library('dplyr') # for data wrangling
library('purrr') # for functional programming
library('tidyr') # for data wangling
library('sf')    # for spatial features

# check layers
st_layers('data/collar_data/raw_collar/GPS_Collar30548_20231005121804.gpx')
#' use `track_points` layer
#********** note that there is  kml file in the folder and a csv file*********
st_layers('data/collar_data/raw_collar/GPS_Collar30548_20231005121804.gpx')
shp_file <- list.files('data/collar_data/raw_collar', full.names = TRUE)[2]
st_layers(shp_file)


# import all gpx files
d <- tibble(
  file = list.files('data/collar_data/raw_collar'),
  track = map(file, function(fn) {
    st_read(paste0('data/collar_data/raw_collar/', fn),
            layer = 'track_points', quiet = TRUE) %>%
      # add animal identifier with ctmm syntax
      mutate(individual.local.identifier = substr(fn, nchar('GPS_CollarX'),
                                                  nchar('GPS_CollarXXXXX'))) %>%
      bind_cols(., st_coordinates(.)) %>% # add coordinates
      st_drop_geometry() %>% # drop sf geometry column
      rename(location.long = X, # rename using ctmm syntax
             location.lat = Y,
             timestamp = time)
  })) %>%
  unnest(track) # turn into a single large tibble

# check which columns have more than one unique (non-NA) value
d %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-c()) %>%
  group_by(name) %>%
  summarize(n = n_distinct(value, na.rm = TRUE)) %>%
  mutate(useful = n > 1) %>%
  arrange(desc(n)) %>%
  print(n = 28)

# drop non-NA columns with more than one unique value
d <- d %>%
  select(individual.local.identifier, location.long, location.lat,
         timestamp, fix, hdop, pdop)

saveRDS(d, 'data/stefano_oreamnos-americanus-tels.rds')

names(d)[1] <- "collar_id"
# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
d <- merge(d, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)


d$timestamp = as.POSIXct(d$timestamp, format = "%Y-%m-%d %H:%M:%S")

d$goat_name <- as.factor(d$goat_name)
# d$timestamp <- as.POSIXct(d$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
d$date <- as.Date(d$timestamp)
d$year <- year(d$timestamp)
d$month <- month(d$timestamp, label = FALSE) #label = false for numerical month
d$day <- day(d$timestamp)
d$month_day <- format(d$timestamp, "%m-%d")
d$doy <- yday(d$timestamp) #day of the year



d$date = as.Date(d$date, "%Y-%m-%d")
d$goat_name <- as.factor(d$goat_name)
d$collar_id <- as.factor(d$collar_id)
# fire period for all years
fire_start <- '07-22' # doy = 203
fire_end <- '10-26' # doy = 299
d <- d[d$month_day >= fire_start & d$month_day <= fire_end, ] #15462

load("data/collar_data/collar_data_20241123.rda")
test <- collar_data[collar_data$month_day >= fire_start & collar_data$month_day <= fire_end, ] #14822

# number of points removed
15462 - 14822
#640














# new data

telemetry.check <- read.csv(file = "data/collar_data/flagged_new_collar_data_20250218.csv")
telemetry.check$timestamp <- as.POSIXct(telemetry.check$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")

fire_start <- '07-22' # doy = 203
fire_end <- '10-26' # doy = 299

d <- telemetry.check
# d$timestamp <- as.POSIXct(d$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
d$date <- as.Date(d$timestamp)
d$year <- year(d$timestamp)
d$month <- month(d$timestamp, label = FALSE) #label = false for numerical month
d$day <- day(d$timestamp)
d$month_day <- format(d$timestamp, "%m-%d")
d$doy <- yday(d$timestamp) #day of the year
d$date = as.Date(d$date, "%Y-%m-%d")



# how many points during the fire period across all years
test <- d[d$month_day >= fire_start & d$month_day <= fire_end, ] # 3540 gps points
#identify rowsnames that has "NA" in it
nrow(test[grepl("NA", rownames(test)),]) # 98 na rows
# drop those rows
test <- test[!grepl("NA", rownames(test)),]

# how many have been flagged?
nrow(test[!test$flag_outlier == 0,]) #31



# total outliers ----

640+31
#671