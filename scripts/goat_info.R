# prep mountain goat info

library(readxl)
library(dplyr)

#.......................................
# mountain goat info ----
#.......................................

# import supplementary mountain goat info 
capture_info <- read_xlsx("./data/goat/Cathedral goat capture database copy.xlsx", sheet = "Sheet1", skip = 1)
head(capture_info)

# rename the columns so they're more workable
capture_info <- rename(capture_info, c(goat_id = `Collar Tag (Goat ID)`,
                                       collar_id = `Collar ID`,
                                       capture_date = Date))

# extract data of interest
goat_info <- subset(capture_info, select = c("goat_id", "collar_id", "capture_date", 
                                             "Age", "Sex", "Kid present",
                                             "Neck", "Chest", "Total length", 
                                             "General location", "Capture Comments", "Fate"))

# data carpentry
# clean up the entry
goat_info$goat_id[1] <- "CA11"

# order the df by goat id
goat_info <- goat_info[order(goat_info$goat_id), ]

# add the mountain goats names :) based on the order that goat_id was sorted in (i.e. numerical order)
goat_info$goat_name <- c("selena_goatmez",
                         "great_goatsby",
                         "cliff",
                         "toats_mcgoats",
                         "alpine_pacino",
                         "goatzilla",
                         "the_goatmother",
                         "goatbriella",
                         "vincent_van_goat",
                         "goatileo",
                         "ryan_goatsling")

goat_info <- relocate(goat_info, goat_name, .after = goat_id)
goat_info$capture_date <- as.Date(goat_info$capture_date)
#convert the tibble into a df
goat_info <- as.data.frame(goat_info)

# create a end of tracking date column based (mortality, collar went offline, etc)
goat_info$tracking_end <- as.Date(NA)
goat_info$tracking_end[2] <- as.Date("2021-09-07")
goat_info$tracking_end[3] <- as.Date("2019-08-24")
goat_info$tracking_end[5] <- as.Date("2023-03-23")
goat_info$tracking_end[8] <- as.Date("2022-07-01")
goat_info$tracking_end[11] <- as.Date("2021-03-16")

# calculate the number of tracking days
goat_info$tracking_days <- as.numeric(goat_info$tracking_end - goat_info$capture_date)
goat_info <- relocate(goat_info, c(tracking_end, tracking_days), .after = capture_date)

goat_info$goat_id <- as.factor(goat_info$goat_id)
goat_info$goat_name <- as.factor(goat_info$goat_name)
goat_info$collar_id <- as.factor(goat_info$collar_id)


# save as rda to retain formatting
save(goat_info, file = "./data/goat/goat_info.rda")
# and as a csv for quick reference without having to open rstudio
write.csv(goat_info, file = "./data/goat/goat_info.csv", row.names = FALSE)

load("./data/goat/goat_info.rda")
