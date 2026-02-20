


library(ctmm)


load("./data/goat/goat_data.rda")

# drop certain columns or will have issues when converting into ctmm object. not including measurement error, refer to previous scripts for explanation and why
goat_data <- subset(goat_data, select = -c(fix_type, hdop, vdop, pdop, dop, altitude_m))
# because no DOP is included, ctmm is going to assume DOP = 1

# convert to ctmm object
tel_data <- as.telemetry(goat_data, mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id"))

# ensure each df in the list is in the correct order with matching row name
for (i in 1:length(tel_data)) {
  # order rows in each df in the list based on fix_id
  tel_data[[i]] <- tel_data[[i]][order(tel_data[[i]][["fix_id"]]),]
}


# summary of the gps data, (i.e., interval, period, long, lat info)
summary(tel_data) # mostly ~6.25h with a few at 5.5h interval

# visualisation of the data
plot(tel_data)


save(tel_data, file = "./data/goat/goat_tel_data.rda")



#..............................................................
# study period ----
#..............................................................


load(file = "./data/goat/study_data.rda")

# drop certain columns or will have issues when converting into ctmm object. not including measurement error, refer to previous scripts for explanation and why
study_data <- subset(study_data, select = -c(fix_type, hdop, vdop, pdop, dop, altitude_m))
# because no DOP is included, ctmm is going to assume DOP = 1

# set id_year as individual.local.identifier to avoid needing to subset by individuals and year, it will create a unique identifier based on the individual and the year of the data and then those will be grouped together as the data for each individual for each year
study_data$individual.local.identifier <- study_data$id_year

# convert to ctmm object
tel_data <- as.telemetry(study_data, mark.rm = TRUE,
                         keep = c("fix_id", "goat_id", "goat_name", "collar_id", "id_year"))
# 35 items 6x6=36-1= 35 CA12 got collared in 2020, missing 1 year = 35 (6 goats x 6 years except 1 has 5 years)

# ensure each df in the list is in the correct order with matching row name
for (i in 1:length(tel_data)) {
  # order rows in each df in the list based on fix_id
  tel_data[[i]] <- tel_data[[i]][order(tel_data[[i]][["fix_id"]]),]
}

# summary of the gps data, (i.e., interval, period, long, lat info), check the period to ensure that its the study period length
summary(tel_data) # all 6.25h, 3.27-3.28 months

# visualisation of the data
plot(tel_data)

save(tel_data, file = "./data/goat/study_period_tel_data.rda")
