

library(ctmm)

# referring to https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf


load("./data/goat/prep/combined_data.rda")

# check the fix and dop types in the data
table(combined_data$fix_type)
# 2D       3D 3D Valid 
# 73    66196    11458 
# Note 3D =/= 3D Valid

table(combined_data$dop_type)
# dop  hdop  pdop 
# 11628    56 66043 

# subset calibration data out (include which() to avoid getting NA rows)
calibration_data <- raw_original[which(raw_original$calibration_data == 1),] #575

# check what kind of fix types the data are from
table(calibration_data$fix_type)
# 3D 
# 575 
table(calibration_data$dop_type)
# pdop 
# 575 




#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT NOTE
# only 1 type of fix type and dop type for the data to be used for calibration. this is problematic as the other types cannot be calibrated without a bunch of additional work needed
# therefore, not applying measurement/location error to the data and running analyzes with error = FALSE as we do not have error models because of mentioned above
#there will a lot of issues and trying to troubleshoot this, see below with all the various attempts (does not include all)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




# how many non-moving fixes for each collar?
table(calibration_data$collar_id)
# how many non-moving fixes for each cluster?
table(calibration_data$cluster_id)
# 1_europe_30548    1_europe_30551    1_europe_30561    1_europe_30567    1_europe_30575    1_europe_30599    1_europe_30636 
# 35                35                35                35                35                34                35 
# 1_europe_30642    1_europe_30648 2_precollar_30548 2_precollar_30551 2_precollar_30561 2_precollar_30567 2_precollar_30575 
# 35                34                18                16                18                17                19 
# 2_precollar_30599 2_precollar_30636 2_precollar_30642 2_precollar_30648           3_30613           4_30551           5_30567 
# 17                18                32                32                11                47                17 

# create a column to indicate the ID based on the cluster_id 
calibration_data$individual.local.identifier <- calibration_data$cluster_id
# cluster_id is needed because then ctmm will think that the collar has jumped between europe, office, mortality and back to the office
# this will mess up trying to estimate the uere, so assigning them cluster_id will indicate that each clump of non-moving signals are to be treated as its own clumping rather than the multiple clustering that a collar may have

# indicate the timezone that ctmm should use (using utc instead of local because itll make things easier later on when everything else is in utc), if you do not indicate which timezone to use, or timestamp, itll see two of them and get confused, youll get a warning
calibration_data$timestamp <- calibration_data$timestamp_utc



# convert to a ctmm telemetry object
calib_tel_data <- as.telemetry(calibration_data, timezone = "UTC",
                               keep = c("fix_id", "goat_id", "goat_name", "collar_id", "data_source"))


#Visualisation of the calibration data
ctmm::plot(calib_tel_data)
ctmm::plot(calib_tel_data, error = FALSE)
#visualize a cluster
ctmm::plot(calib_tel_data[[4]], error = FALSE)


# because calibration data is only of 1 fix_type, cannot calibrate the data without wayyyyyy too much work because of different dop types, fix types to get error models




#......................................................................................
# DOP ----
#......................................................................................



# hdop is unitless but if you need to put some kind of units its like hdop=1 is like 10 mins from that stationary signal

# gps error checks with elevation was initially investigated as there were discrepancies when comparing (refer to gps error checks r script), however, 
# dop values are incorporated into ctmm R package and accounts for them because setting a threshold and discarding points is not a good approach (for more details, refer to https://doi.org/10.1186/s40317-020-00194-z) 



# DOP values are designed to account for the order-of-magnitude heteroskedasticity stemming from satellite reception, geometry, latitude and dynamics
# DOP values provide time-dependent transformation between heteroskedastic location error and homoskedastic "user equivalent range error" (UERE).
# UEREs can be thought of as partially standardized location errors, in that they are proportional to the true location errors, but have a fixed (homoskedastic) variance




#............................................................
# dop checks
#............................................................

# check for NA pdop values
raw_original[is.na(raw_original$pdop),]
test <- raw_original[is.na(raw_original$pdop),] #56, theyre all hdop values

# Fleming et al 2021, S3.1 DOP Proxies, Co-opting DOP values
# In lieu of HDOP values, some GPS devices only record "position DOP" (PDOP), "geometric DOP" (GDOP), or ambiguous DOP values. PDOP values combine HDOP and VDOP; GDOP values combine PDOP and "time DOP" (TDOP) values. As HDOP and VDOP values are both a function of satellite number and spread, they are correlated. Therefore, it is reasonable to co-opt or appropriate related DOP values in the absence of proper HDOP and VDOP values.
# ...assuming the data are not of a special variety (Argos Doppler-shift or e-obs), 
# then the as.telemetry() will first look for an HDOP value. If HDOP values are not found, then as.telemetry() will look for ambiguous DOP values, followed by PDOP and then GDOP values. 
# If no DOP values are found, then as.telemetry() will look for the reported number of satellites, and apply model (S3.1) to approximate DOP values 

# however, you CANNOT substitute hdop values for missing pdop
# raw_original$pdop <- ifelse(is.na(raw_original$pdop & !is.na(raw_original$hdop)), raw_original$hdop, raw_original$pdop)
# need to do this separately from the new data because new data contains only dop value and when combining the two datasets and having dop and pdop, you will run into issues of getting hdop values of all 100, and vdop all at Inf. Even if you drop fix (location class) column but you want to keep fix type because 
# By default, ctmm's as.telemetry() function will assume that any GPS x-type column is meaningful, and create a corresponding location class column in the output telemetry data object. Model selection can then determine whether or not extra location classes are supported by the calibration data. as.telemetry() also checks if some location estimates lack corresponding speed, altitude, and/or DOP values, in which case location classes will be created to distinguish the level of missingness, as this often corresponds to different location estimation algorithms.

# # check again for NA pdop values
# raw_original[is.na(raw_original$pdop),] #none, all pdop values now
# 
# # drop hdop, vdop columns
# raw_original <- subset(raw_original, select = -c(hdop, vdop))




# load("./data/goat/collar_data.rda")
# 
# # how many DOP entries are there? (new data)
# nrow(collar_data[!is.na(collar_data$dop),]) # 11628
# # how many PDOP entries are there? (original data)
# nrow(collar_data[!is.na(collar_data$pdop),]) # 66099
# 
# # average dop value, do not include NA because some are recorded as PDOP
# mean(collar_data$dop, na.rm = TRUE) # 1.627468
# # average pdop value, do not include NA because some are recorded as DOP
# mean(collar_data$pdop, na.rm = TRUE) # 1.643169
# #average dop & pdop value together i.e., all dops value
# mean(c(collar_data$dop, collar_data$pdop), na.rm = TRUE) # 1.64082
# 
# # create a column that are all the dop types
# collar_data$dop_pdop  <- collar_data$dop
# # fill in the NA values that dop didnt have with pdop, if there is a value, leave it as the dop value
# collar_data$dop_pdop <- ifelse(is.na(collar_data$dop_pdop), collar_data$pdop, collar_data$dop_pdop)
# # check if they were all filled
# any(is.na(collar_data$dop_pdop))
# # average of all dops value
# mean(collar_data$dop_pdop, na.rm = TRUE) # 1.64082











#......................................................................................
# UERE ----
#......................................................................................

#Estimating the UERE

# do i estimate the uere for each cluster of stationary fixes??
UERE <- uere.fit(calib_tel_data)
UERE <- uere.fit(calib_tel_data[[4]])
UERE

# what does your uere look like? (meters)
summary(UERE)
# , , horizontal
# 
# low      est     high
# 3D 10.73347 11.19984 11.66596

# 
# save(UERE, file = "./data/goat/prep/uere.rda")
# load("./data/goat/prep/uere.rda")
# 
# # clean environment
# rm(list = ls())
# gc()


