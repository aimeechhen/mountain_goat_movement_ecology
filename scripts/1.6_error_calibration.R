

library(ctmm)

# referring to https://zslpublications.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Facv.12728&file=acv12728-sup-0002-AppendixS2.pdf


load("./data/goat/prep/raw_data_prepped.rda")

# subset calibration data out (include which() to avoid getting NA rows)
calibration_data <- raw_data[which(raw_data$calibration_data == 1),] #575

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
DATA <- as.telemetry(calibration_data, timezone = "UTC")

#Visualisation of the calibration data
ctmm::plot(DATA, error = FALSE)

#Estimating the UERE
UERE <- uere.fit(DATA)
UERE

# what does your uere look like? (meters)
summary(UERE)
# , , horizontal
# 
# low      est     high
# 3D 10.73347 11.19984 11.66596

save(UERE, file = "./data/goat/prep/uere.rda")
load("./data/goat/prep/uere.rda")

# clean environment
rm(list = ls())
gc()
