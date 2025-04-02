# window analysis results

library(ctmm)
library(sf)
library(ggh4x)
library(gridExtra)
library(lubridate)

# WINDOW RESULTS ----

## Set directory path ----
# dir_path <- "./data/window_analysis/original_data/" 
# dir_path <- "./data/window_analysis/new_data/"
# dir_path <- "./data/window_analysis/full_6_combined_data/"
dir_path <- "./data/window_analysis/fire_period/"
# dir_path <- "./data/window_analysis/fire_period_all_years/"



# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "akdes.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
AKDES <- rds_dat

# Load .shp, .tif etc files within a folder including all the subfolders
rds_files <- list.files(dir_path, pattern = "fits.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# combine together as one list
rds_dat <- do.call(c, rds_list)
FITS <- rds_dat


# Load .shp, .tif etc files within a folder including all the subfolders, remember these are saved as df
rds_files <- list.files(dir_path, pattern = "covariates.*\\.rds$", full.names = TRUE, recursive = TRUE)
# Import/read all the files into a list
rds_list <- lapply(rds_files, readRDS)
# # combine together as one df
rds_dat <- do.call(rbind, rds_list)
covariates <- rds_dat

# # Load .shp, .tif etc files within a folder including all the subfolders
# rds_files <- list.files(dir_path, pattern = "rsf.*\\.rds$", full.names = TRUE, recursive = TRUE)
# # Import/read all the files into a list
# rds_list <- lapply(rds_files, readRDS)
# # combine together as one list
# rds_dat <- do.call(c, rds_list)
# RSF <- rds_dat


#clean up environment
rm(rds_dat, rds_list, rds_files)
rm(AKDES, FITS, covariates, original)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~for merged extracted windows
# combining original and new data window analysis

original <- AKDES
original <- FITS
original <- covariates
# original <- RSF

# drop the 4 goats not used in this study
original <- original[!grepl("alpine_pacino", names(original))]
original <- original[!grepl("billy", names(original))]
original <- original[!grepl("great_goatsby", names(original))]
original <- original[!grepl("vertigoat", names(original))]

# rename list entries
# names(original) <- gsub("alpine_pacino", "30551", names(original))
# names(original) <- gsub("billy", "30636", names(original))
names(original) <- gsub("goatzilla", "30548", names(original))
# names(original) <- gsub("great_goatsby", "30599", names(original))
names(original) <- gsub("kid_rock", "30561", names(original))
names(original) <- gsub("rocky", "30613", names(original))
names(original) <- gsub("the_goatmother", "30575", names(original))
names(original) <- gsub("toats_mcgoats", "30642", names(original))
# names(original) <- gsub("vertigoat", "30567", names(original))
names(original) <- gsub("vincent_van_goat", "30648", names(original))


original_akdes <- original
original_fits <- original
original_covariates <- original

new_akdes <- AKDES
new_fits <- FITS
new_covariates <- covariates


names(original_covariates)
names(new_covariates)
# drop column "window_start....as.POSIXct.rep.NA..length.times....tz....America.Vancouver.." and  "window_end....as.POSIXct.rep.NA..length.times....tz....America.Vancouver.." 
original_covariates <- original_covariates[,c(1,4:8)]
new_covariates <- new_covariates[,c(1,4:8)]

# combine original and new data window outputs
covariates <- rbind(original_covariates, new_covariates)
AKDES <- c(original_akdes, new_akdes)
FITS <- c(original_fits, new_fits)


saveRDS(covariates, file = "data/window_analysis/covariates_extracted_combined_data_20250317.rds")
saveRDS(AKDES, file = "data/window_analysis/akdes_extracted_combined_data_20250317.rds")
saveRDS(FITS, file = "data/window_analysis/fits_extracted_combined_data_20250317.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end



#______________________________________________________________________________
# Check units ----

# Default units:
# area (square meters)             
# τ (seconds)                      
# speed (meters/second)            
# diffusion (square meters/second)


# check fitted models units
summary_outputs <- data.frame()
for (i in seq_along(FITS)) {
  summary <- summary(FITS[[i]], units = FALSE)$CI # using SI units
  summary_outputs <- rbind(summary_outputs, 
                           data.frame(Var1 = names(table(rownames(summary))), 
                                      Freq = as.integer(table(rownames(summary)))))
}

summary_outputs <- aggregate(Freq ~ Var1, data = summary_outputs, FUN = sum)
summary_outputs

# speed = distance travelled over time (from a to b)
# diffusion = area travelled over time (area/space when going from a to b)

# Var1 Freq
# 1             area (square meters) 2986
# 2 diffusion (square meters/second) 1488
# 3            speed (meters/second)  973
# 4                      τ (seconds)  962
# 5               τ[decay] (seconds)   11
# 6              τ[period] (seconds)   11
# 7            τ[position] (seconds)  515




#........................................................................
# Check UD units

hr_size <- data.frame()
for (i in 1:length(AKDES)) {
  # cat("Processing AKDE element:", i, "\n")  # Print current loop iteration
  #extract the home range area statistics summary
  tryCatch({
    summary <- as.data.frame(summary(AKDES[[i]], units = FALSE)$CI) # default is square meters
    summary$identifier <- names(AKDES[i])
    #bind the summary to the dataframe
    hr_size <- rbind(hr_size, summary)
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}
# inspect if all the units are the same (units = km²), 
#units = FALSE, default units are m^2, 
# units = TRUE -> km² instead of keeping it in m²
hr_size

table(sub(".*\\((.*)\\).*", "\\1", rownames(hr_size)))
# hectares square kilometers     square meters 
# 5986              6471               371 

# units are still mixed despite units = FALSE, therefore need to account for it



#____________________________________________________________________________
# create dataframe for results ----

win_results <- data.frame(collar_id = character(length(AKDES)))

for (i in 1:length(AKDES)) {
  # cat("Processing AKDE element:", i, "\n")
  tryCatch({
    # extract item from list
    win_results$collar_id[i] <- AKDES[[i]]@info$identity
    # get timestamp from the list element name
    win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(AKDES)[i])
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}


win_results$window_start <- as.POSIXct(win_results$window_start, format = "%Y-%m-%d %H:%M:%S")
win_results$date <- as.Date(win_results$window_start)
win_results$year <- year(win_results$window_start)
win_results$month <- month(win_results$window_start, label = FALSE) #label = false for numerical month
win_results$day <- day(win_results$window_start)
win_results$month_day <- format(win_results$window_start, "%m-%d")
win_results$doy <- yday(win_results$window_start) #day of the year

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ for merged extracted windows
# rename entries
win_results$collar_id <- gsub("goatzilla", "30548", win_results$collar_id)
win_results$collar_id <- gsub("kid_rock", "30561", win_results$collar_id)
win_results$collar_id <- gsub("rocky", "30613", win_results$collar_id)
win_results$collar_id <- gsub("the_goatmother", "30575", win_results$collar_id)
win_results$collar_id <- gsub("toats_mcgoats", "30642", win_results$collar_id)
win_results$collar_id <- gsub("vincent_van_goat", "30648", win_results$collar_id)
unique(win_results$collar_id)
# drop the empty window
win_results <- win_results[-10940,]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end



# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
win_results <- merge(win_results, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
win_results <- dplyr::relocate(win_results, c("collar_id","goat_name", "goat_id"), .before = window_start)


# i <- 1








#.............................................................
# home range results (units = km^2) ----
# units are not consistent, therefore need to account for it
for (i in 1:length(AKDES)) {
  # cat("Processing AKDE element:", i, "\n")
  
  tryCatch({
    
    # subset akde results for one window
    akdesum <- as.data.frame(summary(AKDES[[i]], units = FALSE)$CI)
    
    # using km^2 as default SI units, convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
    if ("area (square kilometers)" %in% rownames(akdesum)) {
      win_results[i, c("hr_min_km2", "hr_est_km2", "hr_max_km2")] <- akdesum[c("low", "est", "high")]
      
    } else if ("area (square meters)" %in% rownames(akdesum)) {
      # convert m^2 to km^2 .-. divide by 1e6
      win_results[i, c("hr_min_km2", "hr_est_km2", "hr_max_km2")] <- akdesum[c("low", "est", "high")] / 1e6    
      
    } else if ("area (hectares)" %in% rownames(akdesum)) { 
      # convert hectares into km^2 (1 hectare = 0.01 km^2 .-. divide by 100)
      win_results[i, c("hr_min_km2", "hr_est", "hr_max_km2")] <- akdesum[c("low", "est", "high")] / 100
      
    } else { 
      cat("no entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
      win_results[i, c("hr_min_km2", "hr_est_km2", "hr_max_km2")] <- NA
    }
    
  }, error = function(e) {
    cat("Error in loop at index", i, ":", conditionMessage(e), "\n")
  })
}





#___________________________________________________________________________
# movement results ----

# i <- 3
# FIT <- FITS[1:100]


# win_results <- data.frame(goat_name = character(length(FIT)))

tic()
for (i in 1:length(FITS)) {
  # for (i in 1:min(100, length(FIT))) { # testing with subset
  
  # extract item from list
  win_results$collar_id[i] <- FITS[[i]]@info$identity
  # get timestamp from the list element name
  win_results$window_start[i] <- sub(".*_(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})", "\\1", names(FITS)[[i]])
  
  
  win_results$movement_model[i] <- summary(FITS[[i]], units = FALSE)$name
  win_results[i, c("DOF_mean", "DOF_area", "DOF_diffusion", "DOF_speed")] <- summary(FITS[[i]], units = FALSE)$DOF[c("mean", "area", "diffusion", "speed")]
  
  
  #..............................................
  ## diffusion ----
  
  # si units has it m^2/sec, convert to km^2/day 0.0864 or 8.64e-2
  if ("diffusion (square meters/second)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI))  {
    # Update the corresponding row in the data frame
    win_results[i, c("diffusion_min_km2_day",
                     "diffusion_est_km2_day",
                     "diffusion_max_km2_day")] <- summary(FITS[[i]], units = FALSE)$CI["diffusion (square meters/second)",
                                                                                       c("low", "est", "high")] * 0.0864
    
  } else {
    cat("no diffusion entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("diffusion_min_km2_day", "diffusion_est_km2_day", "diffusion_max_km2_day")] <- NA
  }
  
  
  
  
  #..............................................
  # tau_p ----
  # si units are seconds, convert into days
  if ("τ[position] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_p_min_s",
                     "tau_p_est_s",
                     "tau_p_max_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                             c("low", "est", "high")]
    
    #convert to seconds to days
    win_results[i, c("tau_p_min_day",
                     "tau_p_est_day",
                     "tau_p_max_day")] <- summary(FITS[[i]], units = FALSE)$CI["τ[position] (seconds)",
                                                                               c("low", "est", "high")] / 86400
    
    
    
  } else {
    cat("no tau p entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_p_min_s", "tau_p_est_s", "tau_p_max_s")] <- NA
    win_results[i, c("tau_p_min_day", "tau_p_est_day", "tau_p_max_day")] <- NA
  }
  
  
  
  #..............................................
  # tau_v ----
  # si units are seconds, convert to minutes
  if ("τ[velocity] (seconds)" %in% rownames(summary(FITS[[i]], units = FALSE)$CI)) {
    
    # Update the corresponding row in the data frame (used for bls calculations)
    win_results[i, c("tau_v_min_s",
                     "tau_v_est_s",
                     "tau_v_max_s")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)",
                                                                             c("low", "est", "high")]
    #convert to seconds to minutes
    win_results[i, c("tau_v_min_min", 
                     "tau_v_est_min", 
                     "tau_v_max_min")] <- summary(FITS[[i]], units = FALSE)$CI["τ[velocity] (seconds)", 
                                                                               c("low", "est", "high")] / 60
    
  } else { 
    # cat("no tau v entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("tau_v_min_s", "tau_v_est_s", "tau_v_max_s")] <- NA
    win_results[i, c("tau_v_min_min", "tau_v_est_min", "tau_v_max_min")] <- NA
  }
  
  
  fit_mu <- as.data.frame(FITS[[i]]$mu)
  centroid_sf <- st_as_sf(fit_mu, coords = c('x', 'y'), crs = FITS[[i]]@info$projection)
  centroid_sf <- st_coordinates(st_transform(centroid_sf, crs = 4326)) # reproject and grab the coordinates
  win_results[i, c("centroid_long", "centroid_lat")] <- centroid_sf[1, c("X", "Y")]
  
  
  # bls ----
  
  # If tau_p and tau_v have values, then calculate BLS
  if (!is.na(win_results$tau_p_est_s[i]) && !is.na(win_results$tau_v_est_s[i])) {
    sigma_p <- ctmm:::area.covm(FITS[[i]]$sigma) # area.covm sigma  = get geometric area/volume
    
    bls_min <- sqrt((win_results$tau_v_min_s[i] / win_results$tau_p_min_s[i]) * sigma_p)
    bls_est <- sqrt((win_results$tau_v_est_s[i] / win_results$tau_p_est_s[i]) * sigma_p)
    bls_max <- sqrt((win_results$tau_v_max_s[i] / win_results$tau_p_max_s[i]) * sigma_p)
    
    win_results[i, c("bls_min", "bls_est", "bls_max")] <- c(bls_min, bls_est, bls_max)
    
  } else {
    cat("no bls entry found for", win_results$goat_name[i], win_results$window_start[i], "\n")
    win_results[i, c("bls_min", "bls_est", "bls_max")] <- NA
  }
  
  
  # end of loop
  
}

toc() #~4min


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ for merged extracted windows
# rename entries
win_results$collar_id <- gsub("goatzilla", "30548", win_results$collar_id)
win_results$collar_id <- gsub("kid_rock", "30561", win_results$collar_id)
win_results$collar_id <- gsub("rocky", "30613", win_results$collar_id)
win_results$collar_id <- gsub("the_goatmother", "30575", win_results$collar_id)
win_results$collar_id <- gsub("toats_mcgoats", "30642", win_results$collar_id)
win_results$collar_id <- gsub("vincent_van_goat", "30648", win_results$collar_id)
win_results <- win_results[complete.cases(win_results$goat_name), ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end


# covariates results ----

# covar <- covariates
covariates <- covar

covariates$collar_id <- as.factor(covariates$collar_id)
covariates$window_start <- as.POSIXct(covariates$window_start, format = "%Y-%m-%d %H:%M:%S")
covariates$date <- as.Date(covariates$window_start)
covariates$year <- year(covariates$window_start)
covariates$month <- month(covariates$window_start, label = FALSE) #label = false for numerical month
covariates$month_day <- format(covariates$window_start, "%m-%d")
covariates$day <- day(covariates$window_start)
covariates$month_day <- as.Date(covariates$month_day, "%m-%d") # set with dummy year to be able to overlay or there will be issues plotting
covariates$doy <- yday(covariates$window_start) #day of the year




# Import supplementary mountain goat info 
goat_info <- read.csv("data/goat_info.csv")
goat_info$collar_id <- as.factor(goat_info$collar_id)
# subset to only the fire goats
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat")
goat_info <- goat_info[goat_info$goat_name %in% goats,]
covariates <- merge(covariates, goat_info[, c("collar_id","goat_name", "goat_id")], by = "collar_id", all.x = TRUE)
covariates <- dplyr::relocate(covariates, c("collar_id","goat_name", "goat_id"), .before = n_fixes)

#11178 rows
#check for NAS
test <- covariates[!complete.cases(covariates$collar_id, covariates$window_start, covariates$window_end, covariates$n_fixes), ] #7725
# drop rows where every item are NA across the rows
covariates <- covariates[complete.cases(covariates$collar_id, covariates$window_start, covariates$window_end, covariates$n_fixes), ] #3453, 5970 for combined window sets, full data = 11916


# covariates <- merge(covariates, win_results[, c("collar_id","goat_name", "goat_id")], by = c("collar_id", "window_start"), all.x = TRUE)



# 
# write.csv(win_results, file = "./data/window_analysis/combined_data_window_partial_results_20250313.csv", row.names = FALSE)
# win_results <- read.csv("./data/window_analysis/combined_data_window_partial_results_20250313.csv")


# full data
write.csv(win_results, file = "./data/window_analysis/merged_windows_extracted_combined_data_20250317.rds", row.names = FALSE)
win_results <- read.csv("./data/window_analysis/merged_windows_extracted_combined_data_20250317.rds")
write.csv(win_results, file = "./data/window_analysis/merged_windows_extracted_covariates_combined_data_20250317.rds", row.names = FALSE)
win_results <- read.csv("./data/window_analysis/merged_windows_extracted_covariates_combined_data_20250317.rds")



#////////////////////////////////////////////////////////
# PLOT ----
#////////////////////////////////////////////////////////


library(ggplot2)


# win_results <- read.csv("./data/window_analysis/combined_data_window_partial_results_20250313.csv")
win_results <- read.csv("./data/window_analysis/merged_windows_extracted_combined_data_20250317.rds")

# formatting
win_results$window_start = as.POSIXct(win_results$window_start, format = "%Y-%m-%d %H:%M:%S")
win_results$date = as.Date(win_results$date, "%Y-%m-%d")
win_results$year <- as.factor(win_results$year)
win_results$goat_name <- as.factor(win_results$goat_name)
win_results$collar_id <- as.factor(win_results$collar_id)

mw_dat <- win_results


#__________________________________________________________________
# 1) Home range estimate ----



## Full scale ----
# based on Ryan's code in the window.hr function but using ggplot

# single goat
# ggplot(data = mw_dat[mw_dat$ID == "30548",]) +
#   geom_point(aes(x = date, y = hr_est_km), shape = 19, size = 1.25) +
#   geom_errorbar(aes(x = date, ymin = hr_min_km, ymax = hr_max_km), width = 0.1) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   ggtitle("30548") +
#   labs(x = 'Date') + 
#   scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")"),
#                      limits = c(0, max(mw_dat$hr_max_km, na.rm = TRUE))) +
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))






goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
mw_dat <- mw_dat[order(mw_dat$collar_id, mw_dat$window_start), ]
mw_dat$goat_color <- factor(mw_dat$collar_id, levels = goats) 
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# create a goat colour column to assign their colour
mw_dat$goat_color <- goat_palette[match(mw_dat$goat_name, goats)]

# determine the doy number for the 1st of each month for xaxis breaks
day_1 <- mw_dat[mw_dat$day == 1, ]
day_1 <- day_1[!duplicated(day_1$month), ]
day_1[order(day_1$month),]
month_breaks <- sort(day_1$doy)
month_breaks

library(ggh4x) # to fill in facet wrap title boxes
strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))


# home range
# all goats
# ggplot(data = mw_dat) +
#   geom_point(aes(x = date, y = hr_est_km2), shape = 19, size = 1.25) +
#   geom_errorbar(aes(x = date, ymin = hr_min_km2, ymax = hr_max_km2), width = 0.1) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-10-26')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap2(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot 
#               ncol = 2, nrow = 3, strip = strip_col,
#               labeller = labeller(collar_id = c('30548' = 'Goatzilla',
#                                                 '30561' = 'Selena Goatmez',
#                                                 '30575' = 'The Goatmother',
#                                                 '30613' = 'Goatileo',
#                                                 '30642' = 'Toats McGoats',
#                                                 '30648' = 'Vincent Van Goat'))) +  
#   scale_size_identity() +  # Keep sizes as defined
#   scale_alpha_identity() +
#   labs(x = "", y = expression("95% Home Range Area Estimate (km"^2*")")) + 
#   # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")"),
#   #                    limits = c(0, max(mw_dat$hr_max_km, na.rm = TRUE))) +
#   scale_y_log10() +
#   # scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   scale_x_date(date_labels = "%b %d", date_breaks = "1 day") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         strip.text = element_text(color = "white", face = "bold"))

# ggsave(last_plot(), file = "figures/moving_window_hr_time.png",
#        height = 6.86, width = 14,
#        units = "in", dpi = 600,
#        bg = "transparent")



#....................................................................
## Calendar scale home range ----

# Single goat

# ggplot(data = mw_dat[mw_dat$ID == "30548",]) +
#   # geom_smooth(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year), linetype = 'solid')) +
#   geom_point(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year))) +
#   geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
#   ggtitle("30548") +
#   scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
#   # scale_y_log10(name = expression("95% Home Range Area Estimate (km"^2*")")) +
#   scale_x_continuous(name = 'Month',
#                      limits = c(-5, 340), 
#                      expand = c(0, 0), # Full year
#                      breaks = month_breaks, # Approximate month starts
#                      # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
#                      labels = month.abb) + # Month abbreviations
#   scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                                "2020" = "#ddaa33",
#                                                "2021" = "#006d2c",
#                                                "2022" = "#33bbee",
#                                                "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))

# 
# 
# # All goats
# ggplot(data = mw_dat) +
#   # geom_point(aes(x = doy, y = hr_est_km2, color = as.factor(year), group = as.factor(year))) +
#   geom_smooth(aes(x = doy, y = hr_est_km2, color = as.factor(year), group = as.factor(year), linetype = 'solid'),  alpha = 0.5) +
#   geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dashed") +
#   labs(y = expression("95% Home Range Area Estimate (km"^2*")")) +
#   facet_wrap(~ goat_name,
#              ncol = 1, nrow = 6, # sorted by ID
#              scales = "fixed", ) +  #set axis so theyre the same for every plot
#   # ggtitle("Home Range Estimates for ID: {facet_var}") +  # Dynamic title using facet variable
#   # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
#   scale_y_log10() +
#   scale_x_continuous(name = 'Month',
#                      limits = c(-5, 370),
#                      expand = c(0, 0), # Full year
#                      breaks = month_breaks, # Approximate month starts
#                      # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
#                      labels = month.abb) + # Month abbreviations
#   scale_color_manual(#name = "Year",
#     name = "",
#     values = c("2019" = "#332288",
#                "2020" = "#ddaa33",
#                "2021" = "#006d2c",
#                "2022" = "#33bbee",
#                "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         legend.position = "top",
#         legend.direction = "horizontal",
#         legend.justification = "center")
# ggsave(last_plot(),  file="figures/moving_window/fire_goats_moving_window_1_hr_overtime_calendar_scale.png",
#        width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")
# 


#_____________________________________________________________________
# 2) Movement ----
# 
# ## Speed ----
# ggplot(data = mw_dat) +
#   geom_point(aes(x = date, y = speed_est)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   labs(x = 'Date',
#        y = 'Mean Speed (m/s)') + 
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# #inf results (OU)
# ggsave(last_plot(),  file="figures/moving_window_speed_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
# 
# 
# 
# ## Diffusion ----


ggplot(data = mw_dat) +
  geom_line(aes(x = date, y = diffusion_est_km2_day)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-10-26')), color = "#bb5566", linetype = "dotdash") +
  # facet_wrap2(~ collar_id, scales = "fixed", #sorted by ID & set axis so theyre the same for every plot
  #             ncol = 2, nrow = 3, strip = strip_col,
  #             labeller = labeller(collar_id = c('30548' = 'Goatzilla',
  #                                               '30561' = 'Selena Goatmez',
  #                                               '30575' = 'The Goatmother',
  #                                               '30613' = 'Goatileo',
  #                                               '30642' = 'Toats McGoats',
  #                                               '30648' = 'Vincent Van Goat'))) +
    scale_color_manual(values = setNames(goat_palette, goats)) +
  labs(x = '',
       y = expression('Diffusion (km'^2*'/day)')) +
  # scale_x_date(date_labels = "%b-%Y", date_breaks = "1 week") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))

# ggsave(last_plot(),  file="figures/moving_window_diffusion_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")

ggsave(last_plot(),  file="figures/window_analysis/window_diffusion_fire_period.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


# might have to log scale y axis


# # colour them by sex
# goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# # goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# sex_palette <- c("#4477AA", "#EE6677", "#EE6677", "#EE6677", "#EE6677", "#4477AA") # colours correspond with the goat order above
# 
# ggplot(data = mw_dat) +
#   geom_line(aes(x = date, y = diffusion_est_km2_day, colour = goat_name)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-10-26')), color = "black", linetype = "dotdash") +
#   labs(x = '',
#        y = expression('Diffusion (km'^2*'/day)')) +
#   scale_color_manual(values = setNames(sex_palette, goats)) +
#   scale_x_date(date_labels = "%b %d", date_breaks = "2 day") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# # check where the data is missing or outside of the scale range 
# mw_dat[is.na(mw_dat$date) | is.na(mw_dat$diffusion_est_km2_day), ] # all goatzilla, IID movement models
# 
# ggsave(last_plot(),  file="figures/window_analysis/window_diffusion_fire_period_sex.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
# 






#_____________________________________________________________________
# 3) Covariates ----

goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
covariates <- covariates[order(covariates$collar_id, covariates$window_start), ]
covariates$goat_color <- factor(covariates$collar_id, levels = goats) 
# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# create a goat colour column to assign their colour
covariates$goat_color <- goat_palette[match(covariates$goat_name, goats)]


# goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "black" )  # colours correspond with the goat order above
# set the strip colours in the facet wrap
strip_col <- strip_themed(background_x = 
                            elem_list_rect(fill = goat_palette))

## Elevation ----


# full scale
ggplot(data = covariates) +
  geom_line(aes(x = date, y = mean_elev)) +
  geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ collar_id, ncol = 2, nrow = 5, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Date',
       y = 'Mean elevation (m)') +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(last_plot(),  file="figures/moving_window_elevation_overtime.png",
       width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")



# # calendar scale
ggplot(data = mw_dat) +
  geom_line(aes(x = doy, y = mean_el, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dashed") +
  facet_wrap(~ goat_name, ncol = 1,
             nrow = 6, # sorted by ID
             scales = "fixed") +  #set axis so theyre the same for every plot
  labs(y = 'Mean elevation (m)',
       x = 'Month') +
  scale_x_continuous(limits = c(-5, 370),
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(name = "",
                     values = c("2019" = "#332288",
                                "2020" = "#ddaa33",
                                "2021" = "#006d2c",
                                "2022" = "#33bbee",
                                "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))

# ggsave(last_plot(),  file="figures/moving_window/fire_goats_moving_window_2_elevation_overtime_calendar_scale.png",
#        width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")
# 







# fire period for all years, using collar_id labels
# plot_el <-
  # ggplot(data = covariates) +
ggplot(data = mw_dat) +
  geom_line(aes(x = month_day, y = mean_elev, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(y = 'Mean elevation (m)',
       x = '') + 
  ggtitle("a)") +
  scale_x_date(date_breaks = "3 day", date_labels = "%b %d") +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))


plot_esc <-
  ggplot(data = covariates) +
  geom_line(aes(x = month_day, y = mean_dist_escape, group = year, 
                colour = ifelse(year == 2023, "2023", "other"),
                alpha = ifelse(year != 2023, 0.2, 1))) +
  facet_wrap2(~ collar_id, scales = "fixed", 
              ncol = 2, nrow = 3, strip = strip_col) +
  labs(y = 'Distance to escape terrain (m)',
       x = '') + 
  ggtitle("b)") +
  scale_x_date(date_breaks = "3 day", date_labels = "%b %d") +
  scale_colour_manual(values = c("2023" = "#e31a1c", "other" = "black")) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(color = "white", face = "bold"))




# Horizontal
plot_h <- grid.arrange(plot_el, plot_esc,
                       nrow = 2)
ggsave(plot_h, width = 8, height = 12, units = "in", dpi = 600, #bg = "transparent",
       # file="figures/home_range/fire_goat_hr_diff_h.png")
       file="figures/living_labs/figure5.png",)









#..............................
## Distance to escape terrain ----
# ggplot(data = mw_dat) +
#   geom_line(aes(x = date, y = mean_dist_escape)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   labs(x = 'Date',
#        y = 'Mean distance to escape terrain (m)') + 
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# ggsave(last_plot(),  file="figures/moving_window_dist_escape_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")

# calendar scale
ggplot(data = mw_dat) +
  geom_line(aes(x = doy, y = mean_dist_escape, color = as.factor(year), group = as.factor(year))) +
  geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dotdash") +
  facet_wrap(~ goat_name, ncol = 1, 
             # nrow = 6, # sorted by ID
             scales = "fixed" ) +  #set axis so theyre the same for every plot
  labs(x = 'Month',
       y = 'Mean distance to escape terrain (m)') + 
  scale_x_continuous(limits = c(-5, 370), 
                     expand = c(0, 0), # Full year
                     breaks = month_breaks, # Approximate month starts
                     labels = month.abb) + # Month abbreviations
  scale_color_manual(name = "", 
                     values = c("2019" = "#332288",
                                "2020" = "#ddaa33",
                                "2021" = "#006d2c",
                                "2022" = "#33bbee",
                                "2023" = 'black')) +
  guides(linetype = "none") +  # Remove linetype legend
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))


ggsave(last_plot(),  file="figures/moving_window/fire_goats_moving_window_3_dist_escape_overtime_calendar_scale.png",
       width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")

#................................................
## Slope ----
# ggplot(data = mw_dat) +
#   geom_line(aes(x = date, y = mean_slope)) +
#   geom_vline(xintercept = as.Date(c('2023-07-22', '2023-09-30')), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   labs(x = 'Date',
#        y = 'Mean slope (m)') + 
#   scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") + 
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# ggsave(last_plot(),  file="figures/moving_window_slope_overtime.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")
# 
# 
# #calendar scale
# ggplot(data = mw_dat) +
#   geom_line(aes(x = doy, y = mean_slope, color = as.factor(year), group = as.factor(year))) +
#   geom_vline(xintercept = c(203, 273), color = "#bb5566", linetype = "dotdash") +
#   facet_wrap(~ ID, ncol = 2, nrow = 6, # sorted by ID
#              scales = "fixed", ) +  #set axis so theyre the same for every plot
#   labs(x = 'Month',
#        y = 'Slope') + 
#   scale_x_continuous(limits = c(-5, 340), 
#                      expand = c(0, 0), # Full year
#                      breaks = month_breaks, # Approximate month starts
#                      labels = month.abb) + # Month abbreviations
#   scale_color_manual(name = "", 
#                      values = c("2019" = "#332288",
#                                 "2020" = "#ddaa33",
#                                 "2021" = "#006d2c",
#                                 "2022" = "#33bbee",
#                                 "2023" = 'black')) +
#   guides(linetype = "none") +  # Remove linetype legend
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
#         legend.position = "top",
#         legend.direction = "horizontal",
#         legend.justification = "center")
# 
# 
# ggsave(last_plot(),  file="figures/moving_window_slope_overtime_calendar_scale.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")


#_____________________________________________________________________
# 4) Centroid ----

# 
# 
# ggplot(data = mw_dat) +
#   geom_point(aes(x = centroid.long, y = centroid.lat, color = as.factor(year))) +
#   facet_wrap(~ ID, ncol = 2, nrow = 5, # sorted by ID
#              scales = "fixed" ) +  #set axis so theyre the same for every plot
#   ggtitle('Centroid') +
#   labs(x = 'Longitude',
#        y = 'Latitude') + 
#   scale_color_manual(name = "Year", values = c("2019" = "#332288",
#                                                "2020" = "#ddaa33",
#                                                "2021" = "#006d2c",
#                                                "2022" = "#33bbee",
#                                                "2023" = 'black')) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
#         plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"))
# 
# ggsave(last_plot(),  file="figures/moving_window_centroid_location.png",
#        width = 12, height = 6, units = "in", dpi = 600, bg = "transparent")






#......................................................................
# single goat all years ----

# mw_dat2 <- mw_dat[mw_dat$goat_name == "vincent_van_goat",]

# Get unique goat names
goat_names <- unique(mw_dat$goat_name)

# Loop through each unique goat_name
for (goat in goat_names) {
  mw_dat2 <- mw_dat[mw_dat$goat_name == goat, ]
  
  # plot
  p <-
    ggplot(data = mw_dat2) +
    geom_point(aes(x = doy, y = hr_est_km, color = as.factor(year), group = as.factor(year))) +
    # geom_smooth(aes(x = doy, y = hr_est, color = as.factor(year), group = as.factor(year), linetype = 'solid'),  alpha = 0.5) +
    geom_vline(xintercept = c(203, 299), color = "#bb5566", linetype = "dashed") +
    labs(y = expression("95% Home Range Area Estimate (km"^2*")")) + 
    facet_wrap(~ year, 
               ncol = 1, nrow = 6, # sorted by ID
               scales = "fixed", ) +  #set axis so theyre the same for every plot
    ggtitle(paste(mw_dat2$goat_name)) +
    # scale_y_continuous(name = expression("95% Home Range Area Estimate (km"^2*")")) +
    scale_y_log10() +
    scale_x_continuous(name = 'Month',
                       limits = c(-5, 370), 
                       expand = c(0, 0), # Full year
                       breaks = month_breaks, # Approximate month starts
                       # breaks = c(0, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), # Approximate month starts
                       labels = month.abb) + # Month abbreviations
    scale_color_manual(#name = "Year", 
      name = "", 
      values = c("2019" = "#332288",
                 "2020" = "#ddaa33",
                 "2021" = "#006d2c",
                 "2022" = "#33bbee",
                 "2023" = 'black')) +
    guides(linetype = "none") +  # Remove linetype legend
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 14, family = "sans", face = "bold"),
          plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "cm"),
          legend.position = "none")
  
  
  
  ggsave(filename = paste0("figures/moving_window/individual/mw_1_hr_overtime_calendar_", goat, ".png"),
         plot = p,
         width = 12, height = 15, units = "in", dpi = 600, bg = "transparent")
}
