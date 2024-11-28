

# How to check if there are missing dates in your dataframe by counting the number of recordings and the number of days and comparing them

# Convert 'timestamp' to Date format
tel.dat$date <- as.Date(tel.dat$timestamp)

# Get a list of unique dates for each collar ID
date_counts <- aggregate(date ~ individual.local.identifier, tel.dat, FUN = function(x) length(unique(x)))

# Get the total number of unique dates in the dataset for each collar ID
total_dates <- aggregate(date ~ individual.local.identifier, tel.dat, FUN = function(x) length(unique(x)))

# Merge the two data frames to compare if there is at least one timestamp for every day for each collar ID
result <- merge(date_counts, total_dates, by = "individual.local.identifier", suffixes = c("_count", "_total"))

# Check completeness
result$complete <- result$date_count == result$date_total

# Print results
print(result)



#...................................................
# the number of dates counted are not the same for all the collars

#How to check dates which are missing/duplicated and for each collar

# Get unique collar IDs
collar_ids <- unique(tel.dat$individual.local.identifier)

# Loop through each collar ID and check for missing or duplicated dates
for (id in collar_ids) {
  # Subset data for the current collar ID
  subset_data <- tel.dat[tel.dat$individual.local.identifier == id, ]
  
  # Get unique dates for the current collar ID
  unique_dates <- unique(subset_data$date)
  
  # Check for missing dates
  missing_dates <- setdiff(seq(min(unique_dates), max(unique_dates), by = "day"), unique_dates)
  
  # Convert missing dates back to Date format
  missing_dates <- as.Date(missing_dates, origin = "1970-01-01")
  
  # Check for duplicated dates
  duplicated_dates <- unique_dates[duplicated(unique_dates)]
  
  # Convert duplicated dates back to Date format
  duplicated_dates <- as.Date(duplicated_dates, origin = "1970-01-01")
  
  # Print results
  cat("Collar ID:", id, "\n")
  cat("Missing Dates:", ifelse(length(missing_dates) > 0, paste(missing_dates, collapse = ", "), "None"), "\n")
  cat("Duplicated Dates:", ifelse(length(duplicated_dates) > 0, paste(duplicated_dates, collapse = ", "), "None"), "\n\n")
}

# Collar ID: 30613 
# Missing Dates: 2023-03-12 
# 
# Collar ID: 30642 
# Missing Dates: 2023-09-23, 2023-09-24, 2023-09-25, 2023-09-26, 2023-09-27 