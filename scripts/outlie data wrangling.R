


#.........................................................................
# prep outlie results df ----
#.........................................................................

# obtain the outlie outputs for all individuals
outlie_results <- outlie(data = tel_data, plot = FALSE) # output is a list

# assign the id for each item in the list
names(outlie_results) <- names(tel_data)

# convert outlie output list into a df for cleaning
outlie_df <- do.call(rbind, lapply(names(outlie_results), function(x) {
  # extract df
  df  <- outlie_results[[x]]
  # record ID
  df$individual.local.identifier <- x
  return(df) # ends the function and spits out the outcome of whatever you did, some value, object, etc
}))

str(outlie_df)
outlie_df$individual.local.identifier <- as.factor(outlie_df$individual.local.identifier)

# convert unix time format back to calender time
outlie_df$timestamp_utc <- as.POSIXct(outlie_df$t, tz = "UTC")

# check for same column names to avoid duplication
intersect(colnames(raw_data), colnames(outlie_df))
intersect(colnames(combined_data), colnames(outlie_df))

# combine dfs
raw_data <- merge(combined_data, outlie_df, by = c("timestamp_utc", "individual.local.identifier", "t"), all.x = TRUE)

# reorganize the df based on combined_data structure
# check what columns match from the main df and order it in that way and what columns dont match are the leftovers and placed at the back/end of the df, 
raw_data <- raw_data[, c(intersect(names(combined_data), names(raw_data)), 
                         setdiff(names(raw_data), names(combined_data)))]

raw_data <- raw_data[order(raw_data$fix_id),]
# reset the row number to proper default
rownames(raw_data) <- NULL

#create a flagged outlier column and assign all to 0 to be flagged below, then to be updated in the outlier column once outlie filtering is complete. flag_outlier is the working column being used during the filtering process
raw_data$flag_outlier <- 0



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check for same column names to avoid duplication
intersect(colnames(combined_data), colnames(outlie_df))

# combine df
outlie_df <- merge(combined_data, outlie_df, by = c("timestamp_utc", "individual.local.identifier"), all.x = TRUE)

# reorganize the df based on combined_data structure
# check what columns match from the main df and order it in that way and what columns dont match are the leftovers and placed at the back/end of the df, 
outlie_df <- outlie_df[, c(intersect(names(combined_data), names(outlie_df)), 
                         setdiff(names(outlie_df), names(combined_data)))]

outlie_df <- outlie_df[order(outlie_df$fix_id),]
# reset the row number to proper default
rownames(outlie_df) <- NULL

#create a flagged outlier column and assign all to 0 to be flagged below, then to be updated in the outlier column once outlie filtering is complete. flag_outlier is the working column being used during the filtering process
outlie_df$flag_outlier <- 0


