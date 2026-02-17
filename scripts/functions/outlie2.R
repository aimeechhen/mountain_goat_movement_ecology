
# extending the outlie() function from the ctmm package

# the purpose of this function is to be able behave similar to as.telemetry, it expands the outlie() function to be able include columns and adds an argument to indicate the columns like in as.telemetry(keep =) because outlie() does not have this option available, so this allows for it. you get the outlie() output data and grab the columns that was indicated in as.telemetry() which contains fix_id column to flag outlier points based on its fix_id

# id = column to refer to as the identifier for the gps point to refer to when flagging outliers

# Additional information:
# output of outlie()

# distance column = `core deviation' denotes distances from the median longitude & latitude
# the speed column = `minimum speed' denotes the minimum speed required to explain the location estimate's displacement as straight-line motion
# NOTE: The speed estimates here are tailored for outlier detection and have poor statistical efficiency.

#............................................................................
# outlie data ----
#............................................................................

outlie2 <- function(telemetry, id = "id") {
  # check if telemetry object contains a id column
  if (!id %in% colnames(telemetry)) {
    stop(paste0("error: telemetry must have a column named '", id, "'"))
  }
  
  # run outlie() as usual but only for a single telemetry object, not a list, list hasn't been tested despite outlie() works on a list
  out <- outlie(telemetry, plot = FALSE)
  
  # combine df that contains both telemetry and outlie output to record flag outliers
  # extract rownames to be able to merge by (rownames needs to be configured prior to ensure that the matching works and is correct)
  telemetry$row_n <- rownames(telemetry)
  out$row_n <- rownames(out)
  
  # check if the rownames match (rowname is the representation of fix_id since outlie() doesn't have the keep argument to carry the information over, assuming it has been configured properly and before)
  if (!identical(rownames(out), rownames(telemetry))) {
    stop("rownames are not the same")
  }
  
  # check if the t value matches in both
  if (!all(out$t == telemetry$t)) {
    stop(paste0("t does not match at row(s): ", paste(which(out$t != telemetry$t), collapse = ", ")))
  }
  
  # check for duplicate columns
  # intersect(colnames(out), colnames(telemetry))
  
  # if the two checks above are true, then cbind the columns while avoiding duplicating columns
  output <- cbind(telemetry[, setdiff(names(telemetry), names(out)), drop = FALSE], out)
  
  # check if rowname and fix_id matches, if not, list the rows that arent
  if (!all(output$row_n == output[[id]])) {
    not_matching <- which(output$row_n != output[[id]])
    stop(paste0("row_n (outlie) and ", id, " (telemetry) not matching at row(s): ",
                paste(not_matching, collapse = ", ")))
  }
  
  output$flag_outlier <- 0
  # end the function and get the output
  return(output)  # ends the function and spits out the outcome of whatever you did, some value, object, etc
  
}