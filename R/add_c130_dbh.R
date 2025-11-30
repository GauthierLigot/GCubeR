#' Add or compute c130 and dbh columns
#'
#' Ensures that both `c130` (circumference at 1.30 m) and `dbh` (diameter at 1.30 m)
#' are present in the dataset. If one is missing, it is computed from the other.
#'
#' @param data A data frame containing tree measurements.
#'   Must include at least one of the following columns:
#'   - `c130`: circumference at 1.30 m (cm)
#'   - `dbh`: diameter at 1.30 m (cm)
#' @param output Optional file path where the resulting data frame should be
#'   exported as a CSV. If NULL (default), no file is written.
#'
#' @return The same data frame with both `c130` and `dbh` columns.
#'   Note: the function does not modify the input data frame in place.
#'   To update your object, you must reassign the result, e.g.:
#'   `data2 <- add_c130_dbh(data2)`
#'   
#' @details
#' - This function should be used at the very beginning of the workflow
#'   to ensure both `c130` and `dbh` columns are available for subsequent functions.
#' - Conversion uses the formula: `dbh = c130 / pi` and `c130 = dbh * pi`.
#' - Units are centimeters (cm).
#' - If both columns are present, values are left unchanged.
#'
#' @examples
#' data <- data.frame(c130 = c(31.4, 62.8))
#' data <- add_c130_dbh(data)
#'
#' data2 <- data.frame(dbh = c(10, 20))
#' data2 <- add_c130_dbh(data2)
#'
#' @export
#' 
# FUNCTION ----
add_c130_dbh <- function(data, output = NULL) {
  # Check that input is a data.frame
  stopifnot(is.data.frame(data))
  
  # Case 1: neither column exists
  if (!("c130" %in% names(data)) && !("dbh" %in% names(data))) {
    stop("Data must contain either a 'c130' or a 'dbh' column.")
  }
  
  # Case 2: c130 exists but not dbh
  if ("c130" %in% names(data) && !"dbh" %in% names(data)) {
    data$dbh <- data$c130 / pi
    message("dbh' column added from 'c130'.")
  }
  
  # Case 3: dbh exists but not c130
  if ("dbh" %in% names(data) && !"c130" %in% names(data)) {
    data$c130 <- data$dbh * pi
    message("c130' column added from 'dbh'.")
  }
  
  # Case 4: both columns exist
  if ("dbh" %in% names(data) && "c130" %in% names(data)) {
    # Fill missing c130 values from dbh
    idx_c130_na <- which(is.na(data$c130) & !is.na(data$dbh))
    if (length(idx_c130_na) > 0) {
      data$c130[idx_c130_na] <- data$dbh[idx_c130_na] * pi
      message(length(idx_c130_na), "'c130' values filled from 'dbh'")
    }
    
    # Fill missing dbh values from c130
    idx_dbh_na <- which(is.na(data$dbh) & !is.na(data$c130))
    if (length(idx_dbh_na) > 0) {
      data$dbh[idx_dbh_na] <- data$c130[idx_dbh_na] / pi
      message(length(idx_dbh_na), "'dbh' values filled from 'c130'")
    }
  }
  
  # exporting the file using function export_output ----
  export_output(data, output)
  return(data)
}