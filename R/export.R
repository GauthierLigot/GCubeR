#' Export data frame to CSV with warnings instead of errors
#'
#' This function exports a data.frame to a CSV file. If \code{output} is NULL,
#' nothing is done. If the path is invalid or the export fails, a warning is
#' issued but the function does not stop, and still returns (invisibly) a
#' logical value indicating success.
#'
#' @param data A data.frame to export.
#' @param output Character string: path to the CSV file. If NULL, nothing is done.
#'
#' @return Invisibly returns \code{TRUE} if the export succeeded, \code{FALSE}
#'   otherwise.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = 1:3,
#'   volume = c(10.5, 12.3, 9.8)
#' )
#'
#' # Export to CSV :
#' export_output(df, "results/volumes.csv")
#'

export_output <- function(data, output) {
  
  # no output ----
  if (is.null(output)) {
    # invisible allows to have values without printing it , here FALSE value
    return(invisible(FALSE))
  }
  
  # file path verification ----
  # verification of the output class and length
  if (!is.character(output) || length(output) != 1) {
    warning("Parameter 'output' must be a single string giving the CSV file path.")
    return(invisible(FALSE))
  }
  
  # try to export the df ----
  # tryCatch manages errors during write.csv; if an error occurs, we warn and return FALSE
  success <- tryCatch({
    write.csv2(data, file = output, row.names = FALSE)
    message("File successfully written to: ", output)
    TRUE
  # load the error message in e
  }, error = function(e) {
    warning("Failed to write CSV: ", e$message)
    FALSE
  })
  
  return(invisible(success))
}
