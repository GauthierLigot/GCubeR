#' Volume Estimation Using the Bouvard Method
#'
#' Computes aerial total volume (`bouvard_vta`) according to the Bouvard method.
#' The function validates input data, ensures required columns are present,
#' and applies the formula only to species `"QUERCUS_SP"`.
#'
#' @param data A data frame containing tree measurements. Must include:
#'   - `species_code`: species name in uppercase Latin format (e.g. `"QUERCUS_SP"`).
#'   - `dbh`: diameter at breast height (cm).
#'   - `htot`: total tree height (m).
#'
#' @return A data frame with the original input columns plus one new output:
#' - `bouvard_vta`: aerial total volume (m³). Computed only for `"QUERCUS_SP"`,
#'   otherwise not created.
#'
#' @details
#' - Input `dbh` must be in centimeters (cm). The function converts it internally to meters.
#' - Input `htot` must be in meters (m).
#' - Formula for aerial total volume (only `"QUERCUS_SP"`):
#'   \deqn{bouvard_vta = 0.5 * (dbh/100)^2 * htot}
#' - Resulting volumes are expressed in cubic meters (m³).
#' - If required columns are missing or non-numeric, the function stops with an error.
#' - The output column is created only if at least one `"QUERCUS_SP"` row is present,
#'   otherwise a warning message is displayed and no column is added.
#'
#' @importFrom dplyr mutate select left_join
#'
#' @examples
#' df <- data.frame(
#'   species_code = c("QUERCUS_SP", "PICEA_ABIES", "FAGUS_SYLVATICA"),
#'   dbh = c(30, 25, 40),   # cm
#'   htot = c(20, 18, 22)   # m
#' )
#' bouvard_vta(df)
#'
#' @export
bouvard_vta <- function(data,
                        na_action = c("error", "omit"),
                        output = NULL) {
  
  # INPUT CHECKS ----
  required_cols <- c("species_code", "dbh", "htot")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.numeric(data$dbh)) stop("'dbh' must be numeric (cm).")
  if (!is.numeric(data$htot)) stop("'htot' must be numeric (m).")
  
  # Clean species names ----
  data <- data %>% dplyr::mutate(species_code = toupper(trimws(species_code)))
  
  # Define compatible species ----
  compatible_species <- c("QUERCUS_SP")
  
  # Identify compatible rows ----
  idx <- which(data$species_code %in% compatible_species)
  
  # If no compatible species → message only ----
  if (length(idx) == 0) {
    message("⚠️ No compatible species found for Bouvard method (only QUERCUS_SP is supported). No volume column created.")
    return(data)
  }
  
  # CONSTANTS ----
  a <- 0.5   # coefficient
  
  # OUTPUT ----
  data$bouvard_vta <- NA_real_
  data$bouvard_vta[idx] <- a * ( (data$dbh[idx] / 100)^2 ) * data$htot[idx]
  
  return(data)
}