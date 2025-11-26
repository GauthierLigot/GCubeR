#' Volume Estimation Using the Algan Method
#'
#' Computes aerial total volume (`algan_vta`) and merchantable volume (`algan_vmerch`)
#' according to the Algan method. The function validates input data, ensures
#' required columns are present and applies formulas only to compatible species.
#'
#' @param data A data frame containing tree measurements. Must include:
#'   - `species_code`: species name in uppercase Latin format (e.g. `"ABIES_ALBA"`).
#'   - `dbh`: diameter at breast height (cm).
#'   - `htot`: total tree height (m).
#'
#' @return A data frame with the original input columns plus two new outputs:
#' - `algan_vta`: aerial total volume (m³). Computed only for `"ABIES_ALBA"`, `NA` otherwise.
#' - `algan_vmerch`: merchantable volume (m³). Computed only for compatible species
#'   (`ABIES_ALBA`, `PICEA_ABIES`, `ALNUS_GLUTINOSA`, `PRUNUS_AVIUM`, `BETULA_SP`),
#'   `NA` otherwise.
#'
#' @details
#' - Input `dbh` must be in centimeters (cm). The function converts it internally to meters.
#' - Input `htot` must be in meters (m).
#' - Formula for aerial total volume (only `"ABIES_ALBA"`):
#'   \deqn{algan_vta = 0.4 * (dbh/100)^2 * htot}
#' - Formula for merchantable volume (compatible species):
#'   \deqn{algan_vmerch = 0.33 * (dbh/100)^2 * htot}
#'   - Domain of application:
#'   - For `"ABIES_ALBA"` and `"PICEA_ABIES"`, the Algan method is valid only if `dbh > 15 cm`.
#'   - For other compatible species (`ALNUS_GLUTINOSA`, `PRUNUS_AVIUM`, `BETULA_SP`), no minimum dbh threshold is applied.
#' - Resulting volumes are expressed in cubic meters (m³).
#' - If required columns are missing or non-numeric, the function stops with an error.
#' - Both output columns are always created to ensure consistency for downstream functions.
#'   
#' @importFrom dplyr
#' 
#' @examples
#' df <- data.frame(
#'   species_code = c("ABIES_ALBA", "PICEA_ABIES", "BETULA_SP", "QUERCUS_ROBUR"),
#'   dbh = c(30, 25, 20, 40),   # cm
#'   htot = c(20, 18, 15, 22)   # m
#' )
#' algan_vta_vmerch(df)
#'
#' @export
 
# VOLUME CALCULATION WITH ALGAN METHOD ----
algan_vta_vmerch <- function(data) {
  
  # INPUT CHECKS ----
  ## Required columns ----
  required_cols <- c("species_code", "dbh", "htot")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  ## Validate numeric inputs ----
  if (!is.numeric(data$dbh)) stop("'dbh' must be numeric (cm).")
  if (!is.numeric(data$htot)) stop("'htot' must be numeric (m).")
  
  ## Clean species names ----
  data <- data %>% dplyr::mutate(species_code = toupper(trimws(species_code)))
  
  ## Define compatible species ----
  merch_species <- c("ABIES_ALBA", "PICEA_ABIES", "ALNUS_GLUTINOSA", "PRUNUS_AVIUM", "BETULA_SP")
  
  ## Identify incompatible species ----
  incompatible <- setdiff(unique(data$species_code), c("ABIES_ALBA", merch_species))
  if (length(incompatible) > 0) {
    message("⚠️ Algan method not defined for species: ", paste(incompatible, collapse = ", "))
  }
  
  ## DOMAIN CHECK ----
  rows_outside_domain <- which(data$species_code %in% c("ABIES_ALBA", "PICEA_ABIES") & data$dbh <= 15)
  if (length(rows_outside_domain) > 0) {
    message("⚠️ The following rows are outside the domain of application (dbh ≤ 15 cm for ABIES_ALBA or PICEA_ABIES): ",
            paste(rows_outside_domain, collapse = ", "))
  }
  
  # CONSTANTS ----
  coef_vmerch = 0.33
  coef_vta = 0.4
  
  # VMERCH: create only if at least one compatible row ----
  vmerch_idx <- which(
    (data$species_code %in% merch_species) &
      !(data$species_code %in% c("ABIES_ALBA", "PICEA_ABIES") & data$dbh <= 15)
  )
  
  if (length(vmerch_idx) > 0) {
    data$algan_vmerch <- NA_real_
    data$algan_vmerch[vmerch_idx] <- coef_vmerch * ( (data$dbh[vmerch_idx] / 100)^2 ) * data$htot[vmerch_idx]
  } else {
    message("⚠️ No compatible species found for Algan merchantable volume (vmerch). No column created.")
  }
  
  # VTA: create only if at least one compatible row ----
  vta_idx <- which(data$species_code == "ABIES_ALBA" & data$dbh > 15)
  
  if (length(vta_idx) > 0) {
    data$algan_vta <- NA_real_
    data$algan_vta[vta_idx] <- coef_vta * ( (data$dbh[vta_idx] / 100)^2 ) * data$htot[vta_idx]
  } else {
    message("⚠️ No compatible species found for Algan aerial total volume (vta). No column created.")
  }
  
  return(data)
}