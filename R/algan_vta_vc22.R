#' Volume Estimation Using the Algan Method
#'
#' Computes aerial total volume (`algan_vta`) and merchantable volume (`algan_vc22`)
#' according to the Algan method. The function validates input data, ensures
#' required columns are present and applies formulas only to compatible species.
#'
#' @param data A data frame containing tree measurements. Must include:
#'   - `species_code`: species name in uppercase Latin format (e.g. `"ABIES_ALBA"`).
#'   - `dbh`: diameter at breast height (cm).
#'   - `htot`: total tree height (m).
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If NULL (default), no file is written.
#'   Export is handled by the utility function \code{export_output()} and
#'   failures trigger warnings without interrupting execution.
#'
#' @return A data frame with the original input columns plus two new outputs:
#' - `algan_vta`: aerial total volume (m³). Computed only for `"ABIES_ALBA"`, `NA` otherwise.
#' - `algan_vc22`: merchantable volume (m³). Computed only for compatible species
#'   (`ABIES_ALBA`, `PICEA_ABIES`, `ALNUS_GLUTINOSA`, `PRUNUS_AVIUM`, `BETULA_SP`),
#'   `NA` otherwise.
#'
#' @details
#' - Input `dbh` must be in centimeters (cm). The function converts it internally to meters.
#' - Input `htot` must be in meters (m).
#' - Formula for aerial total volume (only `"ABIES_ALBA"`):
#'   \deqn{algan_vta = 0.4 * (dbh/100)^2 * htot}
#' - Formula for merchantable volume (compatible species):
#'   \deqn{algan_vc22 = 0.33 * (dbh/100)^2 * htot}
#'   - Domain of application:
#'   - For `"ABIES_ALBA"` and `"PICEA_ABIES"`, the Algan method is valid only if `dbh > 15 cm`.
#'   - For other compatible species (`ALNUS_GLUTINOSA`, `PRUNUS_AVIUM`, `BETULA_SP`), no minimum dbh threshold is applied.
#' - Resulting volumes are expressed in cubic meters (m³).
#' - If required columns are missing or non-numeric, the function stops with an error.
#' - Both output columns are always created to ensure consistency for downstream functions.
#'   
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' 
#' @examples
#' df <- data.frame(
#'   species_code = c("ABIES_ALBA", "PICEA_ABIES", "BETULA_SP", "QUERCUS_ROBUR"),
#'   dbh = c(30, 25, 20, 40),   # cm
#'   htot = c(20, 18, 15, 22)   # m
#' )
#' algan_vta_vc22(df)
#'
#' @export
 
# VOLUME CALCULATION WITH ALGAN METHOD ----
algan_vta_vc22 <- function(data,
                           output = NULL) {
  
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
  vc22_species <- c("ABIES_ALBA", "PICEA_ABIES", "ALNUS_GLUTINOSA", "PRUNUS_AVIUM", "BETULA_SP")
  
  ## Identify incompatible species ----
  incompatible <- setdiff(unique(data$species_code), c("ABIES_ALBA", vc22_species))
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
  coef_vc22 = 0.33
  coef_vta = 0.4
  
  # Initialiser les colonnes de sortie à NA_real_ ----
  data$algan_vc22 <- NA_real_
  data$algan_vta  <- NA_real_
  
  # vc22 ----
  vc22_idx <- which(
    (data$species_code %in% vc22_species) &
      !(data$species_code %in% c("ABIES_ALBA", "PICEA_ABIES") & data$dbh <= 15)
  )
  
  if (length(vc22_idx) > 0) {
    data$algan_vc22[vc22_idx] <- coef_vc22 * ((data$dbh[vc22_idx] / 100)^2) * data$htot[vc22_idx]
  } else {
    message("⚠️ No compatible species found for Algan merchantable volume (vc22). All values are NA.")
  }
  
  # vta ----
  vta_idx <- which(data$species_code == "ABIES_ALBA" & data$dbh > 15)
  
  if (length(vta_idx) > 0) {
    data$algan_vta[vta_idx] <- coef_vta * ((data$dbh[vta_idx] / 100)^2) * data$htot[vta_idx]
  } else {
    message("⚠️ No compatible species found for Algan aerial total volume (vta). All values are NA.")
  }
  
  # exporting the file using function export_output ----
  export_output(data, output)
  
  return(data)
}