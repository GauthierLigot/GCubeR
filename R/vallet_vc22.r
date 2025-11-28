#' @title Calculate Commercial Volume (vc22) up to 7cm Diameter at Breast Height (dbh)
#' 
#' @description Computes the commercial wood volume (vc22, over bark, up to a 7 cm top-diameter) 
#'   using the Vallet polynomial model, based on dbh (cm) and htot (m).
#'
#' @param data A data frame containing tree measurements. Must include the columns:
#'   \code{species_code}, \code{dbh} (diameter at 1.30m, in cm), and \code{htot} (total height, in m).
#' @param na_action How to handle missing input values. \code{"error"} (default) stops if 
#'   core required values are explicitly \code{NA}. \code{"omit"} removes rows with missing core data. 
#' @param output Optional file path (string). If provided, the resulting data frame
#'   will be written to this file using semicolon (;) as a delimiter. NA values are
#'   written as empty strings (""). Defaults to \code{NULL}.
#'
#' @return The resulting data frame with the new column \code{vallet_vc22} 
#'   (Commercial Volume in **m³**).
#'
#' @details
#' The model is valid only for trees with a diameter at 1.30m (\code{dbh}) 
#' **greater than or equal to 7 cm**.
#' 
#' The polynomial formula used is:
#' $$\text{VC22}_{\text{dm³}} = a \cdot \frac{h_{tot}}{\text{dbh}} + (b + c \cdot \text{dbh}) \cdot \left(\frac{\pi \cdot \text{dbh}^2 \cdot h_{tot}}{40}\right)$$
#'
#' @import dplyr readr
#'
#' @examples
#' data_test_vc22 <- data.frame(
#'   species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "UNKNOWN_SPECIES", "QUERCUS_ROBUR"),
#'   dbh = c(19.1, 25.5, 15.9, 6.4), # dbh 6.4 cm is below 7 cm constraint (NA result)
#'   htot = c(25, 18, 20, 22)
#' )
#' 
#' # Expect negative/unrealistic results due to coefficient incompatibility
#' results_console <- vallet_vc22(data_test_vc22)
#' print(results_console)
#'
#' @export
vallet_vc22 <- function(data,
                        na_action = c("error", "omit"),
                        output = NULL) {
  
  na_action <- match.arg(na_action)
  
  min_dbh <- 7 
  rows_to_invalidate <- numeric(0)
  
  # INPUT CHECKS & DATA PREPARATION ----
  required_columns <- c("species_code", "dbh", "htot") 
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("Input data is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  ## Check DBH constraint (>= 7 cm) ----
  rows_too_small <- which(data$dbh < min_dbh & !is.na(data$dbh)) 
  
  if (length(rows_too_small) > 0) {
    warning(paste0("Diameter (dbh) constraint violated: ", length(rows_too_small), 
                   " tree(s) have dbh < ", min_dbh, " cm. vc22 will be set to NA for these rows: ",
                   paste(rows_too_small, collapse = ", ")), call. = FALSE)
    
    rows_to_invalidate <- c(rows_to_invalidate, rows_too_small)
  }
  
  ## Clean species names and join ----
  data <- data %>%
    mutate(species_code = toupper(trimws(species_code))) %>%
    left_join(val_vc22, by = "species_code")
  
  ## Check for unknown species (missing coefficients) ----
  rows_unknown_species <- which(is.na(data$coeff_b))
  
  if (length(rows_unknown_species) > 0) {
    wrong_species <- data[rows_unknown_species, ] %>%
      pull(species_code) %>%
      unique()
    warning("Unknown species (missing vc22 coefficients): ", paste(wrong_species, collapse = ", "),
            ". vc22 will be set to NA for these rows.", call. = FALSE)
  }
  
  rows_to_invalidate <- unique(c(rows_to_invalidate, rows_unknown_species))
  
  ## If no compatible rows, skip calculation ----
  compatible_idx <- which(!is.na(data$coeff_b) & data$dbh >= min_dbh)
  if (length(compatible_idx) == 0) {
    message("⚠️ No compatible species found for Vallet merchantable volume (vc22). No column created.")
    return(data %>% select(-starts_with("coeff_")))
  }
  
  # VC22 CALCULATION ----
  
  data <- data %>%
    mutate(
      # vc22_dm3 = a*(htot/dbh) + ((b + c*dbh) * (pi*dbh^2*htot/40)) 
      
      term1_a = coeff_a * (htot / dbh),
      
      term2_bc = (coeff_b + (coeff_c * dbh)),
      
      term3_geom = (pi * (dbh^2) * htot) / 40, 
      
      vc22_dm3 = term1_a + (term2_bc * term3_geom),
      
      # Convert to m³ (Final Unit)
      vallet_vc22 = vc22_dm3 / 1000 # dm³ -> m³
    ) %>%
    # Remove temporary columns
    select(-starts_with("coeff_"), -vc22_dm3, -starts_with("term"))
  
  # FINAL STEP: Set VC22 to NA for all identified invalid rows
  if (length(rows_to_invalidate) > 0) {
    data$vallet_vc22[rows_to_invalidate] <- NA
  }
  
  # OUTPUT ----
  if (!is.null(output)) {
    write_delim(data, file = output, delim = ";", na = "", col_names = TRUE)
    message("File written: ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  
  return(data)
}