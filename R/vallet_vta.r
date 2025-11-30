#' @title Calculate Total Aboveground Volume (VTA) Vallet Method 
#' 
#' @description Computes the total aboveground volume (VTA) for trees based on
#'   the circumference at 1.30m (c130) and total height (htot) using the
#'   Vallet form factor method.
#'
#' @param data A data frame containing tree measurements. Must include the columns:
#'   \code{species_code} (in uppercase Latin format), \code{c130} (circumference at 1.30m, in cm), 
#'   and \code{htot} (total height, in m).
#' @param na_action How to handle missing input values. \code{"error"} (default) stops if 
#'   core required values are explicitly \code{NA}. \code{"omit"} removes rows with missing core data. 
#'   Note: Model constraint violation (\code{c130} < 45 cm) and unknown species are always
#'   handled by setting VTA and Form Factor to NA, preserving input values.
#' @param output Optional file path (string). If provided, the resulting data frame
#'   will be written to this file using semicolon (;) as a delimiter. NA values are
#'   written as empty strings (""). Defaults to \code{NULL}.
#'
#' @return The resulting data frame (same as the printed data) with the new columns and \code{vallet_vta} (Total Aboveground Volume in **m3**)..
#'
#' @details
#' The model is only valid for trees with a circumference at 1.30m (\code{c130}) of at least 45 cm.
#' For non-compliant trees or unknown species, results are set to \code{NA}.
#'
#' The Form Factor (\code{form}) is calculated as:
#' \deqn{form = (a + b \cdot c_{130} + c \cdot \frac{\sqrt{c_{130}}}{h_{tot}}) \cdot \left(1 + \frac{d}{c_{130}^2}\right)}
#'
#' The Total Aboveground Volume (\code{VTA}) is then:
#' \deqn{VTA = form \cdot \frac{1}{\pi \cdot 40000} \cdot c_{130}^2 \cdot h_{tot}}
#'
#' Coefficients a, b, c, d are loaded from the \code{vallet_vta.csv} file.
#' 
#' @section Supported species:
#' The Vallet VTA method is currently implemented for the following species
#' (via their \code{species_code}):
#' \itemize{
#'   \item \code{"PICEA_ABIES"}
#'   \item \code{"QUERCUS_ROBUR"}
#'   \item \code{"FAGUS_SYLVATICA"}
#'   \item \code{"PINUS_SYLVESTRIS"}
#'   \item \code{"PINUS_PINASTER"}
#'   \item \code{"ABIES_ALBA"}
#'   \item \code{"PSEUDOTSUGA_MENZIESII"}
#' }
#' 
#' @import dplyr readr
#'
#' @examples
#' data_test <- data.frame(
#'   species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "UNKNOWN_SPECIES", "QUERCUS_ROBUR"),
#'   c130 = c(60, 80, 50, 40), 
#'   htot = c(25, 18, 20, 22)
#' )
#' 
#' # Case 1: Print results to console (default)
#' results_console <- vallet_vta(data_test)
#' 
#' # Case 2: Write results to a file
#' # vallet_vta(data_test, output = "vta_results.csv")
#'
#' @export
vallet_vta <- function(data,
                       na_action = c("error", "omit"),
                       output = NULL) { 
  
  na_action <- match.arg(na_action)
  
  # MODEL VALIDATION CONSTANT
  min_c130 <- 45
  rows_to_invalidate <- numeric(0)
  
  # INPUT CHECKS & DATA PREPARATION ----
  required_columns <- c("species_code", "c130", "htot")
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("Input data is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  ## Check c130 constraint (>= 45 cm) ----
  rows_too_small <- which(data$c130 < min_c130 & !is.na(data$c130))
  
  if (length(rows_too_small) > 0) {
    warning(paste0("Circumference (c130) constraint violated: ", length(rows_too_small), 
                   " tree(s) have c130 < ", min_c130, " cm. Form factor and VTA will be set to NA for these rows: ",
                   paste(rows_too_small, collapse = ", ")), call. = FALSE)
    
    rows_to_invalidate <- c(rows_to_invalidate, rows_too_small)
  }
  
  ## Clean species names and join ----
  data <- data %>%
    mutate(species_code = toupper(trimws(species_code))) %>%
    left_join(val_vta, by = "species_code") %>%
    mutate(
      coeff_a = suppressWarnings(as.numeric(coeff_a)),
      coeff_b = suppressWarnings(as.numeric(coeff_b)),
      coeff_c = suppressWarnings(as.numeric(coeff_c)),
      coeff_d = suppressWarnings(as.numeric(coeff_d))
    )
  
  
  ## Check for unknown species (missing coefficients) ----
  rows_unknown_species <- which(is.na(data$coeff_a))
  
  if (length(rows_unknown_species) > 0) {
    wrong_species <- data[rows_unknown_species, ] %>%
      pull(species_code) %>%
      unique()
    warning("Unknown species (missing vta coefficients): ", paste(wrong_species, collapse = ", "),
            ". Form factor and vta will be set to NA for these rows.", call. = FALSE)
  }
  
  # Combine all rows where the VTA/Form calculation must be NA (unknown species + c130 constraint)
  rows_to_invalidate <- unique(c(rows_to_invalidate, rows_unknown_species))
  
  ## If no compatible rows, skip calculation ----
  compatible_idx <- which(!is.na(data$coeff_a) & data$c130 >= min_c130)
  if (length(compatible_idx) == 0) {
    message("No compatible species found for Vallet VTA method. No column created.")
    return(data %>% select(-starts_with("coeff_")))
  }
  
  # VTA CALCULATION ----
  
  data <- data %>%
    mutate(
      # Coerce NA coefficients to 0 for calculation
      a = coeff_a,
      b = coeff_b,
      c = coalesce(coeff_c, 0),
      d = coalesce(coeff_d, 0),
      
      # Step 1: Calculate Form Factor (form)
      term1_c = c * (sqrt(c130) / htot),
      term2_d = 1 + (d / (c130^2)),
      form = (a + (b * c130) + term1_c) * term2_d, 
      
      # Step 2: Calculate VTA
      vallet_vta = form * (1 /(pi *40000)) * (c130^2) * htot
    ) %>%
    # Remove temporary columns, coefficients, keeping 'vallet_vta'
    select(-starts_with("coeff_"), -a, -b, -c, -d, -starts_with("term"),-form)
  
  # Final step: Set vta and form factor to NA for all identified invalid rows
  if (length(rows_to_invalidate) > 0) {
    data$vallet_vta[rows_to_invalidate] <- NA
  }
  
  # exporting the file using function export_output ----
  export_output(data, output)
  return(data)
}