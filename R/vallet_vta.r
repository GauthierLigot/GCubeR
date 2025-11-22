#' @title Calculate Total Aboveground Volume (VTA)
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
#'
#' @return A data frame with the original data plus new columns for the calculated 
#'   \code{form} factor and the total volume \code{vta_vallet} (in m³).
#'
#' @details
#' The model is only valid for trees with a circumference at 1.30m (\code{c130}) of at least **45 cm**.
#' For non-compliant trees or unknown species, results are set to \code{NA}.
#' 
#' The Form Factor (\code{form}) is calculated as:
#' $$\text{form} = (a + b \cdot c_{130} + c \cdot \frac{\sqrt{c_{130}}}{h_{tot}}) \cdot (1 + \frac{d}{c_{130}^2})$$
#' The Total Aboveground Volume (\code{VTA}) is then:
#' $$\text{VTA} = \text{form} \cdot \frac{\pi}{40000} \cdot c_{130}^2 \cdot h_{tot}$$
#' Coefficients a, b, c, d are loaded from the \code{vallet_vta.csv} file.
#'
#' @import dplyr readr
#'
#' @examples
#' data_test <- data.frame(
#'   species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA", "UNKNOWN_SPECIES", "QUERCUS_ROBUR"),
#'   c130 = c(60, 80, 50, 40), # c130=40 is below 45cm constraint
#'   htot = c(25, 18, 20, 22)
#' )
#' 
#' # The function runs, and handles invalid/unknown data with NA and warnings
#' results <- vta_calc(data_test)
#' print(results)
#'
#' @export
vta_calc <- function(data,
                     na_action = c("error", "omit")) {
  
  na_action <- match.arg(na_action)
  
  # MODEL VALIDATION CONSTANT
  min_c130 <- 45
  rows_to_invalidate <- numeric(0) # Initialize vector to store indices of rows to invalidate
  
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
    
    # Store indices to set results to NA later
    rows_to_invalidate <- c(rows_to_invalidate, rows_too_small)
  }
  
  ## Load Coefficients Table ----
  path_coeffs <- file.path("data-raw", "vallet_vta.csv")
  
  vallet_coeffs <- tryCatch(
    read_delim(
      file = path_coeffs,
      delim = ";",
      locale = locale(decimal_mark = ",", encoding = "UTF-8"),
      col_types = cols_only(
        species_code = col_character(),
        coeff_a = col_double(),
        coeff_b = col_double(),
        coeff_c = col_double(),
        coeff_d = col_double()
      ),
      trim_ws = TRUE,
      show_col_types = FALSE
    ),
    error = function(e) {
      stop("Failed to load coefficient file 'vallet_vta.csv' from 'data-raw' directory. Error: ", e$message)
    }
  )
  
  ## Clean species names and join ----
  data <- data %>%
    mutate(species_code = toupper(trimws(species_code))) %>%
    left_join(vallet_coeffs, by = "species_code")
  
  ## Check for unknown species (missing coefficients) ----
  rows_unknown_species <- which(is.na(data$coeff_a))
  
  if (length(rows_unknown_species) > 0) {
    wrong_species <- data[rows_unknown_species, ] %>%
      pull(species_code) %>%
      unique()
    warning("Unknown species (missing VTA coefficients): ", paste(wrong_species, collapse = ", "),
            ". Form factor and VTA will be set to NA for these rows.", call. = FALSE)
  }
  
  # Combine all rows where the VTA/Form calculation must be NA (unknown species + c130 constraint)
  rows_to_invalidate <- unique(c(rows_to_invalidate, rows_unknown_species))
  
  
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
      vta_vallet = form * (pi / 40000) * (c130^2) * htot
    ) %>%
    # Remove only temporary columns, keeping 'form'
    select(-starts_with("coeff_"), -a, -b, -c, -d, -starts_with("term"))
  
  # FINAL STEP: Set VTA and form factor to NA for all identified invalid rows
  if (length(rows_to_invalidate) > 0) {
    data$vta_vallet[rows_to_invalidate] <- NA
    data$form[rows_to_invalidate] <- NA 
  }
  
  # OUTPUT ----
  return(data)
}