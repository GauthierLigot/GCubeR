#' Convert circumference between 1.50 m and 1.30 m
#'
#' Computes stem circumference at 1.30 m (\code{c130}, in cm)
#' from circumference at 1.50 m (\code{c150}, in cm) using
#' species-specific linear coefficients stored in the reference table
#' \code{c150_c130_coeff}. If only \code{c130} is available, the function
#' back-computes \code{c150} using the inverse of the same equation.
#'
#' The conversion equation is:
#' \deqn{
#'   c130 = a \cdot c150 + b
#' }
#' where \eqn{a} and \eqn{b} are species-specific coefficients.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item checks that the input data frame contains the required
#'         variables \code{species_code} and at least one of \code{c150} or \code{c130},
#'   \item validates that numeric values are provided,
#'   \item verifies that all species are available in the \code{c150_c130_coeff}
#'         reference table and issues a warning otherwise,
#'   \item merges the input with \code{c150_c130_coeff} to retrieve coefficients
#'         and species-specific validity ranges (\code{min_c150}, \code{max_c150}),
#'   \item warns when circumference values fall outside the recommended range,
#'   \item computes missing \code{c130} from \code{c150}, or missing \code{c150} from \code{c130},
#'   \item returns the augmented data frame with both columns completed.
#' }
#'
#' @section Supported species:
#' The following species codes are supported by \code{c150_c130}:
#' \itemize{
#'   \item \code{"QUERCUS_SP"}, \code{"QUERCUS_ROBUR"}, \code{"QUERCUS_PETRAEA"},
#'         \code{"QUERCUS_PUBESCENS"}, \code{"QUERCUS_RUBRA"}
#'   \item \code{"FAGUS_SYLVATICA"}, \code{"ACER_PSEUDOPLATANUS"},
#'         \code{"FRAXINUS_EXCELSIOR"}, \code{"ULMUS_SP"}, \code{"PRUNUS_AVIUM"}
#'   \item \code{"BETULA_SP"}, \code{"ALNUS_GLUTINOSA"}, \code{"LARIX_SP"},
#'         \code{"PINUS_SYLVESTRIS"}, \code{"CRATAEGUS_SP"}
#'   \item \code{"PRUNUS_SP"}, \code{"CARPINUS_SP"}, \code{"CASTANEA_SATIVA"},
#'         \code{"CORYLUS_AVELLANA"}, \code{"MALUS_SP"}
#'   \item \code{"PYRUS_SP"}, \code{"SORBUS_ARIA"}, \code{"SAMBUCUS_SP"},
#'         \code{"RHAMNUS_FRANGULA"}, \code{"PRUNUS_CERASUS"}
#'   \item \code{"ALNUS_INCANA"}, \code{"POPULUSxCANADENSIS"},
#'         \code{"POPULUS_TREMULA"}, \code{"PINUS_NIGRA"}, \code{"PINUS_LARICIO"}
#'   \item \code{"TAXUS_BACCATA"}, \code{"ACER_PLATANOIDES"},
#'         \code{"ACER_CAMPESTRE"}, \code{"SORBUS_AUCUPARIA"}, \code{"JUNGLANS_SP"}
#'   \item \code{"TILLIA_SP"}, \code{"AESCULUS_HIPPOCASTANUM"},
#'         \code{"ROBINIA_PSEUDOACACIA"}, \code{"SALIX_SP"}
#' }
#'
#' @param data A \code{data.frame} containing at least:
#'   \itemize{
#'     \item \code{species_code}: tree species code,
#'     \item \code{c150}: stem circumference at 1.50 m (cm), or
#'     \item \code{c130}: stem circumference at 1.30 m (cm).
#'   }
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If NULL (default), no file is written.
#'   Export is handled by the utility function \code{export_output()} and
#'   failures trigger warnings without interrupting execution.
#'   
#' @return A \code{data.frame} identical to the input but augmented with:
#'   \itemize{
#'     \item species-specific coefficients and validity ranges,
#'     \item both \code{c150} and \code{c130} columns completed whenever possible.
#'   }
#'
#' @seealso \code{\link{c150_c130_coeff}} for species-specific coefficients.
#'
#' @examples
#' df <- data.frame(
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA"),
#'   c150 = c(145, NA),
#'   c130 = c(NA, 156)
#' )
#' c150_c130(df)
#'
#' @import dplyr
#' @export

c150_c130 <- function(data,
                         na_action = c("error", "omit"),
                         output = NULL) {
  
  stopifnot(is.data.frame(data))
  if (!"species_code" %in% names(data)) {
    stop("Missing column 'species_code'.")
  }
  if (!("c150" %in% names(data) || "c130" %in% names(data))) {
    stop("Provide at least one of 'c150' or 'c130'.")
  }
  
  # Merge coefficients
  data <- dplyr::left_join(
    data,
    c150_c130_coeff %>%
      dplyr::select(species_code, coeff_a, coeff_b, min_c150, max_c150),
    by = "species_code"
  )
  
  # Force numeric
  data <- data %>%
    mutate(
      coeff_a = as.numeric(coeff_a),
      coeff_b = as.numeric(coeff_b),
      min_c150 = as.numeric(min_c150),
      max_c150 = as.numeric(max_c150)
    )
  
  # Compute c130 if c150 exists
  has_c150 <- "c150" %in% names(data)
  if (!has_c150) data$c150 <- NA_real_
  idx_c150 <- !is.na(data$c150) & !is.na(data$coeff_a) & !is.na(data$coeff_b)
  
  # c150 limits (set c130 to NA if out of bounds)
  out_of_range <- idx_c150 & (data$c150 < data$min_c150 | data$c150 > data$max_c150)
  if (any(out_of_range, na.rm = TRUE)) {
    warning("c150 out of range for rows: ", paste(which(out_of_range), collapse = ", "))
  }
  safe_c150 <- idx_c150 & !out_of_range
  data$c130[safe_c150] <- data$coeff_a[safe_c150] * data$c150[safe_c150] + data$coeff_b[safe_c150]
  
  # Compute c150 if missing but c130 exists
  has_c130 <- "c130" %in% names(data)
  if (!has_c130) data$c130 <- NA_real_
  idx_back <- is.na(data$c150) & !is.na(data$c130) & !is.na(data$coeff_a) & !is.na(data$coeff_b)
  data$c150[idx_back] <- (data$c130[idx_back] - data$coeff_b[idx_back]) / data$coeff_a[idx_back]
  
  # Clean technical columns 
  data <- dplyr::select(
    data,
    -dplyr::any_of(c("coeff_a", "coeff_b", "min_c150", "max_c150"))
  )
  
  # exporting the file using function export_output ----
  export_output(data, output)
  return(data)
}
