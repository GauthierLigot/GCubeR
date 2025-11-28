#' Convert circumference at 1.50 m to circumference at 1.30 m
#'
#' Computes stem circumference at 1.30 m (\code{c130}, in cm)
#' from circumference at 1.50 m (\code{c150}, in cm) and species-specific
#' linear coefficients stored in the reference table \code{c150_c130_coeff}.
#'
#' The conversion equation is:
#' \deqn{
#'   c130 = a \cdot c150 + b
#' }
#' where \eqn{a} and \eqn{b} are species-specific coefficients.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item checks that the input data frame contains the required
#'         variables \code{c150} and \code{species_code},
#'   \item validates that \code{c150} is numeric,
#'   \item verifies that all species are available in the \code{c150_c130_coeff}
#'         reference table and issues a warning otherwise,
#'   \item merges the input with \code{c150_c130_coeff} to retrieve coefficients
#'         and species-specific validity ranges (\code{min_c150}, \code{max_c150}),
#'   \item warns when \code{c150} values fall outside the recommended range,
#'   \item computes \code{c130} and returns the augmented data frame.
#' }
#'
#' @section Supported species:
#' The following species codes are supported by \code{c150_to_c130}:
#' \itemize{
#'   \item \code{"QUERCUS_SP"}, 
#'   \item \code{"QUERCUS_ROBUR"}, 
#'   \item \code{"QUERCUS_PETRAEA"}, 
#'   \item \code{"QUERCUS_PUBESCENS"}, 
#'   \item \code{"QUERCUS_RUBRA"}
#'   \item \code{"FAGUS_SYLVATICA"}, 
#'   \item \code{"ACER_PSEUDOPLATANUS"}, 
#'   \item \code{"FRAXINUS_EXCELSIOR"}, 
#'   \item \code{"ULMUS_SP"}, 
#'   \item \code{"PRUNUS_AVIUM"}
#'   \item \code{"BETULA_SP"}, 
#'   \item \code{"ALNUS_GLUTINOSA"}, 
#'   \item \code{"LARIX_SP"}, 
#'   \item \code{"PINUS_SYLVESTRIS"}, 
#'   \item \code{"CRATAEGUS_SP"}
#'   \item \code{"PRUNUS_SP"}, 
#'   \item \code{"CARPINUS_SP"}, 
#'   \item \code{"CASTANEA_SATIVA"}, 
#'   \item \code{"CORYLUS_AVELLANA"}, 
#'   \item \code{"MALUS_SP"}
#'   \item \code{"PYRUS_SP"}, 
#'   \item \code{"SORBUS_ARIA"}, 
#'   \item \code{"SAMBUCUS_SP"}, 
#'   \item \code{"RHAMNUS_FRANGULA"}, 
#'   \item \code{"PRUNUS_CERASUS"}
#'   \item \code{"ALNUS_INCANA"}, 
#'   \item \code{"POPULUSxCANADENSIS"}, 
#'   \item \code{"POPULUS_TREMULA"}, 
#'   \item \code{"PINUS_NIGRA"}, 
#'   \item \code{"PINUS_LARICIO"}
#'   \item \code{"TAXUS_BACCATA"}, 
#'   \item \code{"ACER_PLATANOIDES"}, 
#'   \item \code{"ACER_CAMPESTRE"}, 
#'   \item \code{"SORBUS_AUCUPARIA"}, 
#'   \item \code{"JUNGLANS_SP"}
#'   \item \code{"TILLIA_SP"}, 
#'   \item \code{"AESCULUS_HIPPOCASTANUM"}, 
#'   \item \code{"ROBINIA_PSEUDOACACIA"}, 
#'   \item \code{"SALIX_SP"}
#' }
#'
#' @param data A \code{data.frame} containing at least:
#'   \itemize{
#'     \item \code{c150}: stem circumference at 1.50 m (cm),
#'     \item \code{species_code}: tree species code.
#'   }
#' @param output Optional argument controlling output format
#'   (currently ignored; the function always returns the augmented data frame).
#'
#' @return A \code{data.frame} identical to the input \code{data} but augmented with:
#'   \itemize{
#'     \item species-specific coefficients and validity ranges,
#'     \item \code{c130}: the computed circumference at 1.30 m (cm).
#'   }
#'
#' @details
#' If one or more species codes are not found in \code{c150_c130_coeff}, the function issues
#' a warning and returns \code{NA}-values for missing coefficients and conversions.  
#' Trees with \code{c150} values outside the recommended species-specific range
#' produce a warning but still receive a computed \code{c130}.
#'
#' @seealso \code{\link{c150_c130_coeff}} for species-specific coefficients.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' df <- data.frame(
#'   c150         = c(145, 156, 234, 233),
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
#'                    "QUERCUS_SP", "FAGUS_SYLVATICA")
#' )
#' c150_to_c130(df)
#'
c150_to_c130 <- function(data,
                         na_action = c("error", "omit"),
                         output = NULL) {
  
  ## Validation of the Dataframe ----
  stopifnot(is.data.frame(data))
  needed <- c("c150", "species_code")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) stop("Missing column(s): ", paste(miss, collapse = ", "))
  
  if (!is.numeric(data$c150))
    stop("c150 must be numeric.")
  
  ## Species management ----
  valid_species <- unique(c150_c130_coeff$species_code)
  wrong <- setdiff(unique(data$species_code), valid_species)
  if (length(wrong) > 0) {
    warning("Unknown species: ", paste(wrong, collapse = ", "),
            "\nYou can find the list of available species in ?c150_c130_coeff")
  }
  
  ## Merge with coefficients ----
  data <- dplyr::left_join(
    data,
    c150_c130_coeff %>% dplyr::select(
      species_code,
      coeff_a, coeff_b,
      min_c150, max_c150
    ),
    by = "species_code"
  )
  
  ## Forcing numeric values ----
  data <- data %>%
    mutate(
      coeff_a = as.numeric(coeff_a),
      coeff_b = as.numeric(coeff_b),
      min_c150 = as.numeric(min_c150),
      max_c150 = as.numeric(max_c150)
    )
  
  ## Check data$c150 constraint ----
  valid <- !is.na(data$c150) &
    !is.na(data$min_c150) &
    !is.na(data$max_c150)
  
  rows_out <- which(valid & (data$c150 < data$min_c150 | data$c150 > data$max_c150))
  
  if (length(rows_out) > 0) {
    details <- paste0(
      "row ", rows_out,
      " (species ", data$species_code[rows_out],
      ", min=", data$min_c150[rows_out],
      ", max=", data$max_c150[rows_out],
      ", found=", data$c150[rows_out], ")"
    )
    
    warning(
      paste("c150 out of range for", length(rows_out), "tree(s):",
            paste(details, collapse = " | ")),
      call. = FALSE
    )
  }
  
  ## Compute c130 ----
  data$c130 <- with(
    data,
    coeff_a * c150 + coeff_b
  )
  
  ## Remove technical columns from c150_c130_coeff, keep everything else + c130 ----
  data <- dplyr::select(
    data,
    -dplyr::any_of(c("coeff_a", "coeff_b", "min_c150", "max_c150"))
  )
  
  return(data)
}
