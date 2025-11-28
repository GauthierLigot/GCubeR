#' Single-entry Dagnelie volume (tarif 1)
#'
#' Computes the standing volume \eqn{v_{c,22}} (in cubic metres per tree) using
#' Dagnelie's single-entry tarif-1 equations.  
#' The volume is derived from the stem circumference at 1.30 m (\code{c130}, in cm)
#' and the tree species, using species-specific polynomial coefficients stored
#' in the reference table \code{dan1}.
#'
#' The tarif-1 volume is calculated as:
#' \deqn{
#'   v_{c,22} = a + b\,c130 + c\,c130^2 + d\,c130^3
#' }
#' where \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} are species-specific coefficients.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item checks that the input data frame contains the required
#'         variables \code{c130} and \code{species_code},
#'   \item validates that \code{c130} is numeric,
#'   \item verifies that all species are available in the \code{dan1}
#'         reference table and issues a warning otherwise,
#'   \item merges the input with \code{dan1} to retrieve coefficients
#'         and species-specific validity ranges (\code{min_c130}, \code{max_c130}),
#'   \item warns when \code{c130} values fall outside the recommended range,
#'   \item computes tarif-1 volume and returns the augmented data frame.
#' }
#'
#' @section Supported species:
#' The following species codes are supported by \code{dagnelie_vc22_1}:
#' \itemize{
#'   \item \code{"QUERCUS_PETRAEA"}
#'   \item \code{"QUERCUS_ROBUR"}
#'   \item \code{"QUERCUS_SP"}
#'   \item \code{"QUERCUS_RUBRA"}
#'   \item \code{"FAGUS_SYLVATICA"}
#'   \item \code{"ACER_PSEUDOPLATANUS"}
#'   \item \code{"FRAXINUS_EXCELSIOR"}
#'   \item \code{"ULMUS_SP"}
#'   \item \code{"PRUNUS_SP"}
#'   \item \code{"ALNUS_GLUTINOSA"}
#'   \item \code{"PICEA_ABIES"}
#'   \item \code{"PSEUDOTSUGA_MENZIESII"}
#'   \item \code{"LARIX_SP"}
#'   \item \code{"PINUS_SYLVESTRIS"}
#'   \item \code{"BETULA_SP"}
#' }
#'
#' @param data A \code{data.frame} containing at least:
#'   \itemize{
#'     \item \code{c130}: stem circumference at 1.30 m (cm),
#'     \item \code{species_code}: tree species code.
#'   }
#' @param output Optional argument controlling output format
#'   (currently ignored; the function always returns the augmented data frame).
#'
#' @return A \code{data.frame} identical to the input \code{data} but augmented with:
#'   \itemize{
#'     \item species-specific coefficients and validity ranges,
#'     \item \code{dagnelie_vc22_1}: the computed Dagnelie tarif-1 volume (m\eqn{^3} per tree).
#'   }
#'
#' @details
#' If one or more species codes are not found in \code{dan1}, the function issues
#' a warning and returns \code{NA}-values for missing coefficients and volumes.  
#' Trees with \code{c130} values outside the recommended species-specific range
#' produce a warning but still receive a computed volume.
#'
#' @seealso \code{\link{dan1}} for species-specific coefficients.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' df <- data.frame(
#'   c130         = c(145, 156, 234, 233),
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
#'                    "QUERCUS_SP", "FAGUS_SYLVATICA")
#' )
#' dagnelie_vc22_1(df)

dagnelie_vc22_1 <- function(data, output = NULL) {
  
  ## Validation of the Dataframe ----
  stopifnot(is.data.frame(data))
  needed <- c("c130", "species_code")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) stop("Missing column(s): ", paste(miss, collapse = ", "))
  
  if (!is.numeric(data$c130))
    stop("c130 must be numeric.")
  
  ## Species management ----
  valid_species <- c(
    "QUERCUS_PETRAEA","QUERCUS_ROBUR","QUERCUS_SP","QUERCUS_RUBRA",
    "FAGUS_SYLVATICA","ACER_PSEUDOPLATANUS","FRAXINUS_EXCELSIOR",
    "ULMUS_SP","PRUNUS_SP","ALNUS_GLUTINOSA","PICEA_ABIES",
    "PSEUDOTSUGA_MENZIESII","LARIX_SP","PINUS_SYLVESTRIS","BETULA_SP"
  )
  
  wrong <- setdiff(unique(data$species_code), valid_species)
  if (length(wrong) > 0) {
    warning("Unknown species: ", paste(wrong, collapse = ", "),
            "\nYou can find the list of available species in ?dan1")
  }
  
 
  ## Merge with dan1 ----
  data <- dplyr::left_join(
    data,
    dan1 %>% dplyr::select(
      species_code,
      coeff_a, coeff_b, coeff_c, coeff_d,
      min_c130, max_c130
    ),
    by = "species_code"
  )
  
  ## Forcing numeric values ----
  data <- data %>%
    mutate(
      coeff_a = as.numeric(coeff_a),
      coeff_b = as.numeric(coeff_b),
      coeff_c = as.numeric(coeff_c),
      coeff_d = as.numeric(coeff_d),
      min_c130 = as.numeric(min_c130),
      max_c130 = as.numeric(max_c130)
    )
  
  
  ## Check data$c130 constraint ----
  valid <- !is.na(data$c130) &
    !is.na(data$min_c130) &
    !is.na(data$max_c130)
  
  rows_out <- which(valid & (data$c130 < data$min_c130 | data$c130 > data$max_c130))
  
  if (length(rows_out) > 0) {
    details <- paste0(
      "row ", rows_out,
      " (species ", data$species_code[rows_out],
      ", min=", data$min_c130[rows_out],
      ", max=", data$max_c130[rows_out],
      ", found=", data$c130[rows_out], ")"
    )
    
    warning(
      paste("c130 out of range for", length(rows_out), "tree(s):",
            paste(details, collapse = " | ")),
      call. = FALSE
    )
  }
  
  ## Compute dagnelie_vc22_1 ----
  data$dagnelie_vc22_1 <- with(
    data,
    coeff_a + coeff_b * c130 + coeff_c * c130^2 + coeff_d * c130^3
  )
  
  ## Remove technical columns from dan1, keep everything else + dagnelie_vc22_1 ----
  data <- dplyr::select(
    data,
    -dplyr::any_of(c("coeff_a", "coeff_b", "coeff_c", "coeff_d",
                     "min_c130", "max_c130"))
  )
  
  return(data)
  
}
