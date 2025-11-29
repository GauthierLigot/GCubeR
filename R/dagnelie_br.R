#' Single-entry Dagnelie branch volume (tarif "br")
#'
#' Computes the branch volume \eqn{v_{br}} (in cubic metres per tree)
#' using Dagnelie's single-entry "br" equations.  
#' The branch volume is derived from the stem circumference at 1.30 m (\code{c130}, in cm)
#' and the tree species, using species-specific polynomial coefficients stored
#' in the reference table \code{danbr}.
#'
#' The "br" tarif branch volume is calculated as:
#' \deqn{
#'   v_{br} = a + b\,c130 + c\,c130^2 + d\,c130^3
#' }
#' where \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} are species-specific coefficients.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item checks that the input data frame contains the required
#'         variables \code{c130} and \code{species_code},
#'   \item validates that \code{c130} is numeric,
#'   \item verifies that all species are available in the \code{danbr}
#'         reference table and issues a warning otherwise,
#'   \item merges the input with \code{danbr} to retrieve coefficients
#'         and species-specific validity ranges (\code{min_c130}, \code{max_c130}),
#'   \item warns when \code{c130} values fall outside the recommended range,
#'   \item computes tarif-"br" branch volume and returns the augmented data frame.
#' }
#'
#' @section Supported species:
#' The following species codes are supported by \code{dagnelie_br}:
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
#'     \item \code{c130}: stem circumference at 1.30 m (cm),
#'     \item \code{species_code}: tree species code.
#'   }
#' @param output Optional argument controlling output format
#'   (currently ignored; the function always returns the augmented data frame).
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If NULL (default), no file is written.
#'   Export is handled by the utility function \code{export_output()} and
#'   failures trigger warnings without interrupting execution.
#'   
#' @return A \code{data.frame} identical to the input \code{data} but augmented with:
#'   \itemize{
#'     \item species-specific coefficients and validity ranges,
#'     \item \code{dagnelie_br}: the computed Dagnelie tarif-"br" branch volume (m\eqn{^3} per tree).
#'   }
#'
#' @details
#' If one or more species codes are not found in \code{danbr}, the function issues
#' a warning and returns \code{NA}-values for missing coefficients and volumes.  
#' Trees with \code{c130} values outside the recommended species-specific range
#' produce a warning but still receive a computed branch volume.
#'
#' @seealso \code{\link{danbr}} for species-specific coefficients.
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
#' dagnelie_br(df)



dagnelie_br <- function(data,
                        na_action = c("error", "omit"),
                        output = NULL) {
  
  ## Validation of the Dataframe ----
  stopifnot(is.data.frame(data))
  needed <- c("c130", "species_code")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) stop("Missing column(s): ", paste(miss, collapse = ", "))
  
  if (!is.numeric(data$c130))
    stop("c130 must be numeric.")
  
  ## Species management ----
  valid_species <- c(
    c("QUERCUS_SP","QUERCUS_ROBUR","QUERCUS_PETRAEA","QUERCUS_PUBESCENS","QUERCUS_RUBRA","FAGUS_SYLVATICA",
      "ACER_PSEUDOPLATANUS","FRAXINUS_EXCELSIOR","ULMUS_SP","PRUNUS_AVIUM","BETULA_SP",
      "ALNUS_GLUTINOSA","LARIX_SP","PINUS_SYLVESTRIS","CRATAEGUS_SP","PRUNUS_SP",
      "CARPINUS_SP","CASTANEA_SATIVA","CORYLUS_AVELLANA","MALUS_SP","PYRUS_SP",
      "SORBUS_ARIA","SAMBUCUS_SP","RHAMNUS_FRANGULA","PRUNUS_CERASUS","ALNUS_INCANA",
      "POPULUSxCANADENSIS","POPULUS_TREMULA","PINUS_NIGRA","PINUS_LARICIO","TAXUS_BACCATA",
      "ACER_PLATANOIDES","ACER_CAMPESTRE","SORBUS_AUCUPARIA","JUNGLANS_SP","TILLIA_SP",
      "AESCULUS_HIPPOCASTANUM","ROBINIA_PSEUDOACACIA","SALIX_SP")
  )
  
  wrong <- setdiff(unique(data$species_code), valid_species)
  if (length(wrong) > 0) {
    warning("Unknown species: ", paste(wrong, collapse = ", "),
            "\nYou can find the list of available species in ?dan1")
  }
  
  ## Merge with coefficients ----
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
  
  ## Compute dagnelie_br ----
  data$dagnelie_br <- with(
    data,
    coeff_a + coeff_b * c130 + coeff_c * c130^2 + coeff_d * c130^3
  )
  
  ## Remove technical columns from dan1, keep everything else + dagnelie_br ----
  data <- dplyr::select(
    data,
    -dplyr::any_of(c("coeff_a", "coeff_b", "coeff_c", "coeff_d",
                     "min_c130", "max_c130"))
  )
  
  # exporting the file using function export_output ----
  export_output(data, output)
  
  return(data)
  
}
