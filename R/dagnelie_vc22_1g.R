#' Graduated single-entry Dagnelie volume (tarif 1g)
#'
#' Computes the standing volume \eqn{v_{c,22}} (in cubic metres per tree) using
#' Dagnelie's *tarif 1g* equations. The volume is calculated from the stem
#' circumference at 1.30 m (\code{c130}, in cm), the dominant height
#' (\code{hdom}, in m), and the tree species, using species-specific polynomial
#' coefficients stored in \code{dan1g}.
#'
#' The function:
#' \itemize{
#'   \item checks that the input data frame contains the required columns
#'         \code{c130}, \code{hdom} and \code{species_code},
#'
#'   \item validates that all species codes are present in the \code{dan1g} table,
#'
#'   \item merges the input data with \code{dan1g} to retrieve:
#'         \code{coeff_a}, \code{coeff_b}, \code{coeff_c}, \code{coeff_d},
#'         \code{coeff_e}, \code{coeff_f},
#'         as well as the species-specific valid ranges
#'         \code{min_c130}, \code{max_c130}, \code{min_hdom}, \code{max_hdom},
#'
#'   \item issues a warning for trees whose \code{c130} is outside the valid
#'         range \code{[min_c130, max_c130]},
#'
#'   \item issues a warning for trees whose \code{hdom} is outside the valid
#'         range \code{[min_hdom, max_hdom]},
#'
#'   \item computes the tarif 1g volume using the species-specific polynomial:
#'         \deqn{
#'            v_{c,22} =
#'            coeff_a +
#'            coeff_b \cdot c130 +
#'            coeff_c \cdot c130^2 +
#'            coeff_d \cdot c130^3 +
#'            coeff_e \cdot hdom +
#'            coeff_f \cdot c130^2 \cdot hdom
#'         }
#' }
#'
#' @param data A \code{data.frame} containing the columns:
#'   \itemize{
#'     \item \code{c130} (stem circumference at 1.30 m, in cm)
#'     \item \code{hdom} (dominant height, in m)
#'     \item \code{species_code} (character code of the tree species)
#'   }
#'
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If \code{NULL} (default), no file is written.
#'   Export is handled by the utility function \code{export_output()}.
#'
#' @return A \code{data.frame} identical to \code{data}, augmented with:
#'   \itemize{
#'     \item the joined columns from \code{dan1g}
#'           (\code{coeff_a}, \code{coeff_b}, \code{coeff_c}, \code{coeff_d},
#'            \code{coeff_e}, \code{coeff_f},
#'            \code{min_c130}, \code{max_c130}, \code{min_hdom}, \code{max_hdom})
#'     \item \code{dagnelie_vc22_1g}: the computed volume (m\eqn{^3} per tree)
#'   }
#'
#' @details
#' Species codes must match those available in the \code{dan1g} table.
#' If one or more species are not found, the function issues a warning.
#'
#' If a tree's \code{c130} or \code{hdom} falls outside the species-specific 
#' validity ranges \code{[min_c130, max_c130]} or \code{[min_hdom, max_hdom]}, 
#' a warning is issued, but the volume is still computed.
#'
#' @seealso \code{\link{dan1g}} for the species-specific coefficients and ranges.
#'
#' @import dplyr
#'
#' @examples
#' df <- data.frame(
#'   c130         = c(145, 156, 234, 233),
#'   hdom         = c(25, 23, 45, 34),
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
#'                    "QUERCUS_SP", "FAGUS_SYLVATICA")
#' )
#' dagnelie_vc22_1g(data = df)
#' @export
dagnelie_vc22_1g <- function(data, output = NULL) {
  
  # Validation of the Dataframe  ----
  ##  Field needed ----
  stopifnot(is.data.frame(data))
  needed <- c("c130","hdom","species_code")           #required names 
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {         
    stop("Missing column : ", paste(miss, collapse = ", "))
  }
  if (!is.numeric(data$c130) && !is.numeric(data$hdom)) {      #numeric data
    stop("c130 must be numeric")
  }
  
  ## Species management ---- 
  wrong <- setdiff(
    unique(data$species_code),
    c("QUERCUS_SP","QUERCUS_ROBUR","QUERCUS_PETRAEA","QUERCUS_PUBESCENS","QUERCUS_RUBRA","FAGUS_SYLVATICA",
      "ACER_PSEUDOPLATANUS","FRAXINUS_EXCELSIOR","ULMUS_SP","PRUNUS_AVIUM","BETULA_SP",
      "ALNUS_GLUTINOSA","PICEA_ABIES","PSEUDOTSUGA_MENZIESII","LARIX_SP","PINUS_SYLVESTRIS",
      "CRATAEGUS_SP","PRUNUS_SP","CARPINUS_SP","CASTANEA_SATIVA","CORYLUS_AVELLANA","MALUS_SP",
      "PYRUS_SP","SORBUS_ARIA","SAMBUCUS_SP","RHAMNUS_FRANGULA","PRUNUS_CERASUS","ALNUS_INCANA",
      "POPULUSxCANADENSIS","POPULUS_TREMULA","PINUS_NIGRA","PINUS_LARICIO","TAXUS_BACCATA",
      "ACER_PLATANOIDES","ACER_CAMPESTRE","SORBUS_AUCUPARIA","JUNGLANS_SP","TILLIA_SP",
      "PICEA_SITCHENSIS","ABIES_ALBA","TSUGA_CANADENSIS","ABIES_GRANDIS","CUPRESSUS_SP",
      "THUJA_PLICATA","AESCULUS_HIPPOCASTANUM","ROBINIA_PSEUDOACACIA","SALIX_SP")
  )
  
  if (length(wrong) > 0) {
    warning("Unknown species : ", paste(wrong, collapse=", "),
            "\n You can find the list of available species in the helper (?dagnelie_vc22_1g)")
  }
  
  ## Merge with dan1g ----
  data <- dplyr::left_join(
    data,
    GCubeR::dan1g %>% dplyr::select(
      species_code,
      coeff_a, coeff_b, coeff_c, coeff_d, coeff_e, coeff_f,
      min_c130, max_c130, min_hdom, max_hdom
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
      coeff_e = as.numeric(coeff_e),
      coeff_f = as.numeric(coeff_f),
      min_c130 = as.numeric(min_c130),
      max_c130 = as.numeric(max_c130),
      max_hdom = as.numeric(max_hdom),
      min_hdom = as.numeric(min_hdom)
    )
  
  ## Check data$c130 constraint ----
  valid <- !is.na(data$c130) & 
    !is.na(data$min_c130) & 
    !is.na(data$max_c130)
  
  rows_out <- which(
    valid & (data$c130 < data$min_c130 | data$c130 > data$max_c130)
  )
  
  ## Check data$hdom constraint ----
  valid <- !is.na(data$hdom) & 
    !is.na(data$min_hdom) & 
    !is.na(data$max_hdom)
  
  rows_out <- which(
    valid & (data$hdom < data$min_hdom | data$hdom > data$max_hdom)
  )
  
  if (length(rows_out) > 0) {
    
    details <- paste0(
      "row ", rows_out,
      " (species ", data$species_code[rows_out],
      ", min=", data$min_hdom[rows_out],
      ", max=", data$max_hdom[rows_out],
      ", found=", data$hdom[rows_out], ")"
    )
    
    warning(
      paste(
        "hdom out of range for", length(rows_out), "tree(s):",
        paste(details, collapse = " | ")
      ),
      call. = FALSE
    )
  }
  if (length(rows_out) > 0) {
    
    details <- paste0(
      "row ", rows_out,
      " (species ", data$species_code[rows_out],
      ", min=", data$min_c130[rows_out],
      ", max=", data$max_c130[rows_out],
      ", found=", data$c130[rows_out], ")"
    )
    
    warning(
      paste(
        "c130 out of range for", length(rows_out), "tree(s):",
        paste(details, collapse = " | ")
      ),
      call. = FALSE
    )
  }
  
  ## Initialisation des colonnes de sortie ----
  data$dagnelie_vc22_1g  <- NA_real_
  nline <- nrow(data)
  
  # Iteration ----
  data$dagnelie_vc22_1g <- with(
    data,
    coeff_a + coeff_b * c130 + coeff_c * c130^2 + coeff_d * c130^3 + coeff_e*hdom + coeff_f* hdom * c130^2
  )
  
  ## Remove technical columns from dan1, keep everything else + tarif1 ----
  data <- dplyr::select(
    data,
    -dplyr::any_of(c("coeff_a", "coeff_b", "coeff_c", "coeff_d","coeff_e", "coeff_f",
                     "min_c130", "max_c130", "max_hdom", "min_hdom"))
  )
  
  # exporting the file using function export_output ----
  export_output(data, output)
  return(data)
}
