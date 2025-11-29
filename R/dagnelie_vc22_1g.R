#' Single-entry Dagnelie volume (tarif 1g)
#'
#' Computes the standing volume \eqn{v_{c,22}} (in cubic metres per tree) using
#' Dagnelie's non parametric equations. The volume is calculated from the
#' stem circumference at 1.30 m (\code{c130}, in cm) and the tree species, using
#' species-specific polynomial coefficients stored in \code{dan1g}.
#'
#' The function:
#' \itemize{
#'   \item checks that the input data frame contains the required columns
#'         \code{c130} and \code{species_code},
#'   \item validates that all species are available in the \code{dan1g} table,
#'   \item merges the input data with \code{dan1g} to retrieve the coefficients
#'         \code{coeff_a}, \code{coeff_b}, \code{coeff_c}, \code{coeff_d} and
#'         the valid range \code{min_c130}, \code{max_c130},
#'   \item issues a warning for trees whose \code{c130} is outside the species-
#'         specific range,
#'   \item computes tarif 1g volume as:
#'         \deqn{v_{c,22} = a + b \cdot c130 + c \cdot c130^2 + d \cdot c130^3 + e \cdot hdom + f \cdot c130^2 \cdot hdom.}
#' }
#'
#' @section Supported species:
#' The following species codes are currently supported by \code{dagnelie_vc22_1g}:
#' \itemize{
#'   \item \code{"QUERCUS_SP"}
#'   \item \code{"QUERCUS_ROBUR"}
#'   \item \code{"QUERCUS_PETRAEA"}
#'   \item \code{"QUERCUS_PUBESCENS"}
#'   \item \code{"QUERCUS_RUBRA"}
#'   \item \code{"FAGUS_SYLVATICA"}
#'   \item \code{"ACER_PSEUDOPLATANUS"}
#'   \item \code{"FRAXINUS_EXCELSIOR"}
#'   \item \code{"ULMUS_SP"}
#'   \item \code{"PRUNUS_AVIUM"}
#'   \item \code{"BETULA_SP"}
#'   \item \code{"ALNUS_GLUTINOSA"}
#'   \item \code{"PICEA_ABIES"}
#'   \item \code{"PSEUDOTSUGA_MENZIESII"}
#'   \item \code{"LARIX_SP"}
#'   \item \code{"PINUS_SYLVESTRIS"}
#'   \item \code{"CRATAEGUS_SP"}
#'   \item \code{"PRUNUS_SP"}
#'   \item \code{"CARPINUS_SP"}
#'   \item \code{"CASTANEA_SATIVA"}
#'   \item \code{"CORYLUS_AVELLANA"}
#'   \item \code{"MALUS_SP"}
#'   \item \code{"PYRUS_SP"}
#'   \item \code{"SORBUS_ARIA"}
#'   \item \code{"SAMBUCUS_SP"}
#'   \item \code{"RHAMNUS_FRANGULA"}
#'   \item \code{"PRUNUS_CERASUS"}
#'   \item \code{"ALNUS_INCANA"}
#'   \item \code{"POPULUSxCANADENSIS"}
#'   \item \code{"POPULUS_TREMULA"}
#'   \item \code{"PINUS_NIGRA"}
#'   \item \code{"PINUS_LARICIO"}
#'   \item \code{"TAXUS_BACCATA"}
#'   \item \code{"ACER_PLATANOIDES"}
#'   \item \code{"ACER_CAMPESTRE"}
#'   \item \code{"SORBUS_AUCUPARIA"}
#'   \item \code{"JUNGLANS_SP"}
#'   \item \code{"TILLIA_SP"}
#'   \item \code{"PICEA_SITCHENSIS"}
#'   \item \code{"ABIES_ALBA"}
#'   \item \code{"TSUGA_CANADENSIS"}
#'   \item \code{"ABIES_GRANDIS"}
#'   \item \code{"CUPRESSUS_SP"}
#'   \item \code{"THUJA_PLICATA"}
#'   \item \code{"AESCULUS_HIPPOCASTANUM"}
#'   \item \code{"ROBINIA_PSEUDOACACIA"}
#'   \item \code{"SALIX_SP"}
#' }
#'
#' @param data A \code{data.frame} containing at least the columns
#'   \code{c130} (stem circumference at 1.30 m, in cm) and
#'   \code{hdom} (hight of the tree)
#'   \code{species_code} (character code of the tree species).
#' @param output Optional character string controlling the format of the output.
#'   Currently ignored; the function always returns the input data frame with
#'   additional columns.
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If NULL (default), no file is written.
#'   Export is handled by the utility function \code{export_output()} and
#'   failures trigger warnings without interrupting execution.
#'   
#' @return A \code{data.frame} identical to \code{data} but augmented with:
#'   \itemize{
#'     \item the joined columns from \code{dan1g}
#'           (\code{coeff_a}, \code{coeff_b}, \code{coeff_c}, \code{coeff_d},
#'           \code{min_c130}, \code{max_c130}),
#'     \item \code{dagnelie_vc22_1g}: the Dagnelie non parametric volume \eqn{v_{c,22}}
#'           in m\eqn{^3} per tree.
#'   }
#'
#' @details
#' Species codes must match those available in the \code{dan1g} reference table.
#' If one or more species are not found, the function issues a warning. For trees
#' where \code{c130} is outside the species-specific range
#' \code{[min_c130, max_c130]}, a warning is issued, but the volume is still
#' computed.
#'
#' @seealso \code{\link{dan2}} for the species-specific coefficients and ranges.
#'
#' @import dplyr
#'
#' @examples
#' df <- data.frame(
#'   c130         = c(145, 156, 234, 233),
#'   hdom            = c(25, 23, 45, 34),
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
#'                    "QUERCUS_SP", "FAGUS_SYLVATICA")
#' )
#' dagnelie_vc22_1g(data = df)
dagnelie_vc22_1g <- function(data,
                             na_action = c("error", "omit"),
                             output = NULL) {
  
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
  
  ## Load dan2 ----
  path_dan1g <- file.path("data-raw", "dan1g.csv")
  dan2 <- readr::read_delim(
    file = path_dan1g,
    delim = ";",
    locale = readr::locale(decimal_mark = ".", encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  ## Merge with dan1g ----
  data <- dplyr::left_join(
    data,
    dan2 %>% dplyr::select(
      species_code,
      coeff_a, coeff_b, coeff_c, coeff_d, coeff_e, coeff_f,
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
      coeff_e = as.numeric(coeff_e),
      coeff_f = as.numeric(coeff_f),
      min_c130 = as.numeric(min_c130),
      max_c130 = as.numeric(max_c130)
    )
  
  ## Check data$c130 constraint ----
  valid <- !is.na(data$c130) & 
    !is.na(data$min_c130) & 
    !is.na(data$max_c130)
  
  rows_out <- which(
    valid & (data$c130 < data$min_c130 | data$c130 > data$max_c130)
  )
  
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
                     "min_c130", "max_c130"))
  )
  
  # exporting the file using function export_output ----
  export_output(data, output)
  return(data)
}
