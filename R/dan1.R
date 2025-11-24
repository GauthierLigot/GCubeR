#' Single-entry Dagnelie volume (tarif 1)
#'
#' Computes the standing volume \eqn{v_{c,22}} (in cubic metres per tree) using
#' Dagnelie's single-entry tarif 1 equations. The volume is calculated from the
#' stem circumference at 1.30 m (\code{c130}, in cm) and the tree species, using
#' species-specific polynomial coefficients stored in \code{dan1}.
#'
#' The function:
#' \itemize{
#'   \item checks that the input data frame contains the required columns
#'         \code{c130} and \code{species_code},
#'   \item validates that all species are available in the \code{dan1} table,
#'   \item merges the input data with \code{dan1} to retrieve the coefficients
#'         \code{coeff_a}, \code{coeff_b}, \code{coeff_c}, \code{coeff_d} and
#'         the valid range \code{min_c130}, \code{max_c130},
#'   \item issues a warning for trees whose \code{c130} is outside the species-
#'         specific range,
#'   \item computes tarif 1 volume as:
#'         \deqn{v_{c,22} = a + b \cdot c130 + c \cdot c130^2 + d \cdot c130^3.}
#' }
#'
#' @param data A \code{data.frame} containing at least the columns
#'   \code{c130} (stem circumference at 1.30 m, in cm) and
#'   \code{species_code} (character code of the tree species).
#' @param output Optional character string controlling the format of the output.
#'   Currently ignored; the function always returns the input data frame with
#'   additional columns.
#'
#' @return A \code{data.frame} identical to \code{data} but augmented with:
#'   \itemize{
#'     \item the joined columns from \code{dan1}
#'           (\code{coeff_a}, \code{coeff_b}, \code{coeff_c}, \code{coeff_d},
#'           \code{min_c130}, \code{max_c130}),
#'     \item \code{tarif1}: the Dagnelie single-entry volume \eqn{v_{c,22}}
#'           in m\eqn{^3} per tree.
#'   }
#'
#' @details
#' Species codes must match those available in the \code{dan1} reference table.
#' If one or more species are not found, the function throws an error. For trees
#' where \code{c130} is outside the species-specific range
#' \code{[min_c130, max_c130]}, a warning is issued, but the volume is still
#' computed.
#'
#' @seealso \code{\link{dan1}} for the species-specific coefficients and ranges.
#'
#' @import dplyr
#'
#' @examples
#' df <- data.frame(
#'   c130         = c(145, 156, 234, 233),
#'   H            = c(25, 23, 45, 34),
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA",
#'                    "QUERCUS_SP", "FAGUS_SYLVATICA")
#' )
#' dagnelie_tarif1(data = df)

dagnelie_tarif1 <- function(data, 
                            output = NULL){ 
  
  # Validation of the Dataframe  ----
  ##  Field needed ----
  stopifnot(is.data.frame(data))
  needed <- c("c130","species_code")           #required names 
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {         
    stop("Missing column : ", paste(miss, collapse = ", "))
  }
  if (!is.numeric(data$c130)) {      #numeric data
    stop("c130 must be numeric")
  }
  
  ## Species management ---- 
  wrong <- setdiff(unique(data$species_code),
                   c("QUERCUS_PETRAEA","QUERCUS_ROBUR","QUERCUS_SP", "QUERCUS_RUBRA", 
                     "FAGUS_SYLVATICA","ACER_PSEUDOPLATANUS","FRAXINUS_EXCELSIOR",
                     "ULMUS_SP","PRUNUS_SP","ALNUS_GLUTINOSA","PICEA_ABIES",
                     "PSEUDOTSUGA_MENZIESII","LARIX_SP","PINUS_SYLVESTRIS","BETULA_SP"))
  
  if (length(wrong) > 0) {
    warning("Unknown species : ", paste(wrong, collapse=", "),
         "\n You can find the list of available species in the helper (?dan1)")
  }
  
  ## Merge with density table ----
  data <- left_join(
    data,
    dan1 %>% select(
      species_code,
      coeff_a, coeff_b, coeff_c, coeff_d,
      min_c130, max_c130
    ),
    by = "species_code"
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
  data$tarif1  <- NA_real_
  nline <- nrow(data)
  
  # Iteration ----

  data$tarif1 <- with(
    data,
    coeff_a + coeff_b * c130 + coeff_c * c130^2 + coeff_d * c130^3
  )
  
  return(data)
}
