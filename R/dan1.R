
#' GCubeR
#'
#' @param data 
#' @param method 
#' @param na_action 
#' @param output 
#'
#' @returns
#' @import library(dplyr)
#' @export
#'
#' @examples
#' df <- data.frame (C130=c(145,156,234,233), 
#' H=c(25,23,45,34), 
#' ESS=c("Pinus_sylvestris","Quercus_robur","Quercus_rubra","Fagus_sylvatica"))
#' gcbr(data=df)

dan1 <- function(data, 
                 output = NULL){ 
  

# Validation of the Dataframe  ----
  ##  Field needed ----
  stopifnot(is.data.frame(data))
  needed <- c("C130","species_code")           #required names 
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {         
    stop("Missing column : ", paste(miss, collapse = ", "))
    }
  if (!is.numeric(data$C130)) {      #numeric data
    stop("C130 must be numeric")
    }

  ## Species management ---- 
  wrong <- setdiff(unique(data$species_code), c("QUERCUS_SP", "QUERCUS_RUBRA", "FAGUS_SYLVATICA", "ACER_PSEUDOPLATANUS",
                                     "FRAXINUS_EXCELSIOR","ULMUS_SP","PRUNUS_SP","ALNUS_GLUTINOSA",
                                     "PICEA_ABIES","PSEUDOTSUGA_MENZIESII","LARIX_SP","PINUS_SYLVESTRIS",
                                     "BETULA_SP")) #liste des essences valides pour les équations dans "eq"

  if (length(wrong) > 0) {
    stop("Unknown species : ", paste(wrong, collapse=", "),
        "\n You can find the list of available species in the helper (?dan1)")
  }

  # Preparation of the df ----
  data <- clean_join(data,"dan1.csv","species_code","species_code")
 
  
  ## Initialisation des colonnes de sortie ----
  data$tarif1  <- NA_real_
  nline <- nrow(data)

  # Iteration ----
  data$tarif1 <- with(data, coeff_a + coeff_b*C130 + coeff_c*C130^2 + coeff_d*C130^3)
  return(data)
  }
