#' Single-entry Dagnelie volume (tarif 1)
#'
#' Computes the standing volume \eqn{v_{c,22}} (in cubic metres per tree) using
#' Dagnelie's single-entry tarif 1 equations. The volume is calculated from the
#' stem circumference at 1.30 m (\code{c130}, in cm) and the tree species, using
#' species-specific polynomial coefficients stored in \code{dan1}.
#'
#' @param data A \code{data.frame} containing at least the columns
#'   \code{c130} (stem circumference at 1.30 m, in cm) and
#'   \code{species_code} (character code of the tree species).
#' @param output Optional output format (currently unused).
#'
#' @return The input data frame augmented with coefficients and the computed
#'   single-entry Dagnelie volume (\code{tarif1}, in m³ per tree).
#'
#' @import dplyr
#' @export
#'
dagnelie_tarif1 <- function(data, output = NULL) {
  
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
  
  ## Load dan1 ----
  path_dan1 <- file.path("data-raw", "dan1.csv")
  dan1 <- readr::read_delim(
    file = path_dan1,
    delim = ";",
    locale = readr::locale(decimal_mark = ",", encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
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
  
  ## Compute dagnelie_vc22_1 ----
  data$dagnelie_vc22_1 <- with(
    data,
    coeff_a + coeff_b * c130 + coeff_c * c130^2 + coeff_d * c130^3
  )
  
  ## Remove technical columns from dan1, keep everything else + tarif1 ----
  data <- dplyr::select(
    data,
    -dplyr::any_of(c("coeff_a", "coeff_b", "coeff_c", "coeff_d",
                     "min_c130", "max_c130"))
  )
  
  return(data)
  
}
