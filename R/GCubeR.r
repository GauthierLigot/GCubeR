#' gcuber main workflow
#'
#' Orchestrates the gcuber pipeline by sequentially applying allometric
#' conversion and biomass/volume functions to a user-provided dataset.
#'
#' @description
#' This function takes a dataframe containing tree measurements (circumference,
#' diameter, height, species code) and enriches it by:
#' \enumerate{
#'   \item Converting circumference at 1.50 m (\code{c150}) to circumference at 1.30 m (\code{c130}).
#'   \item Adding diameter at breast height (\code{dbh}) if missing, or converting back to \code{c130}.
#'   \item Applying a suite of allometric equations for volume, biomass, and carbon stock estimation.
#' }
#'
#' @param data A \code{data.frame} with at least:
#' \itemize{
#'   \item \code{species_code}: tree species identifier (character),
#'   \item \code{c150}, \code{c130}, or \code{dbh}: stem circumference or diameter,
#'   \item optionally \code{htot} (total height) and \code{hdom} (dominant height).
#' }
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If NULL (default), no file is written.
#'   Export is handled by the utility function \code{export_output()} and
#'   failures trigger warnings without interrupting execution.
#'
#' @return A \code{data.frame} identical to the input but augmented with:
#' \itemize{
#'   \item \code{c130} and \code{dbh} (ensured to be present),
#'   \item outputs from Dagnelie, Vallet, Algan, Rondeux, Bouvard functions,
#'   \item biomass and carbon stock estimates.
#' }
#'
#' @details
#' The following functions are called in order:
#' \enumerate{
#'   \item \code{c150_to_c130}
#'   \item \code{add_c130_dbh}
#'   \item \code{dagnelie_vc22_1}
#'   \item \code{dagnelie_vc22_1g}
#'   \item \code{dagnelie_vc22_2}
#'   \item \code{dagnelie_br}
#'   \item \code{vallet_vta}
#'   \item \code{vallet_vc22}
#'   \item \code{algan_vta_vc22}
#'   \item \code{rondeux_vc22_vtot}
#'   \item \code{bouvard_vta}
#'   \item \code{biomass_calc}
#' }
#'
#' @examples
#' df <- data.frame(
#'   tree_id = 1:3,
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA", "FAGUS_SYLVATICA"),
#'   c150 = c(145, NA, NA),
#'   c130 = c(NA, 156, NA),
#'   dbh  = c(NA, NA, 40),
#'   htot = c(25, 30, 28),
#'   hdom = c(NA, 32, NA)
#' )
#' gcuber(df)
#'
#' @export

gcuber <- function(data, output = NULL) {
  stopifnot(is.data.frame(data))
  
  if (!"species_code" %in% names(data)) {
    stop("Missing column 'species_code'.")
  }
  
  # Always applied ----
  data <- c150_c130(data)
  data <- add_c130_dbh(data)
  data <- dagnelie_vc22_1(data)
  data <- dagnelie_br(data)
  
  # Conditional on hdom ----
  if ("hdom" %in% names(data)) {
    data <- dagnelie_vc22_1g(data)
  }
  
  # Conditional on htot ----
  if ("htot" %in% names(data)) {
    data <- dagnelie_vc22_2(data)
    data <- vallet_vta(data)
    data <- vallet_vc22(data)
    data <- algan_vta_vc22(data)
    data <- rondeux_vc22_vtot(data)
    data <- bouvard_vta(data)
  }
  
  # Always applied at the end ----
  data <- biomass_calc(data)
  
  # Export if requested ----
  export_output(data, output)
  return(data)
}