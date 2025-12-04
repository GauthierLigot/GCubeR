#' GCubeR main workflow
#'
#' Orchestrates the GCubeR pipeline by sequentially applying allometric
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
#' @param volume_col Optional character string naming the column of volume to use
#'   for plotting in \code{plot_by_class}. If NULL (default), the function
#'   will automatically select the first available column among:
#'   \code{dagnelie_vc22_1}, \code{dagnelie_vc22_1g}, \code{dagnelie_vc22_2},
#'   \code{dagnelie_br}, \code{vallet_vta}, \code{vallet_vc22}, \code{bouvard_vta},
#'   \code{rondeux_vc22}, \code{rondeux_vtot}, \code{algan_vta}, or \code{algan_vc22}.
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
#' data <- data.frame(
#'   tree_id = 1:3,
#'   species_code = c("PINUS_SYLVESTRIS", "QUERCUS_RUBRA", "FAGUS_SYLVATICA"),
#'   c150 = c(145, NA, NA),
#'   c130 = c(NA, 156, NA),
#'   dbh  = c(NA, NA, 40),
#'   htot = c(25, 30, 28),
#'   hdom = c(NA, 32, NA)
#' )
#' GCubeR(data)
#' 
#' @export
GCubeR <- function(data, output = NULL, volume_col = NULL) {
  stopifnot(is.data.frame(data))
  
  if (!"species_code" %in% names(data)) {
    stop("Missing column 'species_code'.")
  }
  
  # Pipeline ----
  data <- c150_c130(data)
  data <- add_c130_dbh(data)
  data <- dagnelie_vc22_1(data)
  data <- dagnelie_br(data)
  if ("hdom" %in% names(data)) data <- dagnelie_vc22_1g(data)
  if ("htot" %in% names(data)) {
    data <- dagnelie_vc22_2(data)
    data <- vallet_vta(data)
    data <- vallet_vc22(data)
    data <- algan_vta_vc22(data)
    data <- rondeux_vc22_vtot(data)
    data <- bouvard_vta(data)
  }
  data <- biomass_calc(data)
  
  # ---- Plotage after biomass ----
  if (is.null(volume_col)) {
    vol_candidates <- c("dagnelie_vc22_1", "dagnelie_vc22_1g", "dagnelie_vc22_2", "dagnelie_br", "vallet_vta", "vallet_vc22", "bouvard_vta", "rondeux_vc22", "rondeux_vtot", "algan_vta", "algan_vc22")
    volume_col <- vol_candidates[vol_candidates %in% names(data)][1]
  }
  
  if (!is.null(volume_col) && any(!is.na(data[[volume_col]]))) {
    plotage <- plot_by_class(data, volume_col = volume_col)
    print(plotage$plot)
    print(plotage$table)
  } else {
    plotage <- NULL
  }
  
  
  # Export if requested ----
  export_output(data, output)
  
  # ---- Return enriched data ----
  return(data)
}