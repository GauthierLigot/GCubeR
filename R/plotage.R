#' Cross-tabulated volume table by species and c130 classes
#'
#' Produces a cross-tab volume table where:
#' - rows = species (plus TOTAL row),
#' - columns = c130 classes,
#' - cells = summed volume per species per class.
#'
#' Optionally exports the table to CSV using `export_output()`.
#'
#' @param data A data frame containing:
#'   \itemize{
#'     \item \code{c130}: circumference at 1.30 m (cm)
#'     \item \code{species_code}: species identifier
#'     \item a volume column (string)
#'   }
#' @param volume_col Name of the column containing tree volume (string).
#' @param breaks Numeric vector for class boundaries (cm). Defaults to \code{seq(30, 230, by = 25)}.
#' @param output Optional file path where the resulting data frame should be
#'   exported as a CSV. If \code{NULL} (default), no file is written.
#'
#' @return A data frame with species as rows and c130 classes as columns,
#'   plus a TOTAL row for each class.
#'
#' @import dplyr
#' @import tidyr
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   c130 = runif(150, 30, 230),
#'   species_code = sample(c("QUERCUS_ROBUR", "PICEA_ABIES", "BETULA_SP"), 150, TRUE),
#'   vol = runif(150, 0.2, 1.5)
#' )
#'
#' volume_crosstab(df, volume_col = "vol")
#'
#' @export
volume_crosstab <- function(
    data,
    volume_col = "dagnelie_vc22_2",
    breaks = seq(30, 230, by = 25),
    output = NULL
) {
  # ---- Sanity check ----
  if (!volume_col %in% names(data)) {
    stop("Volume column '", volume_col, "' not found in dataset.")
  }
  if (!"c130" %in% names(data)) {
    stop("Column 'c130' is required in 'data'.")
  }
  if (!"species_code" %in% names(data)) {
    stop("Column 'species_code' is required in 'data'.")
  }
  
  # ---- Build c130 classes ----
  data$class <- cut(
    data$c130,
    breaks = c(breaks, Inf),
    include.lowest = TRUE,
    right = FALSE
  )
  
  # ---- Summarise volume per species × class ----
  mat <- data %>%
    dplyr::group_by(species_code, class) %>%
    dplyr::summarise(volume = sum(.data[[volume_col]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = class,
      values_from = volume,
      values_fill = 0
    )
  
  # ---- Reorder class columns by numeric lower bound ----
  class_cols <- setdiff(names(mat), "species_code")
  
  # Extraire la borne inférieure des labels de classe, ex: "[30,55)" -> 30
  lower_bounds <- as.numeric(sub("^\\[([^,]+),.*$", "\\1", class_cols))
  
  class_cols <- class_cols[order(lower_bounds)]
  
  mat <- mat[, c("species_code", class_cols)]
  
  # ---- Add TOTAL row ----
  total_row <- mat %>%
    dplyr::ungroup() %>%
    dplyr::select(-species_code) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) %>%
    dplyr::mutate(species_code = "TOTAL") %>%
    dplyr::select(species_code, dplyr::everything())
  
  result <- dplyr::bind_rows(mat, total_row)
  
  # ---- Export if requested ----
  if (!is.null(output)) {
    export_output(result, output)
  }
  
  return(result)
}
