#' Summarise and plot standing volume by c130 class and species
#'
#' This function builds a cross-tabulated volume table by species and c130
#' classes, adds a TOTAL row per class, optionally exports the table as a CSV,
#' and returns a ggplot object showing the volume distribution by c130 class.
#'
#' The table has:
#' \itemize{
#'   \item rows = species (plus a \code{"TOTAL"} row),
#'   \item columns = c130 classes (e.g. \code{[30,55)}, \code{[55,80)}, ...),
#'   \item cells = summed volume per species and c130 class.
#' }
#'
#' The plot shows a volume-weighted histogram (or barchart) by c130 class,
#' stacked by species, with a trend line for total volume per class and
#' dashed vertical lines marking small, medium and large wood thresholds.
#'
#' @param data A data frame containing at least:
#'   \itemize{
#'     \item \code{c130}: stem circumference at 1.30 m (cm),
#'     \item \code{species_code}: species identifier,
#'     \item a volume column (defaults to \code{"dagnelie_vc22_2"}).
#'   }
#' @param volume_col Name of the column containing tree volume (string).
#'   Defaults to \code{"dagnelie_vc22_2"}.
#' @param breaks Numeric vector defining c130 class boundaries (cm).
#'   Default is \code{seq(30, 230, by = 25)}.
#' @param small_limit Threshold between small and medium wood (cm of c130).
#'   A vertical dashed line is drawn at this value in the plot. Default is 60.
#' @param medium_limit Threshold between medium and large wood (cm of c130).
#'   A vertical dashed line is drawn at this value in the plot. Default is 120.
#' @param output Optional file path where the cross-tabulated table should be
#'   exported as a CSV. If \code{NULL} (default), no file is written.
#'   Export is handled by the utility function \code{export_output()}.
#' @param make_plot Logical; if \code{TRUE} (default), a ggplot object is
#'   created and returned alongside the table.
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{table}: data frame with species as rows and c130 classes
#'           as columns, plus a TOTAL row.
#'     \item \code{plot}: a \code{ggplot2} object (or \code{NULL} if
#'           \code{make_plot = FALSE}).
#'   }
#'
#' @details
#' The c130 classes are built with \code{cut()} using \code{breaks} as class
#' boundaries and an open-ended last class (using \code{Inf} as the upper
#' bound). The resulting factor labels (e.g. \code{"[30,55)"}) are used as
#' column names in the cross-tabulated table.
#'
#' For the plot, volume is used as a weight so that bar heights represent
#' total volume per c130 class. A trend line is computed from total volume
#' per class midpoint using the same binning scheme.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @examples
#' # Simulated dataset with 150 trees
#' set.seed(123)
#' n <- 150
#' c130 <- runif(n, 30, 230)
#' htot <- 0.25 * c130 + rnorm(n, 0, 3)
#' htot <- pmax(5, pmin(htot, 45))
#'
#' species_list <- c(
#'   "PINUS_SYLVESTRIS", "PICEA_ABIES",
#'   "QUERCUS_ROBUR", "FAGUS_SYLVATICA", "BETULA_SP"
#' )
#' species_code <- sample(species_list, n, replace = TRUE)
#'
#' df <- data.frame(
#'   c130 = round(c130, 1),
#'   htot = round(htot, 1),
#'   species_code = species_code
#' )
#'
#' # Example: compute Dagnelie tarif 2 volume, then summarise and plot
#' df <- dagnelie_vc22_2(df)
#' res <- plot_by_class(df, volume_col = "dagnelie_vc22_2")
#'
#' # Inspect the table
#' res$table
#'
#' # Print the plot
#' print(res$plot)
#'
#' @export
plot_by_class <- function(
    data,
    volume_col   = "dagnelie_vc22_2",
    breaks       = seq(30, 230, by = 25),
    small_limit  = 60,
    medium_limit = 120,
    output       = NULL,
    make_plot    = TRUE
) {
  stopifnot(is.data.frame(data))
  
  # ---- Basic checks ----
  if (!volume_col %in% names(data)) {
    stop("Volume column '", volume_col, "' not found in 'data'.")
  }
  if (!"c130" %in% names(data)) {
    stop("Column 'c130' is required in 'data'.")
  }
  if (!"species_code" %in% names(data)) {
    stop("Column 'species_code' is required in 'data'.")
  }
  
  vol_col <- volume_col
  
  # Ensure equally spaced breaks to derive binwidth for the plot
  diffs <- diff(breaks)
  if (length(unique(diffs)) != 1) {
    stop("Argument 'breaks' must define equally spaced classes (constant width).")
  }
  binwidth <- unique(diffs)
  x_min <- min(breaks)
  x_max <- max(breaks)
  
  # ---- Build c130 classes ----
  data$class <- cut(
    data$c130,
    breaks = c(breaks, Inf),
    include.lowest = TRUE,
    right = FALSE
  )
  
  # Filter only rows with non-NA volume and class
  data_valid <- data %>%
    dplyr::filter(
      !is.na(.data[[vol_col]]),
      !is.na(class)
    )
  
  if (nrow(data_valid) == 0) {
    stop("No valid rows with non-NA volume and c130 class.")
  }
  
  # ---- 1) Cross-tabulated table (species x class) ----
  mat <- data_valid %>%
    dplyr::group_by(species_code, class) %>%
    dplyr::summarise(
      volume = sum(.data[[vol_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = class,
      values_from = volume,
      values_fill = 0
    )
  
  # Reorder class columns by lower bound numeric value
  class_cols <- setdiff(names(mat), "species_code")
  lower_bounds <- as.numeric(sub("^\\[([^,]+),.*$", "\\1", class_cols))
  class_cols <- class_cols[order(lower_bounds)]
  mat <- mat[, c("species_code", class_cols)]
  
  # Add TOTAL row (sum over species for each class)
  total_row <- mat %>%
    dplyr::ungroup() %>%
    dplyr::select(-species_code) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) %>%
    dplyr::mutate(species_code = "TOTAL") %>%
    dplyr::select(species_code, dplyr::everything())
  
  table_out <- dplyr::bind_rows(mat, total_row)
  
  # ---- Optional export ----
  if (!is.null(output)) {
    export_output(table_out, output)
  }
  
  # ---- 2) Plot (histogram-style, weighted by volume) ----
  plot_out <- NULL
  
  if (make_plot) {
    df_plot <- data_valid %>%
      dplyr::filter(
        c130 >= x_min,
        c130 <= x_max
      ) %>%
      dplyr::mutate(
        c130_bin = floor((c130 - x_min) / binwidth),
        c130_mid = x_min + c130_bin * binwidth + binwidth / 2
      )
    
    # Total volume per class midpoint for the trend line
    df_trend <- df_plot %>%
      dplyr::group_by(c130_mid) %>%
      dplyr::summarise(
        volume_total = sum(.data[[vol_col]], na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_out <- ggplot2::ggplot(df_plot, ggplot2::aes(x = c130)) +
      ggplot2::geom_histogram(
        ggplot2::aes(weight = .data[[vol_col]], fill = species_code),
        binwidth = binwidth,
        boundary = x_min,
        closed   = "left",
        position = "stack",
        alpha    = 0.8,
        color    = "black"
      ) +
      ggplot2::geom_line(
        data = df_trend,
        ggplot2::aes(x = c130_mid, y = volume_total, group = 1),
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_trend,
        ggplot2::aes(x = c130_mid, y = volume_total),
        size = 2
      ) +
      ggplot2::geom_vline(xintercept = small_limit, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = medium_limit, linetype = "dashed") +
      ggplot2::scale_x_continuous(
        limits = c(x_min, x_max),
        breaks = breaks
      ) +
      ggplot2::labs(
        x        = "c130 (cm)",
        y        = paste("Total volume (m3) -", vol_col),
        fill     = "Species",
        title    = "Standing volume by c130 class and species",
        subtitle = paste0(
          "Small wood: <", small_limit, " cm  |  ",
          "Medium wood: [", small_limit, ", ", medium_limit, "[ cm  |  ",
          "Large wood: >= ", medium_limit, " cm"
        )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }
  
  # ---- Return both table and plot ----
  return(list(
    table = table_out,
    plot  = plot_out
  ))
}
