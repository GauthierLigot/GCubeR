#' Load and merge coefficient tables from a CSV file
#'
#' This utility function reads a coefficient table stored in a CSV file
#' inside the `data-raw/` folder, automatically converts all columns
#' beginning with `"coeff_"` from expression format (e.g., `"0.19546*10^-1"`)
#' into true numeric values, and then joins these cleaned coefficients
#' to a user-provided data frame.
#'
#' The function allows different join column names between the input
#' `data` and the coefficient table, providing flexibility for various
#' datasets and coefficient sources.
#'
#' @param data A data.frame containing the data to which coefficients
#'   should be added. Must contain the join column specified in `by_data`.
#' @param file A character string giving the name of the CSV file located
#'   in the `data-raw/` directory (e.g., `"dan1.csv"`).
#' @param by_data Name of the join column in `data`.
#' @param by_coef Name of the join column in the coefficient CSV file.
#'
#' @return A data.frame identical to `data` but with cleaned coefficient
#'   columns (`coeff_*`) merged in.
#'
#' @import dplyr
#' @importFrom readr read_delim locale
#' @export
clean_join <- function(data, file, by_data, by_coef) {
  
  library(dplyr)
  library(readr)
  
  # ---- Match checking ----
  stopifnot(is.data.frame(data))
  if (!by_data %in% names(data)) {
    stop("Column ", by_data, " not found in 'data'.")
  }
  
  # ---- File reading ----
  csv_path <- file.path("data-raw", file)
  
  coef_tab <- read_delim(
    file = csv_path,
    delim = ";",
    locale = locale(decimal_mark = ",", encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  if (!by_coef %in% names(coef_tab)) {
    stop("Column ", by_coef, " not found in coefficient table.")
  }
  
  # Identify coefficient columns ----
  coef_cols <- grep("^coeff_", names(coef_tab), value = TRUE)
  
  # Convert all coeff_* columns to numeric using parse_expr_num() ----
  coef_tab <- coef_tab %>%
    mutate(across(all_of(coef_cols), ~ parse_expr_num(.x)))
  
  # Remove any existing coeff_* columns from data ----
  data <- data %>% select(-any_of(coef_cols))
  
  # Join cleaned coefficients ----
  data <- left_join(
    data,
    coef_tab %>% select(all_of(c(by_coef, coef_cols))),
    by = setNames(by_coef, by_data)
  )
  
  return(data)
}
