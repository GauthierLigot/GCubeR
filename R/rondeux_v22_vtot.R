#' @title Calculate Total and Commercial Stem Volume (Rondeux, larch)
#'
#' @description
#' Computes the total stem volume (\code{vtot_rondeux}) and the commercial
#' stem volume at 22 cm (\code{vc22_rondeux}) for larch trees according to
#' Rondeux equations, based on the circumference at 1.30 m (\code{c130}, in cm)
#' and the total height (\code{htot}, in meters).
#'
#' @details
#' The implemented equations are:
#'
#' \deqn{v_{\mathrm{tot}} = a_{\mathrm{vtot}} \cdot c_{130}^2 \cdot h_{\mathrm{tot}}}
#' where \eqn{a_{\mathrm{vtot}} = 0.406780 \times 10^{-5}}.
#'
#' \deqn{v_{c22} = a_{\mathrm{vc22}} + b_{\mathrm{vc22}} \cdot c_{130}^2 \cdot h_{\mathrm{tot}}}
#' where \eqn{a_{\mathrm{vc22}} = -0.008877} and
#' \eqn{b_{\mathrm{vc22}} = 0.412411 \times 10^{-5}}.
#'
#' Expected units:
#' \itemize{
#'   \item \code{c130}: cm
#'   \item \code{htot}: m
#'   \item output volumes: m³
#' }
#'
#' These equations are valid **only for larch**. Rows with other species are
#' returned with \code{NA} volumes and a warning is issued.
#'
#' @param data A data frame containing tree measurements. Must include:
#'   \code{species_code} (uppercase character),
#'   \code{c130} (circumference at 1.30 m, in cm),
#'   \code{htot} (total height, in m).
#'
#' @param na_action How to handle missing essential values (\code{c130}, \code{htot}).
#'   \code{"error"} (default) stops if missing values are detected.
#'   \code{"omit"} removes rows with missing essential fields.
#'
#' @return
#' A data frame identical to \code{data}, with two added columns:
#' \itemize{
#'   \item \code{vtot_rondeux}: total stem volume (m³)
#'   \item \code{vc22_rondeux}: commercial stem volume at 22 cm (m³)
#' }
#'
#' @examples
#' df <- data.frame(
#'   species_code = c("LARIX_DECIDUA", "LARIX_DECIDUA", "LARIX_DECIDUA"),
#'   c130 = c(60, 65, 55),
#'   htot = c(15, 18, 20)
#' )
#'
#' v_rondeux_larch(df)
#'
#' @export
v_rondeux_larch <- function(data,
                            na_action = c("error", "omit")) {
  
  na_action <- match.arg(na_action)
  
  # --- RONDEUX MODEL CONSTANTS (LARCH) ----
  a_vtot  <- 0.406780e-5        # coefficient for total volume
  a_vc22  <- -0.008877          # intercept for commercial volume 22cm
  b_vc22  <- 0.412411e-5        # slope for commercial volume 22cm
  
  # --- REQUIRED COLUMNS ----
  required_columns <- c("species_code", "c130", "htot")
  missing_cols <- setdiff(required_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("Input data is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # --- C130 RANGE CHECK (10–70 cm) ----
  min_c130 <- 10
  max_c130 <- 70
  
  rows_c130_out <- which(!is.na(data$c130) &
                           (data$c130 < min_c130 | data$c130 > max_c130))
  
  if (length(rows_c130_out) > 0) {
    warning(
      paste0("Circumference constraint violated: ",
             length(rows_c130_out), 
             " tree(s) have c130 < ", min_c130, 
             " cm or > ", max_c130, " cm. ",
             "Rondeux volumes will be set to NA for these rows: ",
             paste(rows_c130_out, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # --- HANDLE NA VALUES ----
  core_na <- which(is.na(data$c130) | is.na(data$htot))
  if (length(core_na) > 0) {
    if (na_action == "error") {
      stop("Missing values detected in 'c130' or 'htot' for rows: ",
           paste(core_na, collapse = ", "),
           ". Provide complete data or use na_action = 'omit'.")
    } else {
      data <- data[-core_na, , drop = FALSE]
    }
  }
  
  # --- STANDARDIZE SPECIES CODE ----
  data$species_code <- toupper(trimws(data$species_code))
  
  # --- IDENTIFY LARCH ROWS ----
  is_larch <- data$species_code %in% c("LARIX_DECIDUA", "LARIX_SPP")
  
  if (any(!is_larch)) {
    warning(
      "Rondeux larch equations apply only to species_code 'LARIX_DECIDUA' or 'LARIX_SPP'. ",
      "Non-larch rows will receive NA volumes.",
      call. = FALSE
    )
  }
  
  # --- INITIALIZE RESULT COLUMNS ----
  data$vtot_rondeux <- NA_real_
  data$vc22_rondeux <- NA_real_
  
  
  # --- COMPUTE VOLUMES FOR LARCH ----
  idx <- which(is_larch)
  
  if (length(idx) > 0) {
    c130_sq <- data$c130[idx]^2
    htot    <- data$htot[idx]
    
    # total volume
    data$vtot_rondeux[idx] <- a_vtot * c130_sq * htot
    
    # commercial volume 22 cm
    data$vc22_rondeux[idx] <- a_vc22 + b_vc22 * c130_sq * htot
  }
  
  # --- INVALIDATE RESULTS FOR C130 OUT OF RANGE ----
  if (length(rows_c130_out) > 0) {
    data$vtot_rondeux[rows_c130_out] <- NA
    data$vc22_rondeux[rows_c130_out] <- NA
  }
  
  return(data)
}



df_test <- data.frame(
  species_code = "LARIX_DECIDUA",
  c130 = 50,
  htot = 20
)
devtools::load_all()
v_rondeux_larch(df_test)

