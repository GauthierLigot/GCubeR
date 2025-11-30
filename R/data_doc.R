#' Coefficients for circumference conversion (1.50 m ↔ 1.30 m)
#'
#' Species-specific linear coefficients used to convert stem circumference
#' between 1.50 m (\code{c150}) and 1.30 m (\code{c130}).
#' These coefficients are used internally by \code{c150_c130}.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Slope coefficient a (numeric)}
#'   \item{coeff_b}{Intercept coefficient b (numeric)}
#'   \item{min_c150}{Minimum valid circumference at 1.50 m (cm)}
#'   \item{max_c150}{Maximum valid circumference at 1.50 m (cm)}
#' }
#'
#' @source Internal CSV file \code{data-raw/c150_c130_coeff.csv}
#' @usage data(c150_c130_coeff)
"c150_c130_coeff"

#' Dagnelie coefficients (tarif 1)
#'
#' Species-specific polynomial coefficients for the Dagnelie single-entry
#' tarif-1 volume equations used by \code{dagnelie_vc22_1}.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Coefficient a (numeric)}
#'   \item{coeff_b}{Coefficient b (numeric)}
#'   \item{coeff_c}{Coefficient c (numeric)}
#'   \item{coeff_d}{Coefficient d (numeric)}
#'   \item{min_c130}{Minimum circumference at 1.30 m (cm)}
#'   \item{max_c130}{Maximum circumference at 1.30 m (cm)}
#' }
#'
#' @source Internal CSV file \code{data-raw/dan1.csv}
#' @usage data(dan1)
"dan1"

#' Dagnelie coefficients (tarif 1g)
#'
#' Species-specific coefficients for the Dagnelie vc22 model (variant 1g).
#' Loaded from \code{data-raw/dan1g.csv}.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Coefficient a (numeric)}
#'   \item{coeff_b}{Coefficient b (numeric)}
#'   \item{coeff_c}{Coefficient c (numeric)}
#'   \item{coeff_d}{Coefficient d (numeric)}
#'   \item{coeff_e}{Coefficient e (numeric)}
#'   \item{coeff_f}{Coefficient f (numeric)}
#'   \item{min_c130}{Minimum circumference at 1.30m (cm)}
#'   \item{max_c130}{Maximum circumference at 1.30m (cm)}
#'   \item{min_hdom}{Minimum dominant height (m)}
#'   \item{max_hdom}{Maximum dominant height (m)}
#' }
#'
#' @source Internal CSV file \code{data-raw/dan1g.csv}
#' @usage data(dan1g)
"dan1g"

#' Dagnelie coefficients (tarif 2)
#'
#' Species-specific polynomial coefficients for the Dagnelie two-entry
#' tarif-2 volume equations used by \code{dagnelie_vc22_2}.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Coefficient a (numeric)}
#'   \item{coeff_b}{Coefficient b (numeric)}
#'   \item{coeff_c}{Coefficient c (numeric)}
#'   \item{coeff_d}{Coefficient d (numeric)}
#'   \item{coeff_e}{Coefficient e (numeric)}
#'   \item{coeff_f}{Coefficient f (numeric)}
#'   \item{min_c130}{Minimum circumference at 1.30 m (cm)}
#'   \item{max_c130}{Maximum circumference at 1.30 m (cm)}
#'   \item{min_htot}{Minimum total height (m)}
#'   \item{max_htot}{Maximum total height (m)}
#' }
#'
#' @source Internal CSV file \code{data-raw/dan2.csv}
#' @usage data(dan2)
"dan2"

#' Dagnelie branch coefficients (tarif "br")
#'
#' Species-specific polynomial coefficients for the Dagnelie branch volume model (tarif "br").
#' Loaded from \code{data-raw/danbr.csv}.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Coefficient a (numeric)}
#'   \item{coeff_b}{Coefficient b (numeric)}
#'   \item{coeff_c}{Coefficient c (numeric)}
#'   \item{coeff_d}{Coefficient d (numeric)}
#'   \item{min_c130}{Minimum circumference at 1.30 m (cm)}
#'   \item{max_c130}{Maximum circumference at 1.30 m (cm)}
#' }
#'
#' @source Internal CSV file \code{data-raw/danbr.csv}
#' @usage data(danbr)
"danbr"
