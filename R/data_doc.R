#' Example dataset for using GCubeR
#'
#' An inventory dataset in the Bois Jacques Rondeux in Gembloux (Belgium, Wallonia).
#' Data were collected during a student project from Gembloux AgroBio Tech
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{id_tree}{Unique ID of the tree (integer)}
#'   \item{species}{Latin name of the species (character)}
#'   \item{species_code}{Species code in GCubeR (character)}
#'   \item{c150}{Circumference of the tree at 1m50 (numeric, cm)}
#'   \item{htot}{Total height of the tree (numeric, m)}
#' }
#'
#' @source Gauthier Ligot and Hugues Claessens
#' @usage data(data_rondeux)
"data_rondeux"

#' Equations metadata for GCubeR
#'
#' A reference table compiling metadata about allometric equations used in GCubeR
#' (Vallet, Dagnelie, Algan, Rondeux, CNPF, etc.). This dataset is provided
#' for information purposes only and is not directly used by package functions.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{eq_id}{Equation identifier (character)}
#'   \item{method}{Method family (Vallet, Dagnelie, Algan, Rondeux, CNPF…)}
#'   \item{predicted_variable}{Predicted variable (volume, biomass, carbon…)}
#'   \item{output_unit}{Unit of the output (m3, kg, tdm…)}
#'   \item{species_id}{Numeric species identifier (integer)}
#'   \item{species_name_fr}{Species name in French (character)}
#'   \item{species_code}{Species code (uppercase Latin name)}
#'   \item{validity_region}{Region of validity (text)}
#'   \item{validity_range}{Range of validity (text)}
#'   \item{input_variable}{Input variables required (e.g. c130, htot, dbh)}
#'   \item{input_unit}{Units of input variables (e.g. cm, m)}
#'   \item{formula_type}{Equation type (e.g. polynomial, exponential)}
#'   \item{explicit_formula}{Explicit formula as text}
#'   \item{coeff_a}{Equation coefficient a (numeric)}
#'   \item{coeff_b}{Equation coefficient b (numeric)}
#'   \item{coeff_c}{Equation coefficient c (numeric)}
#'   \item{coeff_d}{Equation coefficient d (numeric)}
#'   \item{coeff_e}{Equation coefficient e (numeric)}
#'   \item{coeff_f}{Equation coefficient f (numeric)}
#'   \item{remarks}{Additional notes}
#'   \item{reference_source}{Bibliographic source}
#' }
#'
#' @source Internal CSV file \code{data-raw/equations_GCubeR.csv}
#' @usage data(equations_GCubeR)
"equations_GCubeR"

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

#' Vallet coefficients for merchantable volume (vc22)
#'
#' Species-specific polynomial coefficients (a, b, c) used in the Vallet model
#' to compute commercial wood volume (vc22) up to a 7 cm top diameter.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Coefficient a (numeric)}
#'   \item{coeff_b}{Coefficient b (numeric)}
#'   \item{coeff_c}{Coefficient c (numeric)}
#' }
#'
#' @source Internal CSV file \code{data-raw/vallet_vc22.csv}
#' @usage data(val_vc22)
"val_vc22"

#' Vallet coefficients for total aboveground volume (VTA)
#'
#' Species-specific polynomial coefficients (a, b, c, d) used in the Vallet
#' form factor model to compute total aboveground volume (VTA).
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character)}
#'   \item{coeff_a}{Coefficient a (numeric)}
#'   \item{coeff_b}{Coefficient b (numeric)}
#'   \item{coeff_c}{Coefficient c (numeric)}
#'   \item{coeff_d}{Coefficient d (numeric)}
#' }
#'
#' @source Internal CSV file \code{data-raw/vallet_vta.csv}
#' @usage data(val_vta)
"val_vta"

#' Wood density table for biomass calculation
#'
#' Provides species-specific wood density values (t/m3) and species group
#' classification (conifer vs broadleaf) used in CNPF and Vallet biomass
#' estimation methods.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Tree species code (character, uppercase Latin format)}
#'   \item{density}{Wood density in tonnes of dry matter per cubic meter (numeric)}
#'   \item{con_broad}{Species group: "conifer" or "broadleaf"}
#' }
#'
#' @source Internal CSV file \code{data-raw/density_table.csv}
#' @usage data(density_table)
"density_table"