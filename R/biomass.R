#' Total Biomass, Carbon and CO₂ Estimation for Tree Species
#'
#' Computes total biomass (aboveground + root), carbon content and CO₂ equivalent
#' for tree species using CNIEFEB (with multiple trunk volume sources) and Vallet methods.
#'
#' @param data A data frame containing volume and species information for each tree.
#'   Must include:
#'   - `species_code`: species name in uppercase Latin format (e.g. `"PICEA_ABIES"`),
#'     matched against a density table.
#'   - At least one volume column:
#'     - For CNIEFEB method (trunk volume):
#'       - Dagnelie equations: `dagnelie_vc22_2`, `dagnelie_vc22_1g`, `dagnelie_vc22_1`
#'         (priority order: `dagnelie_vc22_2` > `dagnelie_vc22_1g` > `dagnelie_vc22_1`)
#'       - Vallet equation: `vallet_vc22`
#'       - Rondeux equation: `rondeux_vc22`
#'       - Algan equation: `algan_vc22`
#'     - For Vallet method (total aboveground volume): `vallet_vta`
#'
#'   If multiple trunk volumes are provided, CNIEFEB is computed separately for each source.
#'   If only one is available, the corresponding method is applied.
#'   All volume columns must be numeric and expressed in cubic meters (m³).
#'
#' @param na_action How to handle missing values. `"error"` (default) stops if any required
#'   value is missing. `"omit"` removes rows with missing values.
#' @param output Optional file path where the resulting data frame should be 
#'   exported as a CSV. If NULL (default), no file is written.
#'   Export is handled by the utility function \code{export_output()} and
#'   failures trigger warnings without interrupting execution.
#'   
#' @return A data frame with one row per tree, including:
#' - `species_code`: species name in uppercase Latin format.
#' - `dagnelie_vc22_1`, `dagnelie_vc22_1g`, `dagnelie_vc22_2`, `vallet_vc22`, `rondeux_vc22`,`algan_vc22`:
#'   optional trunk volume inputs (in m³).
#' - `vallet_vta`: optional total aboveground volume (in m³) for Vallet method.
#' - `vc22_dagnelie`: selected trunk volume used for CNIEFEB (Dagnelie), based on priority.
#' - `vc22_source`: name of the Dagnelie column used to populate `vc22`.
#'
#' ### CNIEFEB method outputs:
#' - From Dagnelie (priority selection):
#'   - `cniefeb_dagnelie_bag`, `cniefeb_dagnelie_bbg`, `cniefeb_dagnelie_btot`,
#'     `cniefeb_dagnelie_c`, `cniefeb_dagnelie_co2`
#' - From Vallet trunk volume (`vallet_vc22`):
#'   - `cniefeb_vallet_bag`, `cniefeb_vallet_bbg`, `cniefeb_vallet_btot`,
#'     `cniefeb_vallet_c`, `cniefeb_vallet_co2`
#' - From Rondeux trunk volume (`rondeux_vc22`):
#'   - `cniefeb_rondeux_bag`, `cniefeb_rondeux_bbg`, `cniefeb_rondeux_btot`,
#'     `cniefeb_rondeux_c`, `cniefeb_rondeux_co2`
#' - From Algan trunk volume (`algan_vc22`):
#'   - `cniefeb_algan_bag`, `cniefeb_algan_bbg`, `cniefeb_algan_btot`,
#'     `cniefeb_algan_c`, `cniefeb_algan_co2`

#'
#' ### Vallet method outputs (if `vallet_vta` is available and species is compatible):
#' - `vallet_bag`, `vallet_bbg`, `vallet_btot`, `vallet_c`, `vallet_co2`
#'
#' @details
#' - The density table provides:
#'   - `density`: wood density in tonnes of dry matter per cubic meter (t/m³).
#'   - `con_broad`: species group, either `"conifer"` or `"broadleaf"`.
#' - The expansion factor `feb` is  derived from `con_broad`:
#'   - `feb = 1.3` for conifers
#'   - `feb = 1.56` for broadleaves
#' - Dagnelie trunk volume (`vc22_dagnelie`) is automatically selected from the best available column,
#'   in the following priority: `dagnelie_vc22_2` > `dagnelie_vc22_1g` > `dagnelie_vc22_1`.
#' - CNIEFEB outputs are computed separately for each trunk volume source (Dagnelie, Vallet, Rondeux, Algan).
#' - Vallet method is applied only to a predefined list of compatible species using `vallet_vta`.
#' - If required columns are missing, the corresponding method is skipped with a warning.
#' - Warnings are also displayed if trunk volume columns exist but contain missing values (`NA`).
#' - All biomass values are expressed in tonnes of dry matter (t),
#'   carbon in tonnes of carbon (t C), and CO₂ in tonnes of CO₂ equivalent (t CO₂).
#'
#' @import dplyr
#' @import readr
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   species_code = c("PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA"),
#'   dagnelie_vc22_2 = c(1.1, NA, NA),
#'   dagnelie_vc22_1g = c(NA, NA, NA),
#'   dagnelie_vc22_1 = c(NA, 0.9, NA),
#'   vallet_vc22 = c(NA, 1.2, NA),
#'   rondeux_vc22 = c(NA, NA, 1.0),
#'   algan_vc22 = c(NA,0.8,NA),
#'   vallet_vta = c(1.5, NA, 1.3)
#' )
#'
#' # Run biomass calculation and export to CSV
#' output_path <- tempfile(fileext = ".csv")
#' results <- biomass_calc(data, output = output_path)
#' if (file.exists(output_path)) {
#'   message("✅ CSV file successfully created.")
#' } else {
#'   warning("⚠️ CSV file was not created.")
#' }
#'
#' @export
# BIOMASS CALCULATION ----
biomass_calc <- function(data,
                         na_action = c("error", "omit"),
                         output = NULL) {
  
  na_action <- match.arg(na_action)
  # INPUT CHECKS ----  
  ## vc22 selection and validation for Dagnelie ----
  ### Define required and candidate columns ----
  required_columns <- c("species_code")
  
  #### Dagnelie candidates (priority order) ----
  vc22_candidates <- c("dagnelie_vc22_2", "dagnelie_vc22_1g", "dagnelie_vc22_1")
  
  #### Identify available Dagnelie vc22 columns (respecting priority) ----
  vc22_available <- intersect(vc22_candidates, names(data))  # preserves priority order
  
  #### Create vc22 column  for Dagnelie ----
  if (length(vc22_available) > 0) {
    data <- data %>% mutate(vc22_dagnelie= coalesce(!!!syms(vc22_available)))
  } else {
    message("⚠️ No vc22 column found (dagnelie_vc22_2, dagnelie_vc22_1g, dagnelie_vc22_1). CNIEFEB method (Dagnelie) will be skipped.")
  }
  
  #### Create vc22_source column safely (only for Dagnelie) ----
  if (length(vc22_available) > 0) {
    data$vc22_source <- NA_character_
    for (col in vc22_candidates) {
      if (col %in% names(data)) {
        data$vc22_source[is.na(data$vc22_source) & !is.na(data[[col]])] <- col
      }
    }
  }
  
  #### Notify per-row missing  ----
  if (length(vc22_available) > 0) {
    rows_missing_vc22 <- which(rowSums(!is.na(data[vc22_available])) == 0)
    if (length(rows_missing_vc22) > 0) {
      message("⚠️ The following rows have no trunk volume values in any of the Dagnelie columns (dagnelie_vc22_2, dagnelie_vc22_1g, dagnelie_vc22_1). CNIEFEB (Dagnelie) will be skipped for these rows: ",
              paste(rows_missing_vc22, collapse = ", "))
    }
  }
  
  if ("vallet_vc22" %in% names(data)) {
    rows_missing_vallet <- which(is.na(data$vallet_vc22))
    if (length(rows_missing_vallet) > 0) {
      message("⚠️ The following rows have no trunk volume values in column 'vallet_vc22'. CNIEFEB (Vallet) will be skipped for these rows: ",
              paste(rows_missing_vallet, collapse = ", "))
    }
  }
  
  if ("rondeux_vc22" %in% names(data)) {
    rows_missing_rondeux <- which(is.na(data$rondeux_vc22))
    if (length(rows_missing_rondeux) > 0) {
      message("⚠️ The following rows have no trunk volume values in column 'rondeux_vc22'. CNIEFEB (Rondeux) will be skipped for these rows: ",
              paste(rows_missing_rondeux, collapse = ", "))
    }
  }
  
  if ("algan_vc22" %in% names(data)) {
    rows_missing_algan <- which(is.na(data$algan_vc22))
    if (length(rows_missing_algan) > 0) {
      message("⚠️ The following rows have no trunk volume values in column 'algan_vc22'. CNIEFEB (Algan) will be skipped for these rows: ",
              paste(rows_missing_algan, collapse = ", "))
    }
  }
  
  ## Clean species names ----
  data <- data %>% mutate(species_code = toupper(trimws(species_code)))
  density_table <- density_table %>% mutate(
    species_code = toupper(trimws(species_code)),
    con_broad = tolower(trimws(con_broad)),
    density = as.numeric(gsub(",", ".", as.character(density)))
  )
  
  ## Merge with density table ----
  data <- left_join(
    data,
    density_table %>% select(species_code, density, con_broad),
    by = "species_code"
  )
  
  ## Handle missing values ----
  idx_keep <- rep(TRUE, nrow(data))
  
  ## Determine which rows are usable for each method ----
  has_species <- !is.na(data$species_code)
  
  has_dagnelie_vc22 <- "vc22_dagnelie" %in% names(data) && any(!is.na(data$vc22_dagnelie))
  has_vallet_vc22 <- "vallet_vc22" %in% names(data) && any(!is.na(data$vallet_vc22))
  has_rondeux_vc22 <- "rondeux_vc22" %in% names(data) && any(!is.na(data$rondeux_vc22))
  has_vallet_vta <- "vallet_vta" %in% names(data) && any(!is.na(data$vallet_vta))
  
  ## At least one usable volume source ----
  has_volume <- has_dagnelie_vc22 | has_vallet_vc22 | has_rondeux_vc22 | has_vallet_vta
  
  ## Apply filtering based on na_action ----
  if (na_action == "omit") {
    idx_keep <- has_species & has_volume
    data <- data[idx_keep, ]
  } else if (na_action == "error") {
    if (any(!has_species | !has_volume)) {
      stop("Missing values detected: at least one of vc22 (Dagnelie), vallet_vc22, rondeux_vc22 or vallet_vta must be present, and species_code must not be NA. Use na_action = 'omit' to ignore incomplete rows.")
    }
  }
  data <- data[idx_keep, ]
  
  ## Check species validity ----
  wrong <- setdiff(unique(data$species_code), density_table$species_code)
  if (length(wrong) > 0) {
    message("Unknown species : ", paste(wrong, collapse = ", "))
  }
  
  ## Ensure data exists to avoid errors  ----
  if (!"vallet_vta" %in% names(data)) {
    message("⚠️ Column 'vallet_vta' not found. Vallet method will be skipped.")
  }
  
  if (!"vallet_vc22" %in% names(data)) {
    message("⚠️ Column 'vallet_vc22' not found. CNIEFEB (Vallet) will be skipped.")
  }
  
  if (!"rondeux_vc22" %in% names(data)) {
    message("⚠️ Column 'rondeux_vc22' not found. CNIEFEB (Rondeux) will be skipped.")
  }
  
  if (!"algan_vc22" %in% names(data)) {
    message("⚠️ Column 'algan_vc22' not found. CNIEFEB (Algan) will be skipped.")
  }
  
  # CONSTANTS ----
  a_bbg <- 1.0587
  b_bbg <- 0.8836
  c_bbg <- 0.2840
  a_c   <- 0.475
  a_co2 <- 44 / 12

  # METHODS ----  
  ## Species compatible with Vallet method ----
  vallet_species <- c(
    "PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA",
    "PINUS_SYLVESTRIS", "PINUS_PINASTER", "ABIES_ALBA",
    "PSEUDOTSUGA_MENZIESII"
  )
  
  ## Separate calculations for CNIEFEB and Vallet ----
  data <- data %>%
    mutate(
      feb = case_when(
        con_broad == "conifer" ~ 1.3,
        con_broad == "broadleaf" ~ 1.56,
        TRUE ~ NA_real_
      )
    )
      
    ### CNIEFEB calculations ----
if ("vc22_dagnelie" %in% names(data)) {
  data <- data %>%
    mutate(
      cniefeb_dagnelie_bag = vc22_dagnelie * feb * density,
      cniefeb_dagnelie_bbg = exp(-a_bbg + b_bbg * log(cniefeb_dagnelie_bag) + c_bbg),
      cniefeb_dagnelie_btot = cniefeb_dagnelie_bag + cniefeb_dagnelie_bbg,
      cniefeb_dagnelie_c = cniefeb_dagnelie_btot * a_c,
      cniefeb_dagnelie_co2 = cniefeb_dagnelie_c * a_co2
    )
}

if ("vallet_vc22" %in% names(data)) {
      data <- data %>% mutate(
        cniefeb_vallet_bag = vallet_vc22 * feb * density,
        cniefeb_vallet_bbg = exp(-a_bbg + b_bbg * log(cniefeb_vallet_bag) + c_bbg),
        cniefeb_vallet_btot = cniefeb_vallet_bag + cniefeb_vallet_bbg,
        cniefeb_vallet_c = cniefeb_vallet_btot * a_c,
        cniefeb_vallet_co2 = cniefeb_vallet_c * a_co2
      )
    }
    
if ("rondeux_vc22" %in% names(data)) {
      data <- data %>% mutate(
        cniefeb_rondeux_bag = rondeux_vc22 * feb * density,
        cniefeb_rondeux_bbg = exp(-a_bbg + b_bbg * log(cniefeb_rondeux_bag) + c_bbg),
        cniefeb_rondeux_btot = cniefeb_rondeux_bag + cniefeb_rondeux_bbg,
        cniefeb_rondeux_c = cniefeb_rondeux_btot * a_c,
        cniefeb_rondeux_co2 = cniefeb_rondeux_c * a_co2
      )
    }

  if ("algan_vc22" %in% names(data)) {
    data <- data %>% mutate(
      cniefeb_algan_bag = algan_vc22 * feb * density,
      cniefeb_algan_bbg = exp(-a_bbg + b_bbg * log(cniefeb_algan_bag) + c_bbg),
      cniefeb_algan_btot = cniefeb_algan_bag + cniefeb_algan_bbg,
      cniefeb_algan_c = cniefeb_algan_btot * a_c,
      cniefeb_algan_co2 = cniefeb_algan_c * a_co2
    )
  }
  
   ### Vallet method ----
  if ("vallet_vta" %in% names(data)) {
    data <- data %>%
      mutate(
        vallet_bag = if_else(species_code %in% vallet_species & !is.na(vallet_vta),
                             vallet_vta * density, NA_real_),
        vallet_bbg = if_else(!is.na(vallet_bag),
                             exp(-a_bbg + b_bbg * log(vallet_bag) + c_bbg), NA_real_),
        vallet_btot = vallet_bag + vallet_bbg,
        vallet_c = vallet_btot * a_c,
        vallet_co2 = vallet_c * a_co2
      )
  }
  data <- data %>%
    select(-feb, -con_broad, -density)
  
  # exporting the file using function export_output ----
  export_output(data, output)
  return(data)
}