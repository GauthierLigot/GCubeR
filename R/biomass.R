#' Total Biomass, Carbon and CO₂ Estimation for Tree Species
#'
#' Computes total biomass (aboveground + root), carbon content and CO₂ equivalent for tree species
#' using CNIEFEB and Vallet methods. The function selects the best available trunk volume (`vc22`)
#' from multiple candidate columns for CNIEFEB and uses total aboveground volume (`v_vta`) for Vallet.
#'
#' @param data A data frame containing volume and species information for each tree.
#'   Must include:
#'   - `species_code`: species name in uppercase Latin format (e.g. `"PICEA_ABIES"`), matched against a density table.
#'   - At least one volume column:
#'     - For CNIEFEB method (trunk volume): one of `d_vc22_2`, `d_vc22_1g`, or `d_vc22_1`
#'     - For Vallet method (total aboveground volume): `v_vta`
#'
#'   If both trunk and total aboveground volume are provided, the function computes both methods.
#'   If only one is available, the corresponding method is applied.
#'   The function automatically selects the best available trunk volume in priority order for CNIEFEB method.
#'   All volume columns must be numeric and expressed in cubic meters.
#'   
#' @param na_action How to handle missing values. `"error"` (default) stops if any required
#'   value is missing. `"omit"` removes rows with missing values.
#' @param output Optional file path to export the results as a CSV. If `NULL`, results are printed.
#'
#' @return A data frame with one row per tree, including:
#' - `species_code`: species name in uppercase Latin format.
#' - `d_vc22_1`, `d_vc22_1g`, `d_vc22_2`: optional trunk volume inputs (in m³).
#' - `v_vta`: optional total aboveground volume (in m³) for Vallet method.
#' - `vc22`: selected trunk volume used for CNIEFEB, based on priority.
#' - `vc22_source`: name of the column used to populate `vc22`.
#' - `density`: wood density in tonnes of dry matter per cubic meter (t/m³) matched from the reference table.
#' - `con_broad`: species group, either `"conifer"` or `"broadleaf"`.
#' - `feb`: expansion factor used in CNIEFEB method (1.3 for conifer or 1.56 for broadleaf).
#'
#' ### CNIEFEB method outputs (if `vc22` is available):
#' - `cniefeb_bag`: aboveground biomass in tonnes of dry matter (t).
#'   \deqn{cniefeb_bag = vc22 * feb * density}
#' - `cniefeb_bbg`: belowground biomass in tonnes of dry matter (t).
#'   \deqn{cniefeb_bbg = exp(-a_bbg + b_bbg * log(cniefeb_bag) + c_bbg)}
#' - `cniefeb_btot`: total biomass in tonnes of dry matter (t).
#'   \deqn{cniefeb_btot = cniefeb_bag + cniefeb_bbg}
#' - `cniefeb_c`: carbon stock in tonnes of carbon (t C).
#'   \deqn{cniefeb_c = cniefeb_btot * a_c}
#' - `cniefeb_co2`: CO₂ equivalent in tonnes of CO₂ (t CO₂).
#'   \deqn{cniefeb_co2 = cniefeb_c * a_co2}
#'
#' ### Vallet method outputs (if `v_vta` is available and species is compatible):
#' - `vallet_bag`: aboveground biomass in tonnes of dry matter (t).
#'   \deqn{vallet_bag = v_vta * density}
#' - `vallet_bbg`: belowground biomass in tonnes of dry matter (t).
#'   \deqn{vallet_bbg = exp(-a_bbg + b_bbg * log(vallet_bag) + c_bbg)}
#' - `vallet_btot`: total biomass in tonnes of dry matter (t).
#'   \deqn{vallet_btot = vallet_bag + vallet_bbg}
#' - `vallet_c`: carbon stock in tonnes of carbon (t C).
#'   \deqn{vallet_c = vallet_btot * a_c}
#' - `vallet_co2`: CO₂ equivalent in tonnes of CO₂ (t CO₂).
#'   \deqn{vallet_co2 = vallet_c * a_co2}
#'
#' @details
#' The function supports two biomass estimation methods: CNIEFEB and Vallet.
#' 
#' - Trunk volume (`vc22`) is automatically selected from the best available column, in the following priority: `d_vc22_2` > `d_vc22_1g` > `d_vc22_1`.
#' - Species codes are matched (case-insensitively) against a reference density table located at `"data-raw/density_table.csv"`.
#' - The Vallet method is applied only to a predefined list of compatible species.
#' - If required columns are missing, the corresponding method is skipped with a warning.
#' - All biomass values are expressed in tonnes of dry matter (t), carbon in tonnes of carbon (t C), and CO₂ in tonnes of CO₂ equivalent (teq CO₂).
#' 
#' @import dplyr
#' @import readr
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   species_code = c(
#'     "PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA", "PINUS_SYLVESTRIS",
#'     "BETULA_PENDULA", "ROBINIA_PSEUDOACACIA", "TILIA_CORDATA"
#'   ),
#'   d_vc22_1 = c(1.1, NA, NA, 0.9, 0.8, 0.7, 1.3),
#'   d_vc22_1g = c(NA, NA, NA, 1.3, NA, 1, NA),
#'   d_vc22_2 = c(1, NA, NA, NA, 1.1, NA, NA),
#'   v_vta = c(1,NA,1.1,NA,NA,NA,NA)
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
#' 
# BIOMASS CALCULATION ----
biomass_calc <- function(data,
                         na_action = c("error", "omit"),
                         output = NULL) {
  
  na_action <- match.arg(na_action)
  # INPUT CHECKS ----  
  ## Load density table ----
  path_density <- file.path("data-raw", "density_table.csv")
  density_table <- read_delim(
    file = path_density,
    delim = ";",
    locale = locale(decimal_mark = ",", encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  ## vc22 selection and validation ----
  ### Define required and candidate columns ----
  required_columns <- c("species_code")
  vc22_candidates <- c("d_vc22_2", "d_vc22_1g", "d_vc22_1")
  
  ### Identify available vc22 columns (respecting priority) ----
  vc22_available <- intersect(vc22_candidates, names(data))  # preserves priority order
  
  ### vc22 selection and diagnostics ----
  vc22_candidates <- c("d_vc22_2", "d_vc22_1g", "d_vc22_1")
  vc22_available <- intersect(vc22_candidates, names(data))
  
  # Create vc22 column
  if (length(vc22_available) > 0) {
    data <- data %>% mutate(vc22 = coalesce(!!!syms(vc22_available)))
  } else {
    data$vc22 <- NA_real_
    message("⚠️ No vc22 column found (d_vc22_2, d_vc22_1g, d_vc22_1). CNIEFEB method will be skipped.")
  }
  
  # Create vc22_source column safely
  data$vc22_source <- NA_character_
  for (col in vc22_candidates) {
    if (col %in% names(data)) {
      data$vc22_source[is.na(data$vc22_source) & !is.na(data[[col]])] <- col
    }
  }
  
  # Notify per-row missing vc22 values
  if (length(vc22_available) > 0) {
    rows_missing_vc22 <- which(rowSums(!is.na(data[vc22_available])) == 0)
    if (length(rows_missing_vc22) > 0) {
      message("⚠️ The following rows have no trunk volume values (vc22) in any of the columns: d_vc22_2, d_vc22_1g, d_vc22_1. CNIEFEB method will be skipped for these rows: ", paste(rows_missing_vc22, collapse = ", "))
    }
  }
  
  ## Clean species names ----
  data <- data %>% mutate(species_code = toupper(trimws(species_code)))
  density_table <- density_table %>% mutate(
    species_code = toupper(trimws(species_code)),
    con_broad = tolower(trimws(con_broad)),
    density = as.numeric(gsub(",", ".", as.character(density)))
  )
  
  ## Handle missing values ----
  idx_keep <- rep(TRUE, nrow(data))
  # Determine which rows are usable for each method
  has_vc22 <- "vc22" %in% names(data) && any(!is.na(data$vc22))
  has_vta <- "v_vta" %in% names(data) && any(!is.na(data$v_vta))
  has_species <- !is.na(data$species_code)
  
  # Apply filtering based on na_action
  if (na_action == "omit") {
    idx_keep <- has_species & (has_vc22 | has_vta)
    data <- data[idx_keep, ]
  } else if (na_action == "error") {
    if (any(!has_species | (!has_vc22 & !has_vta))) {
      stop("Missing values detected: at least one of vc22 or v_vta must be present, and species_code must not be NA. Use na_action = 'omit' to ignore incomplete rows.")
    }
  }
  data <- data[idx_keep, ]
  
  ## Check species validity ----
  wrong <- setdiff(unique(data$species_code), density_table$species_code)
  if (length(wrong) > 0) {
    stop("Unknown species : ", paste(wrong, collapse = ", "))
  }
  
  ## Merge with density table ----
  data <- left_join(data, density_table %>% select(species_code, density, con_broad), by = "species_code")
  
  # Ensure v_vta exists to avoid errors in Vallet method
  if (!"v_vta" %in% names(data)) {
    data$v_vta <- NA_real_
    message("⚠️ Column 'v_vta' not found. Vallet method will be skipped.")
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
        con_broad == "broadleaf"  ~ 1.56,
        TRUE ~ NA_real_
      ),
    ### CNIEFEB method ----
      cniefeb_bag = vc22 * feb * density,
      cniefeb_bbg = exp(-a_bbg + b_bbg * log(cniefeb_bag) + c_bbg),
      cniefeb_btot = cniefeb_bag + cniefeb_bbg,
      cniefeb_c = cniefeb_btot * a_c,
      cniefeb_co2 = cniefeb_c * a_co2,
      
    ### Vallet method ----
    vallet_bag = if_else(species_code %in% vallet_species & !is.na(v_vta), v_vta * density, NA_real_),
      vallet_bbg = if_else(!is.na(vallet_bag), exp(-a_bbg + b_bbg * log(vallet_bag) + c_bbg), NA_real_),
      vallet_btot = vallet_bag + vallet_bbg,
      vallet_c = vallet_btot * a_c,
      vallet_co2 = vallet_c * a_co2
    )
  
  # OUTPUT ----
  if (!is.null(output)) {
    write_delim(data, file = output, delim = ";", na = "", col_names = TRUE)
    message("File written : ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  return(data)
}