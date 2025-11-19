biomass_calc <- function(data,
                          na_action = c("error", "omit"),
                          output = NULL) {
  
  na_action <- match.arg(na_action)
  
  # Load required packages
  library(dplyr)
  library(readr)
  
  # Read density file
  path_density <- file.path("data-raw", "density_table.csv")
  density_table <- read_delim(
    file = path_density,
    delim = ";",
    locale = locale(decimal_mark = ",", encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  # Check required columns
  stopifnot(is.data.frame(data), is.data.frame(density_table))
  needed <- c("Vc22", "Vta", "ESS")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {
    stop("Missing columns 'data' : ", paste(miss, collapse = ", "))
  }
  
  # Clean species names
  data <- data %>% mutate(ESS = toupper(trimws(ESS)))
  density_table <- density_table %>% mutate(
    ESS = toupper(trimws(ESS)),
    con_broad = tolower(trimws(con_broad)),
    density = as.numeric(gsub(",", ".", as.character(density)))
  )
  
  # Handle missing values
  idx_keep <- rep(TRUE, nrow(data))
  if (na_action == "omit") {
    idx_keep <- complete.cases(data[, needed])
  } else if (anyNA(data$Vc22) || anyNA(data$Vta) || anyNA(data$ESS)) {
    stop("Missing values detected in Vc22/Vta/ESS. Use na_action = 'omit' to ignore them.")
  }
  data <- data[idx_keep, ]
  
  # Check species validity
  mauvaises <- setdiff(unique(data$ESS), density_table$ESS)
  if (length(mauvaises) > 0) {
    stop("Unknown species : ", paste(mauvaises, collapse = ", "))
  }
  
  # Join with density table
  data <- left_join(data, density_table %>% select(ESS, density, con_broad), by = "ESS")
  
  # Constants
  a_bbg <- 1.0587
  b_bbg <- 0.8836
  c_bbg <- 0.2840
  a_C   <- 0.475
  a_CO2 <- 44 / 12
  
  # Species compatible with Vallet method
  vallet_species <- c(
    "PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA",
    "PINUS_SYLVESTRIS", "PINUS_PINASTER", "ABIES_ALBA",
    "PSEUDOTSUGA_MENZIESII"
  )
  
  # Separate calculations for CNIEFEB and Vallet
  data <- data %>%
    mutate(
      FEB = case_when(
        con_broad == "conifer" ~ 1.3,
        con_broad == "broadleaf"  ~ 1.56,
        TRUE ~ NA_real_
      ),
      # CNIEFEB
      Bag_CNIEFEB = Vc22 * FEB * density,
      Bbg_CNIEFEB = exp(-a_bbg + b_bbg * log(Bag_CNIEFEB) + c_bbg),
      Btot_CNIEFEB = Bag_CNIEFEB + Bbg_CNIEFEB,
      C_CNIEFEB = Btot_CNIEFEB * a_C,
      CO2_CNIEFEB = C_CNIEFEB * a_CO2,
      
      # Vallet (if species compatible)
      Bag_Vallet = if_else(ESS %in% vallet_species, Vta * density, NA_real_),
      Bbg_Vallet = if_else(!is.na(Bag_Vallet), exp(-a_bbg + b_bbg * log(Bag_Vallet) + c_bbg), NA_real_),
      Btot_Vallet = Bag_Vallet + Bbg_Vallet,
      C_Vallet = Btot_Vallet * a_C,
      CO2_Vallet = C_Vallet * a_CO2
    )
  
  # Export or display results
  if (!is.null(output)) {
    write_delim(data, file = output, delim = ";", na = "", col_names = TRUE)
    message("File written : ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  
  return(data)
}

# Test data
data <- data.frame(
  ESS = c(
    "PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA", "PINUS_SYLVESTRIS",  # Vallet-compatible species
    "BETULA_PENDULA", "ROBINIA_PSEUDOACACIA", "TILIA_CORDATA"              # CNIEFEB-only species
  ),
  Vc22 = c(1.2, 0.8, 1.5, 2.0, 1.1, 0.9, 1.3),  # trunk volume
  Vta  = c(1.6, 1.1, 2.0, 2.5, 1.4, 1.2, 1.6)   # total aboveground volume
)

# Run biomass calculation
output_path <- "results.csv"
results <- biomass_calc(data, output = output_path)

if (file.exists(output_path)) {
  message("✅ CSV file successfully created.")
} else {
  warning("⚠️ CSV file was not created.")
}


