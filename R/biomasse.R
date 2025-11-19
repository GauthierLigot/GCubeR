biomasse_calc <- function(data,
                          na_action = c("error", "omit"),
                          output = NULL) {
  
  na_action <- match.arg(na_action)
  
  # Chargement des packages
  library(dplyr)
  library(readr)
  
  # Lecture du fichier de densité
  path_density <- file.path("data-raw", "density_table.csv")
  density_table <- read_delim(
    file = path_density,
    delim = ";",
    locale = locale(decimal_mark = ",", encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  # Vérification des colonnes
  stopifnot(is.data.frame(data), is.data.frame(density_table))
  needed <- c("Vc22", "Vta", "ESS")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {
    stop("Colonnes manquantes dans 'data' : ", paste(miss, collapse = ", "))
  }
  
  # Nettoyage
  data <- data %>% mutate(ESS = toupper(trimws(ESS)))
  density_table <- density_table %>% mutate(
    ESS = toupper(trimws(ESS)),
    con_feu = tolower(trimws(con_feu)),
    density = as.numeric(gsub(",", ".", as.character(density)))
  )
  
  # Gestion des NA
  idx_keep <- rep(TRUE, nrow(data))
  if (na_action == "omit") {
    idx_keep <- complete.cases(data[, needed])
  } else if (anyNA(data$Vc22) || anyNA(data$Vta) || anyNA(data$ESS)) {
    stop("NA détectés dans Vc22/Vta/ESS. Utilise na_action='omit' pour les ignorer.")
  }
  data <- data[idx_keep, ]
  
  # Vérification des essences
  mauvaises <- setdiff(unique(data$ESS), density_table$ESS)
  if (length(mauvaises) > 0) {
    stop("Essences inconnues : ", paste(mauvaises, collapse = ", "))
  }
  
  # Jointure
  data <- left_join(data, density_table %>% select(ESS, density, con_feu), by = "ESS")
  
  # Constantes
  a_bbg <- 1.0587
  b_bbg <- 0.8836
  c_bbg <- 0.2840
  a_C   <- 0.475
  a_CO2 <- 44 / 12
  
  # Espèces Vallet
  vallet_species <- c(
    "PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA",
    "PINUS_SYLVESTRIS", "PINUS_PINASTER", "ABIES_ALBA",
    "PSEUDOTSUGA_MENZIESII"
  )
  
  # Calculs séparés pour CNIEFEB et Vallet
  data <- data %>%
    mutate(
      FEB = case_when(
        con_feu == "conifere" ~ 1.3,
        con_feu == "feuillu"  ~ 1.56,
        TRUE ~ NA_real_
      ),
      # CNIEFEB
      Bag_CNIEFEB = Vc22 * FEB * density,
      Bbg_CNIEFEB = exp(-a_bbg + b_bbg * log(Bag_CNIEFEB) + c_bbg),
      Btot_CNIEFEB = Bag_CNIEFEB + Bbg_CNIEFEB,
      C_CNIEFEB = Btot_CNIEFEB * a_C,
      CO2_CNIEFEB = C_CNIEFEB * a_CO2,
      
      # Vallet (si espèce compatible)
      Bag_Vallet = if_else(ESS %in% vallet_species, Vta * density, NA_real_),
      Bbg_Vallet = if_else(!is.na(Bag_Vallet), exp(-a_bbg + b_bbg * log(Bag_Vallet) + c_bbg), NA_real_),
      Btot_Vallet = Bag_Vallet + Bbg_Vallet,
      C_Vallet = Btot_Vallet * a_C,
      CO2_Vallet = C_Vallet * a_CO2
    )
  
  # Export ou affichage
  if (!is.null(output)) {
    write.csv(data, file = output, row.names = FALSE)
    message("Fichier écrit : ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  
  return(data)
}

# Données test
data <- data.frame(
  ESS = c(
    "PICEA_ABIES", "QUERCUS_ROBUR", "FAGUS_SYLVATICA", "PINUS_SYLVESTRIS",  # Vallet compatibles
    "BETULA_PENDULA", "ROBINIA_PSEUDOACACIA", "TILIA_CORDATA"              # CNIEFEB uniquement
  ),
  Vc22 = c(1.2, 0.8, 1.5, 2.0, 1.1, 0.9, 1.3),  # volume tronc
  Vta  = c(1.6, 1.1, 2.0, 2.5, 1.4, 1.2, 1.6)   # volume total aérien
)

resultats <- biomasse_calc(data)

