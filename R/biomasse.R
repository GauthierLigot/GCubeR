biomasse_calc <- function(data,
                          density_table,
                          na_action = c("error", "omit"),
                          output = NULL) {
  
  na_action <- match.arg(na_action)

  # récupérer le fichier des densité
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
  needed <- c("Volume", "ESS")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {
    stop("Colonnes manquantes dans 'data' : ", paste(miss, collapse = ", "))
  }
  
  # Nettoyage des ESS
  data <- data %>% mutate(ESS = toupper(trimws(ESS)))
  density_table <- density_table %>% mutate(ESS = toupper(trimws(ESS)),
                                      con_feu = tolower(trimws(con_feu)))
  
  # Gestion des NA
  idx_keep <- rep(TRUE, nrow(data))
  if (na_action == "omit") {
    idx_keep <- complete.cases(data[, needed])
  } else if (anyNA(data$Volume) || anyNA(data$ESS)) {
    stop("NA détectés dans Volume/ESS. Utilise na_action='omit' pour les ignorer.")
  }
  data <- data[idx_keep, ]
  
  # Vérification des essences
  mauvaises <- setdiff(unique(data$ESS), density_table$ESS)
  if (length(mauvaises) > 0) {
    stop("Essences inconnues : ", paste(mauvaises, collapse = ", "))
  }
  
  # Jointure directe
  data <- left_join(data, density_table %>% select(ESS, density, con_feu), by = "ESS")
  
  # Constantes
  a_bbg <- 1.0587
  b_bbg <- 0.8836
  c_bbg <- 0.2840
  a_C   <- 0.475
  a_CO2 <- 44 / 12
  
  # Calcul vectorisé
  data <- data %>%
    mutate(
      FEB = case_when(
        con_feu == "conifere" ~ 1.3,
        con_feu == "feuillu"  ~ 1.56,
        TRUE ~ NA_real_
      ),
      Bag = Volume * FEB * density,
      Bbg = exp(-a_bbg + b_bbg * log(Bag) + c_bbg),
      Btot = Bag + Bbg,
      C = Btot * a_C,
      CO2 = C * a_CO2
    )
  
  # Avertissement si type inconnu
  if (any(is.na(data$FEB))) {
    warning("Certains types (con_feu) sont inconnus ou mal orthographiés.")
  }
  
  # Suppression des colonnes intermédiaires
  data <- data %>% select(-any_of(c("density", "con_feu", "FEB")))
  
  # Export ou affichage
  if (!is.null(output)) {
    write.csv(data, file = output, row.names = FALSE)
    message("Fichier écrit : ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  return(data)
}

resultats <- biomasse_calc(data, density_table)
