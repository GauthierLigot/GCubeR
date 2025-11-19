# Chargement des données
data <- data.frame(
  Volume = c(1.2, 0.8, 2.5),
  ESS = c("PICEA_ABIES", "QUERCUS_ROBUR", "PINUS_SYLVESTRIS")
)

dens_table <- data.frame(
  species_code = c("PICEA_ABIES", "QUERCUS_ROBUR", "PINUS_SYLVESTRIS"),
  density = c(400, 650, 500),
  type = c("conifère", "feuillu", "conifère")
)

# Calcul
resultats <- biomasse_calc(data, dens_table)



biomasse_calc <- function(data,
                          dens_table,
                          na_action = c("error", "omit"),
                          output = NULL) {
  
  na_action <- match.arg(na_action)
  
  # Vérification des colonnes
  stopifnot(is.data.frame(data))
  needed <- c("Volume", "ESS")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {
    stop("Colonnes manquantes : ", paste(miss, collapse = ", "))
  }
  
  # Gestion des NA
  idx_keep <- rep(TRUE, nrow(data))
  if (na_action == "omit") {
    idx_keep <- complete.cases(data[, needed])
  } else if (anyNA(data$Volume) || anyNA(data$ESS)) {
    stop("NA détectés dans Volume/ESS. Utilise na_action='omit' pour les ignorer.")
  }
  data <- data[idx_keep, ]
  
  # Vérification des essences
  mauvaises <- setdiff(unique(data$ESS), dens_table$species_code)
  if (length(mauvaises) > 0) {
    stop("Essences inconnues : ", paste(mauvaises, collapse = ", "))
  }
  
  # Initialisation des colonnes
  data$Bag   <- NA_real_
  data$Bbg   <- NA_real_
  data$Btot  <- NA_real_
  data$C     <- NA_real_
  data$CO2   <- NA_real_
  
  # Constantes
  a_bbg <- 1.0587
  b_bbg <- 0.8836
  c_bbg <- 0.2840
  a_C   <- 0.475
  a_CO2 <- 44 / 12
  
  # Boucle
  for (i in seq_len(nrow(data))) {
    ess <- data$ESS[i]
    vol <- data$Volume[i]
    
    ligne_dens <- dens_table[dens_table$species_code == ess, ]
    dens <- as.numeric(ligne_dens$density)
    type <- tolower(ligne_dens$type)
    
    FEB <- if (type == "conifère") 1.3 else if (type == "feuillu") 1.56 else NA
    if (is.na(FEB)) {
      warning("Type inconnu pour l'essence ", ess)
      next
    }
    
    Bag <- vol * FEB * dens
    Bbg <- exp(-a_bbg + b_bbg * log(Bag) + c_bbg)
    Btot <- Bag + Bbg
    C <- Btot * a_C
    CO2 <- C * a_CO2
    
    data$Bag[i]  <- Bag
    data$Bbg[i]  <- Bbg
    data$Btot[i] <- Btot
    data$C[i]    <- C
    data$CO2[i]  <- CO2
  }
  
  # Export ou affichage
  if (!is.null(output)) {
    write.csv(data, file = output, row.names = FALSE)
    message("Fichier écrit : ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  
  return(data)
}

