
#' GCubeR
#'
#' @param data 
#' @param method 
#' @param na_action 
#' @param output 
#'
#' @returns
#' @export
#'
#' @examples
#' df <- data.frame (C130=c(145,156,234,233), 
#' H=c(25,23,45,34), 
#' ESS=c("Pinus_sylvestris","Quercus_robur","Quercus_rubra","Fagus_sylvatica"))
#' gcbr(data=df)

gcbr <- function(data, 
                 method = c("all","Biomasse_Valet","Volume_Vallet"), 
                 na_action = c("error", "omit"), 
                 output) {
  
  na_action <- match.arg(na_action)
  method <- match.arg(method) #si rien de spécifié en méthode il prend la valeur "all"  
  
  if (!"method" %in% names(data)) {
    data$method <- method    #si pas de méthode on prend la valeur par def "all"
  } 
  
  # VERIFICATION DU DATAFRAME ------------------------------------------------------
  
  # --- 1) Vérifs des champs dans le data frame
  stopifnot(is.data.frame(data))
  needed <- c("C130","H","ESS","method")   #paramètre les champs nécessaire
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {                  #crée un vecteur contenant les colonnes manquantes
    stop("Colonnes manquantes: ", paste(miss, collapse = ", "))
  }
  if (!is.numeric(data$C130) || !is.numeric(data$H)) { #vérifie que C130 et H soient numérique
    stop("C130 et H doivent être numériques.")
  }
  
  # --- 2) Gestion des NA (accepté ou non) 
  idx_keep <- rep(TRUE, nrow(data))
  if (na_action == "omit") {         #si la fonction est paramètrée en "omit" on prend les NA
    idx_keep <- stats::complete.cases(data[, c("C130", "H")]) #idx_keep prend la forme d'un vecteur ou les NA sont des valeurs vide
  } else if (anyNA(data$C130) || anyNA(data$H)) {  #si la fonction n'est pas paramètrée en omt alors msg d'erruer si y'a des NA détecté par anyNA
    stop("NA détectés dans C130/H. Utilise na_action='omit' pour les ignorer.")
  }
  
  # --- 3) Gestion des essences 
  mauvaises <- setdiff(unique(data$ESS), c("Pinus_sylvestris", "Picea_abies", "Fagus_sylvatica", "Quercus_robur",
                                           "Prunus_avium","Alnus_glutinosa","Quercus_petraea","Quercus_pubESSens",
                                           "Pseudotsuga_menziesii","Pinus_alapensis","Pinus_pinaster","Pinus_nigra",
                                           "Abies_alba", "Quercus_rubra")) #liste des essences valides pour les équations dans "eq"
  
  if (length(mauvaises) > 0) {
    stop("Espèces inconnues : ", paste(mauvaises, collapse=", "),
         "\n Veuillez lire la liste d'essence dans ?nb2 ou sélectionner une essence de substitution")
  }
  
  
  ## --- 4) Table d'équations 
  if (!exists("équations_GCubeR", inherits = TRUE)) {
    stop("La table interne 'équations_GCubeR' est introuvable.")
  }
  eq <- `équations_GCubeR`
  nline <- nrow(data)
  
  # TRAITEMENT DES DONNEES ------------------------------------------------------
  
  # --- 5) Initialisation des colonnes de sortie
  data$VAL_VTA  <- NA_real_
  data$Biomasse <- NA_real_
  data$Carbone  <- NA_real_
  data$CO2      <- NA_real_  #Crée des colonnes dans le df initial
  
  # --- 6) Boucle
  
  for (i in seq_len(nline)){ #seq_len() défini le longueur de nline
    
    ess <- data$ESS[i]
    met <- data$method[i]
    C130   <- as.numeric(data$C130[i])
    H   <- as.numeric(data$H[i])
    
    Dens <- 0.9 # à reparamètrer pour prendre les vraies valeurs de densité
    Carb <- 0.47
    CO2F <- 44/12
    
    if (ess == "Picea_abies" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Pinus_sylvestris" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Alnus_glutinosa" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Quercus_robur" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Quercus_petraea" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Quercus_pubescens" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Pseudotsuga_menziesii" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Quercus_rubra" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Fagus_sylvatica" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Prunus_avium" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Pinus_halapensis" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Pinus_pinaster" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Pinus_nigra" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb
    }
    if (ess == "Abies_alba" && met == "all") {
      a <- as.numeric(eq$coeff_a[1])
      b <- as.numeric(eq$coeff_b[1])
      c <- as.numeric(eq$coeff_c[2])
      d <- as.numeric(eq$coeff_d[3]) #pas trouvé la valeur de d pour cette eq
      form1 <- a + b * C130 *c^((C130^(1/2)/H)) * (1 + d * C130^2)
      data$VAL_VTA[i] <- form1*(1/40000)*C130^2*H
      data$Biomasse[i] <- data$VAL_VTA[i]*Dens
      data$Carbone[i] <- data$Biomasse[i]*Carb}
  }
  
  # Output ------------------------------------------------------------------
  
  
  if (!is.null(output)) {
    utils::write.csv(data, file = output, row.names = FALSE)
    message("Fichier écrit : ", normalizePath(output, winslash = "/"))
  } else {
    print(data)
  }
  
  return(data)
}
