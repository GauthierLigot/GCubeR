#' Calcule le volume (cubage) selon une équation paramétrée
#'
#' @param data data.frame avec colonnes \code{C130} et \code{H}.
#' @param method Chaîne identifiant l'équation à utiliser (présente dans \code{eq_cubage$method}).
#' @param na_action Que faire si C130/H manquants : \code{"error"} (défaut) ou \code{"omit"} (ignore les lignes NA).
#' @return Le data.frame d'entrée avec une colonne \code{Volume}.
#' @examples
#' df <- data.frame(C130 = c(30, 40), H = c(18, 22))
#' # suppose que 'power_generic' est présent dans eq_cubage
#' # cuber(df, method = "power_generic")
#' @export
LAINCHANT <- function(data, method, na_action = c("error", "omit")) {
  na_action <- match.arg(na_action)

# Vérification du df ------------------------------------------------------


  # --- 1) Vérifs structure
  stopifnot(is.data.frame(data))
  needed <- c("C130")
  miss <- setdiff(needed, names(data))
  if (length(miss) > 0) {
    stop("Colonnes manquantes: ", paste(miss, collapse = ", "))
  }
  if (!is.numeric(data$C130) || !is.numeric(data$H)) {
    stop("C130 et H doivent être numériques.")
  }
  
  # --- 2) Gérer NA si demandé
  idx_keep <- rep(TRUE, nrow(data))
  if (na_action == "omit") {
    idx_keep <- stats::complete.cases(data[, c("C130", "H")])
  } else if (anyNA(data$C130) || anyNA(data$H)) {
    stop("NA détectés dans C130/H. Utilise na_action='omit' pour les ignorer.")
  }

# Traitement des données --------------------------------------------------

  
  # --- 3) Récupère la ligne d'équation dans la donnée interne 'eq_cubage'
  # 'eq_cubage' est créé par use_data(..., internal=TRUE) et chargé en mémoire
  if (!exists("eq_cubage", inherits = TRUE)) {
    stop("La table interne 'eq_cubage' est introuvable. As-tu exécuté data-raw/prepare_eq_cubage.R ?")
  }
  row <- eq_cubage[eq_cubage$method == method, , drop = FALSE]
  if (nrow(row) == 0) stop("Méthode inconnue: '", method, "'. Vérifie eq_cubage$method.")
  if (nrow(row) > 1)  stop("Plusieurs lignes pour la même méthode: '", method, "'. Utilise des identifiants uniques.")
  
  # Coefs (présence optionnelle selon model)
  a <- suppressWarnings(as.numeric(row$a)); b <- suppressWarnings(as.numeric(row$b))
  c <- suppressWarnings(as.numeric(row$c)); d <- suppressWarnings(as.numeric(row$d))
  e <- suppressWarnings(as.numeric(row$e)); f <- suppressWarnings(as.numeric(row$f))
  model <- tolower(trimws(row$model))
  
  # --- 4) Calcul vectorisé selon la famille de modèle
  C130 <- data$C130
  H    <- data$H
  Volume <- rep(NA_real_, nrow(data))
  
  if (model == "power") {
    # V = a * C130^b * H^c
    Volume[idx_keep] <- a * (C130[idx_keep]^b) * (H[idx_keep]^c)
    
  } else if (model == "poly_c_h") {
    # V = a + b*C130 + c*H + d*C130^2 + e*H^2 + f*C130*H
    Volume[idx_keep] <- a +
      b * C130[idx_keep] +
      c * H[idx_keep] +
      d * (C130[idx_keep]^2) +
      e * (H[idx_keep]^2) +
      f * (C130[idx_keep] * H[idx_keep])
    
  } else if (model == "loglin") {
    # V = exp(a + b*log(C130) + c*log(H))
    Volume[idx_keep] <- exp(a + b * log(C130[idx_keep]) + c * log(H[idx_keep]))
    
  } else {
    stop("Modèle inconnu dans eq_cubage$model: '", model, "'.")
  }

# Output ------------------------------------------------------------------

  
  out <- data
  out$Volume <- Volume
  out
}
