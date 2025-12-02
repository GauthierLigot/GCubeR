##############################################################
# Application Shiny - Tarifs GCubeR
# Auteur : Timon LUIZI
# DATE last modif : 03-12-2025 
##############################################################

##############################################################
# 0) - EN-TÊTE ET INSTALLATION DES PACKAGES ----
##############################################################

if (!requireNamespace("GCubeR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_gitlab(
    "David.Linchant/gcuber",
    host = "gitlab.uliege.be"
  )
}

library(shiny)
library(shinythemes)
library(GCubeR)
library(readxl)
library(writexl)
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)

##############################################################
# 1) - DONNÉES DE RÉFÉRENCE ----
##############################################################

## 1.1 - Chargement des métadonnées d'équations ----
# On considère equations_GCubeR comme la référence unique
# pour :
#  - la liste d’essences
#  - la documentation des modèles

if ("equations_GCubeR" %in% data(package = "GCubeR")$results[, "Item"]) {
  data("equations_GCubeR", package = "GCubeR", envir = environment())
  models_doc <- equations_GCubeR
} else {
  stop(
    "Le dataset 'equations_GCubeR' n'a pas été trouvé dans le package GCubeR.\n",
    "Vérifie que le package est bien installé et à jour."
  )
}

## 1.2 - Construction de la liste d’essences à partir d'equations_GCubeR ----
# On extrait les couples (species_code, species_name_fr),
# en gardant une seule ligne par species_code.

species_ref <- unique(models_doc[, c("species_code", "species_name_fr")])

# Gestion des NA éventuels sur species_name_fr :
species_ref$species_name_fr[is.na(species_ref$species_name_fr)] <- ""

species_ref$label <- ifelse(
  nzchar(species_ref$species_name_fr),
  paste0(species_ref$species_name_fr, " (", species_ref$species_code, ")"),
  species_ref$species_code
)

# Option : trier par nom français puis code
species_ref <- species_ref[order(species_ref$species_name_fr,
                                 species_ref$species_code), ]

# Vecteur nommé pour les selectInput :
#  - ce que voit l’utilisateur = label
#  - ce qui est passé à l’app = species_code
species_choices_gcuber <- setNames(
  species_ref$species_code,
  species_ref$label
)

## 1.3 - Table de codes numériques (Dagnelie) + fusion avec GCubeR ----
## Cette table permet de reconnaître les essences encodées en :
##  - code numérique (1, 2, 3, ...)
##  - nom français
##  - species_code

species_codes_num <- data.frame(
  code = as.character(c(
    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
    24,25,26,27,28,29,30,31,32,33,34,35,
    41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,
    110,120,150,210,220
  )),
  nom = c(
    "Chênes indigènes","Chêne rouge","Hêtre","Érable sycomore","Frêne","Ormes","Merisier","Aubépines",
    "Prunellier","Feuillus divers","Bouleau","Aulne blanc","Aulne glutineux","Charme","Châtaignier",
    "Érable plane","Érable champêtre","Sorbier","Marronnier","Noisetier","Noyers",
    "Peuplier hybride","Peuplier tremble","Robinier","Saule marsault","Autres saules",
    "Tilleuls","Pommier","Poirier","Alisiers","Sureaux","Bourdaine","Cerisiers",
    "Épicéa commun","Épicéa de Sitka","Sapin de Douglas","Mélèze","Pin sylvestre",
    "Pin noir Autriche","Pin noir de Corse","Pin weymouth","Sapin pectiné",
    "Sapin de Vancouver","Autres sapins","Cyprès","Tsuga","Thuya","Autres résineux","If",
    "Toutes essences feuillues","Toutes essences résineuses","Feuillus nobles","Autres feuillus","Autres résineux"
  ),
  stringsAsFactors = FALSE
)

## Fusion par nom français : on suppose que nom == species_name_fr
species_lookup <- merge(
  species_ref,
  species_codes_num,
  by.x = "species_name_fr",
  by.y = "nom",
  all = TRUE
)

## 1.4 - Fonction de normalisation des essences (batch) ----
## Entrée : vecteur brut (code numérique, nom FR ou species_code)
## Sortie : species_code (format GCubeR) ou NA

normalize_species <- function(x) {
  x_chr <- as.character(x)
  out <- rep(NA_character_, length(x_chr))
  unknown <- character()
  
  for (i in seq_along(x_chr)) {
    val <- trimws(x_chr[i])
    if (is.na(val) || !nzchar(val)) next
    
    # Cas 1 : code numérique (ex. "3")
    if (grepl("^[0-9]+$", val)) {
      hit <- species_lookup$species_code[species_lookup$code == val]
    } else {
      # Cas 2 : species_code ou nom français
      up <- toupper(val)
      hit <- species_lookup$species_code[
        toupper(species_lookup$species_code) == up |
          toupper(species_lookup$species_name_fr) == up
      ]
    }
    
    hit <- unique(hit[!is.na(hit)])
    if (length(hit) >= 1) {
      out[i] <- hit[1]
    } else {
      unknown <- c(unknown, val)
    }
  }
  
  if (length(unique(unknown)) > 0) {
    warning(
      "Certaines essences n'ont pas pu être reconnues et sont mises à NA : ",
      paste(unique(unknown), collapse = ", ")
    )
  }
  
  out
}

##############################################################
# 2) - FONCTIONS UTILITAIRES ----
##############################################################

parse_vector <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  t <- trimws(txt)
  if (grepl(",", t, fixed = TRUE)) {
    parts <- trimws(strsplit(t, ",", fixed = TRUE)[[1]])
    nums  <- suppressWarnings(as.numeric(parts))
    if (!any(is.na(nums))) return(nums)
    return(parts)
  }
  v <- suppressWarnings(as.numeric(t))
  if (!is.na(v)) return(v)
  t
}

recycle_to <- function(x, n) {
  if (is.null(x)) return(rep(NA, n))
  if (length(x) == n) return(x)
  if (length(x) == 1) return(rep(x, n))
  stop("Longueurs incompatibles : ", length(x), " vs ", n)
}

## Lecture CSV/XLSX ----

guess_sep <- function(path, nlines = 5) {
  con <- file(path, "r"); on.exit(close(con))
  lines <- readLines(con, n = nlines, warn = FALSE)
  counts <- c(
    semicolon = sum(stringr::str_count(lines, ";")),
    comma     = sum(stringr::str_count(lines, ",")),
    tab       = sum(stringr::str_count(lines, "\t"))
  )
  names(which.max(counts))
}

read_table_any <- function(path, ext = "csv",
                           sep_choice = c("auto", "; ", ", ", "tab")) {
  sep_choice <- match.arg(sep_choice)
  if (tolower(ext) == "xlsx") {
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (sep_choice == "auto") {
    which <- guess_sep(path)
  } else {
    which <- switch(sep_choice,
                    "; " = "semicolon",
                    ", " = "comma",
                    "tab" = "tab")
  }
  sep <- switch(which,
                semicolon = ";",
                comma     = ",",
                tab       = "\t")
  utils::read.table(
    path,
    header = TRUE,
    sep = sep,
    quote = "\"",
    dec = ".",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

## 2.3 - Normalisation des mesures (c130 / dbh13 / c150 / hauteurs) ----
# Convention :
#  - d130 = D à 1.30 m  -> dbh (1.30 m, utilisé par GCubeR)
#  - d150 = D à 1.50 m  -> conversion géométrique vers c150, puis c150_c130 + add_c130_dbh

build_base_uni <- function(species_code,
                           meas_type, meas_vals,
                           h_type, h_vals) {
  n <- length(meas_vals)
  if (!is.null(h_vals)) {
    h_vals <- recycle_to(h_vals, n)
  } else {
    h_vals <- rep(NA_real_, n)
  }
  
  df <- data.frame(
    species_code = rep(species_code, n),
    c130 = NA_real_,
    dbh  = NA_real_,   # dbh à 1.30 m (convention GCubeR)
    c150 = NA_real_,
    htot = NA_real_,
    hdom = NA_real_,
    stringsAsFactors = FALSE
  )
  
  if (meas_type == "c130") {
    df$c130 <- meas_vals
  } else if (meas_type == "c150") {
    df$c150 <- meas_vals
  } else if (meas_type == "d130") {
    df$dbh <- meas_vals             # dbh à 1.30 m (DBH)
  } else if (meas_type == "d150") { # DBH à 1.50 m
    df$c150 <- meas_vals * pi       # EXTRAPOLATION : c150 = π * d150
  }
  
  if (any(!is.na(df$c150))) {
    tmp <- tryCatch(
      GCubeR::c150_c130(df),
      error = function(e) {
        warning("Erreur dans c150_c130() : ", conditionMessage(e))
        df
      }
    )
    df$c130 <- tmp$c130
    df$c150 <- tmp$c150
  }
  
  if (any(!is.na(df$c130)) || any(!is.na(df$dbh))) {
    tmp <- tryCatch(
      GCubeR::add_c130_dbh(df),
      error = function(e) {
        warning("Erreur dans add_c130_dbh() : ", conditionMessage(e))
        df
      }
    )
    df$c130 <- tmp$c130
    df$dbh  <- tmp$dbh   # dbh standardisé à 1.30 m
  }
  
  if (h_type == "htot") {
    df$htot <- h_vals
  } else if (h_type == "hdom") {
    df$hdom <- h_vals
  }
  
  df
}

## 2.4 - Version batch de la normalisation (simplifiée) ----
## Hypothèses :
##  - une seule colonne de mesure (cm) pour tout le fichier
##  - un seul type de mesure global (c130 / c150 / d130 / d150)
##  - hauteur : soit aucune, soit htot par colonne, soit Hdom commune

build_base_batch <- function(df_in,
                             col_species,
                             col_meas_value,
                             meas_type_batch,
                             h_mode_batch,
                             col_htot,
                             hdom_value) {
  n <- nrow(df_in)
  
  # Normalisation des essences
  species_raw  <- df_in[[col_species]]
  species_code <- normalize_species(species_raw)
  
  # Colonne de mesure (cm)
  meas_vals <- suppressWarnings(as.numeric(df_in[[col_meas_value]]))
  
  base <- data.frame(
    species_code = species_code,
    c130 = NA_real_,
    dbh  = NA_real_,
    c150 = NA_real_,
    htot = NA_real_,
    hdom = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Type de mesure global
  if (meas_type_batch == "c130") {
    base$c130 <- meas_vals
  } else if (meas_type_batch == "c150") {
    base$c150 <- meas_vals
  } else if (meas_type_batch == "d130") {
    base$dbh <- meas_vals              # dbh (1.30 m)
  } else if (meas_type_batch == "d150") {
    base$c150 <- meas_vals * pi        # EXTRAPOLATION vers c150
  }
  
  # Conversion c150 -> c130
  if (any(!is.na(base$c150))) {
    tmp <- tryCatch(
      GCubeR::c150_c130(base),
      error = function(e) {
        warning("Erreur dans c150_c130() (batch) : ", conditionMessage(e))
        base
      }
    )
    base$c130 <- tmp$c130
    base$c150 <- tmp$c150
  }
  
  # Complétude c130 / dbh
  if (any(!is.na(base$c130)) || any(!is.na(base$dbh))) {
    tmp <- tryCatch(
      GCubeR::add_c130_dbh(base),
      error = function(e) {
        warning("Erreur dans add_c130_dbh() (batch) : ", conditionMessage(e))
        base
      }
    )
    base$c130 <- tmp$c130
    base$dbh  <- tmp$dbh
  }
  
  # Hauteurs
  if (identical(h_mode_batch, "htot") && nzchar(col_htot)) {
    base$htot <- suppressWarnings(as.numeric(df_in[[col_htot]]))
  } else if (identical(h_mode_batch, "hdom")) {
    base$hdom <- rep(hdom_value, n)
  }
  
  base
}

## 2.5 - Labels avec unités ----

label_with_units <- function(cols) {
  # mapping minimal et explicite pour les colonnes principales
  unit_labels <- c(
    species_code = "species_code",
    c130 = "c130 (cm)",
    c150 = "c150 (cm)",
    dbh  = "dbh (cm)",
    htot = "htot (m)",
    hdom = "hdom (m)",
    dagnelie_vc22_1  = "dagnelie_vc22_1 (m³/arbre)",
    dagnelie_vc22_1g = "dagnelie_vc22_1g (m³/arbre)",
    dagnelie_vc22_2  = "dagnelie_vc22_2 (m³/arbre)",
    dagnelie_br      = "dagnelie_br (m³ branches/arbre)",
    vallet_vc22      = "vallet_vc22 (m³/arbre)",
    vallet_vta       = "vallet_vta (m³/arbre)",
    algan_vta        = "algan_vta (m³/arbre)",
    algan_vc22       = "algan_vc22 (m³/arbre)",
    rondeux_vtot     = "rondeux_vtot (m³/arbre)",
    rondeux_vc22     = "rondeux_vc22 (m³/arbre)",
    bouvard_vta      = "bouvard_vta (m³/arbre)"
  )
  
  # Labels pour quelques sorties de biomass_calc (CNIEFEB, Vallet)
  biomass_map <- c(
    cniefeb_dagnelie_bag  = "cniefeb_dagnelie_bag (t biomasse aérienne)",
    cniefeb_dagnelie_bbg  = "cniefeb_dagnelie_bbg (t biomasse racinaire)",
    cniefeb_dagnelie_btot = "cniefeb_dagnelie_btot (t biomasse totale)",
    cniefeb_dagnelie_c    = "cniefeb_dagnelie_c (t C)",
    cniefeb_dagnelie_co2  = "cniefeb_dagnelie_co2 (t CO₂)"
  )
  unit_labels <- c(unit_labels, biomass_map)
  
  sapply(cols, function(x) if (!is.null(unit_labels[[x]])) unit_labels[[x]] else x,
         USE.NAMES = FALSE)
}

##############################################################
# 3) - UI ----
##############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Couleurs Gembloux Agro-Bio Tech (vert clair en couleur principale)
  tags$head(
    tags$style(HTML("
      /* ----- BARRE DE NAVIGATION ----- */
      .navbar-default {
        background-color: #00707F;  /* turquoise principal */
        border-color: #B9CD76;      /* rappel du vert clair des tableaux */
      }
      
      /* Texte de la navbar (titre, onglets) */
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #FFFFFF !important;  /* texte en blanc pour contraste */
      }
      
      /* Onglet actif : léger rappel du vert clair dans le bas */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #00707F;  /* on garde le fond turquoise */
        border-bottom: 3px solid #B9CD76;  /* soulignement vert clair */
        color: #FFFFFF !important;
      }
      
      /* ----- BOUTONS PRIMAIRES (Calculer, etc.) ----- */
      .btn-primary {
        background-color: #00707F;  /* turquoise principal */
        border-color: #00707F;
      }
      
      .btn-primary:hover,
      .btn-primary:focus {
        background-color: #5FA4B0;  /* turquoise plus clair au survol */
        border-color: #5FA4B0;
      }
      
      /* ----- TITRES ----- */
      h3, h4 {
        color: #00707F;  /* titres en turquoise */
      }
      
      /* ----- EN-TÊTES DE TABLEAUX ----- */
      table.table th {
        background-color: #B9CD76;  /* vert clair des tableaux */
      }
    "))
  ),
  
  
  titlePanel("Tarifs de cubage (package GCubeR)"),
  
  navbarPage(
    title = "GCubeR – Outils de cubage",
    
    ##########################################################
    # 3.1 Arbre unique ----
    ##########################################################
    tabPanel(
      "Arbre unique",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Saisis un ou plusieurs arbres. ",
            "Pour plusieurs arbres, utilise des virgules : ex. 100,110,125."
          ),
          
          selectInput(
            "vol_type_uni",
            label = HTML(
              "Choisir le(s) type(s) de volume / biomasse <span style='color:red;'>*</span> :"
            ),
            choices = c(
              "Volume marchand vc22 (m³)"      = "vc22",
              "Volume total tige vtot (m³)"    = "vtot",
              "Volume total aérien vta (m³)"   = "vta",
              "Biomasse / C / CO₂ (t)"         = "biomass"
            ),
            selected = "vc22",
            multiple = TRUE
          ),
          
          helpText(
            tags$em(
              "* Plusieurs types de volumes peuvent être calculés simultanément.",
              " Tous les types ne sont pas disponibles pour toutes les essences."
            )
          ),
          
          selectInput(
            "species_uni",
            label   = "Essence (species_code) :",
            choices = species_choices_gcuber,
            selected = NULL,
            multiple = FALSE
          ),
          
          selectInput(
            "meas_type_uni",
            label = "Type de mesure de la tige (cm) :",
            choices = c(
              "Circonférence à 1.30 m (C130, cm)"        = "c130",
              "Circonférence à 1.50 m (C150, cm)"        = "c150",
              "Diamètre à 1.30 m (D130 / DBH, cm)"       = "d130",
              "Diamètre à 1.50 m (D150, cm)"             = "d150"
            ),
            selected = "c130"
          ),
          
          textInput(
            "meas_value_uni",
            label = "Valeur(s) de mesure (cm) :",
            value = "100",
            placeholder = "Ex. 100, 110, 125"
          ),
          
          helpText("Tu peux encoder plusieurs valeurs séparées par des virgules."),
          
          selectInput(
            "h_type_uni",
            label = "Type de hauteur :",
            choices = c(
              "Hauteur totale (htot, m)"           = "htot",
              "Hauteur dominante (Hdom, m)"        = "hdom",
              "Sans mesure de hauteur"             = "none"
            ),
            selected = "none"
          ),
          
          uiOutput("h_value_ui"),
          
          hr(),
          actionButton("calc_uni", "Calculer (arbre unique)", class = "btn btn-primary"),
          checkboxInput("show_call_uni", "Afficher remarques et avertissements", TRUE)
        ),
        
        mainPanel(
          h3("Résultats – Arbre(s) unique(s)"),
          verbatimTextOutput("result_msg_uni"),
          tableOutput("result_table_uni"),
          
          conditionalPanel(
            condition = "input.show_call_uni == true",
            h4("Remarques et avertissements (équations GCubeR)"),
            verbatimTextOutput("appel_uni")
          ),
          
          hr(),
          h4("Équations disponibles pour l’essence sélectionnée"),
          uiOutput("models_expl_uni")
        )
      )
    ),
    
    ##########################################################
    # 3.2 Jeu de données ----
    ##########################################################
    tabPanel(
      "Jeu de données",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            "file_up",
            "Fichier (.xlsx ou .csv)",
            accept = c(".xlsx", ".csv", ".CSV", ".XLSX")
          ),
          
          radioButtons(
            "sep_choice",
            "Séparateur colonnes",
            inline = TRUE,
            choices = c(
              "Auto"            = "auto",
              "Point-virgule ;" = "; ",
              "Virgule ,"       = ", ",
              "Tabulation"      = "tab"
            ),
            selected = "auto"
          ),
          
          hr(),
          
          checkboxGroupInput(
            "vol_type_batch",
            "Types de volumes / résultats à calculer :",
            choices = c(
              "vc22 (volume marchand, m³)"      = "vc22",
              "vtot (volume total tige, m³)"    = "vtot",
              "vta (volume total aérien, m³)"   = "vta",
              "Biomasse / C / CO₂ (t)"          = "biomass"
            ),
            selected = "vc22"
          ),
          
          hr(),
          uiOutput("mapping_ui"),
          
          hr(),
          actionButton("calc_batch", "Calculer (lot d’arbres)", class = "btn btn-primary"),
          hr(),
          downloadButton("dl_template_xlsx", "Télécharger modèle Excel"),
          downloadButton("dl_results", "Télécharger résultats (CSV)")
        ),
        
        mainPanel(
          h3("Aperçu du fichier importé"),
          tableOutput("preview"),
          hr(),
          h3("Résultats – Lot d’arbres"),
          tableOutput("result_table_batch"),
          verbatimTextOutput("result_msg_batch"),
          h4("Remarques et avertissements (lot d’arbres)"),
          verbatimTextOutput("appel_batch")
        )
      )
    ),
    
    ##########################################################
    # 3.3 Documentation des équations ----
    ##########################################################
    tabPanel(
      "Documentation des équations",
      fluidRow(
        column(
          12,
          h3("Équations et modèles disponibles dans GCubeR"),
          p(
            "Ce tableau reprend les métadonnées des équations (famille, variable prédite,",
            " espèces, domaine de validité, variables d’entrée, source bibliographique)."
          ),
          tableOutput("models_table")
        )
      )
    )
  )
)

##############################################################
# 4) SERVER ----
##############################################################

server <- function(input, output, session) {
  
  ## stockage des warnings pour l’onglet “Arbre unique”
  uni_warnings   <- reactiveVal(character())
  ## stockage des warnings pour l’onglet “Jeu de données”
  batch_warnings <- reactiveVal(character())
  
  ## wrapper de capture des warnings pour arbre unique
  safe_run_gcuber_uni <- function(df, fun, fun_label, ...) {
    w_local <- character()
    res <- tryCatch(
      withCallingHandlers(
        fun(data = df, ...),
        warning = function(w) {
          w_local <<- c(w_local, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        w_local <<- c(
          w_local,
          paste0("Erreur dans ", fun_label, " : ", conditionMessage(e))
        )
        df
      }
    )
    if (length(w_local) > 0) {
      old <- uni_warnings()
      uni_warnings(unique(c(old, paste0("[", fun_label, "] ", w_local))))
    }
    res
  }
  
  safe_run_biomass_uni <- function(df) {
    safe_run_gcuber_uni(df, GCubeR::biomass_calc, "biomass_calc", na_action = "omit")
  }
  
  ## wrapper de capture des warnings pour le lot d’arbres
  safe_run_gcuber_batch <- function(df, fun, fun_label, ...) {
    w_local <- character()
    res <- tryCatch(
      withCallingHandlers(
        fun(data = df, ...),
        warning = function(w) {
          w_local <<- c(w_local, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        w_local <<- c(
          w_local,
          paste0("Erreur dans ", fun_label, " : ", conditionMessage(e))
        )
        df
      }
    )
    if (length(w_local) > 0) {
      old <- batch_warnings()
      batch_warnings(unique(c(old, paste0("[", fun_label, "] ", w_local))))
    }
    res
  }
  
  ## 4.1 Arbre unique ----
  
  output$h_value_ui <- renderUI({
    if (identical(input$h_type_uni, "none")) return(NULL)
    
    label_txt <- if (identical(input$h_type_uni, "hdom")) {
      "Hauteur dominante Hdom (m) :"
    } else {
      "Hauteur totale htot (m) :"
    }
    
    textInput(
      "h_value_uni",
      label = label_txt,
      value = "",
      placeholder = "Ex. 20, 22, 25"
    )
  })
  
  calc_res_uni <- eventReactive(input$calc_uni, {
    uni_warnings(character())  # reset des warnings
    
    meas_vals <- parse_vector(input$meas_value_uni)
    validate(need(!is.null(meas_vals), "Valeur(s) de mesure manquante(s)."))
    
    h_vals <- if (!is.null(input$h_value_uni) && nzchar(input$h_value_uni)) {
      parse_vector(input$h_value_uni)
    } else {
      NULL
    }
    
    base_df <- build_base_uni(
      species_code = input$species_uni,
      meas_type    = input$meas_type_uni,
      meas_vals    = meas_vals,
      h_type       = input$h_type_uni,
      h_vals       = h_vals
    )
    
    df <- base_df
    vol_types <- input$vol_type_uni
    
    if ("vc22" %in% vol_types) {
      df <- safe_run_gcuber_uni(df, GCubeR::dagnelie_vc22_1,  "dagnelie_vc22_1")
      df <- safe_run_gcuber_uni(df, GCubeR::dagnelie_vc22_1g, "dagnelie_vc22_1g")
      df <- safe_run_gcuber_uni(df, GCubeR::dagnelie_vc22_2,  "dagnelie_vc22_2")
      df <- safe_run_gcuber_uni(df, GCubeR::vallet_vc22,      "vallet_vc22")
      df <- safe_run_gcuber_uni(df, GCubeR::algan_vta_vc22,   "algan_vta_vc22")
      df <- safe_run_gcuber_uni(df, GCubeR::rondeux_vc22_vtot,"rondeux_vc22_vtot")
    }
    
    if ("vta" %in% vol_types) {
      df <- safe_run_gcuber_uni(df, GCubeR::vallet_vta,     "vallet_vta")
      df <- safe_run_gcuber_uni(df, GCubeR::bouvard_vta,    "bouvard_vta")
      df <- safe_run_gcuber_uni(df, GCubeR::algan_vta_vc22, "algan_vta_vc22")
    }
    
    if ("vtot" %in% vol_types) {
      df <- safe_run_gcuber_uni(df, GCubeR::rondeux_vc22_vtot, "rondeux_vc22_vtot")
    }
    
    if ("biomass" %in% vol_types) {
      df <- safe_run_biomass_uni(df)
    }
    
    base_cols <- c("species_code", "c130", "dbh", "c150", "htot", "hdom")
    vol_cols <- intersect(
      c("dagnelie_vc22_1", "dagnelie_vc22_1g", "dagnelie_vc22_2",
        "dagnelie_br",
        "vallet_vc22", "vallet_vta",
        "algan_vta", "algan_vc22",
        "rondeux_vtot", "rondeux_vc22",
        "bouvard_vta"),
      names(df)
    )
    biomass_cols <- names(df)[grepl("^cniefeb_|^vallet_b", names(df))]
    
    cols_show <- unique(c(base_cols, vol_cols, biomass_cols))
    cols_show <- intersect(cols_show, names(df))
    
    df_out <- df[, cols_show, drop = FALSE]
    names(df_out) <- label_with_units(names(df_out))
    
    df_out
  }, ignoreInit = TRUE)
  
  output$result_table_uni <- renderTable({
    req(calc_res_uni())
    calc_res_uni()
  })
  
  output$result_msg_uni <- renderPrint({
    req(calc_res_uni())
    df <- calc_res_uni()
    cat("Nombre d’arbres traités :", nrow(df), "\n")
    cat("Les volumes et biomasses affichés proviennent directement des fonctions de GCubeR.\n")
    cat("Les avertissements de domaine de validité (plages de c130, dbh, hauteurs, espèces...) ",
        "sont capturés et affichés ci-dessous.\n", sep = "")
  })
  
  output$appel_uni <- renderPrint({
    req(calc_res_uni())
    cat(
      "Fonctions appelées (selon les types choisis) :\n",
      "- Dagnelie : dagnelie_vc22_1(), dagnelie_vc22_1g(), dagnelie_vc22_2(), dagnelie_br()\n",
      "- Vallet  : vallet_vc22(), vallet_vta()\n",
      "- Algan   : algan_vta_vc22()\n",
      "- Rondeux : rondeux_vc22_vtot()\n",
      "- Bouvard : bouvard_vta()\n",
      "- Biomasse / carbone : biomass_calc()\n\n",
      "Chaque fonction applique ses propres contrôles de validité (espèces supportées, plages de c130/dbh/hauteur).\n",
      "Les résultats sont néanmoins renvoyés même hors domaine de validité ; GCubeR émet des warnings.\n\n"
    )
    w <- uni_warnings()
    if (length(w) == 0) {
      cat("Aucun warning n’a été remonté par les fonctions GCubeR pour ce calcul.\n")
    } else {
      cat("Avertissements remontés par GCubeR :\n")
      for (msg in w) {
        cat(" - ", msg, "\n", sep = "")
      }
    }
  })
  
  output$models_expl_uni <- renderUI({
    req(input$species_uni)
    sp <- input$species_uni
    
    md <- subset(models_doc, species_code == sp)
    if (nrow(md) == 0) {
      return(tags$p(
        "Aucune métadonnée d’équation trouvée pour cette essence dans equations_GCubeR."
      ))
    }
    
    tagList(
      lapply(split(md, md$eq_id), function(m) {
        m1 <- m[1, ]
        tags$details(
          tags$summary(
            paste0(m1$eq_id, " (", m1$method, " – ", m1$predicted_variable, ")")
          ),
          tags$p(
            strong("Méthode : "), m1$method, br(),
            strong("Variable prédite : "), m1$predicted_variable,
            " [", m1$output_unit, "]", br(),
            strong("Espèce : "), m1$species_name_fr, " (", m1$species_code, ")", br(),
            strong("Région de validité : "), m1$validity_region, br(),
            strong("Plage de validité (texte) : "), m1$validity_range, br(),
            strong("Variables d’entrée : "), m1$input_variable,
            " [", m1$input_unit, "]", br(),
            strong("Type de formule : "), m1$formula_type, br(),
            strong("Référence : "), m1$reference_source
          )
        )
      })
    )
  })
  
  ## 4.2 Jeu de données ----
  
  dat_raw <- reactive({
    req(input$file_up)
    ext <- tools::file_ext(input$file_up$name)
    read_table_any(
      path       = input$file_up$datapath,
      ext        = ext,
      sep_choice = input$sep_choice
    )
  })
  
  output$preview <- renderTable({
    req(dat_raw())
    head(dat_raw(), 10)
  })
  
  ## UI de mapping simplifiée pour le lot ----
  output$mapping_ui <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    
    tagList(
      h4("Mapping des colonnes"),
      helpText(
        "Sélectionne la colonne d’essence, la mesure unique de diamètre/circonférence,",
        " et la hauteur si disponible. L’essence peut être encodée en code numérique,",
        " nom français ou species_code."
      ),
      selectInput(
        "col_species", "Colonne essence :", choices = cols
      ),
      selectInput(
        "col_meas_value", "Colonne diamètre / circonférence (cm) :", choices = cols
      ),
      selectInput(
        "meas_type_batch",
        "Type de mesure (cm) :",
        choices = c(
          "Circonférence à 1.30 m (C130)"        = "c130",
          "Circonférence à 1.50 m (C150)"        = "c150",
          "Diamètre à 1.30 m (D130 / DBH)"       = "d130",
          "Diamètre à 1.50 m (D150)"             = "d150"
        ),
        selected = "c130"
      ),
      radioButtons(
        "h_mode_batch",
        "Hauteur :",
        choices = c(
          "Sans hauteur"                              = "none",
          "Hauteur totale individuelle (htot, colonne)" = "htot",
          "Hauteur dominante commune (Hdom, valeur unique)" = "hdom"
        ),
        selected = "none"
      ),
      conditionalPanel(
        condition = "input.h_mode_batch == 'htot'",
        selectInput(
          "col_htot",
          "Colonne hauteur totale htot (m) :",
          choices = cols
        )
      ),
      conditionalPanel(
        condition = "input.h_mode_batch == 'hdom'",
        numericInput(
          "hdom_value",
          "Hauteur dominante Hdom (m) :",
          value = 20,
          min = 0
        )
      )
    )
  })
  
  calc_res_batch <- eventReactive(input$calc_batch, {
    batch_warnings(character())  # reset warnings lot
    
    df_in <- dat_raw()
    validate(need(!is.null(input$col_species), "Sélectionne la colonne d’essence."))
    validate(need(!is.null(input$col_meas_value), "Sélectionne la colonne de mesure."))
    validate(need(!is.null(input$meas_type_batch), "Sélectionne le type de mesure."))
    
    # Construction du data.frame de base (c130, dbh, h, species_code)
    base_df <- build_base_batch(
      df_in         = df_in,
      col_species   = input$col_species,
      col_meas_value = input$col_meas_value,
      meas_type_batch = input$meas_type_batch,
      h_mode_batch  = input$h_mode_batch,
      col_htot      = if (!is.null(input$col_htot)) input$col_htot else "",
      hdom_value    = if (!is.null(input$hdom_value)) input$hdom_value else NA_real_
    )
    
    df <- base_df
    vol_types <- input$vol_type_batch
    
    if ("vc22" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_1,   "dagnelie_vc22_1")
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_1g,  "dagnelie_vc22_1g")
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_2,   "dagnelie_vc22_2")
      df <- safe_run_gcuber_batch(df, GCubeR::vallet_vc22,       "vallet_vc22")
      df <- safe_run_gcuber_batch(df, GCubeR::algan_vta_vc22,    "algan_vta_vc22")
      df <- safe_run_gcuber_batch(df, GCubeR::rondeux_vc22_vtot, "rondeux_vc22_vtot")
    }
    
    if ("vta" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::vallet_vta,        "vallet_vta")
      df <- safe_run_gcuber_batch(df, GCubeR::bouvard_vta,       "bouvard_vta")
      df <- safe_run_gcuber_batch(df, GCubeR::algan_vta_vc22,    "algan_vta_vc22")
    }
    
    if ("vtot" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::rondeux_vc22_vtot, "rondeux_vc22_vtot")
    }
    
    if ("biomass" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::biomass_calc,      "biomass_calc", na_action = "omit")
    }
    
    base_cols <- c("species_code", "c130", "dbh", "c150", "htot", "hdom")
    vol_cols <- intersect(
      c("dagnelie_vc22_1", "dagnelie_vc22_1g", "dagnelie_vc22_2",
        "dagnelie_br",
        "vallet_vc22", "vallet_vta",
        "algan_vta", "algan_vc22",
        "rondeux_vtot", "rondeux_vc22",
        "bouvard_vta"),
      names(df)
    )
    biomass_cols <- names(df)[grepl("^cniefeb_|^vallet_b", names(df))]
    
    cols_show <- unique(c(base_cols, vol_cols, biomass_cols))
    cols_show <- intersect(cols_show, names(df))
    
    df_out <- df[, cols_show, drop = FALSE]
    names(df_out) <- label_with_units(names(df_out))
    
    df_out
  }, ignoreInit = TRUE)
  
  output$result_table_batch <- renderTable({
    req(calc_res_batch())
    head(calc_res_batch(), 50)
  })
  
  output$result_msg_batch <- renderPrint({
    req(calc_res_batch())
    df <- calc_res_batch()
    cat("Nombre de lignes dans le fichier importé :", nrow(dat_raw()), "\n")
    cat("Nombre de lignes dans le tableau de résultats :", nrow(df), "\n")
    cat("Les fonctions GCubeR appliquent leurs propres contrôles de validité ",
        "(plages de c130, dbh, hauteurs, espèces...). ",
        "Les avertissements sont capturés et repris ci-dessous.\n", sep = "")
  })
  
  output$appel_batch <- renderPrint({
    req(calc_res_batch())
    cat(
      "Fonctions appelées sur le lot (selon les types choisis) :\n",
      "- Dagnelie : dagnelie_vc22_1(), dagnelie_vc22_1g(), dagnelie_vc22_2(), dagnelie_br()\n",
      "- Vallet  : vallet_vc22(), vallet_vta()\n",
      "- Algan   : algan_vta_vc22()\n",
      "- Rondeux : rondeux_vc22_vtot()\n",
      "- Bouvard : bouvard_vta()\n",
      "- Biomasse / carbone : biomass_calc()\n\n",
      "Chaque fonction applique ses propres contrôles de validité.",
      " Les résultats sont renvoyés même hors domaine de validité ; GCubeR émet des warnings.\n\n"
    )
    w <- batch_warnings()
    if (length(w) == 0) {
      cat("Aucun warning n’a été remonté par les fonctions GCubeR pour ce lot.\n")
    } else {
      cat("Avertissements remontés par GCubeR (lot) :\n")
      for (msg in w) {
        cat(" - ", msg, "\n", sep = "")
      }
    }
  })
  
  output$dl_template_xlsx <- downloadHandler(
    filename = function() "template_gcuber.xlsx",
    content  = function(file) {
      ## Modèle simple, cohérent avec le nouveau mapping :
      ##  - species : nom FR, species_code ou code numérique
      ##  - meas_value : diamètre/circonférence (cm)
      ##  - htot : hauteur totale (optionnelle)
      tpl <- data.frame(
        species    = c("PICEA_ABIES", "Hêtre", "3"),
        meas_value = c(100, 120, 140),  # cm
        htot       = c(25, 22, NA),     # m (optionnel)
        stringsAsFactors = FALSE
      )
      writexl::write_xlsx(tpl, path = file)
    }
  )
  
  output$dl_results <- downloadHandler(
    filename = function() "resultats_gcuber.csv",
    content  = function(file) {
      req(calc_res_batch())
      write.csv(calc_res_batch(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  ## 4.3 Documentation ----
  
  output$models_table <- renderTable({
    if (!is.null(models_doc) && nrow(models_doc) > 0) {
      cols <- c("eq_id", "method", "predicted_variable", "output_unit",
                "species_name_fr", "species_code",
                "validity_region", "validity_range",
                "input_variable", "input_unit",
                "reference_source")
      cols <- intersect(cols, names(models_doc))
      head(models_doc[, cols, drop = FALSE], 100)
    } else {
      NULL
    }
  })
}

##############################################################
# 5) LANCEMENT ----
##############################################################

shinyApp(ui = ui, server = server)
