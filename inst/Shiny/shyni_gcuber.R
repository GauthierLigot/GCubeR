##############################################################
# Application Shiny - Tarifs GCubeR
# Auteur : Timon LUIZI
# DATE last modif : 20-10-2025 
##############################################################

##############################################################
# 0) - EN-TÊTE ET INSTALLATION DES PACKAGES ----
##############################################################

## Installation de GCubeR si nécessaire ----
if (!requireNamespace("GCubeR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_gitlab(
    "David.Linchant/gcuber",
    host = "gitlab.uliege.be"
  )
}

## Chargement des packages ----
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

## 1.1 - Tableau des essences depuis un data.frame du package ----
# EXTRAPOLATION :
# Je suppose qu'il existe un data.frame 'species_list_gcuber'
# dans le package GCubeR, avec les colonnes au moins :
#   - species_code
#   - nom_fr
#   - nom_latin
# Si le nom réel est différent, il suffit d'adapter ce bloc.

if ("species_list_gcuber" %in% data(package = "GCubeR")$results[, "Item"]) {
  # Chargement explicite de l'objet de données du package
  data("species_list_gcuber", package = "GCubeR", envir = environment())
  species_db <- species_list_gcuber
} else {
  # Si l'objet n'existe pas, on stoppe avec un message explicite.
  stop(
    "Le data.frame 'species_list_gcuber' n'a pas été trouvé dans le package GCubeR.\n",
    "Adapte ce bloc avec le nom réel de ta table d'essences."
  )
}

# Création d'un label lisible pour l'utilisateur
species_db$label <- ifelse(
  is.na(species_db$nom_latin) | species_db$nom_latin == "",
  species_db$nom_fr,
  paste0(species_db$nom_fr, " (", species_db$nom_latin, ")")
)

# Liste nommée pour selectInput :
# - Nom affiché = label
# - Valeur envoyée = species_code
species_choices_gcuber <- setNames(
  species_db$species_code,
  species_db$label
)

## 1.2 - (Optionnel) Tableau de documentation des modèles ----
# EXTRAPOLATION :
# Pour l'onglet "Documentation", l'idée est d'avoir une table
# résumant les modèles disponibles. Pour l'instant, je crée un
# squelette minimal à compléter plus tard.

models_doc <- data.frame(
  modele       = c("dagnelie_vc22_1", "vallet_vc22", "vallet_vta"),
  type_volume  = c("vc22", "vc22", "vta"),
  description  = c(
    "Dagnelie : volume marchand jusqu’à c22",
    "Vallet : volume marchand jusqu’à c22",
    "Vallet : volume total aérien"
  ),
  variables    = c(
    "c130 (+ éventuelle hauteur)",
    "c130 (+ éventuelle hauteur)",
    "c130 (+ éventuelle hauteur)"
  ),
  commentaire  = c(
    "Exemple : compléter avec domaine de validité réel.",
    "Exemple : compléter avec domaine de validité réel.",
    "Exemple : compléter avec domaine de validité réel."
  ),
  stringsAsFactors = FALSE
)
# À adapter avec les informations précises de ton rapport.

##############################################################
# 2) - FONCTIONS UTILITAIRES ----
##############################################################

## 2.1 - Aides générales pour plus tard (server) ----

# Test simple sur les noms d'arguments (reprise de dagnelie)
has_name <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

# Parse un texte "100, 110, 125" -> c(100,110,125)
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

# Recyclage d'un vecteur à une longueur n
recycle_to <- function(x, n) {
  if (is.null(x)) return(rep(NA, n))
  if (length(x) == n) return(x)
  if (length(x) == 1) return(rep(x, n))
  stop("Longueurs incompatibles : ", length(x), " vs ", n)
}

## 2.2 - Lecture robuste de CSV / XLSX (comme dagnelie) ----

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

##############################################################
# 3) - INTERFACE UTILISATEUR (UI) ----
##############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Tarifs de cubage (package GCubeR)"),
  
  navbarPage(
    title = "GCubeR – Outils de cubage",
    
    ##########################################################
    # 3.1 Onglet 1 : Arbre unique / comparaison de modèles ----
    ##########################################################
    tabPanel(
      "Arbre unique",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Saisis un ou plusieurs arbres. ",
            "Pour plusieurs arbres, utilise des virgules : ex. 100,110,125."
          ),
          
          # Types de volumes demandés (vc22, vtot, biomasse)
          selectInput(
            "vol_type_uni",
            label = HTML(
              "Choisir le(s) type(s) de volume <span style='color:red;'>*</span> :"
            ),
            choices = c(
              "Volume marchand (jusqu'à c22)" = "vc22",
              "Volume total tige"             = "vtot",
              "Volume total aérien (vta)"     = "vta",
              "Biomasse / Carbone"           = "biomass"
            ),
            selected = "vc22",
            multiple = TRUE
          ),
          
          helpText(
            tags$em(
              "* Plusieurs types de volumes peuvent être calculés simultanément pour un même arbre. ",
              "Tous les types de volumes ne sont pas disponibles pour toutes les essences."
            )
          ),
          
          # Essence (liste GCubeR)
          selectInput(
            "species_uni",
            label   = "Essence :",
            choices = species_choices_gcuber,
            selected = NULL,
            multiple = FALSE
          ),
          
          # Type de mesure (C130, C150, D130, D150, DBH, ...)
          selectInput(
            "meas_type_uni",
            label = "Type de mesure de la tige :",
            choices = c(
              "Circonférence à 1.30 m (C130)" = "c130",
              "Circonférence à 1.50 m (C150)" = "c150",
              "Diamètre à 1.30 m (D130)"      = "d130",
              "Diamètre à 1.50 m (D150)"      = "d150",
              "Diamètre à 1.30 m (DBH)"       = "dbh"
            ),
            selected = "c130"
          ),
          
          # Valeur(s) de la mesure (cm)
          textInput(
            "meas_value_uni",
            label = "Valeur(s) de circonférence / diamètre (cm) :",
            value = "100",
            placeholder = "Ex. 100, 110, 125"
          ),
          
          helpText("Tu peux encoder plusieurs valeurs séparées par des virgules."),
          
          # Type de hauteur
          selectInput(
            "h_type_uni",
            label = "Type de hauteur :",
            choices = c(
              "Hauteur totale"           = "htot",
              "Hauteur dominante (Hdom)" = "hdom",
              "Sans mesure de hauteur"   = "none"
            ),
            selected = "none"
          ),
          
          # Champ de hauteur dynamique (affiché seulement si nécessaire)
          uiOutput("h_value_ui"),
          
          hr(),
          
          # Choix de l'ensemble de modèles à utiliser (extrapolation)
          checkboxInput(
            "use_all_models_uni",
            "Utiliser tous les modèles compatibles (Dagnelie, Vallet, ...)",
            value = TRUE
          ),
          
          # Plus tard : possibilité de limiter à un sous-ensemble
          # (liste déroulante à partir de models_doc$modele)
          conditionalPanel(
            condition = "!input.use_all_models_uni",
            selectInput(
              "models_subset_uni",
              "Limiter aux modèles suivants :",
              choices  = models_doc$modele,
              selected = models_doc$modele,
              multiple = TRUE
            )
          ),
          
          hr(),
          actionButton("calc_uni", "Calculer (arbre unique)", class = "btn btn-primary"),
          checkboxInput("show_call_uni", "Afficher l'appel R", TRUE)
        ),
        
        mainPanel(
          h3("Résultats – Arbre(s) unique(s)"),
          
          verbatimTextOutput("result_msg_uni"),
          tableOutput("result_table_uni"),
          
          conditionalPanel(
            condition = "input.show_call_uni == true",
            h4("Appels R sous-jacents (résumé)"),
            verbatimTextOutput("appel_uni")
          ),
          
          hr(),
          h4("Modèles utilisés et domaines de validité"),
          uiOutput("models_expl_uni")
        )
      )
    ),
    
    ##########################################################
    # 3.2 Onglet 2 : Jeu de données (lot d'arbres) ----
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
          
          # Types de volumes à calculer pour le lot
          checkboxGroupInput(
            "vol_type_batch",
            "Types de volumes / résultats à calculer :",
            choices = c(
              "vc22 (volume marchand jusqu'à c22)" = "vc22",
              "vtot (volume total tige)"           = "vtot",
              "vta (volume total aérien)"          = "vta",
              "Biomasse / Carbone"                = "biomass"
            ),
            selected = c("vc22")
          ),
          
          hr(),
          
          # Mapping manuel des colonnes (comme demandé)
          uiOutput("mapping_ui"),
          
          hr(),
          checkboxInput(
            "use_all_models_batch",
            "Utiliser tous les modèles compatibles",
            value = TRUE
          ),
          conditionalPanel(
            condition = "!input.use_all_models_batch",
            selectInput(
              "models_subset_batch",
              "Limiter aux modèles suivants :",
              choices  = models_doc$modele,
              selected = models_doc$modele,
              multiple = TRUE
            )
          ),
          
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
          verbatimTextOutput("result_msg_batch")
        )
      )
    ),
    
    ##########################################################
    # 3.3 Onglet 3 : Documentation des modèles ----
    ##########################################################
    tabPanel(
      "Documentation des tarifs",
      fluidRow(
        column(
          12,
          h3("Tarifs / modèles de volume et de biomasse disponibles"),
          p(
            "Ce tableau résume les principaux modèles de volume et de biomasse ",
            "utilisés par le package GCubeR. Il est à compléter avec les données ",
            "issues du rapport et des publications originales."
          ),
          tableOutput("models_table"),
          hr(),
          p(
            em("Remarque : "),
            "la Shiny utilise, pour chaque arbre, uniquement les modèles dont les ",
            "conditions d'application et le domaine de validité sont respectés."
          )
        )
      )
    )
  )
)

##############################################################
# 4) SERVER ----
##############################################################

server <- function(input, output, session) {
  
  ##########################################################
  # 4.1 Arbre unique ----
  ##########################################################
  
  # Champ de hauteur dynamique en fonction du type choisi
  output$h_value_ui <- renderUI({
    if (identical(input$h_type_uni, "none")) {
      return(NULL)
    }
    
    label_txt <- if (identical(input$h_type_uni, "hdom")) {
      "Hauteur dominante Hdom (m) :"
    } else {
      "Hauteur totale (m) :"
    }
    
    textInput(
      "h_value_uni",
      label = label_txt,
      value = "",
      placeholder = "Ex. 20, 22, 25"
    )
  })
  
  # SQUELETTE CALCUL ARBRE UNIQUE (sans logique GCubeR pour l'instant)
  calc_res_uni <- eventReactive(input$calc_uni, {
    # Ici, on ne fait encore aucun appel aux fonctions GCubeR.
    # On construit seulement une table d'entrée propre, qui
    # servira ensuite de base pour brancher les modèles.
    
    meas_vals <- parse_vector(input$meas_value_uni)
    h_vals    <- parse_vector(input$h_value_uni)
    
    n <- length(meas_vals)
    
    data.frame(
      species_code = rep(input$species_uni, n),
      meas_type    = rep(input$meas_type_uni, n),
      meas_value   = meas_vals,
      h_type       = rep(input$h_type_uni, n),
      h_value      = if (!is.null(h_vals)) recycle_to(h_vals, n) else rep(NA, n),
      # Colonnes résultats factices pour l'instant
      vc22_dagnelie = NA_real_,
      vc22_vallet   = NA_real_,
      vta_vallet    = NA_real_,
      biomass_dummy = NA_real_,
      stringsAsFactors = FALSE
    )
  }, ignoreInit = TRUE)
  
  output$result_table_uni <- renderTable({
    req(calc_res_uni())
    calc_res_uni()
  })
  
  output$result_msg_uni <- renderPrint({
    req(calc_res_uni())
    df <- calc_res_uni()
    cat("Nombre d'arbres encodés :", nrow(df), "\n")
    cat("Les valeurs de volume et de biomasse seront calculées une fois la logique GCubeR implémentée.")
  })
  
  output$appel_uni <- renderPrint({
    req(calc_res_uni())
    df <- calc_res_uni()
    cat("Les appels R exacts (GCubeR::...) seront construits ici, ",
        "en fonction des modèles sélectionnés et du type de volume.\n",
        "Exemple (à adapter plus tard) :\n",
        "GCubeR::dagnelie_vc22_1(c130 = ..., htot = ...)")
  })
  
  # Bloc explicatif des modèles utilisés (ici encore statique / à enrichir)
  output$models_expl_uni <- renderUI({
    # Plus tard, filtrer 'models_doc' sur les modèles effectivement utilisés.
    # Pour l'instant, on affiche tout.
    tagList(
      lapply(seq_len(nrow(models_doc)), function(i) {
        m <- models_doc[i, ]
        tags$details(
          tags$summary(m$modele),
          tags$p(
            strong("Type de volume : "), m$type_volume, br(),
            strong("Description : "), m$description, br(),
            strong("Variables nécessaires : "), m$variables, br(),
            strong("Commentaire : "), m$commentaire
          )
        )
      })
    )
  })
  
  ##########################################################
  # 4.2 Jeu de données (lot d'arbres) ----
  ##########################################################
  
  # Import brut du fichier
  dat_raw <- reactive({
    req(input$file_up)
    ext <- tools::file_ext(input$file_up$name)
    read_table_any(
      path       = input$file_up$datapath,
      ext        = ext,
      sep_choice = input$sep_choice
    )
  })
  
  # Aperçu des 10 premières lignes
  output$preview <- renderTable({
    req(dat_raw())
    head(dat_raw(), 10)
  })
  
  # UI de mapping manuel des colonnes
  output$mapping_ui <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    
    tagList(
      h4("Mapping des colonnes"),
      helpText(
        "Sélectionne manuellement les colonnes correspondant aux champs nécessaires. ",
        "Aucune détection automatique n'est faite."
      ),
      
      # Colonne essence
      selectInput(
        "col_species", "Colonne essence (species_code) :",
        choices = cols
      ),
      
      # Type de mesure : colonne ou fixe
      selectInput(
        "col_meas_type", "Colonne type de mesure (ou fixe) :",
        choices = c("<fixe>", cols),
        selected = "<fixe>"
      ),
      conditionalPanel(
        condition = "input.col_meas_type == '<fixe>'",
        selectInput(
          "meas_type_fix", "Type de mesure fixe :",
          choices = c(
            "C130" = "c130",
            "C150" = "c150",
            "D130" = "d130",
            "D150" = "d150",
            "DBH"  = "dbh"
          ),
          selected = "c130"
        )
      ),
      
      # Colonne valeur mesure
      selectInput(
        "col_meas_value", "Colonne valeur de mesure (cm) :",
        choices = cols
      ),
      
      # Type de hauteur : sans / fixe / colonne
      selectInput(
        "h_type_mode",
        "Mode pour la hauteur :",
        choices = c(
          "Sans hauteur"          = "none",
          "Hauteur fixe (m)"      = "fixed",
          "Colonne hauteur (m)"   = "column"
        ),
        selected = "none"
      ),
      conditionalPanel(
        condition = "input.h_type_mode == 'fixed'",
        selectInput(
          "h_type_fix", "Type de hauteur fixe :",
          choices = c("Hauteur totale (htot)" = "htot",
                      "Hauteur dominante (hdom)" = "hdom"),
          selected = "htot"
        ),
        numericInput(
          "h_value_fix", "Valeur de hauteur fixe (m) :",
          value = 20,
          min = 0
        )
      ),
      conditionalPanel(
        condition = "input.h_type_mode == 'column'",
        selectInput(
          "col_h_type", "Colonne type de hauteur (optionnelle) :",
          choices = c("", cols),
          selected = ""
        ),
        selectInput(
          "col_h_value", "Colonne hauteur (m) :",
          choices = cols
        )
      )
    )
  })
  
  # SQUELETTE CALCUL LOT (sans logique GCubeR pour l'instant)
  calc_res_batch <- eventReactive(input$calc_batch, {
    df <- dat_raw()
    
    # Récupération des colonnes mappées (sans vérification avancée pour l'instant)
    species_col <- df[[input$col_species]]
    
    meas_type_vec <- if (input$col_meas_type == "<fixe>") {
      rep(input$meas_type_fix, nrow(df))
    } else {
      as.character(df[[input$col_meas_type]])
    }
    
    meas_value_vec <- df[[input$col_meas_value]]
    
    # Gestion des hauteurs (mode simplifié)
    if (identical(input$h_type_mode, "none")) {
      h_type_vec  <- rep("none", nrow(df))
      h_value_vec <- rep(NA_real_, nrow(df))
    } else if (identical(input$h_type_mode, "fixed")) {
      h_type_vec  <- rep(input$h_type_fix, nrow(df))
      h_value_vec <- rep(input$h_value_fix, nrow(df))
    } else {
      # mode "column"
      h_type_vec <- if (nzchar(input$col_h_type)) {
        as.character(df[[input$col_h_type]])
      } else {
        # si pas de colonne type, on suppose htot
        rep("htot", nrow(df))
      }
      h_value_vec <- df[[input$col_h_value]]
    }
    
    # Résultats factices pour l'instant
    out <- data.frame(
      species_code = species_col,
      meas_type    = meas_type_vec,
      meas_value   = meas_value_vec,
      h_type       = h_type_vec,
      h_value      = h_value_vec,
      # Colonnes résultats placeholder : à remplacer par les
      # sorties réelles des fonctions GCubeR (vc22, vtot, vta, biomasse...)
      vc22_dagnelie = NA_real_,
      vc22_vallet   = NA_real_,
      vta_vallet    = NA_real_,
      biomass_dummy = NA_real_,
      stringsAsFactors = FALSE
    )
    
    out
  }, ignoreInit = TRUE)
  
  output$result_table_batch <- renderTable({
    req(calc_res_batch())
    head(calc_res_batch(), 50)
  })
  
  output$result_msg_batch <- renderPrint({
    req(calc_res_batch())
    df <- calc_res_batch()
    cat("Nombre de lignes dans le fichier :", nrow(dat_raw()), "\n")
    cat("Nombre de lignes dans le tableau résultats :", nrow(df), "\n")
    cat("Les colonnes de volume et biomasse seront remplies après implémentation de la logique GCubeR.")
  })
  
  # Modèle Excel de base
  output$dl_template_xlsx <- downloadHandler(
    filename = function() "template_gcuber.xlsx",
    content  = function(file) {
      tpl <- data.frame(
        species_code = c("PICEA_ABIES", "FAGUS_SYLVATICA"),
        meas_type    = c("c130", "c130"),
        meas_value   = c(100, 120),
        h_type       = c("htot", "hdom"),
        h_value      = c(25, 22),
        stringsAsFactors = FALSE
      )
      writexl::write_xlsx(tpl, path = file)
    }
  )
  
  # Export des résultats (CSV)
  output$dl_results <- downloadHandler(
    filename = function() "resultats_gcuber.csv",
    content  = function(file) {
      req(calc_res_batch())
      write.csv(calc_res_batch(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  ##########################################################
  # 4.3 Documentation des modèles ----
  ##########################################################
  
  output$models_table <- renderTable({
    models_doc
  })
}

##############################################################
# 5) LANCEMENT ----
##############################################################

shinyApp(ui = ui, server = server)
