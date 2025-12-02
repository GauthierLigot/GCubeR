##############################################################
# Application Shiny - Tarifs Dagnelie / GCubeR
# Auteur : Timon LUIZI
# DATE last modif : 20-10-2025 
##############################################################

##############################################################
# 0) - EN-TÊTE ET INSTALLATION DES PACKAGES ----
##############################################################

## Installation si nécessaire ----
if (!requireNamespace("GCubeR", quietly = TRUE)) {
  
  # Installer remotes si absent
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  
  # Installer GCubeR depuis GitLab ULiège
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
# Stringr requis par guess_sep()
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
library(stringr)

##############################################################
# 1) - DONNÉES DE RÉFÉRENCE : LISTE DES ESPÈCES ----
##############################################################

## Liste officielle des codes et noms français complets ----
species_codes <- data.frame(
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

## Codes agrégés (catégories). Par défaut on les bloque en calcul unitaire. ----
aggregate_codes <- c("110","120","150","210","220")

# Chargement de la liste des essences depuis le CSV
species_db <- read.csv(
  "inst/Shiny/species_list_gcuber.csv",
  sep = ";",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# Crée un label combiné "Nom français (Nom latin)"
species_db$label <- ifelse(
  is.na(species_db$nom_latin) | species_db$nom_latin == "",
  species_db$nom_fr,
  paste0(species_db$nom_fr, " (", species_db$nom_latin, ")")
)

# On prépare la liste pour selectInput :
#  - ce que voit l'utilisateur = label
#  - ce qui est envoyé à l'app = species_code
species_choices_gcuber <- setNames(
  species_db$species_code,
  species_db$label
)


##############################################################
# 2) FONCTIONS UTILITAIRES ----
##############################################################

## 2.1) Aides générales ----
has_name <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)

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

## 2.2) Mapping des arguments réels des fonctions tarif ----
# élargi pour couvrir plusieurs conventions possibles.
detect_mapping <- function(f) {
  fn <- names(formals(f))
  fn <- setdiff(fn, "...")
  circ_names <- c("C130","c130","circ","CBH","cbh","C","G","g","Circonference","circonference")
  h_names    <- c("H","h","Htot","Hdom","Hd","height","Ht","Hauteur","hauteur")
  sp_names   <- c("speciesCode","species","sp","essence","espece","code","Code")
  list(
    circ = intersect(circ_names, fn)[1],
    h    = intersect(h_names,    fn)[1],
    sp   = intersect(sp_names,   fn)[1],
    all  = fn
  )
}

## 2.3) Lecture robuste de CSV / XLSX ----
# - Détecte automatiquement le séparateur si "auto".
# - Permet de forcer ; , ou tab.
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

read_table_any <- function(path, ext = "csv", sep_choice = c("auto","; ",", ","tab")) {
  sep_choice <- match.arg(sep_choice)
  if (tolower(ext) == "xlsx") {
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (sep_choice == "auto") {
    which <- guess_sep(path)
  } else {
    which <- switch(sep_choice, "; " = "semicolon", ", " = "comma", "tab" = "tab")
  }
  sep <- switch(which, semicolon = ";", comma = ",", tab = "\t")
  utils::read.table(path, header = TRUE, sep = sep, quote = "\"", dec = ".", stringsAsFactors = FALSE, check.names = FALSE)
}

##############################################################
# 3) - INTERFACE UTILISATEUR (UI) ----
##############################################################

library(shiny)

# ici tu mets ton UI complet (avec tes onglets, etc.)
ui <- fluidPage(
  navbarPage(
    title = "GCubeR – Outils de cubage",
    tabPanel(
      "Arbre unique",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Saisis un ou plusieurs arbres. Pour plusieurs arbres, utilise des virgules : ex. 100,110,125."
          ),
          selectInput(
            "vol_type_uni",
            label = HTML("Choisir le(s) type(s) de volume <span style='color:red;'>*</span> :"),
            choices = c(
              "Volume marchand (jusqu'à c22)" = "vc22",
              "Volume total"                  = "vtot",
              "Biomasse totale"               = "biomass"
            ),
            selected = "vc22",
            multiple = TRUE
          ),
          helpText(
            tags$em(
              "* Plusieurs types de volumes peuvent être calculés simultanément pour un même arbre.",
              "Tous les volumes ne sont pas disponibles pour toutes les essences."
            )
          ),
          selectInput(
            "species_uni",
            label = "Essence :",
            choices = species_choices_gcuber,
            selected = NULL,
            multiple = FALSE
          ),
          selectInput(
            "meas_type_uni",
            label = "Type de mesure de la tige :",
            choices = c(
              "Circonférence à 1.30 m (C130)" = "c130",
              "Circonférence à 1.50 m (C150)" = "c150",
              "Diamètre à 1.30 m (D130)"      = "d130",
              "Diamètre à 1.50 m (D150)"      = "d150"
            ),
            selected = "c130"
          ),
          textInput(
            "meas_value_uni",
            label = "Valeur(s) de circonférence / diamètre (cm) :",
            value = "100",
            placeholder = "Ex. 100, 110, 125"
          ),
          helpText("Tu peux encoder plusieurs valeurs séparées par des virgules."),
          selectInput(
            "h_type_uni",
            label = "Type de hauteur :",
            choices = c(
              "Hauteur totale"              = "htot",
              "Hauteur dominante (Hdom)"    = "hdom",
              "Sans mesure de hauteur"      = "none"
            ),
            selected = "none"
          ),
          uiOutput("h_value_ui"),
          actionButton("calc_uni", "Calculer (arbre unique)", class = "btn btn-primary"),
          checkboxInput("show_call_uni", "Afficher l'appel R", TRUE)
        ),
        mainPanel(
          h3("Résultat"),
          verbatimTextOutput("result_msg_uni"),
          tableOutput("result_table_uni"),
          conditionalPanel(
            condition = "input.show_call_uni == true",
            h4("Appel R"),
            verbatimTextOutput("appel_uni")
          ),
          hr(),
          h4("Aide intégrée"),
          uiOutput("help_uni")
        )
      )
    )
    # ici tu peux laisser les autres onglets dagnelie ou les commenter provisoirement
  )
)

# serveur “vide” pour tester l’interface
server <- function(input, output, session) {
  # pour l’instant : rien, juste pour que l’app démarre
}

shinyApp(ui = ui, server = server)


