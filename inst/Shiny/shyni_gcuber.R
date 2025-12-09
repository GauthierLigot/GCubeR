##############################################################
# Application Shiny - Tarifs GCubeR
# Auteur : Timon LUIZI
# DATE last modif : 09-12-2025 
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

# Spinners de chargement
if (!requireNamespace("shinycssloaders", quietly = TRUE)) {
  install.packages("shinycssloaders")
}
library(shinycssloaders)

##############################################################
# 1) - DONNÉES DE RÉFÉRENCE ----
##############################################################

## 1.1 - Chargement des métadonnées d'équations ----

if ("equations_GCubeR" %in% data(package = "GCubeR")$results[, "Item"]) {
  data("equations_GCubeR", package = "GCubeR", envir = environment())
  models_doc <- equations_GCubeR
} else {
  stop(
    "Le dataset 'equations_GCubeR' n'a pas été trouvé dans le package GCubeR.\n",
    "Vérifie que le package est bien installé et à jour."
  )
}

# Table de densité pour la biomasse
if ("density_table" %in% data(package = "GCubeR")$results[, "Item"]) {
  data("density_table", package = "GCubeR", envir = environment())
} else {
  density_table <- NULL
}

## 1.2 - Construction de la liste d’essences ----

species_ref <- unique(models_doc[, c("species_code", "species_name_fr")])
species_ref$species_name_fr[is.na(species_ref$species_name_fr)] <- ""

# On ne garde QUE les essences avec un species_code renseigné
species_ref <- species_ref[
  !is.na(species_ref$species_code) & nzchar(species_ref$species_code),
]

# Reconstruction du nom latin à partir de species_code (PICEA_ABIES -> "Picea abies")
species_ref$species_name_lat <- vapply(
  species_ref$species_code,
  function(sc) {
    sc2 <- gsub("_", " ", tolower(sc))
    paste(toupper(substr(sc2, 1, 1)), substr(sc2, 2, nchar(sc2)), sep = "")
  },
  FUN.VALUE = character(1)
)

# Label : Nom FR (Nom latin)
species_ref$label <- ifelse(
  nzchar(species_ref$species_name_fr),
  paste0(species_ref$species_name_fr, " (", species_ref$species_name_lat, ")"),
  species_ref$species_name_lat
)

species_ref <- species_ref[order(species_ref$species_name_fr,
                                 species_ref$species_code), ]

species_choices_gcuber <- setNames(
  species_ref$species_code,
  species_ref$label
)

## 1.3 - Table de codes numériques (Dagnelie) ----

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

species_lookup <- merge(
  species_ref,
  species_codes_num,
  by.x = "species_name_fr",
  by.y = "nom",
  all = TRUE
)

## 1.4 - Normalisation des essences (batch) ----

normalize_species <- function(x) {
  x_chr <- as.character(x)
  out <- rep(NA_character_, length(x_chr))
  unknown <- character()
  
  for (i in seq_along(x_chr)) {
    val <- trimws(x_chr[i])
    if (is.na(val) || !nzchar(val)) next
    
    up <- toupper(val)
    hit <- species_lookup$species_code[
      toupper(species_lookup$species_code) == up |
        toupper(species_lookup$species_name_fr) == up
    ]
    
    hit <- unique(hit[!is.na(hit)])
    if (length(hit) >= 1) {
      out[i] <- hit[1]
    } else {
      unknown <- c(unknown, val)
    }
  }
  
  if (length(unique(unknown)) > 0) {
    warning(
      "Certaines essences n'ont pas pu être reconnues (ni comme species_code ni comme nom français) et sont mises à NA : ",
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
  
  ext <- tolower(ext)
  if (ext == "xlsx") {
    out <- tryCatch(
      readxl::read_excel(path),
      error = function(e) {
        stop("Erreur lors de la lecture du fichier Excel (.xlsx) : ",
             conditionMessage(e),
             "\nVérifie que le fichier est bien au format .xlsx et non .xls.")
      }
    )
    return(as.data.frame(out))
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

## 2.3 - Normalisation des mesures ----

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
    dbh  = NA_real_,
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
    df$dbh <- meas_vals
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
    df$dbh  <- tmp$dbh
  }
  
  if (h_type == "htot") {
    df$htot <- h_vals
  } else if (h_type == "hdom") {
    df$hdom <- h_vals
  }
  
  df
}

## 2.4 - Version batch de la normalisation ----

build_base_batch <- function(df_in,
                             col_species,
                             col_meas_value,
                             meas_type_batch,
                             h_mode_batch,
                             col_htot,
                             col_hdom = "",
                             hdom_value) {
  n <- nrow(df_in)
  
  species_raw  <- df_in[[col_species]]
  species_code <- normalize_species(species_raw)
  
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
  
  if (meas_type_batch == "c130") {
    base$c130 <- meas_vals
  } else if (meas_type_batch == "c150") {
    base$c150 <- meas_vals
  } else if (meas_type_batch == "d130") {
    base$dbh <- meas_vals
  }
  
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
  if (identical(h_mode_batch, "htot") || identical(h_mode_batch, "htot_hdom")) {
    if (nzchar(col_htot)) {
      base$htot <- suppressWarnings(as.numeric(df_in[[col_htot]]))
    }
  }
  if (identical(h_mode_batch, "hdom") || identical(h_mode_batch, "htot_hdom")) {
    if (nzchar(col_hdom)) {
      base$hdom <- suppressWarnings(as.numeric(df_in[[col_hdom]]))
    } else if (!is.null(hdom_value) && !is.na(hdom_value)) {
      base$hdom <- rep(hdom_value, n)
    }
  }
  
  base
}

## 2.5 - Labels avec unités ----

label_with_units <- function(cols) {
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

## 2.6 - Types de volume disponibles par essence ----

get_allowed_volume_types_for_species <- function(sp_code) {
  types <- character(0)
  if (is.null(models_doc) || is.na(sp_code)) {
    return(character(0))
  }
  md <- subset(models_doc, species_code == sp_code)
  if (nrow(md) == 0) return(character(0))
  
  eq_ids <- unique(md$eq_id)
  
  if (length(intersect(eq_ids,
                       c("dagnelie_vc22_1","dagnelie_vc22_1g",
                         "dagnelie_vc22_2","vallet_vc22",
                         "algan_vc22","rondeux_vc22"))) > 0) {
    types <- c(types, "vc22")
  }
  if (length(intersect(eq_ids,
                       c("vallet_vta","bouvard_vta","algan_vta"))) > 0) {
    types <- c(types, "vta")
  }
  if (length(intersect(eq_ids,
                       c("rondeux_vtot"))) > 0) {
    types <- c(types, "vtot")
  }
  if (length(intersect(eq_ids,
                       c("dagnelie_br","vc22br"))) > 0) {
    types <- c(types, "br")
  }
  
  if (!is.null(density_table) &&
      sp_code %in% density_table$species_code) {
    types <- c(types, "biomass")
  }
  
  unique(types)
}

##############################################################
# 3) - UI ----
##############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      /* Fond général */
      body {
        background-color: #f5f7f8;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }

      .container-fluid {
        background-color: transparent;
        padding-bottom: 30px;
      }

      /* Cartes / panneaux */
      .well,
      .tab-content {
        background-color: #ffffff;
        border-radius: 10px;
        border: 1px solid #dde5e7;
        box-shadow: 0 2px 6px rgba(0,0,0,0.06);
      }

      /* ============================= */
      /* NAVBAR */
      /* ============================= */

      .navbar-default {
        background-color: #00707F;   /* Bleu Gembloux */
        border-color: #00707F;
      }

      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #FFFFFF !important;
        font-weight: 600;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #5FA4B0;   /* Bleu clair */
        color: #FFFFFF !important;
        border-bottom: 3px solid #B9CD76; /* Vert */
      }

      /* ============================= */
      /* BOUTONS */
      /* ============================= */

      .btn-primary {
        background-color: #00707F;
        border-color: #00707F;
      }

      .btn-primary:hover,
      .btn-primary:focus {
        background-color: #5FA4B0;
        border-color: #5FA4B0;
      }

      /* ============================= */
      /* TITRES */
      /* ============================= */

      h2, h3 {
        color: #00707F;
        font-weight: 700;
      }

      h4 {
        color: #5FA4B0;
        font-weight: 600;
      }

      /* ============================= */
      /* TABLEAUX */
      /* ============================= */

      table.table th {
        background-color: #B9CD76; /* Vert résultats */
        color: #1f2d2f;
        font-weight: 600;
      }

      table.table td {
        background-color: #FFFFFF;
      }

      /* Cadre avec scroll pour tableaux - plus haut */
      .scroll-table {
        max-width: 100%;
        max-height: 600px; /* augmenté */
        overflow-x: auto;
        overflow-y: auto;
        border: 1px solid #B9CD76;
        border-radius: 6px;
        background-color: #ffffff;
        padding: 8px;
        margin-bottom: 12px;
      }

      /* ============================= */
      /* AIDES / TEXTES */
      /* ============================= */

      .help-block {
        font-size: 0.9em;
        color: #5b6f73;
      }

      /* Liens cliquables (équations) */
      details > summary {
        cursor: pointer;
        color: #00707F;
        font-weight: 600;
      }

      details > summary:hover {
        color: #5FA4B0;
        text-decoration: underline;
      }

      /* Décaler un peu le contenu de chaque onglet du bord du cadre */
      .tab-pane {
        padding: 15px 20px 20px 20px;
      }
    "))
  ),
  
  # Bandeau supérieur avec logo + sélecteur de langue
  div(
    style = "
      background-color:#00707F; /* Bleu Gembloux */
      color:white;
      padding:15px;
      font-size:26px;
      font-weight:bold;
      margin-bottom:20px;
      display:flex;
      align-items:center;
      justify-content:space-between;
    ",
    
    # Partie gauche : logo + titre dynamique
    div(
      style = 'display:flex; align-items:center;',
      img(
        src = 'logo.png',
        height = '50px',
        style = 'margin-right:15px; border-radius:50%; background:white; padding:3px;'
      ),
      div(
        style = 'margin-left:5px;',
        textOutput('app_title', container = span)
      )
    ),
    
    # Partie droite : sélecteur de langue
    div(
      style = 'margin-left:auto; min-width:140px;',
      selectInput(
        'lang',
        label   = NULL,
        choices = c('Français' = 'fr', 'English' = 'en'),
        selected = 'fr',
        width   = '140px'
      )
    )
  ),
  
  navbarPage(
    id    = "main_nav",
    title = textOutput("nav_title"),
    
    ##########################################################
    # 3.0 Introduction ----
    ##########################################################
    tabPanel(
      title = textOutput("tab_intro_title", container = span),
      value = "intro",
      fluidRow(
        column(
          width = 10, offset = 1,
          br(),
          uiOutput("intro_block"),
          br()
        )
      )
    ),
    
    ##########################################################
    # 3.1 Arbre unique ----
    ##########################################################
    tabPanel(
      title = textOutput("tab_uni_title", container = span),
      value = "uni",
      sidebarLayout(
        sidebarPanel(
          textOutput("uni_help", container = helpText),
          
          selectInput(
            "species_uni",
            label   = "",
            choices = species_choices_gcuber,
            selected = NULL,
            multiple = FALSE
          ),
          
          checkboxGroupInput(
            "vol_type_uni",
            label = "",
            choices = c(
              "Volume marchand vc22 (m³)"                = "vc22",
              "Volume total tige vtot (m³)"              = "vtot",
              "Volume total aérien vta (m³)"             = "vta",
              "Volume des branches Dagnelie (br, m³)"    = "br"
            ),
            selected = "vc22"
          ),
          
          checkboxInput(
            "biomass_uni",
            label = "",
            value = FALSE
          ),
          
          textOutput("uni_vol_note", container = helpText),
          
          selectInput(
            "meas_type_uni",
            label = "",
            choices = c(
              "Circonférence à 1.30 m (C130, cm)"        = "c130",
              "Circonférence à 1.50 m (C150, cm)"        = "c150",
              "Diamètre à 1.30 m (D130 / DBH, cm)"       = "d130"
            ),
            selected = "c130"
          ),
          
          textInput(
            "meas_value_uni",
            label = "",
            value = "100",
            placeholder = "Ex. 100, 110, 125"
          ),
          
          textOutput("uni_multi_vals_help", container = helpText),
          
          selectInput(
            "h_type_uni",
            label = "",
            choices = c(
              "Hauteur totale (htot, m)"           = "htot",
              "Hauteur dominante (Hdom, m)"        = "hdom",
              "Sans mesure de hauteur"             = "none"
            ),
            selected = "none"
          ),
          
          uiOutput("h_value_ui"),
          
          hr(),
          actionButton("calc_uni", "", class = "btn btn-primary"),
          checkboxInput("show_call_uni", "", TRUE)
        ),
        
        mainPanel(
          textOutput("res_uni_title", container = h3),
          verbatimTextOutput("result_msg_uni"),
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(tableOutput("result_table_uni"))
          ),
          
          conditionalPanel(
            condition = "input.show_call_uni == true",
            textOutput("uni_warn_title", container = h4),
            verbatimTextOutput("appel_uni")
          ),
          
          hr(),
          textOutput("uni_models_title", container = h4),
          textOutput("uni_models_help", container = p),
          uiOutput("models_expl_uni"),
          
          br(), br()
        )
      )
    ),
    
    ##########################################################
    # 3.2 Jeu de données ----
    ##########################################################
    tabPanel(
      title = textOutput("tab_batch_title", container = span),
      value = "batch",
      sidebarLayout(
        sidebarPanel(
          uiOutput("file_up_ui"),
          
          radioButtons(
            "sep_choice",
            "",
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
            "",
            choices = c(
              "vc22 (volume marchand, m³)"                = "vc22",
              "vtot (volume total tige, m³)"              = "vtot",
              "vta (volume total aérien, m³)"             = "vta",
              "Volume des branches Dagnelie (br, m³)"     = "br",
              "Biomasse / C / CO₂ (t)"                    = "biomass"
            ),
            selected = "vc22"
          ),
          
          hr(),
          uiOutput("mapping_ui"),
          
          hr(),
          actionButton("calc_batch", "", class = "btn btn-primary"),
          hr(),
          uiOutput("dl_template_xlsx_ui"),
          uiOutput("dl_results_ui")
        ),
        
        mainPanel(
          textOutput("batch_preview_title", container = h3),
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(tableOutput("preview"))
          ),
          hr(),
          textOutput("batch_res_title", container = h3),
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(tableOutput("result_table_batch"))
          ),
          verbatimTextOutput("result_msg_batch"),
          textOutput("batch_warn_title", container = h4),
          verbatimTextOutput("appel_batch")
        )
      )
    ),
    
    ##########################################################
    # 3.3 Plotage volume par classes ----
    ##########################################################
    tabPanel(
      title = textOutput("tab_plot_title", container = span),
      value = "plot",
      sidebarLayout(
        sidebarPanel(
          uiOutput("file_plot_ui"),
          radioButtons(
            "sep_choice_plot",
            "",
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
          uiOutput("mapping_plot_ui"),
          hr(),
          selectInput(
            "volume_model_plot",
            "",
            choices = c(
              "vc22 combiné (toutes équations disponibles)" = "vc22_combined",
              "Dagnelie tarif 2 (dagnelie_vc22_2)"          = "dagnelie_vc22_2",
              "Dagnelie tarif 1 (dagnelie_vc22_1)"          = "dagnelie_vc22_1",
              "Vallet vc22 (vallet_vc22)"                   = "vallet_vc22",
              "Rondeux vc22 (rondeux_vc22_vtot)"            = "rondeux_vc22",
              "Algan vc22 (algan_vta_vc22)"                 = "algan_vc22"
            ),
            selected = "vc22_combined"
          ),
          textOutput("plot_model_help", container = helpText),
          actionButton("calc_plot", "", class = "btn btn-primary")
        ),
        mainPanel(
          textOutput("plot_table_title", container = h3),
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(tableOutput("plot_table"))
          ),
          hr(),
          textOutput("plot_hist_title", container = h3),
          shinycssloaders::withSpinner(plotOutput("plot_volume")),
          textOutput("plot_hist_help", container = helpText),
          hr(),
          textOutput("plot_export_title", container = h4),
          fluidRow(
            column(
              6,
              uiOutput("dl_plot_csv_ui")
            ),
            column(
              6,
              uiOutput("dl_plot_png_ui")
            )
          )
        )
      )
    ),
    
    ##########################################################
    # 3.4 Liste des essences ----
    ##########################################################
    tabPanel(
      title = textOutput("tab_species_title", container = span),
      value = "species",
      fluidRow(
        column(
          12,
          textOutput("species_title", container = h3),
          textOutput("species_help", container = p),
          div(
            class = "scroll-table",
            tableOutput("species_table")
          )
        )
      )
    ),
    
    ##########################################################
    # 3.5 Documentation des équations ----
    ##########################################################
    tabPanel(
      title = textOutput("tab_models_title", container = span),
      value = "models",
      fluidRow(
        column(
          12,
          textOutput("models_title", container = h3),
          textOutput("models_help", container = p),
          div(
            class = "scroll-table",
            tableOutput("models_table")
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
  
  uni_warnings   <- reactiveVal(character())
  batch_warnings <- reactiveVal(character())
  
  get_lang <- function() {
    if (is.null(input$lang)) "fr" else input$lang
  }
  
  ## --- Boutons de téléchargement bilingues ----
  
  output$dl_template_xlsx_ui <- renderUI({
    lang <- get_lang()
    label <- if (lang == "en") "Download Excel template" else "Télécharger modèle Excel"
    downloadButton("dl_template_xlsx", label = label)
  })
  
  output$dl_results_ui <- renderUI({
    lang <- get_lang()
    label <- if (lang == "en") "Download results (CSV)" else "Télécharger résultats (CSV)"
    downloadButton("dl_results", label = label)
  })
  
  output$dl_plot_csv_ui <- renderUI({
    lang <- get_lang()
    label <- if (lang == "en") "Download table (CSV)" else "Télécharger la table (CSV)"
    downloadButton("dl_plot_csv", label = label)
  })
  
  output$dl_plot_png_ui <- renderUI({
    lang <- get_lang()
    label <- if (lang == "en") "Download graphic (PNG)" else "Télécharger le graphique (PNG)"
    downloadButton("dl_plot_png", label = label)
  })
  
  ## --- Gestion de la langue : titres bandeau + navbar + onglets ----
  
  output$app_title <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "Tree volume tariffs (GCubeR package)"
    } else {
      "Tarifs de cubage (package GCubeR)"
    }
  })
  
  output$nav_title <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "GCubeR – Cubing tools"
    } else {
      "GCubeR – Outils de cubage"
    }
  })
  
  output$tab_intro_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Introduction" else "Introduction"
  })
  output$tab_uni_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Single tree" else "Arbre unique"
  })
  output$tab_batch_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Data set" else "Jeu de données"
  })
  output$tab_plot_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Volume by classes" else "Plotage volumes par classes"
  })
  output$tab_species_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Species list" else "Liste des essences"
  })
  output$tab_models_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Equations documentation" else "Documentation des équations"
  })
  
  ## --- FileInput bilingues ----
  
  output$file_up_ui <- renderUI({
    lang <- get_lang()
    lab <- if (lang == "en") "File (.xlsx or .csv)" else "Fichier (.xlsx ou .csv)"
    fileInput(
      "file_up",
      label = lab,
      accept = c(".xlsx", ".csv", ".CSV", ".XLSX")
    )
  })
  
  output$file_plot_ui <- renderUI({
    lang <- get_lang()
    lab <- if (lang == "en") "File (.xlsx or .csv) for plotting"
    else "Fichier (.xlsx ou .csv) pour le plotage"
    fileInput(
      "file_plot",
      label = lab,
      accept = c(".xlsx", ".csv", ".CSV", ".XLSX")
    )
  })
  
  ## --- Contenu bilingue de l'onglet Introduction ----
  
  output$intro_block <- renderUI({
    lang <- get_lang()
    
    if (lang == "en") {
      tagList(
        h2("GCubeR: tree volume and biomass models"),
        p(
          "GCubeR provides a unified implementation of several volume tariffs and biomass models ",
          "published for temperate forests. This Shiny application allows you to use these models ",
          "without writing R code, focusing on species choice and measured variables."
        ),
        hr(),
        h3("Main features of the application"),
        tags$ul(
          tags$li(
            strong("Single tree: "),
            "computation of volumes (vc22, vtot, vta, branches) and, when available, ",
            "biomass / carbon / CO₂ for one or several trees entered manually."
          ),
          tags$li(
            strong("Data set: "),
            "import of a .xlsx or .csv file, normalisation of species and measurements, ",
            "and batch application of volume tariffs and biomass equations."
          ),
          tags$li(
            strong("Volume plotting by classes: "),
            "aggregation of volumes by c130 classes and species, ",
            "with graphical visualisation and export of the results."
          ),
          tags$li(
            strong("Species list: "),
            "summary of species codes (species_code) and French/Latin names available in GCubeR."
          ),
          tags$li(
            strong("Equation documentation: "),
            "table of equation metadata (family, predicted variable, validity domain, references, etc.)."
          )
        ),
        hr(),
        h3("Structure of the GCubeR package"),
        p(
          "The diagram below gives an overview of the internal structure of the package ",
          "and the links between normalisation functions, volume tariffs and biomass functions."
        ),
        div(
          style = "text-align:center; margin-bottom: 20px;",
          img(
            src   = "function_diag.png",
            style = "max-width: 100%; height: auto;",
            alt   = "GCubeR structure diagram"
          )
        ),
        hr(),
        h3("References and documentation"),
        p(
          "Most of the implemented equations are taken from publications by Dagnelie et al., ",
          "Vallet et al., Rondeux, Bouvard, Algan, as well as CNIEFEB work for the ",
          "volume–biomass–carbon conversion. In R, the detailed documentation is available via ",
          code('browseVignettes("GCubeR")'), " and through the help pages of each function."
        )
      )
      
    } else {
      tagList(
        h2("GCubeR : calcul de volumes et biomasses d’arbres"),
        p(
          "GCubeR implémente de manière homogène plusieurs tarifs de cubage et modèles de biomasse ",
          "publiés pour les forêts tempérées. Cette application Shiny permet d’utiliser ces modèles ",
          "sans écrire de code R, en se concentrant sur le choix des essences et des variables mesurées."
        ),
        hr(),
        h3("Fonctionnalités principales de l’application"),
        tags$ul(
          tags$li(
            strong("Arbre unique : "),
            "calcul des volumes (vc22, vtot, vta, branches) et, si disponible, ",
            "de la biomasse / carbone / CO₂ pour un ou plusieurs arbres saisis manuellement."
          ),
          tags$li(
            strong("Jeu de données : "),
            "import d’un fichier .xlsx ou .csv, normalisation des essences et des mesures ",
            "et application en lot des tarifs de cubage et des équations de biomasse."
          ),
          tags$li(
            strong("Plotage volumes par classes : "),
            "agrégation des volumes par classes de c130 et par essence, ",
            "avec visualisation graphique et export des résultats."
          ),
          tags$li(
            strong("Liste des essences : "),
            "rappel des codes d’essence (species_code) et des noms français/latins disponibles dans GCubeR."
          ),
          tags$li(
            strong("Documentation des équations : "),
            "table des métadonnées des équations (famille, variable prédite, domaine de validité, références, etc.)."
          )
        ),
        hr(),
        h3("Structure du package GCubeR"),
        p(
          "Le schéma ci-dessous illustre de manière synthétique la structure interne du package ",
          "ainsi que les liens entre les fonctions de normalisation, les tarifs de cubage et les fonctions de biomasse."
        ),
        div(
          style = "text-align:center; margin-bottom: 20px;",
          img(
            src   = "function_diag.png",
            style = "max-width: 100%; height: auto;",
            alt   = "Schéma de synthèse GCubeR"
          )
        ),
        hr(),
        h3("Références et documentation"),
        p(
          "La plupart des équations implémentées sont issues de publications de Dagnelie et al., ",
          "Vallet et al., Rondeux, Bouvard, Algan, ainsi que de travaux du CNIEFEB pour la conversion ",
          "volume–biomasse–carbone. Dans R, la documentation détaillée est accessible via ",
          code('browseVignettes("GCubeR")'), " et via l’aide des fonctions."
        )
      )
    }
  })
  
  ## --- Textes de base multilingues pour l'UI principale ----
  
  output$uni_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "Enter one or several trees. For multiple trees, use commas: e.g. 100,110,125."
    } else {
      "Saisis un ou plusieurs arbres. Pour plusieurs arbres, utilise des virgules : ex. 100,110,125."
    }
  })
  
  output$uni_vol_note <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "* Several volume types can be computed at once. Not all types are available for all species."
    } else {
      "* Plusieurs types de volumes peuvent être calculés simultanément. Tous les types ne sont pas disponibles pour toutes les essences."
    }
  })
  
  output$uni_multi_vals_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "You can enter several values separated by commas."
    } else {
      "Tu peux encoder plusieurs valeurs séparées par des virgules."
    }
  })
  
  output$res_uni_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Results – single tree(s)" else "Résultats – Arbre(s) unique(s)"
  })
  
  output$uni_warn_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Remarks and warnings (GCubeR equations)" else "Remarques et avertissements (équations GCubeR)"
  })
  
  output$uni_models_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Equations available for the selected species" else "Équations disponibles pour l’essence sélectionnée"
  })
  
  output$uni_models_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "Click on a line below to display the details of the equation."
    } else {
      "Clique sur une ligne ci-dessous pour afficher le détail de l’équation."
    }
  })
  
  output$batch_preview_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Preview of imported file" else "Aperçu du fichier importé"
  })
  
  output$batch_res_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Results – batch of trees" else "Résultats – Lot d’arbres"
  })
  
  output$batch_warn_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Remarks and warnings (batch of trees)" else "Remarques et avertissements (lot d’arbres)"
  })
  
  output$plot_model_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "* For the option \"vc22 combined\", the priority is: Dagnelie tariff 2 > Dagnelie tariff 1 > Vallet vc22 > Rondeux vc22 > Algan vc22. For each tree, the first available equation in this order is used."
    } else {
      "* Pour l’option « vc22 combiné », la priorité est la suivante : Dagnelie tarif 2 > Dagnelie tarif 1 > Vallet vc22 > Rondeux vc22 > Algan vc22. Pour chaque arbre, la première équation disponible dans cet ordre est utilisée."
    }
  })
  
  output$plot_table_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Table of volumes [m³] by c130 classes" else "Table de volumes [m³] par classes de c130"
  })
  
  output$plot_hist_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Histogram of volumes by c130 classes" else "Histogramme des volumes par classes de c130"
  })
  
  output$plot_hist_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "* Reminder: for \"vc22 combined\", the combination follows the priority Dagnelie 2 > Dagnelie 1 > Vallet > Rondeux > Algan."
    } else {
      "* Rappel : pour « vc22 combiné », la combinaison suit l’ordre de priorité Dagnelie 2 > Dagnelie 1 > Vallet > Rondeux > Algan."
    }
  })
  
  output$plot_export_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Export of the plot" else "Export du plotage"
  })
  
  output$species_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "List of species used by GCubeR" else "Liste des essences utilisées par GCubeR"
  })
  
  output$species_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "The column species_code is the one to be used in the 'Data set' tab. French names are those recognised by the normalisation."
    } else {
      "La colonne species_code est celle à utiliser dans l’onglet 'Jeu de données'. Les noms français sont ceux reconnus par la normalisation."
    }
  })
  
  output$models_title <- renderText({
    lang <- get_lang()
    if (lang == "en") "Equations and models available in GCubeR" else "Équations et modèles disponibles dans GCubeR"
  })
  
  output$models_help <- renderText({
    lang <- get_lang()
    if (lang == "en") {
      "This table summarises equation metadata (family, predicted variable, species, validity domain, input variables, bibliographic source)."
    } else {
      "Ce tableau reprend les métadonnées des équations (famille, variable prédite, espèces, domaine de validité, variables d’entrée, source bibliographique)."
    }
  })
  
  ## --- Mise à jour dynamique des labels des inputs / boutons ----
  
  observe({
    lang <- get_lang()
    
    # Arbre unique
    if (lang == "en") {
      updateSelectInput(session, "species_uni", label = "Species:")
      updateCheckboxGroupInput(
        session, "vol_type_uni",
        label = "Select volume type(s) *:",
        choices = c(
          "Merchantable volume vc22 (m³)"           = "vc22",
          "Stem total volume vtot (m³)"             = "vtot",
          "Total aboveground volume vta (m³)"       = "vta",
          "Branch volume Dagnelie (br, m³)"         = "br"
        )
      )
      updateCheckboxInput(
        session, "biomass_uni",
        label = "Also compute biomass / C / CO₂ (t)"
      )
      updateSelectInput(
        session, "meas_type_uni",
        label = "Type of stem measurement (cm):",
        choices = c(
          "Circumference at 1.30 m (C130, cm)" = "c130",
          "Circumference at 1.50 m (C150, cm)" = "c150",
          "Diameter at 1.30 m (D130 / DBH, cm)"= "d130"
        )
      )
      updateTextInput(
        session, "meas_value_uni",
        label = "Measurement value(s) (cm):",
        placeholder = "e.g. 100, 110, 125"
      )
      updateSelectInput(
        session, "h_type_uni",
        label = "Height type:",
        choices = c(
          "Total height (htot, m)"      = "htot",
          "Dominant height (Hdom, m)"   = "hdom",
          "No height measurement"       = "none"
        )
      )
      updateActionButton(session, "calc_uni", label = "Compute (single tree)")
      updateCheckboxInput(session, "show_call_uni",
                          label = "Show remarks and warnings")
      
      # Batch
      updateRadioButtons(
        session, "sep_choice",
        label = "Column separator",
        choices = c(
          "Auto"           = "auto",
          "Semicolon ;"    = "; ",
          "Comma ,"        = ", ",
          "Tab"            = "tab"
        )
      )
      updateCheckboxGroupInput(
        session, "vol_type_batch",
        label = "Types of volumes / outputs to compute:",
        choices = c(
          "vc22 (merchantable volume, m³)"        = "vc22",
          "vtot (stem total volume, m³)"          = "vtot",
          "vta (total aboveground volume, m³)"    = "vta",
          "Branch volume Dagnelie (br, m³)"       = "br",
          "Biomass / C / CO₂ (t)"                 = "biomass"
        )
      )
      updateActionButton(session, "calc_batch",
                         label = "Compute (batch of trees)")
      
      # Plotage
      updateRadioButtons(
        session, "sep_choice_plot",
        label = "Column separator",
        choices = c(
          "Auto"           = "auto",
          "Semicolon ;"    = "; ",
          "Comma ,"        = ", ",
          "Tab"            = "tab"
        )
      )
      updateSelectInput(
        session, "volume_model_plot",
        label = "Volume equation to use:",
        choices = c(
          "vc22 combined (all available equations)" = "vc22_combined",
          "Dagnelie tariff 2 (dagnelie_vc22_2)"     = "dagnelie_vc22_2",
          "Dagnelie tariff 1 (dagnelie_vc22_1)"     = "dagnelie_vc22_1",
          "Vallet vc22 (vallet_vc22)"              = "vallet_vc22",
          "Rondeux vc22 (rondeux_vc22_vtot)"       = "rondeux_vc22",
          "Algan vc22 (algan_vta_vc22)"            = "algan_vc22"
        )
      )
      updateActionButton(session, "calc_plot",
                         label = "Compute and plot")
      
    } else {
      # FR
      updateSelectInput(session, "species_uni", label = "Essence :")
      updateCheckboxGroupInput(
        session, "vol_type_uni",
        label = HTML("Choisir le(s) type(s) de volume <span style='color:red;'>*</span> :"),
        choices = c(
          "Volume marchand vc22 (m³)"             = "vc22",
          "Volume total tige vtot (m³)"           = "vtot",
          "Volume total aérien vta (m³)"          = "vta",
          "Volume des branches Dagnelie (br, m³)" = "br"
        )
      )
      updateCheckboxInput(
        session, "biomass_uni",
        label = "Calculer aussi biomasse / C / CO₂ (t)"
      )
      updateSelectInput(
        session, "meas_type_uni",
        label = "Type de mesure de la tige (cm) :",
        choices = c(
          "Circonférence à 1.30 m (C130, cm)"        = "c130",
          "Circonférence à 1.50 m (C150, cm)"        = "c150",
          "Diamètre à 1.30 m (D130 / DBH, cm)"       = "d130"
        )
      )
      updateTextInput(
        session, "meas_value_uni",
        label = "Valeur(s) de mesure (cm) :",
        placeholder = "Ex. 100, 110, 125"
      )
      updateSelectInput(
        session, "h_type_uni",
        label = "Type de hauteur :",
        choices = c(
          "Hauteur totale (htot, m)"           = "htot",
          "Hauteur dominante (Hdom, m)"        = "hdom",
          "Sans mesure de hauteur"             = "none"
        )
      )
      updateActionButton(session, "calc_uni",
                         label = "Calculer (arbre unique)")
      updateCheckboxInput(session, "show_call_uni",
                          label = "Afficher remarques et avertissements")
      
      updateRadioButtons(
        session, "sep_choice",
        label = "Séparateur colonnes",
        choices = c(
          "Auto"            = "auto",
          "Point-virgule ;" = "; ",
          "Virgule ,"       = ", ",
          "Tabulation"      = "tab"
        )
      )
      updateCheckboxGroupInput(
        session, "vol_type_batch",
        label = "Types de volumes / résultats à calculer :",
        choices = c(
          "vc22 (volume marchand, m³)"                = "vc22",
          "vtot (volume total tige, m³)"              = "vtot",
          "vta (volume total aérien, m³)"             = "vta",
          "Volume des branches Dagnelie (br, m³)"     = "br",
          "Biomasse / C / CO₂ (t)"                    = "biomass"
        )
      )
      updateActionButton(session, "calc_batch",
                         label = "Calculer (lot d’arbres)")
      
      updateRadioButtons(
        session, "sep_choice_plot",
        label = "Séparateur colonnes",
        choices = c(
          "Auto"            = "auto",
          "Point-virgule ;" = "; ",
          "Virgule ,"       = ", ",
          "Tabulation"      = "tab"
        )
      )
      updateSelectInput(
        session, "volume_model_plot",
        label = "Équation de volume à utiliser :",
        choices = c(
          "vc22 combiné (toutes équations disponibles)" = "vc22_combined",
          "Dagnelie tarif 2 (dagnelie_vc22_2)"          = "dagnelie_vc22_2",
          "Dagnelie tarif 1 (dagnelie_vc22_1)"          = "dagnelie_vc22_1",
          "Vallet vc22 (vallet_vc22)"                   = "vallet_vc22",
          "Rondeux vc22 (rondeux_vc22_vtot)"            = "rondeux_vc22",
          "Algan vc22 (algan_vta_vc22)"                 = "algan_vc22"
        )
      )
      updateActionButton(session, "calc_plot",
                         label = "Calculer et tracer")
    }
  })
  
  ## Fonctions sûres GCubeR ----
  
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
    w_local <- character()
    biomass_cols <- c("cniefeb_dagnelie_bag",
                      "cniefeb_dagnelie_bbg",
                      "cniefeb_dagnelie_btot",
                      "cniefeb_dagnelie_c",
                      "cniefeb_dagnelie_co2")
    
    for (col in biomass_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA_real_
      }
    }
    
    if (is.null(density_table) || !("species_code" %in% names(df))) {
      old <- uni_warnings()
      uni_warnings(unique(c(
        old,
        "[biomass_calc] Table de densité absente ou species_code manquant ; biomasses mises à NA."
      )))
      return(df)
    }
    
    sp_all <- unique(df$species_code)
    sp_all <- sp_all[!is.na(sp_all)]
    
    for (sp in sp_all) {
      if (!sp %in% density_table$species_code) next
      
      idx <- which(df$species_code == sp)
      if (length(idx) == 0) next
      
      df_sub <- df[idx, , drop = FALSE]
      
      res_sub <- tryCatch(
        withCallingHandlers(
          GCubeR::biomass_calc(data = df_sub, na_action = "omit"),
          warning = function(w) {
            w_local <<- c(w_local,
                          paste0("Espèce ", sp, " : ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          w_local <<- c(
            w_local,
            paste0("Erreur dans biomass_calc pour l'espèce ", sp, " : ", conditionMessage(e))
          )
          df_sub
        }
      )
      
      for (col in biomass_cols) {
        if (col %in% names(res_sub)) {
          df[idx, col] <- res_sub[[col]]
        }
      }
    }
    
    if (length(w_local) > 0) {
      old <- uni_warnings()
      uni_warnings(unique(c(old, paste0("[biomass_calc] ", w_local))))
    }
    
    df
  }
  
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
  
  safe_run_biomass_batch <- function(df) {
    w_local <- character()
    biomass_cols <- c("cniefeb_dagnelie_bag",
                      "cniefeb_dagnelie_bbg",
                      "cniefeb_dagnelie_btot",
                      "cniefeb_dagnelie_c",
                      "cniefeb_dagnelie_co2")
    
    for (col in biomass_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA_real_
      }
    }
    
    if (is.null(density_table) || !("species_code" %in% names(df))) {
      old <- batch_warnings()
      batch_warnings(unique(c(
        old,
        "[biomass_calc] Table de densité absente ou species_code manquant ; biomasses mises à NA."
      )))
      return(df)
    }
    
    sp_all <- unique(df$species_code)
    sp_all <- sp_all[!is.na(sp_all)]
    
    for (sp in sp_all) {
      if (!sp %in% density_table$species_code) next
      
      idx <- which(df$species_code == sp)
      if (length(idx) == 0) next
      
      df_sub <- df[idx, , drop = FALSE]
      
      res_sub <- tryCatch(
        withCallingHandlers(
          GCubeR::biomass_calc(data = df_sub, na_action = "omit"),
          warning = function(w) {
            w_local <<- c(w_local,
                          paste0("Espèce ", sp, " : ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          w_local <<- c(
            w_local,
            paste0("Erreur dans biomass_calc pour l'espèce ", sp, " : ", conditionMessage(e))
          )
          df_sub
        }
      )
      
      for (col in biomass_cols) {
        if (col %in% names(res_sub)) {
          df[idx, col] <- res_sub[[col]]
        }
      }
    }
    
    if (length(w_local) > 0) {
      old <- batch_warnings()
      batch_warnings(unique(c(old, paste0("[biomass_calc] ", w_local))))
    }
    
    df
  }
  
  ## 4.1 Arbre unique ----
  
  output$h_value_ui <- renderUI({
    if (identical(input$h_type_uni, "none")) return(NULL)
    
    lang <- get_lang()
    label_txt <- if (identical(input$h_type_uni, "hdom")) {
      if (lang == "en") "Dominant height Hdom (m):" else "Hauteur dominante Hdom (m) :"
    } else {
      if (lang == "en") "Total height htot (m):" else "Hauteur totale htot (m) :"
    }
    
    textInput(
      "h_value_uni",
      label = label_txt,
      value = "",
      placeholder = if (lang == "en") "e.g. 20, 22, 25" else "Ex. 20, 22, 25"
    )
  })
  
  observeEvent(input$species_uni, {
    sp <- input$species_uni
    allowed_types <- get_allowed_volume_types_for_species(sp)
    
    volume_codes <- c("vc22", "vtot", "vta", "br")
    all_choices <- c(
      "Volume marchand vc22 (m³)"             = "vc22",
      "Volume total tige vtot (m³)"           = "vtot",
      "Volume total aérien vta (m³)"          = "vta",
      "Volume des branches Dagnelie (br, m³)" = "br"
    )
    
    allowed_codes <- intersect(allowed_types, volume_codes)
    if (length(allowed_codes) == 0) {
      allowed_codes <- volume_codes
    }
    
    allowed_choices <- all_choices[all_choices %in% allowed_codes]
    current <- input$vol_type_uni
    new_sel <- intersect(current, allowed_codes)
    if (length(new_sel) == 0) new_sel <- allowed_codes[1]
    
    updateCheckboxGroupInput(
      session, "vol_type_uni",
      choices  = allowed_choices,
      selected = new_sel
    )
    
    has_biomass <- "biomass" %in% allowed_types
    lang <- get_lang()
    if (isTRUE(has_biomass)) {
      if (lang == "en") {
        updateCheckboxInput(
          session, "biomass_uni",
          label = "Also compute biomass / C / CO₂ (t)"
        )
      } else {
        updateCheckboxInput(
          session, "biomass_uni",
          label = "Calculer aussi biomasse / C / CO₂ (t)"
        )
      }
    } else {
      if (lang == "en") {
        updateCheckboxInput(
          session, "biomass_uni",
          value = FALSE,
          label = "Biomass not available for this species"
        )
      } else {
        updateCheckboxInput(
          session, "biomass_uni",
          value = FALSE,
          label = "Biomasse non disponible pour cette essence"
        )
      }
    }
  }, ignoreInit = TRUE)
  
  calc_res_uni <- eventReactive(input$calc_uni, {
    uni_warnings(character())
    
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
    
    if ("br" %in% vol_types) {
      df <- safe_run_gcuber_uni(df, GCubeR::dagnelie_br, "dagnelie_br")
    }
    
    if (isTRUE(input$biomass_uni)) {
      df <- safe_run_biomass_uni(df)
    }
    
    base_cols <- c("species_code", "c130", "dbh", "c150", "htot", "hdom")
    vol_cols  <- intersect(
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
  
  output$mapping_ui <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    lang <- get_lang()
    
    if (lang == "en") {
      tagList(
        h4("Column mapping"),
        helpText(
          "Select the species column, the single diameter/circumference measurement, ",
          "and the height if available. Species must be encoded as species_code (PICEA_ABIES, ...) ",
          "or French name (Hêtre, ...). See the 'Species list' tab for possible values."
        ),
        selectInput(
          "col_species", "Species column:", choices = cols
        ),
        selectInput(
          "col_meas_value", "Diameter / circumference column (cm):", choices = cols
        ),
        selectInput(
          "meas_type_batch",
          "Measurement type (cm):",
          choices = c(
            "Circumference at 1.30 m (C130)"        = "c130",
            "Circumference at 1.50 m (C150)"        = "c150",
            "Diameter at 1.30 m (D130 / DBH)"       = "d130"
          ),
          selected = "c130"
        ),
        radioButtons(
          "h_mode_batch",
          "Height:",
          choices = c(
            "No height"                                        = "none",
            "Individual total height (htot, column)"           = "htot",
            "Individual dominant height (Hdom, column)"        = "hdom",
            "Htot and Hdom (2 columns)"                        = "htot_hdom"
          ),
          selected = "none"
        ),
        conditionalPanel(
          condition = "input.h_mode_batch == 'htot' || input.h_mode_batch == 'htot_hdom'",
          selectInput(
            "col_htot",
            "Total height htot column (m):",
            choices = cols
          )
        ),
        conditionalPanel(
          condition = "input.h_mode_batch == 'hdom' || input.h_mode_batch == 'htot_hdom'",
          selectInput(
            "col_hdom",
            "Dominant height Hdom column (m):",
            choices = cols
          )
        )
      )
    } else {
      tagList(
        h4("Mapping des colonnes"),
        helpText(
          "Sélectionne la colonne d’essence, la mesure unique de diamètre/circonférence, ",
          "et la hauteur si disponible. ",
          "L’essence doit être encodée en species_code (PICEA_ABIES, ...) ou en nom français (Hêtre, ...). ",
          "Voir l’onglet 'Liste des essences' pour les valeurs possibles."
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
            "Diamètre à 1.30 m (D130 / DBH)"       = "d130"
          ),
          selected = "c130"
        ),
        radioButtons(
          "h_mode_batch",
          "Hauteur :",
          choices = c(
            "Sans hauteur"                                   = "none",
            "Hauteur totale individuelle (htot, colonne)"    = "htot",
            "Hauteur dominante individuelle (Hdom, colonne)" = "hdom",
            "Htot et Hdom (2 colonnes)"                      = "htot_hdom"
          ),
          selected = "none"
        ),
        conditionalPanel(
          condition = "input.h_mode_batch == 'htot' || input.h_mode_batch == 'htot_hdom'",
          selectInput(
            "col_htot",
            "Colonne hauteur totale htot (m) :",
            choices = cols
          )
        ),
        conditionalPanel(
          condition = "input.h_mode_batch == 'hdom' || input.h_mode_batch == 'htot_hdom'",
          selectInput(
            "col_hdom",
            "Colonne hauteur dominante Hdom (m) :",
            choices = cols
          )
        )
      )
    }
  })
  
  calc_res_batch <- eventReactive(input$calc_batch, {
    batch_warnings(character())
    
    df_in <- dat_raw()
    validate(need(!is.null(input$col_species), "Sélectionne la colonne d’essence."))
    validate(need(!is.null(input$col_meas_value), "Sélectionne la colonne de mesure."))
    validate(need(!is.null(input$meas_type_batch), "Sélectionne le type de mesure."))
    
    base_df <- build_base_batch(
      df_in          = df_in,
      col_species    = input$col_species,
      col_meas_value = input$col_meas_value,
      meas_type_batch = input$meas_type_batch,
      h_mode_batch   = input$h_mode_batch,
      col_htot       = if (!is.null(input$col_htot)) input$col_htot else "",
      col_hdom       = if (!is.null(input$col_hdom)) input$col_hdom else "",
      hdom_value     = NA_real_
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
    
    if ("br" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_br,       "dagnelie_br")
    }
    
    if ("biomass" %in% vol_types) {
      df <- safe_run_biomass_batch(df)
    }
    
    vol_type_to_cols <- list(
      vc22 = c("dagnelie_vc22_1", "dagnelie_vc22_1g", "dagnelie_vc22_2",
               "vallet_vc22", "algan_vc22", "rondeux_vc22", "rondeux_vtot"),
      vta  = c("vallet_vta", "bouvard_vta", "algan_vta"),
      vtot = c("rondeux_vtot", "rondeux_vc22"),
      br   = c("dagnelie_br")
    )
    
    needed_vol_cols <- unique(unlist(vol_type_to_cols[intersect(names(vol_type_to_cols), vol_types)]))
    if (length(needed_vol_cols) > 0) {
      for (col in needed_vol_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- NA_real_
        }
      }
    }
    
    if ("biomass" %in% vol_types) {
      biomass_all <- c("cniefeb_dagnelie_bag",
                       "cniefeb_dagnelie_bbg",
                       "cniefeb_dagnelie_btot",
                       "cniefeb_dagnelie_c",
                       "cniefeb_dagnelie_co2")
      for (col in biomass_all) {
        if (!col %in% names(df)) {
          df[[col]] <- NA_real_
        }
      }
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
      tpl <- data.frame(
        species    = c("PICEA_ABIES", "Hêtre", "FAGUS_SYLVATICA"),
        meas_value = c(100, 120, 140),
        htot       = c(25, 22, NA),
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
  
  ## 4.3 Plotage volume par classes ----
  
  dat_plot_raw <- reactive({
    req(input$file_plot)
    ext <- tools::file_ext(input$file_plot$name)
    read_table_any(
      path       = input$file_plot$datapath,
      ext        = ext,
      sep_choice = input$sep_choice_plot
    )
  })
  
  output$mapping_plot_ui <- renderUI({
    req(dat_plot_raw())
    cols <- names(dat_plot_raw())
    lang <- get_lang()
    
    if (lang == "en") {
      tagList(
        h4("Column mapping for plotting"),
        helpText(
          "Select the species column, the diameter/circumference measurement (cm) ",
          "and the height if needed. Species must be in species_code or French name."
        ),
        selectInput(
          "col_species_plot", "Species column:", choices = cols
        ),
        selectInput(
          "col_meas_value_plot", "Diameter / circumference column (cm):", choices = cols
        ),
        selectInput(
          "meas_type_plot",
          "Measurement type (cm):",
          choices = c(
            "Circumference at 1.30 m (C130)"        = "c130",
            "Circumference at 1.50 m (C150)"        = "c150",
            "Diameter at 1.30 m (D130 / DBH)"       = "d130"
          ),
          selected = "c130"
        ),
        radioButtons(
          "h_mode_plot",
          "Height:",
          choices = c(
            "Equation without height (e.g. Dagnelie 1)" = "none",
            "Individual total height (htot, column)"     = "htot"
          ),
          selected = "none"
        ),
        conditionalPanel(
          condition = "input.h_mode_plot == 'htot'",
          selectInput(
            "col_htot_plot",
            "Total height htot column (m):",
            choices = cols
          )
        )
      )
    } else {
      tagList(
        h4("Mapping des colonnes pour le plotage"),
        helpText(
          "Sélectionne la colonne d’essence, la mesure de diamètre/circonférence (cm) ",
          "et la hauteur si nécessaire. L’essence doit être en species_code ou nom français."
        ),
        selectInput(
          "col_species_plot", "Colonne essence :", choices = cols
        ),
        selectInput(
          "col_meas_value_plot", "Colonne diamètre / circonférence (cm) :", choices = cols
        ),
        selectInput(
          "meas_type_plot",
          "Type de mesure (cm) :",
          choices = c(
            "Circonférence à 1.30 m (C130)"        = "c130",
            "Circonférence à 1.50 m (C150)"        = "c150",
            "Diamètre à 1.30 m (D130 / DBH)"       = "d130"
          ),
          selected = "c130"
        ),
        radioButtons(
          "h_mode_plot",
          "Hauteur :",
          choices = c(
            "Équation sans hauteur (ex. Dagnelie 1)"           = "none",
            "Hauteur totale individuelle (htot, colonne)"      = "htot"
          ),
          selected = "none"
        ),
        conditionalPanel(
          condition = "input.h_mode_plot == 'htot'",
          selectInput(
            "col_htot_plot",
            "Colonne hauteur totale htot (m) :",
            choices = cols
          )
        )
      )
    }
  })
  
  plot_res <- eventReactive(input$calc_plot, {
    df_in <- dat_plot_raw()
    
    validate(need(!is.null(input$col_species_plot), "Sélectionne la colonne d’essence."))
    validate(need(!is.null(input$col_meas_value_plot), "Sélectionne la colonne de mesure."))
    
    h_mode <- input$h_mode_plot
    col_ht <- if (!is.null(input$col_htot_plot)) input$col_htot_plot else ""
    hdom_val <- NA_real_
    
    base_df <- build_base_batch(
      df_in          = df_in,
      col_species    = input$col_species_plot,
      col_meas_value = input$col_meas_value_plot,
      meas_type_batch= input$meas_type_plot,
      h_mode_batch   = h_mode,
      col_htot       = col_ht,
      col_hdom       = "",
      hdom_value     = hdom_val
    )
    
    df <- base_df
    model <- input$volume_model_plot
    vol_col_name <- NULL
    
    if (model == "vc22_combined") {
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_1,   "dagnelie_vc22_1")
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_1g,  "dagnelie_vc22_1g")
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_2,   "dagnelie_vc22_2")
      df <- safe_run_gcuber_batch(df, GCubeR::vallet_vc22,       "vallet_vc22")
      df <- safe_run_gcuber_batch(df, GCubeR::algan_vta_vc22,    "algan_vta_vc22")
      df <- safe_run_gcuber_batch(df, GCubeR::rondeux_vc22_vtot, "rondeux_vc22_vtot")
      
      needed_cols <- c("dagnelie_vc22_2",
                       "dagnelie_vc22_1",
                       "vallet_vc22",
                       "rondeux_vc22",
                       "algan_vc22")
      for (col in needed_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- NA_real_
        }
      }
      
      df$vc22_combined <- NA_real_
      if (nrow(df) > 0) {
        for (i in seq_len(nrow(df))) {
          vals <- c(df$dagnelie_vc22_2[i],
                    df$dagnelie_vc22_1[i],
                    df$vallet_vc22[i],
                    df$rondeux_vc22[i],
                    df$algan_vc22[i])
          idx <- which(!is.na(vals))[1]
          if (!is.na(idx)) {
            df$vc22_combined[i] <- vals[idx]
          }
        }
      }
      vol_col_name <- "vc22_combined"
      
    } else if (model == "dagnelie_vc22_2") {
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_2, "dagnelie_vc22_2")
      vol_col_name <- "dagnelie_vc22_2"
    } else if (model == "dagnelie_vc22_1") {
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_vc22_1, "dagnelie_vc22_1")
      vol_col_name <- "dagnelie_vc22_1"
    } else if (model == "vallet_vc22") {
      df <- safe_run_gcuber_batch(df, GCubeR::vallet_vc22, "vallet_vc22")
      vol_col_name <- "vallet_vc22"
    } else if (model == "rondeux_vc22") {
      df <- safe_run_gcuber_batch(df, GCubeR::rondeux_vc22_vtot, "rondeux_vc22_vtot")
      vol_col_name <- "rondeux_vc22"
    } else if (model == "algan_vc22") {
      df <- safe_run_gcuber_batch(df, GCubeR::algan_vta_vc22, "algan_vta_vc22")
      vol_col_name <- "algan_vc22"
    }
    
    validate(need(!is.null(vol_col_name) && vol_col_name %in% names(df),
                  "La colonne de volume attendue n’a pas pu être produite. Vérifie le modèle et les variables d’entrée."))
    
    res <- GCubeR::plotage_volume_by_class(
      data       = df,
      volume_col = vol_col_name
    )
    
    list(
      table = res$table,
      plot  = res$plot
    )
  }, ignoreInit = TRUE)
  
  output$plot_table <- renderTable({
    req(plot_res())
    plot_res()$table
  })
  
  output$plot_volume <- renderPlot({
    req(plot_res())
    print(plot_res()$plot)
  })
  
  output$dl_plot_csv <- downloadHandler(
    filename = function() {
      paste0("table_volumes_par_classes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(plot_res())
      write.csv(
        plot_res()$table,
        file,
        row.names   = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
  
  output$dl_plot_png <- downloadHandler(
    filename = function() {
      paste0("hist_volumes_par_classes_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(plot_res())
      png(file, width = 1400, height = 900, res = 150)
      print(plot_res()$plot)
      dev.off()
    }
  )
  
  ## 4.4 Liste des essences ----
  
  output$species_table <- renderTable({
    df <- species_lookup
    df <- df[!is.na(df$species_code) & nzchar(df$species_code), ]
    df_out <- data.frame(
      species_code = df$species_code,
      nom_francais = df$species_name_fr,
      nom_latin    = df$species_name_lat,
      stringsAsFactors = FALSE
    )
    df_out <- unique(df_out)
    df_out[order(df_out$nom_francais, df_out$species_code), ]
  })
  
  ## 4.5 Documentation ----
  
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
