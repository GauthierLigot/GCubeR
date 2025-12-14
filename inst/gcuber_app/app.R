##############################################################
# Application Shiny - Tarifs GCubeR (intégrée)
# Auteur : Timon LUIZI (base) + intégrations UI/onglets demandées
# Intégrations :
#  - Patte graphique de l'app 1 (à l'identique : CSS + header + navbar + filigrane)
#  - Onglet "About GCubeR" de l'app 1
#  - Onglet "Volume by classes" (plot + table + exports) de l'app 1
# DATE last modif : 14-12-2025
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

if (!requireNamespace("shinycssloaders", quietly = TRUE)) install.packages("shinycssloaders")
library(shinycssloaders)

library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

##############################################################
# 1) - OUTILS DE TABLE (app 1) ----
##############################################################

gembloux_kable <- function(x, caption = NULL, digits = 3) {
  last_col <- ncol(x)
  
  pal_dark   <- "#004B87"  # dark blue
  pal_light  <- "#e8f5e9"  # light green
  pal_accent <- "#2e7d32"  # mid green
  
  x %>%
    kable(format = "html", digits = digits, caption = caption) %>%
    kable_styling(
      full_width        = TRUE,
      bootstrap_options = c("striped", "hover", "condensed")
    ) %>%
    row_spec(0, background = pal_dark, color = "white", bold = TRUE) %>%
    column_spec(1, background = pal_light, bold = TRUE) %>%
    column_spec(last_col, background = pal_accent, color = "white", bold = TRUE)
}

is_numeric_col <- function(df, col) {
  is.numeric(df[[col]]) || is.integer(df[[col]])
}

##############################################################
# 2) - DONNÉES DE RÉFÉRENCE (app 2) ----
##############################################################

## 2.1 - Chargement des métadonnées d'équations ----
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

## 2.2 - Construction de la liste d’essences ----
species_ref <- unique(models_doc[, c("species_code", "species_name_fr")])
species_ref$species_name_fr[is.na(species_ref$species_name_fr)] <- ""

species_ref <- species_ref[
  !is.na(species_ref$species_code) & nzchar(species_ref$species_code),
]

species_ref$species_name_lat <- vapply(
  species_ref$species_code,
  function(sc) {
    sc2 <- gsub("_", " ", tolower(sc))
    paste(toupper(substr(sc2, 1, 1)), substr(sc2, 2, nchar(sc2)), sep = "")
  },
  FUN.VALUE = character(1)
)

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

## 2.3 - Table de codes numériques (Dagnelie) ----
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

##############################################################
# 3) - FONCTIONS UTILITAIRES (app 2) ----
##############################################################

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

get_allowed_volume_types_for_species <- function(sp_code) {
  types <- character(0)
  if (is.null(models_doc) || is.na(sp_code)) return(character(0))
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
  if (length(intersect(eq_ids, c("rondeux_vtot"))) > 0) {
    types <- c(types, "vtot")
  }
  if (length(intersect(eq_ids, c("dagnelie_br","vc22br"))) > 0) {
    types <- c(types, "br")
  }
  
  if (!is.null(density_table) && sp_code %in% density_table$species_code) {
    types <- c(types, "biomass")
  }
  
  unique(types)
}

##############################################################
# 4) - UI (patte graphique + onglets demandés) ----
##############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # ==== CSS GLOBAL (identique app 1) ====
  tags$head(
    tags$style(HTML("
      /* Table scrollable */
      .scroll-table {
        overflow-x: auto;
        max-width: 100%;
      }
      .scroll-table table {
        width: auto !important;
        max-width: none !important;
      }

      /* Boutons d'export alignés */
      .export-buttons {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-bottom: 10px;
      }
      .export-buttons .btn {
        padding: 6px 12px;
        font-size: 13px;
        border-radius: 6px;
      }

      /* ===== Fond général et logo filigrane ===== */
      body {
        background-color: #f4f5f7;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      body:before {
        content: '';
        position: fixed;
        top: 50%;
        left: 50%;
        width: 1100px;
        height: 1100px;
        transform: translate(-50%, -50%);
        background-image: url('logo.png');
        background-repeat: no-repeat;
        background-position: center center;
        background-size: contain;
        opacity: 0.50;
        pointer-events: none;
        z-index: -1;
      }

      .container-fluid {
        background-color: transparent;
        padding-bottom: 30px;
      }
      .well {
        background-color: rgba(255, 255, 255, 0.92);
        border-radius: 10px;
        border: 1px solid #dde2eb;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }
      .tab-content {
        background-color: rgba(255, 255, 255, 0.90);
        border-radius: 10px;
        padding: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
      }

      .navbar {
        background-color: #003366 !important;
        border-radius: 0 !important;
        border: none;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #ffffff !important;
        font-weight: 500;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #00509e !important;
        color: #ffffff !important;
      }

      h2, h3 {
        color: #003366;
        font-weight: 600;
      }
      h4 {
        color: #00509e;
        font-weight: 600;
      }

      .btn-primary {
        background-color: #0070b8;
        border-color: #0063a1;
      }
      .btn-primary:hover, .btn-primary:focus {
        background-color: #0086da;
        border-color: #0070b8;
      }

      .help-block {
        font-size: 0.9em;
        color: #58657a;
      }
    "))
  ),
  
  # ==== HEADER (identique app 1) ====
  div(
    style = "
      background-color:#003366;
      color:white;
      padding:15px;
      font-size:28px;
      font-weight:bold;
      margin-bottom:20px;
      display:flex;
      align-items:center;
    ",
    img(src = "logo.png", height = "50px", style = "margin-right:15px; border-radius:50%;"),
    "GCubeR : Tree volume calculator"
  ),
  
  navbarPage(
    title = "GCubeR",
    
    #########################################################
    # TAB ABOUT (app 1)
    #########################################################
    tabPanel(
      "About GCubeR",
      fluidRow(
        column(
          width = 10, offset = 1,
          br(),
          h2("GCubeR: Tree Volume and Biomass Calculator"),
          p(
            "GCubeR provides a unified implementation of several published models ",
            "for stem volume and biomass estimation in temperate forests. ",
            "This application lets you use these models without writing any R code."
          ),
          hr(),
          h3("What can GCubeR compute?"),
          tags$ul(
            tags$li(strong("VC22 : "), "merchandising stem volume up to circumference 22 cm."),
            tags$li(strong("VTA : "), "total above-ground stem volume."),
            tags$li(strong("VTOT : "), "total bole volume for some methods (e.g. Rondeux)."),
            tags$li(strong("Biomass / Carbon / CO2 : "),
                    "derived from volume via CNIEFEB and Vallet approaches.")
          ),
          hr(),
          h3("Main families of methods"),
          
          p(
            "Several families of allometric equations are implemented in GCubeR. ",
            "While all functions return valid outputs when their conditions are met, ",
            strong("two model families are generally recommended for routine use"),
            "because they cover a broader range of species and provide robust estimates."
          ),
          
          tags$ul(
            tags$li(
              tags$b("Dagnelie models: "),
              "recommended for general use because they cover a ",
              em("very wide range of temperate tree species"),
              "and rely on easily measured variables (c130, optionally hdom or htot). ",
              "They are non-parametric equations derived from empirical volume tables. ",
              "Best choice when working with diverse forests or mixed inventories."
            ),
            
            tags$li(
              tags$b("Vallet models: "),
              "recommended especially for ",
              em("conifers and species with predictable stem form"),
              ", as they rely on statistical regression and geometric adjustment. ",
              "Available for several broadleaf species as well but most robust for conifers. ",
              "Suitable when dbh and htot are measured accurately."
            ),
            
            tags$li(
              tags$b("Rondeux: "),
              "a parametric model designed specifically for ",
              strong("LARIX_SP"),
              " in southern Belgium. ",
              "Computes both VC22 and VTA. ",
              "Useful in research or operations where larch is a primary species, ",
              "but not applicable outside that domain."
            ),
            
            tags$li(
              tags$b("Bouvard: "),
              "a simplified geometric model for ",
              strong("QUERCUS_SP in coppice-with-standards systems"),
              ". ",
              "Historically intended as a quick field method. ",
              "Best considered as a niche or legacy model rather than a general-purpose estimator."
            ),
            
            tags$li(
              tags$b("Algan: "),
              "a simplified approach for estimating ",
              em("both VC22 and VTA"),
              " based on dbh and height. ",
              "Valid for ",
              strong("ABIES_ALBA (VTA)"),
              "and a small set of species for VC22. ",
              "Best used as a simple, species-specific alternative rather than a universal model."
            )
          ),
          
          br(),
          p(
            strong("Summary: "),
            "Use ",
            strong("Dagnelie"),
            "when you need wide species coverage and simple inputs; ",
            strong("Vallet"),
            "when working with conifers or when dbh and height are reliably measured. ",
            "Other models are valid but cover narrower ecological or historical contexts."
          ),
          
          div(
            style = "text-align:center; margin-top: 20px;",
            tags$a(
              href = "introduction.html",
              target = "_blank",
              class = "btn btn-primary",
              style = "font-size: 16px; padding: 10px 20px;",
              "📄 Open full documentation / vignette"
            )
          ),
          
          br()
        )
      )
    ),
    
    ##########################################################
    # INTRODUCTION (app 2)
    ##########################################################
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          width = 10, offset = 1,
          br(),
          h2("GCubeR : calcul de volumes et biomasses d’arbres"),
          p(
            "GCubeR implémente de manière homogène plusieurs tarifs de cubage et modèles de biomasse ",
            "publiés pour les forêts tempérées. Cette application Shiny permet d’utiliser ces modèles ",
            "sans écrire de code R, en se concentrant sur le choix des essences et des variables mesurées."
          ),
          hr(),
          h3("Fonctionnalités principales"),
          tags$ul(
            tags$li(strong("Arbre unique : "), "calcul des volumes (vc22, vtot, vta, branches) et, si disponible, biomasse / carbone / CO₂."),
            tags$li(strong("Jeu de données : "), "import .xlsx/.csv, normalisation essences/mesures, calcul en lot."),
            tags$li(strong("Plotage volumes par classes : "), "agrégation par classes de c130, visualisation et export."),
            tags$li(strong("Liste des essences : "), "rappel des codes species_code et noms FR/latins."),
            tags$li(strong("Documentation des équations : "), "métadonnées (famille, variable, validité, etc.).")
          ),
          hr(),
          h3("Schéma (optionnel)"),
          p("Si vous utilisez un schéma, placez-le dans www/function_diag.png."),
          div(
            style = "text-align:center; margin-bottom: 20px;",
            img(
              src   = "function_diag.png",
              style = "max-width: 100%; height: auto;",
              alt   = "Schéma de synthèse GCubeR"
            )
          ),
          br()
        )
      )
    ),
    
    ##########################################################
    # ARBRE UNIQUE (app 2)
    ##########################################################
    tabPanel(
      "Arbre unique",
      sidebarLayout(
        sidebarPanel(
          helpText("Saisis un ou plusieurs arbres. Pour plusieurs arbres, utilise des virgules : ex. 100,110,125."),
          
          selectInput(
            "species_uni",
            label   = "Essence :",
            choices = species_choices_gcuber,
            selected = NULL,
            multiple = FALSE
          ),
          
          checkboxGroupInput(
            "vol_type_uni",
            label = HTML("Choisir le(s) type(s) de volume <span style='color:red;'>*</span> :"),
            choices = c(
              "Volume marchand vc22 (m³)"             = "vc22",
              "Volume total tige vtot (m³)"           = "vtot",
              "Volume total aérien vta (m³)"          = "vta",
              "Volume des branches Dagnelie (br, m³)" = "br"
            ),
            selected = "vc22"
          ),
          
          checkboxInput(
            "biomass_uni",
            label = "Calculer aussi biomasse / C / CO₂ (t)",
            value = FALSE
          ),
          
          helpText("* Tous les types ne sont pas disponibles pour toutes les essences."),
          
          selectInput(
            "meas_type_uni",
            label = "Type de mesure de la tige (cm) :",
            choices = c(
              "Circonférence à 1.30 m (C130, cm)"  = "c130",
              "Circonférence à 1.50 m (C150, cm)"  = "c150",
              "Diamètre à 1.30 m (D130 / DBH, cm)" = "d130"
            ),
            selected = "c130"
          ),
          
          textInput(
            "meas_value_uni",
            label = "Valeur(s) de mesure (cm) :",
            value = "100",
            placeholder = "Ex. 100, 110, 125"
          ),
          
          selectInput(
            "h_type_uni",
            label = "Type de hauteur :",
            choices = c(
              "Hauteur totale (htot, m)"    = "htot",
              "Hauteur dominante (Hdom, m)" = "hdom",
              "Sans mesure de hauteur"      = "none"
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
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(htmlOutput("result_table_uni"))
          ),
          
          conditionalPanel(
            condition = "input.show_call_uni == true",
            h4("Remarques et avertissements (équations GCubeR)"),
            verbatimTextOutput("appel_uni")
          ),
          
          hr(),
          h4("Équations disponibles pour l’essence sélectionnée"),
          p("Clique sur une ligne ci-dessous pour afficher le détail de l’équation."),
          uiOutput("models_expl_uni"),
          br(), br()
        )
      )
    ),
    
    ##########################################################
    # JEU DE DONNÉES (app 2)
    ##########################################################
    tabPanel(
      "Jeu de données",
      sidebarLayout(
        sidebarPanel(
          fileInput(
            "file_up",
            label = "Fichier (.xlsx ou .csv)",
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
              "vc22 (volume marchand, m³)"            = "vc22",
              "vtot (volume total tige, m³)"          = "vtot",
              "vta (volume total aérien, m³)"         = "vta",
              "Volume des branches Dagnelie (br, m³)" = "br",
              "Biomasse / C / CO₂ (t)"                = "biomass"
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
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(htmlOutput("preview"))
          ),
          hr(),
          h3("Résultats – Lot d’arbres"),
          div(
            class = "scroll-table",
            shinycssloaders::withSpinner(htmlOutput("result_table_batch"))
          ),
          verbatimTextOutput("result_msg_batch"),
          h4("Remarques et avertissements (lot d’arbres)"),
          verbatimTextOutput("appel_batch")
        )
      )
    ),
    
    ##########################################################
    # PLOT : VERSION APP 1 (demandée)
    ##########################################################
    tabPanel(
      "Volume by classes",
      sidebarLayout(
        sidebarPanel(
          h4("File with precomputed volumes"),
          fileInput(
            "file_plot",
            "Import a CSV/XLSX (separator auto)",
            accept = c(".csv", ".CSV", ".xlsx", ".XLSX")
          ),
          helpText(
            "The file must contain at least the columns ",
            code("c130"), ", ", code("species_code"),
            " and one volume column (e.g. dagnelie_vc22_1)."
          ),
          hr(),
          uiOutput("plot_volume_col_ui"),
          hr(),
          sliderInput(
            "class_width",
            "Class width for c130 (cm):",
            min = 5, max = 50, value = 10, step = 5
          ),
          sliderInput(
            "limit_small_medium",
            "Small / medium threshold (c130, cm):",
            min = 10, max = 200, value = 60, step = 5
          ),
          sliderInput(
            "limit_medium_large",
            "Medium / large threshold (c130, cm):",
            min = 10, max = 200, value = 100, step = 5
          )
        ),
        mainPanel(
          h3("Histogram of volume by c130 classes"),
          wellPanel(
            plotOutput("volume_plot", height = "420px")
          ),
          hr(),
          h3("Export"),
          div(
            class = "export-buttons",
            downloadButton("dl_plot_csv", "Table (CSV)"),
            downloadButton("dl_plot_png", "Plot (PNG)")
          ),
          hr(),
          h3("Aggregated table"),
          wellPanel(
            htmlOutput("plot_table")
          )
        )
      )
    ),
    
    ##########################################################
    # LISTE DES ESSENCES (app 2)
    ##########################################################
    tabPanel(
      "Liste des essences",
      fluidRow(
        column(
          12,
          h3("Liste des essences utilisées par GCubeR"),
          p("La colonne species_code est celle à utiliser. Les noms français sont ceux reconnus par la normalisation."),
          div(
            class = "scroll-table",
            htmlOutput("species_table")
          )
        )
      )
    ),
    
    ##########################################################
    # DOCUMENTATION DES ÉQUATIONS (app 2)
    ##########################################################
    tabPanel(
      "Documentation des équations",
      fluidRow(
        column(
          12,
          h3("Équations et modèles disponibles dans GCubeR"),
          p("Ce tableau reprend les métadonnées des équations (famille, variable prédite, espèces, domaine de validité, variables d’entrée, source)."),
          div(
            class = "scroll-table",
            htmlOutput("models_table")
          )
        )
      )
    )
  )
)

##############################################################
# 5) - SERVER ----
##############################################################

server <- function(input, output, session) {
  
  uni_warnings   <- reactiveVal(character())
  batch_warnings <- reactiveVal(character())
  
  ## ---- UI hauteur (arbre unique)
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
  
  ## ---- Filtrage dynamique des types de volumes selon essence
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
    if (length(allowed_codes) == 0) allowed_codes <- volume_codes
    
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
    if (isTRUE(has_biomass)) {
      updateCheckboxInput(
        session, "biomass_uni",
        label = "Calculer aussi biomasse / C / CO₂ (t)"
      )
    } else {
      updateCheckboxInput(
        session, "biomass_uni",
        value = FALSE,
        label = "Biomasse non disponible pour cette essence"
      )
    }
  }, ignoreInit = TRUE)
  
  ## ---- Safe wrappers (captures warnings)
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
        w_local <<- c(w_local, paste0("Erreur dans ", fun_label, " : ", conditionMessage(e)))
        df
      }
    )
    if (length(w_local) > 0) {
      old <- uni_warnings()
      uni_warnings(unique(c(old, paste0("[", fun_label, "] ", w_local))))
    }
    res
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
        w_local <<- c(w_local, paste0("Erreur dans ", fun_label, " : ", conditionMessage(e)))
        df
      }
    )
    if (length(w_local) > 0) {
      old <- batch_warnings()
      batch_warnings(unique(c(old, paste0("[", fun_label, "] ", w_local))))
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
    
    for (col in biomass_cols) if (!col %in% names(df)) df[[col]] <- NA_real_
    
    if (is.null(density_table) || !("species_code" %in% names(df))) {
      old <- uni_warnings()
      uni_warnings(unique(c(old, "[biomass_calc] Table de densité absente ou species_code manquant ; biomasses mises à NA.")))
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
            w_local <<- c(w_local, paste0("Espèce ", sp, " : ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          w_local <<- c(w_local, paste0("Erreur dans biomass_calc pour l'espèce ", sp, " : ", conditionMessage(e)))
          df_sub
        }
      )
      
      for (col in biomass_cols) if (col %in% names(res_sub)) df[idx, col] <- res_sub[[col]]
    }
    
    if (length(w_local) > 0) {
      old <- uni_warnings()
      uni_warnings(unique(c(old, paste0("[biomass_calc] ", w_local))))
    }
    df
  }
  
  safe_run_biomass_batch <- function(df) {
    w_local <- character()
    biomass_cols <- c("cniefeb_dagnelie_bag",
                      "cniefeb_dagnelie_bbg",
                      "cniefeb_dagnelie_btot",
                      "cniefeb_dagnelie_c",
                      "cniefeb_dagnelie_co2")
    
    for (col in biomass_cols) if (!col %in% names(df)) df[[col]] <- NA_real_
    
    if (is.null(density_table) || !("species_code" %in% names(df))) {
      old <- batch_warnings()
      batch_warnings(unique(c(old, "[biomass_calc] Table de densité absente ou species_code manquant ; biomasses mises à NA.")))
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
            w_local <<- c(w_local, paste0("Espèce ", sp, " : ", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          w_local <<- c(w_local, paste0("Erreur dans biomass_calc pour l'espèce ", sp, " : ", conditionMessage(e)))
          df_sub
        }
      )
      
      for (col in biomass_cols) if (col %in% names(res_sub)) df[idx, col] <- res_sub[[col]]
    }
    
    if (length(w_local) > 0) {
      old <- batch_warnings()
      batch_warnings(unique(c(old, paste0("[biomass_calc] ", w_local))))
    }
    df
  }
  
  ############################################################
  # 5.1 ARBRE UNIQUE
  ############################################################
  
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
  
  output$result_table_uni <- renderUI({
    req(calc_res_uni())
    HTML(as.character(gembloux_kable(calc_res_uni(), caption = "Résultats (arbre(s) unique(s))")))
  })
  
  output$result_msg_uni <- renderPrint({
    req(calc_res_uni())
    df <- calc_res_uni()
    cat("Nombre d’arbres traités :", nrow(df), "\n")
    cat("Les volumes et biomasses affichés proviennent directement des fonctions de GCubeR.\n")
    cat("Les avertissements de domaine de validité sont capturés et affichés ci-dessous.\n")
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
      "Chaque fonction applique ses propres contrôles de validité (espèces supportées, plages de c130/dbh/hauteur).\n\n"
    )
    w <- uni_warnings()
    if (length(w) == 0) {
      cat("Aucun warning n’a été remonté par les fonctions GCubeR pour ce calcul.\n")
    } else {
      cat("Avertissements remontés par GCubeR :\n")
      for (msg in w) cat(" - ", msg, "\n", sep = "")
    }
  })
  
  output$models_expl_uni <- renderUI({
    req(input$species_uni)
    sp <- input$species_uni
    
    md <- subset(models_doc, species_code == sp)
    if (nrow(md) == 0) {
      return(tags$p("Aucune métadonnée d’équation trouvée pour cette essence dans equations_GCubeR."))
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
            strong("Variable prédite : "), m1$predicted_variable, " [", m1$output_unit, "]", br(),
            strong("Espèce : "), m1$species_name_fr, " (", m1$species_code, ")", br(),
            strong("Région de validité : "), m1$validity_region, br(),
            strong("Plage de validité (texte) : "), m1$validity_range, br(),
            strong("Variables d’entrée : "), m1$input_variable, " [", m1$input_unit, "]", br(),
            strong("Type de formule : "), m1$formula_type, br(),
            strong("Référence : "), m1$reference_source
          )
        )
      })
    )
  })
  
  ############################################################
  # 5.2 JEU DE DONNÉES
  ############################################################
  
  dat_raw <- reactive({
    req(input$file_up)
    ext <- tools::file_ext(input$file_up$name)
    read_table_any(
      path       = input$file_up$datapath,
      ext        = ext,
      sep_choice = input$sep_choice
    )
  })
  
  output$preview <- renderUI({
    req(dat_raw())
    HTML(as.character(gembloux_kable(head(dat_raw(), 10), caption = "Aperçu (10 premières lignes)")))
  })
  
  output$mapping_ui <- renderUI({
    req(dat_raw())
    cols <- names(dat_raw())
    
    tagList(
      h4("Mapping des colonnes"),
      helpText(
        "Sélectionne la colonne d’essence, la mesure unique de diamètre/circonférence, et la hauteur si disponible. ",
        "Essence en species_code (PICEA_ABIES, ...) ou en nom français (Hêtre, ...)."
      ),
      selectInput("col_species", "Colonne essence :", choices = cols),
      selectInput("col_meas_value", "Colonne diamètre / circonférence (cm) :", choices = cols),
      selectInput(
        "meas_type_batch",
        "Type de mesure (cm) :",
        choices = c(
          "Circonférence à 1.30 m (C130)"  = "c130",
          "Circonférence à 1.50 m (C150)"  = "c150",
          "Diamètre à 1.30 m (D130 / DBH)" = "d130"
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
        selectInput("col_htot", "Colonne hauteur totale htot (m) :", choices = cols)
      ),
      conditionalPanel(
        condition = "input.h_mode_batch == 'hdom' || input.h_mode_batch == 'htot_hdom'",
        selectInput("col_hdom", "Colonne hauteur dominante Hdom (m) :", choices = cols)
      )
    )
  })
  
  calc_res_batch <- eventReactive(input$calc_batch, {
    batch_warnings(character())
    
    df_in <- dat_raw()
    validate(need(!is.null(input$col_species), "Sélectionne la colonne d’essence."))
    validate(need(!is.null(input$col_meas_value), "Sélectionne la colonne de mesure."))
    validate(need(!is.null(input$meas_type_batch), "Sélectionne le type de mesure."))
    
    base_df <- build_base_batch(
      df_in           = df_in,
      col_species     = input$col_species,
      col_meas_value  = input$col_meas_value,
      meas_type_batch = input$meas_type_batch,
      h_mode_batch    = input$h_mode_batch,
      col_htot        = if (!is.null(input$col_htot)) input$col_htot else "",
      col_hdom        = if (!is.null(input$col_hdom)) input$col_hdom else "",
      hdom_value      = NA_real_
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
      df <- safe_run_gcuber_batch(df, GCubeR::vallet_vta,     "vallet_vta")
      df <- safe_run_gcuber_batch(df, GCubeR::bouvard_vta,    "bouvard_vta")
      df <- safe_run_gcuber_batch(df, GCubeR::algan_vta_vc22, "algan_vta_vc22")
    }
    
    if ("vtot" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::rondeux_vc22_vtot, "rondeux_vc22_vtot")
    }
    
    if ("br" %in% vol_types) {
      df <- safe_run_gcuber_batch(df, GCubeR::dagnelie_br, "dagnelie_br")
    }
    
    if ("biomass" %in% vol_types) {
      df <- safe_run_biomass_batch(df)
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
  
  output$result_table_batch <- renderUI({
    req(calc_res_batch())
    HTML(as.character(gembloux_kable(head(calc_res_batch(), 50), caption = "Résultats (aperçu : 50 premières lignes)")))
  })
  
  output$result_msg_batch <- renderPrint({
    req(calc_res_batch())
    df <- calc_res_batch()
    cat("Nombre de lignes dans le fichier importé :", nrow(dat_raw()), "\n")
    cat("Nombre de lignes dans le tableau de résultats :", nrow(df), "\n")
    cat("Les avertissements sont capturés et repris ci-dessous.\n")
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
      "- Biomasse / carbone : biomass_calc()\n\n"
    )
    w <- batch_warnings()
    if (length(w) == 0) {
      cat("Aucun warning n’a été remonté par les fonctions GCubeR pour ce lot.\n")
    } else {
      cat("Avertissements remontés par GCubeR (lot) :\n")
      for (msg in w) cat(" - ", msg, "\n", sep = "")
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
  
  ############################################################
  # 5.3 PLOT (APP 1)
  ############################################################
  
  dat_plot <- reactive({
    req(input$file_plot)
    ext <- tools::file_ext(input$file_plot$name)
    # lecture auto (csv/xlsx)
    read_table_any(
      path       = input$file_plot$datapath,
      ext        = ext,
      sep_choice = "auto"
    )
  })
  
  output$plot_volume_col_ui <- renderUI({
    df <- dat_plot()
    
    numeric_cols <- names(df)[
      vapply(names(df), function(nm) is_numeric_col(df, nm), logical(1))
    ]
    
    if (length(numeric_cols) == 0) {
      return(helpText("No numeric column found in the file."))
    }
    
    structural <- c("c130","c150","dbh","htot","hdom")
    volume_candidates <- setdiff(numeric_cols, structural)
    if (length(volume_candidates) == 0) volume_candidates <- numeric_cols
    
    selected_col <- if ("dagnelie_vc22_1" %in% volume_candidates) "dagnelie_vc22_1" else volume_candidates[1]
    
    selectInput(
      "plot_volume_col",
      "Volume column:",
      choices  = volume_candidates,
      selected = selected_col
    )
  })
  
  plot_data <- reactive({
    req(dat_plot())
    req(input$plot_volume_col)
    req(input$class_width)
    
    df <- dat_plot()
    
    validate(
      need("c130" %in% names(df), "Column 'c130' is missing from the file."),
      need("species_code" %in% names(df), "Column 'species_code' is missing from the file."),
      need(input$plot_volume_col %in% names(df),
           paste0("Volume column '", input$plot_volume_col, "' is missing."))
    )
    
    vol_col <- df[[input$plot_volume_col]]
    cw      <- input$class_width
    
    df2 <- df %>%
      mutate(
        c130   = as.numeric(c130),
        volume = as.numeric(vol_col)
      ) %>%
      filter(!is.na(c130), !is.na(volume))
    
    if (nrow(df2) == 0) {
      validate(need(FALSE, "No usable values (c130 and volume) in the file."))
    }
    
    df2 <- df2 %>%
      mutate(
        class_lower = floor(c130 / cw) * cw,
        class_upper = class_lower + cw,
        class_label = paste0("[", class_lower, ";", class_upper, ")")
      )
    
    agg <- df2 %>%
      group_by(class_lower, class_label, species_code) %>%
      summarise(volume = sum(volume, na.rm = TRUE), .groups = "drop")
    
    class_levels <- agg %>%
      dplyr::distinct(class_label, class_lower) %>%
      dplyr::arrange(class_lower) %>%
      dplyr::pull(class_label)
    
    agg <- agg %>%
      mutate(class_label = factor(class_label, levels = class_levels))
    
    tab_mat <- xtabs(volume ~ class_label + species_code, data = agg)
    tab_df  <- as.data.frame.matrix(tab_mat)
    tab_df$Total <- rowSums(tab_df)
    tab_df <- cbind(Class = rownames(tab_df), tab_df)
    rownames(tab_df) <- NULL
    
    list(
      agg    = agg,
      table  = tab_df,
      cw     = cw,
      limits = c(input$limit_small_medium, input$limit_medium_large)
    )
  })
  
  volume_plot_obj <- reactive({
    pd     <- plot_data()
    agg    <- pd$agg
    cw     <- pd$cw
    limits <- pd$limits
    
    agg <- agg %>%
      mutate(class_mid = class_lower + cw / 2)
    
    ggplot(agg, aes(x = class_mid, y = volume, fill = species_code)) +
      geom_col(
        width    = cw,
        position = "stack",
        color    = "grey20",
        size     = 0.3
      ) +
      geom_vline(xintercept = limits, linetype = "dashed") +
      labs(
        x     = "c130 (cm) - classes",
        y     = "Total volume (m³ or dm³)",
        fill  = "Species",
        title = "Volume distribution by c130 classes"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16, color = "#003366")
      )
  })
  
  output$volume_plot <- renderPlot({
    volume_plot_obj()
  })
  
  output$plot_table <- renderUI({
    pd <- plot_data()
    req(pd$table)
    
    div(
      class = "scroll-table",
      HTML(as.character(
        gembloux_kable(
          pd$table,
          caption = "Volume by c130 classes and species"
        )
      ))
    )
  })
  
  output$dl_plot_csv <- downloadHandler(
    filename = function() {
      paste0("volume_by_classes_", Sys.Date(), ".csv")
    },
    content = function(file) {
      pd  <- plot_data()
      tab <- pd$table
      write.csv2(tab, file, row.names = FALSE)
    }
  )
  
  output$dl_plot_png <- downloadHandler(
    filename = function() {
      paste0("volume_by_classes_", Sys.Date(), ".png")
    },
    content = function(file) {
      plt <- volume_plot_obj()
      png(file, width = 1400, height = 900, res = 150)
      print(plt)
      dev.off()
    }
  )
  
  ############################################################
  # 5.4 LISTE DES ESSENCES
  ############################################################
  
  output$species_table <- renderUI({
    df <- species_lookup
    df <- df[!is.na(df$species_code) & nzchar(df$species_code), ]
    df_out <- data.frame(
      species_code = df$species_code,
      nom_francais = df$species_name_fr,
      nom_latin    = df$species_name_lat,
      stringsAsFactors = FALSE
    )
    df_out <- unique(df_out)
    df_out <- df_out[order(df_out$nom_francais, df_out$species_code), ]
    
    HTML(as.character(gembloux_kable(df_out, caption = "Espèces (codes et noms)")))
  })
  
  ############################################################
  # 5.5 DOCUMENTATION DES ÉQUATIONS
  ############################################################
  
  output$models_table <- renderUI({
    if (!is.null(models_doc) && nrow(models_doc) > 0) {
      cols <- c("eq_id", "method", "predicted_variable", "output_unit",
                "species_name_fr", "species_code",
                "validity_region", "validity_range",
                "input_variable", "input_unit",
                "reference_source")
      cols <- intersect(cols, names(models_doc))
      df_out <- head(models_doc[, cols, drop = FALSE], 200)
      HTML(as.character(gembloux_kable(df_out, caption = "Métadonnées des équations (aperçu)")))
    } else {
      HTML("<em>Aucune donnée d’équations disponible.</em>")
    }
  })
}

##############################################################
# 6) LANCEMENT ----
##############################################################

shinyApp(ui = ui, server = server)
