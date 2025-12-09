#############################################################
# GCubeR Shiny App
#############################################################

library(shiny)
library(shinythemes)
library(GCubeR)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

#############################################################
# TABLE FORMATTER
#############################################################

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

#############################################################
# SPECIES LISTS
#############################################################

dagnelie_vc22_species <- c(
  "ABIES_ALBA","ACER_CAMPESTRE","ACER_PLATANOIDES","ACER_PSEUDOPLATANUS",
  "AESCULUS_HIPPOCASTANUM","ALNUS_GLUTINOSA","ALNUS_INCANA","BETULA_SP","CARPINUS_SP",
  "CASTANEA_SATIVA","CORLYUS_AVELLANA","CRATAEGUS_SP","CUPRESSUS_SP","FAGUS_SYLVATICA",
  "FRAXINUS_EXCELSIOR","JUNGLANS_SP","LARIX_SP","MALUS_SP","PICEA_ABIES","PICEA_SITCHENSIS",
  "PINUS_LARICIO","PINUS_NIGRA","PINUS_SYLVESTRIS","POPULUS_TREMULA","POPULUSxCANADENSIS",
  "PRUNUS_AVIUM","PRUNUS_CERASUS","PRUNUS_SP","PSEUDOTSUGA_MENZIESII","PYRUS_SP",
  "QUERCUS_PETRAEA","QUERCUS_PUBESCENS","QUERCUS_ROBUR","QUERCUS_RUBRA","QUERCUS_SP",
  "RHAMNUS_FRANGULA","ROBINIA_PSEUDOACACIA","SALIX_SP","SAMBUCUS_SP","SORBUS_ARIA",
  "SORBUS_AUCUPARIA","TAXUS_BACCATA","THUJA_PLICATA","TILIA_SP","ULMUS_SP"
)

vallet_vta_species <- c(
  "PICEA_ABIES","QUERCUS_ROBUR","FAGUS_SYLVATICA","PINUS_SYLVESTRIS",
  "PINUS_PINASTER","ABIES_ALBA","PSEUDOTSUGA_MENZIESII"
)

vallet_vc22_species <- c(
  "QUERCUS_ROBUR","QUERCUS_PETRAEA","QUERCUS_PUBESCENS","FAGUS_SYLVATICA",
  "PINUS_PINASTER","PINUS_SYLVESTRIS","PINUS_LARICIO","PINUS_NIGRA","PINUS_HALEPENSIS",
  "PICEA_ABIES","ABIES_ALBA","PSEUDOTSUGA_MENZIESII"
)

rondeux_vc22_vtot_species <- "LARIX_SP"
bouvard_vta_species       <- "QUERCUS_SP"
algan_vta_vc22_species    <- "ABIES_ALBA"

#############################################################
# METHOD DEFINITIONS
#############################################################

method_defs <- list(
  dagnelie_vc22_1 = list(
    label           = "VC22 : Dagnelie single entry",
    fun             = GCubeR::dagnelie_vc22_1,
    required_cols   = c("species_code","c130"),
    short_desc      = "VC22 from c130.",
    species_allowed = dagnelie_vc22_species
  ),
  dagnelie_vc22_1g = list(
    label           = "VC22 : Dagnelie graduated",
    fun             = GCubeR::dagnelie_vc22_1g,
    required_cols   = c("species_code","c130","hdom"),
    short_desc      = "VC22 from c130 + hdom.",
    species_allowed = dagnelie_vc22_species
  ),
  dagnelie_vc22_2 = list(
    label           = "VC22 : Dagnelie 2 entry",
    fun             = GCubeR::dagnelie_vc22_2,
    required_cols   = c("species_code","c130","htot"),
    short_desc      = "VC22 from c130 + htot.",
    species_allowed = dagnelie_vc22_species
  ),
  vallet_vta = list(
    label           = "VTA : Vallet",
    fun             = GCubeR::vallet_vta,
    required_cols   = c("species_code","c130","htot"),
    short_desc      = "Total volume (VTA).",
    species_allowed = vallet_vta_species
  ),
  vallet_vc22 = list(
    label           = "VC22 : Vallet 2 entry",
    fun             = GCubeR::vallet_vc22,
    required_cols   = c("species_code","dbh","htot"),
    short_desc      = "VC22 from dbh + htot.",
    species_allowed = vallet_vc22_species
  ),
  rondeux_vc22_vtot = list(
    label           = "VTA + VC22 : Rondeux",
    fun             = GCubeR::rondeux_vc22_vtot,
    required_cols   = c("species_code","c130","htot"),
    short_desc      = "VC22 + VTOT.",
    species_allowed = rondeux_vc22_vtot_species
  ),
  bouvard_vta = list(
    label           = "VTA : Bouvard",
    fun             = GCubeR::bouvard_vta,
    required_cols   = c("species_code","dbh","htot"),
    short_desc      = "VTA for QUERCUS_SP.",
    species_allowed = bouvard_vta_species
  ),
  algan_vta_vc22 = list(
    label           = "VTA + VC22 : Algan",
    fun             = GCubeR::algan_vta_vc22,
    required_cols   = c("species_code","dbh","htot"),
    short_desc      = "VTA + VC22 (ABIES_ALBA).",
    species_allowed = algan_vta_vc22_species
  )
)

method_choices <- setNames(
  names(method_defs),
  vapply(method_defs, function(m) m$label, character(1))
)

#############################################################
# HELPER
#############################################################

is_numeric_col <- function(df, col) {
  is.numeric(df[[col]]) || is.integer(df[[col]])
}

#############################################################
# UI
#############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # ==== CSS GLOBAL (UN SEUL tags$head) ====
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
  
  # ==== HEADER ====
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
    # TAB ABOUT
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
    
    #########################################################
    # TAB SINGLE TREE
    #########################################################
    tabPanel(
      "Single tree",
      sidebarLayout(
        sidebarPanel(
          h4("Method"),
          selectInput("method_uni", "Method:", choices = method_choices),
          helpText(textOutput("method_uni_desc")),
          hr(),
          uiOutput("uni_param_ui"),
          actionButton("btn_calc_uni", "Compute", class = "btn btn-primary")
        ),
        mainPanel(
          h3("Result (single tree)"),
          wellPanel(
            style = "background-color: rgba(255,255,255,0.96); border-radius:10px;",
            textOutput("uni_message"),
            br(),
            htmlOutput("uni_result_table")
          )
        )
      )
    ),
    
    
    #########################################################
    # TAB CSV
    #########################################################
    tabPanel(
      "CSV data",
      sidebarLayout(
        sidebarPanel(
          fileInput("file_batch", "Import a CSV (;)", accept = c(".csv", ".CSV")),
          helpText("Required columns depend on the selected method."),
          hr(),
          selectInput("method_batch", "Method:", choices = method_choices),
          helpText(textOutput("method_batch_desc")),
          hr(),
          uiOutput("batch_biomass_options"),
          actionButton("btn_calc_batch", "Compute", class = "btn btn-primary"),
          br(), br(),
          downloadButton("dl_batch", "Download CSV")
        ),
        mainPanel(
          h3("Column check"),
          verbatimTextOutput("batch_check"),
          hr(),
          h3("Preview"),
          div(
            class = "scroll-table",
            tableOutput("batch_preview")
          ),
          hr(),
          h3("Results"),
          div(
            class = "scroll-table",
            htmlOutput("batch_result_table")
          )
        )
      )
    ),
    
        
    
    #########################################################
    # TAB PLOT
    #########################################################
    tabPanel(
      "Volume by classes",
      sidebarLayout(
        sidebarPanel(
          h4("File with precomputed volumes"),
          fileInput(
            "file_plot",
            "Import a CSV (separator ;)",
            accept = c(".csv", ".CSV")
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
    )
  )
)

    
#############################################################
# SERVER
#############################################################

server <- function(input, output, session) {
  
  #### === SINGLE TREE ===
  
  output$method_uni_desc <- renderText({
    m <- method_defs[[input$method_uni]]
    if (is.null(m)) return("")
    m$short_desc
  })
  
  output$uni_param_ui <- renderUI({
    m <- method_defs[[input$method_uni]]
    req(m)
    
    reqs <- m$required_cols
    ui_list <- list()
    
    if ("species_code" %in% reqs) {
      ui_list <- c(ui_list, list(
        selectInput("uni_species_code", "Species code:", choices = m$species_allowed)
      ))
    }
    if ("c130" %in% reqs) {
      ui_list <- c(ui_list, list(
        numericInput("uni_c130", "c130 (cm):", value = NA, min = 0)
      ))
    }
    if ("dbh" %in% reqs) {
      ui_list <- c(ui_list, list(
        numericInput("uni_dbh", "dbh (cm):", value = NA, min = 0)
      ))
    }
    if ("htot" %in% reqs) {
      ui_list <- c(ui_list, list(
        numericInput("uni_htot", "Total height htot (m):", value = NA, min = 0)
      ))
    }
    if ("hdom" %in% reqs) {
      ui_list <- c(ui_list, list(
        numericInput("uni_hdom", "Dominant height hdom (m):", value = NA, min = 0)
      ))
    }
    
    # ---- Biomass options depending on method ----
    # Methods that produce VC22 (for CNIEFEB biomass via biomass_calc)
    vc22_methods <- c(
      "dagnelie_vc22_1",
      "dagnelie_vc22_1g",
      "dagnelie_vc22_2",
      "vallet_vc22",
      "rondeux_vc22_vtot",
      "algan_vta_vc22"
    )
    
    if (input$method_uni %in% vc22_methods) {
      ui_list <- c(ui_list, list(
        hr(),
        checkboxInput(
          "uni_biomass_cnifeb",
          label = tagList(
            shiny::icon("leaf"),
            HTML("&nbsp;Also compute biomass (CNIEFEB method from VC22)")
          ),
          value = FALSE
        )
      ))
    }
    
    # For Vallet VTA (biomass from vallet_vta volume)
    if (input$method_uni == "vallet_vta") {
      ui_list <- c(ui_list, list(
        hr(),
        checkboxInput(
          "uni_biomass_vallet",
          label = tagList(
            shiny::icon("tree"),
            HTML("&nbsp;Also compute biomass (Vallet method from VTA)")
          ),
          value = FALSE
        )
      ))
    }
    
    do.call(tagList, ui_list)
  })
  
  observeEvent(input$btn_calc_uni, {
    m <- method_defs[[input$method_uni]]
    vals <- list()
    
    for (v in m$required_cols) {
      vals[[v]] <- input[[paste0("uni_", v)]]
    }
    
    df  <- as.data.frame(vals)
    res <- tryCatch(m$fun(df), error = function(e) NULL)
    
    if (is.null(res)) {
      output$uni_message <- renderText("Error during calculation.")
      output$uni_result_table <- renderUI(NULL)
      return()
    }
    
    # Start with volume result
    full_res <- res
    msg_parts <- c("Volume calculation completed.")
    
    # ---- Biomass from VC22 (CNIEFEB) ----
    vc22_methods <- c(
      "dagnelie_vc22_1",
      "dagnelie_vc22_1g",
      "dagnelie_vc22_2",
      "vallet_vc22",
      "rondeux_vc22_vtot",
      "algan_vta_vc22"
    )
    
    if (input$method_uni %in% vc22_methods && isTRUE(input$uni_biomass_cnifeb)) {
      full_res <- tryCatch(
        {
          GCubeR::biomass_calc(full_res)
        },
        error = function(e) {
          msg_parts <<- c(
            msg_parts,
            "Biomass (CNIEFEB) could not be computed. Please check species_code and VC22 values."
          )
          full_res  # return original res
        }
      )
      if ("btot" %in% names(full_res)) {
        msg_parts <- c(msg_parts, "Biomass added using CNIEFEB method (from VC22).")
      }
    }
    
    # ---- Biomass from Vallet VTA ----
    if (input$method_uni == "vallet_vta" && isTRUE(input$uni_biomass_vallet)) {
      full_res <- tryCatch(
        {
          GCubeR::biomass_calc(full_res)
        },
        error = function(e) {
          msg_parts <<- c(
            msg_parts,
            "Biomass (Vallet) could not be computed. Please check species_code and vallet_vta values."
          )
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        msg_parts <- c(msg_parts, "Biomass added using Vallet method (from VTA).")
      }
    }
    
    output$uni_message <- renderText(paste(msg_parts, collapse = " "))
    output$uni_result_table <- renderUI({
      HTML(
        paste0(
          "<div style='overflow-x:auto; max-width:100%;'>",
          as.character(gembloux_kable(full_res)),
          "</div>"
        )
      )
    })
  })
  
  #### === CSV IMPORT ===
  
  batch_results <- reactiveVal(NULL)
  
  output$method_batch_desc <- renderText({
    m <- method_defs[[input$method_batch]]
    if (is.null(m)) return("")
    m$short_desc
  })
  # Biomass options for CSV tab
  output$batch_biomass_options <- renderUI({
    # Same list as for the single tree tab
    vc22_methods <- c(
      "dagnelie_vc22_1",
      "dagnelie_vc22_1g",
      "dagnelie_vc22_2",
      "vallet_vc22",
      "rondeux_vc22_vtot",
      "algan_vta_vc22"
    )
    
    ui_list <- list()
    
    # CNIEFEB biomass from VC22
    if (input$method_batch %in% vc22_methods) {
      ui_list <- c(ui_list, list(
        checkboxInput(
          "batch_biomass_cnifeb",
          label = tagList(
            shiny::icon("leaf"),
            HTML("&nbsp;Also compute biomass (CNIEFEB method from VC22)")
          ),
          value = FALSE
        )
      ))
    }
    
    # Vallet biomass from VTA
    if (input$method_batch == "vallet_vta") {
      ui_list <- c(ui_list, list(
        checkboxInput(
          "batch_biomass_vallet",
          label = tagList(
            shiny::icon("tree"),
            HTML("&nbsp;Also compute biomass (Vallet method from VTA)")
          ),
          value = FALSE
        )
      ))
    }
    
    do.call(tagList, ui_list)
  })
  
  
  dat_batch <- reactive({
    req(input$file_batch)
    read.csv2(input$file_batch$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  output$batch_preview <- renderTable({
    req(dat_batch())
    head(dat_batch(), 10)
  })
  
  observeEvent(input$btn_calc_batch, {
    df <- dat_batch()
    m  <- method_defs[[input$method_batch]]
    
    reqs    <- m$required_cols
    missing <- setdiff(reqs, names(df))
    
    # ---- Check required columns ----
    if (length(missing) > 0) {
      output$batch_check <- renderPrint({
        cat("Missing required columns:\n")
        cat(paste0(" - ", missing), sep = "\n")
      })
      output$batch_result_table <- renderUI(NULL)
      return()
    }
    
    # ---- Run main volume / model function ----
    res <- tryCatch(m$fun(df), error = function(e) NULL)
    if (is.null(res)) {
      output$batch_check <- renderPrint({
        cat("Error during calculation (see R console for details).")
      })
      output$batch_result_table <- renderUI(NULL)
      return()
    }
    
    full_res <- res
    biomass_msgs <- character(0)
    
    # ---- Biomass from VC22 (CNIEFEB) ----
    vc22_methods <- c(
      "dagnelie_vc22_1",
      "dagnelie_vc22_1g",
      "dagnelie_vc22_2",
      "vallet_vc22",
      "rondeux_vc22_vtot",
      "algan_vta_vc22"
    )
    
    if (input$method_batch %in% vc22_methods && isTRUE(input$batch_biomass_cnifeb)) {
      full_res <- tryCatch(
        {
          GCubeR::biomass_calc(full_res)
        },
        error = function(e) {
          biomass_msgs <<- c(
            biomass_msgs,
            "Biomass (CNIEFEB) could not be computed. Please check species_code and VC22 columns."
          )
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        biomass_msgs <- c(
          biomass_msgs,
          "Biomass added using CNIEFEB method (from VC22)."
        )
      }
    }
    
    # ---- Biomass from Vallet VTA ----
    if (input$method_batch == "vallet_vta" && isTRUE(input$batch_biomass_vallet)) {
      full_res <- tryCatch(
        {
          GCubeR::biomass_calc(full_res)
        },
        error = function(e) {
          biomass_msgs <<- c(
            biomass_msgs,
            "Biomass (Vallet) could not be computed. Please check species_code and vallet_vta column."
          )
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        biomass_msgs <- c(
          biomass_msgs,
          "Biomass added using Vallet method (from VTA)."
        )
      }
    }
    
    # ---- Optional: species / validity issues if you use check_result_issues() ----
    issues <- NULL
    if (exists("check_result_issues")) {
      issues <- check_result_issues(df, full_res, m)
    }
    
    # ---- Column check + warnings ----
    output$batch_check <- renderPrint({
      cat("Required columns OK:\n")
      cat(paste0(" - ", reqs), sep = "\n")
      
      if (!is.null(issues)) {
        if (length(issues$invalid_species) > 0) {
          cat("\n\nWARNING: some species_code are invalid or not supported:\n")
          cat(paste0(" - ", issues$invalid_species), sep = "\n")
        }
        if (isTRUE(issues$out_of_range)) {
          cat("\nWARNING: some rows have dimensions (c130 / dbh / htot / hdom) ",
              "outside the model validity range (result NA).\n", sep = "")
        }
      }
      
      if (length(biomass_msgs) > 0) {
        cat("\nBiomass:\n")
        cat(paste0(" - ", biomass_msgs), sep = "\n")
      }
    })
    
    # ---- Store and display results ----
    batch_results(full_res)
    
    output$batch_result_table <- renderUI({
      HTML(as.character(
        gembloux_kable(
          head(full_res, 50),
          caption = "Preview (first 50 rows)"
        )
      ))
    })
  })
  
  output$dl_batch <- downloadHandler(
    filename = function() {
      paste0("resultats_", input$method_batch, ".csv")
    },
    content = function(file) {
      res <- batch_results()
      write.csv2(res, file, row.names = FALSE)
    }
  )
  
  #### === PLOT ===
  
  #### === PLOT ===
  
  dat_plot <- reactive({
    req(input$file_plot)
    read.csv2(
      input$file_plot$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  
  output$plot_volume_col_ui <- renderUI({
    df <- dat_plot()
    num <- names(df)[vapply(names(df), function(nm) is_numeric_col(df, nm), logical(1))]
    
    if (length(num) == 0) {
      return(helpText("No numeric column found in the file."))
    }
    
    selectInput(
      "plot_volume_col",
      "Volume column:",
      choices  = num,
      selected = if ("dagnelie_vc22_1" %in% num) "dagnelie_vc22_1" else num[1]
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
    
    # --- NEW: force class_label order by class_lower ---
    class_levels <- agg %>%
      dplyr::distinct(class_label, class_lower) %>%
      dplyr::arrange(class_lower) %>%
      dplyr::pull(class_label)
    
    agg <- agg %>%
      mutate(class_label = factor(class_label, levels = class_levels))
    
    # Build wide table with rows ordered by increasing c130 class
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
}
  
#############################################################
# RUN
#############################################################

shinyApp(ui = ui, server = server)