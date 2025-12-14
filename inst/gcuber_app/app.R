##############################################################
# GCubeR Shiny App (English) — Single method per run
# Adds: Introduction tab, Species list tab, Sources tab
##############################################################

library(shiny)
library(shinythemes)
library(GCubeR)
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)

##############################################################
# Result table styling ----
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

##############################################################
# Species lists + method definitions ----
##############################################################

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
algan_vta_vc22_species    <- c("ABIES_ALBA", "PICEA_ABIES","ALNUS_GLUTINOSA","PRUNUS_AVIUM","BETULA_SP")

method_defs <- list(
  dagnelie_vc22_1 = list(
    label           = "VC22 — Dagnelie (single entry)",
    fun             = GCubeR::dagnelie_vc22_1,
    required_cols   = c("species_code","c130"),
    short_desc      = "VC22 from c130.",
    species_allowed = dagnelie_vc22_species
  ),
  dagnelie_vc22_1g = list(
    label           = "VC22 — Dagnelie (graduated single entry)",
    fun             = GCubeR::dagnelie_vc22_1g,
    required_cols   = c("species_code","c130","hdom"),
    short_desc      = "VC22 from c130 + hdom.",
    species_allowed = dagnelie_vc22_species
  ),
  dagnelie_vc22_2 = list(
    label           = "VC22 — Dagnelie (two-entry)",
    fun             = GCubeR::dagnelie_vc22_2,
    required_cols   = c("species_code","c130","htot"),
    short_desc      = "VC22 from c130 + htot.",
    species_allowed = dagnelie_vc22_species
  ),
  vallet_vta = list(
    label           = "VTA — Vallet",
    fun             = GCubeR::vallet_vta,
    required_cols   = c("species_code","c130","htot"),
    short_desc      = "Total aboveground stem volume (VTA).",
    species_allowed = vallet_vta_species
  ),
  vallet_vc22 = list(
    label           = "VC22 — Vallet (two-entry)",
    fun             = GCubeR::vallet_vc22,
    required_cols   = c("species_code","dbh","htot"),
    short_desc      = "VC22 from dbh + htot.",
    species_allowed = vallet_vc22_species
  ),
  rondeux_vc22_vtot = list(
    label           = "VC22 + VTOT — Rondeux",
    fun             = GCubeR::rondeux_vc22_vtot,
    required_cols   = c("species_code","c130","htot"),
    short_desc      = "Computes VC22 and VTOT (larch, Southern Belgium).",
    species_allowed = rondeux_vc22_vtot_species
  ),
  bouvard_vta = list(
    label           = "VTA — Bouvard",
    fun             = GCubeR::bouvard_vta,
    required_cols   = c("species_code","dbh","htot"),
    short_desc      = "VTA for QUERCUS_SP (coppice-with-standards context).",
    species_allowed = bouvard_vta_species
  ),
  algan_vta_vc22 = list(
    label           = "VTA + VC22 — Algan",
    fun             = GCubeR::algan_vta_vc22,
    required_cols   = c("species_code","dbh","htot"),
    short_desc      = "Computes VTA and VC22 (species-specific).",
    species_allowed = algan_vta_vc22_species
  )
)

method_choices <- setNames(
  names(method_defs),
  vapply(method_defs, function(m) m$label, character(1))
)

is_numeric_col <- function(df, col) {
  is.numeric(df[[col]]) || is.integer(df[[col]])
}

##############################################################
# Reference datasets for Species / Sources tabs ----
##############################################################

models_doc <- NULL
if ("equations_GCubeR" %in% data(package = "GCubeR")$results[, "Item"]) {
  data("equations_GCubeR", package = "GCubeR", envir = environment())
  models_doc <- equations_GCubeR
}

##############################################################
# UI ----
##############################################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
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

      /* Export buttons */
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

      /* Global background + watermark */
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
  
  # Header
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
    "GCubeR — Tree volume calculator"
  ),
  
  navbarPage(
    title = "GCubeR",
    
    #########################################################
    # TAB: INTRODUCTION
    #########################################################
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          width = 10, offset = 1,
          br(),
          h2("Welcome to GCubeR"),
          p(
            "GCubeR provides a unified toolbox for computing tree volumes and derived quantities ",
            "(biomass, carbon and CO2) from common forestry measurements."
          ),
          hr(),
          h3("Minimum inputs (typical)"),
          tags$ul(
            tags$li(strong("species_code"), " — Latin name in uppercase (e.g., QUERCUS_ROBUR)"),
            tags$li(strong("c130"), " — circumference at 1.30 m (cm)")
          ),
          p("Depending on the equation, additional variables may be required (e.g., htot, hdom, dbh)."),
          hr(),
          h3("Workflow in this app"),
          tags$ol(
            tags$li("Choose one method in the dropdown."),
            tags$li("Compute for a single tree or for a CSV batch."),
            tags$li("Optionally compute biomass when available."),
            tags$li("Use the plotting tab to aggregate precomputed volumes by size classes.")
          ),
          br()
        )
      )
    ),
    
    #########################################################
    # TAB: ABOUT
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
            tags$li(strong("VC22: "), "merchantable stem volume up to circumference 22 cm."),
            tags$li(strong("VTA: "), "total above-ground stem volume."),
            tags$li(strong("VTOT: "), "total bole volume for some methods (e.g. Rondeux)."),
            tags$li(strong("Biomass / Carbon / CO2: "), "derived from volume via GCubeR biomass module.")
          ),
          hr(),
          h3("Main families of methods"),
          tags$ul(
            tags$li(
              tags$b("Dagnelie: "),
              "wide species coverage; c130 only or with hdom/htot. ",
              tags$b("(Highly recommended model)")
            ),
            tags$li(
              tags$b("Vallet: "),
              "two-entry vc22 (dbh + htot) and vta (c130 + htot). ",
              tags$b("(Highly recommended model)")
            ),
            tags$li(
              tags$b("Rondeux: "),
              "specific to LARIX_SP; computes vc22 and vtot. ",
              tags$b("(Very specific model – see the vignette before computing)")
            ),
            tags$li(
              tags$b("Bouvard: "),
              "specific to QUERCUS_SP; vta. ",
              tags$b("(Very specific model – see the vignette before computing)")
            ),
            tags$li(
              tags$b("Algan: "),
              "species-specific; computes vta and vc22. ",
              tags$b("(Very specific model – see the vignette before computing)")
            )
          
          
          ),
          br(),
          div(
            style = "text-align:center; margin-top: 10px;",
            tags$a(
              href = "introduction.html",
              target = "_blank",
              class = "btn btn-primary",
              style = "font-size: 16px; padding: 10px 20px;",
              "Open full documentation / vignette"
            )
          ),
          br()
        )
      )
    ),
    
    #########################################################
    # TAB: SINGLE TREE
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
    # TAB: CSV DATA
    #########################################################
    tabPanel(
      "CSV data",
      sidebarLayout(
        sidebarPanel(
          fileInput("file_batch", "Import a CSV (read.csv2: separator ';')", accept = c(".csv", ".CSV")),
          helpText("This tab assumes your file already uses GCubeR column names (species_code, c130, dbh, htot, hdom, ...)."),
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
          div(class = "scroll-table", tableOutput("batch_preview")),
          hr(),
          h3("Results"),
          div(class = "scroll-table", htmlOutput("batch_result_table"))
        )
      )
    ),
    
    #########################################################
    # TAB: PLOT
    #########################################################
    tabPanel(
      "Volume by classes",
      sidebarLayout(
        sidebarPanel(
          h4("File with precomputed volumes"),
          fileInput(
            "file_plot",
            "Import a CSV (read.csv2: separator ';')",
            accept = c(".csv", ".CSV")
          ),
          helpText(
            "The file must contain columns: species_code, c130, and one numeric volume column ",
            "(e.g. dagnelie_vc22_1). If c130 or the volume column contains NA, an error is shown."
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
          wellPanel(plotOutput("volume_plot", height = "420px")),
          hr(),
          h3("Export"),
          div(
            class = "export-buttons",
            downloadButton("dl_plot_csv", "Table (CSV)"),
            downloadButton("dl_plot_png", "Plot (PNG)")
          ),
          hr(),
          h3("Aggregated table"),
          wellPanel(htmlOutput("plot_table"))
        )
      )
    ),
    
    #########################################################
    # TAB: SPECIES LIST
    #########################################################
    tabPanel(
      "Species list",
      fluidRow(
        column(
          width = 12,
          h3("Species list available in GCubeR"),
          p("This table is built from GCubeR metadata (equations_GCubeR) when available."),
          div(class = "scroll-table", tableOutput("species_table"))
        )
      )
    ),
    
    #########################################################
    # TAB: SOURCES / REFERENCES
    #########################################################
    tabPanel(
      "Sources",
      fluidRow(
        column(
          width = 12,
          h3("Equations metadata and sources"),
          p("This table lists equation metadata shipped with GCubeR (equations_GCubeR), including references when provided."),
          div(class = "scroll-table", tableOutput("sources_table"))
        )
      )
    )
  )
)

##############################################################
# SERVER ----
##############################################################

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
        selectInput("uni_species_code", "species_code:", choices = m$species_allowed)
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
        numericInput("uni_htot", "htot (m):", value = NA, min = 0)
      ))
    }
    if ("hdom" %in% reqs) {
      ui_list <- c(ui_list, list(
        numericInput("uni_hdom", "hdom (m):", value = NA, min = 0)
      ))
    }
    
    # Biomass options depending on method
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
          label = "Also compute biomass (biomass_calc from VC22)",
          value = FALSE
        )
      ))
    }
    
    if (input$method_uni == "vallet_vta") {
      ui_list <- c(ui_list, list(
        hr(),
        checkboxInput(
          "uni_biomass_vallet",
          label = "Also compute biomass (biomass_calc from Vallet VTA)",
          value = FALSE
        )
      ))
    }
    
    do.call(tagList, ui_list)
  })
  
  observeEvent(input$btn_calc_uni, {
    m <- method_defs[[input$method_uni]]
    req(m)
    
    vals <- list()
    for (v in m$required_cols) {
      vals[[v]] <- input[[paste0("uni_", v)]]
    }
    df <- as.data.frame(vals)
    
    res <- tryCatch(m$fun(df), error = function(e) NULL)
    if (is.null(res)) {
      output$uni_message <- renderText("Error during calculation.")
      output$uni_result_table <- renderUI(NULL)
      return()
    }
    
    full_res <- res
    msg_parts <- c("Volume calculation completed.")
    
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
        GCubeR::biomass_calc(full_res),
        error = function(e) {
          msg_parts <<- c(msg_parts, "Biomass could not be computed. Check species_code and VC22 columns.")
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        msg_parts <- c(msg_parts, "Biomass added (biomass_calc).")
      }
    }
    
    if (input$method_uni == "vallet_vta" && isTRUE(input$uni_biomass_vallet)) {
      full_res <- tryCatch(
        GCubeR::biomass_calc(full_res),
        error = function(e) {
          msg_parts <<- c(msg_parts, "Biomass could not be computed. Check species_code and vallet_vta column.")
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        msg_parts <- c(msg_parts, "Biomass added (biomass_calc).")
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
  
  output$batch_biomass_options <- renderUI({
    vc22_methods <- c(
      "dagnelie_vc22_1",
      "dagnelie_vc22_1g",
      "dagnelie_vc22_2",
      "vallet_vc22",
      "rondeux_vc22_vtot",
      "algan_vta_vc22"
    )
    
    ui_list <- list()
    
    if (input$method_batch %in% vc22_methods) {
      ui_list <- c(ui_list, list(
        checkboxInput(
          "batch_biomass_cnifeb",
          label = "Also compute biomass (biomass_calc from VC22)",
          value = FALSE
        )
      ))
    }
    
    if (input$method_batch == "vallet_vta") {
      ui_list <- c(ui_list, list(
        checkboxInput(
          "batch_biomass_vallet",
          label = "Also compute biomass (biomass_calc from Vallet VTA)",
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
    req(m)
    
    reqs <- m$required_cols
    missing <- setdiff(reqs, names(df))
    
    if (length(missing) > 0) {
      output$batch_check <- renderPrint({
        cat("Missing required columns:\n")
        cat(paste0(" - ", missing), sep = "\n")
      })
      output$batch_result_table <- renderUI(NULL)
      return()
    }
    
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
        GCubeR::biomass_calc(full_res),
        error = function(e) {
          biomass_msgs <<- c(biomass_msgs, "Biomass could not be computed. Check species_code and VC22 columns.")
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        biomass_msgs <- c(biomass_msgs, "Biomass added (biomass_calc).")
      }
    }
    
    if (input$method_batch == "vallet_vta" && isTRUE(input$batch_biomass_vallet)) {
      full_res <- tryCatch(
        GCubeR::biomass_calc(full_res),
        error = function(e) {
          biomass_msgs <<- c(biomass_msgs, "Biomass could not be computed. Check species_code and vallet_vta column.")
          full_res
        }
      )
      if ("btot" %in% names(full_res)) {
        biomass_msgs <- c(biomass_msgs, "Biomass added (biomass_calc).")
      }
    }
    
    output$batch_check <- renderPrint({
      cat("Required columns OK:\n")
      cat(paste0(" - ", reqs), sep = "\n")
      if (length(biomass_msgs) > 0) {
        cat("\nBiomass:\n")
        cat(paste0(" - ", biomass_msgs), sep = "\n")
      }
    })
    
    batch_results(full_res)
    
    output$batch_result_table <- renderUI({
      HTML(as.character(
        gembloux_kable(head(full_res, 50), caption = "Preview (first 50 rows)")
      ))
    })
  })
  
  output$dl_batch <- downloadHandler(
    filename = function() paste0("results_", input$method_batch, ".csv"),
    content  = function(file) {
      res <- batch_results()
      req(res)
      write.csv2(res, file, row.names = FALSE)
    }
  )
  
  #### === PLOT ===
  
  dat_plot <- reactive({
    req(input$file_plot)
    read.csv2(input$file_plot$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })
  
  output$plot_volume_col_ui <- renderUI({
    df <- dat_plot()
    
    numeric_cols <- names(df)[vapply(names(df), function(nm) is_numeric_col(df, nm), logical(1))]
    if (length(numeric_cols) == 0) return(helpText("No numeric column found in the file."))
    
    structural <- c("c130","c150","dbh","htot","hdom")
    volume_candidates <- setdiff(numeric_cols, structural)
    if (length(volume_candidates) == 0) volume_candidates <- numeric_cols
    
    selected_col <- if ("dagnelie_vc22_1" %in% volume_candidates) "dagnelie_vc22_1" else volume_candidates[1]
    
    selectInput("plot_volume_col", "Volume column:", choices = volume_candidates, selected = selected_col)
  })
  
  plot_data <- reactive({
    req(dat_plot(), input$plot_volume_col, input$class_width)
    
    df <- dat_plot()
    
    validate(
      need("c130" %in% names(df), "ERROR: column 'c130' is missing."),
      need("species_code" %in% names(df), "ERROR: column 'species_code' is missing."),
      need(input$plot_volume_col %in% names(df), paste0("ERROR: volume column '", input$plot_volume_col, "' is missing."))
    )
    
    # Enforce: no NA in c130 or selected volume column
    if (any(is.na(df$c130))) {
      validate(need(FALSE, "ERROR: 'c130' contains NA values. Please fix your file."))
    }
    if (any(is.na(df[[input$plot_volume_col]]))) {
      validate(need(FALSE, paste0("ERROR: '", input$plot_volume_col, "' contains NA values. Please fix your file.")))
    }
    
    vol_col <- as.numeric(df[[input$plot_volume_col]])
    cw <- input$class_width
    
    df2 <- df %>%
      mutate(
        c130 = as.numeric(c130),
        volume = vol_col
      )
    
    validate(
      need(all(is.finite(df2$c130)), "ERROR: 'c130' must be numeric."),
      need(all(is.finite(df2$volume)), "ERROR: selected volume column must be numeric.")
    )
    
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
      distinct(class_label, class_lower) %>%
      arrange(class_lower) %>%
      pull(class_label)
    
    agg <- agg %>%
      mutate(class_label = factor(class_label, levels = class_levels))
    
    tab_mat <- xtabs(volume ~ class_label + species_code, data = agg)
    tab_df  <- as.data.frame.matrix(tab_mat)
    tab_df$Total <- rowSums(tab_df)
    tab_df <- cbind(Class = rownames(tab_df), tab_df)
    rownames(tab_df) <- NULL
    
    list(
      agg = agg,
      table = tab_df,
      cw = cw,
      limits = c(input$limit_small_medium, input$limit_medium_large)
    )
  })
  
  volume_plot_obj <- reactive({
    pd <- plot_data()
    agg <- pd$agg
    cw  <- pd$cw
    limits <- pd$limits
    
    agg <- agg %>% mutate(class_mid = class_lower + cw / 2)
    
    ggplot(agg, aes(x = class_mid, y = volume, fill = species_code)) +
      geom_col(width = cw, position = "stack", color = "grey20", size = 0.3) +
      geom_vline(xintercept = limits, linetype = "dashed") +
      labs(
        x = "c130 (cm) — classes",
        y = "Total volume",
        fill = "Species",
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
  
  output$volume_plot <- renderPlot({ volume_plot_obj() })
  
  output$plot_table <- renderUI({
    pd <- plot_data()
    req(pd$table)
    
    div(
      class = "scroll-table",
      HTML(as.character(gembloux_kable(pd$table, caption = "Volume by c130 classes and species")))
    )
  })
  
  output$dl_plot_csv <- downloadHandler(
    filename = function() paste0("volume_by_classes_", Sys.Date(), ".csv"),
    content  = function(file) {
      pd <- plot_data()
      write.csv2(pd$table, file, row.names = FALSE)
    }
  )
  
  output$dl_plot_png <- downloadHandler(
    filename = function() paste0("volume_by_classes_", Sys.Date(), ".png"),
    content  = function(file) {
      plt <- volume_plot_obj()
      png(file, width = 1400, height = 900, res = 150)
      print(plt)
      dev.off()
    }
  )
  
  #### === SPECIES LIST TAB ===
  
  output$species_table <- renderTable({
    if (!is.null(models_doc) && nrow(models_doc) > 0) {
      df <- unique(models_doc[, intersect(c("species_code", "species_name_fr"), names(models_doc)), drop = FALSE])
      if (!("species_name_fr" %in% names(df))) df$species_name_fr <- NA_character_
      df <- df[!is.na(df$species_code) & nzchar(df$species_code), , drop = FALSE]
      df <- df[order(df$species_code), , drop = FALSE]
      names(df) <- c("species_code", "species_name_fr")
      df
    } else {
      data.frame(
        species_code = sort(unique(c(
          dagnelie_vc22_species, vallet_vta_species, vallet_vc22_species,
          rondeux_vc22_vtot_species, bouvard_vta_species, algan_vta_vc22_species
        ))),
        species_name_fr = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  })
  
  #### === SOURCES TAB ===
  
  output$sources_table <- renderTable({
    if (!is.null(models_doc) && nrow(models_doc) > 0) {
      cols <- intersect(
        c(
          "eq_id", "method", "predicted_variable", "output_unit",
          "species_code", "species_name_fr",
          "validity_region", "validity_range",
          "input_variable", "input_unit",
          "formula_type", "reference_source"
        ),
        names(models_doc)
      )
      out <- models_doc[, cols, drop = FALSE]
      head(out, 200)
    } else {
      data.frame(
        message = "GCubeR dataset 'equations_GCubeR' not found in your installation.",
        stringsAsFactors = FALSE
      )
    }
  })
}

##############################################################
# RUN ----
##############################################################

shinyApp(ui = ui, server = server)
