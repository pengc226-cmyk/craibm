suppressPackageStartupMessages({
  library(shiny)
  library(promises)
  library(future)
  library(bs4Dash)
  library(dplyr)
  library(ggplot2)
  library(craibm)       
})

# The cloud watcher must run outside the main Shiny process so network waits
# cannot block or grey out the page.
future::plan(future::multisession, workers = 2)




# ==============================================================================
# Fish IBM Shiny App (inst/app/app.R)
# All helper functions come from R/helper.R via package namespace.
# ==============================================================================




ui <- dashboardPage(
  title = "Sportfish IBM",
  fullscreen = TRUE,
  header = dashboardHeader(title = "Sportfish IBM Model"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Step 1: Parameters 1: Biological Data Inputs", tabName = "params", icon = icon("sliders-h")),
      menuItem("Step 1: Parameters 2: Global Parameters", tabName = "global", icon = icon("sliders")),
      menuItem("Step 1: Parameters 3: Experiment Design", tabName = "design", icon = icon("flask")),
      menuItem("Step 1: Design Preview", tabName = "combos", icon = icon("table")),
      menuItem("Step 2: Run control", tabName = "runcontrol", icon = icon("microchip")),
      menuItem("Step 3a: Test Simulation", tabName = "sim", icon = icon("play")),
      menuItem("Step 3b: Run Full Simulations", tabName = "run_save", icon = icon("folder-open")),
      menuItem("Step 4: Results and Analysis", tabName = "results", icon = icon("chart-bar")),
      
      # ---- Save / load all Step 1 & Step 2 settings as an .rds file ----
      tags$hr(style = "border-color: #4b5563; margin: 12px 10px;"),
      tags$div(
        style = "padding: 4px 12px 12px 12px;",
        tags$div(
          style = "font-size: 12px; color: #9ca3af; margin-bottom: 6px;",
          icon("save"), " Session settings"
        ),
        shinyFiles::shinySaveButton(
          "save_settings",
          "Save current settings",
          "Save settings as .rds",
          filetype = list(rds = "rds"),
          class = "btn-outline-light btn-sm",
          style = "width: 100%; margin-bottom: 6px; text-align: left;",
          icon = icon("download")
        ),
        fileInput(
          "load_settings",
          NULL,
          accept = ".rds",
          buttonLabel = "Load settings...",
          placeholder = "No file selected",
          width = "100%"
        ),
        
        # Shows the restoration and completeness status after an .rds file is loaded.
        uiOutput("settings_load_log")
      )
    ),
    width = "320px",
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = FALSE,
    fixed = TRUE
  ),
  
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody(
    tags$head(
      tags$style(
        HTML("
      .nav-sidebar .nav-link p {
        font-size: 13px !important;
      }

      @media (max-width: 1100px) {
        .content-wrapper [class*='col-sm-'] {
          width: 100% !important;
          max-width: 100% !important;
          flex: 0 0 100% !important;
        }

        .content-wrapper .box {
          margin-bottom: 15px;
        }
      }
    ")
      )
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      html = tagList(
        img(src = "logo.png", height = "500px"), 
        h3("Welcome to Sportfish IBM Model"),
        h4("System is initializing... Please wait."),
        waiter::spin_flower() 
      ),
      color = "#333e48" 
    ),
    tabItems(
      # ================== Tab 1: Parameterization ==================
      tabItem(
        tabName = "params",
        
        fluidRow(
          
          shiny::column(4,
                        box(title = "1. Growth (VBGF)", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                            fileInput("file_growth", "Upload Length-Age Data (CSV)", accept = ".csv"),
                            helpText( "Required columns: 'Length' in millimeters (mm) and 'Age' in years."),
                            uiOutput("missing_age_choice"),
                            numericInput("boot_b_vbgf", "Bootstrap Replicates", value = 100, min = 100),
                            shiny::actionButton("run_vbgf", "Run Growth Bootstrap", class = "btn-primary", width = "100%"),
                            
                            div(
                              style = "margin-top: 12px;",
                              checkboxInput("show_growth_advanced",
                                            "Advanced: set random seeds", value = FALSE),
                              conditionalPanel(
                                condition = "input.show_growth_advanced == true",
                                wellPanel(
                                  style = "background: #f8f9fa; padding: 10px; margin-bottom: 0;",
                                  helpText(
                                    "Leave a box empty to draw a new seed each run.",
                                    "Enter the seed reported on the right to repeat an earlier run exactly."
                                  ),
                                  numericInput("vbgf_seed_manual", "Growth bootstrap seed",
                                               value = NA, min = 1, step = 1),
                                  uiOutput("alk_seed_input")
                                )
                              )
                            )
                        ),
                        
                        box(title = "2. Age-length Key Data", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                            fileInput("file_alk", "Upload ALK Data (CSV)", accept = c(".csv", ".RData")),
                            helpText("Required columns: 'Age' in years, 'n' as the sample count, ",
                                     "'Length' as mean length in mm, and 'Lengthsd' as standard deviation in mm."),
                            div(style = "margin-top: 10px;",
                                shiny::actionButton("submit_alk", "Submit & Check ALK Data",
                                                    class = "btn-info", width = "100%")
                            ),
                            uiOutput("auto_alk_note")
                        )
          ),
          
          shiny::column(8,
                        tabBox(
                          width = 12,
                          title = "",
                          id = "tab_diag",
                          
                          tabPanel(
                            "Welcome",
                            icon = icon("info-circle"),
                            h4("Welcome to the sportfish IBM model!"),
                            helpText("Please verify your data inputs here before proceeding (upload the data, set parameters and click each button on the left)."),
                            hr(),
                            verbatimTextOutput("step1_info_box"),
                            uiOutput("alk_preview_block")
                          ),
                          
                          tabPanel(
                            "Growth (VBGF)",
                            icon = icon("fish"),
                            h4("VBGF bootstrap parameter distributions"),
                            plotOutput("plot_vbgf", height="500px"),
                            verbatimTextOutput("summary_vbgf")
                          )
                        )
          )
        )
      ),
      
      tabItem(
        tabName = "global",
        
        
        fluidRow(
          box(
            title = "Global Parameters", width = 12, status = "primary",
            solidHeader = TRUE, collapsible = TRUE,
            
            bs4Dash::tabsetPanel(
              id = "global_tabs",
              tabPanel(
                "About Global Parameters",
                
                wellPanel(
                  style = paste0(
                    "background-color: #f8fbff;",
                    "border-left: 5px solid #17a2b8;",
                    "padding: 16px;",
                    "margin-top: 12px;"
                  ),
                  
                  tags$h4(
                    icon("sliders-h"),
                    strong("About Global Parameters")
                  ),
                  
                  tags$p(
                    "Use this step to define the simulation timeline, ",
                    "density-dependent processes, angler-retention behavior, ",
                    "natural mortality, life-history settings, stock–recruitment ",
                    "parameters, and population initialization."
                  ),
                  
                  tags$p(
                    strong("Common units and scales: "),
                    "Fish lengths and size thresholds are entered in millimeters (mm); ",
                    "ages and simulation durations are entered in years; months are ",
                    "entered as integers from 1 to 12; probabilities and proportions ",
                    "range from 0 to 1; and lake area is entered in hectares (ha). ",
                    "Additional units are shown beside the relevant inputs."
                  ),
                  
                  tags$div(
                    style = paste0(
                      "margin-top: 12px;",
                      "padding-top: 10px;",
                      "border-top: 1px solid #d9e6f2;",
                      "color: #5f6b76;"
                    ),
                    
                    icon("book-open"),
                    strong(" Brief guidance only: "),
                    "The descriptions provided in the app are intended as a quick ",
                    "overview. See the user manual for complete variable definitions, ",
                    "equations, recommended ranges, and worked examples."
                  )
                )
              ),
              
              # 2) Timeline
              
              tabPanel(
                "Timeline",
                numericInput("transient_years", "Burn in years (initial model warm-up):", value = 5, min = 0, step = 1),
                numericInput("stable_years", "Stable years (equilibrium phase before policy):", value = 5, min = 0, step = 1),
                helpText("'Before policy' = Burn in years + Stable years combined. "),
                numericInput("policy_years",  "Policy years (the length of simulation policy years):",  value = 5, min = 1, step = 1)
              ),
              
              
              # 3) Density-dependent effects
              
              tabPanel(
                "Density-dependent Effects",
                tags$div(
                  class = "alert alert-info",
                  icon("book-open"),
                  "See the user manual for definitions, equations, units, and descriptions ",
                  "of the density-dependent survival and growth parameters."
                ),
                checkboxInput("use_dd_effects", "Enable density-dependent effects", value = TRUE),
                
                conditionalPanel(
                  condition = "input.use_dd_effects == true",
                  
                  # ---- survival DD ----
                  box(
                    title = "Density-dependent survival", width = 12, status = "info",
                    solidHeader = TRUE, collapsible = TRUE,
                    checkboxInput("use_dd_survival", "Enable Density-dependent survival", value = TRUE),
                    
                    conditionalPanel(
                      condition = "input.use_dd_survival == true",
                      fluidRow(
                        shiny::column(4, numericInput("surv_a", "a", value = 0.339, step = 0.001)),
                        shiny::column(4, numericInput("surv_b", "b", value = 1.1,   step = 0.01)),
                        shiny::column(4, numericInput("surv_c", "c", value = 1.23,  step = 0.01))
                      ),
                      fluidRow(
                        shiny::column(6, numericInput("surv_d_avg1", "d1", value = 210.29, step = 0.01)),
                        shiny::column(6, numericInput("surv_d_avg2", "d2", value = 134.78, step = 0.01))
                      )
                    )
                  ),
                  
                  # ---- growth DD : adult fish ----
                  box(
                    title = "Density-dependent growth — Adult fish", width = 12, status = "info",
                    solidHeader = TRUE, collapsible = TRUE,
                    checkboxInput("use_dd_growth_adult", "Enable Density-dependent growth (Adult)", value = TRUE),
                    
                    conditionalPanel(
                      condition = "input.use_dd_growth_adult == true",
                      fluidRow(
                        shiny::column(4, numericInput("g1_a", "a", value = 0.867, step = 0.001)),
                        shiny::column(4, numericInput("g1_b", "b", value = 0.434, step = 0.001)),
                        shiny::column(4, numericInput("g1_c", "c", value = 1.614, step = 0.001))
                      ),
                      numericInput("g1_d_avg", "d", value = 210.29, step = 0.01)
                    )
                  ),
                  
                  # ---- growth DD : juvenile fish ----
                  box(
                    title = "Density-dependent growth — Juvenile fish", width = 12, status = "info",
                    solidHeader = TRUE, collapsible = TRUE,
                    checkboxInput("use_dd_growth_juv", "Enable Density-dependent growth (Juvenile)", value = TRUE),
                    
                    conditionalPanel(
                      condition = "input.use_dd_growth_juv == true",
                      fluidRow(
                        shiny::column(4, numericInput("g2_a", "a", value = 0.867, step = 0.001)),
                        shiny::column(4, numericInput("g2_b", "b", value = 0.434, step = 0.001)),
                        shiny::column(4, numericInput("g2_c", "c", value = 1.614, step = 0.001))
                      ),
                      numericInput("g2_d_avg", "d", value = 134.78, step = 0.01)
                    )
                  )
                )
              ),
              
              
              # 4) Harvest
              
              tabPanel(
                "Harvest",
                
                tags$div(
                  class = "alert alert-info",
                  style = "margin-bottom: 15px;",
                  icon("info-circle"),
                  strong(" How retention is modeled: "),
                  "After a vulnerable fish is encountered, the model calculates the ",
                  "angler's probability of retaining that fish. The retention inputs below ",
                  "describe angler willingness to retain a fish based on its length. ",
                  "Size-limit legality and compliance are applied separately during ",
                  "policy simulations. Fish lengths on this page are in millimeters (mm)."
                ),
                
                checkboxInput(
                  "flag_harvest_curve",
                  "Enable length-dependent retention probability curve",
                  value = TRUE
                ),
                
                conditionalPanel(
                  condition = "input.flag_harvest_curve == true",
                  
                  box(
                    title = "Retention Probability Curve",
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    tags$details(
                      style = paste0(
                        "margin-bottom: 15px;",
                        "background-color: #fff;",
                        "padding: 10px;",
                        "border: 1px solid #dee2e6;",
                        "border-radius: 5px;"
                      ),
                      
                      tags$summary(
                        icon("info-circle"),
                        strong(" Retention-curve parameter definitions"),
                        style = "cursor: pointer;"
                      ),
                      
                      tags$p(
                        style = "margin-top: 10px;",
                        tags$code(
                          "p(L) = p_max / [1 + exp{-slope × (L - L50)}]"
                        )
                      ),
                      
                      tags$ul(
                        tags$li(
                          strong("L50 (mm): "),
                          "The fish length at which retention probability equals ",
                          "one-half of p_max."
                        ),
                        
                        tags$li(
                          strong("p_max: "),
                          "The maximum retention probability approached for large fish. ",
                          "Enter a value from 0 to 1."
                        ),
                        
                        tags$li(
                          strong("Slope (per mm): "),
                          "Controls how rapidly retention probability increases with fish ",
                          "length around L50. Larger values produce a steeper curve."
                        )
                      )
                    ),
                    
                    fluidRow(
                      shiny::column(
                        4,
                        numericInput(
                          "harv_L50",
                          "L50 (mm)",
                          value = 240,
                          step = 1,
                          min = 0
                        )
                      ),
                      
                      shiny::column(
                        4,
                        numericInput(
                          "harv_pmax",
                          "Maximum retention probability (p_max)",
                          value = 0.98,
                          step = 0.01,
                          min = 0,
                          max = 1
                        )
                      ),
                      
                      shiny::column(
                        4,
                        numericInput(
                          "harv_slope",
                          "Curve slope (per mm)",
                          value = 0.042,
                          step = 0.001,
                          min = 0
                        )
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.flag_harvest_curve == false",
                  
                  box(
                    title = "Fixed Retention Probability",
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    numericInput(
                      "harv_fixed_pmax",
                      "Fixed retention probability (0–1)",
                      value = 0.1,
                      step = 0.01,
                      min = 0,
                      max = 1
                    ),
                    
                    helpText(
                      "When the retention curve is disabled, this probability is applied ",
                      "equally to encountered fish of all lengths before size-limit legality ",
                      "and compliance are considered."
                    )
                  )
                ),
                
                box(
                  title = "Monthly Fishing-Effort Weights",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  textInput(
                    "month_weights",
                    paste0(
                      "Relative monthly fishing-effort weights ",
                      "(January through December; 12 comma-separated values)"
                    ),
                    value = "25,25,50,50,50,42,42,42,37,37,37,25"
                  ),
                  
                  helpText(
                    "Enter 12 non-negative relative weights in calendar order, beginning ",
                    "with January. The model divides each weight by the sum of all 12 ",
                    "weights, so only their relative values matter. At least one weight ",
                    "must be greater than 0."
                  ),
                  
                  tags$p(
                    style = "margin-bottom: 4px;",
                    strong("For equal fishing effort in every month, copy and paste:")
                  ),
                  
                  tags$pre(
                    style = "padding: 8px; margin-bottom: 0;",
                    "1,1,1,1,1,1,1,1,1,1,1,1"
                  )
                )
              ),
              
              
              
              # 5) Survival
              
              tabPanel(
                "Natural Mortality",
                br(),
                div(class = "alert alert-info", style = "margin-bottom: 20px;",
                    icon("lightbulb"), 
                    strong(" Note: "), "On this page, ", strong("M"), 
                    " refers to the ", strong("Instantaneous Natural Mortality coefficient"), "."
                    
                ),
                fluidRow(
                  shiny::column(width = 5,
                                
                                # Part A: Juvenile
                                box(title = "Part A: Juvenile Natural Mortality", width = 12, 
                                    status = "danger", solidHeader = TRUE, collapsible = TRUE, icon = icon("fish"),
                                    
                                    numericInput("juv_annual_M", "Juvenile annual Nature Mortality coefficient (instantaneous)", value = 1.8, step = 0.01, min = 0.001),
                                    helpText(icon("info-circle"), "Applied to fish younger than the 'Transition Age' (defined in 'Other' tab).")
                                ),
                                
                                # Part B: Adult
                                box(title = "Part B: Adult Natural Mortality", width = 12, 
                                    status = "success", solidHeader = TRUE, collapsible = TRUE, icon = icon("skull-crossbones"),
                                    
                                    checkboxInput("use_z_estimation", "M comes from Catch curve estimation", value = TRUE),
                                    conditionalPanel(condition = "input.use_z_estimation == true",
                                                     wellPanel(style = "background: #f8f9fa; border-left: 5px solid #28a745; padding: 10px;",
                                                               h5(strong("1. Configure Estimation")),
                                                               selectInput("z_method", "Method", choices = c("Linear Regression (Heinke)"="lr", "Weighted LR (Chapman-Robson)"="wlr", "Poisson GLM"="pois", "Random-Intercept Poisson"="ripois")),
                                                               numericInput("z_last", "Catch Curve Max Age", 10, min=1),
                                                               
                                                               fluidRow(
                                                                 shiny::column(6, numericInput("z_boot_bg2", "Bootstrap Reps", 1000, min=100)),
                                                                 shiny::column(6, style = "margin-top: 25px;", shiny::actionButton("run_z", "Calculate Z", class="btn-success", width="100%", icon=icon("calculator")))
                                                               ),
                                                               div(
                                                                 style = "margin-top: 4px;",
                                                                 checkboxInput("show_z_advanced",
                                                                               "Advanced: set random seed", value = FALSE),
                                                                 conditionalPanel(
                                                                   condition = "input.show_z_advanced == true",
                                                                   numericInput("z_seed_manual", "Catch curve bootstrap seed",
                                                                                value = NA, min = 1, step = 1),
                                                                   helpText(
                                                                     "Leave empty to draw a new seed each run.",
                                                                     "Enter the seed reported on the right to repeat an earlier run exactly."
                                                                   )
                                                                 )
                                                               ),
                                                               hr(),
                                                               h5(strong("2. Assumed Relationship")),
                                                               numericInput("F_over_Z_ratio", "Assumed M/Z ratio", value = 0.5, step = 0.01, min=0.01, max=0.99)
                                                               
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.use_z_estimation == false",
                                                     wellPanel(style = "background: #fff3cd; border-left: 5px solid #ffc107; padding: 10px;",
                                                               h5(strong("Direct Input Mode")),
                                                               numericInput("fixed_adult_M", "Fixed Adult Annual M", value = 0.5, step = 0.01, min=0.001),
                                                               helpText("Applied uniformly to all adult fish.")
                                                     )
                                    )
                                ),
                                
                                div(style = "border-top: 2px solid #17a2b8; padding-top: 15px; margin-top: 10px;", 
                                    shiny::actionButton("submit_survival", "Confirm & Save Survival Parameters", class = "btn-info btn-lg", width = "100%", icon = icon("check-double")),
                                    br(), br(),
                                    verbatimTextOutput("log_survival")
                                )
                  ),
                  
                  shiny::column(width = 7,
                                
                                conditionalPanel(condition = "input.use_z_estimation == true",
                                                 box(title = "Z Estimation Results", width = 12, status = "primary", solidHeader = TRUE,
                                                     tags$label("Current Status:"),
                                                     verbatimTextOutput("z_status_display", placeholder = TRUE),
                                                     hr(),
                                                     plotOutput("plot_z", height = "400px"),
                                                     hr(),
                                                     h5(icon("list"), "Statistical Summary:"),
                                                     verbatimTextOutput("summary_z")
                                                 )
                                ),
                                
                                conditionalPanel(condition = "input.use_z_estimation == false",
                                                 box(title = "Total mortality Estimation Status", width = 12, status = "secondary", solidHeader = TRUE,
                                                     div(style = "text-align: center; padding: 50px; color: #6c757d;",
                                                         h1(icon("ban")),
                                                         h4("Z Estimation Plot is Not Available"),
                                                         p("You are using a fixed adult natural mortality cofficent.")
                                                         
                                                     )
                                                 )
                                )
                  )
                )
              ),
              
              
              # 6) Other
              
              tabPanel("Other",
                       
                       # 1. PSD Box
                       box(title = "PSD Size Thresholds (mm)", width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                           helpText("Define length thresholds for Stock, Quality, Preferred, Memorable, Trophy."),
                           fluidRow(shiny::column(2, numericInput("psd_stock", "Stock", 130)), 
                                    shiny::column(2, numericInput("psd_quality", "Quality", 200)), 
                                    shiny::column(3, numericInput("psd_preferred", "Preferred", 250)), 
                                    shiny::column(3, numericInput("psd_memorable", "Memorable", 300)), 
                                    shiny::column(2, numericInput("psd_trophy", "Trophy", 380)))
                       ),
                       
                       # 2. [MEGA BOX] Biology & Life History Logic
                       box(title = "Life History & Recruitment Logic", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, icon = icon("dna"),
                           
                           wellPanel(style = "background: #e3f2fd; border-left: 5px solid #2196f3;",
                                     tags$h5(strong("1. Vulnerability Mode")),
                                     radioButtons("f_age_mode", label = NULL,
                                                  choices = c("Length-based" = "size", 
                                                              "Age-based" = "age"),
                                                  selected = "size", inline = TRUE),
                                     
                                     conditionalPanel(
                                       condition = "input.f_age_mode == 'size'",
                                       helpText(icon("info-circle"), strong("Selected: Length-based."), 
                                                " Fish become vulnerable to fishing when they reach 'Stock Size'. "
                                       )
                                     ),
                                     conditionalPanel(
                                       condition = "input.f_age_mode == 'age'",
                                       helpText(icon("exclamation-triangle"), strong("Age-based."), 
                                                " Fish become vulnerable to fishing only when they reach the Fishery Recruit Age (see below).",
                                                "Younger fish (below or equal to this age) are protected from fishing regardless of size.")
                                     )
                           ),
                           
                           tags$hr(),
                           
                           tags$h5(strong("2. Critical Life History Ages")),
                           
                           fluidRow(
                             shiny::column(4, numericInput("age_spawn", "Maturity Age", value = 1.0, step = 0.5, min=0.1)),
                             shiny::column(4, numericInput("min_adult_age", "Transition Age", value = 1.0, step = 0.5, min=0.1)),
                             shiny::column(4, numericInput("z_full", "Recruit Age (Fishery)", value = 1, min=0, step=1))
                           ),
                           
                           tags$details(
                             style = "margin-bottom: 15px; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                             tags$summary(icon("info-circle"), strong(" Click here for parameter definitions"), style = "cursor: pointer; color: #007bff;"),
                             tags$ul(style = "margin-top: 10px; color: #6c757d; font-size: 0.9em;",
                                     tags$li(strong("Maturity Age:"), " Age at which fish start contributing to Spawning Biomass (used in R-S relationship)."),
                                     tags$li(strong("Transition Age:"), " Age when biology changes from Juvenile to Adult (used for applying Natural Mortality and as the full recruitment age in Catch Curve analysis)."),
                                     tags$li(strong("Recruit Age (Fishery):"), 
                                             " Reference age used for (i) recruit-related output summaries (e.g., recruit density / fishery recruit abundance) and ",
                                             tags$span(style = "color: #d9534f; font-weight: bold;", "(ii) when vulnerability Mode = Age-based "), 
                                             "Fish younger than this age are excluded from fishing encounters in age-based mode.")
                             )
                           ),
                           
                           tags$hr(),
                           
                           # --- Section 3: Reproduction & Recruitment (Moved Here!) ---
                           tags$h5(
                             strong("3. Reproduction & Stock–Recruitment Relationship")
                           ),
                           
                           tags$details(
                             style = paste0(
                               "margin-bottom: 15px;",
                               "background-color: #f8f9fa;",
                               "padding: 10px;",
                               "border-radius: 5px;"
                             ),
                             
                             tags$summary(
                               icon("info-circle"),
                               strong(" R–S parameter definitions"),
                               style = "cursor: pointer; color: #007bff;"
                             ),
                             
                             tags$ul(
                               style = "margin-top: 10px; margin-bottom: 0;",
                               
                               tags$li(
                                 strong("R: "),
                                 "Recruitment density produced during the spawning event."
                               ),
                               
                               tags$li(
                                 strong("S: "),
                                 "Spawning-stock density at the spawning event."
                               ),
                               
                               tags$li(
                                 strong("R–S alpha: "),
                                 "The density-independent recruitment-rate parameter. It controls ",
                                 "recruitment at low spawning-stock density."
                               ),
                               
                               tags$li(
                                 strong("R–S beta: "),
                                 "The density-dependent coefficient. It controls how strongly ",
                                 "recruitment is reduced as spawning-stock density increases."
                               ),
                               
                               tags$li(
                                 strong("Ricker model: "),
                                 tags$code("R = alpha × S × exp(-beta × S)")
                               ),
                               
                               tags$li(
                                 strong("Beverton–Holt model: "),
                                 tags$code("R = alpha × S / (1 + beta × S)")
                               )
                             )
                           ),
                           checkboxInput("use_ricker", "Use Ricker Model (if not selected, use B-H model)", value = TRUE),
                           fluidRow(
                             # Months
                             shiny::column(3, numericInput("spawn_month", "Spawn Month", 4, min=1, max=12)),
                             shiny::column(3, numericInput("recruit_entry_month", "Recruits Entry Month", 8, min=1, max=12)),
                             
                             # R-S Parameters
                             shiny::column(3, numericInput("rec_a", "R-S alpha", 18.192)), 
                             shiny::column(3, numericInput("rec_b", "R-S beta", 0.02152))
                           ),
                           helpText("Recruits Entry Month<Spawn Month is allowed. If so, new fish will enter population in the next year ")
                       ),
                       
                       # 3. Environment & General (Now very clean!)
                       box(title = "Environment & Initialization", width = 12, status = "secondary", solidHeader = TRUE, collapsible = TRUE, icon = icon("globe"),
                           fluidRow(
                             shiny::column(6, numericInput("lake_area_ha", "Lake Area (ha)", 2818.635)), 
                             shiny::column(6, numericInput("initial_pop_size","Initial Population Size", 10000))
                           ),
                           helpText("Basic physical settings for the simulation.")
                       )
              )
            ) # End tabsetPanel
          ) # End box
        ), # End fluidRow 1
        
        
        
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = "Validation & Submission",
              width = 12,
              status = "success",
              solidHeader = TRUE,
              
              
              shiny::actionButton("submit_global", "Submit & Check Parameters",
                                  class = "btn-danger btn-lg",
                                  width = "100%",
                                  icon = icon("check-circle")),
              
              br(), br(),
              
              
              verbatimTextOutput("log_step1_2")
            )
          )
        ) # End fluidRow 2
        
      ) ,# End tabItem
      
      
      tabItem(
        tabName = "design",
        fluidRow(
          shiny::column(
            width = 12,
            
            box(
              title = "Experiment Design",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              bs4Dash::tabsetPanel(
                id = "design_tabs",
                
                # ======================================================
                # 1. About Experiment Design
                # ======================================================
                tabPanel(
                  "About Experiment Design",
                  
                  wellPanel(
                    style = paste0(
                      "background-color: #f8fbff;",
                      "border-left: 5px solid #17a2b8;",
                      "padding: 16px;",
                      "margin-top: 12px;"
                    ),
                    
                    tags$h4(
                      icon("flask"),
                      strong("About Experiment Design")
                    ),
                    
                    tags$p(
                      "Use this step to define management size-limit scenarios, ",
                      "uncertainty combinations, annual angler-encounter proportions, ",
                      "release-mortality rates, and size-specific compliance ",
                      "assumptions included in the simulation experiment."
                    ),
                    
                    tags$p(
                      strong("Common units and scales: "),
                      "All fish-length thresholds are entered in millimeters (mm). ",
                      "Annual angler encounter, release mortality, and compliance ",
                      "are entered as proportions from 0 to 1. Scenario names are ",
                      "user-defined labels."
                    ),
                    
                    tags$div(
                      style = paste0(
                        "margin-top: 12px;",
                        "padding-top: 10px;",
                        "border-top: 1px solid #d9e6f2;",
                        "color: #5f6b76;"
                      ),
                      
                      icon("book-open"),
                      strong(" Brief guidance only: "),
                      "The descriptions and examples provided in the app are ",
                      "intended as a quick overview. See the user manual for ",
                      "complete variable definitions, equations, recommended ",
                      "ranges, and experiment-design guidance."
                    )
                  )
                ),
                
                # ======================================================
                # 2. Size Limit Scenarios
                # ======================================================
                tabPanel(
                  "Size Limit Scenarios",
                  
                  fileInput(
                    "size_csv",
                    "Upload Size-Limit CSV",
                    accept = ".csv"
                  ),
                  
                  tags$pre(
                    style = paste0(
                      "white-space: pre-wrap;",
                      "overflow-wrap: normal;",
                      "word-break: normal;",
                      "line-height: 1.55;",
                      "padding: 14px;",
                      "margin-top: 15px;"
                    ),
                    
                    "Example:
scenario_name,min_len_mm,max_len_mm
Minimum_9,228.6,1000
HarvestSlot_8_12,203.2,304.8
ProtectiveSlot_8_12,304.8,203.2

Tips:

1. All fish lengths must be entered in millimeters (mm).

2. Minimum-length limit:
   Enter the minimum legal length in min_len_mm and a sufficiently large upper value in max_len_mm.
   Example: Minimum_9,228.6,1000

3. Harvest slot:
   Fish within the interval may be retained. Enter the lower boundary in min_len_mm and the upper boundary in max_len_mm.
   Example: HarvestSlot_8_12,203.2,304.8

4. Protective slot:
   Fish within the interval are protected, while fish outside the interval may be retained. Enter the upper boundary in min_len_mm and the lower boundary in max_len_mm.
   Example: ProtectiveSlot_8_12,304.8,203.2

5. The scenario_name column can be named at your discretion."
                  )
                ),
                
                # ======================================================
                # 3. Experiment Design Inputs
                # ======================================================
                tabPanel(
                  "Experiment Design Inputs",
                  
                  helpText(
                    "Multiple comma-separated values may be entered for the ",
                    "applicable experiment-design inputs."
                  ),
                  
                  fluidRow(
                    shiny::column(
                      width = 6,
                      
                      tags$h5("Uncertainty"),
                      
                      textInput(
                        "ESD_vec",
                        "Environment stochasticity (ESD), comma-separated",
                        value = "0.3,0.6"
                      ),
                      
                      textInput(
                        "pae_vec",
                        paste0(
                          "Proportion of vulnerable fish with at least one ",
                          "annual angler encounter (PAE), comma-separated"
                        ),
                        value = "0.75"
                      ),
                      
                      textInput(
                        "rm_vec",
                        "Release mortality rate (RM), comma-separated",
                        value = "0.3,0.6"
                      ),
                      
                      tags$div(
                        style = paste0(
                          "color: #dc3545;",
                          "font-weight: bold;",
                          "margin-top: -10px;",
                          "margin-bottom: 10px;",
                          "font-size: 0.95em;"
                        ),
                        
                        icon("exclamation-circle"),
                        
                        paste0(
                          " Reminder: RM is catch-and-release mortality, ",
                          "the probability that a fish dies after being released ",
                          "because it is not legally retained under a size limit."
                        )
                      ),
                      
                      helpText(
                        "When values other than 0 are entered, the model adds 0 ",
                        "automatically as a baseline comparison. When only 0 is ",
                        "entered, release mortality is not included."
                      )
                    ),
                    
                    shiny::column(
                      width = 6,
                      
                      tags$h5("Policy Inputs"),
                      
                      checkboxGroupInput(
                        "compliance_mode",
                        "Size-policy compliance (select at least one)",
                        choices = c(
                          "Yes" = "yes",
                          "No" = "no"
                        ),
                        selected = "yes",
                        inline = TRUE
                      ),
                      
                      helpText(
                        "Selecting 'No' applies the size-specific compliance ",
                        "probabilities defined below rather than assuming zero ",
                        "compliance."
                      ),
                      
                      hr(),
                      
                      tags$h5("Compliance by Size Threshold"),
                      
                      textInput(
                        "comp_breaks",
                        paste0(
                          "Length breakpoints (mm), comma-separated ",
                          "(must start with 0 and increase)"
                        ),
                        value = "0,254"
                      ),
                      
                      textInput(
                        "comp_probs",
                        paste0(
                          "Compliance probabilities (0–1), comma-separated ",
                          "(same number of values as the breakpoints)"
                        ),
                        value = "0.5,0.25"
                      ),
                      
                      helpText(
                        "Example: Breakpoints entered as “0,200,300” with ",
                        "compliance probabilities entered as “0.7,0.5,0.3” ",
                        "indicate that anglers comply with harvest regulations ",
                        "70% of the time for fish < 200 mm, 50% of the time for ",
                        "fish from 200 to < 300 mm, and 30% of the time for fish ",
                        "≥ 300 mm."
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # (Simulation engine + fast-forward moved to Step 2: Run control)
        
        # ：Validation & Submission
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = "Validation & Submission", width = 12, status = "success", solidHeader = TRUE,
              helpText(icon("info-circle"), "Please click the button after you determine all sub-panel parameter entry."),
              shiny::actionButton(
                "submit_design", "Submit & Check Design",
                class = "btn-danger btn-lg",
                style = "background-color: #FF0000; border-color: #CC0000; font-weight: bold; color: white;",
                width = "100%", icon = icon("check-circle")
              ),
              br(), br(),
              verbatimTextOutput("log_step1_3")
            )
          )
        )
      ),
      
      
      tabItem(
        tabName = "combos",
        fluidRow(
          box(
            title = "Design preview", width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
            tags$p(style="color:#2c3e50;", "This page updates automatically based on inputs from the previous parameter pages."),
            
            tags$h4("1) Size limits"),
            DT::DTOutput("size_tbl"),
            hr(),
            
            tags$h4("2) Uncertainty (Scenarios: PAE, ESD, RM)"),
            DT::DTOutput("scen_preview_tbl"),
            helpText("Note: PAE = Prop of fish for annual angler encounters, ESD = Environment Stochasticity, RM = Release Mortality rate."),
            helpText("Label names (Output Folder) are the folder names for each uncertainty combination when saving simulation data files"),
            hr(),
            
            tags$h4("3) Size limit policy condition (Compliance and release mortality rate considered?)"),
            DT::DTOutput("combo_tbl"),
            tags$div(
              style = "display: block; margin-bottom: 6px;",
              helpText(
                "Each row represents one policy condition and corresponds to a separate ",
                "policy output file generated for every simulation iteration."
              )
            ),
            
            tags$div(
              style = "display: block; margin-bottom: 6px;",
              helpText(
                "The policy label shown here is also used in the output file name. ",
                "File naming format: iter####_policy_<policy label>.csv. ",
                "For example: iter0001_policy_1.csv and iter0001_policy_2.csv."
              )
            ),
            
            tags$div(
              style = "display: block;",
              helpText(
                "Policy definitions are saved in policy_combos_info.csv, and the ",
                "corresponding scenario settings are saved in scenario_info.csv. ",
                "In these files, 1 means Yes and 0 means No for the indicator fields ",
                "shown in the table above."
              )
            )
          )
        )
      ),
      
      # ============================================================
      # Step 2: Run control
      # ============================================================
      tabItem(
        tabName = "runcontrol",
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = "Run control", width = 12, status = "primary", solidHeader = TRUE,
              numericInput("n_iter", "Number of iterations (runs)", value = 5, min = 1, step = 1),
              numericInput("seed", "Random seed", value = 123, min = 1, step = 1)
            )
          )
        ),
        
        # ---- Google Cloud execution -------------------------------------
        # Configured once here and applied to the test runs in Step 3a and the
        # full run in Step 3b, so the same settings do not have to be repeated
        # on each page.
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = tagList(icon("cloud"), "Run on Google Cloud"),
              width = 12, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              
              helpText(
                "Runs the simulation on a machine you rent in your own Google",
                "Cloud project instead of on this computer. Useful when the",
                "population is too large for local memory, or when you would",
                "rather not tie up this machine. You are billed by Google for",
                "the time the machine runs."
              ),
              
              checkboxInput("use_cloud", "Use Google Cloud for simulations", value = FALSE),
              
              conditionalPanel(
                condition = "input.use_cloud == true",
                
                fileInput("gcp_key", "Service-account key (.json)",
                          accept = ".json", width = "100%"),
                helpText("The key stays on this computer. It is never saved into a settings file."),
                
                fluidRow(
                  shiny::column(6, textInput("gcp_project", "Project ID",
                                             placeholder = "my-fishery-project")),
                  shiny::column(6, textInput("gcp_region", "Region", value = "us-central1"))
                ),
                fluidRow(
                  shiny::column(6, textInput("gcp_bucket", "Storage bucket",
                                             placeholder = "my-craibm-data")),
                  shiny::column(6, textInput("gcp_machine_type", "Machine type",
                                             value = "n2-highmem-8"))
                ),
                textInput(
                  "gcp_container_image",
                  "Public GHCR container image",
                  value = Sys.getenv("CRAIBM_CLOUD_IMAGE", unset = ""),
                  placeholder = "ghcr.io/your-github-name/craibm:latest",
                  width = "100%"
                ),
                helpText(
                  "Choose a machine type in the Google Cloud console that suits",
                  "your memory and core needs. The container image is produced",
                  "by this package's GitHub workflow and must be public so Batch",
                  "can pull it without storing GitHub credentials."
                ),
                
                actionButton("cloud_check", "Check cloud connection",
                             class = "btn-info", width = "100%",
                             icon = icon("plug")),
                br(), br(),
                verbatimTextOutput("cloud_status_log")
              )
            )
          )
        ),
        
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = tagList(
                icon("project-diagram"),
                "Parallel acceleration"
              ),
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              helpText(
                "These three methods increase speed by running work concurrently.",
                "They may be combined, but their CPU and memory demands also multiply."
              ),
              
              wellPanel(
                style = paste0(
                  "background: #f0f4f8;",
                  "border-left: 4px solid #4a90d9;",
                  "padding: 10px;",
                  "margin-bottom: 16px;"
                ),
                tags$label(
                  icon("search"),
                  "Hardware overview:"
                ),
                verbatimTextOutput(
                  "gpu_detect_display",
                  placeholder = TRUE
                ),
                shiny::actionButton(
                  "btn_detect_gpu",
                  "Refresh Hardware",
                  class = "btn-info btn-sm",
                  icon = icon("sync"),
                  style = "margin-top: 5px;"
                )
              ),
              
              # --------------------------------------------------------
              # 1. Replicate parallelism
              # --------------------------------------------------------
              tags$div(
                style = paste0(
                  "border: 1px solid #d7e3f1;",
                  "border-radius: 8px;",
                  "background: #f8fbff;",
                  "padding: 16px;",
                  "margin-bottom: 14px;"
                ),
                
                fluidRow(
                  shiny::column(
                    width = 4,
                    
                    tags$h5(
                      icon("layer-group"),
                      strong("1. Replicate parallelism")
                    ),
                    
                    helpText(
                      "Runs different repetitions and scenarios simultaneously",
                      "in separate R worker processes."
                    ),
                    
                    tags$span(
                      class = "badge badge-info",
                      "Across repetitions"
                    )
                  ),
                  
                  shiny::column(
                    width = 8,
                    
                    sliderInput(
                      "n_cores",
                      "Parallel replicate workers",
                      min = 1,
                      max = 128,
                      value = max(
                        1,
                        floor(parallel::detectCores() / 2)
                      ),
                      step = 1
                    )
                  )
                )
              ),
              
              # --------------------------------------------------------
              # 2. Policy parallelism
              # --------------------------------------------------------
              tags$div(
                style = paste0(
                  "border: 1px solid #d7e3f1;",
                  "border-radius: 8px;",
                  "background: #f8fbff;",
                  "padding: 16px;",
                  "margin-bottom: 14px;"
                ),
                
                fluidRow(
                  shiny::column(
                    width = 4,
                    
                    tags$h5(
                      icon("tasks"),
                      strong("2. Policy parallelism")
                    ),
                    
                    helpText(
                      "Runs multiple management-policy combinations concurrently",
                      "within each active simulation."
                    ),
                    
                    checkboxInput(
                      "use_gpu",
                      "Enable policy parallelism",
                      value = FALSE
                    )
                  ),
                  
                  shiny::column(
                    width = 8,
                    
                    conditionalPanel(
                      condition = "input.use_gpu == true",
                      
                      sliderInput(
                        "gpu_thread_count",
                        "Policy-combo threads per active replicate",
                        min = 1,
                        max = 128,
                        value = 2,
                        step = 1
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.use_gpu == false",
                      
                      tags$div(
                        class = "alert alert-light",
                        style = "margin-top: 8px;",
                        icon("info-circle"),
                        "Policy combinations will run sequentially."
                      )
                    )
                  )
                )
              ),
              
              # --------------------------------------------------------
              # 3. Individual parallelism
              # --------------------------------------------------------
              tags$div(
                style = paste0(
                  "border: 1px solid #d7e3f1;",
                  "border-radius: 8px;",
                  "background: #f8fbff;",
                  "padding: 16px;"
                ),
                
                fluidRow(
                  shiny::column(
                    width = 4,
                    
                    tags$h5(
                      icon("fish"),
                      strong("3. Individual parallelism")
                    ),
                    
                    helpText(
                      "Uses the large-population optimized engine with OpenMP threading to split",
                      "fish-level survival calculations at each monthly time step."
                    ),
                    
                    checkboxInput(
                      "simulation_engine",
                      "Enable large-population optimization",
                      value = TRUE
                    )
                  ),
                  
                  shiny::column(
                    width = 8,
                    
                    conditionalPanel(
                      condition = "input.simulation_engine == true",
                      
                      sliderInput(
                        "omp_nthreads",
                        "Individual-level parallel threads",
                        min = 1,
                        max = 128,
                        value = 1,
                        step = 1
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.simulation_engine == false",
                      
                      tags$div(
                        class = "alert alert-light",
                        style = "margin-top: 8px;",
                        icon("info-circle"),
                        "The standard simulation engine will be used."
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # ============================================================
        # Non-parallel acceleration methods
        # ============================================================
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = tagList(
                icon("forward"),
                "Non-parallel acceleration"
              ),
              width = 12,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              helpText(
                "This method reduces the amount of simulation work without",
                "creating additional CPU workers or threads."
              ),
              
              tags$div(
                style = paste0(
                  "border: 1px solid #cfe8d5;",
                  "border-radius: 8px;",
                  "background: #f7fcf8;",
                  "padding: 16px;"
                ),
                
                fluidRow(
                  shiny::column(
                    width = 5,
                    
                    tags$h5(
                      icon("fish"),
                      strong("Reduced-memory early-life simulation")
                    ),
                    
                    tags$p(
                      "Newly recruited fish are initially tracked as a group rather than ",
                      "stored as separate individual fish records to speed-up the simulation and save memory."
                    ),
                    
                    tags$p(
                      "During this period, the model still applies monthly survival but ",
                      "records the growth history needed to reconstruct individual fish ",
                      "lengths later."
                    ),
                    
                    tags$p(
                      "Before any fish reaches a threshold that requires individual-level ",
                      "length or age processing, the surviving fish are converted into ",
                      "individual records and continue through the full simulation."
                    ),
                    
                    tags$details(
                      style = paste0(
                        "margin-top: 12px;",
                        "margin-bottom: 12px;",
                        "background-color: #ffffff;",
                        "border: 1px solid #d7eadb;",
                        "border-radius: 5px;",
                        "padding: 10px;"
                      ),
                      
                      tags$summary(
                        icon("info-circle"),
                        strong(" How is the automatic duration determined?"),
                        style = "cursor: pointer; color: #218838;"
                      ),
                      
                      tags$ul(
                        style = "margin-top: 10px; margin-bottom: 0; padding-left: 20px;",
                        
                        tags$li(
                          strong("Stock Size boundary: "),
                          "individual lengths must be available before fish can enter ",
                          "length-based fishing and monthly PSD calculations."
                        ),
                        
                        tags$li(
                          strong("Age and biology boundary: "),
                          "group tracking must stop before the earliest applicable ",
                          "Maturity Age, Transition Age, or Fishery Recruit Age (age-based vulnerability mode)."
                        ),
                        
                        tags$li(
                          strong("Automatic duration: "),
                          "the model uses the earlier of the stock-size boundary and ",
                          "the age/biology boundary."
                        )
                      )
                    ),
                    
                    tags$span(
                      class = "badge badge-success",
                      "Reduces memory and computation"
                    )
                  ),
                  
                  shiny::column(
                    width = 7,
                    
                    radioButtons(
                      "fast_forward_mode",
                      "Early-life simulation mode",
                      choices = c(
                        "Automatic — use the calculated safe duration" = "auto",
                        "Disabled — create individual fish immediately" = "off"
                      ),
                      selected = "auto",
                      inline = FALSE
                    ),
                    
                    
                    
                    uiOutput("t_safe_design_display")
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          shiny::column(
            width = 12,
            box(
              title = "Confirm Run Control", width = 12, status = "success", solidHeader = TRUE,
              helpText(icon("info-circle"),
                       "Review and confirm your run-control and acceleration settings before running the test simulation."),
              shiny::actionButton(
                "confirm_runcontrol", "Confirm Run Control",
                class = "btn-success btn-lg",
                width = "100%", icon = icon("check-circle")
              ),
              br(), br(),
              verbatimTextOutput("log_runcontrol")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "sim",
        
        fluidRow(
          box(
            title = "Step 3a: Test Simulation",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            bs4Dash::tabsetPanel(
              id = "test_sim_tabs",
              
              # ============================================================
              # About Step 3a
              # ============================================================
              tabPanel(
                "About Test Simulation",
                icon = icon("circle-info"),
                
                wellPanel(
                  style = paste0(
                    "background-color:#f8fbff;",
                    "border:1px solid #d7e8f7;",
                    "margin-top:15px;"
                  ),
                  
                  tags$h5("Test 1: Model Validation"),
                  
                  tags$p(
                    "Test 1 is a short model check. It runs one selected scenario, ",
                    "all policies for that scenario, and one replicate. Use the diagnostic ",
                    "plot to determine whether the selected simulation duration is adequate."
                  ),
                  
                  tags$p(
                    "Test 1 also provides a rough estimate of the full-model calculation time. ",
                    "The estimate is rough because this test runs only one model task and does ",
                    "not measure how much simultaneous workers may slow each other down."
                  ),
                  
                  tags$hr(),
                  
                  tags$h5("Test 2: Parallel Performance Check"),
                  
                  tags$p(
                    "Test 2 takes longer because it runs one model worker alone and then runs ",
                    "several model workers at the same time using the parallel settings confirmed ",
                    "in Step 2."
                  ),
                  
                  tags$p(
                    "This test is required before the full simulation. Without a memory-safety ",
                    "check, the full parallel plan could request more memory than the machine can ",
                    "provide, causing workers or the entire simulation to stop."
                  ),
                  
                  tags$p(
                    "Test 2 measures the actual parallel speed and memory use of the selected ",
                    "machine. It therefore provides a more accurate estimate of the full-model ",
                    "calculation time than Test 1."
                  ),
                  
                  tags$p(
                    "The required safety check uses a CPU-balanced worker group to reduce the ",
                    "risk of immediately overloading the machine. An optional full-load check ",
                    "can then test every configured replicate worker and provide the most accurate ",
                    "pre-run time estimate."
                  ),
                  
                  tags$p(
                    icon("book-open"),
                    " For technical definitions and calculation details, please see the Help Guide."
                  )
                )
              ),
              
              # ============================================================
              # Test 1
              # ============================================================
              tabPanel(
                "Test 1: Model Validation",
                icon = icon("chart-line"),
                
                br(),
                
                fluidRow(
                  shiny::column(
                    width = 4,
                    
                    tags$div(
                      class = "alert alert-info",
                      style = "padding:10px;",
                      
                      tags$b("What does this test do?"),
                      tags$br(),
                      
                      "Runs one selected scenario, all of its policies, and one ",
                      "replicate. It checks model output and provides a rough ",
                      "estimate of the full-model calculation time."
                    ),
                    
                    uiOutput("test_selectors"),
                    
                    selectInput(
                      "test_var_y",
                      "Variable to Plot:",
                      choices = c(
                        "Spawning fish density" = "Sden",
                        "Recruit density" = "Rden",
                        "Adult abundance" = "AdultN",
                        "Recruit (fishery) abundance" = "AgeFRN",
                        "Yield (number)" = "Yield_n",
                        "Population size" = "N_pop",
                        "PSD (Quality)" = "PSD_Q",
                        "PSD (Preferred)" = "PSD_P",
                        "PSD (Memorable)" = "PSD_M",
                        "PSD (Trophy)" = "PSD_T",
                        "Angler Encounters (Quality)" = "Enc_Q",
                        "Angler Encounters (Preferred)" = "Enc_P",
                        "Angler Encounters (Memorable)" = "Enc_M",
                        "Angler Encounters (Trophy)" = "Enc_T",
                        "Months of Trophy Seen" = "trophy_seen"
                      )
                    ),
                    
                    actionButton(
                      "run_test_sim",
                      "Run Model Validation",
                      class = "btn-success",
                      width = "100%",
                      icon = icon("play")
                    ),
                    
                    br(),
                    actionButton(
                      "stop_test1_cloud",
                      "Stop Test 1 Cloud Job",
                      class = "btn-danger",
                      width = "100%",
                      icon = icon("stop"),
                      disabled = "disabled"
                    ),
                    
                    uiOutput("run_lock_note_test1"),
                    uiOutput("cloud_watch_panel"),
                    
                    br(),
                    
                    uiOutput("cloud_validation_controls"),
                    
                    tags$h5("Test 1 Report"),
                    verbatimTextOutput("log_step2a")
                  ),
                  
                  shiny::column(
                    width = 8,
                    
                    tags$h4("Model Validation Plot"),
                    
                    plotOutput(
                      "test_sim_plot",
                      height = "500px"
                    ),
                    
                    helpText(
                      "This plot shows one simulated trajectory. Use it to determine ",
                      "whether the selected burn-in, stable, and policy periods are ",
                      "long enough for the intended analysis."
                    )
                  )
                )
              ),
              
              # ============================================================
              # Test 2
              # ============================================================
              tabPanel(
                "Test 2: Parallel Performance Check",
                icon = icon("microchip"),
                
                br(),
                
                fluidRow(
                  shiny::column(
                    width = 4,
                    
                    tags$div(
                      class = "alert alert-info",
                      style = "padding:10px;",
                      
                      tags$b("What does this test do?"),
                      tags$br(),
                      
                      "Runs a small amount of the real model using the parallel ",
                      "settings confirmed in Step 2. It checks whether simultaneous ",
                      "model workers improve speed and whether the full plan can fit ",
                      "in memory."
                    ),
                    
                    radioButtons(
                      "perf_test_mode",
                      "Choose the performance-check level:",
                      
                      choices = stats::setNames(
                        object = c(
                          "safe",
                          "full"
                        ),
                        
                        nm = c(
                          paste0(
                            "Required safety check — use a CPU-balanced worker group ",
                            "(recommended first)"
                          ),
                          
                          paste0(
                            "Optional full-load check — use every configured replicate worker ",
                            "(higher load and more accurate time estimate)"
                          )
                        )
                      ),
                      
                      selected = "safe"
                    ),
                    
                    uiOutput("perf_test_mode_note"),
                    
                    actionButton(
                      "run_oversub_test",
                      "Run Parallel Performance Check",
                      class = "btn-warning",
                      width = "100%",
                      icon = icon("microchip")
                    ),
                    
                    br(),
                    actionButton(
                      "stop_test2_cloud",
                      "Stop Test 2 Cloud Job",
                      class = "btn-danger",
                      width = "100%",
                      icon = icon("stop"),
                      disabled = "disabled"
                    ),
                    
                    uiOutput("run_lock_note_test2"),
                    uiOutput("cloud_watch_panel"),
                    
                    helpText(
                      tags$b(
                        "This test must pass before running the full simulation."
                      )
                    ),
                    
                    br(),
                    
                    uiOutput("cloud_perf_controls")
                  ),
                  
                  shiny::column(
                    width = 8,
                    
                    tags$h4("Speed and Memory Report"),
                    
                    tags$div(
                      class = "alert alert-light",
                      style = "padding:9px;",
                      
                      "The report explains whether the selected parallel settings ",
                      "are efficient, whether the estimated memory use is safe, and ",
                      "approximately how long the full simulation may take."
                    ),
                    
                    verbatimTextOutput("log_oversub"),
                    
                    helpText(
                      icon("book-open"),
                      " For definitions and calculation details, please see the Help Guide."
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      
      
      tabItem(
        tabName = "run_save",
        fluidRow(
          
          shiny::column(width = 6,
                        box(
                          title = "Simulation Control",
                          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          
                          tags$label("Output Folder Path:"),
                          div(style="display:flex; gap:10px;",
                              textInput("out_dir", label = NULL, value =file.path("~", "CRAIBM_Results"), width="100%"),
                              
                              # [] shinyDirButton actionButton
                              # id = "browse_dir_run" (Server )
                              # label = "Browse..." ()
                              # title = "Select Output Folder" ()
                              shinyFiles::shinyDirButton("browse_dir_run", "Browse...", "Select Output Folder",
                                                         class = "btn-secondary", icon = icon("folder-open"))
                          ),
                          helpText("Click 'Browse' to select a folder to save output running data, or create a new file path manually."),
                          hr(),
                          
                          
                          checkboxInput("overwrite_existing", "Overwrite existing files if folder exists (Please be careful, this action will delete all existing files in the folder)", value = FALSE),
                          
                          
                          br(),
                          
                          # In cloud mode nothing is computed on this machine,
                          # so the run mode has no bearing on the run.
                          conditionalPanel(
                            condition = "input.use_cloud == true",
                            tags$div(
                              class = "alert alert-info",
                              style = "padding: 8px; margin-bottom: 10px;",
                              icon("cloud"),
                              tags$b(" Cloud mode is on."),
                              tags$br(),
                              "The simulation runs on your rented machine, so the run mode ",
                              "below and the output folder do not apply. Results are packaged ",
                              "in the cloud and downloaded when the run finishes."
                            )
                          ),
                          
                          # --- Run Mode Selector ---
                          conditionalPanel(
                            condition = "input.use_cloud != true",
                            tags$div(
                              style = "margin-bottom: 10px;",
                              tags$label("Run Mode:"),
                              tags$div(
                                style = "display: flex; gap: 20px; margin-top: 6px;",
                                
                                # Foreground option with tooltip
                                tags$div(
                                  title = paste0(
                                    "Foreground mode:\n",
                                    "✅ MRuns directly in the current Shiny/R session.\n",
                                    "✅ Most compatible (recommended for cloud hosting like shinyapps.io / restricted environments).\n",
                                    "⚠️ The UI may freeze during the run, and Stop cannot force-cancel once started (you must wait for completion)."
                                  ),
                                  style = "cursor: help;",
                                  tags$label(
                                    style = "cursor: help; font-weight: normal;",
                                    tags$input(
                                      type = "radio",
                                      name = "run_mode",
                                      id   = "run_mode_fg",
                                      value = "foreground",
                                      checked = "checked",
                                      style = "margin-right: 5px;"
                                    ),
                                    icon("desktop"), " Foreground mode"
                                  )
                                ),
                                
                                # Background option with tooltip
                                tags$div(
                                  title = paste0(
                                    "Background mode (Run as a separate R process):\n",
                                    "✅ MLaunches a separate R process (the app stays responsive).\n",
                                    "✅ UI stays responsive; Stop can terminate the run immediately (kills the background process).\n",
                                    "⚠️ May be blocked or unstable on managed school/work computers (security policies can restrict spawning processes), and may not be supported on some cloud platforms."
                                  ),
                                  style = "cursor: help;",
                                  tags$label(
                                    style = "cursor: help; font-weight: normal;",
                                    tags$input(
                                      type = "radio",
                                      name = "run_mode",
                                      id   = "run_mode_bg",
                                      value = "background",
                                      style = "margin-right: 5px;"
                                    ),
                                    icon("server"), " Background mode"
                                  )
                                )
                              ),
                              # Hidden input updated by JS so Shiny can read it
                              tags$script(HTML("
                       $(document).on('change', 'input[name=run_mode]', function() {
                         Shiny.setInputValue('run_mode', $(this).val(), {priority: 'event'});
                       });
                       // Set initial value
                       $(document).ready(function() {
                         Shiny.setInputValue('run_mode', 'foreground');
                       });
                     "))
                            ),
                          ),
                          
                          uiOutput("run_lock_note_full"),
                          
                          shiny::actionButton("start_batch", "Start Simulation Run", class = "btn-success btn-lg", width = "100%", icon=icon("rocket")),
                          br(), br(),
                          shiny::actionButton("stop_batch", "Stop Simulation", class = "btn-danger", width = "100%", icon=icon("stop")),
                          uiOutput("cloud_watch_panel"),
                          
                          # Cloud controls appear only while cloud mode is on.
                          conditionalPanel(
                            condition = "input.use_cloud == true",
                            hr(),
                            uiOutput("cloud_run_controls")
                          ),
                          
                          hr(),
                          
                          
                          tags$h4("System Log:"),
                          verbatimTextOutput("batch_log")
                        )
          ),
          
          
          shiny::column(width = 6,
                        box(
                          title = "Task Distribution Preview",
                          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                          verbatimTextOutput("task_preview"),
                          helpText("This shows how iterations will be distributed among cores.")
                        ),
                        box(
                          title = "Folder Structure Preview",
                          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                          tags$pre(
                            "Output_Folder/
  sim_info.log.txt      <<----Good if there is nothing in this file
  size_<scenario_name>__min<min size in mm>__max<max size in mm>/
    scenario_info.csv
    policy_combos_info.csv
    iter0001_before_policy.csv
    iter0001_policy_1.csv
    iter0001_policy_2.csv
    ...
    iter0002_before_policy.csv
    ..."
                          ),
                          helpText("Data is split: 'before_policy' files contain the pre-policy phase and real burn in phase, 'policy_X' files contain the post-policy phase for each policy combo.")
                        )
          )
        )
      ),
      
      
      # ================== Step 3: Results (NEW) ==================
      tabItem(tabName = "results",
              fluidRow(
                box(title = "1. Load & Select Data", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                      shiny::column(6,
                                    tags$label("Output Folder Path:"),
                                    div(style="display:flex; gap:10px;",
                                        textInput("res_out_dir", label = NULL, value = file.path("~", "CRAIBM_Results"),  width="100%"),
                                        shinyFiles::shinyDirButton("browse_output", "Browse...", "Select Output Folder",
                                                                   class = "btn-secondary", icon = icon("folder-open"))
                                    ),
                                    shiny::actionButton("load_results", "Load Scenarios", class = "btn-info", width = "100%")
                      ),
                      shiny::column(6,
                                    uiOutput("result_scen_selector"),
                                    textOutput("res_scen_desc")
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = "2. Visualization Controls", width = 4, status = "warning", solidHeader = TRUE,
                    selectInput("res_var_y", "Variable to Plot:",
                                choices = c(
                                  "Spawning fish density" = "Sden",
                                  "Recruit density" = "Rden",
                                  "Adult abundance" = "AdultN",
                                  "Recruit (fishery) abundance" = "AgeFRN",
                                  "Yield (number)" = "Yield_n",
                                  "Population size" = "N_pop",
                                  "PSD (Quality)" = "PSD_Q",
                                  "PSD (Preferred)" = "PSD_P",
                                  "PSD (Memorable)" = "PSD_M",
                                  "PSD (Trophy)" = "PSD_T",
                                  "Angler Encounters (Quality)" = "Enc_Q",
                                  "Angler Encounters (Preferred)" = "Enc_P",
                                  "Angler Encounters (Memorable)" = "Enc_M",
                                  "Angler Encounters (Trophy)"= "Enc_T",
                                  "Months of Trophy Seen"="trophy_seen" 
                                )),
                    tags$label("Burn-in years (Blue Line):"),
                    fluidRow(
                      shiny::column(8, 
                                    numericInput("res_burn_in", label = NULL, value = 1, min = 0)
                      ),
                      shiny::column(4, 
                                    # Style: margin-top aligns button with input box
                                    shiny::actionButton("btn_update_burnin", "Change", 
                                                        class = "btn-primary", width = "100%")
                      )
                    ),
                    tags$div(
                      class = "alert alert-light",
                      style = "margin-top: 10px; margin-bottom: 0;",
                      
                      tags$p(
                        style = "margin-bottom: 6px;",
                        strong("In the Result Plot:")
                      ),
                      
                      tags$ul(
                        style = "margin-bottom: 0; padding-left: 20px;",
                        
                        tags$li(
                          strong("Blue line: "),
                          "The model burn-in end year. Set this year manually using the ",
                          "“Burn-in years (Blue Line)” field above."
                        ),
                        
                        tags$li(
                          strong("Red line: "),
                          "The size-limit policy start year. This value is detected ",
                          "automatically from the loaded simulation data."
                        )
                      )
                    )
                ),
                box(title = "Result Plot & Legend", width = 8, status = "warning", solidHeader = TRUE,
                    plotOutput("res_main_plot", height = "500px"),
                    hr(),
                    h4("Policy Information and Statistics"),
                    DT::DTOutput("res_policy_tbl")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  start_times <- Sys.time()
  cpp_abs_path <- NULL
  
  old_warn <- getOption("warn")
  options(warn = -1)
  session$onSessionEnded(function() {
    options(warn = old_warn)
    try(.cloud_stop_clock(), silent = TRUE)
  })
  
  vals <- reactiveValues(theta_clean = NULL, growth_data = NULL, z_dist = NULL, alk_data = NULL,
                         alk_source = NULL, alk_info = NULL, growth_fit_note = NULL,
                         alk_seed = NULL, alk_bin_width = NULL,
                         vbgf_seed = NULL, z_seed = NULL, loaded_size_csv = NULL)
  res_policy_year <- reactiveVal(0)
  # parse_num_vec() defined in R/helper.R
  
  # Background process state controller
  proc_state <- reactiveValues(
    job                  = NULL,
    is_running           = FALSE,
    bg_out_dir           = NULL,
    bg_cores             = NULL,
    bg_settings_log_line = NULL,
    # Cloud run state
    cloud_auth           = NULL,
    cloud_verified       = FALSE,
    cloud_release_offer  = FALSE,
    active_run           = NULL,
    active_run_mode      = NULL,
    cloud_job_id         = NULL,
    cloud_watch_job      = NULL,
    cloud_task_type      = NULL,
    cloud_status         = NULL,   # submitted / running / done / failed / cancelled
    cloud_done           = NA_integer_,
    cloud_total          = NA_integer_,
    cloud_result_uri     = NULL,
    cloud_poll_fails     = 0L,
    cloud_submitted_at   = NULL,
    cloud_queue_warned   = FALSE,
    cloud_last_report    = NULL,
    cloud_no_progress    = 0L,
    cloud_perf_requested = NA_integer_,
    cloud_perf_probe     = NA_integer_
  )
  
  # ===== Hardware / thread detection =====
  
  gpu_info_rv <- reactiveVal(list(
    gpu_available = FALSE, gpu_name = "Not yet detected",
    gpu_platform = "N/A", gpu_memory_mb = 0, gpu_type = "none",
    cpu_cores_logical = max(1L, parallel::detectCores(logical = TRUE))
  ))
  
  observe({
    info <- tryCatch(detect_gpu_r(), error = function(e) {
      list(gpu_available = FALSE, gpu_name = paste("Error:", e$message),
           gpu_platform = "N/A", gpu_memory_mb = 0, gpu_type = "none",
           cpu_cores_logical = max(1L, parallel::detectCores(logical = TRUE)))
    })
    gpu_info_rv(info)
  })
  
  observeEvent(input$btn_detect_gpu, {
    info <- tryCatch(detect_gpu_r(), error = function(e) {
      list(gpu_available = FALSE, gpu_name = paste("Error:", e$message),
           gpu_platform = "N/A", gpu_memory_mb = 0, gpu_type = "none",
           cpu_cores_logical = max(1L, parallel::detectCores(logical = TRUE)))
    })
    gpu_info_rv(info)
    if (isTRUE(info$gpu_available)) {
      showNotification(paste("Hardware refreshed. Graphics device:", info$gpu_name), type = "message")
    } else {
      showNotification("Hardware refreshed. Internal acceleration uses CPU threads in the current implementation.", type = "message")
    }
  })
  
  output$gpu_detect_display <- renderText({
    info <- gpu_info_rv()
    cpu_cores <- max(1L, parallel::detectCores(logical = TRUE))
    cpu_phys  <- max(1L, parallel::detectCores(logical = FALSE))
    if (is.na(cpu_cores)) cpu_cores <- 4L
    if (is.na(cpu_phys))  cpu_phys  <- 2L
    
    gpu_line <- if (isTRUE(info$gpu_available)) {
      paste0("✅ GPU: ", info$gpu_name,
             if (info$gpu_memory_mb > 0) paste0(" (", info$gpu_memory_mb, " MB)") else "",
             "\n   Platform: ", info$gpu_platform,
             "\n   Type: ", if (!is.null(info$gpu_type)) info$gpu_type else "unknown")
    } else {
      "ℹ️ Graphics device: not detected (simulation parallelism uses CPU threads)"
    }
    
    omp_info <- if (exists("detect_openmp_info", mode = "function")) {
      tryCatch(detect_openmp_info(), error = function(e) NULL)
    } else NULL
    omp_line <- if (!is.null(omp_info) && isTRUE(omp_info$openmp_available)) {
      paste0("✅ OpenMP: enabled (max threads reported: ", omp_info$max_threads, ")")
    } else {
      "⚠️ OpenMP: not enabled in this build; individual-level parallelism will use one thread."
    }
    
    paste0(
      gpu_line, "\n",
      "💻 CPU: ", cpu_phys, " physical / ", cpu_cores, " logical cores\n",
      omp_line
    )
  })
  
  # ===== END Hardware Detection Logic =====
  
  selected_T_safe <- reactive({
    
    mode <- if (
      is.null(input$fast_forward_mode)
    ) {
      "auto"
    } else {
      input$fast_forward_mode
    }
    
    # Disabled means recruits are converted to individual records immediately.
    if (identical(mode, "off")) {
      return(0L)
    }
    
    # Automatic mode always uses the model-calculated safe duration.
    auto_safe <- if (
      !is.null(vals$T_safe_info) &&
      !is.null(vals$T_safe_info$T_safe)
    ) {
      suppressWarnings(
        as.integer(vals$T_safe_info$T_safe)
      )
    } else {
      0L
    }
    
    if (
      length(auto_safe) == 0L ||
      is.na(auto_safe)
    ) {
      return(0L)
    }
    
    max(0L, auto_safe)
  })
  
  output$t_safe_design_display <- renderUI({
    
    info <- vals$T_safe_info
    
    format_months <- function(x) {
      x <- suppressWarnings(as.integer(x))
      
      if (
        length(x) == 0L ||
        is.na(x[1])
      ) {
        return("Unavailable")
      }
      
      paste0(x[1], " month(s)")
    }
    
    # ------------------------------------------------------------
    # VBGF has not been completed
    # ------------------------------------------------------------
    if (!isTRUE(sys_status$vbgf_ok)) {
      
      return(
        tags$div(
          class = "alert alert-secondary",
          icon("info-circle"),
          strong("Automatic duration is not available yet."),
          tags$br(),
          "Complete and confirm the Growth (VBGF) analysis first. ",
          "The VBGF results are required to estimate how quickly the ",
          "fastest-growing fish may reach Stock Size."
        )
      )
    }
    
    # ------------------------------------------------------------
    
    # ------------------------------------------------------------
    if (
      !isTRUE(sys_status$global_ok) ||
      is.null(info)
    ) {
      
      return(
        tags$div(
          class = "alert alert-secondary",
          icon("info-circle"),
          strong("Automatic duration is not available yet."),
          tags$br(),
          "Review the juvenile density-dependent growth settings, then click ",
          strong("Submit & Check Parameters"),
          " under Global Parameters."
        )
      )
    }
    
    # ------------------------------------------------------------
    # Calculation was attempted but failed
    # ------------------------------------------------------------
    if (!is.null(info$error_msg)) {
      
      return(
        tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          strong("The automatic duration could not be calculated."),
          tags$br(),
          "Recheck the submitted VBGF results and juvenile ",
          "density-dependent growth settings, then submit the ",
          "Global Parameters again."
        )
      )
    }
    
    # ------------------------------------------------------------
    # Successful calculation
    # ------------------------------------------------------------
    vulnerability_note <- if (
      identical(input$f_age_mode, "age")
    ) {
      paste0(
        "Age-based fishing is selected, so Fishery Recruit Age is also ",
        "included in the age/biology boundary."
      )
    } else {
      paste0(
        "Length-based fishing is selected, so fish must have individual ",
        "length records before reaching Stock Size."
      )
    }
    
    tags$div(
      class = "alert alert-info",
      
      strong("Automatic group-tracking duration: "),
      format_months(info$T_safe),
      
      tags$br(),
      "New recruits can remain in reduced-memory group form for this period. ",
      "Afterward, the surviving fish are converted into individual records.",
      
      tags$hr(),
      
      strong("Stock-size boundary: "),
      format_months(info$T_length),
      tags$br(),
      tags$small(
        "This boundary ensures that individual lengths are available before ",
        "length-based fishing or monthly PSD calculations require them."
      ),
      
      tags$br(),
      tags$br(),
      
      strong("Age and biology boundary: "),
      format_months(info$T_age),
      tags$br(),
      tags$small(
        "This boundary prevents group tracking from passing an age at which ",
        "maturity, juvenile-to-adult transition, or age-based fishing may ",
        "change the required model processes."
      ),
      
      tags$br(),
      tags$br(),
      
      vulnerability_note,
      
      tags$hr(),
      
      strong("Current selected duration: "),
      format_months(selected_T_safe())
    )
  })
  
  
  output$perf_test_mode_note <- renderUI({
    
    if (identical(input$perf_test_mode, "full")) {
      
      tags$div(
        class = "alert alert-warning",
        style = "padding:8px;",
        
        tags$b("Full-load check"),
        tags$br(),
        
        "This option runs every replicate worker configured in Step 2. ",
        "It places a heavier load on the machine but provides the most ",
        "accurate pre-run time estimate. A memory pre-check is performed ",
        "before concurrent workers are launched."
      )
      
    } else {
      
      tags$div(
        class = "alert alert-info",
        style = "padding:8px;",
        
        tags$b("Required safety check"),
        tags$br(),
        
        "This option selects a CPU-balanced worker group based on the machine's ",
        "available CPU capacity and the number of threads used by each worker. ",
        "It measures real memory use and estimates whether the complete parallel ",
        "plan is safe before the full simulation can start."
      )
    }
  })
  # run_selected_cpp() now lives in the package (helper.R) and is exported, so
  # the same dispatcher can be called from the Shiny session, from parallel
  # workers, and from a cloud container. It is referenced here unqualified and
  # resolves through the loaded craibm namespace.
  
  # [Server Init]
  
  sys_status <- reactiveValues(
    # 1. Status Booleans
    vbgf_ok     = FALSE,
    alk_ok      = FALSE,
    z_ok      = FALSE,
    survival_ok= FALSE,
    global_ok   = FALSE,
    design_ok   = FALSE,
    runcontrol_ok = FALSE,
    test_run_done          = FALSE,
    mem_safe              = NA,
    memory_check_done      = FALSE,
    memory_retest_required = FALSE,
    loaded_from            = NULL,
    log_cloud              = NULL,
    cloud_summary          = NULL,
    
    # 2. Messages
    msg_intro = paste0(
      "==========================================\n",
      "   Welcome to Sportfish IBM Builder!   \n",
      "==========================================\n",
      "Checklist Status:\n"
    ),
    msg_vbgf = "1. [ ] Growth (VBGF)   : ⚪ Waiting for data...",
    msg_alk  = "2. [ ] ALK Data        : ⚪ Waiting for upload...",
    
    
    log_1_2   = "Waiting for Global Params submission...\n",
    log_1_3   = "Waiting for Design submission...\n",
    log_runcontrol = "Waiting for run control confirmation...\n",
    log_surv  = "⚪ Waiting for survival data submission...\n",
    log_2a    = "Waiting...\n",
    log_oversub = "Waiting for Parallel Performance Check...\n",
    log_2b    = "Waiting...\n",
    log_3     = "Waiting to load...\n",
    batch_log = "Standby. Waiting for command..."
    
  )
  
  get_missing_setup_steps <- function() {
    
    missing_steps <- character()
    
    if (!isTRUE(sys_status$vbgf_ok)) {
      missing_steps <- c(
        missing_steps,
        "Growth (VBGF)"
      )
    }
    
    if (!isTRUE(sys_status$alk_ok)) {
      missing_steps <- c(
        missing_steps,
        "ALK Data"
      )
    }
    
    if (!isTRUE(sys_status$global_ok)) {
      missing_steps <- c(
        missing_steps,
        "Global Parameters"
      )
    }
    
    if (!isTRUE(sys_status$design_ok)) {
      missing_steps <- c(
        missing_steps,
        "Design Scenarios"
      )
    }
    
    # Run-control confirmation is machine/session dependent and is
    # deliberately not trusted after loading a saved file.
    if (!isTRUE(sys_status$runcontrol_ok)) {
      missing_steps <- c(
        missing_steps,
        "Run Control (Step 2)"
      )
    }
    
    # The parallel benchmark must be repeated on the current machine.
    if (
      is.null(sys_status$mem_safe) ||
      length(sys_status$mem_safe) == 0L ||
      is.na(sys_status$mem_safe)
    ) {
      missing_steps <- c(
        missing_steps,
        paste0(
          "Parallel performance check (Step 3a) — ",
          "run it at least once"
        )
      )
    }
    
    missing_steps
  }
  
  output$settings_load_log <- renderUI({
    
    # The panel tracks setup progress whether the data was entered here or
    # restored from a settings file, so it is only hidden before anything at
    # all has been supplied.
    missing_steps <- get_missing_setup_steps()
    
    nothing_yet <- length(missing_steps) >= 6L &&
      is.null(sys_status$loaded_from)
    if (nothing_yet) {
      return(NULL)
    }
    
    # Shown only when the session came from a saved file.
    restored_line <- if (is.null(sys_status$loaded_from)) {
      ""
    } else {
      paste0("\u2705 Settings file restored.\n", sys_status$loaded_from, "\n\n")
    }
    
    # ------------------------------------------------------------
    # Incomplete setup
    # ------------------------------------------------------------
    if (length(missing_steps) > 0L) {
      
      border_color <- "#ffc107"
      
      status_text <- paste0(
        restored_line,
        "🚧 Setup is incomplete.\n",
        "Missing:\n - ",
        paste(
          missing_steps,
          collapse = "\n - "
        )
      )
      
      # ------------------------------------------------------------
      # Previous benchmark failed
      # ------------------------------------------------------------
    } else if (identical(sys_status$mem_safe, FALSE)) {
      
      border_color <- "#dc3545"
      
      status_text <- paste0(
        restored_line,
        "🛑 The most recent parallel performance check ",
        "produced a red result.\n",
        "Reduce the parallel load and run the check again."
      )
      
      # ------------------------------------------------------------
      # Everything required is ready
      # ------------------------------------------------------------
    } else {
      
      border_color <- "#28a745"
      
      status_text <- paste0(
        restored_line,
        "✅ Setup is complete and ready for a full run."
      )
    }
    
    tags$pre(
      style = paste0(
        "white-space: pre-wrap;",
        "word-break: break-word;",
        "font-size: 10.5px;",
        "line-height: 1.35;",
        "background-color: #f8f9fa;",
        "color: #343a40;",
        "border: 1px solid #ced4da;",
        "border-left: 4px solid ", border_color, ";",
        "border-radius: 4px;",
        "padding: 7px;",
        "margin: 6px 0 0 0;",
        "max-height: 190px;",
        "overflow-y: auto;"
      ),
      status_text
    )
  })
  
  # Any run-control change invalidates the previous confirmation and benchmark.
  # After a completed memory benchmark, the revised plan must be benchmarked again.
  observeEvent(
    list(
      input$n_iter,
      input$seed,
      input$n_cores,
      input$use_gpu,
      input$gpu_thread_count,
      input$simulation_engine,
      input$omp_nthreads,
      input$fast_forward_mode
    ),
    {
      had_memory_check <- isTRUE(sys_status$memory_check_done) ||
        identical(sys_status$mem_safe, FALSE) ||
        isTRUE(sys_status$memory_retest_required)
      
      if (had_memory_check) {
        sys_status$memory_retest_required <- TRUE
      }
      
      sys_status$memory_check_done <- FALSE
      sys_status$mem_safe <- NA
      sys_status$runcontrol_ok <- FALSE
      sys_status$test_run_done <- FALSE
      sys_status$log_runcontrol <- paste0(
        "⚠️ Run-control settings changed.\n",
        "Confirm Run Control again before testing or starting the full simulation."
      )
    },
    ignoreInit = TRUE
  )
  
  output$step1_info_box <- renderText({
    txt<-paste(
      sys_status$msg_intro,
      sys_status$msg_vbgf,
      sys_status$msg_alk,
      sep = "\n"
    )
    unname(txt)
  })
  
  session$onFlushed(function() {
    end_times <- Sys.time()
    elapsed_time <- as.numeric(difftime(end_times, start_times, units = "secs"))
    remaining_time <- 5 - elapsed_time
    if (remaining_time > 0) {
      Sys.sleep(remaining_time)
    }
    waiter::waiter_hide()
    
  }, once = TRUE)
  enable_login <- FALSE
  
  if (isTRUE(enable_login)) {
    
    showModal(modalDialog(
      title = tagList(icon("shield-alt"), "Authorized Access Only"),
      tags$p("Please enter your official credentials to access the model."),
      textInput("username", "Username", placeholder = "e.g., user_name"),
      passwordInput("password", "Password", placeholder = "Enter password"),
      footer = shiny::actionButton(
        "login_btn",
        "Secure Log In",
        class = "btn-danger",
        width = "100%"
      ),
      easyClose = FALSE,
      fade = TRUE
    ))
    
    valid_users <- data.frame(
      username = c("jim", "admin", "siufishery", "chen"),
      password = c("crappie", "crappie2026", "fish123", "admin888"),
      stringsAsFactors = FALSE
    )
    
    observeEvent(input$login_btn, {
      match_row <- valid_users[
        valid_users$username == input$username &
          valid_users$password == input$password,
      ]
      
      if (nrow(match_row) > 0) {
        removeModal()
        showNotification(
          paste("Authentication Successful! Welcome,", input$username),
          type = "message"
        )
      } else {
        showNotification(
          "Invalid Credentials. Access Denied.",
          type = "error"
        )
      }
    })
  }
  
  # Logic 1: Growth (VBGF)
  
  # ---- Missing-age detection on the uploaded length-age file ----------------
  growth_upload <- reactive({
    req(input$file_growth)
    tryCatch(
      as.data.frame(readr::read_csv(input$file_growth$datapath, show_col_types = FALSE)),
      error = function(e) NULL
    )
  })
  
  # Number of fish whose age is missing in the uploaded file.
  missing_age_n <- reactive({
    df <- growth_upload()
    if (is.null(df) || !("Age" %in% names(df))) return(0L)
    sum(is.na(suppressWarnings(as.numeric(df$Age))))
  })
  
  # Shown only when the file actually contains fish without an age.
  output$missing_age_choice <- renderUI({
    n_na <- missing_age_n()
    if (is.null(n_na) || n_na < 1L) return(NULL)
    
    df <- growth_upload()
    n_total <- if (is.null(df)) NA_integer_ else nrow(df)
    
    tagList(
      div(
        class = "alert alert-warning",
        style = "padding: 8px; margin-top: 4px; margin-bottom: 8px;",
        icon("exclamation-triangle"),
        tags$b(paste0(" ", n_na, " of ", n_total, " fish have no age.")),
        tags$br(),
        "Their ages will be estimated from an age-length key built from the",
        "fish that do have one. This also fills in the Age-length Key box",
        "below, so no separate ALK file is needed."
      ),
      radioButtons(
        "missing_age_mode",
        "Fit the growth curve to:",
        choices = c(
          "Observed ages only" = "observed",
          "Observed and estimated ages" = "all"
        ),
        selected = "observed"
      ),
      tags$hr(style = "margin-top: 6px; margin-bottom: 10px;")
    )
  })
  
  # The age-assignment seed only matters when there are ages to estimate.
  output$alk_seed_input <- renderUI({
    if (missing_age_n() < 1L) {
      return(helpText("This file has no missing ages, so no age-assignment seed is needed."))
    }
    numericInput("alk_seed_manual", "Age-assignment seed",
                 value = NA, min = 1, step = 1)
  })
  
  # ---- Age-length key preview shown next to the checklist -------------------
  output$alk_preview_block <- renderUI({
    if (is.null(vals$alk_data)) return(NULL)
    
    src_line <- if (identical(vals$alk_source, "auto")) {
      div(
        class = "alert alert-success",
        style = "padding: 8px; margin-bottom: 8px;",
        icon("wand-magic-sparkles"),
        tags$b(" Generated automatically from the estimated ages."),
        if (!is.null(vals$alk_info)) tags$div(style = "margin-top:4px;", vals$alk_info) else NULL
      )
    } else {
      NULL
    }
    
    tagList(
      tags$hr(),
      h5(icon("table"), " Age-length key data"),
      src_line,
      DT::dataTableOutput("alk_table_preview")
    )
  })
  
  output$alk_table_preview <- DT::renderDT({
    req(vals$alk_data)
    DT::datatable(
      vals$alk_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",
        scrollX = TRUE
      )
    )
  })
  
  # Download button for an automatically generated key.
  output$auto_alk_note <- renderUI({
    if (!identical(vals$alk_source, "auto") || is.null(vals$alk_data)) return(NULL)
    tagList(
      div(
        class = "alert alert-success",
        style = "padding: 8px; margin-top: 10px; margin-bottom: 6px;",
        icon("check-circle"),
        tags$b(" Filled in automatically."),
        tags$br(),
        "This key was built from the estimated ages, so no upload is needed."
      ),
      downloadButton("download_auto_alk", "Download this ALK (.csv)",
                     class = "btn-success", style = "width:100%;")
    )
  })
  
  output$download_auto_alk <- downloadHandler(
    filename = function() {
      paste0("age_length_key_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      utils::write.csv(vals$alk_data, file, row.names = FALSE)
    }
  )
  
  # An empty or invalid advanced seed box means "draw a fresh seed".
  manual_seed <- function(x) {
    v <- suppressWarnings(as.integer(x))
    if (length(v) == 0L || is.na(v) || v < 1L) NULL else v
  }
  
  observeEvent(input$run_vbgf, {
    vals$theta_clean <- NULL
    vals$growth_data <- NULL
    vals$vbgf_seed <- NULL
    sys_status$vbgf_ok <- FALSE
    
    if (is.null(input$file_growth)) {
      sys_status$msg_vbgf <- "1. [❌] Growth (VBGF)   : No file uploaded yet."
      showNotification("Please upload a Length-Age CSV file first.", type = "warning")
      return()
    }
    
    sys_status$msg_vbgf <- "1. [ ] Growth (VBGF)   : ⏳ Checking & Running..."
    updateTabsetPanel(session, "tab_diag", selected = "Welcome")
    
    runtime_logs <- character(0)
    
    tryCatch({
      
      
      
      df <- as.data.frame(readr::read_csv(input$file_growth$datapath, show_col_types = FALSE))
      
      # validation , input$file_growth
      # check_vbgf_inputs(file_obj, df, boot_b)
      chk_in <- check_vbgf_inputs(input$file_growth, df, input$boot_b_vbgf)
      
      if(!chk_in$pass) {
        sys_status$msg_vbgf <- paste0("1. [❌] Growth (VBGF)   : Input Error.\n      ", gsub("\n", " ", chk_in$msg))
        return()
      }
      
      # ---- Fish without an age -----------------------------------------
      # Their ages are always estimated from an age-length key, because that
      # key is what fills in the Age-length Key section. The user's choice
      # only decides whether the growth curve is fitted to the observed ages
      # alone or to the observed and estimated ages together.
      n_missing <- sum(is.na(suppressWarnings(as.numeric(df$Age))))
      age_note <- ""
      
      if (n_missing > 0L) {
        alk_seed_in <- manual_seed(input$alk_seed_manual)
        imp <- impute_ages_alk(df, seed = alk_seed_in)
        completed <- imp$data
        
        # The seed is kept so the age assignment can be reported now and
        # saved with the rest of the settings later.
        vals$alk_seed      <- imp$seed
        vals$alk_bin_width <- imp$bin_width
        
        # The completed data also gives us the age-length key itself.
        alk_df <- build_alk_summary(completed)
        vals$alk_data   <- alk_df
        vals$alk_source <- "auto"
        vals$alk_info   <- paste0(
          "Built from ", nrow(completed), " fish (", imp$n_aged, " aged, ",
          imp$n_imputed, " estimated) across ", nrow(alk_df), " age classes."
        )
        sys_status$alk_ok  <- TRUE
        sys_status$msg_alk <- paste0(
          "2. [✅] ALK Data        : Ready! Generated from the estimated ages.",
          "\n      Age-assignment seed: ", imp$seed,
          if (!is.null(alk_seed_in)) " (set manually)" else ""
        )
        
        use_estimated <- identical(input$missing_age_mode, "all")
        if (use_estimated) {
          df <- completed
        }
        # Otherwise df keeps its missing ages and the bootstrap drops them.
        
        age_note <- paste0(
          "\n      Ages estimated for ", imp$n_imputed, " fish",
          " (key from ", imp$n_aged, " aged fish, ",
          "length classes of ", imp$bin_width, ").",
          if (isTRUE(imp$n_filled > 0L)) {
            paste0("\n      ", imp$n_filled,
                   " length class(es) had no aged fish and borrowed the nearest class.")
          } else "",
          if (imp$n_dropped > 0L) {
            paste0("\n      ", imp$n_dropped,
                   " fish were smaller than the key and were set aside.")
          } else "",
          "\n      Growth curve fitted to: ",
          if (use_estimated) "observed and estimated ages." else "observed ages only."
        )
        
        showNotification(
          paste0("Missing ages estimated (seed ", imp$seed,
                 "). The age-length key was filled in automatically."),
          type = "message",
          duration = 8
        )
      }
      
      vals$growth_fit_note <- age_note
      
      vbgf_seed_in <- manual_seed(input$vbgf_seed_manual)
      
      res <- withCallingHandlers({
        run_vbgf_bootstrap_full(df, B = input$boot_b_vbgf, phi_obs = 0.1,
                                seed = vbgf_seed_in)
      }, warning = function(w) {
        runtime_logs <<- c(runtime_logs, w$message)
        invokeRestart("muffleWarning")
      })
      
      unique_warns <- unique(runtime_logs)
      warn_msg_block <- ""
      if(length(unique_warns) > 0) {
        display_warns <- head(unique_warns, 3)
        warn_msg_block <- paste0("\n      ⚠️ Runtime Warnings:\n      - ", paste(display_warns, collapse="\n      - "))
        if(length(unique_warns) > 3) warn_msg_block <- paste0(warn_msg_block, "\n      ... and ", length(unique_warns)-3, " more.")
      }
      
      if(is.null(res) || is.null(res$Theta_clean)) {
        sys_status$msg_vbgf <- paste0("1. [❌] Growth (VBGF)   : Fit Failed.", warn_msg_block)
        return()
      }
      
      chk_out <- check_boot_outcomes(res$Theta_clean, input$boot_b_vbgf)
      if(!chk_out$pass) {
        sys_status$msg_vbgf <- paste0("1. [❌] Growth (VBGF)   : Result Error.\n      ", gsub("\n", " ", chk_out$msg), warn_msg_block)
        return()
      }
      
      full_theta <- res$Theta_clean
      bounds <- apply(full_theta, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
      keep_idx <- (full_theta$Linf >= bounds[1,"Linf"] & full_theta$Linf <= bounds[2,"Linf"]) &
        (full_theta$K    >= bounds[1,"K"]    & full_theta$K    <= bounds[2,"K"])    &
        (full_theta$t0   >= bounds[1,"t0"]   & full_theta$t0   <= bounds[2,"t0"])
      
      vals$theta_clean <- full_theta[keep_idx, ]
      vals$growth_data <- res$Data
      vals$vbgf_seed   <- res$seed
      
      sys_status$vbgf_ok <- TRUE
      
      final_status_note <- if(warn_msg_block != "") " (With Warnings)" else ""
      sys_status$msg_vbgf <- paste0(
        "1. [✅] Growth (VBGF)   : Ready!", final_status_note,
        "\n      Kept ", nrow(vals$theta_clean), " runs.",
        if (!is.null(res$seed)) {
          paste0("\n      Bootstrap seed: ", res$seed,
                 if (!is.null(vbgf_seed_in)) " (set manually)" else "")
        } else "",
        if (!is.null(vals$growth_fit_note)) vals$growth_fit_note else "",
        warn_msg_block
      )
      
      updateTabsetPanel(session, "tab_diag", selected = "Growth (VBGF)")
      
    }, error = function(e) {
      sys_status$msg_vbgf <- paste0("1. [❌] Growth (VBGF)   : Critical Error!\n      ", e$message)
    })
  })
  
  
  # VBGF Plotting Logic (, Server )
  output$plot_vbgf <- renderPlot({
    req(vals$theta_clean)
    
    df_params <- as.data.frame(vals$theta_clean)
    df_long <- df_params %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value")
    
    stats_df <- df_long %>%
      group_by(Parameter) %>%
      summarise(
        p025 = quantile(Value, 0.025, na.rm = TRUE),
        p500 = median(Value, na.rm = TRUE),
        p975 = quantile(Value, 0.975, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(cols = c(p025, p500, p975), names_to = "Quantile", values_to = "Xintercept")
    
    ggplot(df_long, aes(x = Value, fill = Parameter)) +
      geom_histogram(aes(y = after_stat(density)), color = "black", alpha = 0.5, bins = 30) +
      geom_density(alpha = 0.5, adjust = 1.5, linewidth = 1) +
      geom_vline(data = stats_df, aes(xintercept = Xintercept),
                 linetype = "dashed", color = "red", linewidth = 0.8) +
      facet_wrap(~Parameter, scales = "free", ncol = 3) +
      scale_fill_brewer(palette = "Set2") +
      theme_bw(base_size = 14) +
      theme(legend.position = "none",
            strip.background = element_rect(fill = "#f8f9fa"),
            strip.text = element_text(face = "bold")) +
      labs(title = "VBGF Parameter Distributions (Truncated 95%)",
           subtitle = "Red Dashed Lines: 2.5%, 50%, 97.5% quantiles",
           x = "Parameter Value", y = "Density")
  })
  
  output$summary_vbgf <- renderPrint({
    req(vals$theta_clean)
    
    df <- as.data.frame(vals$theta_clean)
    
    stat_fun <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) {
        return(c(N = 0, Mean = NA, SD = NA, P2.5 = NA, P25 = NA, P50 = NA, P75 = NA, P97.5 = NA))
      }
      c(
        N     = length(x),
        Mean  = mean(x),
        SD    = if (length(x) > 1) sd(x) else 0,
        P2.5  = as.numeric(quantile(x, 0.025, names = FALSE)),
        P25   = as.numeric(quantile(x, 0.25,  names = FALSE)),
        P50   = as.numeric(quantile(x, 0.50,  names = FALSE)),
        P75   = as.numeric(quantile(x, 0.75,  names = FALSE)),
        P97.5 = as.numeric(quantile(x, 0.975, names = FALSE))
      )
    }
    
    
    out <- t(vapply(df, stat_fun, FUN.VALUE = numeric(8)))
    out <- as.data.frame(out)
    
    out[] <- lapply(out, function(x) ifelse(is.nan(x), NA, x))
    print(round(out, 4))
  })
  
  
  
  
  # Logic 2: ALK & Weight
  
  observeEvent(input$submit_alk, {
    
    if (is.null(input$file_alk)) {
      # An automatically generated key is already in place, so there is
      # nothing to upload; simply confirm it.
      if (identical(vals$alk_source, "auto") && !is.null(vals$alk_data)) {
        sys_status$alk_ok  <- TRUE
        sys_status$msg_alk <- "2. [✅] ALK Data        : Ready! Generated from the estimated ages."
        showNotification(
          "This age-length key was generated from the estimated ages; no upload is needed.",
          type = "message"
        )
        return()
      }
      
      vals$alk_data <- NULL
      vals$alk_source <- NULL
      vals$alk_info <- NULL
      sys_status$alk_ok <- FALSE
      sys_status$msg_alk <- "2. [❌] ALK Data        : No file uploaded yet."
      showNotification("Please upload an ALK CSV file first.", type = "warning")
      return()
    }
    
    vals$alk_data <- NULL
    vals$alk_source <- NULL
    vals$alk_info <- NULL
    sys_status$alk_ok <- FALSE
    
    sys_status$msg_alk <- "2. [ ] ALK Data        : ⏳ Verifying..."
    updateTabsetPanel(session, "tab_diag", selected = "Welcome") # Welcome
    
    tryCatch({
      
      df <- as.data.frame(readr::read_csv(input$file_alk$datapath, show_col_types = FALSE))
      
      # helper.R ( check_alk_inputs validation.R/helper.R )
      # check_alk_inputs df,
      
      chk <- check_alk_inputs(input$file_alk, df)
      
      if(!chk$pass) {
        
        sys_status$msg_alk <- paste0("2. [❌] ALK Data        : Invalid.\n      ", gsub("\n", " ", chk$msg))
        showNotification("ALK Data Validation Failed!", type = "error")
      } else {
        
        vals$alk_data <- df
        vals$alk_source <- "file"
        vals$alk_info <- NULL
        vals$alk_seed <- NULL
        vals$alk_bin_width <- NULL
        sys_status$alk_ok <- TRUE
        sys_status$msg_alk <- "2. [✅] ALK Data        : Ready!"
        showNotification("ALK Data Verified Successfully!", type = "message")
      }
      
    }, error = function(e) {
      sys_status$msg_alk <- paste0("2. [❌] ALK Data        : Error - ", e$message)
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  
  # Logic 3: Mortality (Z)
  
  output$z_status_display <- renderText({
    sys_status$msg_z
  })
  
  
  observeEvent(input$run_z, {
    vals$z_dist <- NULL
    vals$z_seed <- NULL
    sys_status$z_ok <- FALSE
    
    sys_status$msg_z <- "[ ] Mortality (Z)   : ⏳ Estimating..."
    
    
    runtime_logs_z <- character(0)
    
    tryCatch({
      if(is.null(vals$alk_data)) {
        sys_status$msg_z <- "[❌] Mortality (Z)   : Missing ALK data!Please go back to Step 1 and submit ALK data first!"
        return()
      }
      
      chk_in <- check_z_inputs(vals$alk_data, input$min_adult_age, input$z_last, input$z_boot_bg2)
      if(!chk_in$pass) {
        sys_status$msg_z <- paste0("[❌] Mortality (Z)   : Invalid Params.\n      ", gsub("\n", " ", chk_in$msg))
        return()
      }
      
      # A catch curve needs several age classes along the descending limb.
      # With only one or two the regression cannot be fitted, so adult
      # mortality has to be entered directly instead.
      chk_cc <- check_catch_curve_data(
        vals$alk_data, input$min_adult_age, input$z_last
      )
      
      if (!chk_cc$pass) {
        sys_status$msg_z <- paste0(
          "[❌] Mortality (Z)   : Not enough age classes for a catch curve.\n      ",
          "Only ", chk_cc$n_ages, " usable age class(es) between the Transition Age (",
          input$min_adult_age, ") and the Catch Curve Max Age (", input$z_last, ").",
          "\n      Switched to 'Fixed Adult Annual M': enter the adult mortality directly.",
          "\n      The simulation can still run this way."
        )
        
        # Force the direct-input route, since estimation is not possible.
        updateCheckboxInput(session, "use_z_estimation", value = FALSE)
        
        showNotification(
          paste0("Only ", chk_cc$n_ages,
                 " age class(es) available: Z cannot be estimated from a catch curve. ",
                 "Please enter a Fixed Adult Annual M instead."),
          type = "error",
          duration = 12
        )
        return()
      }
      
      # The mortality bootstrap is random; a seed is recorded so this Z
      # distribution can be reproduced and saved with the settings.
      z_seed_in   <- manual_seed(input$z_seed_manual)
      z_seed_used <- if (is.null(z_seed_in)) sample.int(999999L, 1L) else z_seed_in
      
      z_res <- withCallingHandlers({
        withProgress(message = 'Calculating Z...', detail = 'Bootstrapping Catch Curve...', value = 0.5, {
          run_z_bootstrap_custom(
            raw_data = vals$alk_data,          # ALK
            BG2      = input$z_boot_bg2,
            full     = input$min_adult_age,
            last     = input$z_last,
            method   = input$z_method,
            seed     = z_seed_used
          )
        })
      }, warning = function(w) {
        runtime_logs_z <<- c(runtime_logs_z, w$message)
        invokeRestart("muffleWarning")
      })
      
      
      unique_warns <- unique(runtime_logs_z)
      warn_msg_block <- ""
      if(length(unique_warns) > 0) {
        display_warns <- head(unique_warns, 2)
        warn_msg_block <- paste0("\n      ⚠️ Warnings: ", paste(display_warns, collapse="; "))
        if(length(unique_warns) > 2) warn_msg_block <- paste0(warn_msg_block, " (+", length(unique_warns)-2, " more)")
      }
      
      if(!is.null(z_res) && !all(is.na(z_res))) {
        z_df <- data.frame(Z = z_res)
        chk_out <- check_boot_outcomes(z_df, input$z_boot_bg2)
        
        if(!chk_out$pass) {
          sys_status$msg_z <- paste0("[❌] Mortality (Z)   : Calculation Error.\n      ", gsub("\n", " ", chk_out$msg), warn_msg_block)
          return()
        }
        
        clean_z <- z_res[!is.na(z_res)]
        vals$z_dist <- clean_z
        vals$z_seed <- z_seed_used
        
        sys_status$z_ok <- TRUE
        
        sys_status$msg_z <- paste0(
          "[✅] Mortality (Z)   : Ready! (", length(vals$z_dist), " runs)",
          "\n      Bootstrap seed: ", z_seed_used,
          if (!is.null(z_seed_in)) " (set manually)" else "",
          warn_msg_block
        )
        
        
      } else {
        sys_status$msg_z <- paste0("[❌] Mortality (Z)   : Failed (All runs NA).", warn_msg_block)
      }
      
    }, error = function(e) {
      sys_status$msg_z <- paste0("[❌] Mortality (Z)   : Critical Error!\n      ", e$message)
    })
  })
  
  # Z Plotting Logic ( Server )
  output$plot_z <- renderPlot({
    req(vals$z_dist)
    df_plot <- data.frame(Z = vals$z_dist)
    
    p025 <- as.numeric(quantile(vals$z_dist, 0.025, na.rm = TRUE))
    p500 <- median(vals$z_dist, na.rm = TRUE)
    p975 <- as.numeric(quantile(vals$z_dist, 0.975, na.rm = TRUE))
    
    my_theme <- theme_bw(base_size = 14) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom"
      )
    
    
    
    ggplot(df_plot, aes(x = Z)) +
      geom_histogram(aes(y = after_stat(density)), fill = "#E69F00", color = "white", bins = 30, alpha = 0.8) +
      geom_density(color = "#D55E00", linewidth = 1.2) +
      geom_vline(xintercept = c(p025, p500, p975),
                 linetype = "dashed", color = "black", linewidth = 1) +
      theme_minimal(base_size = 14) +
      labs(title = paste0("Z Distribution (Truncated 95%) - Method: ", input$z_method),
           subtitle = paste0("Lines at: ", round(p025,3), " (2.5%), ", round(p500,3), " (Med), ", round(p975,3), " (97.5%)"),
           x = "Instantaneous Mortality (Z)", y = "Density")+
      my_theme+
      scale_x_continuous(limits = c(0, NA)) 
  })
  output$summary_z <- renderPrint({
    req(vals$z_dist)
    
    z <- as.numeric(vals$z_dist)
    z <- z[!is.na(z)]
    
    if (length(z) == 0) {
      cat("Z summary: no valid values.\n")
      return(invisible(NULL))
    }
    
    out <- data.frame(
      N     = length(z),
      Mean  = mean(z),
      SD    = if (length(z) > 1) sd(z) else 0,
      P2.5  = as.numeric(quantile(z, 0.025, names = FALSE)),
      P25   = as.numeric(quantile(z, 0.25,  names = FALSE)),
      P50   = as.numeric(quantile(z, 0.50,  names = FALSE)),
      P75   = as.numeric(quantile(z, 0.75,  names = FALSE)),
      P97.5 = as.numeric(quantile(z, 0.975, names = FALSE))
    )
    
    print(round(out, 4))
  })  
  
  
  
  observeEvent(input$submit_survival, {
    
    logs <- c("🔍 Starting Survival Parameter Validation...\n")
    all_pass <- TRUE     
    # ---------------------------------------------------------
    # Check 1: Part A (Juvenile M)
    # ---------------------------------------------------------
    chk_juv <- check_num(input$juv_annual_M, "Juvenile Annual M", min_val = 0.001)
    
    if (!is.null(chk_juv)) {
      
      logs <- c(logs, chk_juv) 
      all_pass <- FALSE
    } else {
      
      logs <- c(logs, paste0("✅ Part A: Juvenile M (", input$juv_annual_M, ") is valid.\n"))
    }
    
    # ---------------------------------------------------------
    # Check 2: Part B (Adult M)
    # ---------------------------------------------------------
    if (input$use_z_estimation) {
      
      if (!sys_status$z_ok || is.null(vals$z_dist)) {
        logs <- c(logs, "❌ Part B Error: Z estimation selected but not run.\n   Action: Click the [Calculate Z] button on the right first!\n")
        all_pass <- FALSE
      } else {
        
        chk_ratio <- check_num(input$F_over_Z_ratio, "M/Z Ratio", min_val = 0.01, max_val = 0.99)
        if (!is.null(chk_ratio)) {
          logs <- c(logs, paste0("❌ Part B Error: ", chk_ratio))
          all_pass <- FALSE
        } else {
          
          mean_z <- mean(vals$z_dist, na.rm=TRUE)
          implied_m <- mean_z * input$F_over_Z_ratio
          logs <- c(logs, paste0("✅ Part B: Z Estimation Ready (Mean Z = ", round(mean_z, 3), ").\n"))
          logs <- c(logs, paste0("   -> Implied Adult M = ", round(implied_m, 4), "\n"))
        }
      }
      
    } else {
      
      chk_fixed <- check_num(input$fixed_adult_M, "Fixed Adult M", min_val = 0.001)
      
      if (!is.null(chk_fixed)) {
        logs <- c(logs, paste0("❌ Part B Error: ", chk_fixed))
        all_pass <- FALSE
      } else {
        fixed_val <- input$fixed_adult_M
        vals$z_dist <- runif(1000, min = fixed_val - 0.0001, max = fixed_val + 0.0001)
        sys_status$z_ok <- TRUE
        logs <- c(logs, paste0("✅ Part B: Fixed Adult M (", input$fixed_adult_M, ") is valid.\n"))
      }
    }
    
    sys_status$survival_ok <- all_pass     
    if (all_pass) {
      logs <- c(logs, "\n🎉 SUCCESS: Survival parameters are confirmed and locked!")
      showNotification("Survival Parameters Saved!", type = "message")
    } else {
      logs <- c(logs, "\n⛔ FAILED: Please fix the errors above and try again.")
      showNotification("Validation Failed", type = "error")
    }
    
    
    output$log_survival <- renderText({
      paste(logs, collapse = "")
    })
  })
  
  
  
  # Logic 4: Global Parameters (Submit & Check)
  
  
  sys_status$log_1_2 <- "⚪ Waiting for submission..."
  output$log_step1_2 <- renderText({ sys_status$log_1_2 })
  
  observeEvent(input$submit_global, {
    sys_status$global_ok <- FALSE
    sys_status$log_1_2 <- "⏳ Checking parameters..."
    
    
    sys_cores <- parallel::detectCores(logical = TRUE)
    user_cores <- input$n_cores
    final_cores <- user_cores
    core_warning <- ""
    
    
    if (!is.na(sys_cores) && user_cores > sys_cores) {
      
      safe_cores <- max(1, sys_cores - 2)
      final_cores <- safe_cores
      
      # 1. UI ,
      updateSliderInput(session, "n_cores", value = safe_cores)
      
      
      core_warning <- paste0(
        "\n⚠️ Hardware Warning:\n",
        "   You requested ", user_cores, " cores, but this system only has ", sys_cores, ".\n",
        "   Auto-adjusted to ", safe_cores, " (System - 2) to prevent crash.\n"
      )
      
      
      showNotification(paste0("Cores reduced to ", safe_cores, " to match hardware!"), type = "warning", duration = 5)
    }
    
    # 2. (n_iter UI input, final_cores input$n_cores)
    use_curve <- isTRUE(input$flag_harvest_curve)
    
    if (use_curve) {
      # A: -> UI
      val_h_L50   <- input$harv_L50
      val_h_pmax  <- input$harv_pmax
      val_h_slope <- input$harv_slope
    } else {
      # B: 
      val_h_L50   <- -1000.0             
      val_h_pmax  <- input$harv_fixed_pmax
      val_h_slope <- 1000.0            
    }
    
    params_list <- list(
      # 1. Run Control
      n_iter              = input$n_iter,
      seed                = input$seed,
      # 2. Timeline (New IDs)
      burn_in_years       = input$transient_years, # UI ID: transient_years
      stable_years        = input$stable_years,    # UI ID: stable_years
      policy_years        = input$policy_years,
      # 3. Density Dependent
      use_dd_survival     = input$use_dd_survival,
      surv_a              = input$surv_a,
      surv_b              = input$surv_b,
      surv_d1             = input$surv_d_avg1,
      surv_d2             = input$surv_d_avg2,
      
      # 4. Harvest
      flag_harvest_curve  = use_curve,
      harv_L50            = val_h_L50,
      harv_pmax           = val_h_pmax,
      harv_slope          = val_h_slope,
      month_weights       = input$month_weights,
      
      # 5. Environment & Life History (New IDs!)
      lake_area_ha        = input$lake_area_ha,
      initial_pop_size    = input$initial_pop_size,
      
      rec_a               = input$rec_a,
      rec_b              = input$rec_b,
      
      spawn_month         = input$spawn_month,
      recruit_entry_month = input$recruit_entry_month,
      
      # New Ages & Modes
      age_spawn           = input$age_spawn,
      min_adult_age       = input$min_adult_age,
      age_recruit         = input$z_full,         # UI ID: z_full -> Internal: age_recruit
      f_age_mode          = input$f_age_mode,
      
      # PSD Values
      psd_stock           = input$psd_stock,
      psd_quality         = input$psd_quality,
      psd_preferred       = input$psd_preferred,
      psd_memorable       = input$psd_memorable,
      psd_trophy          = input$psd_trophy,
      
      # 6. Status Checks 
      survival_ok         = sys_status$survival_ok
    )
    
    # 3. Validation
    chk <- check_global_inputs(params_list)
    
    # 4. ( Core Warning )
    sys_status$log_1_2 <- paste0(chk$msg, core_warning)
    
    if (chk$pass) {
      sys_status$global_ok <- TRUE
      
      # ===== Compute T_safe for juvenile fast-forward =====
      t_safe_result <- tryCatch({
        # C++ growth_1 is juvenile growth. In the UI the juvenile controls use g2_* IDs.
        use_juvenile_dd <- isTRUE(input$use_dd_effects) && isTRUE(input$use_dd_growth_juv)
        
        # Fast-forward must stop before maturity or the juvenile/adult biology
        # transition. In age-based mode it must also stop before fishery recruitment.
        safe_age_bound <- min(input$min_adult_age, input$age_spawn)
        if (identical(input$f_age_mode, "age")) {
          safe_age_bound <- min(safe_age_bound, input$z_full)
        }
        
        compute_T_safe(
          theta_clean   = vals$theta_clean,
          juv_onlyM_len = input$psd_stock,
          min_adult_age = safe_age_bound,
          age_recruit   = 0.0,
          g1_a          = input$g2_a,
          g1_b          = input$g2_b,
          g1_c          = input$g2_c,
          g1_d_avg      = input$g2_d_avg,
          use_dd_growth = use_juvenile_dd
        )
      }, error = function(e) {
        list(T_safe = 0L, T_length = 0L, T_age = 0L,
             limiting_factor = "error", error_msg = e$message)
      })
      
      vals$T_safe_info <- t_safe_result
    } else {
      showNotification("Validation Failed. See log.", type = "error")
    }
  })
  
  
  # Logic Step 1-3: Experiment Design (Submit & Jump)
  
  
  sys_status$log_1_3 <- "⚪ Waiting for design submission..."
  output$log_step1_3 <- renderText({ sys_status$log_1_3 })
  
  # reactiveVal CSV ,
  # ： check
  design_csv_data <- reactiveVal(NULL)
  
  # Size limits defined below in Logic 5
  
  observeEvent(input$submit_design, {
    sys_status$design_ok <- FALSE
    sys_status$log_1_3 <- "⏳ Verifying design inputs..."
    
    # 1. CSV ( tryCatch)
    df <- NULL
    
    if (!is.null(input$size_csv)) {
      
      df <- tryCatch(
        as.data.frame(
          readr::read_csv(
            input$size_csv$datapath,
            show_col_types = FALSE
          )
        ),
        error = function(e) {
          sys_status$log_1_3 <- paste0(
            "❌ Error reading CSV: ",
            conditionMessage(e)
          )
          NULL
        }
      )
      
    } else if (!is.null(vals$loaded_size_csv)) {
      
      df <- as.data.frame(vals$loaded_size_csv)
    }
    
    # 2. Validation
    chk <- check_design_inputs(
      file_obj   = input$size_csv,
      df         = df,
      esd_str    = input$ESD_vec,
      pae_str    = input$pae_vec,
      rm_str     = input$rm_vec,
      breaks_str = input$comp_breaks,
      probs_str  = input$comp_probs,
      comp_mode  = input$compliance_mode
    )
    
    # (Engine / OpenMP / fast-forward checks moved to Step 2: Run control)
    
    sys_status$log_1_3 <- chk$msg
    
    if (chk$pass) {
      
      sys_status$design_ok <- TRUE
      design_csv_data(df) # CSV
      vals$loaded_size_csv <- df
      showNotification("Design Verified! Jumping to Preview...", type = "message", duration = 2)
      
      # Step 1: Design preview"
      # tabName "combos"
      # ： UI dashboardSidebar(id = "sidebarMenu", ...)
      updateTabItems(session, "sidebarMenu", selected = "combos")
      
    } else {
      
      showNotification("Design Validation Failed!", type = "error")
    }
  })
  
  
  # ===== Step 2: Run control confirm + check =====
  output$log_runcontrol <- renderText({ sys_status$log_runcontrol })
  
  observeEvent(input$confirm_runcontrol, {
    sys_status$runcontrol_ok <- FALSE
    
    if (
      isTRUE(sys_status$memory_check_done) ||
      identical(sys_status$mem_safe, FALSE)
    ) {
      sys_status$memory_retest_required <- TRUE
    }
    
    sys_status$test_run_done <- FALSE
    sys_status$memory_check_done <- FALSE
    sys_status$mem_safe <- NA
    sys_status$log_runcontrol <- "⏳ Verifying run control..."
    
    logical_cores <- max(1L, parallel::detectCores(logical = TRUE))
    if (is.na(logical_cores)) logical_cores <- 4L
    
    craibm_ns <- asNamespace("craibm")
    
    engine_available <- exists(
      "run_simulation_v2_cpp",
      envir = craibm_ns,
      mode = "function",
      inherits = FALSE
    )
    detect_openmp_fun <- get0(
      "detect_openmp_info",
      envir = craibm_ns,
      mode = "function",
      inherits = FALSE,
      ifnotfound = NULL
    )
    
    omp_info <- if (!is.null(detect_openmp_fun)) {
      tryCatch(
        detect_openmp_fun(),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    openmp_available <- !is.null(omp_info) && isTRUE(omp_info$openmp_available)
    openmp_max <- if (!is.null(omp_info) && !is.null(omp_info$max_threads)) {
      omp_info$max_threads
    } else {
      NA_integer_
    }
    
    auto_safe <- if (!is.null(vals$T_safe_info) && !is.null(vals$T_safe_info$T_safe)) {
      as.integer(vals$T_safe_info$T_safe)
    } else {
      NA_integer_
    }
    
    rc_inputs <- list(
      n_iter                = input$n_iter,
      seed                  = input$seed,
      n_cores               = input$n_cores,
      use_policy_parallel   = isTRUE(input$use_gpu),
      policy_threads        = input$gpu_thread_count,
      use_large_pop = isTRUE(input$simulation_engine),
      omp_threads           = input$omp_nthreads,
      engine_available      = engine_available,
      openmp_available      = openmp_available,
      openmp_max            = openmp_max,
      fast_forward_mode     = input$fast_forward_mode,
      t_safe_manual         = NULL,
      t_safe_auto           = auto_safe,
      logical_cores         = logical_cores,
      # Passed so capacity is judged against the machine that will run the
      # work, which in cloud mode is not this one.
      use_cloud             = isTRUE(input$use_cloud),
      cloud_machine_type    = input$gcp_machine_type
    )
    
    chk <- check_runcontrol_inputs(rc_inputs)
    
    # ---- Parallel methods + worker load plan summary (shown in the log) ----
    if (isTRUE(chk$pass)) {
      rc_policy <- if (isTRUE(input$use_gpu) && !is.null(input$gpu_thread_count) &&
                       as.integer(input$gpu_thread_count) > 0L) {
        as.integer(input$gpu_thread_count)
      } else 1L
      rc_use_large <- isTRUE(input$simulation_engine)
      rc_omp <- if (rc_use_large) max(1L, as.integer(input$omp_nthreads)) else 1L
      
      rc_ff_mode <- if (is.null(input$fast_forward_mode)) "auto" else input$fast_forward_mode
      rc_ff_months <- if (!is.null(vals$T_safe_info) && !is.null(vals$T_safe_info$T_safe)) {
        as.integer(vals$T_safe_info$T_safe)
      } else NA_integer_
      
      # Work out the concurrency the same way the execution preview does.
      scen_try2 <- try(get_scenarios_df(), silent = TRUE)
      n_scen2 <- if (inherits(scen_try2, "try-error") || is.null(scen_try2)) 1L else nrow(scen_try2)
      n_iter2 <- max(1L, as.integer(input$n_iter))
      total2 <- n_scen2 * n_iter2
      cfg_workers2 <- min(max(1L, as.integer(input$n_cores)), total2)
      eff_workers2 <- cfg_workers2
      
      # Per-worker job load.
      wcores2 <- min(eff_workers2, total2)
      wsplit <- parallel::splitIndices(total2, wcores2)
      wlines <- vapply(seq_len(wcores2), function(w)
        sprintf("   Worker %02d: %d run(s)", w, length(wsplit[[w]])), character(1))
      
      peak2 <- eff_workers2 * rc_policy * rc_omp
      
      plan_msg <- paste0(
        "\n\n========================================\n",
        "⚙️  PARALLEL METHODS IN USE\n",
        "   1. Replicate parallelism: ", eff_workers2, " active worker(s)\n",
        "   2. Policy parallelism: ",
        if (rc_policy > 1L) paste0("ON, ", rc_policy, " thread(s)/replicate")
        else "OFF", "\n",
        "   3. Individual parallelism: ",
        if (rc_omp > 1L) paste0("ON, ", rc_omp, " thread(s)/model")
        else "OFF", "\n",
        "🧮 Peak concurrent threads: ", eff_workers2, " x ", rc_policy, " x ",
        rc_omp, " = ", peak2, "\n",
        "🐟 Juvenile fast-forward: ",
        if (identical(rc_ff_mode, "off")) "OFF"
        else paste0("ON (", if (is.na(rc_ff_months)) "auto" else paste0(rc_ff_months, " month(s)"), ")"),
        "\n",
        "========================================\n",
        "👷 WORKER LOAD PLAN\n",
        paste(wlines, collapse = "\n")
      )
      chk$msg <- paste0(chk$msg, plan_msg)
    }
    
    sys_status$log_runcontrol <- chk$msg
    
    if (isTRUE(chk$pass)) {
      sys_status$runcontrol_ok <- TRUE
      showNotification("Run Control Verified!", type = "message")
    } else {
      sys_status$runcontrol_ok <- FALSE
      showNotification("Run Control Validation Failed!", type = "error")
    }
  })
  
  
  
  
  get_size_limits <- reactive({
    
    df <- NULL
    
    if (!is.null(input$size_csv)) {
      df <- tryCatch(
        as.data.frame(
          readr::read_csv(
            input$size_csv$datapath,
            show_col_types = FALSE
          )
        ),
        error = function(e) NULL
      )
    }
    
    if (is.null(df) && !is.null(vals$loaded_size_csv)) {
      df <- as.data.frame(vals$loaded_size_csv)
    }
    
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }
    
    nm <- names(df)
    nm <- gsub("\u00A0", " ", nm, fixed = TRUE)
    nm <- tolower(trimws(nm))
    names(df) <- nm
    
    req_cols <- c(
      "scenario_name",
      "min_len_mm",
      "max_len_mm"
    )
    
    if (!all(req_cols %in% names(df))) {
      return(NULL)
    }
    
    df
  })
  
  # [Logic] Scenarios define Folders
  # RM, PAE, ESD
  get_scenarios_df <- reactive({
    size_df <- get_size_limits()
    req(size_df)
    
    ESD_vec <- parse_num_vec(input$ESD_vec)
    PAE_vec <- parse_num_vec(input$pae_vec)
    RM_vec  <- parse_num_vec(input$rm_vec)
    
    validate(need(length(ESD_vec) > 0, "Need ESD inputs"),
             need(length(PAE_vec) > 0, "Need PAE inputs"))
    
    if(length(RM_vec) == 0) RM_vec <- 0
    
    scenarios <- tidyr::expand_grid(
      size_df,
      ESD = ESD_vec,
      prop_annual_encounters = PAE_vec,
      release_mortality = RM_vec
    ) %>%
      dplyr::mutate(
        scenario_id = dplyr::row_number(),
        comp_mode = 0L
      )
    
    sanitize <- function(x) gsub("[^A-Za-z0-9_\\.]", "", as.character(x))
    
    scenarios$run_label <- paste0(
      "size_", sanitize(scenarios$scenario_name),
      "__min", sanitize(scenarios$min_len_mm),
      "__max", sanitize(scenarios$max_len_mm),
      "_PAE", sanitize(scenarios$prop_annual_encounters),
      "_ESD", sanitize(scenarios$ESD),
      "_RM",  sanitize(scenarios$release_mortality)
    )
    
    scenarios
  })
  
  get_policy_combos_logic <- reactive({
    comp_input <- input$compliance_mode
    validate(need(length(comp_input) > 0, "Select at least one Compliance Mode"))
    
    rm_input_vec <- parse_num_vec(input$rm_vec)
    
    has_nonzero_rm <- any(rm_input_vec > 0)
    
    comp_codes <- integer(0)
    if ("yes" %in% comp_input) comp_codes <- c(comp_codes, 1L)
    if ("no"  %in% comp_input) comp_codes <- c(comp_codes, 0L)
    comp_codes <- sort(comp_codes, decreasing = TRUE)
    
    if (has_nonzero_rm) {
      rm_flags <- c(TRUE, FALSE)
    } else {
      rm_flags <- c(TRUE)
    }
    
    policy_combos <- tidyr::expand_grid(
      comp_mode = comp_codes,
      use_scenario_rm = rm_flags
    ) %>%
      dplyr::mutate(
        policy_combo_id = dplyr::row_number()
      )
    
    policy_combos
  })
  
  get_compliance_struct <- reactive({
    c_breaks <- parse_num_vec(input$comp_breaks)
    c_probs  <- parse_num_vec(input$comp_probs)
    validate(need(length(c_breaks) > 0, "Breaks empty"),
             need(length(c_breaks) == length(c_probs), "Breaks/Probs length mismatch"))
    data.frame(Threshold_mm = c_breaks, Probability = c_probs)
  })
  
  # --- Design Preview Tables ---
  
  output$size_tbl <- DT::renderDT({
    req(get_size_limits())
    DT::datatable(get_size_limits(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Table 2: Uncertainty (Scenarios / Folders)
  output$scen_preview_tbl <- DT::renderDT({
    req(get_scenarios_df())
    df <- get_scenarios_df() %>%
      dplyr::select(
        `Label name (Output Folder)` = run_label,
        PAE = prop_annual_encounters,
        ESD,
        RM = release_mortality
      )
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Table 3: Policy Condition (Files)
  output$combo_tbl <- DT::renderDT({
    req(get_policy_combos_logic())
    df <- get_policy_combos_logic()
    
    rm_input_vec <- parse_num_vec(input$rm_vec)
    has_nonzero_rm <- any(rm_input_vec > 0)
    
    df_show <- df %>%
      dplyr::mutate(
        `Label` = paste0("policy_", policy_combo_id),
        `Compliance?` = ifelse(comp_mode == 1, "Yes", "No"),
        `Release mortality considered?` = case_when(
          !has_nonzero_rm ~ "No",
          use_scenario_rm ~ "Yes",
          TRUE ~ "No"
        )
      ) %>%
      dplyr::select(`Label`, `Compliance?`, `Release mortality considered?`)
    
    DT::datatable(df_show, options = list(pageLength = 5, scrollX = TRUE, dom = 't'))
  })
  
  
  # Logic 6: Parameter Packing (Strictly Matched to C++ Source Code)
  
  get_packed_params <- reactive({
    
    
    req(sys_status$vbgf_ok, sys_status$alk_ok,
        sys_status$global_ok, sys_status$design_ok)
    
    mw <- parse_num_vec(input$month_weights)
    if(length(mw) != 12) mw <- rep(1, 12)
    
    total_burn_in <- as.integer(input$transient_years) + as.integer(input$stable_years)
    c_struct <- get_compliance_struct()
    
    
    ##processing
    
    master_dd <- isTRUE(input$use_dd_effects)
    
    # 1. Survival
    # ( AND ) ,； a=1, b=0
    use_surv <- master_dd && isTRUE(input$use_dd_survival)
    val_surv_a <- if(use_surv) input$surv_a else 1.0
    val_surv_b <- if(use_surv) input$surv_b else 0.0
    
    # UI g1_* controls are Adult; UI g2_* controls are Juvenile.
    use_adult_growth <- master_dd && isTRUE(input$use_dd_growth_adult)
    use_juvenile_growth <- master_dd && isTRUE(input$use_dd_growth_juv)
    
    val_adult_a <- if (use_adult_growth) input$g1_a else 1.0
    val_adult_b <- if (use_adult_growth) input$g1_b else 0.0
    val_juvenile_a <- if (use_juvenile_growth) input$g2_a else 1.0
    val_juvenile_b <- if (use_juvenile_growth) input$g2_b else 0.0
    
    
    use_harv_curve <- isTRUE(input$flag_harvest_curve)
    
    if (use_harv_curve) {
      val_h_L50   <- input$harv_L50
      val_h_pmax  <- input$harv_pmax
      val_h_slope <- input$harv_slope
    } else {
      val_h_L50   <- -1000.0            # Magic
      val_h_pmax  <- input$harv_fixed_pmax
      val_h_slope <- 1000.0             # Magic
    }
    ## 4. Z
    final_f_z_ratio <- if (input$use_z_estimation) {
      1.0 - input$F_over_Z_ratio
    } else {
      0.0
    }
    z_vec       = as.numeric(vals$z_dist)
    zb <- quantile(z_vec,c(0.025,0.975),na.rm = TRUE,names = FALSE)
    z_vec <- z_vec[z_vec >= zb[1] & z_vec <= zb[2]]
    
    packed_list <- list(
      # --- A. Data ---
      agedata_mat = as.matrix(vals$theta_clean),
      alk_mat     = as.matrix(vals$alk_data),
      z_vec       =   z_vec,
      
      # --- B. Time & Space ---
      seed = as.integer(input$seed),
      before_policy_years = total_burn_in,
      policy_years  = as.integer(input$policy_years),
      
      # --- C. Biological Params (KEY NAMES FIXED BY C++ SOURCE) ---
      
      # 1. Harvest Parameters
      # C++ Source: ["p_max"], ["L50"], ["slope"]
      harvest = list(
        flag_harvest_curve = isTRUE(input$flag_harvest_curve),
        L50   = val_h_L50,
        p_max = val_h_pmax,
        slope = val_h_slope
      ),
      
      # C++ growth_1 is Juvenile; map it from the UI g2_* controls.
      growth_1 = list(
        use_dd_growth_juvenile = use_juvenile_growth,
        a = val_juvenile_a,
        b = val_juvenile_b,
        c = input$g2_c,
        d_avg = input$g2_d_avg
      ),
      
      # C++ growth_2 is Adult; map it from the UI g1_* controls.
      growth_2 = list(
        use_dd_growth_adult = use_adult_growth,
        a = val_adult_a,
        b = val_adult_b,
        c = input$g1_c,
        d_avg = input$g1_d_avg
      ),
      
      # 4. Survival
      # C++ Source: ["a"], ["b"], ["c"], ["d_avg1"], ["d_avg2"]
      survival = list(
        use_dd_survival = use_surv,
        a = val_surv_a,
        b = val_surv_b,
        c = input$surv_c,
        d_avg1 = input$surv_d_avg1,
        d_avg2 = input$surv_d_avg2
      ),
      
      # --- D. Other ---
      month_weights = mw,
      compliance_struct = c_struct,
      
      execution = list(
        engine = if (isTRUE(input$simulation_engine)) {
          "v2"
        } else {
          "legacy"
        },
        
        omp_nthreads = if (isTRUE(input$simulation_engine)) {
          max(1L, as.integer(input$omp_nthreads))
        } else {
          1L
        },
        
        combo_threads = if (isTRUE(input$use_gpu)) {
          max(0L, as.integer(input$gpu_thread_count))
        } else {
          0L
        },
        
        fast_forward_mode = if (is.null(input$fast_forward_mode)) {
          "auto"
        } else {
          input$fast_forward_mode
        }
      ),
      
      other = list(
        # R-S & Env
        rec_a = input$rec_a,
        rec_b = input$rec_b,
        rec_v = 0.68,
        lake_area_ha = input$lake_area_ha,
        initial_pop_size = as.integer(input$initial_pop_size),
        F_over_Z_ratio =final_f_z_ratio,
        spawn_month = as.integer(input$spawn_month),
        recruit_entry_month = as.integer(input$recruit_entry_month),
        
        # [NEW] Ages & Mode
        age_spawn = input$age_spawn,
        min_adult_age = input$min_adult_age,
        age_recruit = input$z_full,
        f_age_mode = input$f_age_mode,
        # [NEW] PSD Values
        psd_stock = input$psd_stock,
        psd_quality = input$psd_quality,
        psd_preferred = input$psd_preferred,
        psd_memorable = input$psd_memorable,
        psd_trophy = input$psd_trophy,
        
        juv_onlyM_len = input$psd_stock,
        T_safe = selected_T_safe(),
        simulation_engine = if (isTRUE(input$simulation_engine)) {
          "v2"
        } else {
          "legacy"
        },
        
        omp_nthreads = if (isTRUE(input$simulation_engine)) {
          max(1L, as.integer(input$omp_nthreads))
        } else {
          1L
        },
        combo_threads = if (isTRUE(input$use_gpu)) max(0L, as.integer(input$gpu_thread_count)) else 0L,
        vmonthly_avg = input$juv_annual_M / 12.0,
        use_ricker = input$use_ricker
      )
    )
    
    return(packed_list)
  })
  
  
  
  
  
  # [STEP 2: TEST SIMULATION - UPDATED]
  
  
  output$test_selectors <- renderUI({
    
    if (!isTRUE(sys_status$design_ok)) {
      return(
        tags$div(
          class = "alert alert-warning",
          "Complete and verify Experiment Design first."
        )
      )
    }
    
    scen <- tryCatch(
      get_scenarios_df(),
      error = function(e) NULL
    )
    
    if (is.null(scen) || nrow(scen) == 0L) {
      return(
        tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No test scenarios are available. Re-submit Experiment Design ",
          "or reload a settings file containing the Size limit CSV."
        )
      )
    }
    
    scen_labels <- sprintf(
      "%s | RM:%g | ESD:%g | PAE:%g",
      scen$scenario_name,
      scen$release_mortality,
      scen$ESD,
      scen$prop_annual_encounters
    )
    
    scen_choices <- stats::setNames(
      as.character(scen$scenario_id),
      scen_labels
    )
    
    selectInput(
      "test_scen_id",
      "Choose Scenario (runs all policies listed in Design Preview):",
      choices  = scen_choices,
      selected = as.character(scen$scenario_id[[1L]])
    )
  })
  
  # The scenario selector lives on the Test 1 tab, but Test 2 reads the value
  # it produces. Shiny suspends outputs on tabs that have not been shown, so
  # without this a user who opens Test 2 first would find no scenario selected
  # and be told, wrongly, that none is available.
  outputOptions(output, "test_selectors", suspendWhenHidden = FALSE)
  
  sys_status$log_2a <- "Waiting for run..."
  output$log_step2a <- renderText({ sys_status$log_2a })
  
  
  test_sim_data <- reactiveVal(NULL)
  
  observeEvent(input$run_test_sim, {
    test_sim_data(NULL)
    sys_status$test_run_done <- FALSE
    error_msgs <- character(0)
    test_scen_id <- suppressWarnings(
      as.integer(input$test_scen_id)
    )
    
    if (
      length(test_scen_id) != 1L ||
      is.na(test_scen_id)
    ) {
      error_msgs <- c(
        error_msgs,
        paste0(
          if (is.null(get_size_limits())) {
            paste0(
              "❌ The Size-limit CSV is not available. ",
              "The settings file did not carry it, so upload it again under ",
              "Step 1 Experiment Design and submit that page."
            )
          } else if (!isTRUE(sys_status$design_ok)) {
            paste0(
              "❌ Experiment Design has not been verified. ",
              "Open Step 1 Experiment Design and submit it."
            )
          } else {
            paste0(
              "❌ No test scenario is selected yet. ",
              "Open the scenario list above and choose one."
            )
          }
        )
      )
    }
    
    if (
      isTRUE(input$use_cloud) &&
      !isTRUE(proc_state$cloud_verified)
    ) {
      error_msgs <- c(
        error_msgs,
        paste0(
          "❌ Google Cloud has not been verified for the current settings. ",
          "Return to Step 2 and click Check cloud connection."
        )
      )
    }
    if (!sys_status$vbgf_ok) error_msgs <- c(error_msgs, "❌ VBGF growth estimation is not ready.")
    if (!sys_status$alk_ok) error_msgs <- c(error_msgs, "❌ ALK data are not ready.")
    if (!sys_status$global_ok) error_msgs <- c(error_msgs, "❌ Global parameters have not been verified.")
    if (!sys_status$design_ok) error_msgs <- c(error_msgs, "❌ Experiment design has not been verified.")
    if (is.null(sys_status$runcontrol_ok) || !sys_status$runcontrol_ok) error_msgs <- c(error_msgs, "❌ Run control (Step 2) has not been confirmed.")
    
    if (length(error_msgs) > 0) {
      sys_status$log_2a <- paste0("🛑 PRE-RUN CHECK FAILED!\n", paste(error_msgs, collapse = "\n"))
      return()
    }
    
    # Hold the global lock for the whole observer; it is released however the
    # run ends, including on error.
    .set_active_run("validation", if (isTRUE(input$use_cloud)) "cloud" else "local")
    # Release the lock unless a cloud job for this task really is being
    # tracked when the observer exits. Checking the actual state rather than
    # a flag means an error, an early return, or a refused submission can
    # never leave the start buttons stuck.
    on.exit({
      still_tracking <- (
        identical(proc_state$cloud_task_type, "validation") &&
          !is.null(proc_state$cloud_job_id) &&
          !is.null(proc_state$cloud_status) &&
          proc_state$cloud_status %in% c("submitted", "running")
      )
      if (!still_tracking) .clear_active_run()
    }, add = TRUE)
    
    # Clear the panel left by an earlier cloud job so this attempt does not
    # begin by showing the previous outcome.
    .cloud_reset_display()
    
    tryCatch({
      all_params <- get_packed_params()
      scen_df <- get_scenarios_df()
      
      s_row <- scen_df[
        scen_df$scenario_id == test_scen_id,
        ,
        drop = FALSE
      ]
      
      if (nrow(s_row) != 1L) {
        stop(
          "The selected test scenario could not be found.",
          call. = FALSE
        )
      }
      
      rm_vec_all <- parse_num_vec(input$rm_vec)
      if (length(rm_vec_all) == 0) rm_vec_all <- 0
      burnin_rm_val <- max(rm_vec_all, na.rm = TRUE)
      
      cpp_scen <- list(
        scenario_id              = as.integer(s_row$scenario_id),
        scenario_name            = as.character(s_row$scenario_name),
        prop_annual_encounters   = as.numeric(s_row$prop_annual_encounters),
        ESD                      = as.numeric(s_row$ESD),
        burnin_comp_mode         = 0L,
        burnin_release_mortality = as.numeric(burnin_rm_val),
        min_len_mm               = as.numeric(s_row$min_len_mm),
        max_len_mm               = as.numeric(s_row$max_len_mm)
      )
      
      cpp_pol_df <- get_policy_combos_logic() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          release_mortality = if (use_scenario_rm) as.numeric(s_row$release_mortality) else 0.0
        ) %>%
        dplyr::select(policy_combo_id, comp_mode, release_mortality) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      engine_label <- if (identical(all_params$execution$engine, "v2")) "Large-population optimized" else "Standard"
      omp_threads <- all_params$execution$omp_nthreads
      combo_threads <- all_params$execution$combo_threads
      
      sys_status$log_2a <- paste0(
        "⏳ Running test simulation, please wait...\n",
        "Method: ", engine_label, "\n",
        "Fast-forward: ", all_params$other$T_safe, " month(s)\n",
        "Individual-level threads: ", omp_threads, "\n",
        "Policy-combo threads: ", combo_threads, "\n"
      )
      
      # When cloud mode is on the work is handed to the rented machine instead
      # of running here, and the report separates setup cost from computation
      # so the machine can be judged on its own merits.
      if (isTRUE(input$use_cloud)) {
        sys_status$log_2a <- paste0(
          "\u2601\ufe0f Sending the model validation to Google Cloud...\n",
          "Method: ", engine_label, "\n",
          "Machine: ", input$gcp_machine_type, "\n"
        )
        
        sub <- cloud_submit(
          "validation",
          payload = list(
            all_params = all_params,
            cpp_scen   = cpp_scen,
            cpp_pol_df = cpp_pol_df
          ),
          label = "Model validation"
        )
        
        if (!isTRUE(sub$ok)) {
          .clear_active_run()
          sys_status$log_2a <- paste0(
            "\U0001F6D1 The cloud job could not be started.\n",
            if (!is.null(sub$msg)) sub$msg else "")
        } else {
          .cloud_start_watch(sub$job_id)
          sys_status$log_2a <- paste0(
            "\u2601\ufe0f Model validation submitted to Google Cloud.\n",
            "==========================================\n",
            "Job: ", sub$job_id, "\n",
            "Machine: ", input$gcp_machine_type, "\n",
            "Progress is shown on this Test 1 tab. Please wait while the cloud\n",
            "machine starts; the first report may take a few minutes to appear."
          )
        }
        return()
      }
      
      t0 <- Sys.time()
      result <- withProgress(message = "Running test simulation, please wait...", value = 0.5, {
        run_selected_cpp(all_params, cpp_scen, cpp_pol_df, rep_id = 1L)
      })
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      test_sim_data(result)
      sys_status$test_run_done <- TRUE
      
      n_scenarios <- nrow(scen_df)
      n_iter_val <- as.integer(input$n_iter)
      total_tasks <- n_scenarios * n_iter_val
      
      configured_workers <- min(
        max(1L, as.integer(input$n_cores)),
        max(1L, total_tasks)
      )
      outer_workers <- configured_workers
      
      max_tasks_per_worker <- ceiling(total_tasks / outer_workers)
      est_seconds <- max_tasks_per_worker * elapsed
      
      internal_threads <- max(1L, omp_threads) * max(1L, combo_threads)
      logical_cores <- parallel::detectCores(logical = TRUE)
      if (is.na(logical_cores)) logical_cores <- 1L
      nominal_budget <- outer_workers * internal_threads
      budget_warning <- if (nominal_budget > logical_cores * 2L) {
        paste0("\n⚠️ Nominal thread budget is ", nominal_budget,
               " on ", logical_cores, " logical cores. Reduce workers or internal threads if performance becomes worse.\n")
      } else ""
      
      sys_status$log_2a <- paste0(
        "✅ Test Simulation Complete\n",
        "==========================================\n",
        "Scenario: ", s_row$scenario_name, "\n",
        "Engine: ", engine_label, "\n",
        "Policy combinations: ", nrow(cpp_pol_df), "\n",
        "T_safe: ", all_params$other$T_safe, " month(s)\n",
        "OpenMP threads: ", omp_threads, "\n",
        "Policy-combo threads: ", combo_threads, "\n",
        "Measured test time: ", round(elapsed, 3), " sec\n",
        "------------------------------------------\n",
        "Full-run tasks: ", total_tasks, "\n",
        "Active R workers: ", outer_workers, "\n",
        "Rough full-model calculation-time estimate: ",
        .format_test_duration(est_seconds),
        "\n",
        budget_warning,
        "==========================================\n",
        "Plot updated."
      )
    }, error = function(e) {
      sys_status$log_2a <- paste0("❌ Error during test run:\n", e$message)
    })
  })
  
  
  # ===== Oversubscription & Memory Stress Test =====
  # The observer already writes the cloud submission notice into
  # sys_status$log_oversub, so this output has a single source of truth.
  # Reading cloud state here as well would let a stale job hide any newer
  # message, including the reason a fresh submission was refused.
  output$log_oversub <- renderText({
    sys_status$log_oversub
  })
  
  .perf_value <- function(x, default = NA) {
    if (
      is.null(x) ||
      length(x) == 0L ||
      is.na(x[[1L]])
    ) {
      default
    } else {
      x[[1L]]
    }
  }
  
  
  .perf_seconds <- function(x) {
    x <- suppressWarnings(
      as.numeric(.perf_value(x, NA_real_))
    )
    
    if (!is.finite(x)) {
      return("n/a")
    }
    
    if (x < 60) {
      sprintf("%.1f sec", x)
    } else if (x < 3600) {
      sprintf("%.1f min", x / 60)
    } else {
      sprintf("%.2f hr", x / 3600)
    }
  }
  
  
  .perf_gb <- function(x) {
    x <- suppressWarnings(
      as.numeric(.perf_value(x, NA_real_))
    )
    
    if (!is.finite(x)) {
      return("n/a")
    }
    
    sprintf("%.2f GB", x / 1024)
  }
  
  
  .perf_integer <- function(x) {
    x <- suppressWarnings(
      as.integer(.perf_value(x, NA_integer_))
    )
    
    if (is.na(x)) {
      "n/a"
    } else {
      as.character(x)
    }
  }
  
  
  .perf_total_tasks <- function() {
    scenarios <- tryCatch(
      get_scenarios_df(),
      error = function(e) NULL
    )
    
    n_scenarios <- if (is.null(scenarios)) {
      1L
    } else {
      max(1L, nrow(scenarios))
    }
    
    n_replicates <- suppressWarnings(
      as.integer(input$n_iter)
    )
    
    if (is.na(n_replicates) || n_replicates < 1L) {
      n_replicates <- 1L
    }
    
    max(
      1L,
      n_scenarios * n_replicates
    )
  }
  
  
  .format_perf_report <- function(
    res,
    cloud = FALSE,
    prog = NULL
  ) {
    status <- as.character(
      .perf_value(res$status, "")
    )
    
    memory_class <- as.character(
      .perf_value(res$memory_precheck, "")
    )
    
    total_tasks <- suppressWarnings(
      as.integer(
        .perf_value(
          res$total_tasks,
          .perf_total_tasks()
        )
      )
    )
    
    requested_workers <- suppressWarnings(
      as.integer(
        .perf_value(
          res$requested_workers,
          input$n_cores
        )
      )
    )
    
    effective_workers <- suppressWarnings(
      as.integer(
        .perf_value(
          res$effective_workers,
          requested_workers
        )
      )
    )
    
    probe_workers <- suppressWarnings(
      as.integer(
        .perf_value(
          res$probe_workers,
          .perf_value(res$n_cores, 1L)
        )
      )
    )
    
    if (is.na(total_tasks) || total_tasks < 1L) {
      total_tasks <- 1L
    }
    
    if (is.na(requested_workers) || requested_workers < 1L) {
      requested_workers <- 1L
    }
    
    if (is.na(effective_workers) || effective_workers < 1L) {
      effective_workers <- 1L
    }
    
    if (is.na(probe_workers) || probe_workers < 1L) {
      probe_workers <- 1L
    }
    
    solo_seconds <- suppressWarnings(
      as.numeric(
        .perf_value(res$solo_elapsed, NA_real_)
      )
    )
    
    concurrent_seconds <- suppressWarnings(
      as.numeric(
        .perf_value(res$concurrent_elapsed, NA_real_)
      )
    )
    
    slowdown <- suppressWarnings(
      as.numeric(
        .perf_value(res$oversub_factor, NA_real_)
      )
    )
    
    # The concurrent benchmark completes probe_workers model runs in one
    # elapsed interval. Compare that interval with the time needed to run the
    # same number of jobs one by one. The old "Parallel slowdown" value only
    # described how much slower each worker became while sharing the CPU; it
    # was not the overall benefit of parallel execution.
    sequential_seconds <- if (
      is.finite(solo_seconds) &&
      solo_seconds > 0 &&
      probe_workers > 0L
    ) {
      solo_seconds * probe_workers
    } else {
      NA_real_
    }
    
    parallel_speedup <- if (
      is.finite(sequential_seconds) &&
      is.finite(concurrent_seconds) &&
      concurrent_seconds > 0
    ) {
      sequential_seconds / concurrent_seconds
    } else {
      NA_real_
    }
    
    parallel_efficiency <- if (
      is.finite(parallel_speedup) &&
      probe_workers > 0L
    ) {
      100 * parallel_speedup / probe_workers
    } else {
      NA_real_
    }
    
    per_worker_memory <- .perf_value(
      res$per_worker_mb,
      .perf_value(res$solo_proc_mem, NA_real_)
    )
    
    full_plan_memory <- .perf_value(
      res$projected_full_mem,
      .perf_value(res$projected_mem, NA_real_)
    )
    
    # Test 2 estimates full-run time from the throughput actually measured:
    # number of required worker groups × time required by one tested group.
    estimated_full_seconds <- if (
      is.finite(concurrent_seconds) &&
      concurrent_seconds > 0 &&
      probe_workers > 0L
    ) {
      ceiling(total_tasks / probe_workers) *
        concurrent_seconds
    } else {
      NA_real_
    }
    
    tested_full_load <- (
      probe_workers >= effective_workers
    )
    
    estimate_label <- if (tested_full_load) {
      "More precise full-model calculation-time estimate"
    } else {
      "Updated full-model calculation-time estimate"
    }
    
    cloud_timing <- if (isTRUE(cloud)) {
      paste0(
        "Cloud preparation and input download time: ",
        .perf_seconds(prog$startup_sec),
        "\n",
        
        "Time spent running the performance check time: ",
        .perf_seconds(prog$compute_sec),
        "\n",
        
        "------------------------------------------\n"
      )
    } else {
      ""
    }
    
    machine_statement <- if (isTRUE(cloud)) {
      paste0(
        "These results were measured on the selected Google Cloud machine, ",
        "not on the computer used to open this app."
      )
    } else {
      "These results were measured on this computer."
    }
    
    # --------------------------------------------------------------------------
    # Worker or memory failure
    # --------------------------------------------------------------------------
    
    if (identical(status, "memory_crash")) {
      return(
        paste0(
          "🛑 Test 2: Memory Failure\n",
          "==========================================\n",
          cloud_timing,
          "The machine ran out of memory while the performance check was running.\n",
          "The full simulation remains blocked.\n",
          "------------------------------------------\n",
          "Return to Step 2 and reduce Parallel cores, Policy threads, ",
          "or Individual threads; then run the required safety check again.\n",
          "==========================================\n",
          machine_statement,
          "\nFor technical details, please see the Help Guide."
        )
      )
    }
    
    
    if (identical(status, "worker_error")) {
      return(
        paste0(
          "❌ Test 2: A Model Worker Failed\n",
          "==========================================\n",
          cloud_timing,
          "The performance check did not produce a valid result.\n",
          "The full simulation remains blocked.\n",
          "------------------------------------------\n",
          "Worker message: ",
          .perf_value(res$worker_error, "No error message was returned."),
          "\n",
          "==========================================\n",
          machine_statement,
          "\nFor technical details, please see the Help Guide."
        )
      )
    }
    
    
    if (
      identical(status, "memory_abort") ||
      identical(memory_class, "abort")
    ) {
      max_safe_workers <- suppressWarnings(
        as.integer(
          .perf_value(
            res$max_safe_workers_by_total,
            NA_integer_
          )
        )
      )
      
      action_text <- if (
        !is.na(max_safe_workers) &&
        max_safe_workers >= 1L
      ) {
        paste0(
          "Return to Step 2 and set Parallel cores to ",
          max_safe_workers,
          " or fewer; then confirm Run Control and repeat Test 2."
        )
      } else {
        paste0(
          "Return to Step 2 and reduce Parallel cores, Policy threads, ",
          "Individual threads, or the model population size; then repeat Test 2."
        )
      }
      
      return(
        paste0(
          "🛑 Test 2: Full Simulation Blocked\n",
          "==========================================\n",
          cloud_timing,
          
          "Memory needed by one active worker: ",
          .perf_gb(per_worker_memory),
          "\n",
          
          "Estimated memory needed by the selected parallel plan: ",
          .perf_gb(full_plan_memory),
          "\n",
          
          "Memory currently available: ",
          .perf_gb(res$available_ram_mb),
          "\n",
          
          "Total physical memory: ",
          .perf_gb(res$system_ram_mb),
          "\n",
          
          "Safety limit (95% of total memory): ",
          .perf_gb(res$ram_limit),
          "\n",
          
          "------------------------------------------\n",
          "🛑 The selected parallel plan is not memory-safe.\n",
          action_text,
          "\n",
          
          "==========================================\n",
          machine_statement,
          "\nFor technical details, please see the Help Guide."
        )
      )
    }
    
    
    if (
      !memory_class %in% c("safe", "warning")
    ) {
      return(
        paste0(
          "⚠️ Test 2: Memory Result Unavailable\n",
          "==========================================\n",
          cloud_timing,
          "The app could not determine whether the full simulation would fit ",
          "in memory. The full simulation remains blocked.\n",
          "Install or verify the 'ps' package, then repeat Test 2.\n",
          "==========================================\n",
          machine_statement,
          "\nFor technical details, please see the Help Guide."
        )
      )
    }
    
    # --------------------------------------------------------------------------
    # Successful speed result
    # --------------------------------------------------------------------------
    
    cpu_result <- if (!is.finite(parallel_efficiency)) {
      paste0(
        "ℹ️ Parallel speed could not be compared because one of the ",
        "timing measurements was unavailable."
      )
    } else if (parallel_efficiency >= 80) {
      paste0(
        "✅ Speed result: the selected workers provide efficient parallel acceleration."
      )
    } else if (parallel_efficiency >= 60) {
      paste0(
        "⚠️ Speed result: parallel execution is faster overall, but the workers ",
        "compete noticeably for CPU capacity."
      )
    } else {
      paste0(
        "⚠️ Speed result: the selected workers have low parallel efficiency. ",
        "Reducing one of the parallel settings may complete the full model faster."
      )
    }
    
    memory_result <- if (identical(memory_class, "safe")) {
      paste0(
        "✅ Memory result: the selected full parallel plan is expected to fit ",
        "within the memory currently available."
      )
    } else {
      paste0(
        "⚠️ Memory result: the plan remains below the safety limit, but it ",
        "exceeds the memory currently available. Close other programs before ",
        "starting the full simulation."
      )
    }
    
    slowdown_text <- if (is.finite(slowdown)) {
      sprintf("%.2fx", slowdown)
    } else {
      "n/a"
    }
    
    speedup_text <- if (is.finite(parallel_speedup)) {
      sprintf("%.2fx", parallel_speedup)
    } else {
      "n/a"
    }
    
    efficiency_text <- if (is.finite(parallel_efficiency)) {
      sprintf("%.1f%%", parallel_efficiency)
    } else {
      "n/a"
    }
    
    paste0(
      "✅ Test 2: Parallel Performance Check Complete\n",
      "==========================================\n",
      cloud_timing,
      
      "Full-model jobs: ",
      total_tasks,
      "\n",
      
      "Replicate workers selected in Step 2: ",
      requested_workers,
      "\n",
      
      "Replicate workers used in this check: ",
      probe_workers,
      "\n",
      
      "Policy threads per worker: ",
      .perf_integer(res$combo_threads),
      "\n",
      
      "Individual threads per worker: ",
      .perf_integer(res$omp_threads),
      "\n",
      
      "Logical CPU capacity: ",
      .perf_integer(res$logical_cores),
      "\n",
      
      "------------------------------------------\n",
      "SPEED\n",
      
      "One worker completed one model run in: ",
      .perf_seconds(solo_seconds),
      "\n",
      
      "Estimated one-by-one time for ",
      probe_workers,
      " model runs: ",
      .perf_seconds(sequential_seconds),
      "\n",
      
      probe_workers,
      " workers completed the same ",
      probe_workers,
      " model runs together in: ",
      .perf_seconds(concurrent_seconds),
      "\n",
      
      "Overall parallel speedup: ",
      speedup_text,
      "\n",
      
      "Parallel efficiency: ",
      efficiency_text,
      "\n",
      
      "Per-worker slowdown while sharing CPU: ",
      slowdown_text,
      "\n",
      
      cpu_result,
      "\n",
      
      estimate_label,
      ": ",
      .perf_seconds(estimated_full_seconds),
      "\n",
      
      "------------------------------------------\n",
      "MEMORY SAFETY\n",
      
      "Estimated memory needed by one active worker: ",
      .perf_gb(per_worker_memory),
      "\n",
      
      "Estimated peak for the complete ",
      effective_workers,
      "-worker plan: ",
      .perf_gb(full_plan_memory),
      "\n",
      
      "Memory currently available: ",
      .perf_gb(res$available_ram_mb),
      "\n",
      
      "Total physical memory: ",
      .perf_gb(res$system_ram_mb),
      "\n",
      
      "Safety limit (95% of total memory): ",
      .perf_gb(res$ram_limit),
      "\n",
      
      memory_result,
      "\n",
      
      "------------------------------------------\n",
      "✅ The memory-safety requirement for Step 3b has been completed.\n",
      "==========================================\n",
      machine_statement,
      "\nFor technical details, please see the Help Guide."
    )
  }
  
  
  .apply_perf_result <- function(
    res,
    cloud = FALSE,
    prog = NULL
  ) {
    status <- as.character(
      .perf_value(res$status, "")
    )
    
    memory_class <- as.character(
      .perf_value(res$memory_precheck, "")
    )
    
    if (
      status %in% c("memory_abort", "memory_crash") ||
      identical(memory_class, "abort")
    ) {
      sys_status$mem_safe <- FALSE
      sys_status$memory_check_done <- TRUE
      sys_status$memory_retest_required <- TRUE
      
    } else if (
      identical(status, "worker_error") ||
      !memory_class %in% c("safe", "warning")
    ) {
      sys_status$mem_safe <- NA
      sys_status$memory_check_done <- FALSE
      sys_status$memory_retest_required <- TRUE
      
    } else {
      sys_status$mem_safe <- TRUE
      sys_status$memory_check_done <- TRUE
      sys_status$memory_retest_required <- FALSE
    }
    
    sys_status$log_oversub <- .format_perf_report(
      res = res,
      cloud = cloud,
      prog = prog
    )
    
    invisible(TRUE)
  }
  observeEvent(input$run_oversub_test, {
    
    error_msgs <- character(0)
    test_scen_id <- suppressWarnings(
      as.integer(input$test_scen_id)
    )
    
    if (
      length(test_scen_id) != 1L ||
      is.na(test_scen_id)
    ) {
      error_msgs <- c(
        error_msgs,
        paste0(
          if (is.null(get_size_limits())) {
            paste0(
              "❌ The Size-limit CSV is not available. ",
              "The settings file did not carry it, so upload it again under ",
              "Step 1 Experiment Design and submit that page."
            )
          } else if (!isTRUE(sys_status$design_ok)) {
            paste0(
              "❌ Experiment Design has not been verified. ",
              "Open Step 1 Experiment Design and submit it."
            )
          } else {
            paste0(
              "❌ No test scenario is selected yet. ",
              "Open the scenario list above and choose one."
            )
          }
        )
      )
    }
    
    if (
      isTRUE(input$use_cloud) &&
      !isTRUE(proc_state$cloud_verified)
    ) {
      error_msgs <- c(
        error_msgs,
        paste0(
          "❌ Google Cloud has not been verified for the current settings. ",
          "Return to Step 2 and click Check cloud connection."
        )
      )
    }
    if (!sys_status$vbgf_ok) error_msgs <- c(error_msgs, "❌ VBGF growth estimation is not ready.")
    if (!sys_status$alk_ok) error_msgs <- c(error_msgs, "❌ ALK data are not ready.")
    if (!sys_status$global_ok) error_msgs <- c(error_msgs, "❌ Global parameters have not been verified.")
    if (!sys_status$design_ok) error_msgs <- c(error_msgs, "❌ Experiment design has not been verified.")
    if (is.null(sys_status$runcontrol_ok) || !sys_status$runcontrol_ok) {
      error_msgs <- c(error_msgs, "❌ Run control (Step 2) has not been confirmed.")
    }
    
    if (length(error_msgs) > 0L) {
      sys_status$log_oversub <- paste0(
        "🛑 PRE-CHECK FAILED!\n",
        paste(error_msgs, collapse = "\n")
      )
      return()
    }
    
    
    .set_active_run("perfcheck", if (isTRUE(input$use_cloud)) "cloud" else "local")
    # Release the lock unless a cloud job for this task really is being
    # tracked when the observer exits. Checking the actual state rather than
    # a flag means an error, an early return, or a refused submission can
    # never leave the start buttons stuck.
    on.exit({
      still_tracking <- (
        identical(proc_state$cloud_task_type, "perfcheck") &&
          !is.null(proc_state$cloud_job_id) &&
          !is.null(proc_state$cloud_status) &&
          proc_state$cloud_status %in% c("submitted", "running")
      )
      if (!still_tracking) .clear_active_run()
    }, add = TRUE)
    sys_status$memory_check_done <- FALSE
    sys_status$mem_safe <- NA
    sys_status$memory_retest_required <- TRUE
    
    # Clear the panel left by an earlier cloud job.
    .cloud_reset_display()
    
    tryCatch({
      all_params <- get_packed_params()
      scen_df <- get_scenarios_df()
      s_row <- scen_df[
        scen_df$scenario_id == test_scen_id,
        ,
        drop = FALSE
      ]
      
      if (nrow(s_row) != 1L) {
        stop(
          "The selected test scenario could not be found.",
          call. = FALSE
        )
      }
      
      rm_vec_all <- parse_num_vec(input$rm_vec)
      if (length(rm_vec_all) == 0L) rm_vec_all <- 0
      burnin_rm_val <- max(rm_vec_all, na.rm = TRUE)
      
      cpp_scen <- list(
        scenario_id              = as.integer(s_row$scenario_id),
        scenario_name            = as.character(s_row$scenario_name),
        prop_annual_encounters   = as.numeric(s_row$prop_annual_encounters),
        ESD                      = as.numeric(s_row$ESD),
        burnin_comp_mode         = 0L,
        burnin_release_mortality = as.numeric(burnin_rm_val),
        min_len_mm               = as.numeric(s_row$min_len_mm),
        max_len_mm               = as.numeric(s_row$max_len_mm)
      )
      
      cpp_pol_df <- get_policy_combos_logic() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          release_mortality = if (use_scenario_rm) {
            as.numeric(s_row$release_mortality)
          } else {
            0.0
          }
        ) %>%
        dplyr::select(policy_combo_id, comp_mode, release_mortality) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      requested_workers <- max(
        1L,
        as.integer(input$n_cores)
      )
      
      total_tasks <- max(
        1L,
        nrow(scen_df) * max(
          1L,
          as.integer(input$n_iter)
        )
      )
      
      # The full run cannot use more replicate workers than there are jobs.
      effective_workers <- min(
        requested_workers,
        total_tasks
      )
      
      combo_threads <- max(
        1L,
        as.integer(all_params$execution$combo_threads)
      )
      
      omp_threads <- max(
        1L,
        as.integer(all_params$execution$omp_nthreads)
      )
      
      logical_cores <- parallel::detectCores(
        logical = TRUE
      )
      
      if (
        length(logical_cores) == 0L ||
        is.na(logical_cores) ||
        logical_cores < 1L
      ) {
        logical_cores <- 1L
      }
      
      # Internal threads used by each active replicate worker.
      internal_threads_per_worker <- max(
        1L,
        combo_threads * omp_threads
      )
      
      # Do not let the benchmark itself launch more nominal compute threads
      # than the detected logical CPU capacity.
      cpu_safe_probe_workers <- max(
        1L,
        floor(
          logical_cores /
            internal_threads_per_worker
        )
      )
      
      # Default benchmark:
      # - never exceeds the actual number of jobs;
      # - never exceeds the configured replicate workers;
      # - respects the total nominal CPU-thread budget.
      probe_workers <- min(
        effective_workers,
        cpu_safe_probe_workers
      )
      
      # The full-load option requests the real full-run replicate concurrency.
      # Memory pre-checks in helper.R will still run before concurrent workers
      # are launched.
      if (identical(input$perf_test_mode, "full")) {
        probe_workers <- effective_workers
      }
      
      # ------------------------------------------------------------
      # Build a small self-contained benchmark function.
      # ------------------------------------------------------------
      benchmark_env <- new.env(
        parent = baseenv()
      )
      
      benchmark_env$snap_params <- all_params
      benchmark_env$snap_scen <- cpp_scen
      benchmark_env$snap_pol <- cpp_pol_df
      benchmark_env$run_selected_cpp_worker <- run_selected_cpp
      
      run_one_fn <- function(rep_id) {
        
        run_selected_cpp_worker(
          all_params = snap_params,
          cpp_scen = snap_scen,
          cpp_pol_df = snap_pol,
          rep_id = rep_id
        )
        
        invisible(NULL)
      }
      
      environment(run_one_fn) <- benchmark_env
      
      cluster_setup_fn <- function(cl) {
        
        setup_result <- parallel::clusterEvalQ(
          cl,
          {
            ns <- loadNamespace("craibm")
            
            list(
              ok = TRUE,
              pid = Sys.getpid(),
              package_path = getNamespaceInfo(
                ns,
                "path"
              )
            )
          }
        )
        
        setup_ok <- vapply(
          setup_result,
          function(x) {
            is.list(x) && isTRUE(x$ok)
          },
          logical(1)
        )
        
        if (!all(setup_ok)) {
          stop(
            "One or more benchmark workers could not load craibm.",
            call. = FALSE
          )
        }
        
        invisible(TRUE)
      }
      
      full_thread_budget <- effective_workers *
        internal_threads_per_worker
      
      probe_thread_budget <- probe_workers *
        internal_threads_per_worker
      
      sys_status$log_oversub <- paste0(
        "⏳ Running parallel performance check.\n",
        "==========================================\n",
        "Total scenario × replicate jobs: ",
        total_tasks,
        "\n",
        
        "Configured replicate workers: ",
        requested_workers,
        "\n",
        
        "Actual full-run replicate workers: ",
        effective_workers,
        "\n",
        
        "Benchmark replicate workers: ",
        probe_workers,
        "\n",
        
        "Policy threads per worker: ",
        combo_threads,
        "\n",
        
        "Individual threads per worker: ",
        omp_threads,
        "\n",
        
        "Threads per active worker: ",
        internal_threads_per_worker,
        "\n",
        
        "Benchmark nominal thread budget: ",
        probe_workers,
        " × ",
        internal_threads_per_worker,
        " = ",
        probe_thread_budget,
        "\n",
        
        "Full-run nominal thread budget: ",
        effective_workers,
        " × ",
        internal_threads_per_worker,
        " = ",
        full_thread_budget,
        "\n",
        
        "Detected logical CPU cores: ",
        logical_cores,
        "\n",
        
        if (
          full_thread_budget > logical_cores
        ) {
          paste0(
            "⚠️ Full-run oversubscription: ",
            round(
              full_thread_budget /
                logical_cores,
              2
            ),
            "× logical CPU capacity.\n"
          )
        } else {
          "✅ Full-run nominal thread budget is within logical CPU capacity.\n"
        },
        
        if (
          identical(input$perf_test_mode, "full") &&
          full_thread_budget > logical_cores
        ) {
          paste0(
            "⚠️ Full-load benchmark selected. This benchmark may place ",
            "substantial CPU and memory pressure on the system.\n"
          )
        } else {
          paste0(
            "The default benchmark limits replicate workers according to ",
            "the current job count and nominal CPU-thread capacity.\n"
          )
        },
        
        "This is a real simulation run and may take a while.\n",
        "=========================================="
      )
      
      # On a cloud run the numbers that matter belong to the rented machine, so
      # the check has to happen there rather than on this computer.
      if (isTRUE(input$use_cloud)) {
        sub <- cloud_submit(
          "perfcheck",
          payload = list(
            all_params        = all_params,
            cpp_scen          = cpp_scen,
            cpp_pol_df        = cpp_pol_df,
            requested_workers = requested_workers,
            probe_workers     = probe_workers,
            total_tasks       = total_tasks
          ),
          label = "Parallel performance check"
        )
        
        if (!isTRUE(sub$ok)) {
          .clear_active_run()
          sys_status$log_oversub <- paste0(
            "\U0001F6D1 The cloud job could not be started.\n",
            if (!is.null(sub$msg)) sub$msg else "")
        } else {
          .cloud_start_watch(sub$job_id)
          sys_status$log_oversub <- paste0(
            "\u2601\ufe0f Test 2 was submitted to Google Cloud.\n",
            "==========================================\n",
            "Job: ", sub$job_id, "\n",
            "Machine: ", input$gcp_machine_type, "\n",
            "Replicate workers selected in Step 2: ", requested_workers, "\n",
            "Replicate workers used in this check: ", probe_workers, "\n",
            "------------------------------------------\n",
            "Please wait while the cloud machine starts. The speed and memory report ",
            "will appear on this Test 2 tab when the check finishes."
          )
        }
        return()
      }
      
      res <- withProgress(message = "Parallel performance check", value = 0.5, {
        run_oversubscription_test(
          run_one_fn        = run_one_fn,
          n_cores           = probe_workers,
          requested_workers = requested_workers,
          combo_threads     = combo_threads,
          omp_threads       = omp_threads,
          cluster_setup_fn  = cluster_setup_fn,
          logical_cores     = logical_cores,
          total_tasks       = total_tasks,
          mem_abort_frac    = 0.95
        )
      })
      .apply_perf_result(
        res = res,
        cloud = FALSE,
        prog = NULL)
    }, error = function(e) {
      sys_status$mem_safe <- NA
      sys_status$memory_check_done <- FALSE
      sys_status$memory_retest_required <- TRUE
      sys_status$log_oversub <- paste0(
        "❌ Error during parallel performance check:\n",
        e$message
      )
    })
  })
  
  
  output$test_sim_plot <- renderPlot({
    req(input$test_var_y)
    
    res_raw <- test_sim_data()
    
    validate(
      need(
        !is.null(res_raw),
        "Run Model Validation to display test results."
      )
    )
    
    if (inherits(res_raw, "data.frame")) {
      res_df <- res_raw
    } else {
      res_df <- dplyr::bind_rows(res_raw)
    }
    
    validate(
      need(
        nrow(res_df) > 0,
        "The validation run returned no data to plot."
      )
    )
    
    var_code <- input$test_var_y
    
    required_columns <- c(
      "year",
      "policy_combo_id",
      var_code
    )
    
    missing_columns <- setdiff(
      required_columns,
      names(res_df)
    )
    
    validate(
      need(
        length(missing_columns) == 0,
        paste(
          "The simulation output is missing:",
          paste(missing_columns, collapse = ", ")
        )
      )
    )
    plot_type <- if(!is.null(input$test_plot_type)) input$test_plot_type else "line"
    # test_sim_data() List, DataFrame
    res_raw <- test_sim_data()
    if(inherits(res_raw, "data.frame")) {
      res_df <- res_raw
    } else {
      # List, (bind_rows list of dataframes)
      res_df <- dplyr::bind_rows(res_raw)
    }
    
    var_code <- input$test_var_y
    
    
    t_stable_start <- input$transient_years
    t_policy_start <- input$transient_years + input$stable_years
    
    # 1. Labels Map
    var_base_name <- switch(var_code,
                            "Sden"    = "Spawning fish density",
                            "Rden"    = "Recruit density",
                            "AdultN"  = "Adult abundance",
                            "AgeFRN"   = "Recruit (fishery) abundance",
                            "Yield_n" = "Yield",
                            "N_pop"   = "Population size",
                            "PSD_Q"   = "PSD (Quality)",
                            "PSD_P"   = "PSD (Preferred)",
                            "PSD_M"   = "PSD (Memorable)",
                            "PSD_T"   = "PSD (Trophy)",
                            "Enc_Q"   = "Angler Encounters (Quality)",
                            "Enc_P"   = "Angler Encounters (Preferred)",
                            "Enc_M"   = "Angler Encounters (Memorable)",
                            "Enc_T"   = "Angler Encounters (Trophy)",
                            "trophy_seen" = "Months of Trophy Seen",
                            var_code
    )
    
    unit_suffix <- case_when(
      var_code %in% c("Sden", "Rden") ~ "(ind/ha)",
      var_code %in% c("AdultN", "AgeFRN", "Yield_n", "N_pop") ~ "(number)",
      grepl("PSD", var_code) | grepl("Enc", var_code) ~ "(%)",
      var_code == "trophy_seen"                                  ~ "(months)",
      TRUE ~ ""
    )
    
    final_y_label <- paste(var_base_name, unit_suffix)
    
    my_theme <- theme_bw(base_size = 14) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom"
      )
    
    
    
    # 4. Plotting
    if (var_code == "trophy_seen") {
      # Enc_T: 
      t_blue <- t_stable_start
      t_red  <- t_policy_start
      
      val_before <- res_df %>%
        filter(year > t_blue & year <= t_red) %>%
        filter(policy_combo_id == min(policy_combo_id)) %>%
        summarise(Value = sum(trophy_seen == TRUE | trophy_seen == "TRUE", na.rm = TRUE)) %>%
        mutate(Group = "Before Policy")
      val_policy <- res_df %>%
        filter(year > t_red) %>%
        group_by(policy_combo_id) %>%
        summarise(Value = sum(trophy_seen == TRUE | trophy_seen == "TRUE", na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(Group = paste0("Policy ", policy_combo_id))
      plot_dt <- bind_rows(val_before, val_policy)
      pol_levels <- c("Before Policy", paste0("Policy ", sort(unique(val_policy$policy_combo_id))))
      plot_dt$Group <- factor(plot_dt$Group, levels = pol_levels)
      n_pols    <- length(pol_levels) - 1
      pol_cols  <- if (n_pols > 0) scales::hue_pal()(n_pols) else character(0)
      bar_cols  <- c("Before Policy" = "#999999")
      if (n_pols > 0) {
        names(pol_cols) <- paste0("Policy ", sort(unique(val_policy$policy_combo_id)))
        bar_cols <- c(bar_cols, pol_cols)
      }
      
      ggplot(plot_dt, aes(x = Group, y = Value, fill = Group)) +
        geom_bar(stat = "identity", width = 0.6, color = "black") +
        scale_fill_manual(values = bar_cols) +
        scale_y_continuous(
          limits = c(0, NA),
          expand = expansion(mult = c(0, 0.1)),
          breaks = scales::breaks_width(1)
        ) +
        labs(
          title    = "Total Months with Trophy Fish Sighting (trophy_seen = TRUE)",
          subtitle = paste0("Burn-in End: Year ", t_blue, " | Policy Start: Year ", t_red, "+"),
          y = final_y_label,
          x = NULL
        ) +
        my_theme +
        theme(legend.position = "none")
      
    } else {
      # Line Chart
      df_burn <- res_df %>%
        filter(year <= t_policy_start) %>%
        filter(policy_combo_id == min(policy_combo_id)) %>%
        mutate(Group = "Burn-in")
      
      anchor_point <- df_burn %>% filter(year == t_policy_start)
      df_pols_raw  <- res_df %>% filter(year > t_policy_start)
      unique_pols  <- sort(unique(df_pols_raw$policy_combo_id))
      df_pols_list <- list()
      
      for (pid in unique_pols) {
        current_pol_data <- df_pols_raw %>% filter(policy_combo_id == pid)
        current_anchor   <- anchor_point %>% mutate(policy_combo_id = pid)
        combined_data    <- bind_rows(current_anchor, current_pol_data) %>%
          mutate(Group = paste0("Policy ", pid))
        df_pols_list[[length(df_pols_list) + 1]] <- combined_data
      }
      
      df_pols_final <- bind_rows(df_pols_list)
      raw_dt <- bind_rows(df_burn, df_pols_final)
      
      groups    <- c("Burn-in", paste0("Policy ", unique_pols))
      raw_dt$Group <- factor(raw_dt$Group, levels = groups)
      
      final_cols <- c("Burn-in" = "#999999")
      other_groups <- groups[groups != "Burn-in"]
      if (length(other_groups) > 0) {
        p_cols <- scales::hue_pal()(length(other_groups))
        names(p_cols) <- other_groups
        final_cols <- c(final_cols, p_cols)
      }
      agg_ts_dt <- raw_dt %>%
        group_by(Group, year) %>%
        summarise(Value = mean(.data[[var_code]], na.rm = TRUE), .groups = "drop") %>%
        mutate(Group = recode(Group, "Burn-in" = "Before Policy"))
      names(final_cols)[names(final_cols) == "Burn-in"] <- "Before Policy"
      agg_ts_dt$Group <- factor(agg_ts_dt$Group,
                                levels = c("Before Policy", paste0("Policy ", unique_pols)))
      
      y_max <- if (nrow(agg_ts_dt) > 0) max(agg_ts_dt$Value, na.rm = TRUE) else 1
      
      ggplot(agg_ts_dt, aes(x = year, y = Value, color = Group)) +
        geom_line(linewidth = 1.2) +
        scale_color_manual(values = final_cols) +
        scale_x_continuous(limits = c(0, NA)) +
        
        geom_vline(xintercept = t_stable_start, linetype = "dashed",
                   color = "blue", linewidth = 1) +
        annotate("text", x = t_stable_start, y = y_max,
                 label  = paste0("Burn-in End (Year ", t_stable_start, ")"),
                 angle  = 90, vjust = -0.5, hjust = 1, color = "blue") +
        
        geom_vline(xintercept = t_policy_start, linetype = "solid",
                   color = "red", linewidth = 1) +
        annotate("text", x = t_policy_start, y = y_max,
                 label  = paste0("Policy Start (Year ", t_policy_start, ")"),
                 angle  = 90, vjust = -0.5, hjust = 1, color = "red") +
        
        labs(title = NULL, y = final_y_label, x = "Year", color = NULL) +
        my_theme
    }
  })
  
  
  
  # Step 2b: Batch Run (RUN SIMULATION MODE)
  # run_whole_scenario_job_shiny() defined in R/helper.R
  
  roots <- c(wd = getwd(), shinyFiles::getVolumes()())
  
  # ============================================================================
  # SESSION SETTINGS: save / load all Step 1 & Step 2 settings as .rds
  # ============================================================================
  
  # Inputs that describe the *configuration* and should be saved.
  # Runtime / result-view selectors and machine-specific paths are excluded.
  SETTINGS_INPUT_IDS <- c(
    "boot_b_vbgf", "show_growth_advanced", "vbgf_seed_manual", "alk_seed_manual",
    "missing_age_mode",
    "z_method", "z_last", "z_boot_bg2", "show_z_advanced", "z_seed_manual",
    "z_full", "use_z_estimation", "F_over_Z_ratio", "fixed_adult_M",
    "min_adult_age", "f_age_mode",
    "surv_a", "surv_b", "surv_c", "surv_d_avg1", "surv_d_avg2",
    "g1_a", "g1_b", "g1_c", "g1_d_avg", "g2_a", "g2_b", "g2_c", "g2_d_avg",
    "use_dd_effects", "use_dd_survival", "use_dd_growth_adult", "use_dd_growth_juv",
    "juv_annual_M",
    "flag_harvest_curve", "harv_L50", "harv_pmax", "harv_slope", "harv_fixed_pmax",
    "psd_stock", "psd_quality", "psd_preferred", "psd_memorable", "psd_trophy",
    "age_spawn", "spawn_month", "recruit_entry_month", "rec_a", "rec_b",
    "use_ricker",
    "transient_years", "stable_years", "policy_years",
    "lake_area_ha", "initial_pop_size", "month_weights",
    "ESD_vec", "pae_vec", "rm_vec", "comp_breaks", "comp_probs",
    "compliance_mode",
    "n_iter", "seed", "n_cores",
    "simulation_engine", "omp_nthreads",
    "use_gpu", "gpu_thread_count", "fast_forward_mode",
    # Cloud settings. The service-account key is deliberately excluded so
    # credentials never travel inside a settings file.
    "use_cloud", "gcp_project", "gcp_region", "gcp_bucket", "gcp_machine_type",
    "gcp_container_image"
  )
  
  # Map an input id to the updater family to use on load.
  .input_widget_type <- function(id) {
    checkbox <- c("show_growth_advanced","use_dd_effects","use_dd_survival",
                  "use_dd_growth_adult","use_dd_growth_juv","flag_harvest_curve",
                  "use_z_estimation","show_z_advanced","use_ricker","use_gpu",
                  "simulation_engine","use_cloud")
    radio    <- c("f_age_mode","fast_forward_mode","missing_age_mode")
    select   <- c("z_method")
    slider   <- c("n_cores","gpu_thread_count","omp_nthreads")
    checkgrp <- c("compliance_mode")
    text     <- c("month_weights","ESD_vec","pae_vec","rm_vec","comp_breaks","comp_probs",
                  "gcp_project","gcp_region","gcp_bucket","gcp_machine_type",
                  "gcp_container_image")
    if (id %in% checkbox) return("checkbox")
    if (id %in% radio)    return("radio")
    if (id %in% select)   return("select")
    if (id %in% slider)   return("slider")
    if (id %in% checkgrp) return("checkgroup")
    if (id %in% text)     return("text")
    "numeric"
  }
  
  .read_csv_safe <- function(path) {
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch(as.data.frame(readr::read_csv(path, show_col_types = FALSE)),
             error = function(e) NULL)
  }
  
  collect_settings <- function() {
    input_vals <- list()
    for (id in SETTINGS_INPUT_IDS) {
      v <- input[[id]]
      if (!is.null(v)) input_vals[[id]] <- v
    }
    list(
      meta = list(
        format_version = 1L,
        saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        package_version = tryCatch(as.character(utils::packageVersion("craibm")),
                                   error = function(e) "unknown")
      ),
      inputs = input_vals,
      results = list(
        theta_clean     = vals$theta_clean,
        growth_data     = vals$growth_data,
        z_dist          = vals$z_dist,
        alk_data        = vals$alk_data,
        alk_source      = vals$alk_source,
        alk_info        = vals$alk_info,
        alk_bin_width   = vals$alk_bin_width,
        alk_seed        = vals$alk_seed,
        vbgf_seed       = vals$vbgf_seed,
        z_seed          = vals$z_seed,
        growth_fit_note = vals$growth_fit_note,
        # The automatic fast-forward duration is derived from the global
        # parameters. Saving it means a restored session can show the same
        # figure without asking the user to submit those parameters again.
        T_safe_info     = vals$T_safe_info
      ),
      files = list(
        growth = if (!is.null(input$file_growth)) .read_csv_safe(input$file_growth$datapath) else NULL,
        alk    = if (!is.null(input$file_alk))    .read_csv_safe(input$file_alk$datapath)    else NULL,
        size = if (!is.null(input$size_csv)) {
          .read_csv_safe(input$size_csv$datapath)
        } else if (!is.null(vals$loaded_size_csv)) {
          as.data.frame(vals$loaded_size_csv)
        } else {
          NULL
        }
      ),
      status = list(
        vbgf_ok     = sys_status$vbgf_ok,
        alk_ok      = sys_status$alk_ok,
        z_ok        = sys_status$z_ok,
        survival_ok = sys_status$survival_ok,
        global_ok   = sys_status$global_ok,
        design_ok   = sys_status$design_ok,
        msg_vbgf    = sys_status$msg_vbgf,
        msg_alk     = sys_status$msg_alk,
        msg_z       = sys_status$msg_z
      )
    )
  }
  
  shinyFiles::shinyFileSave(input, "save_settings", roots = roots, session = session,
                            filetypes = c("rds"))
  
  observeEvent(input$save_settings, {
    fileinfo <- shinyFiles::parseSavePath(roots, input$save_settings)
    if (nrow(fileinfo) == 0) return()
    path <- as.character(fileinfo$datapath)
    tryCatch({
      saveRDS(collect_settings(), path)
      showNotification(paste0("Settings saved to: ", basename(path)),
                       type = "message", duration = 6)
    }, error = function(e) {
      showNotification(paste("Could not save settings:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_settings, {
    req(input$load_settings)
    saved <- tryCatch(readRDS(input$load_settings$datapath), error = function(e) NULL)
    if (is.null(saved) || is.null(saved$inputs)) {
      showNotification("This file is not a valid craibm settings file.", type = "error")
      return()
    }
    
    iv <- saved$inputs
    for (id in names(iv)) {
      val <- iv[[id]]
      if (is.null(val)) next
      switch(.input_widget_type(id),
             numeric  = updateNumericInput(session, id, value = val),
             text     = updateTextInput(session, id, value = val),
             select   = updateSelectInput(session, id, selected = val),
             checkbox = updateCheckboxInput(session, id, value = isTRUE(val)),
             radio    = updateRadioButtons(session, id, selected = val),
             slider   = updateSliderInput(session, id, value = val),
             checkgroup = updateCheckboxGroupInput(session, id, selected = val),
             updateTextInput(session, id, value = as.character(val))
      )
    }
    
    r <- saved$results
    if (!is.null(r)) {
      vals$theta_clean     <- r$theta_clean
      vals$growth_data     <- r$growth_data
      vals$z_dist          <- r$z_dist
      vals$alk_data        <- r$alk_data
      vals$alk_source      <- r$alk_source
      vals$alk_info        <- r$alk_info
      vals$alk_bin_width   <- r$alk_bin_width
      vals$alk_seed        <- r$alk_seed
      vals$vbgf_seed       <- r$vbgf_seed
      vals$z_seed          <- r$z_seed
      vals$growth_fit_note <- r$growth_fit_note
      if (!is.null(r$T_safe_info)) vals$T_safe_info <- r$T_safe_info
    }
    
    f <- saved$files
    missing_files <- character(0)
    if (!is.null(f)) {
      if (!is.null(f$alk) && is.null(vals$alk_data)) {
        vals$alk_data <- f$alk
        if (is.null(vals$alk_source)) vals$alk_source <- "file"
      }
      vals$loaded_size_csv <- f$size
    }
    # Settings files written by earlier versions may predate the storing of
    # uploaded tables. Say so at load time rather than letting it surface much
    # later as an unexplained missing scenario in Step 3a.
    if (is.null(f) || is.null(f$size)) {
      missing_files <- c(missing_files, "Size-limit CSV")
    }
    
    st <- saved$status
    if (!is.null(st)) {
      sys_status$vbgf_ok     <- isTRUE(st$vbgf_ok)
      sys_status$alk_ok      <- isTRUE(st$alk_ok)
      sys_status$z_ok        <- isTRUE(st$z_ok)
      sys_status$survival_ok <- isTRUE(st$survival_ok)
      sys_status$global_ok   <- isTRUE(st$global_ok)
      sys_status$design_ok   <- isTRUE(st$design_ok)
      if (!is.null(st$msg_vbgf)) sys_status$msg_vbgf <- st$msg_vbgf
      if (!is.null(st$msg_alk))  sys_status$msg_alk  <- st$msg_alk
      if (!is.null(st$msg_z))    sys_status$msg_z    <- st$msg_z
    }
    # Machine-bound status is deliberately reset: user must redo on this machine.
    sys_status$runcontrol_ok          <- FALSE
    sys_status$test_run_done          <- FALSE
    sys_status$mem_safe               <- NA
    sys_status$memory_check_done      <- FALSE
    sys_status$memory_retest_required <- FALSE
    
    meta <- saved$meta
    sys_status$loaded_from <- if (!is.null(meta)) {
      paste0("Loaded settings saved on ",
             if (!is.null(meta$saved_at)) meta$saved_at else "unknown",
             if (!is.null(meta$package_version)) paste0(" (craibm ", meta$package_version, ")") else "")
    } else "Loaded settings from file."
    
    if (length(missing_files) > 0L) {
      sys_status$loaded_from <- paste0(
        sys_status$loaded_from,
        "\n\u26a0\ufe0f This file did not include: ",
        paste(missing_files, collapse = ", "),
        ". Upload it again in Step 1 and submit that page."
      )
      showNotification(
        paste0("Settings loaded, but the ", paste(missing_files, collapse = " and "),
               " was not in the file. Upload it again in Step 1."),
        type = "warning", duration = 15
      )
    } else {
      showNotification(
        "Settings loaded. Please re-confirm Run Control and re-run the parallel performance check on this machine.",
        type = "message", duration = 10
      )
    }
  })
  
  # ---- Small helpers used by the cloud section -------------------------------
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  
  .format_test_duration <- function(seconds) {
    if (
      is.null(seconds) ||
      length(seconds) == 0L ||
      is.na(seconds[[1L]]) ||
      !is.finite(as.numeric(seconds[[1L]]))
    ) {
      return("n/a")
    }
    
    seconds <- as.numeric(seconds[[1L]])
    
    if (seconds < 60) {
      sprintf("%.1f sec", seconds)
    } else if (seconds < 3600) {
      sprintf("%.1f min", seconds / 60)
    } else {
      sprintf("%.2f hr", seconds / 3600)
    }
  }
  
  
  .estimate_full_run <- function(one_task_seconds) {
    one_task_seconds <- suppressWarnings(
      as.numeric(one_task_seconds[[1L]])
    )
    
    if (!is.finite(one_task_seconds) || one_task_seconds <= 0) {
      return(list(
        total_tasks = NA_integer_,
        workers = NA_integer_,
        seconds = NA_real_,
        formatted = "n/a"
      ))
    }
    
    scenarios <- get_scenarios_df()
    n_scenarios <- nrow(scenarios)
    
    n_replicates <- suppressWarnings(
      as.integer(input$n_iter)
    )
    
    workers_requested <- suppressWarnings(
      as.integer(input$n_cores)
    )
    
    if (is.na(n_replicates) || n_replicates < 1L) {
      n_replicates <- 1L
    }
    
    total_tasks <- max(
      1L,
      n_scenarios * n_replicates
    )
    
    workers <- min(
      max(1L, workers_requested),
      total_tasks
    )
    
    waves <- ceiling(total_tasks / workers)
    estimated_seconds <- waves * one_task_seconds
    
    list(
      total_tasks = total_tasks,
      workers = workers,
      seconds = estimated_seconds,
      formatted = .format_test_duration(estimated_seconds)
    )
  }
  
  .cloud_timing_lines <- function(prog) {
    fmt <- function(x) {
      if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return("n/a")
      x <- as.numeric(x[[1L]])
      if (x < 90) sprintf("%.1f sec", x) else sprintf("%.1f min", x / 60)
    }
    
    inside_container <- if (
      !is.null(prog$startup_sec) &&
      !is.null(prog$compute_sec) &&
      is.finite(as.numeric(prog$startup_sec)) &&
      is.finite(as.numeric(prog$compute_sec))
    ) {
      as.numeric(prog$startup_sec) + as.numeric(prog$compute_sec)
    } else {
      NA_real_
    }
    
    paste0(
      "Container setup and payload download: ", fmt(prog$startup_sec), "\n",
      "Model computation:                    ", fmt(prog$compute_sec), "\n",
      "Measured inside the container:        ", fmt(inside_container), "\n",
      "Google Batch total runtime is longer because it also includes VM\n",
      "provisioning, image pull, result upload, and shutdown."
    )
  }
  
  .cloud_finish_summary <- function(prog) {
    paste0(
      "Finished.\n",
      .cloud_timing_lines(prog),
      if (!is.na(prog$message)) paste0("\n", prog$message) else ""
    )
  }
  
  # ============================================================================
  # GOOGLE CLOUD EXECUTION
  # ============================================================================
  
  cloud_settings <- reactive({
    list(
      key_path     = if (!is.null(input$gcp_key)) input$gcp_key$datapath else NULL,
      project      = trimws(as.character(input$gcp_project %||% "")),
      region       = trimws(as.character(input$gcp_region %||% "")),
      bucket       = trimws(as.character(input$gcp_bucket %||% "")),
      machine_type = trimws(as.character(input$gcp_machine_type %||% "")),
      image        = trimws(as.character(input$gcp_container_image %||% ""))
    )
  })
  
  # ---- Background cloud watcher --------------------------------------------
  # All authentication refreshes and Cloud API requests happen in this separate
  # R process. The main Shiny process never waits for a network request.
  .cloud_status_file <- function(jid) {
    file.path(tempdir(), paste0("craibm-watch-", jid, ".rds"))
  }
  
  cloud_watch <- ExtendedTask$new(
    function(key_path, bucket, project, region, jid, status_path,
             max_hours = 48) {
      future({
        library(craibm)
        
        started   <- Sys.time()
        auth      <- cloud_auth(key_path)
        fails     <- 0L
        max_fails <- 60L
        
        write_status <- function(...) {
          try(saveRDS(list(...), status_path), silent = TRUE)
        }
        
        write_status(
          phase = "waiting",
          detail = "Submitted, waiting for a machine.",
          done = NA_integer_,
          total = NA_integer_,
          batch_state = NA_character_,
          checked_at = Sys.time()
        )
        
        repeat {
          elapsed_min <- as.numeric(
            difftime(Sys.time(), started, units = "mins")
          )
          
          wait_s <- if (elapsed_min < 5) {
            15
          } else if (elapsed_min < 30) {
            60
          } else if (elapsed_min < 240) {
            180
          } else {
            300
          }
          
          Sys.sleep(wait_s)
          
          auth <- tryCatch(
            cloud_refresh_auth(auth),
            error = function(e) {
              tryCatch(cloud_auth(key_path), error = function(e2) NULL)
            }
          )
          
          if (is.null(auth)) {
            fails <- fails + 1L
            if (fails >= max_fails) {
              return(list(
                outcome = "give_up",
                reason = "Authentication kept failing."
              ))
            }
            next
          }
          
          prog <- tryCatch(
            cloud_poll_progress(auth, bucket, jid),
            error = function(e) NULL
          )
          
          if (is.null(prog)) {
            fails <- fails + 1L
            write_status(
              phase = "unreachable",
              detail = paste0(
                "Cannot reach Cloud Storage (attempt ",
                fails,
                "). The cloud job is unaffected."
              ),
              done = NA_integer_,
              total = NA_integer_,
              batch_state = NA_character_,
              checked_at = Sys.time()
            )
            if (fails >= max_fails) {
              return(list(
                outcome = "give_up",
                reason = "Progress could not be read for a long time."
              ))
            }
            next
          }
          fails <- 0L
          
          if (isTRUE(prog$available) && identical(prog$status, "done")) {
            write_status(
              phase = "done",
              detail = "Finished.",
              done = prog$done,
              total = prog$total,
              batch_state = NA_character_,
              checked_at = Sys.time()
            )
            return(list(outcome = "done", prog = prog))
          }
          
          if (isTRUE(prog$available) && identical(prog$status, "failed")) {
            write_status(
              phase = "failed",
              detail = "The cloud run failed.",
              done = prog$done,
              total = prog$total,
              batch_state = NA_character_,
              checked_at = Sys.time()
            )
            return(list(outcome = "failed", prog = prog))
          }
          
          need_state <- !isTRUE(prog$available) || isTRUE(prog$stale)
          state <- NA_character_
          
          if (need_state) {
            st <- tryCatch(
              cloud_job_state(auth, project, region, jid),
              error = function(e) NULL
            )
            
            if (!is.null(st) && isTRUE(st$ok) && !is.na(st$state)) {
              state <- st$state
              
              if (state %in% c("CANCELLED", "DELETION_IN_PROGRESS")) {
                return(list(outcome = "cancelled", state = state))
              }
              if (identical(state, "FAILED")) {
                return(list(outcome = "batch_failed", state = state))
              }
              if (identical(state, "SUCCEEDED") &&
                  !isTRUE(prog$available)) {
                return(list(outcome = "no_report", state = state))
              }
            }
          }
          
          if (isTRUE(prog$available)) {
            write_status(
              phase = "running",
              detail = if (!is.na(prog$done) && !is.na(prog$total)) {
                paste0(prog$done, " of ", prog$total, " runs finished.")
              } else {
                "Running on the cloud machine."
              },
              done = prog$done,
              total = prog$total,
              batch_state = state,
              checked_at = Sys.time()
            )
          } else {
            queued <- state %in% c("QUEUED", "SCHEDULED")
            write_status(
              phase = if (isTRUE(queued)) "queued" else "waiting",
              detail = if (isTRUE(queued)) {
                paste0(
                  "Still QUEUED after ",
                  round(elapsed_min),
                  " minutes. Google holds a job when the requested machine ",
                  "type is unavailable in this region, or a quota is exhausted. ",
                  "No computation has started, so stopping now costs nothing."
                )
              } else {
                "Waiting for the cloud machine to start."
              },
              done = NA_integer_,
              total = NA_integer_,
              batch_state = state,
              checked_at = Sys.time()
            )
          }
          
          if (elapsed_min > max_hours * 60) {
            return(list(
              outcome = "gave_up_time",
              reason = paste0(
                "Still not finished after ",
                max_hours,
                " hours of watching."
              )
            ))
          }
        }
      }, seed = TRUE)
    }
  )
  
  # Obtain a token, renewing it when the previous one is close to expiry.
  # The stored credentials are read under isolate(): this runs inside the
  # polling observers, and a plain read would tie them to a value that is
  # rewritten on every refresh.
  cloud_token <- function() {
    cs <- isolate(cloud_settings())
    if (is.null(cs$key_path)) stop("Upload a service-account key first.")
    
    current <- isolate(proc_state$cloud_auth)
    
    if (is.null(current) || !identical(current$json_path, cs$key_path)) {
      current <- cloud_auth(cs$key_path)
    } else {
      current <- cloud_refresh_auth(current)
    }
    
    proc_state$cloud_auth <- current
    current
  }
  
  # ---- Connection check ------------------------------------------------------
  observeEvent(input$cloud_check, {
    cs <- cloud_settings()
    
    chk <- check_cloud_inputs(cs$key_path, cs$project, cs$region,
                              cs$bucket, cs$machine_type, cs$image)
    if (!chk$pass) {
      proc_state$cloud_verified <- FALSE
      sys_status$log_cloud <- paste0("Cloud settings are incomplete.\n\n", chk$msg)
      return()
    }
    
    sys_status$log_cloud <- "Checking the connection to Google Cloud..."
    
    res <- tryCatch({
      auth <- cloud_token()
      cloud_check_setup(auth, cs$project, cs$region, cs$bucket)
    }, error = function(e) list(pass = FALSE, msg = conditionMessage(e)))
    
    proc_state$cloud_verified <- isTRUE(res$pass)
    sys_status$log_cloud <- paste0(
      if (isTRUE(res$pass)) "\u2705 " else "\u274c ", res$msg,
      if (isTRUE(res$pass)) paste0(
        "\n\nMachine type: ", cs$machine_type,
        " (", parse_machine_type_cores(cs$machine_type), " vCPU)",
        "\nContainer: ", cs$image,
        "\nBatch worker identity: ", res$service_account %||% "uploaded-key account",
        "\nResults will be written to: gs://", cs$bucket, "/jobs/") else ""
    )
  })
  
  output$cloud_status_log <- renderText({
    if (is.null(sys_status$log_cloud)) {
      "Not checked yet. Fill in the details above and check the connection."
    } else sys_status$log_cloud
  })
  
  # ---- Submitting work -------------------------------------------------------
  
  # Shared by all three cloud entry points: validates, uploads and starts.
  cloud_submit <- function(task_type, payload, label) {
    cs <- cloud_settings()
    
    if (!is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")) {
      busy_msg <- paste0(
        "A cloud job from this session is still being tracked.\n",
        "Job: ", proc_state$cloud_job_id, "\n",
        "State: ", proc_state$cloud_status, "\n\n",
        "Stop that job before starting another one. If it was already cancelled ",
        "in the Google Cloud console, pressing Stop here clears the tracking and ",
        "releases this page."
      )
      showNotification(
        "A cloud job is still being tracked. Stop it before starting another.",
        type = "warning", duration = 12
      )
      return(list(ok = FALSE, msg = busy_msg))
    }
    
    chk <- check_cloud_inputs(cs$key_path, cs$project, cs$region,
                              cs$bucket, cs$machine_type, cs$image)
    if (!chk$pass) {
      showNotification("Cloud settings are incomplete. See Step 2.", type = "error")
      return(list(ok = FALSE, msg = chk$msg))
    }
    
    job_id <- cloud_make_job_id()
    
    res <- tryCatch({
      auth <- cloud_token()
      cloud_upload_payload(auth, cs$bucket, job_id, payload)
      cloud_submit_batch(
        auth                   = auth,
        project                = cs$project,
        region                 = cs$region,
        bucket                 = cs$bucket,
        job_id                 = job_id,
        machine_type           = cs$machine_type,
        task_type              = task_type,
        image                  = cs$image,
        worker_service_account = auth$email
      )
      list(ok = TRUE)
    }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    
    if (!isTRUE(res$ok)) {
      showNotification("The cloud job could not be started.", type = "error")
      return(res)
    }
    
    proc_state$cloud_job_id     <- job_id
    proc_state$cloud_task_type  <- task_type
    proc_state$cloud_status     <- "submitted"
    proc_state$cloud_done       <- NA_integer_
    proc_state$cloud_total      <- NA_integer_
    proc_state$cloud_poll_fails <- 0L
    proc_state$cloud_last_report <- NULL
    proc_state$cloud_no_progress <- 0L
    proc_state$cloud_submitted_at <- Sys.time()
    proc_state$cloud_queue_warned <- FALSE
    if (identical(task_type, "perfcheck")) {
      proc_state$cloud_perf_requested <- suppressWarnings(
        as.integer(payload$requested_workers)
      )
      proc_state$cloud_perf_probe <- suppressWarnings(
        as.integer(payload$probe_workers)
      )
    } else {
      proc_state$cloud_perf_requested <- NA_integer_
      proc_state$cloud_perf_probe <- NA_integer_
    }
    # Clear the previous run's report so a finished result is never shown
    # alongside a job that has only just been submitted.
    if (identical(task_type, "validation")) {
      sys_status$log_2a <- NULL
    } else if (identical(task_type, "perfcheck")) {
      sys_status$log_oversub <- NULL
    }
    proc_state$cloud_result_uri <- cloud_result_uri(cs$bucket, job_id)
    sys_status$cloud_summary <- NULL
    showNotification(
      paste0(
        label,
        " was submitted to Google Cloud. ",
        "Please wait while the cloud machine starts."
      ),
      id = "cloud_job_notice",
      type = "message",
      duration = NULL
    )
    list(ok = TRUE, job_id = job_id)
  }
  
  .cloud_start_watch <- function(job_id) {
    cs <- isolate(cloud_settings())
    sp <- .cloud_status_file(job_id)
    try(unlink(sp), silent = TRUE)
    
    proc_state$cloud_watch_job <- job_id
    
    shinyjs::runjs(sprintf("
      window._craibmT0 = %s;
      clearInterval(window._craibmClk);
      window._craibmClk = setInterval(function () {
        var el = document.getElementById('cloud_clock');
        if (!el) return;
        var s = Math.floor((Date.now() - window._craibmT0) / 1000);
        var h = Math.floor(s / 3600), m = Math.floor((s %% 3600) / 60);
        el.textContent = (h > 0 ? h + ' h ' : '') + m + ' min ' + (s %% 60) + ' s';
      }, 1000);
    ", format(as.numeric(Sys.time()) * 1000, scientific = FALSE)))
    
    cloud_watch$invoke(
      key_path    = cs$key_path,
      bucket      = cs$bucket,
      project     = cs$project,
      region      = cs$region,
      jid         = job_id,
      status_path = sp,
      max_hours   = 48
    )
  }
  
  .cloud_stop_clock <- function() {
    shinyjs::runjs("clearInterval(window._craibmClk);")
  }
  
  # ---- Watching a running job ------------------------------------------------
  
  .cloud_fetch_result <- function(job_id, file_name) {
    cs <- isolate(cloud_settings())
    dest <- file.path(tempdir(), paste0("craibm-cloud-", job_id))
    auth <- cloud_token()
    dl <- cloud_download_results(auth, cs$bucket, job_id, dest)
    if (!isTRUE(dl$pass)) {
      return(list(ok = FALSE, msg = dl$msg, value = NULL))
    }
    
    result_path <- file.path(dest, file_name)
    if (!file.exists(result_path)) {
      return(list(
        ok = FALSE,
        msg = paste0("The cloud archive did not contain ", file_name, "."),
        value = NULL
      ))
    }
    
    value <- tryCatch(readRDS(result_path), error = function(e) e)
    if (inherits(value, "error")) {
      return(list(
        ok = FALSE,
        msg = paste0("The downloaded ", file_name, " could not be read: ",
                     conditionMessage(value)),
        value = NULL
      ))
    }
    list(ok = TRUE, msg = dl$msg, value = value)
  }
  
  .cloud_apply_perf_result <- function(res, prog) {
    .apply_perf_result(
      res = res,
      cloud = TRUE,
      prog = prog
    )
  }
  
  .cloud_mark_failed <- function(message) {
    removeNotification("cloud_job_notice")
    .clear_active_run()
    proc_state$cloud_status <- "failed"
    tt <- isolate(proc_state$cloud_task_type)
    if (identical(tt, "validation")) {
      sys_status$test_run_done <- FALSE
    } else if (identical(tt, "perfcheck")) {
      sys_status$mem_safe <- NA
      sys_status$memory_check_done <- FALSE
      sys_status$memory_retest_required <- TRUE
    }
    sys_status$cloud_summary <- message
    showNotification("The cloud run failed.", type = "error", duration = NULL)
  }
  
  # A job that was deliberately stopped is not a failure. It is reported
  # separately so the wording matches what actually happened, whether the job
  # was cancelled from this page or from the Google Cloud console.
  .cloud_mark_cancelled <- function(message, notify = TRUE) {
    removeNotification("cloud_job_notice")
    .clear_active_run()
    proc_state$cloud_status <- "cancelled"
    tt <- isolate(proc_state$cloud_task_type)
    if (identical(tt, "validation")) {
      sys_status$test_run_done <- FALSE
    } else if (identical(tt, "perfcheck")) {
      sys_status$mem_safe <- NA
      sys_status$memory_check_done <- FALSE
      sys_status$memory_retest_required <- TRUE
    }
    sys_status$cloud_summary <- message
    if (isTRUE(notify)) {
      showNotification("The cloud job was cancelled.", type = "warning", duration = 10)
    }
  }
  
  # Clears the display left by a previous cloud job so a new attempt does not
  # start out showing the outcome of the last one.
  .cloud_reset_display <- function() {
    if (!is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")) {
      return(invisible(FALSE))
    }
    proc_state$cloud_status       <- NULL
    proc_state$cloud_job_id       <- NULL
    proc_state$cloud_task_type    <- NULL
    proc_state$cloud_done         <- NA_integer_
    proc_state$cloud_total        <- NA_integer_
    proc_state$cloud_last_report  <- NULL
    proc_state$cloud_result_uri   <- NULL
    proc_state$cloud_no_progress  <- 0L
    proc_state$cloud_poll_fails   <- 0L
    proc_state$cloud_submitted_at <- NULL
    proc_state$cloud_queue_warned <- FALSE
    proc_state$cloud_perf_requested <- NA_integer_
    proc_state$cloud_perf_probe     <- NA_integer_
    sys_status$cloud_summary      <- NULL
    invisible(TRUE)
  }
  
  # Download and apply a completed Test 1 or Test 2 result. Full-model archives
  # stay in Cloud Storage until the user chooses Download.
  .cloud_collect_result <- function(jid, prog) {
    tt <- isolate(proc_state$cloud_task_type)
    if (is.null(tt)) tt <- ""
    
    msg <- switch(
      tt,
      validation = "Model validation has finished. See the Test 1 report.",
      perfcheck = paste0(
        "Parallel performance check has finished. ",
        "See the Test 2 report."
      ),
      full = paste0(
        "The full simulation has finished. ",
        "See Step 3b and download the results."
      ),
      "The cloud run has finished."
    )
    
    showNotification(
      msg,
      id = "cloud_job_notice",
      type = "message",
      duration = NULL
    )
    
    if (identical(tt, "full")) return(invisible(TRUE))
    
    file_name <- if (identical(tt, "validation")) {
      "validation_result.rds"
    } else if (identical(tt, "perfcheck")) {
      "perfcheck_result.rds"
    } else {
      return(invisible(TRUE))
    }
    
    fetched <- tryCatch(
      .cloud_fetch_result(jid, file_name),
      error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e))
      }
    )
    
    if (!isTRUE(fetched$ok)) {
      if (identical(tt, "perfcheck")) {
        sys_status$mem_safe <- NA
        sys_status$memory_check_done <- FALSE
        sys_status$memory_retest_required <- TRUE
        sys_status$log_oversub <- paste0(
          "\u26a0\ufe0f The check finished, but its verdict could not be loaded.\n",
          fetched$msg,
          "\nThe full run stays blocked until a valid verdict exists."
        )
      } else {
        sys_status$test_run_done <- FALSE
        sys_status$log_2a <- paste0(
          "\u26a0\ufe0f Validation finished, but its result could not be loaded.\n",
          fetched$msg,
          "\nThe archive is still available from the download control."
        )
      }
      return(invisible(FALSE))
    }
    
    tryCatch({
      if (identical(tt, "perfcheck")) {
        .cloud_apply_perf_result(fetched$value, prog)
      } else {
        test_sim_data(fetched$value)
        sys_status$test_run_done <- TRUE
        est <- .estimate_full_run(prog$compute_sec)
        sys_status$log_2a <- paste0(
          "\u2705 Test 1: Model Validation Complete\n",
          "==========================================\n",
          "Container preparation and input download: ",
          .format_test_duration(prog$startup_sec),
          "\n",
          "Model calculation for the selected scenario: ",
          .format_test_duration(prog$compute_sec),
          "\n",
          "------------------------------------------\n",
          "Planned full-model jobs: ",
          est$total_tasks,
          "\n",
          "Simultaneous replicate workers: ",
          est$workers,
          "\n",
          "Rough full-model calculation-time estimate: ",
          est$formatted,
          "\n",
          "Cloud machine startup, result writing and transfer add extra time.\n",
          "------------------------------------------\n",
          "The validation result was downloaded and the plot was updated."
        )
      }
    }, error = function(e) {
      if (identical(tt, "perfcheck")) {
        sys_status$mem_safe <- NA
        sys_status$memory_check_done <- FALSE
        sys_status$memory_retest_required <- TRUE
        sys_status$log_oversub <- paste0(
          "\u26a0\ufe0f The result was downloaded but could not be interpreted.\n",
          conditionMessage(e)
        )
      } else {
        sys_status$test_run_done <- FALSE
        sys_status$log_2a <- paste0(
          "\u26a0\ufe0f The result was downloaded but could not be interpreted.\n",
          conditionMessage(e)
        )
      }
    })
    
    invisible(TRUE)
  }
  
  # ExtendedTask pushes this observer awake only when its separate R process
  # returns. No timer and no Cloud API request runs in the Shiny process.
  observe({
    s <- cloud_watch$status()
    if (s %in% c("initial", "running")) return()
    
    tryCatch({
      watched <- isolate(proc_state$cloud_watch_job)
      current <- isolate(proc_state$cloud_job_id)
      
      if (is.null(watched) || !identical(watched, current)) {
        .cloud_stop_clock()
        return()
      }
      
      if (identical(s, "error")) {
        .cloud_stop_clock()
        .cloud_mark_failed(paste0(
          "\U0001F6D1 The watcher process stopped unexpectedly.\n",
          "Reason: ",
          conditionMessage(cloud_watch$result()),
          "\nThe cloud job itself is unaffected and may still be running. ",
          "Check the Batch console, or use Download results once it finishes."
        ))
        return()
      }
      
      r <- cloud_watch$result()
      .cloud_stop_clock()
      outcome <- if (is.null(r$outcome)) "" else r$outcome
      
      if (identical(outcome, "done")) {
        proc_state$cloud_status <- "done"
        proc_state$cloud_done <- r$prog$done
        proc_state$cloud_total <- r$prog$total
        .clear_active_run()
        sys_status$cloud_summary <- .cloud_finish_summary(r$prog)
        .cloud_collect_result(current, r$prog)
      } else if (identical(outcome, "failed")) {
        .cloud_mark_failed(paste0(
          "\U0001F6D1 The cloud run failed.\n",
          if (!is.null(r$prog$error) &&
              length(r$prog$error) == 1L &&
              !is.na(r$prog$error)) {
            paste0("Reason: ", r$prog$error, "\n")
          } else {
            ""
          },
          "Results completed before the failure can still be downloaded."
        ))
      } else if (identical(outcome, "cancelled")) {
        .cloud_mark_cancelled(paste0(
          "The job was cancelled.\n",
          "Billing has stopped. Anything already finished can still be downloaded."
        ))
      } else if (identical(outcome, "batch_failed")) {
        .cloud_mark_failed(paste0(
          "\U0001F6D1 The Batch job failed.\n",
          "Check the Batch logs for image-pull, IAM, quota, or VM errors."
        ))
      } else if (identical(outcome, "no_report")) {
        .cloud_mark_failed(paste0(
          "\U0001F6D1 Batch reported success, but the container never wrote ",
          "progress.json.\nCheck Cloud Logging and the bucket permissions."
        ))
      } else {
        .clear_active_run()
        proc_state$cloud_status <- "failed"
        sys_status$cloud_summary <- paste0(
          "\u26a0\ufe0f This page stopped following the job.\n",
          if (is.null(r$reason)) {
            ""
          } else {
            paste0("Reason: ", r$reason, "\n")
          },
          "IMPORTANT: the cloud job may still be running and billing. ",
          "Check the Batch console. Results, if any, will still be at:\n",
          isolate(proc_state$cloud_result_uri)
        )
        showNotification(
          "Stopped following the cloud job. It may still be running.",
          type = "warning",
          duration = NULL
        )
      }
    }, error = function(e) {
      .cloud_stop_clock()
      .clear_active_run()
      proc_state$cloud_status <- "failed"
      sys_status$cloud_summary <- paste0(
        "\u26a0\ufe0f The cloud result arrived but could not be processed.\n",
        conditionMessage(e),
        "\nThe archive is still available with Download results."
      )
    })
  })
  
  # This is the sole remaining cloud timer. It reads only a tiny local RDS
  # written by the watcher process; it performs no network work.
  output$cloud_watch_panel <- renderUI({
    invalidateLater(60000)
    
    jid <- proc_state$cloud_job_id
    st <- proc_state$cloud_status
    if (is.null(jid) ||
        is.null(st) ||
        !st %in% c("submitted", "running")) {
      return(NULL)
    }
    
    info <- tryCatch({
      p <- .cloud_status_file(jid)
      if (file.exists(p)) readRDS(p) else NULL
    }, error = function(e) NULL)
    
    phase <- if (is.null(info$phase)) "waiting" else info$phase
    detail <- if (is.null(info$detail)) {
      "Waiting for the first report from the cloud machine."
    } else {
      info$detail
    }
    
    box_class <- if (identical(phase, "queued")) {
      "alert alert-warning"
    } else if (identical(phase, "unreachable")) {
      "alert alert-secondary"
    } else {
      "alert alert-info"
    }
    
    tags$div(
      class = box_class,
      style = "padding:8px; margin-bottom:10px;",
      icon(if (identical(phase, "running")) "spinner" else "cloud-arrow-up"),
      tags$b(" Cloud job in progress."),
      tags$br(),
      detail,
      tags$br(),
      tags$span(
        style = "font-size:11px;",
        "Elapsed: ",
        tags$span(id = "cloud_clock", "0 min 0 s")
      ),
      tags$br(),
      tags$span(
        style = "font-size:11px; color:#6c757d;",
        "You may close this page. The run continues; results will be at ",
        tags$code(proc_state$cloud_result_uri)
      )
    )
  })
  
  # ---- Controls shown during and after a cloud run ---------------------------
  
  .build_cloud_controls <- function(cancel_id = NULL) {
    
    task_label <- switch(
      proc_state$cloud_task_type,
      validation = "Model validation",
      perfcheck  = "Parallel performance check",
      full       = "Full simulation",
      "Cloud job"
    )
    
    status <- proc_state$cloud_status
    
    if (is.null(status)) {
      return(helpText("No cloud job has been submitted yet."))
    }
    
    done  <- proc_state$cloud_done
    total <- proc_state$cloud_total
    
    progress_line <- if (!is.na(done) && !is.na(total) && total > 0L) {
      paste0(done, " of ", total, " runs finished")
    } else "waiting for the first progress report"
    
    conn_warning <- if (proc_state$cloud_poll_fails >= 2L) {
      tags$div(class = "alert alert-warning", style = "padding:6px; margin-bottom:6px;",
               icon("wifi"), tags$b(" Connection lost, retrying."),
               tags$br(), "The cloud job is unaffected and keeps running.")
    } else NULL
    
    header <- switch(
      status,
      
      submitted = tags$div(
        class = "alert alert-info",
        style = "padding:6px;",
        icon("cloud-arrow-up"),
        tags$b(paste0(" ", task_label, " submitted.")),
        tags$br(),
        "Please wait while the cloud machine starts.",
        {
          waited <- if (is.null(proc_state$cloud_submitted_at)) 0 else
            as.numeric(difftime(Sys.time(), proc_state$cloud_submitted_at, units = "mins"))
          if (waited >= 1) {
            tagList(
              tags$br(),
              tags$span(style = "font-size:11px;",
                        paste0("Waiting for ", round(waited), " minute(s).")),
              if (waited > 10) {
                tags$div(
                  style = "margin-top:6px; font-size:11px;",
                  tags$b("This is longer than usual."),
                  " Google holds a job in the queue when the requested machine type",
                  " is unavailable in this region or a quota is exhausted.",
                  " Check the job in the Batch console, or cancel it and try a",
                  " different machine type or region."
                )
              }
            )
          }
        }
      ),
      
      running = tags$div(
        class = "alert alert-info",
        style = "padding:6px;",
        icon("spinner"),
        tags$b(paste0(" ", task_label, " is running on Google Cloud.")),
        tags$br(),
        progress_line
      ),
      
      done = tags$div(
        class = "alert alert-success",
        style = "padding:6px;",
        icon("check-circle"),
        tags$b(paste0(" ", task_label, " finished."))
      ),
      
      failed = tags$div(
        class = "alert alert-danger",
        style = "padding:6px;",
        icon("triangle-exclamation"),
        tags$b(paste0(" ", task_label, " stopped before finishing."))
      ),
      
      cancelled = tags$div(
        class = "alert alert-warning",
        style = "padding:6px;",
        icon("ban"),
        tags$b(paste0(" ", task_label, " was cancelled.")),
        tags$br(),
        "Billing has stopped."
      ),
      
      NULL
    )
    
    tagList(
      header,
      conn_warning,
      if (
        identical(proc_state$cloud_task_type, "full") &&
        !is.null(sys_status$cloud_summary)
      ) {
        tags$pre(
          style = "white-space:pre-wrap; font-size:11px;",
          sys_status$cloud_summary
        )
      },
      tags$div(
        style = "font-size:11px; color:#6c757d; margin-bottom:8px;",
        "You may close this page. The run continues, and the results will be at:",
        tags$br(), tags$code(proc_state$cloud_result_uri),
        tags$br(), "Closing the page ends progress reporting."
      ),
      if (
        status %in% c("submitted", "running") &&
        !is.null(cancel_id)
      ) {
        tagList(
          actionButton(cancel_id, "Cancel cloud job (stops billing)",
                       class = "btn-danger", width = "100%", icon = icon("ban")),
          if (isTRUE(proc_state$cloud_release_offer)) {
            tagList(
              br(),
              actionButton("cloud_release_tracking",
                           "Release tracking (does not stop the cloud job)",
                           class = "btn-outline-secondary btn-sm", width = "100%",
                           icon = icon("link-slash"))
            )
          }
        )
      },
      if (status %in% c("done", "failed", "cancelled")) {
        tagList(
          actionButton("cloud_download", "Download results",
                       class = "btn-success", width = "100%",
                       icon = icon("cloud-arrow-down")),
          if (status %in% c("failed", "cancelled")) {
            tagList(br(),
                    actionButton("cloud_download_partial", "Download completed runs only",
                                 class = "btn-warning", width = "100%",
                                 icon = icon("box-open")))
          },
          br(), br(),
          actionButton("cloud_dismiss", "Clear this result",
                       class = "btn-outline-secondary btn-sm", width = "100%",
                       icon = icon("xmark"))
        )
      }
    )
  }
  
  output$cloud_validation_controls <- renderUI({
    
    if (!identical(proc_state$cloud_task_type, "validation")) {
      return(NULL)
    }
    
    .build_cloud_controls(cancel_id = NULL)
  })
  
  
  output$cloud_perf_controls <- renderUI({
    
    if (!identical(proc_state$cloud_task_type, "perfcheck")) {
      return(NULL)
    }
    
    .build_cloud_controls(cancel_id = NULL)
  })
  
  
  output$cloud_run_controls <- renderUI({
    
    if (!identical(proc_state$cloud_task_type, "full")) {
      return(
        helpText(
          "No full cloud simulation has been submitted. ",
          "Model Validation and Parallel Performance Check results are shown in Step 3a."
        )
      )
    }
    
    .build_cloud_controls(cancel_id = "cloud_cancel_full")
  })
  
  
  observeEvent(input$cloud_dismiss, {
    .cloud_reset_display()
  })
  
  # The Test 1 and Test 2 stop buttons are always visible directly beneath
  # their Run buttons. They are enabled only while their own cloud job is
  # active, which avoids duplicate Shiny input IDs across the two tabs.
  observe({
    validation_active <- (
      identical(proc_state$cloud_task_type, "validation") &&
        !is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")
    )
    
    perf_active <- (
      identical(proc_state$cloud_task_type, "perfcheck") &&
        !is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")
    )
    
    if (validation_active) {
      shinyjs::enable("stop_test1_cloud")
    } else {
      shinyjs::disable("stop_test1_cloud")
    }
    
    if (perf_active) {
      shinyjs::enable("stop_test2_cloud")
    } else {
      shinyjs::disable("stop_test2_cloud")
    }
  })
  
  .show_cloud_cancel_modal <- function(expected_task = NULL) {
    active <- (
      !is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")
    )
    matching_task <- (
      is.null(expected_task) ||
        identical(proc_state$cloud_task_type, expected_task)
    )
    
    if (!isTRUE(active) || !isTRUE(matching_task)) {
      showNotification(
        "There is no matching cloud job to stop.",
        type = "warning"
      )
      return(invisible(FALSE))
    }
    
    showModal(modalDialog(
      title = "Cancel the cloud job?",
      "The machine will be released and billing will stop. Runs that have already ",
      "finished are kept and can still be downloaded, but the remainder will not ",
      "be completed and cannot be resumed.",
      footer = tagList(
        modalButton("Keep running"),
        actionButton("cloud_cancel_confirm", "Cancel the job", class = "btn-danger")
      )
    ))
    
    invisible(TRUE)
  }
  
  observeEvent(input$stop_test1_cloud, {
    .show_cloud_cancel_modal("validation")
  })
  
  observeEvent(input$stop_test2_cloud, {
    .show_cloud_cancel_modal("perfcheck")
  })
  
  observeEvent(input$cloud_cancel_full, {
    .show_cloud_cancel_modal("full")
  })
  
  observeEvent(input$cloud_cancel_confirm, {
    cs <- cloud_settings()
    jid <- proc_state$cloud_job_id
    
    if (is.null(jid)) {
      removeModal()
      showNotification(
        "No cloud job is currently being tracked.",
        type = "warning"
      )
      return()
    }
    
    showNotification(
      "Stopping the cloud job...",
      id = "cloud_cancel_progress",
      type = "message",
      duration = NULL
    )
    
    # Always clean up the confirmation modal, even if authentication or the
    # cancellation request produces an error.
    on.exit({
      removeNotification("cloud_cancel_progress")
      removeModal()
      
      # Bootstrap can occasionally leave its modal backdrop behind after a
      # synchronous API request. Remove that orphaned backdrop after Shiny has
      # processed removeModal().
      shinyjs::runjs("
      setTimeout(function() {
        document.querySelectorAll('.modal-backdrop').forEach(function(x) {
          x.remove();
        });
        document.body.classList.remove('modal-open');
        document.body.style.removeProperty('padding-right');
      }, 200);
    ")
    }, add = TRUE)
    
    res <- tryCatch({
      auth <- cloud_token()
      cloud_cancel_job(
        auth,
        cs$project,
        cs$region,
        jid
      )
      TRUE
    }, error = function(e) {
      conditionMessage(e)
    })
    
    if (isTRUE(res)) {
      
      done  <- proc_state$cloud_done
      total <- proc_state$cloud_total
      
      cancel_summary <- paste0(
        "Job cancelled. Billing has stopped.\n",
        if (
          length(done) == 1L &&
          length(total) == 1L &&
          !is.na(done) &&
          !is.na(total)
        ) {
          paste0(
            "Completed before cancelling: ",
            done,
            " of ",
            total,
            " runs.\n"
          )
        } else {
          ""
        },
        "Use 'Download completed runs only' to collect any completed output."
      )
      
      # This shared helper also:
      # 1. removes the old persistent 'submitted' notification;
      # 2. clears the global run lock;
      # 3. resets the Test 1/Test 2 completion state correctly;
      # 4. records the cancellation summary.
      proc_state$cloud_watch_job <- NULL
      .cloud_stop_clock()
      .cloud_mark_cancelled(
        message = cancel_summary,
        notify = FALSE
      )
      
      showNotification(
        "Cloud job cancelled. Billing has stopped.",
        type = "message",
        duration = 10
      )
      
    } else {
      
      showNotification(
        paste(
          "Could not cancel the job:",
          res
        ),
        type = "error",
        duration = 15
      )
      
      sys_status$cloud_summary <- paste0(
        "\U0001F6D1 The job could not be stopped from this page.\n",
        "Reason: ",
        res,
        "\n\n",
        "The cloud job may still be running and billing. Stop it in the Google ",
        "Cloud console, then use the release-tracking control in this app.\n",
        "Releasing tracking only unlocks this page; it does not stop the cloud job."
      )
      
      proc_state$cloud_release_offer <- TRUE
    }
  })
  
  observeEvent(input$cloud_download, {
    cs <- cloud_settings()
    jid <- proc_state$cloud_job_id
    req(!is.null(jid))
    
    dest <- if (!is.null(input$out_dir) && nzchar(input$out_dir)) {
      file.path(input$out_dir, jid)
    } else file.path(tempdir(), jid)
    
    showNotification("Downloading results...", id = "cloud_dl", duration = NULL)
    res <- tryCatch({
      auth <- cloud_token()
      cloud_download_results(auth, cs$bucket, jid, dest)
    }, error = function(e) list(pass = FALSE, msg = conditionMessage(e)))
    removeNotification("cloud_dl")
    
    sys_status$cloud_summary <- res$msg
    showNotification(res$msg, type = if (isTRUE(res$pass)) "message" else "error",
                     duration = 10)
    if (isTRUE(res$pass)) updateTextInput(session, "res_out_dir", value = dest)
  })
  
  observeEvent(input$cloud_download_partial, {
    cs <- cloud_settings()
    jid <- proc_state$cloud_job_id
    req(!is.null(jid))
    
    dest <- if (!is.null(input$out_dir) && nzchar(input$out_dir)) {
      file.path(input$out_dir, paste0(jid, "_partial"))
    } else file.path(tempdir(), paste0(jid, "_partial"))
    
    showNotification("Collecting completed runs...", id = "cloud_dlp", duration = NULL)
    res <- tryCatch({
      auth <- cloud_token()
      cloud_download_partial(auth, cs$bucket, jid, dest)
    }, error = function(e) list(pass = FALSE, msg = conditionMessage(e)))
    removeNotification("cloud_dlp")
    
    sys_status$cloud_summary <- res$msg
    showNotification(res$msg, type = if (isTRUE(res$pass)) "message" else "warning",
                     duration = 10)
    if (isTRUE(res$pass)) updateTextInput(session, "res_out_dir", value = dest)
  })
  
  shinyFiles::shinyDirChoose(input, "browse_dir_run", roots = roots, session = session)
  
  observeEvent(input$browse_dir_run, {
    req(input$browse_dir_run)
    selected_path <- shinyFiles::parseDirPath(roots, input$browse_dir_run)
    if (length(selected_path) > 0 && nzchar(selected_path)) {
      updateTextInput(session, "out_dir", value = selected_path)
    }
  })
  
  
  batch_plan <- reactive({
    req(input$n_iter, input$n_cores)
    
    cloud_mode <- isTRUE(input$use_cloud)
    if (!cloud_mode) {
      req(input$out_dir)
    }
    
    missing_steps <- get_missing_setup_steps()
    
    origin_line <- if (!is.null(sys_status$loaded_from)) {
      paste0("📂 ", sys_status$loaded_from, "\n\n")
    } else ""
    
    if (length(missing_steps) > 0L) {
      return(list(
        valid = FALSE,
        msg = paste0(
          origin_line,
          "🚧 Setup is incomplete.\nMissing:\n - ",
          paste(missing_steps, collapse = "\n - ")
        )
      ))
    }
    
    if (isTRUE(sys_status$memory_retest_required) && !isTRUE(sys_status$memory_check_done)) {
      return(list(
        valid = FALSE,
        msg = paste0(
          "🛑 The run-control plan changed after a memory check.\n",
          "Confirm Run Control, then run the Parallel performance check again ",
          "before starting the full simulation."
        )
      ))
    }
    
    if (identical(sys_status$mem_safe, FALSE)) {
      return(list(
        valid = FALSE,
        msg = paste0(
          "🛑 The most recent parallel performance check produced a red result.\n",
          "Lower Parallel cores (Step 2) or reduce another parallel layer, ",
          "confirm Run Control, and run the check again."
        )
      ))
    }
    
    scenarios_df <- try(get_scenarios_df(), silent = TRUE)
    if (inherits(scenarios_df, "try-error") || is.null(scenarios_df)) {
      return(list(valid = FALSE, msg = "Waiting for scenarios..."))
    }
    
    num_scenarios <- nrow(scenarios_df)
    n_iter_val <- suppressWarnings(as.integer(input$n_iter))
    n_cores_set <- suppressWarnings(as.integer(input$n_cores))
    bucket_preview <- if (
      !is.null(input$gcp_bucket) &&
      length(input$gcp_bucket) == 1L &&
      nzchar(input$gcp_bucket)
    ) {
      input$gcp_bucket
    } else {
      "<bucket>"
    }
    
    out_dir_path <- if (cloud_mode) {
      paste0(
        "gs://",
        bucket_preview,
        "/jobs/<job-id>/"
      )
    } else {
      normalizePath(input$out_dir, mustWork = FALSE)
    }
    
    if (length(n_iter_val) == 0L || is.na(n_iter_val) || n_iter_val < 1L) n_iter_val <- 1L
    if (length(n_cores_set) == 0L || is.na(n_cores_set) || n_cores_set < 1L) n_cores_set <- 1L
    
    total_tasks_count <- num_scenarios * n_iter_val
    atomic_tasks <- vector("list", total_tasks_count)
    idx <- 1L
    for (s in seq_len(num_scenarios)) {
      for (it in seq_len(n_iter_val)) {
        atomic_tasks[[idx]] <- list(sidx = s, iter_i = it)
        idx <- idx + 1L
      }
    }
    
    configured_workers <- min(n_cores_set, total_tasks_count)
    effective_workers <- configured_workers
    
    # Split all tasks directly across worker processes (no batching).
    actual_cores <- min(effective_workers, total_tasks_count)
    worker_assignment <- parallel::splitIndices(total_tasks_count, actual_cores)
    worker_packets <- vector("list", actual_cores)
    worker_loads <- integer(actual_cores)
    for (worker_id in seq_len(actual_cores)) {
      local_idx <- worker_assignment[[worker_id]]
      worker_packets[[worker_id]] <- atomic_tasks[local_idx]
      worker_loads[[worker_id]] <- length(local_idx)
    }
    
    # ---- Parallel methods summary (all three layers) ----
    user_use_gpu <- isTRUE(input$use_gpu)
    user_gpu_n <- if (!is.null(input$gpu_thread_count)) as.integer(input$gpu_thread_count) else 0L
    policy_threads_active <- if (user_use_gpu && user_gpu_n > 0L) user_gpu_n else 1L
    
    use_large_pop = isTRUE(input$simulation_engine)
    omp_threads_active <- if (use_large_pop) max(1L, as.integer(input$omp_nthreads)) else 1L
    
    # Fast-forward status
    ff_mode <- if (is.null(input$fast_forward_mode)) "auto" else input$fast_forward_mode
    ff_active <- !identical(ff_mode, "off")
    ff_months <- if (!is.null(vals$T_safe_info) && !is.null(vals$T_safe_info$T_safe)) {
      as.integer(vals$T_safe_info$T_safe)
    } else NA_integer_
    ff_line <- if (!ff_active) {
      "🐟 Juvenile fast-forward: OFF\n"
    } else {
      paste0("🐟 Juvenile fast-forward: ON (", 
             if (is.na(ff_months)) "auto" else paste0(ff_months, " month(s)"), ")\n")
    }
    
    # Method lines: describe each layer that is actually engaged.
    method_lines <- paste0(
      "⚙️  PARALLEL METHODS IN USE\n",
      "   1. Replicate parallelism: ", effective_workers,
      " active worker process(es)\n",
      "   2. Policy parallelism: ",
      if (policy_threads_active > 1L)
        paste0("ON, ", policy_threads_active, " thread(s) per replicate")
      else "OFF (policies run sequentially)",
      "\n",
      "   3. Individual parallelism: ",
      if (omp_threads_active > 1L)
        paste0("ON, ", omp_threads_active, " thread(s) per model (large-population method)")
      else "OFF (standard method)",
      "\n"
    )
    
    # Total concurrent threads at peak = active workers x policy x individual.
    peak_threads <- effective_workers * policy_threads_active * omp_threads_active
    total_line <- paste0(
      "🧮 Peak concurrent threads: ", effective_workers, " x ",
      policy_threads_active, " x ", omp_threads_active, " = ", peak_threads, "\n"
    )
    
    # ---- WORKER LOAD PLAN (per-worker job counts) ----
    worker_plan_lines <- vapply(
      seq_len(actual_cores),
      function(wid) sprintf("   Worker %02d: %d run(s)", wid, worker_loads[[wid]]),
      character(1)
    )
    worker_plan_block <- paste0(
      "👷 WORKER LOAD PLAN\n", paste(worker_plan_lines, collapse = "\n")
    )
    
    msg <- paste0(
      if (!is.null(sys_status$loaded_from)) paste0("📂 ", sys_status$loaded_from, "\n") else "",
      "🚀 PRE-RUN DIAGNOSTICS (Live)\n",
      "========================================\n",
      "📂 Target Path: ", out_dir_path, "\n",
      "🔢 Scenarios: ", num_scenarios, "\n",
      "🔄 Iterations: ", n_iter_val, " per scenario\n",
      "📦 Total jobs: ", total_tasks_count,
      " (", num_scenarios, " scenarios x ", n_iter_val, " iterations)\n",
      ff_line,
      "========================================\n",
      method_lines,
      total_line,
      "========================================\n",
      worker_plan_block,
      "\n========================================\n",
      "✅ Ready to launch."
    )
    
    list(
      valid = TRUE,
      msg = msg,
      worker_packets = worker_packets,
      configured_workers = configured_workers,
      actual_cores = actual_cores,
      out_dir_path = out_dir_path,
      num_scenarios = num_scenarios,
      n_iter_val = n_iter_val,
      total_tasks_count = total_tasks_count
    )
  })
  
  
  
  
  
  output$task_preview <- renderText({
    plan <- batch_plan()
    return(unname(plan$msg))
  })
  
  output$batch_log <- renderText({ unname(sys_status$batch_log) })
  
  # ==========================================================================
  # Helper: enable/disable Start & Stop buttons based on run state
  # ==========================================================================
  # ==========================================================================
  # GLOBAL RUN LOCK
  #
  # Only one run may be active at a time, whether it is a test on this machine,
  # a test in the cloud, or the full simulation. While anything is running the
  # three start buttons are disabled together, so a second run cannot be
  # started by accident from another tab.
  # ==========================================================================
  
  .set_active_run <- function(kind, mode = "local") {
    proc_state$active_run      <- kind          # validation / perfcheck / full
    proc_state$active_run_mode <- mode          # local / cloud
  }
  
  .clear_active_run <- function() {
    proc_state$active_run      <- NULL
    proc_state$active_run_mode <- NULL
  }
  
  # Keeps every start button in step with the lock.
  observe({
    locked <- !is.null(proc_state$active_run)
    
    for (btn in c("run_test_sim", "run_oversub_test", "start_batch")) {
      if (locked) shinyjs::disable(btn) else shinyjs::enable(btn)
    }
  })
  
  # Explains, on each test tab, why the buttons are unavailable and which
  # control will release them.
  .build_run_lock_note <- function(this_tab) {
    kind <- proc_state$active_run
    if (is.null(kind)) return(NULL)
    
    mode <- proc_state$active_run_mode
    label <- switch(kind,
                    validation = "Test 1 (model validation)",
                    perfcheck  = "Test 2 (parallel performance check)",
                    full       = "the full simulation",
                    "a run"
    )
    
    mine <- identical(kind, this_tab)
    
    tags$div(
      class = "alert alert-secondary",
      style = "padding:8px; margin-bottom:10px;",
      icon("lock"),
      tags$b(paste0(" Start buttons are locked while ", label, " is running.")),
      tags$br(),
      if (identical(mode, "cloud")) {
        if (mine) {
          "Use the Stop button below to cancel it."
        } else {
          "Go to its own tab to stop it, or wait for it to finish."
        }
      } else {
        paste0(
          "This run is on your own computer and cannot be interrupted once it ",
          "has started. The buttons will unlock when it finishes."
        )
      }
    )
  }
  
  output$run_lock_note_test1 <- renderUI({ .build_run_lock_note("validation") })
  output$run_lock_note_test2 <- renderUI({ .build_run_lock_note("perfcheck") })
  output$run_lock_note_full  <- renderUI({ .build_run_lock_note("full") })
  
  sync_batch_buttons <- function(is_running, mode = NULL) {
    if (is.null(mode)) mode <- isolate(input$run_mode)
    if (is.null(mode)) mode <- "foreground"
    
    if (is_running) {
      # Disable Start
      shinyjs::disable("start_batch")
      # Stop: only enabled in background mode
      if (mode == "background") {
        shinyjs::enable("stop_batch")
      } else {
        shinyjs::disable("stop_batch")
      }
    } else {
      # Only release Start if nothing else is holding the global lock.
      if (is.null(proc_state$active_run)) shinyjs::enable("start_batch")
      shinyjs::disable("stop_batch")
    }
  }
  
  # Disable Stop on app start (nothing running yet)
  observe({
    shinyjs::disable("stop_batch")
  })
  
  # When run_mode changes, update Stop button state accordingly
  observeEvent(input$run_mode, {
    if (!proc_state$is_running) {
      shinyjs::disable("stop_batch")
    } else {
      if (input$run_mode == "background") {
        shinyjs::enable("stop_batch")
      } else {
        shinyjs::disable("stop_batch")
      }
    }
  })
  
  # ==========================================================================
  # START button: dispatch to foreground or background based on run_mode
  # ==========================================================================
  observeEvent(input$start_batch, {
    plan <- batch_plan()
    
    if (!plan$valid) {
      sys_status$batch_log <- paste0(
        "🛑 Start aborted.\n",
        plan$msg
      )
      showNotification(
        "Cannot start: setup or memory check is incomplete.",
        type = "error"
      )
      return()
    }
    
    cloud_mode <- isTRUE(input$use_cloud)
    out_dir_base <- if (cloud_mode) "" else plan$out_dir_path
    
    # A Google Cloud run writes to its own job-specific Storage prefix. Local
    # folder existence, contents and Overwrite therefore must not block it.
    if (!cloud_mode) {
      if (out_dir_base == "") return()
      
      if (!dir.exists(out_dir_base)) {
        if (!dir.create(out_dir_base, recursive = TRUE, showWarnings = FALSE)) {
          sys_status$batch_log <- paste0(
            "❌ Error: Could not create directory:\n",
            out_dir_base
          )
          return()
        }
      } else {
        if (!input$overwrite_existing && length(list.files(out_dir_base)) > 0L) {
          sys_status$batch_log <- paste0(
            "⚠️ Warning: Directory exists and is not empty.\n",
            "Check 'Overwrite' to proceed."
          )
          return()
        }
        
        if (input$overwrite_existing && length(list.files(out_dir_base)) > 0L) {
          unlink(out_dir_base, recursive = TRUE)
          dir.create(out_dir_base, recursive = TRUE, showWarnings = FALSE)
        }
      }
    }
    
    sys_status$batch_log <- "⏳ Capturing data snapshots..."
    
    snap_all_params <- get_packed_params()
    snap_scenarios_df <- get_scenarios_df()
    snap_policy_logic <- get_policy_combos_logic()
    snap_comp_struct <- get_compliance_struct()
    snap_rm_vec <- parse_num_vec(input$rm_vec)
    if (length(snap_rm_vec) == 0L) snap_rm_vec <- 0
    snap_burnin_rm <- max(snap_rm_vec, na.rm = TRUE)
    
    # Local runs save their settings beside their result files. Cloud runs
    # already include settings_rds in the uploaded payload, so they must not
    # touch the local output folder at all.
    auto_settings_log_line <- ""
    
    if (!cloud_mode) {
      auto_settings_name <- paste0(
        "work data saved on ",
        format(
          Sys.time(),
          "%Y%m%d_%H%M%S"
        ),
        ".rds"
      )
      
      auto_settings_path <- file.path(
        out_dir_base,
        auto_settings_name
      )
      
      auto_save_result <- tryCatch(
        {
          saveRDS(
            collect_settings(),
            auto_settings_path
          )
          
          list(
            ok = TRUE,
            error = NULL
          )
        },
        error = function(e) {
          list(
            ok = FALSE,
            error = conditionMessage(e)
          )
        }
      )
      
      auto_settings_log_line <- if (isTRUE(auto_save_result$ok)) {
        paste0(
          "💾 Settings snapshot saved: ",
          auto_settings_name,
          "\n"
        )
      } else {
        paste0(
          "⚠️ Settings snapshot could not be saved: ",
          auto_save_result$error,
          "\n"
        )
      }
    }
    
    # A cloud run is handed over here. Nothing is computed on this machine, so
    # the foreground and background distinction does not apply and no child
    # process is started: the application only uploads, submits and then
    # watches from a distance.
    if (cloud_mode) {
      sys_status$batch_log <- paste0(
        "\u2601\ufe0f Uploading the full simulation to Google Cloud.\n",
        "Please wait while the input package is prepared and submitted."
      )
      
      sub <- cloud_submit(
        "full",
        payload = list(
          worker_packets    = plan$worker_packets,
          total_tasks_count = plan$total_tasks_count,
          actual_cores      = plan$actual_cores,
          all_params        = snap_all_params,
          scenarios_df      = snap_scenarios_df,
          policy_logic      = snap_policy_logic,
          burnin_rm         = snap_burnin_rm,
          settings_rds      = collect_settings()
        ),
        label = "Full simulation"
      )
      
      if (!isTRUE(sub$ok)) {
        sys_status$batch_log <- paste0(
          "\U0001F6D1 The cloud job could not be started.\n",
          if (!is.null(sub$msg)) sub$msg else "")
        return()
      }
      
      .cloud_start_watch(sub$job_id)
      
      sys_status$batch_log <- paste0(
        "\u2601\ufe0f SIMULATION SUBMITTED TO GOOGLE CLOUD\n",
        "========================================\n",
        "Job: ", sub$job_id, "\n",
        "Machine: ", input$gcp_machine_type, "\n",
        "Total runs: ", plan$total_tasks_count, "\n",
        "Workers: ", plan$actual_cores, "\n",
        "========================================\n",
        "Results will be written to:\n", proc_state$cloud_result_uri, "\n\n",
        "You may close this page: the run continues without it, and the results\n",
        "can be collected from the address above. Closing the page ends progress\n",
        "reporting, so leave it open if you want to follow along.\n\n",
        "Billing runs until the job finishes or is cancelled."
      )
      return()
    }
    
    run_mode <- input$run_mode
    if (is.null(run_mode)) run_mode <- "foreground"
    
    user_policy_threads <- if (
      isTRUE(input$use_gpu) &&
      !is.null(input$gpu_thread_count)
    ) {
      max(1L, as.integer(input$gpu_thread_count))
    } else {
      1L
    }
    
    engine_label_full <- if (
      identical(snap_all_params$execution$engine, "v2")
    ) {
      "Large-population optimized"
    } else {
      "Standard"
    }
    
    omp_label <- if (
      identical(snap_all_params$execution$engine, "v2")
    ) {
      snap_all_params$execution$omp_nthreads
    } else {
      1L
    }
    
    compute_mode_label <- paste0(
      engine_label_full,
      " | Individual threads=", omp_label,
      " | Policy threads=", user_policy_threads,
      " | Active replicate workers=", plan$actual_cores,
      " | Fast-forward=", snap_all_params$other$T_safe
    )
    
    selected_worker_func <- run_whole_scenario_job_shiny
    
    proc_state$is_running <- TRUE
    .set_active_run("full", if (isTRUE(input$use_cloud)) "cloud" else "local")
    sync_batch_buttons(TRUE, run_mode)
    
    if (run_mode == "background") {
      sys_status$batch_log <- paste0(
        "🚀 SIMULATION IS RUNNING IN THE BACKGROUND\n",
        "========================================\n",
        "💻 Active workers: ", plan$actual_cores, "\n",
        "⚙️ Execution: ", compute_mode_label, "\n",
        "📂 Saving results to: ", out_dir_base, "\n",
        auto_settings_log_line,
        "========================================\n",
        "✅ The UI remains responsive. Use Stop to cancel."
      )
      
      payload <- list(
        worker_packets = plan$worker_packets,
        total_tasks_count = plan$total_tasks_count,
        actual_cores = plan$actual_cores,
        scenarios_df = snap_scenarios_df,
        policy_logic = snap_policy_logic,
        all_params = snap_all_params,
        comp_struct = snap_comp_struct,
        burnin_rm = snap_burnin_rm,
        out_dir = out_dir_base,
        worker_func = selected_worker_func
      )
      
      err_log_path <- file.path(out_dir_base, "sim_info.log")
      trash_log_path <- file.path(out_dir_base, ".pkg_load_trash.log")
      
      proc_state$job <- callr::r_bg(
        func = function(data, real_log_path, trash_path) {
          cl <- NULL
          real_con <- NULL
          
          on.exit({
            if (!is.null(cl)) {
              try(parallel::stopCluster(cl), silent = TRUE)
            }
            try(sink(type = "message"), silent = TRUE)
            if (!is.null(real_con) && isOpen(real_con)) {
              try(close(real_con), silent = TRUE)
            }
          }, add = TRUE)
          
          invisible(loadNamespace("craibm"))
          
          real_con <- file(real_log_path, open = "wt")
          sink(real_con, type = "message")
          
          for (i in seq_len(nrow(data$scenarios_df))) {
            scen_row <- data$scenarios_df[i, ]
            clean_name <- as.character(scen_row$run_label)
            s_dir <- file.path(data$out_dir, clean_name)
            
            if (!dir.exists(s_dir)) {
              dir.create(s_dir, recursive = TRUE)
            }
            
            pol_df <- data$policy_logic |>
              dplyr::rowwise() |>
              dplyr::mutate(
                release_mortality = if (use_scenario_rm) {
                  as.numeric(scen_row$release_mortality)
                } else {
                  0.0
                }
              ) |>
              dplyr::select(
                policy_combo_id,
                comp_mode,
                release_mortality
              ) |>
              dplyr::ungroup() |>
              as.data.frame()
            
            data.table::fwrite(scen_row, file.path(s_dir, "scenario_info.csv"))
            data.table::fwrite(pol_df, file.path(s_dir, "policy_combos_info.csv"))
          }
          
          snap_scenarios_df <- data$scenarios_df
          snap_policy_logic <- data$policy_logic
          snap_burnin_rm <- data$burnin_rm
          out_dir_base <- data$out_dir
          worker_func <- data$worker_func
          
          snap_all_params_packed <- list(
            params = data$all_params,
            data_pack = list(
              zr_vec = data$all_params$z_vec,
              W1_mat = data$all_params$alk_mat,
              Theta_mat = data$all_params$agedata_mat
            ),
            compliance_structure = data$comp_struct
          )
          
          message(sprintf(
            "[RUN] Starting %d job(s) with %d worker(s).",
            data$total_tasks_count,
            data$actual_cores
          ))
          
          cl <- parallel::makeCluster(data$actual_cores)
          
          check_results <- parallel::clusterEvalQ(cl, {
            res <- list(pid = Sys.getpid(), ok = FALSE, msg = "")
            tryCatch({
              
              invisible(loadNamespace("craibm"))
              
              res$ok <- TRUE
              res$msg <- "Ready"
              
            }, error = function(e) {
              
              res$msg <- conditionMessage(e)
            })
            res
          })
          
          bad_workers <- Filter(function(x) !isTRUE(x$ok), check_results)
          if (length(bad_workers) > 0L) {
            err_txt <- paste(
              vapply(
                bad_workers,
                function(x) paste0("[pid ", x$pid, "] ", x$msg),
                character(1)
              ),
              collapse = "\n"
            )
            stop(paste0("Worker package loading failed:\n", err_txt))
          }
          
          parallel::clusterExport(
            cl,
            varlist = c(
              "snap_scenarios_df",
              "snap_policy_logic",
              "snap_all_params_packed",
              "snap_burnin_rm",
              "out_dir_base",
              "worker_func"
            ),
            envir = environment()
          )
          
          packet_results <- parallel::parLapply(
            cl,
            data$worker_packets,
            function(packet) {
              
              lapply(packet, function(task) {
                
                task_info <- list(
                  sidx = task$sidx,
                  iter_i = task$iter_i,
                  burnin_rm_val = snap_burnin_rm
                )
                
                worker_args <- list(
                  task_info = task_info,
                  scenarios_df = snap_scenarios_df,
                  policy_combos_logic = snap_policy_logic,
                  all_params = snap_all_params_packed,
                  out_dir_base = out_dir_base,
                  cpp_abs_path = NULL
                )
                
                tryCatch({
                  
                  do.call(worker_func, worker_args)
                  
                  list(
                    ok = TRUE,
                    sidx = task$sidx,
                    iter_i = task$iter_i,
                    error = ""
                  )
                  
                }, error = function(e) {
                  
                  err_msg <- conditionMessage(e)
                  
                  message(sprintf(
                    "[ERROR] scenario=%d iter=%d : %s",
                    task$sidx,
                    task$iter_i,
                    err_msg
                  ))
                  
                  list(
                    ok = FALSE,
                    sidx = task$sidx,
                    iter_i = task$iter_i,
                    error = err_msg
                  )
                })
              })
            }
          )
          
          # Flatten the list returned by all worker packets.
          job_results <- do.call(c, packet_results)
          
          total_jobs <- length(job_results)
          
          successful_jobs <- sum(
            vapply(
              job_results,
              function(x) isTRUE(x$ok),
              logical(1)
            )
          )
          
          failed_jobs <- total_jobs - successful_jobs
          
          failed_details <- Filter(
            function(x) !isTRUE(x$ok),
            job_results
          )
          
          parallel::stopCluster(cl)
          cl <- NULL
          gc(full = TRUE)
          
          message(sprintf(
            "[RUN] Completed. Total=%d Successful=%d Failed=%d",
            total_jobs,
            successful_jobs,
            failed_jobs
          ))
          
          sink(type = "message")
          close(real_con)
          real_con <- NULL
          
          return(list(
            status = if (failed_jobs == 0L) {
              "success"
            } else if (successful_jobs == 0L) {
              "failed"
            } else {
              "partial"
            },
            
            total_jobs = total_jobs,
            successful_jobs = successful_jobs,
            failed_jobs = failed_jobs,
            failed_details = failed_details
          ))
        },
        args = list(
          data = payload,
          real_log_path = err_log_path,
          trash_path = trash_log_path
        ),
        stderr = trash_log_path,
        stdout = "|"
      )
      
      proc_state$bg_out_dir <- out_dir_base
      proc_state$bg_cores <- plan$actual_cores
      proc_state$bg_settings_log_line <- auto_settings_log_line
      
      showNotification(
        "Simulation started in the background.",
        type = "message"
      )
      
    } else {
      sys_status$batch_log <- paste0(
        "🚀 SIMULATION IS RUNNING\n",
        "========================================\n",
        "💻 Active workers: ", plan$actual_cores, "\n",
        "⚙️ Execution: ", compute_mode_label, "\n",
        "📂 Saving results to: ", out_dir_base, "\n",
        auto_settings_log_line,
        "========================================\n",
        "☕ PLEASE BE PATIENT.\n",
        "The screen will freeze temporarily in foreground mode.\n",
        "Please do not close the window."
      )
      
      run_notif_id <- showNotification(
        "Starting the simulation. The screen may freeze.",
        type = "warning",
        duration = NULL
      )
      
      session$onFlushed(function() {
        Sys.sleep(0.5)
        
        cl <- NULL
        err_log_path <- file.path(out_dir_base, "sim_info.log")
        err_con <- NULL
        
        on.exit({
          try(sink(type = "message"), silent = TRUE)
          if (!is.null(err_con) && isOpen(err_con)) {
            try(close(err_con), silent = TRUE)
          }
          if (!is.null(cl)) {
            try(parallel::stopCluster(cl), silent = TRUE)
          }
          removeNotification(run_notif_id)
          proc_state$is_running <- FALSE
          .clear_active_run()
          .clear_active_run()
          sync_batch_buttons(FALSE, "foreground")
        })
        
        tryCatch({
          invisible(loadNamespace("craibm"))
          
          for (i in seq_len(plan$num_scenarios)) {
            scen_row <- snap_scenarios_df[i, ]
            clean_name <- as.character(scen_row$run_label)
            scenario_dir <- file.path(out_dir_base, clean_name)
            
            if (!dir.exists(scenario_dir)) {
              dir.create(scenario_dir, recursive = TRUE)
            }
            
            current_policy_df <- snap_policy_logic %>%
              dplyr::rowwise() %>%
              dplyr::mutate(
                release_mortality = if (use_scenario_rm) {
                  as.numeric(scen_row$release_mortality)
                } else {
                  0.0
                }
              ) %>%
              dplyr::select(policy_combo_id, comp_mode, release_mortality) %>%
              dplyr::ungroup() %>%
              as.data.frame()
            
            data.table::fwrite(scen_row, file.path(scenario_dir, "scenario_info.csv"))
            data.table::fwrite(current_policy_df, file.path(scenario_dir, "policy_combos_info.csv"))
          }
          
          err_con <- file(err_log_path, open = "wt")
          sink(err_con, type = "message")
          
          snap_all_params_packed <- list(
            params = snap_all_params,
            data_pack = list(
              zr_vec = snap_all_params$z_vec,
              W1_mat = snap_all_params$alk_mat,
              Theta_mat = snap_all_params$agedata_mat
            ),
            compliance_structure = snap_comp_struct
          )
          
          fg_worker_func <- selected_worker_func
          
          message(sprintf(
            "[RUN] Starting %d job(s) with %d worker(s).",
            plan$total_tasks_count,
            plan$actual_cores
          ))
          
          cl <- parallel::makeCluster(plan$actual_cores)
          
          check_results <- parallel::clusterEvalQ(cl, {
            res <- list(pid = Sys.getpid(), ok = FALSE, msg = "")
            tryCatch({
              
              invisible(loadNamespace("craibm"))
              
              res$ok <- TRUE
              res$msg <- "Ready"
              
            }, error = function(e) {
              
              res$msg <- conditionMessage(e)
            })
            res
          })
          
          if (!all(vapply(check_results, function(x) x$ok, logical(1)))) {
            stop("Worker package check failed.")
          }
          
          parallel::clusterExport(
            cl,
            varlist = c(
              "snap_scenarios_df",
              "snap_policy_logic",
              "snap_all_params_packed",
              "snap_burnin_rm",
              "out_dir_base",
              "fg_worker_func"
            ),
            envir = environment()
          )
          
          packet_results <- parallel::parLapply(
            cl,
            plan$worker_packets,
            function(packet) {
              
              lapply(packet, function(task) {
                
                task_info <- list(
                  sidx = task$sidx,
                  iter_i = task$iter_i,
                  burnin_rm_val = snap_burnin_rm
                )
                
                worker_args <- list(
                  task_info = task_info,
                  scenarios_df = snap_scenarios_df,
                  policy_combos_logic = snap_policy_logic,
                  all_params = snap_all_params_packed,
                  out_dir_base = out_dir_base,
                  cpp_abs_path = NULL
                )
                
                tryCatch({
                  
                  do.call(fg_worker_func, worker_args)
                  
                  list(
                    ok = TRUE,
                    sidx = task$sidx,
                    iter_i = task$iter_i,
                    error = ""
                  )
                  
                }, error = function(e) {
                  
                  err_msg <- conditionMessage(e)
                  
                  message(sprintf(
                    "[ERROR] scenario=%d iter=%d : %s",
                    task$sidx,
                    task$iter_i,
                    err_msg
                  ))
                  
                  list(
                    ok = FALSE,
                    sidx = task$sidx,
                    iter_i = task$iter_i,
                    error = err_msg
                  )
                })
              })
            }
          )
          
          job_results <- do.call(c, packet_results)
          
          total_jobs <- length(job_results)
          
          successful_jobs <- sum(
            vapply(
              job_results,
              function(x) isTRUE(x$ok),
              logical(1)
            )
          )
          
          failed_jobs <- total_jobs - successful_jobs
          
          parallel::stopCluster(cl)
          cl <- NULL
          gc(full = TRUE)
          
          message("[RUN] Completed.")
          
          sink(type = "message")
          close(err_con)
          err_con <- NULL
          
          if (failed_jobs == 0L) {
            
            sys_status$batch_log <- paste0(
              "✅ ALL SIMULATION JOBS COMPLETED SUCCESSFULLY.\n",
              "========================================\n",
              "Total jobs: ", total_jobs, "\n",
              "Successful: ", successful_jobs, "\n",
              "Failed: ", failed_jobs, "\n",
              "========================================\n",
              "📂 Data saved to: ", out_dir_base, "\n",
              "Please go to Step 4 to load and view results."
            )
            
            showNotification(
              paste0(
                "All ", successful_jobs,
                " simulation jobs completed successfully."
              ),
              type = "message",
              duration = NULL
            )
            
          } else if (successful_jobs > 0L) {
            
            sys_status$batch_log <- paste0(
              "⚠️ SOME OF THE SIMULATION JOB(S) FAILED.\n",
              "========================================\n",
              "Total jobs: ", total_jobs, "\n",
              "Successful: ", successful_jobs, "\n",
              "Failed: ", failed_jobs, "\n",
              "========================================\n",
              "Successful outputs have been saved.\n",
              "📂 Output folder: ", out_dir_base, "\n",
              "Check sim_info.log for the failed jobs."
            )
            
            showNotification(
              paste0(
                "Some of the simulation job(s) failed. ",
                successful_jobs, " succeeded; ",
                failed_jobs, " failed."
              ),
              type = "warning",
              duration = NULL
            )
            
          } else {
            
            sys_status$batch_log <- paste0(
              "❌ ALL SIMULATION JOBS FAILED.\n",
              "========================================\n",
              "Total jobs: ", total_jobs, "\n",
              "Successful: ", successful_jobs, "\n",
              "Failed: ", failed_jobs, "\n",
              "========================================\n",
              "📂 Output folder: ", out_dir_base, "\n",
              "Check sim_info.log for details."
            )
            
            showNotification(
              paste0(
                "All ", failed_jobs,
                " simulation jobs failed."
              ),
              type = "error",
              duration = NULL
            )
          }
          
        }, error = function(e) {
          try(sink(type = "message"), silent = TRUE)
          if (!is.null(err_con) && isOpen(err_con)) {
            try(close(err_con), silent = TRUE)
          }
          err_con <<- NULL
          
          sys_status$batch_log <- paste0(
            "❌ Simulation ended with an error.\n",
            "📂 Output folder: ", out_dir_base, "\n",
            "Error: ", e$message, "\n",
            "Check sim_info.log in the output folder."
          )
          
          if (!is.null(cl)) {
            try(parallel::stopCluster(cl), silent = TRUE)
          }
          
          showNotification(
            "Simulation run failed!",
            type = "error"
          )
        })
      }, once = TRUE)
    }
  })
  
  # ==========================================================================
  # STOP button: only functional in background mode
  # ==========================================================================
  observeEvent(input$stop_batch, {
    req(proc_state$job)
    if (proc_state$job$is_alive()) {
      proc_state$job$kill()
      sys_status$batch_log <- "🛑 YOU STOP PROCESS SUCCESSFULLY (SEE THE YOUR OUTPUT FOLDER TO GET WHAT HAVE DONE!)"
      showNotification("Simulation Stopped (Process Killed).", type = "warning", duration = NULL)
    }
    if (!is.null(proc_state$bg_out_dir)) {
      trash_file <- file.path(proc_state$bg_out_dir, ".pkg_load_trash.log")
      try(file.remove(trash_file), silent = TRUE)
    }
    proc_state$is_running <- FALSE
    proc_state$job        <- NULL
    proc_state$bg_out_dir <- NULL
    proc_state$bg_cores   <- NULL
    sync_batch_buttons(FALSE, "background")
  })
  
  # ==========================================================================
  # Background watchdog: poll every 1 second to detect completion
  # ==========================================================================
  observe({
    req(proc_state$is_running, !is.null(proc_state$job))
    invalidateLater(1000)
    
    bg_dir     <- proc_state$bg_out_dir
    bg_cores   <- proc_state$bg_cores
    bg_settings_log_line <- if (
      is.null(proc_state$bg_settings_log_line)
    ) {
      ""
    } else {
      proc_state$bg_settings_log_line
    }
    if (proc_state$job$is_alive()) {
      sys_status$batch_log <- paste0(
        "🔄 Simulation Running in Background...\n",
        "========================================\n",
        "💻 Active workers: ", bg_cores, "\n",
        "📂 Saving results to: ", bg_dir, "\n",
        bg_settings_log_line,
        "========================================\n",
        "Use the Stop button to cancel."
      )
    } else {
      # Process ended — collect result
      res <- try(proc_state$job$get_result(), silent = TRUE)
      
      if (inherits(res, "try-error")) {
        
        err_msg <- conditionMessage(attr(res, "condition"))
        
        sys_status$batch_log <- paste0(
          "❌ Simulation process ended with an error.\n",
          "========================================\n",
          "📂 Output folder: ", bg_dir, "\n",
          if (nzchar(err_msg)) {
            paste0("Error: ", err_msg, "\n")
          } else {
            ""
          },
          "Check sim_info.log in the output folder."
        )
        
        showNotification(
          "Simulation run failed. Check sim_info.log.",
          type = "error"
        )
        
      } else if (
        is.list(res) &&
        identical(res$status, "success")
      ) {
        
        sys_status$batch_log <- paste0(
          "✅ ALL SIMULATION JOBS COMPLETED SUCCESSFULLY.\n",
          "========================================\n",
          "Total jobs: ", res$total_jobs, "\n",
          "Successful: ", res$successful_jobs, "\n",
          "Failed: ", res$failed_jobs, "\n",
          "========================================\n",
          "💻 Active workers: ", bg_cores, "\n",
          "📂 Data saved to: ", bg_dir, "\n",
          "========================================\n",
          "Please go to Step 4 to load and view results."
        )
        
        showNotification(
          paste0(
            "All ", res$successful_jobs,
            " simulation jobs completed successfully."
          ),
          type = "message",
          duration = NULL
        )
        
      } else if (
        is.list(res) &&
        identical(res$status, "partial")
      ) {
        
        sys_status$batch_log <- paste0(
          "⚠️ SOME OF THE SIMULATION JOB(S) FAILED.\n",
          "========================================\n",
          "Total jobs: ", res$total_jobs, "\n",
          "Successful: ", res$successful_jobs, "\n",
          "Failed: ", res$failed_jobs, "\n",
          "========================================\n",
          "Successful outputs have been saved.\n",
          "📂 Output folder: ", bg_dir, "\n",
          "Check sim_info.log for the failed jobs."
        )
        
        showNotification(
          paste0(
            "Some of the simulation job(s) failed. ",
            res$successful_jobs, " succeeded; ",
            res$failed_jobs, " failed."
          ),
          type = "warning",
          duration = NULL
        )
        
      } else if (
        is.list(res) &&
        identical(res$status, "failed")
      ) {
        
        sys_status$batch_log <- paste0(
          "❌ ALL SIMULATION JOBS FAILED.\n",
          "========================================\n",
          "Total jobs: ", res$total_jobs, "\n",
          "Successful: ", res$successful_jobs, "\n",
          "Failed: ", res$failed_jobs, "\n",
          "========================================\n",
          "📂 Output folder: ", bg_dir, "\n",
          "Check sim_info.log for details."
        )
        
        showNotification(
          paste0(
            "All ", res$failed_jobs,
            " simulation jobs failed."
          ),
          type = "error",
          duration = NULL
        )
        
      } else {
        
        sys_status$batch_log <- paste0(
          "❌ Simulation returned an unexpected result.\n",
          "📂 Output folder: ", bg_dir, "\n",
          "Check sim_info.log in the output folder."
        )
        
        showNotification(
          "Simulation returned an unexpected result.",
          type = "error"
        )
      }
      trash_file <- file.path(bg_dir, ".pkg_load_trash.log")
      try(file.remove(trash_file), silent = TRUE)
      proc_state$is_running  <- FALSE
      proc_state$job         <- NULL
      proc_state$bg_out_dir  <- NULL
      proc_state$bg_cores    <- NULL
      proc_state$bg_settings_log_line <- NULL
      sync_batch_buttons(FALSE, "background")
    }
  })
  
  
  # [STEP 3 LOGIC] Result Analysis (Dynamic & Robust)
  
  # 1. Browse Button Logic (cross-platform: uses shinyFiles)
  shinyFiles::shinyDirChoose(input, "browse_output", roots = roots, session = session)
  
  observeEvent(input$browse_output, {
    if (!is.integer(input$browse_output)) {
      selected_path <- shinyFiles::parseDirPath(roots, input$browse_output)
      if (length(selected_path) > 0 && nchar(selected_path) > 0) {
        updateTextInput(session, "res_out_dir", value = selected_path)
      }
    }
  })
  
  loaded_scenarios <- reactiveVal(NULL)
  valid_burn_in_val <- reactiveVal(5)
  observe({
    val <- input$transient_years
    updateNumericInput(session, "res_burn_in", value = val)
    valid_burn_in_val(val)
  })
  # 2. Load Scenarios (Scanning)
  observeEvent(input$load_results, {
    target_dir <- normalizePath(input$res_out_dir, mustWork = FALSE)
    
    
    check <- check_results_data(target_dir)
    
    if (!check$pass) {
      showNotification(check$msg, type = "error")
      loaded_scenarios(NULL)
      return()
    }
    
    
    subdirs <- list.dirs(target_dir, full.names = TRUE, recursive = FALSE)
    summary_list <- list()
    
    withProgress(message = 'Scanning folders...', value = 0, {
      for (i in seq_along(subdirs)) {
        dir_path <- subdirs[i]
        info_file <- file.path(dir_path, "scenario_info.csv")
        
        # ( check_results_data ,)
        if (file.exists(info_file)) {
          info_df <- tryCatch(data.table::fread(info_file), error = function(e) NULL)
          
          if (!is.null(info_df) && nrow(info_df) > 0) {
            
            scen_name <- info_df$scenario_name[1]
            esd       <- info_df$ESD[1]
            pae       <- info_df$prop_annual_encounters[1]
            rm_val    <- info_df$release_mortality[1]
            min_len   <- info_df$min_len_mm[1]
            
            label_str <- sprintf("%s | Min:%.0f | ESD:%.1f | PAE:%.2f | RM:%.1f",
                                 scen_name, min_len, esd, pae, rm_val)
            
            summary_list[[length(summary_list) + 1]] <- data.frame(
              folder_path = dir_path,
              folder_name = basename(dir_path),
              label       = label_str ,
              scenario_name = scen_name,
              stringsAsFactors = FALSE
            )
          }
        }
        incProgress(1/length(subdirs))
      }
    })
    
    
    if (length(summary_list) > 0) {
      final_df <- do.call(rbind, summary_list)
      loaded_scenarios(final_df)
      showNotification(check$msg, type = "message") # check_results_data
    } else {
      loaded_scenarios(NULL)
      showNotification("❌ Error: Valid folders found but metadata scan failed.", type = "warning")
    }
  })
  
  observeEvent(input$btn_update_burnin, {
    req(input$res_selected_scen) 
    new_val <- input$res_burn_in
    
    t_red <- res_policy_year()
    
    errors <- c()
    
    if (!is.numeric(new_val) || is.na(new_val)) {
      errors <- c(errors, "Value must be a number.")
    } else {
      if (new_val < 0) {
        errors <- c(errors, "Burn-in years cannot be negative.")
      }
      if (new_val %% 1 != 0) {
        errors <- c(errors, "Burn-in years must be an integer.")
      }
      if (t_red > 0 && new_val >= t_red) {
        errors <- c(errors, paste0("Burn-in end (", new_val, ") must be less than Policy Start Year (", t_red, ")."))
      }
    }
    if (length(errors) > 0) {
      showNotification(paste0("Invalid Input:\n", paste(errors, collapse = "\n")), type = "error")
      updateNumericInput(session, "res_burn_in", value = valid_burn_in_val())
      
    } else {
      valid_burn_in_val(new_val) 
      showNotification(paste0("Burn-in updated to Year ", new_val), type = "message")
    }
  })
  
  
  
  
  # 3. Dynamic Dropdown
  output$result_scen_selector <- renderUI({
    df <- loaded_scenarios()
    if (is.null(df)) return(selectInput("res_selected_scen", "Select Scenario:", choices = c("No data loaded" = "")))
    
    choices <- setNames(df$folder_path, df$label)
    selectInput("res_selected_scen", "Select Scenario:", choices = choices, width = "100%")
  })
  
  output$res_scen_desc <- renderText({
    req(input$res_selected_scen)
    paste("Loaded Folder:", basename(input$res_selected_scen))
  })
  
  observeEvent(input$res_selected_scen,{
    req(input$res_selected_scen)
    scen_path <- input$res_selected_scen
    
    # Burn-in
    files <- list.files(scen_path, pattern = "iter\\d+.*before_policy.*\\.csv", full.names = TRUE)
    
    if(length(files) > 0) {
      tryCatch({
        d <- data.table::fread(files[1], select = "year")
        real_policy_start_year <- max(d$year, na.rm = TRUE)
        
        res_policy_year(real_policy_start_year)
        
      }, error = function(e) {
        
      })
    }
  })
  
  
  # Logic Step 3: Data Processing & Plotting (FINAL VERSION)
  
  
  # 4. Data Processing for Plot
  plot_data <- reactive({
    req(input$res_selected_scen, input$res_var_y)
    scen_path <- input$res_selected_scen
    var_name <- input$res_var_y
    
    
    burn_in_end <- valid_burn_in_val() # Blue Line (Transient End)
    
    
    # (Policy Start) = Blue Line + Stable Length
    # ：, Max Year
    t_blue <- burn_in_end
    t_red <- res_policy_year()
    
    # Policy Map
    poly_file <- file.path(scen_path, "policy_combos_info.csv")
    poly_map <- if(file.exists(poly_file)) data.table::fread(poly_file) else NULL
    
    files <- list.files(scen_path, pattern = "iter\\d+.*\\.csv", full.names = TRUE)
    if(length(files) == 0) return(NULL)
    
    burnin_files <- files[grep("before_policy", files)]
    policy_files <- files[grep("_policy_[0-9]+\\.csv$", files)]
    
    
    # CASE A: Bar Chart Logic (Enc_T)
    
    if (var_name == "trophy_seen") {
      df_burn <- calc_burnin_counts(burnin_files, "Before Policy", t_blue, t_red)
      
      df_pols_list <- list()
      if (length(policy_files) > 0) {
        all_basenames <- basename(policy_files)
        p_ids <- unique(stringr::str_extract(all_basenames, "policy_\\d+"))
        p_ids <- p_ids[!is.na(p_ids)]
        p_ids <- p_ids[order(as.numeric(gsub("policy_", "", p_ids)))]
        for (pid in p_ids) {
          sub_files <- policy_files[grep(paste0(pid, "\\.csv"), all_basenames)]
          pid_num   <- as.integer(gsub("policy_", "", pid))
          label     <- paste0("Policy ", pid_num)
          df_pols_list[[label]] <- calc_policy_counts(sub_files, label, t_red)
        }
      }
      
      final_dt <- rbind(df_burn, do.call(rbind, df_pols_list))
      pol_levels <- if (length(df_pols_list) > 0) names(df_pols_list) else character(0)
      final_dt$Group <- factor(final_dt$Group, levels = c("Before Policy", pol_levels))
      return(list(type = "bar", data = final_dt))
      
    } else {
      
      # CASE B: Line Chart Logic
      
      calc_stats <- function(file_list, group_name) {
        if(length(file_list) == 0) return(NULL)
        dt <- data.table::rbindlist(lapply(file_list, function(f) {
          data.table::fread(f, select = c("year", var_name))
        }))
        agg <- dt[, .(
          Mean = mean(get(var_name), na.rm = TRUE),
          Min  = min(get(var_name), na.rm = TRUE),
          Max  = max(get(var_name), na.rm = TRUE)
        ), by = year]
        agg$Group <- group_name
        return(agg)
      }
      
      df_burn <- calc_stats(burnin_files, "Before Policy")
      anchor_point <- if(!is.null(df_burn)) df_burn[year == max(df_burn$year), ] else NULL
      
      df_pols_list <- list()
      if(length(policy_files) > 0) {
        all_basenames <- basename(policy_files)
        p_ids <- unique(stringr::str_extract(all_basenames, "policy_\\d+"))
        p_ids <- p_ids[!is.na(p_ids)]
        p_ids <- p_ids[order(as.numeric(gsub("policy_", "", p_ids)))]
        for(pid in p_ids) {
          sub_files <- policy_files[grep(paste0(pid, "\\.csv"), all_basenames)]
          pid_num <- as.integer(gsub("policy_", "", pid))
          label <- paste0("Policy ", pid_num)
          agg <- calc_stats(sub_files, label)
          
          if(!is.null(agg) && !is.null(anchor_point)) {
            anchor_copy <- anchor_point
            anchor_copy$Group <- label
            if (min(agg$year) > max(anchor_copy$year)) {
              agg <- rbind(anchor_copy, agg)
            }
          }
          if(!is.null(agg)) df_pols_list[[label]] <- agg
        }
      }
      
      final_dt <- rbind(df_burn, do.call(rbind, df_pols_list))
      pol_levels <- if(length(df_pols_list) > 0) names(df_pols_list) else character(0)
      final_dt$Group <- factor(final_dt$Group, levels = c("Before Policy", pol_levels))
      return(list(type = "line", data = final_dt))
    }
  })
  
  # 5. Dynamic Plotting
  output$res_main_plot <- renderPlot({
    res <- plot_data()
    req(res)
    
    dt <- res$data
    type <- res$type
    var_code <- input$res_var_y
    t_blue <- valid_burn_in_val()
    if(is.null(t_blue) || is.na(t_blue)) t_blue <- 0
    t_red <- res_policy_year()
    
    # Label Map & Colors
    var_base_name <- switch(var_code,
                            "Sden"    = "Spawning fish density",
                            "Rden"    = "Recruit density",
                            "AdultN"  = "Adult abundance",
                            "AgeFRN"   = "Recruit (fishery) abundance",
                            "Yield_n" = "Yield",
                            "N_pop"   = "Population size",
                            "PSD_Q"   = "PSD (Quality)",
                            "PSD_P"   = "PSD (Preferred)",
                            "PSD_M"   = "PSD (Memorable)",
                            "PSD_T"   = "PSD (Trophy)",
                            "Enc_Q"   = "Angler Encounters (Quality)",
                            "Enc_P"   = "Angler Encounters (Preferred)",
                            "Enc_M"   = "Angler Encounters (Memorable)",
                            "Enc_T"   = "Angler Encounters (Trophy)",
                            "trophy_seen" = "Months of Trophy Seen",
                            var_code 
    )
    
    unit_suffix <- case_when(
      var_code %in% c("Sden", "Rden") ~ "(ind/ha)",
      var_code %in% c("AdultN", "AgeFRN", "Yield_n", "N_pop") ~ "(number)",
      grepl("PSD", var_code) | grepl("Enc", var_code) ~ "(%)",
      var_code == "trophy_seen" ~ "(months)", 
      TRUE ~ "" 
    )
    
    if (var_code == "trophy_seen") {
      final_y_label <- paste("Months of Trophy Seen ")
    } else {
      final_y_label <- paste(var_base_name, unit_suffix)
    }
    
    
    my_theme <- theme_bw(base_size = 14) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom"
      )
    
    groups_present <- levels(dt$Group)
    burnin_color <- "#999999"
    n_policies <- sum(groups_present != "Before Policy")
    policy_colors <- if(n_policies > 0) scales::hue_pal()(n_policies) else character(0)
    final_colors <- c()
    if("Before Policy" %in% groups_present) final_colors["Before Policy"] <- burnin_color
    policy_names <- groups_present[groups_present != "Before Policy"]
    if(length(policy_names) > 0) {
      names(policy_colors) <- policy_names
      final_colors <- c(final_colors, policy_colors)
    }
    
    # --- Plotting ---
    if (type == "bar") {
      ggplot(dt, aes(x = Group, y = Mean, fill = Group)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        geom_errorbar(aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD), width = 0.2, linewidth = 0.8) +
        scale_fill_manual(values = final_colors) +
        scale_y_continuous(
          limits = c(0, NA), 
          expand = expansion(mult = c(0, 0.1)), 
          breaks = scales::breaks_width(1)     
        ) +
        labs(title = "Mean and SD of the Months with Trophy Fish Sighting for All Simulation Iteration", 
             subtitle = paste0("Burn in end: Years ", t_blue, " | Policy  Start: Years ", t_red), 
             y = final_y_label, x = "", fill = NULL) +
        my_theme +
        theme(legend.position = "none")
      
    } else {
      # Line Chart
      p <- ggplot(dt, aes(x = year)) +
        geom_ribbon(aes(ymin = Min, ymax = Max, fill = Group), alpha = 0.3) +
        geom_line(aes(y = Mean, color = Group), linewidth = 1) +
        
        # 1. Blue Line (Transient End)
        geom_vline(xintercept = t_blue, linetype = "dashed", color = "blue", linewidth = 0.8, alpha=0.6) +
        annotate("text", x = t_blue, y = Inf, label = paste0("Burn-in End (Year ", t_blue, ")"),
                 vjust = 2, hjust = 1.1, size=3.5, color="blue", fontface="italic") +
        
        # 2. Red Line (Policy Start)
        geom_vline(xintercept = t_red, linetype = "solid", color = "red", linewidth = 0.8, alpha=0.8) +
        annotate("text", x = t_red, y = Inf, label = paste0("Policy Start (Year ", t_red, ")"),
                 vjust = 2, hjust = -0.1, size=3.5, color="red", fontface="bold") +
        
        scale_fill_manual(values = final_colors) +
        scale_color_manual(values = final_colors) +
        scale_y_continuous(labels = scales::comma)+
        labs(title = NULL, subtitle = NULL, y = final_y_label, x = "Year", fill = NULL, color = NULL) +
        my_theme 
      
      return(p)
    }
  })
  # 6. Policy Legend Table
  output$res_policy_tbl <- DT::renderDT({
    req(input$res_selected_scen, plot_data(), input$res_var_y)
    
    p_res <- plot_data()
    dt <- p_res$data
    type <- p_res$type
    var_code <- input$res_var_y
    burn_in <- valid_burn_in_val()
    
    stat_dt <- NULL
    
    if (type == "bar") {
      stat_dt <- dt %>% 
        dplyr::select(Group, Value = Mean)
      
    } else {
      dt_df <- as.data.frame(dt)
      dt_clean <- dt_df %>%
        dplyr::filter(!(Group == "Before Policy" & year <= burn_in))
      
      stat_dt <- dt_clean %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(Value = mean(Mean, na.rm = TRUE), .groups = "drop")
    }
    
    
    path <- input$res_selected_scen
    f <- file.path(path, "policy_combos_info.csv")
    if(!file.exists(f)) return(NULL)
    
    df_info <- data.table::fread(f)
    
    df_pols <- df_info %>%
      dplyr::mutate(
        `Label` = paste0("policy_", policy_combo_id),
        `Compliance?` = ifelse(comp_mode == 1, "Yes", "No"),
        `Release mortality considered?` = ifelse(release_mortality > 0, "Yes", "No"),
        JoinGroup = paste0("Policy ", policy_combo_id)
      ) %>%
      dplyr::select(`Label`, `Compliance?`, `Release mortality considered?`, JoinGroup)
    
    row_before <- data.frame(
      `Label` = "Before Policy",
      `Compliance?` = "-",
      `Release mortality considered?` = "-",
      JoinGroup = "Before Policy",
      check.names = FALSE
    )
    
    df_full <- dplyr::bind_rows(row_before, df_pols)
    df_merged <- df_full %>%
      dplyr::left_join(stat_dt, by = c("JoinGroup" = "Group"))
    
    scale_factor <- 1
    
    unit_label <- case_when(
      var_code == "trophy_seen" ~ "(months)",     
      grepl("PSD", var_code) ~ "(%)",
      grepl("Enc", var_code) ~ "(%)",
      var_code %in% c("Sden", "Rden") ~ "(ind/ha)",
      TRUE ~ "(number)"
    )
    
    df_final <- df_merged %>%
      dplyr::mutate(
        Value = tidyr::replace_na(Value, 0), 
        Value = Value * scale_factor,
        Value = round(Value, 2)
      ) %>%
      dplyr::select(
        `Label`, `Compliance?`, `Release mortality considered?`, `Temp_Value` = Value 
      )
    
    if (var_code == "trophy_seen") {
      final_col_name <- "Average (months)"
    } else {
      final_col_name <- paste("Average", unit_label)
    }
    
    colnames(df_final)[4] <- final_col_name
    
    DT::datatable(df_final, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
  })
}
shinyApp(ui, server)
