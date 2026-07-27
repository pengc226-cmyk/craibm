suppressPackageStartupMessages({
  library(shiny)
  library(promises)
  library(future)
  library(bs4Dash)
  library(dplyr)
  library(ggplot2)
})


loadNamespace("craibm")

.craibm_ns <- asNamespace("craibm")
for (.craibm_nm in ls(.craibm_ns, all.names = FALSE)) {
  assign(.craibm_nm, get(.craibm_nm, envir = .craibm_ns))
}
rm(.craibm_nm, .craibm_ns)

# The cloud watcher must run outside the main Shiny process so network waits
# cannot block or grey out the page.
future::plan(future::multisession, workers = 2)
.craibm_runs <- new.env(parent = emptyenv())
.craibm_runs$local <- NULL
.craibm_runs$cloud <- NULL
.craibm_runs$work <- NULL
.craibm_runs$session_count <- 0L
.craibm_cloud_record_path <- function() {
  dir <- tryCatch(tools::R_user_dir("craibm", "cache"),
                  error = function(e) file.path(tempdir(), "craibm"))
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(dir, "active_cloud_job.rds")
}

.craibm_register_cloud <- function(rec) {
  .craibm_runs$cloud <- rec
  try(saveRDS(rec, .craibm_cloud_record_path()), silent = TRUE)
  invisible(TRUE)
}

.craibm_clear_cloud <- function() {
  .craibm_runs$cloud <- NULL
  try(unlink(.craibm_cloud_record_path()), silent = TRUE)
  invisible(TRUE)
}
.craibm_pending_cloud <- function() {
  rec <- .craibm_runs$cloud
  if (is.null(rec)) {
    rec <- tryCatch({
      p <- .craibm_cloud_record_path()
      if (file.exists(p)) readRDS(p) else NULL
    }, error = function(e) NULL)
  }
  if (is.null(rec) || is.null(rec$job_id)) return(NULL)
  
  # A missing or unreadable timestamp only costs the age display.
  if (!inherits(rec$started, "POSIXct") || length(rec$started) != 1L ||
      is.na(rec$started)) {
    rec$started <- NA
  }
  rec
}

.craibm_pending_local <- function() {
  rec <- .craibm_runs$local
  if (is.null(rec) || is.null(rec$job)) return(NULL)
  
  alive <- tryCatch(rec$job$is_alive(), error = function(e) FALSE)
  if (!isTRUE(alive)) {
    # It finished while nobody was connected. The watchdog in the new
    # session still needs the handle to collect the result, so the record
    # is kept and simply reported as finished.
    rec$finished <- TRUE
  }
  rec
}



.craibm_clear_local <- function() {
  .craibm_runs$local <- NULL
  invisible(TRUE)
}

.craibm_write_work_snapshot <- function(snap) {
  
  if (!is.list(snap)) {
    return(invisible(FALSE))
  }
  .craibm_runs$work <- snap
  
  disk_ok <- tryCatch(
    {
      saveRDS(
        snap,
        .craibm_work_snapshot_path()
      )
      
      TRUE
    },
    error = function(e) {
      message(
        "craibm: could not write the work snapshot: ",
        conditionMessage(e)
      )
      
      FALSE
    }
  )
  
  invisible(disk_ok)
}
.craibm_work_snapshot_path <- function() {
  dir <- tryCatch(tools::R_user_dir("craibm", "cache"),
                  error = function(e) file.path(tempdir(), "craibm"))
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(dir, "work_snapshot.rds")
}

.craibm_read_work_snapshot <- function() {
  
  if (!is.null(.craibm_runs$work) &&
      is.list(.craibm_runs$work)) {
    return(.craibm_runs$work)
  }
  snap <- tryCatch(
    {
      p <- .craibm_work_snapshot_path()
      
      if (!file.exists(p)) {
        return(NULL)
      }
      
      readRDS(p)
    },
    error = function(e) NULL
  )
  
  if (!is.list(snap) || is.null(snap$inputs)) {
    return(NULL)
  }
  .craibm_runs$work <- snap
  
  snap
}

.craibm_clear_work_snapshot <- function() {
  
  .craibm_runs$work <- NULL
  
  try(
    unlink(.craibm_work_snapshot_path()),
    silent = TRUE
  )
  
  invisible(TRUE)
}





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
      menuItem("Step 1a: Parameters 1: Biological Data Inputs", tabName = "params", icon = icon("sliders-h")),
      menuItem("Step 1b: Parameters 2: Global Parameters", tabName = "global", icon = icon("sliders")),
      menuItem("Step 1c: Parameters 3: Experiment Design", tabName = "design", icon = icon("flask")),
      menuItem("Step 1d: Design Preview", tabName = "combos", icon = icon("table")),
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
        actionButton(
          "recover_last_session",
          "Recover last session",
          icon = icon("clock-rotate-left"),
          class = "btn-outline-light btn-sm",
          style = paste0(
            "width: 100%;",
            "margin-bottom: 6px;",
            "text-align: left;"
          )
        ),
        
        textOutput("recover_last_session_log"),
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
    
    # -------------------------------------------------------------------------
    # Browser reconnection signal
    #
    # shiny:connected fires whenever a WebSocket connection is established.
    # The first event is the normal app startup and is ignored. Any later event
    # means that this browser has reconnected after losing its WebSocket.
    # -------------------------------------------------------------------------
    tags$script(
      HTML("
  (function() {
    
    const craibmTabKey =
      'craibm-work-tab:' + window.location.pathname;
    
    let resumeThisTab = false;
    
    try {
      resumeThisTab =
        window.sessionStorage.getItem(craibmTabKey) === '1';
      
      window.sessionStorage.setItem(
        craibmTabKey,
        '1'
      );
    } catch (e) {
      resumeThisTab = false;
    }
    
    $(document).on('shiny:connected', function(event) {
      
      Shiny.setInputValue(
        'craibm_resume_tab',
        {
          resume: resumeThisTab,
          nonce: String(Date.now()) + '-' + String(Math.random())
        },
        { priority: 'event' }
      );
      
      // Any later WebSocket connection in this same page is a reconnect.
      resumeThisTab = true;
    });
    
  })();
")
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
                        box(
                          title = "Growth (VBGF) and Age-Length Key Data",
                          width = 12,
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          
                          fileInput(
                            "file_growth",
                            "Upload Length-Age Data (CSV)",
                            accept = ".csv"
                          ),
                          
                          helpText(
                            "Required columns: 'Length' in millimeters (mm) and 'Age' in years."
                          ),
                          
                          uiOutput("missing_age_choice"),
                          
                          numericInput(
                            "boot_b_vbgf",
                            "Bootstrap Replicates",
                            value = NA,
                            min = 100
                          ),
                          
                          actionButton(
                            "run_vbgf",
                            " Run Growth Bootstrap",
                            icon = icon("rocket"), 
                            class = "btn-primary",
                            style = "width: 100%; font-weight: bold; background-color: #007bff; color: white; border: none; margin-top: 10px;"
                          ),
                          
                          div(
                            style = "margin-top: 12px;",
                            
                            checkboxInput(
                              "show_growth_advanced",
                              "Advanced: set random seeds",
                              value = FALSE
                            ),
                            
                            conditionalPanel(
                              condition = "input.show_growth_advanced == true",
                              
                              wellPanel(
                                style = paste0(
                                  "background: #f8f9fa;",
                                  "padding: 10px;",
                                  "margin-bottom: 0;"
                                ),
                                
                                helpText(
                                  "Leave a box empty to draw a new seed each run.",
                                  "Please enter the seed reported on the right to repeat an earlier run exactly."
                                ),
                                
                                numericInput(
                                  "vbgf_seed_manual",
                                  "Growth bootstrap seed",
                                  value = NA,
                                  min = 1,
                                  step = 1
                                ),
                                
                                uiOutput("alk_seed_input")
                              )
                            )
                          ),
                          
                          actionButton(
                            "use_generated_alk",
                            "Use Automatically Generated ALK",
                            icon = icon("rotate-left"),
                            style = paste0(
                              "width: 100%;",
                              "font-weight: bold;",
                              "background-color: #28a745;",
                              "color: white;",
                              "border: none;",
                              "margin-top: 20px;"
                            )
                          ),
                          
                          wellPanel(
                            style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #17a2b8; margin-top: 20px;",
                            
                            tags$h5(
                              icon("upload"),
                              strong(" Optional: Upload Your Own ALK")
                            ),
                            
                            helpText(
                              "Use the button above to generate an ALK from the uploaded length-age data. ",
                              "You may instead upload your own ALK to replace the generated table."
                            ),
                            
                            fileInput(
                              "file_alk",
                              "Upload ALK Data (CSV)",
                              accept = ".csv"
                            ),
                            
                            helpText(
                              "Required columns: 'Age' in years, 'n' as the sample count, ",
                              "'Length' as mean length in mm, and 'Lengthsd' as standard deviation in mm."
                            ),
                            
                            actionButton(
                              "submit_alk",
                              "Use or Update Uploaded ALK",
                              icon = icon("check-circle"),
                              style = "width: 100%; font-weight: bold; background-color: #17a2b8; color: white; border: none;"
                            ),
                            
                            uiOutput("auto_alk_note")
                          ))
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
                            verbatimTextOutput("step1_info_box")
                          ),
                          
                          tabPanel(
                            "Growth (VBGF)",
                            icon = icon("fish"),
                            h4("VBGF bootstrap parameter distributions"),
                            plotOutput("plot_vbgf", height="500px"),
                            verbatimTextOutput("summary_vbgf")
                          ),
                          tabPanel(
                            "Age-Length Key Data",
                            icon = icon("table"),
                            
                            h4("Age-Length Key Data"),
                            
                            helpText(
                              "Please review the automatically generated or uploaded ALK currently selected ",
                              "for mortality estimation and population initialization."
                            ),
                            
                            uiOutput("alk_preview_block"),
                            
                            # Mounted once, in the static UI. See the note in
                            # output$alk_preview_block.
                            DT::dataTableOutput("alk_table_preview")
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
                numericInput("transient_years", "Burn in years (initial model warm-up):", value = NA, min = 0, step = 1),
                numericInput("stable_years", "Stable years (equilibrium phase before policy):", value = NA, min = 0, step = 1),
                helpText("'Before policy' = Burn in years + Stable years combined. "),
                numericInput("policy_years",  "Policy years (the length of simulation policy years):",  value = NA, min = 1, step = 1)
              ),
              
              
              # 3) Density-dependent effects
              
              tabPanel(
                "Density-dependent Effects",
                tags$div(
                  class = "alert alert-info",
                  icon("book-open"),
                  "Please see the user manual for definitions, equations, units, and descriptions ",
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
                        shiny::column(4, numericInput("surv_a", "a", value = NA, step = 0.001)),
                        shiny::column(4, numericInput("surv_b", "b", value = NA,   step = 0.01)),
                        shiny::column(4, numericInput("surv_c", "c", value = NA,  step = 0.01))
                      ),
                      fluidRow(
                        shiny::column(6, numericInput("surv_d_avg1", "d1", value = NA, step = 0.01)),
                        shiny::column(6, numericInput("surv_d_avg2", "d2", value = NA, step = 0.01))
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
                        shiny::column(4, numericInput("g1_a", "a", value = NA, step = 0.001)),
                        shiny::column(4, numericInput("g1_b", "b", value = NA, step = 0.001)),
                        shiny::column(4, numericInput("g1_c", "c", value = NA, step = 0.001))
                      ),
                      numericInput("g1_d_avg", "d", value = NA, step = 0.01)
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
                        shiny::column(4, numericInput("g2_a", "a", value = NA, step = 0.001)),
                        shiny::column(4, numericInput("g2_b", "b", value = NA, step = 0.001)),
                        shiny::column(4, numericInput("g2_c", "c", value = NA, step = 0.001))
                      ),
                      numericInput("g2_d_avg", "d", value = NA, step = 0.01)
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
                          "Please enter a value from 0 to 1."
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
                          value = NA,
                          step = 1,
                          min = 0
                        )
                      ),
                      
                      shiny::column(
                        4,
                        numericInput(
                          "harv_pmax",
                          "Maximum retention probability (p_max)",
                          value = NA,
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
                          value = NA,
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
                      value = NA,
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
                    value = ""),
                  
                  helpText(
                    "Please enter 12 non-negative relative weights in calendar order, beginning ",
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
                                    
                                    numericInput("juv_annual_M", "Juvenile annual nature mortality coefficient (instantaneous)", value = NA, step = 0.01, min = 0.001),
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
                                                               numericInput("z_last", "Catch curve max age", value = NA, min=1),
                                                               
                                                               fluidRow(
                                                                 shiny::column(6, numericInput("z_boot_bg2", "Bootstrap replicates", value = NA, min=100)),
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
                                                                     "Please enter the seed reported on the right to repeat an earlier run exactly."
                                                                   )
                                                                 )
                                                               ),
                                                               hr(),
                                                               h5(strong("2. Assumed Relationship")),
                                                               numericInput("F_over_Z_ratio", "Assumed M/Z ratio", value = NA, step = 0.01, min=0.01, max=0.99)
                                                               
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.use_z_estimation == false",
                                                     wellPanel(style = "background: #fff3cd; border-left: 5px solid #ffc107; padding: 10px;",
                                                               h5(strong("Direct Input Mode")),
                                                               numericInput("fixed_adult_M", "Fixed Adult Annual M", value = NA, step = 0.01, min=0.001),
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
                           fluidRow(shiny::column(2, numericInput("psd_stock", "Stock", value = NA)),
                                    shiny::column(2, numericInput("psd_quality", "Quality", value = NA)),
                                    shiny::column(3, numericInput("psd_preferred", "Preferred", value = NA)),
                                    shiny::column(3, numericInput("psd_memorable", "Memorable", value = NA)),
                                    shiny::column(2, numericInput("psd_trophy", "Trophy", value = NA)))
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
                             shiny::column(4, numericInput("age_spawn", "Maturity age", value = NA, step = 0.5, min=0.1)),
                             shiny::column(4, numericInput("min_adult_age", "Transition age", value = NA, step = 0.5, min=0.1)),
                             shiny::column(4, numericInput("z_full", "Recruit age (fishery)", value = NA, min=0, step=1))
                           ),
                           
                           tags$details(
                             style = "margin-bottom: 15px; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                             tags$summary(icon("info-circle"), strong(" Click here for parameter definitions"), style = "cursor: pointer; color: #007bff;"),
                             tags$ul(style = "margin-top: 10px; color: #6c757d; font-size: 0.9em;",
                                     tags$li(strong("Maturity age:"), " Age at which fish start contributing to Spawning Biomass (used in R-S relationship)."),
                                     tags$li(strong("Transition age:"), " Age when biology changes from Juvenile to Adult (used for applying Natural Mortality and as the full recruitment age in Catch Curve analysis)."),
                                     tags$li(strong("Recruit age (fishery):"),
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
                             shiny::column(3, numericInput("spawn_month", "Spawn month", value = NA, min=1, max=12)),
                             shiny::column(3, numericInput("recruit_entry_month", "Recruits entry month", value = NA, min=1, max=12)),
                             
                             # R-S Parameters
                             shiny::column(3, numericInput("rec_a", "R-S alpha", value = NA)),
                             shiny::column(3, numericInput("rec_b", "R-S beta", value = NA))
                           ),
                           helpText("Recruits entry month<Spawn month is allowed. If so, new fish will enter population in the next year ")
                       ),
                       
                       # 3. Environment & General (Now very clean!)
                       box(title = "Environment & Initialization", width = 12, status = "secondary", solidHeader = TRUE, collapsible = TRUE, icon = icon("globe"),
                           fluidRow(
                             shiny::column(6, numericInput("lake_area_ha", "Lake area (ha)", value = NA)),
                             shiny::column(6, numericInput("initial_pop_size","Initial population size", value = NA))
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
                        value = ""),
                      
                      textInput(
                        "pae_vec",
                        paste0(
                          "Proportion of vulnerable fish with at least one ",
                          "annual angler encounter (PAE), comma-separated"
                        ),
                        value = ""),
                      
                      textInput(
                        "rm_vec",
                        "Release mortality rate (RM), comma-separated",
                        value = ""),
                      
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
                        value = ""),
                      
                      textInput(
                        "comp_probs",
                        paste0(
                          "Compliance probabilities (0–1), comma-separated ",
                          "(same number of values as the breakpoints)"
                        ),
                        value = ""),
                      
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
            title = "Design Preview", width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
            uiOutput("combos_dynamic_ui")
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
              numericInput("n_iter", "Number of iterations (runs)", value = NA, min = 1, step = 1),
              numericInput("seed", "Random seed", value = NA, min = 1, step = 1)
            )
          )
        ),
        
        
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
                  shiny::column(6, textInput("gcp_region", "Region", value = "",
                                             placeholder = "us-central1"))
                ),
                fluidRow(
                  shiny::column(6, textInput("gcp_bucket", "Storage bucket",
                                             placeholder = "my-craibm-data")),
                  shiny::column(6, textInput("gcp_machine_type", "Machine type",
                                             value = "",
                                             placeholder = "n2-highmem-8"))
                ),
                textInput(
                  "gcp_container_image",
                  "Public GHCR container image",
                  value = Sys.getenv("CRAIBM_CLOUD_IMAGE", unset = ""),
                  placeholder = "ghcr.io/pengc226-cmyk/craibm:latest",
                  width = "100%"
                ),
                helpText(
                  "Please choose a machine type in the Google Cloud console that suits",
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
                      "in separate R worker processes. These are processes, not threads",
                      "and not a direct request for the same number of CPU cores."
                    ),
                    
                    tags$p(
                      style = "font-size:13px; margin-top:8px; margin-bottom:8px;",
                      tags$b("Most useful when: "),
                      "the experiment contains many uncertainty scenarios or ",
                      "iterations that can be completed independently."
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
                      "Concurrent replicate workers (separate R processes)",
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
                      "Uses multiple threads to run management-policy combinations",
                      "concurrently inside each active replicate worker."
                    ),
                    
                    tags$p(
                      style = "font-size:13px; margin-top:8px; margin-bottom:8px;",
                      tags$b("Most useful when: "),
                      "each scenario contains many size-limit or management-policy ",
                      "conditions, such as different compliance or release-mortality ",
                      "assumptions."
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
                        "Policy-combination threads per replicate worker",
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
                      "Uses OpenMP threads inside each policy calculation to split",
                      "fish-level survival calculations at each monthly time step."
                    ),
                    
                    tags$p(
                      style = "font-size:13px; margin-top:8px; margin-bottom:8px;",
                      tags$b("Most useful when: "),
                      "a single model contains a large fish population and monthly ",
                      "fish-level survival calculations account for much of the runtime."
                    ),
                    
                    checkboxInput(
                      "simulation_engine",
                      "Enable large-population optimization",
                      value = FALSE
                    )
                  ),
                  
                  shiny::column(
                    width = 8,
                    
                    conditionalPanel(
                      condition = "input.simulation_engine == true",
                      
                      sliderInput(
                        "omp_nthreads",
                        "Individual-level threads per policy calculation (OpenMP)",
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
                          strong("Calculated safe duration: "),
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
                        "Use calculated safe duration — recommended" = "auto",
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
                    "border-left:5px solid #17a2b8;",
                    "padding:16px;",
                    "margin-top:12px;"
                  ),
                  
                  tags$h4(
                    icon("circle-info"),
                    strong("About Test Simulation")
                  ),
                  
                  tags$p(
                    "Use this step to check the model setup before starting a full ",
                    "simulation. Test 1 examines model behavior and the selected ",
                    "timeline. Test 2 measures speed and memory use on the computer ",
                    "or Google Cloud machine that will run the model."
                  ),
                  
                  tags$hr(),
                  
                  tags$h5(
                    icon("chart-line"),
                    strong("Test 1: Model Validation")
                  ),
                  
                  tags$p(
                    "Test 1 is a short model check. It runs one selected scenario, ",
                    "all policies for that scenario, and one replicate."
                  ),
                  
                  tags$p(
                    "Use the diagnostic plot to judge whether the burn-in period allows ",
                    "the population to stabilize, whether the stable period provides an ",
                    "appropriate pre-policy baseline, and whether the policy period is long ",
                    "enough to show the management response."
                  ),
                  
                  tags$p(
                    "Test 1 also provides a rough estimate of the full-model calculation time. ",
                    "The estimate is rough because this test runs only one model task and does ",
                    "not measure how much simultaneous workers may slow each other down."
                  ),
                  
                  tags$hr(),
                  
                  tags$h5(
                    icon("microchip"),
                    strong("Test 2: Parallel Performance Check")
                  ),
                  
                  tags$p(
                    "Test 2 takes longer because it runs one model worker alone and then runs ",
                    "several model workers at the same time using the parallel settings confirmed ",
                    "in Step 2."
                  ),
                  
                  tags$p(
                    "It measures parallel speed, CPU sharing and memory use. The report helps ",
                    "assess whether the selected computer or Google Cloud machine type is ",
                    "suitable for the planned full simulation and provides a more precise ",
                    "calculation-time estimate than Test 1."
                  ),
                  
                  tags$p(
                    "Test 2 is strongly recommended but does not block the full simulation. ",
                    "Starting without a current check means that speed, memory use and machine ",
                    "suitability have not been confirmed."
                  ),
                  
                  tags$p(
                    tags$b("Balanced check: "),
                    "uses a CPU-balanced sample of the configured replicate workers. It is the ",
                    "quicker and safer option to run first."
                  ),
                  
                  tags$p(
                    tags$b("Optional full-load check: "),
                    "uses every configured replicate worker. It places a heavier load on the ",
                    "machine but provides the most representative pre-run speed and memory report."
                  ),
                  
                  tags$p(
                    tags$b("Google Cloud note: "),
                    "machine startup and resource availability are controlled by Google Cloud. ",
                    "A job that remains QUEUED or SCHEDULED, or temporarily fails to start, may ",
                    "indicate limited capacity or quota in the selected region rather than an ",
                    "unsuitable model or machine type."
                  ),
                  
                  tags$p(
                    icon("book-open"),
                    " For technical definitions and calculation details, please see the Help Guide."
                  ),
                  
                  # Cloud only: where View Result should place the small
                  # test result it downloads. Optional -- blank means a
                  # temporary folder. Never blocks a run.
                  conditionalPanel(
                    condition = "input.use_cloud == true",
                    tags$hr(),
                    tags$h5("Cloud test results"),
                    tags$p(
                      "When a cloud test finishes, use View Result on its tab ",
                      "to download the result and draw its report. Choose a download ",
                      "folder with the button below. If no folder is selected, the app ",
                      "uses a temporary folder automatically."
                    ),
                    shinyFiles::shinyDirButton(
                      "browse_cloud_test_dir",
                      "Choose download folder",
                      "Select a folder for cloud test results",
                      class = "btn-secondary",
                      style = "width:100%;",
                      icon = icon("folder-open")
                    ),
                    uiOutput("cloud_test_dir_status")
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
                      )),
                    
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
                    
                    uiOutput("cloud_watch_panel_test1"),
                    
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
                            "Balanced check — use a CPU-balanced worker group ",
                            "(recommended first)"
                          ),
                          
                          paste0(
                            "Optional full-load check — use every configured replicate worker ",
                            "(higher load and more accurate time estimate)"
                          )
                        )
                      ),
                      
                      selected = "safe"),
                    
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
                    
                    uiOutput("cloud_watch_panel_test2"),
                    
                    helpText(
                      tags$b(
                        "Running this test before the full simulation is ",
                        "strongly recommended."
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
                          
                          shinyFiles::shinyDirButton(
                            "browse_dir_run",
                            "Choose output folder",
                            "Select local output and cloud-download folder",
                            class = "btn-secondary",
                            style = "width:100%;",
                            icon = icon("folder-open")
                          ),
                          
                          uiOutput("run_output_dir_status"),
                          
                          
                          tags$div(
                            style = "display:none;",
                            textInput(
                              "out_dir",
                              label = NULL,
                              value = "")
                          ),
                          helpText(
                            "For a local run, this is where simulation outputs are saved. ",
                            "For a cloud run, this is where the downloaded archive is extracted. ",
                            "Please click 'Browse' to select a folder, or enter a path manually."
                          ),
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
                              "The simulation runs on your rented machine, so the local run mode ",
                              "below does not apply. Results are first stored in Cloud Storage. ",
                              "When you choose Download and prepare results, they are downloaded ",
                              "and extracted into the folder selected above."
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
                          
                          
                          uiOutput("out_dir_required_note"),
                          shiny::actionButton("start_batch", "Start Simulation Run", class = "btn-success btn-lg", width = "100%", icon=icon("rocket")),
                          br(), br(),
                          shiny::actionButton("stop_batch", "Stop Simulation", class = "btn-danger", width = "100%", icon=icon("stop")),
                          uiOutput("cloud_watch_panel_full"),
                          
                          # Cloud controls appear only while cloud mode is on.
                          conditionalPanel(
                            condition = "input.use_cloud == true",
                            hr(),
                            uiOutput("cloud_run_controls")
                          ),
                          
                          hr(),
                          
                          
                          tags$h4("System Log:"),
                          verbatimTextOutput("batch_log"),
                          
                          # Sits under the log because that is where a job
                          # id is read from: the submission message above
                          # prints it, and this is where it is typed back in
                          # to pick the job up again later.
                          #
                          # Its own button, deliberately separate from Check
                          # cloud connection in Step 2. That check answers
                          # "can I reach the project"; this one answers "what
                          # happened to this particular run", and merging
                          # them would make one a precondition of the other.
                          conditionalPanel(
                            condition = "input.use_cloud == true",
                            hr(),
                            tags$h5("Check a previous cloud job"),
                            selectInput(
                              "cloud_job_type_manual",
                              "Cloud job type",
                              choices = c(
                                "Test 1: Model validation" = "validation",
                                "Test 2: Parallel performance check" = "perfcheck",
                                "Full simulation" = "full"
                              ),
                              selected = "full",
                              width = "100%"
                            ),
                            textInput(
                              "cloud_job_id_manual",
                              NULL,
                              value = "",
                              placeholder = "craibm-20260724-1251-1294",
                              width = "100%"
                            ),
                            actionButton(
                              "track_cloud_job_id",
                              "Check this job",
                              class = "btn-outline-primary",
                              width = "100%",
                              icon = icon("magnifying-glass")
                            ),
                          )
                        )
          ),
          
          
          shiny::column(width = 6,
                        box(
                          title = "Task Distribution Preview",
                          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                          verbatimTextOutput("task_preview"),
                          uiOutput("task_preview_warning"),
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
                                        textInput("res_out_dir", label = NULL, value = "",  width="100%"),
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
                                    numericInput("res_burn_in", label = NULL, value = NA, min = 0)
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
  .craibm_runs$session_count <-
    .craibm_runs$session_count + 1L
  
  resume_same_app_process <-
    .craibm_runs$session_count > 1L
  cpp_abs_path <- NULL
  
  old_warn <- getOption("warn")
  options(warn = -1)
  # A browser tab that has been idle for a long time, or a laptop that went
  # to sleep, loses its websocket. Without this the page simply greys out and
  # everything typed into it is gone. With it the browser reattaches to the
  # session it already had, if the server still holds it. "force" is what
  # enables this outside Shiny Server, which is how this app is normally run.
  #
  # It is a cushion, not a guarantee: after a long disconnect the session may
  # be gone and the page will reload empty. Saving settings before stepping
  # away remains the reliable protection.
  session$allowReconnect("force")
  
  
  session$onSessionEnded(function() {
    # Credentials do not outlive the session that supplied them.
    try({
      kp <- isolate(vals$gcp_key_path)
      if (!is.null(kp) && file.exists(kp)) unlink(kp)
    }, silent = TRUE)
    
    options(warn = old_warn)
    
    # Final silent snapshot before this session is discarded.
    try(
      .save_work_snapshot(),
      silent = TRUE
    )
    
    try(
      .cloud_stop_clock(),
      silent = TRUE
    )
  })
  
  # ==========================================================================
  # REAL CLICKS ONLY
  #
  # An action button is an integer counter, and on reconnection Shiny re-sends
  # every input value the client holds. If that counter no longer matches what
  # the server remembers -- the DOM counter starts again at zero after a page
  # is restored, while the server still remembers three -- Shiny sees a changed
  # value and runs the button's observer as though it had been pressed.
  #
  # Nobody pressed anything. The symptom was a warning about missing data
  # appearing on its own after a laptop woke, at the one moment when the
  # uploaded file was gone and its cached copy had not yet been written. The
  # same replay would have started a simulation.
  #
  # A press only counts when the counter goes UP. A lower value means the
  # client restarted its count, so it is recorded and ignored, which leaves the
  # next genuine press one higher and working normally.
  # ==========================================================================
  .guarded_action_ids <- c(
    "run_vbgf",
    "use_generated_alk",
    "submit_alk",
    "submit_survival",
    "submit_global",
    "submit_design",
    "confirm_runcontrol",
    "run_test_sim",
    "run_oversub_test",
    "start_batch",
    "stop_batch","run_z",
    "recover_last_session",
    "load_results"
  )
  
  .click_seen <- reactiveValues()
  .click_guard_ready <- reactiveVal(FALSE)
  
  .real_click <- function(id) {
    
    # Initial values sent by the browser are never clicks.
    if (!isTRUE(isolate(.click_guard_ready()))) {
      return(FALSE)
    }
    
    n <- suppressWarnings(
      as.integer(isolate(input[[id]]))
    )
    
    if (length(n) != 1L || is.na(n) || n < 1L) {
      return(FALSE)
    }
    
    last <- isolate(.click_seen[[id]])
    if (is.null(last)) {
      last <- 0L
    }
    
    # Equal = replay; lower = browser restarted its counter.
    if (n <= last) {
      .click_seen[[id]] <- n
      return(FALSE)
    }
    
    .click_seen[[id]] <- n
    TRUE
  }
  
  # Record every button's browser-side starting value before accepting clicks.
  session$onFlushed(function() {
    shiny::isolate({
      
      for (id in .guarded_action_ids) {
        
        n <- suppressWarnings(
          as.integer(input[[id]])
        )
        
        .click_seen[[id]] <- if (
          length(n) == 1L &&
          !is.na(n) &&
          n >= 0L
        ) {
          n
        } else {
          0L
        }
      }
      
      .click_guard_ready(TRUE)
    })
  }, once = TRUE)
  
  # observeEvent normally ignores actionButton value zero.
  # This observer records a browser counter reset, so the next real click works.
  observe({
    
    for (id in .guarded_action_ids) {
      
      n <- suppressWarnings(
        as.integer(input[[id]])
      )
      
      if (
        length(n) == 1L &&
        !is.na(n) &&
        n == 0L
      ) {
        
        previous <- isolate(.click_seen[[id]])
        
        if (!is.null(previous) && previous > 0L) {
          .click_seen[[id]] <- 0L
        }
      }
    }
  })
  
  vals <- reactiveValues(
    theta_clean = NULL,
    growth_data = NULL,
    z_dist = NULL,
    alk_data = NULL,
    alk_display = NULL,
    alk_source = NULL,
    alk_info = NULL,
    growth_fit_note = NULL,
    alk_seed = NULL,
    alk_bin_width = NULL,
    vbgf_seed = NULL,
    z_seed = NULL,
    # THE tables. Not a cache of something else -- these are where uploaded
    # data lives, and the only place anything reads it from. Written by the
    # upload handlers and by .apply_settings; read by everything else.
    loaded_growth_csv = NULL,
    loaded_size_csv = NULL,
    loaded_alk_csv = NULL,
    # File names, kept for the validators and the status lines. An upload box
    # is empty after a reconnection, so its name has to be remembered too.
    growth_csv_name = NULL,
    size_csv_name = NULL,
    alk_csv_name = NULL,
    # Our own copy of the service-account key, and the name to show for it.
    gcp_key_path = NULL,
    gcp_key_name = NULL
  )
  
  confirmed <- reactiveValues(
    survival  = NULL,
    global    = NULL,
    design    = NULL,
    runcontrol = NULL
  )
  
  SURVIVAL_INPUT_IDS <- c(
    "juv_annual_M",
    "use_z_estimation",
    "F_over_Z_ratio",
    "fixed_adult_M"
  )
  
  GLOBAL_INPUT_IDS <- c(
    "transient_years",
    "stable_years",
    "policy_years",
    
    "use_dd_effects",
    "use_dd_survival",
    "surv_a",
    "surv_b",
    "surv_c",
    "surv_d_avg1",
    "surv_d_avg2",
    
    "use_dd_growth_adult",
    "g1_a",
    "g1_b",
    "g1_c",
    "g1_d_avg",
    
    "use_dd_growth_juv",
    "g2_a",
    "g2_b",
    "g2_c",
    "g2_d_avg",
    
    "flag_harvest_curve",
    "harv_L50",
    "harv_pmax",
    "harv_slope",
    "harv_fixed_pmax",
    "month_weights",
    
    "lake_area_ha",
    "initial_pop_size",
    "rec_a",
    "rec_b",
    "use_ricker",
    "spawn_month",
    "recruit_entry_month",
    
    "age_spawn",
    "min_adult_age",
    "z_full",
    "f_age_mode",
    
    "psd_stock",
    "psd_quality",
    "psd_preferred",
    "psd_memorable",
    "psd_trophy"
  )
  
  DESIGN_INPUT_IDS <- c(
    "ESD_vec",
    "pae_vec",
    "rm_vec",
    "comp_breaks",
    "comp_probs",
    "compliance_mode"
  )
  
  RUNCONTROL_INPUT_IDS <- c(
    "n_iter",
    "seed",
    "n_cores",
    "use_gpu",
    "gpu_thread_count",
    "simulation_engine",
    "omp_nthreads",
    "fast_forward_mode",
    "use_cloud",
    "gcp_project",
    "gcp_region",
    "gcp_bucket",
    "gcp_machine_type",
    "gcp_container_image"
  )
  
  .capture_inputs <- function(ids) {
    
    out <- lapply(
      ids,
      function(id) isolate(input[[id]])
    )
    
    names(out) <- ids
    
    out
  }
  
  .confirmed_value <- function(group, id) {
    
    if (!is.null(group) &&
        !is.null(group$values) &&
        !is.null(group$values[[id]])) {
      return(group$values[[id]])
    }
    
    isolate(input[[id]])
  }
  
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
    cloud_perf_probe     = NA_integer_,
    # A cloud test has finished on the machine but its small result file has
    # not been fetched yet. The user pulls it with View Result. prog is kept
    # so the report can quote the container's own timings.
    cloud_result_ready   = FALSE,
    cloud_result_prog    = NULL,
    
    
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
          strong("Calculated safe duration is not available yet."),
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
          strong("Calculated safe duration is not available yet."),
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
      
      strong("Calculated safe group-tracking duration: "),
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
        
        tags$b("Balanced check"),
        tags$br(),
        
        "This option selects a CPU-balanced worker group based on the machine's ",
        "available CPU capacity and the number of threads used by each worker. ",
        "It measures real memory use and estimates whether the complete parallel ",
        "plan is suitable for the selected machine. It is the recommended first check."
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
    
    test2_loaded_from_settings = FALSE,
    
    restoring_settings = FALSE,
    
    loaded_from = NULL,
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
    
    # Growth must be submitted successfully and must carry the fitted
    # parameter table that is actually sent to the model.
    if (
      !isTRUE(sys_status$vbgf_ok) ||
      is.null(vals$theta_clean)
    ) {
      missing_steps <- c(
        missing_steps,
        "Growth (VBGF)"
      )
    }
    
    # The model uses vals$alk_data, not the appearance of the upload box.
    if (
      !isTRUE(sys_status$alk_ok) ||
      is.null(vals$alk_data)
    ) {
      missing_steps <- c(
        missing_steps,
        "ALK Data"
      )
    }
    
    
    
    if (
      !isTRUE(sys_status$global_ok) ||
      is.null(confirmed$global)
    ) {
      missing_steps <- c(
        missing_steps,
        "Global Parameters"
      )
    }
    
    if (
      !isTRUE(sys_status$design_ok) ||
      is.null(confirmed$design)
    ) {
      missing_steps <- c(
        missing_steps,
        "Design Scenarios"
      )
    }
    
    if (
      !isTRUE(sys_status$runcontrol_ok) ||
      is.null(confirmed$runcontrol)
    ) {
      missing_steps <- c(
        missing_steps,
        "Confirm Run Control (Step 2)"
      )
    }
    
    # Test 2 remains a recommendation and is reported separately in
    # Task Distribution Preview.
    
    missing_steps
  }
  
  output$settings_load_log <- renderUI({
    
    # The panel tracks setup progress whether the data was entered here or
    # restored from a settings file, so it is only hidden before anything at
    # all has been supplied.
    missing_steps <- get_missing_setup_steps()
    
    
    
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
      # Everything required is ready.
      #
      # Test 2 is deliberately not judged here. Reporting it in this panel
      # as well as in the task preview was duplicated warning text; the
      # Step 3b task preview is now the only place that mentions it.
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
      if (isTRUE(sys_status$restoring_settings)) {
        return()
      }
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
        "Please confirm Run Control again before testing or starting the full simulation."
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
  # The stored table, nothing else. No branch on the upload box, so a
  # reconnection that empties the box changes nothing here.
  growth_upload <- reactive({
    df <- vals$loaded_growth_csv
    if (is.null(df)) return(NULL)
    as.data.frame(df)
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
        "fish that do have one."
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
    
    preview_data <- if (!is.null(vals$alk_display)) {
      vals$alk_display
    } else {
      vals$alk_data
    }
    
    if (is.null(preview_data)) {
      return(
        div(
          class = "alert alert-info",
          icon("info-circle"),
          "No ALK data to display yet. Please use the left panel to either generate an ALK from the Length-Age data or upload your own custom ALK file."
        )
      )
    }
    
    src_line <- switch(
      vals$alk_source,
      
      "generated_complete" = div(
        class = "alert alert-success",
        style = "padding: 10px; margin-bottom: 8px;",
        
        icon("check-circle"),
        tags$b(" Generated directly from the uploaded length-age data."),
        
        tags$div(
          style = "margin-top: 4px;",
          vals$alk_info
        )
      ),
      
      "generated_imputed" = div(
        class = "alert alert-success",
        style = "padding: 10px; margin-bottom: 8px;",
        
        icon("wand-magic-sparkles"),
        tags$b(" Generated after estimating the missing ages."),
        
        tags$div(
          style = "margin-top: 4px;",
          paste0(
            "Missing ages were estimated using an empirical age-length key ",
            "constructed from the observed age-length records. ",
            vals$alk_info
          )
        )
      ),
      
      "file" = div(
        class = "alert alert-info",
        style = "padding: 10px; margin-bottom: 8px;",
        
        icon("upload"),
        tags$b(" Using the ALK uploaded by the user."),
        
        tags$div(
          style = "margin-top: 4px;",
          vals$alk_info
        )
      ),
      
      NULL
    )
    
    singleton_note <- NULL
    
    if (
      vals$alk_source %in% c(
        "generated_complete",
        "generated_imputed"
      ) &&
      any(!is.finite(
        suppressWarnings(
          as.numeric(preview_data$Lengthsd)
        )
      ))
    ) {
      
      singleton_note <- div(
        class = "alert alert-warning",
        style = "padding: 10px; margin-bottom: 8px;",
        
        tags$b("Note: "),
        "blank in Lengthsd means that only one fish was available for that age. ",
        "For Z estimation and simulation, the nearest available standard ",
        "deviation is used, with the previous age preferred. ",
        "You may upload your own ALK if you prefer different values."
      )
    }
    
    # The table itself is NOT emitted here.
    #
    # A DT output nested inside renderUI is re-created every time the
    # surrounding block re-runs. Each re-creation binds a fresh DataTables
    # instance under the same output id, the previous instance's Ajax
    # endpoint disappears, and the browser raises "DataTables_Table_N -
    # Ajax error" while the new table draws its header with no rows. It only
    # showed up after restoring a session, because that changes alk_source
    # and alk_info together and so re-runs this block twice in quick
    # succession.
    #
    # The output now lives in the static UI and is only written to by its own
    # render function, which is the arrangement DT expects.
    tagList(
      src_line,
      singleton_note
    )
  })
  
  
  # server = FALSE: the rows travel with the page instead of being fetched
  # from a per-session Ajax endpoint. That endpoint dies with its session,
  # so after a laptop sleeps or a socket drops the browser was left asking
  # a dead address for its data and reporting "Ajax error" with nothing but
  # a header drawn. These tables are small enough that sending them whole
  # costs nothing and removes the dependency entirely.
  output$alk_table_preview <- DT::renderDT({
    
    preview_data <- if (!is.null(vals$alk_display)) {
      vals$alk_display
    } else {
      vals$alk_data
    }
    
    req(preview_data)
    
    DT::datatable(
      preview_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",
        scrollX = TRUE
      )
    )
  }, server = FALSE)
  
  # Download button for an automatically generated key.
  output$auto_alk_note <- renderUI({
    
    # User-uploaded ALK is active.
    if (
      identical(vals$alk_source, "file") &&
      !is.null(vals$alk_data)
    ) {
      return(
        div(
          class = "alert alert-info",
          style = "padding: 8px; margin-top: 10px; margin-bottom: 6px;",
          
          icon("check-circle"),
          tags$b(" The uploaded ALK is currently being used.")
        )
      )
    }
    
    
    # Automatically generated and model-ready ALK.
    if (
      vals$alk_source %in% c(
        "generated_complete",
        "generated_imputed"
      ) &&
      !is.null(vals$alk_data)
    ) {
      return(
        tagList(
          div(
            class = "alert alert-success",
            style = "padding: 8px; margin-top: 10px; margin-bottom: 6px;",
            
            icon("check-circle"),
            tags$b(" The automatically generated ALK is currently being used.")
          ),
          
          downloadButton(
            "download_auto_alk",
            "Download Model-Ready ALK (.csv)",
            class = "btn-success",
            style = "width:100%;"
          )
        )
      )
    }
    
    NULL
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
  
  # Missing source data are user-action messages, not system-status messages.
  # Keep these two notices at the lower left without moving every notification
  # in the application.
  alk_notice_serial <- 0L
  
  show_alk_source_notice <- function(message) {
    alk_notice_serial <<- alk_notice_serial + 1L
    notice_id <- paste0(
      "craibm-alk-source-notice-",
      alk_notice_serial
    )
    selector <- paste0("#", notice_id)
    
    shiny::removeUI(
      selector = "[id^='craibm-alk-source-notice-']",
      immediate = TRUE
    )
    
    shiny::insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = tags$div(
        id = notice_id,
        style = paste0(
          "position: fixed;",
          "left: 20px;",
          "bottom: 20px;",
          "z-index: 99999;",
          "max-width: 360px;",
          "padding: 14px 40px 14px 16px;",
          "border: 1px solid #ffe69c;",
          "border-radius: 4px;",
          "background: #fff3cd;",
          "color: #856404;",
          "box-shadow: 0 4px 14px rgba(0,0,0,0.18);"
        ),
        icon("exclamation-triangle"),
        tags$span(style = "margin-left: 7px;", message),
        tags$button(
          type = "button",
          "\u00d7",
          onclick = paste0(
            "var n=document.getElementById('", notice_id,
            "');if(n){n.remove();}"
          ),
          style = paste0(
            "position:absolute;",
            "top:6px;",
            "right:8px;",
            "border:0;",
            "background:transparent;",
            "font-size:20px;",
            "color:#856404;"
          )
        )
      ),
      immediate = TRUE
    )
    
    later::later(
      function() {
        try(
          shiny::removeUI(selector = selector, immediate = TRUE),
          silent = TRUE
        )
      },
      delay = 5
    )
  }
  
  observeEvent(input$run_vbgf, {
    if (!.real_click("run_vbgf")) return()
    
    df <- growth_upload()
    
    growth_file_obj <- if (!is.null(df)) {
      list(
        name = if (!is.null(vals$growth_csv_name)) {
          vals$growth_csv_name
        } else {
          "length-age.csv"
        }
      )
    } else {
      NULL
    }
    
    
    if (is.null(growth_file_obj) || is.null(df)) {
      sys_status$msg_vbgf <-
        "1. [❌] Growth (VBGF)   : No growth data are available."
      
      showNotification(
        "Please upload a Length-Age CSV file first.",
        type = "warning"
      )
      
      return()
    }
    vals$theta_clean <- NULL
    vals$growth_data <- NULL
    vals$vbgf_seed <- NULL
    sys_status$vbgf_ok <- FALSE
    
    
    sys_status$msg_vbgf <- "1. [ ] Growth (VBGF)   : ⏳ Checking & Running..."
    updateTabsetPanel(session, "tab_diag", selected = "Welcome")
    
    runtime_logs <- character(0)
    
    tryCatch({
      
      chk_in <- check_vbgf_inputs(
        growth_file_obj,
        df,
        input$boot_b_vbgf
      )
      
      if(!chk_in$pass) {
        sys_status$msg_vbgf <- paste0("1. [❌] Growth (VBGF)   : Input Error.\n      ", gsub("\n", " ", chk_in$msg))
        return()
      }
      
      n_missing <- sum(
        is.na(
          suppressWarnings(
            as.numeric(df$Age)
          )
        )
      )
      
      age_note <- ""
      
      # This local copy is used only when estimated ages are included in the
      # VBGF fit. Confirming the model ALK is a separate user action.
      completed_data <- df
      
      # growth_data_for_fit determines whether estimated ages enter the VBGF fit.
      growth_data_for_fit <- df
      
      if (n_missing > 0L) {
        
        alk_seed_in <- manual_seed(input$alk_seed_manual)
        imp <- impute_ages_alk(df, seed = alk_seed_in)
        
        completed_data <- imp$data
        
        use_estimated <- identical(
          input$missing_age_mode,
          "all"
        )
        
        if (use_estimated) {
          growth_data_for_fit <- completed_data
        }
        
        age_note <- paste0(
          "\n      Ages estimated for ", imp$n_imputed, " fish",
          " (key from ", imp$n_aged, " aged fish, ",
          "length classes of ", imp$bin_width, ").",
          "\n      Age-assignment seed: ", imp$seed,
          if (!is.null(alk_seed_in)) " (set manually)." else ".",
          
          if (isTRUE(imp$n_filled > 0L)) {
            paste0(
              "\n      ",
              imp$n_filled,
              " length class(es) had no aged fish and borrowed the nearest class."
            )
          } else {
            ""
          },
          
          if (imp$n_dropped > 0L) {
            paste0(
              "\n      ",
              imp$n_dropped,
              " fish were smaller than the key and were set aside."
            )
          } else {
            ""
          },
          
          "\n      Growth curve fitted to: ",
          
          if (use_estimated) {
            "observed and estimated ages."
          } else {
            "observed ages only."
          }
        )
        
      } else {
        
        # Complete ages: no age assignment is required.
        age_note <- paste0(
          "\n      All fish had observed ages.",
          "\n      Growth curve fitted to observed ages."
        )
      }
      # The VBGF fit still follows the user's observed/estimated-age choice.
      df <- growth_data_for_fit
      
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
  
  observeEvent(input$use_generated_alk, {
    if (!.real_click("use_generated_alk")) return()
    
    df <- growth_upload()
    
    if (is.null(df)) {
      show_alk_source_notice(
        "Please upload a Length-Age CSV file first."
      )
      return()
    }
    
    tryCatch({
      
      if (nrow(df) == 0L) {
        stop("The Length-Age file is empty.")
      }
      
      required_columns <- c("Age", "Length")
      
      if (
        ncol(df) != 2L ||
        !all(required_columns %in% names(df))
      ) {
        stop(
          paste0(
            "The Length-Age file must contain exactly the columns ",
            "'Age' and 'Length'."
          )
        )
      }
      
      if (!is.numeric(df$Age) || !is.numeric(df$Length)) {
        stop("'Age' and 'Length' must both be numeric.")
      }
      
      n_missing <- sum(
        is.na(
          suppressWarnings(
            as.numeric(df$Age)
          )
        )
      )
      
      completed_data <- df
      selected_source <- "generated_complete"
      selected_info <- NULL
      selected_seed <- NULL
      selected_bin_width <- NULL
      seed_was_manual <- FALSE
      
      if (n_missing > 0L) {
        
        alk_seed_in <- manual_seed(input$alk_seed_manual)
        seed_was_manual <- !is.null(alk_seed_in)
        imp <- impute_ages_alk(df, seed = alk_seed_in)
        
        completed_data <- imp$data
        selected_source <- "generated_imputed"
        selected_seed <- imp$seed
        selected_bin_width <- imp$bin_width
        
        selected_info <- paste0(
          "Built from ", nrow(completed_data), " fish (",
          imp$n_aged, " observed ages and ",
          imp$n_imputed, " estimated ages) across ",
          length(unique(completed_data$Age)), " age classes."
        )
        
      } else {
        
        selected_info <- paste0(
          "Summarized from ",
          nrow(completed_data),
          " fish across ",
          length(unique(completed_data$Age)),
          " age classes."
        )
      }
      
      # Compute everything before replacing the active ALK. If any step
      # fails, the previously confirmed source remains untouched.
      selected_display <- build_alk_summary(completed_data)
      selected_model <- fill_alk_sd_for_model(selected_display)
      
      vals$alk_data <- selected_model
      vals$alk_display <- selected_display
      vals$alk_source <- selected_source
      vals$alk_info <- selected_info
      vals$alk_seed <- selected_seed
      vals$alk_bin_width <- selected_bin_width
      
      sys_status$alk_ok <- TRUE
      
      sys_status$msg_alk <- if (
        identical(selected_source, "generated_imputed")
      ) {
        paste0(
          "2. [✅] ALK Data        : Ready! Generated after estimating missing ages.",
          "\n      Age-assignment seed: ",
          selected_seed,
          if (isTRUE(seed_was_manual)) " (set manually)" else ""
        )
      } else {
        "2. [✅] ALK Data        : Ready! Generated directly from complete age-length data."
      }
      
      updateTabsetPanel(
        session,
        "tab_diag",
        selected = "Age-Length Key Data"
      )
      
    }, error = function(e) {
      showNotification(
        paste0(
          "The automatic ALK could not be generated: ",
          e$message,
          " The previously selected ALK remains active."
        ),
        type = "error",
        duration = 12
      )
    })
  })
  
  observeEvent(input$submit_alk, {
    if (!.real_click("submit_alk")) return()
    
    # The upload candidate is kept separately from the active model ALK.
    # A failed validation therefore cannot erase whichever valid ALK the
    # user had selected previously.
    if (is.null(vals$loaded_alk_csv)) {
      show_alk_source_notice(
        "Please upload an ALK CSV file first."
      )
      return()
    }
    
    tryCatch({
      
      df <- as.data.frame(vals$loaded_alk_csv)
      
      alk_file_obj <- list(
        name = if (!is.null(vals$alk_csv_name)) {
          vals$alk_csv_name
        } else {
          "alk.csv"
        }
      )
      
      chk <- check_alk_inputs(
        alk_file_obj,
        df
      )
      
      if (!chk$pass) {
        showNotification(
          paste0(
            "ALK Data Validation Failed. ",
            "The previously selected ALK remains active."
          ),
          type = "error"
        )
        return()
      }
      
      vals$alk_data <- df
      vals$alk_display <- df
      vals$alk_source <- "file"
      vals$alk_info <- NULL
      vals$alk_seed <- NULL
      vals$alk_bin_width <- NULL
      
      sys_status$alk_ok <- TRUE
      sys_status$msg_alk <-
        "2. [✅] ALK Data        : Ready! Using the uploaded ALK."
      
      showNotification(
        "The uploaded ALK is now being used.",
        type = "message"
      )
      
      updateTabsetPanel(
        session,
        "tab_diag",
        selected = "Age-Length Key Data"
      )
      
    }, error = function(e) {
      showNotification(
        paste0(
          "The uploaded ALK could not be checked: ",
          e$message,
          " The previously selected ALK remains active."
        ),
        type = "error"
      )
    })
  })
  
  
  # Logic 3: Mortality (Z)
  
  output$z_status_display <- renderText({
    sys_status$msg_z
  })
  
  
  observeEvent(input$run_z, {
    if (!.real_click("run_z")) return()
    vals$z_dist <- NULL
    vals$z_seed <- NULL
    sys_status$z_ok <- FALSE
    
    sys_status$msg_z <- "[ ] Mortality (Z)   : ⏳ Estimating..."
    
    
    runtime_logs_z <- character(0)
    
    tryCatch({
      if(is.null(vals$alk_data)) {
        sys_status$msg_z <- "[❌] Mortality (Z)   : Missing ALK data!Please go back to Step 1a and determine ALK data first!"
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
        z_numeric <- suppressWarnings(
          as.numeric(z_res)
        )
        
        clean_z <- z_numeric[
          is.finite(z_numeric)
        ]
        
        if (length(clean_z) > 0L) {
          
          # Only valid bootstrap estimates are passed to the general checker.
          chk_out <- check_boot_outcomes(
            data.frame(Z = clean_z),
            input$z_boot_bg2
          )
          
          if (!chk_out$pass) {
            sys_status$msg_z <- paste0(
              "[❌] Mortality (Z)   : Calculation Error.\n      ",
              gsub("\n", " ", chk_out$msg),
              warn_msg_block
            )
            return()
          }
          
          n_failed <- length(z_numeric) - length(clean_z)
          
          failed_note <- if (n_failed > 0L) {
            paste0(
              "\n      ⚠️ ",
              n_failed,
              " bootstrap run(s) did not produce a finite Z estimate and were excluded."
            )
          } else {
            ""
          }
          
          vals$z_dist <- clean_z
          vals$z_seed <- z_seed_used
          
          sys_status$z_ok <- TRUE
          
          sys_status$msg_z <- paste0(
            "[✅] Mortality (Z)   : Ready! (",
            length(vals$z_dist),
            " valid runs)",
            "\n      Bootstrap seed: ",
            z_seed_used,
            if (!is.null(z_seed_in)) " (set manually)" else "",
            failed_note,
            warn_msg_block
          )
          
        } else {
          sys_status$msg_z <- paste0(
            "[❌] Mortality (Z)   : Failed because no valid bootstrap estimates were produced.",
            warn_msg_block
          )
        }
        
        
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
    if (!.real_click("submit_survival")) return()
    
    
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
    
    
    # Held in sys_status rather than assigned to the output directly:
    # an output created inside an observer cannot be saved, restored, or
    # read anywhere else, which is why this page came back blank after a
    # settings file was reloaded.
    sys_status$log_surv <- paste(logs, collapse = "")
    if (isTRUE(all_pass)) {
      
      confirmed$survival <- list(
        values = .capture_inputs(SURVIVAL_INPUT_IDS),
        z_dist = vals$z_dist,
        validation_log = sys_status$log_surv,
        submitted_at = Sys.time()
      )
    }
  })
  
  
  
  # Logic 4: Global Parameters (Submit & Check)
  
  
  sys_status$log_1_2 <- "⚪ Waiting for submission..."
  output$log_step1_2 <- renderText({ sys_status$log_1_2 })
  
  # ==========================================================================
  # TARGET MACHINE
  #
  # Every capacity judgement -- how many workers fit, whether the thread
  # budget is oversubscribed, how large a benchmark to run -- must be made
  # against the machine that will actually execute the work. In cloud mode
  # that is the rented Batch machine, which has nothing to do with the
  # computer running this app. Reading detectCores() there describes the
  # wrong machine and silently sizes the run to the wrong hardware.
  # ==========================================================================
  
  # Logical CPUs of the target machine, or NA when it cannot be established
  # (cloud mode with a machine type this app does not recognise). NA means
  # "do not clamp": guessing from the local machine is worse than not
  # clamping, because the container runs its own memory pre-check anyway.
  .target_logical_cores <- function() {
    if (isTRUE(input$use_cloud)) {
      n <- suppressWarnings(as.integer(
        parse_machine_type_cores(input$gcp_machine_type)
      ))
      if (length(n) == 1L && !is.na(n) && n >= 1L) return(n)
      return(NA_integer_)
    }
    
    n <- suppressWarnings(parallel::detectCores(logical = TRUE))
    if (length(n) != 1L || is.na(n) || n < 1L) return(NA_integer_)
    as.integer(n)
  }
  
  # Wording for messages, so the user is never told about cores belonging to
  # a machine other than the one their run will use.
  .target_machine_label <- function() {
    if (isTRUE(input$use_cloud)) {
      mt <- input$gcp_machine_type
      if (is.null(mt) || !nzchar(mt)) mt <- "unspecified type"
      paste0("the Google Cloud machine (", mt, ")")
    } else {
      "this computer"
    }
  }
  
  # Calculate the juvenile fast-forward boundary from either the current
  # inputs or values restored from an older settings file. Keeping this in one
  # helper ensures that Submit Global and old-RDS recovery use the same model
  # definition.
  .calculate_t_safe_info <- function(saved_inputs = NULL) {
    value_for <- function(id) {
      if (!is.null(saved_inputs) && !is.null(saved_inputs[[id]])) {
        return(saved_inputs[[id]])
      }
      isolate(input[[id]])
    }
    
    tryCatch({
      use_juvenile_dd <- isTRUE(value_for("use_dd_effects")) &&
        isTRUE(value_for("use_dd_growth_juv"))
      
      safe_age_bound <- min(
        as.numeric(value_for("min_adult_age")),
        as.numeric(value_for("age_spawn"))
      )
      if (identical(value_for("f_age_mode"), "age")) {
        safe_age_bound <- min(
          safe_age_bound,
          as.numeric(value_for("z_full"))
        )
      }
      
      compute_T_safe(
        theta_clean   = vals$theta_clean,
        juv_onlyM_len = as.numeric(value_for("psd_stock")),
        min_adult_age = safe_age_bound,
        age_recruit   = 0.0,
        g1_a          = as.numeric(value_for("g2_a")),
        g1_b          = as.numeric(value_for("g2_b")),
        g1_c          = as.numeric(value_for("g2_c")),
        g1_d_avg      = as.numeric(value_for("g2_d_avg")),
        use_dd_growth = use_juvenile_dd
      )
    }, error = function(e) {
      list(
        T_safe = 0L,
        T_length = 0L,
        T_age = 0L,
        limiting_factor = "error",
        error_msg = conditionMessage(e)
      )
    })
  }
  
  observeEvent(input$submit_global, {
    if (!.real_click("submit_global")) return()
    
    sys_status$global_ok <- FALSE
    sys_status$log_1_2 <- "⏳ Checking parameters..."
    
    # Worker count is NOT judged or altered here. Submit Global validates the
    # biological and run parameters only. The number of replicate workers a
    # machine can take depends on the machine that will run the work, and that
    # machine is only settled at Confirm Run Control (Step 2). Rewriting
    # input$n_cores here silently changed the value that Run Control, Test 2
    # and the full model all read downstream -- one clamp against the local
    # computer leaked into every later stage, including cloud runs. Capacity
    # now lives solely in Confirm Run Control.
    
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
      # Master switch
      use_dd_effects       = input$use_dd_effects,
      
      # Density-dependent survival
      use_dd_survival      = input$use_dd_survival,
      surv_a               = input$surv_a,
      surv_b               = input$surv_b,
      surv_c               = input$surv_c,
      surv_d1              = input$surv_d_avg1,
      surv_d2              = input$surv_d_avg2,
      
      # Density-dependent adult growth
      use_dd_growth_adult  = input$use_dd_growth_adult,
      g1_a                 = input$g1_a,
      g1_b                 = input$g1_b,
      g1_c                 = input$g1_c,
      g1_d                 = input$g1_d_avg,
      
      # Density-dependent juvenile growth
      use_dd_growth_juv    = input$use_dd_growth_juv,
      g2_a                 = input$g2_a,
      g2_b                 = input$g2_b,
      g2_c                 = input$g2_c,
      g2_d                 = input$g2_d_avg,
      
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
    
    # Display the global-parameter validation result.
    sys_status$log_1_2 <- chk$msg
    
    if (chk$pass) {
      
      vals$T_safe_info <- .calculate_t_safe_info()
      
      confirmed$global <- list(
        values = .capture_inputs(GLOBAL_INPUT_IDS),
        params = params_list,
        T_safe_info = vals$T_safe_info,
        validation_log = chk$msg,
        submitted_at = Sys.time()
      )
      
      sys_status$global_ok <- TRUE
      
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
    if (!.real_click("submit_design")) return()
    
    sys_status$design_ok <- FALSE
    sys_status$log_1_3 <- "⏳ Verifying design inputs..."
    
    # 1. CSV ( tryCatch)
    df <- NULL
    
    if (!is.null(vals$loaded_size_csv)) {
      df <- as.data.frame(vals$loaded_size_csv)
    }
    
    # 2. Validation
    # The validator uses file_obj only for its name in messages. A restored
    # table has no upload box and must not be reported as a missing file.
    size_file_obj <- if (!is.null(df)) {
      list(
        name = if (!is.null(vals$size_csv_name)) {
          vals$size_csv_name
        } else {
          "size-limits.csv"
        }
      )
    } else {
      NULL
    }
    
    chk <- check_design_inputs(
      file_obj   = size_file_obj,
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
      
      design_csv_data(df)
      vals$loaded_size_csv <- df
      
      confirmed$design <- list(
        values = .capture_inputs(DESIGN_INPUT_IDS),
        size_csv = as.data.frame(df),
        validation_log = chk$msg,
        submitted_at = Sys.time()
      )
      
      sys_status$design_ok <- TRUE
      
      showNotification(
        "Design Verified! Jumping to Preview...",
        type = "message",
        duration = 2
      )
      
      # Step 1: Design preview"
      # tabName "combos"
      # ： UI dashboardSidebar(id = "sidebarMenu", ...)
      updateTabItems(session, "sidebarMenu", selected = "combos")
      
    } else {
      
      showNotification("Design Validation Failed!", type = "error")
    }
  })
  
  
  output$log_survival <- renderText({ sys_status$log_surv })
  
  # ===== Step 2: Run control confirm + check =====
  output$log_runcontrol <- renderText({ sys_status$log_runcontrol })
  
  observeEvent(input$confirm_runcontrol, {
    if (!.real_click("confirm_runcontrol")) return()
    
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
    
    # NA is passed through deliberately: check_runcontrol_inputs() already
    # has a branch for a cloud machine whose capacity is unknown, and it must
    # not be handed this computer's core count as a stand-in.
    logical_cores <- .target_logical_cores()
    if (is.na(logical_cores) && !isTRUE(input$use_cloud)) logical_cores <- 4L
    
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
        "   1. Replicate parallelism: ", eff_workers2, " active R worker process(es)\n",
        "   2. Policy parallelism: ",
        if (rc_policy > 1L) paste0("ON, ", rc_policy, " thread(s) per replicate worker")
        else "OFF", "\n",
        "   3. Individual parallelism: ",
        if (rc_omp > 1L) paste0("ON, ", rc_omp, " OpenMP thread(s) per policy calculation")
        else "OFF", "\n",
        "🧮 Maximum software CPU work slots: ", eff_workers2, " x ", rc_policy, " x ",
        rc_omp, " = ", peak2,
        " (R processes x policy threads x individual-level threads)\n",
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
    
    # Google Cloud is part of the run plan, so its connection has to hold
    # for this confirmation to mean anything. Passing Run Control while the
    # bucket, project or credentials were wrong sent the user all the way to
    # Step 3 before anything complained.
    cloud_mode <- isTRUE(input$use_cloud)
    cloud_ok <- !cloud_mode || isTRUE(proc_state$cloud_verified)
    
    if (!cloud_ok) {
      chk$msg <- paste0(
        "🛑 Google Cloud has not been verified.\n",
        "Cloud execution is switched on, so the connection must be checked ",
        "before Run Control can be confirmed. Use Check cloud connection ",
        "above, then confirm again.\n",
        "----------------------------------------\n",
        chk$msg
      )
    }
    
    sys_status$log_runcontrol <- chk$msg
    if (isTRUE(chk$pass) && isTRUE(cloud_ok)) {
      
      confirmed$runcontrol <- list(
        values = .capture_inputs(RUNCONTROL_INPUT_IDS),
        checked_inputs = rc_inputs,
        validation_log = chk$msg,
        cloud_verified = isTRUE(proc_state$cloud_verified),
        submitted_at = Sys.time()
      )
      
      sys_status$runcontrol_ok <- TRUE
      
      showNotification(
        "Run Control Verified!",
        type = "message"
      )
    } else {
      sys_status$runcontrol_ok <- FALSE
      showNotification(
        if (!cloud_ok) {
          "Run Control failed: Google Cloud has not been verified."
        } else {
          "Run Control Validation Failed!"
        },
        type = "error"
      )
    }
  })
  
  # A verified connection only describes the settings it was run against.
  # Changing any of them, or switching cloud execution on or off, retires
  # both that verification and the Run Control confirmation built on it.
  observeEvent(
    list(
      input$use_cloud,
      input$gcp_key,
      input$gcp_project,
      input$gcp_region,
      input$gcp_bucket,
      input$gcp_machine_type,
      input$gcp_container_image
    ),
    { if (isTRUE(sys_status$restoring_settings)) {
      return()
    }
      # A reconnection empties the key box without the user touching
      # anything, and that fired this observer -- silently discarding a
      # verified connection and the Run Control confirmation resting on it.
      # An empty box while a copy of the key is held is not a change.
      if (is.null(input$gcp_key) &&
          !is.null(vals$gcp_key_path) &&
          file.exists(vals$gcp_key_path)) {
        return()
      }
      
      proc_state$cloud_verified <- FALSE
      sys_status$runcontrol_ok <- FALSE
      sys_status$log_runcontrol <- paste0(
        "⚠️ Cloud settings changed.\n",
        "Please run Check cloud connection, then confirm Run Control again."
      )
    },
    ignoreInit = TRUE
  )
  get_size_limits <- reactive({
    
    df <- if (!is.null(confirmed$design) &&
              !is.null(confirmed$design$size_csv)) {
      
      as.data.frame(confirmed$design$size_csv)
      
    } else if (!is.null(vals$loaded_size_csv)) {
      
      as.data.frame(vals$loaded_size_csv)
      
    } else {
      
      NULL
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
    
    design_commit <- confirmed$design
    req(design_commit)
    
    ESD_vec <- parse_num_vec(
      .confirmed_value(design_commit, "ESD_vec")
    )
    
    PAE_vec <- parse_num_vec(
      .confirmed_value(design_commit, "pae_vec")
    )
    
    RM_vec <- parse_num_vec(
      .confirmed_value(design_commit, "rm_vec")
    )
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
    design_commit <- confirmed$design
    req(design_commit)
    
    comp_input <- .confirmed_value(
      design_commit,
      "compliance_mode"
    )
    
    rm_input_vec <- parse_num_vec(
      .confirmed_value(design_commit, "rm_vec")
    )
    
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
    
    design_commit <- confirmed$design
    req(design_commit)
    
    c_breaks <- parse_num_vec(
      .confirmed_value(design_commit, "comp_breaks")
    )
    
    c_probs <- parse_num_vec(
      .confirmed_value(design_commit, "comp_probs")
    )
    validate(need(length(c_breaks) > 0, "Breaks empty"),
             need(length(c_breaks) == length(c_probs), "Breaks/Probs length mismatch"))
    data.frame(Threshold_mm = c_breaks, Probability = c_probs)
  })
  
  # --- Design Preview Tables ---
  output$combos_dynamic_ui <- renderUI({
    if (!isTRUE(sys_status$design_ok)) {
      return(
        tags$div(
          style = "padding: 30px; text-align: center; color: #6c757d;",
          icon("clipboard-list", "fa-3x"),
          tags$h4(style = "margin-top: 15px;", "This page updates automatically based on your inputs from the previous page (Step 1c: Experiment Design).")
        )
      )
    }
    
    tagList(
      uiOutput("design_job_summary"),
      
      tags$h4("1) Size Limits"),
      DT::DTOutput("size_tbl"),
      hr(),
      
      tags$h4("2) Uncertainty Considered (PAE, ESD, RM)"),
      helpText("Note: PAE = Prop of fish for annual angler encounters, ESD = Environment Stochasticity, RM = Release Mortality rate."),
      DT::DTOutput("scen_preview_tbl"),
      helpText("Label names (Output Folder) are the folder names for each uncertainty combination when saving simulation data files."),
      
      hr(),
      
      tags$h4("3) Size Limit Policy Conditions (Compliance and release mortality rate considered?)"),
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
  }) 
  
  
  output$design_job_summary <- renderUI({
    size_df <- get_size_limits()
    scen_df <- get_scenarios_df()
    policy_df <- get_policy_combos_logic()
    
    req(size_df, scen_df, policy_df)
    
    n_size <- nrow(size_df)
    n_scen <- nrow(scen_df)
    n_policy <- nrow(policy_df)
    
    n_uncertainty <- if (n_size > 0) n_scen / n_size else 0
    
    tags$div(
      class = "alert alert-info",
      style = "margin-top:12px; margin-bottom:16px;",
      tags$h5(
        style = "margin-top:0;",
        icon("calculator"),
        strong("Full-model design summary")
      ),
      tags$p(
        style = "margin-bottom:5px; font-size: 14px;",
        tags$b("Management regulations (Size limits): "), n_size, tags$br(),
        tags$b("Uncertainty combinations: "), n_uncertainty, tags$br(),
        tags$b("Total scenarios (Size limit×Uncertainty combinations): "), paste0(n_size, "×", n_uncertainty, " = ", n_scen), tags$br(),
        tags$b("Policy conditions (Compliance & RM execution within each scenario): "), n_policy
      ),
      tags$small(
        "Each scenario folder is run once per iteration. Every such ",
        "scenario  covers all policy conditions. "
      )
    )
  }) 
  
  
  
  # server = FALSE: the rows travel with the page instead of being fetched
  # from a per-session Ajax endpoint. That endpoint dies with its session,
  # so after a laptop sleeps or a socket drops the browser was left asking
  # a dead address for its data and reporting "Ajax error" with nothing but
  # a header drawn. These tables are small enough that sending them whole
  # costs nothing and removes the dependency entirely.
  output$size_tbl <- DT::renderDT({
    req(get_size_limits())
    DT::datatable(get_size_limits(), options = list(pageLength = 5, scrollX = TRUE))
  }, server = FALSE)
  
  # Table 2: Uncertainty (Scenarios / Folders)
  # server = FALSE: the rows travel with the page instead of being fetched
  # from a per-session Ajax endpoint. That endpoint dies with its session,
  # so after a laptop sleeps or a socket drops the browser was left asking
  # a dead address for its data and reporting "Ajax error" with nothing but
  # a header drawn. These tables are small enough that sending them whole
  # costs nothing and removes the dependency entirely.
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
  }, server = FALSE)
  
  # Table 3: Policy Condition (Files)
  # server = FALSE: the rows travel with the page instead of being fetched
  # from a per-session Ajax endpoint. That endpoint dies with its session,
  # so after a laptop sleeps or a socket drops the browser was left asking
  # a dead address for its data and reporting "Ajax error" with nothing but
  # a header drawn. These tables are small enough that sending them whole
  # costs nothing and removes the dependency entirely.
  output$combo_tbl <- DT::renderDT({
    req(get_policy_combos_logic())
    df <- get_policy_combos_logic()
    
    rm_input_vec <- parse_num_vec(.confirmed_value(confirmed$design,"rm_vec"))
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
  }, server = FALSE)
  
  
  # Logic 6: Parameter Packing (Strictly Matched to C++ Source Code)
  get_packed_params <- reactive({
    
    # -----------------------------------------------------------------------
    # 1. Required submitted/validated states
    # -----------------------------------------------------------------------
    req(
      sys_status$vbgf_ok,
      sys_status$alk_ok,
      sys_status$survival_ok,
      sys_status$global_ok,
      sys_status$design_ok,
      sys_status$runcontrol_ok
    )
    
    global_commit     <- confirmed$global
    survival_commit   <- confirmed$survival
    design_commit     <- confirmed$design
    runcontrol_commit <- confirmed$runcontrol
    
    req(
      global_commit,
      survival_commit,
      design_commit,
      runcontrol_commit
    )
    
    # Read only the values saved by the corresponding successful Submit.
    g <- function(id) {
      .confirmed_value(global_commit, id)
    }
    
    s <- function(id) {
      .confirmed_value(survival_commit, id)
    }
    
    rc <- function(id) {
      .confirmed_value(runcontrol_commit, id)
    }
    
    # -----------------------------------------------------------------------
    # 2. Timeline and monthly weights
    # -----------------------------------------------------------------------
    mw <- parse_num_vec(g("month_weights"))
    
    if (length(mw) != 12L) {
      mw <- rep(1, 12)
    }
    
    total_burn_in <-
      as.integer(g("transient_years")) +
      as.integer(g("stable_years"))
    
    c_struct <- get_compliance_struct()
    
    # -----------------------------------------------------------------------
    # 3. Density-dependent settings
    # -----------------------------------------------------------------------
    master_dd <- isTRUE(g("use_dd_effects"))
    
    # A sub-module is active only when both its own switch and the master
    # density-dependence switch were enabled in the submitted configuration.
    use_surv <- master_dd &&
      isTRUE(g("use_dd_survival"))
    
    use_adult_growth <- master_dd &&
      isTRUE(g("use_dd_growth_adult"))
    
    use_juvenile_growth <- master_dd &&
      isTRUE(g("use_dd_growth_juv"))
    
    # -----------------------------------------------------------------------
    # 3a. Density-dependent survival
    #
    # When disabled:
    #   a = 1 and b = 0 make the multiplier equal to 1.
    #   c = 0 and d = 1 prevent hidden NA values from entering C++.
    # -----------------------------------------------------------------------
    val_surv_a <- if (use_surv) {
      as.numeric(g("surv_a"))
    } else {
      1.0
    }
    
    val_surv_b <- if (use_surv) {
      as.numeric(g("surv_b"))
    } else {
      0.0
    }
    
    val_surv_c <- if (use_surv) {
      as.numeric(g("surv_c"))
    } else {
      0.0
    }
    
    val_surv_d1 <- if (use_surv) {
      as.numeric(g("surv_d_avg1"))
    } else {
      1.0
    }
    
    val_surv_d2 <- if (use_surv) {
      as.numeric(g("surv_d_avg2"))
    } else {
      1.0
    }
    
    # -----------------------------------------------------------------------
    # 3b. Density-dependent adult growth
    # -----------------------------------------------------------------------
    val_adult_a <- if (use_adult_growth) {
      as.numeric(g("g1_a"))
    } else {
      1.0
    }
    
    val_adult_b <- if (use_adult_growth) {
      as.numeric(g("g1_b"))
    } else {
      0.0
    }
    
    val_adult_c <- if (use_adult_growth) {
      as.numeric(g("g1_c"))
    } else {
      0.0
    }
    
    val_adult_d <- if (use_adult_growth) {
      as.numeric(g("g1_d_avg"))
    } else {
      1.0
    }
    
    # -----------------------------------------------------------------------
    # 3c. Density-dependent juvenile growth
    # -----------------------------------------------------------------------
    val_juvenile_a <- if (use_juvenile_growth) {
      as.numeric(g("g2_a"))
    } else {
      1.0
    }
    
    val_juvenile_b <- if (use_juvenile_growth) {
      as.numeric(g("g2_b"))
    } else {
      0.0
    }
    
    val_juvenile_c <- if (use_juvenile_growth) {
      as.numeric(g("g2_c"))
    } else {
      0.0
    }
    
    val_juvenile_d <- if (use_juvenile_growth) {
      as.numeric(g("g2_d_avg"))
    } else {
      1.0
    }
    
    # -----------------------------------------------------------------------
    # 4. Harvest curve
    # -----------------------------------------------------------------------
    use_harv_curve <- isTRUE(g("flag_harvest_curve"))
    
    if (use_harv_curve) {
      
      val_h_L50   <- as.numeric(g("harv_L50"))
      val_h_pmax  <- as.numeric(g("harv_pmax"))
      val_h_slope <- as.numeric(g("harv_slope"))
      
    } else {
      
      # Magic values used by the existing C++ implementation to represent
      # fixed harvest probability rather than a logistic curve.
      val_h_L50   <- -1000.0
      val_h_pmax  <- as.numeric(g("harv_fixed_pmax"))
      val_h_slope <- 1000.0
    }
    
    # -----------------------------------------------------------------------
    # 5. Natural mortality
    # -----------------------------------------------------------------------
    use_z_estimation <- isTRUE(s("use_z_estimation"))
    
    final_f_z_ratio <- if (use_z_estimation) {
      1.0 - as.numeric(s("F_over_Z_ratio"))
    } else {
      0.0
    }
    
    z_vec <- as.numeric(vals$z_dist)
    z_vec <- z_vec[is.finite(z_vec)]
    
    validate(
      need(
        length(z_vec) > 0L,
        "No valid natural-mortality values are available."
      )
    )
    
    zb <- stats::quantile(
      z_vec,
      c(0.025, 0.975),
      na.rm = TRUE,
      names = FALSE
    )
    
    z_vec <- z_vec[
      z_vec >= zb[[1L]] &
        z_vec <= zb[[2L]]
    ]
    
    # -----------------------------------------------------------------------
    # 6. Submitted execution settings
    # -----------------------------------------------------------------------
    use_large_population_engine <-
      isTRUE(rc("simulation_engine"))
    
    engine_name <- if (use_large_population_engine) {
      "v2"
    } else {
      "legacy"
    }
    
    omp_threads <- if (use_large_population_engine) {
      max(
        1L,
        as.integer(rc("omp_nthreads"))
      )
    } else {
      1L
    }
    
    combo_threads <- if (isTRUE(rc("use_gpu"))) {
      max(
        0L,
        as.integer(rc("gpu_thread_count"))
      )
    } else {
      0L
    }
    
    fast_forward_mode <- rc("fast_forward_mode")
    
    if (is.null(fast_forward_mode) ||
        length(fast_forward_mode) != 1L ||
        is.na(fast_forward_mode) ||
        !nzchar(as.character(fast_forward_mode))) {
      fast_forward_mode <- "auto"
    }
    
    fast_forward_mode <- as.character(fast_forward_mode)
    
    # Use the T_safe value calculated for the submitted Global parameters.
    auto_t_safe <- if (
      !is.null(global_commit$T_safe_info) &&
      !is.null(global_commit$T_safe_info$T_safe)
    ) {
      suppressWarnings(
        as.integer(global_commit$T_safe_info$T_safe)
      )
    } else if (
      !is.null(vals$T_safe_info) &&
      !is.null(vals$T_safe_info$T_safe)
    ) {
      suppressWarnings(
        as.integer(vals$T_safe_info$T_safe)
      )
    } else {
      0L
    }
    
    if (length(auto_t_safe) != 1L ||
        is.na(auto_t_safe) ||
        auto_t_safe < 0L) {
      auto_t_safe <- 0L
    }
    
    selected_t_safe <- if (
      identical(fast_forward_mode, "off")
    ) {
      0L
    } else {
      auto_t_safe
    }
    
    # -----------------------------------------------------------------------
    # 7. Final model parameter package
    # -----------------------------------------------------------------------
    packed_list <- list(
      
      # --- A. Data ----------------------------------------------------------
      agedata_mat = as.matrix(vals$theta_clean),
      alk_mat     = as.matrix(vals$alk_data),
      z_vec       = z_vec,
      
      # --- B. Time and space -----------------------------------------------
      seed = as.integer(rc("seed")),
      
      before_policy_years = total_burn_in,
      
      policy_years = as.integer(
        g("policy_years")
      ),
      
      # --- C. Biological parameters ----------------------------------------
      
      # 1. Harvest parameters
      harvest = list(
        flag_harvest_curve = use_harv_curve,
        L50   = val_h_L50,
        p_max = val_h_pmax,
        slope = val_h_slope
      ),
      
      # C++ growth_1 is juvenile; map it from UI g2_* controls.
      growth_1 = list(
        use_dd_growth_juvenile = use_juvenile_growth,
        a = val_juvenile_a,
        b = val_juvenile_b,
        c = val_juvenile_c,
        d_avg = val_juvenile_d
      ),
      
      # C++ growth_2 is adult; map it from UI g1_* controls.
      growth_2 = list(
        use_dd_growth_adult = use_adult_growth,
        a = val_adult_a,
        b = val_adult_b,
        c = val_adult_c,
        d_avg = val_adult_d
      ),
      
      # Density-dependent survival
      survival = list(
        use_dd_survival = use_surv,
        a = val_surv_a,
        b = val_surv_b,
        c = val_surv_c,
        d_avg1 = val_surv_d1,
        d_avg2 = val_surv_d2
      ),
      
      # --- D. Other model settings -----------------------------------------
      month_weights = mw,
      
      compliance_struct = c_struct,
      
      execution = list(
        engine = engine_name,
        omp_nthreads = omp_threads,
        combo_threads = combo_threads,
        fast_forward_mode = fast_forward_mode
      ),
      
      other = list(
        
        # Recruitment and environment
        rec_a = as.numeric(g("rec_a")),
        rec_b = as.numeric(g("rec_b")),
        rec_v = 0.68,
        
        lake_area_ha = as.numeric(
          g("lake_area_ha")
        ),
        
        initial_pop_size = as.integer(
          g("initial_pop_size")
        ),
        
        F_over_Z_ratio = final_f_z_ratio,
        
        spawn_month = as.integer(
          g("spawn_month")
        ),
        
        recruit_entry_month = as.integer(
          g("recruit_entry_month")
        ),
        
        # Ages and fishing-mortality mode
        age_spawn = as.numeric(
          g("age_spawn")
        ),
        
        min_adult_age = as.numeric(
          g("min_adult_age")
        ),
        
        age_recruit = as.numeric(
          g("z_full")
        ),
        
        f_age_mode = as.character(
          g("f_age_mode")
        ),
        
        # PSD thresholds
        psd_stock = as.numeric(
          g("psd_stock")
        ),
        
        psd_quality = as.numeric(
          g("psd_quality")
        ),
        
        psd_preferred = as.numeric(
          g("psd_preferred")
        ),
        
        psd_memorable = as.numeric(
          g("psd_memorable")
        ),
        
        psd_trophy = as.numeric(
          g("psd_trophy")
        ),
        
        juv_onlyM_len = as.numeric(
          g("psd_stock")
        ),
        
        # Execution values duplicated here because the existing downstream
        # C++ wrapper expects them in both execution and other.
        T_safe = selected_t_safe,
        simulation_engine = engine_name,
        omp_nthreads = omp_threads,
        combo_threads = combo_threads,
        
        vmonthly_avg =
          as.numeric(s("juv_annual_M")) / 12.0,
        
        use_ricker = isTRUE(
          g("use_ricker")
        )
      )
    )
    
    packed_list
  })
  
  
  
  
  
  # [STEP 2: TEST SIMULATION - UPDATED]
  
  
  output$test_selectors <- renderUI({
    
    if (!isTRUE(sys_status$design_ok)) {
      return(
        tags$div(
          class = "alert alert-warning",
          "Please complete and verify Experiment Design first to select the scenario you are interested."
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
    if (!.real_click("run_test_sim")) return()
    
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
              "Step 1c Experiment Design and submit that page."
            )
          } else if (!isTRUE(sys_status$design_ok)) {
            paste0(
              "❌ Experiment Design has not been verified. ",
              "Open Step 1c Experiment Design and submit it."
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
    
    # A local full simulation owns this machine's CPU and memory while it
    # runs. Starting a test on top of it contends for both and can exhaust
    # memory, taking down the R process and the background job with it.
    # Checked on click rather than by disabling the button: a refusal cannot
    # leave a control stuck, a disabled button can.
    if (isTRUE(proc_state$is_running)) {
      error_msgs <- c(
        error_msgs,
        paste0(
          "❌ A full simulation is already running on this computer. ",
          "Running a test at the same time competes for the same CPU and ",
          "memory and can exhaust it. Wait for it to finish, or stop it ",
          "with the Stop button in Step 3b."
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
          "Please return to Step 2 and click Check cloud connection."
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
      
      rm_vec_all <- parse_num_vec(.confirmed_value(confirmed$design,"rm_vec")
      )
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
        "Individual-level threads per policy calculation: ", omp_threads, "\n",
        "Policy-combination threads per replicate worker: ", combo_threads, "\n"
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
      logical_cores <- .target_logical_cores()
      if (is.na(logical_cores)) logical_cores <- 1L
      nominal_budget <- outer_workers * internal_threads
      budget_warning <- if (nominal_budget > logical_cores * 2L) {
        paste0("\n⚠️ Estimated CPU work-slot demand is ", nominal_budget,
               " on ", logical_cores, " logical CPUs. Reduce replicate workers or internal threads if performance becomes worse.\n")
      } else ""
      
      sys_status$log_2a <- paste0(
        "✅ Test Simulation Complete\n",
        "==========================================\n",
        "Scenario: ", s_row$scenario_name, "\n",
        "Engine: ", engine_label, "\n",
        "Policy combinations: ", nrow(cpp_pol_df), "\n",
        "T_safe: ", all_params$other$T_safe, " month(s)\n",
        "Individual-level threads per policy calculation (OpenMP): ", omp_threads, "\n",
        "Policy-combination threads per replicate worker: ", combo_threads, "\n",
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
    
    policy_threads <- suppressWarnings(
      as.integer(.perf_value(res$combo_threads, 1L))
    )
    individual_threads <- suppressWarnings(
      as.integer(.perf_value(res$omp_threads, 1L))
    )
    
    if (is.na(policy_threads) || policy_threads < 1L) {
      policy_threads <- 1L
    }
    if (is.na(individual_threads) || individual_threads < 1L) {
      individual_threads <- 1L
    }
    
    check_work_slots <- probe_workers *
      policy_threads *
      individual_threads
    
    full_plan_work_slots <- effective_workers *
      policy_threads *
      individual_threads
    
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
          "Starting the full simulation with the same settings has a high risk of failure.\n",
          "------------------------------------------\n",
          "Please return to Step 2 and reduce concurrent replicate workers, ",
          "policy-combination threads, or individual-level threads; ",
          "then run the safety check again.\n",
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
          "The app cannot provide a reliable speed or memory recommendation from this test.\n",
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
          "Please return to Step 2 and set concurrent replicate workers to ",
          max_safe_workers,
          " or fewer; then confirm Run Control and repeat Test 2."
        )
      } else {
        paste0(
          "Please return to Step 2 and reduce concurrent replicate workers, ",
          "policy-combination threads, individual-level threads, or the ",
          "model population size; then repeat Test 2."
        )
      }
      
      return(
        paste0(
          "🛑 Test 2: High Memory-Use Risk\n",
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
          "in memory. Step 3b remains available, but the risk is not confirmed.\n",
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
      
      "Policy-combination threads per replicate worker: ",
      policy_threads,
      "\n",
      
      "Individual-level threads per policy calculation (OpenMP): ",
      individual_threads,
      "\n",
      
      "Logical CPU capacity: ",
      .perf_integer(res$logical_cores),
      "\n",
      
      "------------------------------------------\n",
      "HOW THE PARALLEL SETTINGS COMBINE\n",
      
      probe_workers,
      " independent replicate worker(s) were active in this check. Each worker ",
      "could use up to ",
      policy_threads,
      " policy-combination thread(s), and each policy calculation could use up to ",
      individual_threads,
      " individual-level thread(s).\n",
      
      "Maximum software CPU work slots requested during this check: ",
      probe_workers,
      " × ",
      policy_threads,
      " × ",
      individual_threads,
      " = ",
      check_work_slots,
      "\n",
      
      "Maximum software CPU work slots requested by the selected full plan: ",
      effective_workers,
      " × ",
      policy_threads,
      " × ",
      individual_threads,
      " = ",
      full_plan_work_slots,
      "\n",
      
      "These are software work slots, not physical CPU cores and not the number ",
      "of full-model jobs. They share the machine's ",
      .perf_integer(res$logical_cores),
      " logical CPU(s).\n",
      
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
    sys_status$test2_loaded_from_settings <- FALSE
    invisible(TRUE)
  }
  observeEvent(input$run_oversub_test, {
    if (!.real_click("run_oversub_test")) return()
    
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
              "Step 1c Experiment Design and submit that page."
            )
          } else if (!isTRUE(sys_status$design_ok)) {
            paste0(
              "❌ Experiment Design has not been verified. ",
              "Open Step 1c Experiment Design and submit it."
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
    
    # A local full simulation owns this machine's CPU and memory while it
    # runs. Starting a test on top of it contends for both and can exhaust
    # memory, taking down the R process and the background job with it.
    # Checked on click rather than by disabling the button: a refusal cannot
    # leave a control stuck, a disabled button can.
    if (isTRUE(proc_state$is_running)) {
      error_msgs <- c(
        error_msgs,
        paste0(
          "❌ A full simulation is already running on this computer. ",
          "Running a test at the same time competes for the same CPU and ",
          "memory and can exhaust it. Wait for it to finish, or stop it ",
          "with the Stop button in Step 3b."
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
          "Please return to Step 2 and click Check cloud connection."
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
      
      rm_vec_all <- parse_num_vec(
        .confirmed_value(
          confirmed$design,
          "rm_vec"
        )
      )
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
      
      # The benchmark size is computed HERE and then sent to the container,
      # so it must describe the machine that will run it. Sizing a cloud
      # benchmark from this computer's core count measures the wrong
      # hardware and reports a verdict about a machine nobody is using.
      logical_cores <- .target_logical_cores()
      cores_known <- !is.na(logical_cores)
      if (!cores_known) logical_cores <- 1L
      
      # Internal threads used by each active replicate worker.
      internal_threads_per_worker <- max(
        1L,
        combo_threads * omp_threads
      )
      
      # Do not let the benchmark itself launch more nominal compute threads
      # than the target machine's logical CPU capacity.
      cpu_safe_probe_workers <- if (cores_known) {
        max(
          1L,
          floor(
            logical_cores /
              internal_threads_per_worker
          )
        )
      } else {
        # Capacity unknown. Clamping to this computer would be a guess about
        # the wrong machine, so run the configured concurrency and let the
        # container's own memory pre-check protect the machine.
        effective_workers
      }
      
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
        
        "Policy-combination threads per replicate worker: ",
        combo_threads,
        "\n",
        
        "Individual-level threads per policy calculation (OpenMP): ",
        omp_threads,
        "\n",
        
        "Maximum CPU work slots per active replicate worker: ",
        internal_threads_per_worker,
        "\n",
        
        "Benchmark maximum software CPU work slots: ",
        probe_workers,
        " × ",
        internal_threads_per_worker,
        " = ",
        probe_thread_budget,
        "\n",
        
        "Full-run maximum software CPU work slots: ",
        effective_workers,
        " × ",
        internal_threads_per_worker,
        " = ",
        full_thread_budget,
        "\n",
        
        "Logical CPU cores on ", .target_machine_label(), ": ",
        if (cores_known) logical_cores else "unknown",
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
          "✅ Full-run maximum CPU work-slot demand is within logical CPU capacity.\n"
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
            "the current job count and the machine's logical CPU capacity.\n"
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
        "Please run Model Validation to display test results."
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
  
  # Start file/folder pickers at the user's home folder instead of exposing
  # the package working directory (for example a confusing www folder).
  roots <- c(
    Home = normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE),
    shinyFiles::getVolumes()()
  )
  
  # Cloud Test 1/Test 2 results use the same folder-picker style as the
  # settings and output controls. The selected path is session-specific;
  # when it is empty, completed test results are placed in tempdir().
  cloud_test_dir <- reactiveVal("")
  output$run_output_dir_status <- renderUI({
    selected_path <- input$out_dir
    
    tags$div(
      class = "alert alert-light",
      style = "padding:8px; margin-top:8px; margin-bottom:8px;",
      icon("folder-open"),
      tags$b(" Selected folder: "),
      if (
        is.null(selected_path) ||
        !nzchar(trimws(selected_path))
      ) {
        "No folder selected"
      } else {
        selected_path
      }
    )
  })
  shinyFiles::shinyDirChoose(
    input,
    "browse_cloud_test_dir",
    roots = roots,
    session = session
  )
  
  observeEvent(input$browse_cloud_test_dir, {
    if (is.integer(input$browse_cloud_test_dir)) return()
    
    selected_path <- shinyFiles::parseDirPath(
      roots,
      input$browse_cloud_test_dir
    )
    
    if (length(selected_path) > 0L && nzchar(selected_path[[1L]])) {
      cloud_test_dir(as.character(selected_path[[1L]]))
    }
  })
  
  output$cloud_test_dir_status <- renderUI({
    selected_path <- cloud_test_dir()
    
    tags$div(
      class = "alert alert-light",
      style = "padding:8px; margin-top:8px; margin-bottom:0;",
      icon(if (nzchar(selected_path)) "folder-open" else "clock"),
      if (nzchar(selected_path)) {
        paste0(" Selected folder: ", selected_path)
      } else {
        " No folder selected — a temporary folder will be used."
      }
    )
  })
  
  # ============================================================================
  # SESSION SETTINGS: save / load all Step 1 & Step 2 settings as .rds
  # ============================================================================
  
  # Inputs that describe the *configuration* and should be saved.
  # Runtime / result-view selectors and machine-specific paths are excluded.
  SETTINGS_INPUT_IDS <- c(
    
    # Step 1a: Growth and ALK
    "boot_b_vbgf",
    "show_growth_advanced",
    "vbgf_seed_manual",
    "alk_seed_manual",
    "missing_age_mode",
    
    # Catch curve and survival
    "z_method",
    "z_last",
    "z_boot_bg2",
    "show_z_advanced",
    "z_seed_manual",
    "z_full",
    "use_z_estimation",
    "F_over_Z_ratio",
    "fixed_adult_M",
    "juv_annual_M",
    
    # Global biological parameters
    "min_adult_age",
    "f_age_mode",
    
    "surv_a",
    "surv_b",
    "surv_c",
    "surv_d_avg1",
    "surv_d_avg2",
    
    "g1_a",
    "g1_b",
    "g1_c",
    "g1_d_avg",
    
    "g2_a",
    "g2_b",
    "g2_c",
    "g2_d_avg",
    
    "use_dd_effects",
    "use_dd_survival",
    "use_dd_growth_adult",
    "use_dd_growth_juv",
    
    "flag_harvest_curve",
    "harv_L50",
    "harv_pmax",
    "harv_slope",
    "harv_fixed_pmax",
    
    "psd_stock",
    "psd_quality",
    "psd_preferred",
    "psd_memorable",
    "psd_trophy",
    
    "age_spawn",
    "spawn_month",
    "recruit_entry_month",
    "rec_a",
    "rec_b",
    "use_ricker",
    
    "transient_years",
    "stable_years",
    "policy_years",
    "lake_area_ha",
    "initial_pop_size",
    "month_weights",
    
    # Design
    "ESD_vec",
    "pae_vec",
    "rm_vec",
    "comp_breaks",
    "comp_probs",
    "compliance_mode",
    
    # Run Control
    "n_iter",
    "seed",
    "n_cores",
    "simulation_engine",
    "omp_nthreads",
    "use_gpu",
    "gpu_thread_count",
    "fast_forward_mode",
    
    # Cloud settings — key deliberately excluded
    "use_cloud",
    "gcp_project",
    "gcp_region",
    "gcp_bucket",
    "gcp_machine_type",
    "gcp_container_image",
    
    # Step 3
    "perf_test_mode",
    "test_scen_id",
    "test_var_y",
    
    # Full Model page
    "out_dir",
    "overwrite_existing",
    "cloud_job_type_manual",
    "cloud_job_id_manual",
    
    # Results page
    "res_out_dir",
    "res_selected_scen",
    "res_var_y",
    "res_burn_in"
  )
  
  # Map an input id to the updater family to use on load.
  .input_widget_type <- function(id) {
    
    checkbox <- c(
      "show_growth_advanced",
      "use_dd_effects",
      "use_dd_survival",
      "use_dd_growth_adult",
      "use_dd_growth_juv",
      "flag_harvest_curve",
      "use_z_estimation",
      "show_z_advanced",
      "use_ricker",
      "use_gpu",
      "simulation_engine",
      "use_cloud",
      "overwrite_existing"
    )
    
    radio <- c(
      "f_age_mode",
      "fast_forward_mode",
      "missing_age_mode",
      "perf_test_mode"
    )
    
    select <- c(
      "z_method",
      "test_scen_id",
      "test_var_y",
      "cloud_job_type_manual",
      "res_selected_scen",
      "res_var_y"
    )
    
    slider <- c(
      "n_cores",
      "gpu_thread_count",
      "omp_nthreads"
    )
    
    checkgrp <- c(
      "compliance_mode"
    )
    
    text <- c(
      "month_weights",
      "ESD_vec",
      "pae_vec",
      "rm_vec",
      "comp_breaks",
      "comp_probs",
      "gcp_project",
      "gcp_region",
      "gcp_bucket",
      "gcp_machine_type",
      "gcp_container_image",
      "cloud_job_id_manual",
      "out_dir",
      "res_out_dir"
    )
    
    if (id %in% checkbox) return("checkbox")
    if (id %in% radio)    return("radio")
    if (id %in% select)   return("select")
    if (id %in% slider)   return("slider")
    if (id %in% checkgrp) return("checkgroup")
    if (id %in% text)     return("text")
    
    "numeric"
  }
  
  # Uploaded tables are copied into vals as soon as they arrive. Shiny
  # deletes the temporary upload files when a session ends, which is the
  # moment the final snapshot is written, so reading them from disk at that
  # point returns nothing. Holding a copy here makes the snapshot
  # independent of those files.
  # The service-account key is a file upload, and file uploads do not
  # survive a reconnection: the browser cannot repopulate a file box, so
  # input$gcp_key comes back NULL after a laptop wakes. Everything that
  # talks to Google reads its path from there, so the connection quietly
  # stopped working while the app still showed it as verified.
  #
  # The browser never reveals where the file came from on disk, so the path
  # cannot simply be remembered -- the upload is copied instead, to a
  # location this process owns for as long as it runs. Reconnecting then
  # finds the key exactly where it left it, with nothing shown to the user.
  #
  # Deliberately NOT in the settings snapshot or the cache directory: a
  # credential should not outlive the R session, and must never end up
  # inside a settings file that gets shared.
  observeEvent(input$gcp_key, {
    req(input$gcp_key)
    
    kept <- file.path(
      tempdir(),
      paste0("craibm-gcp-key-", Sys.getpid(), ".json")
    )
    
    okay <- tryCatch({
      file.copy(input$gcp_key$datapath, kept, overwrite = TRUE)
    }, error = function(e) FALSE)
    
    if (isTRUE(okay)) {
      vals$gcp_key_path <- kept
      vals$gcp_key_name <- input$gcp_key$name
    } else {
      vals$gcp_key_path <- input$gcp_key$datapath
      vals$gcp_key_name <- input$gcp_key$name
    }
  })
  
  # An upload is READ ONCE, here, and stored. Everything afterwards works
  # from the stored table.
  #
  # The alternative -- letting each button read input$file_* and fall back to
  # a copy when that is empty -- put the same three-branch decision in five
  # places and made every one of them fail differently after a reconnection,
  # when the box empties but the data is still perfectly good. There is now
  # one writer per upload and no reader that knows the box exists.
  observeEvent(input$file_growth, {
    req(input$file_growth)
    got <- tryCatch(
      as.data.frame(readr::read_csv(
        input$file_growth$datapath, show_col_types = FALSE)),
      error = function(e) NULL
    )
    if (is.null(got)) {
      showNotification(
        "That file could not be read as a CSV.",
        type = "error"
      )
      return()
    }
    vals$loaded_growth_csv <- got
    vals$growth_csv_name <- input$file_growth$name
  })
  
  observeEvent(input$size_csv, {
    req(input$size_csv)
    got <- tryCatch(
      as.data.frame(readr::read_csv(
        input$size_csv$datapath, show_col_types = FALSE)),
      error = function(e) NULL
    )
    if (is.null(got)) {
      showNotification(
        "That file could not be read as a CSV.",
        type = "error"
      )
      return()
    }
    vals$loaded_size_csv <- got
    vals$size_csv_name <- input$size_csv$name
  })
  
  observeEvent(input$file_alk, {
    req(input$file_alk)
    got <- tryCatch(
      as.data.frame(readr::read_csv(
        input$file_alk$datapath, show_col_types = FALSE)),
      error = function(e) NULL
    )
    if (is.null(got)) {
      showNotification(
        "That file could not be read as a CSV.",
        type = "error"
      )
      return()
    }
    vals$loaded_alk_csv <- got
    vals$alk_csv_name <- input$file_alk$name
  })
  
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
        format_version = 2L,
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
        alk_display = vals$alk_display,
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
      # Cache first, upload box second. The order matters: at session end the
      # upload box may still be populated while the file it points at has
      # already been removed, so trusting the box would lose the table.
      files = list(
        
        growth = if (!is.null(vals$loaded_growth_csv)) {
          as.data.frame(vals$loaded_growth_csv)
        } else {
          NULL
        },
        
        growth_name = vals$growth_csv_name,
        
        alk = if (!is.null(vals$loaded_alk_csv)) {
          as.data.frame(vals$loaded_alk_csv)
        } else {
          NULL
        },
        
        alk_name = vals$alk_csv_name,
        
        size = if (!is.null(vals$loaded_size_csv)) {
          as.data.frame(vals$loaded_size_csv)
        } else {
          NULL
        },
        
        size_name = vals$size_csv_name
      ),
      tests = list(
        # Test 1
        validation_data = test_sim_data(),
        validation_report = sys_status$log_2a,
        validation_done = isTRUE(sys_status$test_run_done),
        validation_scenario = input$test_scen_id,
        validation_variable = input$test_var_y,
        
        # Test 2
        performance_report = sys_status$log_oversub,
        performance_mem_safe = sys_status$mem_safe,
        performance_check_done = sys_status$memory_check_done,
        performance_retest_required = sys_status$memory_retest_required
      ),
      analysis = list(
        loaded_scenarios = isolate(loaded_scenarios()),
        valid_burn_in = isolate(valid_burn_in_val()),
        res_policy_year = isolate(res_policy_year())
      ),
      cloud_job = list(
        job_id = proc_state$cloud_job_id,
        task_type = proc_state$cloud_task_type,
        status = proc_state$cloud_status,
        submitted_at = proc_state$cloud_submitted_at,
        result_uri = proc_state$cloud_result_uri
      ),
      # Everything a step confirmed, captured when it was confirmed. This is
      # what lets a restored session behave like the one that was saved,
      # rather than merely looking like it.
      confirmed = isolate(
        shiny::reactiveValuesToList(
          confirmed,
          all.names = TRUE
        )
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
        msg_z       = sys_status$msg_z,
        # The point of saving a half-finished session is being able to
        # see where it stopped. Without these the reloaded app shows
        # "Waiting for submission" on pages that had in fact been
        # submitted and rejected, so the reason for the failure -- the
        # one thing the user needs -- is the only thing thrown away.
        log_1_2     = sys_status$log_1_2,
        log_1_3     = sys_status$log_1_3,
        log_surv       = sys_status$log_surv,
        runcontrol_ok  = isTRUE(sys_status$runcontrol_ok),
        log_runcontrol = sys_status$log_runcontrol,
        
        log_2a         = sys_status$log_2a,
        log_oversub    = sys_status$log_oversub,
        log_2b         = sys_status$log_2b,
        log_3          = sys_status$log_3,
        batch_log      = sys_status$batch_log,
        
        # Stored as historical display text.
        # External recovery will not treat an old "connected" line as authentication.
        log_cloud      = sys_status$log_cloud,
        cloud_summary  = sys_status$cloud_summary
      )
    )
  }
  
  # Identical to collect_settings(); the only difference is a stamp saying
  # the app wrote this itself. Kept as a separate name because the callers
  # read better for it, not because the content differs.
  collect_recovery_state <- function() {
    
    snap <- isolate(collect_settings())
    
    snap$meta$internal_recovery <- TRUE
    snap$meta$recovery_format <- 2L
    
    snap
  }
  
  .save_work_snapshot <- function() {
    
    worth_saving <- isolate(
      !is.null(vals$loaded_growth_csv) ||
        !is.null(vals$loaded_alk_csv) ||
        !is.null(vals$loaded_size_csv) ||
        
        
        !is.null(vals$theta_clean) ||
        !is.null(vals$growth_data) ||
        !is.null(vals$alk_data) ||
        !is.null(vals$z_dist) ||
        
        
        !is.null(confirmed$survival) ||
        !is.null(confirmed$global) ||
        !is.null(confirmed$design) ||
        !is.null(confirmed$runcontrol) ||
        
        
        !is.null(test_sim_data()) ||
        isTRUE(sys_status$memory_check_done)
    )
    
    if (!isTRUE(worth_saving)) {
      return(invisible(FALSE))
    }
    
    snap <- tryCatch(
      isolate(collect_recovery_state()),
      error = function(e) {
        message(
          "craibm: could not collect the recovery state: ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
    
    if (is.null(snap)) {
      return(invisible(FALSE))
    }
    
    .craibm_write_work_snapshot(snap)
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
  
  
  
  recovery_signal <- reactive({
    
    list(
      files = list(
        growth = vals$loaded_growth_csv,
        alk = vals$loaded_alk_csv,
        size = vals$loaded_size_csv
      ),
      
      results = list(
        theta_clean = vals$theta_clean,
        growth_data = vals$growth_data,
        alk_data = vals$alk_data,
        alk_display = vals$alk_display,
        alk_source = vals$alk_source,
        alk_info = vals$alk_info,
        alk_seed = vals$alk_seed,
        alk_bin_width = vals$alk_bin_width,
        z_dist = vals$z_dist,
        T_safe_info = vals$T_safe_info
      ),
      
      confirmed = list(
        survival = confirmed$survival,
        global = confirmed$global,
        design = confirmed$design,
        runcontrol = confirmed$runcontrol
      ),
      
      status = list(
        vbgf_ok = sys_status$vbgf_ok,
        alk_ok = sys_status$alk_ok,
        z_ok = sys_status$z_ok,
        survival_ok = sys_status$survival_ok,
        global_ok = sys_status$global_ok,
        design_ok = sys_status$design_ok,
        runcontrol_ok = sys_status$runcontrol_ok
      ),
      
      tests = list(
        validation_data = test_sim_data(),
        memory_check_done = sys_status$memory_check_done,
        mem_safe = sys_status$mem_safe,
        memory_retest_required =
          sys_status$memory_retest_required
      ),
      analysis = list(
        loaded_scenarios = loaded_scenarios(),
        valid_burn_in = valid_burn_in_val(),
        res_policy_year = res_policy_year()
      )
    )
  })
  
  recovery_signal_debounced <-
    shiny::debounce(
      recovery_signal,
      millis = 500
    )
  
  observeEvent(
    recovery_signal_debounced(),
    {
      if (isTRUE(sys_status$restoring_settings)) {
        return()
      }
      
      try(
        .save_work_snapshot(),
        silent = TRUE
      )
    },
    ignoreInit = TRUE
  )
  
  # Applies a settings object. Extracted from the upload handler so that the
  # automatic work snapshot can be restored through exactly the same path,
  # rather than a second implementation that would drift out of step with it.
  .restore_is_busy <- function() {
    
    isTRUE(proc_state$is_running) ||
      !is.null(proc_state$active_run) ||
      (
        !is.null(proc_state$cloud_status) &&
          proc_state$cloud_status %in% c(
            "submitted",
            "running"
          )
      )
  }
  
  
  .clear_current_work_for_restore <- function(
    clear_cloud_credentials = TRUE
  ) {
    
    # Calculated and uploaded model data
    vals$theta_clean <- NULL
    vals$growth_data <- NULL
    vals$z_dist <- NULL
    vals$alk_data <- NULL
    vals$alk_display <- NULL
    vals$alk_source <- NULL
    vals$alk_info <- NULL
    vals$growth_fit_note <- NULL
    vals$alk_seed <- NULL
    vals$alk_bin_width <- NULL
    vals$vbgf_seed <- NULL
    vals$z_seed <- NULL
    vals$T_safe_info <- NULL
    
    vals$loaded_growth_csv <- NULL
    vals$loaded_size_csv <- NULL
    vals$loaded_alk_csv <- NULL
    
    vals$growth_csv_name <- NULL
    vals$size_csv_name <- NULL
    vals$alk_csv_name <- NULL
    
    # Confirmed action-button results
    confirmed$survival <- NULL
    confirmed$global <- NULL
    confirmed$design <- NULL
    confirmed$runcontrol <- NULL
    
    # Tests and Results-page state
    test_sim_data(NULL)
    design_csv_data(NULL)
    loaded_scenarios(NULL)
    valid_burn_in_val(5)
    res_policy_year(0)
    
    # Completion state
    sys_status$vbgf_ok <- FALSE
    sys_status$alk_ok <- FALSE
    sys_status$z_ok <- FALSE
    sys_status$survival_ok <- FALSE
    sys_status$global_ok <- FALSE
    sys_status$design_ok <- FALSE
    sys_status$runcontrol_ok <- FALSE
    sys_status$test_run_done <- FALSE
    
    sys_status$mem_safe <- NA
    sys_status$memory_check_done <- FALSE
    sys_status$memory_retest_required <- FALSE
    sys_status$test2_loaded_from_settings <- FALSE
    
    if (isTRUE(clear_cloud_credentials)) {
      
      old_key <- isolate(vals$gcp_key_path)
      
      if (
        !is.null(old_key) &&
        length(old_key) == 1L &&
        file.exists(old_key)
      ) {
        try(unlink(old_key), silent = TRUE)
      }
      
      vals$gcp_key_path <- NULL
      vals$gcp_key_name <- NULL
      
      proc_state$cloud_auth <- NULL
      proc_state$cloud_verified <- FALSE
      sys_status$log_cloud <- NULL
      sys_status$cloud_summary <- NULL
      sys_status$loaded_from <- NULL
      
      proc_state$cloud_auth <- NULL
      proc_state$cloud_verified <- FALSE
      proc_state$cloud_release_offer <- FALSE
      
      proc_state$cloud_job_id <- NULL
      proc_state$cloud_watch_job <- NULL
      proc_state$cloud_task_type <- NULL
      proc_state$cloud_status <- NULL
      
      proc_state$cloud_done <- NA_integer_
      proc_state$cloud_total <- NA_integer_
      proc_state$cloud_result_uri <- NULL
      proc_state$cloud_poll_fails <- 0L
      proc_state$cloud_submitted_at <- NULL
      proc_state$cloud_queue_warned <- FALSE
      proc_state$cloud_last_report <- NULL
      proc_state$cloud_no_progress <- 0L
      
      proc_state$cloud_perf_requested <- NA_integer_
      proc_state$cloud_perf_probe <- NA_integer_
      proc_state$cloud_result_ready <- FALSE
      proc_state$cloud_result_prog <- NULL
      
      try(
        .craibm_clear_cloud(),
        silent = TRUE
      )
    }
    
    invisible(TRUE)
  }
  .apply_settings <- function(
    saved,
    notify = TRUE,
    restore_mode
  ) {
    
    restore_mode <- match.arg(
      restore_mode,
      c(
        "manual",
        "recover",
        "reconnect"
      )
    )
    
    is_internal_recovery <-
      identical(restore_mode, "reconnect")
    
    restore_succeeded <- FALSE
    
    # This must be set before clearing data or updating any input.
    # It prevents update*Input() from invalidating restored confirmations,
    # Test results and Run Control while restoration is still in progress.
    sys_status$restoring_settings <- TRUE
    
    # Install cleanup immediately. Even if clearing or restoration fails,
    # the app cannot remain permanently stuck in restoration mode.
    on.exit(
      later::later(
        function() {
          
          sys_status$restoring_settings <- FALSE
          
          # Manual Load and Recover become the new local recovery snapshot,
          # but only after the entire restore completed successfully.
          if (
            !is_internal_recovery &&
            isTRUE(restore_succeeded)
          ) {
            try(
              .save_work_snapshot(),
              silent = TRUE
            )
          }
          
        },
        delay = 1
      ),
      add = TRUE
    )
    
    # Manual Load and Recover replace the current work and remove credentials.
    # Browser reconnect keeps the current in-process key and running state.
    if (!is_internal_recovery) {
      .clear_current_work_for_restore(
        clear_cloud_credentials = TRUE
      )
    }
    tst <- saved$tests
    
    if (!is.null(tst)) {
      
      if (!is.null(tst$validation_data)) {
        test_sim_data(tst$validation_data)
      }
      
      if (!is.null(tst$validation_report)) {
        sys_status$log_2a <- tst$validation_report
      }
      
      if (!is.null(tst$validation_variable)) {
        updateSelectInput(
          session,
          "test_var_y",
          selected = tst$validation_variable
        )
      }
      
      if (!is.null(tst$validation_scenario)) {
        updateSelectInput(
          session,
          "test_scen_id",
          selected = tst$validation_scenario
        )
      }
    }
    saved_test2_exists <- !is.null(tst) && (
      isTRUE(tst$performance_check_done) ||
        identical(tst$performance_mem_safe, FALSE) ||
        isTRUE(tst$performance_retest_required)
    )
    iv <- saved$inputs
    if (is.null(iv)) {
      iv <- list()
    }
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
    saved_has_t_safe <- !is.null(r) && !is.null(r$T_safe_info)
    
    # A restore replaces the session with the saved one, so these are
    # assigned as they stand -- including the NULLs, which record that the
    # saved session had not got that far. Half-replacing would produce a
    # state that never existed: this file's growth fit beside the previous
    # session's mortality distribution.
    #
    # The service-account key is untouched here and everywhere below. It is
    # never written to a settings file, so there is nothing to restore, and
    # a key already loaded in this session stays as it is.
    if (!is.null(r)) {
      vals$theta_clean     <- r$theta_clean
      vals$growth_data     <- r$growth_data
      vals$z_dist          <- r$z_dist
      vals$alk_data        <- r$alk_data
      vals$alk_display <- if (!is.null(r$alk_display)) {
        r$alk_display
      } else {
        r$alk_data
      }
      vals$alk_source <- if (identical(r$alk_source, "auto")) {
        "generated_imputed"
      } else {
        r$alk_source
      }
      vals$alk_info        <- r$alk_info
      vals$alk_bin_width   <- r$alk_bin_width
      vals$alk_seed        <- r$alk_seed
      vals$vbgf_seed       <- r$vbgf_seed
      vals$z_seed          <- r$z_seed
      vals$growth_fit_note <- r$growth_fit_note
      if (saved_has_t_safe) vals$T_safe_info <- r$T_safe_info
    }
    
    f <- saved$files
    
    if (!is.null(f)) {
      
      vals$loaded_growth_csv <- if (!is.null(f$growth)) {
        as.data.frame(f$growth)
      } else {
        NULL
      }
      
      vals$loaded_alk_csv <- if (!is.null(f$alk)) {
        as.data.frame(f$alk)
      } else {
        NULL
      }
      
      vals$loaded_size_csv <- if (!is.null(f$size)) {
        as.data.frame(f$size)
      } else {
        NULL
      }
      
      vals$growth_csv_name <- f$growth_name
      vals$alk_csv_name <- f$alk_name
      vals$size_csv_name <- f$size_name
      
      # Older RDS may contain an ALK table but no separate alk_data field.
      if (
        !is.null(f$alk) &&
        is.null(vals$alk_data)
      ) {
        vals$alk_data <- as.data.frame(f$alk)
        vals$alk_display <- as.data.frame(f$alk)
        
        if (is.null(vals$alk_source)) {
          vals$alk_source <- "file"
        }
      }
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
      
      # Restored only when present, so that settings files written before
      # these were saved still load and simply keep their default text.
      if (!is.null(st$log_1_2))  sys_status$log_1_2  <- st$log_1_2
      if (!is.null(st$log_1_3))  sys_status$log_1_3  <- st$log_1_3
      if (!is.null(st$log_surv)) sys_status$log_surv <- st$log_surv
      if (!is.null(st$log_runcontrol)) {
        sys_status$log_runcontrol <- st$log_runcontrol
      }
      
      if (!is.null(st$log_2a)) {
        sys_status$log_2a <- st$log_2a
      }
      
      if (!is.null(st$log_oversub)) {
        sys_status$log_oversub <- st$log_oversub
      }
      
      if (!is.null(st$log_2b)) {
        sys_status$log_2b <- st$log_2b
      }
      
      if (!is.null(st$log_3)) {
        sys_status$log_3 <- st$log_3
      }
      
      if (!is.null(st$batch_log)) {
        sys_status$batch_log <- st$batch_log
      }
      
      # An old "connected" Cloud message is only trustworthy during an
      # in-process browser reconnect. Manual Load and Recover have no key.
      if (
        is_internal_recovery &&
        !is.null(st$log_cloud)
      ) {
        sys_status$log_cloud <- st$log_cloud
      }
      
      if (!is.null(st$cloud_summary)) {
        sys_status$cloud_summary <- st$cloud_summary
      }
    }
    
    # Written by every save route now, so this is no longer specific to the
    # internal snapshot.
    a <- saved$analysis
    
    if (!is.null(a)) {
      
      loaded_scenarios(a$loaded_scenarios)
      
      if (!is.null(a$valid_burn_in)) {
        valid_burn_in_val(a$valid_burn_in)
      }
      
      if (!is.null(a$res_policy_year)) {
        res_policy_year(a$res_policy_year)
      }
    }
    saved_confirmed <- saved$confirmed
    
    if (!is.null(saved_confirmed)) {
      
      confirmed$survival <- saved_confirmed$survival
      confirmed$global <- saved_confirmed$global
      confirmed$design <- saved_confirmed$design
      confirmed$runcontrol <- saved_confirmed$runcontrol
      
    } else {
      
      
      if (isTRUE(sys_status$survival_ok)) {
        confirmed$survival <- list(
          values = iv[
            intersect(
              SURVIVAL_INPUT_IDS,
              names(iv)
            )
          ],
          z_dist = vals$z_dist,
          validation_log = sys_status$log_surv,
          submitted_at = Sys.time()
        )
      }
      
      if (isTRUE(sys_status$global_ok)) {
        confirmed$global <- list(
          values = iv[
            intersect(
              GLOBAL_INPUT_IDS,
              names(iv)
            )
          ],
          T_safe_info = vals$T_safe_info,
          validation_log = sys_status$log_1_2,
          submitted_at = Sys.time()
        )
      }
      
      if (isTRUE(sys_status$design_ok)) {
        confirmed$design <- list(
          values = iv[
            intersect(
              DESIGN_INPUT_IDS,
              names(iv)
            )
          ],
          size_csv = vals$loaded_size_csv,
          validation_log = sys_status$log_1_3,
          submitted_at = Sys.time()
        )
      }
      confirmed$runcontrol <- NULL
    }
    
    
    
    
    
    # Settings files from versions before T_safe_info was stored can still
    # recover the calculated safe duration from their saved inputs.
    if (
      !saved_has_t_safe &&
      isTRUE(sys_status$vbgf_ok) &&
      isTRUE(sys_status$global_ok) &&
      !is.null(vals$theta_clean)
    ) {
      vals$T_safe_info <- .calculate_t_safe_info(iv)
    }
    
    saved_runcontrol <- if (!is.null(saved_confirmed)) {
      saved_confirmed$runcontrol
    } else {
      NULL
    }
    
    saved_cloud_mode <- isTRUE(
      saved_runcontrol$values$use_cloud
    )
    
    # No longer restricted to the app's own snapshot. A file written by the
    # Save button now records the same confirmation, so honouring it there
    # too is what makes the three routes behave alike.
    #
    # Cloud mode is still excluded, and not as an oversight: a cloud run's
    # confirmation rests on a connection that has to be re-established
    # before it means anything.
    restore_local_runcontrol <-
      !saved_cloud_mode &&
      isTRUE(st$runcontrol_ok) &&
      !is.null(saved_runcontrol)
    
    if (restore_local_runcontrol) {
      
      confirmed$runcontrol <- saved_runcontrol
      sys_status$runcontrol_ok <- TRUE
      
      if (!is.null(st$log_runcontrol)) {
        sys_status$log_runcontrol <- st$log_runcontrol
      }
      
    } else {
      
      confirmed$runcontrol <- NULL
      sys_status$runcontrol_ok <- FALSE
      
      sys_status$log_runcontrol <- if (saved_cloud_mode) {
        paste0(
          "Cloud run settings were restored. ",
          "Please verify the cloud connection and confirm Run Control again."
        )
      } else {
        "Please confirm Run Control for this computer."
      }
    }
    
    sys_status$test_run_done <-
      !is.null(tst) &&
      isTRUE(tst$validation_done)
    
    if (isTRUE(saved_test2_exists)) {
      
      sys_status$mem_safe <- tst$performance_mem_safe
      sys_status$memory_check_done <-
        isTRUE(tst$performance_check_done)
      sys_status$memory_retest_required <-
        isTRUE(tst$performance_retest_required)
      
      if (!is.null(tst$performance_report)) {
        
        sys_status$log_oversub <- if (
          is_internal_recovery
        ) {
          
          # Same R process and same computer: restore the original live log.
          tst$performance_report
          
        } else {
          
          # Manual Load or Recover: keep the result, but mark its provenance.
          paste0(
            "📂 Loaded from saved settings — not measured in this restored ",
            "session.\n",
            "Please confirm this is the same computer and that the parallel ",
            "configuration is unchanged.\n",
            "==========================================\n",
            tst$performance_report
          )
        }
      }
      
      sys_status$test2_loaded_from_settings <-
        !is_internal_recovery
      
    } else {
      
      sys_status$mem_safe <- NA
      sys_status$memory_check_done <- FALSE
      sys_status$memory_retest_required <- FALSE
      sys_status$test2_loaded_from_settings <- FALSE
    }
    
    meta <- saved$meta
    sys_status$loaded_from <- if (is_internal_recovery) {
      
      paste0(
        "File restored from the browser-reconnection cache",
        if (
          !is.null(meta) &&
          !is.null(meta$saved_at)
        ) {
          paste0(" (cached on ", meta$saved_at, ")")
        } else {
          ""
        },
        "."
      )
      
    } else if (!is.null(meta)) {
      
      paste0(
        "Loaded settings saved on ",
        if (!is.null(meta$saved_at)) {
          meta$saved_at
        } else {
          "unknown"
        },
        if (!is.null(meta$package_version)) {
          paste0(" (craibm ", meta$package_version, ")")
        } else {
          ""
        }
      )
      
    } else {
      
      "Loaded settings from file."
    }
    
    later::later(
      function() {
        
        if (!is.null(iv$test_scen_id)) {
          updateSelectInput(
            session,
            "test_scen_id",
            selected = iv$test_scen_id
          )
        }
        
        if (!is.null(iv$res_selected_scen)) {
          updateSelectInput(
            session,
            "res_selected_scen",
            selected = iv$res_selected_scen
          )
        }
        
      },
      delay = 0.2
    )
    restore_succeeded <- TRUE
    if (isTRUE(notify)) {
      showNotification(
        "Settings loaded.",
        type = "message",
        duration = 6
      )
    }
  }
  
  
  recover_last_session_log <- reactiveVal("")
  
  output$recover_last_session_log <- renderText({
    recover_last_session_log()
  })
  
  observeEvent(input$recover_last_session, {
    if (!.real_click("recover_last_session")) return()
    if (.restore_is_busy()) {
      recover_last_session_log(
        paste0(
          "A simulation is currently being tracked. ",
          "Recovery was not applied because replacing this session could ",
          "detach the running job."
        )
      )
      return()
    }
    recover_last_session_log(
      "Checking for a recoverable local session..."
    )
    
    snap <- .craibm_read_work_snapshot()
    
    if (is.null(snap)) {
      recover_last_session_log(
        "No recoverable local session was found."
      )
      return()
    }
    
    restored <- tryCatch(
      {
        .apply_settings(
          snap,
          notify = FALSE,
          restore_mode = "recover"
        )
        
        TRUE
      },
      error = function(e) {
        recover_last_session_log(
          paste0(
            "The last session could not be recovered: ",
            conditionMessage(e)
          )
        )
        
        FALSE
      }
    )
    
    if (isTRUE(restored)) {
      saved_at <- tryCatch(
        snap$meta$saved_at,
        error = function(e) NULL
      )
      
      recover_last_session_log(
        if (
          !is.null(saved_at) &&
          length(saved_at) == 1L &&
          nzchar(saved_at)
        ) {
          paste0(
            "Last session recovered. Saved on ",
            saved_at,
            "."
          )
        } else {
          "Last session recovered."
        }
      )
    }
  })
  
  observeEvent(input$load_settings, {
    
    req(input$load_settings)
    
    if (.restore_is_busy()) {
      showNotification(
        paste0(
          "Settings cannot be loaded while a simulation is being tracked. ",
          "The current run was not changed."
        ),
        type = "warning",
        duration = 8
      )
      return()
    }
    
    saved <- tryCatch(
      readRDS(input$load_settings$datapath),
      error = function(e) NULL
    )
    
    if (
      is.null(saved) ||
      is.null(saved$inputs)
    ) {
      showNotification(
        "This file is not a valid craibm settings file.",
        type = "error"
      )
      return()
    }
    
    .apply_settings(
      saved,
      notify = TRUE,
      restore_mode = "manual"
    )
  })
  observeEvent(
    input$craibm_resume_tab,
    {
      resume_requested <- isTRUE(
        input$craibm_resume_tab$resume
      )
      
      # A genuinely new app must remain clean.
      if (!resume_requested) {
        return()
      }
      # Do not re-apply settings while a local, test or Cloud job is being
      # tracked. The existing run/reconnect machinery owns that state.
      run_is_active <-
        isTRUE(proc_state$is_running) ||
        !is.null(proc_state$active_run) ||
        (
          !is.null(proc_state$cloud_status) &&
            proc_state$cloud_status %in% c(
              "submitted",
              "running"
            )
        )
      
      if (isTRUE(run_is_active)) {
        return()
      }
      
      snap <- tryCatch(
        .craibm_read_work_snapshot(),
        error = function(e) {
          message(
            "craibm: reconnect snapshot read failed: ",
            conditionMessage(e)
          )
          
          NULL
        }
      )
      
      if (is.null(snap)) {
        return()
      }
      
      tryCatch(
        {
          # Restore the entire internal snapshot, not only Growth data.
          .apply_settings(
            snap,
            notify = FALSE,
            restore_mode = "reconnect"
          )
        },
        error = function(e) {
          message(
            "craibm: reconnect snapshot apply failed: ",
            conditionMessage(e)
          )
        }
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
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
      # The kept copy first. input$gcp_key is only consulted for an upload
      # that has not been copied yet, and is empty after a reconnection.
      key_path     = if (!is.null(vals$gcp_key_path) &&
                         file.exists(vals$gcp_key_path)) {
        vals$gcp_key_path
      } else if (!is.null(input$gcp_key)) {
        input$gcp_key$datapath
      } else {
        NULL
      },
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
        # A worker process has no lexical link back to the namespace, so the
        # four helpers used below are fetched from it explicitly. library()
        # would not do: these are internal and no longer exported.
        .ns <- loadNamespace("craibm")
        cloud_auth          <- .ns$cloud_auth
        cloud_refresh_auth  <- .ns$cloud_refresh_auth
        cloud_poll_progress <- .ns$cloud_poll_progress
        cloud_job_state     <- .ns$cloud_job_state
        
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
    if (is.null(cs$key_path)) stop("Please upload a service-account key first.")
    
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
        "Please stop that job before starting another one. If it was already cancelled ",
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
  
  .cloud_start_watch <- function(job_id, started_at = Sys.time()) {
    cs <- isolate(cloud_settings())
    sp <- .cloud_status_file(job_id)
    
    if (is.null(.craibm_runs$cloud) ||
        !identical(.craibm_runs$cloud$job_id, job_id)) {
      try(unlink(sp), silent = TRUE)
    }
    
    # Remembered on disk so that a later session -- or a later R process --
    # can put the ID back into the lookup box without the user having to
    # find it themselves.
    .craibm_register_cloud(
      list(
        job_id = job_id,
        task_type = isolate(proc_state$cloud_task_type),
        result_uri = isolate(proc_state$cloud_result_uri),
        started = started_at
      )
    )
    
    # Put the ID into the lookup box under the run log. That box is part of
    # the saved settings, so this is what makes a job id travel inside a
    # settings file: submit a run, save your settings, and the file carries
    # the means to find that run again. Without this the box would still be
    # empty at the moment the settings are written.
    updateTextInput(session, "cloud_job_id_manual", value = job_id)
    
    proc_state$cloud_watch_job <- job_id
    
    shinyjs::runjs(sprintf("
      window._craibmT0 = %s;
      clearInterval(window._craibmClk);
      window._craibmClk = setInterval(function () {
        var els = document.querySelectorAll('.cloud-clock');
        if (!els.length) return;
        var s = Math.floor((Date.now() - window._craibmT0) / 1000);
        var h = Math.floor(s / 3600), m = Math.floor((s %% 3600) / 60);
        var txt = (h > 0 ? h + ' h ' : '') + m + ' min ' + (s %% 60) + ' s';
        for (var i = 0; i < els.length; i++) els[i].textContent = txt;
      }, 1000);
    ", format(as.numeric(started_at) * 1000, scientific = FALSE)))
    
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
    
    # Folder chosen with the Step 3a picker, or a temporary one when none
    # has been selected for this session.
    user_dir <- trimws(as.character(isolate(cloud_test_dir()) %||% ""))
    dest <- if (nzchar(user_dir)) {
      file.path(user_dir, job_id)
    } else {
      file.path(tempdir(), paste0("craibm-cloud-", job_id))
    }
    
    auth <- cloud_token()
    
    # Hard ceiling. These result files are tiny, so anything past three
    # minutes means the network or the download address is wrong, not that
    # the transfer is merely slow. httr's own timeout enforces this even
    # though the call is synchronous, so the main process cannot hang past
    # it -- there is no separate watchdog and none is needed.
    dl <- tryCatch(
      withCallingHandlers(
        {
          old <- options(timeout = 180)
          on.exit(options(old), add = TRUE)
          httr::with_config(
            httr::timeout(180),
            cloud_download_results(auth, cs$bucket, job_id, dest)
          )
        },
        error = function(e) e
      ),
      error = function(e) list(pass = FALSE, msg = conditionMessage(e))
    )
    
    if (!isTRUE(dl$pass)) {
      return(list(
        ok = FALSE,
        msg = paste0(
          dl$msg,
          "\nIf this looks like a timeout, check your internet connection ",
          "and the download folder set under About Test Simulation."
        ),
        value = NULL
      ))
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
    if (
      !is.null(proc_state$cloud_status) &&
      proc_state$cloud_status %in%
      c("submitted", "running")
    ) {
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
    proc_state$cloud_result_ready   <- FALSE
    proc_state$cloud_result_prog    <- NULL
    sys_status$cloud_summary      <- NULL
    invisible(TRUE)
  }
  
  # Download and apply a completed Test 1 or Test 2 result. Full-model archives
  # stay in Cloud Storage until the user chooses Download.
  # Called when the watcher reports the cloud job as done. It does NOT
  # download anything: on a local test the result is already in memory, but
  # a cloud test's result sits in the bucket, and pulling it here would put a
  # blocking download back on the main thread at the worst moment. Instead it
  # arms View Result, and the user triggers the (bounded) download when they
  # choose. The full run's large archive keeps its own Download button.
  .cloud_collect_result <- function(jid, prog) {
    tt <- isolate(proc_state$cloud_task_type)
    if (is.null(tt)) tt <- ""
    
    if (identical(tt, "full")) {
      showNotification(
        "The full simulation has finished. See Step 3b and download the results.",
        id = "cloud_job_notice", type = "message", duration = NULL
      )
      return(invisible(TRUE))
    }
    
    if (!tt %in% c("validation", "perfcheck")) return(invisible(TRUE))
    
    # Remember the timings and let the tab show a View Result button.
    proc_state$cloud_result_prog  <- prog
    proc_state$cloud_result_ready <- TRUE
    
    label <- if (identical(tt, "validation")) "Model validation" else "Parallel performance check"
    line <- paste0(
      "\u2705 ", label, " finished on Google Cloud.\n",
      "Please press \"View Result\" to download the result and show the report."
    )
    if (identical(tt, "validation")) {
      sys_status$log_2a <- line
    } else {
      sys_status$log_oversub <- line
    }
    
    showNotification(
      paste0(label, " finished. Press View Result to fetch and display it."),
      id = "cloud_job_notice", type = "message", duration = NULL
    )
    
    invisible(TRUE)
  }
  
  # The actual download + parse + render, run only when the user asks. This
  # is the one place a cloud test blocks the main thread, and it is bounded:
  # .cloud_fetch_result() carries a hard 180-second timeout, so the page
  # cannot hang longer than that even if the network is dead.
  .cloud_view_result <- function() {
    jid <- isolate(proc_state$cloud_job_id)
    tt  <- isolate(proc_state$cloud_task_type)
    prog <- isolate(proc_state$cloud_result_prog)
    if (is.null(jid) || is.null(tt)) return(invisible(FALSE))
    
    file_name <- if (identical(tt, "validation")) {
      "validation_result.rds"
    } else if (identical(tt, "perfcheck")) {
      "perfcheck_result.rds"
    } else {
      return(invisible(FALSE))
    }
    
    # A reactive write here would not paint until AFTER the blocking download
    # returns, because both happen in one cycle on one thread. These two calls
    # render immediately instead: a notification uses its own channel, and
    # disabling the button gives instant visual feedback that the click landed.
    showNotification(
      "Downloading the result. This can take up to three minutes...",
      id = "cloud_view_progress", type = "message", duration = NULL
    )
    shinyjs::disable("view_cloud_result")
    on.exit({
      removeNotification("cloud_view_progress")
      shinyjs::enable("view_cloud_result")
    }, add = TRUE)
    
    fetched <- tryCatch(
      .cloud_fetch_result(jid, file_name),
      error = function(e) list(ok = FALSE, msg = conditionMessage(e))
    )
    
    if (!isTRUE(fetched$ok)) {
      if (identical(tt, "perfcheck")) {
        sys_status$mem_safe <- NA
        sys_status$memory_check_done <- FALSE
        sys_status$memory_retest_required <- TRUE
        sys_status$log_oversub <- paste0(
          "\U0001F6D1 The result could not be downloaded.\n",
          fetched$msg
        )
      } else {
        sys_status$test_run_done <- FALSE
        sys_status$log_2a <- paste0(
          "\U0001F6D1 The result could not be downloaded.\n",
          fetched$msg
        )
      }
      showNotification("Could not download the result. Check your connection.",
                       type = "error", duration = 12)
      # Leave the button armed so the user can retry after fixing the link.
      return(invisible(FALSE))
    }
    
    ok <- tryCatch({
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
          .format_test_duration(prog$startup_sec), "\n",
          "Model calculation for the selected scenario: ",
          .format_test_duration(prog$compute_sec), "\n",
          "------------------------------------------\n",
          "Planned full-model jobs: ", est$total_tasks, "\n",
          "Simultaneous replicate workers: ", est$workers, "\n",
          "Rough full-model calculation-time estimate: ", est$formatted, "\n",
          "Cloud machine startup, result writing and transfer add extra time.\n",
          "------------------------------------------\n",
          "The validation result was downloaded and the plot was updated."
        )
      }
      TRUE
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
      FALSE
    })
    
    if (isTRUE(ok)) {
      proc_state$cloud_result_ready <- FALSE
      .craibm_clear_cloud()
    }
    invisible(ok)
  }
  
  observeEvent(input$view_cloud_result, {
    .cloud_view_result()
  })
  
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
          "Please check the Batch console, or use Download and prepare results once it finishes."
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
          "Please check the Batch logs for image-pull, IAM, quota, or VM errors."
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
          "Please check the Batch console. Results, if any, will still be at:\n",
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
        "\nThe archive is still available with Download and prepare results."
      )
    })
  })
  
  # This is the sole remaining cloud timer. It reads only a tiny local RDS
  # written by the watcher process; it performs no network work.
  # Built once, rendered into three separate outputs. A Shiny output ID may
  # appear only once in the page: three uiOutput("cloud_watch_panel") calls
  # meant only one of them was ever bound, which is why Step 3a showed
  # nothing while Step 3b worked.
  # Shown when a previous session left a job behind. Deliberately not shown
  # while something is already being tracked in this session.
  
  
  
  # Shared by the button above and by the manual lookup. Attaching only ever
  # reads status; nothing here can start, restart or duplicate a run.
  .cloud_attach_job <- function(job_id, task_type, started, result_uri = NULL) {
    if (is.null(task_type) || !nzchar(task_type)) task_type <- "full"
    if (!inherits(started, "POSIXct")) started <- Sys.time()
    
    cs <- isolate(cloud_settings())
    if (is.null(result_uri) || !nzchar(result_uri)) {
      result_uri <- paste0("gs://", cs$bucket, "/jobs/", job_id, "/results.zip")
    }
    
    proc_state$cloud_job_id <- job_id
    proc_state$cloud_task_type <- task_type
    proc_state$cloud_result_uri <- result_uri
    proc_state$cloud_status <- "running"
    
    if (
      task_type %in%
      c(
        "validation",
        "perfcheck",
        "full"
      )
    ) {
      .set_active_run(
        task_type,
        "cloud"
      )
    }
    
    ok <- tryCatch({
      .cloud_start_watch(job_id, started_at = started)
      TRUE
    }, error = function(e) {
      showNotification(
        paste0("Could not attach to that job: ", conditionMessage(e)),
        type = "error", duration = 12
      )
      FALSE
    })
    
    if (!isTRUE(ok)) {
      proc_state$cloud_status <- NULL
      proc_state$cloud_job_id <- NULL
      try(.clear_active_run(), silent = TRUE)
      return(invisible(FALSE))
    }
    
    showNotification(
      paste0("Now tracking cloud job ", job_id, "."),
      type = "message", duration = 10
    )
    invisible(TRUE)
  }
  
  # Manual lookup. The automatic record covers the ordinary case; this covers
  # the ones it cannot -- a different computer, a reinstalled R, a cleared
  # cache, or a job whose record was already tidied away.
  observeEvent(input$track_cloud_job_id, {
    jid <- trimws(as.character(input$cloud_job_id_manual %||% ""))
    if (!nzchar(jid)) {
      showNotification("Please enter a job ID first.", type = "warning")
      return()
    }
    if (!isTRUE(proc_state$cloud_verified)) {
      showNotification(
        "Please check the cloud connection in Step 2 first.",
        type = "warning"
      )
      return()
    }
    # Use the type chosen next to the box. Hard-coding "full" sent a test
    # job down the full-run branch of the collector, which looks for a
    # different result file and reports the wrong thing.
    jtype <- as.character(input$cloud_job_type_manual %||% "full")
    if (!jtype %in% c("validation", "perfcheck", "full")) jtype <- "full"
    
    .cloud_attach_job(jid, jtype, Sys.time(), NULL)
  })
  
  .build_cloud_watch_panel <- function() {
    
    jid <- proc_state$cloud_job_id
    st <- proc_state$cloud_status
    if (
      is.null(jid) ||
      is.null(st) ||
      !st %in% c("submitted", "running")
    ) {
      return(NULL)
    }
    
    # Only one cloud job may run at a time, so every tab reports the same
    # job. Naming it means the panel is never ambiguous.
    task_label <- switch(
      if (is.null(proc_state$cloud_task_type)) "" else proc_state$cloud_task_type,
      validation = "Test 1: model validation",
      perfcheck  = "Test 2: parallel performance check",
      full       = "Full simulation",
      "Cloud job"
    )
    
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
      tags$b(paste0(" ", task_label, " is on Google Cloud.")),
      tags$br(),
      detail,
      tags$br(),
      tags$span(
        style = "font-size:11px;",
        "Elapsed: ",
        tags$span(class = "cloud-clock", "0 min 0 s")
      )
    )
  }
  
  # Re-reads the local status file once a minute, but ONLY while a cloud job
  # is in flight. Ticking unconditionally left a timer running for the whole
  # life of the session on a panel that had nothing to show.
  .cloud_watch_tick <- function() {
    jid <- proc_state$cloud_job_id
    st <- proc_state$cloud_status
    if (!is.null(jid) &&
        !is.null(st) &&
        st %in% c("submitted", "running")) {
      invalidateLater(60000)
    }
    invisible(NULL)
  }
  
  # One output per location, all fed by the builder above.
  output$cloud_watch_panel_test1 <- renderUI({
    .cloud_watch_tick()
    .build_cloud_watch_panel()
  })
  
  output$cloud_watch_panel_test2 <- renderUI({
    .cloud_watch_tick()
    .build_cloud_watch_panel()
  })
  
  output$cloud_watch_panel_full <- renderUI({
    .cloud_watch_tick()
    .build_cloud_watch_panel()
  })
  
  # ---- Controls shown during and after a cloud run ---------------------------
  
  # show_result_actions:
  #   TRUE  for the Step 3b full simulation, whose archive is large and is
  #         only ever fetched on request.
  #   FALSE for the Step 3a tests, whose small result files are downloaded
  #         and applied automatically, and whose panel is reset by the next
  #         run. Offering Download / Clear there was three buttons that had
  #         nothing left to do.
  .build_cloud_controls <- function(cancel_id = NULL,
                                    show_result_actions = TRUE) {
    
    task_label <- switch(
      if (is.null(proc_state$cloud_task_type)) "" else proc_state$cloud_task_type,
      validation = "Model validation",
      perfcheck  = "Parallel performance check",
      full       = "Full simulation",
      "Cloud job"
    )
    
    status <- proc_state$cloud_status
    
    if (is.null(status)) {
      return(helpText("No cloud job has been submitted yet."))
    }
    
    # While a job is in flight the watch panel above already reports its
    # state, elapsed time and queue position. Repeating any of that here
    # produced two stacked boxes saying the same thing, so this block now
    # contributes nothing until the job has finished.
    if (status %in% c("submitted", "running")) {
      return(NULL)
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
                  " Please check the job in the Batch console, or cancel it and try a",
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
          style = "white-space:pre-wrap; font-size:13px;",
          sys_status$cloud_summary
        )
      },
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
      if (
        isTRUE(show_result_actions) &&
        status %in% c("done", "failed", "cancelled")
      ) {
        tagList(
          actionButton("cloud_download", "Download and prepare results",
                       class = "btn-success", width = "100%",
                       icon = icon("cloud-arrow-down")),
          helpText(
            "Downloads the cloud archive and extracts it into the local folder selected above."
          ),
          if (status %in% c("failed", "cancelled")) {
            tagList(br(),
                    actionButton("cloud_download_partial",
                                 "Download and prepare completed runs only",
                                 class = "btn-warning", width = "100%",
                                 icon = icon("box-open")))
          },
          br(), br(),
          actionButton("cloud_dismiss", "Clear run status from this page",
                       class = "btn-outline-secondary btn-sm", width = "100%",
                       icon = icon("xmark")),
          helpText(
            "This clears only the status shown in the app. It does not delete results from Google Cloud Storage."
          )
        )
      }
    )
  }
  
  # The View Result button appears once a cloud test has finished and before
  # its result has been fetched. It is the only control on these tabs that
  # triggers a download; everything else about the result is drawn only
  # after it succeeds.
  .cloud_view_button <- function(expected_task) {
    if (!identical(proc_state$cloud_task_type, expected_task)) return(NULL)
    if (!isTRUE(proc_state$cloud_result_ready)) return(NULL)
    tagList(
      br(),
      actionButton("view_cloud_result", "View Result",
                   class = "btn-primary", width = "100%",
                   icon = icon("eye"))
    )
  }
  
  output$cloud_validation_controls <- renderUI({
    
    if (!identical(proc_state$cloud_task_type, "validation")) {
      return(NULL)
    }
    
    tagList(
      .build_cloud_controls(cancel_id = NULL, show_result_actions = FALSE),
      .cloud_view_button("validation")
    )
  })
  
  
  output$cloud_perf_controls <- renderUI({
    
    if (!identical(proc_state$cloud_task_type, "perfcheck")) {
      return(NULL)
    }
    
    tagList(
      .build_cloud_controls(cancel_id = NULL, show_result_actions = FALSE),
      .cloud_view_button("perfcheck")
    )
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
    
    # The main Step 3b Stop Simulation button handles cloud cancellation,
    # so this panel no longer carries a second cancel button.
    .build_cloud_controls(cancel_id = NULL)
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
        "Please use 'Download and prepare completed runs only' to collect any completed output."
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
    
    # Downloads DO need a real folder. Quietly falling back to tempdir()
    # put the results somewhere the user never chose and R deletes on exit,
    # which looked like a successful download that lost the data.
    if (is.null(input$out_dir) || !nzchar(trimws(input$out_dir))) {
      sys_status$cloud_summary <- paste0(
        "🛑 No download folder has been chosen.\n",
        "Please use \"Choose output folder\" in Simulation Control, then download ",
        "again."
      )
      showNotification(
        "Please choose an output folder before downloading.",
        type = "error", duration = 10
      )
      return()
    }
    
    dest <- file.path(trimws(input$out_dir), jid)
    
    showNotification("Downloading results...", id = "cloud_dl", duration = NULL)
    res <- tryCatch({
      auth <- cloud_token()
      cloud_download_results(auth, cs$bucket, jid, dest)
    }, error = function(e) list(pass = FALSE, msg = conditionMessage(e)))
    removeNotification("cloud_dl")
    
    sys_status$cloud_summary <- res$msg
    showNotification(res$msg, type = if (isTRUE(res$pass)) "message" else "error",
                     duration = 10)
    
    if (isTRUE(res$pass)) {
      .craibm_clear_cloud()
      updateTextInput(session, "res_out_dir", value = dest)
    }
  })
  
  observeEvent(input$cloud_download_partial, {
    cs <- cloud_settings()
    jid <- proc_state$cloud_job_id
    req(!is.null(jid))
    
    # Downloads DO need a real folder. Quietly falling back to tempdir()
    # put the results somewhere the user never chose and R deletes on exit,
    # which looked like a successful download that lost the data.
    if (is.null(input$out_dir) || !nzchar(trimws(input$out_dir))) {
      sys_status$cloud_summary <- paste0(
        "🛑 No download folder has been chosen.\n",
        "Please use \"Choose output folder\" in Simulation Control, then download ",
        "again."
      )
      showNotification(
        "Please choose an output folder before downloading.",
        type = "error", duration = 10
      )
      return()
    }
    
    dest <- file.path(trimws(input$out_dir), paste0(jid, "_partial"))
    
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
  
  
  # Sits directly above Start Simulation Run. In local mode it explains why
  # the run cannot start; in cloud mode it explains that the folder is not
  # needed to run but will be needed to download, so the difference is
  # visible before the button is pressed rather than after.
  output$out_dir_required_note <- renderUI({
    if (!is.null(input$out_dir) && nzchar(trimws(input$out_dir))) {
      return(NULL)
    }
    
    tags$div(
      style = paste0(
        "margin-bottom:10px; padding:10px; ",
        "border:2px solid #dc3545; border-radius:4px; ",
        "background-color:#fdecea;"
      ),
      tags$div(
        style = "color:#b02a37; font-weight:800; font-size:14px;",
        "🛑 Please choose an output folder before starting."
      ),
      tags$div(
        style = "color:#b02a37; font-size:13px; margin-top:6px;",
        "Please use ",
        tags$b("Choose output folder"),
        " above to setup the file path."
      )
    )
  })
  
  batch_plan <- reactive({
    
    cloud_mode <- isTRUE(input$use_cloud)
    
    out_dir_raw <- if (is.null(input$out_dir)) {
      ""
    } else {
      trimws(as.character(input$out_dir)[1])
    }
    
    missing_steps <- get_missing_setup_steps()
    
    origin_line <- if (!is.null(sys_status$loaded_from)) {
      paste0("📂 ", sys_status$loaded_from, "\n\n")
    } else ""
    
    # Test 2 reports a recommendation; it no longer blocks the full run.
    # The verdict is turned into one line of the task preview below and is
    # not repeated anywhere else in the application.
    perf_status <- if (identical(sys_status$mem_safe, FALSE)) {
      "high_risk"
    } else if (
      isTRUE(sys_status$memory_check_done) &&
      isTRUE(sys_status$mem_safe) &&
      !isTRUE(sys_status$memory_retest_required)
    ) {
      "confirmed"
    } else if (isTRUE(sys_status$memory_retest_required)) {
      "not_current"
    } else {
      "not_run"
    }
    
    perf_note <- switch(
      perf_status,
      
      confirmed = paste0(
        "✅ Test 2 confirmed: speed and memory were checked for these ",
        "parallel settings.\n"
      ),
      
      high_risk = paste0(
        "🛑 Test 2 warning: this parallel plan may exceed the machine's ",
        "available memory. The run may fail, and cloud charges would still ",
        "apply. Lowering concurrent replicate workers in Step 2 is recommended.\n"
      ),
      
      not_current = paste0(
        "⚠️ Test 2 is out of date: the parallel settings changed after ",
        "the last check, so speed and memory are not confirmed.\n"
      ),
      
      paste0(
        "⚠️ Test 2 has not been run for these settings, so speed, memory ",
        "use and machine suitability are not confirmed.\n"
      )
    )
    
    
    # The missing-setup list and the Test 2 note are independent. Test 2 is a
    # recommendation, so its verdict is shown here as well: an incomplete
    # setup must not hide it, and it must never appear in the missing list.
    if (length(missing_steps) > 0L) {
      return(list(
        valid = FALSE,
        msg = paste0(
          origin_line,
          "🚧 Setup is incomplete.\nMissing:\n - ",
          paste(missing_steps, collapse = "\n - ")
        ),
        perf_status = perf_status,
        perf_note = perf_note
      ))
    }
    
    n_iter_check <- suppressWarnings(
      as.integer(input$n_iter)
    )
    
    n_cores_check <- suppressWarnings(
      as.integer(input$n_cores)
    )
    
    if (
      length(n_iter_check) != 1L ||
      is.na(n_iter_check) ||
      n_iter_check < 1L ||
      length(n_cores_check) != 1L ||
      is.na(n_cores_check) ||
      n_cores_check < 1L
    ) {
      return(list(
        valid = FALSE,
        msg = paste0(
          origin_line,
          "🚧 Run Control is incomplete.\n",
          "Please enter the number of iterations and worker processes, ",
          "then click Confirm Run Control in Step 2."
        ),
        perf_status = perf_status,
        perf_note = perf_note
      ))
    }
    if (!nzchar(out_dir_raw)) {
      return(list(
        valid = FALSE,
        msg = paste0(
          origin_line,
          "🛑 No output folder has been chosen.\n",
          "Please use \"Choose output folder\" above, then start the run again."
        ),
        perf_status = perf_status,
        perf_note = perf_note
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
      normalizePath(out_dir_raw, mustWork = FALSE)
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
        paste0("ON, ", policy_threads_active, " thread(s) per replicate worker")
      else "OFF (policies run sequentially)",
      "\n",
      "   3. Individual parallelism: ",
      if (omp_threads_active > 1L)
        paste0("ON, ", omp_threads_active, " OpenMP thread(s) per policy calculation")
      else "OFF (standard method)",
      "\n"
    )
    
    # Total concurrent threads at peak = active workers x policy x individual.
    peak_threads <- effective_workers * policy_threads_active * omp_threads_active
    total_line <- paste0(
      "🧮 Maximum software CPU work slots: ", effective_workers, " x ",
      policy_threads_active, " x ", omp_threads_active, " = ", peak_threads,
      " (R processes x policy threads x individual-level threads)\n"
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
      total_tasks_count = total_tasks_count,
      perf_status = perf_status,
      perf_note = perf_note
    )
  })
  
  
  
  
  
  output$task_preview <- renderText({
    plan <- batch_plan()
    return(unname(plan$msg))
  })
  
  # The Test 2 verdict is rendered separately from the monospace preview so
  # it can carry weight and colour. verbatimTextOutput cannot show either,
  # and a plain grey line was too easy to scroll past.
  output$task_preview_warning <- renderUI({
    plan <- batch_plan()
    st <- plan$perf_status
    if (is.null(st)) return(NULL)
    loaded_test2_note <- if (
      isTRUE(sys_status$test2_loaded_from_settings) &&
      !identical(st, "not_run")
    ) {
      tags$div(
        style = paste0(
          "font-size:12px; margin-top:6px; margin-bottom:0; ",
          "padding:6px 8px; border-left:3px solid #6c757d; ",
          "color:#495057; font-weight:normal; font-style:italic;"
        ),
        paste0(
          "(This Test 2 result was loaded from saved data. ",
          "Please confirm that this is the same computer and that the ",
          "parallel configuration is unchanged.)"
        )
      )
    } else {
      NULL
    }
    if (identical(st, "confirmed")) {
      return(
        tagList(
          tags$div(
            class = "alert alert-success",
            style = "padding:8px; margin-top:10px; margin-bottom:0;",
            tags$b(
              "✅ Test 2 confirmed: speed and memory were checked for these ",
              "parallel settings."
            )
          ),
          # Outside the green box, not inside it. Nested, it read as part of
          # the confirmation; the point of the line is that the
          # confirmation came from somewhere else.
          loaded_test2_note
        )
      )
    }
    
    headline <- switch(
      st,
      high_risk = paste0(
        "🛑 Test 2 WARNING: this parallel plan may exceed the memory ",
        "available on this machine."
      ),
      not_current = paste0(
        "⚠️ Test 2 is OUT OF DATE: the parallel settings changed after ",
        "the last check."
      ),
      paste0(
        "⚠️ Test 2 has NOT been run for these settings."
      )
    )
    
    tags$div(
      style = paste0(
        "margin-top:10px; padding:12px; ",
        "border:2px solid #dc3545; border-radius:4px; ",
        "background-color:#fdecea;"
      ),
      
      tags$div(
        style = "color:#b02a37; font-weight:800; font-size:15px; line-height:1.4;",
        headline
      ),
      
      tags$div(
        style = paste0(
          "color:#b02a37; font-weight:700; font-size:13.5px; ",
          "margin-top:8px; line-height:1.45;"
        ),
        "Speed, memory use and machine suitability are NOT confirmed. If the ",
        "selected parallel settings need more memory than this machine has, ",
        "the simulation can exhaust it and FREEZE OR CRASH both this ",
        "application and your computer. Unsaved work may be lost, and a ",
        "cloud run would still be charged for the time."
      ),
      
      tags$div(
        style = "color:#721c24; font-size:12px; margin-top:8px;",
        "Running Test 2 (Parallel Performance Check) in Step 3a first is ",
        "strongly recommended. The full simulation is not blocked, but you ",
        "are starting it without that check."
      ),
      
      loaded_test2_note
    )
  })
  
  output$batch_log <- renderText({ unname(sys_status$batch_log) })
  
  for (output_id in c(
    "step1_info_box",
    "settings_load_log",
    "log_survival",
    "log_step1_2",
    "log_step1_3",
    "log_runcontrol",
    "log_step2a",
    "log_oversub",
    "task_preview",
    "task_preview_warning",
    "batch_log"
  )) {
    outputOptions(
      output,
      output_id,
      suspendWhenHidden = FALSE
    )
  }
  
  .set_active_run <- function(kind, mode = "local") {
    proc_state$active_run      <- kind          # validation / perfcheck / full
    proc_state$active_run_mode <- mode          # local / cloud
  }
  
  .clear_active_run <- function() {
    proc_state$active_run      <- NULL
    proc_state$active_run_mode <- NULL
  }
  
  # NOTE: there is deliberately no observer disabling run_test_sim and
  # run_oversub_test. Locking them added no protection -- cloud_submit()
  # already refuses a second cloud job, and a local run blocks R anyway --
  # but it could leave them dead if a release failed, which is exactly what
  # happened after a foreground full run. active_run is kept purely as
  # bookkeeping for the cloud collector.
  
  # The lock-explanation panels were removed with the lock itself: with no
  # button ever disabled, there is nothing left to explain.
  
  # Kept as a no-op so every existing call site stays valid. Button state is
  # owned entirely by the observer below.
  #
  # Why it must not do the work itself: this function is called from the
  # on.exit handler of the foreground run, which executes inside
  # session$onFlushed(). There is no reactive context and no session domain
  # there, so shinyjs::enable() can fail. When it did, the button stayed
  # disabled in the browser, a disabled button sends no event, no flush ever
  # followed, and nothing could release it -- the page looked alive but
  # answered nothing.
  sync_batch_buttons <- function(is_running, mode = NULL) {
    invisible(NULL)
  }
  
  # Single owner of the two Step 3b buttons. As an ordinary observer this
  # always runs with a reactive context and a session domain, and it re-runs
  # whenever the state it reads changes -- including the writes made from
  # on.exit. A stuck button is therefore not possible.
  observe({
    running_local <- isTRUE(proc_state$is_running)
    
    cloud_full_active <- (
      identical(proc_state$cloud_task_type, "full") &&
        !is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")
    )
    
    # Start
    if (running_local || cloud_full_active) {
      shinyjs::disable("start_batch")
    } else {
      shinyjs::enable("start_batch")
    }
    
    # Stop. A local foreground run cannot be interrupted, so it stays off
    # there; a background process can be killed and a cloud job cancelled.
    local_background_active <- (
      running_local &&
        identical(input$run_mode, "background") &&
        !is.null(proc_state$job)
    )
    
    if (cloud_full_active || local_background_active) {
      shinyjs::enable("stop_batch")
    } else {
      shinyjs::disable("stop_batch")
    }
  })
  
  # ==========================================================================
  # START button: dispatch to foreground or background based on run_mode
  # ==========================================================================
  observeEvent(input$start_batch, {
    if (!.real_click("start_batch")) return()
    
    plan <- batch_plan()
    
    if (!plan$valid) {
      sys_status$batch_log <- paste0(
        "🛑 Start aborted.\n",
        plan$msg
      )
      showNotification(
        if (grepl("No output folder", plan$msg, fixed = TRUE)) {
          "Cannot start: please choose an output folder first."
        } else {
          "Cannot start: required model setup is incomplete."
        },
        type = "error"
      )
      return()
    }
    
    cloud_mode <- isTRUE(input$use_cloud)
    # Used by both modes now: a cloud run writes its launch snapshot here
    # even though its results are computed remotely.
    out_dir_base <- plan$out_dir_path
    
    # A Google Cloud run writes to its own job-specific Storage prefix. Local
    # folder existence, contents and Overwrite therefore must not block it.
    if (!cloud_mode) {
      
      # Defensive: batch_plan() already refuses a blank folder, so this
      # should be unreachable. It reports rather than returning quietly,
      # because a silent return here is exactly what made the button look
      # broken before.
      if (!nzchar(out_dir_base)) {
        sys_status$batch_log <- paste0(
          "🛑 Start aborted.\n",
          "No output folder has been chosen. Please use \"Choose output folder\" ",
          "above to set up the file path."
        )
        showNotification(
          "Cannot start: choose an output folder first.",
          type = "error"
        )
        return()
      }
      
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
            "Please check 'Overwrite' to proceed."
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
    snap_rm_vec <- parse_num_vec(
      .confirmed_value(
        confirmed$design,
        "rm_vec"
      )
    )
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
      
      # Register the cloud full run before leaving this observer. Without
      # this the Start buttons never lock and the Step 3b Stop button never
      # becomes usable, because the cloud branch returns early.
      .set_active_run("full", "cloud")
      
      # Launch snapshot, in the folder the user chose. A local run has always
      # written one of these; a cloud run needs it more, because the job id it
      # records is the only offline way back to a run that finishes days later.
      #
      # The id is injected rather than read back from the input: the box on
      # screen is filled with updateTextInput(), which is a message to the
      # browser and does not change input$cloud_job_id_manual until the next
      # round trip. Reading it here would save the previous value, or none.
      cloud_snapshot_line <- tryCatch({
        if (!dir.exists(out_dir_base)) {
          dir.create(out_dir_base, recursive = TRUE, showWarnings = FALSE)
        }
        snap_name <- paste0(
          "work data saved on ",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".rds"
        )
        snap <- collect_settings()
        snap$inputs$cloud_job_id_manual <- sub$job_id
        saveRDS(snap, file.path(out_dir_base, snap_name))
        paste0(
          "💾 Settings snapshot saved: ", snap_name, "\n",
          "========================================\n"
        )
      }, error = function(e) {
        paste0(
          "⚠️ The settings snapshot could not be written: ",
          conditionMessage(e), "\n",
          "========================================\n"
        )
      })
      
      sys_status$batch_log <- paste0(
        "\u2601\ufe0f SIMULATION SUBMITTED TO GOOGLE CLOUD\n",
        "========================================\n",
        "Job: ", sub$job_id, "\n",
        "Machine: ", input$gcp_machine_type, "\n",
        "Total runs: ", plan$total_tasks_count, "\n",
        "Workers: ", plan$actual_cores, "\n",
        "========================================\n",
        cloud_snapshot_line,
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
      " | Individual-level threads per policy calculation=", omp_label,
      " | Policy-combination threads per replicate worker=", user_policy_threads,
      " | Concurrent replicate workers=", plan$actual_cores,
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
      
      # Process-level record, so a later session can pick this run back up.
      # Holding the handle here is also what stops the garbage collector
      # from killing the child once this session goes away.
      .craibm_runs$local <- list(
        job = proc_state$job,
        out_dir = out_dir_base,
        cores = plan$actual_cores,
        settings_log_line = auto_settings_log_line,
        # Every local run already writes a full settings snapshot next to
        # its output. Remembering the path means a later session can tell
        # the user exactly which file brings their parameters back.
        settings_path = if (exists("auto_settings_path", inherits = FALSE)) {
          auto_settings_path
        } else {
          NULL
        },
        started = Sys.time()
      )
      
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
        
        # This whole block runs outside any reactive context and outside the
        # tryCatch below, so an error here would reach Shiny directly and
        # close the session. Every step is therefore individually guarded.
        on.exit({
          # Restore stderr. ONE call, never a loop.
          #
          # sink.number(type = "message") returns the CONNECTION NUMBER in
          # use for messages, not a nesting depth. With nothing diverted it
          # is 2 (stderr), so a "while (sink.number(...) > 0)" loop never
          # terminates: R spins here forever, the event loop never resumes,
          # no output is flushed and no button responds. Repeating the call
          # is harmless anyway, so a single guarded call is correct.
          try(sink(type = "message"), silent = TRUE)
          
          if (!is.null(err_con) && isOpen(err_con)) {
            try(close(err_con), silent = TRUE)
          }
          if (!is.null(cl)) {
            try(parallel::stopCluster(cl), silent = TRUE)
          }
          
          try(removeNotification(run_notif_id), silent = TRUE)
          
          # Record the state only. Writing reactiveValues without a reactive
          # context is legal and invalidates the button observer, which then
          # re-enables everything on the next flush. Nothing here touches a
          # button directly, so nothing here can leave one stuck.
          try({
            proc_state$is_running <- FALSE
            .clear_active_run()
          }, silent = TRUE)
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
              "Please check sim_info.log for the failed jobs."
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
              "Please check sim_info.log for details."
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
            "Please check sim_info.log in the output folder."
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
    if (!.real_click("stop_batch")) return()
    
    
    # ------------------------------------------------------------
    # Case 1: a Full Simulation running on Google Cloud
    # ------------------------------------------------------------
    cloud_full_active <- (
      identical(proc_state$cloud_task_type, "full") &&
        !is.null(proc_state$cloud_status) &&
        proc_state$cloud_status %in% c("submitted", "running")
    )
    
    if (cloud_full_active) {
      .show_cloud_cancel_modal("full")
      return()
    }
    
    # ------------------------------------------------------------
    # Case 2: a local background process
    # ------------------------------------------------------------
    if (is.null(proc_state$job) || !isTRUE(proc_state$job$is_alive())) {
      showNotification(
        "There is no stoppable full simulation running.",
        type = "warning"
      )
      return()
    }
    
    proc_state$job$kill()
    
    sys_status$batch_log <- paste0(
      "🛑 SIMULATION STOPPED\n",
      "The local background process was terminated.\n",
      "Results completed before cancellation remain in the output folder."
    )
    
    showNotification(
      "Simulation stopped. Completed output files were kept.",
      type = "warning",
      duration = 10
    )
    
    if (!is.null(proc_state$bg_out_dir)) {
      trash_file <- file.path(
        proc_state$bg_out_dir,
        ".pkg_load_trash.log"
      )
      try(file.remove(trash_file), silent = TRUE)
    }
    
    proc_state$is_running        <- FALSE
    proc_state$job               <- NULL
    proc_state$bg_out_dir        <- NULL
    proc_state$bg_cores          <- NULL
    proc_state$bg_settings_log_line <- NULL
    .craibm_clear_local()
    
    # Release the global Start-button lock.
    .clear_active_run()
    sync_batch_buttons(FALSE, "background")
  })
  
  bg_dir_safe <- function() {
    tryCatch(isolate(proc_state$bg_out_dir), error = function(e) NULL)
  }
  
  # ==========================================================================
  # Background watchdog: poll every 1 second to detect completion
  # ==========================================================================
  observe({
    req(proc_state$is_running, !is.null(proc_state$job))
    invalidateLater(1000)
    
    # This observer re-runs every second for the whole life of a background
    # run, and an error inside observe() does not show as a red message: it
    # ends the session. Without the wrapper a single bad second -- a handle
    # that has gone stale, a result object in an unexpected shape -- turns
    # into a grey screen. Recovery makes that worth guarding properly, since
    # the handle may now have been created by an earlier session.
    tryCatch({
      
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
          "Please use the Stop button to cancel."
        )
      } else {
        # Process ended — collect result.
        # Everything from here to the end of the observer is wrapped by the
        # caller-side guard below; get_result() itself is the most likely
        # source of a surprise, so it keeps its own try().
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
            "Please check sim_info.log in the output folder."
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
            "Please check sim_info.log for the failed jobs."
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
            "Please check sim_info.log for details."
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
            "Please check sim_info.log in the output folder."
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
        .craibm_clear_local()
        
        # Release the global run lock. Without this the three Start buttons
        # stayed disabled for the rest of the session after a background run.
        .clear_active_run()
        sync_batch_buttons(FALSE, "background")
      }
    }, error = function(e) {
      # Stop the loop first. is_running gates this observer, so leaving it
      # TRUE would repeat the same error once a second forever.
      proc_state$is_running <- FALSE
      proc_state$job <- NULL
      try(.craibm_clear_local(), silent = TRUE)
      try(.clear_active_run(), silent = TRUE)
      try(sync_batch_buttons(FALSE, "background"), silent = TRUE)
      
      sys_status$batch_log <- paste0(
        "\u26a0\ufe0f Lost contact with the background simulation.\n",
        conditionMessage(e), "\n",
        "This page has stopped following it. The simulation itself may "
        , "still be running: check the output folder for new files.\n",
        if (is.null(bg_dir_safe())) "" else paste0("Output folder: ", bg_dir_safe())
      )
    })
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
    
    # transient_years is blank until Step 1 has been filled in, and the
    # results tab can be used on its own with previously saved output. A
    # missing value therefore means "no burn-in to mark" rather than an
    # error: it becomes 0, which draws the marker at the origin and keeps
    # every comparison against it numeric. It can still be set directly
    # on this tab.
    if (is.null(val) || length(val) != 1L || is.na(val)) {
      val <- 0
    }
    
    updateNumericInput(session, "res_burn_in", value = val)
    valid_burn_in_val(val)
  })
  # 2. Load Scenarios (Scanning)
  observeEvent(input$load_results, {
    if (!.real_click("load_results")) return()
    
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
  # server = FALSE: the rows travel with the page instead of being fetched
  # from a per-session Ajax endpoint. That endpoint dies with its session,
  # so after a laptop sleeps or a socket drops the browser was left asking
  # a dead address for its data and reporting "Ajax error" with nothing but
  # a header drawn. These tables are small enough that sending them whole
  # costs nothing and removes the dependency entirely.
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
  }, server = FALSE)
  # ---------------------------------------------------------------------
  # Recovery
  #
  # Runs once per session. If this R process already has a simulation in
  # progress -- or one that finished while nobody was watching -- the
  # interface is put back into that state instead of coming up blank.
  #
  # The ENTIRE body is inside tryCatch, and that is not defensive padding.
  # onFlushed callbacks run with no reactive context and outside any
  # observer, so an error raised here does not surface as a red message: it
  # escapes into the flush cycle and closes the session. That is precisely
  # how the grey screen at the end of a foreground run used to happen. A
  # malformed record -- an interrupted write, a file from an older version --
  # must degrade into 'start clean', never into an app that greys out every
  # time it opens.
  # ---------------------------------------------------------------------
  session$onFlushed(function() {
    shiny::isolate({
      tryCatch({
        if (
          isTRUE(resume_same_app_process) &&
          is.list(.craibm_runs$work)
        ) {
          
          try(
            .apply_settings(
              .craibm_runs$work,
              notify = FALSE,
              restore_mode = "reconnect"
            ),
            silent = TRUE
          )
        }
        .usable <- function(rec) {
          !is.null(rec) &&
            is.list(rec) &&
            inherits(rec$started, "POSIXct") &&
            length(rec$started) == 1L &&
            !is.na(rec$started)
        }
        .usable_cloud <- function(rec) {
          !is.null(rec) && is.list(rec) &&
            !is.null(rec$job_id) && nzchar(rec$job_id)
        }
        
        local_rec <- tryCatch(.craibm_pending_local(), error = function(e) NULL)
        if (!.usable(local_rec)) local_rec <- NULL
        
        if (!is.null(local_rec)) {
          proc_state$job <- local_rec$job
          proc_state$bg_out_dir <- local_rec$out_dir
          proc_state$bg_cores <- local_rec$cores
          proc_state$bg_settings_log_line <- local_rec$settings_log_line
          proc_state$is_running <- TRUE
          .set_active_run("full", "local")
          
          # is_running stays TRUE even for a job that has already finished:
          # the one-second watchdog is gated on it, and that watchdog is what
          # collects the result, writes the log and releases the buttons.
          # Setting it here hands an orphaned run back to the normal
          # machinery rather than duplicating any of it.
          mins <- round(as.numeric(
            difftime(Sys.time(), local_rec$started, units = "mins")))
          
          sys_status$batch_log <- paste0(
            "🔁 Reconnected to a background simulation started in an ",
            "earlier session.\n",
            "========================================\n",
            "Started: ", format(local_rec$started, "%Y-%m-%d %H:%M"),
            "  (", mins, " minute(s) ago)\n",
            "Output folder: ", local_rec$out_dir, "\n",
            if (isTRUE(local_rec$finished)) {
              "Status: finished. Collecting the result now...\n"
            } else {
              "Status: still running. This page will report when it finishes.\n"
            },
            # Points at the file, without narrating what the app did or why.
            if (!is.null(local_rec$settings_path) &&
                file.exists(local_rec$settings_path)) {
              paste0(
                "----------------------------------------\n",
                "Settings file restored from reconnect cache: ",
                local_rec$settings_path
              )
            } else ""
          )
          
          showNotification(
            if (isTRUE(local_rec$finished)) {
              "A background simulation finished while you were away."
            } else {
              "Reconnected to a background simulation that is still running."
            },
            type = "message", duration = 12
          )
        }
        
        
        
        cloud_rec <- tryCatch(.craibm_pending_cloud(), error = function(e) NULL)
        if (!.usable_cloud(cloud_rec)) cloud_rec <- NULL
        
        if (!is.null(cloud_rec)) {
          
          # The ID goes into the lookup box either way, so that a failure here
          # still leaves the user one button press from picking the job up.
          updateTextInput(
            session,
            "cloud_job_id_manual",
            value = cloud_rec$job_id
          )
          
          if (!is.null(cloud_rec$task_type) &&
              cloud_rec$task_type %in% c("validation", "perfcheck", "full")) {
            updateSelectInput(
              session,
              "cloud_job_type_manual",
              selected = cloud_rec$task_type
            )
          }
          
          # ------------------------------------------------------------
          # Reattach to the job.
          #
          # Only when the same R process is resuming: the key lives in this
          # process and is never written to disk, so a genuinely new process
          # has no credentials to reconnect with and the box above is all it
          # can offer.
          #
          # This reports because it is the one thing here the user cannot see
          # for themselves. A cloud job carries on regardless of this app, so
          # after a reconnection the honest questions are whether the app is
          # watching it again and, if not, why -- and a silent failure would
          # leave a run apparently abandoned when it is running perfectly well.
          # ------------------------------------------------------------
          if (isTRUE(resume_same_app_process)) {
            
            cs_now <- tryCatch(isolate(cloud_settings()), error = function(e) NULL)
            
            have_key <- !is.null(cs_now) &&
              !is.null(cs_now$key_path) &&
              nzchar(cs_now$key_path) &&
              file.exists(cs_now$key_path)
            
            if (isTRUE(have_key)) {
              
              sys_status$log_cloud <- paste0(
                "🔁 Reconnecting to Google Cloud after the connection ",
                "was interrupted...\n",
                "Job: ", cloud_rec$job_id
              )
              
              res <- tryCatch({
                auth <- cloud_token()
                cloud_check_setup(
                  auth, cs_now$project, cs_now$region, cs_now$bucket
                )
              }, error = function(e) {
                list(pass = FALSE, msg = conditionMessage(e))
              })
              
              proc_state$cloud_verified <- isTRUE(res$pass)
              
              if (isTRUE(res$pass)) {
                
                attached <- isTRUE(
                  tryCatch(
                    .cloud_attach_job(
                      cloud_rec$job_id,
                      cloud_rec$task_type,
                      cloud_rec$started,
                      cloud_rec$result_uri
                    ),
                    error = function(e) FALSE
                  )
                )
                
                sys_status$log_cloud <- if (isTRUE(attached)) {
                  paste0(
                    "✅ Google Cloud connection restored.\n",
                    "Saved Job ID: ", cloud_rec$job_id, "\n",
                    "The app is following this job again. Its status will update ",
                    "automatically in the Cloud panel."
                  )
                } else {
                  paste0(
                    "⚠️ Google Cloud was reached, but the saved job could not be ",
                    "followed automatically.\n",
                    "Saved Job ID: ", cloud_rec$job_id, "\n",
                    "Please check the Cloud settings and network connection. If needed, ",
                    "upload the JSON key again, click 'Check cloud connection', and then ",
                    "click 'Check this job'."
                  )
                }
                
              } else {
                sys_status$log_cloud <- paste0(
                  "❌ Automatic Google Cloud reconnection failed.\n",
                  "Saved Job ID: ", cloud_rec$job_id, "\n",
                  if (
                    !is.null(res$msg) &&
                    length(res$msg) == 1L &&
                    nzchar(res$msg)
                  ) {
                    paste0("Reason: ", res$msg, "\n")
                  } else {
                    ""
                  },
                  "The Cloud job is unaffected and may still be running.\n",
                  "Please check the network connection and Cloud settings. If needed, ",
                  "upload the JSON key again, click 'Check cloud connection', and then ",
                  "click 'Check this job'."
                )
              }
            } else {
              
              sys_status$log_cloud <- paste0(
                "❌ Automatic Cloud reconnection could not be completed.\n",
                "The saved service-account key is no longer available in this R session.\n",
                "Please check the Cloud settings, upload the JSON key again, click ",
                "'Check cloud connection', and then try checking the saved Job ID again."
              )
            }
          }
        }
        
      }, error = function(e) {
        # Recovery is a convenience. If it cannot be done, the app must still
        # open normally, so the records are discarded and the failure is
        # reported quietly.
        
        try(.craibm_clear_local(), silent = TRUE)
        try(.craibm_clear_cloud(), silent = TRUE)
        
        message(
          "craibm: could not restore a previous run: ",
          conditionMessage(e)
        )
        
      })
      
    })  # closes shiny::isolate()
    
  }, once = TRUE)
  
}
shinyApp(ui, server)
