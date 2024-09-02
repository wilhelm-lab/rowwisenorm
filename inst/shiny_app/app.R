#
# This is a Shiny web application. You can run the application by calling
# the function 'run_app' from the package rowwisenorm.
#
# OMICS DATA NORMALIZATION
#


library(shiny)
library(rowwisenorm)  # make available
library(pheatmap)

library(edgeR)  # for VST
# library(lumi)  # for VSN
library(limma)  # for VSN
library(preprocessCore)  # for Quantile normalization
library(sva)  # for ComBat + some sva functions part of M-ComBat code

library(shinyjs)  # to handle local parameter: shinyjs::show and shinyjs::hide to adjust the options for downloads

# setting upload size to 100 MB max
options(shiny.maxRequestSize=100*1024^2)


ui <- fluidPage(

    useShinyjs(),  # must be called in the UI in order for all shinyjs functions to work (such as show/hide)

    # graphic adjustment
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}")),  # horizontal line thicker
    ),

    # colors & line spacing of warnings/ errors inside notifications
    tags$head(tags$style("#reading_warning{color: orange; margin-bottom: 20px;")),
    tags$head(tags$style("#reading_error{color: red; margin-bottom: 20px;}")),
    tags$head(tags$style("#normalize_row_warning{color: red; margin-bottom: 20px;}")),
    tags$head(tags$style("#normalization_error{color: red; margin-bottom: 20px;}")),  # used in each normalization to catch an error

    tags$head(tags$style("#datafile_error{color: red; margin-bottom: 20px;}")),
    tags$head(tags$style("#designfile_error{color: red; margin-bottom: 20px;}")),

    # colors & line spacing of notifications of colors, symbols, M-ComBat center
    tags$head(tags$style("#batch_colors_manually_notification{color: darkblue; margin-bottom: 20px;}")),
    tags$head(tags$style("#condition_symbols_manually_notification{color: darkblue; margin-bottom: 20px;}")),
    tags$head(tags$style("#m.combat_notification{color: darkblue; margin-bottom: 20px;}")),

    # color and thickness of lines subtitles
    tags$style(HTML(".title-hr {
      background-color: rgba(0, 0, 139, 0.5); /* Transparent dark blue color */
      height: 1.3px; /* Adjust thickness here */
    }")),

    # text style for PCA score titles and scores
    tags$style("#score_title_raw{color: darkblue;
            font-size: 14px;
            font-weight: bold;}"),
    tags$style("#score_title_raw_pre{color: darkblue;
            font-size: 14px;
            font-weight: bold;}"),
    tags$style("#score_title_norm{color: darkblue;
            font-size: 14px;
            font-weight: bold;}"),
    tags$style("#score_raw{color: black;
            font-size: 16px;
            font-weight: bold;}"),
    tags$style("#score_raw_pre{color: black;
            font-size: 16px;
            font-weight: bold;}"),
    tags$style("#score_norm{color: black;
            font-size: 16px;
            font-weight: bold;}"),

    # color for feature finding checkboxes
    tags$style(HTML("#onlyBySiteCheckbox { color: green;}")),
    tags$style(HTML("#reverseCheckbox { color: green; }")),
    tags$style(HTML("#contaminantCheckbox { color: green; }")),

    # overall appearance
    tags$style(HTML('
      body {
        background: rgba(204, 229, 255, 0.7); /* Blue transparent general background */
      }
      .navbar {
        background: #99ccff; /* Slightly darker but still light blue for the navigation bar */
      }
      .nav-tabs {
        border-bottom: 3px solid #66aaff; /* border between tab bar and content */
      }
      .nav-tabs li a {
        background: #99ccff; /* tab headers */
      }
      .nav-tabs li.active a {
        background: #66aaff; /* active tab */
      }
      table {
        border: 2px solid #00008B; /* Dark blue border for the table */
      }
      th, td {
        border: 1px solid #00008B; /* Dark blue border for table headers and data cells */
      }
      table thead th {
        border: 1px solid #00008B; /* Dark blue border for lines between headers and data cells */
      }
      .fancy-title {  /* design for the title */
          font-family: "Arial Black", Gadget, sans-serif; /* Custom font */
          font-size: 30px; /* Custom font size */
          color: #E5E8E8; /* Text color */
          background-color: #284D8E; /* Background color */
          padding: 10px 20px; /* Padding to style the title area */
          border-radius: 8px; /* Rounded corners */
        }
      .column {
        background: #e0e0e0; /* Light tint for column backgrounds */
        border: 1px solid #ddd; /* Delicate gray border */
      }
    ')),


    # fixed position of the title tab titles, and fixed width of view plots
    tags$head(
      tags$style(
        HTML("
           .fixed-height {  /* used for first two tabs height */
              height: 80vh;  /* set height as 80% viewerport height (adjusted to the window) */
              overflow-y: auto;
              overflow-x: hidden;  /* no horizontal scroll */
           }
           .fixed-height-width {  /* used for third tab height and width */
              height: 80vh;  /* set height as 80% viewerport height (adjusted to the window) */
              width: 1500px;  /* fixed width */
              overflow-y: auto;
              overflow-x: hidden;
           }
           /* fix the position of the title */
           .fixed-title {
              position: sticky;
              top: 0;
              z-index: 1000; /* ensure that title stays on top */
           }
           ")
      )
    ),

    # widths of view plots columns
    tags$style(HTML(".view-plots-column { width: 500px; }")),

    # Application title - fixed on top of app
    div(class = "fixed-title",
        titlePanel(
          div(class = "fancy-title", "Omics Data Normalization"),
          windowTitle = "Omics Data Normalization"
        ),
    ),

    # part the ui in three parts
    tabsetPanel(
      # left tab
      tabPanel("Input and Settings",
               div(class = "fixed-height",
                 # fluid row for the numbers 1 to 3
                 fluidRow(
                   style = "margin-top: 10px;", # Add margin to the top of the fluidRow  #284D8E
                   column(4, align = "center",
                          div(
                            style = "border: 3px solid #284D8E; border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: rgba(0, 0, 139, 0.4);",
                            h2(
                              "1",
                              style = "line-height: 50px; margin: 0; color: white;"
                            )
                          )
                   ),  # First column with the number 1 inside a circle
                   column(4, align = "center",
                          div(
                            style = "border: 3px solid #284D8E; border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: rgba(0, 0, 139, 0.4);",
                            h2(
                              "2",
                              style = "line-height: 50px; margin: 0; color: white;"
                            )
                          )
                   ),  # Second column with the number 2 inside a circle
                   column(4, align = "center",
                          div(
                            style = "border: 3px solid #284D8E; border-radius: 50%; width: 50px; height: 50px; display: flex; justify-content: center; align-items: center; background-color: rgba(0, 0, 139, 0.4);",
                            h2(
                              "3",
                              style = "line-height: 50px; margin: 0; color: white;"
                            )
                          )
                   )  # Third column with the number 3 inside a circle
                 ),
                 fluidRow(
                   # left
                   column(4,
                          div(
                            h3("Data", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # upload files
                          fileInput(inputId = "data", label = "Upload your data",
                                    accept = c(".csv, .tsv, .txt", "text/*")),
                          textOutput("file_name_data"),

                          fileInput(inputId = "exp_design", label = "Upload the experimental design for your data",
                                    accept = c(".csv, .tsv, .txt", "text/*")),
                          textOutput("file_name_exp_design"),

                         ),
                   # middle
                   column(4,
                          div(
                            h3("Settings", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),
                          # Preprocessing possible for all methods: - but log2 not for VST allowed (no negative values allowed)
                          div(
                            h3("Preprocessing", style = "font-size: 17px; font-weight:550;"),
                            class = "title-div"
                          ),
                          hr(),  # horizontal line

                          # filtering of features - only show a checkbox if feature is available in data
                          textOutput("feature_note"),
                          actionButton("generate_features", "Find Features", icon = icon("search")),
                          # status output with space above
                          uiOutput("generate_features_status", style = "margin-top: 20px;"),

                          uiOutput("feature_text"),
                          uiOutput("onlyBySiteCheckbox"),
                          uiOutput("reverseCheckbox"),
                          uiOutput("contaminantCheckbox"),

                          # log2
                          conditionalPanel(
                            condition = "input.method != 'VST' ",
                            checkboxInput(inputId = "log2_t", label = "Logarithmic transformation", value = TRUE),
                            textOutput("log2_transform"),
                          ),

                          # filter_rows
                          checkboxInput(inputId = "filterrows", label = "Filter out rows", value = FALSE),

                          conditionalPanel(
                            condition = "input.filterrows == true",
                            #textInput(inputId = "filterrowsratio", label = "Optionally: Your desired ratio of valid values per row as a decimal between 0 and 1:", placeholder = "Default: 0.5"),
                            sliderInput(inputId = "filterrowsratio", label = "Your desired ratio of valid values per row:", min=0, max=1, value = 0.5, step = 0.01),
                            textOutput("filterrows_note"),
                          ),

                          # sum normalize
                          conditionalPanel(
                            condition = "input.method != 'total-sum' ",
                            checkboxInput(inputId = "sum_norm", label = "Sum normalize", value = FALSE),
                            textOutput("sum_normal"),
                          ),

                          # parameter for sum normalize - same IDs as for total sum since this preprocessing appears for all methods without total sum
                          conditionalPanel(
                            condition = "input.sum_norm & input.method != 'total-sum' ", # important: set off for method total-sum (otherwise, when clicked at a different method it still appears)
                            # refFunc
                            selectInput(inputId = "refFunc_sum", label = "Select the reference Function", choices = c("sum" = "sum", "median" = "median")),
                            # norm
                            checkboxInput(inputId = "norm_sum", label = "Normalize the total sum", value = TRUE),
                            # na.rm - this ID only here (needs to work for all methods, the other na_rm is used for total sum and row wise in specific setups)
                            checkboxInput(inputId = "na_rm_sum", label = "Exclude NA values inside reference function", value = TRUE),
                          ),

                          # median normalize
                          checkboxInput(inputId = "median_norm", label = "Median normalize", value = FALSE),

                          # choice of normalization method
                          div(
                            h3("Normalization Method", style = "font-size: 17px; font-weight:550;"),
                            class = "title-div"
                          ),
                          hr(),
                          selectInput(inputId = "method", label = "Method",
                                      choices = c("row-wise-normalization" = "row-wise-normalization",
                                                  "total-sum" = "total-sum", "VST" = "VST", "VSN" = "VSN",
                                                  "quantile-normalization" = "quantile-normalization",
                                                  "ComBat" = "ComBat", "M-ComBat" = "M-ComBat")),
                          textOutput("selected_method"),

                          ### Setups for specific methods: - only for row-wise, total sum, and M-Combat
                          conditionalPanel(
                            condition = "input.method == 'row-wise-normalization' || input.method == 'total-sum' || input.method == 'M-ComBat' ",
                            div(
                              h3("Method-Specific Setups", style = "font-size: 17px; font-weight:550;"),
                              class = "title-div"
                            ),
                            hr(),  # horizontal line
                          ),

                          # active mode - only for row-wise
                          conditionalPanel(
                            condition = "input.method == 'row-wise-normalization' ",
                            checkboxInput(inputId = "active_mode", label = "Manually setting reference channels", value = FALSE),
                          ),

                          # if active is set: input of references - this input is later used for ref parameter
                          conditionalPanel(
                            condition = "input.active_mode == true & input.method == 'row-wise-normalization' ",  # important: set only for row-wise (otherwise, when clicked at a different method it still appears)
                            textInput(inputId = "refs", label = "Please enter the condition names of the references, separated by a comma:"),
                            textOutput("possible_refs_note"),
                          ),

                          # na.rm - only for row-wise and total-sum
                          conditionalPanel(
                            condition = "input.method == 'row-wise-normalization' || input.method == 'total-sum' ",
                            checkboxInput(inputId = "na_rm", label = "Exclude NA values inside reference function", value = TRUE),
                          ),

                          # refFunc - only for row-wise (because other default)
                          conditionalPanel(
                            condition = "input.method == 'row-wise-normalization' ",
                            selectInput(inputId = "refFunc", label = "Select the reference Function", choices = c("median" = "median", "sum" = "sum")),
                          ),

                          # specific parameters for total sum - only for total-sum
                          conditionalPanel(
                            condition = "input.method == 'total-sum' ",
                            # refFunc
                            selectInput(inputId = "refFunc_sum", label = "Select the reference Function", choices = c("sum" = "sum", "median" = "median")),
                            # norm
                            checkboxInput(inputId = "norm_sum", label = "Normalize the total sum", value = TRUE),
                            # na.rm - use the same as for row-wise
                            # checkboxInput(inputId = "na_rm_sum", label = "Remove NA values", value = TRUE),
                          ),

                          # parameter center for M-ComBat
                          conditionalPanel(
                            condition = "input.method == 'M-ComBat' ",
                            numericInput("m.combat_center", label = "Center batch:", value = 1, step = 1, min = 1),
                            textOutput("m.combat_center_note")
                          ),

                          ### Graphical adjustment
                          div(
                            h3("Plot Adjustment", style = "font-size: 17px; font-weight:550;"),
                            class = "title-div"
                          ),
                          hr(),
                          # batch colors
                          selectInput("batch_colors_manually", "Optionally: Select colors to be used for the batches inside the PCA plots and the heatmaps",
                                      choices = colors(), multiple = TRUE),
                          textOutput("batch_colors_manually_note"),
                          br(),
                          # condition symbols
                          selectInput("condition_symbols_manually", "Optionally: Select symbols to be used for the conditions inside the PCA plots",
                                      choices = 0:18, multiple = TRUE),
                          textOutput("condition_symbols_manually_note"),
                          plotOutput("pca_symbols_plot", height = "200px", width = "60%"),  # adjusted size
                          br(),

                          ),
                   # right
                   column(4,
                          # process button
                          div(
                            h2("Start calculation", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),
                          textOutput("process_note"),
                          actionButton("process", label = "Process", icon = icon("refresh")),

                          # status output with space above
                          uiOutput("process_status", style = "margin-top: 20px; font-size:15px"),

                          # Notifications (warning and error messages)
                          div(
                            h2("Notifications", style = "font-size: 17px; font-weight:550; color:darkblue"),
                            class = "title-div"
                          ),
                          tags$hr(style="border-color: darkblue;"),  # horizontal line
                          tags$style(HTML("
                            #generate_features_note {color: darkblue;}")),  # color for note

                          textOutput("reading_error"),  # handle stop() call inside reading
                          textOutput("reading_warning"),  # handle warning() inside reading
                          textOutput("normalize_row_warning"),  # not valid reference entered
                          textOutput("datafile_error"),
                          textOutput("designfile_error"),
                          textOutput("generate_features_error"),
                          textOutput("generate_features_note"),

                          # possible error in normalization
                          textOutput("normalization_error"),  # used in each normalization to catch an error

                          # errors in preprocessing
                          textOutput("pre_log_error"),
                          textOutput("pre_filter_error"),
                          textOutput("pre_sum_error"),
                          textOutput("pre_median_error"),

                          # notifications for PCA colors, symbols, and M-ComBat center
                          textOutput("batch_colors_manually_notification"),
                          textOutput("condition_symbols_manually_notification"),
                          uiOutput("m.combat_notification"),  # ui so that color can be set inside renderUI in server
                         ),

                 ),
                ),
             ),
      # middle tab
      tabPanel("Download Results",
               div(class = "fixed-height",
                 fluidRow(
                   # left
                   column(6,
                          # show normalized data
                          div(
                            h3("Preview Normalized Data", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # choice which rows to be shown
                          numericInput("start_row", "Row to start:", value = 1, step = 1),
                          numericInput("end_row", "Row to end:", value = 5, step = 1),

                          # button for showing data
                          actionButton(inputId = "show_data", label = "Show normalized data"),
                          div(style = "height: 20px;"),  # Add 20px of space

                          # display data as a table, fill available space without overlapping right column
                          div(style = "max-width: 100%; max-height: 300px; overflow-x: auto;", tableOutput("data_output")),

                          textOutput("show_data_note")
                          ),
                   # right
                   column(6,
                          # download
                          div(
                            h3("Write Into File", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # download button for outfile lowest-level
                          downloadButton("download_outfile", "Download Data on Lowest Level"),

                          # download button for outfile with additional columns
                          downloadButton("download_outfile_comp", "Download Data Complete"),

                          # only for local mode: download outfile - manually
                          div(
                            id = "local-content-1",
                            checkboxInput(inputId = "writeoutfile", div(icon("star"), "Manually: Download normalized data"), value = FALSE),
                            textOutput("writeout"),

                            conditionalPanel(
                              condition = "input.writeoutfile == true",
                              textInput(inputId = "filename_outfile", label = "Optionally: Your desired file name:"),
                              textInput(inputId = "dir_outfile", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                              selectInput(inputId = "outfile_level", label = "Level of complexity",
                                          choices = c("lowest-level" = "lowest-level", "all-columns" = "all-columns")),
                              actionButton(inputId = "save_outfile", label = "Submit"),
                              verbatimTextOutput("outfile_path")
                            ),
                          ),

                          div(
                            h3("Download Plots", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # show labels parameter for PCA labels
                          checkboxInput(inputId = "show_labels", label = "Show labels inside PCA plot", value = FALSE),

                          # svg parameter
                          checkboxInput(inputId = "svg", label = "Additionally create SVG files", value = FALSE),

                          # download button for PDF (and svg) raw
                          downloadButton("download_pdf_raw", "Download Plots Raw Data"),

                          # download button for PDF (and svg) raw pre-processed
                          downloadButton("download_pdf_raw_pre", "Download Plots Raw Data Pre-Processed"),

                          # download button for PDF (and svg) normalized
                          downloadButton("download_pdf_norm", "Download Plots Normalized Data"),

                          # only for local mode: manual downloads
                          div(
                            id = "local-content-2",
                            # download plots raw - manually
                            checkboxInput(inputId = "save_plots_raw", div(icon("star"), "Manually: Save plots for raw data"), value = FALSE),
                            textOutput("saving_plots_raw"),

                            conditionalPanel(
                              condition = "input.save_plots_raw == true",
                              textInput(inputId = "filename_raw", label = "Optionally: Your desired file name and plot title:"),
                              textInput(inputId = "dir_raw", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                              actionButton(inputId = "save_pdf_raw", label = "Submit"),
                              verbatimTextOutput("pdf_path_raw")
                            ),

                            # download plots raw pre-processed - manually
                            checkboxInput(inputId = "save_plots_raw_pre", div(icon("star"), "Manually: Save plots for raw data pre-processed"), value = FALSE),
                            textOutput("saving_plots_raw_pre"),

                            conditionalPanel(
                              condition = "input.save_plots_raw_pre == true",
                              textInput(inputId = "filename_raw_pre", label = "Optionally: Your desired file name and plot title:"),
                              textInput(inputId = "dir_raw_pre", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                              actionButton(inputId = "save_pdf_raw_pre", label = "Submit"),
                              verbatimTextOutput("pdf_path_raw_pre")
                            ),

                            # download plots normalized - manually
                            checkboxInput(inputId = "save_plots_norm", div(icon("star"), "Manually: Save plots for normalized data"), value = FALSE),
                            textOutput("saving_plots_norm"),

                            conditionalPanel(
                              condition = "input.save_plots_norm == true",
                              textInput(inputId = "filename_norm", label = "Optionally: Your desired file name and plot title:"),
                              textInput(inputId = "dir_norm", label = "Optionally: Your desired directory path:", placeholder = "current working directory"),
                              actionButton(inputId = "save_pdf_norm", label = "Submit"),
                              verbatimTextOutput("pdf_path_norm")
                            ),
                          ),
                          )
                 ),
               ),
             ),
      # right tab
      tabPanel("View Plots",
               div(class = "fixed-height-width",
                 fluidRow(
                   # note: both column widths as 6 instead 4 makes them fill the whole space of page, but plots have not correct height:width ratio then
                   # left - raw
                   column(4, class = "view-plots-column",
                          div(
                            h3("Plots of Raw Data", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # show labels parameter for PCA labels
                          checkboxInput(inputId = "show_labels_raw", label = "Show labels inside PCA plot", value = FALSE),

                          # button for showing plots raw data
                          actionButton(inputId = "show_plots_raw", label = "Show plots of raw data"),
                          div(style = "height: 20px;"),  # Add 20px of space

                          textOutput("score_title_raw"),
                          textOutput("score_raw"),
                          br(),

                          plotOutput("plot1_raw"),
                          br(),
                          plotOutput("plot2_raw"),
                          br(),
                          plotOutput("plot3_raw"),
                          br(),
                          plotOutput("plot4_raw"),
                          br(),

                          ),
                   # middle - raw pre-processed
                   column(4, class = "view-plots-column",
                          div(
                            h3("Plots of Raw Data Pre-Processed", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # show labels parameter for PCA labels
                          checkboxInput(inputId = "show_labels_raw_pre", label = "Show labels inside PCA plot", value = FALSE),

                          # button for showing plots raw data pre-processed
                          actionButton(inputId = "show_plots_raw_pre", label = "Show plots of raw data pre-processed"),
                          div(style = "height: 20px;"),  # Add 20px of space

                          textOutput("score_title_raw_pre"),
                          textOutput("score_raw_pre"),
                          br(),

                          plotOutput("plot1_raw_pre"),
                          br(),
                          plotOutput("plot2_raw_pre"),
                          br(),
                          plotOutput("plot3_raw_pre"),
                          br(),
                          plotOutput("plot4_raw_pre"),
                          br(),

                          ),
                   # right - normalized
                   column(4, class = "view-plots-column",
                          div(
                            h3("Plots of Normalized Data", style = "font-size: 20px; font-weight:750; color: darkblue;"),
                            class = "title-div"
                          ),
                          hr(class = "title-hr"),  # horizontal line

                          # show labels parameter for PCA labels
                          checkboxInput(inputId = "show_labels_norm", label = "Show labels inside PCA plot", value = FALSE),

                          # button for showing plots normalized
                          actionButton(inputId = "show_plots_norm", label = "Show plots of normalized data"),
                          div(style = "height: 20px;"),  # Add 20px of space

                          textOutput("score_title_norm"),
                          textOutput("score_norm"),
                          br(),

                          plotOutput("plot1_norm"),
                          br(),
                          plotOutput("plot2_norm"),
                          br(),
                          plotOutput("plot3_norm"),
                          br(),
                          plotOutput("plot4_norm"),
                          br(),

                          ),

                 ),
               ),
             )
      )

)


server <- function(input, output, session) {

    # command line argument: 'local' can be set to include manual download options
    # args <- commandArgs(trailingOnly = TRUE)  # for terminal mode
    # is_local <- "local" %in% args  # for terminal mode

    # Get the "local" option (parameter of run_app.R)
    is_local <- getOption("local", default = FALSE)  # default to handle invalid entries of local parameter

    is_local_reactive <- reactive(is_local)
    # watch if local is set, if so, show the additional content
    observe({
      if (is_local_reactive()) {
        shinyjs::show("local-content-1")  # Show content 1
        shinyjs::show("local-content-2")  # Show content 2
      } else {
        shinyjs::hide("local-content-1")  # Hide content 1
        shinyjs::hide("local-content-2")  # Hide content 2
      }
    })

    # initialize global variables - inside server makes them session specific
    lowest_level_df <- data.frame()  # data frame coming from reading function
    exp_design <- data.frame()
    additional_cols <- data.frame()
    lowest_level_norm <- data.frame()
    lowest_level_df_raw <- data.frame()  # raw data without any modification (no features filtered)
    lowest_level_df_pre <- data.frame()  # raw data pre-processed (pre-processed lowest_level_df)
    pca_colors <- c()
    pca_symbols <- c()
    max_choices_batches <- 1
    max_choices_conds <- 1

    output$selected_method <- renderText({
      paste("Your selected method:", input$method)
    })

    output$process_note <- renderText({
      "Click the button to load the data, update the settings, and perform normalization."
    })

    output$feature_note <- renderText({
      "Click the button to search for features that can be filtered."
    })

    output$m.combat_center_note <- renderText({
      "Please select the batch number based on the order inside the experimental design."
    })

    # make m combat notification reactive (changes whenever m combat function is called)
    m.combat_notification_text <- reactiveVal(NULL)

    output$m.combat_notification <- renderUI({
      text_content <- m.combat_notification_text()

      tags$div(
        id = "m.combat_notification",
        style = "color: darkblue;",
        text_content
      )
    })

    plot_of_symbols <- function() {
      original_Par <- par()
      par(font = 2, mar = c(0.5, 0, 0, 0))
      y = rev(c(rep(1, 4), rep(2, 5), rep(3, 5), rep(4, 5)))  # y values: 4 times 1, 5 times 2, 5 times 3, 5 times 4
      x = c(rep(1:5, 4))  # x values: 4 times 1 2 3 4 5
      pch_numbers <- 0:18
      plot(x[1:length(pch_numbers)], y[1:length(pch_numbers)], pch = pch_numbers, cex = 1.5, ylim = c(1, 4.5), xlim = c(0.5, 5.5),
           axes = FALSE, xlab = "", ylab = "")
      text(x[1:length(pch_numbers)], y[1:length(pch_numbers)], labels = pch_numbers, pos = 3)
      par(mar = original_Par$mar, font = original_Par$font)
    }

    # plot showing the symbols
    output$pca_symbols_plot <- renderPlot({
      plot_of_symbols()
    })


    # important: called to previously get completely raw data frame (without any feature filtering done)
    uploaded_data <- function(){
      output$datafile_error <- renderText({ NULL })  # clear
      req(input$data)
      inFile <- input$data
      tryCatch({
        ext <- tools::file_ext(inFile$name)
        if (ext == "csv") {
          df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
        } else {
          df <- read.table(inFile$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        }
        # important: same as in rowwisenorm package, replace all non-letter and non-number with a dot (safety)
        colnames(df) <- gsub("[^A-Za-z0-9]", ".", colnames(df))  # - otherwise column names do not match
        return(df)
      }, error = function(e) {
        output$datafile_error <- renderText({
          paste("Error reading the file of the data:", e$message)
        })
        return(data.frame())  # return empty data frame
      })
    }

    # called to previously read in uploaded experimental design
    uploaded_design <- function(){
      req(input$exp_design)
      tryCatch({
        design <- read.table(input$exp_design$datapath, header = FALSE, sep = "\t", na.strings = "NaN")
        design[is.na(design)] <- ""
        design <- design[, !apply(design, 2, function(x) all(grepl("^\\s*$", x)))]
        design <- as.data.frame(apply(design, 2, function(x) gsub("[^A-Za-z0-9]", ".", trimws(x))))
        return(design)
      }, error = function(e) {
        output$designfile_error <- renderText({
          paste("Error reading the file of the experimental design:", e$message)
        })
        return(data.frame())  # return empty data frame
      })

    }

    # Reactive value to store available features
    available_features <- reactiveVal(NULL)

    # new data uploaded: Clear the available features when a new file is uploaded
    observeEvent(input$data, {
      available_features(NULL)
      # clear status message of feature filtering
      output$generate_features_status <- renderUI({
        HTML('')
      })
      # clear status message of process button
      output$process_status <- renderUI({
        HTML('')
      })
      # clear notification section
      output$batch_colors_manually_notification <- renderText({ })
      output$condition_symbols_manually_notification <- renderText({ })
      m.combat_notification_text(NULL)
      output$datafile_error <- renderText({ NULL })
      output$reading_error <- renderText({ NULL })
      output$reading_warning <- renderText({ NULL })
      output$generate_features_error <- renderText({ NULL })
      output$generate_features_note <- renderText({ NULL })
      output$normalize_row_warning <- renderText({ NULL })
      output$normalization_error <- renderText({ NULL })

    })

    # new design uploaded:
    observeEvent(input$exp_design, {
      # clear status message of process button
      output$process_status <- renderUI({
        HTML('')
      })
      # clear notification section
      output$batch_colors_manually_notification <- renderText({ })
      output$condition_symbols_manually_notification <- renderText({ })
      m.combat_notification_text(NULL)
      output$designfile_error <- renderText({ NULL })
      output$reading_error <- renderText({ NULL })
      output$reading_warning <- renderText({ NULL })
      output$normalize_row_warning <- renderText({ NULL })
      output$normalization_error <- renderText({ NULL })

      # clear notes about number of colors/symbols to be set
      output$batch_colors_manually_note <- renderText({ NULL })
      output$condition_symbols_manually_note <- renderText({ NULL })

      design <- uploaded_design()

      # modifications as in package function read_files
      design[is.na(design)] <- ""
      design <- design[, !apply(design, 2, function(x) all(grepl("^\\s*$", x)))]
      design <- as.data.frame(apply(design, 2, function(x) gsub("[^A-Za-z0-9]", ".", trimws(x))))

      max_choices_batches <<- ncol(design) -1
      max_choices_conds <<- nrow(design)

      # update information how many colors and symbols need to be set - only when > 0 (no error in design)
      if (max_choices_batches > 0){
        output$batch_colors_manually_note <- renderText({
          paste("There need to be ", max_choices_batches, " colors set.")
        })
      }
      if (max_choices_conds > 0) {
        output$condition_symbols_manually_note <- renderText({
          paste("There need to be ", max_choices_conds, " symbols set.")
        })
      }

      # possible references for newly uploaded design - same as in package
      possible_refs <- c()
      if(nrow(design) > 0 && ncol(design) > 0){  # - only when design not empty
        for (i in 1:nrow(design)){
          counter <- 0  # counts how many columns have a value for this row
          for(j in 1:ncol(design)){
            if(trimws(design[i,j]) != ""){
              counter <- counter + 1
            }
          }
          if(counter == ncol(design)){
            possible_refs <- append(possible_refs, trimws(design[i,1]))
          }
        }
      }
      possible_refs <- unique(possible_refs)  # safety
      possible_refs <- trimws(possible_refs)  # safety
      possible_refs <- paste(possible_refs, collapse = ", ")  # convert to a String

      # update note stating the possible refs
      output$possible_refs_note <- renderText({
        paste("Possible references are: ", possible_refs)
      })

      # when new/another design uploaded:
      updateTextInput(session, "refs", value = "")  # reset input for manually set references
      updateNumericInput(session, "m.combat_center", value = 1)  # reset selected value for center as default 1

    })

    # when button to search for features is clicked
    observeEvent(input$generate_features, {
      # clear status message
      output$generate_features_status <- renderUI({
        HTML('')
      })

      # Get available features for the current data
      features <- c("only by site", "reverse", "contaminant")
      available_features_data <- character(0)
      df <- uploaded_data()  # currently uploaded data
      multiple_matches <- c() # only for an extra note for the user

      for (feat in features) {
        regex_pattern <- gsub("\\s+", ".*", feat)
        matching_col <- c()
        tryCatch({
          matching_col <- grep(regex_pattern, colnames(df), value = TRUE, ignore.case = TRUE, perl = TRUE)
        },  error = function(e) {  # safety
          output$generate_features_error <- renderText({
            paste("Error searching for available features:", e$message)
          })
        })
        if (length(matching_col) == 1) {  # if exactly one column matches
          available_features_data <- append(available_features_data, feat)  # add the feature as a choice
        }
        else if (length(matching_col) > 1){  # if more than 1 matching column
          multiple_matches <- append(multiple_matches, feat) # append current feature name (only used for the note)
        }
      }

      # note in case some features have multiple matching columns - could leave this out
      if (length(multiple_matches) > 0){
        multiple_matches <- paste(multiple_matches, collapse = ", ")
        output$generate_features_note <- renderText({
          paste("Note: The feature(s) ", multiple_matches, " have multiple matching columns inside the data.")
        })
      }

      # Update the available features using the reactiveVal
      available_features(available_features_data)

      # status message
      generate_features_status <- renderUI({
        HTML('<i class="fa fa-check-circle" style="color: green;"></i> Search completed')
      })
      output$generate_features_status <- generate_features_status
    })

    # when at least one feature is present, print title for feature filtering
    output$feature_text <- renderText({
      if ((! is.null(available_features())) & (! identical(available_features(), character(0)))){
        "Features that can be filtered:"
      } else {
        NULL
      }
    })

    # show each checkbox only when feature is available
    output$onlyBySiteCheckbox <- renderUI({
      if ("only by site" %in% available_features()) {
        checkboxInput("onlyBySite", "Only by Site", value = FALSE)
      } else {
        # set value first to false, then remove checkbox
        tagList(
          checkboxInput("onlyBySite", "Only by Site", value = FALSE),
          tags$script(HTML("$(document).ready(function() { $('input#onlyBySite').parent().hide(); });"))
        )
      }
    })

    output$reverseCheckbox <- renderUI({
      if ("reverse" %in% available_features()) {
        checkboxInput("reverse", "Reverse", value = FALSE)
      } else {
        # set value first to false, then remove checkbox
        tagList(
          checkboxInput("reverse", "Reverse", value = FALSE),
          tags$script(HTML("$(document).ready(function() { $('input#reverse').parent().hide(); });"))
        )
      }
    })

    output$contaminantCheckbox <- renderUI({
      if ("contaminant" %in% available_features()) {
        checkboxInput("contaminant", "Contaminant", value = FALSE)
      } else {
        # set value first to false, then remove checkbox
        tagList(
          checkboxInput("contaminant", "Contaminant", value = FALSE),
          tags$script(HTML("$(document).ready(function() { $('input#contaminant').parent().hide(); });"))
        )
      }
    })


    # using uploaded design file to set max number of choices for colors and symbols, and for center of M-ComBat
    observe({
      req(input$exp_design)

      # read the uploaded file
      design <- uploaded_design()

      # maximum numbers of choices for batches and conditions (global variables)
      max_choices_batches <<- ncol(design) -1
      max_choices_conds <<- nrow(design)

      # whenever more colors/symbols are set than allowed, directly reduce the selected ones
      if (max_choices_batches < 1){  # - e.g. error in design file
        updateSelectInput(session, "batch_colors_manually", selected = "")
      }
      else if (length(input$batch_colors_manually) > max_choices_batches){
        updateSelectInput(session, "batch_colors_manually", selected = input$batch_colors_manually[1:max_choices_batches])
      }
      if (max_choices_conds < 1){  # - e.g. error in design file, make selection empty
        updateSelectInput(session, "condition_symbols_manually", selected = "")
      }
      else if (length(input$condition_symbols_manually) > max_choices_conds){
        updateSelectInput(session, "condition_symbols_manually", selected = input$condition_symbols_manually[1:max_choices_conds])
      }
      if (max_choices_batches < 1){
        max_choices_batches <<- 1  # otherwise: empty design leads to max = -1, causes endless loop in next observe block
      }
      updateNumericInput(session, "m.combat_center", max = max_choices_batches)

    })

    # M-ComBat center: when manually a number is entered, modify to be at most max and at least 1
    observe({
      if (!is.null(input$m.combat_center) && !is.na(input$m.combat_center)) {
        # convert float to integer
        updateNumericInput(session, "m.combat_center", value = as.integer(input$m.combat_center))
        if (input$m.combat_center > max_choices_batches) {
          updateNumericInput(session, "m.combat_center", value = max_choices_batches)
        }
        else if (input$m.combat_center < 1){
          updateNumericInput(session, "m.combat_center", value = 1)
        }
      }
      else {
        updateNumericInput(session, "m.combat_center", value = 1)
      }
    })


    # reading of uploaded files by rowwisenorm package
    readin <- function(){  # function makes it possible to control when it is executed explicitly (not as reactive)
      # clear for every new call
      output$reading_warning <- renderText({
        NULL
      })
      output$reading_error <- renderText({
        NULL
      })
      req(input$data)
      req(input$exp_design)
      # if feature is present, set as the choice, otherwise set filtering for this feature as FALSE
      if (! is.null(input$onlyBySite)) boolean_only_by_site <- input$onlyBySite else boolean_only_by_site <- F
      if (! is.null(input$reverse)) boolean_reverse <- input$reverse else boolean_reverse <- F
      if (! is.null(input$contaminant)) boolean_contaminant <- input$contaminant else boolean_contaminant <- F
      tryCatch({
        return_list <- rowwisenorm::read_files(data = input$data$datapath, design = input$exp_design$datapath,
                                               rm_only_by_site = boolean_only_by_site, rm_reverse = boolean_reverse,
                                               rm_contaminant = boolean_contaminant)
        return(return_list)
      }, warning = function(w) {  # print warning (should never happen with feature search)
        output$reading_warning <- renderText({
          paste(w$message)
        })
        return(return_list)
      }, error = function(e) {  # when reading sanity check fails
        output$reading_error <- renderText({
          paste(e$message, " - Please upload the correct files before continuing.")
        })
        return(NULL)  # in case sanity checks fail, return NULL -> is.null in the following always checked
      })

    }


    # print note when filter rows is set
    output$filterrows_note <- renderText({
      if (input$filterrows){
        return("This is an exclusive lower bound.")
      }
      else {
        return(NULL)
      }
    })


    # helper function preprocessing - tryCatch only for safety
    preprocess <- function(lowest_level_df, do_log=F, do_filter=F, do_sum=F, do_median=F){
      # preprocessing: when log2 is set
      if(do_log){
        tryCatch({
          lowest_level_df <- rowwisenorm::log2_transform(lowest_level_df = lowest_level_df)
        }, error = function(e) {
          output$pre_log_error <- renderText({
            paste("Error in log transformation: ", e$message)
          })
        })
      }

      # preprocessing: when filter rows is set
      if(do_filter){
        tryCatch({
          # slider input is automatically a numeric between 0 and 1 - no warnings possible
          lowest_level_df <- rowwisenorm::filter_rows(lowest_level_df, input$filterrowsratio)
        }, error = function(e) {
          output$pre_filter_error <- renderText({
            paste("Error in filtering rows: ", e$message)
          })
        })
      }

      # preprocessing: when sum normalize is set
      if(do_sum){
        tryCatch({
          lowest_level_df <- rowwisenorm::sum_normalize(lowest_level_df, refFunc = input$refFunc_sum,
                                                        norm = input$norm_sum, na.rm = input$na_rm_sum)
        }, error = function(e) {
          output$pre_sum_error <- renderText({
            paste("Error in preprocessing with sum normalization: ", e$message)
          })
        })
      }

      # preprocessing: when median normalize is set
      if(do_median){
        tryCatch({
          lowest_level_df <- rowwisenorm::median_normalize(lowest_level_df)
        }, error = function(e) {
          output$pre_median_error <- renderText({
            paste("Error in preprocessing with median normalization: ", e$message)
          })
        })
      }
      return(lowest_level_df)
    }

    # PROCESS BUTTON:

    # important: assign values for global variables with <<- and not <-
    observeEvent(input$process, {
      withProgress(
        message = 'Processing data...',
        detail = 'This may take a moment...',
        value = 0, {
          # set empty
          lowest_level_df <<- data.frame()
          exp_design <<- data.frame()
          additional_cols <<- data.frame()
          lowest_level_norm <<- data.frame()
          lowest_level_df_pre <<- data.frame()

          pca_colors <<- character(0)  # Reset pca_colors
          pca_symbols <<- character(0) # Reset pca_symbols

          # set any fields that show results empty
          output$process_status <- renderUI({
            HTML('')
          })
          output$data_output <- renderUI({ })  # show data field
          output$show_data_note <- renderText({ })  # show data note
          output$plot1_raw <- renderUI({ })
          output$plot2_raw <- renderUI({ })
          output$plot3_raw <- renderUI({ })
          output$plot4_raw <- renderUI({ })
          output$plot1_raw_pre <- renderUI({ })
          output$plot2_raw_pre <- renderUI({ })
          output$plot3_raw_pre <- renderUI({ })
          output$plot4_raw_pre <- renderUI({ })
          output$plot1_norm <- renderUI({ })
          output$plot2_norm <- renderUI({ })
          output$plot3_norm <- renderUI({ })
          output$plot4_norm <- renderUI({ })
          output$score_title_raw <- renderText({ })
          output$score_raw <- renderText({ })
          output$score_title_raw_pre <- renderText({ })
          output$score_raw_pre <- renderText({ })
          output$score_title_norm <- renderText({ })
          output$score_norm <- renderText({ })
          # notification section
          output$batch_colors_manually_notification <- renderText({ })
          output$condition_symbols_manually_notification <- renderText({ })
          m.combat_notification_text(NULL)
          output$datafile_error <- renderText({ NULL })
          output$designfile_error <- renderText({ NULL })
          output$reading_error <- renderText({ NULL })
          output$reading_warning <- renderText({ NULL })
          output$generate_features_error <- renderText({ NULL })
          output$generate_features_note <- renderText({ NULL })
          output$normalize_row_warning <- renderText({ })  # warning of normalize_row when no valid refs
          output$normalization_error <- renderText({ NULL })

          output$pre_log_error <- renderText({ NULL })   # errors of pre-processing steps
          output$pre_filter_error <- renderText({ NULL })
          output$pre_sum_error <- renderText({ NULL })
          output$pre_median_error <- renderText({ NULL })

          return_list <- readin()
          if (! is.null(return_list)){
            lowest_level_df <<- return_list[["lowest_level_df"]]  # raw on lowest level but feature-filtered and ID column -> further used in pre-processing
            exp_design <<- return_list[["exp_design"]]
            additional_cols <<- return_list[["additional_cols"]]

            pca_colors <<- return_list[["pca_colors"]]  # used for batches in PCA and heatmap
            pca_symbols <<- return_list[["pca_symbols"]]  # used for conditions in PCA

            # change the batch colors in case enough colors were manually set by the user
            batch_colors_manually_set <- input$batch_colors_manually
            number_batches <- ncol(exp_design) -1
            # when correct number of colors are manually set, take them
            if (length(batch_colors_manually_set) == number_batches){
              pca_colors <<- batch_colors_manually_set
            }
            else {  # else: too few colors, use the automatic generated colors (too many not possible)
              output$batch_colors_manually_notification <- renderText({
                paste("There need to be ", number_batches, " colors set. Automatically generated colors are used.")
              })
            }

            # change the condition symbols the same way
            condition_symbols_manually_set <- input$condition_symbols_manually
            condition_symbols_manually_set <- as.numeric(condition_symbols_manually_set)  # important: make entry numeric
            number_conds <- nrow(exp_design)
            # when correct number of symbols are manually set, take them
            if (length(condition_symbols_manually_set) == number_conds){
              pca_symbols <<- condition_symbols_manually_set
            }
            else {  # else: too few symbols, use the automatic generated symbols (too many not possible)
              output$condition_symbols_manually_notification <- renderText({
                paste("There need to be ", number_conds, " symbols set. Automatically generated symbols are used.")
              })
            }

            # completely raw data with all columns  (no ID column, no feature filtering, raw is never needed but for plot comparison)
            lowest_level_df_raw <<- uploaded_data()
            # reduce to lowest level
            lowest_level_df_raw <<- lowest_level_df_raw[colnames(lowest_level_df_raw) %in% colnames(lowest_level_df)]


            # pre-processing (included in each normalization) and normalization
            if(input$method == "row-wise-normalization"){
              lowest_level_norm <<- normalize_rowwise(lowest_level_df, exp_design)
            }
            else if(input$method == "total-sum"){
              lowest_level_norm <<- normalize_totalsum(lowest_level_df)
            }
            else if(input$method == "VST"){
              lowest_level_norm <<- normalize_vst(lowest_level_df)
            }
            else if(input$method == "VSN"){
              lowest_level_norm <<- normalize_vsn(lowest_level_df)
            }
            else if(input$method == "quantile-normalization"){
              lowest_level_norm <<- normalize_quantile(lowest_level_df)
            }
            else if(input$method == "ComBat"){
              lowest_level_norm <<- normalize_combat(lowest_level_df, exp_design)
            }
            else if(input$method == "M-ComBat"){
              lowest_level_norm <<- normalize_m.combat(lowest_level_df, exp_design)
            }

            # status message
            process_status <- renderUI({
              HTML('<i class="fa fa-check-circle" style="color: green;"></i> Data Ready!')
            })
            output$process_status <- process_status

          }
        }
      )
    })


    # NORMALIZATION row-wise
    normalize_rowwise <- function(lowest_level_df, exp_design){
      # clear for every new call
      output$normalize_row_warning <- renderText({ NULL })
      output$normalization_error <- renderText({ NULL })

      # preprocessing
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                    do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        if(input$active_mode){  # when active was set, the input for refs is taken as ref parameter
          input_string <- input$refs
          input_vector <- NULL
          if (!is.null(input_string) && input_string != "") {
            input_vector <- unlist(strsplit(input_string, ",\\s*"))
            input_vector <- trimws(input_vector)
          }
          lowest_level_norm <- rowwisenorm::normalize_row(lowest_level_df = lowest_level_df_pre,
                                                          exp_design = exp_design, ref = input_vector,
                                                          na.rm = input$na_rm, refFunc = input$refFunc)
        }
        else {  # no refs are set manually, automatic mode
          lowest_level_norm <- rowwisenorm::normalize_row(lowest_level_df = lowest_level_df_pre,
                                                          exp_design = exp_design,
                                                          na.rm = input$na_rm, refFunc = input$refFunc)
        }
        return(lowest_level_norm)
      }, warning = function(w) {  # print warning (e.g. when no refs possible or non-possible refs entered)
        output$normalize_row_warning <- renderText({
          paste(w$message)
        })
        return(lowest_level_norm)  # still return the empty data frame that normalize_row returns in case of warning
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }

    # NORMALIZATION total sum
    normalize_totalsum <- function(lowest_level_df){
      # clear for every new call
      output$normalization_error <- renderText({ NULL })

      # preprocessing - no sum normalize
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                    do_sum = F, do_median = input$median_norm)
      tryCatch({
        lowest_level_norm <- rowwisenorm::sum_normalize(lowest_level_df_pre, refFunc = input$refFunc_sum,
                                                        norm = input$norm_sum, na.rm = input$na_rm)
        return(lowest_level_norm)
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }

    # NORMALIZATION VST
    normalize_vst <- function(lowest_level_df){
      # clear for every new call
      output$normalization_error <- renderText({ NULL })

      vst_normalized_data <- data.frame()

      # preprocessing - no log2 (no negatives allowed)
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = F, do_filter = input$filterrows,
                                    do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        lowest_level_df_comp <- lowest_level_df_pre[complete.cases(lowest_level_df_pre), ]  # without missing values
        lowest_level_df_matrix <- as.matrix(lowest_level_df_comp[! colnames(lowest_level_df_comp) %in% "row.number"])  # convert to matrix and exclude ID column
        # calculation
        dge_list <- DGEList(counts = lowest_level_df_matrix)  # allows no negative counts (do not log2)
        vst_normalized_data <- calcNormFactors(dge_list)
        vst_normalized_data <- cpm(vst_normalized_data)
        # back convert and set column names back
        vst_normalized_data <- as.data.frame(vst_normalized_data)
        colnames(vst_normalized_data) <- colnames(lowest_level_df_comp[! colnames(lowest_level_df_comp) %in% "row.number"])
        vst_normalized_data <- cbind("row.number" = lowest_level_df_comp$row.number, vst_normalized_data)  # back append ID column

        return(vst_normalized_data)
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }

    # NORMALIZATION VSN
    normalize_vsn <- function(lowest_level_df){
      # clear for every new call
      output$normalization_error <- renderText({ NULL })

      vsn_normalized_data <- data.frame()

      # preprocessing
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                    do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        lowest_level_df_matrix <- as.matrix(lowest_level_df_pre[! colnames(lowest_level_df_pre) %in% "row.number"])  # convert to matrix and exclude ID column
        lowest_level_df_matrix[is.nan(lowest_level_df_matrix)] <- NA  # safety check that no NaN but only NA present
        # Perform VSN normalization
        vsn_normalized_data <- normalizeVSN(lowest_level_df_matrix)
        # back convert and set column names back
        vsn_normalized_data <- as.data.frame(vsn_normalized_data)
        colnames(vsn_normalized_data) <- colnames(lowest_level_df_pre[! colnames(lowest_level_df_pre) %in% "row.number"])
        vsn_normalized_data <- cbind("row.number" = lowest_level_df_pre$row.number, vsn_normalized_data)  # back append ID column

        return(vsn_normalized_data)
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }

    # NORMALIZATION Quantile
    normalize_quantile <- function(lowest_level_df){
      # clear for every new call
      output$normalization_error <- renderText({ NULL })

      quantile_normalized <- data.frame()

      # preprocessing
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                    do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        lowest_level_df_matrix <- as.matrix(lowest_level_df_pre[! colnames(lowest_level_df_pre) %in% "row.number"])  # convert to matrix and exclude ID column
        # calculation
        quantile_normalized <- preprocessCore::normalize.quantiles(lowest_level_df_matrix)
        # back convert and set column names back
        quantile_normalized <- as.data.frame(quantile_normalized)
        colnames(quantile_normalized) <- colnames(lowest_level_df_pre[! colnames(lowest_level_df_pre) %in% "row.number"])
        quantile_normalized <- cbind("row.number" = lowest_level_df_pre$row.number, quantile_normalized)  # back append ID column

        return(quantile_normalized)
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }

    # NORMALIZATION ComBat
    normalize_combat <- function(lowest_level_df, exp_design){
      # clear for every new call
      output$normalization_error <- renderText({ NULL })

      combat_data <- data.frame()

      # preprocessing
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                         do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        ms_data <- lowest_level_df_pre[! colnames(lowest_level_df_pre) %in% "row.number"]

        # impute missing values with the mean of each column
        ms_data_imputed <- apply(ms_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

        # convert to a numeric matrix
        ms_data_matrix <- as.matrix(ms_data_imputed)

        # get the order of conditions and batches based on the exp_design and order of data columns
        batches <- c()
        conditions <- c()
        # -> check for each column name which batch and which condition it is
        for(col in colnames(ms_data_matrix)){
          for (i in 1:nrow(exp_design)){
            for(j in 2:ncol(exp_design)){
              if (exp_design[i, j] == col){
                batches <- append(batches, j-1)  # batch number
                conditions <- append(conditions, i)  # condition number
              }
            }
          }
        }

        # ComBat from sva package
        combat_data <- ComBat(ms_data_matrix, batch = as.factor(batches), mod = as.factor(conditions))

        # convert to data frame
        combat_data <- as.data.frame(combat_data)

        combat_data <- cbind("row.number" = lowest_level_df_pre$row.number, combat_data)  # back append ID column
        return(combat_data)
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }

    # NORMALIZATION M-ComBat
    normalize_m.combat <- function(lowest_level_df, exp_design){
      # clear for every new call
      output$normalization_error <- renderText({ NULL })

      m.combat_data <- data.frame()

      # preprocessing
      lowest_level_df_pre <<- preprocess(lowest_level_df, do_log = input$log2_t, do_filter = input$filterrows,
                                         do_sum = input$sum_norm, do_median = input$median_norm)

      tryCatch({
        ms_data <- lowest_level_df_pre[! colnames(lowest_level_df_pre) %in% "row.number"]

        # impute missing values with the mean of each column
        ms_data_imputed <- apply(ms_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

        # convert to a numeric matrix
        ms_data_matrix <- as.matrix(ms_data_imputed)

        # get the order of conditions and batches based on the exp_design and order of data columns
        batches <- c()  # same as for ComBat
        conditions <- c()  # same as for ComBat
        # -> check for each column name which batch and which condition it is
        for(col in colnames(ms_data_matrix)){
          for (i in 1:nrow(exp_design)){
            for(j in 2:ncol(exp_design)){
              if (exp_design[i, j] == col){
                batches <- append(batches, j-1)  # batch number
                conditions <- append(conditions, i)  # condition number
              }
            }
          }
        }

        # center
        center <- input$m.combat_center
        m.combat_notification_text(paste("Batch ", center, " was used as center."))

        # file that stores M.COMBAT function
        source(system.file("shiny_app", "M-ComBat.R", package = "rowwisenorm"))

        m.combat_data <- M.COMBAT(ms_data_matrix, batch = as.factor(batches), mod = as.factor(conditions),
                                  center = center)

        m.combat_data <- as.data.frame(m.combat_data)
        m.combat_data <- cbind("row.number" = lowest_level_df_pre$row.number, m.combat_data)  # back append ID column

        return(m.combat_data)
      }, error = function(e){
        output$normalization_error <- renderText({
          paste(e$message)
        })
        return(data.frame())
      })

    }


    # show data
    observeEvent(input$show_data, {
      if(nrow(lowest_level_norm > 0)){
        output$data_output <- renderTable({
          beginning <- input$start_row
          ending <- input$end_row
          if (is.numeric(beginning) && is.numeric(ending) && beginning >= 0 && ending >= 0 && beginning%%1==0  && ending%%1==0){  # check that positive integers
            if(ending > nrow(lowest_level_norm)){  # max ending is the number of rows that are present
              ending <- nrow(lowest_level_norm)
            }
            lowest_level_norm[beginning:ending, ]
          }
          else {
            if(5 > nrow(lowest_level_norm)){
              lowest_level_norm[1:nrow(lowest_level_norm), ]  # default if not valid entry and less than 5 rows present
            } else {
              lowest_level_norm[1:5, ]  # default if not valid entry
            }
          }
        })
        # note
        output$show_data_note <- renderText({
          "The first column stores the original row number inside the data."
        })
      }

    })


    # save PDF manually
    observeEvent(input$save_pdf_raw, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_df_raw) != 0){
        # show labels parameter
        if (input$show_labels) show_lab <- T else show_lab <- F

        # svg parameter
        if (input$svg) make_svg <- T else make_svg <- F

        rowwisenorm::plot_results(lowest_level_df = lowest_level_df_raw, exp_design = exp_design,
                                  filename = trimws(input$filename_raw), output_dir = trimws(input$dir_raw),
                                  show_labels = show_lab, svg = make_svg,
                                  set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols

        # output message stating where the file was saved
        if (trimws(input$dir_raw) != "")  dir_path <- trimws(input$dir_raw) else dir_path <- "current working directory"
        if (trimws(input$filename_raw) != "") file_name <- paste(trimws(input$filename_raw), ".pdf", sep = "") else file_name <- "results.pdf"  # note: hard coded as stated in plot_results
        output$pdf_path_raw <- renderText({
          paste("PDF saved to ", dir_path, " with file name ", file_name)
        })
      }
    })

    observeEvent(input$save_pdf_raw_pre, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_df_pre) != 0){
        # show labels parameter
        if (input$show_labels) show_lab <- T else show_lab <- F

        # svg parameter
        if (input$svg) make_svg <- T else make_svg <- F

        rowwisenorm::plot_results(lowest_level_df = lowest_level_df_pre, exp_design = exp_design,
                                  filename = trimws(input$filename_raw_pre), output_dir = trimws(input$dir_raw_pre),
                                  show_labels = show_lab, svg = make_svg,
                                  set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols

        # output message stating where the file was saved
        if (trimws(input$dir_raw_pre) != "")  dir_path <- trimws(input$dir_raw_pre) else dir_path <- "current working directory"
        if (trimws(input$filename_raw_pre) != "") file_name <- paste(trimws(input$filename_raw_pre), ".pdf", sep = "") else file_name <- "results.pdf"  # note: hard coded as stated in plot_results
        output$pdf_path_raw <- renderText({
          paste("PDF saved to ", dir_path, " with file name ", file_name)
        })
      }
    })

    observeEvent(input$save_pdf_norm, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_norm) != 0){
        # show labels parameter
        if (input$show_labels) show_lab <- T else show_lab <- F

        # svg parameter
        if (input$svg) make_svg <- T else make_svg <- F

        rowwisenorm::plot_results(lowest_level_df = lowest_level_norm, exp_design = exp_design,
                                  filename = trimws(input$filename_norm), output_dir = trimws(input$dir_norm),
                                  show_labels = show_lab, svg = make_svg,
                                  set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols

        # output message stating where the file was saved
        if (trimws(input$dir_norm) != "")  dir_path <- trimws(input$dir_norm) else dir_path <- "current working directory"
        if (trimws(input$filename_norm) != "") file_name <- paste(trimws(input$filename_norm), ".pdf", sep = "") else file_name <- "results.pdf"  # note: hard coded as stated in plot_results
        output$pdf_path_norm <- renderText({
          paste("PDF saved to ", dir_path, " with file name ", file_name)
        })
      }
    })

    # download PDFs
    output$download_pdf_raw <- downloadHandler(
      filename = function(){
        if (input$svg){
          "results.zip"
        }
        else {
          "results.pdf"
        }
      },
      content = function(file) {
        # only do when data was processed (data frame not empty)
        if (nrow(lowest_level_df_raw) != 0){
          # show labels parameter
          if (input$show_labels) show_lab <- T else show_lab <- F

          # svg parameter
          if (input$svg) make_svg <- T else make_svg <- F

          # Save file with a progress indicator (only to get progress message, also works to just generate and save)
          withProgress(
              message = 'Generating file(s)...',
              detail = 'This may take a moment...',
              value = 0, {

                original_working_directory <- getwd()

                # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
                mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
                dir.create(mytemp)

                # Generate the PDF and SVG files in the temporary directory
                rowwisenorm::plot_results(lowest_level_df_raw, exp_design,
                                          output_dir = mytemp, show_labels = show_lab, svg = make_svg,
                                          set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols
                Sys.sleep(0.5)

                setwd(mytemp)

                if (make_svg){
                  # make zip of the files inside temporary directory
                  zip_file <- "results.zip"
                  zip(zip_file, files = c(paste("results", "pdf", sep = "."), paste("results", "01.svg", sep = ""),
                                          paste("results", "02.svg", sep = ""), paste("results", "03.svg", sep = ""),
                                          paste("results", "04.svg", sep = "")))

                  # Move the ZIP archive to the chosen location
                  file.rename(zip_file, file)
                }
                else {
                  file.rename("results.pdf", file)
                }

                setwd(original_working_directory)  # set back to original working directory

                # remove the temporary directory
                unlink(mytemp, recursive = TRUE)
              }
          )

        }
      }
    )

    # same but with pre-processed data
    output$download_pdf_raw_pre <- downloadHandler(
      filename = function(){
        if (input$svg){
          "results.zip"
        }
        else {
          "results.pdf"
        }
      },
      content = function(file) {
        # only do when data was processed (data frame not empty)
        if (nrow(lowest_level_df_pre) != 0){
          # show labels parameter
          if (input$show_labels) show_lab <- T else show_lab <- F

          # svg parameter
          if (input$svg) make_svg <- T else make_svg <- F

          # Save file with a progress indicator (only to get progress message, also works to just generate and save)
          withProgress(
            message = 'Generating file(s)...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the PDF and SVG files in the temporary directory
              rowwisenorm::plot_results(lowest_level_df_pre, exp_design,
                                        output_dir = mytemp, show_labels = show_lab, svg = make_svg,
                                        set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols
              Sys.sleep(0.5)

              setwd(mytemp)

              if (make_svg){
                # make zip of the files inside temporary directory
                zip_file <- "results.zip"
                zip(zip_file, files = c(paste("results", "pdf", sep = "."), paste("results", "01.svg", sep = ""),
                                        paste("results", "02.svg", sep = ""), paste("results", "03.svg", sep = ""),
                                        paste("results", "04.svg", sep = "")))

                # Move the ZIP archive to the chosen location
                file.rename(zip_file, file)
              }
              else {
                file.rename("results.pdf", file)
              }

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )

        }
      }
    )

    output$download_pdf_norm <- downloadHandler(
      filename = function(){
        if (input$svg){
          "results.zip"
        }
        else {
          "results.pdf"
        }
      },
      content = function(file) {
        # only do when data was processed (data frame not empty)
        if (nrow(lowest_level_norm) != 0){
          # show labels parameter
          if (input$show_labels) show_lab <- T else show_lab <- F

          # svg parameter
          if (input$svg) make_svg <- T else make_svg <- F

          # Save file with a progress indicator (only to get progress message, also works to just generate and save)
          withProgress(
            message = 'Generating file(s)...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the PDF and SVG files in the temporary directory
              rowwisenorm::plot_results(lowest_level_norm, exp_design,
                                        output_dir = mytemp, show_labels = show_lab, svg = make_svg,
                                        set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols
              Sys.sleep(0.5)

              setwd(mytemp)

              if (make_svg){
                # make zip of the files inside temporary directory
                zip_file <- "results.zip"
                zip(zip_file, files = c(paste("results", "pdf", sep = "."), paste("results", "01.svg", sep = ""),
                                        paste("results", "02.svg", sep = ""), paste("results", "03.svg", sep = ""),
                                        paste("results", "04.svg", sep = "")))

                # Move the ZIP archive to the chosen location
                file.rename(zip_file, file)
              }
              else {
                file.rename("results.pdf", file)
              }

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )

        }
      }
    )

    # show plots - completely raw, raw pre-processed, normalized
    observeEvent(input$show_plots_raw, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_df_raw) != 0){
        output$plot1_raw <- renderPlot({
          rowwisenorm::plot_correlations(lowest_level_df_raw)
        })
        output$plot2_raw <- renderPlot({
          rowwisenorm::plot_heatmap(lowest_level_df_raw, exp_design, batch_colors = pca_colors)
        })
        output$plot3_raw <- renderPlot({
          rowwisenorm::pcaPlot(lowest_level_df_raw)
        })
        output$plot4_raw <- renderPlot({
          # show labels parameter
          if (input$show_labels_raw) show_lab <- T else show_lab <- F
          rowwisenorm::pcaPlot2(lowest_level_df_raw, exp_design, show_labels = show_lab,
                                set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols
        })
        output$score_title_raw <- renderText({
          "Data-specific Score of the PCA plot:"
        })
        # catch score of PCA (not possible inside renderPlot)
        score <- rowwisenorm::pcaPlot2(lowest_level_df_raw, exp_design, show_labels = F,
                                       set_colors = pca_colors, set_symbols = pca_symbols)
        if(length(dev.list())!=0) {
          dev.off()
        }
        output$score_raw <- renderText({
          score
        })
      }
    })

    observeEvent(input$show_plots_raw_pre, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_df_pre) != 0){
        output$plot1_raw_pre <- renderPlot({
          rowwisenorm::plot_correlations(lowest_level_df_pre)
        })
        output$plot2_raw_pre <- renderPlot({
          rowwisenorm::plot_heatmap(lowest_level_df_pre, exp_design, batch_colors = pca_colors)
        })
        output$plot3_raw_pre <- renderPlot({
          rowwisenorm::pcaPlot(lowest_level_df_pre)
        })
        output$plot4_raw_pre <- renderPlot({
          # show labels parameter
          if (input$show_labels_raw_pre) show_lab <- T else show_lab <- F
          rowwisenorm::pcaPlot2(lowest_level_df_pre, exp_design, show_labels = show_lab,
                                set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols
        })
        output$score_title_raw_pre <- renderText({
          "Data-specific Score of the PCA plot:"
        })
        # catch score of PCA (not possible inside renderPlot)
        score <- rowwisenorm::pcaPlot2(lowest_level_df_pre, exp_design, show_labels = F,
                                       set_colors = pca_colors, set_symbols = pca_symbols)
        if(length(dev.list())!=0) {
          dev.off()
        }
        output$score_raw_pre <- renderText({
          score
        })
      }
    })

    observeEvent(input$show_plots_norm, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_norm) != 0){
        output$plot1_norm <- renderPlot({
          rowwisenorm::plot_correlations(lowest_level_norm)
        })
        output$plot2_norm <- renderPlot({
          rowwisenorm::plot_heatmap(lowest_level_norm, exp_design, batch_colors = pca_colors)
        })
        output$plot3_norm <- renderPlot({
          rowwisenorm::pcaPlot(lowest_level_norm)
        })
        output$plot4_norm <- renderPlot({
          # show labels parameter
          if (input$show_labels_norm) show_lab <- T else show_lab <- F
          rowwisenorm::pcaPlot2(lowest_level_norm, exp_design, show_labels = show_lab,
                                set_colors = pca_colors, set_symbols = pca_symbols)  # set colors and symbols
        })
        output$score_title_norm <- renderText({
          "Data-specific Score of the PCA plot:"
        })
        # catch score of PCA (not possible inside renderPlot)
        score <- rowwisenorm::pcaPlot2(lowest_level_norm, exp_design, show_labels = F,
                                       set_colors = pca_colors, set_symbols = pca_symbols)
        if(length(dev.list())!=0) {
          dev.off()
        }
        output$score_norm <- renderText({
          score
        })

      }
    })


    # save outfile manually
    observeEvent(input$save_outfile, {
      # only do when data was processed (data frame not empty)
      if (nrow(lowest_level_norm) != 0){

        if(input$outfile_level == "lowest-level"){
          rowwisenorm::write_outfile(lowest_level_df = lowest_level_norm,
                                     filename = input$filename_outfile, output_dir = input$dir_outfile)
          # output message
          if (input$dir_outfile != "")  dir_path <- input$dir_outfile else dir_path <- "current working directory"
          if (input$filename_outfile != "") file_name <- paste(input$filename_outfile, ".csv", sep = "") else file_name <- "output.csv"  # note: hard coded as stated in write_outfile
        }
        else if(input$outfile_level == "all-columns"){
          rowwisenorm::write_outfile(lowest_level_df = lowest_level_norm, additional_cols = additional_cols,
                                     filename = input$filename_outfile, output_dir = input$dir_outfile)
          # output message
          if (input$dir_outfile != "")  dir_path <- input$dir_outfile else dir_path <- "current working directory"
          if (input$filename_outfile != "") file_name <- paste(input$filename_outfile, ".csv", sep = "") else file_name <- "output_complete.csv"  # note: hard coded as stated in write_outfile
        }
        output$outfile_path <- renderText({
          paste("Output saved to ", dir_path, " with file name ", file_name)
        })
      }

    })

    # download outfile lowest level
    output$download_outfile <- downloadHandler(
      filename = function(){
        "output.csv"
      },
      content = function(file) {
        # only do when data was processed (data frame not empty)
        if (nrow(lowest_level_norm) != 0){

          # Save file with a progress indicator
          withProgress(
            message = 'Generating file...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the file in the temporary directory
              rowwisenorm::write_outfile(lowest_level_norm, output_dir = mytemp)
              Sys.sleep(0.5)  # Simulate some work for the progress bar

              setwd(mytemp)

              # Move the generated file to the specified location
              file.rename("output.csv", file)

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )
        }
      }
    )

    # download outfile with additional columns
    output$download_outfile_comp <- downloadHandler(
      filename = function(){
        "output_complete.csv"
      },
      content = function(file) {
        # only do when data was processed (data frame not empty)
        if (nrow(lowest_level_norm) != 0){

          # Save file with a progress indicator
          withProgress(
            message = 'Generating file...',
            detail = 'This may take a moment...',
            value = 0, {

              original_working_directory <- getwd()

              # artificial temporary directory inside current working directory to save the files in the first place (otherwise files would be automatically saved in wd which could cause overwriting other files)
              mytemp <- paste0("download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1:9999, 1))
              dir.create(mytemp)

              # Generate the file in the temporary directory
              rowwisenorm::write_outfile(lowest_level_norm, additional_cols = additional_cols, output_dir = mytemp)
              Sys.sleep(0.5)  # Simulate some work for the progress bar

              setwd(mytemp)

              # Move the generated file to the specified location
              file.rename("output_complete.csv", file)

              setwd(original_working_directory)  # set back to original working directory

              # remove the temporary directory
              unlink(mytemp, recursive = TRUE)
            }
          )
        }
      }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
