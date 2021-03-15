#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# add for testing

# Define UI for application that draws a histogram
ui = dashboardPage(
    dashboardHeader(title = "iTIME"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Importing Data", tabName = 'import', icon = icon('upload')),
            menuItem("Univariate Summary", tabName = 'univariate', icon = icon('angle-right')),
            menuItem("Multivariate Summary", tabName = 'multivariate', icon = icon('angle-double-right')),
            menuItem("Spatial", tabName = 'spatial', icon = icon('braille')),
            menuItem("Getting Started", tabName = 'help', icon = icon('glasses')),
            tags$br(),
            fluidRow(column(12, align="center",
                            tags$br(),
                            tags$img(src = "moffitt-logo.png",
                                     position = "absolute",
                                     bottom = "25px",
                                     width = "100px", 
                                     height = "100px")))
            # tags$a(
            #     href = "https://lab.moffitt.org/fridley/", 
            #     target = "_blank", 
            #     tags$div(class = "moffitt-logo")
            # )
        )
    ),
    dashboardBody(
        custom_blue,
        tabItems(
            tabItem(tabName = 'import',
                    h1("Import Data", align="center"),
                    fluidRow(
                        box( status = "primary",
                            fileInput("summaryData", "Choose a Summary File",
                                      multiple = FALSE,
                                      accept = c("csv",
                                                 "HALO summary data file",
                                                 c(".csv"))),
                            uiOutput("choose_summary_merge")
                        ),
                        
                        box( status = "primary",
                            fileInput("clinicalData", "Choose a Clinical Data File",
                                      multiple = FALSE,
                                      accept = c("csv",
                                                 "HALO summary data file",
                                                 c(".csv"))),
                            uiOutput("choose_clinical_merge")
                        ),
                        
                        box( status = "primary",
                            fileInput("spatialData", "Choose a Spatial Data File",
                                      multiple = FALSE,
                                      accept = c("csv",
                                                 "HALO summary data file",
                                                 c(".csv")))
                            #,uiOutput("choose_spatial_merge")
                        ),
                        box( status = "primary",
                            actionButton(
                                inputId = "exampleData",
                                label = "Load Example Data"
                            ),
                            style = "height:118px"
                        )
                    ),
            ),
            
            tabItem(tabName = 'univariate',
                    h1("Univariate Summary and Visualization", align="center"),
                    fluidRow(
                        
                        box(title = "Selection Variables", status = "primary",
                            width = 4,
                            uiOutput("choose_marker"),
                            selectInput("choose_cont_thresh", "Select Contingency Threshold",
                                        choices = c("1%" = 1, 
                                                    "2%" = 2, 
                                                    "3%" = 3,
                                                    "4%" = 4,
                                                    "5%" = 5),
                                        selected = 1),
                            uiOutput("choose_clinical"),
                            selectInput("summaryPlotType", "Select Plot Type",
                                        choices = c("Boxplot" = 1,
                                                    "Violin Plot" = 2, 
                                                    "Histogram" = 3,
                                                    "Scatter Plot" = 4),
                                        selected = 1),
                            selectInput("summaryPlotColors", "Select Color Scheme",
                                        choices = c("Magma" = "magma", 
                                                    "Viridis" = "viridis", 
                                                    "Plasma" = "plasma", 
                                                    "Inferno" = "inferno"),
                                        selected = "viridis"),
                            awesomeCheckbox("sqrt_transform", "Square Root Transformation",
                                          value = FALSE),
                        style = "height:575px"
                            ),
                        
                        box(width = 8, 
                            title = "Summary Plot", status = "primary",
                            plotOutput("boxplot", height = 520),
                        downloadButton('download_boxplot', "Download Plot")
                            )
                        )
                    ,
                    
                    fluidRow(
                        box(width = 4, 
                            title="Contingency Table", status = "primary",
                            div(style = 'height:120px; overflow-x: hidden; overflow-y: scroll', tableOutput('contTable'))
                            ),
                        
#                        box(width = 3, 
#                            title="Frequency Table", status = "primary",
#                            tableOutput("freqTable"),
#                            style = "height:140px",
#                            column(12,align="center")
#                        ),
                        
                        column(width = 8,
                            
                            box(width = NULL,
                                title="Summary Table", status = "primary",
                                div(style = 'height:120px', tableOutput('summaryTable')) #for scroll in style "; overflow-x: scroll"
                                )
                            )
                        
                        )
            ),
            tabItem(tabName='multivariate',
                    h1("Multivariate Summary and Visualization", align="center"),
                    fluidRow(
                        
                        box(width = 4, status = "primary",
                            uiOutput("choose_heatmap_clinical"),
                            selectInput("heatmap_transform", "Select Transformation Method",
                                        choices=c("None" = "none",
                                                  "Square Root" = "square_root"),
                                        selected="square_root"),
                            awesomeCheckbox("cluster_heatmap_annotation", "Group Annotation",
                                            value = TRUE),
                            awesomeCheckbox("cluster_heatmap_Marker", "Cluster by Marker",
                                            value = TRUE),
                            uiOutput("choose_heatmap_marker"),
                            tags$style("awesome-checkbox-group-custom {background-color: #2cdeeb;}"),
                        ),
                        
                        box(title = "Heatmap",
                            width = 8, status = "primary",
                            plotOutput("heatmap", height = 510),
                            downloadButton('download_heatmap', "Download Heatmap"),
                            height = 607
                        )
                    ),
                )
            ,
            tabItem(tabName = 'spatial',
                    h1("Spatial Summary", align="center"),
                    box(title = "Spatial Plot Selections",
                        width=4, status = "primary",
                        uiOutput("choosePlotlyMarkers"),
                        height = 607
                    ),
                    
                    box(title = "Spatial Plot",
                        width = 8, status = "primary",
                        plotlyOutput("spatial_plotly", height = 545)
                    ),
                    
                    box(title = "Spatial Clustering Estimator Selections",
                        width=4, status = "primary",
                        uiOutput("choose_ripley")
                        ,selectInput("ripleysEstimator", "Select an Estimator",
                                     choices = c("Ripley's K" = "K",
                                                 "Besag's L" = "L",
                                                 "Marcon's M" = "M"),
                                     selected = "K"),
                        height = 
                    ),
                    
                    box(title = "Plot of Spatial Clustering Estimator",
                        width = 8, status = "primary",
                        plotOutput("ripleysPlot", height = 250)
                    ),
                    HTML('<footer>
                         In cases of large holes or uneven cell distribution, the estimates of complete spatial randomness (CSR) may be inapporpriate measure.
                         </footer>')
                ),
            tabItem(tabName = 'help',
                    h1("Getting Started", align="center"),
                    box(width = 8, status = "primary",
                        uiOutput('gettingstarted'),
                        ),
                    
#                    box(#title = "Getting Started",
#                        width = 8, status = "primary",
#                        h3("Input Files"),
#                        p("iTIME accepts 3 files as input: a summary level file, ",
#                          "a clinical file, and a spatial file. All files should ",
#                          "be csv files. The summary file should contain summary level ",
#                          "statistics and should contain one row per sample, while ",
#                          "the clinical data file should contain one row per patient. ",
#                          "Both the clinical and sumary files should contain a variable ",
#                          "to link sample IDs and clinical IDs ('Merge Variable'), thought the variables do ",
#                          "not need to be named the same in both datasets."),
#                        
#                        h3("Summary"),
#                        p("The summary page can be used without uploading any spatial",
#                          "data. This page provides a summary of the marker chosen from the ",
#                          "dropdown menu below and allows users to plot a marker against",
#                          "clinical variables and select the appropriate plot type. A heatmap",
#                          "of the available markers is also produced and allows users the",
#                          "opportunity to annotate the heatmap by a clinical characteristics."),
#                        
#                        h3("Spatial"),
#                        p("The spatial page plots the individual cells by positivity for the markers",
#                          "and plots Ripley's K estimates over a range of r values."),
#                        br(),
#                        h3("Summary File"),
#                        img(src='summary-file.png', align = "center", height="100%", width="100%"),
#                        br(),
#                        h3("Clinical File"),
#                        img(src='clinical-file.png', align = "center", height="100%", width="100%"),
#                        br(),
#                        h3("Spatial File"),
#                        img(src='spatial-file.png', align = "center", height="100%", width="100%"),
#                        br()),
                    
                    box(title = "Development Team",
                        width = 4, status = "primary",
                        
                        p("- Brooke Fridley"),
                        p("- Alex Soupir"),
                        p("- Jordan Creed"),
                        p("- Chris Wilson"),
                        p("- Oscar Ospina"),
                        p("- Gregory Kimmel"),
                        p("- Joseph Markowitz"),
                        p("- Christelle Colin Leitzinger"),
                        p("- Nick Chakiryan"),
                        br(),
                        p("For more information about our lab and other projects please check",
                          "out our website at",
                          a("https://lab.moffitt.org/fridley/", href = "https://lab.moffitt.org/fridley/")),
                        br(),
                        p("All code for this project are publically available on",
                          a("GitHub.", href = "https://github.com/FridleyLab/iTIME")),
                        br(),
                        p("If you have any questions or comments, we would love to hear them.",
                          "You can email us at Fridley.Lab@moffitt.org or feel free to",
                          a("open an issue", href = "https://github.com/FridleyLab/iTIME/issues"),
                          "in our GitHub repo.")
                        )
                    )
        )
    )
)

