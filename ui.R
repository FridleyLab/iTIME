#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# add for testing

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
                                     height = "100px")
                            )
                     )
            )
        ),
    dashboardBody(
        custom_blue,
        tabItems(
            tabItem(tabName = 'import',
                    h1("Import Data", align="center"),
                    fluidRow(
                        box(width = 12, status = "primary",
                            fluidRow(
                                column(width = 6,
                                       fileInput("summaryData", "Choose a Summary File",
                                                 multiple = FALSE,
                                                 accept = c("csv",
                                                            "HALO summary data file",
                                                            c(".csv"))),
                                       uiOutput("choose_summary_merge"),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height:200px', tableOutput('summary_preview'))),
                                column(width = 6,
                                       fileInput("clinicalData", "Choose a Clinical Data File",
                                                 multiple = FALSE,
                                                 accept = c("csv",
                                                            "HALO summary data file",
                                                            c(".csv"))),
                                       uiOutput("choose_clinical_merge"),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height:200px', tableOutput('clinical_preview')))),
                            fluidRow(
                                column(width = 6,# h2("Choose a Spatial Data File", style = "font-size:12pt;font-weight:bold"),
                                       fileInput("spatialData", "Choose a Spatial Data File",
                                                 multiple = FALSE,
                                                 accept = c("csv",
                                                            "HALO summary data file",
                                                            c(".csv"))),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height:200px', tableOutput('spatial_preview'))),
                                column(width = 6, h2("", style = "font-size:12pt;font-weight:bold;margin-bottom:1.55em"),
                                       actionButton(
                                           inputId = "exampleData",
                                           label = "Load Example Data"
                                           )
                                       ),style = "height:118px"
                                ),
                            ),
                    ),
            ),
            
            tabItem(tabName = 'univariate',
                    h1("Univariate Summary and Visualization", align="center"),
                    fluidRow(
                        box(status = "primary", width = 12,
                            column(h2("Select Variables",align="center", style = "font-size:14pt"), status = "primary",
                                width = 3,
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
                                              value = FALSE)
                                ),
                            
                            column(width = 9,
                                   column(width = 7,h2("Summary Plot",align="center", style = "font-size:14pt"),
                                plotOutput("boxplot", height = 520),
                                downloadButton('download_boxplot', "Download Plot"),
                            ),
                                column(width = 5, align = "center", h2("Contingency Table",align="center", style = "font-size:14pt"),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll', tableOutput('contTable')),
                                       column(width = 12, align = "center", h2("Frequency Table",align="center", style = "font-size:14pt"),
                                              div(style = 'overflow-x: scroll; overflow-y: scroll', tableOutput('freqTable')),
                                              column(width = 12, h2("Summary Table",align="center", style = "font-size:14pt"),
                                                     div(style = 'overflow-x: scroll; overflow-y: scroll', tableOutput('summaryTable'))
                                                     )
                                              ),
                                       )
                            ),
                            )
                        ),
                    
                    fluidRow(
                        box(width = 12, status = 'primary', title = 'Modeling',
                            column(width = 4,
                                   column(width = 9, h2("Modeling Variables",align="center", style = "font-size:14pt"),
                                   uiOutput("choose_total_cells"),
                                   uiOutput("modeling_reference"),
                                   selectInput("selectedModel", "Select Desired Model",
                                               choices = c("Poisson" = "p", 
                                                           "Negative Binomial" = "nb", 
                                                           "Zero Inflated Binomial" = "zib", 
                                                           "Binomial" = "b", 
                                                           "Beta Binomial" = "bb", 
                                                           "Zero Inflated Poisson" = "zip"),
                                               selected = "bb")),
                                   column(width = 12, align = "center", h2("Model Fit",align="center", style = "font-size:14pt"),
                                          div(style = 'overflow-x: scroll', tableOutput('model_stats')))
                                   ),
                            column(width = 5,
                                   h2("Binomial Family Plots", align="center", style = "font-size:14pt"), status = "primary",
                                plotOutput("cdfplot"),
                                downloadButton('download_cdfplot', 'Download Plot')
                                ),
                            column(width = 3, align = "center",
                                   h2("Akaike Information Criterion (AIC)",align="center", style = "font-size:14pt"),
                                   div(style = 'overflow-x: scroll', tableOutput('aic_table')))#height:120px; ; overflow-y: scroll
                        )
                    )
            ),
            tabItem(tabName='multivariate',
                    h1("Multivariate Summary and Visualization", align="center"),
                    fluidRow(
                        box(width = 12, status = "primary",
                            column(width = 3,
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
                            column(width = 9,
                                   column(width = 6, h2("Heatmap", align="center", style = "font-size:14pt"),
                                          plotOutput("heatmap", width="100%"),#height = 510
                                          downloadButton('download_heatmap', "Download Heatmap"),
                                          ),
                                   column(width = 6, h2("Pricipal Component Analysis (PCA)", align="center", style = "font-size:14pt"),
                                          plotOutput("pca", width="100%"),
                                          downloadButton("download_pca", "Download PCA Plot", style = "margin-bottom:25px")
                                          )
                                   )
                            
                            ),
                        ),
                    ),
            tabItem(tabName = 'spatial',
                    h1("Spatial Summary", align="center"),
                    box(width = 12, status = "primary",
                        column(width = 4, h2("Spatial Plot Selections", style = "font-size:14pt"),
                               uiOutput("choosePlotlyMarkers")),
                        column(width = 8, h2("Spatial Plot", align="center",style = "font-size:14pt"),
                               plotlyOutput("spatial_plotly", width = "85%", height = "575")
                               )
                        ),
                    box(width = 12, status = "primary",
                        column(width = 4, h2("Ripley's K Plot Selections", style = "font-size:14pt"),
                               uiOutput("choose_ripley")
                               ,selectInput("ripleysEstimator", "Select an Estimator",
                                            choices = c("Ripley's K" = "K",
                                                        "Besag's L" = "L",
                                                        "Marcon's M" = "M"),
                                            selected = "K")
                               ),
                        column(width = 8, h2("Ripley's K Plot", align="center", style = "font-size:14pt"),
                               plotOutput("ripleysPlot", height = 350)
                               )
                        ),
                        HTML('<footer>
                         In cases of large holes or uneven cell distribution, the estimates of complete spatial randomness (CSR) may be inapporpriate measure.
                         </footer>')
                    ),
            tabItem(tabName = 'help',
                    h1("Getting Started", align="center"),
                    fluidRow(
                        box(width = 9, status = "primary",
                            uiOutput('gettingstarted'),
                        ),
                        
                        box(title = "Development Team",
                            width = 3, status = "primary",
                            
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
                              a("GitHub.", href = "https://github.com/FridleyLab/iTIME"))
                        )
                    )
            )
        )
    )
)

