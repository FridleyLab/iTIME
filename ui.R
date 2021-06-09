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
                        column(width = 8,
                               
                            box(width = NULL,
                                title="Summary Table", status = "primary",
                                div(style = 'height:120px', tableOutput('summaryTable')) #for scroll in style "; overflow-x: scroll"
                                )
                            )
                        ),
                    fluidRow(
                        box(width = 12,
                            title="Cumulative Distribution Function (CDF) Plots", status = "primary",
                            plotOutput("cdfplot"),
                            downloadButton('download_cdfplot', 'Download Plot')
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
                    fluidRow(
                        box(title = "PCA Plot", 
                            width = 4, status = "primary",
                            plotOutput("pca"),
                            downloadButton("download_pca", "Download PCA Plot")
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

