#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# add for testing

#

#Melanoma was sox10 in the study for tumor stroma
#nicks plotly sort

ui = dashboardPage(
    dashboardHeader(title = "iTIME"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Importing Data", tabName = 'import', icon = icon('upload')),
            menuItem("Univariate Summary", tabName = 'univariate', icon = icon('angle-right')),
            menuItem("Multivariate Summary", tabName = 'multivariate', icon = icon('angle-double-right')),
            menuItem("Spatial", tabName = 'spatial', icon = icon('braille')),
            menuItem("About", tabName = 'about', icon = icon('glasses')),
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
                    h1("iTIME", align="center"),
                    fluidRow(
                        box(width = 12, status = "primary",
                            fluidRow(
                                column(width = 8,
                                       uiOutput("getting_started"),
                                       ),
                                column(width = 4,
                                       fluidRow(
                                           column(
                                               width = 12,
                                               div(style="float:right", actionButton("exampleData", "Load Example Data"))
                                           )
                                       ),
                                       fileInput("summaryData", "Choose a Summary File",
                                                 multiple = FALSE,
                                                 accept = c("csv",
                                                            "HALO summary data file",
                                                            c(".csv"))),
                                       uiOutput("choose_summary_merge"),
                                       #div(style = 'overflow-x: scroll; overflow-y: scroll; height:200px', tableOutput('summary_preview')),
                                       fileInput("clinicalData", "Choose a Clinical Data File",
                                                 multiple = FALSE,
                                                 accept = c("csv",
                                                            "HALO summary data file",
                                                            c(".csv"))),
                                       uiOutput("choose_clinical_merge"),
                                       #div(style = 'overflow-x: scroll; overflow-y: scroll; height:200px', tableOutput('clinical_preview')),
                                       fileInput("spatialData", "Choose a Spatial Data File",
                                                 multiple = FALSE,
                                                 accept = c("csv",
                                                            "HALO summary data file",
                                                            c(".csv"))),
                                       #div(style = 'overflow-x: scroll; overflow-y: scroll; height:200px', tableOutput('spatial_preview'))
                                       ),
                                ),
                            fluidRow(
                                column(width = 6),
                                column(width = 6, h2("", style = "font-size:12pt;font-weight:bold;margin-bottom:1.55em"),
                                       
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
                                                        "Scatter Plot" = 4,
                                                        "Stacked Bar Plot" = 5),
                                            selected = 1),
                                selectInput("summaryPlotColors", "Select Color Scheme",
                                            choices = c("Magma" = "magma", 
                                                        "Viridis" = "viridis", 
                                                        "Plasma" = "plasma", 
                                                        "Inferno" = "inferno"),
                                            selected = "viridis"),
                                selectInput("uni_transformation", "Select Transformation",
                                            choices = c("None" = "none",
                                                        "Square Root" = "sqrt_transform"
                                                        ,
                                                        #"Log 2 Tranformation (0.0001)" = "log2_transform",
                                                        "Logit Tranformation (0.0001)" = "logit_transform"
                                                        )
                                            )
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
                        box(width = 12, status = 'primary', title = textOutput("selectedModelName"),
                            column(width = 4,
                                   column(width = 12, 
                                          h2("Modeling Variables",align="center", style = "font-size:14pt"),
                                          uiOutput("choose_total_cells"),
                                          uiOutput("modeling_reference")),
                                   column(width = 12,
                                          uiOutput("choose_uni_covariates")),
                                   column(width = 12, align = "center", h2("Beta Binomial Model Statistics",align="center", style = "font-size:14pt"),
                                          div(style = 'overflow-x: scroll', tableOutput('model_stats'))),
                                   column(width = 12,
                                          # selectInput("selectedModel", "Select Desired Model",
                                          #             choices = c("Poisson",
                                          #                         "Negative Binomial",
                                          #                         "Zero Inflated Binomial",
                                          #                         "Binomial",
                                          #                         "Beta Binomial",
                                          #                         "Zero Inflated Poisson"),
                                          #             selected = "Beta Binomial")
                                          htmlOutput("modelingDescription"))
                                   ),
                            column(width = 8,
                                   column(width = 12, h2("Cumulative Distribution Function (CDF)", align="center", style = "font-size:14pt"), 
                                          status = "primary",
                                          plotOutput("cdfplot"),
                                          downloadButton('download_cdfplot', 'Download Plot'))
                                ),
                            # column(width = 4, align = "center",
                            #        h2("Akaike Information Criterion (AIC)",align="center", style = "font-size:14pt"),
                            #        h2("Lower indicates a better model fit...",align="center", style = "font-size:10pt"),
                            #        div(style = 'overflow-x: scroll', tableOutput('aic_table')))#height:120px; ; overflow-y: scroll
                        )
                    ),
                    fluidRow(
                        box(width = 12, status = "primary",
                            downloadButton(
                                outputId = "univariate_report",
                                label = "Download Univariate Report"
                            ),
                            checkboxInput(
                                "printFunctions", 
                                "Print Functions in Report?",
                                value=F
                            )
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
                                   column(width = 6, h2("Principal Component Analysis (PCA)", align="center", style = "font-size:14pt"),
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
                               plotlyOutput("spatial_plotly", width = "85%", height = "575"),
                               #downloadButton('download_spatialPlotly', 'Download Plot')
                               )
                        ),
                    box(width = 12, status = "primary",
                        column(width = 4, h2("Spatial  Plot Selections", style = "font-size:14pt"),
                               uiOutput("choose_ripley")
                               ,selectInput("ripleysEstimator", "Select an Estimator",
                                            choices = c("Ripley's K" = "K",
                                                        "Besag's L" = "L",
                                                        "Marcon's M" = "M",
                                                        "Nearest Neighbor G" = "G"),
                                            selected = "K")
                               ),
                        column(width = 8, h2("Spatial Plot", align="center", style = "font-size:14pt"),
                               plotOutput("ripleysPlot", height = 350),
                               downloadButton('download_ripley', 'Download Plot')
                               )
                        ),
                        HTML('<footer>
                         In cases of large holes or uneven cell distributions, the estimates of complete spatial randomness (CSR) may be an inaccurate measure.
                         </footer>')
                    ),
            tabItem(tabName = 'about',
                    h1("About iTIME", align="center"),
                    fluidRow(
                        box(width = 9, status = "primary",
                            uiOutput('aboutitime'),
                        ),
                        
                        box(title = "Development Team",
                            width = 3, status = "primary",
                            
                            p("- Brooke Fridley"),
                            p("- Alex Soupir"),
                            p("- Chris Wilson"),
                            p("- Jordan Creed"),
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

