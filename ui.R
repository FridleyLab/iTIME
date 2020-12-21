#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
ui = dashboardPage(
    dashboardHeader(title = "iTIME"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Importing Data", tabName = 'import', icon = icon('table')),
            menuItem("Summary", tabName = 'summary', icon = icon('drafting-compass')),
            menuItem("Spatial", tabName = 'spatial', icon = icon('braille')),
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
                    fluidRow(
                        box(
                            fileInput("summaryData", "Choose a Summary File",
                                      multiple = FALSE,
                                      accept = c("csv",
                                                 "HALO summary data file",
                                                 c(".csv"))),
                            uiOutput("choose_summary_merge")
                        ),
                        
                        box(
                            fileInput("clinicalData", "Choose a Clinical Data File",
                                      multiple = FALSE,
                                      accept = c("csv",
                                                 "HALO summary data file",
                                                 c(".csv"))),
                            uiOutput("choose_clinical_merge")
                        ),
                        
                        box(
                            fileInput("spatialData", "Choose a Spatial Data File",
                                      multiple = FALSE,
                                      accept = c("csv",
                                                 "HALO summary data file",
                                                 c(".csv")))
                            #,uiOutput("choose_spatial_merge")
                        )
                    ),
            ),
            
            tabItem(tabName = 'summary',
                    fluidRow(
                        box(width = 12, 
                            title="Summary Table",
                            tableOutput("summaryTable")),
                        box(width = 4,
                            uiOutput("choose_clinical"),
                            uiOutput("choose_marker"),
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
                                        selected = "viridis")
                        ),
                        
                        box(width = 8, 
                            title = "Boxplot",
                            plotOutput("boxplot", height = 250)
                        )
                        ),
                    fluidRow(
                        
                        box(width = 4, 
                            uiOutput("choose_heatmap_clinical"),
                            uiOutput("choose_heatmap_marker"),
                            tags$style("awesome-checkbox-group-custom {background-color: #2cdeeb;}"),
                        ),
                        
                        box(width = 8, 
                            title = "Heatmap",
                            plotOutput("heatmap", height = 250),
                            height = 500
                        )
                    ),
                )
            ,
            tabItem(tabName = 'spatial',
                    box(title = "Spatial Plot Selections"
                        ,width=4,
                        uiOutput("choosePlotlyMarkers")
                    ),
                    
                    box(width = 8
                        ,title = "Spatial Plot"
                        ,plotlyOutput("spatial_plotly")
                    ),
                    
                    box(title = "Ripley's K Selections"
                        ,width=4
                        ,uiOutput("choose_ripley")
                        ,selectInput("ripleysEstimator", "Select an Estimator",
                                     choices = c("Ripley's K" = "K",
                                                 "Besag's L" = "L",
                                                 "Marcon's M" = "M"),
                                     selected = "K")
                    ),
                    
                    box(width = 8
                        ,title = "Ripley's Plot"
                        ,plotOutput("ripleysPlot", height = 250)
                    )
                )
        )
    )
)

