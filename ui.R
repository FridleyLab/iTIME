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
            menuItem("Summary Page", tabName = 'summary', icon = icon('drafting-compass')),
            menuItem("Spatial Page", tabName = 'spatial', icon = icon('braille'))
        )
    ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
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
                        box(width = 4,
                            uiOutput("choose_clinical"),
                            uiOutput("choose_marker"),
                            selectInput("summaryPlotType", "Select Plot Type",
                                        choices = c("Boxplot" = 1,
                                                    "Violin Plot" = 2, 
                                                    "Histogram" = 3)),
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
                )
            ,
            tabItem(tabName = 'spatial',
                    box(title = "Spatial Image Selections"
                        ,width=4
                    ),
                    
                    box(title = "Spatial Image Plot"
                        #,plotOutput()
                    ),
                    
                    box(title = "Ripley's K Selections"
                        ,width=4
                        ,uiOutput("choose_ripley")
                        ,selectInput("ripleysEstimator", "Select an Estimator",
                                     choices = c("Ripley's K" = "K",
                                                 "Besag's L" = "L",
                                                 "Ripley's T" = "T"),
                                     selected = "K")
                    ),
                    
                    box(title = "Ripley's Plot"
                        ,plotOutput("ripleysPlot", height = 250)
                    )
                )
        )
    )
)

