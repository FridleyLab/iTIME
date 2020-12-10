#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

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
                                                 c(".csv"))),
                            uiOutput("choose_spatial_merge")
                        )
                    ),
            ),
            
            tabItem(tabName = 'summary',
                    fluidRow(
                        box(width = 4,
                            uiOutput("choose_marker"),
                            uiOutput("choose_clinical"),
                            selectInput("summaryPlotType", "Select Plot Type",
                                        choices = c("Boxplot" = 1,
                                                    "Violin Plot" = 2, 
                                                    "Histogram" = 3)),
                            selectInput()
                        ),
                        
                        box(width = 8, 
                            title = "Boxplot",
                            plotOutput("boxplot", height = 250)
                            )
                    ),
                ),
            tabItem(tabName = 'spatial',
                    box(
                        fileInput("spatialData", "Choose a Spatial Data File",
                                  multiple = FALSE,
                                  accept = c("csv",
                                             "HALO Spatial data file",
                                             c(".csv"))),
                        
                        fileInput("clinicalData", "Choose a Clinical Data File",
                                  multiple = FALSE,
                                  accept = c("csv",
                                             "Patient clinical data file",
                                             c(".csv")))
                    )
                )
        )
    )
)

