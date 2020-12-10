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
            menuItem("Summary Page", tabName = 'summary', icon = icon('dashboard')),
            menuItem("Spatial Page", tabName = 'spatial', icon = icon('th'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'summary',
                    box(
                        fileInput("summaryData", "Choose a Summary File",
                                  multiple = FALSE,
                                  accept = c("csv",
                                             "HALO summary data file",
                                             c(".csv")))
                    ),
                    
                    plotOutput("boxplot", height = 250)
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