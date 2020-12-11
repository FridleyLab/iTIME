#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#jordan did it
custom_blue <- {shinyDashboardThemeDIY(
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "rgb(0,0,0)"
    ,primaryFontColor = "rgb(0,0,0)"
    ,infoFontColor = "rgb(0,0,0)"
    ,successFontColor = "rgb(0,0,0)"
    ,warningFontColor = "rgb(0,0,0)"
    ,dangerFontColor = "rgb(0,0,0)"
    ,bodyBackColor = "rgb(248,248,248)"
    
    ### header
    ,logoBackColor = "rgb(23,103,124)"
    
    ,headerButtonBackColor = "rgb(238,238,238)"
    ,headerButtonIconColor = "rgb(75,75,75)"
    ,headerButtonBackColorHover = "rgb(210,210,210)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    
    ,headerBackColor = "rgb(238,238,238)"
    ,headerBoxShadowColor = "#aaaaaa"
    ,headerBoxShadowSize = "2px 2px 2px"
    
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "rgb(20,97,117)"
        ,colorMiddle = "rgb(56,161,187)"
        ,colorEnd = "rgb(3,22,56)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
    )
    ,sidebarPadding = 0
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#aaaaaa"
    
    ,sidebarUserTextColor = "rgb(255,255,255)"
    
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
    
    ,sidebarTabTextColor = "rgb(255,255,255)"
    ,sidebarTabTextSize = 13
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "rgb(35,106,135)"
    ,sidebarTabBorderWidth = 1
    
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(0,0,0)"
    ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
    
    ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "rgb(75,126,151)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 20px 20px 0px"
    
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(210,214,220)"
    ,boxPrimaryColor = "rgba(44,222,235,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(44,222,235,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"
    
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgba(44,222,235,1)"
    ,tabBoxBorderRadius = 5
    
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
)}


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
                        box(width = 3,
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
                        box(width = 9, 
                            title="Summary Table",
                            tableOutput("summaryTable")),
                        
                        box(width = 9, 
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
                    
                    box(width = 8
                        ,title = "Spatial Image Plot"
                        #,plotOutput()
                    ),
                    
                    box(title = "Ripley's K Selections"
                        ,width=4
                        ,uiOutput("choose_ripley")
                        ,selectInput("ripleysEstimator", "Select an Estimator",
                                     choices = c("Ripley's K" = "K",
                                                 "Ripley's T" = "L",
                                                 "Nick's M" = "M"),
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

