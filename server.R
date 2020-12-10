#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
options(shiny.maxRequestSize = 30*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$summaryout = DT::renderDataTable({
        if(is.null(input$summaryData)){
            return()
        }
        
        temp = read.csv(input$summaryData$datapath)
        #print(colnames(temp))
        #DT::datatable(as.data.frame(temp), options = list(scrollX = TRUE))
        
        assign('summary_data', temp, envir=.GlobalEnv)
        assign('spatial_column_names', colnames(temp), envir=.GlobalEnv)
    })

    output$boxplot <- renderPlot({
        if(is.null(input$summaryData)){
            return()
        }

        # generate bins based on input$bins from ui.R
        x    <- summary_data[, 3]
        bins <- seq(min(x), max(x), length.out = 25)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
