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
    
    summary_data = reactive({
        infile = input$summaryData
        if(is.null(infile)){
            return()
        }
        
        df = read.csv(infile$datapath,check.names = FALSE)
        return(df)
    })
    
    output$summaryout = DT::renderDataTable({
        
        #temp = summary_data()
        #print(colnames(temp))
        DT::datatable(summary_data(), options = list(scrollX = TRUE))
        
        #assign('summary_data', temp, envir=.GlobalEnv)
        #assign('spatial_column_names', colnames(temp), envir=.GlobalEnv)
    })

    output$boxplot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- summary_data()[, input$picked_marker]

        # draw the histogram with the specified number of bins
        boxplot(x, col = 'darkgray', na.rm=TRUE)

    })
    
    output$choose_marker = renderUI({
        
        summary_column_names = colnames(summary_data())
        
        # Keep only acceptable column names used for potential plotting
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         cols,perl=TRUE,ignore.case = TRUE)
        
        acceptable_column_names = summary_column_names[whichcols]
        
        # Remove markers that are not present from being plotted
        acceptable_column_names = acceptable_column_names[sapply(spatial[,newcols],var)>0]
        
        
        selectInput("picked_marker", "Choose marker",
                    choices = acceptable_column_names,
                    selected = acceptable_column_names[1])
        
    })

})

# output$boxplot <- renderPlot({
#     
#     # create list of inputs
#     summary_data <- summaryTable()
#     # main_marker <- input$marker # charcter
#     # clinical_marker <- input$clinical.variable # character value
#     
#     
#     #transform the inputs into tidy compatable pieces
#     
#     
#     
#     # draw the boxplot
#     boxplot.1<-ggplot(summary_data, aes(x = summary_data[[main_marker]], y=clinical_data[[clinical_marker]], fill = clinical_data[[clinical_variable]]))+
#         geom_boxplot()
#     boxplot.1+geom_jitter(shape=16, position=position_jitter(0.2))
#     
# })