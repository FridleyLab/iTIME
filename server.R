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
        
        df = read.csv(infile$datapath, check.names = FALSE)
        return(df)
    })
    
    clinical_data = reactive({
        infile = input$clinicalData
        if(is.null(infile)){
            return()
        }
        
        df = read.csv(infile$datapath, check.names = FALSE)
        return(df)
    })
    
    spatial_data = reactive({
        infile = input$spatialData
        if(is.null(infile)){
            return()
        }
        
        df = read.csv(infile$datapath, check.names = FALSE)
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
        x    <- summary_data_merged()[, input$picked_marker]
        bins <- seq(min(x), max(x), length.out = 25)

        # draw the histogram with the specified number of bins
        boxplot(x, breaks = bins, col = 'darkgray', na.rm=TRUE)

    })
    
    output$choose_summary_merge = renderUI({
        
        summary_column_names = colnames(summary_data())
        
        selectInput("summary_merge", "Choose merge merge",
                    choices = summary_column_names,
                    selected = summary_column_names[1])
        
    })
    
    output$choose_clinical_merge = renderUI({
        
        clinical_column_names = colnames(clinical_data())
        
        selectInput("clinical_merge", "Choose merge merge",
                    choices = clinical_column_names,
                    selected = clinical_column_names[1])
        
    })
    
    output$choose_spatial_merge = renderUI({
        
        spatial_spatial_names = colnames(spatial_data())
        
        selectInput("spatial_merge", "Choose merge merge",
                    choices = spatial_spatial_names,
                    selected = spatial_spatial_names[1])
        
    })
    
    output$choose_marker = renderUI({
        
        spatial_spatial_names = colnames(summary_data_merged())
        
        selectInput("picked_marker", "Choose merge merge",
                    choices = spatial_spatial_names,
                    selected = spatial_spatial_names[1])
        
    })
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        return(df)
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