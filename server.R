#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# require mappable subject ID between files

library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(spatstat)
options(shiny.maxRequestSize = 30*1024^2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    summary_data = reactive({
        infile = input$summaryData
        if(is.null(infile)){
            return()
        }
        
<<<<<<< HEAD
        df = read.csv(infile$datapath,check.names = FALSE)
=======
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
        
        df = read.csv(infile$datapath)
>>>>>>> shiny-alex
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
        
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        # generate bins based on input$bins from ui.R
<<<<<<< HEAD
        x    <- summary_data()[, input$picked_marker]

        # draw the histogram with the specified number of bins
        boxplot(x, col = 'darkgray', na.rm=TRUE)
=======
        cellvar =  input$picked_clinical
        clinvar <- input$picked_marker
        
        data_table = summary_data_merged()

        # draw the histogram with the specified number of bins
        #summary_plots = summary_plots_fn(summary_data_table, y, x)
        #summary_plots[[1]]
        
        plots = summary_plots_fn(data_table, clinvar, cellvar)
        
        plots[[as.integer(input$summaryPlotType)]]
>>>>>>> shiny-alex

    })
    
    output$choose_summary_merge = renderUI({
        
        summary_column_names = colnames(summary_data())
        
<<<<<<< HEAD
        # Keep only acceptable column names used for potential plotting
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         cols,perl=TRUE,ignore.case = TRUE)
        
        acceptable_column_names = summary_column_names[whichcols]
        
        # Remove markers that are not present from being plotted
        acceptable_column_names = acceptable_column_names[sapply(spatial[,newcols],var)>0]
        
        
        selectInput("picked_marker", "Choose marker",
                    choices = acceptable_column_names,
                    selected = acceptable_column_names[1])
=======
        selectInput("summary_merge", "Choose merge merge",
                    choices = summary_column_names,
                    selected = summary_column_names[1])
>>>>>>> shiny-alex
        
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
        
        selectInput("picked_marker", "Choose Clinical Variable to Plot",
                    choices = spatial_spatial_names,
                    selected = spatial_spatial_names[1])
        
    })
    
    output$choose_clinical = renderUI({
        
        spatial_spatial_names = colnames(summary_data_merged())
        
        selectInput("picked_clinical", "Choose Cell Marker to Plot",
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

summary_plots_fn <- function(datatable, clinvar, cellvar){
    box_p <- ggplot(datatable, aes(x=get(clinvar), y=get(cellvar), fill=get(clinvar))) + 
        geom_boxplot() +
        xlab(str_to_title(clinvar)) + ylab(gsub("_", " ", str_to_title(cellvar))) +
        labs(fill=str_to_title(clinvar))
    
    violin_p <- ggplot(datatable, aes(x=get(clinvar), y=get(cellvar), fill=get(clinvar))) + 
        geom_violin() +
        xlab(str_to_title(clinvar)) + ylab(gsub("_", " ", str_to_title(cellvar))) +
        labs(fill=str_to_title(clinvar))
    
    hist_p <- ggplot(datatable, aes(x=get(cellvar), color=get(clinvar))) + 
        geom_histogram(binwidth=100, fill='white') +
        xlab(str_to_title(gsub("_", " ", cellvar))) + ylab("Count") +
        labs(color=str_to_title(clinvar))
    
    summ_plots <- list(box_p, violin_p, hist_p)
    
    return(summ_plots)
    
}

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