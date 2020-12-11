#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# require mappable subject ID between files



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
<<<<<<< HEAD
        x    <- summary_data()[, input$picked_marker]

        # draw the histogram with the specified number of bins
        boxplot(x, col = 'darkgray', na.rm=TRUE)
=======
        cellvar =  input$picked_clinical
        clinvar <- input$picked_marker
=======
        cellvar <-  input$picked_marker
        clinvar <- input$picked_clinical
        colorscheme <- input$summaryPlotColors
>>>>>>> origin/shiny-alex
        
        data_table = summary_data_merged()
        
        plots = summary_plots_fn(data_table, clinvar, cellvar, colorscheme)
        
        plots[[as.integer(input$summaryPlotType)]]
>>>>>>> shiny-alex

    })
    
    output$ripleysPlot = renderPlot({
        if(is.null(spatial_data())){
            return()
        }
        
        Ripley(spatial_data(), input$ripleys_selection, input$ripleysEstimator)
    })
    
    output$choose_summary_merge = renderUI({
        
        summary_column_names = colnames(summary_data())
        
<<<<<<< HEAD
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
=======
        selectInput("summary_merge", "Choose Summary Merge Variable",
>>>>>>> origin/shiny-alex
                    choices = summary_column_names,
                    selected = summary_column_names[1])
>>>>>>> shiny-alex
        
    })
    
    output$choose_clinical_merge = renderUI({
        
        clinical_column_names = colnames(clinical_data())
        
        selectInput("clinical_merge", "Choose Clinical Merge Variable",
                    choices = clinical_column_names,
                    selected = clinical_column_names[1])
        
    })
    
    output$choose_spatial_merge = renderUI({
        
        spatial_column_names = colnames(spatial_data())
        
        selectInput("spatial_merge", "Choose Spatial Merge Variable",
                    choices = spatial_column_names,
                    selected = spatial_column_names[1])
        
    })
    
    output$choose_marker = renderUI({
        
        summary_marker_names = colnames(summary_data())
        
        selectInput("picked_marker", "Choose Cell Marker to Plot",
                    choices = summary_marker_names,
                    selected = summary_marker_names[1])
        
    })
    
    output$choose_clinical = renderUI({
        
        summary_clinical_names = colnames(clinical_data())
        
        selectInput("picked_clinical", "Choose Clinical Variable to Plot",
                    choices = summary_clinical_names,
                    selected = summary_clinical_names[1])
        
    })
    
    output$choose_ripley = renderUI({
        
        ripleys_spatial_names = colnames(Filter(is.numeric, spatial_data()))
        
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         ripleys_spatial_names,perl=TRUE,ignore.case = TRUE)
        tmp = ripleys_spatial_names[whichcols]
        acceptable_ripleys_names =  tmp[sapply(spatial_data()[,tmp],sum)>0]
        
        selectInput("ripleys_selection", "Choose Marker for Ripleys",
                    choices = acceptable_ripleys_names,
                    selected = acceptable_ripleys_names[1])
        
    })
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        return(df)
    })

})