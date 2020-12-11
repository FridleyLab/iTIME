# iteractive Tumor Immune MicroEnvironment
# 
# HALO output

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
        
        df = read.csv(infile$datapath, check.names = FALSE)
        colnames(df) <- gsub("\\%", 'Percent', colnames(df))
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
        cellvar <-  input$picked_marker
        clinvar <- input$picked_clinical
        colorscheme <- input$summaryPlotColors
        
        data_table = summary_data_merged()
        #assign("summary_table", data_table, envir = .GlobalEnv)
        
        plots = summary_plots_fn(data_table, clinvar, cellvar, colorscheme)
        
        plots[[as.integer(input$summaryPlotType)]]

    })
    
    output$heatmap = renderPlot({
        heatmap_data = summary_data_merged()
        
        heat_map(summary_clinical_merge = heatmap_data,
                 markers = input$heatmap_selection,
                 clin_vars = input$picked_clinical_factor)
    }, height = 400)
    
    output$choose_heatmap_marker = renderUI({
        heatmap_names = colnames(summary_data())
        
        heatmap_names2 = heatmap_names[grep("^(?=Percent.*)",
                              heatmap_names,perl=TRUE,ignore.case = TRUE)]
        
        checkboxGroupInput("heatmap_selection", "Choose Cell Marker for Heatmap",
                           choices = heatmap_names2
                           ,selected = heatmap_names2
        )
    })
    
    output$choose_heatmap_clinical = renderUI({
        
        clinical_heatmap_names = colnames(clinical_data())
        
        selectInput("picked_clinical_factor", "Choose Annotation for Heatmap",
                    choices = clinical_heatmap_names,
                    selected = clinical_heatmap_names[3])
        
    })
    
    output$spatial_plotly = renderPlotly({
        validate(need(input$plotly_selection !="", "Please wait while things finish loading....."))
        
        markers = input$plotly_selection
        new_names = markers
        scatter_plotly(data = spatial_data(), markers = markers, new_names = new_names)
    })
    
    output$choosePlotlyMarkers = renderUI({
        ripleys_spatial_names = colnames(Filter(is.numeric, spatial_data()))
        
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         ripleys_spatial_names,perl=TRUE,ignore.case = TRUE)
        tmp = ripleys_spatial_names[whichcols]
        acceptable_ripleys_names =  tmp[sapply(spatial_data()[,tmp],sum)>0]
        
        checkboxGroupInput("plotly_selection", "Choose Markers for Spatial Plot",
                    choices = rev(acceptable_ripleys_names)
                    ,selected = acceptable_ripleys_names[grep("^(?=.*Opal)",acceptable_ripleys_names, perl=TRUE)]
                    )
    })
    
    output$summaryTable = renderTable({
        #temp = data.frame("Number of Subjects" = length(unique(summary_data()$subID)),
        #                  "test2" = length(summary_data()[,2]))
        data_table = summary_data_merged()
        cellvar <-  input$picked_marker
        sub_id = input$summary_merge
        
        temp = data.frame("Min" = min(data_table[,cellvar], na.rm=TRUE),
                          "Q1" = quantile(data_table[,cellvar], probs=0.25, na.rm=TRUE),
                          "Median" = median(data_table[,cellvar], na.rm = TRUE),
                          "Mean" = mean(data_table[,cellvar], na.rm=TRUE),
                          "Q3" = quantile(data_table[,cellvar], probs=0.75, na.rm=TRUE),
                          "Max" = max(data_table[,cellvar], na.rm=TRUE),
                          "SD" = sd(data_table[,cellvar], na.rm=TRUE)
                          ,"N Subs" = length(unique(data_table[,sub_id]))
                          ,"N Samples" = length(data_table[,sub_id])
                          )
        #clinvar <- input$picked_clinical
        #colorscheme <- input$summaryPlotColors
        
        
        #summary.stats = data_table %>%
        #    group_by(clinvar)
        
        #summary_table = ggsummarytable()
        #rint(colnames(data_table))
        #print(sub_id)
        return(temp)
    })
    
    output$choose_summary_merge = renderUI({
        
        summary_column_names = colnames(summary_data())
        
        selectInput("summary_merge", "Choose Summary Merge Variable",
                    choices = summary_column_names,
                    selected = summary_column_names[1])
        
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
                    selected = summary_marker_names[3])
        
    })
    
    output$choose_clinical = renderUI({
        
        summary_clinical_names = colnames(clinical_data())
        
        selectInput("picked_clinical", "Choose Clinical Variable to Plot",
                    choices = summary_clinical_names,
                    selected = summary_clinical_names[3])
        
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
    
    output$ripleysPlot = renderPlot({
        validate(need(input$ripleys_selection !="", "Please wait while calculations are running....."))
        
        if(is.null(spatial_data()) | is.null(clinical_data())){
            return()
        }
        
        progress = shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message="Estimating Confidence Interval", 
                     detail = "This will take some time...")
        
        progress$inc(1/5, message=paste("Assigning Clinical Data"))
        clinical_sample_data = clinical_data()
        
        progress$inc(1/5, message=paste("Selecting Sample Data"))
        sampleInfo = Filter(function(x) !any(is.na(x)),
                            clinical_sample_data[which(clinical_sample_data$image_tag ==
                                                           tail(strsplit(spatial_data()[1,1],
                                                                         "\\\\|[^[:print:]]")[[1]], n=1)),])
        progress$inc(1/5, message=paste("Removing Clinical Merge ID"))
        #sampleInfo = sampleInfo[,-which(names(sampleInfo) %in% input$clinical_merge)]
        
        progress$inc(1/5, message=paste("Running Ripley's Estimator"))
        
        colorscheme <- input$summaryPlotColors
        Ripley(spatial_data(), input$ripleys_selection, input$ripleysEstimator, sampleInfo, colorscheme)
        
        #progress$inc(1/5, message=paste("Finished Estimating"))
    })
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        print(colnames(df))
        return(df)
    })

})