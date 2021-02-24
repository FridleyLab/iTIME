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
    
    buttons = reactiveValues(data = NULL)
    observeEvent(input$exampleData, {
        buttons$data = 1
    })
    
    summary_data = reactive({
        if(is.null(buttons$data)){
            infile = input$summaryData
            if(is.null(infile)){
                return()
            }
            
            df = read.csv(infile$datapath, check.names = FALSE)
        } else {
            df = read.csv("./data/summary.csv", check.names = FALSE)
        }
        
        colnames(df) <- gsub("\\%", 'Percent', colnames(df))
        return(df)
        
    })
    
    clinical_data = reactive({
        if(is.null(buttons$data)){
            infile = input$clinicalData
            if(is.null(infile)){
                return()
            }
            df = read.csv(infile$datapath, check.names = FALSE)
        } else {
            df = read.csv("./data/clinical.csv", check.names = FALSE)
        }
        
        return(df)
    })
    
    spatial_data = reactive({
        if(is.null(buttons$data)){
            infile = input$spatialData
            if(is.null(infile)){
                return()
            }
            df = read.csv(infile$datapath)
        } else {
            df = read.csv("./data/Coghill_P2_Anal-Invasive-TMA1_[5,B].tif_74186_job45081.object_results copy.csv")
        }
        
        return(df)
    })
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        #print(colnames(df))
        return(df)
    })
    
    frequency_table = reactive({
        if(is.null(summary_data_merged())){
            return()
        }
        data_table = summary_data_merged()
        
        markers = colnames(data_table)[grepl(") Positive", colnames(data_table))]
        print(markers)
        
        df = freq_table(data_table, markers = markers, percent_threshold = input$choose_freq_thresh)
        
        return(df)
    })
    
    cont_table = reactive({
        if(is.null(summary_data_merged())){
            return()
        }
        
        data_table = summary_data_merged()
        markers = input$picked_cont_marker
        clinvar <- input$picked_clinical
        
        df = contingency_table(data_table, markers = markers, clin_vars = clinvar, percent_threshold = input$choose_cont_thresh)
        
        return(df)
    })
    
    output$summaryout = DT::renderDataTable({
        
        DT::datatable(summary_data(), options = list(scrollX = TRUE))
        
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
                 clin_vars = input$picked_clinical_factor,
                 colorscheme = input$summaryPlotColors)
    }, height = 400)
    
    output$choose_heatmap_marker = renderUI({
        heatmap_names = colnames(summary_data())
        
        heatmap_names2 = heatmap_names[grep("^(?=Percent.*)",
                              heatmap_names,perl=TRUE,ignore.case = TRUE)]
        
        awesomeCheckboxGroup("heatmap_selection",
                           "Choose Cell Marker for Heatmap",
                           choices = heatmap_names2,
                           selected = heatmap_names2,
                           status = "info"
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
        colorscheme = input$summaryPlotColors
        colorscheme = viridis::viridis_pal(option = colorscheme)(length(markers))
        
        scatter_plotly_old(data = spatial_data(), markers = markers, 
                           new_names = new_names, colorscheme = colorscheme)
    })
    
    output$choosePlotlyMarkers = renderUI({
        ripleys_spatial_names = colnames(Filter(is.numeric, spatial_data()))
        
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         ripleys_spatial_names,perl=TRUE,ignore.case = TRUE)
        tmp = ripleys_spatial_names[whichcols]
        acceptable_ripleys_names =  tmp[sapply(spatial_data()[,tmp],sum)>0]
        print(acceptable_ripleys_names)
        
        awesomeCheckboxGroup("plotly_selection", "Choose Markers for Spatial Plot",
                    choices = rev(acceptable_ripleys_names),
                    selected = acceptable_ripleys_names[grep("^(?=.*Opal)",
                                                             acceptable_ripleys_names, 
                                                             perl=TRUE)],
                    status = "info"
                    )
    })
    
    output$summaryTable = renderTable({

        data_table = summary_data_merged()
        cellvar <-  input$picked_marker
        sub_id = input$clinical_merge
        
        temp = data.frame("Min" = min(data_table[,cellvar], na.rm=TRUE),
                          "Q1" = quantile(data_table[,cellvar], probs=0.25, na.rm=TRUE),
                          "Median" = median(data_table[,cellvar], na.rm = TRUE),
                          "Mean" = mean(data_table[,cellvar], na.rm=TRUE),
                          "Q3" = quantile(data_table[,cellvar], probs=0.75, na.rm=TRUE),
                          "Max" = max(data_table[,cellvar], na.rm=TRUE),
                          "SD" = sd(data_table[,cellvar], na.rm=TRUE),
                          "N Subs" = length(unique(data_table[,sub_id])),
                          "N Samples" = length(data_table[,sub_id])
                          )
        #temp = cbind(temp, freq_table(df, markers = markers, percent_threshold = input$choose_freq_thresh))
        return(temp)
    })
    
    output$freqTable = renderTable({
        return(frequency_table())
    })
    
    output$contTable = renderTable({
        return(cont_table())
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
    
    output$choose_cont_marker = renderUI({
        
        summary_marker_names = colnames(summary_data_merged())[grepl(") Positive", colnames(summary_data_merged()))]
        
        selectInput("picked_cont_marker", "Choose Cell Marker for Contingency Table",
                    choices = summary_marker_names,
                    selected = summary_marker_names[3])
        
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
    
    # summary_data_merged = reactive({
    #     if(is.null(clinical_data()) | is.null(summary_data())){
    #         return()
    #     }
    #     
    #     df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
    #     print(colnames(df))
    #     return(df)
    # })

})