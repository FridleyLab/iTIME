# iteractive Tumor Immune MicroEnvironment
# 
# iTIME Shiny Application is a tool to visualize spatial IF data that is output from HALO. 
# Along with clinical data, brief summary statistics are presented.
#
# Dev team in ui.R

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
            df = read.csv("data/summary.csv", check.names = FALSE)
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
            df = read.csv("data/clinical.csv", check.names = FALSE)
        }
        
        return(df)
    })
    
    spatial_data = reactive({
        if(is.null(buttons$data)){
            infile = input$spatialData
            if(is.null(infile)){
                return()
            }
            df = read.csv(infile$datapath, check.names = FALSE)
        } else {
            df = read.csv("data/Coghill_P2_Anal-Invasive-TMA1_[5,B].tif_74186_job45081.object_results copy.csv",
                          check.names = FALSE)
        }
        
        return(df)
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
                    selected = summary_marker_names[1])
        
    })
    
    output$choose_marker = renderUI({
        
        summary_marker_names = colnames(summary_data_merged())[grepl("^Percent", colnames(summary_data_merged()))]
        
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
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        return(df)
    })
    
    cont_table = reactive({
        validate(need(input$picked_clinical !="", "Please wait while things finish loading....."))
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        data_table = summary_data_merged()
        
        markers = input$picked_marker
        clinvar <- input$picked_clinical
        
        df = contingency_table(data_table, markers = markers, clin_vars = clinvar, percent_threshold = input$choose_cont_thresh)
        
        
        return(df)
    })
    
    frequency_table = reactive({
        validate(need(input$picked_marker !="", "Please wait while things finish loading....."))
        if(is.null(summary_data_merged())){
            return()
        }
        data_table = summary_data_merged()
        
        markers = input$picked_marker
        
        df = freq_table_by_marker(data_table, markers = markers)
        
        return(df)
    })
    
    sumTable = reactive({
        validate(need(summary_data() !="", "Please wait while things finish loading....."))
        if(is.null(summary_data())){
            return()
        }
        
        return(summary_data())
        
    })
    
    output$summaryTable = renderTable({
        validate(need(summary_data_merged() !="", "Please wait while things finish loading....."),
                 need(input$picked_marker !="", "Please wait while things finish loading....."),
                 need(input$clinical_merge !="", "Please wait while things finish loading....."))

        data_table = summary_data_merged()
        cellvar <-  input$picked_marker
        sub_id = input$clinical_merge
        
        temp = data.frame("Min" = min(data_table[,cellvar], na.rm=TRUE),
                          #"Q1" = quantile(data_table[,cellvar], probs=0.25, na.rm=TRUE),
                          "Median" = median(data_table[,cellvar], na.rm = TRUE),
                          "Mean" = mean(data_table[,cellvar], na.rm=TRUE),
                          #"Q3" = quantile(data_table[,cellvar], probs=0.75, na.rm=TRUE),
                          "Max" = max(data_table[,cellvar], na.rm=TRUE),
                          "SD" = sd(data_table[,cellvar], na.rm=TRUE),
                          "N Subs" = length(unique(data_table[,sub_id])),
                          "N Samples" = length(data_table[,sub_id])
                          )
        return(cbind(frequency_table(), temp))
    })
    
    output$freqTable = renderTable({
        return(frequency_table())
    })
    
    output$contTable = renderTable({
        return(cont_table())
    })
    
    univar_plots = reactive({
         validate(need(input$picked_marker !="", "Please wait while things finish loading....."),
                 need(input$picked_clinical !="", ""),
                 need(input$summaryPlotColors !="", ""),
                 need(summary_data_merged() !="", ""))
        # generate bins based on input$bins from ui.R
        cellvar <-  input$picked_marker
        clinvar <- input$picked_clinical
        colorscheme <- input$summaryPlotColors
        
        if(input$sqrt_transform == FALSE){
            data_table = summary_data_merged()
        }else{
            data_table = summary_data_merged()
            data_table[,cellvar] = sqrt(data_table[,cellvar])
        }
        
        #assign("summary_table", data_table, envir = .GlobalEnv)
        #assign("cell_var", cellvar, envir=.GlobalEnv)
        
        plots = summary_plots_fn(data_table, clinvar, cellvar, colorscheme, input$choose_cont_thresh)
        
        plots[[as.integer(input$summaryPlotType)]]
    })

    output$boxplot <- renderPlot({
        univar_plots()
    })
    
    output$download_boxplot = downloadHandler(
        filename = function() { paste(Sys.Date(), '-summary_plot.png', sep='') },
        
        content = function(file) {
            ggsave(file, plot = univar_plots(), device = "png",width = 12, height = 10, units = "in")
        }
    )
    
    output$choose_heatmap_marker = renderUI({
        heatmap_names = colnames(summary_data())
        
        heatmap_names2 = heatmap_names[grep("^(?=Percent.*)",
                              heatmap_names,perl=TRUE,ignore.case = TRUE)]
        
        awesomeCheckboxGroup("heatmap_selection",
                           "Choose Cell Marker for Heatmap",
                           choices = heatmap_names2,
                           selected = heatmap_names2,
                           status = "primary"
        )
    })
    
    output$choose_heatmap_clinical = renderUI({
        
        clinical_heatmap_names = colnames(clinical_data())
        
        selectInput("picked_clinical_factor", "Choose Annotation for Heatmap",
                    choices = clinical_heatmap_names,
                    selected = clinical_heatmap_names[3])
        
    })
    
    heatmap_plot = reactive({
         validate(need(input$heatmap_selection !="", "Please wait while things finish loading....."))
        if(is.null(summary_data_merged())){
            return()
        }
        
        if(input$heatmap_transform == "none"){
            heatmap_data = summary_data_merged()
        }else if(input$heatmap_transform == "square_root"){
            heatmap_data = summary_data_merged()
            heatmap_data[,input$heatmap_selection] = sqrt(heatmap_data[,input$heatmap_selection])
        }
        
        
        
        pheat_map(summary_clinical_merge = heatmap_data,
                 markers = input$heatmap_selection,
                 clin_vars = input$picked_clinical_factor,
                 colorscheme = input$summaryPlotColors,
                 anno_clust = input$cluster_heatmap_annotation,
                 mark_clust = input$cluster_heatmap_Marker)
        # heat_map(summary_clinical_merge = heatmap_data,
        #          markers = input$heatmap_selection,
        #          clin_vars = input$picked_clinical_factor,
        #          colorscheme = input$summaryPlotColors)
    })
    
    output$heatmap = renderPlot({
       heatmap_plot()
    }, height = 500)
    
    output$download_heatmap = downloadHandler(
        filename = function() { paste(Sys.Date(), '-heatmap.png', sep='') },
        
        content = function(file) {
            ggsave(file, plot = heatmap_plot(), device = "png",
                   width = 10, height = 7, units = 'in')
        }
    )
    
    output$choosePlotlyMarkers = renderUI({
        ripleys_spatial_names = colnames(Filter(is.numeric, spatial_data()))
        
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         ripleys_spatial_names,perl=TRUE,ignore.case = TRUE)
        tmp = ripleys_spatial_names[whichcols]
        acceptable_ripleys_names =  tmp[sapply(spatial_data()[,tmp],sum)>0]
        
        awesomeCheckboxGroup("plotly_selection", "Choose Markers for Spatial Plot",
                    choices = rev(acceptable_ripleys_names),
                    selected = acceptable_ripleys_names[grep("^(?=.*Opal)",
                                                             acceptable_ripleys_names, 
                                                             perl=TRUE)],
                    status = "info"
                    )
    })
    
    spatial_plot = reactive({
        validate(need(input$plotly_selection !="", "Please wait while things finish loading....."))
        
        markers = input$plotly_selection
        new_names = markers
        colorscheme = input$summaryPlotColors
        colorscheme = viridis::viridis_pal(option = colorscheme)(length(markers))
        
        scatter_plotly_old(data = spatial_data(), markers = markers, 
                           new_names = new_names, colorscheme = colorscheme)
    })
    
    output$spatial_plotly = renderPlotly({
        spatial_plot()
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
        #sampleInfo = Filter(function(x) !any(is.na(x)),
        #                    clinical_sample_data[which(clinical_sample_data$image_tag ==
        #                                                   tail(strsplit(spatial_data()[1,1],
        #                                                                 "\\\\|[^[:print:]]")[[1]], n=1)),])
        progress$inc(1/5, message=paste("Removing Clinical Merge ID"))
        #sampleInfo = sampleInfo[,-which(names(sampleInfo) %in% input$clinical_merge)]
        
        progress$inc(1/5, message=paste("Running Ripley's Estimator"))
        
        colorscheme <- input$summaryPlotColors
        Ripley(spatial_data(), input$ripleys_selection, input$ripleysEstimator)
        
        #progress$inc(1/5, message=paste("Finished Estimating"))
    })
    
    output$gettingstarted <- renderUI({
        withMathJax({
            k = knitr::knit(input = "GettingStarted.Rmd", quiet = T)
            HTML(markdown::markdownToHTML(k, fragment.only = T))
        })
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