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
            
            df = fread(infile$datapath, check.names = FALSE, data.table = FALSE)
        } else {
            df = fread("example_data/deidentified_summary.csv", check.names = FALSE, data.table = FALSE)
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
            df = fread(infile$datapath, check.names = FALSE, data.table = FALSE)
        } else {
            df = fread("example_data/deidentified_clinical.csv", check.names = FALSE, data.table = FALSE)
        }
        
        return(df)
    })
    
    spatial_data = reactive({
        if(is.null(buttons$data)){
            infile = input$spatialData
            if(is.null(infile)){
                return()
            }
            df = fread(infile$datapath, check.names = FALSE, data.table = FALSE)
        } else {
            df = fread("example_data/deidentified_spatial.csv", check.names = FALSE, data.table = FALSE)
        }
        
        assign("spatial", df, envir = globalenv())
        
        return(df)
    })
    
    output$summary_preview = renderTable({
        head(summary_data(), n = 15L)
    })
    
    output$clinical_preview = renderTable({
        head(clinical_data(), n = 15L)
    })
    
    output$spatial_preview = renderTable({
        head(spatial_data()[,-3], n = 15L)
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
        t = sapply(clinical_data(), function(x){return(length(unique(x)))})
        good = t[t > 1 & t < 10]
        print(good)
        selectInput("picked_clinical", "Choose Clinical Variable to Plot and Test",
                    choices = summary_clinical_names,
                    selected = names(good)[1]) #select a variable that has a decent amount of levels in order to perform the models
        
    })
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        return(df)
    })
    
#univariate
    
    output$choose_total_cells = renderUI({
        summary_clinical_names = colnames(summary_data_merged())
        
        selectInput("picked_total_cells", "Choose Column Name for Total Number of Cells",
                    choices = summary_clinical_names,
                    selected = summary_clinical_names[grep("Total", summary_clinical_names)])
    })
    output$modeling_reference = renderUI({
        model_references = unique(summary_data_merged()[input$picked_clinical])
        selectInput("picked_modeling_reference", "Choose Clinical Reference",
                    choices = model_references,
                    selected = model_references[1])
    })
    
    model_list = reactive({
        validate(need(input$picked_clinical !="", "Please select a clinical variable....."),
                 need(summary_data_merged() !="", "Please upload clinical and summary data....."),
                 need(input$picked_marker !="", "Please pick a marker....."),
                 need(input$picked_total_cells !="", "Please select column with total cell count....."),
                 need(input$picked_modeling_reference !="", "Select level for reference....."))
        suppressWarnings({
            df = model_checked_repeated(summary_data_merged = summary_data_merged(), markers = input$picked_marker,
                        Total = input$picked_total_cells, clin_vars = input$picked_clinical, reference = input$picked_modeling_reference,
                        choose_clinical_merge = input$clinical_merge) #assuming IDs are merging variable (patientID, subjectID, etc)
        })
        return(df)
    })
    
    output$aic_table = renderTable({
        models1 = model_list()
        return(data.frame(models1$aic))
    }, digits = 4)
    
    output$model_stats = renderTable({
        validate(need(model_list(), "Please wait while things finish loading....."))
        models1 = model_list()
        df = models1$models[[input$selectedModel]] %>% summary() %>% coefficients()
        df1 = data.frame(Terms = gsub("tmp\\$clin_vars", "", row.names(df)),
                         df, check.names = F)
        return(df1)
    }, digits = 4)
    
    cont_table = reactive({
        validate(need(input$picked_clinical !="", "Please wait while things finish loading....."))
        
        df = contingency_table(summary_data_merged(), markers = input$picked_marker, clin_vars = input$picked_clinical, percent_threshold = input$choose_cont_thresh)
        
        return(df)
    })
    
    output$selectedModelName = renderText({
        paste("Statistical Modeling of the", input$picked_marker)
    })
    
    output$contTable = renderTable({
        return(cont_table())
    })
    
    frequency_table = reactive({
        validate(need(input$picked_marker !="", "Please wait while things finish loading....."))
        if(is.null(summary_data_merged())){
            return()
        }
        
        df = freq_table_by_marker(summary_data_merged(), markers = input$picked_marker)
        
        return(df)
    })
    
    output$freqTable = renderTable({
        return(frequency_table())
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
                          "Median" = median(data_table[,cellvar], na.rm = TRUE),
                          "Mean" = mean(data_table[,cellvar], na.rm=TRUE),
                          "Max" = max(data_table[,cellvar], na.rm=TRUE),
                          "SD" = sd(data_table[,cellvar], na.rm=TRUE),
                          "N Subs" = length(unique(data_table[,sub_id])),
                          "N Samples" = length(data_table[,sub_id])
                          )
        return(temp)
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
        data_table = summary_data_merged()
        
        if(input$uni_transformation == "none"){
            thres = input$choose_cont_thresh
        } else if(input$uni_transformation == "sqrt_transform"){
            data_table[,cellvar] = sqrt(data_table[,cellvar])
            thres = sqrt(as.numeric(input$choose_cont_thresh))
        } else if(input$uni_transformation == "log2_transform"){
            data_table[,cellvar] = log2(data_table[,cellvar]+0.0001)
            thres = log2(as.numeric(input$choose_cont_thresh)+0.0001)
        } else if(input$uni_transformation == "logit_transform"){
            p = (data_table[,cellvar]/100)+0.0001
            data_table[,cellvar] = log10(p/(1-p))
            tmp = (as.numeric(input$choose_cont_thresh)/100) + 0.0001
            thres = log10(tmp/(1-tmp))
        }
        plots = summary_plots_fn(data_table, clinvar, cellvar, colorscheme, thres)
        
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
    
    cdf_plot_react = reactive({
        validate(need(summary_data_merged() !="", "Please upload Summary and Clinical files....."),
                 need(input$picked_marker !="", "Please select a marker above....."))
        if(is.null(summary_data_merged())){
            return(NULL)
        }
        
        marker = input$picked_marker
        data_table = summary_data_merged()
        CDF_plots(summary_data_merge = data_table, markers = substr(marker, 9, nchar(marker)))
    })
    
    output$cdfplot = renderPlot({
        cdf_plot_react()
    })
    
    output$univariate_report <- downloadHandler(
        filename <-  "univariate_report.pdf",
        content = function(file) {
            tempReport <- file.path(tempdir(), "volanoes_report.Rmd")
            file.copy("../report_templates/univariate_report.Rmd", tempReport, overwrite = TRUE)
            params <- list(selected_marker = input$picked_marker,
                           contingency_threshold = input$choose_cont_thresh,
                           picked_clinical = input$picked_clinical,
                           boxplot = boxplot(),
                           contingency_Table = contTable(),
                           frequency_table = freqTable(),
                           summary_table = summaryTable(),
                           total_cell_column = input$picked_total_cells,
                           modeling_reference = input$picked_modeling_reference,
                           selected_univariate_model = input$selectedModel,
                           chosen_model_stats = model_stats(),
                           cdf_plot = cdfplot(),
                           model_aic_table = aic_table()
                           )
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )

#multivariate
    
    output$choose_heatmap_marker = renderUI({
        heatmap_names = colnames(summary_data())
        
        heatmap_names2 = heatmap_names[grep("^(?=Percent.*)",
                              heatmap_names,perl=TRUE,ignore.case = TRUE)]
        
        awesomeCheckboxGroup("heatmap_selection", "Choose Cell Marker for Heatmap",
                           choices = heatmap_names2, selected = heatmap_names2,
                           status = "primary"
        )
    })
    
    output$choose_heatmap_clinical = renderUI({
        
        clinical_heatmap_names = colnames(clinical_data())
        
        selectInput("picked_clinical_factor", "Choose Annotation for Heatmap",
                    choices = clinical_heatmap_names, selected = clinical_heatmap_names[3])
        
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
    })
    
    output$heatmap = renderPlot({
       heatmap_plot()
    })
    
    output$download_heatmap = downloadHandler(
        filename = function() { paste(Sys.Date(), '-heatmap.png', sep='') },
        
        content = function(file) {
            ggsave(file, plot = heatmap_plot(), device = "png",
                   width = 10, height = 7, units = 'in')
        }
    )
    
    pca_plot = reactive({
        validate(need(summary_data_merged() !="", "Please upload Summary and Clinical files....."),
                 need(input$heatmap_selection !="", "Please select a markers to use....."),
                 need(input$picked_clinical_factor !="", "Please select a clinical variable....."))
        
        if(is.null(summary_data_merged())){
            return()
        }
        
        return(pca_plot_function(summary_clinical_merged = summary_data_merged(), markers = input$heatmap_selection, clin_vars = input$picked_clinical_factor))
        
        
    })
    
    output$pca = renderPlot({
        pca_plot()
    })
    
    output$download_pca = downloadHandler(
        filename = function () {paste(Sys.Date(), '-pca.png', sep='')},
        
        content = function(file){
            ggsave(file, plot = pca_plot(), device = "png",
                   width = 7, height = 7, units = "in")
        }
    )
    
#spatial
    output$choosePlotlyMarkers = renderUI({
        validate(need(spatial_data() !="", "Please wait while spatial data is loaded....."))
        if(is.null(spatial_data())){
            return()
        }
        
        ripleys_spatial_names = colnames(Filter(is.numeric, spatial_data()))
        
        whichcols = grep("^(?!.*(nucle|max|min|cytoplasm|area|path|image|Analysis|Object))",
                         ripleys_spatial_names,perl=TRUE,ignore.case = TRUE)
        tmp = ripleys_spatial_names[whichcols]
        print(class(spatial_data()))
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
        #colorscheme = input$summaryPlotColors
        colorscheme = viridis::viridis_pal(option = "D")(length(markers))
        
        scatter_plotly_old(data = spatial_data(), markers = markers, 
                           new_names = new_names, colorscheme = colorscheme) #
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
    
    ripley_data = reactive({
        validate(need(input$ripleys_selection !="", "Please wait while calculations are running....."))
        
        Ripley(spatial_data(), input$ripleys_selection)
    })
    
    output$ripleysPlot = renderPlot({
        
        Ripley_plot(ripley_data = ripley_data(), estimator = input$ripleysEstimator)
        
    })
    
#Getting started RMD rendering
    
    output$gettingstarted <- renderUI({
        withMathJax({
            k = knitr::knit(input = "GettingStarted.Rmd", quiet = T)
            HTML(markdown::markdownToHTML(k, fragment.only = T))
        })
    })

})