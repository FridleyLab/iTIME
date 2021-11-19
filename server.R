# iteractive Tumor Immune MicroEnvironment
# 
# iTIME Shiny Application is a tool to visualize spatial IF data that is output from HALO. 
# Along with clinical data, brief summary statistics are presented.
#
# Dev team in ui.R
#clinical_data = fread("example_data/deidentified_clinical.csv", check.names = FALSE, data.table = FALSE)
#summary_data = fread("example_data/deidentified_summary.csv", check.names = FALSE, data.table = FALSE)
#summary_data_merged = merge(clinical_data, summary_data, by = "deidentified_id")

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
        df[is.na(df)] = "Missing"
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
        
        df[is.na(df)] = "Missing"
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
        
        df[is.na(df)] = "NA"
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
    
    output$merged_preview = renderTable({
        head(summary_data_merged(), n=15L)
    })
    
    output$choose_summary_merge = renderUI({
        
        summary_column_names = colnames(summary_data())
        
        selectInput("summary_merge", "Choose Summary Merge Variable",
                    choices = summary_column_names,
                    selected = summary_column_names[1])
        
        #print("summary merge variable selected")
    })
    
    output$choose_clinical_merge = renderUI({
        
        clinical_column_names = colnames(clinical_data())
        
        selectInput("clinical_merge", "Choose Clinical Merge Variable",
                    choices = clinical_column_names,
                    selected = clinical_column_names[1])
        
        #print('clinical merge variable selected')
        
    })
    
    summary_data_merged = reactive({
        if(is.null(clinical_data()) | is.null(summary_data())){
            return()
        }
        
        df = merge(clinical_data(), summary_data(), by.x = input$clinical_merge, by.y = input$summary_merge)
        return(df)
    })
    
#univariate
    
    output$choose_marker = renderUI({
        
        summary_marker_names = colnames(summary_data_merged())[grepl("^Percent", colnames(summary_data_merged()))]
        
        selectInput("picked_marker", "Choose Cell Marker to Plot",
                    choices = summary_marker_names,
                    selected = summary_marker_names[3])
        
    })
    
    output$choose_clinical = renderUI({
        validate(need(ncol(clinical_data()) > 0, "Loading Clinical Data....."),
                 need(ncol(summary_data_merged()) > 0, "Waiting on merging clinical and summary data....."))
        summary_clinical_names = colnames(summary_data_merged())[(colnames(summary_data_merged()) %in% colnames(clinical_data()))]
        t = sapply(summary_data_merged() %>% select(all_of(summary_clinical_names)), function(x){return(length(unique(x)))})
        good = t[t > 1 & t < 10]
        
        selectInput("picked_clinical", "Choose Clinical Variable to Plot and Test",
                    choices = summary_clinical_names,
                    selected = names(good)[1]) #select a variable that has a decent amount of levels in order to perform the models
        
    })
    
    output$choose_uni_covariates = renderUI({
      validate(need(ncol(clinical_data()) > 0, "Waiting on Clinical data....."))
      summary_clinical_names = colnames(summary_data_merged())[(colnames(summary_data_merged()) %in% colnames(clinical_data()))]
      t = sapply(summary_data_merged() %>% select(all_of(summary_clinical_names)), function(x){return(length(unique(x)))})
      
      good = summary_clinical_names[t>1]
      good = good[!good %in% input$picked_clinical]
      
      pickerInput(
        inputId = "uni_covariates_selected", 
        label = "Choose Model Covariates", 
        choices = good, 
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      )
    })
    
    univar_plots = reactive({
         validate(need(input$picked_marker !="", "Please wait while things finish loading....."),
                 need(input$picked_clinical !="", "Waiting to pick a clinical variable"),
                 need(input$summaryPlotColors !="", "waiting on plot colors"),
                 need(ncol(summary_data_merged()) > 0, "waiting on merging data"),
                 need(input$uni_transformation != "", "have to wait for tranformation options to load"),
                 need(input$summaryPlotType != "", "have to wait for plot type options to load"))
        
        data_table = summary_data_merged()
        cellvar = input$picked_marker
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
        
        
        plots = summary_plots_fn(data_table, clinvar = input$picked_clinical,
                                 cellvar = cellvar, colorscheme <- input$summaryPlotColors, thres)
        
        plots[[as.integer(input$summaryPlotType)]]
    })

    output$boxplot <- renderPlot({
        univar_plots()
    })
    
    cont_table = reactive({
        validate(need(input$picked_clinical !="", "Please wait while things finish loading....."),
                 need(ncol(summary_data_merged()) > 0, ""),
                 need(input$picked_marker != "", ""),
                 need(input$choose_cont_thresh != "", ""))
        
        df = contingency_table(summary_data_merged(), markers = input$picked_marker, clin_vars = input$picked_clinical, percent_threshold = input$choose_cont_thresh)
        
        return(df)
    })
    
    output$contTable = renderTable({
        return(cont_table())
    })
    
    frequency_table = reactive({
        validate(need(input$picked_marker !="", "Please wait while things finish loading....."))
        
        df = freq_table_by_marker(summary_data_merged(), markers = input$picked_marker, clinical = input$picked_clinical)
        return(df)
    })
    
    output$freqTable = renderTable({
        return(frequency_table())
    })
    
    output$selectedModelName = renderText({
        marker = substr(input$picked_marker, 9, nchar(input$picked_marker)-1)
        paste("Statistical Modeling of the", marker, "Counts")
    })
    
    sum_table = reactive({
        validate(need(ncol(summary_data_merged()) > 0, "Please wait while things finish loading....."),
                 need(input$picked_marker !="", "Please wait while things finish loading....."),
                 need(input$clinical_merge !="", "Please wait while things finish loading....."))
        
        return(summary_table(summary_data_merged(), marker = input$picked_marker, clinical = input$picked_clinical, merged = input$clinical_merge))
    })
    
    output$summaryTable = renderTable({
        sum_table()
    })
    
    output$download_boxplot = downloadHandler(
        filename = function() { paste(Sys.Date(), '-summary_plot.pdf', sep='') },
        
        content = function(file) {
            ggsave(file, plot = univar_plots(), device = "pdf",width = 12, height = 10, units = "in")
        }
    )
    
    output$choose_total_cells = renderUI({
        summary_clinical_names = colnames(summary_data_merged())
        
        selectInput("picked_total_cells", "Choose Column Name for Total Number of Cells",
                    choices = summary_clinical_names,
                    selected = summary_clinical_names[grep("Total", summary_clinical_names)])
    })
    output$modeling_reference = renderUI({
        validate(need(ncol(summary_data_merged()) > 0, "Please wait while Summary and Clinical Data are merged....."),
                 need(input$picked_clinical !="", "Please select a clinical variable for comparison....."))
        model_references = unique(summary_data_merged()[input$picked_clinical])
        selectInput("picked_modeling_reference", "Choose Clinical Reference",
                    choices = model_references,
                    selected = model_references[1])
    })
    
    model_list = reactive({
        validate(need(input$picked_clinical !="", "Please select a clinical variable....."),
                 need(ncol(summary_data_merged()) > 0, "Please upload clinical and summary data....."),
                 need(input$picked_marker !="", "Please pick a marker....."),
                 need(input$picked_total_cells !="", "Please select column with total cell count....."),
                 need(input$picked_modeling_reference !="", ""))
        marker = input$picked_marker
        marker = substr(marker, 9, nchar(marker))
        marker = c(marker, gsub("\\ Positive\\ ", "\\ ", marker))
        covars = input$uni_covariates_selected
        suppressWarnings({
        df = model_checked_repeated(summary_data_merged = summary_data_merged(), markers = marker,
                                    Total = input$picked_total_cells, clin_vars = input$picked_clinical, reference = input$picked_modeling_reference,
                                    choose_clinical_merge = input$clinical_merge, covars = covars) #assuming IDs are merging variable (patientID, subjectID, etc)
        })
        
    return(df)
    })
    
    # output$aic_table = renderTable({
    #     aic_table_react()
    # }, digits = 4)
    
    aic_table_react = reactive({
        models1 = model_list()
        return(data.frame(models$aic))
    })
    
    chosen_model_stats = reactive({
        #validate(need(model_list(), "Please wait while things finish loading....."))
        withProgress(message = "Modeling", value = 0,{
            incProgress(0.33, detail = "Fitting Beta-Binomial")
            models1 = model_list()
            incProgress(0.33, detail = "Extracting Statistics")
            df = models1$models[["Beta Binomial"]]
            if(class(df)=="character"){
                df1 = data.frame(df)
            } else if(class(df)=="MixMod"){
                df1 = summary(df)$coef_table
                df1 = data.frame(Terms = gsub("tmp\\$clin_vars", "", row.names(df1)),
                                 df1, check.names = F)
            }else{
                df = df %>% summary() %>% coefficients()#input$selectedModel
                df1 = data.frame(Terms = gsub("tmp\\$clin_vars", "", row.names(df)),
                                 df, check.names = F)
                df1 = df1[-2,]
            }
            levs = summary_data_merged()[[input$picked_clinical]] %>% unique() %>% length()-1
            incProgress(0.33, detail = "Completed")
            df = df1[c(1,(nrow(df1)-levs+1):nrow(df1)),]
            #assign("df", df, envir = .GlobalEnv)
            return(df)
        })
    })
    
    output$model_stats = renderTable({
        chosen_model_stats()
    }, digits = 4)
    
    cdf_plot_react = reactive({
        validate(need(ncol(summary_data_merged()) > 0, "Please upload Summary and Clinical files....."),
                 need(input$picked_marker !="", "Please select a marker above....."))
        
        marker = input$picked_marker
        marker = substr(marker, 9, nchar(marker))
        marker = c(marker, gsub("\\ Positive\\ ", "\\ ", marker))
        data_table = summary_data_merged()
        
        CDF_plots(summary_data_merge = data_table, markers = marker)
    })
    
    output$cdfplot = renderPlot({
        cdf_plot_react()
    })
    
    model_description = reactive({
        validate(need(ncol(chosen_model_stats()) > 0, "Please wait while the model is fit....."))
        model_statistics = chosen_model_stats()
        coefficient_of_interest = model_statistics[2,]
        marker = substr(input$picked_marker, 9, nchar(input$picked_marker)-15)
        
        if(any(table(summary_data_merged()[[input$clinical_merge]])>1)){
            repeated_measure = paste("Merge variable <b>",input$clinical_merge,"</b> has repeated measures.<br>", sep="")
        }else{
            repeated_measure = paste("Merge variable <b>",input$clinical_merge,"</b> does not have repeated measures.<br>", sep="")
        }
        
        paste(repeated_measure,
              "The predictor of interest, <b>",
              as.character(input$picked_clinical),
              "</b>, odds ratio on abundance of the immune marker of interest, <b>", marker, "</b> positive cell counts, is <b>",
              round(exp(as.numeric(coefficient_of_interest$Estimate)), digits = 4), "</b> [exp(<b>", paste(coefficient_of_interest$Terms)," Estimate</b>)],
              meaning that for a cell from <b>",
              coefficient_of_interest$Terms, "</b> is <b>", round(exp(as.numeric(coefficient_of_interest$Estimate)), digits = 4), "x</b> as likely to be <b>", 
              marker, "</b> positive than a cell from <b>", input$picked_modeling_reference,
              "</b>. The p-value for the effect of the predictor of interest <b>", as.character(input$picked_clinical), "</b> on the abundance of <b>", 
              marker, "</b> positive cells is <b>", round(as.numeric(coefficient_of_interest[,ncol(coefficient_of_interest)]), digits = 4),
              "</b>. A small p-value (less than 0.05, for example) indicates the association is unlikely to occur by chance and indicates 
              a significant association of the predictor <b>", as.character(input$picked_clinical) ,"</b> on immune abundance for <b>",
              marker, "</b>.",
              sep="")
    })
    
    output$modelingDescription <- renderText({
        model_description()
    })
    
    output$univariate_report <- downloadHandler(
        filename <-  "univariate_report.pdf",
        content = function(file) {
            tempReport <- file.path(tempdir(), "volanoes_report.Rmd")
            file.copy("report_templates/univariate_report.Rmd", tempReport, overwrite = TRUE)
            params <- list(include_functions = input$printFunctions,
                           selected_marker = input$picked_marker,
                           contingency_threshold = input$choose_cont_thresh,
                           picked_clinical = input$picked_clinical,
                           boxplots = univar_plots(),
                           contingency_table = cont_table(),
                           frequency_table = frequency_table(),
                           summary_table = sum_table(),
                           cdf_plot = cdf_plot_react(),
                           total_cell_column = input$picked_total_cells,
                           modeling_reference = input$picked_modeling_reference,
                           chosen_model_stats = chosen_model_stats(),
                           modelDescription = model_description()
                           #selected_univariate_model = input$selectedModel,
                           #model_aic_table = aic_table_react(),
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
                           choices = heatmap_names2, selected = heatmap_names2[grepl("Opal", heatmap_names2)],
                           status = "primary"
        )
    })
    
    output$choose_heatmap_clinical = renderUI({
        validate(need(ncol(clinical_data()) > 0, "Loading Clinical Data....."),
                 need(ncol(summary_data_merged()) > 0, "Waiting on merging clinical and summary data....."))
        
        clinical_heatmap_names = colnames(summary_data_merged())[(colnames(summary_data_merged()) %in% colnames(clinical_data()))]
        t = sapply(summary_data_merged() %>% select(all_of(clinical_heatmap_names)), function(x){return(length(unique(x)))})
        good = t[t > 1 & t < 10]
        
        selectInput("picked_clinical_factor", "Choose Annotation for Heatmap",
                    choices = clinical_heatmap_names, 
                    selected = names(good)[1])
        
    })
    
    heatmap_plot = reactive({
         validate(need(length(input$heatmap_selection) > 1, "Please select 2 or more markers....."),
                  need(ncol(summary_data_merged()) > 1, "wait for magic"))
        
        if(input$heatmap_transform == "none"){
            heatmap_data = summary_data_merged()
        }else if(input$heatmap_transform == "square_root"){
            heatmap_data = summary_data_merged()
            heatmap_data[,input$heatmap_selection] = sqrt(heatmap_data[,input$heatmap_selection])
        }
        
        
        
        pheat_map(summary_clinical_merge = heatmap_data,
                 markers = input$heatmap_selection,
                 clin_vars = input$picked_clinical_factor,
                 anno_clust = input$cluster_heatmap_annotation,
                 mark_clust = input$cluster_heatmap_Marker)
    })
    
    output$heatmap = renderPlot({
       heatmap_plot()
    })
    
    output$download_heatmap = downloadHandler(
        filename = function() { paste(Sys.Date(), '-heatmap.pdf', sep='') },
        
        content = function(file) {
            ggsave(file, plot = heatmap_plot(), device = "pdf",
                   width = 10, height = 7, units = 'in')
        }
    )
    
    pca_plot = reactive({
        validate(need(ncol(summary_data_merged()) > 0, "Please upload Summary and Clinical files....."),
                 need(length(input$heatmap_selection) > 1, "Please select 2 or more markers....."),
                 need(input$picked_clinical_factor !="", "Please select a clinical variable....."))
        
        if(is.null(summary_data_merged())){
            return()
        }
        
        if(input$heatmap_transform == "none"){
            pca_data = summary_data_merged()
        }else if(input$heatmap_transform == "square_root"){
            pca_data = summary_data_merged()
            pca_data[,input$heatmap_selection] = sqrt(pca_data[,input$heatmap_selection])
        }
        
        return(pca_plot_function(summary_clinical_merged = pca_data, markers = input$heatmap_selection, clin_vars = input$picked_clinical_factor))
        
        
    })
    
    output$pca = renderPlot({
        pca_plot()
    })
    
    output$download_pca = downloadHandler(
        filename = function () {paste(Sys.Date(), '-pca.pdf', sep='')},
        
        content = function(file){
            ggsave(file, plot = pca_plot(), device = "pdf",
                   width = 7, height = 7, units = "in")
        }
    )
    
#spatial
    output$choosePlotlyMarkers = renderUI({
        validate(need(ncol(spatial_data()) > 0, "Please wait while spatial data is loaded....."))
        if(is.null(spatial_data())){
            return()
        }
        
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
        validate(need(nrow(spatial_data()) > 0, "Please wait while things finish loading....."))
        
        spatial_plotly(data = spatial_data(), markers = input$plotly_selection) #
    })
    
    output$spatial_plotly = renderPlotly({
        spatial_plot()
    })
    
    # output$download_spatialPlotly = downloadHandler(
    #     filename = function() { paste(Sys.Date(), '-spatial_plot.pdf', sep='') },
    #     #https://github.com/plotly/orca#installation
    #     #conda install -c plotly plotly-orca
    #     content = function(file) {
    #         orca(file, plot = spatial_plot(), format = "pdf",width = 12*96, height = 10*96)
    #     }
    # )
    
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
        validate(need(input$ripleys_selection !="", "Please wait while calculations are running....."),
                 need(sum(spatial_data()[[input$ripleys_selection]]) > 5, "Please select a marker with more than 5 positive cells....."))
        #print(input$ripleysEstimator %in% c("M", "K", "L"))
        withProgress(message = "Calculating", value = 0,{
            incProgress(0.33, detail = "Ripley's K.....")
            ripley = Ripley(spatial_data(), input$ripleys_selection)
            incProgress(0.33, detail = "Nearest Neighbor.....")
            g = NN_G(spatial_data(), input$ripleys_selection)
            incProgress(0.33, detail = "Completed!")
            return(list(ripley, g))
        })
    })
    
    spatialStatsPlot = reactive({
        validate(need(input$ripleys_selection !="", "Please wait while calculations are running....."))
        if(input$ripleysEstimator %in% c("M", "K", "L")){
            Ripley_plot(ripley_data = ripley_data()[[1]], estimator = input$ripleysEstimator)
        } else if(input$ripleysEstimator == "G"){
            G_plot(G_data = ripley_data()[[2]])
        }
    })
    
    output$ripleysPlot = renderPlot({
        spatialStatsPlot()
    })
    
    output$download_ripley = downloadHandler(
        filename = function() { paste(Sys.Date(), '-spatialStats_plot.pdf', sep='') },
        
        content = function(file) {
            ggsave(file, plot = spatialStatsPlot(), device = "pdf",width = 12, height = 10, units = "in")
        }
    )
    
#Getting started RMD rendering
    
    output$aboutitime <- renderUI({
        withMathJax({
            k = knitr::knit(input = "AboutiTIME.Rmd", quiet = T)
            HTML(markdown::markdownToHTML(k, fragment.only = T))
        })
    })
    
    output$getting_started <- renderUI({
        withMathJax({
            k = knitr::knit(input = "GettingStarted.Rmd", quiet = T)
            HTML(markdown::markdownToHTML(k, fragment.only = T))
        })
    })

})