#chris wilson
#summary_data_merged
#input$heatmap_selection
#input$picked_clinical_factor
#input$summaryPlotColors

pca_plot_function = function(summary_clinical_merged, markers = markers, clin_vars = clin_vars){
  summary_clinical_merged = summary_clinical_merged %>% mutate_at(clin_vars, as.factor)
  
  pca = prcomp(summary_clinical_merged %>% select(markers))
  
  plot = summary_clinical_merged %>% 
    mutate(PC1 = pca$x[,1], 
           PC2 = pca$x[,2]) %>%
    ggplot(aes(x = PC1, y = PC2, color = get(clin_vars))) + 
    geom_point() + theme_bw() + labs(color = str_to_title(clin_vars))
  
  return(plot)
}


#Testing
# clin_vars = 'status'
# summary_data = read.csv("example_data/deidentified_summary.csv", check.names = FALSE)
# clinical_data = read.csv("example_data/deidentified_clinical.csv", check.names = FALSE)
# summary_data_merged = merge(clinical_data, summary_data)
# 
# markers = summary_data_merged %>% select(grep('Positive' ,colnames(.))) %>%
#   select(grep('\\%', colnames(.))) %>% colnames()
# 
# 
# pca_plot(summary_clinical_merge = summary_data_merged, markers = markers, clin_vars = clin_vars,
#          colorscheme = NULL)

