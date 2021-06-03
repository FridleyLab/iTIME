pca_plot <- function(summary_clinical_merge, markers = markers,
                      clin_vars = clin_vars, colorscheme){
  
  pca = prcomp(summary_clinical_merge %>% select(markers))
  
  plot = summary_clinical_merge %>% 
    mutate(PC1 = pca$x[,1], 
           PC2 = pca$x[,2]) %>%
    ggplot(aes(x = PC1, y = PC2, color = get(clin_vars))) + 
    geom_point() + theme_bw() + labs(color = str_to_title(clin_vars))
  
  return(plot)
}


#Testing
clin_vars = 'status'
summary_data = read.csv("example_data/deidentified_summary.csv", check.names = FALSE)
clinical_data = read.csv("example_data/deidentified_clinical.csv", check.names = FALSE)
summary_data_merged = merge(clinical_data, summary_data)

markers = summary_data_merged %>% select(grep('Positive' ,colnames(.))) %>%
  select(grep('\\%', colnames(.))) %>% colnames()


pca_plot(summary_clinical_merge = summary_data_merged, markers = markers, clin_vars = clin_vars,
         colorscheme = NULL)

