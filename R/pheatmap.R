#Alex please look at this link to see if we can get this heatmap in the shiny app
#https://stackoverflow.com/questions/59445092/how-to-plot-heatmap-with-r-shiny


pheat_map <- function(summary_clinical_merge, markers = markers,
                     clin_vars = clin_vars, colorscheme){
  
  cells <- summary_clinical_merge %>% select(any_of(markers))
  cells <- as.matrix(cells)
  rownames(cells) = 1:nrow(cells)
  annotation <- summary_clinical_merge %>% select(all_of(clin_vars)) %>%
    data.frame(check.names = FALSE)
  rownames(annotation) = 1:nrow(cells)
  pheatmap::pheatmap(t(sqrt(cells)),show_rownames = TRUE, 
                     show_colnames = FALSE, treeheight_col = 0,
                     treeheight_row = 0, annotation_col = annotation,
                    )
  
}




