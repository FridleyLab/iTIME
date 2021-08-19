#Alex please look at this link to see if we can get this heatmap in the shiny app
#https://stackoverflow.com/questions/59445092/how-to-plot-heatmap-with-r-shiny
#removed forced sqrt of cells' value

pheat_map <- function(summary_clinical_merge,
                      markers = markers,
                      clin_vars = clin_vars,
                      anno_clust = anno_clust, 
                      mark_clust = mark_clust){
  
  tmp <- summary_clinical_merge %>% select(any_of(markers),all_of(clin_vars)) %>%
    group_by(.[[clin_vars]]) %>% arrange(.[[clin_vars]]) %>% 
    mutate_at(clin_vars,as.factor) %>%
    data.frame(check.names = FALSE)
  cells <- tmp %>% select(any_of(markers))
  cells <- as.matrix(cells)
  rownames(cells) <- 1:nrow(cells)
  colnames(tmp) = gsub('Percent ', "", colnames(tmp))
  colnames(tmp) = gsub(' Positive Cells', "", colnames(tmp))
  colnames(cells) = gsub('Percent ', "", colnames(cells))
  colnames(cells) = gsub(' Positive Cells', "", colnames(cells))
  annotation <- tmp %>% select(all_of(clin_vars)) %>%
    data.frame(check.names = FALSE)
  rownames(annotation) = 1:nrow(cells)
  if(anno_clust==F){
    cutree_cols = length(levels(tmp[[clin_vars]]))
  }
  pheatmap::pheatmap(t(cells), 
                     show_rownames = T, 
                     cluster_rows = mark_clust, 
                     cluster_cols = !anno_clust, 
                     show_colnames = F, 
                     treeheight_col = 0, 
                     treeheight_row = 5, 
                     annotation_col = annotation,
                     cutree_cols = cutree_cols)
  
}
