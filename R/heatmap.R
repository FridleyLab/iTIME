heat_map = function(clinical = clinical, summary = summary, markers = markers, 
         clin_vars = clin_vars){
data_for_plot = data %>% inner_join(clinical) %>% select(markers, clin_vars, image_tag) %>%
  data.frame(check.names = FALSE)

library(pheatmap)
cells = data_for_plot %>% select(markers)
rownames(cells) = data_for_plot$image_tag
annotation = data_for_plot %>% select(image_tag, clin_vars)
rownames(annotation) = annotation$image_tag

annotation = annotation %>% select(-image_tag)
return(pheatmap(t(sqrt(cells)), show_colnames = FALSE, treeheight_row = 0, treeheight_col = 0,
         annotation_col = annotation))
}


summary = summary %>% mutate(image_tag = `Image Tag`)
clin_vars = c('race', 'hiv_status')  
data = summary
markers = grep('\\% ', colnames(summary),value=TRUE)

heat_map(clinical = clinical, summary = summary, markers = markers, 
       clin_vars = clin_vars)