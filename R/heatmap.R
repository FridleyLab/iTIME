heat_map = function(summary_clinical_merge, markers = markers, 
         clin_vars = clin_vars){
#data_for_plot = summary_clinical_merge %>% select(get(markers), get(clin_vars), image_tag) %>%
#  data.frame(check.names = FALSE)

data_for_plot = summary_clinical_merge[,c(markers, clin_vars, "image_tag")]
 
cells = data_for_plot %>% select(markers)
cells = data_for_plot[,c(markers)]

rownames(cells) = data_for_plot$image_tag
annotation = data_for_plot %>% select(image_tag, clin_vars)
#annotation = data_for_plot[,c( clin_vars, image_tag)]
rownames(annotation) = annotation$image_tag
# 
annotation = annotation %>% select(-image_tag)
#out = pheatmap(t(sqrt(cells)), show_colnames = FALSE, treeheight_row = 0, treeheight_col = 0
#               ,annotation_col = annotation
#               )
#out

## annotation for TP53 fusion


#names(ha) <- clin_vars




ha<-HeatmapAnnotation(Annotation = anno_simple(annotation),
                      show_legend = TRUE, height=unit(0.5, "cm"),
                      annotation_name_side = "right",
                      show_annotation_name = TRUE,
                      annotation_name_gp = gpar(fontsize=14)
                      )
ht.mrna<-Heatmap(mat=t(sqrt(cells)), #col=col_fun ,
                 show_column_names=FALSE,
                 show_row_names=TRUE,
                 cluster_columns=TRUE,
                 show_row_dend = FALSE,
                 show_column_dend = FALSE,
                 row_title="",
                 row_title_gp=gpar(fontsize=15),
                 heatmap_legend_param = list(title = "Percent"),
                 border=TRUE,
                 row_names_gp = gpar(fontsize = 14)
                 ,top_annotation = ha)


ht.mrna


}


# summary = summary %>% mutate(image_tag = `Image Tag`)
# clin_vars = c('race', 'hiv_status')  
# data = summary
# markers = grep('\\% ', colnames(summary),value=TRUE)
# 
# heat_map(clinical = clinical, summary = summary, markers = markers, 
#        clin_vars = clin_vars)

