
heat_map <- function(summary_clinical_merge, markers = markers,
                    clin_vars = clin_vars, colorscheme){

  cells <- summary_clinical_merge %>% select(any_of(markers))
  cells <- as.matrix(cells)
  assign("cells", cells, envir=globalenv())
  
  annotation <- summary_clinical_merge %>% select(all_of(clin_vars))
  assign("annotation", annotation, envir=globalenv())
  row.names(cells) = annotation[,1]
  annotation_colors <- unique(colourvalues::colour_values(annotation[,1], palette = colorscheme)[[1]])
  #assign("colors" , annotation_colors, envir =)
  
  ha <- HeatmapAnnotation(anno = row.names(cells),
                          col = colourvalues::colour_values(annotation[,1], palette = colorscheme),
                          show_legend = TRUE,
                          height = unit(0.5, "cm"),
                          annotation_name_side = "right",
                          annotation_label = clin_vars,
                          show_annotation_name = TRUE,
                          annotation_name_gp = gpar(fontsize=14),
                          annotation_legend_param = list(title = clin_vars,
                                                         fill = annotation_colors)

                          )
  
  # ha = HeatmapAnnotation(anno = row.names(cellsells),
  #                       annotation_legend_param = list(anno = list(title = "test")))
  
  full_heatmap <- Heatmap(mat = t(cells), 
                          col = viridis::viridis_pal(option = colorscheme)(20),
                          show_column_names = FALSE,
                          show_row_names = TRUE, 
                          row_names_side = "left",
                          cluster_columns = TRUE,
                          show_row_dend = FALSE,
                          show_column_dend = FALSE,
                          row_title = "",
                          # row_title_gp = gpar(fontsize = 15),
                          heatmap_legend_param = list(title = "Percent"),
                          border = FALSE,
                          row_names_gp = gpar(fontsize = 10),
                          top_annotation = ha
                          )
  

  draw(full_heatmap, padding = unit(c(2, 40, 2, 40), "mm"))
  }




