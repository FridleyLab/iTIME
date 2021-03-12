library(heatmap.plus)
library(RColorBrewer)
library(gplots)

itime_heatmap <- function(data){
  df1 <- data %>% select(starts_with("%"))
  annotation_info <- data_frame(topbar = summary_clinical_merge$race)
  
  df2 <- t(scale(df1))
  
  # annotation_info
  ann <- annotation_info %>% distinct(topbar)
  # ann
  # NROW(ann)
  annotation_colors <- data_frame(colss = c(RColorBrewer::brewer.pal(n = NROW(ann), "Set3")))# c(colors())
  # annotation_colors
  # class(annotation_colors)
  foo <- bind_cols(ann, annotation_colors)
  # foo
  # 
  super <- left_join(annotation_info, foo, by = ("topbar"))
  # super
  
  heatmap.2(df2, main = "",
            
            trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1,
            margins = c(1,16), # bottom, right
            ColSideColors = super$colss,
            scale = "column")
}

itime_heatmap(summary_clinical_merge)
