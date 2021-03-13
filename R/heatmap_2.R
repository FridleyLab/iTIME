library(heatmap.plus)
library(RColorBrewer)
library(gplots)

# toogle_var <- "race"
# annotation_info <- tibble(topbar = select(summary_clinical_merge, all_of(toogle_var)) )

itime_heatmap <- function(data, toogle_var){
  df1 <- data %>% select(starts_with("%"))
  annotation_info <- tibble(topbar = select(data, all_of(toogle_var)) )
  
  df2 <- t(scale(df1))
  
  ann <- annotation_info %>% distinct(topbar)
  annotation_colors <- tibble(colss = c(RColorBrewer::brewer.pal(n = NROW(ann), "Set3")))# c(colors())
  color_df <- bind_cols(ann, annotation_colors)
  color_df <- left_join(annotation_info, color_df, by = ("topbar"))

  heatmap.2(df2, main = "",
            
            trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1,
            margins = c(1,16), # bottom, right
            ColSideColors = color_df$colss,
            scale = "column")
}

itime_heatmap(summary_clinical_merge, "race")
