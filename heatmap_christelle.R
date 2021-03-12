library(pheatmap)
library(ComplexHeatmap)
library(tidyverse)
library(readr)

clinical <- read_csv("data/clinical.csv", locale = locale(encoding = "Latin1"))
summary_data <- read_csv("data/summary.csv", locale = locale(encoding = "Latin1"))
summary_clinical_merge <- right_join(clinical, summary_data, by = c("image_tag" = "Image Tag"))


# with heatmap.plus
library(heatmap.plus)
library(RColorBrewer)
library(gplots)


df1 <- summary_clinical_merge %>% 
  select(starts_with("%")) %>% 
  `rownames<-`(annotation)
ids <- summary_clinical_merge$image_tag
annotation_info <- data_frame(chrom = summary_clinical_merge$race)

df2 <- t(scale(df1))

annotation_info
ann <- annotation_info %>% distinct(chrom)
ann
NROW(ann)
annotation_colors <- data_frame(colss = c(RColorBrewer::brewer.pal(n = NROW(ann), "Set3")))# c(colors())
annotation_colors
class(annotation_colors)
foo <- bind_cols(ann, annotation_colors)
foo

super <- left_join(annotation_info, foo, by = ("chrom"))
super
RColorBrewer::brewer.pal(n = NROW(ann), "Set3")

heatmap.2(df2, main = "",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          labCol = FALSE, margins = c(1,16), # bottom, right
          lwid=c(2, 20), # change the left dend sixe
          ColSideColors = super$colss,
          labRow = left,
          key.par=list(mar=c(3.5,1,3,0)),
          scale = "column")
dev.off()
# heatmap.2() funtion

itime_heatmap <- function(data = summary_clinical_merge){
  df1 <- data %>% select(starts_with("%"))
  annotation_info <- data_frame(chrom = summary_clinical_merge$race)
  
  df2 <- t(scale(df1))
  
  # annotation_info
  ann <- annotation_info %>% distinct(chrom)
  # ann
  # NROW(ann)
  annotation_colors <- data_frame(colss = c(RColorBrewer::brewer.pal(n = NROW(ann), "Set3")))# c(colors())
  # annotation_colors
  # class(annotation_colors)
  foo <- bind_cols(ann, annotation_colors)
  # foo
  # 
  super <- left_join(annotation_info, foo, by = ("chrom"))
  # super
  
  heatmap.2(df2, main = "",

            trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1,
            margins = c(1,10), # bottom, right
            ColSideColors = super$colss,
            scale = "column")
}

itime_heatmap(summary_clinical_merge)
# With Heatmap
annotation_info
ann <- annotation_info %>% distinct(chrom)
ann
annotation_colors <- data_frame(colss = c("blue", "red", "black"))# c(colors())
annotation_colors
class(annotation_colors)
foo <- bind_cols(ann, annotation_colors)
foo

super <- left_join(annotation_info, foo, by = ("chrom"))
super
as.matrix(super)
# names(annotation_colors) <- unique(annotation_info$chrom)
annotation_colorss <- super$colss
annotation_colorss
class(annotation_colorss)
names(annotation_colorss) <- super$chrom
annotation_colorss

vec <- as.vector(annotation_colorss)
str(vec)


annotation_colorss <- c(rep(c("black", "white"), 128), "red")
names(annotation_colorss) <- paste("chr", c(seq(1,257)), sep = "")

# left_join(annotation_info, as.data.frame(annotation_colors))
ha <- HeatmapAnnotation(annotation_info, col = list(chrom = annotation_colorss)#,
                  # annotation_name_side = "right",
                  # show_annotation_name = TRUE,
                  # annotation_name_gp = gpar(fontsize=14)
                  )

Heatmap(df2,
        show_column_names=FALSE,
        # show_row_names=TRUE,
        # cluster_columns=TRUE, # TRUE is default for clustering so show the ids in order of the data
        # show_row_dend = FALSE,
        show_column_dend = FALSE,
        # row_title="",
        row_names_side = "left",
        row_title_gp=gpar(fontsize=15),
        heatmap_legend_param = list(title = "Percent"),
        border=TRUE,
        row_names_gp = gpar(fontsize = 8),
        top_annotation = ha
        )

super

Heatmap(df2,
        show_column_names=FALSE,
        # show_row_names=TRUE,
        # cluster_columns=TRUE, # TRUE is default for clustering so show the ids in order of the data
        # show_row_dend = FALSE,
        # show_column_dend = FALSE,
        # row_title="",
        row_names_side = "left",
        row_title_gp=gpar(fontsize=15),
        heatmap_legend_param = list(title = "Percent"),
        border=TRUE,
        row_names_gp = gpar(fontsize = 8)
)



