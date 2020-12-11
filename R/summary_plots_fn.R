# @datatable a data frame with clinical and summary data merged by "Image tag"
# @clinvar the name of the column for the clinical variable.
# @cellvar the name of the column for the cell type (marker)
# @return summ_plots a list with three ggplot objecta containing a boxplot, a violin plot, and a histogram with the clinical variable as a factor.

summary_plots_fn <- function(datatable, clinvar, cellvar, colorscheme){
  box_p <- ggplot(datatable, aes(x=get(clinvar), y=get(cellvar), fill=get(clinvar))) + 
    geom_boxplot() +
    xlab(str_to_title(clinvar)) + ylab(gsub("_", " ", str_to_title(cellvar))) +
    labs(fill=str_to_title(clinvar)) + theme_classic() +
    viridis::scale_fill_viridis(option = colorscheme, discrete = TRUE)
  
  violin_p <- ggplot(datatable, aes(x=get(clinvar), y=get(cellvar), fill=get(clinvar))) + 
    geom_violin() +
    xlab(str_to_title(clinvar)) + ylab(gsub("_", " ", str_to_title(cellvar))) +
    labs(fill=str_to_title(clinvar)) + theme_classic() +
    viridis::scale_fill_viridis(option = colorscheme, discrete = TRUE)
  
  hist_p <- ggplot(datatable, aes(x=get(cellvar), color=get(clinvar))) + 
    geom_histogram(binwidth=, fill='white') +
    xlab(str_to_title(gsub("_", " ", cellvar))) + ylab("Count") +
    labs(color=str_to_title(clinvar)) + theme_classic() +
    viridis::scale_color_viridis(option = colorscheme, discrete = TRUE)
  
  summ_plots <- list(box_p, violin_p, hist_p)
  
  return(summ_plots)
  
}


# # Load tidyverse for data frame manipulation.
# library('tidyverse')
# 
# # Load data from .Rdata file.
# #load("data/example_data.RData")
# 
# # Clean column names for the image summary data and the clinical data.
# summary_clean <- janitor::clean_names(summary)
# clinical_clean <- janitor::clean_names(clinical)
# 
# #Join the image summary data and the clinical data by the filename of the image ("Image tag").
# summary_clinical <- left_join(clinical_clean, summary_clean, by="image_tag")
# 
# # Show columns available for selection.
# cat("CLINICAL VARIABLES:\n")
# names(clinical_clean)
# cat("\n")
# cat("CELL TYPES:\n")
# names(summary_clean)
# 
# # Specify user-selected clinical column.
# # Specify user-selected cell type column.
# clinical_var <- 'race'
# celltype_var <- 'cd3_opal_570_positive_cells'
# 
# plots <- summary_plots_fn(clinical_var, celltype_var)


