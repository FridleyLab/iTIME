# @clinvar the name of the column for the clinical variable.
# @cellvar the name of the column for the cell type (marker)
# @return summ_plots a list with three ggplot objecta containing a boxplot, a violin plot, and a histogram with the clinical variable as a factor.
summary_plots_fn <- function(clinvar, cellvar){
  box_p <- ggplot(summary_clinical, aes(x=get(clinical_var), y=get(celltype_var), fill=get(clinical_var))) + 
    geom_boxplot() +
    xlab(str_to_title(clinical_var)) + ylab(gsub("_", " ", str_to_title(celltype_var))) +
    labs(fill=str_to_title(clinical_var))
  
  violin_p <- ggplot(summary_clinical, aes(x=get(clinical_var), y=get(celltype_var), fill=get(clinical_var))) + 
    geom_violin() +
    xlab(str_to_title(clinical_var)) + ylab(gsub("_", " ", str_to_title(celltype_var))) +
    labs(fill=str_to_title(clinical_var))
  
  hist_p <- ggplot(summary_clinical, aes(x=get(celltype_var), color=get(clinical_var))) + 
    geom_histogram(binwidth=100, fill='white') +
    xlab(str_to_title(gsub("_", " ", celltype_var))) + ylab("Count") +
    labs(color=str_to_title(clinical_var))
  
  summ_plots <- list(box_p, violin_p, hist_p)
  
  return(summ_plots)
  
}


# Load tidyverse for data frame manipulation.
library('tidyverse')

# Load data from .Rdata file.
load("data/example_data.RData")

# Clean column names for the image summary data and the clinical data.
summary_clean <- janitor::clean_names(summary)
clinical_clean <- janitor::clean_names(clinical)

#Join the image summary data and the clinical data by the filename of the image ("Image tag").
summary_clinical <- left_join(clinical_clean, summary_clean, by="image_tag")

# Show columns available for selection.
cat("CLINICAL VARIABLES:\n")
names(clinical_clean)
cat("\n")
cat("CELL TYPES:\n")
names(summary_clean)

# Specify user-selected clinical column.
# Specify user-selected cell type column.
clinical_var <- 'race'
celltype_var <- 'cd3_opal_570_positive_cells'

plots <- summary_plots_fn(clinical_var, celltype_var)


