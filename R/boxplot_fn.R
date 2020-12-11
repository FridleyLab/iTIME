# @clinvar the name of the column for the clinical variable.
# @cellvar the name of the column for the cell type (marker)
# @return returns a ggplot object containing a boxplot with the clinical variable
# as a factor.

boxplot_fn <- function(clinvar, cellvar){
  box_p <- ggplot(summary_clinical, aes(x=get(clinical_var), y=get(celltype_var), fill=get(clinical_var))) + 
    geom_boxplot() +
    xlab(str_to_title(clinical_var)) + ylab(gsub("_", " ", str_to_title(celltype_var))) +
    labs(fill=str_to_title(clinical_var))
  
  return(box_p)
}