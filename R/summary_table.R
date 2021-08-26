#Alex Soupir
#iTIME
summary_table = function(summary_clinical_merged,
                         marker,
                         clinical,
                         merged){
  data = summary_clinical_merged %>% 
    select(paste(clinical), paste(merged), any_of(paste(marker)))
  colnames(data) = c("clinical","merged_var","marker")
  table = 
    data %>% 
    group_by(clinical) %>% 
    summarise(Min = min(marker),
              Median = median(marker),
              Mean = mean(marker),
              Max = max(marker),
              SD = sd(marker),
              Subjects = length(unique(merged_var)),
              Samples = length(marker))
  
  colnames(table)[1] = clinical
  return(table)
}
