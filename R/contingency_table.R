library(tidyverse)
summary <- read_csv("data/summary.csv")
clinical <- read_csv("data/clinical.csv")

contingency_table <- function(summary_clinical_merge, markers = markers,
                              clin_vars = clin_vars, percent_threshold = percent_threshold){
  #Maybe provide an error for multiple columns
  cells <- summary_clinical_merge %>% select(any_of(paste('%', markers)))
  
  above <- function(x, percent_threshold){ifelse(x > percent_threshold, 
                                                 paste0('Greater than ', percent_threshold, '%'),
                                                 paste0('Less than ', percent_threshold, '%'))}
  
  table <- cells %>% mutate_all( ~ above(x = ., percent_threshold = percent_threshold)) %>% 
    bind_cols(.,summary_clinical_merge %>% select(clin_vars)) %>%
    group_by(.[[paste('%', markers)]],.[[clin_vars]]) %>% 
    tally() %>%
    pivot_wider(names_from = `.[[clin_vars]]`, values_from = n) %>% mutate_all(~replace_na(.,0))
  colnames(table)[1] = paste('%', markers)
  return(table)
}


#Testing
summary_clinical_merge = inner_join(clinical,summary)
markers = c('FOXP3 (Opal 620) Positive Cells','CD3 (Opal 570) Positive Cells','CD8 (Opal 520) Positive Cells')
clin_vars = c('race','hiv_status')
percent_threshold = 1
grid = expand.grid(markers,clin_vars)

for(i in 1:nrow(grid)){
 print(contingency_table(summary_clinical_merge, markers = grid[i,1],
                                clin_vars = grid[i,2], percent_threshold = percent_threshold))
}  



