library(tidyverse)
summary <- read_csv("data/summary.csv")
clinical <- read_csv("data/clinical.csv")

summary_clinical_merge = inner_join(clinical,summary)
markers = c('FOXP3 (Opal 620) Positive Cells')
clin_vars = 'race'

contingency_table <- function(summary_clinical_merge, markers = markers,
                       clin_vars = clin_vars, percent_threshold = percent_threshold){
  #Maybe provide an error for multiple columns
cells <- summary_clinical_merge %>% select(any_of(paste('%', markers)))
  
  above <- function(x, percent_threshold){ifelse(x > percent_threshold, 
                                                  paste0('Greater than ', percent_threshold, '%'),
                                                  paste0('Less than ', percent_threshold, '%'))}
  
  table <- cells %>% mutate_all( ~ above(x = ., percent_threshold = percent_threshold)) %>% 
    bind_cols(.,summary_clinical_merge %>% select(clin_vars)) %>%
    group_by(`% FOXP3 (Opal 620) Positive Cells`, race) %>% 
    tally() %>% pivot_wider(names_from = race, values_from = n) %>% mutate_all(~replace_na(.,0))
  return(table)
  }
  



