#library(tidyverse)
#summary <- read_csv("data/summary.csv")
#clinical <- read_csv("data/clinical.csv")

#summary_clinical_merge = inner_join(clinical,summary)
#markers = c('FOXP3 (Opal 620) Positive Cells', 'CD3 (Opal 570) Positive Cells',
#            'CD8 (Opal 520) Positive Cells', 'CD3+ FOXP3+ Positive Cells', 'CD3+ CD8+ Positive Cells')

freq_table <- function(summary_clinical_merge, markers = markers, percent_threshold = percent_threshold){
  cells <- summary_clinical_merge %>% select(any_of(paste(markers)))

above <- function(x, percent_threshold) (x > percent_threshold)

table <- cells %>% mutate_all( ~ above(x = ., percent_threshold = percent_threshold)) %>%
  summarize_all(~sum(.))
return(table)
}

  
  
  
  
  