# library(tidyverse)
# summary <- read_csv("data/summary.csv")
# clinical <- read_csv("data/clinical.csv")
# 
# summary_clinical_merge = merge(clinical, summary,
#                                by.x = 'subID', by.y = 'samples')
# colnames(summary_clinical_merge) = gsub(pattern = '%',
#                                         replacement = 'Percent',
#                                         colnames(summary_clinical_merge))
# markers_all = c(
#   'FOXP3 (Opal 620) Positive Cells',
#   'CD3 (Opal 570) Positive Cells',
#   'CD8 (Opal 520) Positive Cells',
#   'CD3+ FOXP3+ Positive Cells',
#   'CD3+ CD8+ Positive Cells'
# )
# markers = markers_all[1]
# percent_threshold  = c(1, 3, 5)
freq_table <-
  function(summary_clinical_merge,
           markers = markers) {
    cells <-
      summary_clinical_merge %>% select(any_of(paste('Percent', markers)))
    
    table <-
      cells %>% 
      mutate(`> 1%` = .[[paste('Percent', markers)]]>1,
             `> 2%` = .[[paste('Percent', markers)]]>2,
             `> 3%` = .[[paste('Percent', markers)]]>3,
             `> 4%` = .[[paste('Percent', markers)]]>4,
             `> 5%` = .[[paste('Percent', markers)]]>5) %>% 
      select(`> 1%`,`> 2%`,`> 3%`,`> 5%`,`> 5%`) %>%
      summarize_all( ~ sum(.))
    rownames(table) = markers
    
    return(table)
  }
# 
# for(i in markers_all){
#   print(freq_table(summary_clinical_merge,
#              markers = i))
# }





