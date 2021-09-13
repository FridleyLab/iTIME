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

#Chris's function
#removed percent
#alex edit to group by clinical variable
freq_table_by_marker <-
  function(summary_clinical_merge,
           clinical = clinical,
           markers = markers) {
    cells <-
      summary_clinical_merge %>% select(paste(clinical),any_of(paste( markers)))
    
    table <-
      cells %>% 
      mutate(`> 1%` = .[[paste( markers)]]>1,
             `> 2%` = .[[paste( markers)]]>2,
             `> 3%` = .[[paste( markers)]]>3,
             `> 4%` = .[[paste( markers)]]>4,
             `> 5%` = .[[paste( markers)]]>5,
             `> 10%` = .[[paste(markers)]]>10) %>% 
      group_by(.[[paste(clinical)]]) %>%
      select(`> 1%`,`> 2%`,`> 3%`,`> 4%`,`> 5%`, `> 10%`) %>%
      summarize_all( ~ sum(.))
    colnames(table)[1] = clinical
    
    return(table)
}
# 
# for(i in markers_all){
#   print(freq_table(summary_clinical_merge,
#              markers = i))
# }





