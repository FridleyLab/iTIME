# #Chris's function
# #edited out % with markers to fix issues with contingency printout
# added as.numeric for the percent threshold, don't know why such error

contingency_table <- function(summary_clinical_merge, markers = markers,
                              clin_vars = clin_vars, percent_threshold = percent_threshold){
  #Maybe provide an error for multiple columns
  cells <- summary_clinical_merge %>% select(any_of(paste(markers)))

  above <- function(x, percent_threshold){ifelse(x > percent_threshold,
                                                 paste0('Greater than ', percent_threshold, '%'),
                                                 paste0('Less than ', percent_threshold, '%'))}

  table <- cells %>% mutate_all( ~ above(x = ., percent_threshold = as.numeric(percent_threshold))) %>%
    bind_cols(.,summary_clinical_merge %>% select(clin_vars)) %>%
    group_by(.[[paste(markers)]],.[[clin_vars]]) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = `.[[clin_vars]]`, values_from = n) %>%
    mutate_all(~replace_na(.,0))
  colnames(table)[1] = markers
  return(table)
}