#chris Wilsom
#summary_data_merge
#picked_marker
CDF_plots = function(summary_data_merge = summary_data_merge, markers = markers){
sample_stats = summary_data_merged %>% select(grep('Total', colnames(.)), markers) %>%
  pivot_longer(cols = 2:ncol(.), values_to = 'Count', names_to = 'Marker') %>%
  group_by(Marker) %>%
  summarize(prob0 = mean(Count == 0, na.rm = TRUE),
            Avg_p = mean(Count/`Total Cells`, na.rm = TRUE),
            Avg_Count = mean(Count, na.rm = TRUE),
            Avg_Total = round(mean(`Total Cells`, na.rm = TRUE)))
   
  cdfs = summary_data_merged %>%  select(grep('Total', colnames(.)), markers) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = 'Marker', values_to = 'Count') %>%
    mutate(ecdf = ecdf(Count)(Count)) %>%
    mutate(Poisson =  ppois(q = Count, 
                            lambda = sample_stats$Avg_Count),
           Binomial = pbinom(q = Count, 
                             size = round(sample_stats$Avg_Total),
                             prob = sample_stats$Avg_p,
           ),
           `ZI Poisson` =  pzipois(q = Count, 
                                   lambda = sample_stats$Avg_Count,
                                   pstr0 = sample_stats$prob0
           ),
           `ZI Binomial` = pzibinom(q = Count, 
                                    size = round(sample_stats$Avg_Total),
                                    prob = sample_stats$Avg_p,
                                    pstr0 = sample_stats$prob0
           ),
           `Negative Binomial` =  pnbinom(q = Count, 
                                          size = round(sample_stats$Avg_Count),
                                          prob = 1 - sample_stats$prob0
           ),
           `Beta Binomial` =  pbetabinom(q = Count, 
                                         size = round(sample_stats$Avg_Total),
                                         prob = sample_stats$Avg_p,
                                         rho = sample_stats$prob0)
           
    ) %>% 
    pivot_longer(col = 5:ncol(.), values_to = 'CDF', names_to = 'Distribution')

cdfs = cdfs %>% mutate(family = ifelse(Distribution %in% c('Poisson', 'ZI Poisson', 'Negative Binomial'),
                                       'Poisson', 'Binomial'),
                       Distribution = factor(Distribution))


binomial_plot = cdfs %>% filter(family == 'Binomial', Marker == markers) %>%
  ggplot(aes(x = Count, y = ecdf, color = 'Empirical')) + geom_line(aes(color = 'Empirical'), color = 'black') + 
  geom_line(aes(x = Count, y = CDF, color = Distribution)) + theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size=16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16)) + 
  labs(color = 'Distribution') + ggtitle('Binomial Family')


poisson_plot = cdfs %>% filter(family == 'Poisson', Marker == markers) %>%
  ggplot(aes(x = Count, y = ecdf, color = 'Empirical')) + geom_line(, color = 'black') + 
  geom_line(aes(x = Count, y = CDF, color = Distribution)) + theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size=16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16)) + 
  labs(color = 'Distribution') + ggtitle('Poisson Family')


final = ggpubr::ggarrange(plotlist = list(binomial_plot, poisson_plot))

return(final)
}

# 
# summary_data = read.csv("example_data/deidentified_summary.csv", check.names = FALSE)
# clinical_data = read.csv("example_data/deidentified_clinical.csv", check.names = FALSE)
# summary_data_merged = merge(clinical_data, summary_data)
# 
# markers = summary_data_merged %>% select(grep('Cells' ,colnames(.))) %>% 
#   select(!grep('\\%',colnames(.))) %>% colnames()
# 
# CDF_plots(summary_data_merged, markers[5])
