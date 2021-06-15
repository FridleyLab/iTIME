# library(VGAM)
# summary_data = read.csv("example_data/deidentified_summary.csv", check.names = FALSE)
# clinical_data = read.csv("example_data/deidentified_clinical.csv", check.names = FALSE)
# summary_data_merged = merge(clinical_data, summary_data)
# 
# markers = summary_data_merged %>% select(grep('Cells' ,colnames(.))) %>%
#   select(!grep('\\%',colnames(.))) %>% colnames()
# 
# a = 2
# markers = markers[a]
# Total = 'Total Cells'
# clin_vars = 'status'
# mods = models(summary_data_merged = summary_data_merged, markers = markers,
#        Total = Total, clin_vars, 'B')

#variables used
#summary_data_merged()
#input$picked_marker
#input$picked_total_cells
#input$picked_clinical
#input$picked_modeling_reference

#chris wilson
#alex edited - fixed try for zero-inflation error models (try-catch maybe better)
#              added try-catch for all other models too

models = function(summary_data_merged, markers,
                  Total, clin_vars, reference){
markers = substr(markers, 9, nchar(markers))
out = list()
Percent = paste('%', markers)
levels = unique(summary_data_merged[[clin_vars]])
tmp = summary_data_merged %>% select(Total, markers, clin_vars) %>%
  mutate(clin_vars = factor(.[[clin_vars]], 
                                 levels = c(reference,
                                            levels[levels != reference]
                                            )))

model_fit_poisson = try(glm(tmp[[markers]] ~ tmp$clin_vars, 
                         family = poisson(link = 'log')), silent = TRUE)
if(class(model_fit_poisson) == "try-error"){
  model_fit_poisson = NULL
  AIC_poisson = NULL
} else {
  aov_poisson = coefficients(summary(model_fit_poisson))
  AIC_poisson = AIC(model_fit_poisson)
}

model_fit_binomial = try(glm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]])~ tmp$clin_vars,
                      family = "binomial"), silent = TRUE)
if(class(model_fit_binomial) == "try-error"){
  model_fit_binomial = NULL
  AIC_binomial = NULL
} else {
  aov_binomial = coefficients(summary(model_fit_binomial))
  AIC_binomial = AIC(model_fit_binomial)
}

model_fit_negbinom = try(vglm(tmp[[markers]] ~ tmp$clin_vars, negbinomial), silent = TRUE)
if(class(model_fit_negbinom) == "try-error"){
  model_fit_negbinom = NULL
  AIC_negbinom = NULL
} else {
  aov_negbinom = coefficients(summary(model_fit_negbinom))
  AIC_negbinom = AIC(model_fit_negbinom)
}
  
model_fit_zibinomial <- try(vglm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]]) ~ tmp$clin_vars, 
                        zibinomial), silent = TRUE)
if(class(model_fit_zibinomial) == "try-error"){
  model_fit_zibinomial = NULL
  AIC_zibinomial = NULL
} else {
  aov_zibinomial = coefficients(summary(model_fit_zibinomial))
  AIC_zibinomial = AIC(model_fit_zibinomial)
}

model_fit_zipoisson = try(vglm(tmp[[markers]] ~ tmp$clin_vars, zipoisson), silent = TRUE)
if(class(model_fit_zipoisson) == "try-error"){
  model_fit_zipoisson = NULL
  AIC_zipoisson = NULL
} else {
  aov_zipoisson = coefficients(summary(model_fit_zipoisson))
  AIC_zipoisson = AIC(model_fit_zipoisson)
}


model_fit_zinegbinomial = try(vglm(tmp[[markers]] ~ tmp$clin_vars, zinegbinomial, data = tmp), silent = TRUE)
if(class(model_fit_zinegbinomial) == "try-error"){
  model_fit_zinegbinomial = NULL
  AIC_zinegbinomial = NULL
} else {
  aov_zinegbinomial = coefficients(summary(model_fit_zinegbinomial))
  AIC_zinegbinomial = AIC(model_fit_zinegbinomial)
}


model_fit_bb = try(VGAM::vglm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]]) ~ tmp$clin_vars, 
                           betabinomial(zero = 2), data = tmp), silent = TRUE)
if(class(model_fit_bb) == "try-error"){
  model_fit_bb = NULL
  AIC_bb = NULL
} else {
  aov_bb = coefficients(summary(model_fit_bb))
  AIC_bb = AIC(model_fit_bb)
}


out$aic = data.frame(Distribution = c('Poisson', 'Negative Binomial', 'Zero Inflated Poisson', "",
                                      'Binomial', 'Beta Binomial', 'Zero Inflated Binomial'),
                     AIC = c(AIC_poisson, AIC_negbinom, AIC_zipoisson, "", 
                                        AIC_binomial, AIC_bb, AIC_zibinomial),
                     Family = c(rep('Poisson', 3),"" , rep('Binomial', 3)), 
                     check.names = FALSE)

out$models = list('Beta Binomial' = model_fit_bb, 'Binomial' = model_fit_binomial, 
                  'Negative Binomial' = model_fit_negbinom,
                  'Poisson' = model_fit_poisson, 
                  'Zero Inflated Binomial' = model_fit_zibinomial, 
                  'Zero Inflated Poisson' = model_fit_zipoisson)

return(out)
} 



