
#Not considering repeated measures
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
                  Total, clin_vars, reference,digits=4){
  out = list()
  levels = unique(summary_data_merged[[clin_vars]])
  tmp = summary_data_merged %>% select(Total, markers, clin_vars) %>%
    mutate(clin_vars = factor(.[[clin_vars]], 
                                   levels = c(reference,
                                              levels[levels != reference]
                                              )))
  
  round4 = function(x){return(round(x,digits=digits))}
  # #Fit Poisson Model
  # model_fit_poisson = try(glm(tmp[[markers]] ~ tmp$clin_vars, 
  #                          family = poisson(link = 'log')), silent = TRUE)
  # 
  # if(class(model_fit_poisson) == "try-error"){
  #   model_fit_poisson = NULL
  #   AIC_poisson = NULL
  # } else {
  #   aov_poisson = coefficients(summary(model_fit_poisson))
  #   AIC_poisson = round4(AIC(model_fit_poisson))
  # }
  # 
  # #Fit Binomial Model
  # model_fit_binomial = try(glm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]])~ tmp$clin_vars,
  # family = "binomial"), silent = TRUE)
  # if(class(model_fit_binomial) == "try-error"){
  #   model_fit_binomial = NULL
  #   AIC_binomial = NULL
  # } else {
  #   aov_binomial = coefficients(summary(model_fit_binomial))
  #   AIC_binomial = round4(AIC(model_fit_binomial))
  # }
  # 
  # #Fit negative binomial model 
  # model_fit_negbinom = try(vglm(tmp[[markers]] ~ tmp$clin_vars, negbinomial), silent = TRUE)
  # if(class(model_fit_negbinom) == "try-error"){
  #   model_fit_negbinom = NULL
  #   AIC_negbinom = NULL
  # } else {
  #   aov_negbinom = coefficients(summary(model_fit_negbinom))
  #   AIC_negbinom = round4(AIC(model_fit_negbinom))
  # }
  #   
  # model_fit_zibinomial <- try(vglm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]]) ~ tmp$clin_vars, 
  #                         zibinomial), silent = TRUE)
  # if(class(model_fit_zibinomial) == "try-error"){
  #   model_fit_zibinomial = NULL
  #   AIC_zibinomial = NULL
  # } else {
  #   aov_zibinomial = coefficients(summary(model_fit_zibinomial))
  #   AIC_zibinomial = round4(AIC(model_fit_zibinomial))
  # }
  # 
  # 
  # #Fit Zero inflated poisson
  # model_fit_zipoisson = try(vglm(tmp[[markers]] ~ tmp$clin_vars, zipoisson), silent = TRUE)
  # if(class(model_fit_zipoisson) == "try-error"){
  #   model_fit_zipoisson = NULL
  #   AIC_zipoisson = NULL
  # } else {
  #   aov_zipoisson = coefficients(summary(model_fit_zipoisson))
  #   AIC_zipoisson = round4(AIC(model_fit_zipoisson))
  # }
  # 
  # #Fit Zero inflated negative binomial model
  # model_fit_zinegbinomial = try(vglm(tmp[[markers]] ~ tmp$clin_vars, zinegbinomial, data = tmp), silent = TRUE)
  # if(class(model_fit_zinegbinomial) == "try-error"){
  #   model_fit_zinegbinomial = NULL
  #   AIC_zinegbinomial = NULL
  # } else if(is.infinite(model_fit_zinegbinomial@criterion$loglikelihood)){
  #   model_fit_zinegbinomial = NULL
  #   AIC_zinegbinomial = NULL
  # } else {
  #   aov_zinegbinomial = coefficients(summary(model_fit_zinegbinomial))
  #   AIC_zinegbinomial = round4(AIC(model_fit_zinegbinomial))
  # }
  #Fit Betabinomial model
  model_fit_bb = try(VGAM::vglm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]]) ~ tmp$clin_vars, 
                             betabinomial(zero = 2), data = tmp), silent = TRUE)
  if(class(model_fit_bb) == "try-error"){
    model_fit_bb = NULL
    AIC_bb = NULL
  } else {
    aov_bb = coefficients(summary(model_fit_bb))
    AIC_bb = round4(AIC(model_fit_bb))
  }
  #####
  # out$aic = data.frame(Distribution = c('Poisson', 'Negative Binomial', 'Zero Inflated Poisson', "",
  #                                       'Binomial', 'Beta Binomial', 'Zero Inflated Binomial'),
  #                      AIC = c(AIC_poisson, AIC_negbinom, AIC_zipoisson, "", 
  #                                         AIC_binomial, AIC_bb, AIC_zibinomial),
  #                      Family = c(rep('Poisson', 3),"" , rep('Binomial', 3)), 
  #                      check.names = FALSE)
  
  out$models = list('Beta Binomial' = model_fit_bb
                    # ,'Binomial' = model_fit_binomial
                    # ,'Negative Binomial' = model_fit_negbinom
                    # ,'Poisson' = model_fit_poisson
                    # ,'Zero Inflated Binomial' = model_fit_zibinomial
                    # ,'Zero Inflated Poisson' = model_fit_zipoisson
                    )
  
  return(out)
} 



#Repeated measures
models_repeated_measures = function(summary_data_merged, markers,
                  Total, clin_vars, reference, choose_clinical_merge, digits = 4){
  out = list()
  round4 = function(x){return(round(x,digits=digits))}
  levels = unique(summary_data_merged[[clin_vars]])
  tmp = summary_data_merged %>% select(choose_clinical_merge, 
                                       Total, markers, clin_vars) %>%
    mutate(clin_vars = factor(.[[clin_vars]], 
                              levels = c(reference,
                                         levels[levels != reference]
                              )))
  
  # #Fit Poisson Model
  # model_fit_poisson = try(glm(tmp[[markers]] ~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]), 
  #                             family = poisson(link = 'log')), silent = TRUE)
  # if(class(model_fit_poisson) != 'try-error'){
  #   aov_poisson = coefficients(summary(model_fit_poisson))
  #   AIC_poisson = AIC(model_fit_poisson)
  # }else{
  #   aov_poisson = NULL
  #   AIC_poisson = NA
  # }
  # 
  # #Fit Binomial Model
  # model_fit_binomial = try(glm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]])~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]),
  #                              family = "binomial"), silent = TRUE)
  # if(class(model_fit_binomial) != 'try-error'){
  #   aov_binomial = coefficients(summary(model_fit_binomial))
  #   AIC_binomial = AIC(model_fit_binomial)
  # }else{
  #   aov_binomial = NULL
  #   AIC_binomial = NA
  # }
  # 
  # #Fit negative binomial model 
  # model_fit_negbinom = try(vglm(tmp[[markers]] ~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]), negbinomial), silent = TRUE)
  # if(class(model_fit_negbinom) != 'try-error'){
  #   aov_negbinom = coefficients(summary(model_fit_negbinom))
  #   AIC_negbinom = AIC(model_fit_negbinom)
  # }else{
  #   aov_negbinom = NULL
  #   AIC_negbinom = NA
  # }
  # 
  # #Fit Zero inflated binomial distribution
  # model_fit_zibinomial <- try(vglm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]]) ~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]), 
  #                                  zibinomial), silent = TRUE)
  # if(class(model_fit_zibinomial) != 'try-error'){
  #   aov_zibinomial = coefficients(summary(model_fit_zibinomial))
  #   AIC_zibinomial = AIC(model_fit_zibinomial)
  # }else{
  #   aov_zibinomial = NULL
  #   AIC_zibinomial = NA
  # }
  # 
  # 
  # #Fit Zero inflated poisson
  # model_fit_zipoisson = try(vglm(tmp[[markers]] ~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]), zipoisson), silent = TRUE)
  # if(class(model_fit_zipoisson) != 'try-error'){
  #   aov_zipoisson = coefficients(summary(model_fit_zipoisson))
  #   AIC_zipoisson = AIC(model_fit_zipoisson)
  # }else{
  #   aov_zipoisson = NULL
  #   AIC_zipoisson = NA
  # }
  # 
  # #Fit Zero inflated negative binomial model
  # model_fit_zinegbinomial = try(vglm(tmp[[markers]] ~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]), zinegbinomial, data = tmp), silent = TRUE)
  # if(class(model_fit_zinegbinomial) != 'try-error'){
  #   aov_zinegbinomial = coefficients(summary(model_fit_zinegbinomial))
  #   AIC_zinegbinomial = AIC(model_fit_zinegbinomial)
  # }else{
  #   aov_zinegbinomial = NULL
  #   AIC_zinegbinomial = NA
  # }
  ########
  #Fit Betabinomial model
  model_fit_bb = try(VGAM::vglm(cbind(tmp[[markers]], tmp[[Total]] - tmp[[markers]]) ~ tmp$clin_vars + (1|tmp[[choose_clinical_merge]]), 
                                betabinomial(zero = 2), data = tmp), silent = TRUE)
  if(class(model_fit_bb) != 'try-error'){
    aov_bb = coefficients(summary(model_fit_bb))
    AIC_bb = AIC(model_fit_bb)
  }else{
    aov_bb = NULL
    AIC_bb = NA
    model_fit_bb = model_fit_bb[1]
  }
  
  # out$aic = data.frame(Distribution = c('Poisson', 'Negative Binomial', 'Zero Inflated Poisson', "",
  #                                       'Binomial', 'Beta Binomial', 'Zero Inflated Binomial'),
  #                      AIC = c(AIC_poisson, AIC_negbinom, AIC_zipoisson, "", 
  #                              AIC_binomial, AIC_bb, AIC_zibinomial),
  #                      Family = c(rep('Poisson', 3),"" , rep('Binomial', 3)), 
  #                      check.names = FALSE)
  
  out$models = list('Beta Binomial' = model_fit_bb
                    # ,'Binomial' = model_fit_binomial
                    # ,'Negative Binomial' = model_fit_negbinom
                    # ,'Poisson' = model_fit_poisson
                    # ,'Zero Inflated Binomial' = model_fit_zibinomial
                    # ,'Zero Inflated Poisson' = model_fit_zipoisson
                    )
  
  return(out)
} 



#This is the function that needs to included in the app script
model_checked_repeated = function(summary_data_merged,markers,
                                  Total, clin_vars, reference,
                                  choose_clinical_merge, digits=4){
  round4 = function(x){return(round(x,digits=digits))}
  markers = substr(markers, 9, nchar(markers))
  #Check whether or not any subject has multiple samples
  #Maybe we print a message saying whether repeated measures were detected?
  if(any(table(summary_data_merged[[choose_clinical_merge]])>1)){
    return(
      models_repeated_measures(summary_data_merged, markers,
                               Total, clin_vars, reference, 
                               choose_clinical_merge, digits))
  }else{
    return(
      models(summary_data_merged, markers,Total, clin_vars, reference, digits))
  }
  
}


################################################################################
#test
# library(VGAM)
# library(tidyverse)
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
#               Total = Total, clin_vars, 'B')
# 
# 
# 
# #Randomly simulate a copy of the dataset
# sim = summary_data_merged %>% 
#  select(deidentified_id, clin_vars, Total, markers) %>% 
#   rowwise() %>% 
#   mutate(new = rpois(1,30)) %>%
#   pivot_longer(cols = c(markers, new), names_to = 'name', values_to = markers) %>%
#   select(-name)
# 
# mods_repeated = models_repeated_measures(summary_data_merged = sim,markers = markers,
#                                          Total = Total, clin_vars = clin_vars, reference = 'B',
#                                          choose_clinical_merge = 'deidentified_id')
# 
# mod_ifelse = model_checked_repeated(summary_data_merged,markers,
#                                   Total, clin_vars, reference = 'B',
#                                   choose_clinical_merge = 'deidentified_id')
# 
# mod_ifelse_repeated = model_checked_repeated(summary_data_merged = sim,markers = markers,
#                                     Total = Total, clin_vars = clin_vars, reference = 'B',
#                                     choose_clinical_merge = 'deidentified_id')
# 
# #Something is strange mods_repeated should produce same output mod_ifelse_repeated
# #mods_repeated is correct (I think since there are only two repeated measures 
# #more parameters in ZI and beta binomial models and we are estimating)
# 
# sim_2 = summary_data_merged %>% 
#   select(deidentified_id, clin_vars, Total, markers) %>% 
#   rowwise() %>% 
#   mutate(new = rpois(1,30),
#          new_2 = rpois(1,30)) %>%
#   pivot_longer(cols = c(markers, new, new_2), names_to = 'name', values_to = markers) %>%
#   select(-name)
# 
# mods_repeated = models_repeated_measures(summary_data_merged = sim_2,markers = markers,
#                                          Total = Total, clin_vars = clin_vars, reference = 'B',
#                                          choose_clinical_merge = 'deidentified_id')
# 
# 
# mod_ifelse_repeated = model_checked_repeated(summary_data_merged = sim_2,markers = markers,
#                                              Total = Total, clin_vars = clin_vars, reference = 'B',
#                                              choose_clinical_merge = 'deidentified_id')
# 
