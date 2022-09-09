#alex 
Permute_positives_r = function(data, ripleys_list, cell_type, sims = 100){
  data = data %>%
    dplyr::mutate(xloc = (XMin + XMax)/2,
                  yloc = (YMin + YMax)/2)
  win = spatstat.geom::convexhull.xy(x = data$xloc,
                                     y = data$yloc)
  grid = expand.grid(cell_type, 1:sims) %>%
    dplyr::mutate(Var1 = as.character(Var1))
  
  data2 = data %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(cell_type), 
                        names_to = "Marker",
                        values_to = "Positive")
  rs = ripleys_list[[1]]$r
  
  cat("Permuting Ripley's K() CSR.....\n")
  perms = purrr::map_df(.x = 1:nrow(grid), 
                        ~{
                          cat(paste0(.x, ", "))
                          data_new = data2 %>%
                            dplyr::mutate(Positive = sample(Positive, nrow(data2), replace = F))
                          data_pos = data_new %>%
                            dplyr::filter(Positive == 1)
                          K_obs = spatstat.geom::ppp(x = data_pos$xloc,
                                                     y = data_pos$yloc,
                                                     window = win) %>%
                            spatstat.core::Kest(r=rs) %>%
                            data.frame() %>%
                            dplyr::select(-border) %>%
                            dplyr::mutate(iter = .x)
                          return(K_obs)
                        })
  
  ripleys_list$perm = perms %>%
    dplyr::group_by(r) %>%
    summarise(perm_trans = mean(trans), perm_iso = mean(iso))
  return(ripleys_list)
}



#christelle

Ripley <- function(data, cell_type, alpha=0.05, sims = 100)
{
  location2 <- data %>% mutate(Xloc = (XMin + XMax)/2, Yloc = (YMin + YMax)/2)
  loc <- location2 %>% select(c(Xloc, Yloc, cell_type)) %>% filter(.[[cell_type]] == 1)
  n = nrow(loc)
  w <- convexhull.xy(x = loc$Xloc, y = loc$Yloc)
  po_pp <- ppp(x= loc$Xloc, y= loc$Yloc, window = w)
  
  est <- as.data.frame(Kest(po_pp)) %>% select(-border)
  set.seed(333)
  EL = envelope(po_pp[w], Kest, nsim=sims) %>% data.frame() #variability of the point process under the null hypothesis of CSR
  
  return(list(est, EL))
}

Ripley_plot = function(ripley_data = NULL, estimator){
  est = ripley_data[[1]]
  EL = ripley_data[[2]]
  perm = ripley_data[[3]]
  
  if(estimator == "K"){
    #est <- as.data.frame(Kest(po_pp)) %>% select(-border)
    #set.seed(333)
    #EL = envelope(po_pp[w], Kest, nsim=sims)
    est2 <- est %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    est2$value = est2$value
    EL$lo = EL$lo
    EL$hi = EL$hi
    ylabel = "Ripley's K"
    perm2 = perm %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    
    new_dat = full_join(ripleys_list[[1]], ripleys_list[[3]]) %>% 
      gather("type", "value", -r)
    
  } else if(estimator=="L"){
    #est <- as.data.frame(Lest(po_pp)) %>% select(-border)
    #set.seed(333)
    #EL = envelope(po_pp[w], Lest, nsim=sims)
    est2 <- est %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    est2$value = sqrt(est2$value / pi) - est2$r
    EL$lo = sqrt(EL$lo / pi) - EL$r
    EL$hi = sqrt(EL$hi / pi) - EL$r
    ylabel = expression(paste(H^"*","(r) = L(r) - r"))
    perm2 = perm %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    perm2$value = sqrt(perm2$value / pi) - perm2$r
    # p = full_join(ripleys_list[[1]], ripleys_list[[3]]) %>%
    #   rowwise() %>%
    #   mutate_at(vars(-r), list(centered = ~ .-perm_trans)) %>% 
    #   select(-(theo:perm_iso)) %>% 
    #   gather("type", "value", -r) %>% 
    #   ggplot() + geom_line(aes(x = r, y = value, color = type)) +
    #   scale_color_manual(name = "Estimate", ############### NEW
    #                      labels = c("Theoretical CSR", "Permuted Isotropic CSR",
    #                                 "Permuted Translational CSR", "Observed Isotropic","Observed Translational"),
    #                      breaks = c("theo_centered", "perm_trans_centered", 
    #                                 "perm_iso_centered", "iso_centered", "trans_centered"),
    #                      values = c("theo_centered" = 'black', "perm_trans_centered" = "green",
    #                                 "perm_iso_centered" = "orange", "iso_centered" = 'red', 
    #                                 "trans_centered" = 'blue')) + 
    #   geom_ribbon(data = ripleys_list[[2]] %>%
    #                 full_join(ripleys_list[[3]]) %>%
    #                 mutate_at(vars(-r), list(centered = ~ .-perm_trans)) %>% 
    #                 select(-(obs:hi)),
    #               aes(x= r, ymin=lo_centered, ymax=hi_centered), inherit.aes=FALSE, alpha=0.4, color=NA) + 
    #   ylab(ylabel)
    
  } else {
    #est <- as.data.frame(Kest(po_pp)) %>% select(-border)
    #set.seed(333)
    #EL = envelope(po_pp[w], Kest, nsim=sims)
    est2 <- est %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    est2$value = est2$value / (pi * (est2$r)^2)
    EL$lo = EL$lo / (pi * (EL$r)^2)
    EL$hi = EL$hi / (pi * (EL$r)^2)
    ylabel = "Marcon's M"
    perm2 = perm %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    perm2$value = perm2$value / (pi * (perm2$r)^2)
  }
  
  p = ggplot() + geom_line(aes(x=r, y=value, color = type),bind_rows(est2, perm2)) + 
    #ggtitle(paste(n, " points; ", paste0(paste(names(sampleInfo), sampleInfo, sep=":")[-length(sampleInfo)], collapse = "; "), sep="")) +
    theme_classic(base_size = 20)
  #theme(legend.position="bottom") +
  #viridis::scale_color_viridis(option = colorscheme, discrete = TRUE, name = "Estimate", ############### NEW
  #                             labels = c("Observed Isotropic", "Theoretical CSR", 
  #                                        "Observed Translational"))
  #+ # Add base_size
  p = p + scale_color_manual(name = "Estimate", ############### NEW
                             labels = c("Theoretical CSR", "Permuted Isotropic CSR",
                                        "Permuted Translational CSR", "Observed Isotropic","Observed Translational"),
                             breaks = c("theo", "perm_trans", "perm_iso", "iso", "trans"),
                             values = c("theo" = 'black', "perm_trans" = "green",
                                        "perm_iso" = "orange", "iso" = 'red', 
                                        "trans" = 'blue')) + 
    geom_ribbon(data = EL, aes(x= r, ymin=lo, ymax=hi), inherit.aes=FALSE, alpha=0.4, color=NA) + 
    ylab(ylabel)
  if(estimator == "M"){
    #0/0 occurs when r is 0, and values for first few M are really high sometimes.
    p + ylim(0,max(EL$hi[-(1:10)],3))
  }else{
    p
  }
}

#Ripley(data = df, cell_type = "CD3..CD8.")
