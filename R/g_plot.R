#alex
#permute CSR

Permute_positives_g = function(data, g_list, cell_type, sims = 100){
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
  rs = g_list[[1]]$r
  
  cat("Permuting NN G() CSR.....\n")
  perms = purrr::map_df(.x = 1:nrow(grid), 
                        ~{
                          cat(paste0(.x, ", "))
                          data_new = data2 %>%
                            dplyr::mutate(Positive = sample(Positive, nrow(data2), replace = F))
                          data_pos = data_new %>%
                            dplyr::filter(Positive == 1)
                          G_obs = spatstat.geom::ppp(x = data_pos$xloc,
                                                     y = data_pos$yloc,
                                                     window = win) %>%
                            spatstat.core::Gest(r=rs) %>%
                            data.frame() %>%
                            #dplyr::select(-border) %>%
                            dplyr::mutate(iter = .x)
                          return(G_obs)
                        })
  
  g_list$perm = perms %>%
    dplyr::group_by(r) %>%
    summarise(perm_han = mean(han), perm_rs = mean(rs))
  return(g_list)
}

#Chris
# This function will compuite the nearest neighbor distrubtion for a specific markers,
# and produce an envelope analougously to what was done for G. 

NN_G <- function(data, cell_type, alpha=0.05, sims = 100)
{
  location2 <- data %>% mutate(Xloc = (XMin + XMax)/2, 
                               Yloc = (YMin + YMax)/2)
  loc <- location2 %>% select(c(Xloc, Yloc, cell_type)) %>% filter(.[[cell_type]] == 1)
  n = nrow(loc)
  w <- convexhull.xy(x = loc$Xloc, y = loc$Yloc)
  po_pp <- ppp(x= loc$Xloc, y= loc$Yloc, window = w)
  
  est <- as.data.frame(Gest(po_pp)) %>% select(-c(km,hazard,theohaz))
  set.seed(333)
  EL = envelope(po_pp[w], Gest, nsim=sims) %>% data.frame()
  
  return(list(est, EL))
}

G_plot = function(G_data = NULL){
  est = G_data[[1]] %>%
    pivot_longer(cols = 2:ncol(.), names_to = 'type')
  EL = G_data[[2]]
  perm = G_data[[3]] %>%
    pivot_longer(cols = 2:ncol(.), names_to = "type")
  
  p = ggplot() + geom_line(aes(x=r, y=value, color = type), bind_rows(est, perm)) + 
    #ggtitle(paste(n, " points; ", paste0(paste(names(sampleInfo), sampleInfo, sep=":")[-length(sampleInfo)], collapse = "; "), sep="")) +
    theme_classic(base_size = 20)
  #theme(legend.position="bottom") +
  #viridis::scale_color_viridis(option = colorscheme, discrete = TRUE, name = "Estimate", ############### NEW
  #                             labels = c("Observed Isotropic", "Theoretical CSR", 
  #                                        "Observed Translational"))
  #+ # Add base_size
  
  p = p + scale_color_manual(name = "Estimate", ############### NEW
                             labels = c("Theoretical CSR", "Permuted Hanisch CSR", "Permuted RS CSR",
                                        "Observed Hanisch","Observed Reduced Sampled"),
                             breaks = c("theo", "han", "perm_han", "perm_rs", "rs"),
                             values = c("theo" = 'black', "perm_han" = "green", 
                                        "perm_rs" = "orange", "han" = 'red', 
                                        "rs" = 'blue')) + 
    geom_ribbon(data = EL, aes(x= r, ymin=lo, ymax=hi), inherit.aes=FALSE, alpha=0.4, color=NA) + 
    ylab('G(r)')
    p
  }

#Testing
# data = read.csv("example_data/deidentified_spatial.csv", check.names = FALSE)
# cell_type = 'CD3..Opal.570..Positive'
# G = NN_G(data = data, cell_type = cell_type)
# K = Ripley(data = data, cell_type = cell_type)
# ggpubr::ggarrange(plotlist = list(G_plot(G_data = G),
#                                  Ripley_plot(ripley_data = K, 
#                                              estimator = 'M')), 
#                   legend = 'bottom')
 
 