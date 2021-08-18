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
  
  p = ggplot() + geom_line(aes(x=r, y=value, color = type), est) + 
    #ggtitle(paste(n, " points; ", paste0(paste(names(sampleInfo), sampleInfo, sep=":")[-length(sampleInfo)], collapse = "; "), sep="")) +
    theme_classic(base_size = 20)
  #theme(legend.position="bottom") +
  #viridis::scale_color_viridis(option = colorscheme, discrete = TRUE, name = "Estimate", ############### NEW
  #                             labels = c("Observed Isotropic", "Theoretical CSR", 
  #                                        "Observed Translational"))
  #+ # Add base_size
  
  p = p + scale_color_manual(name = "Estimate", ############### NEW
                             labels = c("Theoretical CSR", "Observed Hanisch","Observed Reduced Sampled"),
                             breaks = c("theo", "han", "rs"),
                             values = c("theo" = 'black', "han" = 'red', 
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
 
 