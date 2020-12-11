#christelle

Ripley <- function(data, cell_type, estimator, alpha=0.05, sims = 100)
{
  location2 <- data %>% mutate(Xloc = (XMin + XMax)/2, Yloc = (YMin + YMax)/2)
  loc <- location2 %>% select(c(Xloc, Yloc, cell_type)) %>% filter(.[[cell_type]] == 1)
  n = nrow(loc)
  w <- convexhull.xy(x = loc$Xloc, y = loc$Yloc)
  po_pp <- ppp(x= loc$Xloc, y= loc$Yloc, window = w)
  if(estimator == "K"){
    est <- as.data.frame(Kest(po_pp)) %>% select(-border)
    set.seed(333)
    EL = envelope(po_pp[w], Kest, nsim=sims)
    est2 <- est %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    
    
  } else if(estimator == "L") {
    est <- as.data.frame(Lest(po_pp)) %>% select(-border)
    set.seed(333)
    EL = envelope(po_pp[w], Lest, nsim=sims, rank = (alpha*(n+1)))
    est2 <- est %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    
  } else {
    est <- as.data.frame(Lest(po_pp)) %>% select(-border)
    set.seed(333)
    EL = envelope(po_pp[w], Lest, nsim=sims)
    est2 <- est %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")
    est2$value = est2$value - est2$r
    EL$lo = EL$lo - EL$r
    EL$hi = EL$hi - EL$r
    
  }
  ggplot() + geom_line(aes(x=r, y=value, color = type),est2) +
    geom_ribbon(data = EL, aes(x=r, ymin=lo, ymax=hi), inherit.aes=FALSE, alpha=0.4, color=NA) +
    theme_bw()
}

#Ripley(data = df, cell_type = "CD3..CD8.")
