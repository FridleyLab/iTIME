location <- do.call(rbind.data.frame, spatial)

df <- location %>% 
  filter(image.tag == "Coghill_P2_Anal-Invasive-TMA1_[1,C].tif")

Ripley <- function(data, cell_type)
{
  location2 <- data %>% mutate(Xloc = (XMin + XMax)/2, Yloc = (YMin + YMax)/2)
  loc <- location2 %>% select(c(Xloc, Yloc, cell_type)) %>% filter(.[[cell_type]] == 1)
  w <- convexhull.xy(x = loc$Xloc, y = loc$Yloc)
  po_pp <- ppp(x= loc$Xloc, y= loc$Yloc, window = w)
  K <- as.data.frame(Kest(po_pp)) %>% select(-border)
  K2 <- K %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "Ripley's K")
  p <- ggplot(K2,
              aes(x= r, y= `Ripley's K`, color = type))
  p + geom_line() +
    theme_bw(base_size = 20) + # Add base_size
    scale_color_discrete(name = "Estimate", ############### NEW
                          labels = c("Observed \nIsotropic", "Theoretical \nCSR", "Observed \nTranslate"))
}

Ripley(data = df, cell_type = "CD3..CD8.")



