data = spatial[[2]] %>% select(XMin, YMin, CD3..CD8.) %>%
  filter(CD3..CD8. == 1)
w = convexhull.xy(data$XMin,data$YMin)
point_proc = ppp(x = data$XMin, y = data$YMin, window=w) 
K  = Kest(point_proc)
plot(K)