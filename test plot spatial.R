# Import packages

library(tidyverse)
library(spatstat)

# Data cleaning

location <- do.call(rbind.data.frame, spatial)

colnames(location)
table(location$Analysis.Inputs)


location1 <- location %>% 
  mutate(Xloc = (XMin + XMax)/2, Yloc = (YMin + YMax)/2) %>% 
  group_by(image.tag) %>% 
  mutate(id = cur_group_id()) %>% 


plot(location1$Xloc, location1$Yloc)
colnames(location1)

location1 %>% ggplot() + 
  geom_point(aes(Xloc, Yloc))+
  facet_wrap(. ~ image.tag)

location2 <- location1 %>% 
  mutate(CD3..FOXP3. = ifelse(CD3..FOXP3. == 1, "CD3+FOXP3+", NA_character_)) %>% 
  mutate(CD3..CD8. = ifelse(CD3..CD8. == 1, "CD3+CD8+", NA_character_)) %>% 
  mutate(CD3..CD8..FOXP3. = ifelse(CD3..CD8..FOXP3. == 1, "CD3+CD8+FOXP3+", NA_character_)) %>% 
  mutate(CD3..PD1. = ifelse(CD3..PD1. == 1, "CD3+PD1+", NA_character_)) %>% 
  mutate(CD3..PD.L1. = ifelse(CD3..PD.L1. == 1, "CD3+PD.L1+", NA_character_)) %>% 
  mutate(CD8..PD1. = ifelse(CD8..PD1. == 1, "CD8+PD1+", NA_character_)) %>% 
  mutate(CD3..CD8..PD.L1. = ifelse(CD3..CD8..PD.L1. == 1, "CD3+CD8+PD.L1+", NA_character_)) %>% 
  unite(color, c("CD3..FOXP3.":"CD3..CD8..PD.L1."), na.rm = TRUE, remove = FALSE) %>% 
  mutate(color = case_when(
    color == ""          ~ NA_character_,
    !is.na(color)         ~ color
  )) %>%
  mutate(cell = case_when(
    is.na(color)          ~ "cell",
    !is.na(color)         ~ NA_character_
  )) %>%
  # mutate(color = case_when(
  #   color == ""          ~ "cell",
  #   !is.na(color)         ~ color
  #   )) %>%
  select(c("image.tag", "color", everything()))


location2 %>% ggplot() +
  geom_point(aes(Xloc, Yloc, color = color), size = .5)+
  facet_wrap(. ~ image.tag) +
  scale_color_manual(name = "Cells",
                     values = c("cell" = "grey"))




# Plotting K ----
cell_type <- "CD3..CD8."

loc <- location1 %>% filter(id == 2) %>% select(c(Xloc, Yloc, cell_type))
loc <- loc %>% filter(.[[cell_type]] == 1)

w <- convexhull.xy(x = loc$Xloc, y = loc$Yloc)
po_pp <- ppp(x= loc$Xloc, y= loc$Yloc, window = w)
K <- as.data.frame(Kest(po_pp))

K %>% 
  ggplot(aes(x= r)) +
  # geom_line(aes(y = border), color = "green")+
  geom_line(aes(y = trans), color = "red") +
  geom_line(aes(y = iso), color = "black") +
  geom_line(aes(y = theo), color = "blue") +
  theme_bw()

K <- as.data.frame(K)
K2 <- K %>% pivot_longer(2:ncol(.), names_to = "type", values_to = "value")


K2 %>% 
  ggplot(aes(x= r, y= value, color = type)) +
  geom_line() +
  theme_bw()

testplot <- function(data)
{
  p <- ggplot(data, 
              aes(x= r, y= value, color = type))
  p + geom_line() +
    theme_bw()
}
testplot(data = K2)


