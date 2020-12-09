rm(list = ls())
library(plotly)
library(tidyverse)
library(FNN)
library(RANN)
library(spatstat)
load('/Volumes/Lab_Fridley/IHC/Peres/data/TMA.clin.Rdata')

marker_names <- c(
  CD11 = 'CD11b (Opal 620) Positive',
  CD15 = 'CD15 (Opal 520) Positive',
  CD3 = 'CD3 (Opal 650) Positive',
  FOXP3 = 'FOXP3 (Opal 540) Positive',
  CD8_marker = 'CD8 (Opal 570) Positive'
)

mnames = marker_names[1]
data = tma.data[[4]]

sapply(tma.data, function(data){
X <- data %>% 
  mutate(
    xloc = (XMin + XMax) / 2,
    yloc = (YMin + YMax) / 2) %>%
  select(xloc, yloc, make.names(marker_names)) %>%
  mutate(num_pos = rowSums(select(.,!!make.names(marker_names))))
return(sum(X$num_pos>0))
})

#Get the cell labels 
X <- tma.data[[21]] %>% 
  mutate(
    xloc = (XMin + XMax) / 2,
    yloc = (YMin + YMax) / 2) %>%
  select(xloc, yloc, make.names(marker_names)) %>%
  mutate(num_pos = rowSums(select(.,!!make.names(marker_names))),
         keep = case_when(num_pos < 2 ~ 'Keep',
                          CD11b..Opal.620..Positive + CD15..Opal.520..Positive > 0 &
                            CD3..Opal.650..Positive + FOXP3..Opal.540..Positive + 
                            CD8..Opal.570..Positive ==0 ~ 'Keep',
                          CD3..Opal.650..Positive + FOXP3..Opal.540..Positive == 2 &
                            CD11b..Opal.620..Positive + CD15..Opal.520..Positive == 0 ~ 
                            'Keep',
                          CD3..Opal.650..Positive + CD8..Opal.570..Positive == 2 &
                            CD11b..Opal.620..Positive + CD15..Opal.520..Positive == 0 ~ 
                            'Keep')) %>%
  filter(keep == 'Keep') %>%
  mutate(cell_type = case_when(
    CD11b..Opal.620..Positive == TRUE & num_pos == 1 ~ 'CD11+',
    CD15..Opal.520..Positive == TRUE & num_pos == 1 ~ 'CD15+',
    CD3..Opal.650..Positive == TRUE & num_pos == 1 ~ 'CD3+',
    FOXP3..Opal.540..Positive  == TRUE & num_pos == 1 ~ 'FOXP3+',
    CD8..Opal.570..Positive  == TRUE & num_pos == 1 ~ 'CD8+',
    CD11b..Opal.620..Positive + CD15..Opal.520..Positive == 2 ~ 'Neutrophil',
    CD3..Opal.650..Positive + FOXP3..Opal.540..Positive == 2 ~ 'Treg',
    CD3..Opal.650..Positive + CD8..Opal.570..Positive == 2 ~ 'Cytotoxic',
    num_pos == 0 ~ 'None'
  ),
  cell_type = factor(cell_type, levels = c('CD11+', 'CD15+','CD3+','FOXP3+',
                                           'CD8+','Neutrophil', 'Treg', 'Cytotoxic', 'None'))) %>%
  filter(cell_type != 'None')


#Get nearest neighbors of all positive cells
nn = get.knn(data = X %>% select(xloc, yloc) , k = 1)
nn_dist = nn$nn.dist
nn_cell_type = X$cell_type[nn$nn.index]

#Get number of nearest neighbors within a radius
#This will take time as X and r grow
num_nn = nn2(data = X %>% select(xloc, yloc), k = nrow(X),
             radius = 25, searchtype = 'radius')
num_close = apply(num_nn$nn.idx, 1, function(a) sum(a!=0))

X = cbind.data.frame(X, nn_dist = nn_dist, nn_cell_type = nn_cell_type,
                     num_close = num_close)

K = function(type, radius){
  tmp = X %>% filter(cell_type == type) 
  w = convexhull.xy(x = X$xloc,y = X$yloc)
  pp = ppp(x = tmp$xloc, y = tmp$yloc,window = w)
  K = Kest(X = pp, correction = 'isotropic',
           r = c(0,radius))
  out = data.frame(K) %>% filter(r == radius) %>%
    mutate(K_diff = iso-theo)
  return(out)
}

L = function(type, radius){
  tmp = X %>% filter(cell_type == type) 
  w = convexhull.xy(x = X$xloc,y = X$yloc)
  pp = ppp(x = tmp$xloc, y = tmp$yloc, window = w)
  L = Lest(X = pp, correction = 'isotropic',
           r = c(0,radius))
  out = data.frame(L) %>% filter(r == radius) %>%
    mutate(L_diff = iso-theo)
  return(out)
}


rip_k = setNames(levels(X$cell_type),levels(X$cell_type)) %>% 
  map_df(.f = K, radius = 25, .id = 'Cell_type')
bes_l = setNames(levels(X$cell_type),levels(X$cell_type)) %>% 
  map_df(.f = L, radius = 25, .id = 'Cell_type')

X$K = round(rip_k$K_diff[X$cell_type],3)
X$L = round(bes_l$L_diff[X$cell_type],3)

fig <- plot_ly(data = X, x = ~xloc, y = ~yloc, color = ~cell_type,
               text = X$cell_type,
               hovertemplate = ~paste('</br> Type: ', cell_type,
                                      '</br> NN Type: ', nn_cell_type,
                                      '</br> Num_Ns within r: ', num_close,
                                      "</br> Ripley's: ", K,
                                      "</br> Besag's L: ", L)
               )

fig




