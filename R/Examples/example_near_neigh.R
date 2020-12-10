Type1 = 'CD3..FOXP3.'
Type2 = 'CD3..CD8.'

#Filtered to only the CD3..FOXP3. and CD3..CD8., then making another column 
#indicating what cell's type
data = spatial[[2]] %>% filter(.[[Type1]] == 1 | .[[Type2]] == 1) %>%
  mutate(Final_Type = case_when(.[[Type1]] == 1 ~ 'Type1',
                                .[[Type2]] == 1 ~ 'Type2')) %>%
  select(XMin,YMin,Final_Type)

w = convexhull.xy(data$XMin,data$YMin)
point_proc = ppp(x = data$XMin, y = data$YMin, window=w) 

#Obtain nearest neighbor index and nearest neighbor distance for each cell 
nn_dist = nndist(point_proc) # Finds the distance to the nearest neoghbor
nn_idx = nnwhich(point_proc) # Finds the index (row number on data) to the nearest neoghbor
nn_type = data$Final_Type[nn_idx] #Finds thw type of the nearest neoghbor

#Final summary of nearest neighbor
final_nn_data = data.frame(Cell_Type = data$Final_Type, nn_type = nn_type,
                           nn_dist = nn_dist)

