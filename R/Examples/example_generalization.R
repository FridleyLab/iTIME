library(spatstat)
library(tidyverse)

data = spatial
data$xloc = (data$XMax + data$XMin)/2
data$yloc = (data$YMax + data$YMin)/2

#clean columns

#User defined information
#Clean names
summary = janitor::clean_names(summary) %>% data.frame()
clinical = janitor::clean_names(clinical) %>% data.frame()

#What column are the x-locations?
x = 'xloc'
#What column are the y-locations?
y = 'yloc'
#What are the markers of interest?
cell_types = c('Treg' = "CD3 FOXP3 Cells", 
               'Cytoxic' = "CD3 CD8 Cells")
clean_cell_types = setNames(janitor::make_clean_names(cell_types), names(cell_types))

#Choose radius for K and L
r = 50

#What is you clinical variable of interest?
clin_var = c('Race' = 'race', 'HIV Status' = 'hiv_status')


#Should user provide clinical and summary info already merged?
#Especially in this case when the there no common column names
clin_summ = inner_join(clinical,summary,by.x = 'image_tag')

for(a in 1:length(clin_var)){
clin_plot = ggplot(data = clin_summ, aes_string(x = unname(clin_var[a]), y = unname(clean_cell_types[1]), 
                                    color = unname(clin_var[a]))) + 
  geom_boxplot()
print(clin_plot)
}


