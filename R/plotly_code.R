#Call libraries
library(tidyverse)
library(spatstat)
library(plotly)
library(ggpubr)
library(ggplot2)
library(vioplot)
library(ggfortify)
library(pheatmap)
library(RColorBrewer)
library(stringr)


library(viridis)
load("~/iTIME/data/example_data.RData")


########################
scatter_plotly = function(data = data, markers = markers, new_names = new_names){
data$x <- (data$XMin + data$XMax) / 2
data$y <- (data$YMin + data$YMax) / 2
num_cells = c()
for(a in 1:length(markers)){
  num_cells = append(num_cells,sum(data[[markers[a]]]==1))
  data$marks[data[[markers[a]]]==1] = new_names[a]
}

data$marks[is.na(data$marks)]  = 'Negative' 
data$marks = as.factor(data$marks)
w <- convexhull.xy(data.frame(data$x,data$y))

ppp <- ppp(data$x, data$y, window = w, marks = data$marks)
nn1 <- nndist(ppp, by=marks(ppp))
data$nn1 <- round(nn1,1)

data$text = NA
for(i in 1:nrow(data)){
text = rep(length(levels(data$marks)))
for (a in 1:length(levels(data$marks))) {
  text[a] = c(paste(c(
    paste("\\<br\\> ", levels(data$marks)[a], ": ",
          data$nn1[i, a])
  )))
}
text = paste(text,collapse = '')
text = substr(text,1,nchar(text)-1)
text = gsub("\\\\", '', text)
text = paste('NN Distances:',text)
data$text[i] = text
}

data$marks = factor(data$marks, 
                    levels = c('Negative', 
                               levels(data$marks)[levels(data$marks)!='Negative']))

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)



pal<-c(Negative = 'grey',#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080',
         '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', 'grey80', '#000000',
         brewer.pal(n = length(levels(data$marks)), name = 'Set3')[c(4,5,10)],"darkgreen",brewer.pal(n = 10, name = 'Paired')[10])

pal = viridis_pal(option = "D")(6)

plot = plot_ly() %>%
  add_trace(data = data[data$marks == 'Negative',], x = ~x, y = ~y, 
            type="scatter",
            mode="markers",
            symbol = ~Classifier.Label,
            symbols = c('3', 'o'),
            marker=list(size=3,
                        color = 'lightgrey'),
            hovertemplate = ~text) %>%
  add_trace(data = data[data$marks != 'Negative',], x = ~x, y = ~y, 
            type="scatter",
            mode="markers",
            color = ~marks,
            colors = pal,
            marker=list(size=3,
                        symbol = '200'),
            hovertemplate = ~text) %>%
  layout(xaxis = ax, yaxis = ax)

return(plot)
}

markers = colnames(spatial[[1]])[11:17]
new_names = new_names
scatter_plotly(spatial[[8]], markers, new_names)
