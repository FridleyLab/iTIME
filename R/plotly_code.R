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

load("~/iTIME/data/example_data.RData")

data = spatial[[3]]
markers = c("CD3..FOXP3." , "CD3..CD8.", "CD3..CD8..FOXP3.", 
  "CD3..PD1.", "CD3..CD8..PD1.", "CD3..CD8..PDL1.")
new_names = markers
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

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)


plot = plot_ly(data = data, x = ~x, y = ~y, 
        type="scatter",
        mode="markers",
        color = ~marks,
        symbol = ~Classifier.Label,
        marker=list(size=3),
        hovertemplate = ~text
        ) %>%
  layout(xaxis = ax, yaxis = ax)

return(plot)
}

scatter_plotly(spatial[[1]], markers, new_names)
