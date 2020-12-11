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

ggplot(data=data, aes(x=x, y=y, shape=Classifier.Label, color=marks)) + geom_point() + 
  theme_bw() + theme(axis.ticks = element_blank(), axis.text = element_blank(),
                     axis.title = element_blank())
ggplotly()

##########################

data$marks[data$CD3..FOXP3.==1]<- "CD3+/FOXP3+" 
data$marks[data$CD3..CD8.==1]<- "CD3+/CD8+"
data$marks[data$CD3..CD8..FOXP3.==1]<- "CD3+/CD8+/FOXP3+"
data$marks[data$CD3..PD1.==1]<- "CD3+/PD1+"
data$marks[data$CD3..PD.L1.==1]<- "CD3+/PDL1+"
data$marks[data$CD8..PD1.==1]<- "CD3+/CD8+/PD1+"
data$marks[data$CD3..CD8..PD.L1.==1]<- "CD3+/CD8+/PDL1+"

w <- convexhull.xy(data.frame(data$x,data$y))

ppp <- ppp(data$x, data$y, window = w, marks = data$marks_chris)

nn1 <- nndist(ppp,by=marks(ppp))
nn1<-round(nn1,1)
plot_df <- cbind(data,nn1)

Plotly <- plot_ly(data = plot_df, x = ~plot_df$x, y = ~plot_df$y, 
                  type="scatter",
                  mode="markers",
                  color = ~plot_df$marks, 
                  colors="Paired",
                  marker=list(size=3),
                  text= ~plot_df$marks,
                  symbol = ~ Classifier.Label,
                  hovertemplate = ~paste(
                    "<b>%{text} Cell </b><br>",
                    "NN Distances:",
                    "<br>    CD3+FOXP3+:",plot_df$`CD3+/FOXP3+`,
                    "<br>    CD3+CD8+:",plot_df$`CD3+/CD8+`,
                    "<br>    CD3+FOXP3+:",plot_df$`CD3+/CD8+/FOXP3+`,
                    "<br>    CD3+PD1+:",plot_df$`CD3+/PD1+`,
                    "<br>    CD3+PDL1+:",plot_df$`CD3+/PDL1+`,
                    "<br>    CD3+CD8+PD1:",plot_df$`CD3+/CD8+/PD1`,
                    "<br>    CD3+CD8+PDL1:",plot_df$`CD3+/CD8+/PDL1`
                  )
)

Plotly <- Plotly %>% layout(xaxis = ax, yaxis = ax)
Plotly

