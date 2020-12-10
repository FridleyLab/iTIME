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

load("/Users/nickandbianca/Desktop/iTIME/data/example_data.RData")
setwd("/Users/nickandbianca/Desktop/iTIME")

dir.create(str_c(getwd(),"/", "plot prototypes"))


ROI_GRAPHS <- function(i){
  
  spatial[[i]]$x <- (spatial[[i]]$XMin + spatial[[i]]$XMax) / 2
  spatial[[i]]$y <- (spatial[[i]]$YMin + spatial[[i]]$YMax) / 2
  spatial[[i]]$marks <- spatial[[i]]$Classifier.Label
  spatial[[i]]$marks[spatial[[i]]$CD3..FOXP3.==1]<- "CD3+/FOXP3+"
  spatial[[i]]$marks[spatial[[i]]$CD3..CD8.==1]<- "CD3+/CD8+"
  spatial[[i]]$marks[spatial[[i]]$CD3..CD8..FOXP3.==1]<- "CD3+/CD8+/FOXP3+"
  spatial[[i]]$marks[spatial[[i]]$CD3..PD1.==1]<- "CD3+/PD1+"
  spatial[[i]]$marks[spatial[[i]]$CD3..PD.L1.==1]<- "CD3+/PDL1+"
  spatial[[i]]$marks[spatial[[i]]$CD8..PD1.==1]<- "CD3+/CD8+/PD1+"
  spatial[[i]]$marks[spatial[[i]]$CD3..CD8..PD.L1.==1]<- "CD3+/CD8+/PDL1+"
  spatial[[i]]$marks <- factor(spatial[[i]]$marks,levels=c("Tumor","Stroma","CD3+/FOXP3+","CD3+/CD8+","CD3+/CD8+/FOXP3+","CD3+/PD1+","CD3+/PDL1+","CD3+/CD8+/PD1+","CD3+/CD8+/PDL1+"))
  
  Plot <- ggplot(spatial[[i]]) +
    geom_point(aes(spatial[[i]]$x, spatial[[i]]$y, color=spatial[[i]]$marks),size=1,shape=16) +
    coord_fixed()+
    scale_color_brewer(palette = "Paired")+
    theme_light()+
    guides(colour = guide_legend(override.aes = list(size=6)))+
    theme(panel.grid=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(), legend.position = "bottom", legend.title=element_blank(),legend.text=element_text(size=12))
  plotfile <- str_c(getwd(),"/", "plot prototypes", "/", spatial[[i]]$image.tag, "_Plot.png")
  png(file=plotfile)
  plot(Plot)
  dev.off()
}

K_ANALYSIS <- function(i){
  
spatial[[i]]$x <- (spatial[[i]]$XMin + spatial[[i]]$XMax) / 2
spatial[[i]]$y <- (spatial[[i]]$YMin + spatial[[i]]$YMax) / 2
spatial[[i]]$marks <- spatial[[i]]$Classifier.Label
spatial[[i]]$marks[spatial[[i]]$CD3..FOXP3.==1]<- "CD3+/FOXP3+"
spatial[[i]]$marks[spatial[[i]]$CD3..CD8.==1]<- "CD3+/CD8+"
spatial[[i]]$marks[spatial[[i]]$CD3..CD8..FOXP3.==1]<- "CD3+/CD8+/FOXP3+"
spatial[[i]]$marks[spatial[[i]]$CD3..PD1.==1]<- "CD3+/PD1+"
spatial[[i]]$marks[spatial[[i]]$CD3..PD.L1.==1]<- "CD3+/PDL1+"
spatial[[i]]$marks[spatial[[i]]$CD8..PD1.==1]<- "CD3+/CD8+/PD1+"
spatial[[i]]$marks[spatial[[i]]$CD3..CD8..PD.L1.==1]<- "CD3+/CD8+/PDL1+"
spatial[[i]]$marks <- factor(spatial[[i]]$marks,levels=c("Tumor","Stroma","CD3+/FOXP3+","CD3+/CD8+","CD3+/CD8+/FOXP3+","CD3+/PD1+","CD3+/PDL1+","CD3+/CD8+/PD1+","CD3+/CD8+/PDL1+"))

pppROI <- ppp(spatial[[i]]$x, spatial[[i]]$y, window = owin(c(min(spatial[[i]]$x), max(spatial[[i]]$x)), c(min(spatial[[i]]$y), max(spatial[[i]]$y)), marks = spatial[[i]]$marks))

}

for(i in 1:length(spatial)){ROI_GRAPHS(i)}
for(i in 1:length(spatial)){K_ANALYSIS(i)}
