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
dir.create(str_c(getwd(),"/", "K(r) plots"))
dir.create(str_c(getwd(),"/", "L(r) plots"))
dir.create(str_c(getwd(),"/", "L(r) - r plots"))
dir.create(str_c(getwd(),"/", "K(r)/pir2 plots"))

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

w <- convexhull.xy(data.frame(spatial[[3]]$x,spatial[[3]]$y))

ppp <- ppp(spatial[[i]]$x, spatial[[i]]$y, window = w, marks = spatial[[i]]$marks)

if(sum(spatial[[i]]$marks=="CD3+/FOXP3+")>1) {
  KTumor <- Kmulti(ppp, spatial[[i]]$marks=='Tumor',spatial[[i]]$marks=='Tumor', correction=c("isotropic","translate"))
  
  KTumorfile <- str_c(getwd(),"/", "_KGraphs","/","_Tumor", "/", filenamelist[i], "_Tumor_KGraph.png")
  png(file=KTumorfile)
  plot(KTumor,./(pi*r^2) ~ r)
  dev.off()
  
  attach(KTumor)
  
  AUCtheo <- sum(diff(r)*(head(theo/(pi*(r^2)),-1)),na.rm=TRUE)
  AUCiso <- sum(diff(r)*(head(iso/(pi*(r^2)),-1)),na.rm=TRUE)
  AUCdiffTumor<- AUCiso-AUCtheo
  summarydf[[i,2]] <<- AUCiso-AUCtheo
  KTumor <- as.data.frame(KTumor)
  summarydf[[i,3]] <<- KTumor[103,3]/(pi*(25^2))
  summarydf[[i,4]] <<- KTumor[306,3]/(pi*(75^2))
  
  detach(KTumor)
}

}

for(i in 1:length(spatial)){ROI_GRAPHS(i)}
for(i in 1:length(spatial)){K_ANALYSIS(i)}


##Playground##

if(sum(spatial[[3]]$marks=="CD3+/FOXP3+")>1) {
  K_CD3_FOX <- Kest(ppp, spatial[[3]]$marks=='CD3+/FOXP3+',correction=c("isotropic","translate"))
  
  KTumorfile <- str_c(getwd(),"/", "_KGraphs","/","_Tumor", "/", filenamelist[i], "_Tumor_KGraph.png")
  png(file=KTumorfile)
  
  plot(K_CD3_FOX,(sqrt(./pi))~r)
  plot(K_CD3_FOX,(sqrt(./pi)-r)~r)
  plot(K_CD3_FOX,./(pi*r^2) ~ r)
  dev.off()
  
  attach(KTumor)
  
  AUCtheo <- sum(diff(r)*(head(theo/(pi*(r^2)),-1)),na.rm=TRUE)
  AUCiso <- sum(diff(r)*(head(iso/(pi*(r^2)),-1)),na.rm=TRUE)
  AUCdiffTumor<- AUCiso-AUCtheo
  summarydf[[i,2]] <<- AUCiso-AUCtheo
  KTumor <- as.data.frame(KTumor)
  summarydf[[i,3]] <<- KTumor[103,3]/(pi*(25^2))
  summarydf[[i,4]] <<- KTumor[306,3]/(pi*(75^2))
  
  detach(KTumor)
}

}

spatial[[3]]$x <- (spatial[[3]]$XMin + spatial[[3]]$XMax) / 2
spatial[[3]]$y <- (spatial[[3]]$YMin + spatial[[3]]$YMax) / 2
spatial[[3]]$marks <- spatial[[3]]$Classifier.Label
spatial[[3]]$marks[spatial[[3]]$CD3..FOXP3.==1]<- "CD3+/FOXP3+" 
spatial[[3]]$marks[spatial[[3]]$CD3..CD8.==1]<- "CD3+/CD8+"
spatial[[3]]$marks[spatial[[3]]$CD3..CD8..FOXP3.==1]<- "CD3+/CD8+/FOXP3+"
spatial[[3]]$marks[spatial[[3]]$CD3..PD1.==1]<- "CD3+/PD1+"
spatial[[3]]$marks[spatial[[3]]$CD3..PD.L1.==1]<- "CD3+/PDL1+"
spatial[[3]]$marks[spatial[[3]]$CD8..PD1.==1]<- "CD3+/CD8+/PD1+"
spatial[[3]]$marks[spatial[[3]]$CD3..CD8..PD.L1.==1]<- "CD3+/CD8+/PDL1+"
spatial[[3]]$marks <- factor(spatial[[3]]$marks,levels=c("Tumor","Stroma","CD3+/FOXP3+","CD3+/CD8+","CD3+/CD8+/FOXP3+","CD3+/PD1+","CD3+/PDL1+","CD3+/CD8+/PD1+","CD3+/CD8+/PDL1+"))

w <- convexhull.xy(data.frame(spatial[[3]]$x,spatial[[3]]$y))

ppp <- ppp(spatial[[3]]$x, spatial[[3]]$y, window = w, marks = spatial[[3]]$marks)

nn <- nndist(ppp)
plot(ppp %mark% nndist(ppp),markscale=1)

nn1 <- nndist(ppp,by=marks(ppp))

minnn <- minnndist(ppp,by=marks(ppp))

m <- nnwhich(ppp,by=marks(ppp))

b <- ppp[m]
arrows(ppp$x, ppp$y, b$x, b$y, angle=15, length=0.15, col="red")