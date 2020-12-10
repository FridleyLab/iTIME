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

  K_CD3_FOX <- Kest(ppp, spatial[[3]]$marks=='CD3+/FOXP3+',correction=c("isotropic","translate"))
  
  plot(K_CD3_FOX,.~r)
  plot(K_CD3_FOX,(sqrt(./pi))~r)
  plot(K_CD3_FOX,(sqrt(./pi)-r)~r)
  plot(K_CD3_FOX,./(pi*r^2) ~ r)

  #add confidence band
  
  attach(K_CD3_FOX)
  AUCtheo <- sum(diff(r)*(head(sqrt(theo/(pi-r)),-1)),na.rm=TRUE)
  AUCiso <- sum(diff(r)*(head(sqrt(iso/(pi-r)),-1)),na.rm=TRUE)
  AUCdiff<- AUCiso-AUCtheo

#PLOTLY 
  
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

nn1 <- nndist(ppp,by=marks(ppp))

nn1<-round(nn1,1)

plot_df <- cbind(spatial[[3]],nn1)

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

Plotly <- plot_ly(data = plot_df, x = ~plot_df$x, y = ~plot_df$y, 
                  type="scatter",
                  mode="markers",
                  color = ~plot_df$marks, 
                  colors="Paired",
                  marker=list(size=3),
                  text= ~plot_df$marks,
                  hovertemplate = ~paste(
                    "<b>%{text} Cell </b><br>",
                    "NN Distances:",
                    "<br>    Tumor:", plot_df$Tumor,
                    "<br>    Stroma:",plot_df$Stroma,
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