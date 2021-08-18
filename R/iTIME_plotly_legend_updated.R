#nicks function
#editing plotting format
spatial_plotly = function(data = data, markers = NULL){
  data$x <- (data$XMin + data$XMax) / 2
  data$y <- (data$YMin + data$YMax) / 2
  if(is.null(markers)){
    data$marks = NA
  } else {
    for(a in 1:length(markers)){
      data$marks[data[[markers[a]]]==1] = markers[a]
    }
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
  
  
  plot = plot_ly() %>% #
    add_trace(data = data[data$marks == 'Negative',], x = ~x, y = ~y, 
              type="scatter",
              mode="markers",
              symbol = ~Classifier.Label,
              symbols = c('3', 'circle-open'),
              legendgroup="Classifier Label",
              marker=list(size=3,
                          color = 'black'),
              hovertemplate = ~text) %>%
    add_trace(data = data[data$marks != 'Negative',], x = ~x, y = ~y, 
              type="scatter",
              mode="markers",
              color = ~marks,
              legendgroup="marks",
              marker=list(size=3,
                          symbol = 'circle'),
              hovertemplate = ~text) %>%
    layout(xaxis = ax, yaxis = ax)%>%
    add_annotations( text="Tumor/Stroma Status:", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.89, yanchor="bottom",   
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    add_annotations( text="Marker Type:", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.75, yanchor="bottom",   
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    layout(legend=list(tracegroupgap =30, y=0.9, yanchor="top"))
  
  
  return(plot)
}

