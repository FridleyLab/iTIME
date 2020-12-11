#
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
  
  
  plot = plot_ly() %>%
    add_trace(data = data[data$marks == 'Negative',], x = ~x, y = ~y, 
              type="scatter",
              mode="markers",
              symbol = ~Classifier.Label,
              symbols = c('3', 'o'),
              legendgroup="Classifier Label",
              marker=list(size=3,
                          color = 'lightgrey'),
              hovertemplate = ~text) %>%
    add_trace(data = data[data$marks != 'Negative',], x = ~x, y = ~y, 
              type="scatter",
              mode="markers",
              color = ~marks,
              colors = "Set1",
              legendgroup="marks",
              marker=list(size=3,
                          symbol = '200'),
              hovertemplate = ~text) %>%
    layout(xaxis = ax, yaxis = ax)%>%
    add_annotations( text="Tumor/Stroma Status:", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.9, yanchor="bottom",   
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    add_annotations( text="Marker Type:", xref="paper", yref="paper",
                     x=1.02, xanchor="left",
                     y=0.7, yanchor="bottom",   
                     legendtitle=TRUE, showarrow=FALSE ) %>%
    layout(legend=list(tracegroupgap =30, y=0.9, yanchor="top"))
  
  
  return(plot)
}

