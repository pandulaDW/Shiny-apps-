library(plotly)

# Function to create the main equity graph
main_equity_graph <- function(df){
  
  p <- plot_ly()
  
  for (i in 2:ncol(df)){
    
    x = c(1:nrow(df))
    y = colnames(df)[i]
    p <- add_trace(p,
                   x = x,
                   y = df[[y]],
                   type = 'scatter',
                   mode = 'lines',
                   name = y)
  }
  
  p <- p %>% layout(title='Main Equity Graph')
  
 return(p)
}

# Single Equity Graph 

single_cluster_graph = function(MtoM, cumsum, max_cum, lower_band, dd, n){
  p <- plot_ly() 
  p <- add_trace(p = p, data = MtoM, x = c(1:(nrow(cumsum))), y = ~as.numeric(MtoM[n,-1]),
                 type = 'bar', name = 'MtoM')
  
  p <- add_trace(p = p, data = cumsum, x = c(1:nrow(cumsum)), y = ~cumsum[[n+1]],
                 type='scatter', mode = 'lines', name = 'Ecurve')
  
  p <- add_trace(p = p, data = max_cum, x = c(1:nrow(cumsum)), y = ~max_cum[[n+1]],
                 type = 'scatter', mode = 'lines', name = 'Ecurve_High')
  
  p <- add_trace(p = p, data = lower_band, x = c(1:nrow(cumsum)), y = ~lower_band[[n+1]],
                 type = 'scatter', mode = 'lines', name = 'Stop Limit',
                 line = list(width = 3, dash = 'dash'))
  
  p <- add_trace(p = p, data = dd, x = c(1:nrow(cumsum)), y = ~dd[[n+1]],
                 type = 'scatter', fill = 'tozeroy', mode = 'none', name = 'Drawdown',
                 fillcolor = 'rgba(220, 220, 220, 0.9')
  
  return(p)
}

# Subcluster main equity graph 

sub_main_equity_graph <- function(df){
  
  p <- plot_ly()
  
  for (i in 2:ncol(df)){
    
    x = c(1:nrow(df))
    y = colnames(df)[i]
    p <- add_trace(p,
                   x = x,
                   y = df[[y]],
                   type = 'scatter',
                   mode = 'lines',
                   name = y)
  }
  
  p <- p %>% layout(title='Main Equity Graph')
  
  return(p)
}


