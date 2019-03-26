arrange_cluster = function() {
  
  df = read.csv('final.csv')
  df['y'] = seq(1:nrow(df))
  tidy <- spread(df, key = clusters, value = X, convert = TRUE, fill = NA)
  
  tidy$y <- NULL
  
  for(col in colnames(tidy)){
    
    tidy[col] = tidy[col] %>%  arrange(rowSums(is.na(.)))
    
  }
  
  tidy <- sapply(tidy, as.character) 
  tidy[is.na(tidy)] <- ""
  tidy <- as.data.frame(tidy)
  return(tidy)

}

create_MtoM = function() {
  
  df1 = read.csv('df.csv')
  df2 = read.csv('final.csv', stringsAsFactors = FALSE)
  
  t_df <- as.data.frame(t(df1[,-1]))
  colnames(t_df) <- df1[, 1]
  t_df['cluster'] = df2$clusters
  
  MtoM <- t_df %>% group_by(cluster) %>% 
                   summarise_all(.funs = sum) %>% 
                   ungroup()
  
  return(MtoM)
}

create_cumsum = function(MtoM){
  
  check <- apply(MtoM[,-1], MARGIN = 1, FUN = cumsum)
  check <- as.data.frame(check)
  
  check <- cbind(date = rownames(check), check)
  rownames(check) <- NULL
  colnames(check) = c('date', paste('Cluster', unique(MtoM$cluster), sep = '_'))
  return(check)
  
}

create_maxcum = function(cumsum){
  max_cum = as.data.frame(apply(cumsum[, -1], MARGIN = 2, FUN = cummax))
  max_cum = cbind(date=cumsum[, 1], max_cum)
  return(max_cum)
}

create_DD = function(cumsum, max_cum){
  DD = (max_cum[, -1] - cumsum[, -1]) * -1
  DD = cbind(date=cumsum[, 1], DD) 
  return(DD)
}

create_lowerband = function(cumsum, max_cum, DD){
  lower_band = max_cum
  max_drawdowns = apply(DD[, -1], 2, min)
  
  for(j in (1:(ncol(cumsum)-1))){
    for(i in (1:nrow(cumsum))){
    
    if(lower_band[,-1][i,j] > 0){
      lower_band[,-1][i,j] = lower_band[,-1][i,j] + max_drawdowns[j]
    }
    else{
      lower_band[,-1][i,j] = 0
    }
   }
  }
  return(lower_band)
}



