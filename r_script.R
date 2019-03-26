require(dtw)
require(dtwclust)

cluster_function <- function(df, clusters_number){
  
  ls <- as.list(df)
  ls <- ls[2:length(df)]
  
  hc_sbd_new <- tsclust(ls, type = "h", k = clusters_number,
                        seed = 899,
                        distance = "sbd", centroid = shape_extraction,
                        control = hierarchical_control(method = "average"))
  
  clusters = hc_sbd_new@cluster
  final = as.data.frame(clusters)
  return(final)
  
}


# png(paste(path, '\\clusters.png', sep=""), width = 781, height = 538)
# plot(hc_sbd_new, type = "sc")
# dev.off()

