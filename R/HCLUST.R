HCLUST<-function(data, k){
  # get the distances
  N <- nrow(data)
  dist.mat <- as.matrix(stats::dist(data))
  #enter loop to reduce down based on minimum values
  cluster.values <- matrix(0, nrow = N, ncol = 1)
  clusters <- N
  cluster.id <- 1
  while(clusters > k){
    #loop through to find the minimum
    min.val <- .Machine$integer.max
    col<-0
    row<-0
    for(i in 1:N){
      for(j in i:N){
        if( i!=j & (cluster.values[i] == 0 | cluster.values[i] != cluster.values[j]) ){
          if(dist.mat[i,j] < min.val){
            min.val <- dist.mat[i,j]
            col <- j
            row <- i
          }
        }
      }
    }
    
    # merge the cluster
    if(cluster.values[col] == 0 & cluster.values[row] == 0){
      cluster.values[col] = cluster.id
      cluster.values[row] = cluster.id
      cluster.id = cluster.id + 1
    }
    else if(cluster.values[col] == 0){
      cluster.values[col] = cluster.values[row]
    }
    else if(cluster.values[row] == 0){
      cluster.values[row] = cluster.values[col]
    }
    else {
      cluster.values[which(cluster.values == max(cluster.values[row],cluster.values[col]))] <- min(cluster.values[col],cluster.values[row])
    }
    
    clusters = clusters-1
    
  }
  
  #replace the cluster ids
  
  counter <- max(cluster.values) + 1
  
  for(i in 1:length(cluster.values)){
    if(cluster.values[i] == 0){
      cluster.values[i] = counter
      counter = counter +1
    }
  }
  
  cluster.values = cluster.values + k
  for(i in 1:k){
    cluster.values[which(cluster.values == max(cluster.values))] = i
  }
  
  
  return(cluster.values)
}