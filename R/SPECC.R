#' Spectral Clustering
#'
#' @param data data as a matrix
#' @param k number of clusters
#' @param epsilon distance metric
#'
#' @return vector of cluster IDs
#' @export
#'
#' @examples
#' SPECC(as.matrix(c(1,2,3,4,1,2,3,4,1,2,3,4)), 2, 3)
SPECC <- function(data, k, epsilon){
  data<-as.matrix(data)
  N <- nrow(data)
  #compute the similarity matrix with guasian similarity
  sim.mat <- matrix(0,nrow=N, ncol=N)
  for(i in 1:N){
    for(j in i:N){
      sim.mat[i,j] <- exp(-1*norm(as.matrix(data[i,]-data[j,]), type="F"))
    }
  }
  sim.mat <- Matrix::forceSymmetric(sim.mat)

  #use the threshold value epsilon to compute edges and store as adjacency matrix
  adj.mat <- matrix(0,nrow=N, ncol=N)
  for(i in 1:N){
    close<- sort(sim.mat[i,], decreasing = TRUE)[1:epsilon]
    for(j in 1:epsilon){
      adj.mat[i,which(sim.mat[i,] == close[j])] = close[j]
    }
  }
  adj.mat <- Matrix::forceSymmetric(adj.mat)


  #create the diagonal matrix
  diag.mat <- diag(apply(adj.mat, 1, sum ))

  #get the lap mat
  lap.mat <- diag.mat -adj.mat

  # get the eigenvectors of the laplace
  eigens <- eigen(lap.mat)
  e.val <- sort(eigens$values)
  e.vec <- eigens$vectors[,(ncol(eigens$vectors)-k+1):ncol(eigens$vectors)]



  #run kmeans on the eigen vecs
  cl<-stats::kmeans(e.vec, k)$cluster
  return(cl)

}
