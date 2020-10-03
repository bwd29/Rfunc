# Rfunc
Spectral and Hierarchical Clustering (Proj 1)


################################
Install Instructions

To instal use:
install_github("bwd29/Rfunc")


################################
Usage Instructions/Example R code





install_github("bwd29/Rfunc")

#random numbers from 1 to 10

data <- as.numeric(c(1,2,3,4,5,6,7,8,9,10,2,3,4,5,3,2,4,6))

#setting clusters to 3

k <- 3

#setting epsilon to 5

epsi <- 5

#using the SPECC function

cluster.ids <- Rfunc::SPECC(data, k, epsi)

print(cluster.ids)

#using the HCLUST fubction

cluster.ids <- Rfunc::HCLUST(data, k)

print(cluster.ids)
