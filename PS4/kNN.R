setwd("~/Desktop")

assert <- function(features, test, labels, k, p){
  library(assertthat)
  not_empty(features)
  not_empty(labels)
  not_empty(test)
  is.count(k)
  is.count(p)
  assert_that(p %in% c(1,2,Inf))
  assert_that(is.vector(labels)==TRUE)
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
#create distance matrix
get_dist_matrix <- function(features, test,p){
  if (p==1){
    dist <- as.matrix(dist2(features,test, method = "manhattan",p=1))
  }
  if (p==2){
    dist <- as.matrix(dist2(features,test, method = "euclidean",p=2))
  }
  if (p==Inf){
    dist <- as.matrix(dist2(features,test,  method = "maximum", p=Inf))
  }
  return(dist)
}
#get k nearest neigbors
get_neighbors <- function(order_dist,k){
  neighbors <- as.matrix(order_dist[,1:k])
  return(neighbors)
}
#convert neighbors matrix to matrix of labels
get_label_matrix <- function(neighbors, labels){
  row <- nrow(neighbors)
  col <- ncol(neighbors)
  label_matrix <- matrix(0,nrow=row, ncol=col)
  label_matrix[,] <- labels[neighbors]
  return(label_matrix)
}
#find majority vote 
get_majority_vote <- function(label_matrix){
  vote <- apply(label_matrix,1,mode)
  return(vote)
}
#get probability
get_prob <- function(label_matrix, predictLabels,k){
  hits <- apply(label_matrix,2,"==",predictLabels)
  sum_hits <- apply(hits, 1, sum)
  prob <- sum_hits / k
  return(prob)
}
#save csv file
save_csv <- function(features,labels,predictLabels, prob ){
  data <- data.frame(x=features, y=labels, predLabels=predictLabels, prob=prob)
  write.csv(data, file = "predictions.csv")
}

kNN <- function(features, labels, test, k, p){
  if (!require("flexclust")) install.packages("flexclust"); library(flexclust)
  assert  <- assert(features, test, labels, k, p)
  dist <- get_dist_matrix(features, test, p)
  order_dist <- t(as.matrix(apply(dist,2,order)))
  neighbors <- get_neighbors(order_dist, k=k)
  label_matrix <- get_label_matrix(neighbors, labels)
  predictLabels <- get_majority_vote(label_matrix)
  prob <- get_prob(label_matrix, predictLabels,k)
  return(list(predLabels=predictLabels, prob=prob))
}
