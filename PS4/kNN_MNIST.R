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

get_neighbors <- function(order_dist,k){
  neighbors <- as.matrix(order_dist[,1:k])
  return(neighbors)
}

get_label_matrix <- function(neighbors, labels){
  row <- nrow(neighbors)
  col <- ncol(neighbors)
  label_matrix <- matrix(0,nrow=row, ncol=col)
  label_matrix[,] <- labels[neighbors]
  return(label_matrix)
}

get_majority_vote <- function(label_matrix){
  vote <- apply(label_matrix,1,mode)
  return(vote)
}

get_prob <- function(label_matrix, predictLabels,k){
  hits <- apply(label_matrix,2,"==",predictLabels)
  sum_hits <- apply(hits, 1, sum)
  prob <- sum_hits / k
  return(prob)
}

save_csv <- function(features,labels,predictLabels, prob ){
  data <- data.frame(x=features, y=labels, predLabels=predictLabels, prob=prob)
  write.csv(data, file = "predictions.csv")
}

kNN <- function(features, labels, test, k, p){
  if (!require("flexclust")) install.packages("flexclust"); library(flexclust)
  assert  <- assert(features, test, labels, k, p)
  features <- scale(features)
  test <- scale(test)
  dist <- get_dist_matrix(features, test, p)
  order_dist <- t(as.matrix(apply(dist,2,order)))
  neighbors <- get_neighbors(order_dist, k=k)
  label_matrix <- get_label_matrix(neighbors, labels)
  predictLabels <- get_majority_vote(label_matrix)
  prob <- get_prob(label_matrix, predictLabels,k)
  return(predLabels=predictLabels)
}

train_data <- read.csv("MNIST_training.csv", header=FALSE)
labels <- train_data$V1
features <- train_data[,2:257]
test <- read.csv("MNIST_test.csv", header=FALSE)

k <- rep(seq(1,71,4))
lk <- length(k)

# 4 fold Cross-Validation
n <- nrow(train)
fold <- sample(1:4, n, replace=TRUE)

acc <- rep (NA, lk)
cvpred <- matrix(NA,nrow=n ,ncol=ncol(train))

for (h in 1:lk){
  ac <- rep(NA,4)
  for (i in 1:4){
    l <- kNN(features = features[fold!=i, ],labels = labels[fold!=i], 
             test = features[fold==i, ],  k = k[h] , p=2)
    ac[i] <- (1/length(l)) * sum(labels[fold==i] == l)*100
  }
  acc[h] <- mean(ac)
}

k_max <- k[which.max(acc)]

results <- kNN(features, labels, test, k_max,p)
df <- data.frame(predLabels=results$predLabels)

write.csv(df,file="MNIST_predictions.csv")

