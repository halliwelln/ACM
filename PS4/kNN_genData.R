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
  return(list(predLabels=predictLabels, prob=prob))
}

get_data <- function(slice, seed = 12345){
  set.seed(seed)
  x <- seq(0,2*pi,slice)
  y <- runif(length(x)) + sin(x)
  z <- runif(length(x)) + cos(x)
  data <- data.frame(x1 = rep(x,2), x2 = c(y,z), 
                     y = c(rep(0,length(y)), rep(1,length(z)) ))
  return(data)
}

get_pdf <- function(data, results){
  library(extrafont)
  library(ggplot2)
  plot <- ggplot(data=data) + theme_bw()
  plot <- plot + geom_point(data = data, 
                           aes(x=x1, y=x2,color=factor(predLabels)))
  plot <- plot + stat_density2d(aes(x=x1, y=x2, color=factor(predLabels)))
  pdf("plot.pdf", family = "Arial", width = 5.5, height = 5)
  print(plot)
  dev.off()
  embed_fonts("plot.pdf")
}

train_data <- get_data(0.01)
test_data <- get_data(0.01, seed=666)
test <- test_data$x2
labels_test <- test_data$y
features <- train_data$x2
labels <- train_data$y

results <- kNN(features, labels,test, k=3, p=2)
predLabels <- results$predLabels
prob <- results$prob
get_pdf(train_data, predLabels)
save_csv(features, labels, predLabels, prob)

mean(results$predLabels==labels_test)