setwd("~/Desktop")
col <- c(2,4,8,10,11)
data <- mtcars[,col]
labels <- mtcars$am

setwd("~/Desktop")
col <- c(2,4,8,10,11)
data <- mtcars[,col]
labels <- mtcars$am
value <- 100

split_data <- function(data, column, value){
  if (is.numeric(data[,column]) == TRUE){
    index <- (data[,column] >= value)
  }else{
    index <- (data[,column] == value)
  }
  left_split <- data[index,]
  right_split <- data[!index,]
  return(list(LeftSplit=left_split,RightSplit=right_split))
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

get_frequency <- function(labels){
  freq <- as.data.frame(table(labels))[,2]
  return(freq)
}

left_split <- split_data(data, 2, value)$LeftSplit
right_split <- split_data(data,2,value)$RightSplit
results <- get_frequency(labels)

get_prob <- function(results,N){
  prob <- c()
  len <- length(results)
  for (i in 1:len){
    prob[i] <- (results[i] / N)
  }
  return(prob)
}

N <- nrow(data)

get_entropy <- function(split_data, N){
  labels <- split_data[,ncol(split_data)]
  results <- get_frequency(labels)
  prob <- get_prob(results, N)
  cross_entropy <- - sum(prob * log2(prob))
  return(cross_entropy)
}

get_gini <- function(split_data, N){
  labels <- split_data[,ncol(split_data)]
  results <- get_frequency(labels)
  prob <- get_prob(results, N)
  gini <- sum(prob*(1-prob))
  return(gini)
}

get_misclass <- function(split_data,N){
  labels <- split_data[,ncol(split_data)]
  results <- get_frequency(labels)
  prob <- get_prob(results, N)
  misclass <- 1 - max(prob)
  return(misclass)
}

if (nrow(data) <= 1){}
current_score <- 
  best_gain <- 0
best_criteria <- ""
best_splits <- ""

column_count <- nrow(data)-1

for (col in 1:column_count){
  column_values <- c()
  
}


#all.vars


cTree <- function(formula, data, depth, minpoints, costFnc){
  if (costFnc == "Entropy"){}
  if (costFnc == "Gini"){}
  if (costFnc == "ME"){}
  
}


prob <- get_prob(labels)
column <- data$cyl
costFnc <- "Entropy"
left_split <- split_data(data, 2, 100)$LeftSplit
right_split <- split_data(data,2,100)$RightSplit

get_gain(column, costFnc, left_split, right_split, labels)
