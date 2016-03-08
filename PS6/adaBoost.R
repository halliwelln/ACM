get_indicator <- function(pred_train, train_labels){
  #this function checks each element of the predicted labels match 
  #the training labels if they are equal, a zero is introduced, if they are
  #unequal, a 1 is introduced
  indicator <- ifelse(train_labels != pred_train, 1,0)
  return(indicator)
}

get_error <- function(weight, indicator){
  #this function calculates the error returning a vector
  error <- sum(weight * indicator)/sum(weight)
  return(error)
}
update_weights <- function(weight, alpha_vector, indicator){
  #this function updates the weights returning a vector
  weight <- weight * exp(alpha_vector * indicator)
  return(weight)
}
assert_that <- function(data, test, depth, noTrees){
  not_empty(data)
  not_empty(test)
  is.count(noTrees)
  is.count(depth)
}

adaBoost <- function(formula, data, test, depth=30, noTrees=10){
  if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
  if (!require("rpart")) install.packages("rpart"); library(rpart)
  assert <- assert_that(data, test, depth, noTrees)
  #define formula
  formula <- as.formula(formula)
  #convert labels to factor
  data[,all.vars(formula)[1]] <- as.factor(data[,all.vars(formula)[1]])
  #get training labels
  train_labels <- data[,all.vars(formula)[1]]
  n <- nrow(data)
  #initialize weights
  weight <- rep((1/n),n)
  #initialize alpha vector
  alpha_vector <- c()
  #initialize matrix to store classifiers
  store <- matrix(0,noTrees, n)
  
  for (i in 1:noTrees){
    #add column to training data
    data$weight <- weight
    #calculate decision tree
    tree <- rpart(formula, data=data, weights=weight ,
                  control=rpart.control(maxdepth = depth))
    #predict labels with training data
    pred_train <- predict(tree, data, type="class")
    #predict with test data
    store[i,] <- ifelse(predict(tree, test, type="class")!=1, -1, 1)
    #get indicator function
    indicator <- get_indicator(pred_train, train_labels)
    #compute error
    error <- get_error(weight, indicator)
    #append to alpha vector
    alpha_vector <- append(alpha_vector, log((1-error)/ error))
    #update weights
    weight <- update_weights(weight, alpha_vector[i], indicator)
  }
  classifier <- sign(colSums(alpha_vector * store))
  return(list(predLabels=classifier))
}


