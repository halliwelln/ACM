setwd("~/Desktop")

full <- read.table("spambase.data", sep=",")
data <- full[,-ncol(full)]

formula <-as.formula(paste0(names(full)[58], 
                            "~",paste(names(data), collapse = "+")))
source("adaBoost.R")
my_train_error <- c()
my_test_error <- c()
other_train_error <- c()
other_test_error <- c()
full$V58      <- as.factor(ifelse( full$V58 == 0, -1, 1)) 

for (i in seq(1,100, 10)){
  set.seed(i)
  permutation   <- sample.int(nrow(full)-1)
  size          <- floor(0.5 * length(permutation))
  train_index   <- permutation[1:size]
  test_index    <- setdiff(permutation, train_index)[1:2300]
  train_sample  <- full[train_index, ]
  train_labels  <- full$V58[train_index]
  test_sample   <- full[test_index,]
  test_labels   <- full$V58[test_index]
  predicted     <- as.vector(unlist(adaBoost(formula, train_sample, test_sample)))
  
  my_train_error <- append(train_error, 
                        (sum(train_labels==predicted)/length(predicted)))
  my_test_error <- append(test_error, 
                       (sum(test_labels==predicted)/length(predicted)))
  
}






error <- data.frame(My_Training=my_train_error, My_Test=my_test_error)

plot <-ggplot(data=error, aes(x=seq(1,100, 10), 
                              y=sort(error$Training, decreasing=T)))
plot <- plot+theme_bw()+geom_line(colour="red")
plot <- plot+ geom_line(data=error, 
                        aes(x=seq(1,100, 10), y=sort(error$Test, decreasing=T)), 
                        colour="blue")+xlab("Iteration")+ylab("Error")
plot <- plot + ggtitle("Training Error (Red) vs. Test Error (Blue)")
plot
