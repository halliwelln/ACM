setwd("~/Desktop")
library(extrafont)
library("ggplot2")

get_data <- function(slice, seed = 12345){
  set.seed(seed)
  x <- seq(0,2*pi,slice)
  y <- runif(length(x)) + sin(x)
  z <- runif(length(x)) + cos(x)
  data <- data.frame(x,y,z)
  return(data)
}

save_csv <- function(data){
  write.csv(data, file = "dataset.csv")
}

get_pdf <- function(data){
  plot <- ggplot(data = data, aes(x, y=value, color=class))
  plot <- plot + geom_point(aes(y=y,col="0"))
  plot <- plot + geom_point(aes(y=z,col="1"))
  plot <- plot + theme_bw()
  pdf("dataPlot.pdf", family = "Arial", width = 5.5, height = 5)
  print(plot)
  dev.off()
  embed_fonts("dataPlot.pdf")
}

main <- function(slice=0.01, save = TRUE, plot = TRUE, seed = 12345){
  data <- get_data(slice)
  if (save == TRUE){
    save_csv(data)
  }
  if (plot == TRUE){
    get_pdf(data)
  }
  return(data)
}

main()