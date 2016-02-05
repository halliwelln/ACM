setwd("~/Desktop")
library("ggplot2")
library(mvtnorm)
library(extrafont)

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

loanData <- function(noApproved, noDenied, noUndecided, muApproved, 
                     muDenied, muUndecided, sdApproved,sdDenied, sdUndecided, 
                     rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, 
                            sdX=sdUndecided[1], sdY=sdUndecided[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  loanDf <- as.data.frame(rbind(approved,denied, undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), 
            rep("Undecided", noUndecided)) 
  target = c(rep(0, noApproved), rep(1, noDenied), rep(2, noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target") 
  
  return(loanDf)
}

noApproved  <- 50; noDenied <- 50; noUndecided <- 50

loanDf <- loanData(noApproved, noDenied, noUndecided, c(4, 150), c(10,100), c(7,210) ,
                   c(1,20), c(2,30), c(.5,3), -0.1, 0.6, 0.1, 1221)


loanDf <- cbind(loanDf,
                target1 = c(rep(0, noApproved), rep(1, noDenied),rep(0,noUndecided)), 
                target2 = c(rep(1, noApproved), rep(0, noDenied),rep(0,noUndecided)),
                target3 = c(rep(0, noApproved), rep(0, noDenied),rep(1,noUndecided))
                )

X <- as.matrix(cbind(ind=rep(1,nrow(loanDf)),loanDf[,c("PIratio","solvency")]))
Y <- cbind(target1 = c(rep(0,noApproved),rep(1,noDenied),rep(0,noUndecided)),
           target2 = c(rep(1,noApproved),rep(0,noDenied),rep(0,noUndecided)),
           target3 = c(rep(0,noApproved),rep(0,noDenied),rep(1,noUndecided)))

w <- solve(t(X)%*%X) %*% t(X) %*% Y

predictions <- X %*% w


denied <- (predictions==apply(predictions,1,max))[,1]
approved <- (predictions==apply(predictions,1,max))[,2]
undecided <- (predictions==apply(predictions,1,max))[,3]
predictedLabels <- ifelse(denied, "Denied", "Approved")

loanDf <- cbind(loanDf, predictions, denied, approved, undecided)

write.csv(loanDf, file = "predictions.csv")


int1 <- ((w[1,2]-w[1,1]) / (w[3,1] - w[3,2]))
int2 <- ((w[1,2]-w[1,3])/ (w[3,3]-w[3,2]))
int3 <- ((w[1,1]-w[1,3])/ (w[3,3]-w[3,1]))
slope1 <- ((w[2,2]-w[2,1])/(w[3,1]-w[3,2]))
slope2 <-((w[2,2]-w[2,3])/ (w[3,3]-w[3,2]))
slope3 <-((w[2,1]-w[2,3])/ (w[3,3]-w[3,1]))

x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]),
         length.out = noApproved+noDenied)

line1 <- int1 + slope1 * x
line2 <- int2 + slope2 * x
line3 <- int3 + slope3 * x

boundaryDf2 <- data.frame(PIratio=x, solvency=line1,
                          deny=rep("Boundary1", length(x)))
boundaryDf3 <- data.frame(PIratio=x, solvency=line2,
                          deny=rep("Boundary2", length(x)))
boundaryDf4 <- data.frame(PIratio=x, solvency=line3,
                          deny=rep("Boundary3", length(x)))



plot <- ggplot(data = loanDf, 
               aes(x = solvency, y = PIratio, colour=deny, fill=deny)) 
plot <- plot + geom_point()
plot <- plot + xlab("solvency") + ylab("PIratio")
plot <- plot + theme_bw() + geom_line(data=boundaryDf2) 
plot <- plot + geom_line(data=boundaryDf3) + geom_line(data=boundaryDf4)
plot <- plot + theme(text=element_text(family="Arial"))
plot <- plot + scale_x_continuous(limits=c(0,250))
pdf("discFunction3C.pdf", family = "Arial", width = 5.5, height = 5)
print(plot)
dev.off()
embed_fonts("discFunction3C.pdf")
