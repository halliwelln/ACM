setwd("~/Desktop")
library(shiny)
library(ggplot2)
library(mvtnorm)

shinyServer(function(input,output){
  sigma <- function(sdX,sdY,rho){
    matrix(c(sdX^2,rho*sdX*sdY,rho*sdX*sdY,sdY^2),2,2,byrow = TRUE)
  }
  loanData <- function(noApproved, noDenied, muApprov_PI, muApprov_Solv, 
                       muDenied_PI, muDenied_Solv, sdApprov_PI, sdApprov_Solv,
                       sdDenied_PI, sdDenied_Solv, rhoApproved=-0.1, 
                       rhoDenied=0.8,seed=12345){
    set.seed(seed)
    sigma_approv <- sigma(sdApprov_PI,sdApprov_Solv,rhoApproved)
    sigma_denied <- sigma(sdDenied_PI,sdDenied_Solv,rhoDenied)
    approved <- rmvnorm(noApproved, c(muApprov_PI, muApprov_Solv),sigma_approv) 
    
    denied <- rmvnorm(noDenied, c(muDenied_PI, muDenied_Solv), sigma_denied)
    
    loanDf <- data.frame(as.data.frame(rbind(approved,denied)), 
                         c(rep("Approved", noApproved), rep("Denied", noDenied)), 
                         c(rep(0, noApproved), rep(1, noDenied)))
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
    loanDf
  }

  output$plot <- renderPlot({
    loanDf <- loanData(100,100,input$M_PI_Approv,input$M_Solv_Approv,
                       input$M_PI_Deny,input$M_Solv_Deny,input$SD_PI_Approv,
                       input$SD_Solv_Approv,input$SD_PI_Deny,input$SD_Solv_Deny)
    datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    
    ggplot(data = loanDf, aes(x = solvency, y = PIratio, 
                                      colour=deny, fill=deny))+geom_point() + 
      xlab("solvency") + ylab("Weight") + theme_bw()+ 
      theme(text=element_text(family="Arial"))+ 
      geom_abline(intercept = intercept, slope = slope)})
  
  output$confusion <- renderTable({
    loanDf <- loanData(100,100,input$M_PI_Approv,input$M_Solv_Approv,
                       input$M_PI_Deny,input$M_Solv_Deny,input$SD_PI_Approv,
                       input$SD_Solv_Approv,input$SD_PI_Deny,input$SD_Solv_Deny)
    datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied")
    confMatrixFreq <- table(loanDf$deny, predictedLabels)
  })
})
