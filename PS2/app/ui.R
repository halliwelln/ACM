setwd("~/Desktop")
library(shiny)
library(ggplot2)
library(mvtnorm)

shinyUI(fluidPage(
  fluidRow(
    column(2, h4("Approved"),
           sliderInput(inputId="M_PI_Approv", label="Mean PI Ratio",
                       value = 5, min=-5, max=20),
           sliderInput(inputId="SD_PI_Approv", label="St. Dev PI Ratio",
                       value = 1, min=-5, max=20),
           sliderInput(inputId="M_Solv_Approv", label="Mean Solvency",
                       value = 5, min=-5, max=20),
           sliderInput(inputId="SD_Solv_Approv", label="St. Dev Solvency",
                       value = 1, min=-5, max=20)),
    column(2, h4("Denied"),
           sliderInput(inputId="M_PI_Deny", label="Mean PI Ratio",
                       value = 5, min=-5, max=20),
           sliderInput(inputId="SD_PI_Deny", label="St. Dev PI Ratio",
                       value = 1, min=-5, max=20),
           sliderInput(inputId="M_Solv_Deny", label="Mean Solvency",
                       value = 5, min=-5, max=20),
           sliderInput(inputId="SD_Solv_Deny", label="St. Dev Solvency",
                       value = 1, min=-5, max=20)),
    column(6,plotOutput("plot")),
    column(3,tableOutput("confusion")))
  ))
