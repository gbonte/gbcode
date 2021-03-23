
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("INFOF422"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    dashboardSidebar(
      column(3,sliderInput("N",
                  "Number of samples:",
                  min = 1,
                  max = 50,
                  value = 30))
    ) ## sidebarPanel
    ,
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Univariate",
                 fluidPage(
                   fluidRow(column(3,sliderInput("mean",
                                        "Mean:",
                                        min = -5,
                                        max = 5,
                                        value = 0),
                          sliderInput("variance",
                                      "Variance:",
                                      min = 0.001,
                                      max = 2,
                                      value = 0.1))),
                   fluidRow(column(4, plotOutput("uniPlotP",width = "100%")),
                          column(4,plotOutput("uniPlotH",width = "100%")),
                       column(4,plotOutput("uniPlotD",width = "100%")))
                   )),
        tabPanel("Bivariate",plotOutput("text1")),
        tabPanel("Trivariate",plotOutput("distPlot")),
        tabPanel("Help",htmlOutput("help"))
      )
    ) ## mainPanel
  ) ## sidebarLayout
))
