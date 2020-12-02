library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)
library(latex2exp)

BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Boostrap", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 200,
                  value = 50,step=2),
      sliderInput("R",
                  "Number of trials:",
                  min = 100,
                  max = 5000,
                  value = 500,step=2),
      sliderInput("B",
                  "Number of boostraps:",
                  min = 100,
                  max = 5000,
                  value = 5,step=2),
      menuItem("1D point estimation (Gaussian)", tabName = "Univariate", icon = icon("th"))
    ) # sidebar Menu
  ), # dashboard sidebar
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Univariate",
              fluidRow(
                box(width=4,sliderInput("mean",withMathJax(sprintf('$$\\mu:$$')),min = -BOUND1, max = BOUND1 ,
                                        value = 0),
                    sliderInput("var",withMathJax(sprintf('$$\\sigma^2:$$')),min = 0.001,max = 2, value = 1),
                    uiOutput('EVar')),
                box(width=6,title = "Distribution",collapsible = TRUE,
                    plotOutput("uniPlotP",height = 300))),
              fluidRow(   
                box(width=5,title = "Sampling Distribution Mean",
                    plotOutput("uniSamplingM", height = 300)),
                box(width=5,title = "Sampling Distribution Variance",
                    plotOutput("uniSamplingV", height = 300))),
              fluidRow( 
                box(width=5,title = "Bootstrap Distribution Mean",
                    plotOutput("uniBootM", height = 300)),
                box(width=5,title = "Bootstrap Distribution Variance",
                    plotOutput("uniBootV", height = 300))
              )
      )
    ) ## tabItems
  )# DashboardBody
) # ui dashboardPage

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(input$mean-BOUND1,input$mean+BOUND1,by=0.01)
    plot(xaxis,dnorm(xaxis,input$mean,sqrt(input$var)),
         xlab="z",ylab="density",type="l",lwd=2)
    
    points(rnorm(input$N,input$mean,sqrt(input$var)),numeric(input$N))
  })
  
  output$uniSamplingM <- renderPlot( {
    meanD<-NULL
    for (r in 1:input$R){
      meanD<-c(meanD,mean(rnorm(input$N,input$mean,sqrt(input$var))))
      
    }
    hist(meanD,xlim=c(-BOUND1,BOUND1),
         xlab=TeX(sprintf("$\\hat{\\mu}$")),
         main=paste("Bias=", round(mean(meanD)-input$mean,2),
                    "Variance=", round(var(meanD),2)))
    
    
  })
  
  output$uniSamplingV <- renderPlot( {
    varD<-NULL
    for (r in 1:input$R){
      varD<-c(varD,var(rnorm(input$N,input$mean,sqrt(input$var))))
      
    }
    hist(varD,xlab=TeX(sprintf("$\\hat{\\sigma}^2$")),
         xlim=c(-BOUND1,BOUND1),main=paste("Bias=", round(mean(varD)-input$var,2)))
    
    
  })
  
  output$uniBootM <- renderPlot( {
    meanD<-NULL
    D<-rnorm(input$N,input$mean,sqrt(input$var))
    
    for (r in 1:input$B){
      Db<-sample(D,replace=TRUE)
      meanD<-c(meanD,mean(Db))
      
    }
    hist(meanD,xlab=TeX(sprintf("$\\hat{\\mu}_{(b)}$")),
         xlim=c(-BOUND1,BOUND1),main=paste("Bootstrap bias=", round(mean(meanD)-mean(D),2), 
                                           "; Bootstrap variance=", round(var(meanD),2)))
    
    
  })
  
  output$uniBootV <- renderPlot( {
    varD<-NULL
    D<-rnorm(input$N,input$mean,sqrt(input$var))
    for (r in 1:input$B){
      Db<-sample(D,replace=TRUE)
      
      varD<-c(varD,var(Db))
      
    }
    
    hist(varD,xlab=TeX(sprintf("$\\hat{\\sigma^2}_{(b)}$")),
         xlim=c(-BOUND1,BOUND1),main=paste("Bootstrap bias=", round(mean(varD)-var(D),2)))
    
    
  })
  
  output$EVar <- renderUI({
    withMathJax(sprintf('Estimator mean: variance   $$\\frac{\\sigma^2}{N}= %.04f$$',round(input$var/input$N,2)))
  })
}



shinyApp(ui, server)
