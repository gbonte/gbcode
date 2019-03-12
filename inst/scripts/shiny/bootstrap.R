

library(shinydashboard)
#library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-2
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="Bootstrap estimator"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 5,
                  max = 200,
                  value = 50,step=2),
      sliderInput("R",
                  "Number of simulation trials:",
                  min = 100,
                  max = 5000,
                  value = 1000,step=2),
      sliderInput("mean","Mean:",min = -BOUND1, max = BOUND1 ,
                  value = 0,step=0.05),
      sliderInput("var","Var:",min = 0.25,max = 1.5, value = 0.5),
      uiOutput('EVar'),
      menuItem("Univariate normal", tabName = "Univariate", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Univariate",
              fluidRow(
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP", height = 300))),
              fluidRow(   
                box(width=6,title = "Estimator Mean: Sampling Distribution ",plotOutput("uniSamplingM", height = 300)),
                box(width=6,title = "Estimator Variance: Sampling Distribution ",plotOutput("uniSamplingV", height = 300))
              ),
              fluidRow(   
                box(width=6,title = " Estimator Mean: Bootstrap Distribution ",plotOutput("BSamplingM", height = 300)),
                box(width=6,title = " Estimator Variance: Bootstrap Distribution ",plotOutput("BSamplingV", height = 300))
              )
      )
  )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  
  
  
  
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(input$mean-2*BOUND1,input$mean+2*BOUND1,by=0.01)
    D<<-rnorm(input$N,input$mean,sqrt(input$var))
    plot(xaxis,dnorm(xaxis,input$mean,sqrt(input$var)),
         ylab="density",type="l",lwd=2,xlim=c(-2*BOUND1,2*BOUND1))
    points(D,D*0)
    
  })
  
  output$uniSamplingM <- renderPlot( {
    meanD<-NULL
    for (r in 1:input$R){
      meanD<-c(meanD,mean(rnorm(input$N,input$mean,sqrt(input$var))))
      
    }
    hist(meanD,xlim=c(-BOUND1,BOUND1),main=paste("Avg=",round(mean(meanD),2),
                                                 "Var=",round(var(meanD),5) ))
    
    
  })
  
  output$uniSamplingV <- renderPlot( {
    varD<-NULL
    for (r in 1:input$R){
      varD<-c(varD,var(rnorm(input$N,input$mean,sqrt(input$var))))
      
    }
    hist(varD,xlim=c(0,2*BOUND1),main=paste("Avg=",round(mean((varD)),3)))
    
    
  })
  
  output$EVar <- renderUI({
    withMathJax(sprintf('Estimator mean: variance   $$\\frac{\\sigma^2}{N}= %.04f$$',input$var/input$N))
  })
  
 
  
  output$BSamplingM <- renderPlot( {
    input$N
    input$mean
    input$var
    input$R
    meanD<-NULL
    for (r in 1:input$R){
      Db<-D[sample(1:input$N,input$N,replace =TRUE)]
      meanD<-c(meanD,mean(Db))
      
    }
    biasEst<--mean(D)+mean(meanD)
    hist(meanD,xlim=c(-BOUND1,BOUND1),main=paste("Avg=",round(mean(meanD),2),
                                                 "Var=",round(var(meanD),5),
                                                 "B bias=",round(biasEst,2)))
    
    
  })
  
  output$BSamplingV <- renderPlot( {
    input$N
    input$mean
    input$var
    input$R
    varD<-NULL
    for (r in 1:input$R){
      Db<-D[sample(1:input$N,input$N,replace =TRUE)]
      varD<-c(varD,var(Db))
      
    }
    biasEst<--var(D)+mean(varD)
    hist(varD,xlim=c(0,2*BOUND1),main=paste("Avg=",round(mean((varD)),3),
                                            "B bias=",round(biasEst,2)))
    
    
  })
  
  output$Uinfo<- renderUI({
    withMathJax(sprintf('$$\\mu= %.04f,  \\sigma^2= %.04f$$ \n $$\\frac{\\sigma^2}{N}= %.04f $$',mean(input$bound), (1/12*(input$bound[2]-input$bound[1])^2),
                        (1/12*(input$bound[2]-input$bound[1])^2)/input$N))
   
  })
  
 
  
}



shinyApp(ui, server)
