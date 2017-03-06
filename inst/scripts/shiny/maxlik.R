

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-0.8
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Maximum likelihood"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 50,
                  max = 100,
                  value = 70,step=2),
      sliderInput("mean","Mean:",min = -BOUND1, max = BOUND1 ,
                  value = 0,step=0.05),
      sliderInput("sdev","St Dev:",min = 0.1,max = 0.5, value = 0.2),
      sliderInput("par","Estimate:",min = -BOUND1, max = BOUND1 ,
                  value = 0),
      menuItem("Data", tabName = "Data", icon = icon("th")),
      menuItem("Likelihood", tabName = "Likelihood", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Data",
              fluidRow(
                
                box(width=5,title = "Distribution",collapsible = TRUE,plotOutput("DuniPlotP", height = 300)),
                box(width=5,title = "Fitting ",plotOutput("DFit", height = 300)))
      ),
      tabItem(tabName = "Likelihood",
              fluidRow(
                
                box(width=5,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP", height = 300)),
                box(width=5,title = "Fitting ",plotOutput("Fit", height = 300))),
              fluidRow( 
                box(width=5,title = "Likelihood ",plotOutput("Like", height = 300)),
                box(width=5,title = "- Log Likelihood ",plotOutput("LogLike", height = 300))
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
    D<<-rnorm(input$N,input$mean,input$sdev)
    xaxis=seq(input$mean-2*BOUND1,input$mean+2*BOUND1,by=0.01)
    plot(xaxis,dnorm(xaxis,input$mean,input$sdev),
         ylab="density",type="l",lwd=2)
    
  })
  
  output$Fit <- renderPlot( {
    input$N
    input$mean
    xaxis=seq(-BOUND1,BOUND1,by=0.01)
    
    
    plot(xaxis,dnorm(xaxis,input$par,input$sdev),xlim=c(-BOUND1,BOUND1),type="l")
    points(D,D*0)
    
  })
  
  output$DuniPlotP <- renderPlot( {
    D<<-rnorm(input$N,input$mean,input$sdev)
    xaxis=seq(input$mean-2*BOUND1,input$mean+2*BOUND1,by=0.01)
    plot(xaxis,dnorm(xaxis,input$mean,input$sdev),
         ylab="density",type="l",lwd=2)
    
  })
  
  output$DFit <- renderPlot( {
    input$N
    input$mean
    xaxis=seq(-BOUND1,BOUND1,by=0.01)
    plot(xaxis,dnorm(xaxis,input$par,input$sdev),xlim=c(-BOUND1,BOUND1),type="l")
    points(D,D*0)
    
  })
  
  
  output$Like <- renderPlot( {
    
    input$mean
    
    xaxis=seq(-BOUND1,BOUND1,by=0.01)
    lik<-numeric(length(xaxis))+1
    for (i in 1:length(xaxis)){
      for (j in 1:input$N){
        
        lik[i]<-lik[i]*dnorm(D[j],xaxis[i],input$sdev)
      }
    }
    plot(xaxis,lik,type="l",lwd=2)
    
    abline(v=input$par,col="red")
    
  })
  
  
  output$LogLike <- renderPlot( {
    input$mean
    xaxis=seq(-BOUND1,BOUND1,by=0.01)
    lik<-numeric(length(xaxis))+1
    for (i in 1:length(xaxis)){
      for (j in 1:input$N){
        
        lik[i]<-lik[i]*dnorm(D[j],xaxis[i],input$sdev)
      }
    }
    likpar=lik[which.min(abs(xaxis-input$par))]
    plot(xaxis,-log(lik),type="l",main=paste("-Log Lik=",round(-log(likpar),2)),lwd=2)
    abline(v=input$par,col="red")
    
  })
  
  
  
}



shinyApp(ui, server)
