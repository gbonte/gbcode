

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-1
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="Interval estimator"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 5,
                  max = 200,
                  value = 50,step=2),
      actionButton("action", label = "Generate Dataset"),
      sliderInput("mean","Mean:",min = -BOUND1, max = BOUND1 ,
                  value = 0,step=0.05),
      sliderInput("var","Var:",min = 0.25,max = 1.5, value = 0.5),
      sliderInput("alpha","Alpha:",min = 0.0001,max = 0.2, value = 0.05),
      
      menuItem("Univariate normal", tabName = "Univariate", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Univariate",
              fluidRow(
                box(width=8,title = "Variance known",collapsible = TRUE,plotOutput("uniPlotP"))),
              fluidRow(
                box(width=8,title = "Variance unknown",collapsible = TRUE,plotOutput("uniPlotT")))
      )
  )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
cnt<-0
R<-0
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  observe ({
    input$N
    input$alpha
    input$mean
    input$var
    cnt<<-0
    R<<-0
  })
  
  
  
  output$uniPlotP <- renderPlot( {
    input$action
    sigma<-sqrt(input$var)
    xaxis=seq(input$mean-2*BOUND1,input$mean+2*BOUND1,by=0.01)
    D<<-rnorm(input$N,input$mean,sigma)
    z.alpha<-qnorm(input$alpha/2, lower=FALSE)
    
    R<<-R+1
    if ((input$mean>(mean(D)-z.alpha*sigma/sqrt(input$N)))& (input$mean<(mean(D)+z.alpha*sigma/sqrt(input$N))))
      cnt<<-cnt+1
    
    
    plot(xaxis,dnorm(xaxis,input$mean,sqrt(input$var)),
         ylab="density",type="l",lwd=2,xlim=c(-BOUND1,BOUND1))
    if (cnt>2)
      title(paste("success rate=",round(cnt/(R),3),"; 1-alpha=", 1-input$alpha))
    points(D,D*0)
    abline(v=input$mean,lwd=3,col="black")
    abline(v=mean(D)-z.alpha*sqrt(input$var)/sqrt(input$N),col="red")
    abline(v=mean(D)+z.alpha*sqrt(input$var)/sqrt(input$N),col="red")
    
  })
  
  
  
  output$uniPlotT <- renderPlot( {
    input$action
    sigma<-sqrt(input$var)
    xaxis=seq(input$mean-2*BOUND1,input$mean+2*BOUND1,by=0.01)
    
    t.alpha<-qt(input$alpha/2, lower=FALSE,df=input$N-1)
    sigma.hat=sd(D)
    R<<-R+1
    if ((input$mean>(mean(D)-t.alpha*sigma.hat/sqrt(input$N)))
        & (input$mean<(mean(D)+t.alpha*sigma.hat/sqrt(input$N))))
      cnt<<-cnt+1
    
    
    plot(xaxis,dnorm(xaxis,input$mean,sqrt(input$var)),
         ylab="density",type="l",lwd=2,xlim=c(-BOUND1,BOUND1))
    if (cnt>2)
      title(paste("success rate=",round(cnt/(R),3),"; 1-alpha=", 1-input$alpha))
    points(D,D*0)
    abline(v=input$mean,lwd=3,col="black")
    abline(v=mean(D)-t.alpha*sigma.hat/sqrt(input$N),col="red")
    abline(v=mean(D)+t.alpha*sigma.hat/sqrt(input$N),col="red")
    
  })
  
  
  
  
  
  
  
  
}



shinyApp(ui, server)
