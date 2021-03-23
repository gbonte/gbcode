
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-5
BOUND2<-5
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Conditional probability", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 500,step=2),
      sliderInput("tdt",
                  "3D theta:",
                  min = -60,
                  max = 60,
                  value = 0,step=5),
      sliderInput("tdp",
                  "3D phi:",
                  min = 0,
                  max = 90,
                  value = 75,step=1),
      ## sliderInput("dx","X density:", min = 0.1, max = 0.3, value = 0.15,step=0.01),
      ##  sliderInput("dy", "Y density:", min = 0.1, max = 0.3, value = 0.15,step=0.01),
      
      menuItem("Bivariate gaussian distribution", tabName = "Bivariatemixture", icon = icon("th")),
      menuItem("Regression function", tabName = "Regression", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      # Second tab content
      tabItem(tabName = "Bivariatemixture",
              fluidRow(
                box(width=4,collapsible = TRUE,sliderInput("rot1","Rotation:", min = -3/2,max = 3/2, 
                                                           value = -0.75),
                    sliderInput("ax11","Axis1:",min = 0.01,max = BOUND2,value = 3,step=0.05),
                    sliderInput("ax21","Axis2:", min = 0.01, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("x","x:", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    textOutput("textB")),
                box(width=8,title = "3D joint density visualization",collapsible = TRUE,plotOutput("biPlotP"))),
              fluidRow(   box(width=6,collapsible = TRUE,title = "Data",plotOutput("biPlotD")),
                          box(width=6,collapsible = TRUE,title = "Conditional distribution",plotOutput("biCond")))
      ), ## tabItem
      tabItem(tabName = "Regression",
              fluidRow(box(width=4,collapsible = TRUE,
                           sliderInput("ord","Functions:", min = -3,max = 3, 
                                       value = 1,step=1),
                           sliderInput("sdw","Cond sdev:", min = 0.5,max = 2.5, 
                                       value = 1,step=0.1),
                           sliderInput("rx","x:", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)), 
                       box(width=6,title = "3D joint density visualization",collapsible = TRUE,plotOutput("regrPlotP"))),## fluidRow
              fluidRow(   box(width=6,collapsible = TRUE,title = "Data",plotOutput("regrPlotD")),
                          box(width=6,collapsible = TRUE,title = "Conditional distribution",plotOutput("regrCond")))
              
      ),
      tabItem(tabName = "about",
              fluidPage(
                includeHTML("about/about.condpro.html")
              )
      ) ## tabItem
    )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  f<-function(x,ord){
    f<-numeric(length(x))
    if (ord==-1)
      f<-sin(x)
    if (ord==-2)
      f<-cos(2*x)
    if (ord==-3)
      f<-cos(4*x)
    if (ord==1)
      f<-x
    if (ord==2)
      f<-x^2-2
    if (ord==3)
      f<--x^2+1
    
    
    f
  }
  
  
  
  output$biPlotP <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    ax1<-input$ax11
    th=input$rot1
    
    ax2<-input$ax21
    Rot<-array(c(cos(th), sin(th), -sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    E<<-eigen(Sigma)
    
    
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-dmvnorm(c(x[i],y[j]),sigma=Sigma)
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    surface<-persp3D(x, y, prob.z, theta = input$tdt, phi = input$tdp, expand = 0.5, col = "blue",facets=FALSE)
    
    
    
    lines (trans3d(x=input$x, y = seq(-BOUND2, BOUND2, by= .2), z = 0, pmat = surface), col = "red",lwd=3)
    
  })
  
  output$biPlotD <- renderPlot( {
    th=input$rot1
    
    Rot<-array(c(cos(th), sin(th), -sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax11, 0, 0, input$ax21),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    
    D1=rmvnorm(input$N,sigma=Sigma)
    
    D<<-D1
    
    
    
    plot(D[,1],D[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),xlab="x", ylab="y")
    lines(ellipse(Sigma))
    abline(v=input$x,  col = "red",lwd=3)
    
  })
  
  output$biCond <- renderPlot( {
    th=input$rot1
    
    Rot<-array(c(cos(th), sin(th), -sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax11, 0, 0, input$ax21),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    
    sigma2=sqrt(Sigma[2,2])
    sigma1=sqrt(Sigma[1,1])
    rho=Sigma[1,2]/(sigma1*sigma2)
    
    x=seq(-1.5*BOUND2, 1.5*BOUND2, by= .02)
    plot(x,dnorm(x,rho*sigma2*(input$x)/sigma1,sd=sigma2^2*(1-rho^2)),type="l",col="red",
         lwd=2,ylab="Conditional density")
    lines(x,dnorm(x,0,sd=sigma2^2))
    legend(x=BOUND2,y=1,legend=c("Conditional","Marginal"),lty=1,col=c("red","black"))
    
  })
  
  
  output$textB <- renderText({ 
    input$rot
    input$ax1
    input$ax2
    paste("Eigen1=", E$values[1], "\n Eigen2=", E$values[2])
  })
  
  output$regrPlotP <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= 0.1)
    y <- seq(-BOUND2, BOUND2, by= 0.1)
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    
    
    muy<-f(x,ord=input$ord)
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-dnorm(y[j],mean=muy[i],sd=input$sdw)
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    surface<-persp3D(x, y, prob.z, theta = input$tdt, phi =input$tdp, expand = 0.5, col = "blue",facets=FALSE)
    
    
    
    lines (trans3D(x=input$rx, y = seq(-BOUND2, BOUND2, by= .2), z = 0, pmat = surface), col = "red",lwd=3)
    
  })
  
  output$regrPlotD <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=f(X,ord=input$ord)
    Y=muy+rnorm(input$N,sd=input$sdw)
    
    D<<-cbind(X,Y)
    plot(D[,1],D[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),
         xlab="x",ylab="y")
    lines(D[,1],muy, lwd=3)
    abline(v=input$rx,  col = "red",lwd=3)
    
  })
  
  output$regrCond <- renderPlot( {
    th=input$rot1
    
    
    muy=f(input$rx,input$ord)
    x=seq(-1.5*BOUND2, 1.5*BOUND2, by= .02)
    plot(x,dnorm(x,mean=muy,sd=input$sdw),type="l",col="red",lwd=2,ylab="Conditional density")
    
  })
  
  
  
}



shinyApp(ui, server)
