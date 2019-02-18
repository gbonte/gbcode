
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
BOUND1<-7
BOUND2<-5
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: unimodal"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 30,step=2),
      menuItem("Univariate", tabName = "Univariate", icon = icon("th")),
      menuItem("Bivariate", tabName = "Bivariate", icon = icon("th")),
      menuItem("Trivariate", tabName = "Trivariate", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Univariate",
              fluidRow(
                box(width=4,sliderInput("mean","Mean:",min = -BOUND1, max = BOUND1 ,
                                        value = 0),
                    sliderInput("variance","Variance:",min = 0.001,max = 3, value = 0.8)),
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP"))),
              
              fluidRow(   box(width=5,title = "Data",plotOutput("uniPlotD")),
                          box(width=5,title = "Histogram",plotOutput("uniPlotH"))                
              )
      ), # tabItem
      ###
      tabItem(tabName = "Bivariate",
              fluidRow(
                box(width=4,sliderInput("rot","Rotation:", min = -3.14,max = 3.14, value = 0),
                    sliderInput("ax1","Axis1:",min = 0,max = BOUND2,value = 2),
                    sliderInput("ax2","Axis2:", min = 0.1, max = BOUND2, value = 0.5),
                    textOutput("textB")),
                box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("biPlotP"))),
              fluidRow(   box(width=12,title = "Data",plotOutput("biPlotD")))
            
      ),# tabItem
      tabItem(tabName = "Trivariate",
              box(width=4,sliderInput("rotx","Rotation x :",min = -3.14,max = 3.14,value = 0),
                  sliderInput("roty","Rotation y:",min = -3.14,max = 3.14,value = 0),
                  sliderInput("rotz","Rotation z:", min = -3.14,max = 3.14, value = 0),
                  sliderInput("ax31","Axis1:",min = 0.01,max = BOUND2,value = 2),
                  sliderInput("ax32", "Axis2:",  min = 0.01, max = BOUND2,value = 0.5),
                  sliderInput("ax33", "Axis3:", min = 0.01,max = BOUND2,value = 0.5)),
              fluidRow(   box(width=12,title = "Data",plotOutput("triPlotD")))
              
    )
  )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(input$mean-BOUND1,input$mean+BOUND1,by=0.01)
    plot(xaxis,dnorm(xaxis,input$mean,input$variance),
         ylab="density",type="l")
    
  })
  
  output$uniPlotH <- renderPlot( {
    
    D<<-rnorm(input$N,input$mean,input$variance)
    hist(D)
    
    
  })
  
  output$uniPlotD <- renderPlot( {
    xaxis=seq(input$mean-BOUND1,input$mean+BOUND1,by=0.01)
    input$mean
    input$variance
    input$N
    plot(D,0*D,xlim=c(-BOUND1,BOUND1),ylab="")
    lines(xaxis,dnorm(xaxis,input$mean,input$variance),
         ylab="density",type="l",lty=3)
    
    
  })
  
  output$biPlotP <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .4)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    ax1<-input$ax1
    th=input$rot
    
    ax2<-input$ax2
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
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
    
    persp(x, y, prob.z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

    
    
  })
  
  output$biPlotD <- renderPlot( {
    th=input$rot
    
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax1, 0, 0, input$ax2),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    
    D=rmvnorm(input$N,sigma=Sigma)
    plot(D[,1],D[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2))
    lines(ellipse(Sigma))
    
  })
  
  output$textB <- renderText({ 
    input$rot
    input$ax1
    input$ax2
    paste("Eigen1=", E$values[1], "\n Eigen2=", E$values[2])
  })
  
  output$triPlotD <- renderPlot({
    
    
    Rotx<-array(c(1,0,0,0, cos(input$rotx), sin(input$rotx), 0, -sin(input$rotx), cos(input$rotx)),dim=c(3,3)); #rotation matrix
    
    Roty<-array(c(cos(input$roty), 0, -sin(input$roty), 0, 1,0,  sin(input$roty), 0, cos(input$roty)),dim=c(3,3));
   
    Rotz<-array(c(cos(input$rotz), sin(input$rotz), 0, -sin(input$rotz), cos(input$rotz),0, 0, 0, 1),dim=c(3,3));
    A<-array(c(input$ax31, 0, 0, 0, input$ax32,0, 0,0,input$ax33 ),dim=c(3,3))
    Rot=Rotx%*%Roty%*%Rotz
    Sigma<-(Rot%*%A)%*%t(Rot)
    D3=rmvnorm(round(input$N/2),sigma=Sigma)
    s3d<-scatterplot3d(D3,xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),zlim=c(-BOUND2,BOUND2),xlab="x",ylab="y",zlab="z")
    D3bis=rmvnorm(round(input$N/2),sigma=Sigma)
    s3d$points3d(D3bis,col="red")
  })
  
}



shinyApp(ui, server)
