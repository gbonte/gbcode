

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Gaussian"),
  dashboardSidebar(
    sidebarMenu(
      
      
      
      menuItem("Univariate Standard", tabName = "Standard", icon = icon("th")),
      menuItem("Bivariate Normal", tabName = "Bivariatemixture", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Standard",
              fluidRow(  checkboxInput("Inner", label = "Inner", value = FALSE),
                         box(width=4,sliderInput("Xrange","Range X:", min = -5, max = 5,
                                                 value = c(-1,1),step=0.01)),
                         box(width=10,title = "Standard normal distribution",plotOutput("dnormal"))
                         
              )
      ),
      # Second tab content
      tabItem(tabName = "Bivariatemixture",
              fluidRow(
                box(width=4,
                    sliderInput("N",
                                "Number of samples:",
                                min = 10,
                                max = 1000,
                                value = 100,step=2),
                    sliderInput("rot1","Rotation 1:", min = -3.14,max = 3.14, value = 0),
                    sliderInput("ax11","Axis1 1:",min = 0.01,max = BOUND2,value = 3,step=0.05),
                    sliderInput("ax21","Axis2 1:", min = 0.01, max = BOUND2, value = 0.15,step=0.05),
                    textOutput("textB")
                ),
                box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("biPlotP"))),
              fluidRow(   box(width=12,title = "Data",plotOutput("biPlotD")))
              
      )
    )
  )
) # ui

d<-NULL
ns<-NULL
BOUND<-5
sX<-NULL
server<-function(input, output,session) {
  
  
  
  
  
  output$dnormal = renderPlot({
    
    
    x<-seq(-BOUND,BOUND,by=0.001)
    P=pnorm(input$Xrange[2])-pnorm(input$Xrange[1])
    if (!input$Inner)
      P=1-P
    plot(x,dnorm(x), type="l",ylab="Normal density",main=paste("Prob=",P))
    abline(v=input$Xrange[1])
    abline(v=input$Xrange[2])
    
  })
  
  output$biPlotP <- renderPlot({
    
    x <- seq(-2*BOUND2, 2*BOUND2, by= .2)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    ax1<-input$ax11
    th=input$rot1
    
    ax2<-input$ax21
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    
    ax1<-input$ax12
    th=input$rot2
    
    
    
    
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
    th=input$rot1
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax11, 0, 0, input$ax21),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    E<<-eigen(Sigma)
    D1=rmvnorm(input$N,sigma=Sigma)
    
    plot(D1[,1],D1[,2],xlim=c(-2*BOUND2,2*BOUND2),ylim=c(-2*BOUND2,2*BOUND2),xlab="x",ylab="y")
    lines(ellipse(Sigma))
    
    
  })
  
  output$textB <- renderText({ 
    input$rot
    input$ax11
    input$ax21
    paste("Eigen1=", E$values[1], "\n Eigen2=", E$values[2])
  })
}



shinyApp(ui, server)
