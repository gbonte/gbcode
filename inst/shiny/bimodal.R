
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
BOUND1<-5
BOUND2<-5
ui <- dashboardPage(
  dashboardHeader(title="InfoF422"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 100,step=2),
      menuItem("Univariate mixture", tabName = "Univariatemixture", icon = icon("th")),
      menuItem("Bivariate mixture", tabName = "Bivariatemixture", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Univariatemixture",
              fluidRow(
                box(width=4,sliderInput("mean1","Mean1:",min = -BOUND1, max = BOUND1 ,
                                        value = -2),
                    sliderInput("variance1","Variance1:",min = 0.001,max = 2, value = 0.5),
                    sliderInput("mean2","Mean2:",min = -BOUND1, max = BOUND1 ,
                                value = 2),
                    sliderInput("variance2","Variance2:",min = 0.001,max = 2, value = 0.5),
                sliderInput("p1","P1:",min = 0, max = 1 ,
                            value = 0.5)),
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP"))),
              
              fluidRow(   box(width=5,title = "Data",plotOutput("uniPlotD")),
                          box(width=5,title = "Histogram",plotOutput("uniPlotH"))                
              )
      ),
      # Second tab content
      tabItem(tabName = "Bivariatemixture",
              fluidRow(
                box(width=4,sliderInput("rot1","Rotation 1:", min = -3.14,max = 3.14, value = 0),
                    sliderInput("ax11","Axis1 1:",min = 0.01,max = BOUND2,value = 3,step=0.05),
                    sliderInput("ax21","Axis2 1:", min = 0.01, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("rot2","Rotation 2:", min = -3.14,max = 3.14, value = 0),
                    sliderInput("ax12","Axis1 2:",min = 0.01,max = BOUND2,value = 0.15,step=0.05),
                    sliderInput("ax22","Axis2 2:", min = 0.01, max = BOUND2, value = 3,step=0.05),
                    sliderInput("P1","P1:",min = 0, max = 1 ,value = 0.5),
                    textOutput("textB")),
                box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("biPlotP"))),
              fluidRow(   box(width=6,title = "Data",plotOutput("biPlotD")))
            
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
    
    xaxis=seq(min(input$mean1,input$mean2)-BOUND1,max(input$mean1,input$mean2)+BOUND1,by=0.01)
    plot(xaxis,input$p1*dnorm(xaxis,input$mean1,input$variance1)+(1-input$p1)*dnorm(xaxis,input$mean2,input$variance2),
         ylab="density")
    
  })
  
  output$uniPlotH <- renderPlot( {
    
    D1<-rnorm(input$N,input$mean1,input$variance1)
    D2<-rnorm(input$N,input$mean2,input$variance2)
    I1<-sample(1:input$N,round(input$p1*input$N))
    I2<-sample(1:input$N,round((1-input$p1)*input$N))
    D<<-c(D1[I1],D2[I2])
    hist(D)
    
    
  })
  
  output$uniPlotD <- renderPlot( {
    xl=min(input$mean1,input$mean2)-BOUND1
    xu=max(input$mean1,input$mean2)+BOUND1
    input$variance1+input$variance2+input$p1libra
    input$N
    
    plot(D,0*D,xlim=c(xl,xu))
    
    
  })
  
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
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    E<<-eigen(Sigma)
    
    ax1<-input$ax12
    th=input$rot2
    
    ax2<-input$ax22
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
    Sigma2<-(Rot%*%A)%*%t(Rot)
    E<<-eigen(Sigma2)
    
   
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-(input$P1)*dmvnorm(c(x[i],y[j]),sigma=Sigma)+(1-input$P1)*dmvnorm(c(x[i],y[j]),sigma=Sigma2)
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
    
    D1=rmvnorm(input$N,sigma=Sigma)
    
    th=input$rot2
    
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax12, 0, 0, input$ax22),dim=c(2,2))
    Sigma2<-(Rot%*%A)%*%t(Rot)
    
    D2=rmvnorm(input$N,sigma=Sigma2)
    
    I1<-sample(1:input$N,round(input$P1*input$N))
    I2<-sample(1:input$N,round((1-input$P1)*input$N))
    D<<-rbind(D1[I1,],D2[I2,])
    
    plot(D[,1],D[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2))
    lines(ellipse(Sigma))
    lines(ellipse(Sigma2))
    
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
