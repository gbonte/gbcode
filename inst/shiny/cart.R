

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-5
BOUND2<-2
STEP=0.05
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Decision Tree", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 200,
                  value = 50,step=2),
      sliderInput("x0","x0:",min = -1,max = 1,value = 0.3,step=0.01),
      sliderInput("y0","y0:",min = -1,max = 1, value = 0.3,step=0.01),
      sliderInput("x1","x1:",min = -1,max = 1,value = -0.3,step=0.01),
      sliderInput("y1","y1:",min = -1,max = 1, value = -0.3,step=0.01),
      sliderInput("sd0","sd0:",min = 0.001,max = 1, value = 0.5,step=0.01),
      sliderInput("sd1","sd1:",min = 0.001,max = 1, value = 0.5,step=0.01),
      menuItem("Classification", tabName = "Classification", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Classification",
              fluidRow(  
                box(width=2,sliderInput("thx","split X:", min = -1, max = 1,
                                        value = 0,step=STEP)),
                box(width=2,sliderInput("thy","split Y:", min = -1, max = 1,
                                        value = 0,step=STEP))),
              fluidRow( 
                box(width=4,title = "Data",plotOutput("Scatter")),
                box(width=4,title = "Decrease Empirical Error",plotOutput("Cost"))
              )
      )
    )
  )
) # ui

d<-NULL
ns<-NULL
BOUND<-5
sX<-NULL
server<-function(input, output,session) {
  
  
  X<-reactive({rbind(
    cbind(rnorm(input$N,input$x0,sd=input$sd0),rnorm(input$N,input$y0,sd=input$sd0)),
    cbind(rnorm(input$N,input$x1,sd=input$sd1),rnorm(input$N,input$y1,sd=input$sd1))
  )
    })
  Y<-reactive({ y<-numeric(2*input$N)
  w1<-1:input$N
  y[w1]=1
  y})
  
  
  output$Scatter = renderPlot({
    
    w0<-which(Y()==0)
    w1<-which(Y()==1)
    plot(X()[w0,1],X()[w0,2],col="black",xlim=c(-1,1),ylim=c(-1,1),xlab="x1",ylab="x2")
    points(X()[w1,1],X()[w1,2],col="red")
    abline(v=input$thx,col="blue")
    abline(h=input$thy,col="green")
    
  })
  
  output$Cost = renderPlot({
    
    xaxis=seq(-1,1,by=STEP)
    yaxis=seq(-1,1,by=STEP)
    Deltax=NULL
    yhat=mean(Y())
    e=Y()-yhat
    for (x in xaxis){
      
      R1<-which(X()[,1]<=x)
      R2<-which(X()[,1]>x)
      if (length(R1)>0  & length(R2)>0){
        Y1=Y()[R1]
        yhat1=mean(Y1)
        e1=Y1-yhat1
        
        Y2=Y()[R2]
        yhat2=mean(Y2)
        e2=Y2-yhat2
        Deltax<-c(Deltax, sum(e^2)-(sum(e1^2)+sum(e2^2)))
      } else
        Deltax<-c(Deltax, 0)
    }
    
    Deltay=NULL
    
    for (y in yaxis){
      
      R1<-which(X()[,2]<=y)
      R2<-which(X()[,1]>y)
      if (length(R1)>0  & length(R2)>0){
      Y1=Y()[R1]
      yhat1=mean(Y1)
      e1=Y1-yhat1
      
      Y2=Y()[R2]
      yhat2=mean(Y2)
      e2=Y2-yhat2
      Deltay<-c(Deltay, sum(e^2)-(sum(e1^2)+sum(e2^2)))
      } else {
        Deltay<-c(Deltay, 0)
      }
    }
    
    
    plot(xaxis,Deltax,type="l",col="blue",ylim=c(-0.01,max(c(Deltax,Deltay))))
    abline(v=input$thx,col="blue")
    
    lines(yaxis,Deltay,type="l",col="green")
    abline(v=input$thy,col="green")
    
  })
  
}



shinyApp(ui, server)
