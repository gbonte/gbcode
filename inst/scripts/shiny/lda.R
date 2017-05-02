
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(shinyRGL)
library(rgl) 
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
      sliderInput("mu11","mu1[1]:",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
      sliderInput("mu12","mu1[2]:", min = -BOUND2, max = BOUND2, value = 0,step=0.05),
      sliderInput("mu21","mu2[1]:",min = -BOUND2,max = BOUND2,value = 2,step=0.05),
      sliderInput("mu22","mu2[1]:", min = -BOUND2, max = BOUND2, value = 2,step=0.05),
      menuItem("Bivariate mixture", tabName = "Bivariatemixture", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Second tab content
      tabItem(tabName = "Bivariatemixture",
              fluidRow(
                box(width=4,title = "p(x| green)",collapsible = TRUE,plotOutput("biPlotP1")),
                box(width=4,title = "p(x|red)",collapsible = TRUE,plotOutput("biPlotP2"))),
              
              fluidRow(   box(width=6,title = "LDA",plotOutput("biPlotD")))
              
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
  
  output$biPlotP1 <- renderPlot({
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z1[i,j]<-dmvnorm(c(x[i],y[j]),c(input$mu11,input$mu12))
      }
    }
    z1[is.na(z1)] <- 1
    
    op <- par(bg = "white")
    persp(x, y, z1, theta = 30, phi = 30, expand = 0.5, col = "green")
  })
  
  output$biPlotP2 <- renderPlot({
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    
    z2<-array(0,dim=c(length(x),length(y)))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        px2=0.25*dmvnorm(c(x[i],y[j]),c(input$mu21,input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),-c(input$mu21,input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),c(input$mu21,-input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),c(-input$mu21,input$mu22))
        z2[i,j]<-px2
      }
    }
    
    z2[is.na(z2)] <- 1
    op <- par(bg = "white")
    persp(x, y, z2, theta = 30, phi = 30, expand = 0.5, col = "red")
  })
  
  output$biPlotC1 <- renderPlot({
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        px1=dmvnorm(c(x[i],y[j]),c(input$mu11,input$mu12))
        px2=0.25*dmvnorm(c(x[i],y[j]),c(input$mu21,input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),-c(input$mu21,input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),c(input$mu21,-input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),c(-input$mu21,input$mu22))
        z[i,j]<-px1*0.5/(px1*0.5+px2*0.5)
      }
    }
    
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    persp(x, y, z, theta = 30, phi = 50, expand = 0.5, col = "green")
  })
  
  output$biPlotC2 <- renderPlot({
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        px1=dmvnorm(c(x[i],y[j]),c(input$mu11,input$mu12))
        px2=0.25*dmvnorm(c(x[i],y[j]),c(input$mu21,input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),-c(input$mu21,input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),c(input$mu21,-input$mu22))+
          0.25*dmvnorm(c(x[i],y[j]),c(-input$mu21,input$mu22))
        z[i,j]<-px1*0.5/(px1*0.5+px2*0.5)
      }
    }
    
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    persp(x, y, 1-z, theta = 30, phi = 50, expand = 0.5, col = "red")
  })
  
  norm<-function(x){
    sqrt(sum(x^2))
  }
  output$biPlotD <- renderPlot( {
    th=0
    
    
    D1=rmvnorm(input$N,c(input$mu11,input$mu12))
    
    plot(D1[,1],D1[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="green")
    
    
    D2=rbind(rmvnorm(round(input$N/4),c(input$mu21,input$mu22)),
             rmvnorm(round(input$N/4),-c(input$mu21,input$mu22)),
             rmvnorm(round(input$N/4),c(-input$mu21,input$mu22)),
             rmvnorm(round(input$N/4),c(input$mu21,-input$mu22)))
    
    
    
    points(D2[,1],D2[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="red")
    
    
    Xd<--10:10
    mu.1<-apply(D1,2,mean)
    mu.2<-apply(D2,2,mean)
    P<-c(0.5,0.5)
    sigma2<-mean(apply(D1,2,var))
    w<-mu.1-mu.2
    x0<-0.5*(mu.1+mu.2)-sigma2/(norm(mu.1-mu.2)^2)*(mu.1-mu.2)*log(P[1]/P[2])
    
    m<--w[1]/w[2]
    intc<-w[1]/w[2]*x0[1]+x0[2]
    
    abline(a=intc,b=m)
    ## lines(x=c(mu.1[1],mu.2[1]),y=c(mu.1[2],mu.2[2]))
    points(x0[1],x0[2])
    
    
  })
  
  
  
  
  
}



shinyApp(ui, server)
