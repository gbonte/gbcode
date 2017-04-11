
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
BOUND2<-10
ui <- dashboardPage(
  dashboardHeader(title="InfoF422"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Feed-forward", tabName = "Feed-forward", icon = icon("th")),
      menuItem("RBF", tabName = "RBF", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      # Second tab content
      tabItem(tabName = "Feed-forward",
              fluidRow(
                box(width=3, sliderInput("b0","bias 0:",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w011","w01(1):",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w111","w11(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("w211","w21(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)),
                box(width=3,
                    sliderInput("w021","w02(1):",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w121","w12(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("w221","w22(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)),
                box(width=3,
                    sliderInput("b1","bias 1:",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w012","w01(2):",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w112","w11(2):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("w212","w21(2):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)),
                box(width=3,radioButtons("radio1", label = h3("Layer1"),
                                         choices = list("logistic" = 1, "linear" = 0), 
                                         selected = 1),
                    radioButtons("radio2", label = h3("Layer2"),
                                 choices = list("logistic" = 1, "linear" = 0), 
                                 selected = 1))
                
              ),
              fluidRow( box(width=5,title = "1st layer ",collapsible = TRUE,plotOutput("oneL")),
                        box(width=5,title = "2nd layer ",collapsible = TRUE,plotOutput("twoL")))
              
      ), ## tabItem
      tabItem(tabName = "RBF",
              fluidRow(
                box(width=3, 
                    sliderInput("mu11","mu1[1]:",min = -BOUND2,max = BOUND2,value = 1,step=0.05),
                    sliderInput("mu12","mu1[2]:", min = -BOUND2, max = BOUND2, value = 1,step=0.05),
                    sliderInput("sigma1","sigma1:", min = 0, max = 10, value = 5,step=0.05)),
                box(width=3,
                    sliderInput("mu21","mu2[1]:",min = -BOUND2,max = BOUND2,value = -1,step=0.05),
                    sliderInput("mu22","mu2[2]:", min = -BOUND2, max = BOUND2, value = 2,step=0.05),
                    sliderInput("sigma2","sigma2:", min = 0, max = 10, value = 5,step=0.05)),
                box(width=3,
                    sliderInput("mu31","mu3[1]:",min = -BOUND2,max = BOUND2,value = -3,step=0.05),
                    sliderInput("mu32","mu3[2]:", min = -BOUND2, max = BOUND2, value = -5,step=0.05),
                    sliderInput("sigma3","sigma3:", min = 0, max = 10, value = 5,step=0.05)),
                box(width=3,
                    sliderInput("w1","w1:",min = -BOUND2,max = BOUND2,value = 3,step=0.05),
                    sliderInput("w2","w2:",min = -BOUND2,max = BOUND2,value = 3,step=0.05),
                sliderInput("w3","w3:",min = -BOUND2,max = BOUND2,value = -7,step=0.05))
                
              ),
              fluidRow( box(width=9,title = "RBF ",collapsible = TRUE,plotOutput("rbf")))
              
      ) ## tabItem
    )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  
  output$twoL <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .5)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        a11=input$w011*input$b0+input$w111*x[i]+input$w211*y[j]  ## a_1^(1)
        a21=input$w021*input$b0+input$w121*x[i]+input$w221*y[j]
        
        z11=a11
        z21=a21
        if (input$radio1>0){
          z11=1/(1+exp(-a11))
          z21=1/(1+exp(-a21))
        }
        
        z[i,j]=input$w012*input$b1+input$w112*z11+input$w212*z21  ## a_1^(1
        if (input$radio2>0)
          z[i,j]=1/(1+exp(-z[i,j]))
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    persp(x, y, prob.z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
    
    
    
  })
  
  output$oneL <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .5)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        a11=input$w011*input$b0+input$w111*x[i]+input$w211*y[j]  ## a_1^(1)
        a21=input$w021*input$b0+input$w121*x[i]+input$w221*y[j]
        
        z11=a11
        
        if (input$radio1>0){
          z11=1/(1+exp(-a11))
          
        }
        
        z[i,j]=z11
        
        
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    persp(x, y, prob.z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
    
    
    
  })
  
  
  output$rbf <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .5)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
   
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        d1=(x[i]-input$mu11)^2+(y[j]-input$mu12)^2
        d2=(x[i]-input$mu21)^2+(y[j]-input$mu22)^2
        d3=(x[i]-input$mu31)^2+(y[j]-input$mu32)^2
       
        z[i,j]=input$w1*exp(-d1/(input$sigma1)^2)+(input$w2)*exp(-d2/input$sigma2^2)+(input$w3)*exp(-d3/input$sigma3^2)
        
        
      }
    }
   
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    
    
    persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",zlim=c(-4,4))
    
    
    
  })
  
}



shinyApp(ui, server)
