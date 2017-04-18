
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
      menuItem("RBF", tabName = "RBF", icon = icon("th")),
      menuItem("LMN", tabName = "LMN", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      # Second tab content
      tabItem(tabName = "Feed-forward",
              fluidRow(
                box(width=3, title="Weights 1st hidden node", sliderInput("b0","bias 0:",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w011","w01(1):",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w111","w11(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("w211","w21(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)),
                box(width=3, title="Weights 2nd hidden node",
                    sliderInput("w021","w02(1):",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w121","w12(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("w221","w22(1):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)),
                box(width=3,title="Weights output node",
                    sliderInput("b1","bias 1:",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w012","w01(2):",min = -BOUND2,max = BOUND2,value = 0,step=0.05),
                    sliderInput("w112","w11(2):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
                    sliderInput("w212","w21(2):", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05)),
                box(width=3,radioButtons("radio1", label = h4("Activation layer1"),
                                         choices = list("logistic" = 1, "linear" = 0), 
                                         selected = 1),
                    radioButtons("radio2", label = h4("Activation Layer2"),
                                 choices = list("logistic" = 1, "linear" = 0), 
                                 selected = 1))
                
              ),
              fluidRow( box(width=5,title = "Output 1st hidden node ",collapsible = TRUE,plotOutput("oneL")),
                        box(width=5,title = "Output 2nd layer ",collapsible = TRUE,plotOutput("twoL")))
              
      ), ## tabItem
      tabItem(tabName = "RBF",
              fluidRow(
                box(width=3, title="Basis 1",
                    sliderInput("mu11","c1[1]:",min = -BOUND2,max = BOUND2,value = 1,step=0.05),
                    sliderInput("mu12","c1[2]:", min = -BOUND2, max = BOUND2, value = 1,step=0.05),
                    sliderInput("sigma1","B1:", min = 0, max = 10, value = 5,step=0.05)),
                box(width=3,title="Basis 2",
                    sliderInput("mu21","c2[1]:",min = -BOUND2,max = BOUND2,value = -1,step=0.05),
                    sliderInput("mu22","c2[2]:", min = -BOUND2, max = BOUND2, value = 2,step=0.05),
                    sliderInput("sigma2","B2:", min = 0, max = 10, value = 5,step=0.05)),
                box(width=3,title="Basis 3",
                    sliderInput("mu31","c3[1]:",min = -BOUND2,max = BOUND2,value = -3,step=0.05),
                    sliderInput("mu32","c3[2]:", min = -BOUND2, max = BOUND2, value = -5,step=0.05),
                    sliderInput("sigma3","B3:", min = 0, max = 10, value = 5,step=0.05)),
                box(width=3,
                    sliderInput("w1","h1:",min = -BOUND2,max = BOUND2,value = 3,step=0.05),
                    sliderInput("w2","h2:",min = -BOUND2,max = BOUND2,value = 3,step=0.05),
                sliderInput("w3","h3:",min = -BOUND2,max = BOUND2,value = -7,step=0.05))
                
              ),
              fluidRow( box(width=9,title = "RBF ",collapsible = TRUE,plotOutput("rbf")))
              
      )       , ## tabItem
      tabItem(tabName = "LMN",
              fluidRow(
                box(width=3, title="Basis 1",
                    sliderInput("Lmu11","c1[1]:",min = -BOUND2,max = BOUND2,value = 1,step=0.05),
                    sliderInput("Lmu12","c1[2]:", min = -BOUND2, max = BOUND2, value = 1,step=0.05),
                    sliderInput("Lsigma1","B1:", min = 0, max = 10, value = 2,step=0.05),
                    sliderInput("La1","a1:", min = -1, max = 1, value = 0.1,step=0.05),
                    sliderInput("Lb1","b1:", min = -1, max = 1, value = 0.1,step=0.05)),
                box(width=3,title="Basis 2",
                    sliderInput("Lmu21","c2[1]:",min = -BOUND2,max = BOUND2,value = -1,step=0.05),
                    sliderInput("Lmu22","c2[2]:", min = -BOUND2, max = BOUND2, value = 2,step=0.05),
                    sliderInput("Lsigma2","B2:", min = 0, max = 10, value = 2,step=0.05),
                    sliderInput("La2","a2:", min = -1, max = 1, value = 0.2,step=0.05),
                    sliderInput("Lb2","b2:", min = -1, max = 1, value = 0.15,step=0.05)),
                box(width=3,title="Basis 3",
                    sliderInput("Lmu31","c3[1]:",min = -BOUND2,max = BOUND2,value = -3,step=0.05),
                    sliderInput("Lmu32","c3[2]:", min = -BOUND2, max = BOUND2, value = -5,step=0.05),
                    sliderInput("Lsigma3","B3:", min = 0, max = 10, value = 2,step=0.05),
                    sliderInput("La3","a3:", min = -1, max = 1, value = 0.3,step=0.05),
                    sliderInput("Lb3","b3:", min = -1, max = 1, value = 0.2,step=0.05))
                
              ),
              fluidRow( box(width=9,title = "LMN ",collapsible = TRUE,plotOutput("lmn")))
              
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
  
  
  output$lmn <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .5)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        d1=(x[i]-input$Lmu11)^2+(y[j]-input$Lmu12)^2
        d2=(x[i]-input$Lmu21)^2+(y[j]-input$Lmu22)^2
        d3=(x[i]-input$Lmu31)^2+(y[j]-input$Lmu32)^2
        ww1=exp(-d1/(input$Lsigma1)^2)
        ww2=exp(-d2/input$Lsigma2^2)
        ww3=exp(-d3/input$Lsigma3^2)
        w1=ww1/(ww1+ww2+ww3)
        w2=ww2/(ww1+ww2+ww3)
        w3=ww3/(ww1+ww2+ww3)
        
        z[i,j]=(input$La1*x[i]+input$Lb1*y[j])*w1+
          (input$La2*x[i]+input$Lb2*y[j])*w2+
          (input$La3*x[i]+input$Lb3*y[j])*w3
        
        
      }
    }
    
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",zlim=c(-4,4))
    
    
    
  })
  

  
}



shinyApp(ui, server)
