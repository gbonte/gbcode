
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("quadprog")
library("MASS")
require(shiny)
library(plotly)
library(shinydashboard)
library(mvtnorm)

BOUND1<-1.5
BOUND2<-1.5
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: conditional and inverse distributions", titleWidth = 500),
  dashboardSidebar( sliderInput("N",
                                "Number of samples:",
                                min = 1,
                                max = 1000,
                                value = 500,step=2),
                    sidebarMenu(
                      menuItem("Unimodal ", tabName = "Unimodal", icon = icon("th")),
                      menuItem("Bimodal ", tabName = "Bimodal", icon = icon("th")))
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      tabItem(tabName = "Unimodal",
              fluidRow(
                box(width=3,
                    sliderInput("mean11",withMathJax(sprintf('$$\\mu_{11} \\text{ (green):}$$ ')),min = -BOUND1, max = BOUND1 ,
                                value = -2,step=0.1)),
                box(width=3,
                    sliderInput("mean12",withMathJax(sprintf('$$\\mu_{12} \\text{ (green):}$$')),min = -BOUND1, max = BOUND1 ,
                                value = -2,step=0.1)),
                box(width=3,
                    sliderInput("variance11",withMathJax(sprintf('$$\\sigma^2_1 \\text{ (green):}$$')),
                                min = 0.5,max = 1, value = 0.75,
                                step=0.05))),
              fluidRow(
                box(width=3,
                    sliderInput("mean21",withMathJax(sprintf('$$\\mu_{21} \\text{ (red):}$$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1)),
                box(width=3,
                    sliderInput("mean22",withMathJax(sprintf('$$\\mu_{22} \\text{ (red):}$$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1)),
                box(width=3,
                    sliderInput("variance22",withMathJax(sprintf('$$\\sigma^2_2 \\text{ (red):}$$')),min = 0.5,max = 1, 
                                value = 0.75,step=0.05)),
                box(width=3,
                    sliderInput("p11",withMathJax(sprintf('$$P_1 \\text{ (green):} $$')),min = 0.001, max = 0.999 ,
                                value = 0.5,step=0.05))),
              fluidRow(box(width=6,title = "Distributions p(x|y)",collapsible = TRUE,
                           plotlyOutput("uniPlotP")),
                       box(width=6,title = "Conditional Distributions p(y|x)",collapsible = TRUE,
                           plotlyOutput("uniCondP"))),
              fluidRow(box(width=6,title = "Samples",collapsible = TRUE,
                           plotlyOutput("uniSamples")))
              #         fluidRow(   box(width=6,title = "Data",plotOutput("biPlotD"))                )
      ),
      tabItem(tabName = "Bimodal",
              fluidRow(
                box(width=2,
                    sliderInput("bmean11a",withMathJax(sprintf('$$\\mu^a_{11} \\text{ (green):}$$')),min = -BOUND1, max = BOUND1 ,
                                value = -2,step=0.1)),
                box(width=2,
                    sliderInput("bmean12a",withMathJax(sprintf('$$\\mu^a_{12} \\text{ (green):} $$')),min = -BOUND1, max = BOUND1 ,
                                value = -2,step=0.1)),
                box(width=2,
                    sliderInput("bmean11b",withMathJax(sprintf('$$\\mu^b_{11} \\text{ (green):} $$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1)),
                box(width=2,
                    sliderInput("bmean12b",withMathJax(sprintf('$$\\mu^b_{12} \\text{ (green):}$$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1)),
                box(width=2,
                    sliderInput("bvariance11",withMathJax(sprintf('$$\\sigma^2_1 \\text{ (green):}$$')),min = 0.5,max = 1, value = 0.75,
                                step=0.05))
              ),
              
              fluidRow(
                box(width=2,
                    sliderInput("bmean21",withMathJax(sprintf('$$\\mu_{21} \\text{ (red):}$$')),min = -BOUND1, max = BOUND1 ,
                                value = 0,step=0.1)),
                box(width=2,
                    sliderInput("bmean22",withMathJax(sprintf('$$\\mu_{22} \\text{ (red):} $$')),min = -BOUND1, max = BOUND1 ,
                                value = 0,step=0.1)),
                
                box(width=2,
                    sliderInput("bvariance22",withMathJax(sprintf('$$\\sigma^2_2 \\text{ (red):} $$')),min = 0.5,max = 1, 
                                value = 0.75,step=0.05)),
                box(width=2,
                    sliderInput("bp11",withMathJax(sprintf('$$P_1 \\text{ (green):}$$')),min = 0.01, max = 0.99 ,
                                value = 0.5,step=0.05))),
              fluidRow(box(width=6,title = "Distributions p(x|y)",collapsible = TRUE,
                           plotlyOutput("biPlotP")),
                       box(width=6,title = "Conditional Distributions p(y|x)",collapsible = TRUE,
                           plotlyOutput("biCondP"))),
              fluidRow(box(width=6,title = "Samples",collapsible = TRUE,
                           plotlyOutput("biSamples")))#,
              #         fluidRow(   box(width=6,title = "Data",plotOutput("biPlotD"))                )
      )
      
      
    )## tabItems
  ) # DashboardBody
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  
  
  output$uniPlotP <- renderPlotly({
    #p(x|y)
    x <- seq(-3*BOUND2, 3*BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z1[i,j]<-dmvnorm(c(x[i],y[j]),mean=c(input$mean11, input$mean12),sigma=input$variance11*diag(2))
        z2[i,j]<-dmvnorm(c(x[i],y[j]),mean=c(input$mean21, input$mean22),sigma=input$variance22*diag(2))
      }
    }
    z1[is.na(z1)] <- 1
    z2[is.na(z2)] <- 1
    
    fig <- plot_ly(x=x,y=y,z=z1, showscale=FALSE )
    fig <- fig %>% add_surface(colorscale="Greens")
    fig <- fig %>% add_surface(x=x,y=y,z=z2,colorscale="Reds")  %>% 
      layout( scene = list(camera = list(eye = list(x = 1.25, y = -1.25, z = 1.25)),
                           xaxis = list(title = 'x1'), yaxis = list(title = 'x2'),
                           zaxis = list(title = 'p(x|y)')))%>% 
      layout(showlegend = FALSE) 
    fig
    
  })
  
  output$uniCondP <- renderPlotly({
    # 'p(y|x)'
    x <- seq(-3*BOUND2, 3*BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        
        z1[i,j]<-(input$p11)*dmvnorm(c(x[i],y[j]),mean=c(input$mean11, input$mean12),sigma=input$variance11*diag(2))/
          (input$p11*dmvnorm(c(x[i],y[j]),mean=c(input$mean11, input$mean12),sigma=input$variance11*diag(2))+
             (1-input$p11)*dmvnorm(c(x[i],y[j]),mean=c(input$mean21, input$mean22),sigma=input$variance22*diag(2)))
        
        z2[i,j]<-(1-input$p11)*dmvnorm(c(x[i],y[j]),mean=c(input$mean21, input$mean22),sigma=input$variance22*diag(2))/
          (input$p11*dmvnorm(c(x[i],y[j]),mean=c(input$mean11, input$mean12),sigma=input$variance11*diag(2))+
             (1-input$p11)*dmvnorm(c(x[i],y[j]),mean=c(input$mean21, input$mean22),sigma=input$variance22*diag(2)))
      }
    }
   
    z1[is.na(z1)] <- 1
    z2[is.na(z2)] <- 1
    
    fig <- plot_ly(x=x,y=y,z=z1, showscale=FALSE )
    fig <- fig %>% add_surface(colorscale="Greens")
    fig <- fig %>% add_surface(x=x,y=y,z=z2,colorscale="Reds") %>% 
      layout( scene = list(xaxis = list(title = 'x1'), yaxis = list(title = 'x2'),
                           zaxis = list(title = 'p(y|x)'),
                                        camera = list(eye = list(x = 1.25, y = -1.25, z = 1.25))))%>% 
      layout(showlegend = FALSE) 
    fig
    
  })
  
  
  output$uniSamples <- renderPlotly({
    
    Ngreen=max(1,round(input$p11*input$N))
    Nred=max(1,input$N-Ngreen)
    
    Dg=rmvnorm(Ngreen,mean=c(input$mean11, input$mean12),sigma=input$variance11*diag(2))
    Dr=rmvnorm(Nred,mean=c(input$mean21, input$mean22),sigma=input$variance22*diag(2))
    
    
    fig <- plot_ly(x=Dg[,1],y=Dg[,2])
    fig <- fig %>% add_trace( type="scatter",mode = 'markers',marker = list(size = 10,
                                                                            color = 'green'))
    fig <- fig %>% add_trace(x=Dr[,1],y=Dr[,2], type="scatter", mode = 'markers',marker = list(size = 10,
                                                                                               color = 'red'))%>% 
      layout(showlegend = FALSE,xaxis = list(title = 'x1'), yaxis = list(title = 'x2'))  
    fig
    
  })
  
  
  output$biSamples <- renderPlotly({
    
    Ngreena=max(1,round(input$bp11*input$N/2))
    Ngreenb=Ngreena
    Nred=max(1,input$N-Ngreena-Ngreenb)
    
    Dga=rmvnorm(Ngreena,mean=c(input$bmean11a, input$bmean12a),sigma=input$variance11*diag(2))
    Dgb=rmvnorm(Ngreenb,mean=c(input$bmean11b, input$bmean12b),sigma=input$variance11*diag(2))
    Dr=rmvnorm(Nred,mean=c(input$bmean21, input$bmean22),sigma=input$variance22*diag(2))
    
    
    fig <- plot_ly(x=Dga[,1],y=Dga[,2])
    fig <- fig %>% add_trace( type="scatter",mode = 'markers',marker = list(size = 10,
                                                                            color = 'green'))
    fig <- fig %>% add_trace(x=Dgb[,1],y=Dgb[,2], type="scatter", 
                             mode = 'markers',marker = list(size = 10,
                                                            color = 'green'))%>% 
      layout(showlegend = FALSE)
    fig <- fig %>% add_trace(x=Dr[,1],y=Dr[,2], 
                             type="scatter", mode = 'markers',marker = list(size = 10,
                                                                            color = 'red'))%>% 
      layout(showlegend = FALSE,xaxis = list(title = 'x1'), yaxis = list(title = 'x2')) 
    fig
    
  })
  
  
  output$biPlotP <- renderPlotly({
    
    x <- seq(-3*BOUND2, 3*BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z1[i,j]<-dmvnorm(c(x[i],y[j]),mean=c(input$bmean11a, input$bmean12a),sigma=input$bvariance11*diag(2))+
          dmvnorm(c(x[i],y[j]),mean=c(input$bmean11b, input$bmean12b),sigma=input$bvariance11*diag(2))
        z2[i,j]<-dmvnorm(c(x[i],y[j]),mean=c(input$bmean21, input$bmean22),sigma=input$bvariance22*diag(2))
      }
    }
    z1[is.na(z1)] <- 1
    z2[is.na(z2)] <- 1
    
    fig <- plot_ly(x=x,y=y,z=z1, showscale=FALSE )
    fig <- fig %>% add_surface(colorscale="Greens")
    fig <- fig %>% add_surface(x=x,y=y,z=z2,colorscale="Reds") %>% 
      layout( scene = list(xaxis = list(title = 'x1'), yaxis = list(title = 'x2'),
                           zaxis = list(title = 'p(x|y)'),
                           camera = list(eye = list(x = 1.25, y = -1.25, z = 1.25))))%>% 
      layout(showlegend = FALSE) 
    fig
    
  })
  
  output$biCondP <- renderPlotly({
    
    x <- seq(-3*BOUND2, 3*BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        
        z1[i,j]<-(input$bp11)*(dmvnorm(c(x[i],y[j]),mean=c(input$bmean11a, input$bmean12a),sigma=input$bvariance11*diag(2))+
                                 dmvnorm(c(x[i],y[j]),mean=c(input$bmean11b, input$bmean12b),sigma=input$bvariance11*diag(2)))/
          (input$bp11*(dmvnorm(c(x[i],y[j]),mean=c(input$bmean11a, input$bmean12a),sigma=input$bvariance11*diag(2))+
                         dmvnorm(c(x[i],y[j]),mean=c(input$bmean11b, input$bmean12b),sigma=input$bvariance11*diag(2)))+
             (1-input$bp11)*dmvnorm(c(x[i],y[j]),mean=c(input$bmean21, input$bmean22),sigma=input$bvariance22*diag(2)))
        
        z2[i,j]<-(1-input$bp11)*(dmvnorm(c(x[i],y[j]),mean=c(input$bmean21, input$bmean22),sigma=input$bvariance22*diag(2)))/
          (input$bp11*(dmvnorm(c(x[i],y[j]),mean=c(input$bmean11a, input$bmean12a),sigma=input$bvariance11*diag(2))+
                         dmvnorm(c(x[i],y[j]),mean=c(input$bmean11b, input$bmean12b),sigma=input$bvariance11*diag(2)))+
             (1-input$bp11)*dmvnorm(c(x[i],y[j]),mean=c(input$bmean21, input$bmean22),sigma=input$bvariance22*diag(2)))
      }
    }
    
    z1[is.na(z1)] <- 1
    z2[is.na(z2)] <- 1
    
    fig <- plot_ly(x=x,y=y,z=z1, showscale=FALSE )
    fig <- fig %>% add_surface(colorscale="Greens")
    fig <- fig %>% add_surface(x=x,y=y,z=z2,colorscale="Reds") %>% 
      layout( scene = list(xaxis = list(title = 'x1'), yaxis = list(title = 'x2'),
                           zaxis = list(title = 'p(y|x)'),
                                        camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25))))%>% 
      layout(showlegend = FALSE) 
    fig
    
  })
  
  
  
}



shinyApp(ui, server)
