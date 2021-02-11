
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
  dashboardHeader(title="InfoF422: Conditional Independence", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2000,
                  max = 10000,
                  value = 2500,step=50),
      sliderInput("a",
                  "a:",
                  min = 1,
                  max = 2,
                  value = 1.1,step=0.01),
      sliderInput("b",
                  "b:",
                  min = -0.2,
                  max = -0.1,
                  value = -0.11,step=0.01),
      sliderInput("z",
                  "z:",
                  min = -0.05,
                  max = 0.05,
                  value = 0,step=0.01),
      sliderInput("sdw",
                  "sd noise:",
                  min = 0.3,
                  max = 0.5,
                  value = 0.35,step=0.01),
      
      menuItem("Dependence -> cond. independence", tabName = "d2i", icon = icon("th")),
      menuItem("Independence -> cond. dependence", tabName = "i2d", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      
      # Second tab content
      tabItem(tabName = "d2i",
              fluidRow(
                box(width=10,title = "y=az, x=bz",collapsible = TRUE,plotOutput("marg")))
      ),
      tabItem(tabName = "i2d",
              fluidRow(
                box(width=10,title = "z=ax+by",collapsible = TRUE,plotOutput("cond")))
      ),
      tabItem(tabName = "about",
              fluidPage(
                includeHTML("about/about.condind.html"))
  )
  
  )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
 
  
  z<-NULL
  x<-NULL
  y<-NULL
  
  output$marg <- renderPlot({
    z<<-rnorm(input$N)
    y <<- input$a*z+rnorm(input$N,sd=input$sdw)
    x <<- input$b*z+rnorm(input$N,sd=input$sdw)
    D=data.frame(x,y)
    colnames(D)<-c("x","y")
    reg1 <- lm(y~.,data=D) 
    
    
    
    plot(x,y,xlim=c(-2,2),ylim=c(-2,2) )  
    abline(reg1,col="black",lw=3)
    
    Iz<-sort(abs(z-input$z),decr=FALSE,index=TRUE)$ix[1:50]
    x <- x[Iz]
    y <- y[Iz]
    D=data.frame(x,y)
    colnames(D)<-c("x","y")
    reg1 <- lm(y~.,data=D) 
    points(x,y,xlim=c(-2,2),ylim=c(-2,2),col="red")    
    abline(reg1,col="red",lw=3)
    
  })
  
  output$cond <- renderPlot({
    
    x <<- rnorm(input$N,sd=input$sdw)
    y <<- rnorm(input$N,sd=input$sdw)
    z<<-input$a*x+input$b*y
    D=data.frame(x,y)
    colnames(D)<-c("x","y")
    reg1 <- lm(y~.,data=D) 
    
    
    
    plot(x,y,xlim=c(-2,2),ylim=c(min(y),max(y)) )  
    abline(reg1,col="black",lw=3)
    
    Iz<-sort(abs(z-input$z),decr=FALSE,index=TRUE)$ix[1:20]
    x <- x[Iz]
    y <- y[Iz]
    D=data.frame(x,y)
    colnames(D)<-c("x","y")
    reg1 <- lm(y~.,data=D) 
    points(x,y,xlim=c(-2,2),ylim=c(min(y),max(y)),col="red")    
    abline(reg1,col="red",lw=3)
    
  })
  
  
}



shinyApp(ui, server)
