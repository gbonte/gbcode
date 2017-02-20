

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
      
      checkboxInput("Inner", label = "Inner", value = FALSE),
      
      sliderInput("Xrange",
                  "Range X:",
                  min = -5,
                  max = 5,
                  value = c(-1,1),step=0.01),
      menuItem("Gaussian", tabName = "Gaussian", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Gaussian",
              
              
              fluidRow(   
               
                box(width=10,title = "Standard normal distribution",plotOutput("dnormal"))
              #  valueBoxOutput("Statistic")
                
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
  
  
  
  
  
  output$dnormal = renderPlot({
    
    
    x<-seq(-BOUND,BOUND,by=0.001)
    P=pnorm(input$Xrange[2])-pnorm(input$Xrange[1])
    if (!input$Inner)
      P=1-P
    plot(x,dnorm(x), type="l",ylab="Normal density",main=paste("Prob=",P))
    abline(v=input$Xrange[1])
    abline(v=input$Xrange[2])
    
  })
  
 
  
}



shinyApp(ui, server)
