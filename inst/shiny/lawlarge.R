

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Frequency"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 50000,
                  value = 50,step=2),
      
      sliderInput("P",
                  "Probability of success:",
                  min = 0,
                  max = 1,
                  value = 0.5,step=0.05),
      actionButton("sample", label = "Sample"),
      checkboxInput("Normalized", label = "Normalized", value = FALSE),
      sliderInput("Yrange",
                  "Range Y:",
                  min = 0,
                  max = 1,
                  value = c(0,1),step=0.01),
      sliderInput("Xrange",
                  "Range X:",
                  min = 0,
                  max = 100,
                  value = c(0,100),step=1),
      menuItem("Large numbers", tabName = "Pvalue", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Pvalue",
              
              
              fluidRow(   
                box(width=8,title = "Frequency of success",plotOutput("frequency"))),
              fluidRow( box(width=8,title = "(number successes) - (number failures)",plotOutput("headstails"))
              #  valueBoxOutput("Statistic")
                
              )
      )
    )
  )
) # ui

d<-NULL
ns<-NULL
BOUND<-150
sX<-NULL
require(statmod)
server<-function(input, output,session) {
  
  
  
  observeEvent(input$N,  {
    updateSliderInput(session = session, inputId = "Xrange", max = input$N, value = c(input$Xrange[1],input$N))
  })
  
  
  output$frequency = renderPlot({
    
    N<-input$N
    input$sample
    X<-rbinom(N,1,prob=input$P) ##sample(c(0,1),N,prob=c(1-input$P,input$P),rep=TRUE)
    sX<<-cumsum(X)
    
    
    f<-NULL
    d<<-NULL
    ns<<-NULL
    for (i in 1:length(sX)){
      f<-c(f,sX[i]/i)
      d<<-c(d,2*sX[i]-i) ## # successes= sum(X[1:i]), # fails= i- sum(X[1:i])
      ns<<-c(ns,(2*sX[i]-i)/i)
    }
    eps=pinvgauss(1.9999/2,mean=0)*sqrt(input$N*input$P*(1-input$P))/input$N
      
    plot(1:(length(f)),f, type="l",ylim=input$Yrange,xlab="Number of trials",ylab="Frequency",xlim=input$Xrange)
    lines(1:(length(f)),numeric(length(f))+input$P,col="red")
    lines(1:(length(f)),numeric(length(f))+input$P+eps,col="green")
    lines(1:(length(f)),numeric(length(f))+input$P-eps,col="green")
    
  })
  
  
  output$headstails = renderPlot({
    input$sample
   input$N
    input$P
    if (!input$Normalized) {
      plot(d,ylim=c(min(d),max(d)),type="l",xlim=input$Xrange)
    }else {
      plot(1:input$N,ns,ylim=c(-1,1),type="l",xlim=input$Xrange)
      lines(1:input$N,numeric(input$N)+2*input$P-1,col="red") 
    }
  })
 
  
}



shinyApp(ui, server)
