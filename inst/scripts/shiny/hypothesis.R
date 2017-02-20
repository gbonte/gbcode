

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Hypothesis testing"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 200,
                  value = 50,step=2),
      
      menuItem("Pvalue", tabName = "Pvalue", icon = icon("th")),
      menuItem("Alternative", tabName = "Alternative", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Pvalue",
              fluidRow(
                box(width=4,sliderInput("mean","Mean:",min = -BOUND1, max = BOUND1 ,
                                        value = 0),
                    sliderInput("sdev","St Dev:",min = 0.001,max = 2, value = 0.1)),
                column(dataTableOutput('mytable'),width=6)),
              
              fluidRow(   
                box(width=8,title = "Sampling Distribution Statistic",plotOutput("uniStudent"))
              #  valueBoxOutput("Statistic")
                
              )
      ), # tabItem
      tabItem(tabName = "Alternative",
              fluidRow(
                box(width=4,collapsible = TRUE,sliderInput("rot1","Rotation 1:", min = -3/2,max = 3/2, 
                                                           value = -0.75),
                    sliderInput("ax11","Axis1 1:",min = 0.01,max = BOUND2,value = 3,step=0.05),
                    sliderInput("ax21","Axis2 1:", min = 0.01, max = BOUND2, value = 0.15,step=0.05)
                ),
                box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("biPlotP", height = 300))),
              fluidRow(   
                box(width=5,title = "Sampling Distribution Mean",plotOutput("biSamplingM"), height = 300),
                box(width=5,title = "Sampling Distribution Covariance",plotOutput("biSamplingV", height = 300))
              )
      ) # tabItem
    )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  output$mytable = renderDataTable({
    D<<-array(rnorm(input$N,mean=input$mean,sd=input$sdev),c(input$N,1))
    Da=data.frame(D)
    colnames(Da)<-"Dataset"
    Da
  }, options = list( pageLength = 5,searching = FALSE,paging=TRUE))
  
  
  output$Statistic <- renderValueBox({
    input$N
    input$mean
    input$sdev
    valueBox(
      "Statistic", paste0(mean(D)), icon = icon(""),
      color = "purple"
    )
  })
    
  
  
  
  output$uniStudent <- renderPlot( {
    x=seq(-BOUND1,BOUND1,by=0.1)
    plot(dt(x,df=input$N),type="l")
    
  })
  
 
  
}



shinyApp(ui, server)
