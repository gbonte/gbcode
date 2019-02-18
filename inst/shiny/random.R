
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
  dashboardHeader(title="InfoF422: random variates"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 100,
                  max = 1000,
                  value = 500,step=2),
      menuItem("Operations on one RV", tabName = "RV1", icon = icon("th")),
      menuItem("Central limit demo", tabName = "CL", icon = icon("th")),
      menuItem("Operations on two RVs", tabName = "RV", icon = icon("th")),
      menuItem("Linear combination of RVs", tabName = "NormalRV", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "RV1",
              fluidRow(
                box(width=4,sliderInput("low","Lower (red):",min = -BOUND1, max = 0 ,
                                        value = -1,step=0.01),
                    sliderInput("up","Upper (red):",min = 0,max = BOUND1, value = 0.5,step=0.01)
                    
                ),
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP1"))),
              
              fluidRow(   
                box(width=4,title = "Histogram Z^2",plotOutput("uniPowH1")),
                box(width=4,title = "Histogram Z^3",plotOutput("uniPow3H1")),
                box(width=4,title = "Histogram Sqrt(|Z|)",plotOutput("uniSqrtH1"))                
              )
      ),
      tabItem(tabName = "CL",
              fluidRow(
                box(width=4,sliderInput("lowC","Lower (red):",min = -BOUND1, max = 0 ,
                                        value = -1,step=0.01),
                    sliderInput("upC","Upper (red):",min = 0,max = BOUND1, value = 0.5,step=0.01),
                    sliderInput("NN","Number terms:",min = 1,max = 100, value = 1,step=1)
                    
                ),
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotCL"))),
              
              fluidRow(   
                box(width=5,title = "Histogram sum",plotOutput("sumCL")),
                box(width=5,title = "Histogram mean",plotOutput("meanCL"))              
              )
      ),
      tabItem(tabName = "RV",
              fluidRow(
                box(width=4,sliderInput("mean1","Lower (red):",min = -BOUND1, max = 0 ,
                                        value = -1,step=0.01),
                    sliderInput("variance1","Upper (red):",min = 0,max = BOUND1, value = 0.5),
                    sliderInput("mean2","Mean (green):",min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.01),
                    sliderInput("variance2","Variance (green):",min = 0.001,max = 2, value = 0.5)
                ),
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP"))),
              
              fluidRow(   
                box(width=4,title = "Histogram (red+green)",plotOutput("uniSumH")),
                box(width=4,title = "Histogram (red-green)",plotOutput("uniDiffH")),
                box(width=4,title = "Histogram (red*green)",plotOutput("uniPowH"))                
              )
      ),
      tabItem(tabName = "NormalRV",
              fluidRow(
                box(width=4,sliderInput("meanN1","Mean (red):",min = -BOUND1, max = BOUND1 ,
                                        value = -1,step=0.01),
                    sliderInput("varianceN1","Variance (red):",min = 0.001,max = 2, value = 0.5),
                    sliderInput("meanN2","Mean (green):",min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.01),
                    sliderInput("varianceN2","Variance (green):",min = 0.001,max = 2, value = 0.5),
                    sliderInput("a","coefficient a:",min = -2,max = 2, value = 0.5,step=0.1),
                    sliderInput("b","coefficient b:",min = -2,max = 2, value = 0.5,step=0.1)
                ),
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotNP"))),
              
              fluidRow(   
                box(width=10,title = "Linear combination (a*red+b*green)",plotOutput("SumNP"))               
              )
      )
    )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  output$uniPlotP1 <- renderPlot( {
    
    xaxis=seq(input$low-BOUND1,input$up+BOUND1,by=0.01)
    
    dred=dunif(xaxis,input$low,input$up)
    
    plot(xaxis, dred,col="red",type="l")
    
  })
  
  
  
  
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(min(input$mean1,input$mean2)-BOUND1,max(input$mean1,input$mean2)+BOUND1,by=0.01)
    
    dgreen=dnorm(xaxis,input$mean2,input$variance2)
    dred=dunif(xaxis,input$mean1,input$variance1)
    
    plot(xaxis, dgreen,col="green",type="l",ylim=c(0,max(c(dgreen,dred))))
    lines(xaxis,dred,col="red",type="l")
  })
  
  output$uniPowH1 <- renderPlot( {
    
    D1<-runif(input$N,input$low,input$up)
    D<-D1^2
    hist(D,xlim=c(0,BOUND2*BOUND2),freq=FALSE,main="")
    
    
  })
  
  output$uniPow3H1 <- renderPlot( {
    
    D1<-runif(input$N,input$low,input$up)
    D<-D1^3
    hist(D,xlim=c(-BOUND2^3,BOUND2^3),freq=FALSE,main="")
    
    
  })
  
  output$uniSqrtH1 <- renderPlot( {
    
    D1<-runif(input$N,input$low,input$up)
    D<-sqrt(abs(D1))
    hist(D,xlim=c(0,sqrt(1.5*BOUND2)),freq=FALSE,main="")
    
    
  })
  
  
  
  output$uniPlotCL <- renderPlot( {
    
    xaxis=seq(input$low-BOUND1,input$up+BOUND1,by=0.01)
    
    dred=dunif(xaxis,input$lowC,input$upC)
    
    plot(xaxis, dred,col="red",type="l")
    
  })
  
  
  output$sumCL <- renderPlot( {
    
    
    D=runif(input$N,input$lowC,input$upC)
    if (input$NN>1)
      for (i in 2:input$NN)
        D<-D+runif(input$N,input$lowC,input$upC)
      vth=1/12*(input$upC-input$lowC)^2
    hist(D,freq=FALSE,main=paste("Th Var= ",round(vth*input$NN,2),
                                         "Sample Var=", round(var(D),2)))
    
  })
  
  output$meanCL <- renderPlot( {
    
      
    D=runif(input$N,input$lowC,input$upC)
    
    if (input$NN>1)
      for (i in 2:input$NN)
        D<-D+runif(input$N,input$lowC,input$upC)
    vth=1/12*((input$upC-input$lowC)^2)
    D=D/input$NN
    hist(D,xlim=c(-BOUND1,BOUND1),freq=FALSE,main=paste("Th Var= ",round(vth/input$NN,4),
                                                                 "Sample Var=", round(var(D),4)))
    
  })
  
  output$uniSumH <- renderPlot( {
    
    D1<-runif(input$N,input$mean1,input$variance1)
    D2<-rnorm(input$N,input$mean2,input$variance2)
    
    D<<-D1+D2
    hist(D,xlim=c(-2*BOUND2,2*BOUND2),freq=FALSE,main="")
    
    
  })
  
  output$uniDiffH <- renderPlot( {
    
    D1<-runif(input$N,input$mean1,input$variance1)
    D2<-rnorm(input$N,input$mean2,input$variance2)
    
    D<<-D1-D2
    hist(D,xlim=c(-2*BOUND2,2*BOUND2),main="",freq=FALSE)
    
    
  })
  
  output$uniPowH <- renderPlot( {
    
    D1<-runif(input$N,input$mean1,input$variance1)
    D2<-rnorm(input$N,input$mean2,sqrt(input$variance2))
    
    D<<-D1*D2
    hist(D,xlim=c(-BOUND2*BOUND2,BOUND2*BOUND2),main="",freq=FALSE)
    
    
  })
  
  output$uniPlotD <- renderPlot( {
    xl=min(input$mean1,input$mean2)-BOUND1
    xu=max(input$mean1,input$mean2)+BOUND1
    input$variance1+input$variance2+input$p1libra
    input$N
    
    plot(D,0*D,xlim=c(xl,xu))
    
    
  })
  
  output$uniPlotNP <- renderPlot( {
    
    xaxis=seq(min(input$meanN1,input$meanN2)-BOUND1,max(input$meanN1,input$meanN2)+BOUND1,by=0.01)
    
    dgreen=dnorm(xaxis,input$meanN2,sqrt(input$varianceN2))
    dred=dnorm(xaxis,input$meanN1,sqrt(input$varianceN1))
    
    plot(xaxis, dgreen,col="green",type="l",ylim=c(0,max(c(dgreen,dred))))
    lines(xaxis,dred,col="red",type="l")
  })
  
  output$SumNP <- renderPlot( {
    
    
    
    Dred=rnorm(input$N,input$meanN1,sqrt(input$varianceN1))
    Dgreen=rnorm(input$N,input$meanN2,sqrt(input$varianceN2))
    
    D=input$a*Dred+input$b*Dgreen
    meanD=mean(D)
    varD=var(D)
    
    hist(D,xlim=c(-BOUND2*BOUND2,BOUND2*BOUND2),
         main=paste("Th Mean=",round(input$a*input$meanN1+input$b*input$meanN2,2),
                    "Sample Mean=",round(meanD,2),
                    "; Th Var=",round((input$a^2)*input$varianceN1+(input$b^2)*input$varianceN2,2),
                    " Sample Var=",round(varD,2)),freq=FALSE)
    
  })
  
  
}

shinyApp(ui, server)
