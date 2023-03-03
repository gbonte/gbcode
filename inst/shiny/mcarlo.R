
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(latex2exp)
BOUND1<-5
BOUND2<-5
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Monte Carlo illustration of r.v. properties",titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of MC trials:",
                  min = 1000,
                  max = 50000,
                  value = 5000,step=2),
      menuItem("Transformation of a single RV", tabName = "RV1", icon = icon("th")),
      menuItem("Central limit demo", tabName = "CL", icon = icon("th")),
      menuItem("Operations on two RVs", tabName = "RV", icon = icon("th")),
      menuItem("Lin comb independent RVs", tabName = "NormalRV", icon = icon("th")),
      menuItem("Lin comb dependent RVs", tabName = "NormalRV2", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "RV1",
              fluidRow(
                box(width=4,sliderInput("rangeU","Range (red):",min = -BOUND1, max = BOUND1 ,
                                        value = c(-1,1),step=0.05)
                    
                ),
                box(width=6,title = "Distribution of r.v. z",collapsible = TRUE,plotOutput("uniPlotP1"))),
              
              fluidRow(   
                box(width=4,title = "Histogram Z^2",plotOutput("uniPowH1")),
                box(width=4,title = "Histogram Z^3",plotOutput("uniPow3H1")),
                box(width=4,title = "Histogram Sqrt(|Z|)",plotOutput("uniSqrtH1"))                
              )
      ),
      tabItem(tabName = "CL",
              fluidRow(
                box(width=4,sliderInput("rangeU2","Range (red):",min = -BOUND1, max = BOUND1 ,
                                        value = c(-1,1),step=0.05),
                    sliderInput("NN","Number terms:",min = 1,max = 100, value = 1,step=1)
                    
                ),
                box(width=6,title = "Distribution of a single term",collapsible = TRUE,plotOutput("uniPlotCL"))),
              
              fluidRow(   
                box(width=5,title = "Histogram sum",plotOutput("sumCL")),
                box(width=5,title = "Histogram mean",plotOutput("meanCL"))              
              )
      ),
      tabItem(tabName = "RV",
              fluidRow(
                box(width=4,sliderInput("rangeU3","Range (red):",min = -BOUND1, max = BOUND1 ,
                                        value = c(-1,1),step=0.01),
                    #sliderInput("variance1","Upper (red):",min = 0,max = BOUND1, value = 0.5),
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
                box(width=10,title = "Independent case: linear combination (a*red+b*green)",plotOutput("SumNP"))               
              )
      ),
      tabItem(tabName = "NormalRV2",
              fluidRow(
                box(width=4,sliderInput("DmeanN1",withMathJax(sprintf('$$\\mu_x:$$')),min = -BOUND1, max = BOUND1 ,
                                        value = -1,step=0.01),
                    sliderInput("DmeanN2",withMathJax(sprintf('$$\\mu_y:$$')),min = -BOUND1, max = BOUND1 ,
                                value = -1,step=0.01),
                    sliderInput("sx",withMathJax(sprintf('$$s_x:$$')),min = 0, max = BOUND1 ,
                                value = 1,step=0.01),
                    sliderInput("sy",withMathJax(sprintf('$$s_y:$$')),min = 0, max = BOUND1 ,
                                value = 1,step=0.01),
                    sliderInput("rot",withMathJax(sprintf('$$\\theta:$$')), min = -3/2,max = 3/2, 
                                value = -0.75),
                    sliderInput("Da","coefficient a:",min = -2,max = 2, value = 0.5,step=0.1),
                    sliderInput("Db","coefficient b:",min = -2,max = 2, value = 0.5,step=0.1)
                ),
                box(width=6,title = "Joint distribution",collapsible = TRUE,plotOutput("uniPlotNP2"))),
              
              fluidRow(   
                box(width=20,title = "Dependent case: linear combination",plotOutput("SumNP2"))               
              )
      ),
      tabItem(tabName = "about",
              fluidPage(
                includeHTML("about/about.random.html")))
    ))
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  output$uniPlotP1 <- renderPlot( {
    
    xaxis=seq(input$rangeU[1]-BOUND1,input$rangeU[2]+BOUND1,by=0.01)
    
    dred=dunif(xaxis,input$rangeU[1],input$rangeU[2])
    
    plot(xaxis, dred,col="red",type="l")
    
  })
  
  
  
  
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(min(input$rangeU3[1],input$mean2)-BOUND1,max(input$rangeU3[2],input$mean2)+BOUND1,by=0.01)
    
    dgreen=dnorm(xaxis,input$mean2,input$variance2)
    dred=dunif(xaxis,input$rangeU3[1],input$rangeU3[2])
    
    plot(xaxis, dgreen,col="green",type="l",ylim=c(0,max(c(dgreen,dred))))
    lines(xaxis,dred,col="red",type="l")
  })
  
  output$uniPowH1 <- renderPlot( {
    
    D1<-runif(input$N,input$rangeU[1],input$rangeU[2])
    D<-D1^2
    hist(D,xlim=c(0,BOUND2*BOUND2),freq=FALSE,main="")
    
    
  })
  
  output$uniPow3H1 <- renderPlot( {
    
    D1<-runif(input$N,input$rangeU[1],input$rangeU[2])
    D<-D1^3
    hist(D,xlim=c(-BOUND2^3,BOUND2^3),freq=FALSE,main="")
    
    
  })
  
  output$uniSqrtH1 <- renderPlot( {
    
    D1<-runif(input$N,input$rangeU[1],input$rangeU[2])
    D<-sqrt(abs(D1))
    hist(D,xlim=c(0,sqrt(1.5*BOUND2)),freq=FALSE,main="")
    
    
  })
  
  
  
  output$uniPlotCL <- renderPlot( {
    
    xaxis=seq(input$rangeU[1]-BOUND1,input$rangeU[2]+BOUND1,by=0.01)
    
    dred=dunif(xaxis,input$rangeU2[1],input$rangeU2[2])
    
    plot(xaxis, dred,col="red",type="l")
    
  })
  
  
  output$sumCL <- renderPlot( {
    
    
    D=runif(input$N,input$rangeU2[1],input$rangeU2[2])
    if (input$NN>1)
      for (i in 2:input$NN)
        D<-D+runif(input$N,input$rangeU2[1],input$rangeU2[2])
      vth=1/12*(input$rangeU2[2]-input$rangeU2[1])^2
      hist(D,freq=FALSE,main=paste("Th Var= ",round(vth*input$NN,2),
                                   "; MC Var=", round(var(D),2)))
      
  })
  
  output$meanCL <- renderPlot( {
    
    
    D=runif(input$N,input$rangeU2[1],input$rangeU2[2])
    
    if (input$NN>1)
      for (i in 2:input$NN)
        D<-D+runif(input$N,input$rangeU2[1],input$rangeU2[2])
      vth=1/12*((input$rangeU2[2]-input$rangeU2[1])^2)
      D=D/input$NN
      hist(D,xlim=c(-BOUND1,BOUND1),freq=FALSE,main=paste("Th Var= ",round(vth/input$NN,4),
                                                          "; MC Var=", round(var(D),4)))
      
  })
  
  output$uniSumH <- renderPlot( {
    
    D1<-runif(input$N,input$rangeU3[1],input$rangeU3[2])
    D2<-rnorm(input$N,input$mean2,input$variance2)
    
    D<<-D1+D2
    hist(D,xlim=c(-2*BOUND2,2*BOUND2),freq=FALSE,main="")
    
    
  })
  
  output$uniDiffH <- renderPlot( {
    
    D1<-runif(input$N,input$rangeU3[1],input$rangeU3[2])
    D2<-rnorm(input$N,input$mean2,input$variance2)
    
    D<<-D1-D2
    hist(D,xlim=c(-2*BOUND2,2*BOUND2),main="",freq=FALSE)
    
    
  })
  
  output$uniPowH <- renderPlot( {
    
    D1<-runif(input$N,input$rangeU3[1],input$rangeU3[2])
    D2<-rnorm(input$N,input$mean2,sqrt(input$variance2))
    
    D<<-D1*D2
    hist(D,xlim=c(-BOUND2*BOUND2,BOUND2*BOUND2),main="",freq=FALSE)
    
    
  })
  
  output$uniPlotD <- renderPlot( {
    xl=min(input$rangeU3[1],input$mean2)-BOUND1
    xu=max(input$rangeU3[2],input$mean2)+BOUND1
    
    plot(D,0*D,xlim=c(xl,xu))
    
    
  })
  
  output$uniPlotNP <- renderPlot( {
    
    xaxis=seq(min(input$meanN1,input$meanN2)-BOUND1,max(input$meanN1,input$meanN2)+BOUND1,by=0.01)
    
    dgreen=dnorm(xaxis,input$meanN2,sqrt(input$varianceN2))
    dred=dnorm(xaxis,input$meanN1,sqrt(input$varianceN1))
    
    plot(xaxis, dgreen,col="green",type="l",ylim=c(0,max(c(dgreen,dred))),ylab="density",xlab="")
    lines(xaxis,dred,col="red",type="l")
  })
  
  output$uniPlotNP2 <- renderPlot( {
    th=input$rot
    
    Rot<-array(c(cos(th), sin(th), -sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$sx, 0, 0, input$sy),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
   
    plot(ellipse(Sigma,centre=c(input$DmeanN1,input$DmeanN2)),
         type="l",main=paste("Var[x]=", 
                             round(Sigma[1,1],2),"Var[y]=", round(Sigma[2,2],2),
                             "\n Cov[x,y]=",round(Sigma[1,2],2)),xlim=c(-4,4),ylim=c(-4,4))
    
  })
  
  
  output$SumNP2 <- renderPlot( {
    
    th=input$rot
    
    Rot<-array(c(cos(th), sin(th), -sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$sx, 0, 0, input$sy),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    D1=rmvnorm(input$N,mean=c(input$DmeanN1, input$DmeanN2),sigma=Sigma)
    
    Dred=D1[,1] 
    Dgreen=D1[,2] 
    
    D=input$Da*Dred+input$Db*Dgreen
    meanD=mean(D)
    varD=var(D)
    
    hist(D,xlim=c(-BOUND2*BOUND2,BOUND2*BOUND2),
         main=TeX(sprintf("Th Mean= %.02f  ; MC Mean= %.02f ;  
                          Th $V(ax+by)=a^2 V (x) +b^2V (y)+2ab Cov(x,y)$=%.02f ;
                          MC Var= %.02f",
                          round(meanD,2),round(input$Da*input$DmeanN1+input$Db*input$DmeanN2,2),
                          round((input$Da^2)*Sigma[1,1]+(input$Db^2)*Sigma[2,2]+
                                  2*input$Da*input$Db*Sigma[1,2],2),
                          round(varD,2))),freq=FALSE)
    
  })
  
  
  output$SumNP <- renderPlot( {
    
    
    
    Dred=rnorm(input$N,input$meanN1,sqrt(input$varianceN1))
    Dgreen=rnorm(input$N,input$meanN2,sqrt(input$varianceN2))
    
    D=input$a*Dred+input$b*Dgreen
    meanD=mean(D)
    varD=var(D)
    
    hist(D,xlim=c(-BOUND2*BOUND2,BOUND2*BOUND2),
         main=paste("Th Mean=",round(input$a*input$meanN1+input$b*input$meanN2,2),
                    "; MC Mean=",round(meanD,2),
                    "\n Th Var=",round((input$a^2)*input$varianceN1+(input$b^2)*input$varianceN2,2),
                    "; MC Var=",round(varD,2)),freq=FALSE)
    
  })
  
  
}

shinyApp(ui, server)
