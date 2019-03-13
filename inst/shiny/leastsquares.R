library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)
library(latex2exp)
BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Least squares"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 200,
                  value = 50,step=2),
      sliderInput("b1",withMathJax(sprintf('$$\\beta_1:$$')), min = -BOUND2,max = BOUND2, 
                  value = 1,step=0.01),
      sliderInput("b0",withMathJax(sprintf('$$\\beta_0:$$')), min = -BOUND2,max = BOUND2, 
                  value = 1,step=0.01), 
      #sliderInput("ord","Target Function:", min = -2,max = 2, 
       #           value = 1,step=1),
      sliderInput("nvdw","Cond var:", min = 0.01,max = 0.3, 
                  value = 0.1,step=0.01),
      sliderInput("tdt",
                  "3D theta:",
                  min = -60,
                  max = 60,
                  value = 30,step=5),
      sliderInput("tdp",
                  "3D phi:",
                  min = 0,
                  max = 90,
                  value = 10,step=1),
      menuItem("Least squares", tabName = "LS", icon = icon("th"))
    ) # sidebar Menu
  ), # dashboard sidebar
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "LS",
              fluidRow(box(width=3,collapsible = TRUE,
                           
                           sliderInput("m",withMathJax(sprintf('$$\\hat{\\beta}_1:$$')), min = -BOUND2,max = BOUND2, 
                                       value = 0,step=0.01),
                           sliderInput("q",withMathJax(sprintf('$$\\hat{\\beta}_0:$$')), min = -BOUND2,max = BOUND2, 
                                       value = 0,step=0.01)),
                           box(width=6,collapsible = TRUE,title = "Data set",plotOutput("Data", height = 300)) 
                       ##  box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("nlinearPlotP", height = 400))
              ),## fluidRow
              fluidRow(
                          box(width=9,collapsible = TRUE,title = "Residual sum of squares",
                              plotOutput("EmpErr", height = 500)))
              
      )
    ) ## tabItems
  )# DashboardBody
) # ui dashboardPage

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  f<-function(x,ord){
    f<-numeric(length(x))
    if (ord==-1)
      f<-sin(x)
    if (ord==-2)
      f<-cos(2*x)
    if (ord==-3)
      f<-cos(5*x)
    if (ord==-4)
      f<-sin(10*x)
    if (ord>=1)
      for (o in 1:ord)
        f<-f+x^o
      
      
      
      f
  }
  
  hyp<-function(X,Y, Xts, h){
    X.tr<-NULL
    X.ts<-NULL
    N<-length(Xts)
    if (h==0){
      
      return(numeric(N)+mean(Y))
    }
    for (ord in 1:h){
      X.tr<-cbind(X.tr,X^ord)
      X.ts<-cbind(X.ts,Xts^ord)
    }
    
    p<-h+1
    
    DN<-data.frame(cbind(Y,X.tr))
    
    mod<-lm(Y~.,DN)
    Dts=data.frame(X.ts)
    colnames(Dts)=colnames(DN)[2:(h+1)]
    return(predict(mod,Dts))
  }
  
  
  
  
  
  
  
  output$Data <- renderPlot( {
    
    
    Xtr=runif(input$N,-BOUND2, BOUND2)
    set.seed(0)
    Ytr=input$b1*Xtr+input$b0+rnorm(input$N,0,sd=sqrt(input$nvdw))
    
    yhat=input$m*Xtr+input$q
    
    x.hat<-mean(Xtr)
    S.xx<-sum((Xtr-x.hat)^2)
    
    y.hat<-mean(Ytr)
    S.xy<-sum((Xtr-x.hat)*Ytr)
    
    beta.hat.1<-S.xy/S.xx
    beta.hat.0<-y.hat-beta.hat.1*x.hat
    plot(Xtr,Ytr,main=TeX(paste(sprintf("Least-squares $\\hat{\\beta}_1 = %.02f %s \\hat{\\beta}_0 = %.02f$ ", beta.hat.1, ";    ", beta.hat.0))),
         xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2))
    # , 
    #plot(Xtr,Ytr,main=paste("LS estimates: betahat1=", round(beta.hat.1,2), ": betahat0=", round(beta.hat.0,2)),
     #    xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2))
    lines(Xtr,yhat,col="red")
    lines(Xtr,beta.hat.1*Xtr+beta.hat.0,col="green")
  })
  
  
  output$EmpErr <- renderPlot( {
    
    
    Xtr=runif(input$N,-BOUND2, BOUND2)
    set.seed(0)
    Ytr=input$b1*Xtr+input$b0+rnorm(input$N,0,sd=sqrt(input$nvdw))
    
    
    maxis=seq(-BOUND1/2,BOUND1/2,by=0.05)
    qaxis=seq(-BOUND1/2,BOUND1/2,by=0.05)
    
    EE<-array(0,c(length(maxis),length(qaxis)))
    for (j in 1:length(maxis)){
      for (k in 1:length(qaxis)){
        yhat=maxis[j]*Xtr+qaxis[k]
        EE[j,k]=sum((Ytr-yhat)^2)
        
      }
    }
    
    Emq=0
    yhat=input$m*Xtr+input$q
    Emq=sum((Ytr-yhat)^2)
    
    x.hat<-mean(Xtr)
    S.xx<-sum((Xtr-x.hat)^2)
    
    y.hat<-mean(Ytr)
    S.xy<-sum((Xtr-x.hat)*Ytr)
    
    beta.hat.1<-S.xy/S.xx
    beta.hat.0<-y.hat-beta.hat.1*x.hat
    
    
    op <- par(bg = "white")
    
    surface<-persp(maxis, qaxis, EE, 
                   theta = input$tdt, phi =input$tdp, expand = 0.5, xlim=c(min(maxis),max(maxis)),
                   ylim=c(min(qaxis),max(qaxis)),
                         main=paste("Err=",round(Emq,2), "; LS Err=",round(min(EE),2)))
    
    points (trans3d(x=input$m, 
                    y = input$q, z = Emq, pmat = surface), col = "red",lwd=8)
    points (trans3d(x=beta.hat.1, 
                    y = beta.hat.0, z = min(c(EE)), pmat = surface), col = "green",lwd=8)
    # lines (trans3d(y=input$varhatL2, 
    #               x = seq(-BOUND2, BOUND2, by= .2), z =elogLik, pmat = surface), col = "green",lwd=1)
    #plot(xaxis,logLik,type="l",main=paste("logLik=",round(elogLik,2)))
    #abline(v=input$meanhatL2,col="red")
    
  })
  
  
  
  
  
  
  
 
 
  
}



shinyApp(ui, server)
