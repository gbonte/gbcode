library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)
library(gbcode)
BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Regression"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 200,
                  value = 50,step=2),
      sliderInput("ord","Target Function:", min = -4,max = 4, 
                  value = 1,step=1),
      sliderInput("nvdw","Cond var:", min = 0.1,max = 1, 
                  value = 0.25,step=0.01),
      sliderInput("nrx","x:", min = -BOUND2, max = BOUND2, value = 0.15,step=0.05),
      sliderInput("R",
                  "Number of trials:",
                  min = 100,
                  max = 5000,
                  value = 500,step=2),
      
      menuItem("Polynomial fitting", tabName = "Nonlinear", icon = icon("th")),
      menuItem("Local constant fitting", tabName = "LC", icon = icon("th")),
      menuItem("Local linear fitting", tabName = "LL", icon = icon("th")),
      menuItem("Random Forest fitting", tabName = "RF", icon = icon("th"))
    ) # sidebar Menu
  ), # dashboard sidebar
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Nonlinear",
              fluidRow(box(width=4,collapsible = TRUE,
                           
                           sliderInput("h","Degree polynomial hypothesis:", min = 0,max = 10, 
                                       value = 1,step=1)) 
                     ##  box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("nlinearPlotP", height = 400))
                       ),## fluidRow
              fluidRow(   box(width=6,collapsible = TRUE,title = "Sampling distribution",plotOutput("nlinearBV", height = 300)),
                          box(width=6,collapsible = TRUE,title = "Conditional sampling distribution",plotOutput("nlinearCond", height = 300)))
              
      ), ## tabItem
      tabItem(tabName = "LC",
              fluidRow(box(width=4,collapsible = TRUE,
                           
                           sliderInput("K","Number neighbors:", min = 3,max = 10, 
                                       value = 3,step=1)) 
                       ##box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("nLLPlotP", height = 400))
                       ),## fluidRow
              fluidRow(   box(width=6,collapsible = TRUE,title = "Sampling distribution",plotOutput("nLCBV", height = 300)),
                          box(width=6,collapsible = TRUE,title = "Conditional sampling distribution",plotOutput("nLCCond", height = 300)))
              
      ), ## tabItem
      tabItem(tabName = "LL",
              fluidRow(box(width=4,collapsible = TRUE,
                           
                           sliderInput("KK","Number neighbors:", min = 3,max = 10, 
                                       value = 3,step=1)) 
                       ##box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("nLLPlotP", height = 400))
              ),## fluidRow
              fluidRow(   box(width=6,collapsible = TRUE,title = "Sampling distribution",plotOutput("nLLBV", height = 300)),
                          box(width=6,collapsible = TRUE,title = "Conditional sampling distribution",plotOutput("nLLCond", height = 300)))
              
      ),
      tabItem(tabName = "RF",
              fluidRow(box(width=4,collapsible = TRUE,
                           
                           sliderInput("TREE","Number trees:", min = 10,max = 100, 
                                       value = 3,step=10)) 
                       ##box(width=8,title = "Distribution",collapsible = TRUE,plotOutput("nLLPlotP", height = 400))
              ),## fluidRow
              fluidRow(   box(width=6,collapsible = TRUE,title = "Sampling distribution",plotOutput("nRFBV", height = 300)),
                          box(width=6,collapsible = TRUE,title = "Conditional sampling distribution",plotOutput("nRFCond", height = 300)))
              
      ) ## tabItem
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
  
  
 
  
  
  
  output$linearPlotP <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= input$dx)
    y <- seq(-BOUND2, BOUND2, by= input$dy)
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    
    
    muy<-input$q+input$m*x
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-dnorm(y[j],mean=muy[i],sd=sqrt(input$vdw))
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    surface<-persp3D(x, y, prob.z, theta = input$tdt, phi =input$tdp, expand = 0.5, col = "blue",facets=FALSE)
    
    
    
    
  })
  
  

  
 
  output$nlinearBV <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=f(X,input$ord)
    x.hat=mean(X)
    Y.hat<-array(NA,c(input$R,input$N))
    E.hat<-array(NA,c(input$R,input$N))
    
    beta.hat.1<-numeric(input$R)
    beta.hat.0<-numeric(input$R)
    var.hat.w<-numeric(input$R)
    plot(X,muy,xlim=c(-BOUND2,BOUND2),type="n")
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r,]<-hyp(Xtr,Y,X,input$h)
      E.hat[r,]=muy-Y.hat[r,]
      lines(X,Y.hat[r,])
    }
    
    meanY.hat=apply(Y.hat,2,mean)
    bias=muy- meanY.hat
    avg.bias2=mean(bias^2)
    varY.hat=apply(Y.hat,2,var)
    avg.var=mean(varY.hat)
    mseY.hat=apply(E.hat^2,2,mean)
    avg.mse=mean(mseY.hat)
    
    bvtitle=paste("Bias^2=", round(avg.bias2,2), "Var=", round(avg.var,2), "MSE=", round(avg.mse,2) )
    title(bvtitle)
    lines(X,muy,lwd=4,col="blue")
    lines(X,meanY.hat,lwd=4,col="green")
    abline(v=input$nrx,  col = "red",lwd=1)
    
  })
  
  output$nlinearCond <- renderPlot( {
    Y.hat<-numeric(input$R)  
    
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r]<-hyp(Xtr,Y,input$nrx,input$h)
      
      
    }
    
    
    
    Yq=f(input$nrx,input$ord)
    bvtitle=paste("Bias^2=", round((Yq-mean(Y.hat))^2,2), "Var=", round(var(Y.hat),2), "MSE=", round(mean((Yq-Y.hat)^2),2) )
    hist(Y.hat,xlim=c(min(c(Y.hat,Yq)),max(c(Y.hat,Yq))),main=bvtitle)
    abline(v=Yq,col="blue",lwd=3)
    abline(v=mean(Y.hat),  col = "green",lwd=3)
    
  })
  
  
  
  output$nLCBV <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=f(X,input$ord)
    x.hat=mean(X)
    Y.hat<-array(NA,c(input$R,input$N))
    E.hat<-array(NA,c(input$R,input$N))
    
    beta.hat.1<-numeric(input$R)
    beta.hat.0<-numeric(input$R)
    var.hat.w<-numeric(input$R)
    plot(X,muy,xlim=c(-BOUND2,BOUND2),type="n")
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r,]<-pred("lazy",Xtr,Y,X,conPar=c(input$K,input$K),cmbPar=1,class=FALSE)
      E.hat[r,]=muy-Y.hat[r,]
      lines(X,Y.hat[r,])
    }
    
    meanY.hat=apply(Y.hat,2,mean)
    bias=muy- meanY.hat
    avg.bias2=mean(bias^2)
    varY.hat=apply(Y.hat,2,var)
    avg.var=mean(varY.hat)
    mseY.hat=apply(E.hat^2,2,mean)
    avg.mse=mean(mseY.hat)
    
    bvtitle=paste("Bias^2=", round(avg.bias2,2), "Var=", round(avg.var,2), "MSE=", round(avg.mse,2) )
    title(bvtitle)
    lines(X,muy,lwd=4,col="blue")
    lines(X,meanY.hat,lwd=4,col="green")
    abline(v=input$nrx,  col = "red",lwd=1)
    
  })
  
  
  output$nLCCond <- renderPlot( {
    Y.hat<-numeric(input$R)  
    
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r]<-pred("lazy",Xtr,Y,input$nrx,conPar=c(input$K,input$K),cmbPar=1,class=FALSE)
      
    }
    
    
    
    Yq=f(input$nrx,input$ord)
    bvtitle=paste("Bias^2=", round((Yq-mean(Y.hat))^2,2), "Var=", round(var(Y.hat),2), "MSE=", round(mean((Yq-Y.hat)^2),2) )
    hist(Y.hat,xlim=c(min(c(Y.hat,Yq)),max(c(Y.hat,Yq))),main=bvtitle)
    abline(v=Yq,col="blue",lwd=3)
    abline(v=mean(Y.hat),  col = "green",lwd=3)
    
  })
  
  
  output$nLLBV <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=f(X,input$ord)
    x.hat=mean(X)
    Y.hat<-array(NA,c(input$R,input$N))
    E.hat<-array(NA,c(input$R,input$N))
    
    beta.hat.1<-numeric(input$R)
    beta.hat.0<-numeric(input$R)
    var.hat.w<-numeric(input$R)
    plot(X,muy,xlim=c(-BOUND2,BOUND2),type="n")
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r,]<-pred("lazy",Xtr,Y,X,conPar=c(input$KK,input$KK),cmbPar=1,class=FALSE)
      E.hat[r,]=muy-Y.hat[r,]
      lines(X,Y.hat[r,])
    }
    
    meanY.hat=apply(Y.hat,2,mean)
    bias=muy- meanY.hat
    avg.bias2=mean(bias^2)
    varY.hat=apply(Y.hat,2,var)
    avg.var=mean(varY.hat)
    mseY.hat=apply(E.hat^2,2,mean)
    avg.mse=mean(mseY.hat)
    
    bvtitle=paste("Bias^2=", round(avg.bias2,2), "Var=", round(avg.var,2), "MSE=", round(avg.mse,2) )
    title(bvtitle)
    lines(X,muy,lwd=4,col="blue")
    lines(X,meanY.hat,lwd=4,col="green")
    abline(v=input$nrx,  col = "red",lwd=1)
    
  })
  
  
  output$nLLCond <- renderPlot( {
    Y.hat<-numeric(input$R)  
    
    for (r in 1:input$R){
      Xtr=rnorm(input$N)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r]<-pred("lazy",Xtr,Y,input$nrx,conPar=c(input$KK,input$KK),cmbPar=1,class=FALSE)
      
    }
    
    
    
    Yq=f(input$nrx,input$ord)
    bvtitle=paste("Bias^2=", round((Yq-mean(Y.hat))^2,2), "Var=", round(var(Y.hat),2), "MSE=", round(mean((Yq-Y.hat)^2),2) )
    hist(Y.hat,xlim=c(min(c(Y.hat,Yq)),max(c(Y.hat,Yq))),main=bvtitle)
    abline(v=Yq,col="blue",lwd=3)
    abline(v=mean(Y.hat),  col = "green",lwd=3)
    
  })
  
  
  output$nRFBV <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=f(X,input$ord)
    x.hat=mean(X)
    Y.hat<-array(NA,c(input$R,input$N))
    E.hat<-array(NA,c(input$R,input$N))
    
    beta.hat.1<-numeric(input$R)
    beta.hat.0<-numeric(input$R)
    var.hat.w<-numeric(input$R)
    plot(X,muy,xlim=c(-BOUND2,BOUND2),type="n")
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
     
      Y.hat[r,]<-pred("rf",Xtr,Y,X,class=FALSE,ntree=input$TREE)
      E.hat[r,]=muy-Y.hat[r,]
      lines(X,Y.hat[r,])
    }
    
    meanY.hat=apply(Y.hat,2,mean)
    bias=muy- meanY.hat
    avg.bias2=mean(bias^2)
    varY.hat=apply(Y.hat,2,var)
    avg.var=mean(varY.hat)
    mseY.hat=apply(E.hat^2,2,mean)
    avg.mse=mean(mseY.hat)
    
    bvtitle=paste("Bias^2=", round(avg.bias2,2), "Var=", round(avg.var,2), "MSE=", round(avg.mse,2) )
    title(bvtitle)
    lines(X,muy,lwd=4,col="blue")
    lines(X,meanY.hat,lwd=4,col="green")
    abline(v=input$nrx,  col = "red",lwd=1)
    
  })
  
  
  output$nRFCond <- renderPlot( {
    Y.hat<-numeric(input$R)  
    
    for (r in 1:input$R){
      Xtr=runif(input$N,-BOUND2, BOUND2)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=sqrt(input$nvdw))
      
      Y.hat[r]<-pred("rf",Xtr,Y,input$nrx,class=FALSE,ntree=input$TREE)
      
    }
    
    
    
    Yq=f(input$nrx,input$ord)
    bvtitle=paste("Bias^2=", round((Yq-mean(Y.hat))^2,2), "Var=", round(var(Y.hat),2), "MSE=", round(mean((Yq-Y.hat)^2),2) )
    hist(Y.hat,xlim=c(min(c(Y.hat,Yq)),max(c(Y.hat,Yq))),main=bvtitle)
    abline(v=Yq,col="blue",lwd=3)
    abline(v=mean(Y.hat),  col = "green",lwd=3)
    
  })
  
}



shinyApp(ui, server)
