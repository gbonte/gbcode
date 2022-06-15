library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)
library(latex2exp)
BOUND1<-5
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: Parameter identification and validation", titleWidth = 1000),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 2,
                  max = 200,
                  value = 50,step=2),
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
      menuItem("Linear Least squares", tabName = "LS", icon = icon("th")),
      menuItem("NNET LS ", tabName = "NLS", icon = icon("th")),
      menuItem("KNN CV ", tabName = "CV", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("question"))
    ) # sidebar Menu
  ), # dashboard sidebar
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "LS",
              fluidRow(column(4,
                              sliderInput("b1",withMathJax(sprintf('$$\\beta_1:$$')), min = -BOUND2,max = BOUND2, 
                                          value = 1,step=0.01),
                              sliderInput("b0",withMathJax(sprintf('$$\\beta_0:$$')), min = -BOUND2,max = BOUND2, 
                                          value = -1,step=0.01)),
                       column(4,
                              sliderInput("m",withMathJax(sprintf('$$\\hat{\\beta}_1:$$')), min = -BOUND2,max = BOUND2, 
                                          value = 0,step=0.01),
                              sliderInput("q",withMathJax(sprintf('$$\\hat{\\beta}_0:$$')), min = -BOUND2,max = BOUND2, 
                                          value = 0,step=0.01)),
                       column(4,    sliderInput("eta",withMathJax(sprintf('$${\\eta}:$$')), min = 0.0001,max = 1, 
                                                value = 0.01,step=0.01)),
                       actionButton("do", "Gradient step"),
                       
                       box(width=6,collapsible = TRUE,title = "Data set",plotOutput("Data", height = 300)), 
                       box(width=6,collapsible = TRUE,title = "Residual sum of squares",
                           plotOutput("EmpErr", height = 300)))
              
      ),
      tabItem(tabName = "NLS",
              fluidRow(box(width=6,collapsible = TRUE,title = "NNET 1 hidden layer, 3 hidden nodes",
                           sliderInput("NLsteps","# steps", min = 10,max = 5000, 
                                       value = 100,step=10),
                           sliderInput("NLeta",withMathJax(sprintf('$${\\eta}:$$')), min = 0.0001,max = 1, 
                                       value = 0.1,step=0.01)),
                       actionButton("NLdo", "Gradient step NNet"),
                       actionButton("NLreset", "Reset weights")),
              fluidRow(box(width=6,collapsible = TRUE,title = "Data set",plotOutput("NLData", height = 300)),
                       box(width=6,collapsible = TRUE,title = "Residual sum of squares",
                           plotOutput("NLEmpErr", height = 300)))
              
      ),
      tabItem(tabName = "CV",
              fluidRow(box(width=6,collapsible = TRUE,title = "KNN CV",
                           sliderInput("folds","# folds", min = 2,max = 10, 
                                       value = 5,step=1),
                           sliderInput("neigh","# neighbours", min = 1,max = 10, 
                                       value = 3,step=1)),
                       actionButton("CVdo", "CV step")),
              fluidRow(box(width=6,collapsible = TRUE,title = "Data set",plotOutput("CVData", height = 300)),
                       box(width=6,collapsible = TRUE,title = "CV sum of squares",
                           plotOutput("CVErr", height = 300)))
              
      ),
      tabItem(tabName = "about",
              fluidPage(
                includeHTML("about/about.leastsquares.html")
              ))
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
  
  sigmoid <- function(z){1.0/(1.0+exp(-z))}
  
  # Partial derivative of activation function
  sigmoid_prime <- function(z){sigmoid(z)*(1-sigmoid(z))}
  
  
  ### W= w01.1, w11.1, w02.1, w12.1, w03.1, w13.1,  w11.2, w21.2,w31.2, B
  
  nnetf<-function(x,W){
    
    a1<-W[2]*x+ W[1]
    z1<-sigmoid(a1)
    a2<-W[4]*x+ W[3]
    z2<-sigmoid(a2)
    a3<-W[6]*x+ W[5]
    z3<-sigmoid(a3)
    
    y=W[7]*z1+W[8]*z2+W[9]*z3 +W[10]
    y
    
  }
  
  ### W= w01.1, w11.1, w02.1, w12.1,  w11.2, w21.2, B
  gradnet<-function(X,E,W){
    
    G<-numeric(length(W))
    N<-length(X)
    for (i in 1:N){
      x=X[i]
      e=E[i]
      a1<-W[2]*x+ W[1]
      z1<-sigmoid(a1)
      a2<-W[4]*x+ W[3]
      z2<-sigmoid(a2)
      
      a3<-W[6]*x+ W[5]
      z3<-sigmoid(a3)
      
      
      gw01.1=W[7]*sigmoid_prime(a1)
      gw11.1=W[7]*sigmoid_prime(a1)*x
      
      gw02.1=W[8]*sigmoid_prime(a2)
      gw12.1=W[8]*sigmoid_prime(a2)*x
      
      gw03.1=W[9]*sigmoid_prime(a3)
      gw13.1=W[9]*sigmoid_prime(a3)*x
      
      
      gw11.2=z1
      gw21.2=z2
      gw31.2=z3
      
      gB=1
      
      G<-G-2*e*c(gw01.1,gw11.1,gw02.1,gw12.1,gw03.1, gw13.1 ,
                 gw11.2, gw21.2 , gw31.2, gB)
    }
    
    G/N
  }
  W<-reactiveValues(W=rnorm(10))
  allE<-reactiveValues(allE=NULL)
  CV<-reactiveValues(k=0,I.k.tr=NULL,I.k.ts=NULL,E=NULL,I=NULL )
  
  Xtr<-reactive({allE$allE<-NULL
  CV$I.k.tr=1:input$N
  CV$I=sample(input$N)
  CV$I.k.ts= NULL
  CV$E=numeric(input$N)
  sort(runif(input$N,-BOUND2, BOUND2))})
  Ytr<-reactive({
    input$b1*Xtr()+input$b0+rnorm(input$N,0,sd=sqrt(input$nvdw))})
  
  Ytr2<-reactive({allE$allE<-NULL
  sin(pi*Xtr())+rnorm(input$N,sd=sqrt(input$nvdw)) })
  
  output$Data <- renderPlot( {
    
    yhat=input$m*Xtr()+input$q
    
    x.hat<-mean(Xtr())
    S.xx<-sum((Xtr()-x.hat)^2)
    
    y.hat<-mean(Ytr())
    S.xy<-sum((Xtr()-x.hat)*Ytr())
    
    beta.hat.1<-S.xy/S.xx
    beta.hat.0<-y.hat-beta.hat.1*x.hat
    plot(Xtr(),Ytr(),
         main=TeX(paste(sprintf("Least-squares $\\hat{\\beta}_1 = %.02f %s \\hat{\\beta}_0 = %.02f$ ", beta.hat.1, ";    ", beta.hat.0))),
         xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2),xlab="x",ylab="y")
    
    #plot(Xtr,Ytr,main=paste("LS estimates: betahat1=", round(beta.hat.1,2), ": betahat0=", round(beta.hat.0,2)),
    #    xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2))
    lines(Xtr(),yhat,col="red",lwd=2)
    lines(Xtr(),beta.hat.1*Xtr()+beta.hat.0,col="green",lwd=2)
  })
  
  observeEvent(input$do,{
    Yhat=input$q+input$m*Xtr()
    e=Yhat-Ytr()
    grad0=-2*mean(e)
    grad1=-2*mean(e*Xtr())
    eta=input$eta
    updateSliderInput(session, "q", value =input$q+eta*grad0 )
    updateSliderInput(session, "m", value =input$m+eta*grad1 )
  })
  
  
  
  output$EmpErr <- renderPlot( {
    
    
    
    maxis=seq(-BOUND1/2,BOUND1/2,by=0.05)
    qaxis=seq(-BOUND1/2,BOUND1/2,by=0.05)
    
    EE<-array(0,c(length(maxis),length(qaxis)))
    for (j in 1:length(maxis)){
      for (k in 1:length(qaxis)){
        yhat=maxis[j]*Xtr()+qaxis[k]
        EE[j,k]=sum((Ytr()-yhat)^2)
        
      }
    }
    
    Emq=0
    yhat=input$m*Xtr()+input$q
    Emq=sum((Ytr()-yhat)^2)
    
    x.hat<-mean(Xtr())
    S.xx<-sum((Xtr()-x.hat)^2)
    
    y.hat<-mean(Ytr())
    S.xy<-sum((Xtr()-x.hat)*Ytr())
    
    beta.hat.1<-S.xy/S.xx
    beta.hat.0<-y.hat-beta.hat.1*x.hat
    y.ls<-beta.hat.1*Xtr()+beta.hat.0
    E.ls=sum((Ytr()-y.ls)^2)
    op <- par(bg = "white")
    
    surface<-persp(maxis, qaxis, EE, 
                   theta = input$tdt, phi =input$tdp, expand = 0.5, xlim=c(min(maxis),max(maxis)),
                   ylim=c(min(qaxis),max(qaxis)),
                   main=paste("Err=",round(Emq,2), "; LS Err=",round(E.ls,2)),
                   xlab="beta1", ylab="beta0" )
    
    points (trans3d(x=input$m, 
                    y = input$q, z = Emq, pmat = surface), col = "red",lwd=8)
    points (trans3d(x=beta.hat.1, 
                    y = beta.hat.0, z = min(c(EE)), pmat = surface), col = "green",lwd=8)
    # lines (trans3d(y=input$varhatL2, 
    #               x = seq(-BOUND2, BOUND2, by= .2), z =elogLik, pmat = surface), col = "green",lwd=1)
    #plot(xaxis,logLik,type="l",main=paste("logLik=",round(elogLik,2)))
    #abline(v=input$meanhatL2,col="red")
    
  })
  
  
  output$NLData <- renderPlot( {
    
    yhat=nnetf(Xtr(),W$W)
    
    plot(Xtr(),Ytr2(),
         xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2),xlab="x",ylab="y")
    
    lines(Xtr(),yhat,col="red",lwd=2)
    lines(Xtr(),sin(pi*Xtr()),col="green",lwd=2)
    
  })
  
  observeEvent(input$NLdo,{
    for (i in 1:input$NLsteps){
      Yhat=nnetf(Xtr(),W$W)
      E=Ytr2()-Yhat
      allE$allE<-c(allE$allE,mean(E^2))
      W$W<-W$W-input$NLeta*gradnet(Xtr(),E,W$W)
      
    }
    
  })
  
  observeEvent(input$NLreset,{
    allE$allE<-NULL
    W$W<-rnorm(10,sd=2)
    
    
    
  })
  
  
  output$NLEmpErr <- renderPlot( {
    if (length(allE$allE)>0){
      
      plot( allE$allE,type="l",main="Empirical error",ylab="E") 
      
    }
    
  }
  )
  
  
  output$CVData <- renderPlot( {
    
    Xtr=Xtr()[CV$I.k.tr]
    Ytr=Ytr2()[CV$I.k.tr]
    Xts=Xtr()[CV$I.k.ts]
    Yts=Ytr2()[CV$I.k.ts]
    
    
    
    plot(Xtr,Ytr,
         xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2),xlab="x",ylab="y")
    points(Xts,Yts,
           xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2),xlab="x",ylab="y",col="green",lwd=3)
    
    if (length(Yts)>0){
      Yhat=NULL
      for (i in 1:length(Yts)){
        dis=abs(Xtr-Xts[i])
        s=sort(dis,decreasing=FALSE,index=TRUE)$ix
        Yhat=c(Yhat,mean(Ytr[s[1:input$neigh]]))
        
      }
     
      CV$E[CV$I.k.ts]=Yts-Yhat
      points(Xts,Yhat,
             xlim=c(-BOUND2, BOUND2),ylim=c(-1.5*BOUND2, 1.5*BOUND2),xlab="x",ylab="y",col="red")
      title(paste("Fold=",isolate(CV$k),"/",input$folds))
      legend(x=-BOUND2,y=-BOUND2,legend=c("prediction","test"), col=c("red","green"),pch="o")
      
    }
  })
  
  observeEvent(input$CVdo,{
    N<-NROW(Xtr())
    if (isolate(CV$k)>=input$folds){
      CV$k<-1
      CV$E=CV$E*0
    } else
      CV$k<-(isolate(CV$k)+1)
    k<-min(input$folds,isolate(CV$k))
    N.k<-round(N/input$folds)
    CV$I.k.ts<-CV$I[((k-1)*N.k+1):min(N,N.k*k)]
    CV$I.k.tr<-setdiff(1:N,isolate(CV$I.k.ts))
    if (any(is.na(CV$I.k.ts)))
      stop("error")
    
   
    
  })
  
  
  output$CVErr <- renderPlot( {
    if (length(CV$E)>0){
      
      plot( Xtr(),CV$E,main=paste("CV err=",mean(CV$E^2)),ylim=c(-2,2)) 
      
    }
    
  }
  )
}



shinyApp(ui, server)
