

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)
library(tree)

BOUND1<-2
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="Tree"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 15,
                  max = 200,
                  value = 100,step=2),
      sliderInput("R",
                  "Number of simulation trials:",
                  min = 50,
                  max = 100,
                  value = 50,step=2),
      
     
      menuItem("NNet fitting", tabName = "Nonlinear", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
     
      tabItem(tabName = "Nonlinear",
              fluidRow(box(width=2,collapsible = TRUE,
                           selectInput("ord", label = h3("Functions"), 
                                       choices = list( "Pol 3" = 3, "sin x" = -1, "cos(2x)"=-2,"3/(1+x^2)"=-3), 
                                       selected = -2),
                           #sliderInput("ord","Function:", min = 0,max = 3,  value = 1,step=1),
                           sliderInput("h","Min leaf samples:", min = 2,max = 150, 
                                       value = 0,step=1), height = 300), 
                       box(width=2,
                           sliderInput("nsdw","Cond sdev:", min = 0.1,max = 1.5, 
                                       value = 0.5,step=0.1),
                           sliderInput("K","# folds:", min = 2, max = 10, value = 10,step=1), height = 300),
                       box(width=8,collapsible = TRUE,title = "Sampling distribution",plotOutput("nlinearBV", height = 300))
                       ),## fluidRow
              fluidRow(   
                #box(width=3,collapsible = TRUE,title = "Bias^2 vs p",plotOutput("B", height = 300)),
                #                   box(width=3,collapsible = TRUE,title = "Variance vs p",plotOutput("V", height = 300)),
                                   box(width=6,collapsible = TRUE,title = "MSE vs p",plotOutput("M", height = 300)),
                                   box(width=6,collapsible = TRUE,title = "MSE_CV vs p",plotOutput("MCV", height = 300)))
              
      )
    )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
O<-NULL
B<-NULL
V<-NULL
M<-NULL
MCV<-NULL
avg.var<-NULL
avg.cv<-NULL
avg.mse<-NULL
avg.bias2<-NULL
Remp<-NULL
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  f<-function(x,ord){
    f<-numeric(length(x))
    if (ord==-1)
      f<-sin(x)
    if (ord==-2)
      f<-cos(2*x)
    if (ord==-3)
      f<-3/(1+x^2)
    if (ord>=1)
      for (o in 1:ord)
        f<-f+x^o
      
      
      
      f
  }
  
  hyp<-function(X,Y, Xts, h){
    
    N<-length(Xts)
    if (h==0){
      
      return(numeric(N)+mean(Y))
    }
   
    
    DN<-data.frame(cbind(Y,X))
    colnames(DN)[1]<-'Y'
    mod<-tree(Y~.,data=DN,minsize=h)
    
    
    
    Dts=data.frame(array(Xts,c(length(Xts),1)))
    colnames(Dts)=colnames(DN)[2]
    
    
    return(predict(mod,newdata=Dts))
  }
  
  cv<-function(X,Y, h, K){
    N.tr<-length(Y)
    
    N.k<-round(N.tr/K)
    cv<-rep(0,K)
    for (k in 1:K)
    {
      I.k.ts<-min(N.tr,((k-1)*N.k+1)):min(N.tr,N.k*k)
      I.k.tr<-setdiff(1:N.tr,I.k.ts)
      X.k.tr <-X[I.k.tr]
      Y.k.tr <- Y[I.k.tr]
      
      
      X.k.ts <-X[I.k.ts]
      Y.k.ts <- Y[I.k.ts]
      Y.k.hat.ts<- hyp(X.k.tr,Y.k.tr,X.k.ts,input$h)
      test.k.MSE <- mean((Y.k.ts-Y.k.hat.ts)^2)
      cv[k]<-test.k.MSE
    }
    return(mean(cv))
  }

  
  computeBV<-function(){
    
    set.seed(0)
    input$K
    if(input$h==2){
      O<<-NULL
      B<<-NULL
      V<<-NULL
      M<<-NULL
      MCV<-NULL
    }
    
    Nts=100
    Xts<<-seq(-BOUND2, BOUND2,length.out=Nts)
    Xtr=seq(-BOUND2, BOUND2,length.out=input$N)
    
    
    Y.hat.ts<<-array(NA,c(input$R,Nts))
    Y.hat.tr<-array(NA,c(input$R,input$N))
    E.hat.tr<-array(NA,c(input$R,input$N))
    E.hat.ts<-array(NA,c(input$R,Nts))
    CV<-numeric(input$R)
    
    var.hat.w<-numeric(Nts)
    muy.ts<<-f(Xts,input$ord)
    plot(Xts,muy.ts,xlim=c(-BOUND2,BOUND2),type="n")
    muy.tr=f(Xtr,input$ord)
    
    for (r in 1:input$R){
      
      Ytr=muy.tr+rnorm(input$N,sd=input$nsdw)
      Yts=muy.ts+rnorm(Nts,sd=input$nsdw)
      
      Y.hat.ts[r,]<<-hyp(Xtr,Ytr,Xts,input$h)
      Y.hat.tr[r,]<-hyp(Xtr,Ytr,Xtr,input$h)
      E.hat.tr[r,]=Ytr-Y.hat.tr[r,]
      E.hat.ts[r,]=Yts-Y.hat.ts[r,]
      CV[r]<-cv(Xtr,Ytr,input$h,input$K)
      
    }
    
    meanY.hat.ts<<-apply(Y.hat.ts,2,mean)
    bias=muy.ts- meanY.hat.ts
    avg.bias2<<-mean(bias^2)
    varY.hat=apply(Y.hat.ts,2,var)
    avg.var<<-mean(varY.hat)
    mseY.hat.ts=apply(E.hat.ts^2,2,mean)
    avg.mse<<-mean(mseY.hat.ts)
    avg.cv<<-mean(CV)
    
    
    e=E.hat.tr[1,]
    sdw.hat=sum(e^2)/(input$N-input$h)
    Remp<<-mean(e^2)
    
    
    O<<-c(O,input$h)
    B<<-c(B,avg.bias2)
    V<<-c(V,avg.var)
    M<<-c(M,avg.mse)
    MCV<<-c(MCV,avg.cv)
    
  }
  
  cr<-reactive({computeBV()})
  output$nlinearBV <- renderPlot( {
    
    
    cr()
    
    bvtitle=paste("B2=", round(avg.bias2,3), "V=", round(avg.var,3), "MSE=", round(avg.mse,3) ,
                  "MSemp=",round(Remp,3),"MCV=",round(avg.cv,3) )
   
    plot(Xts,muy.ts,lwd=4,col="blue",type="l")
    for (r in 1:input$R)
      lines(Xts,Y.hat.ts[r,])
    lines(Xts,meanY.hat.ts,lwd=4,col="green")
    lines(Xts,muy.ts,lwd=4,col="blue")
    title(bvtitle)
    
  })
  
  output$B <- renderPlot( {
    
    cr()
    if (length(B)>=2 & input$h>0){
      sO<-sort(O,index.return=TRUE)
      
      plot(sO$x,B[sO$ix],col="blue",type="l",xlab="# parameters", ylab="BIAS^2", lwd=2)
      
    }
    
  }
  
  )
  
  output$V <- renderPlot( {
   
    cr()
    if (length(B)>=2 & input$h>0){
      sO<-sort(O,index.return=TRUE)
      plot(sO$x,V[sO$ix],col="red",type="l",xlab="# parameters", ylab="VAR", lwd=2)
    }
    
  }
  )
  
  output$M <- renderPlot( {
    
    cr()
    if (length(B)>=2 & input$h>0){
      sO<-sort(O,index.return=TRUE)
      plot(sO$x,M[sO$ix],col="magenta",type="l",xlab="# parameters", ylab="MISE", lwd=2,main=paste("arg min=",O[which.min(M)]))
    }
    
  }
  )
  
  
  output$MCV <- renderPlot( {
    
    cr()
    if (length(B)>=2 & input$h>0){
      sO<-sort(O,index.return=TRUE)
      plot(sO$x,MCV[sO$ix],col="cyan",type="l",xlab="# parameters", ylab="MISE", lwd=2,main=paste("arg min=",O[which.min(MCV)]))
    }
    
  }
  )
  
  
  
  
}



shinyApp(ui, server)
