

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-2
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="Bagging"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 50,
                  max = 300,
                  value = 50,step=1),
      
      
      menuItem("Nonlinear Regression", tabName = "Nonlinear", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      
      tabItem(tabName = "Nonlinear",
              fluidRow(box(width=3,collapsible = TRUE,
                           selectInput("ord", label = h3("Functions"), 
                                       choices = list( "Pol 3" = 3, "sin(2x)" = -1, "cos(2x)"=-2,"3/(1+x^2)"=-3), 
                                       selected = -2),
                           #sliderInput("ord","Function:", min = 0,max = 3,  value = 1,step=1),
                           sliderInput("h","Order polynomial model:", min = 0,max = 12, 
                                       value = 9,step=1), height = 300), 
                       box(width=3,
                           sliderInput("nsdw","Cond sdev:", min = 0.1,max = 5, 
                                       value = 1.7,step=0.1),
                           sliderInput("B","Botstrap resamplings:", min = 0,max = 100, 
                                       value = 0,step=1)
                           
                       )),## fluidRow
              fluidRow(     box(width=8,collapsible = TRUE,title = "Bagging prediction",plotOutput("Bagging", height = 300)))
              
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
      f<-sin(2*x)
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
    colnames(DN)[1]="Y"
    mod<-lm(Y~.,DN)
    Dts=data.frame(X.ts)
    colnames(Dts)=colnames(DN)[2:(h+1)]
    return(predict(mod,Dts))
  }
  
  
  output$Bagging <- renderPlot( {
    
    set.seed(0)
    input$K
    Nts=200
    Xts<<-seq(-BOUND2, BOUND2,length.out=Nts)
    if (input$B==0){
      Xtr<<-rnorm(input$N,sd=BOUND2)
      muy.tr<<-f(Xtr,input$ord)
      Ytr<<-muy.tr+rnorm(input$N,sd=input$nsdw)
    }
    
    Y.hat.ts<<-array(NA,c(input$R,Nts))
    Y.hat.tr<-array(NA,c(input$R,input$N))
    E.hat.tr<-array(NA,c(input$R,input$N))
    E.hat.ts<-array(NA,c(input$R,Nts))
    
    
    var.hat.w<-numeric(Nts)
    muy.ts<-f(Xts,input$ord)
    plot(Xts,muy.ts,xlim=c(-BOUND2,BOUND2),type="n")
    
    Y.hat.ts<-hyp(Xtr,Ytr,Xts,input$h)
    
    plot(Xts,muy.ts,lwd=4,col="blue",type="l")
    
    lines(Xts,Y.hat.ts)
    points(Xtr,Ytr)
    Yhat=Y.hat.ts
    if (input$B >0){
      Y.hat.b<-array(NA,c(input$B,Nts))
      for (b in 1:input$B){
        Ib<-sample(1:input$N,input$N,replace = TRUE)
        Y.hat.b[b,]<-hyp(Xtr[Ib],Ytr[Ib],Xts,input$h)
        
      }
      Yhat=apply(Y.hat.b,2,mean)
      
    }
    lines(Xts,Yhat,col="red",lwd=3)
    title(paste("MSE=",mean((muy.ts-Yhat)^2)))
    
  })
  
  
  
  
}



shinyApp(ui, server)
