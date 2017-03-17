

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-2
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="Recursive LS"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 5,
                  max = 200,
                  value = 50,step=2),
      sliderInput("sd",
                  "Cond sd:",
                  min = 0.01,
                  max = 1,
                  value = 0.1,step=0.01),
      sliderInput("P",
                  "Proportion of samples:",
                  min = 0.1,
                  max = 1,
                  value = 0,step=0.01),
      menuItem("Nonlinear Regression", tabName = "Nonlinear", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      
      tabItem(tabName = "Nonlinear",
              fluidRow(box(width=6,collapsible = TRUE,
                           selectInput("ord", label = h3("Functions"), 
                                       choices = list("Pol 0" = 0, "Pol 1" = 1, "Pol 2" = 2, "Pol 3" = 3, "sin x" = -1, "cos(2x)"=-2,"3/(1+x^2)"=-3), 
                                       selected = 2), 
                           sliderInput("mu","Forgetting factor:", min = 0.6,max = 1, 
                                       value = 0.8,step=0.01))),## fluidRow
              fluidRow(   box(width=10,collapsible = TRUE,title = "Linear fitting",plotOutput("linearFit", height = 300)))
              
      )
    )
  
)) # ui

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
  
  
  rls<-function(x,y,t,P,mu=1){
    
    P.new <-(P-(P%*%x%*%x%*%P)/as.numeric(1+x%*%P%*%x))/mu
    ga <- P.new%*%x
    epsi <- y-x%*%t
    t.new<-t+ga*as.numeric(epsi)
    list(t.new,P.new)
  }
  
  
  output$linearFit <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=f(X,input$ord)
  
    Y=muy+rnorm(input$N,sd=input$sd)
    
    plot(X,Y,xlim=c(-BOUND2,BOUND2))
    
      
      n<-1
    #  Y.hat<-hyp(X,Y,X,1)
      
     lines(X,muy)
    
     t<-numeric(2)
     P<-500*diag(n+1)
     mu<-0.9
     for (i in 1:round(input$N*input$P)){
       rls.step<-rls(c(1, X[i]),Y[i],t,P,input$mu)
       t<-rls.step[[1]]
       P<-rls.step[[2]]
      # plot(X[1:i],y[1:i],
       #     xlim=c(-4,4),
        #    ylim=c(-2,2),
         #   main=paste("Forgetting factor mu<-",mu))
       
       
       
       
     }
     lines(X[1:i],cbind(array(1,c(i,1)), X[1:i])%*%t,
           col="red",lwd=3
     )
    
  })
  
 
  
}



shinyApp(ui, server)
