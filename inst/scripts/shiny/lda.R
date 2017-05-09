
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library("quadprog")
library("MASS")


BOUND1<-5
BOUND2<-5
ui <- dashboardPage(
  dashboardHeader(title="Classification"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 100,step=2),
      sliderInput("mu11","mu1[1]:",min = -BOUND2,max = BOUND2,value = -2,step=0.05),
      sliderInput("mu12","mu1[2]:", min = -BOUND2, max = BOUND2, value = -2,step=0.05),
      sliderInput("mu21","mu2[1]:",min = -BOUND2,max = BOUND2,value = 2,step=0.05),
      sliderInput("mu22","mu2[1]:", min = -BOUND2, max = BOUND2, value = 2,step=0.05),
      sliderInput("V","var:", min = 0.001, max = 2, value = 1,step=0.01),
      menuItem("LDA", tabName = "LDA", icon = icon("th")),
      menuItem("Perceptron", tabName = "Perceptron", icon = icon("th")),
      menuItem("SVM", tabName = "SVM", icon = icon("th")),
      menuItem("KNN", tabName = "KNN", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Second tab content
      tabItem(tabName = "LDA",
              fluidRow(
                #  box(width=3,title = "p(x| green)",collapsible = TRUE,plotOutput("biPlotP1")),
                #  box(width=3,title = "p(x|red)",collapsible = TRUE,plotOutput("biPlotP2")),
                box(width=8,title = "LDA",plotOutput("biPlotD")))
              
      ),
      tabItem(tabName = "Perceptron",
              fluidRow(   box(width=3,actionButton("action", label = "Gradient step")),
                          box(width=3,sliderInput("eta","eta:", min = 0.0001, max = 0.1, value = 0.01,step=0.0001))),
              fluidRow(   box(width=8,title = "HYP",plotOutput("biPlotH")))
      ),
      tabItem(tabName = "SVM",
              fluidRow(   box(width=8,title = "SVM",plotOutput("biPlotSVM")))
      ),
      tabItem(tabName = "KNN",
              fluidRow(  
                          box(width=3,sliderInput("K","K:", min = 1, max = 10, value = 3,step=1))),
              fluidRow(   box(width=8,title = "KNN",plotOutput("biPlotKNN")))
      )
    )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  histdata <- rnorm(500)
  beta0<<-0
  beta1<<-numeric(2)
  mu1<-numeric(2)
  mu2<-numeric(2)
  V<-0
  KNN<- function(X,Y,k,q){
    l<-levels(Y)
    N<-nrow(X)
    d<-sqrt(apply((X-array(1,c(N,1))%*%q)^2,1,sum)) ## Euclidean metric
    ## d<-sqrt(apply(abs(X-array(1,c(N,1))%*%q),1,sum)) ## Manhattan metric
    ##  d<-1/cor(t(X),q)           ## correlation metric
    
    index<-sort(d,index.return=TRUE)
    cnt<-numeric(length(l))
    for (i in 1:k){
      cnt[Y[index$ix[i]]]<-cnt[Y[index$ix[i]]]+1
      
    }
    l[which.max(cnt)]
    
  }
  
  
  
  
  output$biPlotP1 <- renderPlot({
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z1[i,j]<-dmvnorm(c(x[i],y[j]),mean=c(input$mu11,input$mu12),sigma=diag(2)*input$V)
      }
    }
    z1[is.na(z1)] <- 1
    
    op <- par(bg = "white")
    persp(x, y, z1, theta = 30, phi = 30, expand = 0.5, col = "green")
  })
  
  output$biPlotP2 <- renderPlot({
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    
    z2<-array(0,dim=c(length(x),length(y)))
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        
        px2=dmvnorm(c(x[i],y[j]),c(input$mu21,input$mu22),sigma=diag(2)*input$V)
        z2[i,j]<-px2
      }
    }
    
    z2[is.na(z2)] <- 1
    op <- par(bg = "white")
    persp(x, y, z2, theta = 30, phi = 30, expand = 0.5, col = "red")
  })
  
  
  
  
  
  norm<-function(x){
    sqrt(sum(x^2))
  }
  output$biPlotD <- renderPlot( {
    th=0
    
    
    D1=rmvnorm(input$N,c(input$mu11,input$mu12),sigma=input$V*diag(2))
    
    plot(D1[,1],D1[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="green",xlab="x1",ylab="x2")
    
    
    D2=rmvnorm(input$N,c(input$mu21,input$mu22),sigma=input$V*diag(2))
    
    
    
    points(D2[,1],D2[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="red")
    
    
    Xd<--10:10
    mu.1<-apply(D1,2,mean)
    mu.2<-apply(D2,2,mean)
    P<-c(0.5,0.5)
    sigma2<-mean(apply(D1,2,var))
    w<-mu.1-mu.2
    x0<-0.5*(mu.1+mu.2)-sigma2/(norm(mu.1-mu.2)^2)*(mu.1-mu.2)*log(P[1]/P[2])
    
    m<--w[1]/w[2]
    intc<-w[1]/w[2]*x0[1]+x0[2]
    
    abline(a=intc,b=m)
    ## lines(x=c(mu.1[1],mu.2[1]),y=c(mu.1[2],mu.2[2]))
    #points(x0[1],x0[2])
    
    
  })
  
  
  
  output$biPlotH <- renderPlot( {
    th=0
    
    if (input$action==0 || sum(mu1+mu2)!=sum(c(input$mu11,input$mu12)+c(input$mu21,input$mu22)) || V !=input$V){
      mu1<<-c(input$mu11,input$mu12)
      mu2<<-c(input$mu21,input$mu22)
      V<<-input$V
      D1<<-rmvnorm(input$N,c(input$mu11,input$mu12),sigma=diag(2)*input$V)
      
      
      
      D2<<-rmvnorm(input$N,c(input$mu21,input$mu22),sigma=diag(2)*input$V)
      
      
      
      D<<-rbind(D1,D2)
      Y<<-c(numeric(NROW(D1))+1,numeric(NROW(D2))-1)
      
      
      beta0<<-0
      beta1<<-c(0,1)
      
      N<<-length(Y)
      E<<-0
      G1<<-0
      miscl<<-0
      for (i in 1:N){
        
        if (Y[i]*(D[i,1]*beta1[1]+D[i,2]*beta1[2]+beta0)<0){
          E<<-E-Y[i]*(D[i,1]*beta1[1]+D[i,2]*beta1[2]+beta0)
          miscl<<-miscl+1
        }
      }
      
      
    } else {
      E<<-0
      G1<<-0
      G0<<-0
      miscl<<-0
      for (i in 1:N){
        
        if (Y[i]*(D[i,1]*beta1[1]+D[i,2]*beta1[2]+beta0)<0){
          E<<-E-Y[i]*(D[i,1]*beta1[1]+D[i,2]*beta1[2]+beta0)
          G1<<-G1-Y[i]*(D[i,])
          G0<<-G0-Y[i]
          miscl<<-miscl+1
        }
      }
      
      beta0<<-beta0-input$eta*G0
      beta1<<-beta1-input$eta*G1
      
      
    }
    ## beta0+ x1*beta1[1]+x2*beta1[2]=0 ==> x2=- x1 *beta1[1]/beta1[2] -beta0/beta1[2]
    
    
    plot(D1[,1],D1[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="green",main=paste("miscl=",miscl, "E=",E),xlab="x1",ylab="x2")
    
    points(D2[,1],D2[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="red")
    abline(b=-beta1[1]/beta1[2],a=-beta0/beta1[2])
    
    
    
  })
  
  
  output$biPlotSVM <- renderPlot( {
    th=0
    normv<-function(x,p=2){
      sum(x^p)^(1/p)
      
    }
    
    separable<-TRUE
    
    if (!separable){
      gam<-0.05
    } else {
      gam<-Inf
    }
    eps<-0.001
    
    
    mu1<<-c(input$mu11,input$mu12)
    mu2<<-c(input$mu21,input$mu22)
    V<<-input$V
    D1<<-rmvnorm(input$N,c(input$mu11,input$mu12),sigma=diag(2)*input$V)
    D2<<-rmvnorm(input$N,c(input$mu21,input$mu22),sigma=diag(2)*input$V)
    plot(D1[,1],D1[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="green",xlab="x1",ylab="x2")
    
    points(D2[,1],D2[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="red")
    
    X<<-rbind(D1,D2)
    Y<<-c(numeric(NROW(D1))+1,numeric(NROW(D2))-1)
    N<-NROW(X)
    Dmat<-array(NA,c(N,N))
    
    for (i in 1:(N)){
      for (j in 1:(N)){
        Dmat[i,j]<-Y[i]*Y[j]*(X[i,]%*%X[j,])
      }
    }
    
    Dmat<-Dmat+1e-3*diag(N)
    d<-array(1,c(N,1))
    
    A<-cbind(Y,diag(N))
    b<-numeric(N+1)
    
    if (! separable){
      A<-cbind(A,-diag(N/2))
      b<-c(b,numeric(N/2))
      b[(N+2):(2*N+1)]<--gam
      ##  min_b(-d^T b + 1/2 b^T D b) with the constraints A^T b >= bvec.
      ## b-> alpha [2N,1]
      ## 1st constraint sum_i y_i*alpha_i=0
      ## 2:(2N+1) constraint alpha_i >=0
      ## (2N+2):(4N+1) constraint -alpha_i>=-gam
    }
    
    
    
    S<-solve.QP(Dmat,dvec=d,Amat=A,meq=1,bvec=b)
    ##  min_b(-d^T b + 1/2 b^T D b) with the constraints A^T b >= bvec.
    ## b-> alpha [2N,1]
    ## 1st contraint sum_i y_i*alpha_i=0
    ## 2:(2N+1) constraint alpha_i >=0
    
    
    alpha<-S$solution
    alpha[alpha<eps]<-0
    ind.j<-which(alpha>eps & alpha<gam-eps)
    if (all(alpha<=gam+eps) & length(ind.j)>0){
      #  cat("min value=",S$value,"\n")
      #  cat("min value2=",-t(d)%*%alpha+(1/2*t(alpha)%*%Dmat%*%alpha),"\n")
      #  cat("sum_i y_i*alpha_i=0:",alpha%*%Y,"\n")
      
      
      beta<-numeric(2)
      for ( i in 1:(N))
        beta<-beta+alpha[i]*Y[i]*X[i,]
      
      ind1<-which(alpha[1:round(N/2)]>eps)
      ind2<-which(alpha[(N/2+1):(N)]>eps)
      
      ## PLOT Support Vector
      
      
      if (separable){
        beta0<--0.5*(beta%*%X[ind1[1],]+beta%*%X[N/2+ind2[1],])
        marg<-1/normv(beta)
        
      } else {
        L<-0
        for (i in 1:(N)){
          for (j in 1:(N)){
            L=L+Y[i]*Y[j]*alpha[i]*alpha[j]*(X[i,]%*%X[j,])
          }
        }
        
        
        beta0<-0
        
        beta0<-(1-Y[ind.j[1]]*beta%*%X[ind.j[1],])/Y[ind.j[1]]
        
        marg<-1/sqrt(L)
        
        
        ## points whose slack variable is positive
        ind3<-which(abs(alpha[1:(N/2)]-gam)<eps)
        ind4<-which(abs(alpha[(N/2+1):(N)]-gam)<eps)
        
        
        
        
      }
      ##  cat("beta=",beta,"\n")
      theta<-atan(-beta[1]/beta[2])
      ##    cat("theta=",theta,"\n")
      ## PLOT Separating Hyperplane
      abline(b=-beta[1]/beta[2],a=-beta0/beta[2])
      
      ## PLOT Margin
      abline(b=-beta[1]/beta[2],
             a=-beta0/beta[2]+ marg/(cos(pi-theta)),lty=3)
      
      abline(b=-beta[1]/beta[2],
             a=-beta0/beta[2]- marg/(cos(pi-theta)),lty=3)
      
    }
  })
  
  
  
  
  
  
  output$biPlotKNN <- renderPlot( {
    th=0
    
    
    D1=rmvnorm(input$N,c(input$mu11,input$mu12),sigma=input$V*diag(2))
    
    plot(D1[,1],D1[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="green",xlab="x1",ylab="x2")
    
    
    D2=rmvnorm(input$N,c(input$mu21,input$mu22),sigma=input$V*diag(2))
    
    points(D2[,1],D2[,2],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),col="red")
    
    X<-rbind(D1,D2)
    Y<-c(numeric(NROW(D1))+1,numeric(NROW(D2))-1)
    x <- seq(-BOUND2, BOUND2, by= .1)
    y <- x
    Yplot<-NULL
    Dplot<-NULL
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        Dplot<-rbind(Dplot,c(x[i],y[j]))
        Yplot<-c(Yplot, as.numeric(KNN(X,factor(Y),input$K,c(x[i],y[j]))))
      }
    }
    
    I0<-which(Yplot>0)
    I1<-which(Yplot<0)
    points(Dplot[I0,1],Dplot[I0,2],col="green",cex=0.1)
    points(Dplot[I1,1],Dplot[I1,2],col="red",cex=0.1)
    
  })
}
  
  
  shinyApp(ui, server)
  
  