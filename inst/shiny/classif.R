
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("quadprog")
library("MASS")
require(shiny)
library(plotly)
library(shinydashboard)
library(mvtnorm)
#library(scatterplot3d)
##library(ellipse)

#library(rgl)
BOUND1<-1.5
BOUND2<-1.5
ui <- dashboardPage(
  dashboardHeader(title="InfoF422: classification and assessment", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 1,
                  max = 1000,
                  value = 500,step=2),
      menuItem("Univariate mixture", tabName = "Univariatemixture", icon = icon("th")),
 #     menuItem("Bivariate mixture", tabName = "Bivariatemixture", icon = icon("th")),
      menuItem("Linear Discriminant", tabName = "Discriminant", icon = icon("th")),
      menuItem("Perceptron", tabName = "perceptron", icon = icon("th")),
      menuItem("Assessment classifier", tabName = "roc", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Univariatemixture",
              fluidRow(
                box(width=3,sliderInput("mean1",withMathJax(sprintf('$$\\mu_1:$$')),min = -BOUND1, max = BOUND1 ,
                                        value = -2,step=0.1),
                    sliderInput("variance1",withMathJax(sprintf('$$\\sigma^2_1:$$')),min = 0.5,max = 1, value = 0.75,step=0.05),
                    sliderInput("mean2",withMathJax(sprintf('$$\\mu_2:$$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1),
                    sliderInput("variance2",withMathJax(sprintf('$$\\sigma^2_2:$$')),min = 0.5,max = 1, value = 0.75,step=0.05),
                    sliderInput("p1",withMathJax(sprintf('$$P_1:$$')),min = 0, max = 1 ,
                                value = 0.5)),
                box(width=7,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP"))),
              
              fluidRow(   box(width=6,title = "Dataset (1:red, 2:green)",plotOutput("uniPlotD"))                
              )
      ),
      tabItem(tabName = "Bivariatemixture",
              fluidRow(
                box(width=3,
                    sliderInput("mean11",withMathJax(sprintf('$$\\mu_{11}:$$')),min = -BOUND1, max = BOUND1 ,
                                        value = -2,step=0.1),
                    sliderInput("mean12",withMathJax(sprintf('$$\\mu_{12}:$$')),min = -BOUND1, max = BOUND1 ,
                                value = -2,step=0.1),
                   sliderInput("variance11",withMathJax(sprintf('$$\\sigma^2_1:$$')),min = 0.5,max = 1, value = 0.75,step=0.05),
                    sliderInput("mean21",withMathJax(sprintf('$$\\mu_{21}:$$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1),
                    sliderInput("mean22",withMathJax(sprintf('$$\\mu_{22}:$$')),min = -BOUND1, max = BOUND1 ,
                                value = 2,step=0.1),
                    sliderInput("variance22",withMathJax(sprintf('$$\\sigma^2_2:$$')),min = 0.5,max = 1, value = 0.75,step=0.05),
                    sliderInput("p11",withMathJax(sprintf('$$P_1:$$')),min = 0, max = 1 ,
                                value = 0.5)),
                box(width=7,title = "Distribution",collapsible = TRUE,
                    plotOutput("plot")))
                    ##plotlyOutput("biPlotP")))#,
     #         fluidRow(   box(width=6,title = "Data",plotOutput("biPlotD"))                )
      ),
      # Second tab content
      tabItem(tabName = "Discriminant",
              fluidRow(
                box(width=3,sliderInput("sigma1",withMathJax(sprintf('$$\\sigma^2_1:$$')),min = 0.01,max = 1.1,value = 1,step=0.05)),
                box(width=3, sliderInput("mux1",withMathJax(sprintf('$$\\mu_{1x}:$$')), min = -BOUND2, max = BOUND2, value = -BOUND2/2,step=0.05)),
                box(width=3, sliderInput("muy1",withMathJax(sprintf('$$\\mu_{1y}:$$')), min = -BOUND2, max = BOUND2, value = -BOUND2/2,step=0.05))
              ),
              fluidRow(
                
                box(width=3,    sliderInput("sigma2",withMathJax(sprintf('$$\\sigma^2_2:$$')),min = 0.01,max = 1.1,value = 1,step=0.05)),
                box(width=3, sliderInput("mux2",withMathJax(sprintf('$$\\mu_{2x}:$$')), min = -BOUND2, max = BOUND2, value = BOUND2/2,step=0.05)),
                box(width=3, sliderInput("muy2",withMathJax(sprintf('$$\\mu_{2y}:$$')), min = -BOUND2, max = BOUND2, value = BOUND2/2,step=0.05)),
                box(width=3,    sliderInput("P1",withMathJax(sprintf('$$P_1:$$')),min = 0.01, max = 0.99 ,value = 0.5,step=0.05))
              ),
              fluidRow(   box(width=9,title = "Dataset (1: red, 2:green)",
                              plotOutput("biPlotD")))
              
      ),
      tabItem(tabName = "perceptron",
              fluidRow(
                box(width=3,sliderInput("NLsigma1",withMathJax(sprintf('$$\\sigma^2_1:$$')),min = 0.01,max = 1.1,value = 0.1,step=0.05)),
                box(width=3, sliderInput("NLmux1",withMathJax(sprintf('$$\\mu_{1x}:$$')), min = -BOUND2, max = BOUND2, value = -BOUND2/2,step=0.05)),
                box(width=3, sliderInput("NLmuy1",withMathJax(sprintf('$$\\mu_{1y}:$$')), min = -BOUND2, max = BOUND2, value = -BOUND2/2,step=0.05))
              ),
              fluidRow(
                
                box(width=3,    sliderInput("NLsigma2",withMathJax(sprintf('$$\\sigma^2_2:$$')),min = 0.01,max = 1.1,value = 0.1,step=0.05)),
                box(width=3, sliderInput("NLmux2",withMathJax(sprintf('$$\\mu_{2x}:$$')), min = -BOUND2, max = BOUND2, value = BOUND2/2,step=0.05)),
                box(width=3, sliderInput("NLmuy2",withMathJax(sprintf('$$\\mu_{2y}:$$')), min = -BOUND2, max = BOUND2, value = BOUND2/2,step=0.05)),
                box(width=3,    sliderInput("NLP1",withMathJax(sprintf('$$P_1:$$')),min = 0, max = 1 ,value = 0.5))
              ),
              fluidRow(box(width=3,collapsible = TRUE,
                           sliderInput("NLsteps","# steps", min = 1,max = 100, 
                                       value = 1,step=1),
                           sliderInput("NLeta",withMathJax(sprintf('$${\\eta}:$$')), min = 0.0001,max = 1, 
                                       value = 0.01,step=0.01)),
                       box(width=3,collapsible = TRUE,checkboxInput("SVM", "SVM", FALSE),
                           actionButton("NLdo", "Gradient step NNet"),
                           actionButton("NLreset", "Reset weights"))
              ),
              fluidRow(box(width=8,collapsible = TRUE,title = "Dataset (1: red, 2:green)",plotOutput("NLData", height = 600)),
                       box(width=4,collapsible = TRUE,title = "Misclassification",
                           plotOutput("NLEmpErr", height = 600)))
              
      ),
      tabItem(tabName = "roc",
              fluidRow(
                box(width=3,sliderInput("Rmean1",withMathJax(sprintf('$$\\mu_{(-)}:$$')),min = -BOUND1, max = BOUND1 ,
                                        value = -0.5,step=0.1),
                    sliderInput("Rvariance1",withMathJax(sprintf('$$\\sigma^2_{(-)}:$$')),min = 0.05,max = 2, value = 0.75)),
                box(width=3,sliderInput("Rmean2",withMathJax(sprintf('$$\\mu_{(+)}:$$')),min = -BOUND1, max = BOUND1 ,
                                        value = 0.5,step=0.1),
                    sliderInput("Rvariance2",withMathJax(sprintf('$$\\sigma^2_{(+)}:$$')),min = 0.05,max = 2, value = 0.75,step=0.05)),
                box(width=4,    sliderInput("Rp",withMathJax(sprintf('$$P_{(-)}:$$')),min = 0, max = 1 ,
                                            value = 0.5),
                    sliderInput("thr","thr:",min = -2*BOUND1, max = 2*BOUND1 ,
                                value = 0,step=0.01))
              ),
              fluidRow( box(width=6,title = "Scores classifier (-: red, +: green)",plotOutput("ROCPlotD")) ,
                        box(width=6,title = "ROC curve",plotOutput("ROCPlotROC"))),
              fluidRow(box(width=6,tableOutput('table'),htmlOutput("textB"), height=400),
                       box(width=6,title = "PR curve",plotOutput("ROCPlotPR")))
              #h3(htmlOutput("textB")))
      ),
      tabItem(tabName = "about",
              fluidPage(
                includeHTML("about/about.classif.html")
              ))## tabItem
    )## tabItems
  ) # DashboardBody
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  SVM<-function(X1,Y1,X2,Y2,separable=TRUE){
    normv<-function(x,p=2){
      sum(x^p)^(1/p)
      
    }
    
    if (!separable){
      gam<-0.01
    } else {
      gam<-Inf
    }
    
    eps<-0.001
    X<-rbind(X1,X2)
    Y<-c(Y1,Y2)
    N1<-length(Y1)
    N2<-length(Y2)
    N<-N1+N2
    
    
    ##########################################
    ########## SVM parametric identification
    
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
      A<-cbind(A,-diag(N))
      b<-c(b,numeric(N))
      b[(N+1):(2*N+1)]<--gam
      
      
      ##  min_b(-d^T b + 1/2 b^T D b) with the constraints A^T b >= bvec.
      ## b-> alpha [2N,1]
      ## 1st constraint sum_i y_i*alpha_i=0
      ## 2:(2N+1) constraint alpha_i >=0
      ## (2N+2):(4N+1) constraint -alpha_i>=-gam  -> alpha_i <= gam
    }
    
    S<-solve.QP(Dmat,dvec=d,Amat=A,meq=1,bvec=b)
    ##  min_b(-d^T b + 1/2 b^T D b) with the constraints A^T b >= bvec.
    ## b-> alpha [N,1]
    ## 1st contraint sum_i y_i*alpha_i=0
    ## 2:(N+1) constraint alpha_i >=0
    
    alpha<-S$solution
    alpha[alpha<eps]<-0
    ind.j<-which(alpha>eps & alpha<gam-eps)
    if (all(alpha<=gam+eps) & length(ind.j)>0){
      beta<-numeric(2)
      for ( i in 1:(N))
        beta<-beta+alpha[i]*Y[i]*X[i,]
      ind1<-which(alpha[1:N1]>eps)
      ind2<-which(alpha[(N1+1):(N)]>eps)
      if (separable){
        beta0<--0.5*(beta%*%X[ind1[1],]+beta%*%X[N1+ind2[1],])
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
      }
      
      theta<-atan(-beta[1]/beta[2])
     
      return(list(b=-beta[1]/beta[2],a=-beta0/beta[2], 
                  a1=-beta0/beta[2]+ marg/(cos(pi-theta)), 
                  a2= -beta0/beta[2]- marg/(cos(pi-theta))))
    }
  }
  
  output$uniPlotP <- renderPlot( {
    input$variance1+input$variance2+input$p1
    input$N
    xaxis=seq(min(input$mean1,input$mean2)-BOUND1,max(input$mean1,input$mean2)+BOUND1,by=0.01)
    redp=dnorm(xaxis,input$mean1,input$variance1)
    greenp=dnorm(xaxis,input$mean2,input$variance2)
    plot(xaxis,redp,col="red",type="l",lwd=2,ylim=c(-0.1,1.2),ylab="",xlab="x")
    lines(xaxis,greenp,col="green",lwd=2)
    
    eps=1e-3
    postp=(redp*input$p1)/(redp*input$p1+greenp*(1-input$p1))
    I=which(redp<eps && greenp < eps)
    xI=xaxis[I]
    
    dr=xI-input$mean1
    dg=xI-input$mean2
    postp[I[which(dr>dg)]]=0
    postp[I[which(dr<dg)]]=1
    
    
    
    lines(xaxis,postp,col="blue",type="l",lwd=4)
    lines(xaxis,postp*(1-postp),col="black",type="l",lwd=1, lty=2)
    lines(xaxis,0.5*(numeric(length(xaxis))+1),lwd=1)
    legend("topright", c(" cond density class1", " cond density class2", 
                         "posterior", "posterior var"), 
           col = c("red","green","blue","black"), lty=c(1,1,1,2))    
    
  })
  
  
  
  output$uniPlotD <- renderPlot( {
    input$variance1+input$variance2+input$p1
    input$N
    D1<-rnorm(input$N,input$mean1,input$variance1)
    D2<-rnorm(input$N,input$mean2,input$variance2)
    I1<-sample(1:input$N,round(input$p1*input$N))
    I2<-sample(1:input$N,round((1-input$p1)*input$N))
    D1<-D1[I1]
    D2<-D2[I2]
    xl=min(input$mean1,input$mean2)-BOUND1
    xu=max(input$mean1,input$mean2)+BOUND1
    
    
    plot(D1,0*D1,xlim=c(xl,xu),col="red",ylab="")
    points(D2,0.11*(numeric(length(D2))+1),xlim=c(xl,xu),col="green")
    
    
  })
  output$plot <- renderPlot({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
  output$biPlotP <- renderPlotly({
    
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z1<-array(0,dim=c(length(x),length(y)))
    z2<-array(0,dim=c(length(x),length(y)))
      
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z1[i,j]<-(input$p11)*dmvnorm(c(x[i],y[j]),mean=c(input$mean11, input$mean21))
        z2[i,j]<-(input$p11)*dmvnorm(c(x[i],y[j]),mean=c(input$mean11, input$mean21))
      }
    }
    z1[is.na(z1)] <- 1
    z2[is.na(z2)] <- 1
    op <- par(bg = "white")
    
 
   
   #fig <- plot_ly(z = ~volcano)
  #  fig <- fig %>% add_surface()
     
   #   fig
    
  })
  
  
  discr<-function(x,y,mu,Sigma,p=0.5){
    n=2
    v=c(x,y)
    #browser()
    return(-0.5*as.numeric((v-mu)%*%solve(Sigma)%*%(v-mu))-0.5*log(det(Sigma))+log(p))
    
  }
  
  norm<-function(x){
    sqrt(sum(x^2))
  }
  
  output$biPlotD <- renderPlot( {
    
    Sigma1<-diag(2)*input$sigma1^2
    
    D1=rmvnorm(input$N,mean=c(input$mux1,input$muy1),sigma=Sigma1)
    
    Sigma2<-diag(2)*input$sigma2^2
    
    D2=rmvnorm(input$N,mean=c(input$mux2,input$muy2),sigma=Sigma2)
    
    I1<-sample(1:input$N,round(input$P1*input$N))
    I2<-sample(1:input$N,round((1-input$P1)*input$N))
    D1<-D1[I1,]
    D2<-D2[I2,]
    par(pty="s")
    plot(D1[,1],D1[,2],xlim=c(-2*BOUND2,2*BOUND2),ylim=c(-2*BOUND2,2*BOUND2),
         col="red",xlab="x1",ylab="x2")
    points(D2[,1],D2[,2],col="green")
    
    mu.1<-c(input$mux1,input$muy1)
    mu.2<-c(input$mux2,input$muy2)
    sigma1=mean(diag(Sigma1))
    sigma2=mean(diag(Sigma2))
    P=c(input$P1,1-input$P1)
    w<-mu.1-mu.2
    x0<-0.5*(mu.1+mu.2)-sigma2/(norm(mu.1-mu.2)^2)*(mu.1-mu.2)*log(P[1]/P[2])
    if (abs(w[2])<0.001)
      w[2]=0.01
    m<--w[1]/w[2]
    intc<-w[1]/w[2]*x0[1]+x0[2]
    abline(a=intc,b=m)
    #x=seq(-2*BOUND2,2*BOUND2,by=0.01)
    #y=seq(-2*BOUND2,2*BOUND2,by=0.01)
    #O1=outer(x,y,Vectorize(discr,vectorize.args =c("x","y")),mu=c(input$mux1,input$muy1),
    #         Sigma=Sigma1,p=input$P1)
    #O2=outer(x,y,Vectorize(discr,vectorize.args =c("x","y")),mu=c(input$mux2,input$muy2),
    #         Sigma=Sigma2,p=1-input$P1)
    #O=abs(O1-O2)
    
    #y0=NULL
    #for (i in 1:length(x)){
    #  y0=c(y0,y[which.min(O[i,])])
    #}
    
    #lines(x,y0,col="green")
  })
  allE<-reactiveValues(allE=NULL)
  W<-reactiveValues(W=rnorm(3))
  D1<-reactive({
    allE$allE<-NULL
    Sigma1<-diag(2)*input$NLsigma1^2
    D<-rmvnorm(input$N,mean=c(input$NLmux1,input$NLmuy1),sigma=Sigma1)
    I1<-sample(1:input$N,round(input$NLP1*input$N))
    D[I1,]
  })  ## red: class -1
  
  D2<-reactive({
    allE$allE<-NULL
    Sigma2<-diag(2)*input$NLsigma2^2
    D<-rmvnorm(input$N,mean=c(input$NLmux2,input$NLmuy2),sigma=Sigma2)
    I2<-sample(1:input$N,round((1-input$NLP1)*input$N))
    D[I2,]
  })  ## green: class 1
  
  output$NLData <- renderPlot( {
    
    
    d1<-D1()
    d2<-D2()
    plot(d1[,1],d1[,2],xlim=c(-2*BOUND2,2*BOUND2),ylim=c(-2*BOUND2,2*BOUND2),
         col="red",xlab="x1",ylab="x2")
    points(d2[,1],d2[,2],col="green")
    m<--W$W[1]/W$W[2]
    intc<--W$W[3]/W$W[2]
    abline(a=intc,b=m)
    if (input$SVM){
      S<-SVM(d1,numeric(NROW(d1))-1,d2,numeric(NROW(d2))+1)
      
      abline(a=S$a,b=S$b,col="blue")
      abline(a=S$a1,b=S$b,col="blue",lty=2)
      abline(a=S$a2,b=S$b,col="blue",lty=2)
    }
    
  })
  observeEvent(input$NLreset,{
    allE$allE<-NULL
    W$W<-rnorm(3)
    
  })
  
  observeEvent(input$NLdo,{
    N1<-NROW(D1())
    N2<-NROW(D2())
    
    for (i in 1:input$NLsteps){
      E<-0
      grad<-c(0,0,0)
      for (j in 1:N1){
        y1=-1
        pred=W$W[1]*D1()[j,1]+W$W[2]*D1()[j,2]+W$W[3]
        if ((y1*pred)<0){
          grad[1]=grad[1]-(y1*D1()[j,1])
          grad[2]=grad[2]-(y1*D1()[j,2])
          grad[3]=grad[3]-(y1)
          E=E+1
        }
      }
      
      for (j in 1:N2){
        y1=1
        pred=W$W[1]*D2()[j,1]+W$W[2]*D2()[j,2]+W$W[3]
        if ((y1*pred)<0){
          grad[1]=grad[1]-(y1*D2()[j,1])
          grad[2]=grad[2]-(y1*D2()[j,2])
          grad[3]=grad[3]-(y1)
          E=E+1
        }
      }
      
      
      W$W<-W$W-input$NLeta*grad/(N1+N2)
      allE$allE<-c(allE$allE,E)
    }
  })
  output$NLEmpErr <- renderPlot( {
    if (length(allE$allE)>0){
      
      plot( allE$allE,type="l",main=paste("# misclassified points",allE$allE[length(allE$allE)]),ylab="M") 
      
    }
    
  }
  )
  
  
  STATS<-reactiveValues(FPR=NULL,SE=NULL,
                        TP=NULL,TN=NULL,
                        FP=NULL, FN=NULL, PR=NULL)
  
  
  RD1<-reactive({
    STATS$FPR=NULL
    STATS$SE=NULL
    STATS$TP=NULL
    STATS$FP=NULL
    STATS$FN=NULL
    STATS$TN=NULL
    STATS$PR=NULL
    rnorm(input$N*2*(1-input$Rp),input$Rmean1,input$Rvariance1)
  })  ## red: class -1
  
  RD2<-reactive({
    STATS$FPR=NULL
    STATS$SE=NULL
    STATS$TP=NULL
    STATS$FP=NULL
    STATS$FN=NULL
    STATS$TN=NULL
    STATS$PR=NULL
    rnorm(input$N*2*(input$Rp),input$Rmean2,input$Rvariance2)
  })  
  
  output$ROCPlotD <- renderPlot( {
    
    D1<-RD1() # red  negative class
    D2<-RD2()
    
    xl=min(input$Rmean1,input$Rmean2)-3*BOUND1
    xu=max(input$Rmean1,input$Rmean2)+3*BOUND1
    
    delta=1.2
    plot(D1,0*D1,xlim=c(xl,xu),col="red",ylab="", main=paste("N (red)=",NROW(D1), "P (green)=",NROW(D2)))
    points(D2,0.1*(numeric(length(D2))+1),xlim=c(xl,xu),col="green")
    abline(v=input$thr)
    
    thr<-input$thr
    
    FP<-length(which(D1>thr))
    FN<-length(which(D2<thr))
    TP<-length(which(D2>thr))
    TN<-length(which(D1<thr))
    
    text(input$thr+delta, 1, paste("FP=",FP),col="red")
    text(input$thr+delta, 0.8, paste("TP=",TP),col="green")
    text(input$thr-delta, 0.8, paste("FN=",FN),col="green")
    text(input$thr-delta, 1, paste("TN=",TN),col="red")
    rect(input$thr, -1, xu, 0.7, border = "green", col = "green",density=5,lty=2)
    rect(xl , -1, input$thr , 0.7, border = "red", col = "red",density=5,lty=2)
    CO<-data.frame(array(0,c(3,3)))
    CO[1,1]=paste("TN=",TN)
    CO[1,2]=paste("FP=",FP) 
    CO[1,3]=paste("N=",TN+FP)
    CO[1,2]=paste("FP=",FP) 
    CO[2,1]=paste("FN=",FN)
    CO[2,2]=paste("TP=",TP)
    CO[2,3]=paste("P=",FN+TP)
    CO[3,1]=paste("Nhat=",TN+FN)
    CO[3,2]=paste("Phat=",TP+FP)
    CO[3,3]=paste("All=",FN+TP+TN+FP)
    
    STATS$TP<-c(isolate(STATS$TP),TP)
    STATS$FP<-c(isolate(STATS$FP),FP)
    STATS$TN<-c(isolate(STATS$TN),TN)
    STATS$FN<-c(isolate(STATS$FN),FN)
    STATS$PR<-c(isolate(STATS$PR),TP/(TP+FP))
    colnames(CO)=c("Predicted red","Predicted green", " " )
    rownames(CO)=c("Actual red (-)","Actual green (+)", " " )
    output$table <- renderTable(CO,rownames=TRUE)
    fpr=isolate(STATS$FPR)
    se<-isolate(STATS$SE)
    
    
    if ((FP+TN)>0)
      STATS$FPR<-c(fpr,FP/(FP+TN))
    else
      STATS$FPR<-c(fpr,0)
    
    if ((TP+FN)>0)
      STATS$SE<-c(se,TP/(TP+FN))
    else
      STATS$SE<-c(se,0)
    
    
  })
  
  AUROC<-function(se,fpr){
    s<-sort(se,decr=FALSE,index=TRUE)$ix
    se<-se[s]
    fpr<-fpr[s]
    if (length(se)<4)
      return(mean(se))
    AUC<-0
    for (i in 2:length(se)){
      AUC=AUC+(fpr[i]-fpr[i-1])*(se[i]+se[i-1])/2
    }
    return(AUC)
  }
  output$ROCPlotROC <- renderPlot( {
    if (length(STATS$FPR)>0){
      fpr<-c(0,1,STATS$FPR)
      se<-c(0,1,STATS$SE)
      s<-sort(fpr,decr=FALSE,index=TRUE)$ix
      plot(fpr[s],se[s],xlim=c(0,1),ylim=c(0,1),type="l",
           xlab="FPR",ylab="SE",main=paste("FPR= FP/(TN+FP)=", round(STATS$FPR[length(STATS$FPR)],2),
                                           "\n SE=TN/(FP+TN)=", round(STATS$SE[length(STATS$SE)],2),
                                           "\n AUROC=",round(AUROC(STATS$SE,STATS$FPR),2)))
      points(STATS$FPR[length(STATS$FPR)],STATS$SE[length(STATS$SE)],col="red",lwd=4)
      abline(a=0,b=1)
    }
  }) 
  
  output$ROCPlotPR <- renderPlot( {
    if (length(STATS$FPR)>0){
      pr<-c(STATS$PR)
      se<-c(STATS$SE)
      s<-sort(se,decr=FALSE,index=TRUE)$ix
      plot(se[s],pr[s],xlim=c(0,1),ylim=c(0,1),type="l",
           xlab="Recall",ylab="PR",main=paste("PR= TP/(TP+FP)=", round(STATS$FPR[length(STATS$FPR)],2),
                                              "\n RE=TN/(FP+TN)=", round(STATS$SE[length(STATS$SE)],2)))
      points(STATS$SE[length(STATS$SE)],STATS$PR[length(STATS$PR)],col="red",lwd=4)
      abline(h=NROW(RD2())/(NROW(RD1())+NROW(RD2())),lty=2)
    }
  }) 
  
  
  output$textB <- renderText({ 
    TP=STATS$TP[length(STATS$TP)]
    TN=STATS$TN[length(STATS$TN)]
    FP=STATS$FP[length(STATS$FP)]
    FN=STATS$FN[length(STATS$FN)]
    N=TP+TN+FP+FN
    paste("ER= (FP+FN)/N =", round((FP+FN)/N,4), 
          "<br> BER= 0.5*(FP/(TN+FP)+FN/(FN+TP))=", round(0.5*(FP/(TN+FP)+FN/(FN+TP)),4),
          "<br> Sensitivity= TPR= Recall= TP/(TP+FN)= ",round(TP/(TP+FN),4),
          "<br> Specificity= TNR= TN/(TN+FP)=",round(TN/(TN+FP),4),
          "<br> FPR= FP/(TN+FP) =", round(FP/(TN+FP),4),
          "<br> FNR= FN/(TP+FN)= ", round(FN/(TP+FN),4),
          "<br> PPV= Precision= TP/(TP+FP) = ", round(TP/(TP+FP),4),
          "<br> FDR= FP/(TP+FP)= ", round(FP/(TP+FP),4)
    )
  })
  
}



shinyApp(ui, server)
