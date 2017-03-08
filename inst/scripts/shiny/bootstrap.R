

library(shinydashboard)
library(mvtnorm)
library(scatterplot3d)
library(ellipse)
library(plot3D)

BOUND1<-2
BOUND2<-2
ui <- dashboardPage(
  dashboardHeader(title="Bootstrap estimator"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput("N",
                  "Number of samples:",
                  min = 5,
                  max = 200,
                  value = 50,step=2),
      sliderInput("R",
                  "Number of simulation trials:",
                  min = 100,
                  max = 5000,
                  value = 1000,step=2),
      sliderInput("mean","Mean:",min = -BOUND1, max = BOUND1 ,
                  value = 0,step=0.05),
      sliderInput("var","Var:",min = 0.25,max = 1.5, value = 0.5),
      uiOutput('EVar'),
      menuItem("Univariate normal", tabName = "Univariate", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #
      tabItem(tabName = "Univariate",
              fluidRow(
                box(width=6,title = "Distribution",collapsible = TRUE,plotOutput("uniPlotP", height = 300))),
              fluidRow(   
                box(width=5,title = "Estimator Mean: Sampling Distribution ",plotOutput("uniSamplingM", height = 300)),
                box(width=5,title = "Estimator Variance: Sampling Distribution ",plotOutput("uniSamplingV", height = 300))
              ),
              fluidRow(   
                box(width=5,title = "Bootstrap Estimator Mean: Sampling Distribution ",plotOutput("BSamplingM", height = 300)),
                box(width=5,title = "Bootstrap Estimator Variance: Sampling Distribution ",plotOutput("BSamplingV", height = 300))
              )
      )
  )
  )
) # ui

D<-NULL ## Univariate dataset
E<-NULL ## Bivariate eigenvalue matrix
server<-function(input, output,session) {
  
  set.seed(122)
  
  
  
  
  
  
  output$uniPlotP <- renderPlot( {
    
    xaxis=seq(input$mean-2*BOUND1,input$mean+2*BOUND1,by=0.01)
    D<<-rnorm(input$N,input$mean,sqrt(input$var))
    plot(xaxis,dnorm(xaxis,input$mean,sqrt(input$var)),
         ylab="density",type="l",lwd=2)
    points(D,D*0)
    
  })
  
  output$uniSamplingM <- renderPlot( {
    meanD<-NULL
    for (r in 1:input$R){
      meanD<-c(meanD,mean(rnorm(input$N,input$mean,sqrt(input$var))))
      
    }
    hist(meanD,xlim=c(-BOUND1,BOUND1),main=paste("Avg=",round(mean(meanD),2),
                                                 "Var=",round(var(meanD),5) ))
    
    
  })
  
  output$uniSamplingV <- renderPlot( {
    varD<-NULL
    for (r in 1:input$R){
      varD<-c(varD,var(rnorm(input$N,input$mean,sqrt(input$var))))
      
    }
    hist(varD,xlim=c(0,2*BOUND1),main=paste("Avg=",round(mean((varD)),3)))
    
    
  })
  
  output$EVar <- renderUI({
    withMathJax(sprintf('Estimator mean: variance   $$\\frac{\\sigma^2}{N}= %.04f$$',input$var/input$N))
  })
  
 
  
  output$BSamplingM <- renderPlot( {
    input$N
    input$mean
    input$var
    meanD<-NULL
    for (r in 1:input$R){
      Db<-D[sample(1:input$N,input$N,replace =TRUE)]
      meanD<-c(meanD,mean(Db))
      
    }
    hist(meanD,xlim=c(-BOUND1,BOUND1),main=paste("Avg=",round(mean(meanD),2),
                                                 "Var=",round(var(meanD),5) ))
    
    
  })
  
  output$BSamplingV <- renderPlot( {
    input$N
    input$mean
    input$var
    varD<-NULL
    for (r in 1:input$R){
      Db<-D[sample(1:input$N,input$N,replace =TRUE)]
      varD<-c(varD,var(Db))
      
    }
    hist(varD,xlim=c(0,2*BOUND1),main=paste("Avg=",round(mean((varD)),3)))
    
    
  })
  
  output$Uinfo<- renderUI({
    withMathJax(sprintf('$$\\mu= %.04f,  \\sigma^2= %.04f$$ \n $$\\frac{\\sigma^2}{N}= %.04f $$',mean(input$bound), (1/12*(input$bound[2]-input$bound[1])^2),
                        (1/12*(input$bound[2]-input$bound[1])^2)/input$N))
   
  })
  
  output$biPlotP <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= .2)
    y <- x
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    ax1<-input$ax11
    th=input$rot1
    
    ax2<-input$ax21
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    E<<-eigen(Sigma)
    
    
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-dmvnorm(c(x[i],y[j]),sigma=Sigma)
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    surface<-persp3D(x, y, prob.z, theta = input$tdt, phi = input$tdp, expand = 0.5, col = "blue",facets=FALSE)
    
    
    
    
  })
  
  
  
  
  output$biSamplingM <- renderPlot( {
    th=input$rot1
    
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax11, 0, 0, input$ax21),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    
    allmeanD<-NULL
    for (r in 1:input$R){
      D=rmvnorm(input$N,sigma=Sigma)
      meanD=apply(D,2,mean)
      allmeanD<-rbind(allmeanD,meanD)
      
      
    }
    
    plot(allmeanD[,1],allmeanD[,2],xlim=c(-BOUND1/2,BOUND1/2), ylim=c(-BOUND1/2,BOUND1/2))
    lines(ellipse(cov(allmeanD)),col="red",lwd=2)
    
    
  })
  
  output$biSamplingV <- renderPlot( {
    th=input$rot1
    
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(input$ax11, 0, 0, input$ax21),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    
    allmeanD<-NULL
    for (r in 1:input$R){
      D=rmvnorm(input$N,sigma=Sigma)
      if (r==1)
        plot(ellipse(cov(D)),type="l",xlim=c(-2*BOUND1,2*BOUND1), ylim=c(-2*BOUND1,2*BOUND1))
      else
        lines(ellipse(cov(D)))
      
      
    }
    
    
    
  })
  
  
  output$textB <- renderText({ 
    input$rot
    input$ax1
    input$ax2
    paste("Eigen1=", E$values[1], "\n Eigen2=", E$values[2])
  })
  
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
        z[i,j]<-dnorm(y[j],mean=muy[i],sd=input$sdw)
      }
    }
    z[is.na(z)] <- 1
    op <- par(bg = "white")
    prob.z<-z
    
    surface<-persp3D(x, y, prob.z, theta = input$tdt, phi =input$tdp, expand = 0.5, col = "blue",facets=FALSE)
    
    
    
    
  })
  
  output$linearBV <- renderPlot( {
    
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=input$q+input$m*X
    
    Y.hat<-array(NA,c(input$R,input$N))
    
    beta.hat.1<-numeric(input$R)
    beta.hat.0<-numeric(input$R)
    var.hat.w<-numeric(input$R)
    plot(X,muy,xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),lwd=3)
    for (r in 1:input$R){
      X.tr<-rnorm(input$N)
      x.hat<-mean(X.tr)
      S.xx<-sum((X.tr-x.hat)^2)
      muy.tr=input$q+input$m*X.tr
      Y=muy.tr+rnorm(input$N,sd=input$sdw)
      
      y.hat<-mean(Y)
      S.xy<-sum((X.tr-x.hat)*Y)
      
      beta.hat.1[r]<-S.xy/S.xx
      beta.hat.0[r]<-y.hat-beta.hat.1[r]*x.hat
      
      Y.hat[r,]<-beta.hat.0[r]+beta.hat.1[r]*X
      var.hat.w[r]<-sum((Y-Y.hat[r,])^2)/(input$N-2)
      lines(X,Y.hat[r,],xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2))
    }
    
    lines(X,muy,xlim=c(-BOUND2,BOUND2),ylim=c(-BOUND2,BOUND2),lwd=3,col="lightblue")
    abline(v=input$rx,  col = "red",lwd=3)
    
  })
  
  output$linearCond <- renderPlot( {
    X=seq(-BOUND2, BOUND2,length.out=input$N)
    muy=input$q+input$m*X
    x.hat=mean(X)
    Y.hat<-array(NA,c(input$R,input$N))
    x.hat<-mean(X)
    S.xx<-sum((X-x.hat)^2)
    beta.hat.1<-numeric(input$R)
    beta.hat.0<-numeric(input$R)
    var.hat.w<-numeric(input$R)
    
    for (r in 1:input$R){
      Y=muy+rnorm(input$N,sd=input$sdw)
      
      y.hat<-mean(Y)
      S.xy<-sum((X-x.hat)*Y)
      
      beta.hat.1[r]<-S.xy/S.xx
      beta.hat.0[r]<-y.hat-beta.hat.1[r]*x.hat
      
      Y.hat[r,]<-beta.hat.0[r]+beta.hat.1[r]*input$rx
      var.hat.w[r]<-sum((Y-Y.hat[r,])^2)/(input$N-2)
      
    }
    hist(Y.hat)
    abline(v=input$q+input$m*input$rx,col="lightblue")
  })
  
  output$nlinearPlotP <- renderPlot({
    
    x <- seq(-BOUND2, BOUND2, by= input$dx)
    y <- seq(-BOUND2, BOUND2, by= input$dy)
    z<-array(0,dim=c(length(x),length(y)))
    #th : rotation angle of the first principal axis
    #ax1: length principal axis 1
    #ax2: length principal axis 2
    
    
    
    muy<-f(x,input$ord)
    
    
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-dnorm(y[j],mean=muy[i],sd=input$nsdw)
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
      Xtr=rnorm(input$N)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=input$nsdw)
      
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
    lines(X,muy,lwd=2,col="blue")
    lines(X,meanY.hat,lwd=2,col="green")
    abline(v=input$nrx,  col = "red",lwd=1)
    
  })
  
  output$nlinearCond <- renderPlot( {
    Y.hat<-numeric(input$R)  
    
    for (r in 1:input$R){
      Xtr=rnorm(input$N)
      muy.tr=f(Xtr,input$ord)
      Y=muy.tr+rnorm(input$N,sd=input$nsdw)
      
      Y.hat[r]<-hyp(Xtr,Y,input$nrx,input$h)
      
      
    }
    
    
    
    Yq=f(input$nrx,input$ord)
    bvtitle=paste("Bias^2=", round((Yq-mean(Y.hat))^2,2), "Var=", round(var(Y.hat),2), "MSE=", round(mean((Yq-Y.hat)^2),2) )
    hist(Y.hat,xlim=c(min(c(Y.hat,Yq)),max(c(Y.hat,Yq))),main=bvtitle)
    abline(v=Yq,col="blue",lwd=3)
    abline(v=mean(Y.hat),  col = "green",lwd=3)
    
  })
  
  
}



shinyApp(ui, server)
