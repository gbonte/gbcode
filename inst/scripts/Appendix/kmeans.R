## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

library("gbcode")
set.seed(0)

dist2<-function(X1,X2){
  ## X1 [N1,n]
  ## X2 [N2,n]
  
  
  N1<-NROW(X1)
  n<-NCOL(X1)
  n2<-NCOL(X2)
  N2<-NROW(X2)
  
  
  
  if (n != n2){
    cat("\n n=",n)
    cat("\n n2=",n2)
    stop('Matrix sizes do not match.')
  }
  
  y<-array(0,c(N1,N2))
  
  if (n==1){
    
    for (i in 1:N1){
      x <- array(1,c(N2,1))%*%as.numeric(X1[i])
      y[i,] <- abs(x-X2)
    }
  }else {
    if (N1<N2){
      for (i in 1:N1){
        x <- array(1,c(N2,1))%*%as.numeric(X1[i,])
        y[i,] <-apply(((x-X2)^2),1,sum)
        
      }
    }else {
      
      for (j in 1:N2){
        
        x <- array(1,c(N1,1))%*%as.numeric(X2[j,])
        y[,j] <-apply(((x-X1)^2),1,sum)
        
      }
      
    }
  }
  
  
  
  ## y[N1,N2]
  sqrt(y)
  
}
N<-500

mu1<-c(0,0)
mu2<-c(6,0)
mu3<-c(4,4)
mu4<-c(1,1)

D1<-cbind(rnorm(N,mu1[1]),rnorm(N,mu1[2]))

D2<-cbind(rnorm(N,mu2[1]),rnorm(N,mu2[2]))

D3<-cbind(rnorm(N,mu3[1]),rnorm(N,mu3[2]))

D4<-cbind(rnorm(N,mu4[1]),rnorm(N,mu4[2]))

D<-rbind(D1,D2,D3,D4)

m<-4

Seeds<-array(rnorm(m*2),c(m,2))

plot(D,xlab="g1",ylab="g2")
readline()

for (it in 1:100){
  
  
  Dist<-dist2(D,Seeds)
  Is<-apply(Dist,1,which.min)
  
  for (i in 1:m){
    Isi<-which(Is==i)
    
    if (i==1)
      plot(D[Isi,],col="red",xlim=c(min(D[,1]),max(D[,1])),ylim=c(min(D[,2]),max(D[,2])),main=paste("Iteration=",it,"No. clusters=",m),xlab="g1",ylab="g2")
    if (i==2)
      points(D[Isi,],col="blue")
    
    if (i==3)
      points(D[Isi,],col="green")
    
    if (i==4)
      points(D[Isi,],col="yellow")
    
    if (i==5)
      points(D[Isi,],col="darkgrey")
    if (i==6)
      points(D[Isi,],col="orange")
    Seeds[i,]<-apply(D[Isi,],2,mean)
  }
  
  readline()
  
}
