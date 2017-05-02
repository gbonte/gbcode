
rm(list=ls())

f<-function(x1,x2){
  N<-length(x1)
  return(x1^2+x2^2)
  if (x1<0)
    return(numeric(N)+1)
  return(-1+numeric(N))
  
}


N<-100
X<-array(rnorm(2*N),c(N,2))
Y<-numeric(N)
for (i in 1:N)
  Y[i]<-f(X[i,1],X[i,2])+rnorm(1,sd=0.1)


cX<-seq(-1.5,1.5,by=0.1)
z <- outer(cX, cX, FUN = "f")




V=var(Y)
Vt<-NULL
for (j in 1:2)
for (th in cX){
  par(mfrow=c(1,2))
  
  contour(cX, cX,z, method = "edge", vfont = c("sans serif", "plain"))
  
  In=which(Y<0)
  points(X[In,1],X[In,2],col="red")
  Ip=which(Y>0)
  points(X[Ip,1],X[Ip,2],col="green")
  
  
  if (j==1)
  abline(v=th)
  if (j==2)
    abline(h=th)
  I1=which(X[,j]<th)
  I2=which(X[,j]>th)
  N1<-length(I1)
  N2<-length(I2)
  Y1<-Y[I1]
  Y2<-Y[I2]
  Vt<-c(Vt,N1/N*var(Y1)+N2/N*var(Y2))
  title(paste("MSE=",round(V,2),"MSE1=",round(var(Y1),2),"MSE2=",round(var(Y2),2)))
  plot(1:length(Vt),V+numeric(length(Vt)),type="l",ylim=c(min(c(V,Vt)),max(c(V,Vt))))
  lines(1:length(Vt),Vt)
  browser()
}