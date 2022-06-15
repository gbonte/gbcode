## Estimate of a correlation by the variance of difference of the mean

rm(list=ls())

R=10000
N=5000
Dy0=NULL
Dy1=NULL
D=NULL
CC=NULL
Y0=sample(c(0,1),N,rep=TRUE)

Y1=2*Y0-1
Y1[sample(N,100)]=0
CC=cor(Y0,Y1)

I0=sample(N,round(N/2))
I1=setdiff(1:N,I0)


for (r in 1:R){
  Ib0=sample(I0,length(I0)-5)
  Ib1=sample(I1,length(I1)-5)
  muY0=mean(Y0[Ib0])
  muY1=mean(Y1[Ib1])
  
  Dy0=c(Dy0,muY0)
  Dy1=c(Dy1,muY1)
  d=muY0-muY1
  D=c(D,d)
  
  
}
## V[mux-muy]=V[mux]+V[muy]-2 rho sqrt(V[mux] V[muY])
cat((-var(D)+var(Dy0)+var(Dy1))/(2*sd(Dy0)*sd(Dy1)),":",CC)
